#if (defined(__MACH__) && defined(__APPLE__)) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#else
#include <values.h>
#endif
#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"
#include "const.h"
#include <cmplrs/rcodes.h>
#include <upc_wn_util.h>
#include <upc_symtab_utils.h>
#include <pf_cg.h>
#include <stack>
#include <wn_lower.h>
#include <upc_coalescing.h>

extern void fdump_tree(FILE *f, WN *wn);
extern SRCPOS upc_srcpos;


//indicates whether valget calls should be used instead of 
//memory-to-memory accesses for integral types
//This is basically always false now that we have the type-based calls
BOOL Use_Valget = FALSE;

//indicates whether we're using symmetric pointer, and 
//thus can directly derefence a phaseless pointer-to-shared
//The flag is currently not used.
BOOL Everything_Local = FALSE;


BOOL Use_Type_Access = FALSE;

SPTR_OFFSET_ACCUMULATION_STACK sptr_off_accumulation_stack;
SPTR_ACCUMULATION_STATE sptr_accumulation_state = NO_ACCUMULATION;;

std::stack<CONSISTENCY_class> consistency_stack;

//memory pool for UPC optimizations
//The pool is used to facilitate communication between the optimizer (producer)
//and the backend lowering phase (consumer)
//It is also used to back up the memory storage for the following maps 
MEM_POOL upc_mem_pool;

//see upc_wn_util.h for their meanings
WN_MAP upc_comm_map = WN_MAP_UNDEFINED;
WN_MAP upc_nbi_map = WN_MAP_UNDEFINED;


//create a new 1d array type, given the size and the element type
//This is useful for creating stack temporary for vectorization and coalescing  
TY_IDX Get_Array_Type(TY_IDX ety, UINT64 size) {

  TY_IDX ar_idx;
  TY& ar_ty = New_TY(ar_idx);
  UINT64 num_elt = size / Adjusted_Type_Size(ety);
  TY_Init(ar_ty, size, KIND_ARRAY, MTYPE_M, Save_Str("tmp_ar"));
  Set_TY_etype(ar_idx, ety);
  Set_TY_align(ar_idx, TY_align(ety));
  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, 0, 0);
  Set_TY_arb (ar_ty, arb);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride (arb);
  Set_ARB_stride_val (arb, num_elt);
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_ubnd_val (arb, num_elt-1);
  return ar_idx;
}


//
//  This handles the case where a field access is convered into pointer arithmetic
//  in the front end (or the optimizer). and the offset for the field needs to be adjusted.
//  This usually happens when the accesed field is itself an array, 
//  or when reading a field from an array of structs
//
void Adjust_Consts_For_Ptr_Arithmetic(WN *ptr, WN*off) 
{

  //ptr holds the base
  // add( TAS(ILOAD(FIELD)) , OFF) is a field access and the offset needs adjustment
  // add ( ILOAD(FIELD), OFF)  means is pointer arithmetic on the fiels itself
  // and no adjustment is needed
  

  TY_IDX idx_ptr;
  BOOL adjust = FALSE;

  if(WN_operator(ptr) == OPR_TAS && 
     (WN_operator(WN_kid0(ptr)) == OPR_TAS || OPERATOR_is_load(WN_operator(WN_kid0(ptr)))) ) {
    //We should adjust the offset in this case only when the type is 
    //([shared] char *), since only int this case is the offset actually being used
    //as an index into the struct
    TY_IDX ty1, ty2;
    ty1 = WN_ty(ptr);
    if(TY_kind(ty1) == KIND_POINTER && TY_size(TY_pointed(ty1)) == 1 
       && TY_block_size(TY_pointed(ty1)) == 0) {
      ptr = WN_kid0(ptr);
      adjust = TRUE;
    } else {
      return;
    }
  }
  
  if(WN_operator(ptr) == OPR_LDA && WN_offset(ptr) && WN_field_id(ptr) == 0)
    idx_ptr = WN_ty(ptr);
  else
    idx_ptr = WN_Get_Ref_TY(ptr);

  if (TY_kind(idx_ptr) != KIND_POINTER) {
    //FIXME: the condition should be an assert, but leave it like this for now
    //so we can test for other bugs
    return;
  }
  
  TY_IDX eidx  = TY_pointed(idx_ptr);
  if(TY_kind(eidx) == KIND_ARRAY) 
    eidx = Get_Inner_Array_Type(eidx);
  if(TY_kind(eidx) == KIND_STRUCT && 
     WN_operator(off) == OPR_INTCONST ||
     //64 bit mode
     (WN_operator(off) == OPR_CVT && WN_operator(WN_kid0(off)) == OPR_MPY  &&
      WN_operator(WN_kid1(WN_kid0(off))) == OPR_INTCONST &&
      WN_const_val(WN_kid1(WN_kid0(off))) == TY_size(eidx)) ||	
     (WN_operator(off) == OPR_MPY && 
      WN_operator(WN_kid1(off)) == OPR_INTCONST &&
      WN_const_val(WN_kid1(off)) == TY_size(eidx))) {

    UINT rem, items;
    UINT offst;
    UINT sz = TY_size(eidx);
    if(WN_operator(off) == OPR_INTCONST) {
      offst = WN_const_val(off);
      //this is the case where you have an array indexed with a constant, 
      //otherwise the front-end should have already adjusted the offset by now so leave 
      // it unchanged
      // 	if(offst >= sz) {
      rem = offst % sz;
      items = offst / sz;
      //if the type is a ptr to a shared struct and the offset is a constant
      // do not adjust
      // see bug 932 
      //However, the front-end changed and for bug932 the code goes through another path
      // and we need to adjust the offset for expressions like this
      // Global->G_root->field, where both Global and G_root are pointers to shared
      
      
      if(Type_Is_Shared_Ptr(idx_ptr) && WN_operator(ptr) != OPR_TAS) {
	if(adjust && ((WN_operator(ptr) == OPR_ILOAD && TY_kind(idx_ptr) == KIND_POINTER)
		      || (WN_operator(ptr) == OPR_LDID && TY_is_shared(idx_ptr) //bug1118
			  ))) {
	  WN_const_val(off) = Adjusted_Type_Size(eidx) *  items + Adjust_Field_Offset(eidx,rem) ; 
	}
      } else { 
	//when --opt is turned on for expressions like 
	// one = 1;
	// p[one] ...
	// we get here with an already adjusted offset
	int asize = Adjusted_Type_Size(eidx);
	if(offst != items*sz + (asize - sz)*items)  
	  WN_const_val(off) = asize *  items + Adjust_Field_Offset(eidx,rem);
      }
    } else {
      if(WN_operator(off) == OPR_CVT) { //64 bit mode
	offst = WN_const_val(WN_kid1(WN_kid0(off)));
	if(offst >= sz) { 
	  rem = offst % sz;
	  items = offst / sz;
	  WN_const_val(WN_kid1(WN_kid0(off))) = Adjusted_Type_Size(eidx);
	}
      } else {
	offst = WN_const_val(WN_kid1(off));
	if(offst >= sz) { 
	  rem = offst % sz;
	  items = offst / sz;
	  WN_const_val(WN_kid1(off)) = Adjusted_Type_Size(eidx) *  items + rem;
	}
      }
    }
  } 

}



static inline TYPE_ID
Widen_Mtype (TYPE_ID t)
{
  if (MTYPE_is_m(t))
    return t;
  if (MTYPE_is_void(t) || t == MTYPE_BS) {
    Fail_FmtAssertion ("Widen_Mtype: for MTYPE_V or MTYPE_BS");
    return t;
  }
  if (MTYPE_byte_size(t) >= 4)
    return t;
  return Mtype_TransferSize(MTYPE_I4, t);
}


INTRINSIC 
WN_Type_To_SyncIntrinsic(TYPE_ID mtype) {
  switch (mtype) {
  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
  case MTYPE_I8:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
  case MTYPE_U8:
  case MTYPE_A4:
  case MTYPE_A8:
    return Use_Valget ? INTRN_WAIT_SYNC_REG : INTRN_WAIT_SYNC_MEM;
    
    //WEI: There's no sync function for float and double, 
    //use the general wait_syncnb instead
  case MTYPE_F4:   
  case MTYPE_F8:
  default:
    return INTRN_WAIT_SYNC_MEM;
  }
}


//Pick the runtime function for a given shared load/store operation, 
//The selection criteria are as the follows:
//
//-- non-blocking memory-to-memory operations (upcr_get_nb_shared, etc.) for load/store
//   identified by the optimizer as split-phase accesses
//-- pshared pointers use the type-based calls (upcr_get_pshared_type, etc.)
//-- blocking memory-to-register calls (upcr_get_shared_val, etc.) if the valget option is 
//   enabled and the load/store value is an integral type that fits in the register
//-- float values either use the different variants of upcr_get_floatval calls 
//-- blocking memory-to-memory calls (upcr_get_shared, etc.) are used otherwise 
//-- strict accesses use the strict versions of the corresponding relaxed calls 

static INTRINSIC 
WN_Type_To_Intrinsic(OPERATOR opr, int mtype, int strict, BOOL phaseless, BOOL is_split) {
  switch(opr) {
  case OPR_LDID:
  case OPR_ILOAD:
  case OPR_MLOAD:
    switch(mtype) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_I8:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_U8:
    case MTYPE_A4:
    case MTYPE_A8:
      if (is_split) {
	return phaseless ? INTRN_GET_NB_P : INTRN_SMLD_NB;
      }
      if (strict) { 
	if (Use_Valget && MTYPE_byte_size(mtype) <= TY_size(upc_hsync_reg_ty)) {
	  return phaseless ? INTRN_GET_P_VALS : INTRN_GET_S_VALS;
	}
	if (Use_Type_Access) {
	  return phaseless ? INTRN_GET_P_TYPE_S : INTRN_SMLDS;
	}  else {
	  return phaseless ? INTRN_GET_PS : INTRN_SMLDS;
	}

      }
      if (Use_Valget && MTYPE_byte_size(mtype) <= TY_size(upc_hsync_reg_ty)) {
	return phaseless ? INTRN_GET_P_VAL : INTRN_GET_S_VAL;
      } 
      if (Use_Type_Access) {
	return phaseless ? INTRN_GET_P_TYPE : INTRN_SMLD; 
      } else {
	return phaseless ? INTRN_GET_P : INTRN_SMLD; 
      }
      
    case MTYPE_F4:
      if (is_split) {
	return phaseless ? INTRN_GET_NB_P : INTRN_SMLD_NB;
      } 
      if (strict) {
	return phaseless ? INTRN_GET_P_FVALS : INTRN_GET_S_FVALS;
      }
      return phaseless ? INTRN_GET_P_FVAL : INTRN_GET_S_FVAL;
    case MTYPE_F8:
      if (is_split) {
	return phaseless ? INTRN_GET_NB_P : INTRN_SMLD_NB;
      } 
      if (strict) {
	return phaseless ? INTRN_GET_P_DVALS : INTRN_GET_S_DVALS;
      }
      return phaseless ? INTRN_GET_P_DVAL : INTRN_GET_S_DVAL;
    case MTYPE_F10:
    case MTYPE_F16:
    case MTYPE_FQ:
    case MTYPE_C4:
    case MTYPE_C8:
    case MTYPE_CQ:
    case MTYPE_BS:
    case MTYPE_M:
      if (is_split) {
	return phaseless ? INTRN_GET_NB_P : INTRN_SMLD_NB;
      }

      if (strict) {
	if (Use_Type_Access) {
	  return phaseless ? INTRN_GET_P_TYPE_S : INTRN_SMLDS;
	}  else {
	  return phaseless ? INTRN_GET_PS : INTRN_SMLDS;
	}
      } 

      if (Use_Type_Access) {
	return phaseless ? INTRN_GET_P_TYPE : INTRN_SMLD; 
      } else {
	return phaseless ? INTRN_GET_P : INTRN_SMLD; 
      }
    default: 
      Is_True(0, ("Wrong mtype in WN_Type_To_Intrinsic",""));
    }
    break;
  case OPR_ISTORE:
  case OPR_STID:
  case OPR_MSTORE:
    switch(mtype) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_I8:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_U8:
    case MTYPE_A4:
    case MTYPE_A8:
      if (is_split) {
	return (phaseless ? INTRN_PUT_NB_P_VAL : INTRN_SIST_NB);
      } else {
	if (phaseless) {
	  if (Use_Type_Access) {
	    return strict? INTRN_PUT_P_TYPE_S : INTRN_PUT_P_TYPE;
	  } else {
	    return strict ? INTRN_PUT_P_VALS : INTRN_PUT_P_VAL;
	  }
	} else {
	  return strict ? INTRN_SISTS : INTRN_SIST;
	}
      }
    case MTYPE_F4:
      if (is_split) {
	return (phaseless ? INTRN_PUT_NB_P_VAL : INTRN_SIST_NB);
      } else {
	return strict ? (phaseless ? INTRN_PUT_P_FVALS : INTRN_PUT_S_FVALS) :
	  (phaseless ? INTRN_PUT_P_FVAL : INTRN_PUT_S_FVAL); 
      }
    case MTYPE_F8:
      if (is_split) {
	return (phaseless ? INTRN_PUT_NB_P_VAL : INTRN_SIST_NB);
      } else {
	return strict ? (phaseless ? INTRN_PUT_P_DVALS : INTRN_PUT_S_DVALS) :
	  (phaseless ? INTRN_PUT_P_DVAL : INTRN_PUT_S_DVAL); 
      }
    case MTYPE_F10:
    case MTYPE_F16:
    case MTYPE_FQ:
    case MTYPE_C4:
    case MTYPE_C8:
    case MTYPE_CQ:
    case MTYPE_BS:
    case MTYPE_M:
      if (is_split) {
	return phaseless ? INTRN_PUT_NB_P : INTRN_SMST_NB;
      } else {
	/*
	if (phaseless) {
	  if (Use_Type_Access) {
	    return strict? INTRN_PUT_P_TYPE_S : INTRN_PUT_P_TYPE;
	  } else {
	    return strict ? INTRN_PUT_P_VALS : INTRN_PUT_P_VAL;
	  }
	} else {
	  return strict ? INTRN_SISTS : INTRN_SIST;
	}
	*/
	return strict ? (phaseless ? INTRN_PUT_PS : INTRN_SMSTS) : 
	  (phaseless ? INTRN_PUT_P : INTRN_SMST);
      }
    default :
      Is_True(0, ("Wrong mtype in WN_Type_To_Intrinsic",""));
    }
    break;
  default:
    Is_True(0, ("Operator not supported in WFE_Type_To_Intrinsic",""));
  }
}


// For opr == OPR_EQ, bsize and idx1 are either shared_ptr_idx or pshared 
INTRINSIC
WN_Operator_To_Intrinsic (OPERATOR opr, INT bsize  = 0, INT idx1 = 0, INT esize = -1)
{  
  switch (opr) {
  case OPR_ADD:
  case OPR_SUB:
    switch(bsize) {
    case 0:
      if(esize == 0)
	 return INTRN_SPTRADD;
      else 
	return INTRN_ADD_PI;
    case 1:
      if(esize == 0)
	 return INTRN_SPTRADD;
      else
	return INTRN_ADD_P1;
    default:
      return INTRN_SPTRADD;
    }
    break;
  case OPR_EQ:
    if (idx1 == shared_ptr_idx) {
      return (bsize == shared_ptr_idx) ? INTRN_EQ_S_S :
	(bsize == pshared_ptr_idx) ? INTRN_EQ_S_P : INTRN_ISNULL_S;
    } else if (idx1 == pshared_ptr_idx) 
      return (bsize == shared_ptr_idx) ? INTRN_EQ_S_P :
	(bsize == pshared_ptr_idx) ? INTRN_EQ_P_P : INTRN_ISNULL_P;
    else 
      return bsize == shared_ptr_idx ? INTRN_ISNULL_S : INTRN_ISNULL_P; 
    break;
  default:
    Is_True(0, ("",""));
  }

 
  
  return INTRN_SPTRADD;
}



static WN* 
Spill_And_Take_Address(WN *wn, BOOL spill = FALSE, TY_IDX ty_idx = 0) {

  WN *result;
  TY_IDX spill_ty;

  if(WN_operator(wn) == OPR_LDA || ((WN_operator(wn) == OPR_ADD || 
     WN_operator(wn) == OPR_MPY || WN_operator(wn) == OPR_SUB) && !spill) )
    return wn;
  else  if(WN_operator(wn) == OPR_TAS) {
    wn = Strip_TAS(wn);
  }

  switch(WN_operator(wn)) {
  case OPR_ILOAD:
    spill_ty = WN_ty(wn);
    if(WN_field_id(wn) && Type_Is_Shared_Ptr(spill_ty))
      spill_ty = TY_To_Sptr_Idx(spill_ty);
    break;
  case OPR_ARRAY:
    result = WN_kid0(wn);
    if(WN_operator(result) == OPR_LDID)
      spill_ty = Get_Inner_Array_Type(WN_ty(result));
    else 
      if(WN_operator(result) == OPR_LDA)
	spill_ty = Get_Inner_Array_Type(TY_pointed(WN_ty(result)));
   
    break;
  case OPR_MLOAD:
	spill_ty = TY_pointed(WN_ty(wn));
	break;
  default:
    if(ty_idx == 0)
      spill_ty =  MTYPE_To_TY(WN_rtype(wn));
    else spill_ty = ty_idx;
    break;
  }

  TYPE_ID desc = TY_mtype(spill_ty);
  result = WN_CreateBlock (); 
  ST *handle_st = Gen_Temp_Symbol(spill_ty, (char*) ".spillstoreparm");
  WN_INSERT_BlockLast (result, WN_Stid (desc, 0, handle_st, spill_ty, wn));
  result  = WN_CreateComma (OPR_COMMA, Pointer_Mtype, MTYPE_V, result, 
			    WN_Lda(Pointer_Mtype, 0, handle_st, 0));
  return result;
}

static
TYPE_ID Shared_Load_Extend_Mtyp (TYPE_ID typ) 
{
  BOOL _64_bit_target = TY_size(MTYPE_To_TY(Pointer_type)) > 4;
  switch (typ) {
  case MTYPE_B:
  case MTYPE_I1:
  case MTYPE_I2:       
  case MTYPE_I4: 
  case MTYPE_U1:        
  case MTYPE_U2:        
  case MTYPE_U4: 
    if(_64_bit_target)
      return MTYPE_I8;
    else
      return MTYPE_I4;
  case MTYPE_I8:   
  case MTYPE_U8:
    if(_64_bit_target)
      return MTYPE_I8;
    else
      Is_True(0,("",""));
    
  default:
    return typ;
  }

}

//this shouldn't really be necessary if the pragmas are inserted properly,
//but just in case...
void Clear_Consistency_Info() {

  while (!consistency_stack.empty()) {
    consistency_stack.pop();
  }
}

void Enter_Consistency_Info(mUINT16 pragma_id) {

  switch(pragma_id) {
  case WN_PRAGMA_UPC_STRICT_CONSISTENCY_START:
    consistency_stack.push(STRICT_CONSISTENCY);
    break;
  case WN_PRAGMA_UPC_RELAXED_CONSISTENCY_START:
    consistency_stack.push(RELAXED_CONSISTENCY);
    break;
  case WN_PRAGMA_UPC_STRICT_CONSISTENCY_STOP:
  case WN_PRAGMA_UPC_RELAXED_CONSISTENCY_STOP:
    consistency_stack.pop();
  }
}

CONSISTENCY_class 
Get_Access_Consistency (TY_IDX idx) {
  
  if (idx == 0)
    return consistency_stack.top();
  
  switch(TY_kind(idx)) {
  case KIND_POINTER:
    idx = TY_pointed(idx);
    break;
  case KIND_ARRAY:
    idx = Get_Inner_Array_Type(idx);
    break;
  }
  
  return  TY_is_strict (idx) ? STRICT_CONSISTENCY : 
    (TY_is_relaxed(idx) ? RELAXED_CONSISTENCY : consistency_stack.top());
}

//a WN that represents a C type name ("int", "struct foo", etc.), but no array types
//Since type is not a first class member of C, we wrap it as an intrinsic that is not
//lowered in the backend and handled specially by whirl2c.  The actual type is stored 
//as the type of the only PARM node
static WN* WN_Create_Type_Expr(TY_IDX type) {

  WN *kid[1];
  kid[0] = WN_CreateParm(TY_mtype(type), WN_Intconst(Integer_type, 0), type, WN_PARM_BY_VALUE);
  WN* type_intrn = WN_Create_Intrinsic(OPR_INTRINSIC_OP, Integer_type, MTYPE_V, INTRN_TYPE_EXPR, 1, kid);
  return type_intrn;
}


/**
 *
 * When WN_Create_Shared_Store(), there are cases when the value to be stored 
 * is a preg that can't be (easily) spilled: e.g., the preg may be created by 
 * the optimizer during PRE.  Since their address can't be taken, we have 
 * to use the register-based versions of the runtime calls instead.
 */
static WN* 
WN_Create_Val_Store(WN* lhs, WN* rhs, WN* offst, CONSISTENCY_class consistency) {

  INTRINSIC fn_name = WN_Type_To_Intrinsic(OPR_STID, WN_rtype(rhs),
					   consistency == STRICT_CONSISTENCY, WN_ty(lhs) == pshared_ptr_idx, false);

  int num_args = WN_rtype(rhs) == MTYPE_F4 || WN_rtype(rhs) == MTYPE_F8 ? 3 : 4;

  WN* call_wn = WN_Create (OPR_INTRINSIC_CALL, WN_rtype(rhs), MTYPE_V, num_args);
  WN_intrinsic(call_wn) = fn_name;
  WN_kid0(call_wn) = lhs;
  WN_kid1(call_wn) = WN_CreateParm(Integer_type, offst,
				   MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid2(call_wn) = WN_CreateParm(WN_rtype(rhs), rhs, WN_ty(rhs), WN_PARM_BY_VALUE);
  if (num_args == 4) {
    //case for put_shared_val
    WN_kid3(call_wn) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, MTYPE_byte_size(WN_rtype(rhs))), 
				     MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  }
  
  //fprintf(stderr, "the valget function: \n");
  //fdump_tree(stderr, call_wn);
  return call_wn;

}

  

WN*
WN_Create_Shared_Store (WN *st, BOOL src_is_shared, WN_OFFSET xtra_offst,
			BOOL has_offt, WN *offt, BOOL spill) {
  
  ST *op_st = 0, *handle_st; 
  WN *arg0, *call_wn;
  WN *wn0, *wn1, *init_wn = 0, *handle_wn;
  TYPE_ID rtype, desc;
  WN_OFFSET offset = 0;
  CONSISTENCY_class consistency = STRICT_CONSISTENCY;
  TY_IDX handle_ty_idx;
  TY_IDX actual_shared_ptr_idx;
  rtype = ((WN_desc(st) == MTYPE_V) ? MTYPE_M : WN_desc(st)) ;
  int strictw;
  int transfer_size = 0;
  WN *spilled_preg_ldid = 0;
  TY_IDX ref_ty = 0;
  TY_IDX pt_idx;

  BOOL _64_bit_target = TY_size(MTYPE_To_TY(Pointer_type)) > 4;
  static TY_IDX void_ptr_idx = 0;
  if(void_ptr_idx == 0) {
    void_ptr_idx = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  }

  sync_handle_t* handle = (sync_handle_t*) WN_MAP_Get(upc_comm_map, st);
  bool is_split = handle != NULL;
 
  switch (WN_operator(st)) {
  case OPR_STID:
    op_st = WN_st(st);
    ref_ty = WN_ty(st);
    consistency = Get_Access_Consistency(WN_ty(st));
    if(TY_kind(WN_ty(st)) == KIND_POINTER && TY_is_shared(WN_ty(st))) {
      if(Type_Is_Shared_Ptr(TY_pointed(WN_ty(st)))) {
	 rtype = TY_mtype(TY_To_Sptr_Idx(TY_pointed(WN_ty(st))));
	 transfer_size = TY_size(TY_To_Sptr_Idx(TY_pointed(WN_ty(st))));
      } else 
	 transfer_size = TY_size(WN_ty(st));
    } else if (TY_kind(WN_ty(st)) == KIND_STRUCT && WN_field_id(st)) {
      TY_IDX f_idx = Get_Field_Type(WN_ty(st), WN_field_id(st));
      ref_ty = f_idx;
      if(Type_Is_Shared_Ptr(f_idx, TRUE)) {
	rtype = TY_mtype(TY_To_Sptr_Idx(f_idx));
	transfer_size = TY_size(TY_To_Sptr_Idx(f_idx));
      } else { 
	rtype = TY_mtype(f_idx);
	transfer_size = TY_size(f_idx);
      }
    }
    break;
  case OPR_ISTORE:
  case OPR_MSTORE: {
    arg0 = WN_kid1(st);
    pt_idx = TY_pointed(WN_ty(st));
    ref_ty = pt_idx;
    consistency = Get_Access_Consistency(pt_idx);
    if(Type_Is_Shared_Ptr(pt_idx)) {
      if(Type_Is_Shared_Ptr(pt_idx, TRUE)) {
	//ptr to shared - need to adjust size to sizeof(shared_ptr)
	rtype = TY_mtype(TY_To_Sptr_Idx(TY_pointed(WN_ty(st))));
	transfer_size = TY_size(TY_To_Sptr_Idx(TY_pointed(WN_ty(st))));
      } else if(TY_kind(pt_idx) == KIND_STRUCT) {
	if(WN_field_id(st)) {
	  pt_idx = Get_Field_Type(pt_idx, WN_field_id(st));
	  if(Type_Is_Shared_Ptr(pt_idx, TRUE)) {
	    rtype = TY_mtype(TY_To_Sptr_Idx(pt_idx));
	    transfer_size = TY_size(TY_To_Sptr_Idx(pt_idx));
	  }  else {
	    rtype = TY_mtype(pt_idx);
	    transfer_size = TY_size(pt_idx);
	  }
	} else {
	  rtype = MTYPE_M;
	  transfer_size = TY_size(pt_idx);
	}
      } else if(TY_kind(pt_idx) == KIND_ARRAY) {
	if(WN_field_id(st)) {
	  pt_idx = Get_Field_Type(TY_etype(pt_idx), WN_field_id(st));
	  if(Type_Is_Shared_Ptr(pt_idx, TRUE)) {
	    rtype = TY_mtype(TY_To_Sptr_Idx(pt_idx));
	    transfer_size = TY_size(TY_To_Sptr_Idx(pt_idx));
	  }  else {
	    rtype = TY_mtype(pt_idx);
	    transfer_size = TY_size(pt_idx);
	  }
	}
      }
    }
    ref_ty = pt_idx;
    arg0 = Strip_TAS(arg0);
    break;
  }
  default:
    Is_True(0,("",""));
  }

  actual_shared_ptr_idx = TY_To_Sptr_Idx(WN_ty(st));

  if(!_64_bit_target && (rtype == MTYPE_I8 ||  rtype == MTYPE_A8 || rtype == MTYPE_U8) ) {
    transfer_size = TY_size(MTYPE_To_TY(rtype));
    rtype = MTYPE_M;
  } else if(consistency == RELAXED_CONSISTENCY && (rtype == MTYPE_F4 || rtype == MTYPE_F8)) {
    transfer_size = TY_size(MTYPE_To_TY(rtype));
    rtype = MTYPE_M;
  }
  
  handle_ty_idx = upc_hsync_mem_ty;
  //always use blocking calls, except when it's optimized.
  desc = is_split ? TY_mtype(handle_ty_idx) : MTYPE_V;
  offset = xtra_offst;
  // shared scalar of field of shared struct
  if (op_st) {
    if(WN_field_id(st) >1) 
      offset += Adjust_Field_Offset(ST_type(op_st), WN_field_id(st),0);
    else
      offset += WN_offset(st);

    arg0 = WN_CreateParm(TY_mtype(actual_shared_ptr_idx), 
                         WN_CreateLdid(OPR_LDID, TY_mtype(actual_shared_ptr_idx), 
				       TY_mtype(actual_shared_ptr_idx),
				       0 /*offset*/, ST_st_idx(*op_st),actual_shared_ptr_idx, 0), 
			 actual_shared_ptr_idx, WN_PARM_BY_VALUE);
			 
  } else {
    arg0 = WN_CreateParm(TY_mtype(actual_shared_ptr_idx), arg0, actual_shared_ptr_idx, 
			 WN_PARM_BY_VALUE);
  }

  bool phaseless = actual_shared_ptr_idx == pshared_ptr_idx;

  //All put calls take 4 args, except for put_fval and put_dval
  call_wn = WN_Create(OPR_INTRINSIC_CALL, desc, desc, (!is_split && MTYPE_is_float(rtype)) ? 3 : 4);
  WN_intrinsic(call_wn) = WN_Type_To_Intrinsic(OPR_STID, 
					       src_is_shared ? MTYPE_M : rtype, 
					       consistency == STRICT_CONSISTENCY, 
					       phaseless,
					       is_split);

  {
    INT32 is_nbi = WN_MAP32_Get(upc_nbi_map, st); 
    if (is_nbi == 1) {
      if (actual_shared_ptr_idx == pshared_ptr_idx) {
	WN_intrinsic(call_wn) = INTRN_PUT_NBI_P;
      } else {
	WN_intrinsic(call_wn) = INTRN_PUT_NBI_S;
      }
    }
  }

  WN_Set_Linenum(call_wn, upc_srcpos);
  if (src_is_shared) {
    WN_intrinsic(call_wn) = INTRN_UPC_MEMCPY;
    WN_set_kid_count(call_wn, 3);
  }

  WN_kid0 (call_wn) = arg0;

  if (!src_is_shared)
    WN_kid1(call_wn) = WN_CreateParm(Integer_type, 
				     has_offt ? offt : 
				     WN_Intconst(Integer_type, offset),
				     MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  else  
    WN_kid1(call_wn) = WN_CreateParm(TY_mtype(actual_shared_ptr_idx), Strip_TAS(WN_kid0(st)),
				     actual_shared_ptr_idx, WN_PARM_BY_VALUE);

  if (src_is_shared) {
    WN_kid2(call_wn) = WN_CreateParm(Integer_type,  
				     (WN_operator(st) == OPR_MSTORE || WN_operator(st) == OPR_ISTORE ) ? 
				     WN_COPY_Tree(WN_kid2(st)) : 
				     WN_Intconst(Integer_type,
						 TY_size(actual_shared_ptr_idx)),
				     MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  } else {
    WN *value = WN_kid0(st);
    TY_IDX parm_ty_idx;
   

    if (WN_operator(value) == OPR_TAS &&
	(WN_operator(WN_kid0(value)) == OPR_INTCONST || 
	 WN_operator(WN_kid0(value)) == OPR_CONST)) {
      //this is probably not needed anymore, but leave here to avoid regressions
      parm_ty_idx =  MTYPE_To_TY(WN_rtype(WN_kid0(value)));
    } else {
      parm_ty_idx = WN_Get_Ref_TY(value);
    }
    
    if (parm_ty_idx == 0)
      parm_ty_idx =  MTYPE_To_TY(WN_rtype(value));

    /*
    if(OPERATOR_is_load(WN_operator(value)) || WN_operator(value) == OPR_TAS) {
      if(WN_operator(value) == OPR_TAS && 
	 (WN_operator(WN_kid0(value)) == OPR_INTCONST || 
	  WN_operator(WN_kid0(value)) == OPR_CONST))
	parm_ty_idx =  MTYPE_To_TY(WN_rtype(WN_kid0(value)));
      else 
	parm_ty_idx = WN_Get_Ref_TY(value);
    } else {
      parm_ty_idx = WN_Get_Ref_TY(value);
      if (parm_ty_idx == 0)
	parm_ty_idx =  MTYPE_To_TY(WN_rtype(value));
    }
    */
   
    if (TY_is_shared(parm_ty_idx)) {
      //the loaded value should have private type
      parm_ty_idx = Shared_To_Private_Type(parm_ty_idx);
    }
   
    if(TY_kind(parm_ty_idx) != KIND_SCALAR && Type_Is_Shared_Ptr(parm_ty_idx))
      transfer_size = TY_size(TY_To_Sptr_Idx(parm_ty_idx));
    value = Strip_TAS(value);
    if (WN_rtype(value) != MTYPE_M && rtype != MTYPE_M) {
      WN_kid2 (call_wn) = WN_CreateParm(Mtype_comparison(WN_rtype(value)), value,
					parm_ty_idx, WN_PARM_BY_VALUE);
    }
    else { // need to extract the address of the symbol accessed by value
      if (WN_operator(value) == OPR_LDID && WN_st_idx(value) != 0) {
	ST *tst = 0;
	//need to check here for the special case of ldid of return_preg
	//can't take it's address - spill
	if(ST_class(WN_st(value)) == CLASS_PREG || 
	   (Use_Valget && transfer_size <= TY_size(upc_hsync_reg_ty))) {
	  return WN_Create_Val_Store(arg0, value, has_offt ? offt : WN_Intconst(Integer_type, offset), consistency);
	} else 
	  tst = WN_st(value);
	
 
	if(TY_size(ST_type(tst)) < transfer_size && TY_kind(ref_ty) == KIND_SCALAR ) {
	  wn0 = Spill_And_Take_Address(value, true, Shared_To_Private_Type(ref_ty));
	} else { 
	  wn0 = WN_Lda(Pointer_Mtype, WN_offset(value)/*0*/, tst, WN_field_id(value)/*0*/);
	}
	
	WN_kid2(call_wn) = WN_CreateParm(Pointer_Mtype, wn0, void_ptr_idx, WN_PARM_BY_VALUE);
      } else if(WN_operator(value) == OPR_COMMA && 
		WN_operator(WN_kid1(value)) == OPR_LDID){
	WN *kid1 = WN_kid1(value);
	WN *kid0 = WN_kid0(value);
	Is_True(WN_st(kid1), ("",""));
	
	value = WN_CreateComma (OPR_COMMA, Pointer_Mtype, MTYPE_V,
				kid0, WN_Lda(Pointer_Mtype, 0, WN_st(kid1), 0));
	WN_kid2(call_wn) = WN_CreateParm(Pointer_Mtype, value,
					 void_ptr_idx,
					 WN_PARM_BY_VALUE);
      }  else if (WN_operator(value) == OPR_INTCONST) {
	TYPE_ID desc = WN_rtype(value);

	//NULL ptr assignment 
	if (TY_kind(WN_Get_Ref_TY(st)) == KIND_POINTER 
	    || (TY_kind(WN_ty(st)) == KIND_STRUCT && Type_Is_Shared_Ptr(ref_ty, TRUE))) {
	  WN_kid2(call_wn) = WN_CreateParm(Pointer_Mtype, 
                                           WN_Lda(Pointer_Mtype, 0, 
						  TY_is_pshared(TY_pointed(ref_ty)) ? 
						  pshared_null : shared_null, 
						  0),
                                           void_ptr_idx, WN_PARM_BY_VALUE);
	 
	} else {  
	  //this is for constants assigned to 64 bit int values on 32 bit targets
	  //spill the const into a variable and pass the address into the assignment.
	  
	  init_wn = WN_CreateBlock ();
	  WN_Set_Linenum(init_wn, upc_srcpos);
	  if(MTYPE_is_signed(desc))
	    desc = MTYPE_I8;
	  else 
	    desc = MTYPE_U8;
	  TY_IDX spill_ty = TY_is_shared(ref_ty) ? Shared_To_Private_Type(ref_ty) : ref_ty;
	  handle_st = Gen_Temp_Symbol(spill_ty, (char*) ".spill64ct");
	  WN_INSERT_BlockLast (init_wn, WN_Stid (desc, 0, handle_st, spill_ty, value));
	  WN_kid2(call_wn) = WN_CreateParm(Pointer_Mtype, WN_Lda(Pointer_Mtype, 0, handle_st, 0),
					   void_ptr_idx, WN_PARM_BY_VALUE);
	}
      } else {
	if(WN_operator(WN_kid0(st)) == OPR_ILOAD || WN_operator(WN_kid0(st)) == OPR_MLOAD)
	  spill = 0;
	WN_kid2(call_wn) = WN_CreateParm(Pointer_Mtype, 
					 Spill_And_Take_Address(value,spill, MTYPE_To_TY(TY_mtype(ref_ty))), 
					 void_ptr_idx, WN_PARM_BY_VALUE);
      }
    }
  }

  if (WN_kid_count(call_wn) == 4) {
    if (Use_Type_Access && phaseless && MTYPE_is_integral(rtype)) {
      //fourth argument is the local type of the store
      //3rd arg is a value
      WN_kid3(call_wn) = WN_Create_Type_Expr(TY_is_shared(ref_ty) ? Shared_To_Private_Type(ref_ty) : ref_ty);
    } else {
      Is_True((rtype == MTYPE_M && transfer_size != 0) || rtype != MTYPE_M, ("Illegal transfer size for remote store",""));
      WN_kid3(call_wn) = WN_CreateParm(Integer_type, 
				       (WN_operator(st) == OPR_MSTORE) ?  
				       WN_Intconst(Integer_type, TY_adjusted_size(pt_idx)):
				       WN_Intconst(Integer_type, 
						   (rtype == MTYPE_M) ? transfer_size : TY_size(MTYPE_To_TY(rtype))),
				       MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
    }
  }  

  if (desc != MTYPE_V) {
    TYPE_ID hdesc = TY_mtype(handle_ty_idx);
    handle_st = Gen_Temp_Symbol(handle_ty_idx, (char*) ".Msync");

    wn1 = WN_Ldid (hdesc, -1, Return_Val_Preg, handle_ty_idx); 
    if(!init_wn)
      init_wn = WN_CreateBlock (); 
    WN_INSERT_BlockLast (init_wn, call_wn);
    init_wn = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V, init_wn, wn1);
    wn1 = WN_CreateBlock();
    WN_INSERT_BlockLast (wn1, WN_Stid (hdesc, 0, handle_st, handle_ty_idx, init_wn));
    //at this point wn1 is the nonblocking put call

    if (handle != NULL) {
      //The sync call is to be generated elsewhere
      handle->st = handle_st;
      return wn1;
    }
  } else if (init_wn) {
    //this should happen only for strict puts with long long cts as srcs
    WN_INSERT_BlockLast (init_wn, call_wn);
    call_wn = init_wn;
  }
  WN_Set_Linenum(call_wn, upc_srcpos);
  return call_wn;

}

//Costin -- this function is simply too messy, please consider documenting
//at least the input and output conditions (e.g., what kind of type can dest have?
//when is xtra_offst passed to this function)...
WN*
WN_Create_Shared_Load( WN *ld, 
		       WN *dest, 
		       WN_OFFSET xtra_offst, 
		       TY_IDX  access_ty, 
		       BOOL has_off, WN *off_wn)
{
  WN *call_wn;
  WN *src, *dst, *offt, *size;

  WN *init_wn = 0, *wn1 = 0, *wn2 = 0;
  WN *handle_wn = 0, *wn0 = 0;
  INTRINSIC iop;
  TYPE_ID hdesc, rtype, desc;
  CONSISTENCY_class consistency;
  TY_IDX handle_ty_idx = 0;
  WN *ldc;
  TY_IDX ret_ty; 
  TY_IDX sptr_idx = 0;
  WN_OFFSET offset = 0;
  int asize = 0;
  BOOL _64_bit_target = TY_size(MTYPE_To_TY(Pointer_type)) > 4;
  static TY_IDX void_ptr_idx = 0;

  if(void_ptr_idx == 0) {
    void_ptr_idx = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  }

  sync_handle_t * handle = (sync_handle_t*) WN_MAP_Get(upc_comm_map, ld); 
  bool is_split = (handle != NULL);

  consistency = Get_Access_Consistency(WN_ty(ld));
  rtype = WN_rtype(ld);
  desc = WN_desc(ld);
  
  if(WN_operator(ld) == OPR_TAS)
    ld = Strip_TAS(ld);

  switch (WN_operator(ld)) {
  case OPR_LDID:
    ldc = WN_COPY_Tree(ld);
    offset = WN_field_id(ld) > 1 ? Adjust_Field_Offset(WN_ty(ld), WN_field_id(ld), 0) : WN_offset(ld) + xtra_offst;
    
    // modify on the fly the type of the argument, otherwise
    // can't create parm node
    // have to set it back at the end
    sptr_idx = TY_To_Sptr_Idx(WN_ty(ld));
    WN_set_ty(ldc, sptr_idx);
    WN_set_rtype(ldc, TY_mtype(sptr_idx));
    WN_set_desc (ldc, TY_mtype(sptr_idx));
    WN_set_field_id(ldc, 0);
    WN_offset(ldc) = 0;
    ret_ty = WN_object_ty(ld);
    //shared ptr to shared
    if ((TY_kind(WN_ty(ld)) == KIND_POINTER || 
	 TY_kind(WN_ty(ld)) == KIND_ARRAY )   && TY_is_shared(WN_ty(ld))) 
      {
	TY_IDX tmp_idx = WN_ty(ld);
	if(WN_field_id(ld)) 
	  tmp_idx = Get_Field_Type(tmp_idx, WN_field_id(ld)); 
	if(TY_kind(tmp_idx) == KIND_POINTER) {
	  if (Type_Is_Shared_Ptr(TY_pointed(tmp_idx))) {
	    tmp_idx = TY_pointed(tmp_idx);
	    rtype = desc = TY_mtype(TY_To_Sptr_Idx(tmp_idx));
	    ret_ty = TY_To_Sptr_Idx(tmp_idx);
	  } else {
	    ret_ty = Make_Pointer_Type(TY_pointed(tmp_idx));
	    rtype = desc = TY_mtype(ret_ty);
	  }
	} else if(TY_kind(tmp_idx) == KIND_ARRAY) {
	  Is_True(0,("",""));
	}    

      } else if (Type_Is_Shared_Ptr(ret_ty, TRUE)) {
	ret_ty = TY_To_Sptr_Idx(ret_ty);
	rtype = desc = TY_mtype(ret_ty);
      }
    break;
    
  case OPR_MLOAD:
  case OPR_ILOAD:
    ldc  = WN_COPY_Tree(WN_kid0(ld));
    offset = WN_offset(ld) + xtra_offst;
    ret_ty = WN_object_ty(ld);
    sptr_idx = TY_To_Sptr_Idx(WN_ty(ld));

    if (WN_operator(ld) == OPR_ILOAD && WN_field_id(ld) != 0) {
      //when loading a field from a shared struct,
      //the bsize of the iload type is set to 0, while we want the 
      //blocksize of the struct itself
      sptr_idx = TY_To_Sptr_Idx(WN_load_addr_ty(ld));
    }

    if((TY_is_shared(WN_ty(ld)) &&   
	(TY_kind(WN_ty(ld)) == KIND_POINTER ||TY_kind(WN_ty(ld)) == KIND_ARRAY))
       ) {
      TY_IDX tmp_idx = ret_ty;
      if(TY_kind(tmp_idx) == KIND_POINTER) {
	  if (Type_Is_Shared_Ptr(TY_pointed(tmp_idx))) {
	    tmp_idx = TY_pointed(tmp_idx);
	    rtype = desc = TY_mtype(TY_To_Sptr_Idx(tmp_idx));
	    ret_ty = TY_To_Sptr_Idx(tmp_idx);
	  } else {
	    ret_ty = Make_Pointer_Type(TY_pointed(tmp_idx));
	    rtype = desc = TY_mtype(ret_ty);
	  }
	} else if(TY_kind(tmp_idx) == KIND_ARRAY) {
	  Is_True(0,("",""));
	}    
    } if(Type_Is_Shared_Ptr(ret_ty, TRUE)) {
      if(TY_kind(WN_ty(ld)) == KIND_POINTER &&  !TY_is_shared(WN_ty(ld)) && 
	 TY_kind(TY_pointed(WN_ty(ld))) == KIND_ARRAY && WN_operator(ld) == OPR_MLOAD) {
	ret_ty = Shared_To_Private_Type(Get_Inner_Array_Type(TY_pointed(WN_ty(ld))));
      } else {
	ret_ty = TY_To_Sptr_Idx(ret_ty);
      }
      rtype = desc = TY_mtype(ret_ty);
    }     

    break;
    
  case OPR_COMMA:
    Is_True(WN_rtype(ld) == TY_mtype(shared_ptr_idx) || WN_rtype(ld) == TY_mtype(pshared_ptr_idx),
	    ("Bad rtype for shared ld src",""));
    ldc = WN_COPY_Tree(ld);
    offset += xtra_offst;
    ret_ty = access_ty;
    sptr_idx = TY_To_Sptr_Idx(WN_ty(WN_kid1(ld)));
    break;
  default:
    Is_True(0,("Not implemented yet",""));
  }

  bool phaseless = sptr_idx == pshared_ptr_idx;

  iop =  WN_Type_To_Intrinsic(OPR_LDID, dest ? MTYPE_M : rtype, 
			      consistency == STRICT_CONSISTENCY, phaseless, is_split);

  if(!access_ty && dest) {
    //bug 411
    if(TY_kind(TY_pointed(WN_ty(dest))) == KIND_STRUCT) {
      asize = TY_adjusted_size(TY_pointed(WN_ty(dest)));
    } else 
      asize = TY_size(TY_pointed(WN_ty(dest)));
  }
  //bug 1897 - For  struct A a  = *p; with p shared, the split phase optimization generates
  //           code that is not lowered (we should figure out why). 
  //           Meanwhile, YET another exception

  if(access_ty == 0 && dest == 0 && WN_operator(ld) == OPR_ILOAD && TY_kind(ret_ty) == KIND_STRUCT)
    size = WN_CreateParm(Integer_type, (WN_operator(ld)) == OPR_MLOAD ? WN_COPY_Tree(WN_kid1(ld)) :
			 WN_Intconst(Integer_type, TY_adjusted_size(ret_ty)),
			 MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  else //end fix for 1897
    size = WN_CreateParm(Integer_type, (WN_operator(ld)) == OPR_MLOAD ? WN_COPY_Tree(WN_kid1(ld)) :
			 WN_Intconst(Integer_type, 
				     dest ? (access_ty ? TY_adjusted_size(access_ty) : asize) : 
				     TY_size(ret_ty)),
			 MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);

  src = WN_CreateParm(TY_mtype(sptr_idx), Strip_TAS(ldc),
		      sptr_idx, WN_PARM_BY_VALUE);

  if(has_off) 
    offt = WN_CreateParm(Integer_type, off_wn,  MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  else
    offt = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, offset),
			 MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);

  if(TY_kind(ret_ty) == KIND_SCALAR && ret_ty != pshared_ptr_idx && ret_ty != shared_ptr_idx){
    if (TY_is_shared(ret_ty)) {
      ret_ty = Shared_To_Private_Type(ret_ty);
    }
  } 

  WN* result;
  ST* ret_st;
  switch(iop) {
  case INTRN_GET_NB_P: 
  case INTRN_SMLD_NB:
    //nonblocking calls
    hdesc = TY_mtype(upc_hsync_mem_ty);
    handle_ty_idx  = upc_hsync_mem_ty;
    call_wn = WN_Create(OPR_INTRINSIC_CALL, hdesc, MTYPE_V, 4);
    //We wish to store to the temp var (UPC_CSE_N) directly, to save a temp var 
    //Print_ST(stderr, handle->st, true);
    dest = WN_Lda(Pointer_Mtype, 0, handle->st, 0);
    WN_kid0(call_wn) = WN_CreateParm(Pointer_Mtype, dest, void_ptr_idx, WN_PARM_BY_VALUE);
    WN_kid1(call_wn) = src;
    WN_kid2(call_wn) = offt;
    WN_kid3(call_wn) = size;
    result = WN_CreateBlock();
    WN_INSERT_BlockLast(result, call_wn);
    result = WN_CreateComma (OPR_COMMA, hdesc, MTYPE_V, result,
			     WN_Ldid (hdesc, -1, Return_Val_Preg, handle_ty_idx));
    //Lhs (the sync handle) is generated in wn_lower.cxx, no ret_st needed
    break;
  case INTRN_GET_P_VAL:
  case INTRN_GET_S_VAL:
  case INTRN_GET_P_VALS:
  case INTRN_GET_S_VALS:
    //blocking valget calls 
    {
    call_wn = WN_Create(OPR_INTRINSIC_CALL, TY_mtype(ret_ty), MTYPE_V, 3);
    WN_kid0(call_wn) = src;
    WN_kid1(call_wn) = offt;
    WN_kid2(call_wn) = size;
    result = WN_CreateBlock();
    WN_INSERT_BlockLast(result, call_wn);
    result = WN_CreateComma (OPR_COMMA, TY_mtype(ret_ty), MTYPE_V, result,
			     WN_Ldid (rtype, -1, Return_Val_Preg, ret_ty));	     
    ret_st = Gen_Temp_Symbol(ret_ty, (char*) ".spillld");
    WN* wn1 = WN_CreateBlock();
    WN_INSERT_BlockLast (wn1, WN_Stid (rtype, 0, ret_st, ret_ty, result));
    result = WN_CreateComma(OPR_COMMA, TY_mtype(ret_ty), MTYPE_V, wn1,
			    WN_Ldid(rtype, 0, ret_st, ret_ty));
    break;
    }
  case INTRN_GET_P_FVAL: 
  case INTRN_GET_S_FVAL:
  case INTRN_GET_P_DVAL: 
  case INTRN_GET_S_DVAL:
  case INTRN_GET_P_FVALS: 
  case INTRN_GET_S_FVALS:
  case INTRN_GET_P_DVALS: 
  case INTRN_GET_S_DVALS: 
    {
      //the blocking float valget calls
      call_wn = WN_Create(OPR_INTRINSIC_CALL, rtype, MTYPE_V, 2);
      WN_kid0(call_wn) = src;
      WN_kid1(call_wn) = offt;
      result = WN_CreateBlock();
      TY_IDX result_ty = MTYPE_To_TY(rtype);
      WN_INSERT_BlockLast(result, call_wn);
      result = WN_CreateComma (OPR_COMMA, rtype, MTYPE_V, result,
			       WN_Ldid (rtype, -1, Return_Val_Preg, result_ty));	     
      ret_st = Gen_Temp_Symbol(result_ty, (char*) ".spillld");
      
      WN* wn1 = WN_CreateBlock();
      WN_INSERT_BlockLast (wn1, WN_Stid (rtype, 0, ret_st, result_ty, result));
      result = WN_CreateComma(OPR_COMMA, rtype, MTYPE_V, wn1,
			      WN_Ldid(rtype, 0, ret_st, ST_type(ret_st)));
    }
    break;
  default:
    //blocking get calls
    call_wn = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 4);
    if (dest == NULL) {
      //we need a stack temporary to store the result
      ret_st = Gen_Temp_Symbol(ret_ty, (char*) ".spillld");
      if (Use_Type_Access && phaseless) {
	WN_kid0(call_wn) = WN_CreateParm(TY_mtype(ret_ty), WN_Ldid(TY_mtype(ret_ty), 0, ret_st, ret_ty),
					 ret_ty, WN_PARM_BY_VALUE);
      } else {
	WN_kid0(call_wn) = WN_CreateParm(Pointer_Mtype, WN_Lda(Pointer_Mtype, 0, ret_st,0), 
					 void_ptr_idx, WN_PARM_BY_VALUE);
      }
    } else {
      if (Use_Type_Access && phaseless ) {
	if (WN_operator(dest) == OPR_LDA) {
	  ret_st = WN_st(dest);
	  ret_ty = ST_type(ret_st);
	  WN_kid0(call_wn) = WN_CreateParm(TY_mtype(ret_ty), WN_Ldid(TY_mtype(ret_ty), 0, ret_st, ret_ty),
					   ret_ty, WN_PARM_BY_VALUE);
	} else {
	  //create an iload to obtain the lvalue
	  WN* lval = WN_Iload(TY_mtype(ret_ty), 0, ret_ty, WN_COPY_Tree(dest), 0);
	  WN_kid0(call_wn) = WN_CreateParm(TY_mtype(ret_ty), lval, ret_ty, WN_PARM_BY_VALUE);
	}
      } else {
	WN_kid0(call_wn) = WN_CreateParm(Pointer_Mtype, WN_COPY_Tree(dest), void_ptr_idx, WN_PARM_BY_VALUE);
      }
    }
    WN_kid1(call_wn) = src;
    WN_kid2(call_wn) = offt;
    if (Use_Type_Access && phaseless) {
      WN_kid3(call_wn) = WN_Create_Type_Expr(ret_ty);
      //Print_TY(stderr, ret_ty);
    } else {
      WN_kid3(call_wn) = size;    
    }
    result = WN_CreateBlock();
    WN_INSERT_BlockLast(result, call_wn);
    if (!dest) {
      //Dest != NULL means we have a simple assignment a = exp.  In this case
      //We don't need to generate a return value.
      result = WN_CreateComma (OPR_COMMA, TY_mtype(ret_ty), MTYPE_V, 
			       result, WN_Ldid(TY_mtype(ret_ty), 0, ret_st, ST_type(ret_st)));	     
    }
  }

  WN_intrinsic(call_wn) = iop; 
  WN_Set_Linenum(call_wn, upc_srcpos);

  return result;
}  




// assumes lowering already called on the children of the ADD/SUB
// i.e. base returns a shared_ptr_t
WN*
WN_Create_Shared_Ptr_Arithmetic( WN *base, WN *disp, OPERATOR opr, 
				 UINT esize, UINT bsize, int phaseless)
{
 //  fprintf(stderr,"Shared Ptr Arith IN : ");
//   fdump_tree(stderr, base);
//   fprintf(stderr,"PTR ARITH DISP : ");
//   fdump_tree(stderr, disp);
  
  if( esize == 0 ) {
    fprintf(stderr, "WARNING: Ptr Arithmetic on void type\n");
  }

  if ((WN_operator(disp) == OPR_INTCONST) &&
      (WN_const_val(disp) == 0)) {
    // Ptr Arithmetic w/ disp==0 is a NO-OP
    return base;
  }

  WN *call_wn;
  TY_IDX actual_shared_ptr_idx = (bsize <= 1 && esize > 0) ? pshared_ptr_idx : shared_ptr_idx;
  TYPE_ID rtype = TY_mtype(actual_shared_ptr_idx);

  call_wn = WN_Create (OPR_INTRINSIC_CALL,
		       (rtype == MTYPE_M) ? MTYPE_M : Shared_Load_Extend_Mtyp(rtype),  
		       MTYPE_V,  (bsize <= 1 && esize > 0) ? 3 : 4 );    
  
  WN_Set_Linenum(call_wn, upc_srcpos);

  if (opr == OPR_SUB) {
    disp = WN_Neg(MTYPE_I8, disp);
  }
  WN_intrinsic(call_wn) = WN_Operator_To_Intrinsic (opr, bsize, 0, esize);
  
  if(WN_operator(base) == OPR_TAS)
    base = WN_kid0(base);
  
  // XXX: Parms are of types size_t and ptrdiff_t, but we are using uintptr_t and
  // intptr_t as "close enough".  In the event a platform uses narrower types than
  // these, then the backend compiler will eventually truncate appropriately.
  TYPE_ID intptr_type, uintptr_type;
  if (TY_size(MTYPE_To_TY(Pointer_type)) > 4) {
    intptr_type  = MTYPE_I8;
    uintptr_type = MTYPE_U8;
  } else {
    intptr_type  = MTYPE_I4;
    uintptr_type = MTYPE_U4;
  }

  WN_kid0(call_wn) =  WN_CreateParm(TY_mtype(actual_shared_ptr_idx), base, 
				    actual_shared_ptr_idx, WN_PARM_BY_VALUE);
  TY_IDX tidx = WN_ty(base);
  WN_kid1(call_wn) =  WN_CreateParm(uintptr_type,  
                                 WN_Intconst(uintptr_type, esize),
                                 MTYPE_To_TY(uintptr_type), 
                                 WN_PARM_BY_VALUE);  

  WN_kid2(call_wn) = WN_CreateParm(intptr_type, disp,
                                MTYPE_To_TY(intptr_type),
				WN_PARM_BY_VALUE);
  if (bsize > 1 || esize == 0)
    WN_kid3(call_wn) =  WN_CreateParm(uintptr_type, 
		    WN_Intconst(uintptr_type,  bsize),
		    MTYPE_To_TY(uintptr_type), WN_PARM_BY_VALUE);
  
  WN *wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast (wn0, call_wn); 
  WN *wn1 = WN_Ldid(rtype, -1, Return_Val_Preg, actual_shared_ptr_idx);
  WN *wn2;
  
  ST *ret_st;
  ret_st = Gen_Temp_Symbol(actual_shared_ptr_idx, (char*) ".Mptra.");
  wn1 = WN_Stid (TY_mtype(actual_shared_ptr_idx), 0, ret_st, actual_shared_ptr_idx, wn1);
  WN_Set_Linenum(wn1, upc_srcpos);
  WN_INSERT_BlockLast (wn0, wn1);
  wn2 = WN_CreateLdid(OPR_LDID, rtype, rtype, ST_ofst(ret_st),
		      ret_st, actual_shared_ptr_idx, 0);
 
  
  wn1 = WN_CreateComma (OPR_COMMA, WN_rtype (wn2), MTYPE_V, wn0, wn2);
  // fprintf(stderr,"Shared Ptr Arith OUT : ");
//   fdump_tree(stderr, wn1);
  return wn1;
} 


WN*
WN_Convert_Shared_To_Local ( WN *ptr, TY_IDX ty, ST* st)
{
  int st0 = 0;


  // A TAS here was artificially generated in the front-end only to
   // pass the type info.
  if(WN_operator(ptr) == OPR_TAS)
    ptr = WN_kid0(ptr);
  
  TY_IDX sptr_idx = WN_ty(ptr);

  if (!sptr_idx) {
    Is_True(WN_operator(ptr) == OPR_COMMA, 
	    ("Can't determine the the implementation type of shared ptr in cast to local",""));
    sptr_idx = TY_To_Sptr_Idx(WN_ty(WN_kid1(ptr)));
  } else 
    sptr_idx = TY_To_Sptr_Idx(sptr_idx);

  if(!st /*&& TY_mtype(sptr_idx) != MTYPE_M */) {
    // cast to local in the middle of an expression
    if(ty == 0)
      ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V), TRUE);
    st = Gen_Temp_Symbol(ty, (char*) ".Mcvtptr.");
    st0 = 1;
  }
   
  WN *call_wn  = WN_Create (OPR_INTRINSIC_CALL, Pointer_Mtype, MTYPE_V, 1);
  
  if (sptr_idx == shared_ptr_idx  /*bsize > 1*/)
    WN_intrinsic (call_wn) = INTRN_SCVTADDR;
  else 
    WN_intrinsic (call_wn) = INTRN_P_TO_L;

  WN_Set_Linenum(call_wn, upc_srcpos);

  WN_kid0 (call_wn) = WN_CreateParm(TY_mtype(sptr_idx), WN_COPY_Tree(ptr), sptr_idx, WN_PARM_BY_VALUE);

  WN *wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast (wn0, call_wn);
  WN *wn1 = WN_Ldid(Pointer_Mtype, -1, Return_Val_Preg, ty);
  call_wn  = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V, wn0, wn1);
  if(st0) {
    call_wn = WN_CreateStid(OPR_STID, MTYPE_V, Pointer_Mtype, 0, st, ty, call_wn, 0);
    wn0 = WN_CreateBlock();
    WN_INSERT_BlockLast(wn0, call_wn);
    wn1 = WN_Ldid(TY_mtype(ty), 0, st, ty, 0);
    call_wn = WN_CreateComma(OPR_COMMA, TY_mtype(ty), MTYPE_V, wn0, wn1);
  }

   //fprintf(stderr, "SHARED_TO_LOCAL \n");
   //fdump_tree(stderr, call_wn);
  return call_wn;
}

WN*
WN_Convert_Shared_To_Int ( WN *ptr )
{
  // A TAS here was artificially generated in the front-end only to
   // pass the type info.
  if(WN_operator(ptr) == OPR_TAS)
    ptr = WN_kid0(ptr);

  TY_IDX sptr_idx = WN_ty(ptr);
  if (!sptr_idx) {
    Is_True(WN_operator(ptr) == OPR_COMMA, 
	    ("Can't determine the the implementation type of shared ptr in cast to integer",""));
    sptr_idx = TY_To_Sptr_Idx(WN_ty(WN_kid1(ptr)));
  } else 
    sptr_idx = TY_To_Sptr_Idx(sptr_idx);

  /* XXX: really want size_t here: */
  TYPE_ID ret_mtype = (TY_size(MTYPE_To_TY(Pointer_type)) > 4) ? MTYPE_U8 : MTYPE_U4;
  TY_IDX ret_ty = MTYPE_To_TY(ret_mtype);

  WN *call_wn = WN_Create(OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, 1);
  WN_intrinsic(call_wn) = (sptr_idx == shared_ptr_idx)
				? INTRN_ADDROF_S : INTRN_ADDROF_P;
  WN_Set_Linenum(call_wn, upc_srcpos);
  WN_kid0(call_wn) = WN_CreateParm(TY_mtype(sptr_idx), ptr, sptr_idx, WN_PARM_BY_VALUE);
  
  WN *wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast (wn0, call_wn);

  WN *wn1 = WN_Ldid(ret_mtype, -1, Return_Val_Preg, ret_ty);
   
  ST *ret_st = Gen_Temp_Symbol(ret_ty, (char*) ".Mptrint.");
  wn1 = WN_Stid (ret_mtype, 0, ret_st, ret_ty, wn1);
  WN_INSERT_BlockLast (wn0, wn1);
  wn1 = WN_CreateLdid(OPR_LDID, ret_mtype, ret_mtype, 0,
		      ret_st, ret_ty, 0);
  
  return WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V, wn0, wn1);
}

WN *
WN_Create_PtrEq_Test(OPERATOR op, WN *awn0, WN *awn1, TY_IDX idx0, TY_IDX idx1) 
{
  INTRINSIC iop = INTRN_EQ_P_P;
  BOOL negate = FALSE;
  UINT args = 2;
  
  WN *arg0 = awn0;
  WN *arg1 = awn1;
  TY_IDX tmp;

 

  if (op == OPR_GE || op == OPR_GT || op == OPR_LE || op == OPR_LT) {
    //For non-equality shared pointer comparison, we need to use sub
    WN* sub = WN_Create_Shared_Ptr_Diff(awn0, awn1, idx0, idx1);
    WN* result = WN_Relational(op, Integer_type, sub, WN_Intconst(Integer_type, 0));
    return result;
  }

  //one of idx0 and idx1 should be pointer-to-shared
  if (Type_Is_Shared_Ptr(idx0, true))
    idx0 = TY_To_Sptr_Idx (TY_pointed(idx0));
  
  if (Type_Is_Shared_Ptr(idx1, true))
    idx1 = TY_To_Sptr_Idx (TY_pointed(idx1));
  
  iop = WN_Operator_To_Intrinsic(OPR_EQ, idx0, idx1);
  
  if(WN_operator(awn0) == OPR_INTCONST) {
    args = 1;
    arg0 = awn1;
  } else if (WN_operator(awn1) == OPR_INTCONST) {
    args = 1;
    arg0 = awn0;
  } else if(idx0 == pshared_ptr_idx) {
    arg0 = awn1;
    arg1 = awn0;
    tmp = idx0;
    idx0 = idx1;
    idx1 = tmp;
    
  }
      
  switch (op) {
  case OPR_EQ:
    break;
  case OPR_NE:
    negate = TRUE;
    break;
  default:
    Is_True(0,("",""));
  }
 
  WN *call_wn = WN_Create(OPR_INTRINSIC_CALL, MTYPE_I4, MTYPE_V, args);
  WN_intrinsic(call_wn) = iop;

  WN_Set_Linenum(call_wn, upc_srcpos);

  WN_kid0(call_wn) = WN_CreateParm(TY_mtype(idx0), arg0, idx0, WN_PARM_BY_VALUE);
  
  if(args > 1) {
    WN_kid1(call_wn) = WN_CreateParm(TY_mtype(idx1), arg1, idx1, WN_PARM_BY_VALUE);
  }
  
  WN *wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast (wn0, call_wn);
  WN *wn1 = WN_Ldid(MTYPE_I4, -1, Return_Val_Preg, MTYPE_To_TY(MTYPE_I4));
   
  wn1 = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V, wn0, wn1);
  if (negate)
      wn1 = WN_LNOT(wn1);

  ST *st = Gen_Temp_Symbol(MTYPE_To_TY(MTYPE_I4), (char*) ".Mptreq.");  
  idx0 = MTYPE_To_TY(MTYPE_I4);
  wn1 = WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_I4, 0, st, idx0, wn1, 0);
  wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast(wn0, wn1);
  wn1 = WN_Ldid(MTYPE_I4, 0, st, idx0, 0);
  wn1 = WN_CreateComma(OPR_COMMA, MTYPE_I4, MTYPE_V, wn0, wn1);
 
  return  wn1;
}


WN*
WN_Create_StoP_Cvt(WN *init_wn, INTRINSIC iop)
{
  
  TY_IDX ret_ty;
  TY_IDX arg_ty;

  switch(iop) {
  case INTRN_S_RESET:
    ret_ty = arg_ty = shared_ptr_idx;
    break;
  case INTRN_S_TO_P:
    ret_ty = pshared_ptr_idx;
    arg_ty = shared_ptr_idx;
    break;
  case INTRN_P_TO_S:
    ret_ty = shared_ptr_idx;
    arg_ty = pshared_ptr_idx;
    break;
  case INTRINSIC_LAST:
    return init_wn;
  default:
    Is_True(0, ("Bad intrinsic value in PtoS_Cvt",""));
  }

  WN *call_wn = WN_Create(OPR_INTRINSIC_CALL, TY_mtype(ret_ty),
			  MTYPE_V, 1);
  WN_intrinsic(call_wn) = iop;
  WN_Set_Linenum(call_wn, upc_srcpos);

  WN_kid0(call_wn) = 
    WN_CreateParm(TY_mtype(arg_ty), init_wn, arg_ty, WN_PARM_BY_VALUE);
  
  WN *wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast (wn0, call_wn);

  WN *wn1 = WN_Ldid(TY_mtype(ret_ty), -1, Return_Val_Preg, ret_ty);
   
  ST *ret_st = Gen_Temp_Symbol(ret_ty, (char*) ".Mstopcvt.");
  wn1 = WN_Stid (TY_mtype(ret_ty), 0, ret_st, ret_ty, wn1);
  WN_INSERT_BlockLast (wn0, wn1);
  wn1 = WN_CreateLdid(OPR_LDID, TY_mtype(ret_ty), TY_mtype(ret_ty), 0,
		      ret_st, ret_ty, 0);
  
  wn1 = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V, wn0, wn1);

  return  wn1;

}



WN *
Spill_Shared_Load( WN *ld)
{
 //  fdump_tree(stderr, ld);
  Is_True(WN_operator(ld) == OPR_COMMA || WN_operator(ld) == OPR_LDID, ("",""));
  WN *block = WN_CreateBlock();
  
  TY_IDX idx = WN_ty(WN_operator(ld) == OPR_COMMA ? WN_kid1(ld) : ld);
  ST* ret_st = Gen_Temp_Symbol(idx, 
			    Index_To_Str(Save_Str2((char*)".Mreturn.",(char*)".Mreturn.")));
  WN *temp = WN_Stid (TY_mtype(idx), 0, ret_st, idx, ld);
  WN_Set_Linenum(temp, upc_srcpos);
  WN_INSERT_BlockLast (block, temp);
  return  WN_CreateComma (OPR_COMMA, Widen_Mtype(TY_mtype(idx)),MTYPE_V, block, 
			  WN_Ldid(TY_mtype(idx), 0, ret_st, idx));
}

//Create the UPC related maps and mempool for this function
//void LowerUPC_Init_Consistency()
void Create_UPC_Maps()
{

  static bool create_mempool = true;

  if (create_mempool) {
    MEM_POOL_Initialize(&upc_mem_pool, "upc split-phase opt pool", FALSE);
    create_mempool = false;
    MEM_POOL_Push(&upc_mem_pool);
  }
  upc_comm_map = WN_MAP_Create(&upc_mem_pool);
  upc_nbi_map = WN_MAP32_Create(&upc_mem_pool);
}

void Destroy_UPC_Maps() {

  WN_MAP_Delete(upc_comm_map);
  WN_MAP_Delete(upc_nbi_map);
}

WN *
WN_Create_Shared_Ptr_Diff( WN *op0, WN *op1, TY_IDX t1, TY_IDX t2)
{

  WN *call_wn;
  
  TYPE_ID rtype = Pointer_type;
  TY_IDX ret_ty = MTYPE_To_TY(rtype);
  int esize = Get_Type_Inner_Size(t1);
  int bsize = Get_Type_Block_Size(t1);
  //t1, t2 are both guaranteed to be local pointer to shared data
  t1 = TY_pointed(t1);
  t2 = TY_pointed(t2);

  if (!TY_is_pshared(t1)) {
    call_wn = WN_Create (OPR_INTRINSIC_CALL, rtype, MTYPE_V,  4 );    
    WN_intrinsic(call_wn) = INTRN_SUB_S;
    
    WN_Set_Linenum(call_wn, upc_srcpos);
    WN_kid0(call_wn) =  WN_CreateParm(TY_mtype(shared_ptr_idx), op0, 
				      shared_ptr_idx, WN_PARM_BY_VALUE);
    if (TY_is_pshared(t2)) {
      op1 = WN_Create_StoP_Cvt(op1, INTRN_P_TO_S);
    }
    WN_kid1(call_wn) = WN_CreateParm(TY_mtype(shared_ptr_idx), op1,
				     shared_ptr_idx, WN_PARM_BY_VALUE);
    WN_kid2(call_wn) =  WN_CreateParm(Integer_type,  
				      WN_Intconst(Integer_type, esize),
				      MTYPE_To_TY(Integer_type), 
				      WN_PARM_BY_VALUE);  
    WN_kid3(call_wn) = WN_CreateParm(Integer_type, 
				     WN_Intconst(Integer_type,  bsize ? bsize: 1),
				     MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  } else {
    call_wn = WN_Create (OPR_INTRINSIC_CALL, rtype, MTYPE_V,  3);    
    if (TY_block_size(t1) == 1) {
      WN_intrinsic(call_wn) = INTRN_SUB_P1;
    } else {
      WN_intrinsic(call_wn) = INTRN_SUB_PI;
    }
      WN_kid0 (call_wn) =  WN_CreateParm(TY_mtype(pshared_ptr_idx), op0, 
					 pshared_ptr_idx, WN_PARM_BY_VALUE);
    
      if (!TY_is_pshared(t2)) {
	op1 = WN_Create_StoP_Cvt(op1, INTRN_S_TO_P);
      }
      WN_kid1(call_wn) = WN_CreateParm(TY_mtype(pshared_ptr_idx), op1,
				       pshared_ptr_idx, WN_PARM_BY_VALUE);
      WN_kid2(call_wn) =  WN_CreateParm(Integer_type,  
					WN_Intconst(Integer_type, esize),
					MTYPE_To_TY(Integer_type), 
					WN_PARM_BY_VALUE);      
  }
  
  WN *wn0 = WN_CreateBlock();
  WN_INSERT_BlockLast (wn0, call_wn); 
  WN *wn1 = WN_Ldid(rtype, -1, Return_Val_Preg, ret_ty);
  WN *wn2;
  
  ST *ret_st;
  ret_st = Gen_Temp_Symbol(ret_ty, (char*) ".Mreturn.");
  wn1 = WN_Stid (rtype, 0, ret_st, ret_ty, wn1);
  WN_INSERT_BlockLast (wn0, wn1);
  wn2 = WN_CreateLdid(OPR_LDID, rtype, rtype, ST_ofst(ret_st),
		      ret_st, ret_ty, 0);
  
  wn1 = WN_CreateComma (OPR_COMMA, rtype, MTYPE_V, wn0, wn2);
  return wn1;
}



WN*
WN_SetNull_Sptr(WN *st) {
  
  WN *call_wn;
  TY_IDX actual_shared_ptr_idx;
  WN *arg;
  INTRINSIC iop;

  call_wn = WN_Create (OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 1);
  actual_shared_ptr_idx = TY_To_Sptr_Idx(WN_ty(st));
  switch (WN_operator(st)) {
  case OPR_STID:

    WN_kid0(call_wn) = WN_CreateParm(Pointer_Mtype, WN_Lda(Pointer_Mtype, WN_offset(st), WN_st(st), 0),
					 MTYPE_To_TY(Pointer_Mtype), WN_PARM_BY_VALUE);
    WN_intrinsic(call_wn) = (actual_shared_ptr_idx == shared_ptr_idx) ? 
      INTRN_SETNULL_S : INTRN_SETNULL_P;
    break;
  case OPR_ISTORE:
  case OPR_MSTORE:
    if(actual_shared_ptr_idx == pshared_ptr_idx) {
      //need to convert to shared first
      WN_kid1(st) = WN_Create_StoP_Cvt(WN_kid1(st), INTRN_P_TO_S);
      actual_shared_ptr_idx = shared_ptr_idx;
    }
    
    WN_kid0(call_wn)  = WN_CreateParm(TY_mtype(actual_shared_ptr_idx), WN_kid1(st),
				      actual_shared_ptr_idx, 
				      WN_PARM_BY_VALUE);
    WN_intrinsic(call_wn) = (TY_To_Sptr_Idx(TY_pointed(WN_ty(st))) == shared_ptr_idx) ? 
      INTRN_SETNULL_S : INTRN_SETNULL_P;
     break;
  default:
    Is_True(0,("",""));
  }
  
  WN_Set_Linenum(call_wn, upc_srcpos);
  return call_wn;
}



WN*
Strip_TAS(WN *wn, TY_IDX ty_idx) 
{
  
  while(WN_operator(wn) == OPR_TAS && 
	(ty_idx == 0 ||( ty_idx != 0 && WN_ty(wn) == ty_idx)) )
    wn = WN_kid0(wn);
  return wn;
}

WN *Combine_Offset_Terms(SPTR_OFFSET_TERM_STACK &stack)
{
  if(stack.empty())
    return 0;
  WN *wn0 = stack.top();
  stack.pop();
  if(stack.empty())
      return wn0;
  WN *wn1 = stack.top();
  stack.pop();
  
  wn0 = WN_Binary (OPR_ADD, MTYPE_I4, wn0, wn1);

  while(!stack.empty()) {
    wn1 = stack.top();
    stack.pop();
    wn0 = WN_Binary (OPR_ADD, MTYPE_I4, wn0, wn1);
  }

  return wn0;
  
}

//wn must be a FUNC_ENTRY
bool No_Opt_for_Func(WN* wn) {

  FmtAssert(WN_operator(wn) == OPR_FUNC_ENTRY, ("expecting func_entry node"));
  WN* pragma = WN_first(WN_func_pragmas(wn));
  while (pragma != NULL) {
    if (WN_pragma(pragma) == WN_PRAGMA_GENERIC) {
      char *s = Index_To_Str(ST_name_idx(WN_st(pragma)));
      if (strcmp(s, "noopt") == 0) {
	fprintf(stderr, "disabling optimization for %s()\n", ST_name(WN_st(wn))); 
	return true;
      }
    }
    pragma = WN_next(pragma);
  }
  return false;
}

