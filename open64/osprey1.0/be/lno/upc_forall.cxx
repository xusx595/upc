#include "upc_forall.h"
#include "wn.h"
#include "wn_pragmas.h"
#include "symtab.h"
#include "upc_symtab_utils.h"
#include "upc_wn_util.h"
#include "lwn_util.h"
#include "wn_util.h"
#include "ir_reader.h"
#include "lnopt_main.h"
#include "wn_simp.h"
#include "opt_alias_mgr.h"

void UPC_AFF_EXP::Find_base_and_index() {

  _base_arr = NULL;
  _index_exp = NULL;
  
  for (WN_ITER* iter = WN_WALK_TreeIter(_aff_exp); iter != NULL; iter = WN_WALK_TreeNext(iter)) {
    WN* wn = WN_ITER_wn(iter);
    switch (WN_operator(wn)) {
    case OPR_LDA:
    case OPR_LDID:
    case OPR_ILOAD:
      if (Type_Is_Shared_Ptr(WN_Get_Ref_TY(wn), true)) {
	_base_arr = wn;
      }
    default:
      ; //do nothing
    }
  }
  
  if (_base_arr == NULL) {
    _index_exp = _aff_exp;
  } else {
    FmtAssert(WN_operator(_aff_exp) == OPR_ADD, ("Affinity exp should be an add"));
    _index_exp = WN_kid0(_aff_exp);
  }
}

/**
 *
 *  Check if two array bases a and b are thread aligned, i.e,
 *  they have the same blocksize, and a[i] and b[i] belong to the same thread
 *  If ar1/ar2 is null(i.e., for integer affinity expressions), it represents an array
 *  Ideally we want a locality analysis manager that will supply the answer at each
 *  program point; without it we can't support base that are pointers
 */
bool UPC_AFF_EXP::Base_aligned(WN* arr) {

  TY_IDX ty = WN_Get_Ref_TY(arr);
  if (_base_arr == NULL) {
    return TY_kind(TY_pointed(ty)) == KIND_ARRAY || Alias_Mgr->Points_to_thread(arr, 0);
  } else {
    if (WN_Simp_Compare_Trees(Strip_TAS(_base_arr), Strip_TAS(arr)) == 0) {
      return true;
    }
    TY_IDX base_ty = WN_Get_Ref_TY(_base_arr);
    if ((TY_kind(TY_pointed(base_ty)) == KIND_ARRAY || Alias_Mgr->Points_to_thread(_base_arr, 0)) &&
	(TY_kind(TY_pointed(ty)) == KIND_ARRAY || Alias_Mgr->Points_to_thread(arr, 0))) {
	//both base pointers start on thread 0
	return true;
    }
    //return Points_to_same_thread(Alias_Mgr, arr, _base_arr);
    return Alias_Mgr->Points_to_same_thread(arr, _base_arr);
  }

}

void UPC_AFF_EXP::Print(FILE* file) {

  fprintf(file, "WN for affinity expression: \n");
  fdump_tree(file, _aff_exp);
  fprintf(file, "Base Array: %s\n", (_base_arr == NULL) ? "null" : "");
  if (_base_arr != NULL) {
    fdump_tree(file, _base_arr);
  }
  fprintf(file, "induction variable: %s\n", ST_name(Ind_var()));
  fprintf(file, "scale factor for induction var: %d\n", _scale);
}

//check whether the aff_exp is in the form <i + inv_exp>.
//inv_exp is either an integer expression or a shared address
static UPC_AFF_EXP* Process_aff_exp(WN* aff_exp, WN* ind_var) {

  WN* ind_exp = NULL;
  int scale = 1;
  for (WN_ITER* iter = WN_WALK_TreeIter(aff_exp); iter != NULL; iter = WN_WALK_TreeNext(iter)) {
    WN* wn = WN_ITER_wn(iter);
    switch (WN_operator(wn)) {
    case OPR_LDID: 
      {
	if (WN_st(wn) == WN_st(ind_var) &&
	    WN_offset(wn) == WN_idname_offset(ind_var)) {
	  if (ind_exp != NULL) {
	    //can only have one induction var
	    return NULL;
	  }

	  WN* parent = wn;
	  if (parent != aff_exp) {
	    do {
	      parent = LWN_Get_Parent(parent);
	      switch (WN_operator(parent)) {
	      case OPR_ADD:
		//do nothing
		break;
		/*
		  case OPR_SUB:
		  scale *= -1;
		  break;
		*/
	      case OPR_MPY:
		//require the other operand to be an intconst
		if (WN_operator(WN_kid0(parent)) == OPR_INTCONST) {
		  scale *= WN_const_val(WN_kid0(parent));
		} else if (WN_operator(WN_kid1(parent)) == OPR_INTCONST) {
		  scale *= WN_const_val(WN_kid1(parent));
		} else {
		  if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
		    fprintf(TFile, "forall opt warning: non const multiplier to induction var\n");
		    fdump_tree(TFile, aff_exp);	
		  }
		  return NULL;
		}
		break;
	      default:
		if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
		  fprintf(TFile, "forall opt warning: non affine affinity expression\n");
		  fdump_tree(TFile, aff_exp);	
		}
		return NULL;
	      }
	    } while (parent != aff_exp);
	  }
	  ind_exp = wn;
	}
	break;
      }
    default:
      ; //do nothing
    }
  }

  if (ind_exp == NULL) {
    return NULL;
  } else {
    return CXX_NEW(UPC_AFF_EXP(aff_exp, ind_var, scale), &LNO_default_pool);
  }
}

//get the value of GCD (x, threads)
static WN* Get_GCD_Call(int x) {

  if (x == 1 || x == -1) {
    //short circuit the common special case
    return WN_Intconst(Integer_type, 1);
  }
  
  //call gcd(x,y) in the runtime
  WN* call = WN_Create(OPR_INTRINSIC_CALL, Integer_type, MTYPE_V, 2);
  WN_intrinsic(call) = INTRN_GCD;
  WN_kid0(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, x), MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Integer_type, LWN_Get_Threads(), MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN* tmp_block = WN_CreateBlock();
  WN_INSERT_BlockLast(tmp_block, call);
  call = WN_CreateComma(OPR_COMMA, Integer_type, MTYPE_V, tmp_block,
			WN_Ldid(Integer_type, -1, Return_Val_Preg, MTYPE_To_TY(Integer_type)));
  return call;
}

//Get the starting iteration for a thread.
static WN* Get_My_Start(WN* lo, int step, int scale, WN* start_thread) {

  WN* threads = LWN_Get_Threads();
  WN* mythread = WN_Ldid(TY_mtype(ST_type(upc_mythread_st)), 0, upc_mythread_st, ST_type(upc_mythread_st));
  int aff_incr = step * scale;
  if (aff_incr == 1 || aff_incr == -1) {
    WN* my_start = (aff_incr > 0) ? WN_Sub(Integer_type, mythread, start_thread) :
      WN_Sub(Integer_type, start_thread, mythread);
    my_start = WN_CreateExp2(OPC_I4MOD, my_start, threads);
    return (aff_incr > 0) ? WN_Add(Integer_type, WN_COPY_Tree(lo), my_start) :
      WN_Sub(Integer_type, WN_COPY_Tree(lo), my_start);
  }

  //call forall_start(start_thread, step, upper_bnd)
  //where start_thread is the thread executing the first iteration,
  //step is the amount the induction variable is incremented by every iteration
  WN* call = WN_Create(OPR_INTRINSIC_CALL, Integer_type, MTYPE_V, 4);
  WN_intrinsic(call) = INTRN_FORALL_START;
  WN_kid0(call) = WN_CreateParm(Integer_type, start_thread, MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, step), MTYPE_To_TY(Integer_type), 
				WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Integer_type, WN_COPY_Tree(lo), MTYPE_To_TY(Integer_type), 
				WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, scale), MTYPE_To_TY(Integer_type), 
				WN_PARM_BY_VALUE);
  WN* tmp_block = WN_CreateBlock();
  WN_INSERT_BlockLast(tmp_block, call);
  call = WN_CreateComma(OPR_COMMA, Integer_type, MTYPE_V, tmp_block,
			WN_Ldid(Integer_type, -1, Return_Val_Preg, MTYPE_To_TY(Integer_type)));
  return call;
 
}

//Do some pattern matching to find the affinity expression
//ind_var is the forall loop's induction variable in IDNAME form
static UPC_AFF_EXP *find_aff_exp(WN* aff_test, WN* ind_var) {

  if (WN_operator(aff_test) != OPR_IF) {
    if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
      fprintf(TFile, "ill-formed upc forall loop\n");
      fdump_tree(TFile, aff_test);
    }
    return NULL;
  }
  WN* aff_exp = NULL;

  if (WN_operator(WN_kid1(WN_if_test(aff_test))) == OPR_MOD) {
    aff_exp = WN_kid0(WN_kid1(WN_if_test(aff_test)));
  } else if (WN_operator(WN_kid0(WN_if_test(aff_test))) == OPR_MOD) {
    aff_exp = WN_kid0(WN_kid0(WN_if_test(aff_test)));
  }

  if (aff_exp != NULL) {
    //case of integer aff exp
    return Process_aff_exp(aff_exp, ind_var);
  } 

  //affinity expression is a shared address, of the form &a[i+inv_exp]
  for (WN* wn = WN_prev(aff_test); wn != NULL; wn = WN_prev(wn)) {
    if (WN_operator(wn) == OPR_INTRINSIC_CALL &&
	(WN_intrinsic(wn) == INTRN_AFF_P || WN_intrinsic(wn) == INTRN_AFF_S)) {
      aff_exp = WN_kid0(WN_kid0(wn));
      while (WN_operator(aff_exp) == OPR_TAS) {
	aff_exp = WN_kid0(aff_exp);
      }
      if (WN_operator(aff_exp) == OPR_ADD) {
	//index exp are normalized so that the induction var is always the outermost arg0 
	return Process_aff_exp(aff_exp, ind_var);
      }
      return NULL;
    }
  }
  return NULL;
}


//FIXME: does this work with field accesses???
static void Localize_Cyclic_Array_Ref(WN* loop, WN* parent, ST* new_ind) {

  FmtAssert(WN_operator(parent) == OPR_ILOAD || WN_operator(parent) == OPR_ISTORE, 
	    ("Must be either iload/istore"));
  WN* body = WN_do_body(loop);
  WN* block = LWN_Get_Parent(loop);
  
  WN* array_wn = WN_operator(parent) == OPR_ILOAD ? WN_kid0(parent) : WN_kid1(parent);
  TY_IDX local_ty = Make_Pointer_Type(Shared_To_Private_Type(WN_object_ty(parent)));

  ST* local_var = Gen_Temp_Symbol(local_ty, "local_ptr");
  WN* init = LWN_Get_Tas(WN_COPY_Tree(array_wn), local_ty);
  init = WN_Stid(Pointer_Mtype, 0, local_var, ST_type(local_var), init);
  WN_INSERT_BlockBefore(block, loop, init);


  //replace the a[exp] with la[i], where la and i are the new base/induction_var, respectively
  WN_array_base(array_wn) = WN_Ldid(Pointer_Mtype, 0, local_var, ST_type(local_var));
  WN_array_index(array_wn, 0) = WN_Ldid(Integer_type, 0, new_ind, ST_type(new_ind));
  if (WN_operator(parent) == OPR_ILOAD) {
    WN_set_ty(parent, TY_pointed(local_ty));
    WN_set_load_addr_ty(parent, ST_type(local_var));
  } else {
    WN_set_ty(parent, ST_type(local_var));
  }

  //fdump_tree(stderr, block);
}

//Called after array privatization, 
//check if the old IV is no longer used in the loop
static bool IV_Can_Be_Removed(WN* loop) {

  WN* body = WN_do_body(loop);
  WN* index = WN_index(loop);
  for (WN_ITER* iter = WN_WALK_TreeIter(body); iter != NULL; iter = WN_WALK_TreeNext(iter)) {
      WN* wn = WN_ITER_wn(iter);
      if (WN_operator(wn) == OPR_LDID && WN_st(wn) == WN_st(index) &&
	  WN_offset(wn) == WN_idname_offset(index)) {
	  //the old IV is still used 
	  return false;
      }
  }
  return true;
} 


//Defined in upc_vectorize.cxx. creates the ARA_Info field
//in a DO_LOOP_INFO
extern void Create_ARA_Info(WN* wn);


/*
 *  For forall loops with cyclic affinity expressions, we can use the knowledge from the affinity expression
 *  to localize (cyclic) array expressions that can be statically determined to be local.
 *  This involves comparing the index expression of an array access to the affinity expression;
 *  if they are equivalent (affinity expressions can never be modified inside the loop, so we merely have to
 *  check for structural equivalence) and the threadof() of the two base arrays are also the same, we can
 *  conclude that tha access is local
 *
 *  FIXME:  only handles 1D array for now, as 2D arrays are not marked in the front end anyway
 *
 */
static void Upc_Localize_Cyclic(WN* loop, UPC_AFF_EXP* aff_exp) {

  DYN_ARRAY<WN*> local_arrays;

  //for pointers also need to check for scalar defs
  bool base_is_modified = false;
  WN* body = WN_do_body(loop);
  local_arrays.Set_Mem_Pool(&LNO_default_pool);

  //fdump_tree(stderr, aff_exp->Aff_exp());

  for (WN_ITER* iter = WN_WALK_TreeIter(body); iter != NULL; iter = WN_WALK_TreeNext(iter)) {
    WN* wn = WN_ITER_wn(iter);
    if (WN_operator(wn) == OPR_ARRAY) {
      WN* parent = LWN_Get_Parent(wn);
      if (WN_operator(parent) != OPR_ILOAD && WN_operator(parent) != OPR_ISTORE) {
	continue;
      }
      WN* base = WN_array_base(wn);      
      WN* index = WN_array_index(wn, 0);
      while (WN_operator(base) == OPR_ARRAY) {
	base = WN_array_base(base);
      }
      ST* a_st = WN_st(base);
      TY_IDX a_ty = WN_Get_Ref_TY(base);
      TY_IDX obj_ty = WN_object_ty(parent);
      if (!Type_Is_Shared_Ptr(a_ty) || TY_block_size(obj_ty) != 1) {
	//only support cyclic arrays
	continue;
      }

      if (TY_kind(a_ty) == KIND_POINTER && TY_kind(TY_pointed(a_ty)) != KIND_ARRAY) {
	DO_LOOP_INFO *dl_info = Get_Do_Loop_Info(loop);
	if (dl_info->ARA_Info == NULL) {
	  Create_ARA_Info(loop);
	}
	
	//assume the pointer base may be modified
	//FIXME: this condition may be too strong as it is apparently set to true
	//if the loop body contains any pointers...
	if (dl_info->Has_Bad_Mem) {
	  //fprintf(stderr, "loop has bad mem\n");
	  continue;
	}

	//check if the pointer base is explicitly modified by scalar operations
	ARA_LOOP_INFO * info = dl_info->ARA_Info;
	SCALAR_STACK & scalar_def = info->SCALAR_MAY_DEF();
	for (int j = 0; j < scalar_def.Elements(); j++) {
	  if (a_st == scalar_def.Bottom_nth(j)->_scalar.St()) {
	    //fprintf(stderr, "base addr is modified in loop\n");
	    base_is_modified = true;
	    break;
	  }
	}
	if (base_is_modified) {
	  continue;
	}
      }


      if (aff_exp->Base_arr() == NULL) {
	//int aff exp
	if (WN_Simp_Compare_Trees(index, aff_exp->Aff_exp()) == 0) {
	  if (aff_exp->Base_aligned(base)) {
	    local_arrays.AddElement(parent);
	  }
	}
      } else {
	if (WN_Simp_Compare_Trees(wn, aff_exp->Aff_exp()) == 0) {
	  //exact match
	  local_arrays.AddElement(parent);
	} else {
	  //index terms match, and base arrays are aligned
	  if (WN_Simp_Compare_Trees(index, aff_exp->Index_exp()) == 0) {
	    if (aff_exp->Base_aligned(base)) {
	      local_arrays.AddElement(parent);
	    }
	  }
	}
      }
    }
  }

  if (local_arrays.Elements() > 0) {
    //set up the common variables for all array accesses to be localized
    ST* orig_ind = WN_st(WN_index(loop));
    WN* start = WN_kid0(WN_start(loop));
    WN* step = WN_kid1(WN_kid0(WN_step(loop)));
    WN* block = LWN_Get_Parent(loop);
    ST* new_ind = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "new_iv");

    //initialize the old induction var to the new lower bound, so the start value of 
    //the local pointer is correct
    WN* ind_init = WN_Stid(TY_mtype(ST_type(orig_ind)), WN_idname_offset(WN_index(loop)), 
			   orig_ind, ST_type(orig_ind), WN_COPY_Tree(start));
    WN_INSERT_BlockBefore(block, loop, ind_init);

    for (int i =0; i < local_arrays.Elements(); i++) {
      if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
	fprintf(TFile, "localize an array access in the forall loop\n");
      }
      Localize_Cyclic_Array_Ref(loop, local_arrays[i], new_ind);
    }

    

    //initialize the new induction variable before entering the loop
    ind_init = WN_Stid(Integer_type, 0, new_ind, ST_type(new_ind), WN_Intconst(Integer_type, 0));
    WN* IV_incr = WN_Div(Integer_type, WN_COPY_Tree(step), LWN_Get_Threads());
    IV_incr = WN_Stid(Integer_type, 0, new_ind, ST_type(new_ind),
		       WN_Add(Integer_type, WN_Ldid(Integer_type, 0, new_ind, ST_type(new_ind)), IV_incr));

    if (IV_Can_Be_Removed(loop)) {
	//replace the old IV with the new one
	WN_start(loop) = ind_init;
	WN_step(loop) = IV_incr;
	WN* upper_bnd = WN_end(loop);
	bool val_is_kid0 = true;
	if (WN_operator(WN_kid0(upper_bnd)) == OPR_LDID &&
	    WN_st(WN_kid0(upper_bnd)) == orig_ind) {
	    val_is_kid0 = false;
	}
	upper_bnd = WN_COPY_Tree(val_is_kid0 ? WN_kid0(upper_bnd) : WN_kid1(upper_bnd));
	WN* new_end = WN_Sub(Integer_type, upper_bnd, start);
	new_end = LWN_CreateDivceil(Integer_type, new_end, LWN_Get_Threads());
	WN* new_IV = WN_Ldid(Integer_type, 0, new_ind, ST_type(new_ind));
	WN_end(loop) = WN_Relational(WN_operator(WN_end(loop)), Integer_type, 
				     val_is_kid0 ? new_end : new_IV,
				     val_is_kid0 ? new_IV : new_end);
	WN_index(loop) = WN_CreateIdname(0, ST_st_idx(new_ind));
    } else {
	WN_INSERT_BlockBefore(block, loop, ind_init);
	WN_INSERT_BlockLast(body, IV_incr);
    }

    local_arrays.Free_array();
  }
} 

/*
  check if the given loop is a upc forall loop, and try to optimize away the affinity test
*/
static void Upc_Forall_Opt(WN* loop, WN* aff_test, UPC_AFF_EXP* aff_exp) {

  WN* body = WN_do_body(loop);
  WN* block = LWN_Get_Parent(loop);
  WN* index = WN_index(loop);
  WN* lower_bnd = WN_kid0(WN_start(loop));
  WN* upper_bnd = WN_end(loop);
  WN* step = WN_kid0(WN_step(loop));
  if (WN_operator(WN_kid0(step)) == OPR_INTCONST) {
    step = WN_kid0(step);
  } else if (WN_operator(WN_kid1(step)) == OPR_INTCONST) {
    step = WN_kid1(step);
  } else {
    //only handle compile-time constant stride for now
    return;
  }

  //aff_exp->Print();

  int step_val = WN_const_val(step);
  WN* threads = LWN_Get_Threads();
  WN* mythread = WN_Ldid(TY_mtype(ST_type(upc_mythread_st)), 0, upc_mythread_st, ST_type(upc_mythread_st));


  WN* start_thread = NULL;
  //block size for the base array of the shared address
  unsigned int bsize = aff_exp->Bsize(); 
  WN* arr = aff_exp->Base_arr();

  if (bsize == 0) {
    //the trivial case where only one thread needs to run
    start_thread = LWN_Get_UPC_Intrinsic_Call(WN_COPY_Tree(arr), INTRN_THREADOF_S);
    WN* if_test = WN_EQ(Integer_type, start_thread, mythread);
    if_test = WN_CreateIf(if_test, WN_CreateBlock(), WN_CreateBlock());
    WN_INSERT_BlockBefore(block, loop, if_test);
    WN_next(if_test) = WN_next(loop);
    if (WN_next(loop) != NULL) {
      WN_prev(WN_next(loop)) = if_test;
    }
    WN_do_body(loop) = WN_then(aff_test);
    WN_INSERT_BlockLast(WN_then(if_test), loop);
    LWN_Parentize(block);
    if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
      fprintf(TFile, "remove aff expression for indefinite array\n");
    }
    return;
  } else if (bsize != 1) {
    //we'll need 2D loops here, plus the stride may be different each time (The strides will repeat themselves in at most b-iterations,
    //where b is the blocksize.  Ignore this case for now, as they require building a table to store the strides
    return;
  }

  if (arr == NULL) {
    //integer aff exp, start thread is simply the inv_exp
    start_thread = WN_CreateExp2(OPC_I4MOD, aff_exp->Aff_exp(), threads);
  } else {
    //address aff exp, start thread is threadof(inv_exp).
    start_thread = LWN_Get_UPC_Intrinsic_Call(WN_COPY_Tree(aff_exp->Aff_exp()), INTRN_THREADOF_S);
  }

  /*
    For affine affinity expressions with cyclic arrays (this includes integer affinity expressions), 
    we can derive a general solution.  Consider the following forall loop:

    forall (i = L; i < U; i += S; ai + b) 
    
    Define the following symbols:
    start_thread = (a*L + b) % Threads  -- This is the thread executing the first iteration
    gcd (x, y)  -- A runtime function computing the GCD of two (possibly negative?) integers
    forall_start() -- Another runtime function computing the starting iteration for MYTHREAD.
                      Returns a value larger than U if the thread will not execute the forall loop
    
    Equipped by the above definition, we can rewrite the above loop into an equivalent for loop:

    int gcd = gcd (S*a, THREADS);
    int my_start = forall_start(start_thread, S, a, L);
    for (i = my_start; i < U; i += THREADS * step / gcd) 

  */
  
  ST* gcd = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "gcd");
  WN* gcd_val = WN_Stid(Integer_type, 0, gcd, ST_type(gcd), Get_GCD_Call(step_val * aff_exp->Scale()));
  WN_INSERT_BlockBefore(block, loop, gcd_val);
  //must initialize induction var to the lower bound for new_start to take right value
  WN* init_ind = WN_Stid(Integer_type, aff_exp->Ind_var_ofst(), aff_exp->Ind_var(), 
			 ST_type(aff_exp->Ind_var()), WN_COPY_Tree(lower_bnd));
  WN_INSERT_BlockBefore(block, loop, init_ind);
  //spill start_thread into a stack var, since apparently we can not have nested commas both
  //refering to preg_return_val
  ST* start_th = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "start_thread");
  WN_INSERT_BlockBefore(block, loop, WN_Stid(Integer_type, 0, start_th, ST_type(start_th), start_thread));
  
  ST* new_start = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "new_start");
  WN* my_start = Get_My_Start(lower_bnd, step_val, aff_exp->Scale(), 
			      WN_Ldid(Integer_type, 0, start_th, ST_type(start_th)));
  my_start = WN_Stid(Integer_type, 0, new_start, ST_type(new_start), my_start);
  WN_INSERT_BlockBefore(block, loop, my_start);
  
  WN* new_step = LWN_Get_Threads();
  new_step = WN_Div(Integer_type, new_step, WN_Ldid(Integer_type, 0, gcd, ST_type(gcd)));
  new_step = WN_Mpy(Integer_type, new_step, WN_Intconst(Integer_type, step_val));

  WN_kid0(WN_start(loop)) = WN_Ldid(Integer_type, 0, new_start, ST_type(new_start));
  WN_kid1(WN_kid0(WN_step(loop))) = new_step;
  
  WN_do_body(loop) = WN_then(aff_test);
  
  {
    //we should make sure that we didn't inadvertantly change induction var's exit value
    WN* upper_bnd = WN_end(loop);
    ST* ind_var = WN_st(WN_index(loop));
    WN_OFFSET ind_ofst = WN_idname_offset(WN_index(loop));
    WN* exit_val;
    if (WN_operator(WN_kid0(upper_bnd)) == OPR_LDID &&
	WN_st(WN_kid0(upper_bnd)) == ind_var) {
      exit_val = WN_COPY_Tree(WN_kid1(upper_bnd));
    } else if (WN_operator(WN_kid1(upper_bnd)) == OPR_LDID &&
	       WN_st(WN_kid0(upper_bnd)) == ind_var) {
      exit_val = WN_COPY_Tree(WN_kid0(upper_bnd));
    } else {
      return;
    }
    
    if (step_val != 1 && step_val != -1) {
	//need more work here...
	exit_val = WN_Sub(Integer_type, upper_bnd, lower_bnd);
	exit_val = LWN_CreateDivceil(Integer_type, exit_val, WN_Intconst(Integer_type, step_val));
	exit_val = WN_Mpy(Integer_type, exit_val, WN_Intconst(Integer_type, step_val));
	exit_val = WN_Add(Integer_type, lower_bnd, exit_val);
    }
    
    switch (WN_operator(upper_bnd)) {
    case OPR_LE:
      exit_val = WN_Add(Integer_type, exit_val, WN_Intconst(Integer_type, step_val));
      break;
    case OPR_GE:
      exit_val = WN_Sub(Integer_type, exit_val, WN_Intconst(Integer_type, step_val));
      break;
    }
    
    exit_val = WN_Stid(Integer_type, ind_ofst, ind_var, ST_type(ind_var), exit_val);
    WN_INSERT_BlockAfter(block, loop, exit_val);
  }

  LWN_Parentize(block);
  if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
    fprintf(TFile, "affinity test removed for cyclic arrays/integer affinity expression\n");
  }

  //Upc_Localize_Cyclic(loop, aff_exp);

}


extern void Upc_Forall_Opt(WN* func_nd) {

  OPCODE opcode = WN_opcode(func_nd);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(func_nd);
    while (kid) {
      WN *next = WN_next(kid);
      Upc_Forall_Opt(kid);
      kid = next;
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(func_nd); kidno++) {
      WN *kid = WN_kid(func_nd, kidno);
      Upc_Forall_Opt(kid);
    }
    if (opcode == OPC_DO_LOOP) {
      DO_LOOP_INFO* do_info = Get_Do_Loop_Info(func_nd);
      if (do_info != NULL) {
	WN* loop = func_nd;
	WN* body = WN_do_body(loop);
	WN* aff_prag = WN_last(body); 
	if (WN_opcode(aff_prag) == OPC_PRAGMA &&
	    WN_pragma(aff_prag) == WN_PRAGMA_UPC_FORALL_AFFINITY) {
	  if (Get_Trace(TP_LNOPT2, TT_UPC_OPT)) {
	    fprintf(TFile, "found a forall loop\n");
	  }
	  UPC_AFF_EXP* aff_exp = find_aff_exp(WN_prev(aff_prag), WN_index(loop));
	  if (aff_exp != NULL) {
	    //aff_exp->Print();
	    Upc_Forall_Opt(loop, WN_prev(aff_prag), aff_exp);
	  }
	}
      } 
    }
  }
}

