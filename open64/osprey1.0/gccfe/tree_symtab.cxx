/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* translate gnu decl trees to symtab references */

#include <stdlib.h>
#if (defined(__MACH__) && defined(__APPLE__)) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#else
#include <values.h>
#endif
#include "defs.h"
#include "errors.h"
#include "gnu_config.h"
extern "C" {
#include "gnu/flags.h"
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/toplev.h"
#include "gnu/c-tree.h"
#include "gnu/upc-act.h"
}
#ifdef TARG_IA32
// the definition in gnu/config/i386/i386.h causes problem
// with the enumeration in common/com/ia32/config_targ.h
#undef TARGET_PENTIUM
#endif /* TARG_IA32 */

#include "symtab.h"
#include "strtab.h"
#include "tree_symtab.h"
#include "wn.h"
#include "wfe_expr.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "ir_reader.h"
#include <cmplrs/rcodes.h>

#include "glob.h"
#include "cxx_memory.h"

#include <math.h>
#include <stack>
#include <map>

extern FILE *tree_dump_file; // For debugging only

extern INT pstatic_as_global;

extern string utoa(UINT64 i);

int Scope_level = 1;
int For_scope = 0;

TY_IDX char_ty = 0; /* see tree_symtab.h */
TY_IDX long_ty = 0;
TY_IDX long_unsigned_ty = 0;

/* UPC specific */

tree shared_ptr_tree;
BOOL double_align_exception = FALSE;
BOOL double_inner_exception = FALSE;
BOOL longlong_align_exception = FALSE;
BOOL longlong_inner_exception = FALSE;
BOOL sptr_inner_exception = FALSE;
BOOL psptr_inner_exception = FALSE;
BOOL sptr_align_exception = FALSE;
BOOL psptr_align_exception  = FALSE;
BOOL struct_align_promote = FALSE;


//store the shared variables.  This is processed at the end of every PU
//to deal with function static variables.
std::map<ST_IDX, int> upc_st_orig_ty;


//store the TLD vars. This is a set because of forward declarations 
//(e.g. extern decl followed by its definition in the same translation unit)
std::set<ST_IDX> upc_tld; 

//record all incomplete struct types that are shared
std::vector<TY_IDX> incomplete_structs;

std::stack<stack<ST*>*> inner_scope_vbles;
std::stack<ST *> file_scope_statics;


std::multimap<TY_IDX, TY_IDX> incomplete_array_types;

/* A list of integer types */
static TY_IDX int_ty[itk_none];
static const char* int_type_name[itk_none] = {"char", "signed char", "unsigned char", "short", "unsigned short", "int", "unsigned int", "long", "unsigned long", "long long", "unsigned long long"}; 

/**
 *  Initialize the TY_IDX for each integer type.
 *  This is necessary so we can distinguish between 
 *  different types with the same size (e.g. long and int on ia32)
 *  , and not cause warnings in the generated code 
 *  See bug 409.
 */

void init_int_type() {

  static bool initialized = false;
  
  if (initialized) {
    return;
  }
  TY& char_ty = New_TY(int_ty[itk_char]);
  TY_Init(char_ty, base_ty_size[_CHAR][0], KIND_SCALAR, MTYPE_I1, 
	  /* prepend the name with upc, so we can later find these types in whirl2c*/
	  Save_Str(int_type_name[itk_char]));
  Set_TY_is_logical(char_ty); //overload this qualifier to represent C integral types

  for (int i = itk_signed_char; i < itk_none; i++) {
    TY& ty = New_TY (int_ty[i]);
    TYPE_ID mtype;
    unsigned size = Get_Integer_Value(TYPE_SIZE(integer_types[i])) / BITSPERBYTE;
    switch (size) {
    case 1: 
      mtype = MTYPE_I1;
      break;
    case 2:
      mtype = MTYPE_I2;
      break;
    case 4:
      mtype = MTYPE_I4;
      break;
    case 8:
      mtype = MTYPE_I8;
      break;
    default:
      Fail_FmtAssertion("Unexpected size for %s: %d", int_type_name[i], size);   
    }
    if (TREE_UNSIGNED(integer_types[i])) {
      mtype = MTYPE_complement(mtype);
    }
    
    TY_Init(ty, size, KIND_SCALAR, mtype, Save_Str(int_type_name[i]));
    Set_TY_is_logical(int_ty[i]);
    //fprintf(stderr, "type: %s size %d mtype %d\n", TY_name(ty), TY_size(ty), TY_mtype(ty));
  }

  initialized = true;
}

/**
 *  Return the hash value of a string. 
 */
unsigned int hash_val(string s) {

  unsigned int length = s.size();
  unsigned int res = 0;
  for (int i = 0; i < length; i++) {
    res = s[i] + 31 * res;
  }
  return res + random();
}

static TY_IDX Get_Innermost_Field_Type (TY_IDX fld_idx) {

  if(TY_kind(fld_idx) != KIND_STRUCT && TY_kind(fld_idx) != KIND_ARRAY)
    return fld_idx;

  if(TY_kind(fld_idx) == KIND_STRUCT)
    return Get_Innermost_Field_Type(Get_Field_Type(fld_idx, 1));

  if(TY_kind(fld_idx) == KIND_ARRAY)
    return Get_Innermost_Field_Type(Get_Inner_Array_Type(fld_idx));
  
}

int recompute_align(TY_IDX idx) {
  
  int alignment = TY_align(idx);
  TY_IDX last_fld_idx;
  FLD_ITER  fiter  = Make_fld_iter(TY_fld(idx));
  FLD_ITER last = Fld_Table.end ();
  FLD_HANDLE fh;
  do {
    fh = FLD_HANDLE(fiter);
    last_fld_idx = FLD_type(fh);
    if(alignment < TY_align(last_fld_idx))
      alignment = TY_align(last_fld_idx);
  } while (!FLD_last_field(fiter) && ++fiter != last);
  return alignment;
}


static bool is_star(tree block) {
  return block == star_layout_node;
}

static bool is_mangled = false;

static string mangle_name() {

  static char buf[MAX_PATH];
  if (!is_mangled) {
    realpath(Orig_Src_File_Name, buf);
    is_mangled = true;
  }
  return (string) buf;
}


//This code patches the sizes correctly
//Problem is that 

void Patch_Incomplete_Arrays(TY_IDX idx) 
{
  typedef multimap<TY_IDX,TY_IDX>::iterator MI;
  
  pair<MI,MI> r = incomplete_array_types.equal_range(TY_IDX_index(idx));
  MI p;
  TY_IDX aty;
  ARB_HANDLE arb;
  int nelems;
  
 
  for(p= r.first; p != r.second; ++p) {
    if(p->first == TY_IDX_index(idx)) { 
      aty = p->second;
      arb = TY_arb(aty);
      nelems = ARB_ubnd_val(arb)+1;
      Set_TY_adjusted_size(aty, nelems * TY_adjusted_size(idx));		  
      Set_TY_size(aty, nelems*TY_size(idx));
      Set_TY_align (aty, TY_align(idx));
      if(Type_Is_Shared_Ptr(idx, TRUE))
	Set_TY_align(idx, TY_align(TY_To_Sptr_Idx(idx)));
      if(ARB_const_stride(arb))
	 Set_ARB_stride_val (arb, TY_size(idx) / BITSPERBYTE);
      Set_TY_etype(Ty_Table[aty], idx);
      Patch_Incomplete_Arrays(aty);
    }	  
  } 
  
  


}


//For all that I can remember:
// return 1 if block = NULL, 
// otherwise return the value of the block tree 
int Type_Tree_Block_Size(tree type_tree) {

  tree block = 0;
  int bsize = -1;

  switch (TREE_CODE (type_tree)) {
  case VOID_TYPE:
  case BOOLEAN_TYPE:
  case INTEGER_TYPE:
  case CHAR_TYPE:  
  case ENUMERAL_TYPE:
  case REAL_TYPE:
  case RECORD_TYPE:
  case UNION_TYPE:
  case COMPLEX_TYPE:
    block = TYPE_BLOCK_SIZE(type_tree); 
    //if (block && TYPE_SIZE(type_tree)) {
    /* We may have incomplete structs here */
    if (block) {
      bsize = Get_Integer_Value(block);
    }
    break;
  case ARRAY_TYPE:
    block = TYPE_BLOCK_SIZE(get_inner_array_type(type_tree));
    if (block) {
      int elt_size = Get_Integer_Value(TYPE_SIZE(get_inner_array_type(type_tree)));
      if (is_star(block)) {
	int ar_size = Get_Integer_Value(TYPE_SIZE(type_tree));
	if (threads_int == 0) {
	  //In dynamic env, block size is the same as the product of dimensions
	  bsize = ar_size / elt_size;
	} else {
	  //in static case, needs to use the formula
	  bsize = (ar_size / elt_size + threads_int - 1) / threads_int; 
	}
	if (bsize > max_bsize) {
	  error ("Maximum block size in this implementation is %lu", max_bsize);    
	}
      } else {
	//regular case 
	bsize = Get_Integer_Value(block);
      }
    }
    break;
  case POINTER_TYPE:
  case REFERENCE_TYPE:
    block = TYPE_BLOCK_SIZE(type_tree);
    if (block) {
      int elt_size = Get_Integer_Value(TYPE_SIZE(type_tree));
      if (is_star(block)) {
	//can't have [*] in pointers
	error ("The [*] qualifier can only be used for arrays");
      }	
      //return Get_Integer_Value(block) / elt_size;
      bsize = Get_Integer_Value(block);
    }
    break;
  default:
    break;
  }

  if (bsize == -1) {
    bsize = 1;
  }
  return bsize;
}

static char*
Get_Name (tree node)
{
	static UINT anon_num = 0;
	static char buf[64];

	if (node == NULL) {
		++anon_num;
		sprintf(buf, "%s%d_%ul", UPC_ANON_PREFIX, anon_num, hash_val(mangle_name()));
		return buf; // EEK! this is BAD practice but our callers are disciplined
	}
	else if (TREE_CODE (node) == IDENTIFIER_NODE)
		return IDENTIFIER_POINTER (node);
	else if (TREE_CODE (node) == TYPE_DECL)
		// If type has a typedef-name, the TYPE_NAME is a TYPE_DECL.
		return IDENTIFIER_POINTER (DECL_NAME (node));
	else
		FmtAssert(FALSE, ("Get_Name unexpected tree"));
		return NULL;
}

//For normal typedefs, in the output we simply use the actual type
//instead of the typename.
//If the actual type is a struct however, we may not be able to use it 
//directly since it may be anonymous.
//For this case, in the output we use the typedef name to represent the type;
//a "T " is prepended to the typedef name to distinguish it from 
//ordinary struct types
static STR_IDX Get_typedef_name(tree type_tree) {

  char tmp[256];
  strcpy(tmp, "T ");
  strncpy(&tmp[2], IDENTIFIER_POINTER(DECL_NAME(TYPE_NAME(type_tree))), 250);
  return Save_Str(tmp);
}

static void Set_Type_Quals(tree type_tree, TY_IDX& idx) {

  if (TYPE_READONLY(type_tree))
    Set_TY_is_const (idx);
  if (TYPE_VOLATILE(type_tree))
    Set_TY_is_volatile (idx);
  if (TYPE_RESTRICT(type_tree) && TY_kind(idx) == KIND_POINTER)
    Set_TY_is_restrict(idx);
}

// idx is non-zero only for RECORD and UNION, when there is forward declaration

extern TY_IDX
Create_TY_For_Tree (tree type_tree, TY_IDX idx)
{
	if (TREE_CODE(type_tree) == ERROR_MARK)
		exit (RC_USER_ERROR);

	TY_IDX orig_idx = idx;
	if(TREE_CODE_CLASS(TREE_CODE(type_tree)) != 't') {
	    DevWarn("Bad tree class passed to Create_TY_For_Tree %c",
		TREE_CODE_CLASS(TREE_CODE(type_tree)));
	    return idx;
	}
	

	// for typedefs get the information from the base type
	if (TYPE_NAME(type_tree) &&
	    idx == 0 &&
	    (TREE_CODE(type_tree) == RECORD_TYPE ||
	     TREE_CODE(type_tree) == UNION_TYPE) &&
	    TREE_CODE(TYPE_NAME(type_tree)) == TYPE_DECL &&
	    TYPE_MAIN_VARIANT(type_tree) != type_tree) {
	  idx = Get_TY (TYPE_MAIN_VARIANT(type_tree));

	  Set_Type_Quals(type_tree, idx);
	  
	  if (TY_kind(idx) == KIND_STRUCT) {
	    if (TY_is_anonymous(idx)) {
	      /* For anonymous structs, set its name to its corresponding typedef */
	      Set_TY_name_idx(idx, Get_typedef_name(type_tree));
	    }
	  }

	  if (TYPE_SHARED(type_tree)) {
	    idx = Make_Shared_Type(idx, 
				   TYPE_BLOCK_SIZE(type_tree) ? 
				   Get_Integer_Value( TYPE_BLOCK_SIZE(type_tree)) : 1);
	    
	  }
	  
	  
	  
	  TYPE_TY_IDX(type_tree) = idx;
	  if(Debug_Level >= 2) {
	    struct mongoose_gcc_DST_IDX dst = 
	      Create_DST_type_For_Tree(type_tree,idx,orig_idx);
	    TYPE_DST_IDX(type_tree) = dst;
	  }
	  
	  return idx;
	}
	
	TYPE_ID mtype;
	UINT64 tsize;
	BOOL variable_size = FALSE;
	tree type_size = TYPE_SIZE(type_tree);
	

	UINT align = TYPE_ALIGN(type_tree) / BITSPERBYTE;
	if (TREE_CODE(type_tree) == VOID_TYPE)
		tsize = 0;
	else
	if (type_size == NULL) {
		// incomplete structs have 0 size
		FmtAssert(TREE_CODE(type_tree) == ARRAY_TYPE 
			|| TREE_CODE(type_tree) == UNION_TYPE
			|| TREE_CODE(type_tree) == RECORD_TYPE
			  || TREE_CODE(type_tree) == ENUMERAL_TYPE,	
		  ("Create_TY_For_Tree: type_size NULL for non ARRAY/RECORD"));
		tsize = 0;
	}
	else {
		if (TREE_CODE(type_size) != INTEGER_CST) {
			if (TREE_CODE(type_tree) == ARRAY_TYPE)
				DevWarn ("Encountered VLA at line %d", lineno);
			else
				Fail_FmtAssertion ("VLA at line %d not currently implemented", lineno);
			variable_size = TRUE;
			tsize = 0;
		}
		else
			tsize = Get_Integer_Value(type_size) / BITSPERBYTE;
		
		if (TREE_CODE(type_tree) == ARRAY_TYPE && 
		    threads_int != 0 && UPC_TYPE_HAS_THREADS(TYPE_DOMAIN(type_tree))) {
		  /* see comments in c-decl.c */
		  tsize *= threads_int;
		  //fprintf(stderr, "tsize after : %ull\n", tsize);
		}
	}
	switch (TREE_CODE(type_tree)) {
	case VOID_TYPE:
		idx = MTYPE_To_TY (MTYPE_V);	// use predefined type
		break;
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:

	  init_int_type();

	  switch (tsize) {
	  case 1:  mtype = MTYPE_I1; break;
	  case 2:  mtype = MTYPE_I2; break;
	  case 4:  mtype = MTYPE_I4; break;
	  case 8:  mtype = MTYPE_I8; break;
#ifdef _LP64
	  case 16:  mtype = MTYPE_I8; break;
#endif /* _LP64 */
	  default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
	  }
	  
	  if (TREE_UNSIGNED(type_tree)) {
	    mtype = MTYPE_complement(mtype);
	  }

	  idx = MTYPE_To_TY (mtype);	// use predefined type
	  
	  /* Use the actual C integral types when possible, instead of using
	     the internal fix-width types */
	  //if (!TYPE_SHARED(type_tree)) {
	    for (int i = 0; i < itk_none; i++) {
	      if (type_tree == integer_types[i]) {
		idx = int_ty[i];
		break;
	      } else if (TYPE_MAIN_VARIANT(type_tree) == integer_types[i]) {
		//Handle the case of type-qualified integral types
		idx = int_ty [i];
		break;
	      }
	    }
	    //}

	  Set_TY_align (idx, align);
	  break;
	case CHAR_TYPE:
		mtype = (TREE_UNSIGNED(type_tree) ? MTYPE_U1 : MTYPE_I1);
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case ENUMERAL_TYPE:
#if 1 // Use natural C types
		init_int_type();
		idx = TREE_UNSIGNED(type_tree) ? int_ty[itk_unsigned_int] : int_ty[itk_int];
#else
		mtype = (TREE_UNSIGNED(type_tree) ? MTYPE_U4 : MTYPE_I4);
		idx = MTYPE_To_TY (mtype);	// use predefined type
#endif
		break;
	case REAL_TYPE:
		switch (tsize) {
		case 4:  mtype = MTYPE_F4; break;
		case 8:  mtype = MTYPE_F8; break;
		case 16: mtype = MTYPE_FQ; break;  //for alpha
		case 12: mtype = MTYPE_FQ; break;  //for IA32
		default: 
		  if (tsize == base_ty_size[_LONGDOUBLE][0]) {
		    mtype = MTYPE_FQ;
		  } else {
		    FmtAssert(FALSE, ("Get_TY unexpected REAL_TYPE size %d", tsize));
		  }
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case COMPLEX_TYPE:
		switch (tsize) {
		case  8: mtype = MTYPE_C4; break;
		case 16: mtype = MTYPE_C8; break;
#ifdef TARG_MIPS
		case 32: mtype = MTYPE_CQ; break;
#endif /* TARG_MIPS */
#ifdef TARG_IA64
		case 24: mtype = MTYPE_C10; break;
#endif /* TARG_IA64 */
#ifdef TARG_IA32
		case 24: mtype = MTYPE_C10; break;
#endif /* TARG_IA32 */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case REFERENCE_TYPE:
	case POINTER_TYPE:
	 
	  idx = Get_TY (TREE_TYPE(type_tree));
	  if(TY_is_shared(idx) && TY_kind(idx) == KIND_STRUCT && TY_size(idx) == 0)
	    incomplete_structs.push_back(idx);

	  Set_Type_Quals(TREE_TYPE(type_tree), idx);

	  idx = Make_Pointer_Type (idx);
	  Set_TY_align (idx, align);
	  if (TYPE_RESTRICT(type_tree)) {
	    Set_TY_is_restrict(idx);
	  }
	  if(Type_Is_Shared_Ptr(idx, TRUE)) {
	    Set_TY_adjusted_size(idx, TY_size(TY_To_Sptr_Idx(idx)));
	  } else 
	    Set_TY_adjusted_size(idx, TY_size(idx));
	  break;
	case ARRAY_TYPE:
		{
		  bool incomplete = FALSE;
		  tree elem_type = get_inner_array_type(type_tree);
		  TY_IDX tmp_idx = Get_TY(elem_type);
		 
		  if(TY_kind(tmp_idx) == KIND_STRUCT && TY_size(tmp_idx) == 0)
		    incomplete_structs.push_back(tmp_idx);

		TY &ty = New_TY (idx);
		TY_Init (ty, tsize, KIND_ARRAY, MTYPE_M, 
			 Save_Str(Get_Name(TYPE_NAME(type_tree))) );
		//If block size is *, we need to fix the element type's block size first
		if (is_star(TYPE_BLOCK_SIZE(elem_type))) {
		  int bsize = Type_Tree_Block_Size(type_tree);
		  TY_IDX eidx = TYPE_TY_IDX(elem_type);
		  TYPE_BLOCK_SIZE(elem_type) = 
		    build_int_2(bsize, 0);
		  //bug 2778 - at this point, elem_type has alredy been instantiated in Ty_Table
		  // patch  the whirl type with the new block size
		  if( eidx && bsize != Get_Type_Block_Size(eidx))
		    eidx = Copy_TY(eidx);
		    Set_TY_block_size(eidx, bsize);
		    TYPE_TY_IDX(elem_type) = eidx;
		}
		TY_IDX ety = Get_TY (TREE_TYPE(type_tree));
		Set_Type_Quals(TREE_TYPE(type_tree), ety);

		Set_TY_etype (ty, ety);
		if(TY_size(ety) == 0) {
		  if(TREE_CODE(TYPE_NAME(elem_type)) == TYPE_DECL) {
		    incomplete = TRUE;
		    incomplete_array_types.insert(make_pair(TY_IDX_index(ety),idx));
		  } else {
		    error("Incomplete or unknown type: %s", Get_Name(TYPE_NAME(TREE_TYPE(type_tree))));
		    exit (RC_USER_ERROR); // bug244 - we crash if we continue
		  }
		}

		if(!incomplete)
		  Set_TY_adjusted_size(idx, 
				       (TY_size(idx)/TY_size(ety)) * TY_adjusted_size(ety));		  
		
		Set_TY_align (idx, TY_align(TY_etype(ty)));
		if(Type_Is_Shared_Ptr(TY_etype(ty), TRUE))
		  Set_TY_align(idx, TY_align(TY_To_Sptr_Idx(TY_etype(ty))));
		
		// assumes 1 dimension
		// nested arrays are treated as arrays of arrays
		ARB_HANDLE arb = New_ARB ();
		ARB_Init (arb, 0, 0, 0);
		Set_TY_arb (ty, arb);
		Set_ARB_first_dimen (arb);
		Set_ARB_last_dimen (arb);
		Set_ARB_dimension (arb, 1);
		if(!TYPE_SIZE(TREE_TYPE(type_tree)) && !incomplete) {

		    error("incomplete type for array of %s", Get_Name(TYPE_NAME(TREE_TYPE(type_tree))));
		    exit (RC_USER_ERROR);
		 
		} 
		if(!incomplete) {
		  if (TREE_CODE(TYPE_SIZE(TREE_TYPE(type_tree))) == INTEGER_CST) {
		    
		    Set_ARB_const_stride (arb);
		    Set_ARB_stride_val (arb, 
					Get_Integer_Value (TYPE_SIZE(TREE_TYPE(type_tree))) 
					/ BITSPERBYTE);
		  }
		  else {
		    WN *swn;
		    swn = WFE_Expand_Expr (TYPE_SIZE(TREE_TYPE(type_tree)));
		    if (WN_opcode (swn) == OPC_U4I4CVT ||
			WN_opcode (swn) == OPC_U8I8CVT) {
		      swn = WN_kid0 (swn);
		    }
		    FmtAssert (WN_operator (swn) == OPR_LDID,
			       ("stride operator for VLA not LDID"));
		    ST *st = WN_st (swn);
		    TY_IDX ty_idx = ST_type (st);
		    WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
					       (ST_IDX) NULL, 1);
		    WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
		    WFE_Stmt_Append (wn, Get_Srcpos());
		    Clear_ARB_const_stride (arb);
		    Set_ARB_stride_var (arb, (ST_IDX) ST_st_idx (st));
		  }
		}
		  Set_ARB_const_lbnd (arb);
		  Set_ARB_lbnd_val (arb, 0);
		 
		if (type_size) {
		    if (TREE_CODE(TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree))) ==
			INTEGER_CST) {
		      INT64 val = Get_Integer_Value(type_size);
		      if (val > 0) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, Get_Integer_Value (
								  TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) ));
		      } else {
			Clear_ARB_const_ubnd(arb);
			Set_ARB_ubnd_val(arb, 0);
		      }
		    }
		    else {
			WN *uwn = WFE_Expand_Expr (TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) );
			if (WN_opcode (uwn) == OPC_U4I4CVT ||
			    WN_opcode (uwn) == OPC_U8I8CVT) {
				uwn = WN_kid0 (uwn);
			}
			FmtAssert (WN_operator (uwn) == OPR_LDID,
				("bounds operator for VLA not LDID"));
			ST *st = WN_st (uwn);
			TY_IDX ty_idx = ST_type (st);
			WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
						   (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WFE_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_var (arb, ST_st_idx (st));
		    }
		}
		else {
		  
		  if(incomplete) {
		    Set_ARB_ubnd_val(arb, Get_Integer_Value(TYPE_MAX_VALUE(TYPE_DOMAIN(type_tree))));
		  }  else { 
		    Clear_ARB_const_ubnd (arb);
		    Set_ARB_ubnd_val (arb, 0);
		  }
		}

		if (variable_size) {
			WN *swn, *wn;
			swn = WFE_Expand_Expr (type_size);
			if (TY_size(TY_etype(ty))) {
				if (WN_opcode (swn) == OPC_U4I4CVT ||
				    WN_opcode (swn) == OPC_U8I8CVT) {
					swn = WN_kid0 (swn);
				}
				FmtAssert (WN_operator (swn) == OPR_LDID,
					("size operator for VLA not LDID"));
				ST *st = WN_st (swn);
				TY_IDX ty_idx = ST_type (st);
				TYPE_ID mtype = TY_mtype (ty_idx);
				swn = WN_Div (mtype, swn, WN_Intconst (mtype, BITSPERBYTE));
				wn = WN_Stid (mtype, 0, st, ty_idx, swn);
				WFE_Stmt_Append (wn, Get_Srcpos());
			}
		}
	} // end array scope
	break;
	case RECORD_TYPE:
	case UNION_TYPE: {	// new scope for local vars
		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
		STR_IDX name;
		
		if (TYPE_NAME(type_tree) && TREE_CODE(TYPE_NAME(type_tree)) == TYPE_DECL) {
		  name = Get_typedef_name(type_tree);
		} else {
		  name = Save_Str(Get_Name(TYPE_NAME(type_tree)));
		}

	
		TY_Init (ty, tsize, KIND_STRUCT, MTYPE_M, name);
		
		if (TREE_CODE(type_tree) == UNION_TYPE) {
			Set_TY_is_union(idx);
		}
		if (align == 0) align = 1;	// in case incomplete type
		Set_TY_align (idx, align);
		// set idx now in case recurse thru fields
		TYPE_TY_IDX(type_tree) = idx;	

		// to handle nested structs and avoid entering flds
		// into wrong struct, make two passes over the fields.
		// first create the list of flds for the current struct,
		// but don't follow the nested types.  Then go back thru
		// the fields and set the fld_type, recursing down into
		// nested structs.
  		Set_TY_fld (ty, FLD_HANDLE());
		FLD_IDX first_field_idx = Fld_Table.Size ();
		tree field;
		FLD_HANDLE fld;
		for (field = TREE_PURPOSE(type_tree); 
			field;
			field = TREE_CHAIN(field) )
		{
			if (TREE_CODE(field) == TYPE_DECL) {
				DevWarn ("got TYPE_DECL in field list");
				continue;
			}
			if (TREE_CODE(field) == CONST_DECL) {
				DevWarn ("got CONST_DECL in field list");
				continue;
			}
			fld = New_FLD ();
			FLD_Init (fld, Save_Str(Get_Name(DECL_NAME(field))), 
				0, // type
				Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
				Get_Integer_Value(DECL_FIELD_BIT_OFFSET(field))
					/ BITSPERBYTE );
#ifdef OLDCODE
			if ( ! DECL_BIT_FIELD(field)
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field))) 
					* BITSPERBYTE) && 
			     TY_kind(Get_TY(TREE_TYPE(field))) == KIND_SCALAR )
			{
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because 
				// otherwise the field type is wrong.
				DevWarn("field size %d doesn't match type size %d", 
					Get_Integer_Value(DECL_SIZE(field)),
					TY_size(Get_TY(TREE_TYPE(field)))
						* BITSPERBYTE );
				DECL_BIT_FIELD(field) = 1;
			}
			if (DECL_BIT_FIELD(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld, 
//					Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
					% BITSPERBYTE );
				Set_FLD_bsize (fld, Get_Integer_Value(DECL_SIZE(field)));
			}
#endif /* OLDCODE */
		}
  		FLD_IDX last_field_idx = Fld_Table.Size () - 1;
		if (last_field_idx >= first_field_idx) {
			Set_TY_fld (ty, FLD_HANDLE (first_field_idx));
			Set_FLD_last_field (FLD_HANDLE (last_field_idx));
		}
		// now set the fld types.
		fld = TY_fld(ty);
		UINT64 prev_off = 0;
		UINT64 displ = 0;
		//UINT align = 1;
		TY_IDX prev_fld_ty = 0;
		FLD_HANDLE prev_fld = FLD_HANDLE();
		
		for (field = TREE_PURPOSE(type_tree);
			field;
			field = TREE_CHAIN(field))
		{
			if (TREE_CODE(field) == TYPE_DECL)
				continue;
			if (TREE_CODE(field) == CONST_DECL)
				continue;
			if ( ! DECL_BIT_FIELD(field)
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field))) 
				      * BITSPERBYTE) && 
			     TY_kind(Get_TY(TREE_TYPE(field))) == KIND_SCALAR )
			{
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because 
				// otherwise the field type is wrong.
				DevWarn("field size %llu doesn't match type size %llu", 
					(UINT64) Get_Integer_Value(DECL_SIZE(field)),
					TY_size(Get_TY(TREE_TYPE(field))) * BITSPERBYTE);
				DECL_BIT_FIELD(field) = 1;
			}
			if (DECL_BIT_FIELD(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld, 
//					Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
					% BITSPERBYTE );
				Set_FLD_bsize (fld, Get_Integer_Value(DECL_SIZE(field)));
			}
			TY_IDX fty_idx = Get_TY(TREE_TYPE(field));
			if(TY_kind(fty_idx) == KIND_STRUCT && TY_align(fty_idx) == 1) {
 			  //typedefs to incomplete struct types
  			  Set_TY_align(fty_idx, recompute_align(fty_idx));
  			}

			Set_Type_Quals(TREE_TYPE(field), fty_idx);

			if(prev_fld_ty == 0) {
			  //this is the first  field - change here the struct alignment
			  // for exceptions .... (double on PPC....)
			  prev_fld_ty = fty_idx;
			  TY_IDX innermost_idx = Get_Innermost_Field_Type(fty_idx);
			 
			  //on PPC double as first field of struct are aligned at 4
			  // instead of 8
			  if(double_align_exception && 
			     (TY_mtype(innermost_idx) == MTYPE_F8 || 
			      (TY_kind(innermost_idx) == KIND_ARRAY && 
			       TY_mtype(Get_Inner_Array_Type(innermost_idx)) == MTYPE_F8)))
			    {
			      Set_TY_align(idx, 8);
			      align = 8;
			    }
			  if(longlong_align_exception && 
			     (TY_mtype(innermost_idx) == MTYPE_I8 ||
			      TY_mtype(innermost_idx) == MTYPE_U8) ) {
			    Set_TY_align(idx, 8);
			    align = 8;
			  }
			  if(Type_Is_Shared_Ptr(innermost_idx, TRUE)) {
			    if(sptr_align_exception && TY_To_Sptr_Idx(innermost_idx) == shared_ptr_idx)
			      Set_TY_align(idx, 8);
			    else if(psptr_align_exception && TY_To_Sptr_Idx(innermost_idx) == pshared_ptr_idx)
			      Set_TY_align(idx, 8);
			    else
			      Set_TY_align(idx, TY_align(TY_To_Sptr_Idx(innermost_idx)));
			  } else if(align == 1 && TY_align(innermost_idx) > 1) {
			    //for typedefs to incomplete types
			    Set_TY_align(idx, TY_align(innermost_idx));
			  }
			} else { //second or after fields
			  align = TY_align(fty_idx);
			  TY_IDX innermost_idx = 0;
			  if(Type_Is_Shared_Ptr(fty_idx)) {
			    align = TY_align(TY_To_Sptr_Idx(fty_idx));
			    if(align > TY_align(idx))
			      Set_TY_align(idx, align);
			  }
 
			  if(struct_align_promote && align > TY_align(idx))
			    Set_TY_align(idx, align);
			  
			  
			  //see bug 1452
			  {
			      BOOL field_is_struct = FALSE;
	 
			      switch(TY_kind(fty_idx)) {
			      case KIND_STRUCT:
				innermost_idx = Get_Field_Type(fty_idx, 1);
                                innermost_idx = Get_Innermost_Field_Type(innermost_idx);
                                field_is_struct = TRUE;
				break;
			      case KIND_ARRAY:
				innermost_idx = Get_Inner_Array_Type(fty_idx);
				if(TY_kind(innermost_idx) == KIND_STRUCT) {
				  innermost_idx = Get_Field_Type(innermost_idx, 1);
                                  innermost_idx = Get_Innermost_Field_Type(innermost_idx);
                                  field_is_struct = TRUE;
                                }
				break;
			      default:
				innermost_idx = 0;
			      }
			      if(double_inner_exception && double_align_exception) {
				if(field_is_struct && innermost_idx && TY_mtype(innermost_idx) == MTYPE_F8) {
				  FmtAssert(align == 8,("Contradictory  configuration file flags align_dbl_1st and align_dbl_inner"));
				  align = 4;
				}
			      }
			      
			      if(longlong_inner_exception && longlong_align_exception) {
				if(field_is_struct && innermost_idx && (TY_mtype(innermost_idx) == MTYPE_I8 || 
						     TY_mtype(innermost_idx) == MTYPE_U8)) {
				  FmtAssert(align == 8,("Contradictory  configuration file flags align_dbl_1st and align_dbl_inner"));
				  align = 4;
				}
			      }
			      
			      if(field_is_struct && innermost_idx && Type_Is_Shared_Ptr(innermost_idx, TRUE)) {
				if (sptr_align_exception && sptr_inner_exception &&
				    TY_To_Sptr_Idx(innermost_idx) == shared_ptr_idx) {
				  align = 4;
				}
				if(psptr_align_exception && psptr_inner_exception &&
				   TY_To_Sptr_Idx(innermost_idx) == pshared_ptr_idx) {
				  align = 4;
				}
			      } 
			  }
			  
			  if (FLD_is_bit_field(fld)) {
			      //use whatever the offset determined by front end
			      //see bug1303
			      displ = FLD_ofst(fld) - FLD_ofst(prev_fld);
			      prev_off = prev_off + displ;
			  } else {
			      if(!TY_is_union(idx))
				  displ = prev_off + Adjusted_Type_Size(prev_fld_ty);
			      if(displ % align == 0) 
				  prev_off = displ;
			      else 
				  prev_off = displ + (align - displ % align);
			  }
			  Set_FLD_adjusted_ofst(fld, prev_off);

			}
			
			if(!TY_is_union(idx))
			  prev_fld_ty = fty_idx;
			else {
			  //for unions keep the field with the largest size
			  if(TY_adjusted_size(prev_fld_ty) < TY_adjusted_size(fty_idx))
			    prev_fld_ty = fty_idx;
			}
			

			if ((TY_align (fty_idx) > align) || (TY_is_packed (fty_idx)))
				Set_TY_is_packed (ty);
			Set_FLD_type(fld, fty_idx);
			prev_fld = fld;
			fld = FLD_next(fld);
		}
		
		//now adjust the struct type size
		displ = prev_off;
		if (prev_fld != FLD_HANDLE() && FLD_is_bit_field(prev_fld)) {
		  //in case the last bit field wraps around word boundary... (bug1303)
		  int ofst = FLD_bofst(prev_fld) + FLD_bsize(prev_fld);
		  displ = prev_off + ofst / MTYPE_bit_size(Pointer_Mtype); 
		} else {
		  //see bug 1456
		  //For array types A_T_S returns the non-adjusted size for some reason.
		  //Ideally should change the implementation of that function.
		  if(TY_kind(prev_fld_ty) == KIND_ARRAY) 
		    displ = prev_off + TY_adjusted_size(prev_fld_ty);
		  else
		    displ = prev_off + Adjusted_Type_Size(prev_fld_ty);
		}
		align = TY_align(idx);
		if(displ % align == 0) 
		  prev_off = displ;
		else 
		  prev_off = displ + (align - displ % align);
		Set_TY_adjusted_size(idx,prev_off);
		

		//for forward declarations need to change the alignment
		if(orig_idx != 0 && orig_idx != idx) {
		  Set_TY_align(orig_idx, TY_align(idx));
		}
		
		if (tsize != 0) {
		  //try to fix incomplete struct with shared type.  
		  //Here we can not rely on using TYPE_TY_IDX
		  // to detect the forward declaration, since a shared struct and private struct have 
		  // different type_trees
		  //See bug442 
		  //Wei - this is inefficient. Why not use a multimap? 
		  for (int i = 0; i < incomplete_structs.size(); i++) {
		    TY_IDX tmp_idx = incomplete_structs[i];
		    char * ty_name = TY_name(tmp_idx);
		    if (strncmp(ty_name, TY_name(idx), 256) == 0) {
		      Set_TY_fld(tmp_idx, TY_fld(idx));
		      Set_TY_size(tmp_idx, TY_size(idx));
		      Set_TY_adjusted_size(tmp_idx, TY_adjusted_size(idx));
		      Set_TY_align(tmp_idx, TY_align(idx));
		      Patch_Incomplete_Arrays(tmp_idx);
		      incomplete_structs.erase(incomplete_structs.begin() + i);
		    }
		  }
		  Patch_Incomplete_Arrays(idx);
		} else {
		  //incomplete struct types in UPC mode will not have the TY_is_written()
		  //(this happens in finish_struct()).  So we set them here (see bug1323)
		  if (keep_decl_for_w2c) {
		    Set_TY_is_written(idx);
		  }
		}
		    

	} // end record scope
		break;
	case METHOD_TYPE:
		DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
	case FUNCTION_TYPE:
		{	// new scope for local vars
		tree arg;
		INT32 num_args;
		TY &ty = New_TY (idx);
		TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0); 
		Set_TY_align (idx, 1);
		TY_IDX ret_ty_idx;
		TY_IDX arg_ty_idx;
		TYLIST tylist_idx;

		// allocate TYs for return as well as parameters
		// this is needed to avoid mixing TYLISTs if one
		// of the parameters is a pointer to a function

		ret_ty_idx = Get_TY(TREE_TYPE(type_tree));
		if(TY_is_shared(ret_ty_idx)) {
		  error_with_file_and_line(input_filename, lineno, "Function returning a scalar shared value.");
		}
		for (arg = TYPE_ARG_TYPES(type_tree);
		     arg;
		     arg = TREE_CHAIN(arg))
			arg_ty_idx = Get_TY(TREE_VALUE(arg));

		// if return type is pointer to a zero length struct
		// convert it to void
		if (!WFE_Keep_Zero_Length_Structs    &&
		    TY_mtype (ret_ty_idx) == MTYPE_M &&
		    TY_size (ret_ty_idx) == 0) {
			// zero length struct being returned
		  	DevWarn ("function returning zero length struct at line %d", lineno);
			ret_ty_idx = Be_Type_Tbl (MTYPE_V);
		}

		Set_TYLIST_type (New_TYLIST (tylist_idx), ret_ty_idx);
		Set_TY_tylist (ty, tylist_idx);
		for (num_args = 0, arg = TYPE_ARG_TYPES(type_tree);
		     arg;
		     num_args++, arg = TREE_CHAIN(arg))
		{
			arg_ty_idx = Get_TY(TREE_VALUE(arg));
			if (!WFE_Keep_Zero_Length_Structs    &&
			    TY_mtype (arg_ty_idx) == MTYPE_M &&
			    TY_size (arg_ty_idx) == 0) {
				// zero length struct passed as parameter
				DevWarn ("zero length struct encountered in function prototype at line %d", lineno);
			}
			else
				Set_TYLIST_type (New_TYLIST (tylist_idx), arg_ty_idx);
		}
		if (num_args)
		{
			Set_TY_has_prototype(idx);
			if (arg_ty_idx != Be_Type_Tbl(MTYPE_V))
			{
				Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
				Set_TY_is_varargs(idx);
			}
			else
				Set_TYLIST_type (Tylist_Table [tylist_idx], 0);
		}
		else
			Set_TYLIST_type (New_TYLIST (tylist_idx), 0);

		} // end FUNCTION_TYPE scope
		break;
	default:
		FmtAssert(FALSE, ("Get_TY unexpected tree_type"));
	}

	if (TYPE_SHARED(type_tree)) {
	  idx = Make_Shared_Type(idx, Type_Tree_Block_Size(type_tree), 
	    TYPE_STRICT(type_tree) ? STRICT_CONSISTENCY :
	    TYPE_RELAXED(type_tree) ? RELAXED_CONSISTENCY : NO_CONSISTENCY);    
	}

	if (TY_kind(idx) == KIND_STRUCT && TY_size(idx) == 0 &&
	    TY_is_shared(idx)) {
	  /* It's an incomplete struct with shared type.
	   * Mark it down so we can correct it later
	   */
	  incomplete_structs.push_back(idx);
	}
	TYPE_TY_IDX(type_tree) = idx;

	/* Check for shared [] void *, which is now illegal in UPC 1.1
	   See bug507.
	 */
	if (TY_kind(idx) == KIND_POINTER) {
	  TY_IDX pointed = TY_pointed(idx);
	  if (TY_kind(pointed) == KIND_VOID && TY_is_shared(pointed) &&
	      TY_block_size(pointed) != 1) {
	    error("UPC forbids specifying a block size for a pointer to void type");
	  }
	}

	if(Debug_Level >= 2) {
	  struct mongoose_gcc_DST_IDX dst = 
	    Create_DST_type_For_Tree(type_tree,idx,orig_idx);
	  TYPE_DST_IDX(type_tree) = dst;
	}

        if(TY_adjusted_size(idx) == 0)
	  Set_TY_adjusted_size(idx, TY_size(idx));
	return idx;
}

ST*
Create_ST_For_Tree (tree decl_node)
{
  TY_IDX     ty_idx;
  ST*        st;
  string name;
  ST_SCLASS  sclass;
  ST_EXPORT  eclass;
  SYMTAB_IDX level;
  BOOL pstatic = FALSE;
  BOOL  fstatic = FALSE;
  char linebuf[64];

  if (TREE_CODE(decl_node) == ERROR_MARK)
    exit (RC_USER_ERROR);

  if (DECL_NAME (decl_node)) {
    tree enclosing_decl = DECL_CONTEXT(decl_node);
    name = IDENTIFIER_POINTER (DECL_NAME (decl_node));
    if (compiling_upc && DECL_LANG_FLAG_5(decl_node) == 1) {
      /* only mangle names if it's not from a .h file */
      string suffix = "";
      pstatic = enclosing_decl != NULL && TREE_STATIC(decl_node) && TREE_CODE(enclosing_decl) == FUNCTION_DECL;
      fstatic = enclosing_decl == NULL && !TREE_PUBLIC(decl_node);
      //rename static local/global variables by adding the hash value of (fn name + file name) to the variable
      if (pstatic) {
	suffix += "N" + (string)utoa(strlen(IDENTIFIER_POINTER(DECL_NAME(enclosing_decl)))) + 
	  "_" + (string) IDENTIFIER_POINTER(DECL_NAME(enclosing_decl)) + "_";
      }
      if (pstatic || fstatic) {
	suffix += mangle_name();
	name = "_N" + utoa(strlen(name.data())) + "_" + name;
	UINT hash_value = hash_val(suffix);
	name += "_N" + utoa(strlen(utoa(hash_value).data())) + "_" + utoa(hash_value) + "_";
	
      }
      if(TREE_CODE(decl_node) == VAR_DECL &&
	 Debug_Level >= 2 &&
	 (Scope_level > 2 || For_scope)) {
	name = "_N" + utoa(strlen(name.data())) + "_" + name;
      }
    }
    
    if( TREE_CODE(decl_node) != FUNCTION_DECL && DECL_LANG_FLAG_5(decl_node) == 1 && 
       ((Scope_level > 2 || For_scope) || pstatic || fstatic)
       && Debug_Level >= 2) {
      sprintf(linebuf,"%d", /*lineno*/ DECL_SOURCE_LINE(decl_node));
      name = "__BLN_" + name + "_L" + linebuf;
      if(fstatic)
	name = name + "_LL";
    }
    
  } else {
    DevWarn ("no name for DECL_NODE");
    name = "__unknown__";
  }

  switch (TREE_CODE(decl_node)) {
    
  case FUNCTION_DECL:
    { const char *srcfilepath = DECL_SOURCE_FILE(decl_node);

      for (int i = 0; i < num_header_dirs; i++) {
	char * dir = upc_header_dirs[i];
        int upchdrmatch = 0;
	if (*dir == '^') { /* leading ^ requests an exact path match */
          dir++;
          upchdrmatch = !strncmp(dir, srcfilepath, strlen(dir));
        } else { /* substring search */
          upchdrmatch = !!strstr(srcfilepath, dir);
        }
        if (upchdrmatch) {
	  //Do not emit the function's prototype
	  DECL_LANG_FLAG_5(decl_node) = 0;
	  break;
	} 
      }

      TY_IDX func_ty_idx = Get_TY(TREE_TYPE(decl_node));

      if (DECL_WIDEN_RETVAL (decl_node)) {
	tree type_tree = TREE_TYPE(decl_node);
	tree ret_type_tree = TREE_TYPE (type_tree);
	TY_IDX ret_ty_idx = Get_TY(ret_type_tree);
	if (MTYPE_signed (TY_mtype (ret_ty_idx)))
	  TREE_TYPE (type_tree) = long_long_integer_type_node;
	else
	  TREE_TYPE (type_tree) = long_long_unsigned_type_node;
	TY_IDX old_func_ty_idx = func_ty_idx;
	func_ty_idx = Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
	TREE_TYPE (type_tree) = ret_type_tree;
	TYPE_TY_IDX(type_tree) = old_func_ty_idx;
      }
      
      sclass = SCLASS_EXTERN;
      eclass = TREE_PUBLIC(decl_node) ? EXPORT_PREEMPTIBLE : EXPORT_LOCAL;
      level  = GLOBAL_SYMTAB+1;
      
      PU_IDX pu_idx;
      PU&    pu = New_PU (pu_idx);

      PU_Init (pu, func_ty_idx, level);
      
      st = New_ST (GLOBAL_SYMTAB);
      char *an;
      
      if (compiling_upc) {
	  //Save_Upc_Rts_Calls((char *) name.c_str(), st);
	an = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node));
	Upc_Translate_Name((char *) name.c_str(), &an);
      }  else {
	an = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node));
      }
      if (strcmp(an, "main") == 0) {
	an = "user_main";
      }
      
      ST_Init (st,
	       Save_Str (an),
	       CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));

    }
    break;
    
  case PARM_DECL:
  case VAR_DECL:
    {
      if (TREE_CODE(decl_node) == PARM_DECL) {
	sclass = SCLASS_FORMAL;
	eclass = EXPORT_LOCAL;
	level = CURRENT_SYMTAB;
      }
      else {
	if (DECL_CONTEXT (decl_node) == 0) {
	  if (TREE_PUBLIC (decl_node)) {
	    if (DECL_INITIAL(decl_node))
	      sclass = SCLASS_DGLOBAL;
	    else if (TREE_STATIC(decl_node)) {
	      if (flag_no_common || DECL_SECTION_NAME(decl_node))
		sclass = SCLASS_UGLOBAL;
	      else
		sclass = SCLASS_COMMON;
	    }
	    else
	      sclass = SCLASS_EXTERN;
	    eclass = EXPORT_PREEMPTIBLE;
	  }
	  else {
	    sclass = SCLASS_FSTATIC;
	    eclass = EXPORT_LOCAL;
	  }
	  level = GLOBAL_SYMTAB;
	}
	else {
	  if (DECL_EXTERNAL(decl_node)) {
	    sclass = SCLASS_EXTERN;
	    level  = GLOBAL_SYMTAB;
	    eclass = EXPORT_PREEMPTIBLE;
	  }
	  else {
	    if (TREE_STATIC (decl_node)) {
	      sclass = SCLASS_PSTATIC;
	      if (pstatic_as_global)
		level = GLOBAL_SYMTAB;
	      else
		level = CURRENT_SYMTAB;
	    }
	    else {
	      sclass = SCLASS_AUTO;
	      level = decl_node->decl.symtab_idx ?
		decl_node->decl.symtab_idx : CURRENT_SYMTAB;
              }
	    eclass = EXPORT_LOCAL;
	  }
	}
      }
      st = New_ST (level);
      ty_idx = Get_TY (TREE_TYPE(decl_node));

      if(TY_size(ty_idx) == 0 && TY_kind(ty_idx) == KIND_STRUCT && sclass != SCLASS_EXTERN) {
	error_with_file_and_line(input_filename, lineno, "Unknown storage size");
      }
      
      // UPC
      if (TY_kind(ty_idx) == KIND_SCALAR && TREE_THIS_SHARED(decl_node)) {
	TYPE_TY_IDX(TREE_TYPE(decl_node)) = ty_idx;
      }


      if (TY_kind (ty_idx) == KIND_ARRAY &&
            TREE_STATIC (decl_node) &&
            DECL_INITIAL (decl_node) == FALSE &&
            TY_size (ty_idx) == 0) {
	Set_TY_size (ty_idx, TY_size (Get_TY (TREE_TYPE (TREE_TYPE (decl_node)))));
      }
      if (TY_mtype (ty_idx) == MTYPE_M &&
	  Aggregate_Alignment > 0 &&
	  Aggregate_Alignment > TY_align (ty_idx))
	Set_TY_align (ty_idx, Aggregate_Alignment);

      //Top level qualifiers are set on the decl node
      if (TREE_READONLY(decl_node)) 
	Set_TY_is_const (ty_idx);
      if (TREE_THIS_VOLATILE(decl_node)) {
	if (TY_kind(ty_idx) != KIND_ARRAY) {
	  //don't do this for array (see bug1435)
	  Set_TY_is_volatile (ty_idx);
	}
      }
      if (TYPE_RESTRICT(TREE_TYPE(decl_node))) {
	Set_TY_is_restrict(ty_idx);
      }

      ST_Init (st, Save_Str(name.c_str()), CLASS_VAR, sclass, eclass, ty_idx);
      if (TREE_CODE(decl_node) == PARM_DECL) {
	Set_ST_is_value_parm(st);
      }

      /* UPC specific */
      if(compiling_upc) {
	if (TREE_CODE(decl_node) == VAR_DECL) {
	  bool shared = TY_is_shared(ty_idx);
	  unsigned int thread_dim = 0;
	  tree decl_type = TREE_TYPE(decl_node);	  
	  if (TY_kind(ty_idx) == KIND_POINTER && TY_is_shared(TY_pointed(ty_idx))) {
	    //a local pointer to shared data
	    if (is_star(TYPE_BLOCK_SIZE(TREE_TYPE(decl_type)))) {
	      error("[*] qualifier may not be used in declaration of pointers");
	    }
	  }
	  
	  if (TY_is_shared(ty_idx)) {
	    if (TY_kind(ty_idx) == KIND_VOID) {
	      	/* This could happen for upc_lock_t (declared as "shared void" in upc.h)
		 * This is an user error (upc_lock_t is an opaque type and should not be manipulated directly) 
		 */
	      error ("\'%s\' is declared as having shared void type (upc_lock_t?)", ST_name(st));
	      break; // Avoids crash somewhere below (bug244)
	    }

	    UINT bsize = Get_Type_Block_Size(ty_idx);
	    int elt_shared = 0;
	    if (TREE_CODE(decl_type) == ARRAY_TYPE) {
	      //find the dimension that contains THREADS, and perform the appropriate checks
	      elt_shared = TYPE_SHARED(TREE_TYPE(decl_type)) 
		|| (TREE_CODE(TREE_TYPE(decl_type)) == POINTER_TYPE && TYPE_SHARED(TREE_TYPE(TREE_TYPE(decl_type))));
	      int i = 1;
	      for (tree base_type = decl_type; TREE_CODE(base_type) == ARRAY_TYPE; base_type = TREE_TYPE(base_type), i++) {
		if (UPC_TYPE_HAS_THREADS(base_type)) {
		  TY_IDX tidx = Get_TY(base_type);
		  Set_TY_uses_threads(tidx);
		  thread_dim = i;
		  break;
		}
	      }
	      if (bsize == 0 && thread_dim > 0) {
		//we have an error here, since arrays with indef block size can't have threads in its dimension
		error("In the dynamic translation environment, THREADS may not appear in declarations of shared arrays with indefinite block size. Offending variable: %s", name.c_str());
	      }
	      if (bsize != 0 && threads_int == 0 && thread_dim == 0) {
		error("In the dynamic translation environment, THREADS must appear exactly once in declarations of shared arrays with definite block size.  Offending variable: %s", name.c_str());
	      }
	    } else if (is_star(TYPE_BLOCK_SIZE(decl_type))) {
	      error("[*] qualifier is only allowed for arrays");
	    }

	    // If allocation blocksize of an array is too big for size_t, then we risk
	    // letting the backend compiler truncate it.  That results in a warning at
	    // compile time, but leads to an incorrect under-allocation at runtime.
	    // So, we raise the error here (see bugs 247 and 2587). -PHH
	    if (TY_kind(ty_idx) == KIND_ARRAY) {
		UINT64 size = get_real_size(ty_idx);
		if (bsize != 0) {
		    UINT64 eltsize = get_real_size(Get_Inner_Array_Type(ty_idx));
		    size = min(bsize*eltsize, size);
		}
		if (size > Get_Integer_Value(TYPE_MAX_VALUE(sizetype))) {
		    error ("size of array `%s' is too large", name.c_str());
		}
	    }

	    //fprintf(stderr, "SHARED: %s %d %d\n", ST_name(st), ty_idx, ST_st_idx(st));
	    upc_st_orig_ty[ST_st_idx(st)] = thread_dim;
	  } else if ((DECL_CONTEXT(decl_node) == 0 || TREE_STATIC(decl_node)) || DECL_EXTERNAL(decl_node)) {
	    if (name != "MYTHREAD" && name != "THREADS" &&
		name != "UPCR_SHARED_SIZE" && name != "UPCR_PSHARED_SIZE") { 	      
	      upc_tld.insert(ST_st_idx(st));
	      //fprintf(stderr, "TLD: %s %d %d\n", ST_name(st), ty_idx, ST_st_idx(st));
	    }
	  }
	}
	
	if (strcmp(name.c_str(), "MYTHREAD") == 0) 
	  upc_mythread_st = st;
	if (strcmp(name.c_str(), "THREADS") == 0) 
	  upc_threads_st = st;
      }
    }
    break;
    
  default:
    {
      Fail_FmtAssertion ("Create_ST_For_Tree: unexpected tree type");
    }
      break;
  }

  DECL_ST(decl_node) = st;

  if ((DECL_WEAK (decl_node)) && (TREE_CODE (decl_node) != PARM_DECL)) {
    Set_ST_is_weak_symbol (st);
  }

#if 0
  {
    tree enum_node = TREE_TYPE(decl_node);

    while (enum_node &
           (TREE_CODE(enum_node) == ARRAY_TYPE ||
            TREE_CODE(enum_node) == FUNCTION_TYPE ||
            TREE_CODE(enum_node) == POINTER_TYPE ||
            TREE_CODE(enum_node) == REFERENCE_TYPE)) {
      if (TREE_CODE(enum_node) == ARRAY_TYPE)
        enum_node = get_inner_array_type(enum_node);
      else
        enum_node = TREE_TYPE(enum_node);
    }

    if (enum_node && (TREE_CODE(enum_node) == ENUMERAL_TYPE)) {
      const char *kind = (TREE_CODE(decl_node) == FUNCTION_DECL ? "function" :
                          (TREE_CODE(decl_node) == PARM_DECL ? "paramater" : "variable"));
      fprintf(stderr, "@ %s '%s' involves enum type '%s'\n", kind, name.c_str(), Get_Name(TYPE_NAME(enum_node)));
    }
  }
#endif

  if (DECL_SECTION_NAME (decl_node)) {
    if (TREE_CODE (decl_node) == FUNCTION_DECL)
      level = GLOBAL_SYMTAB;
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
                  Save_Str (TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node))));
    Set_ST_has_named_section (st);
  }

  if (DECL_SYSCALL_LINKAGE (decl_node)) {
	Set_PU_has_syscall_linkage (Pu_Table [ST_pu(st)]);
  }
  if(Debug_Level >= 2) {
     struct mongoose_gcc_DST_IDX dst =
       Create_DST_decl_For_Tree(decl_node,st);
     DECL_DST_IDX(decl_node) = dst;
  }
  
  if(DECL_LANG_FLAG_5(decl_node) == 1) 
    Set_ST_keep_name_w2f(st);
  else 
    Clear_ST_keep_name_w2f(st);

  

  if(Debug_Level >= 2 && (Scope_level > 2 || For_scope || pstatic || fstatic)) {
    if(ST_class(st) != CLASS_FUNC) {
      if(fstatic)
	file_scope_statics.push(st);
      else 
	inner_scope_vbles.top()->push(st);
    }
    // fprintf(stderr, "Level = %d , line = %d,  name = %s \n", Scope_level, lineno, name.data());
    }
  return st;
}


void Pad_Field_for_UPC_Align(tree type, 
			     unsigned int actual_align, unsigned int known_align, 
			     unsigned int desired_align) {
  TY_IDX ty_idx;
  
 
 
}
