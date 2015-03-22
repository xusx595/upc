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


// translate gnu decl trees to whirl
#if (defined(__MACH__) && defined(__APPLE__)) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#else
#include <values.h>
#endif
#include <sys/types.h>
#include <elf.h>
#include "defs.h"
#include "errors.h"
#include "gnu_config.h"
extern "C" {
#include "gnu/flags.h"
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/toplev.h"
#include "function.h"
#include "c-pragma.h"
#include "c-tree.h"
}
#ifdef TARG_IA32
// the definition in gnu/config/i386/i386.h causes problem
// with the enumeration in common/com/ia32/config_targ.h
#undef TARGET_PENTIUM
#endif /* TARG_IA32 */

#include "glob.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "const.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "tree_symtab.h"
#include "wfe_decl.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include <stack>
extern FILE *tree_dump_file; // for debugging only

extern string utoa(UINT64 i); // in shared-alloc.cxx

extern PU_Info *PU_Tree_Root;
static PU_Info *PU_Info_Table     [258] = {0};
static ST      *Return_Address_ST [258] = {0};
static INT32    Save_Expr_Table   [258] = {0};
extern INT32    wfe_save_expr_stack_last;
static BOOL map_mempool_initialized = FALSE;
static MEM_POOL Map_Mem_Pool;
ST* WFE_Vararg_Start_ST;

// Because we build inito's piecemeal via calls into wfe for each piece,
// need to keep track of current inito and last initv that we append to.
static INITO_IDX aggregate_inito = 0;
static INITV_IDX last_aggregate_initv = 0;	
static BOOL not_at_root = FALSE;

static int __ctors = 0;
static int __dtors = 0;

extern "C" tree lookup_name (tree);
extern WN *last_ptr_arith_wn;

/* Generate WHIRL representing an asm at file scope (between
  functions). This is an awful hack. */
void
WFE_Assemble_Asm(char *asm_string)
{

  ST *asm_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(asm_st,
	  Str_To_Index (Save_Str (asm_string),
			Global_Strtab),
	  CLASS_NAME,
	  SCLASS_UNKNOWN,
	  EXPORT_LOCAL,
	  (TY_IDX) 0);

  Set_ST_asm_function_st(*asm_st);

  WN *func_wn = WN_CreateEntry(0,
			       asm_st,
			       WN_CreateBlock(),
			       NULL,
			       NULL);

  /* Not sure how much setup of WN_MAP mechanism, etc. we need to do.
   * Pretty certainly we need to set up some PU_INFO stuff just to get
   * this crazy hack of a FUNC_ENTRY node written out to the .B file.
   */

  /* This code patterned after "wfe_decl.cxx":WFE_Start_Function, and
     specialized for the application at hand. */

#ifdef ASM_NEEDS_WN_MAP
    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    /* set up the mem pool for the map table and predefined mappings */
    if (!map_mempool_initialized) {
        MEM_POOL_Initialize(&Map_Mem_Pool,"Map_Mem_Pool",FALSE);
        map_mempool_initialized = TRUE;
    } else {
        MEM_POOL_Pop(&Map_Mem_Pool);
    }

    MEM_POOL_Push(&Map_Mem_Pool);

    /* create the map table for the next PU */
    (void)WN_MAP_TAB_Create(&Map_Mem_Pool);
#endif

    // This non-PU really doesn't need a symbol table and the other
    // trappings of a local scope, but if we create one, we can keep
    // all the ir_bread/ir_bwrite routines much more blissfully
    // ignorant of the supreme evil that's afoot here.

    FmtAssert(CURRENT_SYMTAB == GLOBAL_SYMTAB,
	      ("file-scope asm must be at global symtab scope."));

    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    if (Show_Progress) {
      fprintf (stderr, "Asm(%s)\n", ST_name (asm_st));
      fflush (stderr);
    }
    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, func_wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(asm_st);
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (asm_st,/*tree=*/0);
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;
    else
      PU_Tree_Root = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;

  /* This code patterned after "wfe_decl.cxx":WFE_Finish_Function, and
     specialized for the application at hand. */

    // write out all the PU information
    pu_info = PU_Info_Table [CURRENT_SYMTAB];

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    PU_Init(pu, (TY_IDX) 0, CURRENT_SYMTAB);
    Set_PU_no_inline(pu);
    Set_PU_no_delete(pu);
    Set_ST_pu(*asm_st, pu_idx);

    Write_PU_Info (pu_info);

    // What does the following line do?
    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    Delete_Scope(CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
}

extern "C" void WFE_Set_Consistency(char *, int);

void WFE_Set_Consistency (char *descriptor, int action)
{
  
  if(!compiling_upc)
    return;

  WN_PRAGMA_ID prdesc;
  
  if (strcmp(descriptor, "relaxed") == 0) {
    switch (action) {
    case START_CONSISTENCY_SCOPE:
      prdesc = WN_PRAGMA_UPC_RELAXED_CONSISTENCY_START;
      break;
    case END_CONSISTENCY_SCOPE:
      prdesc = WN_PRAGMA_UPC_RELAXED_CONSISTENCY_STOP;
      break;
    default:
      Is_True(0,("Unsupported consistency action",""));
    }
  } else if (strcmp(descriptor, "strict") == 0) {
    switch (action) {
    case START_CONSISTENCY_SCOPE:
      prdesc = WN_PRAGMA_UPC_STRICT_CONSISTENCY_START;
      break;
    case END_CONSISTENCY_SCOPE:
      prdesc = WN_PRAGMA_UPC_STRICT_CONSISTENCY_STOP;
      break;
    default:
      Is_True(0,("Unsupported consistency action",""));
    }
  } else if(strcmp(descriptor, "upc_code") == 0) {
	keep_decl_for_w2c = 1;	  
	return;
  } else 
    Is_True(0,("Unsupported consistency descriptor",""));
  
  WN *wn = WN_CreatePragma(prdesc,(ST_IDX) 0,0,0);
  WFE_Stmt_Append(wn,Get_Srcpos());
  
}



extern void
WFE_Start_Function (tree fndecl)
{

    if (CURRENT_SYMTAB != GLOBAL_SYMTAB) {

      Set_PU_uplevel (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
    }

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    /* set up the mem pool for the map table and predefined mappings */
    if (!map_mempool_initialized) {
        MEM_POOL_Initialize(&Map_Mem_Pool,"Map_Mem_Pool",FALSE);
        map_mempool_initialized = TRUE;
    } else {
        MEM_POOL_Pop(&Map_Mem_Pool);
    }

    MEM_POOL_Push(&Map_Mem_Pool);

    /* create the map table for the next PU */
    (void)WN_MAP_TAB_Create(&Map_Mem_Pool);

    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    // handle VLAs in the declaration
    WN *vla_block = WN_CreateBlock ();
    WFE_Stmt_Push (vla_block, wfe_stmk_func_body, Get_Srcpos());

    ST        *func_st;
    ST_EXPORT  eclass = TREE_PUBLIC(fndecl) ? EXPORT_PREEMPTIBLE
                                            : EXPORT_LOCAL;

    if (DECL_INLINE (fndecl) && TREE_PUBLIC (fndecl)) {
      if (DECL_EXTERNAL (fndecl) && DECL_ST2 (fndecl) == 0) {
        // encountered first extern inline definition
        ST *oldst = DECL_ST (fndecl);
        DECL_ST (fndecl) = 0;
        func_st =  Get_ST (fndecl);
        DECL_ST (fndecl) = oldst;
        DECL_ST2 (fndecl) = func_st;
        eclass = EXPORT_LOCAL;
      }
      else {
        // encountered second definition, the earlier one was extern inline
        func_st = Get_ST (fndecl);
        DECL_ST2 (fndecl) = 0;
      }
    }
    else
      func_st = Get_ST (fndecl);

    if(keep_decl_for_w2c) {
      Set_ST_keep_name_w2f(func_st);
    }  else {
      Clear_ST_keep_name_w2f(func_st);
    }
    

    Set_ST_sclass (func_st, SCLASS_TEXT);
    Set_PU_lexical_level (Pu_Table [ST_pu (func_st)], CURRENT_SYMTAB);
    Set_PU_c_lang (Pu_Table [ST_pu (func_st)]);

    if (DECL_INLINE(fndecl)) {
      Set_PU_is_inline_function (Pu_Table [ST_pu (func_st)]);
      wfe_invoke_inliner = TRUE;
    }
    Set_ST_export (func_st, eclass);

    if (Show_Progress) {
      fprintf (stderr, "Compiling %s \n", ST_name (func_st));
      fflush (stderr);
    }

    INT num_args = 0;
    tree pdecl;
    for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl)) {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else
	++num_args;
    }

    Scope_tab [Current_scope].st = func_st;

    WN *body, *wn;
    body = WN_CreateBlock ( );
    wn = WN_CreateEntry ( num_args, func_st, body, NULL, NULL );

    //Implement the scheme for global pragmas as described in bug1056
    if (compiling_upc && global_pragmas.size() > 0) {
      WN* pragmas = WN_CreateBlock();
      for (int i = 0; i < global_pragmas.size(); i++) {
	WN_INSERT_BlockLast(pragmas, global_pragmas[i]);
      }
      global_pragmas.clear();
      WN_func_pragmas(wn) = pragmas;
    }

    /* from 1..nkids, create idname args */
    INT i = 0;
    for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl) )
    {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      ST *st = Get_ST(pdecl);
      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else {
        if (TY_mtype (arg_ty_idx) == MTYPE_F4 &&
            !TY_has_prototype (ST_pu_type (func_st)))
          Set_ST_promote_parm (st);
          WN_kid(wn,i) = WN_CreateIdname ( 0, ST_st_idx(st) );
          ++i;
      }
    }

    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(func_st);
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (func_st,fndecl);
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;

    else
    if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1)
      PU_Tree_Root = pu_info;

    else
      PU_Info_child (PU_Info_Table [CURRENT_SYMTAB -1]) = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;
    Save_Expr_Table [CURRENT_SYMTAB] = wfe_save_expr_stack_last;

    WFE_Stmt_Pop (wfe_stmk_func_body);

    WFE_Stmt_Push (wn, wfe_stmk_func_entry, Get_Srcpos());
    WFE_Stmt_Push (body, wfe_stmk_func_body, Get_Srcpos());
    wn = WN_CreatePragma (WN_PRAGMA_PREAMBLE_END, (ST_IDX) NULL, 0, 0);
    WFE_Stmt_Append (wn, Get_Srcpos());
    WFE_Stmt_Append (vla_block, Get_Srcpos());
    WFE_Set_Consistency(IDENTIFIER_POINTER (current_consistency_stack->value), 
			START_CONSISTENCY_SCOPE);
    PU& pu = Get_Current_PU();
    Set_PU_prototype(pu, ST_pu_type(func_st)); 

    WFE_Vararg_Start_ST = NULL;
    if (current_function_varargs) {
      // the function uses varargs.h
      // throw off the old type declaration as it did not 
      // take into account any arguments
      PU& pu = Pu_Table[ST_pu (func_st)];
      TY_IDX ty_idx;
      TY &ty = New_TY (ty_idx);
      TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
      Set_TY_align (ty_idx, 1);
      TYLIST tylist_idx;
      Set_TYLIST_type (New_TYLIST (tylist_idx),
                       Get_TY(TREE_TYPE(TREE_TYPE(fndecl))));
      Set_TY_tylist (ty, tylist_idx);
      for (pdecl = DECL_ARGUMENTS (fndecl); pdecl; pdecl = TREE_CHAIN (pdecl) ) {
	WFE_Vararg_Start_ST = Get_ST(pdecl);
        Set_TYLIST_type (New_TYLIST (tylist_idx), ST_type(WFE_Vararg_Start_ST));
      }
      Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
      Set_TY_is_varargs (ty_idx);
      Set_PU_prototype (pu, ty_idx);
    }
   
}

/* functions for the initialization of global data */
extern void add_shared_symbol(ST_IDX st, int thread_dim);
extern void add_TLD_symbol(ST_IDX st);

void add_symbols() {

  /**
   *  This seems like a good spot to add the global symbols...
   */
  if (compiling_upc) {
    std::map<ST_IDX,int>::iterator i = upc_st_orig_ty.begin();
    for(;i != upc_st_orig_ty.end(); i++) {
      add_shared_symbol(i->first, i->second);
    }
    /* bug 2299 - some intialized symbols are dependendent on other symbols
       make sure we output the unitialized ones first */
    //for (std::map<ST_IDX, ST*>::iterator i = upc_tld.begin(); i!= upc_tld.end(); i++) {
    for (std::set<ST_IDX>::iterator i = upc_tld.begin(); i!= upc_tld.end(); i++) {
      if(!ST_is_initialized(ST_ptr(*i))) {
	add_TLD_symbol(*i);
      } 
    }
    for (std::set<ST_IDX>::iterator i = upc_tld.begin(); i!= upc_tld.end(); i++) {
      if(ST_is_initialized(ST_ptr(*i))) {
	add_TLD_symbol(*i);
      } 
    }
    //clear the two maps...
    upc_st_orig_ty.clear();
    upc_tld.clear();
  }
}

static bool has_return_end(WN* wn) {

  switch(WN_operator(wn)) {
  case OPR_BLOCK: 
    {
      for (WN* last = WN_last(wn); last != NULL; last = WN_prev(last)) {
	if (WN_operator(last) == OPR_PRAGMA) {
	  continue;
	}
	return has_return_end(last);
      }
      return false;
    }
  case OPR_IF: 
    return has_return_end(WN_then(wn)) && has_return_end(WN_else(wn));
  case OPR_RETURN:
  case OPR_RETURN_VAL:
    return 1;
  case OPR_DO_WHILE:
  case OPR_WHILE_DO:
    return has_return_end(WN_while_body(wn));
  default: /* are there other nodes that's missing? */
    return false;
  }
}

extern void
WFE_Finish_Function (void)
{
    WFE_Check_Undefined_Labels ();

    /**
     *  This seems like a good spot to add the global symbols...
     *  Costin - adding global symbols here conflicts with generating debug information
     *           for file scope static vbles and  forces a non-uniform mangling scheme.
     *           Leave as is and implement exception to mangling rules.  
    */
    add_symbols();

    PU_Info *pu_info = PU_Info_Table [CURRENT_SYMTAB];
    wfe_save_expr_stack_last = Save_Expr_Table [CURRENT_SYMTAB];

    if (CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) {

      DevWarn ("Encountered nested function");
      Set_PU_is_nested_func (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
    }

    WFE_Set_Consistency(IDENTIFIER_POINTER (current_consistency_stack->value),
			END_CONSISTENCY_SCOPE);
    // write out all the PU information
    WN *wn = WFE_Stmt_Pop (wfe_stmk_func_body);

    // Insert a RETURN at the end if it does not exist
    if (!has_return_end(wn)) {
      PU func = Get_Current_PU();
      TY_IDX ret_ty = TY_ret_type(PU_prototype(func));
      if (TY_kind(ret_ty) != KIND_VOID) {
	ST* st = Get_Current_PU_ST();
	if (strncmp(ST_name(st), "user_main", 9) != 0) {
	  warning("control reaches the end of non-VOID function");
	}
      }
      WN* ret_val;
      if (TY_mtype(ret_ty) >= MTYPE_I1 && TY_mtype(ret_ty) <= MTYPE_U8) {
	ret_val = WN_Intconst(TY_mtype(ret_ty), 0);
	WN_INSERT_BlockLast(wn, WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(ret_val), MTYPE_V, ret_val));
      } else if (TY_mtype(ret_ty) >= MTYPE_F4 && TY_mtype(ret_ty) <= MTYPE_F16) {
	ret_val = WN_Intconst(MTYPE_I8, 0);
	WN_INSERT_BlockLast(wn, WN_CreateReturn_Val(OPR_RETURN_VAL, TY_mtype(ret_ty), MTYPE_V, ret_val));
      } else {
	WN_INSERT_BlockLast (wn, WN_CreateReturn ());
      }
    }

    WN *func_wn = WFE_Stmt_Pop (wfe_stmk_func_entry);
    if (PU_has_syscall_linkage (Get_Current_PU ())) {
      Set_PU_no_inline (Get_Current_PU ());
    }

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    Write_PU_Info (pu_info);

    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    if (Return_Address_ST [CURRENT_SYMTAB]) {
      Set_PU_has_return_address (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
      Return_Address_ST [CURRENT_SYMTAB] = NULL;
    }

    Delete_Scope (CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
//  if (CURRENT_SYMTAB > GLOBAL_SYMTAB)
//    Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];
}

/* Fix bug7:  Handle the case when a extern declaration is followed 
 * by its definition in the same file.  We output only its definition 
 * (at the location of the earlier declaration, so its address can be
 *  used to as initialization expressions) if there's no PU between 
 * the declaration and the definition, otherwise we output both
 * the declaration and definition in the same order
 *
*/

static void Add_Definition(ST* st, tree decl) {

    TY_IDX ty = Get_TY(TREE_TYPE(decl));
    if (ty != ST_type(st)) {
	//the case where there's a type mismatch between declaration and definition
	bool err = true;
	if (TY_kind(ty) == KIND_ARRAY && TY_kind(ST_type(st)) == KIND_ARRAY) {
	    //for arrays, the last dimension in the declaration is allowed to be unspecified
	    if (TY_etype(ty) == TY_etype(ST_type(st)) &&
		TY_adjusted_size(ST_type(st)) == 0) {
		err = false;
	    }
	} else {
	  //see bug 1338 - need to check for qualifiers 
	  if((TY_IDX_index(ty) == TY_IDX_index(ST_type(st))) || 
	     Types_Are_Equiv(ty,ST_type(st))) {
	    //I'm not exactly sure how safe is calling Types_Are_Equiv
	    //T_A_E tries to go one level down (ptr,array) and
	    // returns true if the sizes match
	    //However - if the error is real - the gnu front-end
	    // will catch it. (shared [] int64 * vs shared [] double *)
	    err = false;
	  } 
	  
	}
	if (err) 
	    error("conflicting types for %s", ST_name(st));
    }

  if (DECL_LANG_FLAG_5(decl)) {
    Set_ST_keep_name_w2f(st);
  }
 
  if (!TY_is_shared(ST_type(st))) {
    upc_tld.insert(ST_st_idx(st));
  }
}

void
WFE_Start_Aggregate_Init (tree decl)
{
  /* needed for variables with array initializers */
  if (keep_decl_for_w2c) 
    DECL_LANG_FLAG_5(decl) = 1;  

  if (compiling_upc) {
    ST* st = Get_ST(decl);
    if (ST_sclass(st) == SCLASS_EXTERN && DECL_CONTEXT(decl) == 0) {
      /* Fix bug7:  handle the case when an extern decl is followed 
	 by its definition with an array initializer */
      Set_ST_sclass(st, SCLASS_DGLOBAL);
      Add_Definition(st, decl);
    }
  }

  if (TREE_STATIC(decl)) {
    ST *st = Get_ST(decl);
    Set_ST_is_initialized(st);
    if (ST_sclass(st) == SCLASS_UGLOBAL ||
	ST_sclass(st) == SCLASS_EXTERN  ||
	ST_sclass(st) == SCLASS_COMMON)
      Set_ST_sclass(st, SCLASS_DGLOBAL);
    aggregate_inito = New_INITO (st);
    not_at_root = FALSE;
    last_aggregate_initv = 0;
  } 
}

static void
WFE_Add_Aggregate_Init_Zeros (INT count, TY_IDX ety)
{
  if (aggregate_inito == 0) return;
  if (count <= 0) return;

  INITV_IDX inv = New_INITV();
  TYPE_ID mtype = TY_mtype(ety);
  if (MTYPE_is_integral(mtype) || MTYPE_is_pointer(mtype))
    INITV_Init_Integer (inv, mtype, 0, count);
  else if (MTYPE_is_float(mtype))
    INITV_Init_Float (inv, mtype, 0.0, count);
  else
    error("Unsupported element type in sparse initializer");

  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Padding (INT size)
{
  if (aggregate_inito == 0) return;
  //if (size < 0) return;	// actually happens from assemble_zeroes
  if (size < 0) {
    //WEI: We still need to pad here (ow the init program will break), but make the size of pad to 0
    size = 0;
  }

  INITV_IDX inv = New_INITV();
  INITV_Init_Pad (inv, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Integer (INT64 val, INT size, INT is_unsigned = 0)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TYPE_ID mtype;
  if (size == 1) mtype = MTYPE_I1;
  else if (size == 2) mtype = MTYPE_I2;
  else if (size == 4) mtype = MTYPE_I4;
  else if (size == 8) mtype = MTYPE_I8;
  else FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Integer unexpected size"));

  if (is_unsigned) {
    mtype = MTYPE_complement(mtype);
  }

  INITV_Init_Integer (inv, mtype, val);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

static void
WFE_Add_Init_Block(void)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv_blk = New_INITV();
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv_blk);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv_blk);
  last_aggregate_initv = inv_blk;
}

void 
WFE_Add_Aggregate_Init_Real (REAL_VALUE_TYPE real, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    tc;
  int     t1;
  int     buffer [4];
  switch (size) {
  case 4:
    REAL_VALUE_TO_TARGET_SINGLE (real, t1);
    tc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
    break;
  case 8:
    REAL_VALUE_TO_TARGET_DOUBLE (real, buffer);
    tc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
    break;
  case 12:
  case 16:
    REAL_VALUE_TO_TARGET_DOUBLE (real, buffer);
    tc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
    break;
  default:
    /* For other possible sizes of long double */
    if (size == base_ty_size[_LONGDOUBLE][0]) {
      REAL_VALUE_TO_TARGET_DOUBLE (real, buffer);
      tc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
    } else {
      FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Real unexpected size"));
    }
    break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
} /* WGE_Add_Aggregate_Init_Real */

void 
WFE_Add_Aggregate_Init_Complex (REAL_VALUE_TYPE rval, REAL_VALUE_TYPE ival, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    rtc;
  TCON    itc;
  int     t1;
  int     buffer [4];
  switch (size) {
    case 8:
      REAL_VALUE_TO_TARGET_SINGLE (rval, t1);
      rtc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
      REAL_VALUE_TO_TARGET_SINGLE (ival, t1);
      itc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
      break;
    case 16:
      REAL_VALUE_TO_TARGET_DOUBLE (rval, buffer);
      rtc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
      REAL_VALUE_TO_TARGET_DOUBLE (ival, buffer);
      itc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
      break;
    default:
      FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Complex unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(rtc), 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
  inv = New_INITV();
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(itc), 1);
  Set_INITV_next(last_aggregate_initv, inv);
  last_aggregate_initv = inv;
}

void 
WFE_Add_Aggregate_Init_String (const char *s, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_String (inv, (char *) s, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Symbol (ST *st, WN_OFFSET offset = 0)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff (inv, st, offset);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Label (LABEL_IDX lab)
{
  DevWarn ("taking address of a label at line %d", lineno);
  Set_PU_no_inline (Get_Current_PU ());
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Label (inv, lab, 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
  Set_LABEL_addr_saved (lab);
}

void
WFE_Add_Aggregate_Init_Address (tree init)
{
  switch (TREE_CODE (init)) {

  case VAR_DECL:
  case FUNCTION_DECL:
	WFE_Add_Aggregate_Init_Symbol (Get_ST (init));
	break;

  case STRING_CST:
	{
	TCON tcon = Host_To_Targ_String (MTYPE_STRING,
				       TREE_STRING_POINTER(init),
				       TREE_STRING_LENGTH(init));
	ST *const_st = New_Const_Sym (Enter_tcon (tcon), 
		Get_TY(TREE_TYPE(init)));
      	WFE_Add_Aggregate_Init_Symbol (const_st);
	}
    	break;

  case PLUS_EXPR:
	if ( TREE_CODE(TREE_OPERAND(init,0)) == ADDR_EXPR
	  && TREE_CODE(TREE_OPERAND(init,1)) == INTEGER_CST)
	{
		tree addr_kid = TREE_OPERAND(TREE_OPERAND(init,0),0);
		FmtAssert(TREE_CODE(addr_kid) == VAR_DECL
			|| TREE_CODE(addr_kid) == FUNCTION_DECL,
			("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symbol ( Get_ST (addr_kid),
			Get_Integer_Value(TREE_OPERAND(init,1)) );
	}
	else
	{
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
		WN_Delete (init_wn);
	}
	break;

  case INTEGER_CST:
	WFE_Add_Aggregate_Init_Integer (Get_Integer_Value (init), Pointer_Size);
	break;

  case LABEL_DECL:
	{
	 	LABEL_IDX label_idx = WFE_Get_LABEL (init, FALSE);
		WFE_Add_Aggregate_Init_Label (label_idx);
	}
	break;

  default:
	{
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected operator encountered"));
		WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
		WN_Delete (init_wn);
	}
      	break;
  }
} /* WFE_Add_Aggregate_Init_Address */

void
WFE_Finish_Aggregate_Init (void)
{
  if (aggregate_inito == 0) return;
  ST *st = INITO_st(aggregate_inito);
  TY_IDX ty = ST_type(st);
  if (TY_size(ty) == 0 ||
      (TY_kind(ty) == KIND_ARRAY &&
       !ARB_const_ubnd (TY_arb(ty)) &&
       TY_size(ty) <= Get_INITO_Size(aggregate_inito))) {
	// e.g. array whose size is determined by init;
	// fill in with initv size
	Set_TY_size(ty, Get_INITO_Size(aggregate_inito));
	if (TY_kind(ty) == KIND_ARRAY) {
		Set_ARB_const_ubnd (TY_arb(ty));
		Set_ARB_ubnd_val (TY_arb(ty), 
			(TY_size(ty) / TY_size(TY_etype(ty))) - 1 );
	}
  }
  if (last_aggregate_initv == 0) {
    WFE_Add_Aggregate_Init_Padding (0);
  }
  aggregate_inito = 0;
  not_at_root = FALSE;
}


static BOOL
Has_Non_Constant_Init_Value (tree init)
{
  if (init == NULL) {
	return FALSE;
  }
  switch (TREE_CODE(init)) {
  case CONSTRUCTOR:
	return Has_Non_Constant_Init_Value (CONSTRUCTOR_ELTS(init));
  case TREE_LIST:
	{
	tree p;
	for (p = init; p != NULL; p = TREE_CHAIN(p)) {
		if (Has_Non_Constant_Init_Value (TREE_VALUE(p))) {
			return TRUE;
		}
/*
		if (TREE_CODE(TREE_PURPOSE(p)) == FIELD_DECL
			&& TREE_CODE(DECL_CONTEXT(TREE_PURPOSE(p))) == UNION_TYPE
    			&& TYPE_FIELDS(DECL_CONTEXT(TREE_PURPOSE(p))) != TREE_PURPOSE(p))
		{
			// initializing union type and this NOT the first field
			return TRUE;
		}
*/
/*
		if (TREE_CODE(TREE_PURPOSE(p)) == FIELD_DECL
			&& DECL_BIT_FIELD(TREE_PURPOSE(p)))
		{
			// if bitfield, then do each element separately
			// rather than combine into initv field.
			return TRUE;
		}
*/
	}
	return FALSE;
	}
  case INTEGER_CST:
  case REAL_CST:
  case STRING_CST:
	return FALSE;
  case NOP_EXPR:
	if (TREE_CODE(TREE_OPERAND(init,0)) == ADDR_EXPR
    	    && TREE_CODE(TREE_OPERAND(TREE_OPERAND(init,0),0)) == STRING_CST) 
		return FALSE;
	else
		return TRUE;
  default:
	return TRUE;
  }
}

// For a dynamic initialization, we can either
// do a series of moves for each element,
// or we can create a static INITO and structure copy that value.
// GCC allows non-constant initial values, 
// so if any of those exist, we need to assign each element.
// Also, if the init is small we can optimize better
// if we make each element assignment be explicit.
// But otherwise, we create the static INITO since that saves code space.
static BOOL
Use_Static_Init_For_Aggregate (ST *st, tree init)
{
	if (TY_size(ST_type(st)) <= (2*MTYPE_byte_size(Spill_Int_Mtype))) {
		return FALSE;
	}
	else if (Has_Non_Constant_Init_Value(init)) {
		return FALSE;
	}
	else {
		return TRUE;
	}
}


static void
Add_Initv_For_Tree (tree val, UINT size)
{
	WN *init_block;
	WN * init_wn;

	switch (TREE_CODE(val)) {
	case INTEGER_CST:
		WFE_Add_Aggregate_Init_Integer (
			Get_Integer_Value(val), size);
		break;
	case REAL_CST:
		WFE_Add_Aggregate_Init_Real (
			TREE_REAL_CST(val), size);
		break;
	case STRING_CST:
		WFE_Add_Aggregate_Init_String (
			TREE_STRING_POINTER(val), size);
		break;
#if 0
	case PLUS_EXPR:
		if ( TREE_CODE(TREE_OPERAND(val,0)) == ADDR_EXPR
		     && TREE_CODE(TREE_OPERAND(val,1)) == INTEGER_CST)
		{
			tree addr_kid = TREE_OPERAND(TREE_OPERAND(val,0),0);
			FmtAssert(TREE_CODE(addr_kid) == VAR_DECL
				  || TREE_CODE(addr_kid) == FUNCTION_DECL,
				("expected decl under plus_expr"));
			WFE_Add_Aggregate_Init_Symbol ( Get_ST (addr_kid),
			Get_Integer_Value(TREE_OPERAND(val,1)) );
		}
		else
			FmtAssert(FALSE, ("unexpected tree code %s", 
				tree_code_name[TREE_CODE(val)]));
		break;
#endif
	case NOP_EXPR:
		tree kid;
		kid = TREE_OPERAND(val,0);
		if (TREE_CODE(kid) == ADDR_EXPR
	    		&& TREE_CODE(TREE_OPERAND(kid,0)) == STRING_CST) 
		{
			kid = TREE_OPERAND(kid,0);
			WFE_Add_Aggregate_Init_Address (kid);
			break;
		}
		// fallthru
	default:
		{
		init_block = WN_CreateBlock ();
                WFE_Stmt_Push (init_block, wfe_stmk_func_body, Get_Srcpos());
		init_wn = WFE_Expand_Expr (val);
                WFE_Stmt_Pop (wfe_stmk_func_body);

		if ((WN_opcode (init_wn) == OPC_I4U4CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U4LDA) ||
		    (WN_opcode (init_wn) == OPC_I8U8CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U8LDA)) {
			WFE_Add_Aggregate_Init_Symbol (WN_st (WN_kid0 (init_wn)),
						       WN_offset (WN_kid0 (init_wn)));
			WN_DELETE_Tree (init_wn);
			break;
		}

		if (WN_operator (init_wn) == OPR_LDA) {
			WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
						       WN_offset (init_wn));
			WN_DELETE_Tree (init_wn);
			break;
		}
		else if (WN_operator(init_wn) == OPR_INTCONST) {
			WFE_Add_Aggregate_Init_Integer (
				WN_const_val(init_wn), size);
			break;
		}
		// following cases for ADD and SUB are needed because the
		// simplifier may be unable to fold due to overflow in the
		// 32-bit offset field
		else if (WN_operator(init_wn) == OPR_ADD) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid0),
				     WN_offset(kid0) + WN_const_val(kid1));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		 	else if (WN_operator(kid1) == OPR_LDA &&
			    WN_operator(kid0) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid1),
				     WN_offset(kid1) + WN_const_val(kid0));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		else if (WN_operator(init_wn) == OPR_SUB) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid0),
				     WN_offset(kid0) - WN_const_val(kid1));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		FmtAssert(FALSE, ("unexpected tree code %s", 
			tree_code_name[TREE_CODE(val)]));
		}
	}
}

// buffer for simulating the initialized memory unit; it is managed independent
// of host's endianness
class INITBUF { 
public:
  UINT64 ival;

  INITBUF(void) {}
  INITBUF(UINT64 i): ival(i) {}
  ~INITBUF(void) {}
  mUINT8 Nth_byte(INT i) { // i must be from 0 to 7
		      INT rshft_amt = (Target_Byte_Sex == BIG_ENDIAN) ? 7-i : i;
		      return (ival >> (rshft_amt * 8)) & 0xff;
		    }
};

// at entry, assumes that in the current struct, initv for "bytes" bytes have 
// been generated; at exit, "bytes" will be updated with the additional
// bytes that this invocation generates.
static void
Add_Bitfield_Initv_For_Tree (tree val, FLD_HANDLE fld, INT &bytes)
{
  FmtAssert(TREE_CODE(val) == INTEGER_CST,
	    ("initialization value of bitfield expected to be integer, not %s",
	     tree_code_name[TREE_CODE(val)]));
  INT bofst = FLD_bofst(fld);
  INT bsize = FLD_bsize(fld);
  if (bsize == 0)
    return;

  INITBUF ib(Get_Integer_Value(val));
  // truncate ival according to the bitfield size and leave it left-justified
  ib.ival = ib.ival << (64 - bsize);
  // shift the value back right to the precise position within INITBUF
  if (Target_Byte_Sex == BIG_ENDIAN) 
    ib.ival = ib.ival >> bofst;
  else ib.ival = ib.ival >> (64 - bofst - bsize);

  // find number of bytes to output
  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
  // find number of bytes that have been output with previous bitfields
  INT bytes_out = bytes - FLD_ofst(fld);
  INT i;
  if (bytes_out > 0) {
    // verify that, other than the last output byte, the earlier bytes in 
    // ib are all 0
    for (i = 0; i < bytes_out - 1; i++)
      FmtAssert(ib.Nth_byte(i) == 0, 
		("processing error in Add_Bitfield_Initv_For_Tree"));
    if (ib.Nth_byte(bytes_out-1) != 0) {// merge and change last_aggregate_initv
      if (INITV_kind(last_aggregate_initv) == INITVKIND_VAL) {
        TCON &tc = INITV_tc_val(last_aggregate_initv);
        mUINT8 last_ival = TCON_k0(tc);
        tc.vals.k0 = last_ival | ib.Nth_byte(bytes_out-1);
      }
      else { // need to create a new TCON
        if (INITV_kind(last_aggregate_initv) == INITVKIND_ONE) 
	  INITV_Init_Integer(last_aggregate_initv, MTYPE_I1, 
			     1 | ib.Nth_byte(bytes_out-1));
	else {
	  FmtAssert(INITV_kind(last_aggregate_initv) == INITVKIND_ZERO,
		    ("processing error in static bit field initialization"));
	  INITV_Init_Integer(last_aggregate_initv, MTYPE_I1, 
			     ib.Nth_byte(bytes_out-1));
	}
      }
    }
  }
  // output the remaining bytes
  for (i = bytes_out; i < num_of_bytes; i++)
    WFE_Add_Aggregate_Init_Integer(ib.Nth_byte(i), 1);
  bytes += num_of_bytes - bytes_out;
}

// "bytes" will be updated with the additional bytes that this invocation
// generates stores into
static void
Gen_Assign_Of_Init_Val (ST *st, tree init, UINT offset, UINT array_elem_offset,
	TY_IDX ty, BOOL is_bit_field, UINT field_id, FLD_HANDLE fld, INT &bytes)
{
  WN *tas;
  WN *init_wn = WFE_Expand_Expr (init);
  TY_IDX sty = ST_type(st);
  BOOL is_struct = FALSE;
  
  if(TY_kind(sty) == KIND_ARRAY) {
    TY_IDX t = Get_Inner_Array_Type(sty);
    if(TY_kind(t) == KIND_STRUCT) {
      sty = t;
      is_struct = TRUE;
    }
  } else if(TY_kind(sty) == KIND_STRUCT)
	is_struct = TRUE;
  
  //FmtAssert(TY_kind(sty) == KIND_STRUCT,(""));
  
  
    if (TREE_CODE(init) == STRING_CST && TY_kind(ty) == KIND_ARRAY)
    {
	// have to store string into address,
	// rather than directy copy assignment,
	// so need special code.
	UINT size = TY_size(ty);
	TY_IDX ptr_ty = Make_Pointer_Type(ty);
	WN *load_wn = WN_CreateMload (0, ptr_ty, init_wn,
				      WN_Intconst(MTYPE_I4, size));
	WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
	WFE_Stmt_Append(
		WN_CreateMstore (offset, ptr_ty,
				 load_wn,
				 addr_wn,
				 WN_Intconst(MTYPE_I4,size)),
		Get_Srcpos());
	bytes += size;
	if (size == 0) {
	  /* size must be set so whirl2c can work correctly */
	  Set_TY_size(ty, TREE_STRING_LENGTH(init));
	  Set_TY_AR_ubnd_val(ty, 0, TREE_STRING_LENGTH(init) - 1);
	  Set_TY_AR_lbnd_var(ty, 0, 0);
	}

    }
    else {
	TYPE_ID mtype = is_bit_field ? MTYPE_BS : TY_mtype(ty);
	if (is_bit_field) { 
	    offset = array_elem_offset;	// uses array element offset instead
	}
	WFE_Set_ST_Addr_Saved (init_wn);
	WN *wn;
	// UPC specific 
	// distinguish intializers  
	//           T *p = shared T *p1
	if (compiling_upc && shared_ptr_idx && 
	    (TYPE_SHARED(TREE_TYPE(init)) || init_wn == last_ptr_arith_wn)) 
	  {
	    wn = WN_Stid (mtype, ST_ofst(st) + offset, st, is_struct ? sty : ty, 
			  init_wn, field_id);
	  } else if(Type_Is_Shared_Ptr(ty) && TY_kind(ty) == KIND_POINTER &&
		    TREE_CODE(init) == INTEGER_CST ) {
	    tas  = WN_Create(OPR_TAS, TY_mtype(ty), MTYPE_V, 1);
	    WN_kid0(tas) = init_wn;
	    WN_set_ty(tas,ty);
	    init_wn = tas;
	    wn = WN_Stid (mtype, ST_ofst(st) + offset, st,
			  is_struct ?  sty : ty, init_wn, field_id);
	    
	  } else { 
	    if(is_struct && field_id && 
	       TY_kind(Get_Field_Type(sty, field_id)) == KIND_ARRAY) {
	      TY_IDX fty = Get_Field_Type(sty, field_id);
	      TY_IDX ety = Get_Inner_Array_Type(fty);
	      wn = WN_Lda(Pointer_Mtype, 0, st, field_id);

	      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, TY_mtype(ety), 
				   ST_ofst(st)+offset,
				   Make_Pointer_Type(ety),
				   init_wn, wn, 0);

	     
	    } else 
	      wn = WN_Stid (mtype, ST_ofst(st) + offset, st,
			    is_struct ? sty : ty, init_wn, field_id);
	  }
	WFE_Stmt_Append(wn, Get_Srcpos());
	if (! is_bit_field) 
	  bytes += TY_size(ty);
	else {
	  INT bofst = FLD_bofst(fld);
	  INT bsize = FLD_bsize(fld);
	  // find number of bytes to output
	  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
	  // find number of bytes that have been output with previous bitfields
	  INT bytes_out = bytes - FLD_ofst(fld);
	  bytes += num_of_bytes - bytes_out;
	}
    }
}

UINT
Traverse_Aggregate_Constructor (
  ST   *st, tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

UINT
Traverse_Aggregate_Struct (
  ST   *st, tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

// For the specified symbol, generate padding at the offset specified.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Pad (
  ST     *st,
  BOOL   gen_initv,
  UINT   pad,
  UINT   current_offset, UINT field_id)
{
  
  BOOL is_struct = FALSE;
  TY_IDX sty = ST_type(st);
  if(TY_kind(sty) == KIND_ARRAY) {
    sty = Get_Inner_Array_Type(sty);
    if(TY_kind(sty) == KIND_STRUCT)
      is_struct = TRUE;
    sty = Make_Pointer_Type(sty);
  } else if (TY_kind(sty) == KIND_STRUCT) {
     sty = Make_Pointer_Type(sty);
     is_struct = TRUE;
  }
  FmtAssert(TY_kind(sty) == KIND_POINTER,(""));
  
  if (gen_initv) {
     WFE_Add_Aggregate_Init_Padding (pad);
  }
  else {
    WN *zero_wn = WN_Intconst(MTYPE_U4, 0);
    WN *pad_wn = WN_Intconst(MTYPE_U4, pad);
    WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
    TY_IDX mstore_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U1)); // char *
    pad_wn = WN_CreateMstore (current_offset, is_struct ? sty : mstore_ty,
			      zero_wn, addr_wn, pad_wn);
    WN_set_field_id(pad_wn,field_id);
    
    WFE_Stmt_Append (pad_wn, Get_Srcpos());
  }
} /* Traverse_Aggregate_Pad */

// The aggregate element for the specified symbol at the current_offset
// is an array having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Array (
  ST   *st,            // symbol being initialized
  tree init_list,      // list of initializers for each array element
  tree type,           // type of array
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset,
 UINT field_id) // offset of array from start of symbol
{
  INT    emitted_bytes = 0;
  INT    pad;
  TY_IDX ty            = Get_TY(type);
  TY_IDX ety           = TY_etype (ty);
  UINT   esize         = TY_size (ety);
  tree   init;
  INT num_elt = 0;
  INT next_elt = 0;

  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = TREE_CHAIN(init), num_elt++, next_elt++) {
    // bug 858: insert explict zeros in gaps caused by array designators.
    if (TREE_PURPOSE(init) && TREE_CODE(TREE_PURPOSE(init)) == INTEGER_CST) {
      INT curr_elt = Get_Integer_Value(TREE_PURPOSE(init));
      INT count = (curr_elt - next_elt);
      if (count > 0) {
        /* Can't use ..Init_Padding() because w2c will just skip over it */
        if (TREE_CODE(TREE_VALUE (init)) == CONSTRUCTOR) {
          tree empty = build (CONSTRUCTOR, type, NULL_TREE, NULL_TREE);
          for (int i = 0; i < count; ++i) {
            Traverse_Aggregate_Constructor (st, empty, TREE_TYPE(type),
                                            gen_initv, current_offset, current_offset,
                                            0);
            emitted_bytes += esize;
            current_offset += esize;
          }
        } else if (gen_initv) {
          WFE_Add_Aggregate_Init_Zeros(count, ety);
          current_offset += esize * count;
          emitted_bytes += esize * count;
        } else {
          tree zero = (MTYPE_is_pointer(TY_mtype(ety))) ? null_pointer_node : integer_zero_node;
          for (int i = 0; i < count; ++i) {
            Gen_Assign_Of_Init_Val (st, zero, current_offset, 0,
                                    ety, FALSE, field_id, FLD_HANDLE (), emitted_bytes);
            current_offset += esize;
	  }
        }
      }
      next_elt = curr_elt;
    }

    // loop through each array element
    if (TREE_CODE(TREE_VALUE (init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      // update array_elem_offset to current_offset to
      // keep track of where each array element starts
      Traverse_Aggregate_Constructor (st, TREE_VALUE(init), TREE_TYPE(type),
                                      gen_initv, current_offset, current_offset,
                                      0);
      emitted_bytes += esize;
    }

    else {
      // initialize SCALARs and POINTERs
      // note that we should not be encountering bit fields
      if (gen_initv) {
        Add_Initv_For_Tree (TREE_VALUE(init), esize);
        emitted_bytes += esize;
      }
      else
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init), current_offset, 0,
                                ety, FALSE, field_id, FLD_HANDLE (), emitted_bytes);
    }

    current_offset += esize;
  }

  if (TY_size(ty) == 0) {
    /* WEI: Size is determined by the initializer.  
       Onl the first dimension of the array may have their size omitted */
    Set_TY_size(ST_type(st), emitted_bytes);
    Set_TY_AR_ubnd_val(ST_type(st), 0, num_elt-1);
    Set_TY_AR_lbnd_var(ST_type(st), 0, 0);
  }

  // If the entire array has not been initialized, pad till the end
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset, 1);

} /* Traverse_Aggregate_Array */

// The aggregate element for the specified symbol at the current_offset
// is a struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the struct, and returns the field_id
// of the last element in the struct if it has elements, otherwise
// it returns the field_id passed in for empty structs

UINT
Traverse_Aggregate_Struct (
  ST   *st,               // symbol being initialized
  tree init_list,         // list of initializers for elements in STRUCT
  tree type,              // type of struct
  BOOL gen_initv,         // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset,    // offset from start of symbol for current struct
  UINT array_elem_offset, // if struct is with an array, then it is the
                          //   offset of the outermost struct from the
                          //   array enclosing the struct
                          // if struct is not within an array, it is zero
                          // this is needed when field_id is used to generate
                          //   stores for initialization
  UINT field_id)          // field_id of struct
{
  TY_IDX     ty    = Get_TY(type);       // get WHIRL type
  tree       field = TREE_PURPOSE(type); // get first field in gcc
  FLD_HANDLE fld   = TY_fld (ty);        // get first field in WHIRL

  INT        emitted_bytes = 0;          // keep track of # of bytes initialize;
  INT        current_offset_base = current_offset;
  INT        pad;
  BOOL       is_bit_field;
  tree       init;
  TY_IDX     fld_ty;

  //FIXME: The TY_sizes() in this function really should be Adjusted_TY_Size()(the 
  //correct size after taking the shared pointer size into consideration). But we 
  //couldn't use the latter since all of the offset/size adjustment is done in the backend,
  //and we can run into the problem of adjusting an offset twice.  It'd be nice if we can
  //have a way of marking whether an offset has been adjusted...
  
  // For empty initializers, we initialize the first field to its "zero" by
  // forcing a single pass through the same loop that "fills gaps" between fields.
  BOOL empty_init = (CONSTRUCTOR_ELTS(init_list) == NULL_TREE);

  for (init = CONSTRUCTOR_ELTS(init_list);
       init || empty_init;
       init = TREE_CHAIN(init)) {
    // loop through each initializer specified

    ++field_id; // compute field_id for current field

    // if the initialization is not for the current field,
    // advance the fields till we find it
    if (empty_init || (field && TREE_PURPOSE(init) && TREE_CODE(TREE_PURPOSE(init)) == FIELD_DECL)) {
      // bug 858: insert explict zeros in gaps caused by field designators.
      for (;;) {
        if (init && field == TREE_PURPOSE(init)) {
          break;
        }
        pad = FLD_ofst (fld) - emitted_bytes;
        if (pad > 0) {
          if (gen_initv) 
            Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset, field_id);
          current_offset += pad;
          emitted_bytes  += pad;
        }
        fld_ty = FLD_type(fld);
        UINT esize = TY_size(fld_ty);
#if 0
        if (!empty_init && TREE_CODE (type) == UNION_TYPE) {
          // This is a union initialier w/ discriminatior for a non-first member.
          // DO NOT emit zero initializers for the uninitialized members.
          // XXX: Disabled because w2c will use incorrectly initialize the 1st member
          //      but putting in the UNWANTED extra INITO elements ensures that w2c
          //      will fail rather than generate incorect code (lesser of 2 evils).
	  esize = 0;
        } else 
#endif
        if (TY_kind (fld_ty) == KIND_STRUCT || TY_kind (fld_ty) == KIND_ARRAY) { 
          // recursively process nested ARRAYs and STRUCTs
          tree empty = build (CONSTRUCTOR, field, NULL_TREE, NULL_TREE);
          field_id = Traverse_Aggregate_Constructor (st, empty,
                                                     TREE_TYPE(field), gen_initv,
                                                     current_offset,
                                                     array_elem_offset, field_id);
          emitted_bytes += esize;
        }
        else {
          // initialize SCALARs and POINTERs
          is_bit_field = FLD_is_bit_field(fld);
          if (gen_initv) {
            if (! is_bit_field) {
              WFE_Add_Aggregate_Init_Zeros(1, fld_ty);
              emitted_bytes += esize;
            } else {
              Add_Bitfield_Initv_For_Tree (integer_zero_node, fld, emitted_bytes);
            }
          } else {
            tree zero = (MTYPE_is_pointer(TY_mtype(fld_ty))) ? null_pointer_node : integer_zero_node;
            Gen_Assign_Of_Init_Val (st, zero,
                                    current_offset, array_elem_offset,
                                    is_bit_field ? ty : fld_ty,
                                    is_bit_field, field_id, fld, emitted_bytes);
          }
        }
        current_offset += esize;
        ++field_id;
        field = TREE_CHAIN(field);
        fld = FLD_next(fld);
        if (empty_init) break;
      }
      if (empty_init) break;
    }

    // check if we need to pad upto the offset of the field
    pad = FLD_ofst (fld) - emitted_bytes;

    if (pad > 0) {
      //Fix bug930
      //There is no reason to initialize the padding bytes to zero, as the C99 standard
      //states that padding bytes in structs have unspecified values.
      //This was causing an error in the backend when adjusting field offsets, 
      //since the offset of the padding doesn't correspond to an actual field
      if (gen_initv) 
	Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset, field_id);
      current_offset += pad;
      emitted_bytes  += pad;
    }

    fld_ty = FLD_type(fld);
    if (TREE_CODE(TREE_VALUE(init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      tree element_type;
      element_type = TREE_TYPE(field);
      field_id = Traverse_Aggregate_Constructor (st, TREE_VALUE(init),
                                                 element_type, gen_initv,
                                                 current_offset,
                                                 array_elem_offset, field_id);
      emitted_bytes += TY_size(fld_ty);
    }
    else {
      // initialize SCALARs and POINTERs
      is_bit_field = FLD_is_bit_field(fld);
      if (gen_initv) {
        if (! is_bit_field) {
          Add_Initv_For_Tree (TREE_VALUE(init), TY_size(fld_ty));
          emitted_bytes += TY_size(fld_ty);
        }
        else { // do 1 byte a time
          Add_Bitfield_Initv_For_Tree (TREE_VALUE(init), fld, emitted_bytes);
          // emitted_bytes updated by the call as reference parameter
        }
      }
      else {
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init),
                                current_offset, array_elem_offset,
                                is_bit_field ? ty : fld_ty,
                                is_bit_field, field_id, fld, emitted_bytes);
        // emitted_bytes updated by the call as reference parameter
      }
    }

    // advance ot next field
    current_offset = current_offset_base + emitted_bytes;
    field = TREE_CHAIN(field);
    fld = FLD_next(fld);
  }

  bool has_leftover_fld = false;
  // if not all fields have been initialized, then loop through
  // the remaining fields to update field_id
  // Also check to see if any bit fields need to be initialized 
  // to zero to handle the case where the bit field shares the
  // same byte as last bit field which was initialized.
  while ( ! fld.Is_Null()) {
      has_leftover_fld = true;
    ++field_id;
    if (!gen_initv && FLD_is_bit_field(fld)) {
      INT bofst = FLD_bofst(fld);
      INT bsize = FLD_bsize(fld);
      // find number of bytes to output
      INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
      // find number of bytes that have been output with previous bitfields
      INT bytes_out = current_offset - FLD_ofst(fld);
      if (num_of_bytes == bytes_out) {
	TY_IDX fld_ty = FLD_type(fld);
	WN *init_wn = WN_Intconst (TY_mtype (fld_ty), 0);
	WN *wn = WN_Stid (MTYPE_BS, ST_ofst(st) + array_elem_offset, st,
			  ty, init_wn, field_id);
	WFE_Stmt_Append(wn, Get_Srcpos());
      }
    }
    field = TREE_CHAIN(field);
    fld = FLD_next(fld);
  }

  // if not all fields have been initilaized, then check if
  // padding is needed to the end of struct
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
      if (!compiling_upc || has_leftover_fld) {
	  //we bzero the remaining memory only if there are unintialized fields, which by 
	  //C99 6.7.8.20 needs to be zero initialized.
	  //If all fields have been initialized,the padding bytes will not be bzeroed (see comments for bug930) 
	  Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset, field_id);
      }
  
  return field_id;
} /* Traverse_Aggregate_Struct */

// The aggregate element for the specified symbol at the current_offset
// is either an array or  struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the element in the enclosing struct
// used for computing field_ids (0 if no such struct exists)
// If the aggregate element is non-array, it returns the field_id of 
// last field within the aggregate element.
// If the aggregate element is array, then it returns the field_id passed in

UINT
Traverse_Aggregate_Constructor (
  ST   *st,               // symbol being initialized
  tree init_list,         // list of initilaizers for this aggregate
  tree type,              // type of aggregate being initialized
  BOOL gen_initv,         // TRUE  if initializing with INITV,
                          // FALSE if initializing with statements
  UINT current_offset,    // offset from start of symbol for this aggregate
  UINT array_elem_offset,
  UINT field_id)
{
  TY_IDX ty = Get_TY(type);

  INITV_IDX last_aggregate_initv_save;

  if (gen_initv) {

    WFE_Add_Init_Block();
    INITV_Init_Block(last_aggregate_initv, INITV_Next_Idx());
    not_at_root = TRUE;
    last_aggregate_initv_save = last_aggregate_initv;
    last_aggregate_initv = 0;
  }

  if (TY_kind (ty) == KIND_STRUCT) {

    field_id = Traverse_Aggregate_Struct (st, init_list, type, gen_initv,
                                          current_offset, array_elem_offset,
                                          field_id);
  }

  else
  if (TY_kind (ty) == KIND_ARRAY) {

    Traverse_Aggregate_Array (st, init_list, type, gen_initv, current_offset,
			      field_id);
  }

  else
    Fail_FmtAssertion ("Traverse_Aggregate_Constructor: non STRUCT/ARRAY");

  // restore current level's last_aggregate_initv and return
  last_aggregate_initv = last_aggregate_initv_save;

  return field_id;
} /* Traverse_Aggregate_Constructor */

static void
Add_Inito_For_Tree (tree init, tree decl, ST *st)
{
  tree kid;
  last_aggregate_initv = 0;
  TY_IDX ty;
  //WEI: don't think we need this anymore
  //  if (TY_is_shared(ST_type(st))) {
    // get the original type
    //ty = decl->common.type->type.orig_ty_idx;
  //ty = TYPE_SHARE_ORIG_TY_IDX(decl);
  //} else {
  ty = ST_type(st);

  TY_IDX init_ty = Get_TY(TREE_TYPE(init));
  BOOL shared_from_private = TY_is_shared(ty) && !Type_Is_Shared_Ptr(init_ty);

  switch (TREE_CODE(init)) {
  case INTEGER_CST:
	UINT64 val;
	val = Get_Integer_Value (init);
	// if section-attribute, keep as dglobal inito
	if (val == 0 && ! DECL_SECTION_NAME (decl)) {
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;

	WFE_Add_Aggregate_Init_Integer (val, TY_size(ty));
	return;
  case REAL_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Real (TREE_REAL_CST(init), 
		TY_size(ty));
	return;
  case COMPLEX_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Complex (TREE_REAL_CST(TREE_REALPART(init)), 
					TREE_REAL_CST(TREE_IMAGPART(init)), 
					TY_size(ty));
	return;
  case STRING_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_String (TREE_STRING_POINTER(init), 
                                       TREE_STRING_LENGTH(init));
	if (TY_size (ST_type(st)) > TREE_STRING_LENGTH(init))
		WFE_Add_Aggregate_Init_Padding ( TY_size (ty) -
						 TREE_STRING_LENGTH(init));
	if (TY_size(ST_type(st)) == 0) {
	  /* size must be set so whirl2c can work correctly */
	  Set_TY_size(ST_type(st), TREE_STRING_LENGTH(init));
	  Set_TY_AR_ubnd_val(ST_type(st), 0, TREE_STRING_LENGTH(init) - 1);
	  Set_TY_AR_lbnd_var(ST_type(st), 0, 0);
	}

	return;
  case NOP_EXPR:
	Add_Inito_For_Tree (TREE_OPERAND(init,0), decl, st);
	return;
  case ADDR_EXPR:
	if (shared_from_private) {
	    error("shared variable initialized with thread-specific address");
	    return;
	}
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == VAR_DECL ||
	    TREE_CODE(kid) == FUNCTION_DECL ||
	    TREE_CODE(kid) == STRING_CST) {
		aggregate_inito = New_INITO (st);
		not_at_root = FALSE;
		WFE_Add_Aggregate_Init_Address (kid);
		return;
	}
  case PLUS_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == ADDR_EXPR) {
	    if (shared_from_private) {
		error("shared variable initialized with thread-specific address");
		return;
	    }
	    #if 0 // NO - we'd drop INITV_ofst() when building the initializer (bugs 544 and 1275)
		// symbol+offset
		Add_Inito_For_Tree (kid, decl, st);
		kid = TREE_OPERAND(init,1);
		if (INITV_kind(last_aggregate_initv) == INITVKIND_SYMOFF
			&& TREE_CODE(kid) == INTEGER_CST)
		{
			Set_INITV_ofst (last_aggregate_initv,
				Get_Integer_Value(kid));
			return;
		}
	    #else
		error_with_file_and_line(input_filename, lineno, "Initializer expression for '%s' not supported by BUPC", ST_name(st));
	    #endif
	}
	break;
  case MINUS_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == ADDR_EXPR) {
	    if (shared_from_private) {
		error("shared variable initialized with thread-specific address");
		return;
	    }
	    #if 0 // NO - we'd drop INITV_ofst() when building the initializer (bugs 544 and 1275)
		// symbol-offset
		Add_Inito_For_Tree (kid, decl, st);
		kid = TREE_OPERAND(init,1);
		if (INITV_kind(last_aggregate_initv) == INITVKIND_SYMOFF
			&& TREE_CODE(kid) == INTEGER_CST)
		{
			Set_INITV_ofst (last_aggregate_initv,
				-Get_Integer_Value(kid));
			return;
		}
	    #else
		error_with_file_and_line(input_filename, lineno, "Initializer expression for '%s' not supported by BUPC", ST_name(st));
	    #endif
	}
	break;
  case CONSTRUCTOR:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
	Traverse_Aggregate_Constructor (st, init, TREE_TYPE(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
	return;
  }

  // not recognized, so try to simplify
  WN *init_wn = WFE_Expand_Expr (init);
  if (WN_operator(init_wn) == OPR_INTCONST) {
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Integer (
		WN_const_val(init_wn), TY_size(ST_type(st)));
	return;
  }
  else 
  if (WN_operator(init_wn) == OPR_LDA) {
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn), WN_offset (init_wn));
	return;
  }
  else
  if (WN_operator(init_wn) == OPR_ADD) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      if(WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
	WFE_Add_Aggregate_Init_Symbol (WN_st(WN_kid0(init_wn)),
				       WN_offset(WN_kid0(init_wn)) + WN_const_val(WN_kid1(init_wn)));
	return;
      }
    }
  }
  else
  if (WN_operator(init_wn) == OPR_SUB) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symbol (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) - WN_const_val(WN_kid1(init_wn)));
      return;
    }
  }
  error_with_file_and_line(input_filename, lineno, "Initializer expression for '%s' not supported by BUPC", ST_name(st));
  // Fail_FmtAssertion ("unexpected static init tree for %s", ST_name(st));
}


extern ST *
WFE_Generate_Temp_For_Initialized_Aggregate (tree init, char * name)
{
  TY_IDX ty_idx = Get_TY(TREE_TYPE(init));
  ST *temp = New_ST (CURRENT_SYMTAB);
  static UINT counter = 0;
  string suffix = "_" + utoa(string_hash(Src_File_Name) + counter++) + ".init";
  ST_Init (temp,
	Save_Str2 (name, suffix.c_str()),
	CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
	ty_idx );
  if (TREE_CODE(init) == CONSTRUCTOR
	&& ! Use_Static_Init_For_Aggregate (temp, init)) 
  {
	// do sequence of stores to temp
	Set_ST_sclass(temp, SCLASS_AUTO);	// put on stack
	Traverse_Aggregate_Constructor (temp, init, TREE_TYPE(init),
                                        FALSE /*gen_initv*/, 0, 0, 0);
  }
  else {
	// setup inito for temp
	Set_ST_is_initialized(temp);
	aggregate_inito = New_INITO (temp);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
	Traverse_Aggregate_Constructor (temp, init, TREE_TYPE(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
	WFE_Finish_Aggregate_Init ();
  }
  return temp;
}

extern void
WFE_Initialize_Decl (tree decl)
{
  if (DECL_IGNORED_P(decl)) {
  	// ignore initialization unless really used
	// e.g. FUNCTION and PRETTY_FUNCTION
	return;
  }
 
  if (keep_decl_for_w2c) 
    DECL_LANG_FLAG_5(decl) = 1;  

  ST *st = Get_ST(decl);
  tree init = DECL_INITIAL(decl);

  if (TREE_STATIC(decl) || DECL_CONTEXT(decl) == NULL) 
    {
      /* Check if this definition has a previous extern declaration */
      bool has_extern = (ST_sclass(st) == SCLASS_EXTERN);
      // static or global context, so needs INITO)
      if ((ST_sclass(st) == SCLASS_UGLOBAL &&
	   !ST_init_value_zero(st)) ||
	  ST_sclass(st) == SCLASS_EXTERN  ||
	  ST_sclass(st) == SCLASS_COMMON)
	Set_ST_sclass(st, SCLASS_DGLOBAL);
      if (!ST_is_initialized(st)) {
	Set_ST_is_initialized(st);
	Add_Inito_For_Tree (init, decl, st);
	WFE_Finish_Aggregate_Init ();
      }
      if (TREE_READONLY(decl))
	Set_ST_is_const_var (st);
      
      if (compiling_upc && has_extern) {
	//fix bug7 for declarations with initialization expression
	Add_Definition(st, decl);
      }
    }
  else {
	// mimic an assign
	if (TREE_CODE(init) == CONSTRUCTOR) {
		// is aggregate
		if (Use_Static_Init_For_Aggregate (st, init)) {
			// create inito for initial copy
			// and store that into decl
			ST *copy = WFE_Generate_Temp_For_Initialized_Aggregate(
					init, ST_name(st));
			WN *init_wn = WN_CreateLdid (OPR_LDID, MTYPE_M, MTYPE_M,
				0, copy, ST_type(copy));
			WFE_Stmt_Append(
				WN_CreateStid (OPR_STID, MTYPE_V, MTYPE_M,
					0, st, ST_type(st), init_wn),
				Get_Srcpos());
		}
		else {
			// do sequence of stores for each element
			Traverse_Aggregate_Constructor (st, init, TREE_TYPE(init),
                                FALSE /*gen_initv*/, 0, 0, 0);
		}
		return;
	}
	else {
		INT emitted_bytes;
		Gen_Assign_Of_Init_Val (st, init, 
			0 /*offset*/, 0 /*array_elem_offset*/,
			ST_type(st), FALSE, 0 /*field_id*/,
			FLD_HANDLE(), emitted_bytes);
	}
  }
}

// Called for declarations without initializers.
// Necessary to do Get_ST so things like
// int errno (at global level) get ST
// entries so a variable is emitted in whirl (and thus .o).
//
void
WFE_Decl (tree decl)
{
  
  TY_IDX ty_idx;
  
  if (keep_decl_for_w2c) 
    DECL_LANG_FLAG_5(decl) = 1;

  switch (TREE_CODE(decl)) {
  case TYPE_DECL:
/*
Removed to fix bug 1926.
There is no reason why a typedef should determine if the
base type's declaration needs to be emited or "put back".
    if(DECL_LANG_FLAG_5(decl)) {
      ty_idx = Get_TY(DECL_ORIGINAL_TYPE(decl));
      Set_TY_is_written(ty_idx);
      ty_idx = Get_TY(TREE_TYPE(decl));
      Set_TY_is_written(ty_idx);
    }
*/
    break;
  case VAR_DECL:
    //Make sure that we create the symbol table entries for
    //stack variables before  we  start processing the procedure
    //statements. Create_ST gets called the first time the vble is
    //used. In debug mode, if the variable is used inside a compound
    //statement  the line information is wrong.
    if(Debug_Level >= 2 && 
       (Scope_level >= 2 || (Scope_level >= 2 && TREE_STATIC(decl)))
       ) {
      // fprintf(stderr, "WFE_Decl %s \n", IDENTIFIER_POINTER(DECL_NAME(decl)));
      ST *st = Get_ST(decl);
    }
     break;
  case PARM_DECL:
     break;
  case FUNCTION_DECL:
    break;
  default:
    break;
  }

  if (DECL_INITIAL (decl) != 0) return; // already processed
  if (DECL_IGNORED_P(decl)) return;
  if (TREE_CODE(decl) != VAR_DECL) return;
  if (DECL_CONTEXT(decl) != 0) return;  // local
  if ( ! TREE_PUBLIC(decl)) return;     // local
  
  /* if we're compiling upc, then we want to process an external decl
   * to make sure it's output in the correct order by the init program 
   * This is important if the decl's address is used to initialize other global variables
   */
  if (!compiling_upc) {
    if ( ! TREE_STATIC(decl)) return;     // extern
  }

  // is something that we want to put in symtab
  // (a global var defined in this file).
  ST* st = Get_ST(decl);
  if (compiling_upc) {
    if (ST_sclass(st) == SCLASS_EXTERN &&
	DECL_CONTEXT(decl) == 0 && !DECL_EXTERNAL(decl)) {
      //The case when an extern decl is followed by its definition
      //This fixes bug7
      Set_ST_sclass(st, SCLASS_UGLOBAL);
      Add_Definition(st, decl);
    }
  }

  return;
}

void
WFE_Assemble_Alias (tree decl, tree target)
{
  DevWarn ("__attribute alias encountered at line %d", lineno);
  tree base_decl = lookup_name (target);
  FmtAssert (base_decl != NULL,
             ("undeclared base symbol %s not yet declared in __attribute__ alias is not currently implemented",
              IDENTIFIER_POINTER (target)));
  ST *base_st = Get_ST (base_decl);
  ST *st = Get_ST (decl);
  if (ST_is_weak_symbol(st)) {
    Set_ST_strong_idx (*st, ST_st_idx (base_st));
    Set_ST_sclass (st, SCLASS_EXTERN);
  }
  else {
    Set_ST_base_idx (st, ST_st_idx (base_st));
    Set_ST_emit_symbol(st);	// for cg
    Set_ST_sclass (st, ST_sclass (base_st));
    if (ST_is_initialized (base_st))
      Set_ST_is_initialized (st);
  }
/*
  if (ST_is_initialized (base_st)) {
    Set_ST_is_initialized (st);
    if (ST_init_value_zero (base_st))
      Set_ST_init_value_zero (st);
  }
*/
} /* WFE_Assemble_Alias */

void
WFE_Assemble_Constructor (const char *name)
{
  DevWarn ("__attribute__ ((constructor)) encountered at line %d", lineno);
  tree func_decl = lookup_name (get_identifier (name));
  ST *func_st = Get_ST (func_decl);
  INITV_IDX initv = New_INITV ();
  INITV_Init_Symoff (initv, func_st, 0, 1);
  ST *init_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (init_st, Save_Str2i ("__ctors", "_", ++__ctors),
           CLASS_VAR, SCLASS_FSTATIC,
           EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
  Set_ST_is_initialized (init_st);
  INITO_IDX inito = New_INITO (init_st, initv);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                Save_Str (".ctors"));
}

void
WFE_Assemble_Destructor (const char *name)
{
  DevWarn ("__attribute__ ((destructor)) encountered at line %d", lineno);
  tree func_decl = lookup_name (get_identifier (name));
  ST *func_st = Get_ST (func_decl);
  INITV_IDX initv = New_INITV ();
  INITV_Init_Symoff (initv, func_st, 0, 1);
  ST *init_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (init_st, Save_Str2i ("__dtors", "_", ++__dtors),
           CLASS_VAR, SCLASS_FSTATIC,
           EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
  Set_ST_is_initialized (init_st);
  INITO_IDX inito = New_INITO (init_st, initv);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                Save_Str (".dtors"));
  Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
  Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
  Set_ST_addr_saved (func_st);
}

ST *
WFE_Get_Return_Address_ST (int level)
{
  ST *return_address_st = Return_Address_ST [CURRENT_SYMTAB - level];
  if (return_address_st == NULL) {
    return_address_st = New_ST (CURRENT_SYMTAB - level);
    ST_Init (return_address_st, Save_Str ("__return_address"), CLASS_VAR,
             SCLASS_AUTO, EXPORT_LOCAL, 
             Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE));
    Set_ST_is_return_var (return_address_st);
    Return_Address_ST [CURRENT_SYMTAB - level] = return_address_st;
  }

  return return_address_st;
} /* WFE_Get_Return_Address_ST */

ST *
WFE_Alloca_0 (void)
{
  WN *wn;
 error_with_file_and_line(input_filename, lineno,"C99 variable length arrays not yet supported by Berkeley UPC ");


  TY_IDX ty_idx = Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE);
  ST* alloca_st = Gen_Temp_Symbol (ty_idx, "__alloca");
  wn = WN_CreateAlloca (WN_CreateIntconst (OPC_I4INTCONST, 0));
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ty_idx, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  Set_PU_has_alloca (Get_Current_PU ());
  return alloca_st;
} /* WFE_Alloca_0 */

ST *
WFE_Alloca_ST (tree decl)
{
  ST *st = Create_ST_For_Tree (decl);
  ST *alloca_st = New_ST (CURRENT_SYMTAB);
  ST_Init (alloca_st, Save_Str (ST_name (st)),
           CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
           Make_Pointer_Type (ST_type (st), FALSE));
  Set_ST_is_temp_var (alloca_st);
  Set_ST_pt_to_unique_mem (alloca_st);
  Set_ST_base_idx (st, ST_st_idx (alloca_st));
  WN *swn = WFE_Expand_Expr (TYPE_SIZE(TREE_TYPE(decl)));
  WN *wn  = WN_CreateAlloca (swn);
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  return st;
} /* WFE_Alloca_ST */

void
WFE_Dealloca (ST *alloca_st, tree vars)
{
  int  nkids = 0;
  tree decl;
  WN   *wn;
  ST   *st;
  ST   *base_st;

  for (decl =vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == VAR_DECL && DECL_ST (decl)) {
      st = DECL_ST (decl);
      base_st = ST_base (st);
      if (st != base_st)
        ++nkids;
  }

  wn = WN_CreateDealloca (nkids+1);
  WN_kid0 (wn) = WN_Ldid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st));
  nkids = 0;

  for (decl =vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == VAR_DECL && DECL_ST (decl)) {
      st = DECL_ST (decl);
      base_st = ST_base (st);
      if (st != base_st)
        WN_kid (wn, ++nkids) = WN_Ldid (Pointer_Mtype, 0, base_st, ST_type (base_st));
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Dealloca */

void
WFE_Record_Asmspec_For_ST (tree decl, const char *asmspec, int reg)
{
  extern PREG_NUM Map_Reg_To_Preg []; // defined in common/com/arch/config_targ.cxx
  PREG_NUM preg = Map_Reg_To_Preg [reg];
  FmtAssert (preg >= 0,
             ("mapping register %d to preg failed\n", reg));
  ST *st = Get_ST (decl);
  TY_IDX ty_idx = ST_type (st);
  Set_TY_is_volatile (ty_idx);
  Set_ST_type (st, ty_idx);
  Set_ST_assigned_to_dedicated_preg (st);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (CURRENT_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_DEDICATED_REGISTER, preg);
} /* WFE_Record_Asmspec_For_ST */

void
WFE_Resolve_Duplicate_Decls (tree olddecl, tree newdecl)
{
  ST     *st      = DECL_ST(olddecl);
  tree    newtype = TREE_TYPE(newdecl);
  tree    newsize = TYPE_SIZE(newtype);
  TY_IDX  ty      = ST_type (st);

  if (TREE_STATIC(olddecl) == FALSE &&
      TREE_STATIC(newdecl) == TRUE  &&
      TREE_PUBLIC(olddecl) == TRUE  &&
      TREE_PUBLIC(newdecl) == FALSE) {
    Set_ST_sclass (st, SCLASS_FSTATIC);
    Set_ST_export (st, EXPORT_LOCAL);
  }

  if (newsize                           &&
      TREE_CODE(newsize) == INTEGER_CST &&
      TY_size (ty) <= Get_Integer_Value (newsize) / BITSPERBYTE) {
    UINT64 size = Get_Integer_Value (newsize) / BITSPERBYTE;
    Set_TY_size (ty, size);
    if (TY_kind (ty) == KIND_ARRAY) {
      Set_ARB_const_ubnd (TY_arb(ty));
      Set_ARB_ubnd_val (TY_arb(ty), (size / TY_size(TY_etype(ty))) - 1);
    }
  } 
} /* WFE_Resolve_Duplicate_Decls */


void
WFE_Add_Weak ()
{
  tree decl = lookup_name (get_identifier (weak_decls->name));
  if (decl) {
    ST *st = DECL_ST (decl);
    if (st)
      Set_ST_is_weak_symbol (st);
  }
} /* WFE_Add_Weak */


void
WFE_Weak_Finish ()
{
  struct weak_syms *t;
  for (t = weak_decls; t; t = t->next) {
    if (t->name) {
      tree decl = lookup_name (get_identifier (t->name));
      if (!decl) 
        warning ("did not find declaration `%s' for used in #pragma weak", t->name);
      else {
        ST *st = DECL_ST (decl);
	if (st == NULL && t->value) {
	  st = Get_ST (decl);
	}
        if (st) {
          Set_ST_is_weak_symbol (st);
          if (t->value) {
            tree base_decl = lookup_name (get_identifier (t->value));
            if (!base_decl)
               warning ("did not find declaration for `%s' used in #pragma weak", t->value);
            else {
              ST *base_st = DECL_ST (base_decl);
              if (base_st)
                Set_ST_strong_idx (*st, ST_st_idx (base_st));
            }
          }
        }
      }
    }
  }
} /* WFE_Weak_Finish */



extern "C" void Mark_TY_Written(tree type) {
  
  TY_IDX ty_idx = Get_TY(type);
  Set_TY_is_written(ty_idx);

}


extern stack<stack<ST*> *> inner_scope_vbles;
extern stack<ST*> file_scope_statics;

void WFE_Patch_Inner_Scope_Names(stack<ST*> *scope, int lineno) 
{
  ST *st;
  string new_name;
  char linebuf[64];
  
  while(!scope->empty()) {
    st = scope->top();
    scope->pop();
    new_name = ST_name(*st);
    sprintf(linebuf,"%d",lineno);
    new_name = new_name + "_L" + linebuf;
    Set_ST_name_idx(st,Save_Str(new_name.c_str()));
  }
}


extern "C" void WFE_Start_Inner_Scope() 
{
  stack<ST*> *new_scope;
  Scope_level++;
  
  new_scope = new stack<ST*>;
  inner_scope_vbles.push(new_scope);

}


extern "C" void WFE_Finish_Inner_Scope() 
{
  stack<ST*> *scope;
  Scope_level--; 
  if(Scope_level) {
    scope = inner_scope_vbles.top();
    WFE_Patch_Inner_Scope_Names(scope, lineno);
    Is_True(scope->empty(), ("",""));
    inner_scope_vbles.pop();
    delete scope;
  }  
}



void WFE_Patch_File_Statics() 
{
  WFE_Patch_Inner_Scope_Names(&file_scope_statics, lineno);
} 

