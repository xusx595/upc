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


//-*-c++-*-

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#if (defined(__MACH__) && defined(__APPLE__)) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#else
#include <values.h>
#endif
#include <isam.h>
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif
#include <sys/signal.h>
#include <elf.h>
#include <map>

//Rice code
#if defined(__GNUC__) && (__GNUC__ >= 3)
# define USING_HASH_SET 1
# include <ext/hash_set>
using namespace __gnu_cxx;
#elif defined(__sgi) && !defined(__GNUC__)
# define USING_HASH_SET 1
# include <hash_set>
#else
# include <set>
#endif

#include "defs.h"
#include "config.h"
#include "config_asm.h"
#include "config_debug.h"
#include "config_opt.h"
#include "config_targ_extra.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "glob.h"
#include "timing.h"
#include "stab.h"
#include "strtab.h"
#include "util.h"
#include "wn.h"
#include "wn_util.h"
#include "stblock.h"
#include "data_layout.h"
#include "ir_reader.h"
#include "targ_sim.h"
#include "targ_const.h"
#include "const.h"
#include "ttype.h"
#include "wio.h"
#include "wn_mp.h"
#include "wn_pragmas.h"
#include "wn_simp.h"
#include "opt_alias_interface.h"
#include "wn_lower.h"
#include "region_util.h"
#include "wutil.h"
#include "wn_map.h"
#include "wn_fio.h"
#include "wn_trap.h"
#include "pu_info.h"
#include "w2op.h"
#include "be_symtab.h"
#include "betarget.h"
#include "be_util.h"
#include "opt_cvtl_rule.h"
#include "fb_whirl.h"
#include "intrn_info.h"
#include "upc_symtab_utils.h"
#include "upc_wn_util.h"
#include "upc_coalescing.h"

/* My changes are a hack till blessed by Steve. (suneel) */
#define SHORTCIRCUIT_HACK 1

/* this next header should be after the external declarations in the others */
#include "pragma_weak.h"	/* Alias routines defined in wopt.so */

/* ====================================================================
 *			 Exported Functions
 * ====================================================================
 */
extern PREG_NUM AssignExpr(WN *, WN *, TYPE_ID);

extern BOOL lower_is_aliased(WN *, WN *, INT64 size, BOOL defalt);

extern INT32 compute_copy_alignment(TY_IDX, TY_IDX, INT32 offset);

extern TYPE_ID compute_copy_quantum(INT32 );

extern WN *WN_I1const(TYPE_ID, INT64 con);

/*
 * defined in config.c (should be in config.h)
 */
extern INT32	MinStructCopyLoopSize;
extern INT32	MinStructCopyMemIntrSize;

/* ====================================================================
 *			 Imported Declarations
 * ====================================================================
 */
extern WN *emulate(WN *, WN *);

extern WN *intrinsic_runtime(WN *, WN *);

extern WN *make_pointer_to_node(WN *, WN *);

extern char *INTR_intrinsic_name( WN *);

extern void fdump_dep_tree(FILE *, WN *, struct ALIAS_MANAGER *);

extern void InitParityMaps(void);

extern "C" void LNOPreserveMapPair(WN *, WN *, WN *);

extern "C" void LNOPruneMapsUsingParity(void);

extern "C" void LNOPrintDepGraph(FILE *);

extern void enable_tree_freq_display(void);

extern TYPE_ID INTR_return_mtype(INTRINSIC id);

extern BE_ST_TAB   Be_st_tab;


/* ====================================================================
 *			 Forward Declarations
 * ====================================================================
 */

static WN *lower_scf(WN *, WN *, LOWER_ACTIONS);
static WN *lower_expr(WN *, WN *, LOWER_ACTIONS);
static WN *lower_store(WN *, WN *, LOWER_ACTIONS);
static WN *lower_call(WN *, WN *, LOWER_ACTIONS);
static WN *lower_intrinsic(WN *, WN *, LOWER_ACTIONS);
static WN *lower_intrinsic_call(WN *, WN *, LOWER_ACTIONS);
static WN *lower_intrinsic_op(WN *, WN *, LOWER_ACTIONS);
static WN *lower_if(WN *, WN *, LOWER_ACTIONS);
static WN *lower_stmt(WN *, WN *, LOWER_ACTIONS);
static WN *lower_entry(WN *, LOWER_ACTIONS);
static WN *lower_eval(WN *, WN *, LOWER_ACTIONS);
static WN *lower_copy_tree(WN *, LOWER_ACTIONS);
static WN *lower_emulation(WN *, WN *, LOWER_ACTIONS, BOOL &intrinsic_lowered);
static WN *lower_mstore(WN *, WN *, LOWER_ACTIONS);
static WN *lower_nary_madd(WN *, WN *, LOWER_ACTIONS);
static WN *lower_madd(WN *, WN *, LOWER_ACTIONS);
static WN *lower_assert(WN *, WN *, LOWER_ACTIONS);
static WN *lower_trapuv(WN *, WN *, LOWER_ACTIONS);
static WN *lower_base_reference(WN *, ST *, INT64, LOWER_ACTIONS);
static WN *lower_base_register(WN *, ST *, INT64, LOWER_ACTIONS);
static WN *lower_dereference(WN *, INT64, ST *, PREG_NUM, LOWER_ACTIONS);
static WN *lower_split_sym_addrs(WN *, INT64, LOWER_ACTIONS);
static WN *improve_Malignment(WN *, WN *, WN *, INT64);
static WN *lower_branch(WN *, WN *, LOWER_ACTIONS);
static WN *lower_branch_condition(BOOL, LABEL_IDX, WN *, WN **, LOWER_ACTIONS);
static WN *lower_conditional(WN *, WN *, LABEL_IDX, LABEL_IDX, BOOL,
			     LOWER_ACTIONS);
static WN *lower_tree_height(WN *, WN *, LOWER_ACTIONS);


static TY_IDX coerceTY(TY_IDX, TYPE_ID);
static ST *coerceST(const ST *, TYPE_ID);
static ST *coerceST(const ST &, TYPE_ID);
static WN_OFFSET coerceOFFSET(WN *, TYPE_ID, WN_OFFSET);


static WN *lower_mload(WN *, WN *, LOWER_ACTIONS);
static void lower_mload_formal(WN *, WN *, PLOC, LOWER_ACTIONS);
static void lower_mload_actual (WN *, WN *, PLOC, LOWER_ACTIONS);
static void lower_complex_emulation(WN *, WN *, LOWER_ACTIONS, WN **, WN **);
static void lower_complex_expr(WN *, WN *, LOWER_ACTIONS, WN **, WN **);


static void lower_copy_maps(WN *, WN *, LOWER_ACTIONS);
static void lower_tree_copy_maps(WN *, WN *, LOWER_ACTIONS);

static INT32 compute_alignment(WN *, INT64);
static TYPE_ID compute_next_copy_quantum(TYPE_ID , INT32);

static WN* lower_TLD(WN*, WN*, LOWER_ACTIONS);
static WN* lower_TLD_lda(WN*, WN*, LOWER_ACTIONS);

static TY_IDX struct_addr_ty(WN* tree);
static int find_field_from_offset(TY_IDX ty, unsigned int offset);

/* ====================================================================
 *			 private variables
 * ====================================================================
 */
static INT   max_region;
static char *current_preg_name;
static UINT16 loop_nest_depth;
static BOOL contains_a_loop;
static struct ALIAS_MANAGER *alias_manager;
#define PARITY_MAP_ARRAY_SIZE 32
static WN_MAP parity_map_array[PARITY_MAP_ARRAY_SIZE];
static WN_MAP lowering_parity_map = 0;
static INT32 parity_map_index = -1;
static LOWER_ACTIONS lowering_actions= 0;
static BOOL save_Div_Split_Allowed ;

static BOOL traceIO              = FALSE;
static BOOL traceSpeculate       = FALSE;
static BOOL traceAlignment       = FALSE;
static BOOL traceTreeHeight      = FALSE;
static BOOL traceSplitSymOff     = FALSE;
static BOOL traceWoptFinishedOpt = FALSE;
static BOOL traceMload           = FALSE;
// static BOOL traceUplevel = FALSE;

CURRENT_STATE	current_state;
SRCPOS upc_srcpos;
extern int debug_requested;
#define	current_srcpos		current_state.srcpos
#define	current_stmt		current_state.stmt
#define	current_actions		current_state.actions
#define current_function	current_state.function

typedef enum MSTORE_ACTIONS
{
  MSTORE_aggregate,
  MSTORE_loop,
  MSTORE_intrinsic_bzero,
  MSTORE_intrinsic_memset,
  MSTORE_intrinsic_bcopy
} MSTORE_ACTIONS;

static const char * MSTORE_ACTIONS_name(MSTORE_ACTIONS);

static TYPE_ID Promoted_Mtype[MTYPE_LAST + 1] = {
  MTYPE_UNKNOWN,  /* MTYPE_UNKNOWN */
  MTYPE_UNKNOWN,  /* MTYPE_B */
  MTYPE_I4,       /* MTYPE_I1 */
  MTYPE_I4,       /* MTYPE_I2 */
  MTYPE_I4,       /* MTYPE_I4 */
  MTYPE_I8,       /* MTYPE_I8 */
  MTYPE_U4,       /* MTYPE_U1 */
  MTYPE_U4,       /* MTYPE_U2 */
  MTYPE_U4,       /* MTYPE_U4 */
  MTYPE_U8,       /* MTYPE_U8 */
  MTYPE_F4,       /* MTYPE_F4 */
  MTYPE_F8,       /* MTYPE_F8 */
  MTYPE_UNKNOWN,  /* MTYPE_F10 */
  MTYPE_UNKNOWN,  /* MTYPE_F16 */
  MTYPE_UNKNOWN,  /* MTYPE_STR */
  MTYPE_FQ,       /* MTYPE_FQ */
  MTYPE_M,        /* MTYPE_M */
  MTYPE_C4,       /* MTYPE_C4 */
  MTYPE_C8,       /* MTYPE_C8 */
  MTYPE_CQ,       /* MTYPE_CQ */
  MTYPE_V         /* MTYPE_V */
};

/* ====================================================================
 *			Private macros
 * ====================================================================
 */

#define OPCODE_is_intrinsic(op)                                 	\
		((OPCODE_operator((op)) == OPR_INTRINSIC_CALL) ||       \
		(OPCODE_operator((op)) == OPR_INTRINSIC_OP))

#define	Action(x)			(actions & (x))
#define	NotAction(x)			(Action(x)==0)
#define	RemoveScfAction(x)		(x & ~(LOWER_SCF))
#define	RemoveShortCircuitAction(x)	(x & ~(LOWER_SHORTCIRCUIT))


#define	WN_has_alias_info(x)	(OPCODE_is_load(WN_opcode(x))	||	\
				 OPCODE_is_store(WN_opcode(x))	||	\
				 WN_operator_is(x, OPR_PARM))
#define	WN_has_offset(x)	(OPCODE_has_offset(WN_opcode(x)))

#define	WN_nary_intrinsic(x)	(WN_operator_is(x, OPR_INTRINSIC_OP) &&	\
      				((WN_intrinsic(x)== INTRN_NARY_ADD) ||	\
      				 (WN_intrinsic(x)== INTRN_NARY_MPY)))
#define	WN_nary_add(x)		(WN_operator_is(x, OPR_INTRINSIC_OP) &&	\
      				 (WN_intrinsic(x)== INTRN_NARY_ADD))
#define	WN_nary_mpy(x)		(WN_operator_is(x, OPR_INTRINSIC_OP) &&	\
      				 (WN_intrinsic(x)== INTRN_NARY_MPY))

#define	WN_is_block(x)		(WN_opcode(x) == OPC_BLOCK)

#define INTRN_is_nary(x)	(((x)==INTRN_NARY_ADD) || ((x)==INTRN_NARY_MPY))

#define	WN_is_commutative(x)	(WN_opcode(x) == OPCODE_commutative_op(WN_opcode(x)))

#define	TY_is_pointer(x)	(TY_kind(x) == KIND_POINTER)

#define	lower_truebr(l,c,b,a)	lower_branch_condition(TRUE,l,c,b,a)
#define	lower_falsebr(l,c,b,a)	lower_branch_condition(FALSE,l,c,b,a)

/* ====================================================================
 *
 * BOOL mem_offset_must_be_split(WN_OFFSET offset)
 * WN_OFFSET mem_offset_hi(WN_OFFSET offset)
 * WN_OFFSET mem_offset_lo(WN_OFFSET offset)
 *
 * Returns TRUE iff <offset> must be split for a target-machine memory
 * reference.  If so, mem_offset_hi(offset) returns the high part of
 * the split offset, and mem_offset_lo(offset) returns the low part of
 * the split offset.
 * 
 * ==================================================================== */

#define mem_offset_hi(offset) ((offset) & ~0x7fff)
#define mem_offset_lo(offset) ((offset) & 0x7fff)

#define mem_offset_must_be_split(offset)		\
        (!(-32768 <= (offset) && (offset) < 32768))

#define mem_offset_2GB(offset)				\
	(!(INT32_MIN <= offset && offset <= INT32_MAX))

#define	PIC_SHARED		(Gen_PIC_Shared || Gen_PIC_Call_Shared)
#define	PIC_NONSHARED		(!PIC_SHARED)

#define ABS(x)			(((x)<0) ? -(x) : (x))
#define IS_POWER_OF_2(val)      ((val != 0) && ((val & (val-1)) == 0))




/* ====================================================================
 *
 * UINT32 compute_offset_alignment(INT32 offset, UINT32 align)
 *
 * return gcd of offset,align;
 * Used for alignment reasons;
 * For maximum efficiency, offset should be >= align
 *
 * ==================================================================== */
UINT32 compute_offset_alignment(INT32 offset, UINT32 align)
{
  UINT32 m = ABS(offset);
  UINT32 n = align;
  while (n != 0) {
    INT32 new_n = m % n;
    m = n;
    n = new_n;
  }
  return m;
}


/* ====================================================================
 *
 *  The semantics of memset require replicating the byte constant
 *  so replicate the constant into a I8/U8 depending on WN_rtype(con)
 *
 * ==================================================================== */
extern WN *WN_I1const(TYPE_ID type, INT64 con)
{
  // assume con is a byte constant, so clear the other bits
  // (otherwise replicate will "or" with sign bits).
  INT64	n=	  con & 0xff;
  INT64	maxAlign= MTYPE_alignment(Max_Uint_Mtype);

  if (con)
  {
    INT64 i;
    for(i=1; i<maxAlign; i++)
    {
      n |=	(n << 8);
    }
  }

  return WN_Intconst(Mtype_AlignmentClass(maxAlign,
					  MTYPE_type_class(type)), n);
}


/* ====================================================================
 *
 * void push_current_state(WN *tree, STATE *state)
 *
 * return and set current state
 *
 * ==================================================================== */
static void setCurrentState(WN *tree, LOWER_ACTIONS actions)
{
  if (tree) {
    current_stmt =	tree;
    current_srcpos =	WN_Get_Linenum(tree);
    current_actions =	actions;

    if (WN_opcode(tree) == OPC_FUNC_ENTRY)
      current_function = tree;
  }
}

static void setCurrentStateBlockFirst(WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_BLOCK, ("expected BLOCK node"));

  setCurrentState(WN_first(tree), actions);
}

static void setCurrentStateBlockLast(WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_BLOCK, ("expected BLOCK node"));

  setCurrentState(WN_last(tree), actions);
}


static CURRENT_STATE pushCurrentState(WN *tree, LOWER_ACTIONS actions)
{
  CURRENT_STATE saveState = current_state;

  setCurrentState(tree, actions);

  return saveState;
}

static void popCurrentState(CURRENT_STATE state)
{
  current_state = state;
}
    

/* ====================================================================
 *
 * BOOL foldConstOffset(WN *con, INT64 offset)
 *
 * BOOL foldLdaOffset(WN *lda, INT64 offset)
 *
 * can con and offset be folded to "fit" into a WN_OFFSET (INT32?)
 *
 * ==================================================================== */

static BOOL foldConstOffset(WN *con, INT64 offset)
{
  if (WN_operator_is(con, OPR_INTCONST))
  {
    INT64 sum= offset + WN_const_val(con);

    if (INT32_MIN <= sum && sum <= INT32_MAX)
      return TRUE;
  }
  return FALSE;
}

static BOOL foldLdaOffset(WN *lda, INT64 offset)
{
  

  //This has the effect of moving
  //offsets from the tree nodes that compute the memory address to 
  // the wrapping ld/st and whirl2c gets very confused.
  //For UPC (src2src) translation we do not care that much about folding
  //constants as we care about being able to easily recuperate the high
  //level type information.
  if(Compile_Upc) 
    return FALSE;
  if (WN_operator_is(lda, OPR_LDA))
  {
    INT64 sum= offset + WN_lda_offset(lda);

    Is_True((WN_class(lda) != CLASS_PREG), ("lda class is PREG!!!"));

    if (INT32_MIN <= sum && sum <= INT32_MAX)
    {
      ST  *sym= WN_st(lda);

     /*
      *	if the offset is greater than the symbol size, we may generate
      * a relocation that is out of bounds (ex LFTR). See pv 482353 for
      * this rare condition
      */
      if (ST_class(sym) == CLASS_BLOCK && STB_size(sym) < sum)
	return FALSE;

      return TRUE;
    }
  }
  return FALSE;
}








/* ====================================================================
 *
 * WN_OFFSET coerceOFFSET(WN *tree, TYPE_ID realTY, WN_OFFSET offset)
 *
 * The offset may either be an offset or preg number.
 *
 * There is an amazing kludge for complex return values where we
 * return F0, F2
 *
 * ==================================================================== */
static WN_OFFSET coerceOFFSET(WN *tree, TYPE_ID realTY, WN_OFFSET offset)
{

  switch (WN_operator(tree))
  {
  case OPR_ILOAD:
  case OPR_ILOADX:
  case OPR_ISTORE:
  case OPR_ISTOREX:
    return offset + MTYPE_RegisterSize(realTY);

  case OPR_LDID:
  case OPR_STID:
    if (WN_class(tree) == CLASS_PREG)
    {
     /*
      *  amazing kludge
      *  for dedicated return register (F0) the ABI defines [F0,F2]
      *  as the return values
      */
      if (Preg_Is_Dedicated(offset) && offset == Float_Preg_Min_Offset)
      {
	  return Float_Preg_Min_Offset + 2;
      }
      else
      {
	return offset + Preg_Increment(realTY);
      }
    }
    return offset + MTYPE_RegisterSize(realTY);
  }
  Fail_FmtAssertion("unexpected complex op (%s)",
		    OPCODE_name(WN_opcode(tree)));
  /*NOTREACHED*/
}

/* ====================================================================
 *
 *
 * ==================================================================== */

static void rename_preg(char *f, char *preg_class)
{
  static char name[41];

  name[0] = '\0';
  
  strncat(name, f, 30);

  if (preg_class)
  {
    strncat(name, preg_class, 10);
  }
  current_preg_name = name;
}

static void rename_reset(void)
{
  current_preg_name = NULL;
}


static void WN_Set_Flags(WN *src, WN *dst)
{
  if (OPCODE_has_flags(WN_opcode(src)))
  {
    Is_True(OPCODE_has_flags(WN_opcode(dst)), ("expected wn with flags"));
    WN_set_flag(dst, WN_flag(src));
  }
}

extern void WN_annotate_intrinsic_flags(INTRINSIC id, ST *sym)
{
  if (INTRN_is_pure(id)) {
    Set_PU_is_pure(Get_Current_PU());
  }
  if (INTRN_has_no_side_effects(id)) {
    Set_PU_no_side_effects(Get_Current_PU());
  }
}

extern void WN_annotate_call_flags(WN *call, ST *sym)
{
  WN_Set_Call_Default_Flags(call);

  if (PU_no_side_effects(Pu_Table[ST_pu(sym)]))
  {   
    WN_Reset_Call_Non_Data_Mod(call);
    WN_Reset_Call_Non_Parm_Mod(call);
    WN_Reset_Call_Parm_Mod(call);

  }
  if (PU_is_pure(Pu_Table[ST_pu(sym)]))
  {
    WN_Reset_Call_Non_Data_Mod(call);
    WN_Reset_Call_Non_Parm_Mod(call);
    WN_Reset_Call_Parm_Mod(call);

    WN_Reset_Call_Non_Data_Ref(call);
    WN_Reset_Call_Non_Parm_Ref(call);
  }
}

/* ====================================================================
 *
 *
 * Create a new label with linenum info
 *
 * ==================================================================== */
static LABEL_IDX NewLabel(void)
{
  LABEL_IDX label;
  LABEL& lab = New_LABEL(CURRENT_SYMTAB, label);
  // create label name
  char *name = (char *) alloca (strlen(".L..") + 8 + 8 + 1);
  sprintf(name, ".L%s%d%s%d", Label_Name_Separator, Current_PU_Count(), 
	Label_Name_Separator, label);
  LABEL_Init (lab, Save_Str(name), LKIND_DEFAULT);
  return label;
}

static WN *WN_Label(LABEL_IDX l)
{
  WN *label = WN_CreateLabel(ST_IDX_ZERO, l, 0, NULL);
  WN_Set_Linenum(label, current_srcpos);
  return label;
}

static WN *WN_NewLabel(void)
{
  LABEL_IDX label;
  label = NewLabel();
  return WN_Label(label);
}



/* ====================================================================
 *
 *
 * Create false/true branch with line info
 *
 * ==================================================================== */
static WN *WN_Falsebr(LABEL_IDX label, WN *cond)
{
  WN *branch = WN_CreateFalsebr(label, cond);
  WN_Set_Linenum(branch, current_srcpos);
  return branch;
}

static WN *WN_Truebr(LABEL_IDX label, WN *cond)
{
  WN *branch = WN_CreateTruebr(label, cond);
  WN_Set_Linenum(branch, current_srcpos);
  return branch;
}

static WN *WN_Goto(LABEL_IDX label)
{
  WN *branch =	WN_CreateGoto((ST_IDX) 0, label);
  WN_Set_Linenum(branch, current_srcpos);
  return branch;
}

static BOOL expr_is_speculative(WN *tree)
{
  if (OPT_Lower_Speculate)
  {
    BOOL  speculate;

    speculate = WN_Expr_Can_Be_Speculative(tree, alias_manager);
    if (traceSpeculate && speculate)
    {
      DevWarn("WN_lower: found speculative expression: line %d",
	      Srcpos_To_Line(current_srcpos));
    }
    return speculate;
  }
  return FALSE;
}

/* ====================================================================
 *
 *
 * Create a Nary representation of the expression passed  
 *
 * ==================================================================== */

static WN *WN_Nary(WN *tree, WN *x0, WN *x1, WN *x2, WN *x3)
{
  TYPE_ID	type = WN_rtype(tree);
  OPCODE	op;
  INTRINSIC	id;

  op = OPCODE_make_op (OPR_INTRINSIC_OP, type, MTYPE_V);
 /*
  *  figure out id from tree
  */
  switch(WN_operator(tree))
  {
  case OPR_ADD:
  case OPR_SUB:
    id = INTRN_NARY_ADD;
    break;
  case OPR_MPY:
    id = INTRN_NARY_MPY;
    break;
  default:	
    Is_True(FALSE,("unexpected nary op"));
  }

  {
    WN		*args[4], **kids;
    INT16	i, n, argN;

   /*
    *  if the child is a nary op of the same type, integrate it
    */
    n= argN = 0;
    if (x0)
      args[n++] = x0;
    if (x1)
      args[n++] = x1;
    if (x2)
      args[n++] = x2;
    if (x3)
      args[n++] = x3;

    for(i=0; i<n; i++)
    {
      WN *wn = (WN *) args[i];

      if (WN_opcode(wn) == op && WN_intrinsic(wn) == id)
	argN += WN_kid_count(wn);
      else
        argN++;
    }

    kids = (WN **) alloca(argN * sizeof(WN *));

    for(i= argN= 0; i<n; i++)
    {
      WN *wn = args[i];

      if (WN_opcode(wn) == op && WN_intrinsic(wn) == id)
      {
	INT16  j;

	for(j=0; j< WN_kid_count(wn); j++)
	  kids[argN++] = WN_kid(wn, j);

	WN_Delete(wn);
      }
      else
      {
	kids[argN++] = wn;
      }
    }

    WN_Delete(tree);
    tree = WN_Create_Intrinsic(op, id, argN, kids);
  }
  return tree;
}





/* ====================================================================
 *
 * WN *WN_ExprToNary(WN *tree, TYPE_ID type)
 *
 * Create a Nary representation of the expression passed  
 *
 * The nary representation looks like an INTRINSIC_OP 	 
 * with an id = INTRN_NARY_ADD | INTRN_NARY_MPY		
 *
 * ==================================================================== */
extern WN *WN_ExprToNaryType(WN *tree, TYPE_ID type, INT count)
{
  WN	*l, *r;

  if (count > 16) {
	Lmt_DevWarn(1,("WN_ExprToNaryType more than 16 deep, so stop"));
	return tree;
  }
  switch(WN_operator(tree))
  {
  case OPR_ADD:
  case OPR_MPY:
    l = WN_kid0(tree);
    r = WN_kid1(tree);

    if ((WN_opcode(tree) == WN_opcode(l)) &&
        (WN_opcode(tree) == WN_opcode(r)))
    {
      WN_kid0(l) = WN_ExprToNaryType(WN_kid0(l), type, count+1);
      WN_kid1(l) = WN_ExprToNaryType(WN_kid1(l), type, count+1);
      WN_kid0(r) = WN_ExprToNaryType(WN_kid0(r), type, count+1);
      WN_kid1(r) = WN_ExprToNaryType(WN_kid1(r), type, count+1);

      tree = WN_Nary(tree, WN_kid0(l), WN_kid1(l), WN_kid0(r), WN_kid1(r));
      WN_Delete(l);
      WN_Delete(r);
    }
    else if (WN_opcode(tree) == WN_opcode(l))
    {
      WN_kid0(l) = WN_ExprToNaryType(WN_kid0(l), type, count+1);
      WN_kid1(l) = WN_ExprToNaryType(WN_kid1(l), type, count+1);

      tree = WN_Nary(tree, WN_kid0(l), WN_kid1(l), r, NULL);
      WN_Delete(l);
    }
    else if (WN_opcode(tree) == WN_opcode(r))
    {
      WN_kid0(r) = WN_ExprToNaryType(WN_kid0(r), type, count+1);
      WN_kid1(r) = WN_ExprToNaryType(WN_kid1(r), type, count+1);

      tree = WN_Nary(tree, WN_kid0(r), WN_kid1(r), l, NULL);
      WN_Delete(r);
    }
    break;

  case OPR_SUB:
    {
      l = WN_kid0(tree);
      r = WN_kid1(tree);

      if (WN_operator_is(l, OPR_ADD))
      {
	TYPE_ID	rtype = WN_rtype(tree);

	WN  *neg = WN_Neg(rtype, r);

	WN_kid0(l) = WN_ExprToNaryType(WN_kid0(l), type, count+1);
	WN_kid1(l) = WN_ExprToNaryType(WN_kid1(l), type, count+1);

	tree = WN_Nary(tree, WN_kid0(l), WN_kid1(l), neg, NULL);
	WN_Delete(l);
      }
    }
    break;
  }
  return tree;
}



/* ====================================================================
 *
 * WN *WN_NaryToExpr(WN *tree)
 *
 * The nary representation looks like an INTRINSIC_OP 	 
 * with an id = INTRN_NARY_ADD | INTRN_NARY_MPY		
 *
 * ==================================================================== */
extern WN *WN_NaryToExpr(WN *tree)
{
  if (WN_nary_intrinsic(tree))
  {
    INT16	i;
    WN		*wn = WN_kid0(tree);
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);
    TYPE_ID	rtype = WN_rtype(tree);
    INT 	num_parms = WN_kid_count(tree);

    for (i = 1; i < num_parms; i++)
    {
      WN *actual = WN_kid(tree, i);

      actual = WN_NaryToExpr(actual);

      switch(id)
      {
      case INTRN_NARY_ADD:
	if (WN_operator_is(actual, OPR_NEG))
	{
	  wn = WN_Sub(rtype, wn, WN_kid0(actual));
	  WN_Delete(actual);
	}
	else
	{
	  wn = WN_Add(rtype, wn, actual);
	}
        break;
      case INTRN_NARY_MPY:
	wn = WN_Mpy(rtype, wn, actual);
        break;
      }
    }
    return wn;
  }

  return tree;
}



/* ====================================================================
 *
 * WN *WN_NaryDelete(WN *tree, INT32 n)
 *
 * Delete the nth kid of a nary intrinsic op
 * The rest of the children get moved and the num_kids are updated
 *
 * ==================================================================== */

static WN *WN_NaryDelete(WN *tree, INT32 n)
{
  INT32	i;
  INT32	num_kids= WN_kid_count(tree);

  Is_True((n<num_kids),("cannot delete nth kid"));
  Is_True(WN_nary_intrinsic(tree),("expected nary op"));

  for(i=n+1; i<num_kids; i++)
  {
    WN_actual(tree, i-1) = WN_actual(tree, i);
  }
  WN_set_kid_count(tree, WN_kid_count(tree)-1);

  return tree;
}

/* ====================================================================
 *
 * Get offset field (avoid preg offsets)
 * amazing I cannot find anything like this
 *
 * ==================================================================== */

extern INT64 lower_offset(WN *tree, INT64 offset)
{
  if (WN_has_offset(tree))
  {
    switch(WN_operator(tree))
    {
    case OPR_LDA:
      offset +=	WN_lda_offset(tree);
      break;
    case OPR_MSTORE:
    case OPR_ISTORE:
    case OPR_ISTOREX:
      offset +=	WN_store_offset(tree);
      break;
    case OPR_STID:
      if (WN_class(tree) != CLASS_PREG)
        offset +=	WN_store_offset(tree);
      break;
    case OPR_LDID:
      if (WN_class(tree) != CLASS_PREG)
	offset +=	WN_load_offset(tree);
      break;
    case OPR_MLOAD:
    case OPR_ILOAD:
    case OPR_ILOADX:
      offset +=	WN_load_offset(tree);
      break;
    }
  }
  return offset;
}


/* ====================================================================
 *
 * Compute alignment consistent with address and offset
 *
 * The new alignment may be improved (or not. see pv [559228])
 *
 * ==================================================================== */
extern TY_IDX compute_alignment_type(WN *tree, TY_IDX type, INT64 offset)
{
  INT32 newAlign;

  newAlign=	compute_alignment(tree, lower_offset(tree, 0));

  newAlign=	compute_offset_alignment(offset, newAlign);

  if (TY_align(type) != newAlign)
    Set_TY_align(type, newAlign);

  return type;
}

static INT32 compute_alignment(WN *tree, INT64 offset)
{
  WN	  *addr;
  TY_IDX   type;
  INT32	   align, align0, align1;

  switch(WN_operator(tree))
  {
  case OPR_MSTORE:
    type = TY_pointed(Ty_Table[WN_ty(tree)]);
    addr = WN_kid1(tree);

    if (WN_has_sym(addr) && WN_ty(addr))
    {
      return compute_alignment(addr, offset+WN_lda_offset(addr));
    }
    align = TY_align(type);
    break;

  case OPR_MLOAD:
    type = TY_pointed(Ty_Table[WN_ty(tree)]);
    addr = WN_kid0(tree);

    if (WN_has_sym(addr) && WN_ty(addr))
    {
      return compute_alignment(addr, offset+WN_lda_offset(addr));
    }
    align = TY_align(type);
    break;

  case OPR_ILOAD:
  case OPR_ILOADX:
  case OPR_LDA:
  case OPR_LDID:
    type = WN_ty(tree);
    if (TY_is_pointer(Ty_Table[type]))
    {
      type = TY_pointed(Ty_Table[type]);
    }
    else
    {
      return 1;
    }
    align = TY_align(type);
    break;

  case OPR_ARRAY:
    align=	compute_alignment(WN_array_base(tree), offset);
    offset=	WN_element_size(tree);
    break;

  case OPR_ADD:
  case OPR_SUB:
    align0=	compute_alignment(WN_kid0(tree), 0);
    align1=	compute_alignment(WN_kid1(tree), 0);
    align=	MIN(align0, align1);
    break;

  case OPR_INTCONST:
    offset=	WN_const_val(tree);
    align=	MTYPE_alignment(Max_Uint_Mtype);
    break;

  default:
    if (traceAlignment)
    {
      DevWarn("compute_alignment(): unrecognized WN returning alignment of 1");
    }
    return 1;
  }

  align=	compute_offset_alignment(offset, MAX(1, align));

  if (WN_has_sym(tree))
  {
    INT32	newAlign = align;
    ST		*sym = WN_st(tree);

    if (WN_operator_is(tree, OPR_LDA))
    {
      newAlign=	ST_alignment(sym);
    }
    else if (WN_operator_is(tree, OPR_LDID)	&&
	     ST_type(sym)			&&
	     TY_is_pointer(Ty_Table[ST_type(sym)]))
    {
      newAlign = TY_align( TY_pointed(Ty_Table[ST_type(sym)]));
    }

    align = compute_offset_alignment(offset, MAX(newAlign, align));
  }
  return align;
}


/* ====================================================================
 *
 * BOOL lower_is_aliased(WN *wn1, WN *wn2, INT64 size)
 *
 * Are these addresses aliased? (used in bcopy/memcpy/memmove)
 *
 * ==================================================================== */
extern BOOL lower_is_aliased(WN *wn1, WN *wn2, INT64 size)
{
  if (alias_manager &&
      Valid_alias(alias_manager, wn1) &&
      Valid_alias(alias_manager, wn2) &&
      (Aliased(alias_manager, wn1, wn2) == NOT_ALIASED))
  {
    return FALSE;
  }

  if (WN_operator_is(wn1, OPR_LDA) && WN_operator_is(wn2, OPR_LDA)) {
    ST	*sym1 = WN_st(wn1);
    ST	*sym2 = WN_st(wn2);
    ST	*base1, *base2;
    INT64 newoffset1, newoffset2;

    if (sym1 != sym2) return FALSE;

    Base_Symbol_And_Offset_For_Addressing(
		    sym1, WN_lda_offset(wn1), &base1, &newoffset1);
    Base_Symbol_And_Offset_For_Addressing(
		    sym2, WN_lda_offset(wn2), &base2, &newoffset2);

    if (ABS(newoffset1 - newoffset2) >= size) return FALSE;
  }

  return TRUE;
}



/* ====================================================================
 *
 * WN *lower_copy_tree(WN *tree, INT32 n)
 *
 * Copy the tree and duplicate the maps
 *
 * ==================================================================== */

static void lower_tree_copy_maps(WN *tree, WN *dup, LOWER_ACTIONS actions)
{
  if (tree == NULL)
  {
    Is_True((dup == NULL),("inconsistency while copying trees"));
  }
  Is_True((WN_opcode(tree) == WN_opcode(dup)),
	  ("inconsistency while copying trees"));

  if (WN_has_map_id(tree) && WN_has_alias_info(tree))
  {
    lower_copy_maps(tree, dup, actions);
  }
	
  if (WN_opcode(tree) == OPC_BLOCK)
  {
    WN  *treeStmt = WN_first(tree);
    WN  *dupStmt  = WN_first(dup);

    while(treeStmt)
    {
      lower_tree_copy_maps(treeStmt, dupStmt, actions);
      treeStmt = WN_next(treeStmt);
      dupStmt  = WN_next(dupStmt);
    }
  }
  else
  {
    INT	n;
    for(n = 0; n < WN_kid_count(tree); n++)
    {
      if (WN_kid(tree, n))
      {
	lower_tree_copy_maps(WN_kid(tree,n), WN_kid(dup,n), actions);
      }
    }
  }
}

static WN *lower_copy_tree(WN *tree, LOWER_ACTIONS actions)
{
  WN  *dup;

  dup = WN_COPY_Tree(tree);

  lower_tree_copy_maps(tree, dup, actions);

  return dup;
}

/* ====================================================================
 *
 * PREG_NUM AssignPregExprPos(WN *block, WN *tree, TY_IDX ty, SRCPOS srcpos,
 *                            LOWER_ACTIONS actions)
 *
 * PREG_NUM AssignExprPos(WN *block, WN *tree, TYPE_ID type, SRCPOS srcpos,
 *                        LOWER_ACTIONS)
 *
 * PREG_NUM AssignExpr(WN *block, WN *tree, TYPE_ID type)
 *
 * PREG_NUM AssignExprTY(WN *block, WN *tree, TY_IDX ty)
 *
 * Allocate a preg of type ST_type(preg) and assign expression tree to it
 * and attach it to block. 
 *
 * Assign srcpos (if not NULL)
 *
 * ==================================================================== */

static PREG_NUM AssignPregExprPos(WN *block, WN *tree, TY_IDX ty,
				  SRCPOS srcpos, LOWER_ACTIONS actions)
{
  PREG_NUM	pregNo;
  TYPE_ID	type;
  ST		*preg = MTYPE_To_PREG(TY_mtype(Ty_Table[ty]));

  Is_True((WN_operator_is(tree, OPR_PARM)==FALSE),("bad parm"));

  type = TY_mtype(Ty_Table[ty]);
  pregNo = Create_Preg(type, current_preg_name);

  {
    WN	*stBlock, *stid;

    stid = WN_Stid(type, pregNo, preg, ty, tree);

    if (srcpos)
      WN_Set_Linenum (stid, srcpos);

    stBlock = WN_CreateBlock();

   /*
    *	This lowering may leed to infinite regress if the
    * 	children cannot be lowered (and are allocated a temp, for example) 
    */
    if (actions)
      stid = lower_store(stBlock, stid, actions);

    WN_INSERT_BlockLast(stBlock, stid);

    WN_INSERT_BlockLast(block, stBlock);
  }

  return pregNo;
}


extern PREG_NUM AssignExprTY(WN *block, WN *tree, TY_IDX type)
{
  return AssignPregExprPos(block, tree, type, current_srcpos,
			   current_actions);
}

extern PREG_NUM AssignExpr(WN *block, WN *tree, TYPE_ID type)
{
  return AssignPregExprPos(block, tree, MTYPE_To_TY(type), current_srcpos,
			   current_actions);
}



static BOOL WN_unconditional_goto(WN *tree)
{
  switch(WN_operator(tree))
  {
  case OPR_GOTO:
  case OPR_REGION_EXIT:
  case OPR_RETURN:
  case OPR_RETURN_VAL:
    return TRUE;
  }
  return FALSE;
}




/* ====================================================================
 *
 * PARITY WN_parity(WN *tree)
 *
 * return the PARITY associated with a tree.
 *
 * Parity encapsulates dependence information, like complex real, imag
 * ==================================================================== */

PARITY WN_parity(WN *tree)
{
  if (WN_map_id(tree) != -1)
  {
    INT32	map;

    Is_True((lowering_parity_map != 0), ("parity map not initialized"));
    map = WN_MAP32_Get(lowering_parity_map, tree);
   
    if (map!=0)
      return (PARITY) map;
  }
  return PARITY_UNKNOWN;
}

BOOL WN_parity_independent(WN *wn1, WN *wn2)
{

  if (wn1 == NULL		||
      wn2 == NULL		||
      WN_map_id(wn1) == -1	||
      WN_map_id(wn2) == -1)
    return FALSE;

  {
    PARITY p1 = WN_parity(wn1);
    PARITY p2 = WN_parity(wn2);

   return (p1 & p2) ? FALSE : TRUE;
  }
}




/* ====================================================================
 *
 *	MAP PRESERVATION
 *
 *
 * void lower_copy_maps(WN *orig, WN *tree, LOWER_ACTIONS action)
 *
 * void lower_complex_maps(WN *orig, WN *real, WN *imag, LOWER_ACTIONS action)
 *
 * void lower_quad_maps(WN *orig, WN *hipart, WN *lopart, LOWER_ACTIONS action)
 *
 *
 *  copy alias information to tree
 *
 * ==================================================================== */

static void lower_maps_init(LOWER_ACTIONS actions)
{

}

static void lower_maps_reset(LOWER_ACTIONS actions)
{
  if (Action(LOWER_DEPGRAPH_MAPS))
  {
    LNOPruneMapsUsingParity();
  }
}

static void lower_map(WN *tree, LOWER_ACTIONS actions)
{
  if (Action(LOWER_ALIAS_MAPS))
  {
    if (alias_manager)
    {
      if (Valid_alias(alias_manager, tree) == FALSE)
	Create_alias(alias_manager, tree);
    }
  }
}

static void lower_copy_maps(WN *orig, WN *tree, LOWER_ACTIONS actions)
{
  if (orig == NULL)
    return;

 /*
  *	The tree may no longer be valid at the point of call
  *	(ie. may be deleted) so we must check validity
  */
  if (WN_has_map_id(orig))
  {
    if (WN_has_alias_info(orig)	&&
        WN_has_alias_info(tree))
    {
      if (Action(LOWER_PREFETCH_MAPS))
      {
        WN_CopyMap(tree, WN_MAP_PREFETCH, orig);
      }
      if (Action(LOWER_ALIAS_MAPS))
      {
        if (alias_manager)
	  Copy_alias_info(alias_manager, orig, tree);
      }
    }
  }
  WN_Set_Flags(orig, tree);
}


/*
 * If an original node has a TY with an f90_pointer attribute on it, copy it
 * to the TYs of the new nodes. node2 might be NULL (for cases in which we
 * only produce one new node)
 */
static void lower_copy_tys (WN *orig, WN *node1, WN *node2) 
{
  TY_IDX  ty;

  if (WN_operator_is(orig, OPR_ILOAD))
  {
    ty =	WN_load_addr_ty(orig);

    if (TY_is_f90_pointer(Ty_Table[ty]))
    {
      if (node1 && WN_operator_is(node1, OPR_ILOAD))
	WN_set_load_addr_ty(node1, ty);

      if (node2 && WN_operator_is(node2, OPR_ILOAD))
	WN_set_load_addr_ty(node2, ty);
    }
  }
  else if (WN_operator_is(orig, OPR_ISTORE))
  {
    ty =	WN_ty(orig);
    if (TY_is_f90_pointer(Ty_Table[ty]))
    {
      if (node1 && WN_operator_is(node1, OPR_ISTORE))
	WN_set_ty(node1, ty);
      if (node2 && WN_operator_is(node2, OPR_ISTORE))
	WN_set_ty(node2, ty);
    }
  }
}


static void lower_quad_maps(WN *orig, WN *hipart, WN *lopart,
			    LOWER_ACTIONS actions)
{
  lower_copy_maps(orig, hipart, actions);
  lower_copy_maps(orig, lopart, actions);
  lower_copy_tys(orig,hipart,lopart);

  if (Action(LOWER_DEPGRAPH_MAPS))
  {
    LNOPreserveMapPair(orig, hipart, lopart);
  }

  if (Action(LOWER_PARITY_MAPS))
  {
    WN_MAP32_Set(lowering_parity_map, hipart, PARITY_QUAD_HI);
    WN_MAP32_Set(lowering_parity_map, lopart, PARITY_QUAD_LO);
  }
}

static void lower_complex_maps(WN *orig, WN *real, WN *imag,
			       LOWER_ACTIONS actions)
{
  lower_copy_maps(orig, real, actions);
  lower_copy_maps(orig, imag, actions);
  lower_copy_tys(orig,real,imag);

  if (Action(LOWER_DEPGRAPH_MAPS))
  {
    LNOPreserveMapPair(orig, real, imag);
  }

  if (Action(LOWER_PARITY_MAPS))
  {
    WN_MAP32_Set(lowering_parity_map, real, PARITY_COMPLEX_REAL);
    WN_MAP32_Set(lowering_parity_map, imag, PARITY_COMPLEX_IMAG);
  }

}

static WN *add_to_base(WN **base, WN *tree)
{
  if (*base)
  {
    return WN_Add(Pointer_type, *base, tree);
  }
  return tree;
}

static WN *sub_from_base(WN **base, WN *tree)
{
  if (*base)
  {
    return WN_Sub(Pointer_type, *base, tree);
  }
  return tree;
}

static BOOL baseAddress(WN *tree)
{
  switch(WN_operator(tree))
  {
  case OPR_LDA:
  case OPR_LDID:
  case OPR_ILOAD:
  case OPR_ILOADX:
    return TRUE;
  }
  return FALSE;
}


/* ====================================================================
 *
 * void lower_to_base_index(WN *addr, WN **base, WN **index) 
 *
 * Pattern match an address and create a bad/index for it
 *
 * Before this routine we would store the address in a preg
 * and use it (twice).  This would require the ST being marked
 * addr_taken_stored (very bad)
 *
 * This is getting worse and worse. Now the routine is recursive
 * for OPR_ADD.
 * 
 * 17 Dec 1998, R. Shapiro - Add a default case so that this routine never 
 *   fails. At worst, the trees get a little bigger. 
 *
 * ==================================================================== */

static void lower_to_base_index(WN *addr, WN **base, WN **index) 
{
  WN	*l, *r;

  switch (WN_operator(addr))
  {
  case OPR_ARRAY:
    *base  = add_to_base(base, WN_array_base(addr));
    *index = add_to_base(index, addr);
    WN_array_base(addr) = WN_Zerocon(WN_rtype(addr));
    break;

  case OPR_ADD:
    l = WN_kid0(addr);
    r = WN_kid1(addr);

    if (baseAddress(l))
    {
      *base  = add_to_base(base, l);
      *index = add_to_base(index, r);
    }
    else if (baseAddress(r))
    {
      *base  = add_to_base(base, r);
      *index = add_to_base(index, l);
    }
    else if (WN_operator_is(r, OPR_ARRAY))
    {
      *base  = add_to_base(base, WN_array_base(r));
      WN_array_base(r) = WN_Zerocon(WN_rtype(r));
      *index = add_to_base(index, addr);
    }
    else if (WN_operator_is(l, OPR_ARRAY))
    {
      *base  = add_to_base(base, WN_array_base(l));
      WN_array_base(l) = WN_Zerocon(WN_rtype(l));
      *index = add_to_base(index, addr);
    }
    else if ((WN_operator_is(r, OPR_ADD)  ||  WN_operator_is(r, OPR_SUB))
	     && WN_operator_is(l, OPR_INTCONST))
    {
      lower_to_base_index(r, base, index);
      *index = add_to_base(index, l);
    }
    else if ((WN_operator_is(l, OPR_ADD)  ||  WN_operator_is(l, OPR_SUB)) 
	     && WN_operator_is(r, OPR_INTCONST))
    {
      lower_to_base_index(l, base, index);
      *index = add_to_base(index, r);
    }
    else
    {
      // Give up
      *base  = add_to_base(base, addr);
      *index = add_to_base(index, WN_Zerocon(WN_rtype(addr)));
    }
    break;

  case OPR_SUB:
    l = WN_kid0(addr);
    r = WN_kid1(addr);

    if (baseAddress(l))
    {
      *base  = add_to_base(base, l);
      *index = sub_from_base(index, r);
    }
    else if (baseAddress(r))
    {
      *base  = sub_from_base(base, r);
      *index = add_to_base(index, l);
    }
    else if (WN_operator_is(r, OPR_ARRAY))
    {
      *base  =	sub_from_base(base, WN_array_base(r));
      WN_array_base(r) = WN_Zerocon(WN_rtype(r));
      *index =	add_to_base(index, addr);
    }
    else if (WN_operator_is(l, OPR_ARRAY))
    {
      *base  = add_to_base(base, WN_array_base(l));
      WN_array_base(l) = WN_Zerocon(WN_rtype(l));
      *index = add_to_base(index, addr);
    }
    else if ((WN_operator_is(r, OPR_ADD) ||  WN_operator_is(r, OPR_SUB))
	     && WN_operator_is(l, OPR_INTCONST))
    {
      lower_to_base_index(r, base, index);
      *base  = WN_Neg(WN_rtype(*base),*base);
      *index = sub_from_base(&l,*index);
    }
    else if ((WN_operator_is(l, OPR_ADD) || WN_operator_is(l, OPR_SUB)) 
	     && WN_operator_is(r, OPR_INTCONST))
    {
      lower_to_base_index(l, base, index);
      *index = sub_from_base(index, r);
    }
    else
    {
      // Give up
      *base  = add_to_base(base, addr);
      *index = add_to_base(index, WN_Zerocon(WN_rtype(addr)));
    }
    break;
  default:
    // Give up
    *base  = add_to_base(base, addr);
    *index = add_to_base(index, WN_Zerocon(WN_rtype(addr)));
    break;
  }
}


/*
 * Describe a leaf expression (see Make_Leaf/Load_Leaf).
 */
typedef enum {LEAF_IS_CONST, LEAF_IS_INTCONST, LEAF_IS_PREG} LEAF_KIND;
typedef struct {
  LEAF_KIND kind;
  TYPE_ID type;
  union {
    PREG_NUM n;
    INT64 intval;
    TCON tc;
  } u;
} LEAF;

/* ====================================================================
 *
 * LEAF Make_Leaf(WN *block, WN *tree, TYPE_ID type)
 *
 * Make an aribtrary expression tree into a leaf.
 * If the expression is an integer or floating point constant
 * no transformation is made. However, other expressions are stored
 * into a PREG.
 *
 * Make_Leaf is used in place of AssignExpr when performing a sort
 * of "poor man's" CSE, and you want to avoid creating unnecessary
 * PREGs for constants (which can also thwart the simplifier).
 *
 * ==================================================================== */

static LEAF Make_Leaf(WN *block, WN *tree, TYPE_ID type)
{
  LEAF leaf;
  leaf.type = type;
  switch (WN_operator(tree)) {
  case OPR_CONST:
    leaf.kind = LEAF_IS_CONST;
    leaf.u.tc = Const_Val(tree);
    WN_Delete(tree);
    break;
  case OPR_INTCONST:
    leaf.kind = LEAF_IS_INTCONST;
    leaf.u.intval = WN_const_val(tree);
    WN_Delete(tree);
    break;
  default:
    leaf.kind = LEAF_IS_PREG;
    leaf.u.n = AssignExpr(block, tree, type);
    break;
  }
  return leaf;
}

/* ====================================================================
 *
 * WN *Load_Leaf(const LEAF &leaf)
 *
 * Generate whirl to load the value of a leaf expression created by
 * Make_Leaf().
 *
 * ==================================================================== */

static WN *Load_Leaf(const LEAF &leaf)
{
  switch (leaf.kind) {
  case LEAF_IS_CONST:
    return Make_Const(leaf.u.tc);
  case LEAF_IS_INTCONST:
    return WN_CreateIntconst(OPR_INTCONST, leaf.type, MTYPE_V, leaf.u.intval);
  case LEAF_IS_PREG:
    return WN_LdidPreg(leaf.type, leaf.u.n);
  }
  FmtAssert(FALSE, ("unhandled leaf kind in Load_Leaf"));
  /*NOTREACHED*/
}

/* ====================================================================
 *
 * void lower_complex_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				 WN **realpart, WN **imagpart)
 *
 * Split complex-type expression <tree> into its lowered real and
 * imaginary parts.  <actions> must include LOWER_COMPLEX.  Note that
 * like the other lowering functions, this one destroys <tree>.  Upon
 * return, *<realpart> points to the real part, and *<imagpart> points
 * to the imaginary part.
 *
 * ==================================================================== */

static void lower_complex_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
			       WN **realpart, WN **imagpart)
{
  TYPE_ID	type;


  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(Action(LOWER_COMPLEX), ("actions does not contain LOWER_COMPLEX"));
  Is_True((MTYPE_is_complex(WN_rtype(tree))),
	  ("expected complex-type node, not %s",
	   OPCODE_name(WN_opcode(tree))));
  type = Mtype_complex_to_real(WN_rtype(tree));

 /*
  *  Complex Arithmetic Notation
  *
  *  Let R(c) = real part of c
  *      I(c) = imaginary part of c
  *
  *  then, if z is complex it can be represented as follows
  *
  *      z= R(z) + I(z)i
  *
  */
  switch (WN_operator(tree))
  {
  case OPR_LDID:
    {
     /*
      *  we must create a new ST of type real
      *  we implicitly assume the storage of complex is
      *		 z =  {  R(z) , I(z) }
      */
      TY_IDX    beTY   = MTYPE_To_TY(type);
      WN_OFFSET offset = WN_load_offset(tree);

      if (WN_class(tree) == CLASS_CONST && offset == 0)
      {
	TCON	val = WN_val(tree);
	TYPE_ID	valType = WN_val_type(tree);

	if (WN_rtype(tree) == valType)
	{
	  *realpart = Make_Const( Extract_Complex_Real(val));
	  *imagpart = Make_Const( Extract_Complex_Imag(val));
 	  break;
	}
      }

      *realpart = WN_Ldid(type, offset,
			  coerceST(WN_st(tree), type),
			  beTY);

      *imagpart = WN_Ldid(type,
			  coerceOFFSET(tree, type, offset),
			  coerceST(WN_st(tree), type),
			  beTY);
    }
    break;

  case OPR_ILOAD:
    {
     /*
      *  we implicitly assume the storage of complex is
      *    z =  {  R(z) , I(z) }
      *
      *  The LOWER_BASE_INDEX will try to split the address into a
      *  base and index. The index is put in a preg (and reused) while
      *  the base is cloned.
      */
      WN_OFFSET offset = WN_load_offset(tree);

      if (Action(LOWER_BASE_INDEX))
      {
	WN	*addr, *base, *index;
	LEAF	indexN;

	base = index=	NULL;
	lower_to_base_index(WN_kid0(tree), &base, &index) ;

	base = lower_expr(block, base, actions);
	index = lower_expr(block, index, actions);

	indexN = Make_Leaf(block, index, Pointer_type);
	
	addr = WN_Add(Pointer_type,
		      Load_Leaf(indexN),
		      lower_copy_tree(base, actions)); 

	*realpart = WN_Iload(type,
			     offset,
			     coerceTY(WN_ty(tree), type),
			     addr);

	addr = WN_Add(Pointer_type, Load_Leaf(indexN), base);

	*imagpart = WN_Iload(type,
			     coerceOFFSET(tree, type, offset),
			     coerceTY(WN_ty(tree), type),
			     addr);
      }
      else
      {
	WN	*addr;
	LEAF	addrN;
	
	addr = lower_expr(block, WN_kid0(tree), actions);
	
	addrN = Make_Leaf(block, addr, Pointer_type);
	
	*realpart = WN_Iload(type,
			     offset,
			     coerceTY(WN_ty(tree), type),
			     Load_Leaf(addrN));
	
	*imagpart = WN_Iload(type,
			     coerceOFFSET(tree, type, offset),
			     coerceTY(WN_ty(tree), type),
			     Load_Leaf(addrN));
      }
      lower_complex_maps(tree, *realpart, *imagpart, actions);
    }
    break;

  case OPR_ILOADX:
    Is_True(FALSE, ("unexpected complex ILOADX"));
    break;

  case OPR_NEG:
    {
     /*
      *    -z  = -R(z) + -I(z)i
      */
      WN	*rz, *iz;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      *realpart = WN_Neg( type, rz);
      *imagpart = WN_Neg( type, iz);

    }
    break;

  case OPR_ADD:
    {
      WN	*rz, *rw, *iz, *iw;
     /*
      *    z + w = (R(z) + R(w)) + (I(z) + I(w))i
      */

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);
 
      *realpart = WN_Add( type, rz, rw);
      *imagpart = WN_Add( type, iz, iw);
    }
    break;

  case OPR_SUB:
    {
     /*
      *    z - w = (R(z) - R(w)) + (I(z) - I(w))i
      */
      WN	*rz, *rw, *iz, *iw;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);
 
      *realpart = WN_Sub( type, rz, rw);
      *imagpart = WN_Sub( type, iz, iw);
    }
    break;

  case OPR_MPY:
    {
     /*
      *    z * w = (R(z)*R(w) - I(z)*I(w)) + (R(z)*I(w)+ R(w)*I(z))i
      *
      */
      WN	*rz, *rw, *iz, *iw;
      LEAF	rzN, rwN, izN, iwN;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);

      rzN = Make_Leaf(block, rz, type);
      rwN = Make_Leaf(block, rw, type);
      izN = Make_Leaf(block, iz, type);
      iwN = Make_Leaf(block, iw, type);

      *realpart = WN_Sub(type,
			 WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(rwN)),
			 WN_Mpy(type, Load_Leaf(izN), Load_Leaf(iwN)));

      *imagpart = WN_Add(type,
			 WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(iwN)),
			 WN_Mpy(type, Load_Leaf(rwN), Load_Leaf(izN)));
    }
    break;

  case OPR_COMPLEX:
    /*
     *	Create a complex number from two real children, ie.
     *    z = CMPLX(x,y)
     *    z = (x) + (y) i
     */
    *realpart = lower_expr(block, WN_kid0(tree), actions);
    *imagpart = lower_expr(block, WN_kid1(tree), actions);
    WN_Delete(tree);
    break;

  case OPR_MADD:
  case OPR_MSUB:
  case OPR_NMADD:
  case OPR_NMSUB:
    Is_True( FALSE, ("unexpected complex madd"));
    break;

  case OPR_CVT:
    {
      WN	*rz, *iz;
      TYPE_ID	desc = WN_desc(tree);

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
 
      *realpart = WN_Cvt( type, Mtype_complex_to_real(desc), rz);
      *imagpart = WN_Cvt( type, Mtype_complex_to_real(desc), iz);
    }
    break;

  case OPR_CONST:
    {
     /*
      *	extract the real and imaginary parts of the complex number
      */
      TCON	val = Const_Val(tree);

      *realpart = Make_Const( Extract_Complex_Real(val));
      *imagpart = Make_Const( Extract_Complex_Imag(val));

      WN_Delete(tree);
    }
    break;

  case OPR_RSQRT:
    {
      TYPE_ID	desc = WN_desc(tree);
      WN	*div;

      div = WN_Div(desc,
		   WN_Floatconst(desc, 1.0),
		   WN_Sqrt(desc, WN_kid0(tree)));

      lower_complex_expr(block, div, actions, realpart, imagpart);

      WN_Delete(tree);
    }
    break;

  case OPR_SQRT:
    lower_complex_emulation(block, tree, actions, realpart, imagpart);
    break;


  case OPR_PAREN:
    {
      lower_complex_expr(block, WN_kid0(tree), actions, realpart, imagpart);

      *realpart = WN_Paren(type, *realpart);
      *imagpart = WN_Paren(type, *imagpart);
      WN_Delete(tree);
    }
    break;

  case OPR_ARRAY:
    break;

  case OPR_RECIP:
    {
      /* TODO_FEEDBACK: Get frequencies right, especially when we
       * create an IF statement. We have to assume half the frequency
       * goes to the then part and half goes to the else part; there's
       * no other information available, is there?
       */

      LEAF	rzN, izN;
      {
       /*
	*  assign pregs to their corresponding expressions
	*  Since the expressions will be reused, this avoids building a DAG
	*/
	WN	*rz, *iz; 
    
	lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      
	rzN = Make_Leaf(block, rz, type);
	izN = Make_Leaf(block, iz, type);
      }
    
      if (Fast_Complex_Allowed)
      {
       /*
	*   1 / z
	*    real =	(   R(z) ) /  ( R(z)**2 + I(z)**2 )
	*    imag =	( - I(z) ) /  ( R(z)**2 + I(z)**2 )
	*
	*/
	LEAF	denomN;
	WN	*rz2, *iz2, *add;
    
	rz2 = WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(rzN));
	iz2 = WN_Mpy(type, Load_Leaf(izN), Load_Leaf(izN));
	add = WN_Add(type, rz2, iz2);
	denomN = Make_Leaf(block, add, type);
    
	*realpart = WN_Div(type, Load_Leaf(rzN), Load_Leaf(denomN));
	*imagpart = WN_Neg(type,
			   WN_Div(type, Load_Leaf(izN), Load_Leaf(denomN)));
      }
      else
      {
       /*
	*   1 / z
	*    real =	(   R(z) ) /  ( R(z)**2 + I(z)**2 )
	*    imag =	( - I(z) ) /  ( R(z)**2 + I(z)**2 )
	*
	*    After factoring out max( |R(z)| , |I(z)| )
	*
	*	| R(z) | >  | I(z) |
	*
	*	Let x = I(z) / R(z)
	*	real = ( 1 )    /  ( R(z) + I(z)*x)
	*	imag = ( - x )  /  ( R(z) + I(z)*x)
	*
	*	| I(z) | >  | R(z) |
	*
	*	Let x = R(z) / I(z)
	*	real = ( x )   /  ( R(z)*x + I(z) )
	*	imag = (-1 )   /  ( R(z)*x + I(z) )
	*
	*/
	WN		*if_then, *if_else, *IF;
	PREG_NUM	realpartN, imagpartN;
	LEAF		xN, arzN, aizN;
    
	{
	  WN	*numer, *denom, *div;
    
	  arzN = Make_Leaf(block,
			   WN_Abs(type, Load_Leaf(rzN)),
    			   type);
    
	  aizN = Make_Leaf(block,
    			   WN_Abs(type, Load_Leaf(izN)),
    			   type);
    
	 /*
	  *  numer =  | R(w) | >  | I(w) |  ?  I(w) : R(x)
	  *  denom =  | R(w) | >  | I(w) |  ?  R(w) : I(x)
	  *
	  *  Let x = numer / denom
	  */
	  numer = WN_Select(type,
			    WN_GT(type, Load_Leaf(arzN), Load_Leaf(aizN)),
    			    Load_Leaf(izN),
    			    Load_Leaf(rzN));
	  denom = WN_Select(type,
			    WN_GT(type, Load_Leaf(arzN), Load_Leaf(aizN)),
    			    Load_Leaf(rzN),
    		  	    Load_Leaf(izN));

	  div = WN_Div(type, numer, denom);

	  xN = Make_Leaf(block, div, type);
	}

	if_then = WN_CreateBlock();
	{
	 /*
	  *	scale = ( R(z) + I(z)*x )
	  *	real = ( 1 )    /  scale
	  *	imag = ( - x )  /  scale
	  */
	  WN	*scale, *div;
	  LEAF	scaleN;
    
	  scale = WN_Add(type,
			 Load_Leaf(rzN),
			 WN_Mpy(type, Load_Leaf(izN), Load_Leaf(xN)));
	  scaleN = Make_Leaf(if_then, scale, type);

	  div = WN_Inverse(type, Load_Leaf(scaleN));

	  realpartN = AssignExpr(if_then, div, type);

	  div =  WN_Div(type,
			WN_Neg(type, Load_Leaf(xN)),
			Load_Leaf(scaleN));
    
	  imagpartN = AssignExpr(if_then, div, type);
	}

	if_else = WN_CreateBlock();
	{
	 /*
	  *	scale =	( R(z)*x + I(z) )
	  *	real = ( x )   /  scale
	  *	imag = ( 1 )   /  scale
	  */
	  WN	*scale, *div, *stid;
	  LEAF	scaleN;
	  ST	*preg =	MTYPE_To_PREG(type);
    
	  scale = WN_Add(type,
			 WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(xN)),
			 Load_Leaf(izN));
	  scaleN = Make_Leaf(if_else, scale, type);

	  div =  WN_Div(type, Load_Leaf(xN), Load_Leaf(scaleN));
	  stid = WN_StidIntoPreg(type, realpartN, preg, div);
	  WN_INSERT_BlockLast(if_else, stid);
    
	  div = WN_Neg(type,WN_Inverse(type, Load_Leaf(scaleN)));
	  stid = WN_StidIntoPreg(type, imagpartN, preg, div);
	  WN_INSERT_BlockLast(if_else, stid);
	}
    
	IF =  WN_CreateIf(WN_GT(type, Load_Leaf(arzN), Load_Leaf(aizN)),
			  if_then, if_else);

	if ( Cur_PU_Feedback ) {
	  Cur_PU_Feedback->Annot( IF, FB_EDGE_BRANCH_TAKEN, FB_FREQ_UNKNOWN );
	  Cur_PU_Feedback->Annot( IF, FB_EDGE_BRANCH_NOT_TAKEN,
				  FB_FREQ_UNKNOWN );
	}

	WN_INSERT_BlockLast(block, lower_if(block, IF, actions));
    
	*realpart = WN_LdidPreg(type, realpartN);
	*imagpart = WN_LdidPreg(type, imagpartN);
      }
      WN_Delete(tree);
    }
    break;

  case OPR_DIV:
    {
     /*
      *  assign pregs to their corresponding expressions
      *  Since the expressions will be reused, this avoids building a DAG
      */
      WN	*rz, *rw, *iz, *iw; 
      LEAF	rzN, rwN, izN, iwN;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);
  
      rzN = Make_Leaf(block, rz, type);
      izN = Make_Leaf(block, iz, type);
      rwN = Make_Leaf(block, rw, type);
      iwN = Make_Leaf(block, iw, type);

      if (Fast_Complex_Allowed)
      {
       /*
	*   z / w
	*    real =	(R(z)*R(w) + I(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*    imag =	(I(z)*R(w) - R(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*
	*/
	LEAF	denomN;
	{
	  WN	*rw2, *iw2, *add;
    
	  rw2 = WN_Mpy(type,Load_Leaf(rwN), Load_Leaf(rwN));
	  iw2 = WN_Mpy(type, Load_Leaf(iwN), Load_Leaf(iwN));
	  add = WN_Add(type, rw2, iw2);
	  denomN = Make_Leaf(block, add, type);
	}
	{
	  WN	*rzrw, *iziw;
    
	  rzrw = WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(rwN));
	  iziw = WN_Mpy(type, Load_Leaf(izN), Load_Leaf(iwN));
	  *realpart = WN_Div(type,
    			 WN_Add(type, rzrw, iziw),
    			 Load_Leaf(denomN));
	}
	{
	  WN	*rziw, *izrw;
    
	  izrw = WN_Mpy(type, Load_Leaf(izN), Load_Leaf(rwN));
	  rziw = WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(iwN));
	  *imagpart = WN_Div(type,
    			 WN_Sub(type, izrw, rziw),
    			 Load_Leaf(denomN));
	}
      }
      else
      {
       /*
	*   z / w
	*    real =	(R(z)*R(w) + I(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*    imag =	(I(z)*R(w) - R(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*
	*    After factoring out max( |R(w)| , |I(w)| )
	*
	*	| R(w) | >  | I(w) |
	*
	*	Let x = I(w) / R(w)
	*	  real = ( R(z) + I(z)*x ) /  ( R(w) + I(w)*x )
	*	  imag = ( I(z) - R(z)*x ) /  ( R(w) + I(w)*x )
	*
	*	| I(w) | >  | R(w) |
	*
	*	Let x = R(w) / I(w)
	*	  real = ( R(z)*x + I(z) ) /  ( R(w)*x + I(w) )
	*	  imag = ( I(z)*x - R(z) ) /  ( R(w)*x + I(w) )
	*
	*/
	WN		*if_then, *if_else, *IF;
	LEAF		xN, arwN, aiwN;
	PREG_NUM	realpartN, imagpartN;
	{
	  WN	*numer, *denom, *div;
    
	  arwN = Make_Leaf(block,
    			   WN_Abs(type, Load_Leaf(rwN)),
    			   type);
	  aiwN = Make_Leaf(block,
    			   WN_Abs(type, Load_Leaf(iwN)),
    			   type);
    
	 /*
	  *  numer =  | R(w) | >  | I(w) |  ?  I(w) : R(x)
	  *  denom =  | R(w) | >  | I(w) |  ?  R(w) : I(x)
	  *
	  *  Let x = numer / denom
	  */
	  numer = WN_Select(type,
			    WN_GT(type, Load_Leaf(arwN), Load_Leaf(aiwN)),
    			    Load_Leaf(iwN),
			    Load_Leaf(rwN));
	  denom = WN_Select(type,
			    WN_GT(type, Load_Leaf(arwN), Load_Leaf(aiwN)),
    			    Load_Leaf(rwN),
    		 	    Load_Leaf(iwN));
	  div = WN_Div(type, numer, denom);
	  xN = Make_Leaf(block, div, type);
	}

	if_then = WN_CreateBlock();
	{
	  WN		*scale;
	  LEAF	scaleN;
    
	  scale = WN_Add(type,
			 Load_Leaf(rwN),
			 WN_Mpy(type, Load_Leaf(iwN), Load_Leaf(xN)));
	  scaleN = Make_Leaf(if_then, scale, type);
	  {
	   /*
    	    *  real = ( R(z)   + I(z)*x )  / scale
    	    */
    	    WN	*numer, *div;
    
    	    numer = WN_Add(type,
  			   Load_Leaf(rzN),
  			   WN_Mpy(type, Load_Leaf(izN), Load_Leaf(xN)));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
	    realpartN = AssignExpr(if_then, div, type);
	  }
	  {
	   /*
	    *  imag = ( I(z) - R(z)*x ) /  scale
    	    */
    	    WN	*numer, *div;
    
	    numer = WN_Sub(type,
    			   Load_Leaf(izN),
			   WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(xN)));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
    	    imagpartN = AssignExpr(if_then, div, type);
	  }
	}
    
	if_else = WN_CreateBlock();
	{
	  WN	*scale;
	  LEAF	scaleN;
	  ST	*preg =	MTYPE_To_PREG(type);

	  scale = WN_Add(type,
			 WN_Mpy(type, Load_Leaf(rwN), Load_Leaf(xN)),
    			 Load_Leaf(iwN));
	  scaleN = Make_Leaf(if_else, scale, type);
	  {
	   /*
    	    *  real = ( R(z)*x + I(z) ) /  scale
    	    *  store away result in an already defined preg
    	    */
    	    WN	*numer, *div, *stid;
    
    	    numer =  WN_Add(type,
    			    WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(xN)),
    			    Load_Leaf(izN));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
	    stid = WN_StidIntoPreg(type, realpartN, preg, div);
	    WN_INSERT_BlockLast(if_else, stid);
	  }
	  {
	   /*
	    *  imag = ( I(z)*x - R(z) ) /  scale
    	    *  store away result in an already defined preg
    	    */
    	    WN	*numer, *div, *stid;
    
    	    numer = WN_Sub(type,
    			   WN_Mpy(type, Load_Leaf(izN), Load_Leaf(xN)),
    			   Load_Leaf(rzN));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
	    stid = WN_StidIntoPreg(type, imagpartN, preg, div);
	    WN_INSERT_BlockLast(if_else, stid);
	  }
	}
    
	IF =  WN_CreateIf(WN_GT(type, Load_Leaf(arwN), Load_Leaf(aiwN)),
			  if_then, if_else);

	if ( Cur_PU_Feedback ) {
	  Cur_PU_Feedback->Annot( IF, FB_EDGE_BRANCH_TAKEN, FB_FREQ_UNKNOWN );
	  Cur_PU_Feedback->Annot( IF, FB_EDGE_BRANCH_NOT_TAKEN,
				  FB_FREQ_UNKNOWN );
	}
    
	WN_INSERT_BlockLast(block, lower_if(block, IF, actions));
    
	*realpart = WN_LdidPreg(type, realpartN);
	*imagpart = WN_LdidPreg(type, imagpartN);
      }
      WN_Delete(tree);
    }
    break;

  case OPR_RND:
  case OPR_TRUNC:
  case OPR_MOD:
  case OPR_REM:
  case OPR_ABS:
    Fail_FmtAssertion("unexpected complex op (%s)",
		      OPCODE_name(WN_opcode(tree)));
    /*NOTREACHED*/

  case OPR_INTRINSIC_OP:
    {
      INTRINSIC     id = (INTRINSIC) WN_intrinsic(tree);

      switch(id)
      {
      case INTRN_C4CONJG:
      case INTRN_C8CONJG:
      case INTRN_CQCONJG:
	{
	  WN	*iz;

	  lower_complex_expr(block, WN_actual(tree, 0), actions,
			     realpart, &iz);

	  *imagpart = WN_Neg(type, iz);

	  WN_Delete(tree);
	}
        break;

	//*****************************************************************
	//
	// N.B. Any complex intrinsic which does not have an emulation must
	// appear in the list below.
	//

	//       case INTRN_F4CIS:
	//case INTRN_F8CIS:
	//case INTRN_FQCIS:
	//  actions |= LOWER_INTRINSIC;
	  /* Fall Through */

      default:
	if (INTRN_is_actual(WN_intrinsic(tree)))
	{
	  tree = lower_intrinsic(block, tree, actions);
	}
	else
	{
	  lower_complex_emulation(block, tree, actions, realpart, imagpart);
	}
        break;
      }
    }
    break;

  case OPR_SELECT:
    {
       WN *r1, *i1;
       WN *r2, *i2;
       LEAF cond;
       
       cond = Make_Leaf(block, WN_kid0(tree), WN_rtype(WN_kid0(tree)));
       
       lower_complex_expr(block, WN_kid1(tree), actions, &r1, &i1);
       lower_complex_expr(block, WN_kid2(tree), actions, &r2, &i2);
       
       *realpart = WN_Select(type, Load_Leaf(cond), r1, r2);
       *imagpart = WN_Select(type, Load_Leaf(cond), i1, i2);
    }
    break;

  case OPR_PARM:
    lower_complex_expr(block, WN_kid0(tree), actions, realpart, imagpart);
    break;
  }
}




/* ====================================================================
 *
 * void lower_quad_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				 WN **hipart, WN **lopart)
 *
 * ==================================================================== */

static void lower_quad_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
				     WN **hipart, WN **lopart)
{
  TYPE_ID	type =	MTYPE_F8;


  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(actions & LOWER_QUAD, ("actions does not contain LOWER_QUAD"));
  Is_True((MTYPE_is_quad(WN_rtype(tree))),
	  ("expected quad-type node, not %s", OPCODE_name(WN_opcode(tree))));
 /*
  *  first lower the quad as expressions.
  *
  *  This will create and serialize a call chain
  *  that should always return a leaf that can be decomposed
  */
  tree = lower_expr(block, tree, actions);

  switch (WN_operator(tree))
  {
  case OPR_LDID:
    {
     /*
      *  we must create a new ST of type real*8
      */
      TY_IDX    beTY   = MTYPE_To_TY(type);
      WN_OFFSET offset = WN_load_offset(tree);

      *hipart = WN_Ldid(type, offset,
			coerceST(WN_st(tree), type), beTY);

      *lopart = WN_Ldid(type,
			coerceOFFSET(tree, type, offset),
			coerceST(WN_st(tree), type),
			beTY);

      lower_quad_maps(tree, *hipart, *lopart, actions);
    }
    break;

  case OPR_ILOAD:
    {
      WN	*addr;
      WN_OFFSET offset = WN_load_offset(tree);
      LEAF	addrN;

      addr = lower_expr(block, WN_kid0(tree), actions);

      addrN = Make_Leaf(block, addr, Pointer_type);

      *hipart = WN_Iload(type,
			 offset,
			 coerceTY(WN_ty(tree), type),
			 Load_Leaf(addrN));

      *lopart = WN_Iload(type,
			 coerceOFFSET(tree, type, offset),
			 coerceTY(WN_ty(tree), type),
			 Load_Leaf(addrN));

      lower_quad_maps(tree, *hipart, *lopart, actions);
    }
    break;

  case OPR_CONST:
    {
      TCON	val = Const_Val(tree);

      *hipart = Make_Const( Extract_Quad_Hi(val));
      *lopart = Make_Const( Extract_Quad_Lo(val));

    }
    break;

  case OPR_PAREN:
    {
     /*
      *	preserve the parens
      */
      lower_quad_expr(block, WN_kid0(tree), actions, hipart, lopart);

      *hipart = WN_Paren(type, *hipart);
      *lopart = WN_Paren(type, *lopart);
    }
    break;

  case OPR_PARM:
    lower_quad_expr(block, WN_kid0(tree), actions, hipart, lopart);
    break;

  case OPR_ILOADX:
    Is_True( FALSE, ("unexpected QUAD ILOADX"));
    break;

  default:
    Is_True((FALSE),
	    ("lower_quad_expr didn't %s", OPCODE_name(WN_opcode(tree))));
  }
}




static WN *WN_Coerce(TYPE_ID dst, WN *expr)
{
  TYPE_ID src= WN_rtype(expr);

  if (MTYPE_size_min(dst) == MTYPE_size_min(src))
    return expr;

  return WN_Cvt(src, dst, expr);
}

/* Given a expression specifying the dimension of a shared array, 
   find if it contains a THREADS node, and return that node.
   If it can't be found, return null */
static WN* find_Threads(WN* exp) {
  
  if (WN_operator(exp) == OPR_LDID) {
    if (strcmp(ST_name(WN_st(exp)), "THREADS") == 0) {
      return exp;
    }
  } else if (WN_operator(exp) == OPR_MPY) {
    WN* kid = find_Threads(WN_kid0(exp));
    if (kid != NULL) {
      return kid;
    } 
    kid = find_Threads(WN_kid1(exp));
    return kid;
  }
  return NULL;
}



/* ====================================================================
 *
 * WN *lower_linearize_array_addr(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on ARRAY expr <tree>,
 * returning linearized address expression tree.  <actions> must
 * contain LOWER_ARRAY.
 *
 *  We are assuming that the index and array_dim information are unique
 *
 * Note: If the element size is < 0, it means that we have a non-contiguous F90
 *       array. These are linearized slightly differently. Instead of
 *       the extent children being extents, they are stride multiplier factors. 
 *       The sum of the products of the index and stride multiplier is scaled
 *       by -element_size to get the offset from the base.  
 *
 * Note for UPC:  This is normally not accessed  (the front end does not 
 *                generate ARRAY nodes but instead uses pointer arithmetic), 
 *                but may be called when LNO is enabled, and the loop optimizer 
 *                converts pointer arithmetic nodes back into ARRAYs.
 * ==================================================================== */

static WN *lower_linearize_array_addr(WN *block, WN *tree,
				      LOWER_ACTIONS actions)
{
  TYPE_ID rtype = WN_rtype(tree);
  WN		*result, *product;
  INT32		n, i;
  BOOL          is_non_contig=FALSE;
  WN_ESIZE      element_size;
  WN  *elm_size;
  TY_IDX arr_ty, el_ty;

   
  Is_True((WN_operator_is(tree,OPR_ARRAY)),
	  ("expected ARRAY node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(Action(LOWER_ARRAY), ("actions doesn't contain LOWER_ARRAY"));
  Is_True(WN_num_dim(tree) > 0, ("WN_num_dim of ARRAY node not > 0"));

  n = WN_num_dim(tree);
  element_size = WN_element_size(tree);
  //arr_ty = Get_Type_From_ArrayOp(WN_array_base(tree));
  arr_ty = WN_Get_Ref_TY(tree);

  //Print_TY(stderr, arr_ty);
  if (Compile_Upc && !Type_Is_Shared_Ptr(arr_ty, true) &&
      !(TY_kind(arr_ty) == KIND_ARRAY && TY_is_shared(arr_ty))) {
    return tree; //no reason to lower private arrays
  }

  if (element_size < 0) {
     is_non_contig = TRUE;
     element_size = -element_size;
  }

  if (is_non_contig)
  {
     WN *stride_mult;
     
     result = WN_Coerce(rtype, WN_array_index(tree, n-1));
     stride_mult = WN_Coerce(rtype, WN_array_dim(tree, n-1));
     result = WN_Mpy(rtype,result,stride_mult);

     for (i = n-2; i >= 0; i--) {
	product = WN_Coerce(rtype, WN_array_index(tree, i));
	stride_mult = WN_Coerce(rtype, WN_array_dim(tree, i));
	product = WN_Mpy(rtype,product,stride_mult);
	result = WN_Add(rtype,result,product);
     }
  }
  else
  {
   /*
    *  result <- index[n-1]
    */
    if (Compile_Upc) {
      /* This avoids a cast to U8 in 64-bit environements */
      /* FIXME:  Need to figure out C semantics when index calculation overflows */
      result = WN_array_index(tree, n-1);
    } else {
      result = WN_Coerce(rtype, WN_array_index(tree, n-1));
    }
     
   /*
    *  result <- result + index[i] * ( dim[n-1] * dim[n-2] ... dim[i+1] )
    */
    for (i = n-2; i >= 0; i--)
    {
      INT32	m;
      WN	*mpy;
	     
      product = WN_Coerce(rtype, lower_copy_tree(WN_array_dim(tree, n-1),
						 actions));
      for (m=n-2; m>i; m--)
      {
	product = WN_Mpy(rtype,
			 product,
			 WN_Coerce(rtype, lower_copy_tree(WN_array_dim(tree, m),
							  actions)));
      }

      mpy = WN_Mpy(rtype,
		   WN_Coerce(rtype, WN_array_index(tree,i)),
		   product);
      result = WN_Add(rtype, result, mpy);
    }
  }  
  

  TY_IDX elt_type = 0;
  if (WN_operator(WN_array_base(tree)) == OPR_LDA) {
    //Since the indexes are linearized, we want the type to be a pointer to base type
    //instead of pointer to the array
    for (elt_type = TY_pointed(WN_ty(WN_array_base(tree))); TY_kind(elt_type) == KIND_ARRAY; elt_type = TY_etype(elt_type));
    elt_type = Make_Pointer_Type(elt_type);
  }  

  /*
   *  result <- base + result * elm_size
   */
  {
   
    elm_size = WN_Intconst(rtype, element_size);
    
    SPTR_ACCUMULATION_STATE saved_acc_state = sptr_accumulation_state;
    SPTR_OFFSET_TERM_STACK *term_stack;
    if (sptr_accumulation_state !=  ARRAY_ACCUMULATION) {
      sptr_accumulation_state = ARRAY_ACCUMULATION;
      sptr_off_accumulation_stack.push(CXX_NEW(SPTR_OFFSET_TERM_STACK,Malloc_Mem_Pool));
    }
    
    term_stack = sptr_off_accumulation_stack.top();
    WN *base = lower_expr(block, WN_array_base(tree), actions);
    
    if (saved_acc_state == ARRAY_ACCUMULATION) {
      sptr_accumulation_state = NO_ACCUMULATION;
      result = lower_expr(block, result, actions);
      term_stack->push(result);
      WN_Delete (tree);
      sptr_accumulation_state = saved_acc_state;
      return base;
    } else {
      sptr_accumulation_state = NO_ACCUMULATION;
      result = lower_expr(block, result, actions);
      /* The front end (or LNO's can.cxx) always geneates 1d arrays */
      WN* dim_size = WN_array_dim(tree, n-1);
      WN* threads;
      if (!term_stack->empty() && (threads = find_Threads(dim_size)) != NULL) {
	/* Dimension has a THREADS multiplier, adjust the previous index */
	WN* prev = term_stack->top();
	term_stack->pop();
	prev = WN_Mpy(WN_rtype(prev), prev, threads);
	term_stack->push(prev);
      }
      
      term_stack->push(result);
      result  = Combine_Offset_Terms(*term_stack);
      
      sptr_accumulation_state = saved_acc_state;
      sptr_off_accumulation_stack.pop();
      CXX_DELETE(term_stack, Malloc_Mem_Pool);
      
      /* We cannot allow this lowering to happen before wopt
	 (this creates COMMA nodes).  So just return a normal add
	 and let the later UPC_TO_INTR phase handle it */
      result = WN_Add(rtype, base, result);
      TY_IDX tas_ty = elt_type != 0 ? elt_type : WN_Get_Ref_TY(base);
      
      //wei: WN_Tas optimizes away the TAS if the rtype does not change,
      //so we need to explicitly create a TAS.
      WN* tmp = WN_Create(OPR_TAS, TY_mtype(tas_ty), MTYPE_V, 1);
      WN_kid0(tmp) = result;
      result = tmp;
      WN_set_ty(result, tas_ty);
    }
  }
  
  return result;
}




/* ====================================================================
 *
 * WN *lower_unsigned_to_float(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on conversions <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_unsigned_to_float(WN *block, WN *expr, TYPE_ID src,
				   TYPE_ID dst, LOWER_ACTIONS actions)
{
  LEAF	srcNo, dstNo;


  /* Special case of U4. Don't do this special case if the cvt.d.l 
   * instruction is slow. This is true on the R5000 which emulates
   * this in software due to a chip bug.
   */
  if (src == MTYPE_U4 && !Slow_CVTDL)
  {
     BOOL  simp=	WN_Simplifier_Enable(FALSE);
     WN * r;

     r = WN_Cvt(MTYPE_I8,dst,WN_Cvt(MTYPE_U4, MTYPE_U8, expr));

     WN_Simplifier_Enable(simp);
     return (r);
  }


  /*
   *  store the expr into a preg to avoid building a dag
   *	src = expr;
   */
  srcNo = Make_Leaf(block, expr, src);

  /*
   *  dst = (signed cvt) src ;
   */
  {
    WN	*ldid, *cvt;

    ldid = Load_Leaf(srcNo);

    cvt  = WN_Cvt(MTYPE_complement(src), dst, ldid);

    dstNo = Make_Leaf(block, cvt, dst);
  }

  /*
   *  build the select tree that looks like
   *	(src < 0) ? dst + 2**N : dst
   *	 where N is the src size
   */
  {
    WN	*rel, *add, *select;

    rel = WN_LT(MTYPE_complement(src),
		Load_Leaf(srcNo),
		WN_Zerocon(MTYPE_complement(src)));

    add = WN_Add(dst,
		 Load_Leaf(dstNo),
		 WN_ConstPowerOf2( dst, MTYPE_size_reg(src)));

    select = WN_Select(dst, rel, add, Load_Leaf(dstNo));
    return select;
  }
}




/* ====================================================================
 *
 * WN *lower_float_to_unsigned(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on conversions <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_float_to_unsigned(WN *block, WN *expr, TYPE_ID src,
				   TYPE_ID dst, LOWER_ACTIONS actions)
{
  LEAF srcNo;
  WN *trunc1,*r;
  WN *rel, *sub, *trunc2,*add;


  /* Two cases, dest type = U8, and dest type = U4 */
  if (dst == MTYPE_U4) {
     r = WN_Cvt(MTYPE_I8,dst,WN_Trunc(src,MTYPE_I8,expr));
  } else if (src==MTYPE_FQ) {  /* Need to do this this way because there
				* is no quad floor */
     
     /*
      *  store the expr into a preg to avoid building a dag
      *	src = expr;
      */
     srcNo = Make_Leaf(block, expr, src);
     
     /*
      *  build the select tree that looks like
      *
      * (2**(N-1) <= src) ? : 2**(N-1) + TRUNC(src-2**(N-1)) : TRUNC(src)
      *
      *	where N is the unsigned dst size
      */


     trunc1 = WN_Trunc(src, MTYPE_complement(dst), Load_Leaf(srcNo));
     
     sub = WN_Sub(src, Load_Leaf(srcNo),
		  WN_ConstPowerOf2(src, MTYPE_size_reg(dst)-1));

     trunc2 =  WN_Trunc(src, MTYPE_complement(dst),sub);

     add = WN_Add(dst,trunc2,WN_ConstPowerOf2(dst, MTYPE_size_reg(dst)-1));
     
     rel = WN_LE(src,
		 WN_ConstPowerOf2(src, MTYPE_size_reg(dst)-1),
		 Load_Leaf(srcNo));
     
     r = WN_Cselect(dst,rel,add,trunc1);
  } else {
     
     /*
      *  store the expr into a preg to avoid building a dag
      *	src = expr;
      */
     srcNo = Make_Leaf(block, expr, src);
     
     /*
      *  build the select tree that looks like
      *
      * (2**(N-1) <= src) ? : FLOOR(src-2**(N)) : TRUNC(src)
      *
      *	where N is the unsigned dst size
      */


     trunc1 = WN_Trunc(src, MTYPE_complement(dst), Load_Leaf(srcNo));
     
     sub = WN_Sub(src, Load_Leaf(srcNo),
		  WN_ConstPowerOf2(src, MTYPE_size_reg(dst)));

     trunc2 =  WN_Floor(src, MTYPE_complement(dst),sub);

     rel = WN_LE(src,
		 WN_ConstPowerOf2(src, MTYPE_size_reg(dst)-1),
		 Load_Leaf(srcNo));
     
     r = WN_Cselect(dst,rel,trunc2,trunc1);
  }
  return (r);
}




/* ====================================================================
 *
 * WN *lower_cvt(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on conversions <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_cvt(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN	*expr, *cvt;
  TYPE_ID dst = WN_rtype(tree);
  TYPE_ID src = WN_desc(tree);

  expr = lower_expr(block, WN_kid0(tree), actions);
  WN_kid0(tree) = expr;

  if (   Targ_Lower_Unsigned_To_Float 
      && MTYPE_is_unsigned(src) && MTYPE_is_float(dst))
  {
    WN_Delete(tree);

    cvt = lower_unsigned_to_float(block, expr, src, dst, actions);

    return lower_expr(block, cvt, actions);
  }
  else if (   Targ_Lower_Float_To_Unsigned 
	   && MTYPE_is_float(src) && MTYPE_is_unsigned(dst))
  {
    WN_Delete(tree);

    cvt = lower_float_to_unsigned(block, expr, src, dst, actions);

    return lower_expr(block, cvt, actions);
  }
  else if (   OPERATOR_is_compare(WN_operator(expr))
	   && WN_rtype(expr) == MTYPE_B
	   && (MTYPE_is_integral(dst) && src == MTYPE_B))
  {

    /* Optimize converts of MTYPE_B compares to integral values.
     */
    WN_Delete(tree);

    WN_set_rtype(expr, dst);

    return lower_expr(block, expr, actions);
  }
  
  return tree;
}



/* ====================================================================
 *
 * WN *lower_nary_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 *  Create nary expression reassociate to produce MADDS
 *
 * ==================================================================== */

static WN *lower_nary_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
{

  if (Enable_NaryExpr && Roundoff_Level >= ROUNDOFF_ASSOC)
  {
    TYPE_ID  type = WN_rtype(tree);

    tree = WN_ExprToNaryType(tree, type, 0);

    if (WN_nary_add(tree))
    {
      INT32	i;
      BOOL	found= TRUE;

     /*
      *  Find mpy then a non mpy under nary tree.
      *  The tree may change (ex. to a binary tree)
      */
      while(found)
      {
	INT32	mpyI, exprI, nmpyI, narympyI;
	WN	*mpy, *nmpy, *expr, *narympy;
        INT32	num_kids = WN_kid_count(tree);

	found=	FALSE;
	mpy  =  nmpy  = expr = narympy  = NULL;
	mpyI =  exprI = nmpyI = narympyI = 0;

	for(i = 0; i < num_kids; i++)
	{
	  WN  *actual= WN_actual(tree, i);

	  if (WN_operator_is(actual, OPR_MPY))
	  {
	    mpyI = i;
	    mpy = actual;
	  }
	  else if (WN_operator_is(actual, OPR_NEG)	&&
		   WN_operator_is(WN_kid0(actual), OPR_MPY))
	  {
	    nmpyI = i;
	    nmpy = WN_kid0(actual);
	  }
	  else if (WN_nary_mpy(actual) && WN_kid_count(actual) >= 2)
	  {
	    narympyI = i;
	    narympy = actual;
	  }
	  else 
	  {
	    exprI = i;
	    expr = actual;
	  }

	  if (mpy && expr)
	  {
	    WN_actual(tree, mpyI)= WN_Madd(type, expr, WN_kid0(mpy),
					   WN_kid1(mpy));
	    tree = WN_NaryDelete(tree, exprI);
	    found=	TRUE;
	    break;
	  }
	  else if (nmpy && expr)
	  {
	    WN_actual(tree, nmpyI)= WN_Nmsub(type, expr, WN_kid0(nmpy),
					     WN_kid1(nmpy));
	    tree = WN_NaryDelete(tree, exprI);
	    found=	TRUE;
	    break;
	  }
	  else if (narympy && expr)
	  {
	    mpy=	WN_kid0(narympy);
	    narympy=	WN_NaryDelete(narympy, 0);

	    WN_actual(tree, narympyI)= WN_Madd(type, expr, mpy, narympy);
	    tree = WN_NaryDelete(tree, exprI);
	    found=	TRUE;
	    break;
	  }
	}
      }
     /*
      *  There may be madd opportunites at the children
      */
      for(i = 0; i < WN_kid_count(tree); i++)
      {
	WN_actual(tree, i)= lower_madd(block, WN_actual(tree,i), actions);
      }

    }
    
    if (WN_nary_intrinsic(tree))
    {
      tree = WN_NaryToExpr(tree);
    }
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on expression <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID  type = WN_rtype(tree);

 /*
  *  look for madd patterns
  */
  switch (WN_operator(tree))
  {
  case OPR_NEG:
    {
      WN	*child = WN_kid0(tree);

      switch(WN_operator(child))
      {
      case OPR_MADD:
	WN_Delete(tree);
	return WN_Nmadd(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));

      case OPR_MSUB:
	WN_Delete(tree);
	return WN_Nmsub(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));

      case OPR_NMADD:
	WN_Delete(tree);
	return WN_Madd(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));

      case OPR_NMSUB:
	WN_Delete(tree);
	return WN_Msub(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));
      }
    }
    break;

  case OPR_ADD:
    {
      WN	*l= WN_kid0(tree);
      WN	*r= WN_kid1(tree);

      if (WN_operator_is(l, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Madd(type, r, WN_kid0(l), WN_kid1(l));
      }
      else if (WN_operator_is(r, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Madd(type, l, WN_kid0(r), WN_kid1(r));
      }
    }
    break;

  case OPR_SUB:
    {
      WN	*l= WN_kid0(tree);
      WN	*r= WN_kid1(tree);

      if (WN_operator_is(l, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Msub(type, r, WN_kid0(l), WN_kid1(l));
      }
      else if (WN_operator_is(r, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Nmsub(type, l, WN_kid0(r), WN_kid1(r));
      }
    }
    break;

  case OPR_MADD:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Msub(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;

  case OPR_MSUB:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Madd(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;

  case OPR_NMADD:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Nmsub(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;

  case OPR_NMSUB:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Nmadd(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_tree_height(WN *block, WN *wn, LOWER_ACTIONS actions)
 *
 *  Reassociate binary commutative operator at this level 
 *  (ie no recursion)
 *
 * ==================================================================== */

static WN *lower_tree_height(WN *block, WN *wn, LOWER_ACTIONS actions)
{
  OPCODE  opcode = WN_opcode(wn);
  WN	  *l, *r;

  if (NotAction(LOWER_TREEHEIGHT))
    return wn;

  Is_True(WN_is_commutative(wn),("expected commutative op"));

  l = WN_kid0(wn);
  r = WN_kid1(wn);

 /*
  *  do not transform an already balanced tree
  */
  if (opcode == WN_opcode(l) ^ opcode == WN_opcode(r))

  {
    WN *wn1 =  (opcode == WN_opcode(l)) ? l : r;
    WN *wn1X = (opcode == WN_opcode(l)) ? r : l;

    WN *wn1_l = WN_kid0(wn1);
    WN *wn1_r = WN_kid1(wn1);

    if (opcode == WN_opcode(wn1_l) ^ opcode == WN_opcode(wn1_r))
    {

      WN *wn2 =  (opcode == WN_opcode(wn1_l)) ? wn1_l : wn1_r;
      WN *wn2X = (opcode == WN_opcode(wn1_l)) ? wn1_r : wn1_l;
     /*
      *  rearrange tree
      */
      WN_kid0(wn1) =	wn2X;
      WN_kid1(wn1) =	wn1X;

      WN_kid0(wn) =	wn2;
      WN_kid1(wn) =	wn1;

      if (traceTreeHeight)
      {
	DevWarn("lower_tree_height: trace (%s) has been reassociated (line %d)",
	        OPCODE_name(WN_opcode(wn)), Srcpos_To_Line(current_srcpos));
      }
    }
  }
  return wn;
}




/* ====================================================================
 *
 * WN *lower_recip(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * WN *lower_rsqrt(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * lower recip/rsqrt based on ISA flags
 *
 * Quad is lowered unconditionally, as there is no runtime support
 * (kludge alert: turn off simplifer when creating quad divide !!)
 *
 * ==================================================================== */

static WN *lower_recip(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID  type = WN_rtype(tree);
  WN	  *kid0 = WN_kid0(tree);
  WN	  *div =  NULL;

  Is_True(WN_operator_is(tree, OPR_RECIP), ("expected recip"));
  Is_True(MTYPE_float(type), ("expected float type"));


  if (Recip_Allowed == FALSE)
  {
    div = WN_Div(type, WN_Floatconst(type, 1.0), kid0);
  }
  else if (Action(LOWER_QUAD) && MTYPE_is_quad(type))
  {
    BOOL  simp=	WN_Simplifier_Enable(FALSE);

    div = WN_Div(type, WN_Floatconst(type, 1.0), kid0);

    WN_Simplifier_Enable(simp);
  }

  if (div)
  {
    WN_Delete(tree);
    return div;
  }

  return tree;
}

static WN *lower_rsqrt(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID  type = WN_rtype(tree);

  Is_True(WN_operator_is(tree, OPR_RSQRT), ("expected rsqrt"));
  Is_True(MTYPE_float(WN_rtype(tree)), ("expected float type"));

  if (Rsqrt_Allowed == FALSE || MTYPE_is_quad(type))
  {
    WN	*div;

    div = WN_Div(type, WN_Floatconst(type, 1.0), WN_Sqrt(type, WN_kid0(tree)));
    WN_Delete(tree);

    return div;
  }
  return tree;
}


/* IPA can mark variables gp-relative, even if they are not allocated
 * in this source file. To deal with them, we explicitly check if a 
 * variable is gp-relative.
 */
static BOOL Symbol_Is_Base_Register (ST *sym)
{
  return ((ST_class(sym) == CLASS_BLOCK && STB_is_basereg(sym)) ||
          (ST_class(sym) == CLASS_VAR && ST_gprel(sym)));
}



/* ====================================================================
 *
 * WN *lower_split_sym_addrs(WN *tree, INT64 offset, LOWER_ACTIONS actions)
 *
 * Split symbols into base/offset depending on type/class etc.
 *
 *
 * ==================================================================== */

static WN *lower_split_sym_addrs(WN *tree, INT64 offset, LOWER_ACTIONS actions)
{
  ST *sym  = WN_st(tree);
  ST *base = ST_base(sym);
  INT64	newoffset = 0;

  if (traceSplitSymOff)
    return NULL;

  switch(ST_class(sym))
  {
  case CLASS_CONST:
   /*
    *  for non_shared, non-gprelative constants we want to expose the
    *  base for RVI
    */
    if (PIC_NONSHARED && (ST_size(sym) > Max_Sdata_Elt_Size))
    {
      Allocate_Object(sym);
      Base_Symbol_And_Offset_For_Addressing(sym, offset, &base, &newoffset);
      return lower_base_reference(tree, base, newoffset, actions);
    }
    return NULL;

  case CLASS_PREG:
    return NULL;

  case CLASS_FUNC:
    if (PIC_SHARED && ST_is_preemptible(sym))
    {
      return NULL;
    }
    break;

  case CLASS_BLOCK:
  case CLASS_VAR:
    Base_Symbol_And_Offset_For_Addressing(sym, offset, &base, &newoffset);

    if (ST_is_uplevelTemp(sym))
    {
      return NULL;
    }
    if (ST_assigned_to_dedicated_preg(sym)) 
    {
	return NULL;
    }

   /*
    *  We cannot keep lowering LDA's or we will forever recurse
    */
    if (WN_operator_is(tree, OPR_LDA) && base == sym) 
      return NULL;

    switch(ST_sclass(sym))
    {
    case SCLASS_REG:
    case SCLASS_TEXT:
      return NULL;

    case SCLASS_FORMAL_REF:	
     /*
      *	 If we expand the base, we will lose the FORMAL_REF'ness of the ST
      *  and will not be able to dereference it later
      */
      if (base != sym)
      {
 	base =	sym;
      }
      break;

    case SCLASS_FORMAL:
     /*
      *  Do not allocate Formal_Arg_StkSeg, as it is not correct yet !!
      *  (it will eventually be .sp + <offset> )
      */
      return NULL;

    case SCLASS_AUTO:
      /* 
       * For now, never split a stack variable.
       * We only see stack variables with bases when we do regions,
       * and for now we want to keep those cases looking like the
       * regular cases for correctness.  But at a later date we should
       * try to split these in the region case, as that may allow a
       * base register to be allocated for things like FP+32000,
       * so we could then get 1-instruction accesses to large offsets.
       * But that requires a cgexp change too, so wait on it.
       */
#if 0
      if (Uses_Small_Offset(sym, offset))
        break;
#endif
      return NULL;

    case SCLASS_EXTERN:
     /*
      *	okay to split these. A dlopen() can never redefine these (unless
      * they are weak)
      */
      if (ST_is_weak_symbol(sym))
	return NULL;
      break;

    case SCLASS_COMMON:
     /*
      *	commons may be preempted by a dlopen() (if the new definition is
      * initialized) see wilson/lillian for details of this rather obscure
      * point suneel says not to worry about this though, so please direct
      * bugs to him
      *
      * In the new symbol table, we need to also split common symbols
      * here. The reason is that now both members and the common itself
      * are SCLASS_COMMON, whereas before they used to be SCLASS_BASED.
      * We no longer have to worry about the ST_Full that was present in
      * the old symbol table
      */
      sym = base;
      break;

    case SCLASS_PSTATIC:
    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL:
    case SCLASS_FSTATIC:
     /*
      *  okay to split these. We will never split incorrectly as
      *	 Base_Symbol_And_Offset_For_Addressing() will not split a preemptible
      *  symbol
      */
      break;

    default:
      return NULL;
    }

    if (ST_gprel(sym))
    {
      return NULL;
    }
    tree = lower_base_reference(tree, base, newoffset, actions);
    return tree;

  default:
    return NULL;
  }

  return NULL;
}




/* ====================================================================
 *
 * WN *lower_uplevel_reference(WN *tree, INT64 offset, LOWER_ACTIONS actions)
 *
 * Split uplevel symbols into indirect of slink
 *
 * ==================================================================== */

static WN *lower_uplevel_reference(WN *tree, INT64 offset,
				   LOWER_ACTIONS actions)
{
  ST	*sym = WN_st(tree);
  ST	*base;
  INT64	newoffset;

  Is_True(ST_is_uplevelTemp(sym), ("expected uplevel %s",ST_name(sym)));

  Base_Symbol_And_Offset_For_Addressing(sym, offset, &base, &newoffset);
  base = Find_Slink_For_ST(sym);

  tree = lower_dereference(tree, newoffset, base, 0, actions);
  return tree;
}




/* ====================================================================
 *
 * WN *lower_formal_ref(WN *tree, INT64 offset, ST *base,
 *                      LOWER_ACTIONS actions)
 *
 * lower an SCLASS_FORMAL_REF into a dereference of the new base.
 * or
 * lower an SCLASS_FORMAL into a preg, already computed
 *
 * ==================================================================== */
static WN *lower_formal_ref(WN *tree, INT64 offset, ST *base,
			    LOWER_ACTIONS actions)
{
  PREG_NUM	preg;

  switch(ST_sclass(base))
  {
  case SCLASS_FORMAL_REF:
    base = Get_ST_formal_ref_base(base);
    if (preg = Get_ST_formal_preg_num(base))
    {
      Is_True((ST_has_nested_ref(WN_st(tree))==FALSE),
	      ("lower_formal_ref: cannot use preg for nested ref %s",
	       ST_name(WN_st(tree))));
      base = MTYPE_To_PREG(TY_mtype(Ty_Table[ST_type(base)]));
    }
    return lower_dereference(tree, offset, base, preg, actions);

  case SCLASS_FORMAL:
    if (preg= Get_ST_formal_preg_num(base))
    {
      base = MTYPE_To_PREG(TY_mtype(Ty_Table[ST_type(base)]));
      tree = lower_base_register(tree, base, preg, actions);
      return tree;
    }
    break;
  }
  return NULL;
}




/* ====================================================================
 *
 * WN *lower_base_register(WN *tree, ST *base, INT64 offset,
 *                         LOWER_ACTIONS actions)
 *
 * common routine for lowering to a base register for LDA, LDID, STID
 * into using a new base/offset
 *
 * ==================================================================== */

static WN *lower_base_register(WN *tree, ST *base, INT64 offset,
			       LOWER_ACTIONS actions)
{
  Is_True(WN_st(tree) != base, ("lower_base_register(): possible recursion"));

  switch (WN_operator(tree))
  {
  case OPR_LDA:
   /*
    * Convert
    *     LDA (offset) <sym> into
    *
    *     LDA (offset) <base>
    */
    WN_st_idx(tree) = ST_st_idx(base);
    WN_lda_offset(tree) = mem_offset_hi(offset);
    return tree;

  case OPR_LDID:
   /*
    * Convert
    *	  LDID (offset> <sym>  into
    * 	  LDID (offset <base>
    */
    WN_st_idx(tree) = ST_st_idx(base);
    WN_load_offset(tree) = offset;
    break;

  case OPR_STID:
   /*
    * Convert
    *	  STID (offset> <sym>  into
    * 	  STID (offset <base>
    */
    WN_st_idx(tree) = ST_st_idx(base);
    WN_store_offset(tree) = offset;
    break;

  default:
    Is_True(FALSE,("expected lda/ldid/stid"));
    return NULL;
  }

  return tree;
}


// replace the type of LDID/STID nodes with non-zero field_id to the type
// of the field
static void
lower_field_id (WN* tree)
{
  OPERATOR opr = WN_operator(tree);

  Is_True (opr == OPR_LDID || opr == OPR_STID || opr == OPR_MLOAD ||
	   opr == OPR_MSTORE, ("expecting LDID or STID nodes"));

  if (WN_field_id (tree) == 0)
    return;

  BOOL is_ptr_type = (opr == OPR_MLOAD || opr == OPR_MSTORE);

  TY_IDX ty_idx = is_ptr_type ? TY_pointed (WN_ty (tree)) : WN_ty (tree);

  Is_True (TY_kind (ty_idx) == KIND_STRUCT,
	   ("non-zero field id must have KIND_STRUCT"));

  UINT field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (ty_idx, WN_field_id (tree), field_id); 
  
  Is_True (! fld.Is_Null (), ("invalid bit-field ID for %s",
			      OPERATOR_name(opr)));

  WN_set_ty (tree, (is_ptr_type ? Make_Pointer_Type (FLD_type (fld)) :
		    FLD_type (fld)));
  WN_set_field_id (tree, 0);
  return;
} // lower_field_id


/* ====================================================================
 *
 * WN *lower_base_reference(WN *tree, ST *base, INT64 offset,
 *                          LOWER_ACTIONS actions)
 *
 * common routine for lowering to a base reference for LDA, LDID, STID
 *
 * ==================================================================== */

static WN *lower_base_reference(WN *tree, ST *base, INT64 offset,
				LOWER_ACTIONS actions)
{
  WN    *addr, *wn;

  switch (WN_operator(tree))
  {
  case OPR_LDA:
   /*
    * Convert
    *     LDA (offset) <sym>
    * 
    * (lo > 0)
    *       LDA (hi) <base>
    *       CONST (lo)
    *     ADD
    *
    * (offset >= 2GB)
    *       LDA (0) <base>
    *       CONST (offset)
    *     ADD
    */

    WN_st_idx(tree) = ST_st_idx(base);

    if (mem_offset_2GB(offset))
    {
      addr =	WN_Add(Pointer_type, tree, WN_Intconst(Pointer_type, offset));
      WN_lda_offset(tree)=  0;
      return addr;
    }
    else if (Action(LOWER_SPLIT_CONST_OFFSETS)	&&
	     mem_offset_lo(offset))
    {
     /* turn off simplifier else ADD may be removed */
      BOOL  simp=        WN_Simplifier_Enable(FALSE);

      addr =	WN_Add(Pointer_type,
		       tree,
		       WN_Intconst(Pointer_type, mem_offset_lo(offset)));
      WN_lda_offset(tree)=	mem_offset_hi(offset);

      WN_Simplifier_Enable(simp);
      return addr;
    }
    WN_lda_offset(tree)=  offset;
    return tree;

  case OPR_LDID:
  case OPR_STID:
  case OPR_LDBITS:
  case OPR_STBITS:
   /*
    * Process the common address
    *
    * if (offset > 2GB)
    *      LDA (hi) <sym>
    *	   CONST offset
    *    ADD
    *
    * if (offset > 16K)
    *      LDA (hi) <sym>
    *    ILOAD | ISTORE (lo)
    */
    addr =	WN_Lda(Pointer_type, 0, base);

    if (mem_offset_2GB(offset))
    {
      addr =	WN_Add(Pointer_type, addr, WN_Intconst(Pointer_type, offset));
      offset =	0;
    }
    if (Action(LOWER_SPLIT_CONST_OFFSETS) && mem_offset_must_be_split(offset))
    {
      WN_lda_offset(addr) =	mem_offset_hi(offset);
      offset =			mem_offset_lo(offset);
    }
    break;

  default:
    Is_True(FALSE,("expected lda/ldid/stid"));
    return NULL;
  }	

  switch (WN_operator(tree))
  {
  case OPR_LDID:
   /*
    * Convert (LDID () <sym>) into
    *      LDA (0) <base>
    *    ILOAD (offset+ofst)
    */
    if (WN_field_id (tree) != 0)
      lower_field_id (tree);
    
    wn =	WN_RIload(WN_rtype(tree), WN_desc(tree), offset, WN_ty(tree),
			  addr);
    break;
  case OPR_LDBITS:
    wn =	WN_RIload(WN_rtype(tree), WN_desc(tree), offset, WN_ty(tree),
			  addr);
    WN_set_operator(wn, OPR_ILDBITS);
    WN_set_bit_offset_size(wn, WN_bit_offset(tree), WN_bit_size(tree));
    break;

  case OPR_STID:
   /*
    * Convert   STID (offset) <sym>
    *  into
    *       LDA (0) <base>
    *     ISTORE (offs+ofst)
    */
    if (WN_field_id (tree) != 0)
      lower_field_id (tree);
    
    wn =	WN_Istore(WN_desc(tree),
			  offset,
			  // The resulting TY is not an f90 pointer
			  // type because we're converting an STID.
			  // In the case where this STID
			  // came about because of conversion of
			  // ISTORE(something) in the past, and that
			  // ISTORE was through an f90 pointer, the
			  // destination of the STID will have the
			  // F90_TARGET attribute, and we're OK.
			  Make_Pointer_Type(WN_ty(tree)),
			  addr,
			  WN_kid0(tree));
    WN_Set_Linenum(wn, WN_Get_Linenum(tree));
    break;
  case OPR_STBITS:
    wn =	WN_Istore(WN_desc(tree),
			  offset,
			  Make_Pointer_Type(WN_ty(tree)),
			  addr,
			  WN_kid0(tree));
    WN_set_operator(wn, OPR_ISTBITS);
    WN_set_bit_offset_size(wn, WN_bit_offset(tree), WN_bit_size(tree));
    WN_Set_Linenum(wn, WN_Get_Linenum(tree));
    break;
  }	

  lower_copy_maps(tree, wn, actions);
  WN_Delete(tree);

  return wn;
}



/* ====================================================================
 *
 * WN *lower_dereference(WN *tree, INT64 offset, ST *base,
 *                       PREG_NUM *preg, LOWER_ACTIONS actions)
 *
 * Perform address dereferencing for SCLASS_FORMAL_REF and uplevel variable
 * Caller is responsible for any further lowering.
 *
 *  For uplevel references the idea is to replace the st with a reference
 *  of the slink_sym at level ST_level(st) + 1
 *  This will only work recursively
 *  (ie. that sym must be referenced via the next level's scope
 *
 * ==================================================================== */
static WN
*lower_dereference(WN *tree, INT64 offset, ST *base, PREG_NUM preg,
		   LOWER_ACTIONS actions)
{
  WN	 *addr, *deref;
  TY_IDX  addr_ty;

  switch (WN_operator(tree))
  {
  case OPR_LDA:
   /*
    * Convert uplevel reference
    *     LDA (offset) <sym>
    *  into 
    *       LDID (0) <base>
    *       CONST (offset)    
    *     ADD
    */
    deref =	WN_Ldid(Pointer_type, preg, base, WN_ty(tree));
    addr =	WN_Add(Pointer_type, deref, WN_Intconst(Pointer_type, offset));
    return addr;

  case OPR_LDID:
  case OPR_STID:
   /*
    * Process the common address
    *
    */
    addr_ty = Make_Pointer_Type(WN_ty(tree));
    addr =	WN_Ldid(Pointer_type, preg, base, addr_ty);

    if (mem_offset_2GB(offset))
    {
      addr =	WN_Add(Pointer_type, addr, WN_Intconst(Pointer_type, offset));
      offset =	0;
    }
    break;

  default:
    Is_True(FALSE,("expected lda/ldid/stid"));
    return NULL;
  }	

  switch (WN_operator(tree))
  {
  case OPR_LDID:
   /*
    * Convert uplevel reference  LDID () <sym>
    * into
    *      LDID (preg) <base>
    *    ILOAD (offset)
    */
    deref = WN_RIload(WN_rtype(tree), WN_desc(tree), offset, WN_ty(tree), addr);
    break;

  case OPR_STID:
   /*
    * Convert  STID () <sym>
    * into
    *       LDID (preg) <base> 
    *     ISTORE (offset)          
    */
    deref = WN_Istore(WN_desc(tree), offset, addr_ty, addr, WN_kid0(tree));
    WN_Set_Linenum(deref, WN_Get_Linenum(tree));
    break;
  }	

  lower_copy_maps(tree, deref, actions);
  WN_Delete(tree);

  return deref;
}

/* ====================================================================
 *
 * WN *lower_return_ldid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on 'LDID Return_Val_Preg'
 * nodes returning a lowered 'LDID normal_PREG' node.
 *
 * ==================================================================== */

static WN *lower_return_ldid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX   ty_idx  = WN_ty(tree);
  TY&      ty      = Ty_Table[ty_idx];
  TYPE_ID  mtype   = TY_mtype (ty);

  Is_True((WN_operator(tree) == OPR_LDID),
	  ("expected LDID node, not %s", OPCODE_name(WN_opcode(tree))));   

  switch (mtype) {

    case MTYPE_I8:
    case MTYPE_U8:
      WN_st_idx(tree) = ST_st_idx(MTYPE_To_PREG(mtype));
      WN_load_offset(tree) = First_Int_Preg_Return_Offset;
      return tree;

    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      WN_st_idx(tree) = ST_st_idx(Int_Preg);
      WN_load_offset(tree) = First_Int_Preg_Return_Offset;
      return tree;

    case MTYPE_F4:
    case MTYPE_F8:
    case MTYPE_FQ:
    case MTYPE_C4:
    case MTYPE_C8:
    case MTYPE_CQ:
      WN_st_idx(tree) = ST_st_idx(Float_Preg);
      WN_load_offset(tree) = First_Float_Preg_Return_Offset;
      return tree;

    case MTYPE_M:
      Fail_FmtAssertion ("MLDID of Return_Val_Preg not allowed in middle"
			 " of expression");
      /*NOTREACHED*/
    default:
      Fail_FmtAssertion ("Unexpected type in lower_return_ldid");
      /*NOTREACHED*/
  }
}


static TY_IDX
get_field_type (TY_IDX struct_type, UINT field_id)
{
  Is_True (TY_kind (struct_type) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (struct_type, field_id, cur_field_id);
  Is_True (! fld.Is_Null(), ("Invalid field id %d for type 0x%x",
			     field_id, struct_type));
  return FLD_type (fld);
}


/* ====================================================================
 *
 * WN *lower_mldid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MLDID nodes returning
 * an equivalent MLOAD node.  Note that MLDID's of Return_Val_Preg are
 * not lowered here.  They are handled by lower_return_ldid.
 *
 * ==================================================================== */

static WN *lower_mldid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX ty_idx  = WN_ty(tree);
  TY_IDX fld_idx = WN_field_id(tree) == 0 ? 0 : 
    get_field_type (ty_idx, WN_field_id (tree));
  TY_IDX pty_idx;

  UINT64 size    = fld_idx == 0 ? Adjusted_Type_Size(ty_idx) : TY_size(fld_idx);
  
  if(Action(LOWER_UPC_MFIELD) && WN_field_id(tree) && Type_Is_Shared_Ptr(fld_idx))
    size = TY_size(TY_To_Sptr_Idx(fld_idx));

  WN*    wn;
  WN*    awn;
  WN*    swn;

  FmtAssert((WN_opcode(tree) == OPC_MMLDID),
	    ("expected mldid node, not %s", OPCODE_name(WN_opcode(tree))));
  
  pty_idx = Make_Pointer_Type (ty_idx, FALSE);

  swn = WN_CreateIntconst(OPC_U4INTCONST, size);

  // Put the offset in the mload node, instead of the LDA (for bug411)
  awn = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0, pty_idx, WN_st(tree));
  wn = WN_CreateMload(WN_load_offset(tree), pty_idx, awn, swn);
  WN_set_field_id(wn, WN_field_id(tree));
  wn  = lower_expr(block, wn, actions);

  WN_Delete(tree);
  return wn;
}

/* ====================================================================
 *
 * WN *lower_miload(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MILOAD nodes returning
 * an equivalent MLOAD node.
 *
 * ==================================================================== */

static WN *lower_miload(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  //fix iload
  TY_IDX ty_idx = WN_ty(tree);
  TY_IDX pty_idx = WN_load_addr_ty(tree);
  UINT64 size    = TY_size(Ty_Table[ty_idx]);

  // Similar to lower_mstid, lowering is skipped if the current tree is created as part of
  // the split-phase optimization.
  // Refer to the comment in the lower_mstid for more description of this lowering skip. 
  if (upc_comm_map != NULL && WN_MAP_Get(upc_comm_map, tree) != NULL) {
      return tree;
  }

  if(Action(LOWER_UPC_MFIELD)) {
     if (WN_field_id(tree) && Type_Is_Shared_Ptr(ty_idx)) {
       //pty_idx = Make_Pointer_Type(WN_ty(tree));
       size = TY_size(TY_To_Sptr_Idx(ty_idx));
     } else {
       //type-based get may introduce new miload,
       //and their size needs to be adjusted (bug1813)
       size = TY_adjusted_size(ty_idx);
       //fprintf(stderr, "!!!!!!!!!!!!!!size is %d\n", size);
     }
  }
  WN*    wn;
  WN*    swn;

  Is_True((WN_opcode(tree) == OPC_MMILOAD),
	  ("expected miload node, not %s", OPCODE_name(WN_opcode(tree))));

  swn = WN_CreateIntconst (OPC_U4INTCONST, size);
  //fprintf(stderr, "IN LOWER MILOAD: %d \n", WN_field_id(tree));
  //Print_TY(stderr, TY_pointed(pty_idx));
  //fdump_tree(stderr, tree);
  wn  = WN_CreateMload (WN_offset(tree), pty_idx, WN_kid0(tree), swn);
  WN_set_field_id(wn, WN_field_id(tree));
  if(Action(LOWER_UPC_MFIELD))
    //offsets already adjusted, just return
    return wn;
  wn  = lower_expr (block, wn, actions);

  WN_Delete (tree);
  return wn;
}

/* ====================================================================
 *
 * lower_load_bits
 *
 * lower LDBITS and ILDBITS into shifts
 *
 * ==================================================================== */
static WN*
lower_load_bits (WN* block, WN* wn, LOWER_ACTIONS actions)
{
  Is_True (WN_operator (wn) == OPR_LDBITS || WN_operator (wn) == OPR_ILDBITS,
	   ("expected LDBITS or ILDBITS, not %s",
	    OPERATOR_name(WN_operator(wn))));

  TYPE_ID rtype = WN_rtype (wn);
  TYPE_ID desc = WN_desc (wn);
  INT delta = MTYPE_bit_size(rtype) - MTYPE_bit_size(desc);
  if (delta < 0) {
    rtype = Mtype_TransferSize (desc, rtype);
    delta = 0;
  }

  WN* tree = wn;

  INT bit_size = WN_bit_size (wn);
  INT bit_ofst = Target_Byte_Sex == BIG_ENDIAN ?
    WN_bit_offset (wn) :
    MTYPE_bit_size(desc) - bit_size - WN_bit_offset (wn);
  BOOL bits_signed = MTYPE_signed(rtype);
  
  if (bit_ofst == 0)
    bit_size += delta;
  else {
    bit_ofst += delta;
    if (bits_signed)
      tree = WN_Shl (rtype, tree, WN_Intconst (MTYPE_I4, bit_ofst));
    else {
      INT shift_count = 64 - (MTYPE_bit_size(rtype) - bit_ofst);
      mUINT64 mask = (~(mUINT64)0) >> shift_count;
      tree = WN_Band (rtype, tree,
		      WN_Intconst (Mtype_TransferSign (MTYPE_U4, rtype),
				   mask));
      bit_size += bit_ofst;
    }
  }

  INT right_shift = MTYPE_bit_size(rtype) - bit_size;

  if (right_shift > 0) {
    OPERATOR opr = bits_signed ? OPR_ASHR : OPR_LSHR;
    tree = WN_Binary (opr, rtype, tree, WN_Intconst (MTYPE_I4, right_shift));
  }

  TYPE_ID orig_rtype = WN_rtype (wn);
  
  WN_set_rtype (wn, rtype);
  WN_set_bit_offset_size (wn, 0, 0);
  WN_set_operator (wn, WN_operator (wn) == OPR_LDBITS ? OPR_LDID : OPR_ILOAD);

  if (rtype != orig_rtype)
    tree = WN_Type_Conversion (tree, orig_rtype);

  return lower_expr (block, tree, actions);
} // lower_load_bits


/* ====================================================================
 *
 * lower_store_bits
 *
 * lower STBITS and ISTBITS
 *
 * ==================================================================== */
static WN*
lower_store_bits (WN* block, WN* wn, LOWER_ACTIONS actions)
{
  Is_True (WN_operator (wn) == OPR_STBITS || WN_operator (wn) == OPR_ISTBITS,
	   ("expected STBITS or ISTBITS, not %s",
	    OPERATOR_name (WN_operator (wn))));

  INT bit_size = WN_bit_size (wn);
  INT bit_ofst = WN_bit_offset (wn); 

  WN* orig_value;

  if (WN_operator (wn) == OPR_ISTBITS) {
    WN* load_address = lower_copy_tree (WN_kid1 (wn), actions);
    orig_value = WN_Iload (Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
			   WN_offset (wn), WN_ty (wn), load_address, 0);
  } else 
    orig_value = WN_Ldid (Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
			  WN_offset (wn), WN_st_idx (wn), WN_ty (wn), 0);
  
  TYPE_ID cmp_type = WN_rtype (orig_value);
  
  INT shift = Target_Byte_Sex == BIG_ENDIAN ?
      MTYPE_bit_size (WN_desc (wn)) - bit_ofst - bit_size :
      bit_ofst;
  mUINT64 mask = ~((~((mUINT64)0) >> (64 - bit_size)) << shift);
  orig_value = WN_Band (cmp_type, orig_value, WN_Intconst (cmp_type, mask));

  WN* new_value = WN_kid0 (wn);

  // check if we need to sign-extend the value
  if (bit_size > MTYPE_bit_size (WN_rtype (new_value)))
    new_value =
      WN_CreateCvtl (OPR_CVTL,
		     Mtype_TransferSign (WN_rtype (new_value), cmp_type),
		     WN_rtype (new_value),
		     MTYPE_bit_size (WN_rtype (new_value)),
		     new_value);
  
  // now, truncate to the right bit size for store
  mask = ~((mUINT64)0) >> (64 - bit_size);
  new_value =
    WN_Band (cmp_type, new_value,
	     WN_Intconst (Mtype_TransferSize (WN_rtype (new_value), cmp_type),
			  mask));

  // move the bits to the right position
  if (shift > 0)
    new_value = WN_Shl (cmp_type, new_value, WN_Intconst (cmp_type, shift));

  // set the value with bitwise or
  new_value = WN_Bior (cmp_type, orig_value, new_value);

  WN_kid0 (wn) = new_value;
  WN_set_bit_offset_size (wn, 0, 0);
  WN_set_operator (wn, WN_operator(wn) == OPR_STBITS ? OPR_STID : OPR_ISTORE);

  return lower_store (block, wn, actions);
} // lower_store_bits

#if 0
/* ====================================================================
 *
 * check_unaligned
 *
 * required_alignment is the natural alignment; offset is the actual offset
 * used in the current access; ty_align is the alignment in the TY of the
 * current access.  Return whether the access is unaligned.
 *
 * ==================================================================== */
static bool check_unaligned(INT required_alignment, INT offset, INT ty_align)
{
  if (required_alignment <= 1)
    return FALSE;
  INT align = ty_align;
  if (offset) {
    INT offset_align = offset % required_alignment;
    if (offset_align)
      align = MIN(align, offset_align);
  }
  return align < required_alignment;
}
#endif


// --------------------------------------------------------------------
// This function mimics FLD_get_to_field from common/com/symtab.cxx,
// but it also computes the offset of <field_id> within <struct_ty_idx>
// We need this because FLD_ofst only gives the offset within the first
// enclosing struct.
// --------------------------------------------------------------------
FLD_HANDLE
FLD_And_Offset_From_Field_Id (TY_IDX  struct_ty_idx, 
                              UINT    field_id, 
                              UINT&   cur_field_id,
                              UINT64& offset)
{
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_field_id++;
    if (cur_field_id == field_id) {
      offset += FLD_ofst(fld);
      return fld;
    }
    if (TY_kind(FLD_type(fld)) == KIND_STRUCT &&
        TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
      UINT64 nested_offset = offset + FLD_ofst(fld);
      fld = FLD_And_Offset_From_Field_Id(FLD_type(fld), field_id, 
                                         cur_field_id, nested_offset);
      if (cur_field_id == field_id) {
        offset = nested_offset;
        return fld;
      }
    }
  } while (!FLD_last_field(fld_iter++));
  
  return FLD_HANDLE();
} 

/* ====================================================================
 *
 * lower_bit_field_id
 *
 * The given LDID/STID/ILOAD/ISTORE node has desc type MTYPE_BS.  Lower
 * the node by changing the field_id to bit_offset/bit_size combination.
 * The desc field is changed to reflect the unit in memory to load, and
 * the offset field may need to be updated.
 *
 * ==================================================================== */
static void lower_bit_field_id(WN *wn)
{
  TY_IDX struct_ty_idx;
  TY_IDX ty_idx;
  TYPE_ID rtype;
  OPERATOR opr = WN_operator(wn);
  OPERATOR new_opr;
  BOOL indirect;
  if (opr == OPR_LDID || opr == OPR_STID) {
    ST_IDX st_idx = WN_st_idx(wn);
    struct_ty_idx = WN_ty(wn);
    new_opr = (opr == OPR_LDID) ? OPR_LDBITS : OPR_STBITS;
    indirect = FALSE;
  }
  else {
    if (WN_operator(wn) == OPR_ILOAD) {
      ty_idx = WN_load_addr_ty(wn);
      new_opr = OPR_ILDBITS;
    }
    else {	// ISTORE
      ty_idx = WN_ty(wn);
      new_opr = OPR_ISTBITS;
    }
    Is_True(TY_kind(ty_idx) == KIND_POINTER,
	    ("addr ty not pointer type for %s", OPERATOR_name(opr)));
    struct_ty_idx = TY_pointed(ty_idx);
    indirect = TRUE;
  }
  Is_True(TY_kind(struct_ty_idx) == KIND_STRUCT,
	  ("struct type not associated with bit-field access for %s", OPERATOR_name(opr)));
  UINT cur_field_id = 0;
  UINT64 field_offset = 0;
  FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(struct_ty_idx, 
                                                WN_field_id(wn),
                                                cur_field_id,
                                                field_offset); 
  Is_True(! fld.Is_Null(),
	  ("invalid bit-field ID for %s", OPERATOR_name(opr)));
  TY_IDX fld_ty_idx = FLD_type(fld);
  WN_set_ty (wn, (opr == OPR_ISTORE ?
		  Make_Pointer_Type (fld_ty_idx, FALSE) :
		  fld_ty_idx));
  
  Is_True(FLD_is_bit_field(fld),
	  ("non-bit-field associated with bit-field access for  %s", OPERATOR_name(opr)));

  // analyze bit field accessed
  UINT bytes_accessed = TY_size(fld_ty_idx);
  if (OPERATOR_is_store(new_opr))
    rtype = TY_mtype(fld_ty_idx);
  else rtype = WN_rtype(wn);
  INT ofst = field_offset;
#if 0
  BOOL unaligned_field = check_unaligned(bytes_accessed * 8, ofst,
					 TY_align(struct_ty_idx));
#endif
  if (ofst >= 0)
    ofst = ofst / bytes_accessed * bytes_accessed;
  else ofst =  (ofst - bytes_accessed + 1) / bytes_accessed * bytes_accessed;
  UINT bsize = FLD_bsize(fld);
  UINT bofst = FLD_bofst(fld) + (field_offset-ofst) * 8;
  if ((bofst + bsize) > (bytes_accessed * 8)) {
    if (bytes_accessed == MTYPE_byte_size(Max_Int_Mtype)) { 
      // can't enlarge; reverse the adjustment
      ofst = field_offset;
      bofst = FLD_bofst(fld);
    }
    else bytes_accessed *= 2;
  }
  WN_load_offset(wn) = WN_load_offset(wn) + ofst; 

  if ((bsize & 7) == 0 && 		   // field size multiple of bytes
      (bytes_accessed * 8 % bsize) == 0 && // bytes_accessed multiple of bsize
      (bofst % bsize) == 0) {		   // bofst multiple of bsize
    // bit-field operation not needed; leave operator as previous one
    WN_set_field_id(wn, 0);
    WN_set_desc(wn, Mtype_AlignmentClass(bsize >> 3, MTYPE_type_class(rtype)));
    WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);
  }
  else { // generate lowered-to bit-field operator
#if defined(TARG_MIPS) || defined(TARG_IA32)
    if ((indirect || WN_class(wn) != CLASS_PREG) && 
	bofst % 8 == 0 &&		// bit offset on byte boundary
	compute_offset_alignment(bytes_accessed*8, bofst) >= bsize) {
      // adjust bytes accessed to get rid of the left-shift
      WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);
      bytes_accessed = compute_offset_alignment(bytes_accessed, bofst >> 3);
      bofst = 0;
    }
#endif
    WN_set_operator(wn, new_opr);
    WN_set_desc(wn, Mtype_AlignmentClass(bytes_accessed, MTYPE_type_class(rtype)));
    if (OPERATOR_is_load(new_opr) && 
	MTYPE_byte_size(WN_rtype(wn)) < bytes_accessed)
      WN_set_rtype(wn, WN_desc(wn));
    WN_set_bit_offset_size(wn, bofst, bsize);
  }
}

static void lower_trapuv_alloca (WN *block, WN *tree, LOWER_ACTIONS actions)
{
	WN *size = WN_kid0(tree);
	WN *con = WN_UVConst(WN_rtype(size));
	WN *mstore;
	if (WN_operator(size) == OPR_INTCONST && WN_const_val(size) == 0)
		return;	// nothing to do

	mstore = WN_CreateMstore(0,
		Make_Pointer_Type(MTYPE_To_TY(WN_rtype(size)),TRUE),
		con,
		WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
		WN_COPY_Tree(size) );
  	mstore = lower_store (block, mstore, actions);
	WN_INSERT_BlockLast(block, mstore);
}


inline BOOL Should_Call_Divide(TYPE_ID rtype)
{
#if defined(TARG_IA64)
  if (!OPT_Inline_Divide) {
    if (   rtype == MTYPE_I8 || rtype == MTYPE_U8
	|| rtype == MTYPE_I4 || rtype == MTYPE_U4
	|| rtype == MTYPE_F4 || rtype == MTYPE_F8) return TRUE;
  }
#elif defined(EMULATE_LONGLONG)
  if (rtype == MTYPE_I8 || rtype == MTYPE_U8) return TRUE;
#endif
  return FALSE;
}

static BOOL Is_Fast_Divide(WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
  case OPR_DIV:
  case OPR_REM:
  case OPR_MOD:
    {
      if (WN_operator_is(WN_kid1(wn), OPR_INTCONST)) {
	TYPE_ID rtype = OPCODE_rtype(WN_opcode(wn));
	INT64 constval = WN_const_val(WN_kid1(wn));

	return   opr == OPR_DIV
	       ? Can_Do_Fast_Divide(rtype, constval)
	       : Can_Do_Fast_Remainder(rtype, constval);
      }
    }
    break;
  }

  return FALSE;
}

static WN *lower_parm(WN *block, WN *tree, LOWER_ACTIONS actions){
  

  Is_True(Action(LOWER_UPC_TO_INTR) && WN_operator(tree) == OPR_PARM,
	  ("Incorrect call to lower parm",""));
  
  WN *kid = WN_kid0(tree);
  TY_IDX actual_ty;
  TY_IDX formal_ty = WN_ty(tree);
  TY_IDX access_ty;
  INT field_id;
  WN_OFFSET offset;
  WN *result;
  INTRINSIC iop;

   
  SPTR_ACCUMULATION_STATE saved_acc_state = sptr_accumulation_state;
  sptr_accumulation_state = NO_ACCUMULATION;
  
  field_id = WN_field_id(kid);
  offset = WN_offset(kid);
  
  switch (WN_operator(kid)) {
  case OPR_LDID: {
    BOOL cvt = FALSE;
    INTRINSIC iop;
    if(field_id) {
      actual_ty = WN_object_ty(kid);
      access_ty = WN_ty(kid);
    } else
      actual_ty = ST_type(WN_st(kid));
    
    if(Type_Is_Shared_Ptr(formal_ty)) {
      //formal is shared ptr	
      kid = lower_expr(block, kid, actions);
      if(Need_StoP_Cvt(actual_ty, formal_ty, &iop)) {
	kid = WN_Create_StoP_Cvt(kid, iop);	
	actual_ty = formal_ty;
      }
      if(TY_kind(formal_ty) == KIND_SCALAR && TY_kind(actual_ty) == KIND_SCALAR)
	result =  WN_CreateParm(TY_mtype(actual_ty), kid, actual_ty, WN_PARM_BY_VALUE);
      else if (TY_kind(formal_ty) == KIND_POINTER) {
	//problem here is that for shared pointer-to-shared TY_To_Sptr_Idx uses the 
	//block size of the pointer.
	//(This is the behavior that front end/whirl2c wants)
	//When dealing with parm nodes, however, we want to use the 
	//block size of the pointed type to determine whether it's pshared
	result =  WN_CreateParm(TY_mtype(TY_To_Sptr_Idx(TY_pointed(actual_ty))), kid,
				TY_To_Sptr_Idx(TY_pointed(actual_ty)), WN_PARM_BY_VALUE);
      } else
	result =  WN_CreateParm(TY_mtype(TY_To_Sptr_Idx(actual_ty)), kid,
				TY_To_Sptr_Idx(actual_ty), WN_PARM_BY_VALUE);

    } else {
      // formal is not shared 
      Is_True(Type_Is_Shared_Ptr(actual_ty) || Type_Is_Shared_Ptr(access_ty),
	      ("",""));
      if(WN_rtype(tree) != MTYPE_M) {
	kid = lower_expr(block, kid, actions);
	kid = Spill_Shared_Load(kid);
	result =   WN_CreateParm(TY_mtype(formal_ty), kid,
				 formal_ty, WN_PARM_BY_VALUE);
      } else {
	//load of struct
      }
      
    }
  }
    break;
  case OPR_ILOAD:  
    if (WN_field_id(kid) == 0) {
      actual_ty = WN_ty(kid);
      access_ty = WN_load_addr_ty(kid);
    } else {
      actual_ty = TY_pointed(WN_load_addr_ty(kid));
      access_ty = Make_Pointer_Type(WN_ty(kid));
    }
    
    if (TY_mtype(actual_ty) != MTYPE_M ||  
	(field_id && WN_object_ty(kid) != MTYPE_M))
      {
	INTRINSIC iop;
	kid = lower_expr(block, kid, actions); 
	if(WN_operator(WN_kid0(tree)) == OPR_TAS)
	  kid = Strip_TAS(kid);
	TY_IDX src = actual_ty;
	TY_IDX dest = formal_ty;
	
	if(Type_Is_Shared_Ptr(actual_ty, TRUE) && Type_Is_Shared_Ptr(formal_ty, TRUE) &&
	   Need_StoP_Cvt(actual_ty, formal_ty, &iop)) {
	  kid = WN_Create_StoP_Cvt(kid, iop);	
	  actual_ty = formal_ty;
	}
	
	if(TY_kind(formal_ty) != KIND_SCALAR && Type_Is_Shared_Ptr(formal_ty)) {
	  //for a function parameter, its type can never be shared
	  if (TY_kind(formal_ty) == KIND_POINTER) 
	    formal_ty = TY_To_Sptr_Idx(TY_pointed(formal_ty));
	  else 
	    formal_ty =  TY_To_Sptr_Idx(formal_ty);
	}
	result =   WN_CreateParm(TY_mtype(formal_ty), kid,
				 formal_ty, WN_PARM_BY_VALUE);
    } else {
      if (Type_Is_Shared_Ptr(access_ty)) {
	WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	if(WN_operator(WN_kid0(tree)) == OPR_TAS)
	  WN_kid0(tree) = WN_kid0(WN_kid0(tree));
	
	ST *spill_st = Gen_Temp_Symbol(actual_ty, (char*) ".Mspill");
	WN *dest = WN_Lda(Pointer_Mtype, 0, spill_st, 0);
	kid  = WN_Create_Shared_Load(kid, dest, 0, actual_ty);
	WN *blk = WN_CreateBlock();
	WN_INSERT_BlockLast(blk, kid);
	WN *ld = WN_Ldid(MTYPE_M, 0, spill_st, access_ty);
	kid = WN_CreateComma(OPR_COMMA, MTYPE_M, MTYPE_V, blk, ld);
	WN_kid0(tree) = kid;
	result =  tree;
      } else 
	Is_True(0,("",""));
    }
    break;
  case OPR_MLOAD:
    {
      //WEI: for some reason ty is not set here
      actual_ty = WN_ty(tree);
      access_ty = WN_ty(kid);
           
      WN_OFFSET offt = 0;//  = WN_offset(kid);
      
      if(TY_kind(access_ty) == KIND_POINTER)
	access_ty = TY_pointed(access_ty);
      else 
	Is_True(0,("",""));

      WN_offset(kid) = Adjust_Field_Offset(access_ty, WN_offset(kid));
      WN_kid0(kid) = lower_expr(block, WN_kid0(kid), actions);
      WN_kid1(kid) = lower_expr(block, WN_kid1(kid), actions);

      if(TY_is_shared(actual_ty) && (TY_kind(actual_ty) == KIND_STRUCT || TY_kind(actual_ty) == KIND_SCALAR)) {
	//This involves scanning the symbol table; a better fix might be 
	//to generate a private type in the (gnu) front end
	actual_ty = Shared_To_Private_Type(actual_ty);
      }

      ST *spill_st = Gen_Temp_Symbol(actual_ty, (char*) ".Mspill");
      WN *dest = WN_Lda(Pointer_Mtype, 0, spill_st, 0);
      kid  = WN_Create_Shared_Load(kid, dest, offt, actual_ty);
      WN *blk = WN_CreateBlock();
      WN_INSERT_BlockLast(blk, kid);
      WN *ld = WN_Ldid(MTYPE_M, 0, spill_st, actual_ty);
      kid = WN_CreateComma(OPR_COMMA, MTYPE_M, MTYPE_V, blk, ld);
      WN_kid0(tree) = kid;
      result = tree;
    }
    break;
    
  case OPR_TAS: 
    {
      BOOL cvt = TRUE;
      actual_ty = WN_ty(kid);
      kid = lower_expr(block, kid, actions);
      kid = Strip_TAS(kid);
      
      if (!Type_Is_Shared_Ptr(formal_ty, TRUE)) {
	if(Type_Is_Shared_Ptr(actual_ty, TRUE)) {
	  if (TY_is_pointer(Ty_Table[formal_ty]))
	    kid = WN_Convert_Shared_To_Local(kid);
	  else
	    kid = WN_Convert_Shared_To_Int(kid);
	}
      } else {
	if (cvt && Type_Is_Shared_Ptr(actual_ty, TRUE) && 
	    Need_StoP_Cvt(actual_ty, formal_ty, &iop))
	  {
	    kid = WN_Create_StoP_Cvt(kid, iop);	    
	  }
      }
      
      WN_kid0(tree) = kid;
      result = tree;
    }
    break;
  case OPR_LDA:
    {
      actual_ty = WN_ty(kid);
      
      if(Type_Is_Shared_Ptr(actual_ty) && Type_Is_Shared_Ptr(formal_ty) &&
	 Need_StoP_Cvt(actual_ty, formal_ty, &iop)) {
	kid = lower_expr(block, kid, actions);
	kid =  WN_Create_StoP_Cvt(kid, iop);	
      } else 
	kid = lower_expr(block, kid, actions);
      WN_kid0(tree) = kid;
      result = tree;
    }
    break;
  case OPR_INTCONST:
    if(Type_Is_Shared_Ptr(formal_ty, TRUE)) {
      WN_kid0(tree) = WN_CreateLdid(OPR_LDID, TY_mtype(formal_ty), TY_mtype(formal_ty), 0,
				    TY_is_pshared(TY_pointed(formal_ty)) ? pshared_null : shared_null, formal_ty,
				    0);
      result = tree;
    } else {
      WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
      result = tree;
    }
    break;
  default:
    WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
    result = tree;
  }
  
  sptr_accumulation_state = saved_acc_state;
  return result;
} 


static WN* lower_upc_ptr_test(WN* block, WN* tree, LOWER_ACTIONS actions) {

  TY_IDX idx0 = 0;
  TY_IDX idx1 = 0;
  OPERATOR op0 = WN_operator(WN_kid0(tree));
  OPERATOR op1 = WN_operator(WN_kid1(tree));
  if(OPERATOR_is_load(op0) || op0 == OPR_LDA || op0 == OPR_TAS) { 
    idx0 = WN_ty(WN_kid0(tree));
    if(WN_operator(WN_kid0(tree)) == OPR_LDID && WN_field_id(WN_kid0(tree))) {
      idx0 = WN_object_ty(WN_kid0(tree));
    } else if (WN_operator(WN_kid0(tree)) == OPR_MLOAD &&  WN_field_id(WN_kid0(tree))) {
      idx0 = WN_Get_Ref_TY(WN_kid0(tree));
    }
  }
  if(OPERATOR_is_load(op1) || op1 == OPR_LDA || op1 == OPR_TAS) { 
    idx1 = WN_ty(WN_kid1(tree));
    if(WN_operator(WN_kid1(tree)) == OPR_LDID && WN_field_id(WN_kid1(tree))) {
      idx0 = WN_object_ty(WN_kid1(tree));
    } else if (WN_operator(WN_kid1(tree)) == OPR_MLOAD &&  WN_field_id(WN_kid1(tree))) {
      idx1 = WN_Get_Ref_TY(WN_kid1(tree));
    }
  }
  WN *kid0 = lower_expr(block, WN_kid0(tree), actions);
  WN *kid1 = lower_expr(block, WN_kid1(tree), actions);
  BOOL is_sptr0 = Type_Is_Shared_Ptr(idx0, TRUE);
  BOOL is_sptr1 = Type_Is_Shared_Ptr(idx1, TRUE);
  
  if((is_sptr0 && is_sptr1) ||
     ((WN_operator(kid0) == OPR_INTCONST && is_sptr1) || 
      (WN_operator(kid1) == OPR_INTCONST && is_sptr0))) {
    tree = WN_Create_PtrEq_Test(WN_operator(tree), kid0, kid1, idx0, idx1);
  } //can't tell what was the original intention for this OR test
    // It might create problems, but it does catch bugs
    // where we have local == shared and there's no shared_to_local inserted
  else if(is_sptr1 &&  !is_sptr0) {
    WN_kid0(tree) = kid0;
    if (TY_is_pointer(Ty_Table[idx0]))
      WN_kid1(tree) = WN_Convert_Shared_To_Local(kid1);
    else
      WN_kid1(tree) = WN_Convert_Shared_To_Int(kid1);
  } else if (!is_sptr1 &&  is_sptr0) {
    WN_kid1(tree) = kid1;
    if (TY_is_pointer(Ty_Table[idx1]))
      WN_kid0(tree) = WN_Convert_Shared_To_Local(kid0);
    else
      WN_kid0(tree) = WN_Convert_Shared_To_Int(kid0);
  }else {
    WN_kid0(tree) = kid0;
    WN_kid1(tree) = kid1;
  }  
  return tree;
}

static SPTR_OFFSET_TERM_STACK sptr_term_stack;

static WN* enter_terms(WN* tree) {
  
  WN* result = NULL;
  //if ((WN_operator(tree) == OPR_LDA || WN_operator(tree) == OPR_LDID || WN_operator(tree) == OPR_TAS) &&
  if (Type_Is_Shared_Ptr(WN_ty(tree), true)) {
    //this is the base we want
    return tree;
  } else if (WN_operator(tree) != OPR_ADD) {
    sptr_term_stack.push(tree);
    return NULL;
  } else {
    //recursively examine both operands
    result = enter_terms(WN_kid0(tree));
    if (result) {
      enter_terms(WN_kid1(tree));
    } else {
      result = enter_terms(WN_kid1(tree));
    }
    return result;
  }
    
}

//Optimizing UPC shared array accesses,
//by rearranging the terms so that all of the offset terms 
//appear as one subtree while the base address (either a LDID or LDA) appears as the other.
//(In effect we are undoing the simplifier's change to the tree)
//We do not perform any combining of the terms here, so it should not
//affect the offset adjustment that might be needed later
static WN* rearrange_upc_ptr_arith(WN* block, WN* tree, LOWER_ACTIONS actions) {

  FmtAssert(WN_operator(tree) == OPR_ADD, ("tree must be an shared add expression"));
  WN* kid0 = WN_kid0(tree);
  WN* kid1 = WN_kid1(tree);
  if (Type_Is_Shared_Ptr(WN_ty(kid0), true)) {
    //already in good form
    return tree; 
  }
  if (Type_Is_Shared_Ptr(WN_ty(kid1), true)) {
    return tree; 
  }
  
  WN* base = enter_terms(tree);
  
  if (base == NULL) {
    //should not happen 
    fprintf(TFile, "!!!rearranging upc pointer arithmetic expression failed!!!\n");
    fdump_tree(TFile, tree);
    return tree;
  }

  //build up a new add tree
  WN* index_terms = sptr_term_stack.top();
  sptr_term_stack.pop();
  while (!sptr_term_stack.empty()) {
    //index_terms = WN_Add(Pointer_Mtype, index_terms, sptr_term_stack.top());
    //Bypass the simplifier to avoid the terms being reassociated again 
    WN* tmp = WN_Create(OPR_ADD, Pointer_Mtype, MTYPE_V, 2);
    WN_kid0(tmp) = index_terms;
    WN_kid1(tmp) = sptr_term_stack.top();
    index_terms = tmp;
    sptr_term_stack.pop();
  }

  FmtAssert(sptr_term_stack.empty(), ("term stack should be empty at this point"));
  WN* new_exp = WN_Create(OPR_ADD, Pointer_Mtype, MTYPE_V, 2);
  WN_kid0(new_exp) = base;
  WN_kid1(new_exp) = index_terms;

  //fprintf(stderr, "before rearranging:\n");
  //fdump_tree(stderr, tree);
  //fprintf(stderr, "after rearranging:\n");
  //fdump_tree(stderr, new_exp);
  //fprintf(stderr, "\n");
  return new_exp;
}


/* ====================================================================
 *
 * WN *lower_expr(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on expression <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_expr(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  BOOL kids_lowered = FALSE;	/* becomes TRUE when kids are lowered */
  BOOL intrinsic_lowered;

  TYPE_ID type = WN_rtype(tree);

  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));

  if (OPCODE_is_load(WN_opcode(tree)))
    lower_map(tree, actions);

 /*
  *  before children are processed reassociate for madd oportunities
  */
  if (Action(LOWER_MADD)	&&
      Madd_Allowed		&&
     (MTYPE_id(type) == MTYPE_F4 || MTYPE_id(type) == MTYPE_F8))
  {
    tree = lower_nary_madd(block, tree, actions);
    tree = lower_madd(block, tree, actions);
  }
  if (Action(LOWER_TREEHEIGHT)	&&
      WN_is_commutative(tree))
  {
    if (MTYPE_is_integral(type) || Roundoff_Level >= ROUNDOFF_ASSOC)
      tree = lower_tree_height(block, tree, actions);
  }

 /* Note: We must split constant offsets after lowering complex exprs
  * and splitting symbol addresses since these may create new offsets
  * that need to be split.
  */
  switch (WN_operator(tree)) {

  case OPR_SUB:
    {
      if(Action(LOWER_UPC_TO_INTR)) {
	if(WN_Type_Is_Shared_Ptr(WN_kid0(tree), TRUE) &&  
	   WN_Type_Is_Shared_Ptr(WN_kid1(tree), TRUE)) {
	  // shared ptr difference
	  TY_IDX eltype = WN_ty(WN_kid0(tree));
	  TY_IDX eltype2 = WN_ty(WN_kid1(tree));
	  return WN_Create_Shared_Ptr_Diff(lower_expr(block, WN_kid0(tree), actions),
					   lower_expr(block, WN_kid1(tree), actions),
					   eltype,
					   eltype2);
	} else if (WN_Type_Is_Shared_Ptr(WN_kid0(tree), TRUE) &&
		   MTYPE_is_integral(WN_rtype(WN_kid1(tree)))) {
	  /* ptr - int */
	  TY_IDX eltype = WN_ty(WN_kid0(tree));
	  return WN_Create_Shared_Ptr_Arithmetic(lower_expr(block, WN_kid0(tree), actions),
						 lower_expr(block, WN_kid1(tree), actions),
						 OPR_SUB,
						 Get_Type_Inner_Size(eltype),
						 Get_Type_Block_Size(eltype));
	}
      }
    }
    break;
  case OPR_PARM:
    {
      if(Action(LOWER_UPC_TO_INTR)) {
	TY_IDX formal_idx = WN_ty(tree);
	if (Type_Is_Shared_Ptr(formal_idx) || 
	    WN_Type_Is_Shared_Ptr(WN_kid0(tree))) {
	  return  lower_parm(block, tree, actions);
	  //fdump_tree(stderr, WN_kid0(tree));
	} else if (WN_operator(WN_kid0(tree)) == OPR_ADD) {
	  //WEI: a special case for when exprs like foo->x[i] is passed to a function,
	  //and we need to adjust the field offset
	  WN* kid = WN_kid0(tree);
	  WN* offset = WN_kid1(kid);
	}
      }
    }
    break; /* PARM */
    
  case OPR_INTRINSIC_OP:
    
    if (INTRN_is_nary(WN_intrinsic(tree)))
      break;

    if (INTRN_cg_intrinsic(WN_intrinsic(tree)))
      break;

    if (INTRN_is_actual(WN_intrinsic(tree)))
    {
      if (Action(LOWER_INTRINSIC))
      {
	tree = lower_intrinsic(block, tree, actions);
        kids_lowered = TRUE;
      }
      break;
    }
    if (Action(LOWER_INTRINSIC) ||
	Action(LOWER_INLINE_INTRINSIC) ||
	Action(LOWER_INL_STACK_INTRINSIC))
    {
      tree = lower_emulation(block, tree, actions, intrinsic_lowered);
      kids_lowered = TRUE;
    }
    break;

  case OPR_ARRAY:
    if (Action(LOWER_ARRAY))
    {
      
      tree = lower_linearize_array_addr(block, tree, actions);
      kids_lowered = TRUE;
    
    }
    break;

  case OPR_ADD:
    if (Action(LOWER_SPLIT_CONST_OFFSETS))
    {
     /*
      * Split
      *       LDA   (c1) <sym>
      *       CONST (c2)
      *     ADD
      * Into
      *       LDA   (hi) <sym>
      *       CONST (low)
      *     ADD
      */
      WN *lda = WN_kid0(tree);
      WN *con = WN_kid1(tree);

      if (WN_operator_is(con, OPR_INTCONST) &&
	  foldLdaOffset(lda, WN_const_val(con)))
      {
        WN_OFFSET  offset = WN_lda_offset(lda) + WN_const_val(con);

	if (mem_offset_must_be_split(offset))
	{
	  WN_lda_offset(lda) = mem_offset_hi(offset);
	  WN_const_val(con) =  mem_offset_lo(offset);
	}
      }
    }
    else
    {
     /*
      * Fold
      *       LDA   (c1) <sym>
      *       CONST (c2)
      *     ADD
      * Into
      *     LDA (c1+c2) <sym>
      */
      WN *lda = WN_kid0(tree);
      WN *con = WN_kid1(tree);

      if (Action(LOWER_UPC_TO_INTR)) {
      
	TY_IDX idx_ptr, idx_off;
	OPERATOR op;
	idx_ptr = 0;
	idx_off = 0;
	
	UINT esize = 0;
	UINT bsize = 0;
	WN *ptr, *off;
	BOOL ptr_ptr = FALSE;

	op = WN_operator(lda);
	if(OPERATOR_is_load(op) || op == OPR_LDA || op == OPR_TAS)
	  idx_ptr = WN_ty(lda);
	op = WN_operator(con);
	if(OPERATOR_is_load(op) || op == OPR_LDA || op == OPR_TAS)
	  idx_off = WN_ty(con);

	if (WN_operator(lda) == OPR_INTCONST) {
	  idx_ptr = 0;
	}  else if(OPERATOR_is_load(WN_operator(lda)) && WN_field_id(lda)) {
	  //idx_ptr = Get_Field_Type(WN_ty(lda), WN_field_id(lda));
	  idx_ptr = WN_object_ty(lda);
	}
 
	if (WN_operator(con) == OPR_INTCONST) {
	  idx_off = 0;
	} else if ((WN_operator(con) == OPR_LDID || WN_operator(con) == OPR_MLOAD) && WN_field_id(con))  {
	  idx_off = WN_Get_Ref_TY(con); //Get_Field_Type(WN_ty(con), WN_field_id(con));
	} 

	/* if idx_ptr == idx_off == 0, i.e. we should have a regular
	   scalar add. However, the optimizer folds cts to right hand side
	   of the tree. While doing this it loses type information.
	   Ex: compile  a[j+100][i+100]
	   Try recapture that info here. The right way would be of course to fix
	   the optimizer (This happens after a call to SSA::Create_CODEMAP -> Value_number) */

	if (!idx_ptr && !idx_off) {
	  TY_IDX op_ty;
	  
	  op_ty = 0;
	  if (WN_operator(lda) == OPR_ADD) {
	    op = WN_operator(WN_kid0(lda));
	    if(OPERATOR_is_load(op) || op == OPR_LDA || op == OPR_TAS)
	      op_ty = WN_ty(WN_kid0(lda));
	    if(Type_Is_Shared_Ptr(op_ty) && TY_kind(op_ty) != KIND_SCALAR)
	      idx_ptr = op_ty;
	    op = WN_operator(WN_kid1(lda));
	    if(OPERATOR_is_load(op) || op == OPR_LDA || op == OPR_TAS)
	      op_ty = WN_ty(WN_kid1(lda));
	    if(Type_Is_Shared_Ptr(op_ty) && TY_kind(op_ty) != KIND_SCALAR)
	      idx_ptr = op_ty;
	  }
	  else if (WN_operator(con) == OPR_ADD) {
	    op = WN_operator(WN_kid0(con));
	    if(OPERATOR_is_load(op) || op == OPR_LDA || op == OPR_TAS)
	      op_ty = WN_ty(WN_kid0(con));
	    if(Type_Is_Shared_Ptr(op_ty) && TY_kind(op_ty) != KIND_SCALAR)
	      idx_off = op_ty;
	    op = WN_operator(WN_kid1(con));
	    if(OPERATOR_is_load(op) || op == OPR_LDA || op == OPR_TAS)
	      op_ty = WN_ty(WN_kid1(con));
	    
	    if(Type_Is_Shared_Ptr(op_ty) && TY_kind(op_ty) != KIND_SCALAR)
	      idx_off = op_ty;
	  } 
	}
	

	/* lower shared ptr arithmetic - check for the base case where 
	   one of the operands is shared */
	if (idx_ptr || idx_off  ) {
	  
	  ptr = lda;
	  off = con;

	  if ( TY_kind(idx_ptr) == KIND_POINTER && 
	       ( TY_is_shared(TY_pointed(idx_ptr)) || 
		 (TY_kind(TY_pointed(idx_ptr)) == KIND_ARRAY && TY_is_shared(TY_etype(TY_pointed(idx_ptr)))))) {
	    idx_off = TY_pointed(idx_ptr);
	    esize = Get_Type_Inner_Size(idx_off);
	    
	    bsize = Get_Type_Block_Size(idx_off);
	    
	    
	  } else if (TY_kind(idx_off) == KIND_POINTER && 
		     (TY_is_shared(TY_pointed(idx_off)) || 
		      (TY_kind(TY_pointed(idx_off)) == KIND_ARRAY && TY_is_shared(TY_etype(TY_pointed(idx_off)))))) {
	    ptr = con;
	    off = lda;
	    idx_ptr = idx_off;
	   
	    idx_off = TY_pointed(idx_off);
	    esize = Get_Type_Inner_Size(idx_off);	    
	    bsize = Get_Type_Block_Size(idx_off);
	    
	  } else if ( TY_kind(idx_ptr) == KIND_ARRAY && TY_is_shared(TY_etype(idx_ptr)) ) {
	    Is_True(0,("",""));
	    esize = TY_size(TY_etype(idx_ptr));
	    bsize = TY_block_size(TY_etype(idx_ptr));
	  } else if ( TY_kind(idx_off) == KIND_ARRAY && TY_is_shared(TY_etype(idx_off)) ) {
	    Is_True(0,("",""));
	    ptr = con;
	    off = lda;
	    esize = TY_size(TY_etype (idx_off));
	    bsize = TY_block_size(TY_etype(idx_off));
	  } else {
	    if(!idx_ptr) {
	      ptr = con;
	      off = lda;
	      idx_ptr = idx_off;
	    }
	    ptr_ptr = TRUE; 
	  }	  

	  
	  if(sptr_accumulation_state == ACCUMULATION) {
	    if(!ptr_ptr) {
	      if(WN_offset(ptr)) {
		if(OPERATOR_is_load(WN_operator(ptr)) && WN_field_id(ptr) && 
		   Type_Is_Shared_Ptr(WN_ty(ptr)))
		  ;
		else {
		  sptr_off_accumulation_stack.top()->push(WN_Intconst(Integer_type, 
								      WN_offset(ptr)));
		  WN_offset(ptr) = 0;
		}
	      }
	    }
	  }

	  //for pointer arithmetic on pointers to local structs, need to adjust the
	  //struct size in the offset code, otherwise whirl2c gets confused and
	  // we generate incorrect code
	  
	  if(idx_ptr && WN_Has_Valid_Type(ptr)) {
	    if((WN_operator(ptr) != OPR_LDID || !Type_Is_Shared_Ptr(idx_ptr)) &&
	       !(WN_operator(ptr) == OPR_LDA && Type_Is_Shared_Ptr(idx_ptr)) &&
	       //test for H.ptr[ct].field
	       !(WN_operator(ptr) == OPR_TAS && WN_Has_Valid_Type(WN_kid0(ptr)) &&
		WN_ty(ptr) == WN_Get_Ref_TY(WN_kid0(ptr))) 
	       )
	      //test for shared ptr + cnst which looks like s->field, except for
	      // the TAS
	      Adjust_Consts_For_Ptr_Arithmetic(ptr,off);
	  }
	  
	  ptr = lower_expr(block, ptr, actions);
	  off = lower_expr(block, off, actions);
	  
	  if(ptr_ptr) {
	    //talentless hack
	    // this is for multidimensional array indexing expressions
	    if (WN_operator(ptr) == OPR_COMMA && 
		(WN_ty(WN_kid1(ptr)) == shared_ptr_idx || WN_ty(WN_kid1(ptr)) == pshared_ptr_idx))
	      goto do_arith;
	    else if (WN_operator(off) == OPR_COMMA && 
		     (WN_ty(WN_kid1(off)) == shared_ptr_idx || WN_ty(WN_kid1(off)) == pshared_ptr_idx)){
	      lda = ptr;
	      ptr = off;
	      off = lda;
	      goto do_arith;
	    }   
	    
	    WN_kid0(tree) = ptr;
	    WN_kid1(tree) = off;
	    return tree;
	  }

do_arith:
	  
	  if (sptr_accumulation_state == NO_ACCUMULATION) {
	    // here idx_ptr should hold the pointer type
	    Is_True((TY_kind(idx_ptr) == KIND_POINTER || TY_kind(idx_ptr) == KIND_ARRAY)
		    && Type_Is_Shared_Ptr(idx_ptr), ("",""));
               
	    if(TY_kind(idx_ptr) == KIND_POINTER) 
	      idx_ptr = TY_pointed(idx_ptr);
	    else 
	      idx_ptr = TY_etype(idx_ptr);
	    if(TY_is_shared(idx_ptr))
	       ptr  = WN_Create_Shared_Ptr_Arithmetic (ptr, off, OPR_ADD,  esize, bsize);
	    else {
	      WN_kid0(tree) = ptr;
	      WN_kid1(tree) = off;
	      return tree;
	    }
	  } else {
	    //bug 2504 for shared struct T a[]; a[i].a[0][0] ACCUMULATION is set for the add(i,a)
	    // need to revert this here
	    if(TY_kind(idx_ptr) == KIND_POINTER && TY_kind(TY_pointed(idx_ptr)) == KIND_ARRAY) {
	      if (TY_is_shared(TY_pointed(idx_ptr))) {
		ptr =  WN_Create_Shared_Ptr_Arithmetic (ptr, off, OPR_ADD,  esize, bsize);
	      } else {
		sptr_off_accumulation_stack.top()->push(off);
		Fail_FmtAssertion("What do I do here?");
	      }
	    } else 
	      sptr_off_accumulation_stack.top()->push(off);
	  }
	  WN_Delete (tree);

	  tree = ptr;
	  kids_lowered = TRUE;
	  // fdump_tree(stderr, tree);
	}
      } 
      
no_shared:
      if (WN_operator_is(con, OPR_INTCONST) &&
	  foldLdaOffset(lda, WN_const_val(con)))
      {
	WN_lda_offset(lda) += WN_const_val(con);
	WN_Delete(tree);
	WN_Delete(con);
	tree = lower_expr(block, lda, actions);;
      }
    }
    break;

  case OPR_MLOAD:  
    if (Action(LOWER_UPC_TO_INTR)) {
      TY_IDX idx = WN_ty(tree);
      WN *ild;
      if (Type_Is_Shared_Ptr(idx)) {
	SPTR_ACCUMULATION_STATE saved_acc_state = sptr_accumulation_state;
	sptr_accumulation_state = ACCUMULATION;
	sptr_off_accumulation_stack.push(CXX_NEW(SPTR_OFFSET_TERM_STACK, Malloc_Mem_Pool));
	SPTR_OFFSET_TERM_STACK *term_stack = sptr_off_accumulation_stack.top();
	
	if(WN_offset(tree)) {
	  term_stack->push(WN_Intconst(Integer_type, WN_offset(tree)));
	  WN_offset(tree) = 0;			   
	}
		
	WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	WN_kid1(tree) = lower_expr(block, WN_kid1(tree), actions);

	if(WN_operator(WN_kid0(tree)) == OPR_TAS)
	  WN_kid0(tree) = Strip_TAS(WN_kid0(tree));
	
	ild = Combine_Offset_Terms(*term_stack);
	
	sptr_accumulation_state = saved_acc_state;
	sptr_off_accumulation_stack.pop();
	CXX_DELETE(term_stack, Malloc_Mem_Pool);
	if(ild)
	  return WN_Create_Shared_Load(tree, 0, 0, 0, TRUE, ild);
	else 
	   return WN_Create_Shared_Load(tree, 0, 0, 0, FALSE, 0);
      }
    } else
      if (Align_Object)
	{
	  WN_kid0(tree)=	lower_expr(block, WN_kid0(tree), actions);
	 //  fdump_tree(stderr, WN_kid0(tree));
	  kids_lowered = 	TRUE;
	  if(Action(LOWER_UPC_MFIELD) && 
	     WN_operator(WN_kid1(tree)) == OPR_INTCONST) {
	    TY_IDX idx = WN_Get_Ref_TY(tree);
	    WN_const_val(WN_kid1(tree)) =   Adjusted_Type_Size(idx);
	    if(!Type_Is_Shared_Ptr(WN_ty(tree)) && WN_field_id(tree) == 0) {  
	      UINT offst = WN_offset(tree);
	      UINT rem, items;
	      UINT sz = TY_size(TY_pointed(WN_ty(tree)));
		//this is the case where you have an array indexed with a constant, 
		//otherwise the front-end should have already adjusted the offset by now so leave 
		// it unchanged
		if(offst > sz) {
		  rem = offst % sz;
		  items = offst / sz;
		  WN_offset(tree) = Adjusted_Type_Size(idx) *  items + rem;
		}
	    }
	  }
	  tree = improve_Malignment(tree, WN_kid0(tree), WN_kid1(tree),
				    WN_load_offset(tree));
	  break;
	}  /* fall thru */

  case OPR_ILOAD:
    if (Action(LOWER_MLDID_MSTID) && WN_opcode(tree) == OPC_MMILOAD)
      return lower_miload(block, tree, actions);

    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator(tree) == OPR_ILDBITS)
	return lower_load_bits (block, tree, actions);
    }

    if (Action(LOWER_SPLIT_CONST_OFFSETS))
    {
     /*
      * Convert
      *      EXPR
      *    ILOAD (offset>16bits)
      * Into
      *        CONST (hi)
      *        EXPR
      *      ADD
      *    ILOAD (lo)
      */
      WN_OFFSET  offset = WN_load_offset(tree);
      if (mem_offset_must_be_split(offset))
      {
	WN_kid0(tree) =  WN_Add(Pointer_type,
				WN_kid0(tree),
				WN_Intconst(Pointer_type,
					    mem_offset_hi(offset)));
	WN_load_offset(tree) = mem_offset_lo(offset);
      }
    }
    else
    {
     /*
      * Fold
      *       LDA (c1) <sym>
      *     ILOAD (c2)
      * Into
      *       LDA (0)  <sym>
      *     ILOAD (c1+c2)
      */
      WN *kid = WN_kid0(tree);

      if (foldLdaOffset(kid, WN_load_offset(tree)))
      {

	//wei: this folding was causing a problem when the optimizier is enabled.
	if (!Compile_Upc) {
	  WN_load_offset(tree) += WN_lda_offset(kid);
	  WN_lda_offset(kid) = 0;
	}
      }

     /*
      * Fold
      *           EXPR 
      *           CONST (c1)
      *       ADD expr
      *     ILOAD (c1)
      * Into
      *       EXPR
      *     ILOAD (c1+c2)
      */
      else if (!Compile_Upc && 
	       WN_operator_is(kid, OPR_ADD) &&
	       foldConstOffset(WN_kid1(kid), WN_load_offset(tree)))
      {
	WN_load_offset(tree) += WN_const_val(WN_kid1(kid));
	WN_kid0(tree) = WN_kid0(kid);
	WN_Delete(WN_kid1(kid));
	WN_Delete(kid);
      }
    }

    if(Action(LOWER_UPC_TO_INTR) ) {
      TY_IDX idx = WN_ty(tree);
      WN *ild = 0;
      BOOL has_off = TRUE;
      BOOL local_off = FALSE;
      WN *offt = 0;
      TY_IDX src_idx;
     
      if (TY_is_shared(WN_ty(tree))) {
	SPTR_ACCUMULATION_STATE saved_acc_state = sptr_accumulation_state;
	sptr_off_accumulation_stack.push(CXX_NEW(SPTR_OFFSET_TERM_STACK, Malloc_Mem_Pool));
	SPTR_OFFSET_TERM_STACK *term_stack = sptr_off_accumulation_stack.top();
	
	WN *kid0 = WN_kid0(tree);

	if((WN_operator(kid0) == OPR_LDID && !WN_offset(tree)) ||
	   WN_operator(kid0) == OPR_ARRAY ||
	   // the optimizer creates LDA(off, array) and the pointer arith
	   // is performed when lowering the LDA
	  ( WN_operator(kid0) == OPR_TAS && WN_operator(WN_kid0(kid0)) == OPR_LDA &&
	    TY_kind(TY_pointed(WN_ty(WN_kid0(kid0)))) == KIND_ARRAY) )
	  has_off = FALSE;
	
	if(WN_offset(tree) || WN_field_id(tree)) {
	  offt = WN_Intconst(Integer_type,  
			     WN_field_id(tree) > 1 ? 
			     Adjust_Field_Offset(TY_pointed(WN_load_addr_ty(tree)), 
						 WN_offset(tree)) : 
			     WN_offset(tree));
	  WN_offset(tree) = 0;
	  local_off = TRUE;
	}
	
	if(WN_operator(WN_kid0(tree)) == OPR_TAS)  
	  WN_set_load_addr_ty(tree, WN_ty(WN_kid0(tree)));

  
	switch (WN_operator(WN_kid0(tree))) {
	case OPR_LDID:
	   idx = ST_type(WN_st(WN_kid0(tree)));
	   break;
	case OPR_TAS:
	  idx = WN_ty(WN_kid0(tree));
	  break;
	default:
	  break;
	}
	
	WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	sptr_accumulation_state = ACCUMULATION;	 
	if(WN_operator(WN_kid0(tree)) == OPR_TAS) { 
	  src_idx = WN_ty(WN_kid0(tree));
	  WN_kid0(tree) = Strip_TAS(WN_kid0(tree));
	} else
	  src_idx = idx;
	
	if(has_off) {	 
	  ild = Combine_Offset_Terms(*term_stack);
	  //don't even ask why
	  if(( TY_kind(idx) == KIND_POINTER && TY_kind(TY_pointed(idx)) != KIND_STRUCT)
	     ||
	     ( TY_kind(idx) == KIND_POINTER && TY_kind(TY_pointed(idx)) == KIND_STRUCT && ild)
	     ||
	     (TY_kind(idx) == KIND_ARRAY && TY_kind(Get_Inner_Array_Type(idx)) != KIND_STRUCT)
	     ||
	     (TY_kind(idx) == KIND_ARRAY && TY_kind(Get_Inner_Array_Type(idx)) == KIND_STRUCT && ild)
	     ) {
	    if (TY_kind(src_idx) == KIND_POINTER && 
		(WN_operator(WN_kid0(tree)) == OPR_LDID &&
		 Types_Are_Equiv(/*TY_pointed(src_idx)*/src_idx, ST_type(WN_st(WN_kid0(tree))))
		 || !local_off)) {
	      UINT esize = Get_Type_Inner_Size(idx);
	     
	      if(TY_kind(idx) == KIND_POINTER && TY_is_shared(TY_pointed(idx))) {
		
		if(TY_kind(TY_pointed(idx)) == KIND_POINTER)
		  esize = TY_size(TY_To_Sptr_Idx(idx));
		else if(TY_kind(TY_pointed(idx)) == KIND_ARRAY)  {
		  TY_IDX tmp = TY_etype(TY_pointed(idx));
		  if(TY_kind(tmp) != KIND_SCALAR && TY_kind(tmp) != KIND_STRUCT)
		    esize = TY_size(TY_To_Sptr_Idx(TY_etype(TY_pointed(idx))));
		}
	      }
	      if(ild)
	      WN_kid0(tree)  = WN_Create_Shared_Ptr_Arithmetic (WN_kid0(tree), ild, OPR_ADD,
								esize,
								Get_Type_Block_Size(idx),
								Get_Type_Block_Size(idx) <= 1);
	      if(offt) 
		ild = offt;
	      else
		has_off = FALSE;
	    }	    
	  }
	} 
	//patch now offset for arrays
	if(!ild && offt) {
	  ild = offt;
	  has_off = TRUE;
	} else if(!ild)
	  has_off = FALSE;
	sptr_accumulation_state = saved_acc_state;
	sptr_off_accumulation_stack.pop();
	CXX_DELETE(term_stack, Malloc_Mem_Pool);
	if(TY_is_shared(src_idx) && 
	   (TY_kind(src_idx) == KIND_POINTER && !TY_is_shared(TY_pointed(src_idx)))) {
	  //shared ptr to local element?
	  return tree;
	} else
	  return WN_Create_Shared_Load(tree, 0, 0, 0, has_off, ild);
      } else {
	if(Type_Is_Shared_Ptr(WN_ty(tree), TRUE)) {
	  //loading a pointer-to-shared
	  idx = TY_To_Sptr_Idx(WN_ty(tree));
	  WN_set_rtype(tree, TY_mtype(idx));
	  WN_set_desc(tree,TY_mtype(idx)); 
	}
	if(WN_field_id(tree) > 0) {
	  TY_IDX tidx;
	  if(WN_operator(tree) == OPR_MLOAD)
	    tidx = WN_ty(tree);
	  else 
	    tidx = WN_load_addr_ty(tree);
	  WN_load_offset(tree) = 
	    Adjust_Field_Offset(TY_pointed(tidx), WN_field_id(tree), 
				WN_load_offset(tree));
	  if(Type_Is_Shared_Ptr(tidx)) {
	    WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	    return WN_Create_Shared_Load(tree, 0, 0, 0, 0, 0);
	  }
	}
      }
    }
			
			/* added by parkcs - instrument indirect load operations */
			if (Action(LOWER_UPC_PRIVATE_MEMACC)) {
					//fprintf(stderr, "lower_expr(ILOAD): case Action==UPC_PRIVATE_MEMACC reached.\n");
					WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);

					WN* parm[2];
					WN* newblk = WN_CreateBlock();
					TYPE_ID desc = WN_desc(tree);
					TYPE_ID rtype = WN_rtype(tree);
					TY_IDX ty = TY_pointed(WN_load_addr_ty(tree));
					/*if(TY_Is_Structured(ty)) { }*/
					UINT field_id = WN_field_id(tree);
					if(field_id > 0) {
						//fprintf(stderr, "Stucture type, field_id = %d\n", field_id);
						ty = Get_Field_Type(ty, field_id);
					}
					parm[0] = WN_CreateParm(Pointer_type, 
									WN_Add(Pointer_type, WN_kid0(tree), 
											WN_Intconst(Integer_type, WN_load_offset(tree))), 
									MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
					parm[1] = WN_CreateParm(Integer_type, 
									WN_Intconst(Integer_type, TY_size(ty)), 
									MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
					WN_INSERT_BlockLast(newblk, WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, 
											INTRN_UPCT_BEFORE_LREAD, 2, parm));
					ST* st = Gen_Temp_Symbol(ty, (const char*) ".Mprvldval.");
					WN_INSERT_BlockLast(newblk, WN_Stid(desc, 0, st, ty, tree, 0));
					tree = WN_CreateComma(OPR_COMMA, rtype, desc, newblk, WN_Ldid(desc, 0, st, ty ,0));

					//fprintf(stderr, "INSTRUMENT ILOAD\n");
					//fdump_tree(stderr, tree);

				  kids_lowered = TRUE;
					return tree;
			}
			/* end parkcs */
			

    break; /* ILOAD */

  case OPR_LDID: {
  
    if (Action(LOWER_RETURN_VAL) && WN_st(tree) == Return_Val_Preg)
      return lower_return_ldid(block, tree, actions);
    
    if (Action(LOWER_MLDID_MSTID) && WN_opcode(tree) == OPC_MMLDID)
      return lower_mldid(block, tree, actions);
    
    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator(tree) == OPR_LDBITS)
	return lower_load_bits (block, tree, actions);
    }

    if(Action(LOWER_UPC_TO_INTR) ) {
      WN *ical;
      TY_IDX idx, src_idx;
      
      if ( TY_is_shared(ST_type(WN_st(tree)))) { //shared scalar
	ical = WN_Create_Shared_Load(tree);
      } else if (TY_kind(ST_type(WN_st(tree))) == KIND_POINTER && 
		 TY_is_shared(TY_pointed(ST_type(WN_st(tree)))) ) {
	// local ptr to shared
	// change the descriptors to LDID
	ical = WN_COPY_Tree(tree);
	idx = TY_To_Sptr_Idx(ST_type(WN_st(tree)));
	WN_set_rtype(ical, TY_mtype(idx));
	WN_set_desc (ical, TY_mtype(idx));
	WN_set_ty(ical, idx);
      } else {
	idx = WN_field_id(tree) ? get_field_type(WN_ty(tree), WN_field_id(tree)) : WN_ty(tree);
	if(WN_field_id(tree) && Type_Is_Shared_Ptr(idx, TRUE)) {
	  ical = WN_COPY_Tree(tree);
	  src_idx = TY_To_Sptr_Idx(idx);
	  WN_set_rtype(ical, TY_mtype(src_idx));
	  WN_set_desc (ical, TY_mtype(src_idx));
	  WN_load_offset(ical) = Adjust_Field_Offset(WN_ty(tree), WN_load_offset(tree));
	} else if(WN_field_id(tree) > 1) {
	  
	  WN_load_offset(tree) = Adjust_Field_Offset(WN_ty(tree), WN_load_offset(tree));
	  goto skip;
	} else
	  goto skip;
      }
      return ical;
    }
  skip:
    if ((WN_class(tree) == CLASS_CONST)	&& (WN_load_offset(tree) == 0))
    {
      TCON	val = WN_val(tree);
      TYPE_ID	valType = WN_val_type(tree);
      WN	*con;

      if (MTYPE_is_integral(type) && MTYPE_is_integral(valType))
      {
	con = WN_Intconst(type, Targ_To_Host( val));
        WN_Delete(tree);
        return con;
      }
     /*
      *  check for real (complex constant) conversion not handled by Targ_Conv
      */
      else if ((MTYPE_is_float(type) && MTYPE_is_float(valType))	&&
	      !(!MTYPE_is_complex(type) && MTYPE_is_complex(valType)))
      {
	if (type != valType)
	{
	  val = Targ_Conv(type, val);
	}
	con =	Make_Const(val);
	WN_Delete(tree);
	return con;
      }
    }
    {
      PREG_NUM last_preg = Get_Preg_Num(PREG_Table_Size(CURRENT_SYMTAB));
      if ((WN_class(tree) == CLASS_PREG) &&
	  (WN_load_offset(tree) > last_preg))
      {
	  DevWarn("lower_expr() pregno %d > SYMTAB_last_preg(%d)",
		  WN_load_offset(tree), last_preg);
      }
    }
   /*
    * Exposes the LDA for RVI usage
    */
    if (Action(LOWER_SPLIT_SYM_ADDRS))
    {
      WN	*iload;
      iload = lower_split_sym_addrs(tree, WN_load_offset(tree), actions);
      if (iload)
      {
	return lower_expr(block, iload, actions);
      }
    }
    if ( Action(LOWER_FORMAL_REF) && WN_class(tree) == CLASS_VAR)
    {
      WN	   *iload;

      iload = lower_formal_ref(tree, WN_load_offset(tree),
			       WN_st(tree), actions);
      if (iload)
      {
        return lower_expr(block, iload, actions);
      }
    }
    if ( Action(LOWER_UPLEVEL))
      {
	ST *sym = WN_st(tree);
	
	if (ST_is_uplevelTemp(sym))
	  {
	    WN	   *iload;
	    
	    iload = lower_uplevel_reference(tree, WN_load_offset(tree), actions);
	    tree = lower_expr(block, iload, actions);
	    return tree;
	  }
      }
    break;
  }
  case OPR_ILDBITS:
  case OPR_LDBITS:
    if (Action(LOWER_BITS_OP))
      return lower_load_bits (block, tree, actions);
    break;
    
  case OPR_LDA:
   /* 
    *  use of LDA should mark STFL_ADDR_USED_LOCALLY
    */ 
    {
      ST *sym = WN_st(tree); 
 
      // if ((ST_class(sym) == CLASS_VAR) ||
      //     (ST_class(sym) == CLASS_FUNC)) {
      //   Do nothing here. ADDR flags should only grow more
      //   optimistic; they should never become more conservative,
      //   because the program's semantics cannot grow worse as we
      //   compile it.
      // }

      if (ST_class(sym) == CLASS_BLOCK && STB_merge(sym))
      {
	DevWarn("LDA (%s) potential bad exposed use of a mergeable symbol",
		ST_name(sym));
      }
    }
    if (Action(LOWER_SPLIT_SYM_ADDRS))
    {
      WN  *lda;
      lda =	lower_split_sym_addrs(tree, WN_lda_offset(tree), actions);
      if (lda)
      {
	return lower_expr(block, lda, actions);
      }
    }
    if ( Action(LOWER_FORMAL_REF) && WN_class(tree) == CLASS_VAR)
    {
      WN	   *ldid;

      ldid =	lower_formal_ref(tree, WN_lda_offset(tree), WN_st(tree),
				 actions);
      if (ldid)
      {
	return lower_expr(block, ldid, actions);
      }
    }
    if ( Action(LOWER_UPLEVEL))
    {
      ST *sym = WN_st(tree);

      if (ST_is_uplevelTemp(sym))
      {
        WN	   *ldid;

        ldid = lower_uplevel_reference(tree, WN_lda_offset(tree), actions);
	tree = lower_expr(block, ldid, actions);
	return tree;
      }
    }

    if ( Action(LOWER_UPC_TO_INTR) ) {
      TY_IDX tidx = WN_ty(tree);
      
      if ( (TY_kind(tidx) == KIND_POINTER && TY_is_shared(TY_pointed(tidx))) ||
	   (TY_kind(tidx) == KIND_ARRAY && TY_is_shared(TY_etype(tidx))) ){
	WN *ld; 
	TY_IDX idx1 = TY_To_Sptr_Idx(TY_kind(tidx) == KIND_POINTER ? 
				     TY_pointed(tidx) : TY_etype(tidx));
	ld  = WN_Ldid(TY_mtype(idx1), 0, WN_st(tree), idx1);
	if(WN_offset(tree)) {
	if ((TY_kind(tidx) == KIND_POINTER && TY_kind(TY_pointed(tidx)) != KIND_STRUCT) ||
	    (TY_kind(tidx) == KIND_ARRAY && TY_kind(TY_etype(tidx)) != KIND_STRUCT))
	{  
	  ld = WN_Create_Shared_Ptr_Arithmetic(ld, 
					       WN_Intconst(Integer_type,
							   WN_offset(tree)),
					       OPR_ADD,
					       Get_Type_Inner_Size(tidx),
					       Get_Type_Block_Size(tidx)); 
	} else {
	  UINT off = Adjust_Field_Offset(TY_kind(tidx) == KIND_POINTER ? 
					      TY_pointed(tidx) : TY_etype(tidx), 
					      WN_offset(tree));
          TY_IDX base_ty = TY_kind(tidx) == KIND_POINTER ? TY_pointed(tidx) : TY_etype(tidx); 
          if(off < TY_adjusted_size(base_ty) && Get_Type_Block_Size(base_ty) > 1) {
                ld = WN_Create_StoP_Cvt(ld, INTRN_S_TO_P);
          }
	  ld = WN_Create_Shared_Ptr_Arithmetic(ld,  WN_Intconst(Integer_type,off),
					       OPR_ADD, 1, 0, 1);
	}  
	
	}
	return ld;
      } else {
	tidx = TY_pointed(tidx);
	if(TY_kind(tidx) == KIND_ARRAY && TY_kind(TY_etype(tidx)) == KIND_STRUCT)
	  tidx = TY_etype(tidx);

	if(WN_offset(tree) && TY_kind(tidx) == KIND_STRUCT ) {
	  WN_offset(tree) = Adjust_Field_Offset(tidx, WN_offset(tree));
	}
	
      }
    }
    
    break;

  case OPR_CVT:
  case OPR_TRUNC:
  case OPR_RND:
    if (Action(LOWER_CVT))
    {
      tree = lower_cvt(block, tree, actions);
      kids_lowered = TRUE;
    } else if(Action(LOWER_UPC_TO_INTR)) {
      TY_IDX ty_idx = 0;
      switch (WN_operator(WN_kid0(tree))) {
      case OPR_LDID:
      case OPR_TAS:
	ty_idx = WN_ty(WN_kid0(tree));
	break;
      case OPR_COMMA:
      
      default:
	break;
      }
      if(Type_Is_Shared_Ptr(ty_idx, TRUE)) 
	return WN_Convert_Shared_To_Int(lower_expr(block, WN_kid0(tree), actions));
    }
    break;

  case OPR_TAS:

    /* If the operand of the TAS is a load from memory, try to
     * replace with a load matching the result type of the TAS.
     * Doing so may avoid a move from one register set to another.
     */
    if (Action(LOWER_TO_CG))
    {
      WN *load = WN_kid0(tree);
      if (   OPERATOR_is_load(WN_operator(load))
	  && (!WN_has_sym(load) || WN_class(load) != CLASS_PREG))
      {
	TYPE_ID tas_rtype = WN_rtype(tree);
	if (MTYPE_byte_size(tas_rtype) == MTYPE_byte_size(WN_desc(load))) {
	  WN_set_rtype(load, tas_rtype);
	  WN_set_desc(load, tas_rtype);
	  WN_Delete(tree);
	  return lower_expr(block, load, actions);
	}
      }
    }

    if (Action(LOWER_UPC_TO_INTR)) {
      //Combine with forall lowering to save a pass through the program
      //They can be combined because both lowering should be done before 
      //the optimization phase
      if (WN_operator(WN_kid0(tree)) == OPR_ADD &&
	  Type_Is_Shared_Ptr(WN_ty(tree), true)) {
	WN_kid0(tree) = rearrange_upc_ptr_arith(block, WN_kid0(tree), actions);
      }
    }
    
    // for TAS of shared ptr - need to do the lowering here.
    //Otherwise the regular code calls the whirl simplifyer that
    //deletes TAS when rtypes match.
    if(Action(LOWER_UPC_TO_INTR)) {
      if (WN_Type_Is_Shared_Ptr(tree)) {

	// the optimizer creates TAS(TAS ...
	// when doing copy propagation . Strip all inner TASes,
	// but save the outermost one for StoP conversion
	
	WN *kid0 = WN_kid0(tree);
	TY_IDX kid_idx = WN_ty(kid0);
	kid0 = Strip_TAS(kid0, WN_ty(tree));
	UINT field_id;
	if(WN_operator(WN_kid0(tree)) != OPR_TAS || WN_ty(kid0) != 0) { 
	  if(TY_kind(kid_idx) == KIND_POINTER && 
	     TY_mtype(TY_pointed(kid_idx)) == MTYPE_V) 
	    // save shared void* 
	    ;
	  else
	    kid_idx = WN_ty(kid0);
	  
	  if(OPERATOR_is_load(WN_operator(kid0)) && WN_field_id(kid0)) {
	    switch(WN_operator(kid0)) {
	    case OPR_LDID:
	    case OPR_ILOAD:
	      kid_idx = WN_object_ty(kid0);
	      break;
	    default:
	      Is_True(0,("",""));
	    }
	  }
	}
	WN_kid0(tree) = kid0;

	if (WN_operator(kid0) == OPR_INTCONST) {
	  if (Type_Is_Shared_Ptr(WN_ty(tree), true) && WN_const_val(kid0) == 0) {
	    /* wei: catch the case of 
	       INTCONST 0
	       TAS (pointer-to-shared)
	       and convert it into upc_null_{p}shared. 
	       Note this should not conflict with set_null_sptr, 
	       which handles the case when such node appears on the rhs of an assignment
	    */
	    //TY_IDX sptr_idx = TY_To_Sptr_Idx(WN_ty(tree)); 
	    TY_IDX sptr_idx = TY_To_Sptr_Idx(TY_pointed(WN_ty(tree))); 
	    return WN_Ldid(TY_mtype(sptr_idx), 0, (sptr_idx == shared_ptr_idx) ? ST_st_idx(shared_null) : ST_st_idx(pshared_null), sptr_idx, 0);
	  } else {
	    //drop the cast on the constant
	    return kid0; 
	  }
	}

	TY_IDX tas_idx = WN_ty(tree);
	INTRINSIC iop;
	BOOL spill = (WN_operator(kid0) == OPR_LDID &&  ST_class(WN_st(kid0)) == CLASS_PREG);
        TY_IDX base_ty = (TY_kind(WN_ty(kid0)) == KIND_POINTER) ? TY_pointed(WN_ty(kid0)) : TY_etype(WN_ty(kid0));
	
	if(Type_Is_Shared_Ptr(kid_idx, TRUE) && !(WN_operator(kid0) == OPR_LDA && TY_kind(base_ty) == KIND_STRUCT && WN_offset(kid0) < TY_size(base_ty) ) && Need_StoP_Cvt(kid_idx, tas_idx, &iop)) 
	  {	
	    WN *res = lower_expr(block, kid0, actions);
	    return  WN_Create_StoP_Cvt(res, iop);
	  } 
      } else {
	TY_IDX kid_idx = 0;
	WN *kid = WN_kid0(tree);
	kid_idx = WN_Get_Ref_TY(kid);

	if (Type_Is_Shared_Ptr(kid_idx)) {
	  kid = lower_expr(block, kid, actions);
	  if(TY_kind(kid_idx) == KIND_SCALAR ) 
	    return kid;
	  else 
	    return WN_Convert_Shared_To_Local(kid, WN_ty(tree), 0);
	}
      }
      
      WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
    
      // Fixup cases where lower_expr() missed a shared/pshared conversion
      if (WN_Type_Is_Shared_Ptr(tree)) {
	TY_IDX base_idx = (TY_kind(WN_ty(tree)) == KIND_POINTER)
			? TY_pointed(WN_ty(tree)) : TY_etype(WN_ty(tree));
	if (Type_Is_Shared_Ptr(base_idx)) {
	  TY_IDX kid_idx = WN_ty(WN_kid0(tree));

	  if (TY_is_pshared(base_idx)) {
	    if (kid_idx == shared_ptr_idx) { // This is the bug2500 case
	      WN_kid0(tree) = WN_Create_StoP_Cvt(WN_kid0(tree), INTRN_S_TO_P);
	    }
	  } else {
	    if (kid_idx == pshared_ptr_idx) { // This is just here to be safe
	      WN_kid0(tree) = WN_Create_StoP_Cvt(WN_kid0(tree), INTRN_P_TO_S);
	    }
	  }
	}
      }

      return tree;
      
    }
      
    break;

  case OPR_IMAGPART:
    if (Action(LOWER_COMPLEX))
    {
      WN	*realexp, *imagexp;
      lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);
      WN_Delete(tree);
      tree = lower_expr(block, imagexp, actions);
    }
    break;

  case OPR_REALPART:
    if (Action(LOWER_COMPLEX))
    {
      WN	*realexp, *imagexp;

      lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);
      WN_Delete(tree);
      tree = lower_expr(block, realexp, actions);
    }
    break;
    
  case OPR_EQ:
      if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      /*
       *  x == y
       *    R(x)==R(y) && I(x)==I(y)
       */
      WN	*rx, *ry, *ix, *iy;
      TYPE_ID	realTY = Mtype_complex_to_real( WN_desc(tree));

      lower_complex_expr(block, WN_kid0(tree), actions, &rx, &ix);
      lower_complex_expr(block, WN_kid1(tree), actions, &ry, &iy);
 
      tree = WN_LAND(WN_EQ(realTY, rx, ry),
		     WN_EQ(realTY, ix, iy));

      return lower_expr(block, tree, actions);
    }
    if(Action(LOWER_UPC_TO_INTR)) {
      if((WN_Type_Is_Shared_Ptr(WN_kid0(tree))) || 
	 WN_Type_Is_Shared_Ptr(WN_kid1(tree))) {
	return lower_upc_ptr_test(block, tree, actions);
      }
    }
    break;

  case OPR_NE:
      if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
	{
	  /*
	   *  x != y
	   *    ! ( R(x)==R(y)  &&  I(x)==I(y) )
	   */
	  WN	*rx, *ry, *ix, *iy;
	  TYPE_ID	realTY = Mtype_complex_to_real( WN_desc(tree));
	  
	  lower_complex_expr(block, WN_kid0(tree), actions, &rx, &ix);
	  lower_complex_expr(block, WN_kid1(tree), actions, &ry, &iy);
	  
	  tree = WN_LNOT(WN_LAND(WN_EQ(realTY, rx, ry),
				 WN_EQ(realTY, ix, iy)));
	  
	  return lower_expr(block, tree, actions);
	}
      if(Action(LOWER_UPC_TO_INTR)) {
	if(WN_Type_Is_Shared_Ptr(WN_kid0(tree)) || 
	   WN_Type_Is_Shared_Ptr(WN_kid1(tree))) {
	  return lower_upc_ptr_test(block, tree, actions);
	}
      }
      break;
  case OPR_GE:
  case OPR_GT:
  case OPR_LT:
  case OPR_LE:
    if(Action(LOWER_UPC_TO_INTR)) {
      if(WN_Type_Is_Shared_Ptr(WN_kid0(tree)) ||
	 WN_Type_Is_Shared_Ptr(WN_kid1(tree))) {
	return lower_upc_ptr_test(block, tree, actions);
      }
    }
    break;
      
  case OPR_MADD:
    if (Action(LOWER_QUAD) && MTYPE_is_quad(type))
    {
     /*
      *   kid1 * kid2 + kid0
      */
      WN	*wn;

      wn = WN_Add(type,
		  WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)),
		  WN_kid0(tree));
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_MSUB:
    if (Action(LOWER_QUAD) && MTYPE_is_quad(type))
    {
     /*
      *   kid1 * kid2 - kid0
      */
      WN	*wn;

      wn = WN_Sub(type,
		  WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)),
		  WN_kid0(tree));
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_NMADD:
    if (Action(LOWER_QUAD) && MTYPE_is_quad(type))
    {
     /*
      *   - (kid1 * kid2 + kid0)
      */
      WN	*wn, *madd;

      madd = WN_Add(type,
		    WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)),
		    WN_kid0(tree));

      wn = WN_Neg(type, madd);
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_NMSUB:
    if (Action(LOWER_QUAD) && MTYPE_is_quad(type))
    {
     /*
      *   - (kid1 * kid2 - kid0)  -->   (kid0 - kid1 * kid2)
      */
      WN	*wn;

      wn = WN_Sub(type,
		  WN_kid0(tree),
		  WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)));
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_RSQRT:
    tree = lower_rsqrt(block, tree, actions);
    break;

  case OPR_RECIP:
    tree = lower_recip(block, tree, actions);
    break;

  case OPR_SELECT:
    {
      WN * const kid0 = WN_kid0(tree);	// the condition expression
      if (WN_operator_is(kid0, OPR_INTCONST))
      {
	INT64 flag = WN_const_val(kid0);
	WN * const kid = flag ? WN_kid1(tree) : WN_kid2(tree);
	return lower_expr(block, kid, actions);
      } else if (WN_operator(kid0) == OPR_LNOT) {
	// swap the select inputs and replace the condition with
	// operand of the LNOT. Then lower the whole tree again
	// as there may be further lowerings that may occur with the
	// new operands.
	WN * const new_kid1 = WN_kid2(tree);
	WN * const new_kid2 = WN_kid1(tree);
	WN * const new_kid0 = WN_kid0(kid0);
	TYPE_ID new_desc = (WN_rtype(new_kid0) == MTYPE_B) ? MTYPE_B : MTYPE_V;
	WN_kid0(tree) = new_kid0;
	WN_kid1(tree) = new_kid1;
	WN_kid2(tree) = new_kid2;
	WN_set_desc(tree, new_desc);
	WN_Delete(kid0);
	return lower_expr(block, tree, actions);
      }
    }
    break;

  case OPR_PAREN:
    if (Roundoff_Level > ROUNDOFF_ASSOC)
    {
     /*
      *  At suitable roundoff we may remove these parens
      *  This will allow better MADD generation latter (pv 316380)
      */
      WN *kid0 = WN_kid0(tree);

      WN_Delete(tree);
      return lower_expr(block, kid0, actions);
    }
    break;

  case OPR_DIV:
  case OPR_REM:
  case OPR_MOD:

    {

      /* If not inlining divides, then generate an INTRINSIC_OP that is
       * later lowered to a call
       */
      TYPE_ID rtype = OPCODE_rtype(WN_opcode(tree));
      if (Should_Call_Divide(rtype) && !Is_Fast_Divide(tree)) {

#ifdef EMULATE_LONGLONG
        if (rtype == MTYPE_I8 || rtype == MTYPE_U8) {
	  FmtAssert (OPCODE_rtype(WN_opcode(WN_kid0(tree))) == rtype,
		     ("DIV/REM/MOD: kid0 should be %d, is %d",
		     rtype, OPCODE_rtype(WN_opcode(WN_kid0(tree)))));
	  FmtAssert (OPCODE_rtype(WN_opcode(WN_kid1(tree))) == rtype,
		     ("DIV/REM/MOD: kid1 should be %d, is %d",
		     rtype, OPCODE_rtype(WN_opcode(WN_kid1(tree)))));
	}
#endif

	WN *kids[2];
	WN *iwn;
	LEAF tmpY;
	INTRINSIC intrinsic;
	BOOL is_unsigned = MTYPE_is_unsigned(rtype);
	BOOL is_float = MTYPE_is_float(rtype);
	BOOL is_double = MTYPE_is_size_double(rtype);
	switch (WN_operator(tree)) {
	case OPR_DIV:
	  if (is_float) {
	    intrinsic = (is_double ? INTRN_DIVDF3 : INTRN_DIVSF3);
	  } else if (is_double) {
	    intrinsic = (is_unsigned ? INTRN_UDIVDI3 : INTRN_DIVDI3);
	  } else {
	    intrinsic = (is_unsigned ? INTRN_UDIVSI3 : INTRN_DIVSI3);
	  }
	  break;
	case OPR_MOD:
	  FmtAssert(!is_float, ("Unexpected MOD operator on float"));
	  // Unsigned MOD is the same as REM.
	  // Signed MOD is a REM followed by an adjustment which
	  // uses the divisor, so save it to a temp and replace the
	  // divisor operand of the tree with a load from the temp.
	  if (!is_unsigned) {
	    tmpY = Make_Leaf(block, WN_kid1(tree), type);
	    WN_kid1(tree) = Load_Leaf(tmpY);
	  }
	  /*FALLTHROUGH*/
	case OPR_REM:
	  FmtAssert(!is_float, ("Unexpected REM operator on float"));
	  if (is_double) {
	    intrinsic = (is_unsigned ? INTRN_UMODDI3 : INTRN_MODDI3);
	  } else {
	    intrinsic = (is_unsigned ? INTRN_UMODSI3 : INTRN_MODSI3);
	  }
	  break;
	default:
	  #pragma mips_frequency_hint NEVER
	  FmtAssert (FALSE, ("Unexpected division operator"));
	  /*NOTREACHED*/
	}
	kids[0] = WN_CreateParm (rtype,
				 lower_expr(block, WN_kid0(tree), actions),
				 Be_Type_Tbl(rtype),
				 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
	kids[1] = WN_CreateParm (rtype, 
				 lower_expr(block, WN_kid1(tree), actions),
				 Be_Type_Tbl(rtype),
				 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
	iwn = WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_OP,
						 rtype, MTYPE_V),
				  intrinsic, 2, kids);

	if (WN_operator(tree) == OPR_MOD && !is_unsigned) {
	  // For signed MOD, we need to add the divisor to the result
	  // of the REM if both of the operands are negative.
	  WN *t2, *t3, *t4;
	  PREG_NUM t1;
	  t1 = AssignExpr(block, iwn, type);
	  t2 = WN_Bxor(type, WN_LdidPreg(type, t1), Load_Leaf(tmpY));
	  t3 = WN_Ashr(type, t2, WN_Intconst(type, MTYPE_size_reg(type) - 1));
	  t4 = WN_Band(type, Load_Leaf(tmpY), t3);
	  iwn = WN_Add(type, WN_LdidPreg(type, t1), t4);
	  iwn = lower_expr(block, iwn, actions);
	}

	WN_Delete(tree);
	return iwn;
      }
    }

    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_rtype(tree))) {
	// complex div creates if-then-else structure,
	// so want to expand this early, then just return C*LDID preg here
	// (e.g. in case is under a PARM node, and is not first parm).
	// Note that fortran fe moves this out from under call, 
	// but C doesn't.  Apparently only get here for C PARM case.
	TYPE_ID	cdiv_mtype = WN_rtype(tree);
	// using a preg causes wopt to get confused and not
	// connect the F* preg numbers with the C* preg number.
	// so use temp symbol instead.
	TY_IDX cdiv_ty = MTYPE_To_TY(cdiv_mtype);
	ST *cdiv_st = Gen_Temp_Symbol (cdiv_ty, ".complex_div");
	WN *stid = WN_Stid(cdiv_mtype, 0, cdiv_st, cdiv_ty, tree);
      	WN_Set_Linenum (stid, current_srcpos);
	stid = lower_store(block, stid, actions);
	WN_INSERT_BlockLast(block, stid);
	WN *ldid = WN_Ldid(cdiv_mtype, 0, cdiv_st, cdiv_ty);
	return ldid;
    }
    break;

  case OPR_COMMA:
    {
      WN *commaBlock;
      commaBlock = lower_block(WN_kid0(tree), actions);
      
      //during UPC lowering we create comma operators that
      // are otherwise illegal when compiling a regular file
      // through the code gen
      if (!Action(LOWER_UPC_TO_INTR) && !Action(LOWER_UPC_INTRINSIC))
	DevWarn("lower_expr(): comma operator seen, line %d",
		Srcpos_To_Line(current_srcpos));

      WN_INSERT_BlockLast(block, commaBlock);
    }
    return lower_expr(block, WN_kid1(tree), actions);

  case OPR_CSELECT:
   /*
    *  
    */
  
    if (Action(LOWER_SHORTCIRCUIT))
    {
      /*******************************************************************

      DevWarn("lower_expr(): cselect operator seen, line %d",
              Srcpos_To_Line(current_srcpos));
      *******************************************************************/

      if (expr_is_speculative(tree))
      {
	WN *select = WN_Select(type, WN_kid0(tree), WN_kid1(tree),
			       WN_kid2(tree));
  
	WN_Delete(tree);
	return select;
      }
      else
      {
	PREG_NUM  tmpN;
	WN *if_tree, *if_then, *if_else, *stid;
	WN *body = WN_CreateBlock();

	if_then = WN_CreateBlock();
	tmpN = AssignExpr(if_then, WN_kid1(tree), type);

	if_else = WN_CreateBlock();
	stid = WN_StidIntoPreg(type, tmpN, MTYPE_To_PREG(type), WN_kid2(tree));
	WN_INSERT_BlockLast(if_else, stid);

	if_tree = WN_CreateIf( WN_kid0(tree), if_then, if_else );
	if ( Cur_PU_Feedback )
	  Cur_PU_Feedback->FB_lower_branch( tree, if_tree );
	WN_INSERT_BlockLast( block, lower_if( body, if_tree, actions ) );

	return WN_LdidPreg(type, tmpN);
      }
    }
    break;

  case OPR_CAND:
  case OPR_CIOR:
   /*
    *  return boolean 0/1 (build CSELECT)
    */
    if (Action(LOWER_SHORTCIRCUIT))
    {
      if (expr_is_speculative(tree))
      {
	WN *cond;

	if (WN_operator_is(tree, OPR_CAND))
	  cond = WN_LAND( WN_kid0(tree), WN_kid1(tree));
	else
	  cond = WN_LIOR( WN_kid0(tree), WN_kid1(tree));

	WN_Delete(tree);
	return lower_expr(block, cond, actions);
      }
      else
      {
	WN *select = WN_Cselect(type,
				tree,	
				WN_Intconst(Boolean_type, 1),
				WN_Intconst(Boolean_type, 0));
	return lower_expr(block, select, actions);
      }
    }
    break;
  }

  
  if (Action(LOWER_QUAD))
  {
    if (WN_desc(tree) == MTYPE_FQ)
    {
      switch (WN_operator(tree))
      {
      case OPR_CONST:
      case OPR_LDID:
      case OPR_ILOAD:
	break;
      case OPR_EQ:
      case OPR_NE:
      case OPR_LE:
      case OPR_LT:
      case OPR_GT:
      case OPR_GE:
      case OPR_CVT:
      case OPR_TRUNC:
      case OPR_RND:
      case OPR_CEIL:
      case OPR_FLOOR:
	tree = lower_emulation(block, tree, actions,intrinsic_lowered);
	break;
      default:
	break;
      }
    }
    if (WN_rtype(tree) == MTYPE_FQ)
    {
      switch (WN_operator(tree))
      {
      case OPR_CONST:
      case OPR_LDID:
      case OPR_ILOAD:
	break;

      case OPR_SELECT:
      case OPR_NEG:
      case OPR_ABS:
      case OPR_SQRT:
      case OPR_ADD:
      case OPR_SUB:
      case OPR_MPY:
      case OPR_DIV:
      case OPR_MOD:
      case OPR_REM:
      case OPR_MAX:
      case OPR_MIN:
      case OPR_CVT:
      case OPR_TRUNC:
      case OPR_RND:
	tree = lower_emulation(block, tree, actions, intrinsic_lowered);
	break;
      default:
	break;
      }
    }
  }

  if (WN_nary_intrinsic(tree))
  {
    tree = WN_NaryToExpr(tree);
  }

  /* Lower kids if not done already. */
  if (! kids_lowered)
  {
     INT16 i;
     for (i = 0; i < WN_kid_count(tree); i++) {
	if (WN_kid(tree, i))
	    WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
     }
     tree = WN_Simplify_Rebuild_Expr_Tree(tree,alias_manager);
  }

  return tree;
}


/* ====================================================================
 *
 *  static TY_IDX coerceTY(TY_IDX type, TYPE_ID btype)
 *
 * return TY corresponding to the btype
 * (type might be pointer -> btype)
 *
 * ==================================================================== */

static TY_IDX coerceTY(TY_IDX type, TYPE_ID btype)
{
  TY &ty = Ty_Table[type];

  if (TY_is_pointer(ty))
    return Make_Pointer_Type(coerceTY(TY_pointed(ty), btype));

  return MTYPE_To_TY(btype);
}




/* ====================================================================
 *
 *  static ST * coerceST(const ST *st, TYPE_ID type)
 *
 * return ST corresponding to the type
 *
 * ==================================================================== */

static ST *coerceST(const ST *st, TYPE_ID type)
{
  if (ST_class(st) == CLASS_PREG)
  {
   /*
    *  for now, only pregs must correspond to the type
    */
    return MTYPE_To_PREG(type);
  }

  return (ST *) st;
}

static ST *coerceST(const ST &st, TYPE_ID type)
{
  if (ST_class(&st) == CLASS_PREG)
  {
   /*
    *  for now, only pregs must correspond to the type
    */
    return MTYPE_To_PREG(type);
  }

  return (ST *) &st;
}


/* ====================================================================
 *
 * static BOOL WN_StoreIsUnused(WN *tree)
 *
 * Find if store has been marked by IPA as unused
 * This may require traversal of the address expression to find
 * an LDA or ARRAY
 * ==================================================================== */
static BOOL WN_StoreIsUnused(WN *tree)
{
  ST  *sym;

  switch(WN_operator(tree))
  {
  case OPR_LDA:
  case OPR_STID:
    sym = WN_st(tree);

    if (ST_class(sym) != CLASS_PREG  &&
        ST_class(sym) != CLASS_BLOCK &&
	ST_is_not_used(sym))
      return TRUE;
    break;

  case OPR_ARRAY:
    return WN_StoreIsUnused(WN_array_base(tree));
  }

  return FALSE;
}

/* ====================================================================
 *
 * WN *add_fake_parm(WN *o_call, WN *fake_actual)
 *
 * Add the fake actual parameter as the first parameter to the original call.
 * All original parameters are shifted down by 1.
 *
 * ==================================================================== */
static WN *add_fake_parm(WN *o_call, WN *fake_actual, TY_IDX ty_idx)
{
  WN *n_call;
  if (WN_operator(o_call) == OPR_ICALL)
    n_call = WN_Icall(MTYPE_V, MTYPE_V, WN_kid_count(o_call)+1, WN_ty(o_call));
  else
    n_call = WN_generic_call(WN_operator(o_call), MTYPE_V, MTYPE_V, 
			     WN_kid_count(o_call)+1, WN_st_idx(o_call));
  WN_call_flag(n_call) = WN_call_flag(o_call);
  WN_Set_Linenum(n_call, WN_Get_Linenum(o_call));
  if ( Cur_PU_Feedback ) {
    Cur_PU_Feedback->FB_lower_call( o_call, n_call );
  }
  WN_kid0(n_call) = WN_CreateParm(Pointer_Mtype, fake_actual, ty_idx,
			      WN_PARM_BY_REFERENCE | WN_PARM_PASSED_NOT_SAVED);
  for (INT32 i = 0; i < WN_kid_count(o_call); i++)
    WN_kid(n_call, i+1) = WN_COPY_Tree(WN_kid(o_call, i));
  return n_call;
}

/* ====================================================================
 *
 * WN *lower_return_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * "tree" must be an MSTID whose kid is MLDID of Return_Val_Preg (-1).
 * Perform lowering of MSTID whose rhs is Return_Val_Preg; translate to
 * either MSTORE or a sequence of STIDs.
 *
 * ==================================================================== */
static WN *lower_return_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID mtype;
  ST *preg_st;
  WN *n_rhs;
  WN *wn = NULL;	// init to prevent upward-exposed use
  RETURN_INFO return_info = Get_Return_Info(WN_ty(tree), Complex_Not_Simulated);
  if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
    if(Compile_Upc) return tree;
    // get the previous MCALL statement
    WN *call = WN_last(block);
    Is_True(WN_operator(call) == OPR_CALL || WN_operator(call) == OPR_ICALL ||
	    WN_operator(call) == OPR_PICCALL,
	    ("statement preceding MMLDID of Return_Val_Preg must be a call"));
    Is_True(WN_rtype(call) == MTYPE_M,
	    ("call preceding MMLDID of Return_Val_Preg not type M"));
    WN *awn = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 
			   WN_store_offset(tree), 
			   Make_Pointer_Type(WN_ty(tree)), WN_st_idx(tree));
    awn = lower_expr(block, awn, actions);
    WN *n_call = add_fake_parm(call, awn, WN_ty(awn));
    WN_DELETE_FromBlock(block, call);
    WN_INSERT_BlockLast(block, n_call); 

    WN_DELETE_Tree(tree);
    return NULL; // original MSTID disappears
  }
  else { // return via 1 or more return registers
    if (Compile_Upc && WN_desc(tree) == MTYPE_M) {
      /* We don't lower this into pregs, since whirl2c only allows 
       * a function to use at most two return pregs.  Instead, we fix this
       * by using a tmp variable in whirl2c.
       */
      return tree;
    }

    for (INT32 i = 0; i < RETURN_INFO_count(return_info); i++) {
      if (i != 0)
        WN_INSERT_BlockLast (block, wn); // insert the last STID created 
      mtype = RETURN_INFO_mtype(return_info, i);
      preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
      n_rhs = WN_CreateLdid(OPR_LDID, mtype, mtype, 
			    RETURN_INFO_preg(return_info, i), preg_st,
			    Be_Type_Tbl(mtype));
      if (TY_align(ST_type(WN_st(tree))) < MTYPE_alignment(mtype)) {
	// shared_ptr_idx and pshared_ptr_idx are opaque types
	// when the type does not fit in a register we are conservative
	// and asume an alignment of 1
	if(ST_type(WN_st(tree)) != shared_ptr_idx && ST_type(WN_st(tree)) != pshared_ptr_idx)
	  DevWarn("return_info struct alignment is smaller than register size, may produce wrong results");
      }
      wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, 
		         WN_store_offset(tree)+i*MTYPE_byte_size(mtype),
			 WN_st_idx(tree), Be_Type_Tbl(mtype), n_rhs);
      wn  = lower_store (block, wn, actions);
      WN_Set_Linenum (wn, WN_Get_Linenum(tree));
    }
    WN_DELETE_Tree(tree);
    return wn;
  }
}

/* ====================================================================
 *
 * WN *lower_return_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * "tree" must be an MISTORE whose rhs is MLDID of Return_Val_Preg (-1).
 * Perform lowering of MISTORE whose rhs is Return_Val_Preg; translate to
 * either MSTORE or a sequence of ISTOREs.
 *
 * ==================================================================== */
static WN *lower_return_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID mtype;
  ST *preg_st;
  WN *n_rhs;
  WN *wn = NULL;	// init to prevent upward-exposed use
  RETURN_INFO return_info = Get_Return_Info(WN_ty(tree), Complex_Not_Simulated);
  if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
    // get the previous MCALL statement
    WN *call = WN_last(block);
    Is_True(WN_operator(call) == OPR_CALL || WN_operator(call) == OPR_ICALL ||
	    WN_operator(call) == OPR_PICCALL,
	    ("statement preceding MMLDID of Return_Val_Preg must be a call"));
    Is_True(WN_rtype(call) == MTYPE_M,
	    ("call preceding MMLDID of Return_Val_Preg not type M"));
    WN *awn = WN_COPY_Tree(WN_kid1(tree));
    if (WN_store_offset(tree) != 0) { // generate an ADD node for the offset
      WN *iwn = WN_CreateIntconst(OPR_INTCONST, Pointer_Mtype, MTYPE_V, 
				  WN_store_offset(tree));
      awn = WN_CreateExp2(OPR_ADD, Pointer_Mtype, Pointer_Mtype, awn, iwn);
    }
    awn = lower_expr(block, awn, actions);
    WN *n_call = add_fake_parm(call, awn, WN_ty(tree));
    WN_DELETE_FromBlock(block, call);
    WN_INSERT_BlockLast(block, n_call); 

    WN_DELETE_Tree(tree);
    return NULL; // original MSTID disappears
  }
  else { // return via 1 or more return registers
    WN *base_expr;
    for (INT32 i = 0; i < RETURN_INFO_count(return_info); i++) {
      if (i != 0)
        WN_INSERT_BlockLast (block, wn); // insert the last STID created 
      mtype = RETURN_INFO_mtype(return_info, i);
      preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
      n_rhs = WN_CreateLdid(OPR_LDID, mtype, mtype, 
			    RETURN_INFO_preg(return_info, i), preg_st,
			    Be_Type_Tbl(mtype));
      base_expr = WN_COPY_Tree(WN_kid1(tree));
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, mtype, 
		           WN_store_offset(tree)+i*MTYPE_byte_size(mtype),
			   Be_Type_Tbl(mtype), n_rhs, base_expr);
      wn  = lower_store (block, wn, actions);
      WN_Set_Linenum (wn, WN_Get_Linenum(tree));
    }
    WN_DELETE_Tree(tree);
    return wn;
  }
}

/* ====================================================================
 *
 * WN *lower_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MSTID nodes returning
 * an equivalent MSTORE node.
 *
 * ==================================================================== */

static WN *lower_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX ty_idx  = WN_ty(tree);
  TY_IDX pty_idx = WN_field_id(tree) == 0 ? 0 : get_field_type (ty_idx, WN_field_id(tree));
  UINT64 size    = pty_idx  == 0 ?  Adjusted_Type_Size(ty_idx):  TY_size(pty_idx );

  //As part of recent driver change, the MLDID_MSTID lowering is now performed after the optimizer is invoked,
  //so that its lowering would not confuse the optimizer.  This, however, means that we have to be careful with 
  //the MSTID/MLDID that may be created as part of the split-phase optimization, since we don't want them 
  //to be converted into MSTOREs
  if (WN_MAP_Get(upc_comm_map, WN_kid0(tree)) != NULL) {
      return tree;
  }
  
  if(Action(LOWER_UPC_MFIELD) && pty_idx) {
    if( Type_Is_Shared_Ptr(pty_idx))
      size = TY_size(TY_To_Sptr_Idx(pty_idx));
    else if(TY_kind(pty_idx) == KIND_ARRAY && Type_Is_Shared_Ptr(Get_Inner_Array_Type(pty_idx)))
      size = TY_size(TY_To_Sptr_Idx(Get_Inner_Array_Type(pty_idx)));
  }
  
  WN*    wn;
  WN*    awn;
  WN*    swn;
  WN *rhs = WN_kid0(tree);

  // if it is ptr to shared patch both the STID and the LDID to 
  // have the mtype corresponding to the shared_ptr_type 
  if (Action(LOWER_RETURN_VAL)) {
    if( Type_Is_Shared_Ptr(ty_idx) && WN_operator(rhs) == OPR_LDID &&
	WN_st(rhs) == Return_Val_Preg ) {
      ty_idx = TY_To_Sptr_Idx(ty_idx);
      TYPE_ID rtype = TY_mtype(ty_idx);
      WN_set_desc(tree, rtype);
      WN_set_ty(tree, ty_idx);
      
      WN_set_desc(rhs, rtype);
      WN_set_rtype(rhs, rtype);
      WN_set_ty(rhs, TY_To_Sptr_Idx(WN_ty(rhs)));
    }  
  }


  Is_True((WN_operator(tree) == OPR_STID && WN_desc(tree) == MTYPE_M),
	  ("expected mstid node, not %s", OPCODE_name(WN_opcode(tree))));

  pty_idx = Make_Pointer_Type (ty_idx, FALSE);

  if (Action(LOWER_RETURN_VAL)) {
    if (WN_opcode(rhs) == OPC_MMLDID && WN_st(rhs) == Return_Val_Preg) {
      // handle lowering of MLDID of Return_Val_Preg followed by MSTID
      //   Is_True(Action(LOWER_RETURN_VAL),
      //	      ("LOWER_RETURN_VAL action must be specified"));
      return lower_return_mstid(block, tree, actions);
    }
  }

  awn = WN_CreateLda (OPR_LDA, Pointer_Mtype, MTYPE_V, 0, // WN_store_offset(tree)
		      pty_idx, WN_st(tree));
  swn = WN_CreateIntconst (OPC_U4INTCONST, size);
  wn  = WN_CreateMstore (WN_store_offset(tree)// 0
			 , pty_idx, WN_kid0(tree), awn, swn);
  WN_set_field_id(wn, WN_field_id(tree));
  if(Action(LOWER_UPC_MFIELD))
    //delete MFIELD from actions, otherwise the offset
    //is adjusted twice - bug411
    actions = actions & (~LOWER_UPC_MFIELD);

  WN_Set_Linenum(wn, WN_Get_Linenum(tree));
  wn  = lower_store (block, wn, actions);

  WN_Delete(tree);
  return wn;
}


/* ====================================================================
 *
 * WN *lower_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MISTORE nodes returning
 * an equivalent MSTORE node.
 *
 * ==================================================================== */

static WN *lower_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX pty_idx  = WN_ty(tree);
  TY_IDX ty_idx  = TY_pointed(pty_idx);

  if (WN_field_id (tree) != 0)
    ty_idx = get_field_type (ty_idx, WN_field_id (tree));
  
  UINT64 size    = TY_size(Ty_Table[ty_idx]);
  if(Action(LOWER_UPC_MFIELD) && WN_field_id(tree) && Type_Is_Shared_Ptr(ty_idx))
    size = TY_size(TY_To_Sptr_Idx(ty_idx));
  WN*    wn;
  WN*    swn;

  Is_True((WN_operator(tree) == OPR_ISTORE && WN_desc(tree) == MTYPE_M),
	  ("expected mistore node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(TY_kind(pty_idx) == KIND_POINTER,
	  ("type specified in MISTORE not pointer"));
//Is_True(size > 0, ("type in MISTORE cannot be zero size"));
  if (size == 0)
    DevWarn ("type in MISTORE cannot be zero size");
  WN *rhs = WN_kid0(tree);
  if (WN_opcode(rhs) == OPC_MMLDID && WN_st(rhs) == Return_Val_Preg) {
    // handle lowering of MLDID of Return_Val_Preg followed by MISTORE
    Is_True(Action(LOWER_RETURN_VAL),
	    ("LOWER_RETURN_VAL action must be specified"));
    return lower_return_mistore(block, tree, actions);
  }

  swn = WN_CreateIntconst(OPC_U4INTCONST, size);
  wn  = WN_CreateMstore(WN_offset(tree), pty_idx, 
			WN_COPY_Tree(WN_kid0(tree)),
                        WN_COPY_Tree(WN_kid1(tree)), swn);
  WN_set_field_id(wn, WN_field_id(tree));
  wn  = lower_store(block, wn, actions);

  WN_DELETE_Tree (tree);
  return wn;
}

/*
 * See if the given expr is of the form
 *   
 *   LDID struct
 *   EXPR
 * ADD
 * (this happens for expr like foo->x[i])
 *
 *  If so, return the type of the struct, ow return 0
 */
static TY_IDX struct_addr_ty(WN* tree) {

  if (WN_operator(tree) == OPR_ADD &&
      WN_operator(WN_kid0(tree)) == OPR_LDID) {
    TY_IDX addr_ty = WN_ty(WN_kid0(tree));
    if (TY_kind(addr_ty) == KIND_POINTER &&
	TY_kind(TY_pointed(addr_ty)) == KIND_STRUCT) {
      return TY_pointed(addr_ty);
    }
  }
  return 0;
}

static int find_field_from_offset(TY_IDX ty, unsigned int offset) { 
  
  /*
   * field offset for component ref are now adjusted in the gnu front end,
   * so we don't need this function anymore
    
    Is_True(TY_kind(ty) == KIND_STRUCT, ("TY is not a struct", ""));
    int cur_field = 1;
    FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
    do {
    FLD_HANDLE fld(fld_iter);
    if (FLD_ofst(fld) == offset) {
      return cur_field;
      } 
      if (FLD_ofst(fld) > offset) {
      cerr << "can't find field with exact same offset in find_field_from_offset" << endl;
      return cur_field - 1;
      }
    TY_IDX fld_ty = FLD_type(fld);
    if (TY_kind(fld_ty) == KIND_STRUCT && !is_upcr_ptr(fld_ty) &&
    TY_fld(fld_ty) != FLD_HANDLE() && offset < (FLD_ofst(fld) + TY_size(fld_ty)) ) {
    return cur_field + find_field_from_offset(fld_ty, offset - FLD_ofst(fld));
    }
    cur_field++;
    } while (!FLD_last_field(fld_iter++));
  */
  return -1;
}

//see upc_coalescing.h for more details
//Note that tree is a reference because we may need to delete the get call (
//in the case it is subsumed by coalescing) by setting it to NULL
static void do_coalescing(WN* block, WN* &tree, WN* next_node, sync_handle_t *handle) {

  //list of get() calls that may be coalescing candidates. see upc_coalescing.h for details
  static DYN_ARRAY<COMM*> *coalescing_list = NULL;
  COMM* comm;

  GET* get = CXX_NEW(GET(tree, handle), &upc_mem_pool);
  //get->Print(stderr);
  if (get->Has_const_bound()) {
    bool is_added = false;
    if (coalescing_list == NULL) {
      coalescing_list = CXX_NEW (DYN_ARRAY<COMM*>(&upc_mem_pool), &upc_mem_pool);
    }
    for (int i = 0; i < coalescing_list->Elements(); i++) {
      comm = (*coalescing_list)[i];
      if (Get_addr_are_equivalent(get->Src(), comm->Base())) {
	fprintf(TFile, "found get with same base addr\n");
	comm->Add_get(get);
	is_added = true;
	break;
      }
    }
    if (!is_added) {
      comm = CXX_NEW(COMM(&upc_mem_pool), &upc_mem_pool);
      comm->Add_get(get);
      coalescing_list->AddElement(comm);
    }
  } else {
    CXX_DELETE(get, &upc_mem_pool);
  }

  if (WN_operator(next_node) != OPR_STID ||
      WN_MAP_Get(upc_comm_map, WN_kid0(next_node)) == NULL) {
    //We have recorded all of the gets in the communication point,
    //can start coalescing
    for (int i = 0; i < coalescing_list->Elements(); i++) {
      comm = (*coalescing_list)[i];
      if (comm->Coalesce(block)) {
	//remove the indivdual calls that are subsumed by the coalesced call
	for (int i = 0; i < comm->Num_get(); i++) {
	  WN* wn = comm->Nth_get(i)->Get_stmt();
	  if (tree == wn) {
	    tree = NULL;
	  } else {
	    WN_DELETE_FromBlock(block, wn);
	  }
	}
      }
      //CXX_DELETE(comm, &upc_mem_pool);
    }
    
    //coalescing_list.Free_array();
    CXX_DELETE(coalescing_list, &upc_mem_pool);
    coalescing_list = NULL;
  }
}




/* ====================================================================
 *
 * WN *lower_store(block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on store statement <tree>,
 * returning lowered tree.
 *
 * ==================================================================== */

static WN *lower_store(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  BOOL kids_lowered = FALSE;	/* becomes TRUE when kids are lowered */

  Is_True(OPCODE_is_store(WN_opcode(tree)),
	  ("expected store node, not %s", OPCODE_name(WN_opcode(tree))));

  /* If the store is to memory and the expression begins with a TAS, try to
   * replace with a store matching the result type of the TAS's expression.
   * Doing so may avoid a move from one register set to another.
   */
  if (Action(LOWER_TO_CG))
  {
    WN *tas = WN_kid0(tree);
    if (   WN_operator(tas) == OPR_TAS
	&& (!WN_has_sym(tree) || WN_class(tree) != CLASS_PREG))
    {
      WN *tas_kid0 = WN_kid0(tas);
      TYPE_ID tas_kid0_rtype = WN_rtype(tas_kid0);
      if (MTYPE_byte_size(WN_rtype(tas)) == MTYPE_byte_size(tas_kid0_rtype)) {
	WN_set_desc(tree, tas_kid0_rtype);
	WN_kid0(tree) = tas_kid0;
	WN_Delete(tas);
      }
    }
  }

  /*
   * create any maps that need to be present
   */
  lower_map(tree, actions);

  upc_srcpos = WN_Get_Linenum(tree);
  
  /* Note: We must split constant offsets after lowering complex stores
   * and splitting symbol addresses since these may create new offsets
   * that need to be split.
   */
  
  switch (WN_operator(tree))
  {
  case OPR_ISTORE: {
    if (WN_StoreIsUnused(WN_kid1(tree)))
    {
      WN	*eval;

      eval = lower_eval(block, WN_CreateEval(WN_kid0(tree)), actions);

      WN_Delete(tree);

      return eval;
    }

    if (Action(LOWER_MLDID_MSTID) && WN_desc(tree) == MTYPE_M)
      return lower_mistore(block, tree, actions);

    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator (tree) == OPR_ISTBITS)
	return lower_store_bits (block, tree, actions);
    }

    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      WN	*realstore, *imagstore;
      LEAF	realexpN, imagexpN;
      WN_OFFSET	offset = WN_store_offset(tree);
      TYPE_ID	realTY = Mtype_complex_to_real( WN_desc(tree));
      {
       /*
	* create the real/imaginary stores
	* load the temporary values into a preg before the store (pv314583)
	* as the store may interfere with the expression.
	*/
	WN	*realexp, *imagexp;

	lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);
	
	realexpN = Make_Leaf(block, realexp, realTY);
	imagexpN = Make_Leaf(block, imagexp, realTY);
      }

      if (Action(LOWER_BASE_INDEX))
      {
	WN	*base, *index, *addr;
	LEAF	indexN;

	base = index=	NULL;
	lower_to_base_index(WN_kid1(tree), &base, &index) ;

	base = lower_expr(block, base, actions);
	index = lower_expr(block, index, actions);

	indexN = Make_Leaf(block, index, Pointer_type);

	addr = WN_Add(Pointer_type,
		      Load_Leaf(indexN),
		      lower_copy_tree(base, actions));

	realstore = WN_Istore(realTY,
			      offset,
			      coerceTY(WN_ty(tree), realTY),
			      addr,
			      Load_Leaf(realexpN));

	addr = WN_Add(Pointer_type, Load_Leaf(indexN), base);

	imagstore = WN_Istore(realTY,
			      coerceOFFSET(tree, realTY, offset),
			      coerceTY(WN_ty(tree), realTY),
			      addr,
			      Load_Leaf(imagexpN));
      }
      else
      {
	WN	*addr;
        LEAF	addrN;

	addr = lower_expr(block, WN_kid1(tree), actions);
	addrN = Make_Leaf(block, addr, Pointer_type);

        realstore =  WN_Istore(realTY,
			       offset,
			       coerceTY(WN_ty(tree), realTY),
			       Load_Leaf(addrN),
			       Load_Leaf(realexpN));

        imagstore =  WN_Istore(realTY,
			       coerceOFFSET(tree, realTY, offset),
			       coerceTY(WN_ty(tree), realTY),
			       Load_Leaf(addrN),
			       Load_Leaf(imagexpN));
      }
      realstore = lower_store(block, realstore, actions);
      WN_Set_Linenum (realstore, WN_Get_Linenum(tree));
      WN_INSERT_BlockLast(block, realstore);

      imagstore = lower_store(block, imagstore, actions);
      lower_complex_maps(tree, realstore, imagstore, actions);
      WN_Delete(tree);

      return imagstore;
    }
    else if (Action(LOWER_QUAD) && MTYPE_is_quad(WN_desc(tree)))
    {
      WN	*hi, *lo, *hipart, *lopart;
      TYPE_ID	realTY = MTYPE_F8;
      WN_OFFSET	offset = WN_load_offset(tree);

      lower_quad_expr(block, WN_kid0(tree), actions, &hi, &lo);

      if (Action(LOWER_BASE_INDEX))
      {
	WN	*addr, *base, *index;
        LEAF	indexN;

	base = index = NULL;
	lower_to_base_index(WN_kid1(tree), &base, &index) ;

	base  = lower_expr(block, base, actions);
	index = lower_expr(block, index, actions);

	indexN = Make_Leaf(block, index, Pointer_type);

	addr   = WN_Add(Pointer_type,
		        Load_Leaf(indexN),
		        lower_copy_tree(base, actions));

	hipart = WN_Istore(realTY,
			   offset,
			   coerceTY(WN_ty(tree), realTY),
			   addr,
			   hi);

	addr   = WN_Add(Pointer_type, Load_Leaf(indexN), base);

	lopart = WN_Istore(realTY,
			   coerceOFFSET(tree, realTY, offset),
			   coerceTY(WN_ty(tree), realTY),
			   addr,
			   lo);
      }
      else
      {
        WN	*addr;
        LEAF	addrN;

	addr   = lower_expr(block, WN_kid1(tree), actions);

	addrN  = Make_Leaf(block, addr, Pointer_type);

	hipart = WN_Istore(realTY,
			   offset,
			   coerceTY(WN_ty(tree), realTY),
			   Load_Leaf(addrN),
			   hi);

	lopart = WN_Istore(realTY,
			   coerceOFFSET(tree, realTY, offset),
			   coerceTY(WN_ty(tree), realTY),
			   Load_Leaf(addrN),
			   lo);
      }
      hipart = lower_store(block, hipart, actions);
      WN_Set_Linenum (hipart, WN_Get_Linenum(tree));
      WN_INSERT_BlockLast(block, hipart);

      lopart = lower_store(block, lopart, actions);
      lower_quad_maps(tree, hipart, lopart, actions);
      WN_Delete(tree);
      return lopart;
    }
    else if (Action(LOWER_SPLIT_CONST_OFFSETS))
    {
      /*
       * Split
       *       ADDR
       *     ISTORE (offset>16bits)
       * into
       *         ADDR
       *         CONST (hi)
       *       ADD
       *     ISTORE (lo)
       */
      WN_OFFSET offset = WN_store_offset(tree);

      if (mem_offset_must_be_split(offset))
      {
	WN_kid1(tree) = WN_Add(Pointer_type, WN_kid1(tree),
			       WN_Intconst(Pointer_type,
					   mem_offset_hi(offset)));
	WN_store_offset(tree) = mem_offset_lo(offset);
      }
    }
    else {
     /*
      * Split
      *       LDA  (c1) <sym> 
      *	    ISTORE (c2)
      * into
      *       LDA  (0) <sym>
      *     ISTORE (c1+c2)
      *
      * provided sym is not a PREG.
      */
      WN  *addr_kid = WN_kid1(tree);

      if (foldLdaOffset(addr_kid, WN_store_offset(tree)))
      {
	WN_store_offset(tree) += WN_lda_offset(addr_kid);
	WN_lda_offset(addr_kid) = 0;
      }
     /*
      * Fold
      *         EXPR
      *         CONST (c1)
      *       ADD
      *     ISTORE c2
      * Into
      *	      EXPR
      *     ISTORE c1+c2
      */
      if (WN_operator_is(addr_kid, OPR_ADD) &&
	  foldConstOffset(WN_kid1(addr_kid), WN_store_offset(tree)))
      {
	WN_store_offset(tree) += WN_const_val(WN_kid1(addr_kid));
	WN_kid1(tree) = WN_kid0(addr_kid);
	WN_Delete(WN_kid1(addr_kid));
	WN_Delete(addr_kid);
      }
    } 
    if ( Action(LOWER_UPC_TO_INTR) ) {
 
      TY_IDX idx = WN_ty(tree);
      TY_IDX dst_idx = 0;
      if(Type_Is_Shared_Ptr(idx)) {
	TY_IDX src_idx = WN_ty(WN_kid1(tree));
	TY_IDX ref_ty = WN_Get_Ref_TY(tree);
	WN *ist = 0;
	BOOL do_arith = TRUE;
	BOOL is_field = FALSE;
	WN *offt = 0;

	SPTR_ACCUMULATION_STATE saved_acc_state = sptr_accumulation_state;
        sptr_accumulation_state = ACCUMULATION;
        sptr_off_accumulation_stack.push(CXX_NEW(SPTR_OFFSET_TERM_STACK, Malloc_Mem_Pool));
        SPTR_OFFSET_TERM_STACK *term_stack = sptr_off_accumulation_stack.top();
	WN *kid1 = WN_kid1(tree);
	
	if((WN_operator(kid1) == OPR_LDID && !WN_offset(tree)) ||
	   WN_operator(kid1) == OPR_ARRAY ||
	   // the optimizer creates LDA(off, array) and the pointer arith
	   // is performed when lowering the LDA
	  ( WN_operator(kid1) == OPR_TAS && WN_operator(WN_kid0(kid1)) == OPR_LDA &&
	   TY_kind(TY_pointed(WN_ty(WN_kid0(kid1)))) == KIND_ARRAY) )
	  do_arith = FALSE;
	
        if(WN_offset(tree)) {
	  dst_idx = TY_pointed(WN_ty(tree));
	  if(TY_kind(dst_idx) == KIND_ARRAY)
	    dst_idx = TY_etype(dst_idx);
	  offt = WN_Intconst(Integer_type, 
			     WN_field_id(tree) > 0 ? Adjust_Field_Offset(dst_idx, 
									 //WN_field_id(tree),
									 WN_offset(tree)) : 
			     WN_offset(tree));
          WN_offset(tree) = 0;
	  dst_idx = 0;
        }

	dst_idx = (OPERATOR_is_load(WN_operator(WN_kid0(tree)))  || 
		   WN_operator(WN_kid0(tree)) == OPR_TAS) ? 
		   WN_ty(WN_kid0(tree)) : 0;
	sptr_accumulation_state = NO_ACCUMULATION;
	WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	
	INTRINSIC iop;
	if(ref_ty && dst_idx &&
	   Type_Is_Shared_Ptr(ref_ty, TRUE) && 
	   Type_Is_Shared_Ptr(dst_idx, TRUE) &&
	   Need_StoP_Cvt(dst_idx, ref_ty, &iop)) { 
	    WN_kid0(tree) = WN_Create_StoP_Cvt(WN_kid0(tree), iop);
	 
	}
	

	sptr_accumulation_state = NO_ACCUMULATION;
	WN_kid1(tree) = lower_expr(block, WN_kid1(tree), actions);	
	// sptr_accumulation_state = ACCUMULATION;

	if(WN_operator(kid1) == OPR_TAS) {
	  dst_idx =  WN_ty(kid1);
	  WN_kid1(tree) = Strip_TAS(WN_kid1(tree));
	} else 
	  dst_idx = WN_ty(tree);
	
	//fprintf(stderr, "Store after lowering : kid0\n");
 	//fdump_tree(stderr,WN_kid0(tree));
  	//fprintf(stderr, "Store after lowering : kid1\n");
 	//fdump_tree(stderr,WN_kid1(tree));	

	if (do_arith) {
	  ist = Combine_Offset_Terms(*term_stack);
	  if (WN_operator(WN_kid1(tree)) == OPR_LDID) {
	    src_idx = ST_type(WN_st(WN_kid1(tree)));
	    is_field = WN_field_id(tree)-1;
	  } else if (WN_operator(WN_kid1(tree)) == OPR_COMMA) {
	    Is_True(src_idx, ("",""));
	  } else 
	    Is_True(src_idx,("",""));
	    
	  UINT esize = Get_Type_Inner_Size(idx);
	  //for accesses to fields of shared structs, dst_idx = shared [] char
	  // and the ptr arithmetic needs to have the elem size 1;
	  if(dst_idx && Type_Is_Shared_Ptr(dst_idx, TRUE) && 
	     TY_size(TY_pointed(dst_idx)) == 1 && Get_Type_Block_Size(dst_idx) == 0) {
	    Is_True(Get_Type_Block_Size(idx) == 0, ("",""));
	    esize = 1;
	  } //how the heck can I tell the difference between shared struct { shared int *p ..}
	    // and shared struct { int *p ..}
	  if(TY_kind(idx) == KIND_POINTER && TY_is_shared(TY_pointed(idx)) && 
	     TY_kind(TY_pointed(idx)) == KIND_POINTER)
	    esize = TY_size(TY_To_Sptr_Idx(idx));
	  /* dst_idx and src_idx are reversed here 
	     dst_idx = rhs_type
	     src_idx = lhs_type */
	
	  if(ist  
	     && 
	     (TY_kind(src_idx) == KIND_POINTER ||  TY_kind(src_idx) == KIND_ARRAY)
	     &&
	     (dst_idx == 0 || src_idx == 0 || Types_Are_Equiv(TY_pointed(dst_idx), src_idx) ) ) {
	    WN_kid1(tree)  = WN_Create_Shared_Ptr_Arithmetic (WN_kid1(tree), ist, OPR_ADD,
							      esize,
							      Get_Type_Block_Size(idx),
							      Get_Type_Block_Size(idx) <= 1);
	    
	    if(offt) {
	      ist = offt;
	    } else 
	      do_arith = FALSE;
	    
	  } else if(offt)
	    ist = offt;
	} 

	sptr_accumulation_state = saved_acc_state;
        sptr_off_accumulation_stack.pop();
        CXX_DELETE(term_stack, Malloc_Mem_Pool);
	
	if(!ist) {
	  ist = offt;
	  do_arith = offt ? TRUE : FALSE;
	}
	ist = WN_Create_Shared_Store (tree, FALSE, 0, do_arith /*True*/, ist);
	WN_Delete(tree);
	return ist;
      } else if (WN_Type_Is_Shared_Ptr(WN_kid0(tree)) || WN_operator(WN_kid0(tree)) == OPR_TAS)
	{
	  
	  WN *kid0 = lower_expr(block, WN_kid0(tree), actions);
	  if (TY_kind(WN_object_ty(WN_kid0(tree))) == KIND_SCALAR)
	    WN_kid0(tree) = kid0;
	  else {
	    if(OPERATOR_is_load(WN_operator(WN_kid0(tree))) && WN_field_id(WN_kid0(tree)))
	      WN_kid0(tree) = kid0;
	    else {
	      idx = WN_field_id(tree) ?  
		get_field_type(TY_pointed(WN_ty(tree)), WN_field_id(tree)) :
		WN_ty(tree) ;
	      if(!Type_Is_Shared_Ptr(idx)) {
		//used to be that we performed the casts to local here - 
		// lowering TAS should take care of it
		WN_kid0(tree) = kid0;
	      } else  if(WN_field_id(tree) && !Type_Is_Shared_Ptr(WN_ty(tree)) &&  
		       Type_Is_Shared_Ptr(idx, TRUE)){
		//This is the case where we store into a field of a local struct, the type of
		//of the field being a sptr

		//According to my current understanding of this code, idx holds the type of the
		//rsh.  WN_ty(tree) is a struct type. The type of the lhs is given 
		// by the field_id. 
		dst_idx = get_field_type(TY_pointed(WN_ty(tree)), WN_field_id(tree));
		//dst_idx = TY_To_Sptr_Idx(idx);
		INTRINSIC iop = INTRINSIC_LAST;
		if(( Type_Is_Shared_Ptr(WN_ty(kid0)) &&
		     Need_StoP_Cvt(WN_ty(kid0), idx, &iop) ) ||
		   Need_StoP_Cvt(WN_ty(WN_kid0(tree)), idx, &iop))
		  kid0 = WN_Create_StoP_Cvt(Strip_TAS(kid0), iop);
		else 
		  WN_kid0(tree) = kid0;
		
		if(TY_mtype(dst_idx) == MTYPE_M) {
		  WN_set_rtype(tree, MTYPE_V);
		  WN_set_desc(tree, MTYPE_M);
		  WN_kid0(tree) = kid0;
		}	     
	      } 
	      
	    }
	  }
	  WN_kid1(tree) = lower_expr(block, WN_kid1(tree), actions);
	  if(WN_field_id(tree) > 0) {
	    WN_store_offset(tree) = Adjust_Field_Offset(TY_pointed(WN_ty(tree)),
							//WN_field_id(tree),
							WN_store_offset(tree));
	  } 
	  return tree;
	}  else if(WN_field_id(tree) > 1) {
	  TY_IDX pty = TY_pointed(WN_ty(tree));
	  WN_store_offset(tree) = Adjust_Field_Offset(pty,
						      WN_store_offset(tree));
	} else if(Type_Is_Shared_Ptr(TY_pointed(idx)) &&
		  WN_operator(WN_kid0(tree)) == OPR_INTCONST) {
	  /* setting a shared pointer to null through indirection */
	  Is_True(WN_const_val(WN_kid0(tree)) == 0, ("Setting a shared ptr to a constant different from NULL",""));
	  WN_kid1(tree) = lower_expr(block, WN_kid1(tree), actions);
	  return WN_SetNull_Sptr(tree);
	}
    }
							 /* added by parkcs - instrument indirect store operations */
							 if (Action(LOWER_UPC_PRIVATE_MEMACC)) {
								 //fprintf(stderr, "lower_stmt(ISTORE): case Action==UPC_PRIVATE_MEMACC reached.\n");
								 WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
								 WN_kid1(tree) = lower_expr(block, WN_kid1(tree), actions);

								 WN* parm[2];
								 WN* newblk = WN_CreateBlock();
								 TYPE_ID desc = WN_desc(WN_kid0(tree));
								 TYPE_ID rtype = WN_rtype(WN_kid0(tree));

								 TY_IDX ty = TY_pointed(WN_ty(tree));
								 UINT field_id = WN_field_id(tree);
								 if(field_id > 0) {
								  	 //fprintf(stderr, "Stucture type, field_id = %d\n", field_id);
									 ty = Get_Field_Type(ty, field_id);
								 }

								 ST* st = Gen_Temp_Symbol(ty, (const char*) ".Mprvstval.");

								 WN_INSERT_BlockLast(newblk, WN_Stid(desc, 0, st, ty, WN_kid0(tree), 0));
								 //parm[0] = WN_CreateParm(Pointer_type, WN_CopyNode(WN_kid1(tree)), 
								 parm[0] = WN_CreateParm(Pointer_type, 
												WN_Add(Pointer_type, WN_kid1(tree),
														WN_Intconst(Integer_type, WN_store_offset(tree))),
												MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
								 parm[1] = WN_CreateParm(Integer_type, 
												//WN_Intconst(Integer_type, MTYPE_byte_size(rtype)), 
												WN_Intconst(Integer_type, TY_size(ty)), 
												MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
								 WN_INSERT_BlockLast(newblk, WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, 
														 INTRN_UPCT_BEFORE_LWRITE, 2, parm));
								 WN_kid0(tree) = WN_Ldid(desc, 0, st, ty, 0);
								 WN_INSERT_BlockLast(newblk, tree);
								 tree = newblk;
 
								 //fprintf(stderr, "INSTRUMENT ISTORE\n");
								 //fdump_tree(stderr, tree);
								 kids_lowered = TRUE;
								 return tree;
							 }
							 /* end parkcs */
    break;
  } 
  case OPR_STID:
    {
      PREG_NUM last_preg = Get_Preg_Num (PREG_Table_Size(CURRENT_SYMTAB));

      if ((WN_class(tree) == CLASS_PREG) &&
	  (WN_store_offset(tree) > last_preg))
      {
	  DevWarn("lower_store() pregno %d > SYMTAB_last_preg(%d)",
		  WN_load_offset(tree), last_preg);
      }
    }

    if (WN_StoreIsUnused(tree))
    {
      WN	*eval;

      eval = lower_eval(block, WN_CreateEval(WN_kid0(tree)), actions);

      WN_Delete(tree);

      return eval;
    }

    if (Action(LOWER_MLDID_MSTID) && WN_desc(tree) == MTYPE_M)
      return lower_mstid(block, tree, actions);

    if (Action(LOWER_MLDID_MSTID) && Action(LOWER_RETURN_VAL) && WN_Type_Is_Shared_Ptr(tree) && 
	TY_mtype(TY_To_Sptr_Idx(WN_ty(tree))) == MTYPE_M && 
	WN_operator(WN_kid0(tree)) == OPR_LDID && WN_st(WN_kid0(tree)) == Return_Val_Preg) 
      return lower_mstid(block, tree, actions);
    
    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator (tree) == OPR_STBITS)
	return lower_store_bits (block, tree, actions);
    }

    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      WN	*realexp, *imagexp;
      TYPE_ID	realTY;

      realTY =	Mtype_complex_to_real( WN_desc(tree));
      lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);

     /*
      * create the real/imaginary stores
      * load the temporary values into a preg before the store (pv314583)
      * as the store may interfere with the expression.
      */
      {
        WN_OFFSET	offset = WN_store_offset(tree);
        PREG_NUM	realexpN, imagexpN;
        WN		*wn;
	WN *realexp_copy,*imagexp_copy;

	if (WN_operator(realexp) == OPR_CONST) {
	   realexp_copy = realexp;
	} else {
	   realexpN = AssignExpr(block, realexp, realTY);
	   realexp_copy = WN_LdidPreg(realTY, realexpN);
	}

	if (WN_operator(imagexp) == OPR_CONST) {
	   imagexp_copy = imagexp;
	} else {
	   imagexpN = AssignExpr(block, imagexp, realTY);
	   imagexp_copy = WN_LdidPreg(realTY, imagexpN);
	}

	wn = WN_Stid(realTY,
		     offset, 
		     coerceST(WN_st(tree), realTY),
		     MTYPE_To_TY(realTY),
		     realexp_copy);

        realexp = lower_store(block, wn, actions);
        WN_Set_Linenum (realexp, WN_Get_Linenum(tree));
        WN_INSERT_BlockLast(block, realexp);

	wn = WN_Stid(realTY,
		     coerceOFFSET(tree, realTY, offset),
		     coerceST(WN_st(tree), realTY),
		     MTYPE_To_TY(realTY),
		     imagexp_copy);

        imagexp = lower_store(block, wn, actions);

        lower_complex_maps(tree, realexp, imagexp, actions);

        WN_Delete(tree);

        return imagexp;
      }
    }

    else if (Action(LOWER_QUAD) && MTYPE_is_quad(WN_desc(tree)))
    {
      WN_OFFSET	offset = WN_store_offset(tree);
      WN	*hiexp, *loexp, *wn, *hipart, *lopart;
      TYPE_ID	realTY = MTYPE_F8;

      lower_quad_expr(block, WN_kid0(tree), actions, &hiexp, &loexp);

      /*
       * create the hi/lo stores
       */
      wn = WN_Stid(realTY, 
		   offset, 
		   coerceST(WN_st(tree), realTY),
		   MTYPE_To_TY(realTY), hiexp);

      hipart = lower_store(block, wn, actions);
      WN_Set_Linenum (hipart, WN_Get_Linenum(tree));
      WN_INSERT_BlockLast(block, hipart);

      wn = WN_Stid(realTY,
		   coerceOFFSET(tree, realTY, offset),
		   coerceST(WN_st(tree), realTY),
		   MTYPE_To_TY(realTY), loexp);
      lopart = lower_store(block, wn, actions);

      lower_quad_maps(tree, hipart, lopart, actions);

      WN_Delete(tree);

      return lopart;
    }

    if (Action(LOWER_SPLIT_SYM_ADDRS))
    {
     /*
      * Convert   (STID (offset) <sym>) into
      *       LDA (0) <base> 
      *     ISTORE (offs+ofst)          
      */
      WN  *istore;

      istore =	lower_split_sym_addrs(tree, WN_store_offset(tree), actions);
      if (istore)
      {
	return lower_store(block, istore, actions);
      }
    }

    if ( Action(LOWER_FORMAL_REF) && WN_class(tree) == CLASS_VAR)
    {
      WN  *istore;

      istore =  lower_formal_ref(tree, WN_store_offset(tree),
				 WN_st(tree), actions);
      if (istore)
      {
	return lower_store(block, istore, actions);
      }
    }

    if ( Action(LOWER_UPLEVEL))
    {
     ST *sym = WN_st(tree);

      if (ST_is_uplevelTemp(sym))
      {
        WN	   *istore;

        istore = lower_uplevel_reference(tree, WN_store_offset(tree), actions);
	return lower_store(block, istore, actions);
      }
    }
    
    if(Action(LOWER_UPC_TO_INTR)){
      
      WN* rhs = WN_kid0(tree);
      //the special case for split-phase optimization
      //For lv = v (v is a shared load, lv a local temp var),
      //we generate the init_call for the fetch of v
      //the corresponding sync call is to be generated later at each use of lv
      sync_handle_t * handle = (sync_handle_t*) WN_MAP_Get(upc_comm_map, rhs);       
      if (handle) {
	//fprintf(stderr, "found the init call\n");
	//we only want the init call here, not the sync
	WN* next_node = WN_next(tree);
	rhs = lower_expr(block, rhs, actions);
	FmtAssert(WN_operator(rhs) == OPR_COMMA, ("rhs should be a COMMA node"));
	ST* handle_st = Gen_Temp_Symbol(WN_ty(WN_kid1(rhs)), (char*) ".Msync");
	tree = WN_Stid(WN_rtype(rhs), 0, handle_st, ST_type(handle_st), rhs);
	handle->st = handle_st;

	if (run_ptr_coalescing) {
	  if (Get_Trace(TP_WOPT2, 1)) {
	    fprintf(TFile, "======Performing fine-grained coalescing=============\n");
	  }
	  do_coalescing(block, tree, next_node, handle);
	}
	return tree;
      }

      INTRINSIC iop = INTRINSIC_LAST;
      TY_IDX src_idx = 0, dst_idx = 0;
      if( TY_is_shared(ST_type(WN_st(tree)))) {
	if(WN_Has_Valid_Type(WN_kid0(tree))) 
	  src_idx = WN_Get_Ref_TY(WN_kid0(tree));
	dst_idx = WN_Get_Ref_TY(tree);

	WN_kid0(tree)  = lower_expr(block, WN_kid0(tree), actions);
	if(Need_StoP_Cvt(src_idx, dst_idx,&iop))
	  WN_kid0(tree) = WN_Create_StoP_Cvt(lower_expr(block, Strip_TAS(WN_kid0(tree)),actions), iop); 
	if(WN_operator(WN_kid0(tree)) == OPR_COMMA)
	  WN_kid0(tree) = Spill_Shared_Load(WN_kid0(tree));
	
	WN *ical = WN_Create_Shared_Store(tree, FALSE);
	WN_Delete(tree);
	kids_lowered = TRUE;
	return ical;
      } else if (TY_kind(WN_ty(tree)) == KIND_POINTER && TY_is_shared(TY_pointed(WN_ty(tree))) ||
		 TY_kind(WN_ty(tree)) == KIND_ARRAY && TY_is_shared(TY_etype(WN_ty(tree))) ) {
	//WEI: set to correct type
	TY_IDX base_idx = TY_kind(WN_ty(tree)) == KIND_POINTER ? TY_pointed(WN_ty(tree)) : TY_etype(WN_ty(tree));
	TY_IDX real_ty = TY_is_pshared(base_idx) ? pshared_ptr_idx : shared_ptr_idx;
	
	WN *kid0 = WN_kid0(tree);
	if(WN_offset(tree)) {
	  //this is the case where we have a store into an array with a constant index
	  // happens for static initializers
	  WN_offset(tree) = (WN_offset(tree) / Pointer_Size) * TY_size(real_ty) ;
	}
	//check here for the case where NULL is assigned to a shared pointer.
	if(TY_kind(WN_ty(tree)) == KIND_POINTER && 
	   ((WN_operator(kid0) == OPR_TAS &&
	   WN_operator(WN_kid0(kid0)) == OPR_INTCONST) || 
	   (WN_operator(kid0) == OPR_INTCONST && WN_const_val(kid0) == 0))) {
	  WN_kid0(tree) = kid0 = Strip_TAS(kid0);
	  Is_True(WN_const_val(kid0) == 0, ("Setting a shared ptr to a constant different from NULL",""));
	  return WN_SetNull_Sptr(tree);
	} else if(Type_Is_Shared_Ptr(WN_ty(kid0)) && Need_StoP_Cvt(WN_Get_Ref_TY(kid0), WN_ty(tree), &iop)) {
	  kids_lowered = TRUE;
	  //WEI: strip of the TAS since they're handled by StoP_Cvt
	  WN_kid0(tree)  = WN_Create_StoP_Cvt(lower_expr(block, Strip_TAS(kid0),actions), iop);
	} 

	WN_set_ty(tree, real_ty);
	WN_set_desc (tree, TY_mtype(real_ty));
	
      } else if (WN_Type_Is_Shared_Ptr(WN_kid0(tree)) || WN_operator(WN_kid0(tree)) == OPR_TAS)
	{
	  TY_IDX src_idx = 0;
	  WN *kid0 = WN_kid0(tree);
	  BOOL kid_tas = (WN_operator(kid0) == OPR_TAS);

	  if(WN_operator(kid0) == OPR_TAS || OPERATOR_is_load(WN_operator(kid0)) ||
	     WN_operator(kid0) == OPR_LDA) {
	    src_idx = WN_ty(kid0);
	    if(WN_operator(kid0) == OPR_LDID && WN_field_id(kid0)) {
	      src_idx = Get_Field_Type(src_idx, WN_field_id(kid0));
	    }
	    if(WN_operator(kid0) == OPR_MLOAD && TY_kind(TY_pointed(src_idx)) == KIND_ARRAY)
	      src_idx = Get_Inner_Array_Type(TY_pointed(src_idx));
	  }
	  kid0 = lower_expr(block,
			    //bug 498: for return_val stripping the TAS prevents the
			    // StoP cast from happening
			    // WN_operator(WN_kid0(tree)) == OPR_TAS ? WN_kid0(WN_kid0(tree)) :
			    WN_kid0(tree),
			    actions);
	  if (TY_kind(WN_object_ty(WN_kid0(tree))) == KIND_SCALAR)
	    WN_kid0(tree) = kid0;
	  else {
	    if(OPERATOR_is_load(WN_operator(WN_kid0(tree))) && WN_field_id(WN_kid0(tree)))
	      WN_kid0(tree) = kid0;
	    else {
	      TY_IDX  idx = WN_field_id(tree) ?  
		get_field_type(WN_ty(tree), WN_field_id(tree)) :
		WN_ty(tree) ;
	      if(!Type_Is_Shared_Ptr(idx)) {
		if(src_idx && !TY_is_shared(src_idx) && 
		   TY_kind(src_idx) != KIND_SCALAR && TY_kind(src_idx) != KIND_STRUCT) {
		  if(WN_class(tree) == CLASS_PREG) 
		    WN_kid0(tree) = kid0;
		  else if(!kid_tas)
		    WN_kid0(tree) = WN_Convert_Shared_To_Local(kid0, idx, 0);
		  else { 
		    WN_kid0(tree) = kid0;
		    if(WN_rtype(kid0) != WN_desc(tree))
		       WN_set_desc(tree, WN_rtype(kid0));
		  }
		} else
		  WN_kid0(tree) = kid0;
	      }
	      else  if(WN_field_id(tree) && !Type_Is_Shared_Ptr(WN_ty(tree)) &&  
		       Type_Is_Shared_Ptr(idx, TRUE)){
		//this is the case where we store into a field of a local struct, the type of
		//of the field being a sptr
		// 	Ty_Table[WN_ty(tree)].Print(stderr);
		// 	Ty_Table[WN_load_addr_ty(tree)].Print(stderr);
		TY_IDX dst_idx = TY_To_Sptr_Idx(idx);
		WN_kid0(tree) = kid0;
		if(TY_mtype(dst_idx) == MTYPE_M) {
		  WN_set_rtype(tree, MTYPE_V);
		  WN_set_desc(tree, MTYPE_M);
		}	     
	      }
	    } 
	  }
	  if(WN_store_offset(tree) && ST_class(WN_st(tree)) != CLASS_PREG) 
	    WN_store_offset(tree) = Adjust_Field_Offset(WN_ty(tree), WN_store_offset(tree));
	  return tree;
	} else if(WN_field_id(tree) > 1) {
	  WN_store_offset(tree) = Adjust_Field_Offset(WN_ty(tree), WN_store_offset(tree));
	} else if (WN_operator(WN_kid0(tree)) == OPR_ADD) {
	  //WEI: We may need to fix up the offset 
	  WN* offset = WN_kid1(WN_kid0(tree));
	} 
      
    }
    
    break; // OPR_STID

  case OPR_ISTBITS:
  case OPR_STBITS:
    if (Action(LOWER_BITS_OP))
      return lower_store_bits (block, tree, actions);
    break;

  case OPR_MSTORE:
    {
      WN *rhs = WN_kid0(tree);
      Is_True(!(WN_opcode(rhs) == OPC_MMLDID && WN_st(rhs) == Return_Val_Preg),
	      ("MMLDID of Return_Val_Preg cannot be rhs of MSTORE"));
    }
    
    if (Action(LOWER_UPC_TO_INTR)) {
      if((WN_ty(WN_kid1(tree)) && TY_is_shared(TY_pointed(WN_ty(WN_kid1(tree))))) ||  
	 (WN_ty(WN_kid0(tree)) && TY_is_shared(TY_pointed(WN_ty(WN_kid0(tree))))) ) {
	BOOL src_shared = FALSE;
	BOOL spill = TRUE;
	if (TY_kind(WN_ty(WN_kid0(tree))) == KIND_POINTER && 
	    TY_is_shared(TY_pointed(WN_ty(WN_kid0(tree)))))
	  src_shared = TRUE;
	WN_OFFSET ld_xtra_oft = 0;
	WN_OFFSET st_xtra_oft = 0;
	TY_IDX dest_ty = 0;
	TY_IDX orig_ty = WN_ty(WN_kid1(tree));
	//short circuit structure copies
	// s1 = s2;
	if(WN_operator(WN_kid0(tree)) == OPR_MLOAD ||
	   WN_operator(WN_kid0(tree)) == OPR_ILOAD)
	  spill = FALSE;
	
	if (WN_operator(WN_kid0(tree)) == OPR_MLOAD ) {
	  WN *kid0 = WN_kid0(tree);
	  dest_ty = TY_pointed(WN_ty(kid0));
	  
	  if(OPCODE_is_load(WN_opcode(kid0))) {
	    ld_xtra_oft = Adjust_Field_Offset(dest_ty, WN_offset(kid0));
	  }
	  /* I do not remember the motivation for following the children twice
	     kid0(kid0()). For MLOAD(ARRAY) this causes bug 1008. */
	  if(WN_operator(WN_kid0(WN_kid0(tree))) == OPR_ARRAY)
	    WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	  else
	    WN_kid0(tree) = lower_expr(block, WN_kid0(WN_kid0(tree)), actions);
	} else
	  WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
	
	
	st_xtra_oft = Adjust_Field_Offset(TY_pointed(WN_ty(tree)),  WN_offset(tree));
	WN_kid1(tree) = lower_expr(block, WN_kid1(tree), actions);
	WN_kid2(tree) = lower_expr(block, WN_kid2(tree), actions);
	if (src_shared && !Type_Is_Shared_Ptr(orig_ty)) {
	  //non-shared = shared;
	  TY_IDX access_idx = 0;
	  if(!WN_Has_Valid_Type(WN_kid1(tree))) {
	    access_idx =  WN_Get_Ref_TY(tree); 
	  }
	  return WN_Create_Shared_Load(WN_kid0(tree), WN_kid1(tree), 
				       ld_xtra_oft, access_idx, 0, 0);
	} else  if (!src_shared) {
	  return WN_Create_Shared_Store(tree, src_shared, st_xtra_oft, FALSE, 0, spill);
	} else {
	  // shared struct = shared struct
	  if(TY_kind(dest_ty) == KIND_STRUCT) {
	    dest_ty = Shared_To_Private_Type(dest_ty);
	  }

	  ST *spill_st = Gen_Temp_Symbol(dest_ty, (char*) ".Mspill");
	  WN *dest = WN_Lda(Pointer_Mtype, 0, spill_st, 0);
	  dest  = WN_Create_Shared_Load(WN_kid0(tree), dest, 0, dest_ty);
	  WN *wn0 = WN_CreateBlock();
	  WN_INSERT_BlockLast(wn0, dest);
	  wn0 = WN_CreateComma(OPR_COMMA, MTYPE_M, MTYPE_V, wn0, 
			       WN_Ldid(MTYPE_M, 0, spill_st, ST_type(spill_st)));
	  WN_kid0(tree) = wn0;
	  return WN_Create_Shared_Store(tree, FALSE, st_xtra_oft);
					  
	}
      } else {
	TY_IDX ty = TY_pointed(WN_ty(tree));
	if(WN_offset(tree) && TY_kind(ty) == KIND_STRUCT)
	  WN_offset(tree) = Adjust_Field_Offset(ty,  WN_offset(tree));
	
      }
    } else if(Action(LOWER_UPC_MFIELD)) {
      TY_IDX idx = TY_pointed(WN_ty(tree));
      TY_IDX fld_idx = WN_field_id(tree) ? Get_Field_Type(idx, WN_field_id(tree)) : 0;
      WN *kid = WN_kid2(tree);
      WN_OFFSET offset = WN_offset(tree);
      if(WN_operator(kid) == OPR_INTCONST) {
	
	//for struct initializers that require padding the constant value holds the
	// number of bytes to the end of the struct (or array of structs);
	// see bugzilla/bug567

	if( (WN_const_val(kid) >=  TY_size(idx) &&  
	     WN_const_val(kid) <  Adjusted_Type_Size(idx)) ||
	    (TY_size(idx) == Adjusted_Type_Size(idx) &&
	     WN_const_val(kid) ==  TY_size(idx))) {
	  int elems = WN_const_val(kid) / TY_size(idx);
	  int offt = WN_const_val(kid) % TY_size(idx);
	  WN_const_val(kid) = elems * Adjusted_Type_Size(idx) + Adjust_Field_Offset(idx, offt); 
	} else if ( WN_field_id(tree) == 1 && TY_kind(fld_idx) == KIND_ARRAY) {
	  //bug972 - partial initialization of array
	  //offset is already adjusted and we need to adjust only the size const;
	  WN_const_val(kid) = (WN_const_val(kid) / TY_size(Get_Inner_Array_Type(fld_idx))) *
	    Adjusted_Type_Size(Get_Inner_Array_Type(fld_idx));
	} else {
	  idx = WN_field_id(tree) == 0 ? idx : Get_Field_Type(idx, WN_field_id(tree));
	  WN_const_val(kid) =  Adjusted_Type_Size(idx);
	}
      }   
    }

    if (Align_Object)
    {
      INT16 i;
      for (i = 0; i < WN_kid_count(tree); i++)
        WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);

      tree = improve_Malignment(tree, WN_kid1(tree), WN_kid2(tree),
				WN_store_offset(tree));
    }

    if (Action(LOWER_MSTORE))
    {
      if (WN_StoreIsUnused(WN_kid1(tree)))
      {
	WN	*eval;

	eval = lower_eval(block, WN_CreateEval(WN_kid0(tree)), actions);

	WN_Delete(tree);

	return eval;
      }
     /*
      *  rewrite
      *		MSTORE (MCSELECT (expr, MLOAD, MLOAD)
      *  into
      *		MSTORE (MLOAD (CSELECT expr, MLOAD, MLOAD), size)
      */
      if (WN_opcode(WN_kid0(tree)) == OPC_MCSELECT)
      {
	WN	*select, *expr, *mload0, *mload1, *cselect;

	select = WN_kid0(tree);
	expr = WN_kid0(select);
	mload0 = WN_kid1(select);
	mload1 = WN_kid2(select);

	Is_True(WN_operator_is(mload0, OPR_MLOAD),
		("unexpected MSTORE (MCSELECT) pattern"));
	Is_True(WN_operator_is(mload1, OPR_MLOAD),
		("unexpected MSTORE (MCSELECT) pattern"));

	cselect = WN_Cselect(Pointer_type, expr, WN_kid0(mload0),
			     WN_kid0(mload1));
	WN_kid0(mload0) = cselect;
	WN_kid0(tree) = mload0;
      }
      tree = lower_mstore(block, tree, actions);
      kids_lowered = TRUE;
    }
    
    
    
    break; /* MSTORE */

  }
  
  /* Lower kids if not done already. */
  if (! kids_lowered)
  {
    INT16 i;
    for (i = 0; i < WN_kid_count(tree); i++) {
	if (WN_kid(tree, i))
	    WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
    }
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_eval(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on eval statement <tree>,
 * returning lowered tree.
 *
 * ==================================================================== */

static WN *lower_eval(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN	*child = WN_kid0(tree);

  Is_True(WN_opcode(tree) == OPC_EVAL,
	  ("expected EVAL node, not %s", OPCODE_name(WN_opcode(tree))));

  if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_rtype(child)))
  {
    WN	*realexp, *imagexp, *eval;

    lower_complex_expr(block, child, actions, &realexp, &imagexp);

    realexp = lower_expr(block, realexp, actions);
    eval = WN_CreateEval(realexp);
    WN_INSERT_BlockLast(block, eval);

    child = imagexp;
  }
  else if (Action(LOWER_QUAD) && MTYPE_is_quad(WN_rtype(child)))
  {
    WN	*hi, *lo, *eval;

    lower_quad_expr(block, child, actions, &hi, &lo);

    hi = lower_expr(block, hi, actions);
    eval = WN_CreateEval(hi);
    WN_INSERT_BlockLast(block, eval);

    child = lo;
  }
  else if (Action(LOWER_MSTORE) && WN_operator_is(child, OPR_MLOAD))
  {
    TY_IDX mloadTY = TY_pointed(Ty_Table[WN_ty(child)]);

    if (TY_is_volatile(mloadTY))
    {
      DevWarn("eval of volatile (mload) seen. I hoped to never see this");
    }
    else if (Action(LOWER_TO_CG) && traceWoptFinishedOpt)
    {
      DevWarn("eval of (mload) processed (wopt should have removed this)");
    }
    block = lower_mload(block, child, actions);
    return block;
  }

  {
    WN	*eval;

    child = lower_expr(block, child, actions);
    eval = WN_CreateEval(child);
    WN_Delete(tree);

    return eval;
  }
  
}



static INT32
calculateLoadStore(INT64 size, INT64 offset, TYPE_ID quantum, WN *src)
{
  INT32 n = (size-offset) / MTYPE_RegisterSize(quantum);

  return WN_operator_is(src, OPR_INTCONST) ? n : 2*n;
}

/* ====================================================================
 *
 * static MSTORE_ACTIONS GenerateMstoreAction(WN *size, INT32 offset, 
 * TYPE_ID quantum, WN *expr)
 * 
 * Generate loop/inline or memory intrinsic code base on size and expr
 * The number of moves is relative to the quantum.
 * Acount for ld/st based on whether expr is constant
 *
 * for size nonconstant
 *	generate loop 
 *	we could generate a call BTW, but based on what criteria ? 
 *
 * for size constant
 *	nMoves >= MinStructCopyMemIntrSize
 *		generate intrinsic based on expr
 *		special case for memset as the expr must be a char.
 *		this means the expr must be same in all n bytes
 *	nMoves >= MinStructCopyLoopSize
 *		generate loop code
 *	else generate aggregate moves
 * 

 * ==================================================================== */
static MSTORE_ACTIONS
GenerateMstoreAction(WN *size, INT32 offset, TYPE_ID quantum, WN *expr)
{
  MSTORE_ACTIONS action;

  INT32 nMoves;
  BOOL sizeIsConstant =  WN_operator_is(size, OPR_INTCONST);

  if (sizeIsConstant)
  {
    nMoves = calculateLoadStore(WN_const_val(size), offset, quantum, expr);
  }

  if (MinStructCopyMemIntrSize	&&
     ((sizeIsConstant==FALSE)		||
      (sizeIsConstant==TRUE && MinStructCopyMemIntrSize <= nMoves)))
  {
    if (WN_operator_is(expr, OPR_INTCONST))
    {
      INT64	val=	WN_const_val(expr);

      if (val == 0)
	action = MSTORE_intrinsic_bzero;
      else
      {
	WN *i1con= WN_I1const(WN_rtype(expr), val);

	action = (val == WN_const_val(i1con)) ? MSTORE_intrinsic_memset
	  : MSTORE_loop;
	WN_Delete(i1con);
      }
    }
    else if (WN_operator_is(expr, OPR_MLOAD))
    {
      action = MSTORE_intrinsic_bcopy;
    }
    else
    {
      action = MSTORE_loop;
    }
  }
  else if (sizeIsConstant==TRUE		&&
	   MinStructCopyLoopSize	&&
	   MinStructCopyLoopSize <= nMoves)
  {
      action = MSTORE_loop;
  }
  else
  {
    action = (sizeIsConstant) ? MSTORE_aggregate : MSTORE_loop;
  }

  if (traceMload)
  {
    if (sizeIsConstant) {
      DevWarn("GenerateMstoreAction: %s : line %d: quantum %d, "
	      "size %lld, nMoves %d",
	      MSTORE_ACTIONS_name(action),
	      Srcpos_To_Line(current_srcpos),
	      MTYPE_alignment(quantum),
	      WN_const_val(size),
	      nMoves);
    } else {
      DevWarn("GenerateMstoreAction: %s : line %d: quantum %d, "
	      "size unknown",
	      MSTORE_ACTIONS_name(action),
	      Srcpos_To_Line(current_srcpos),
	      MTYPE_alignment(quantum));
    }
  }

  return action;
}

// If the size of the mtype (used in load/store) is the same as the size of 
// the struct, return the struct's type, otherwise, return a predefined
// type corresponding to mtype.
static inline TY_IDX
struct_memop_type (TYPE_ID mtype, TY_IDX struct_type)
{
    if (TY_size (struct_type) != MTYPE_byte_size (mtype))
	Set_TY_IDX_index (struct_type, TY_IDX_index(MTYPE_To_TY (mtype)));
    return struct_type;
}


/* ====================================================================
 *
 * Auxillary routine to copy aggregrate
 * 
 *  Copy size bytes, starting at offset
 * 
 *   There are so many formals that you probably deserve an explanation
 *	WN *block
 *
 *	TY_IDX srcAlign
 *	TY_IDX dstAlign
 *		alignment restrictions of the generated load/stores as cg
 *		needs to know whether to generate unaligned 
 *	INT32 offset
 *		start copying at (byte) offset
 *	INT32 size (or WN *)
 *		struct (byte) size 
 *		for loop code , this is a size expression
 *	TYPE_ID quantum
 *		unit to load/store
 *	ST *preg
 *	PREG_NUM srcPreg
 *	PREG_NUM dstPreg
 *		if srcAlign is NULL
 *		    srcPreg contains an expression (probably zero)
 *		else
 *		    srcPreg contains the address of the src to copy
 *		dstPreg contains the address of the dst to copy
 *		preg should be Int_Preg.
 *	WN *origLoad
 *	WN *origStore
 *		original load/store nodes (or NULL) needed to preserve alias
 *		maps.
 *
 * ==================================================================== */

static void
copy_aggregate(WN *block, TY_IDX srcAlign, TY_IDX dstAlign, INT32 offset,
	       INT32 size, TYPE_ID quantum, ST *preg, PREG_NUM srcPreg,
	       PREG_NUM dstPreg, WN *origLoad, WN *origStore,
	       INT32 copy_alignment, LOWER_ACTIONS actions)
{
  INT32	stride = MTYPE_RegisterSize(quantum);
  INT32	nMoves = size / stride;

  if (size <= 0)
    return;

  if (nMoves>0)
  {
   /*
    *  generate  unrolled load/store
    */
    while(nMoves--)
    {
     /*
      *  semantics are similar to the following:
      *      (quantum) *(dst + offset) =   (quantum) *(src + offset);
      *		or
      *      (quantum) *(dst + offset) =   srcPreg;
      */
      WN 	*value, *addr, *store;

      if (srcAlign)
      {
	value = WN_IloadLdid(quantum, offset,
			     struct_memop_type (quantum, srcAlign),
			     preg, srcPreg);  

	lower_copy_maps(origLoad, value, actions);
      }
      else
      {
	value = WN_LdidPreg(quantum, srcPreg);
      }

      addr = WN_LdidPreg(Pointer_type, dstPreg);

      store = WN_Istore(quantum,
			offset,
			Make_Pointer_Type (struct_memop_type (quantum,
							      dstAlign)),
			addr,
			value);
      lower_copy_maps(origStore, store, actions);

      WN_INSERT_BlockLast(block, store);

      offset  += stride;
      size -= stride;
    }
  }
  if (size > 0)
  {
   /*
    *  If there is a residue we must recompute a new quantum
    *  and generate a copy for that.
    */
    quantum = compute_next_copy_quantum(quantum, copy_alignment);

    copy_aggregate(block,
		   srcAlign, dstAlign,
		   offset,
		   size,
		   quantum,
		   preg,
		   srcPreg, dstPreg,
		   origLoad, origStore,
		   copy_alignment,
		   actions);
  }
}




static void
copy_element_and_increment(WN *block, TY_IDX srcAlign, TY_IDX dstAlign,
			   PREG_NUM offsetN, TYPE_ID quantum, PREG_NUM srcPreg,
			   PREG_NUM dstPreg, WN *origLoad, WN *origStore,
			   LOWER_ACTIONS actions)
{
 /*
  *   (quantum) *(dst + offset) =   (quantum) *(src + offset);
  *		or
  *   (quantum) *(dst + offset) =   srcPreg;
  *    		
  *  and increment offset
  *
  *		offset		+=  stride;
  */
  INT32  stride = MTYPE_RegisterSize(quantum);
  WN 	*value, *addr, *store, *add, *inc;
  ST	*intPreg = MTYPE_To_PREG(Integer_type);

  if (srcAlign)
  {
    addr = WN_Add(Pointer_type,
		  WN_LdidPreg(Pointer_type, srcPreg),
		  WN_LdidPreg(Integer_type, offsetN));
    value = WN_CreateIload (OPR_ILOAD, Mtype_comparison(quantum), quantum,
			    0, struct_memop_type (quantum, srcAlign),
			    Make_Pointer_Type (srcAlign), addr);

    lower_copy_maps(origLoad, value, actions);
  }
  else
  {
    value = WN_LdidPreg(quantum, srcPreg);
  }

  addr = WN_Add(Pointer_type,
		WN_LdidPreg(Pointer_type, dstPreg),
		WN_LdidPreg(Integer_type, offsetN));
  store = WN_Istore(quantum,
		    0,
		    Make_Pointer_Type(struct_memop_type (quantum, dstAlign)),
		    addr,
		    value);

  lower_copy_maps(origStore, store, actions);

  WN_INSERT_BlockLast(block, store);

  /*
   *  offset += stride
   */

  add  = WN_Add(Integer_type,
 		WN_LdidPreg(Integer_type, offsetN),
  		WN_Intconst(Integer_type, stride));
  inc = WN_StidIntoPreg(Integer_type, offsetN, intPreg, add);
  WN_INSERT_BlockLast(block, inc);
}




/* ====================================================================
 *
 * Auxillary routine to copy aggregrate loop 
 *
 *	The size must be an integer constant
 *
 * ==================================================================== */
static void
copy_aggregate_loop_const(WN *block, TY_IDX srcAlign, TY_IDX dstAlign,
			  INT32 offset, INT32 size, TYPE_ID quantum,
			  PREG_NUM srcPreg, PREG_NUM dstPreg, WN *origLoad,
			  WN *origStore, INT32 copy_alignment,
			  LOWER_ACTIONS actions)
{
 /*
  *  generate the following
  *    n = nMoves;
  *    index = 0;
  *    do
  *    {
  *	(quantum) *(dst + offset) =   (quantum) *(src + offset);
  *	       n--;
  *    } while(n>0);
  *
  * Try generating a DO loop instead of a WHILE loop
  *
  *	(TBD)	we should really build an array expression 
  *		dst[offset] = src[offset]
  */
  PREG_NUM		offsetN;
  ST		*intPreg = MTYPE_To_PREG(Integer_type);
  INT32		stride   = MTYPE_RegisterSize(quantum);
  INT64		nMoves   = size  / stride;
  INT64		residue  = size - ( nMoves * stride );

  /*
   *  Bail out if there is nothing to move and no residue
   */
  if ((nMoves <= 0) && residue == 0)
    return;

  offsetN = AssignExpr(block, WN_Intconst(Integer_type, offset), Integer_type);

 /*
  *	create loop count variable, traditionally called n
  * 	offset is most likely zero
  */
  if (nMoves > 0)
  {
    PREG_NUM	n;
    WN	 	*body;
    WN          *incr;
    WN          *start;
    WN          *test;
    WN	        *doLoop;

    n = Create_Preg(Integer_type,"mstore_loopcount");
    body= WN_CreateBlock();
    start = WN_StidIntoPreg( Integer_type, n, intPreg,
			     WN_Intconst(Integer_type, nMoves));
    incr  = WN_StidIntoPreg( Integer_type, n, intPreg,
			     WN_Sub(Integer_type,
				    WN_LdidPreg(Integer_type, n),
				    WN_Intconst(Integer_type, 1)));
    test = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, n),
		 WN_Zerocon(Integer_type));
    
   /*
    *   (quantum) *(dst + offset) =   (quantum) *(src + offset);
    *		or
    *   (quantum) *(dst + offset) =   srcPreg;
    *   and increment offset
    *			offset    +=  stride;
    */
    copy_element_and_increment(body,
			       srcAlign, dstAlign,
			       offsetN,
			       quantum,
			       srcPreg, dstPreg,
			       origLoad, origStore,
			       actions);
    doLoop = WN_CreateDO(WN_CreateIdname(n, intPreg),
			 start, test, incr, body, NULL);
    WN_INSERT_BlockLast(block, doLoop);
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_mstore_to_loop( origStore, doLoop, nMoves );
  }

 /*
  *  If there is a residue we must recompute a new quantum
  *  and generate a copy for that.
  *
  *	if (residue > 0)
  *	{
  *	    if (residue >= 4)
  *	    {
  *  		(int) *(dst + offset) =   (int) *(src + offset);
  *		offset  += 4;
  *		residue -= 4;
  *	    }
  *	    if (residue >= 2)
  *	    {
  *  		 (short) *(dst + offset) =   (short) *(src + offset);
  *		offset  += 2;
  *		residue -= 2;
  *	    }
  *	    etc.
  *	}
  */
  if (residue)
  {
    WN	  *residue_block=  WN_CreateBlock();

    while(residue>0)
    {
      quantum = compute_next_copy_quantum(quantum, copy_alignment);

      while (residue >= MTYPE_alignment(quantum))
      {
	copy_element_and_increment(residue_block, srcAlign, dstAlign,
				   offsetN, quantum, srcPreg, dstPreg,
				   origLoad, origStore, actions);
	residue -= MTYPE_alignment(quantum);
      }
    }
    WN_INSERT_BlockLast(block, residue_block);
  }
}




/* ====================================================================
 *
 * Auxillary routine to copy aggregrate loop 
 *
 *	The size must be an expression
 *
 * ==================================================================== */

static void
copy_aggregate_loop_n(WN *block, TY_IDX srcAlign, TY_IDX dstAlign,
		      INT32 offset, WN *size, TYPE_ID quantum,
		      PREG_NUM srcPreg, PREG_NUM dstPreg, WN *origLoad,
		      WN *origStore, INT32 copy_alignment,
		      LOWER_ACTIONS actions)
{
 /*
  *  generate the following
  *    n = nMoves;
  *    index = 0;
  *    while(n>0)
  *    {
  *	(quantum) *(dst + offset) =   (quantum) *(src + offset);
  *	       n--;
  *    }
  *
  *	(TBD)	we should really build an array expression 
  *		dst[offset] = src[offset]
  */
  PREG_NUM	offsetN, nMovesN, residueN;
  ST		*intPreg = MTYPE_To_PREG(Integer_type);
  INT32		stride   = MTYPE_RegisterSize(quantum);

  Is_True((WN_operator(size)!=OPR_INTCONST),("unexpected const"));

 /*
  *  Create the following expressions
  *
  *	nMovesn = ( size ) / stride
  *
  *	residue =   size - ( nMoves * stride )
  */
  {
    WN  *nMoves, *residue;

    nMoves   = WN_Div(Integer_type, WN_COPY_Tree(size),
		      WN_Intconst(Integer_type, stride));

    nMovesN  = AssignExpr(block, nMoves, Integer_type);

    residue  = WN_Sub(Integer_type,
		      WN_COPY_Tree(size),
		      WN_Mpy(Integer_type,
			     WN_LdidPreg(Integer_type, nMovesN),
			     WN_Intconst(Integer_type, stride)));

    residueN = AssignExpr(block, residue, Integer_type);
  }

 /*
  *	create loop count variable, traditionally called n
  * 	offset is most likely zero
  */
#if 0
  n = AssignExpr(block, WN_LdidPreg(Integer_type, nMovesN), Integer_type);

  offsetN = AssignExpr(block, WN_Intconst(Integer_type, offset), Integer_type);

  {
    WN	*body, *sub, *dec, *test, *whileDo;

    body = WN_CreateBlock();
   /*
    *	while(n>0)
    *   {
    *
    *	  (quantum) *(dst + offset) =   (quantum) *(src + offset);
    *		or
    *	  (quantum) *(dst + offset) =   srcPreg;
    *
    *			offset    +=  stride;
    *
    * 			n --;
    *   }
    */
    copy_element_and_increment(body,
			       srcAlign, dstAlign,
			       offsetN, quantum,
			       srcPreg, dstPreg,
			       origLoad, origStore,
			       actions);

    sub  = WN_Sub(Integer_type,
		  WN_LdidPreg(Integer_type, n),
		  WN_Intconst(Integer_type, 1));
    dec  = WN_StidIntoPreg(Integer_type, n, intPreg, sub);
    WN_INSERT_BlockLast(body, dec);


    test = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, n),
		 WN_Zerocon(Integer_type));
  	
    whileDo = WN_CreateWhileDo(test, body);
    // ADD FEEDBACK INFO !!!
    WN_INSERT_BlockLast(block, whileDo);
  }
#else
  {
    PREG_NUM	n;
    WN	 	*body;
    WN          *incr;
    WN          *start;
    WN          *test;
    WN	        *doLoop;

    n = Create_Preg(Integer_type,"mstore_loopcount");
    offsetN = AssignExpr(block, WN_Intconst(Integer_type, offset),
			 Integer_type);

    body= WN_CreateBlock();
    start = WN_StidIntoPreg(Integer_type, n, intPreg,
			    WN_LdidPreg(Integer_type, nMovesN));
    incr  = WN_StidIntoPreg(Integer_type, n, intPreg,
			    WN_Sub(Integer_type,
				   WN_LdidPreg(Integer_type, n),
				   WN_Intconst(Integer_type, 1)));
    test = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, n),
		 WN_Zerocon(Integer_type));
    
   /*
    *   (quantum) *(dst + offset) =   (quantum) *(src + offset);
    *		or
    *   (quantum) *(dst + offset) =   srcPreg;
    *   and increment offset
    *			offset    +=  stride;
    */
    copy_element_and_increment(body,
			       srcAlign, dstAlign,
			       offsetN,
			       quantum,
			       srcPreg, dstPreg,
			       origLoad, origStore,
			       actions);
    doLoop = WN_CreateDO(WN_CreateIdname(n,intPreg),start,test,incr,body,NULL);
    WN_INSERT_BlockLast(block, doLoop);
  }
  
#endif





 /*
  *  If there is a residue we must recompute a new quantum
  *  and generate a copy for that.
  *
  *	if (residue > 0)
  *	{
  *	    if (residue >= 4)
  *	    {
  *  		(int) *(dst + offset) =   (int) *(src + offset);
  *		offset  += 4;
  *		residue -= 4;
  *	    }
  *	    if (residue >= 2)
  *	    {
  *  		 (short) *(dst + offset) =   (short) *(src + offset);
  *		offset  += 2;
  *		residue -= 2;
  *	    }
  *	    etc.
  *	}
  *   We supply an incorrect alignment to compute_next_copy_quantum()
  *   because we cant skip the unaligned halfword 
  */
  {
    WN	*if_then, *unused, *cond, *IF;

    if_then = WN_CreateBlock();
    unused = WN_CreateBlock();	/* will be empty */

    cond = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, residueN),
		 WN_Zerocon(Integer_type));

    IF = WN_CreateIf(cond, if_then, unused);
    // ADD FEEDBACK INFO !!

    quantum = compute_next_copy_quantum(quantum,
					MTYPE_alignment(Max_Uint_Mtype));
  	
    while(MTYPE_alignment(quantum) > 0)
    {
      WN	*if_residue_block, *unused, *test, *IF_residue;


      if_residue_block = WN_CreateBlock();
      unused = WN_CreateBlock();

      test = WN_GE(Integer_type,
		   WN_LdidPreg(Integer_type, residueN),
		   WN_Intconst(Integer_type, MTYPE_alignment(quantum)));

      IF_residue = WN_CreateIf(test, if_residue_block, unused);
      // ADD FEEDBACK INFO !!
      lower_copy_maps(origStore, IF_residue, actions);
	
      copy_element_and_increment(if_residue_block, srcAlign, dstAlign,
				 offsetN, quantum, srcPreg, dstPreg,
				 origLoad, origStore, actions);

     /*
      *  residue -= stride
      */
      {
	WN	*sub, *dec;

	sub  = WN_Sub(Integer_type,
		      WN_LdidPreg(Integer_type, residueN),
		      WN_Intconst(Integer_type, MTYPE_alignment(quantum)));
	dec = WN_StidIntoPreg(Integer_type, residueN, intPreg, sub);
        lower_copy_maps(origStore, dec, actions);
	WN_INSERT_BlockLast(if_residue_block, dec);
      }

      WN_INSERT_BlockLast(if_then, IF_residue);

      quantum = compute_next_copy_quantum(quantum,
					  MTYPE_alignment(Max_Uint_Mtype));
    }
    WN_INSERT_BlockLast(block, IF);
  }
}




/* ====================================================================
 *
 * Auxillary routine to copy aggregrate (loop version)
 *
 *  The size can be an expression or integer constant of any value
 *  so it must be tested
 *
 *  loop_const:	will generate a
 *		n= CONST;	do {} while(n<0);
 *
 *  loop_n:	will generate a
 *		n= size;	while(n<0) {};
 *
 * ==================================================================== */

static void
copy_aggregate_loop(WN *block, TY_IDX srcTY, TY_IDX dstTY, INT32 offset,
		    WN *size, TYPE_ID quantum, ST *preg, PREG_NUM srcPreg,
		    PREG_NUM dstPreg, WN *origLoad, WN *origStore,
		    INT32 copy_alignment, LOWER_ACTIONS actions)
{
  if (WN_operator_is(size, OPR_INTCONST))
  {
    if (WN_const_val(size)>0)
    {
      copy_aggregate_loop_const(block,
				srcTY, dstTY,
				offset,
				WN_const_val(size),
				quantum,
				srcPreg, dstPreg,
				origLoad, origStore,
				copy_alignment,
				actions);
    }
  }
  else
  {
    copy_aggregate_loop_n(block,
			  srcTY, dstTY,
			  offset,
			  size,
			  quantum,
			  srcPreg,
			  dstPreg,
			  origLoad, origStore,
			  copy_alignment,
			  actions);
  }
}

/*
 *  mark LDA as addr taken passed for intrinsic memory routines.
 *  Do not recurse on ILOAD (it would mark the pointer as taken_passed
 *  and not the object)
 */
static void markAddrTakenPassed(WN *expr)
{
  switch(WN_operator(expr))
  {
  case OPR_LDA:
    {
      ST *sym = WN_st(expr);
      // if (ST_class(sym) == CLASS_VAR || ST_class(sym) == CLASS_FUNC) {
      //   Do nothing here. ADDR flags should only grow more
      //   optimistic; they should never become more conservative,
      //   because the program's semantics cannot grow worse as we
      //   compile it.
      // }
    }
    return;
  case OPR_ILOAD:
  case OPR_ILOADX:
    return;
  }

  {
    INT32	i;
    for (i = 0; i < WN_kid_count(expr); i++)
      markAddrTakenPassed(WN_actual(expr,i));
  }
}


/* ====================================================================
 * 
 * Generate a call to an intrinsic -- NOT an intrinsic!
 * (currently from lower_mstore)
 *
 * ==================================================================== */

inline WN *WN_Generate_Intrinsic_Call(WN *block, INTRINSIC id, 
				      INT32 nparm, WN *parm[])
{
  WN *call = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, id, nparm, parm);
  return intrinsic_runtime(block, call);
}


static void copy_aggregate_bzero(WN *block, TY_IDX dstTY, WN *dst, WN *size)
{
  WN  *call, *parms[2];

  markAddrTakenPassed(dst);

  parms[0]=  WN_CreateParm(Pointer_type,
			   dst,
			   dstTY,
			   WN_PARM_BY_VALUE);

  parms[1]=  WN_CreateParm(WN_rtype(size),
			   WN_COPY_Tree(size),
			   MTYPE_To_TY(WN_rtype(size)),
			   WN_PARM_BY_VALUE);

  call=	WN_Generate_Intrinsic_Call(block, INTRN_BZERO, 2, parms);

  WN_INSERT_BlockLast(block, call);
}

static void copy_aggregate_memset(WN *block, TY_IDX dstTY, WN *src, WN *dst,
				  WN *size)
{
  WN  *call, *parms[3];

  markAddrTakenPassed(dst);
  parms[0]=  WN_CreateParm(Pointer_type,
			   dst,
			   dstTY,
			   WN_PARM_BY_VALUE);

  parms[1]=  WN_CreateParm(WN_rtype(src),
			   src,
			   MTYPE_To_TY(WN_rtype(src)),
			   WN_PARM_BY_VALUE);

  parms[2]=  WN_CreateParm(WN_rtype(size),
			   WN_COPY_Tree(size),
			   MTYPE_To_TY(WN_rtype(size)),
			   WN_PARM_BY_VALUE);

  call=	WN_Generate_Intrinsic_Call(block, INTRN_MEMSET, 3, parms);

  WN_INSERT_BlockLast(block, call);
}

static void copy_aggregate_bcopy(WN *block, INT32 offset, TY_IDX srcTY,
				 TY_IDX dstTY, WN *src, WN *dst, WN *size)
{
  WN  *call, *parms[3];

  if (offset)
  {
    src = WN_Add(Pointer_type, src, WN_Intconst(Pointer_type, offset));
    dst = WN_Add(Pointer_type, dst, WN_Intconst(Pointer_type, offset));
  }
  markAddrTakenPassed(src);
  markAddrTakenPassed(dst);

  parms[0]=  WN_CreateParm(WN_rtype(src),
			   src,
			   srcTY ? srcTY : MTYPE_To_TY(WN_rtype(src)),
			   WN_PARM_BY_VALUE);

  parms[1]=  WN_CreateParm(Pointer_type,
			   dst,
			   dstTY ? dstTY : MTYPE_To_TY(WN_rtype(dst)),
			   WN_PARM_BY_VALUE);


  parms[2]=  WN_CreateParm(WN_rtype(size),
			   WN_COPY_Tree(size),
			   MTYPE_To_TY(WN_rtype(size)),
			   WN_PARM_BY_VALUE);

  call=	WN_Generate_Intrinsic_Call(block, INTRN_BCOPY, 3, parms);

  WN_INSERT_BlockLast(block, call);
}



/* ====================================================================
 *
 * These routine compute alignment used by the MSTORE/MLOAD routines
 *
 *	aligned memory access
 *		return minimum required alignment
 *
 *	otherwise
 *		form a ratio based on align vrs unaligned reference.
 *		Each unaligned ld/st requires 2 instructions.
 *		In addition, some processors may support 2 memory operations
 *		per clock, but only one unaligned reference (r8k and alien)
 *
 * When we can use unaligned quantums, do not return halfword
 * quantums as there is no hardware support (pv544367)
 *
 * ==================================================================== */

extern INT32 compute_copy_alignment(TY_IDX src, TY_IDX dst, INT32 offset)
{
  INT32	srcAlign, dstAlign, align;
  INT32 max=	MTYPE_alignment(Max_Uint_Mtype);

  srcAlign= (src) ? TY_align(src) : max;
  dstAlign= (dst) ? TY_align(dst) : max;

  align= MIN(srcAlign, dstAlign);
  align= MIN(align, max);

  Is_True((compute_offset_alignment(offset, align) == align),
	  ("compute_copy_alignment: alignment not consistent with offset"));

  return MIN(align, max);
}


extern TYPE_ID compute_copy_quantum(INT32 alignment)
{
  if (UseAlignedCopyForStructs==FALSE)
  {
    INT32  maxAlign= MTYPE_alignment(Max_Uint_Mtype);
    INT32  ratio = Copy_Quantum_Ratio();
    
    if (alignment*ratio < maxAlign) 
      return Max_Uint_Mtype;
  }

  return Mtype_AlignmentClass(alignment, MTYPE_CLASS_UNSIGNED_INTEGER);
}

static TYPE_ID compute_next_copy_quantum(TYPE_ID quantum, INT32 alignment)
{
  TYPE_ID next = Mtype_prev_alignment(quantum);

  if (UseAlignedCopyForStructs==FALSE)
  {
    if ((MTYPE_alignment(next) > alignment)	&&
        (next == MTYPE_U2 || next == MTYPE_I2))
      next = Mtype_prev_alignment(next);
  }
  return next;
}




/* ====================================================================
 *
 * TY_IDX computeNewAlignmentTY(addr, type, WN_const_val(size), offset);
 *
 * Compute a new quantum (alignment), subject to size restrictions.
 * For the new quantum to apply we must have an ST of the appropriate
 * type/offset to align/pad
 *
 * ==================================================================== */

static TY_IDX computeNewAlignmentTY(WN *addr, TY_IDX type, INT32 size,
				    INT64 offset)
{
  TY_IDX newType;
  INT32	align =	TY_align(type);
  INT32	maxAlign = MTYPE_alignment(Max_Uint_Mtype);

  if ((size <= align) || (align >= maxAlign))
    return TY_IDX_ZERO;

 /*
  *  create offset consistent with the offset of addr and offset
  */
  newType = compute_alignment_type(addr, type, offset);
 
  if (WN_operator_is(addr, OPR_LDA) && ST_class(WN_st(addr)) != CLASS_BLOCK)
  {
    ST	   *sym     = WN_st(addr);
    TY_IDX  symType = compute_alignment_type(addr, ST_type(sym), offset);
   /*
    *  If the objected is defined in the current module but not allocated yet
    *  we are free to redefine the alignment so compute largest possible
    *  consistent alignment. The alignment should be consistent with the
    *  offset of course
    */
    if (ST_pu_defined(sym) && !Is_Allocated(sym))
    {
      INT32  stAlign;

      stAlign = compute_offset_alignment(WN_lda_offset(addr), maxAlign);
      stAlign = compute_offset_alignment(offset, stAlign);

      if (TY_align(symType) < stAlign)
      {
        if (traceAlignment)
        {
          DevWarn("realign ST %s (from %d to %d) line %d", ST_name(sym),
		  TY_align(symType), stAlign, Srcpos_To_Line(current_srcpos));
        }
	// Convert Make_Align_Type once real iterators are implemented
	// to iterate over the TY table.
	symType = Make_Align_Type(symType, stAlign);

        if ((ST_class(sym) == CLASS_CONST)              &&
            (TCON_ty(STC_val(sym)) == MTYPE_STR)        &&
             TY_is_pointer(Ty_Table[ST_type(sym)]))
        {
          symType = Make_Pointer_Type(newType);
        }
        Set_ST_type(sym, symType);

        if (stAlign > align)
        {
          return Make_Align_Type(newType, stAlign);
        }
        return TY_IDX_ZERO;
      }
    }
  }

  if (TY_align(newType) > align)
    return newType;

  return TY_IDX_ZERO;
}




static WN *compute_new_size(WN *tree, WN *size, INT32 align)
{
  if (Align_Padding)
  {
    INT64  sizeN   = WN_const_val(size);
    INT64  residue = sizeN % align;
    INT32  popcnt  = TARG_INT_Pop_Count(residue);

    if (popcnt > 1)
    {
      INT64  newsize = sizeN + (align-residue);

      return WN_Intconst(WN_rtype(size), newsize);
    }
  }
  return size;
}




/* ====================================================================
 *
 * WN *improve_Malignment(WN *tree, WN *addr, WN *size, INT64 offset)
 *
 * Given an mstore or mload try to improve the alignment (if possible)
 *
 *
 * ==================================================================== */
static WN *improve_Malignment(WN *tree, WN *addr, WN *size, INT64 offset)
{
  TY_IDX type, newType;

  if (Align_Object == FALSE)
    return tree;

  if (!WN_operator_is(size, OPR_INTCONST))
  {
    return tree;
  }

  TY &tree_ty = Ty_Table[WN_ty(tree)];

  if (TY_is_f90_pointer(tree_ty)) {
     /* It's not safe to realign F90 pointers */
     return tree;
  }

  type = TY_pointed(tree_ty);

  newType = computeNewAlignmentTY(addr, type, WN_const_val(size), offset);

  if (newType)
  {
    if (traceAlignment)
    {
      DevWarn("realign OPCODE %s (from %d to %d) line %d",
	      OPCODE_name(WN_opcode(tree)), TY_align(type), TY_align(newType),
	      Srcpos_To_Line(current_srcpos));
    }

    WN_set_ty(tree, Make_Pointer_Type(newType));

    switch(WN_operator(tree))
    {
    case OPR_MSTORE:
      WN_kid2(tree)= compute_new_size(tree, size, TY_align(newType));
      break;

    case OPR_MLOAD:
      WN_kid1(tree)= compute_new_size(tree, size, TY_align(newType));
      break;
    }
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_mstore(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on mstore statement <tree>,
 * returning lowered tree.
 *
 * The following control the expansion
 *    
 *    BOOL GenerateLoopCode(INT32 size, TYPE_ID quantum)
 *	  See notes on this function for more information
 *
 *    BOOL UseAlignedCopyForStructs
 *	  TRUE
 *		expand using minimum common alignment 
 *	  FALSE 
 *		expand using maximum integer type
 *
 * ==================================================================== */

static WN *lower_mstore(WN * /*block*/, WN *mstore, LOWER_ACTIONS actions)
{
  WN		*newblock;
  WN		*load, *addr, *size;
  SRCPOS	srcpos = WN_Get_Linenum(mstore);
  CURRENT_STATE	mstate = pushCurrentState(mstore, actions);

  load =	WN_kid0(mstore);
  addr =	WN_kid1(mstore);
  size =	WN_kid2(mstore);

  Is_True((WN_opcode(mstore) == OPC_MSTORE),
	  ("expected MSTORE node, not %s", OPCODE_name(WN_opcode(mstore))));


  if (WN_field_id (mstore) != 0)
      lower_field_id (mstore);

  newblock = WN_CreateBlock();
  {
    TY_IDX  dstTY = TY_pointed(Ty_Table[WN_ty(mstore)]);
    TY_IDX  srcTY = TY_IDX_ZERO;
    WN     *expr = load;

   /*
    *  The "load" may be any expression (ex. constant 0)
    *  If so, there should be no alignment restrictions
    */
    if (WN_opcode(load) == OPC_MLOAD)
    {
      srcTY = TY_pointed(Ty_Table[WN_ty(load)]);
      expr =	WN_kid0(load);
     /*
      *  the mload may have an offset
      */
      if (WN_load_offset(load))
      {
        expr = WN_Add(Pointer_type,
		      expr,
		      WN_Intconst(Pointer_type, WN_load_offset(load)));
	WN_kid0(load)=	expr;
      }
    }
    else
    {
      load =		NULL;
    }

    {
      INT32	copy_alignment;
      PREG_NUM	dstPreg, srcPreg;
      TYPE_ID	quantum;
      ST	*preg=    MTYPE_To_PREG(Pointer_type);

      if (WN_store_offset(mstore))
      {
        addr = WN_Add(Pointer_type,
		      addr,
		      WN_Intconst(Pointer_type, WN_store_offset(mstore)));
      }

      copy_alignment = compute_copy_alignment(srcTY, dstTY,
					      WN_store_offset(mstore));
      quantum =	compute_copy_quantum(copy_alignment);

      switch(GenerateMstoreAction(size, 0, quantum, WN_kid0(mstore)))
      {
      case MSTORE_intrinsic_bzero:
	copy_aggregate_bzero(newblock, WN_ty(mstore), addr, size);
	break;

      case MSTORE_intrinsic_memset:
	copy_aggregate_memset(newblock, WN_ty(mstore), expr, addr, size);
	break;

      case MSTORE_intrinsic_bcopy:
	copy_aggregate_bcopy(newblock, 0, TY_pointer(srcTY), TY_pointer(dstTY),
			     expr, addr, size);
	break;

      case MSTORE_loop:
        srcPreg = AssignExpr(newblock, expr, WN_rtype(expr));
        dstPreg = AssignExpr(newblock, addr, Pointer_type);
	copy_aggregate_loop(newblock,
			    srcTY,
			    dstTY,
			    0,
			    size,
			    quantum,
			    preg,
			    srcPreg,
			    dstPreg,
			    load,
			    mstore,
			    copy_alignment,
			    actions);
	break;

      case MSTORE_aggregate:
        srcPreg = AssignExpr(newblock, expr, WN_rtype(expr));
        dstPreg = AssignExpr(newblock, addr, Pointer_type);
	copy_aggregate(newblock,
		       srcTY,
		       dstTY,
		       0,
      		       WN_const_val(size),
		       quantum,
		       preg,
		       srcPreg,
		       dstPreg,
		       load,
		       mstore,
		       copy_alignment,
		       actions);
	break;
      }
    }
  }

  WN_Delete( WN_kid2(mstore));
  WN_Delete(mstore);

  newblock = lower_block(newblock, actions);

  popCurrentState(mstate);
  return newblock;
}



/* ====================================================================
 *
 * WN *lower_mload(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * This expansion is generated only for eval (mload), where the TY is volatile
 * I hope this never happens in practice
 * With gccfe this can happen when TY is not volatile and we don't optimize.
 *
 * ==================================================================== */

static WN *lower_mload(WN * /*block*/, WN *mload, LOWER_ACTIONS actions)
{
  PREG_NUM  srcPreg;
  TYPE_ID   quantum;
  INT32     size, copy_alignment;
  WN        *newblock;

  Is_True((WN_opcode(mload) == OPC_MLOAD),
	  ("expected MLOAD node, not %s", OPCODE_name(WN_opcode(mload))));
  Is_True((WN_operator_is(WN_kid1(mload), OPR_INTCONST)),
	  ("expected MLOAD size constant  "));

  if (WN_field_id (mload) != 0)
      lower_field_id (mload);

  WN       *addr   = WN_kid0(mload);
  INT32     offset = WN_load_offset(mload);
  TY_IDX    srcTY  = TY_pointed(Ty_Table[WN_ty(mload)]);

  newblock = WN_CreateBlock();

  size = WN_const_val( WN_kid1(mload) );
  copy_alignment = compute_copy_alignment( srcTY, TY_IDX_ZERO, offset );
  quantum = compute_copy_quantum( copy_alignment );

  srcPreg = AssignExpr(newblock, addr, WN_rtype(addr));

  while (size > 0)
  {
    INT32 stride = MTYPE_RegisterSize(quantum);
    INT32 nMoves = size / stride;

   /*
    *  generate  unrolled eval (load)
    */
    while (nMoves > 0)
    {
      WN	*value, *eval;

      // use quantum TY rather than srcTY since want quantum-sized loads.
      value = WN_IloadLdid(quantum, offset, MTYPE_To_TY(quantum),
			   MTYPE_To_PREG(Pointer_type), srcPreg);

      lower_copy_maps(mload, value, actions);

      eval = WN_CreateEval(value);

      WN_INSERT_BlockLast(newblock, eval);

      offset += stride;
      size   -= stride;
      nMoves--;
    }
   /*
    *  If there is a residue we must recompute a new quantum
    *  and generate a copy for that.
    */
    if (size > 0)
    {
      quantum = compute_next_copy_quantum(quantum, copy_alignment);
    }
  }

  WN_Delete( WN_kid1(mload));
  WN_Delete(mload);

  newblock = lower_block(newblock, actions);

  return newblock;
}




/* ====================================================================
 *
 * void lower_complex_actual
 *
 * Perform lowering (see WN_Lower description) for complex actuals
 * This is used in fortran only , and only by value (%val)
 *
 *
 * ==================================================================== */

static void lower_complex_actual (WN *block, WN *val, PLOC ploc,
				  LOWER_ACTIONS actions)
{
  WN     *size, *mload, *addr;
  TY_IDX  complexType = TY_Of_Expr(val);

  size = WN_Intconst(Integer_type, TY_size(Ty_Table[complexType]));

  addr = make_pointer_to_node(block, val);

  mload = WN_CreateMload(0, Make_Pointer_Type(complexType), addr, size);

  lower_mload_actual (block, mload, ploc, actions);
}




/* ====================================================================
 *
 * void lower_mload_actual
 *
 * Perform lowering (see WN_Lower description) for structure actuals
 * the structure will be copied to [ regs ] and [ stack ]
 *
 *
 * ==================================================================== */

static void lower_mload_actual (WN *block, WN *mload, PLOC ploc,
				LOWER_ACTIONS actions)
{
  ST       *preg;
  INT32     size, mloadOffset = 0; 
  PREG_NUM  addrN;

  if (WN_field_id(mload) != 0)
      lower_field_id (mload);

  TY_IDX    mloadTY = TY_pointed(Ty_Table[WN_ty(mload)]);
  WN       *mloadSize = WN_kid1(mload);

  if (WN_operator(mloadSize) != OPR_INTCONST) {
	DevWarn("mload_actual size is not INTCONST");
	if (WN_operator(mloadSize) == OPR_CVTL) {
		mloadSize = WN_kid0(mloadSize);
	}
  }
  size = WN_const_val(mloadSize);
  if (size<=0 || WN_operator(mloadSize) != OPR_INTCONST)
  {
    DevWarn("size of mload actual should be > 0");
    size = TY_size(Ty_Table[mloadTY]);
    DevWarn("adjusting size of (%s) to TY_size= %d",
	    TY_name(Ty_Table[mloadTY]), size);
    mloadSize = WN_Intconst(Integer_type, size);
  }

  Is_True((WN_opcode(mload) == OPC_MLOAD),
	  ("expected MLOAD node, not %s", OPCODE_name(WN_opcode(mload))));

  Setup_Struct_Output_Parameter_Locations(mloadTY);
  ploc = Get_Struct_Output_Parameter_Location(ploc);

 /*
  *  create addrN assignment to hold the address of the mload
  *  Adjust the address if there is an offset in the mload
  */
  preg = MTYPE_To_PREG(Pointer_type);
  {
    WN	*addr = WN_COPY_Tree(WN_kid0(mload));

    if (WN_load_offset(mload))
    {
      addr = WN_Add(Pointer_type,
                    addr,
                    WN_Intconst(Pointer_type, WN_load_offset(mload)));
    }
    addrN = AssignExpr(block, addr, Pointer_type);
  }

  while (PLOC_is_nonempty(ploc))
  {
    if (PLOC_on_stack(ploc))
    {
      PREG_NUM	dstPreg;
     /*
      *  create WN to copy mload to sp+offset
      *
      *  dstPreg = SP + offset - mloadoffset
      *  copy_aggregate() will add mloadoffset later
      */
      {
        WN 	*add;
	INT64	offset;

	offset = PLOC_offset(ploc) - Formal_Save_Area_Size
	  + Stack_Offset_Adjustment - mloadOffset; 

	add = WN_Add(Pointer_type,
		     WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
		     WN_Intconst(Pointer_type, offset));
	dstPreg = AssignExpr(block, add, Pointer_type);
      }
      {
	TY_IDX srcTY =	mloadTY;
	TY_IDX dstTY =	MTYPE_To_TY(Max_Uint_Mtype);
	TYPE_ID		quantum;
	WN		*con;
	INT32		copy_alignment, nMoves;
	INT32		todo= size - mloadOffset;

       /*
	* the dest is guaranteed to be Max_Uint aligned
	*/
	copy_alignment= compute_copy_alignment(srcTY, dstTY, 0);
	quantum =	compute_copy_quantum(copy_alignment);

	/*
	 *  we cannot (now) generate an intrinsics as we are in the middle
	 *  of setting call registers!!
         */
	nMoves= calculateLoadStore(todo, mloadOffset, quantum, WN_kid0(mload));

	if (MinStructCopyLoopSize && MinStructCopyLoopSize <= nMoves)
	{
	  con = WN_Intconst(Integer_type, todo);

	  copy_aggregate_loop(block,
			      srcTY,
			      dstTY,
			      mloadOffset,
			      con,
			      quantum,
			      preg,
			      addrN,
			      dstPreg,
			      mload,
			      NULL,
			      copy_alignment,
			      actions);
	  WN_Delete(con);
	}
	else
	{
	  copy_aggregate(block,
			 srcTY,
			 dstTY,
			 mloadOffset,
			 todo,
			 quantum,
			 preg,
			 addrN,
			 dstPreg,
			 mload,
			 NULL,
			 copy_alignment,
			 actions);
	}
      }
      return;
    }
    else
    {
     /*
      *  copy structure element to register
      */
      PREG_NUM regNo = PLOC_reg(ploc);
      ST	*reg = Preg_Offset_Is_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID   type = TY_mtype(Ty_Table[ST_type(reg)]);

      {
	WN      *load, *stid;
	INT32	todo = size - mloadOffset;

	if (PLOC_size(ploc) < MTYPE_size_reg(type)
		&& type == MTYPE_F8 && PLOC_size(ploc) == 4)
	{
		// need to copy a smaller size than default reg-size
		DevWarn("actual_mload: switch from mtype_f8 to mtype_f4");
		type = MTYPE_F4;
		reg = MTYPE_To_PREG(type);
	}

       /*
	*  special case "small" structs (or remainder of struct)
	*  we will try not to run off the end of the structure (as bad)
	*/
	if (todo < MTYPE_alignment(type))
	{
	  TYPE_ID	quantum;
	  INT32		newAlign, shiftn;

	  Is_True(Preg_Offset_Is_Int(regNo),
		  ("mload actual->reg(size/alignment problem"));
	  newAlign = nearest_power_of_two(todo);

	  quantum = Mtype_AlignmentClass(newAlign, MTYPE_type_class(type));

	  load = WN_IloadLdid(quantum, mloadOffset,
			      struct_memop_type (quantum, mloadTY), preg,
			      addrN); 

	  lower_copy_maps(mload, load, actions);

	  if (Target_Byte_Sex == BIG_ENDIAN) {
	    shiftn = MTYPE_size_reg(type) - MTYPE_size_reg(quantum);
  
	    load = WN_Shl(type, load, WN_Intconst(type, shiftn));
	  }
	}
	else
	{
	  load = WN_IloadLdid(type, mloadOffset,
			      struct_memop_type (type, mloadTY), preg,
			      addrN); 

	  lower_copy_maps(mload, load, actions);
	}

	stid= WN_Stid(type, regNo, reg, ST_type(reg), load);

	WN_INSERT_BlockLast(block, stid);
      }
    }
    mloadOffset += PLOC_size(ploc);
    ploc = Get_Struct_Output_Parameter_Location(ploc);
  }
}


/* ====================================================================
 *
 * static void lower_mload_formal
 *
 * Perform lowering (see WN_Lower description) for structure formals
 * registers will be copied to [ stack ]
 *
 * ==================================================================== */

static void lower_mload_formal(WN *block, WN *mload, PLOC ploc,
			       LOWER_ACTIONS actions)
{
  INT32   size, offset = 0; 
  ST     *sym = WN_st(mload);
  TY_IDX  symTY = ST_type(sym);

  Setup_Struct_Input_Parameter_Locations(symTY);
  ploc = Get_Struct_Input_Parameter_Location(ploc);
  size = TY_size(Ty_Table[symTY]);

  while (PLOC_is_nonempty(ploc))
  {
    if (PLOC_on_stack(ploc))
    {
     /*
      *  structure is already on the stack, nothing to do
      */
      return;
    }
    else
    {
     /*
      *	copy register to stack
      */
      PREG_NUM 	regNo =	PLOC_reg(ploc);
      ST	*reg = Preg_Offset_Is_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID   type = TY_mtype(ST_type(reg));
      INT32	todo = size - offset;
      {
	WN        *ldid, *store;

	if (PLOC_size(ploc) < MTYPE_size_reg(type)
		&& type == MTYPE_F8 && PLOC_size(ploc) == 4)
	{
		// need to copy a smaller size than default reg-size
		DevWarn("formal_mload: switch from mtype_f8 to mtype_f4");
		type = MTYPE_F4;
		reg = MTYPE_To_PREG(type);
	}

	ldid = WN_LdidPreg(type, regNo);
       /*
	*  special case "small" structs (or remainder of struct)
	*  to use type closer to size of remaining todo
	*/
	if (todo < MTYPE_alignment(type))
	{
	  TYPE_ID	quantum;
          INT32         newAlign, shiftn;

          Is_True(Preg_Offset_Is_Int(regNo),
		  ("mload actual->reg(size/alignment problem"));
          newAlign = nearest_power_of_two(todo);

          quantum = Mtype_AlignmentClass(newAlign, MTYPE_type_class(type));

          shiftn = MTYPE_size_reg(type) - MTYPE_size_reg(quantum);

	  if (Target_Byte_Sex == BIG_ENDIAN) {
	   /*
	    *  fix (kludge) for kernel bug pv 369702
	    *  Since U4 and I4 are both sign extended we could use
	    *  an arithmetic shift
	    */
	    if (MTYPE_alignment(quantum) == 4)
	    {
	      ldid= WN_Ashr(type, ldid, WN_Intconst(type, shiftn));
	    }
	    else
	    {
	      ldid= WN_Lshr(type, ldid, WN_Intconst(type, shiftn));
	    }
	  }

	  store = WN_Stid (quantum, offset, sym,
			   struct_memop_type (quantum, symTY) , ldid);
        }
	else
	{
	  store = WN_Stid (type, offset, sym, struct_memop_type (type, symTY),
			   ldid);   
	}
	lower_copy_maps(mload, store, actions);

        WN_INSERT_BlockLast(block, store);
      }
      offset += PLOC_size(ploc);
    }
    ploc = Get_Struct_Input_Parameter_Location(ploc);
  }
}




/* ====================================================================
 *
 *  void lower_complex_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				WN **realpart, WN **imagpart)
 *
 * Perform lowering (see WN_Lower description) on an expression
 * or intrinsic op, returning lowered tree.  
 *
 * ==================================================================== */
static void lower_complex_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
				   WN **realpart, WN **imagpart)
{
  WN		*wn;
  BOOL          intrinsic_lowered;

  Is_True(MTYPE_is_complex(WN_rtype(tree)),
	 ("expected complex type in lower_complex_emulation"));

 /*
  *  there is no way to turn off intrinsics consistently
  *  ie. the best we could do is 
  *		z = c4intrinsic(...)
  *  and then we would try to lower that !!
  */

  wn = lower_emulation(block, tree, actions, intrinsic_lowered);

  // Don't do this. It causes the optimizer to miss a lot of CSE's. 
  // R. Shapiro 10/5/98
  if (!intrinsic_lowered) {
       actions |= LOWER_INTRINSIC;
   }
  lower_complex_expr(block, wn, actions, realpart, imagpart);
}


/* ====================================================================
 *
 * WN *lower_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
 *                     BOOL & intrinsic_lowered)
 *
 * Perform lowering (see WN_Lower description) on an expression
 * or intrinsic op, returning lowered tree.  
 * 
 * intrinsic_lowered is set to TRUE if lowereing occurred on an intrinsic
 *
 * Should be called only if actions contains LOWER_INTRINSIC.
 *
 * ==================================================================== */
static WN *lower_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
			   BOOL & intrinsic_lowered)
{
  WN	*wn, *emBlock;
  OPCODE op = WN_opcode(tree);

  intrinsic_lowered = FALSE;
 /*
  *  try experiment with lowering children first
  *  (remember these are most likely complex expressions)
  */
  {
    INT32	i;

    for (i = 0; i < WN_kid_count(tree); i++)
      WN_actual(tree,i) = lower_expr(block, WN_actual(tree,i), actions);
  }

 /*
  *  Create a callblock and try to emulate call
  */
  wn = NULL;
  emBlock = WN_CreateBlock();

  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    Is_True((INTRN_is_nary(WN_intrinsic(tree))==FALSE),("nary slipped by"));
    if (Action(LOWER_INLINE_INTRINSIC) || Action(LOWER_INL_STACK_INTRINSIC))
    {
      wn = emulate(emBlock , tree);
    }
    if (wn == NULL && NotAction(LOWER_INTRINSIC))
    {
       return tree;
    }
    intrinsic_lowered = TRUE;
  }
 /*
  *  most likely quad emulation
  */
  else
  {
    wn = emulate(emBlock , tree);
  }


  if (wn)
  {
    emBlock = lower_block(emBlock, actions);
    WN_INSERT_BlockLast(block, emBlock);

    /*
     *  the emulation may contain nodes that may need to be lowered
     */
    if (OPCODE_is_stmt(WN_opcode(wn)))
    {
      wn = lower_stmt(block, wn, actions);
    }
    else if (OPCODE_is_expression(WN_opcode(wn)))
    {
      wn = lower_expr(block, wn, actions);
    }
  }
  else
  {
    WN_Delete(emBlock);

    if (OPCODE_is_call(op))
    {
      wn = lower_intrinsic(block, tree, actions);
    }
    else
    {
      //FIX:
      if (WN_intrinsic(tree) == INTRN_TLD_ADDR) {
	//prevent the lowering of TLD_ADDR from happening twice
	return tree;
      }

      wn = lower_intrinsic_op(block, tree, actions);
    }
  }
  return wn;
}


#ifdef linux
/* ====================================================================
 *
 * WN *lower_cis_intrinsic(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on CIS intrinsics for linux. On linux we'ld like
 * so use the standard sincos() library routines rather than the irix
 * *cis() routines. The trouble is that sincos returns its results
 * through pointers passed as arguments. Therefore it must be an
 * INSTRINSIC_CALL and not an INTRINSIC_OP (we want the later in order for 
 * wopt to perform the CSEs of the combined function, which operaters on
 * expressions only). Therefore we generate the CIS INTRINSIC_OP and
 * lower it here to a call to sincos(). The lowering looks like this:
 *
 * Before:
 *	C*INTRINSIC_OP INTRN_*CIS (angle)
 *
 * After:
 *      create C* temp
 *	VINTRINSIC_CALL INTRN_SINCOS (angle, &temp+x, &temp+0)
 *	C*C*LDID temp
 *
 * Note that the INTRINSIC_CALL is lowered here as well.
 *
 * ==================================================================== */
static WN *lower_cis_intrinsic(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN *callblock, *call, *parms[3];
  INTRINSIC sincos_id;
  TYPE_ID real_mtyp, complex_mtyp;
  TY_IDX real_ty, complex_ty;
  ST *return_sincos;
  WN *asinx, *acosx;

  complex_mtyp = WN_rtype(tree);
  real_mtyp = Mtype_complex_to_real(complex_mtyp);
  complex_ty = MTYPE_To_TY(complex_mtyp);
  real_ty = MTYPE_To_TY(real_mtyp);

  return_sincos = Gen_Temp_Symbol(complex_ty, ".sincos");
  acosx = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 
		       0,
		       Make_Pointer_Type(real_ty), return_sincos);
  asinx = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 
		       MTYPE_byte_size(real_mtyp),
		       Make_Pointer_Type(real_ty), return_sincos);

  switch (real_mtyp) {
  case MTYPE_F4:  sincos_id = INTRN_SINCOSF; break;
  case MTYPE_F8:  sincos_id = INTRN_SINCOS;  break;
  case MTYPE_F10: sincos_id = INTRN_SINCOSL; break;
  default:        sincos_id = INTRINSIC_INVALID; break;
  }

  parms[0] = WN_kid0(tree);
  parms[1] = WN_CreateParm(Pointer_type, asinx, MTYPE_To_TY(Pointer_type),
			   WN_PARM_BY_VALUE);
  parms[2] = WN_CreateParm(Pointer_type, acosx, MTYPE_To_TY(Pointer_type),
			   WN_PARM_BY_VALUE);
  call = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, sincos_id, 3, parms);
  WN_Delete(tree);

  callblock = WN_CreateBlock();
  callblock = lower_block(callblock, actions);
  WN_INSERT_BlockLast(block, callblock);

  call = lower_intrinsic_call(block, call, actions);
  WN_INSERT_BlockLast(block, call);

  WN *ldid = WN_Ldid(complex_mtyp, 0, return_sincos, complex_ty);
  return lower_expr(block, ldid, actions);
}
#endif


/* ====================================================================
 *
 * WN *lower_intrinsic_op(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on intrinsic statement <tree>,
 * returning lowered tree.  Should be called only if actions contains
 * LOWER_INTRINSIC.
 *
 * Since we are calling a runtime routine from a non-statement level
 * we need to
 *    [1]	insert call (lower_intrinsic)
 *    [2]	assign return value to preg temp
 *    [3]	return preg temp
 *
 * The return preg value may require special processing after
 * (see below for examples)
 * ==================================================================== */
static WN *lower_intrinsic_op(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN		*wn;
  OPCODE	op = WN_opcode(tree);
  INTRINSIC	id = WN_intrinsic(tree);

  Is_True(Action(LOWER_INTRINSIC),
	  ("actions does not contain LOWER_INTRINSIC"));
  Is_True(OPCODE_is_intrinsic(op),
	  ("expression is not intrinsic"));

  if (Compile_Upc && id == INTRN_TYPE_EXPR) {
    //This is just a dummy marker that's handled specially by whirl2c,
    //so do not lower it
    return tree;
  }

#ifdef linux
  switch (id) {
  case INTRN_F4CIS:
  case INTRN_F8CIS:
//  case INTRN_F10CIS:
    return lower_cis_intrinsic(block, tree, actions);
  }
#endif

 /*
  *  To evaluate the intrinsic call we need to save the return type 
  *  and create a temporary to hold the return value
  */
  wn = lower_intrinsic(block, tree, actions);

  Is_True(OPCODE_is_call(WN_opcode(wn)), 
	  ("lowered instrinsic op is not a call"));
  WN_INSERT_BlockLast(block, wn);

  {
    TYPE_ID	ty1, ty2;
    PREG_NUM	reg1, reg2;
    TYPE_ID	type = WN_rtype(tree);

    rename_preg(INTR_intrinsic_name(tree), ".return");

    if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (MTYPE_To_TY(type),
                                                 Complex_Not_Simulated);

      if (RETURN_INFO_count(return_info) <= 2) {

        ty1  = RETURN_INFO_mtype (return_info, 0);
        ty2  = RETURN_INFO_mtype (return_info, 1);
        reg1 = RETURN_INFO_preg (return_info, 0);
        reg2 = RETURN_INFO_preg (return_info, 1);
      }

      else
        Fail_FmtAssertion("lower_intrinsic_op: more than 2 return registers");
	/*NOTREACHED*/
    }

    else
      Get_Return_Mtypes(MTYPE_To_TY(type), Complex_Not_Simulated, &ty1, &ty2);

    switch(type)
    {
    case MTYPE_C4:
    case MTYPE_C8:
      {
	PREG_NUM	rzN, izN;
	TYPE_ID		real = Mtype_complex_to_real(type);

	if (!WHIRL_Return_Info_On) 
	  Get_Return_Pregs ( ty1, ty2, &reg1, &reg2 );
	Is_True((reg1 && reg2),
		("expected complex return value from intrinsic_op"));

	rzN = AssignExpr(block, WN_LdidPreg(real, reg1), real);
	izN = AssignExpr(block, WN_LdidPreg(real, reg2), real);

	wn = WN_Complex(type,
			WN_LdidPreg(real, rzN),
			WN_LdidPreg(real, izN));
      }
      break;

    case MTYPE_CQ:
     /*
      *  call is RSTYLE_VIA_FIRST_ARG
      *
      *  hopefully our intrinsic lowerer has inserted the address of the
      *  temporary area.
      */
      {
	TYPE_ID   real = Mtype_complex_to_real(type);
	TY_IDX    realTY = MTYPE_To_TY(real);
	WN       *temp,*temp1, *actual = WN_kid0(wn);

	if (WN_operator_is(actual, OPR_PARM))
	  actual = WN_kid0(actual);

	temp = lower_copy_tree( actual, actions);
	temp1 = lower_copy_tree( actual, actions);

	Is_True((OPCODE_is_call(WN_opcode(wn))), ("expected call opcode"));

	wn = WN_Complex(type,
			WN_Iload(real, 0, realTY, temp),
			WN_Iload(real, MTYPE_RegisterSize(real),
				 realTY, temp1));
      }
      break;

    default:
      {
	PREG_NUM	retReg;

	if (!WHIRL_Return_Info_On) 
	  Get_Return_Pregs ( ty1, ty2, &reg1, &reg2 );

	Is_True((reg1 != 0), ("expected return value from intrinsic_op"));
	Is_True((reg2 == 0), ("cannot evaluate 2 regs into an expression"));

	retReg = AssignExpr(block, 
			    WN_LdidPreg(ty1, reg1),
			    type);

	wn = WN_LdidPreg(type, retReg);
      }
      break;
    }
    rename_reset();
  }

  /*
   *  These require special processing as the function return
   *  value must be interpreted (i think)
   *
   *	    s_cmp(a, b, len_a, len_b)
   *		a <  b		negative
   *		a == 0		0
   *		a >  0		positive
   */
  switch (id) {
  case INTRN_CEQEXPR:
    wn = WN_EQ(Boolean_type, wn, WN_Zerocon(Boolean_type));
    break;
  case INTRN_CNEEXPR:
    wn = WN_NE(Boolean_type, wn, WN_Zerocon(Boolean_type));
    break;
  case INTRN_CGEEXPR:
    wn = WN_GE(Boolean_type, wn, WN_Zerocon(Boolean_type));
    break;
  case INTRN_CGTEXPR:
    wn = WN_GT(Boolean_type, wn, WN_Zerocon(Boolean_type));
    break;
  case INTRN_CLEEXPR:
    wn = WN_LE(Boolean_type, wn, WN_Zerocon(Boolean_type));
    break;
  case INTRN_CLTEXPR:
    wn = WN_LT(Boolean_type, wn, WN_Zerocon(Boolean_type));
    break;
  }

 /*
  *  The preg may need map processing
  */
  return lower_expr(block, wn, actions);
}




/* ====================================================================
 *
 * WN *lower_intrinsic(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on an expression or,
 * intrinsic returning lowered tree.  
 *
 * Should be called only if actions contains LOWER_INTRINSIC.
 *
 * ==================================================================== */
static WN *lower_intrinsic(WN *block, WN *tree, LOWER_ACTIONS actions)
{

  Is_True(Action(LOWER_INTRINSIC),
	  ("actions does not contain LOWER_INTRINSIC"));
  Is_True(OPCODE_is_intrinsic(WN_opcode(tree)),
	  ("expression is not intrinsic"));

  {
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);

    if (INTRN_is_actual(id))
    {
     /*
      *  weird fortran anachronism, we need to create an ST entry
      * 	and pass an LDA of the address
      */
      TYPE_ID	rtype = WN_rtype(tree);
      WN	*lda;
      {
	TY_IDX  ty = Make_Function_Type(MTYPE_To_TY(rtype));
	ST     *st = Gen_Intrinsic_Function(ty, INTRN_rt_name(id));
	lda = WN_Lda(Pointer_type, 0, st);
      }

      WN_Delete(tree);
      return lower_expr(block, lda, actions);
    }
  }
  {
   /*
    *  Create a callblock and generate call.
    *  The arguments may be expanded to match the runtime routine.
    */
    WN	*call, *callblock;

    callblock = WN_CreateBlock();

    call = intrinsic_runtime(callblock, tree);

    callblock = lower_block(callblock, actions);
    WN_INSERT_BlockLast(block, callblock);

    if (Action(LOWER_TO_CG))
	actions |= LOWER_CALL;
    call = lower_call(block, call, actions);

    return call;
  }
}


/* ====================================================================
 *
 * WN *lower_actual(WN *block, WN *actual, TYPE_ID parmType, INT32 reg)
 *
 * Auxilary function to lower_call to put and actual in a register
 * This was motivated by -DEBUG:Varargs_Prototypes=off, where for
 * unprototyped calls, all floating point registers need to go in the
 * integer registers as well
 *
 * ==================================================================== */
static WN *lower_actual(WN *block, WN *actual, TYPE_ID parmType, INT32 reg)
{
 /*
  * float parm goes in int register, so convert
  */
  if (MTYPE_float(parmType) && Preg_Offset_Is_Int(reg))
  {
    TYPE_ID type = TY_mtype(ST_type(Int_Preg));

    actual = WN_Tas(type, ST_type(Int_Preg), actual);
  }
  
  {
    WN	*stid;
    ST	*regST;

    if (Preg_Offset_Is_Int(reg)) 
	regST = Int_Preg;
    else
    	// keep float size in preg
	regST=	MTYPE_To_PREG(parmType);

    TYPE_ID type = TY_mtype(ST_type(regST));

    stid = WN_StidIntoPreg (type, reg, regST, actual);
    WN_Set_Linenum(stid, current_srcpos);
    WN_INSERT_BlockLast(block, stid);
  }

  return actual;
}

/* ====================================================================
 *  For each function generate a call to a profiler function
 *	__prof( &func, &call, funcname, callname, state);
 *  where
 *	state==0	before call
 *     state==1	after call
 *
 * ==================================================================== */

static WN *lower_profile_call(WN *block, WN *tree, INT32 state,
			      LOWER_ACTIONS actions)
{
  if (Gen_Profile && WN_has_sym(current_function))
  {
    WN		*profcall;
    char	*name;

    {
      TY_IDX  ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
      ST     *st = Gen_Intrinsic_Function(ty, Gen_Profile_Name);

      Set_PU_no_side_effects(Pu_Table[ST_pu(st)]);
      Set_PU_is_pure(Pu_Table[ST_pu(st)]);

      profcall = WN_Call(MTYPE_V, MTYPE_V, 5, st);
    }

    ST *current_fcn_st = WN_st(current_function);

    WN_actual(profcall, 0) = WN_Lda(Pointer_type, 0, current_fcn_st);
    name = ST_name(current_fcn_st);
    WN_actual(profcall, 2) = WN_LdaString(name, 0, strlen(name)+1);
    WN_actual(profcall, 4) = WN_Intconst(MTYPE_I4, state);

    if (WN_has_sym(tree))
    {
      ST *st = WN_st(tree);

      WN_actual(profcall, 1) = WN_Lda(Pointer_type, 0, st);
      name = ST_name(st);
      WN_actual(profcall, 3) = WN_LdaString(name, 0, strlen(name)+1);
    }
    else
    {
      Is_True((WN_operator_is(tree, OPR_ICALL)),
	      ("expected icall node, not %s", OPCODE_name(WN_opcode(tree))));

      WN_actual(profcall, 1)
	= lower_copy_tree(WN_actual(tree, WN_num_actuals(tree)), actions);
      WN_actual(profcall, 3) = WN_Zerocon(Pointer_type);
    }

    Gen_Profile = FALSE;
    profcall = lower_call(block, profcall, actions);
    Gen_Profile = TRUE;

    return profcall;
  }
  return NULL;
}

/* ====================================================================
 *
 * WN *lower_call(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on call statement <tree>,
 * returning lowered tree.  Should be called only if actions contains
 * LOWER_CALL.
 *
 * ==================================================================== */

static WN *lower_call(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  INT16	       i;
  PLOC	       ploc;
  TY_IDX       call_ty;
  WN	      *callblock;
  SRCPOS       srcpos = WN_Get_Linenum(tree);
  INT	       num_actuals = WN_num_actuals(tree);
  ST          *callee_st = NULL;

  Is_True(OPERATOR_is_call(WN_operator(tree)),
	  ("expected call node, not %s", OPERATOR_name(WN_operator(tree))));

  for (i = 0; i < WN_kid_count(tree); i++)
    WN_actual(tree, i) = lower_expr(block, WN_actual(tree, i), actions);

  /* Get the ST of the callee if available */
  if (WN_has_sym(tree)) {
    callee_st = WN_st(tree);
  }

  if (WHIRL_Return_Val_On && Action(LOWER_RETURN_VAL) && 
      WN_rtype(tree) == MTYPE_V &&
      (WN_operator(tree) == OPR_CALL || WN_operator(tree) == OPR_ICALL ||
       WN_operator(tree) == OPR_PICCALL)) {
    // if it is a call returning struct coerced to void, may need to introduce
    // a fake first parameter
    TY_IDX prototype;
    if (WN_operator(tree) == OPR_ICALL) 
      prototype = WN_ty(tree);
    else {
      ST_IDX func_stidx = WN_st_idx(tree);
      PU_IDX puidx = ST_pu(St_Table[func_stidx]);
      prototype = PU_prototype(Pu_Table[puidx]);
    }
    RETURN_INFO return_info = Get_Return_Info(TY_ret_type(prototype),
                                              Complex_Not_Simulated);
    if (RETURN_INFO_return_via_first_arg(return_info)) {
      ST *return_st = Gen_Temp_Symbol(TY_ret_type(prototype), ".vcall");
      WN *awn = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0,
                             Make_Pointer_Type(TY_ret_type(prototype)), 
			     return_st);
      awn = lower_expr(block, awn, actions);
      WN *n_call = add_fake_parm(tree, awn, WN_ty(awn));
      WN_Delete(tree);
      tree = n_call;
    }
  }

  if (Action(LOWER_PICCALL))
  {
   /* convert a CALL to PICCALL for the following cases:
    *
    *  1. we are generating PIC code.
    *  2. -TARG:force_jalr
    *  3. We need to setup $t9.
    *  4. We are generating CPIC code and the callee is visible outside the
    *     dso.
    */
    if (WN_operator_is(tree, OPR_CALL)  &&
        (Force_Jalr                     ||
         PU_needs_t9(Pu_Table[ST_pu(callee_st)]) ||
         (Gen_PIC_Calls &&
          (Gen_PIC_Shared                       ||
          (Gen_PIC_Call_Shared &&
           ST_visible_outside_dso(*callee_st)) )))) 
    {
      INT16       n = WN_kid_count(tree);
      WN          *itree;
      /*
       *  convert (CALL sym args ...) into (PICCALL args ... (LDA sym)). 
       *  The LDA does not have an OP_PARM
       */
      itree = WN_Piccall( WN_rtype(tree), WN_desc(tree), n+1, callee_st);
  
      Is_True(callee_st == WN_st(tree),
	      ("lower_call: something changed that Robert didn't expect!"));
      WN_actual(itree, n) = WN_Lda(Pointer_type, 0, callee_st);
      WN_Set_Linenum(itree, srcpos);
      WN_Set_Flags(tree, itree);
  
      while (--n >= 0)
        WN_actual(itree, n) = WN_actual(tree, n);

      if (Cur_PU_Feedback) {
	Cur_PU_Feedback->FB_lower_call( tree, itree );
      }
      WN_Delete(tree);
      tree = itree;
    }
  }
  
  if(Action(LOWER_UPC_TO_INTR) &&
     (WN_operator(tree) == OPR_CALL || WN_operator(tree) == OPR_ICALL ||
      WN_operator(tree) == OPR_PICCALL)) {
    
    // for calls returning shared ptrs need to change the rtype of the call 
    // node, otherwise whirl2c gets very confused
    TY_IDX prototype;
    if (WN_operator(tree) == OPR_ICALL) 
      prototype = WN_ty(tree);
    else {
      ST_IDX func_stidx = WN_st_idx(tree);
      PU_IDX puidx = ST_pu(St_Table[func_stidx]);
      prototype = PU_prototype(Pu_Table[puidx]);
    }
    
    if(Type_Is_Shared_Ptr(TY_ret_type(prototype), TRUE)) {
      WN_set_rtype(tree, TY_mtype(TY_To_Sptr_Idx(TY_ret_type(prototype))));
    }
    
    return tree;
  }

  if (NotAction(LOWER_CALL))
    return tree;

 /*
  *  For each function generate a call to a profiler function
  *	__prof( &func, &call, funcname, callname);
  */
  if (Gen_Profile)
  {
    WN  *profcall = lower_profile_call(block, tree, 0, actions);
    WN_INSERT_BlockLast (block, profcall);
  }

  callblock = WN_CreateBlock();

  /*
   * TODO:
   * (a) Just before each call passing a struct by value, if (part of) the
   *     struct can be passed in register(s), convert MLOAD into multiple
   *     ILOADs.  MEDIUM PRIORITY (at least detect condition)
   * (b) Just before each call, pass static link to nested calls.
   * (d) For -o32 -shared, just after returning from a call, restore $gp
   *     (but see bug 159929).
   */

  /*
   * Create block containing stores of actual loads into pregs.
   * The call can continue to have the original actuals, because 
   * we don't use that except to see the number of parameters. 
   *
   * New:  Go through the list twice to give localize an easier job
   *	   First handle structures (there may be loop code to move the
   *       structure)
   */
  call_ty = (WN_operator_is(tree,OPR_ICALL) ? WN_ty(tree) :
	     ST_pu_type(callee_st));
  Is_True(WN_operator_is(tree, OPR_ICALL) ||
	  callee_st == WN_st(tree),
	  ("lower_call: something changed that Robert didn't expect!"));
  ploc = Setup_Output_Parameter_Locations(call_ty);

  for (i = 0; i < num_actuals; i++)
  {
    WN		*parm = WN_actual(tree, i);
    TYPE_ID	parmType = WN_rtype(parm);
    WN		*actual = WN_operator_is(parm, OPR_PARM) ? WN_kid0(parm)
                                                         : parm;

    if (MTYPE_is_m(parmType))
    {
     /*
      * structure parameter
      */
      ploc = Get_Output_Parameter_Location( TY_Of_Parameter(parm));
      lower_mload_actual (callblock, actual, ploc, actions);
    }
    else if (MTYPE_is_complex(parmType) && PU_ftn_lang(Get_Current_PU()))
    {
     /*
      * Fortran by value can generate this construct
      * we must make it look like a structure parameter
      * Note that GCC has complex type, which should go to
      * normal processing (like quads).
      */
      ploc = Get_Output_Parameter_Location( TY_Of_Parameter(parm));
      lower_complex_actual (callblock, actual, ploc, actions);
    }
    else
    {
      ploc = Get_Output_Parameter_Location( MTYPE_To_TY(parmType));
    }
  }

  ploc = Setup_Output_Parameter_Locations(call_ty);
  for (i = 0; i < num_actuals; i++)
  {
   /*
    * We need to get the TY of the parameter: 
    * if no prototype, then we don't have a list of formals;
    * the actual may need to be converted if there is a prototype;
    * the rtype of the opcode is the converted type, so use it. 
    */
    WN         *parm = WN_actual(tree, i);
    TYPE_ID     parmType = WN_rtype(parm);
    WN         *actual = WN_operator_is(parm, OPR_PARM) ? WN_kid0(parm) : parm;
    TY_IDX      ty;

    if (MTYPE_is_m(parmType) 
	|| (MTYPE_is_complex(parmType) && PU_ftn_lang(Get_Current_PU())) )
    {
     /*
      * already processed
      */
      ploc = Get_Output_Parameter_Location( TY_Of_Parameter(parm));
      continue;
    }

    ty =  TY_Of_Parameter(parm);
    ploc = Get_Output_Parameter_Location( MTYPE_To_TY(parmType));
    {
     /*
      *  canonicalize [I,U][1,2] types (to [I,U]4
      */
      TYPE_ID  type = Mtype_comparison( Fix_TY_mtype(ty));

      if (parmType != type)
      {
	DevWarn("lower_call(): line %d, parm #%d type mismatch (WN_rtype(parm)"
		" = %s) (cannonical TY_mtype(parm))) %s)",
		Srcpos_To_Line(WN_Get_Linenum(tree)), i,
		Mtype_Name(parmType), Mtype_Name(type));
      }
    }

    if (PLOC_on_stack(ploc))
    {
     /*
      * stack offset 
      * We can use the preg for $sp here, but a simpler way
      * is to use the SP symbol. 
      */
      WN *wn;

      wn = WN_Stid(parmType, PLOC_offset(ploc) - Formal_Save_Area_Size
		             + Stack_Offset_Adjustment,
		   SP_Sym, ty, actual);
      WN_Set_Linenum(wn, srcpos);
      WN_INSERT_BlockLast(callblock, wn);
    }
   /*
    * special case (as quad -> integer doesn't make much sense).
    * Store argument into temp preg float parm goes in int register, so convert
    */
    else if (MTYPE_is_quad(parmType) && Preg_Offset_Is_Int(PLOC_reg(ploc)))
    {
      PREG_NUM qN;

      qN = AssignExpr(callblock, actual, parmType);

      lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN),   MTYPE_F8,
		   PLOC_reg(ploc));

      lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN+1), MTYPE_F8,
		   PLOC_reg(ploc)+1);
    }
    else
    {
      actual = lower_actual(callblock, actual, parmType, PLOC_reg(ploc));

      if (WN_operator_is(parm, OPR_PARM))
	WN_kid0(parm) = actual;
      else
	WN_actual(tree, i) = actual;

      if (! TY_has_prototype(call_ty)        &&
	  DEBUG_Varargs_Prototypes == FALSE  &&
	  Preg_Offset_Is_Float(PLOC_reg(ploc)))
      {
	actual = lower_copy_tree(actual, actions);

	if (MTYPE_is_quad(parmType))
	{
	  PREG_NUM  qN;

	  qN = AssignExpr(callblock, actual, parmType);

	  lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN),   MTYPE_F8,
		       PLOC_vararg_reg(ploc));
	  lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN+1), MTYPE_F8,
		       PLOC_vararg_reg(ploc)+1);
	}
	else
	{
	  lower_actual(callblock, actual, parmType, PLOC_vararg_reg(ploc));
	}
      }
    }
  }

 /*
  * store param size in tree for later re-use 
  * unless has no prototype (then could have varying # args). 
  */
  if (num_actuals > 0 &&
      TY_has_prototype(call_ty) &&
      Get_PU_arg_area_size(call_ty) == 0 &&
      ! TY_is_varargs(call_ty))
  {
      Set_PU_arg_area_size(call_ty, PLOC_total_size(ploc));
  }


 /*
  *  copy the arguments to avoid building a dag
  *  (region code might delete these nodes)
  *  CG will use these arguments to build register usage information
  */
  {
    INT16       n= num_actuals;

    while (--n >= 0)
      WN_actual(tree, n) = lower_copy_tree( WN_actual(tree, n), actions);
  }

  if (MTYPE_is_complex(WN_rtype(tree)))
  {
    INT16       n = WN_kid_count(tree);
    WN          *itree;
    TYPE_ID	rtype, desc;
    
    rtype = Mtype_complex_to_real(WN_rtype(tree));
    desc = Mtype_complex_to_real(WN_desc(tree));

   /*
    *  For complex the call type will be changed by WN_Call()
    *  into (PICCALL args ... (LDA sym)).
    */
    if (WN_operator_is(tree, OPR_CALL))
    {
      itree = WN_Call( rtype, desc, n, WN_st(tree));
    }
    else if (WN_operator_is(tree, OPR_ICALL))
    {
      itree = WN_Icall( rtype, desc, n, WN_ty(tree));
    }
    WN_Set_Linenum(itree, srcpos);
    WN_Set_Flags(tree, itree);

    while (--n >= 0)
      WN_actual(itree, n) = WN_actual(tree, n);

    if (Cur_PU_Feedback) {
      Cur_PU_Feedback->FB_lower_call( tree, itree );
    }
    WN_Delete(tree);
    tree = itree;
  }

  /*
   *  F90 needs this 
   *  The callee will require the current FP or Slink in $2
   */
  if (Action(LOWER_UPLEVEL))
  {
    if (callee_st && PU_is_nested_func(Pu_Table[ST_pu(callee_st)]))
    {
      WN  *wn, *link;
     
     /*
      *  These are "downlevel" calls for F90 (ex mp procedures),
      *  or uplevel (child calls parent)
      *  This should only happen for F90.
      */
      if (PU_lexical_level(&St_Table[WN_st_idx(current_function)]) <
	  PU_lexical_level(callee_st))
      {
	link = WN_LdidPreg(Pointer_type, Frame_Pointer_Preg_Offset);
      }

     /*
      *  Need to set up the register for "sideways" (internal calling internal)
      *  and uplevel calls. This should only happen for F90.
      */
      else 
      {
	ST  *slink = Find_Slink_For_Scope(WN_st(current_function),
					  callee_st);

	link = WN_Ldid (Pointer_type, 0, slink, ST_type(slink));
      }

      wn = WN_StidIntoPreg(Pointer_type,
			   Static_Link_Preg_Offset,
			   MTYPE_To_PREG(Pointer_type), link);
	  
      WN_Set_Linenum(wn, srcpos);
      WN_INSERT_BlockLast(callblock, wn);
     }
  }

  /* 
   * Check that stack space needed for actuals doesn't overflow 
   * what we initially reserved. 
   */
  Check_Actual_Stack_Size (tree);

  callblock = lower_block(callblock, actions);
  WN_INSERT_BlockLast (block, callblock);

  /******************************************************
	STUPID: cannot generate call here as it will affect the return value
	for this function
  if (Gen_Profile)
  {
    WN_INSERT_BlockLast (block, tree);

    return lower_profile_call(block, tree, 1, actions);
  }
  ******************************************************/

  return tree;
}




/* ====================================================================
 *
 * WN *lower_compgoto(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on switch val in COMPGOTO
 * node <tree>, returning XGOTO.
 *
 * If there is a default label, code is generated to test the range
 * of switch values
 * ==================================================================== */

static WN *lower_compgoto(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  INT32		pointerSize = MTYPE_RegisterSize(Pointer_type);
  INT32		num_entries = WN_num_entries(tree);
  WN		*index, *add;
  ST		*table_st;
  LEAF		indexN, addN;

  Is_True(WN_opcode(tree) == OPC_COMPGOTO,
	  ("expected COMPGOTO node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(WN_num_entries(tree) > 0,
	  ("expected COMPGOTO nu,_entries > 0 not %d", WN_num_entries(tree)));

  WN_kid0(tree) = lower_expr(block,
			     WN_Coerce(Pointer_type, WN_kid0(tree)),
			     actions);

  if (NotAction(LOWER_COMPGOTO))
    return tree;

  index = WN_kid0(tree);

  indexN = Make_Leaf(block, WN_Coerce(Pointer_type, index), Pointer_type);

 /*
  * create fake, incomplete array type so table_st has correct size
  *
  *  generate jump for computed goto
  *    agoto ( & table + (i * sizeof(Pointer_type)));
  *
  *  create
  *    addN = table + (i * sizeof(Pointer_type));
  */
  {
    TY_IDX table;
    WN	*mpy;

    table = Make_Array_Type(Pointer_type, 1, num_entries);

    table_st = Gen_Read_Only_Symbol (table, "jump_table");
    Set_ST_is_initialized(table_st);	/* so goes in rdata section */
    
    mpy = WN_Mpy(Pointer_type, 
		 Load_Leaf(indexN),
		 WN_Intconst(Pointer_type, pointerSize));

    add = WN_Add(Pointer_type,
		 WN_Lda (Pointer_type, 0, table_st),
		 mpy);
    addN = Make_Leaf(block, add, Pointer_type);
  }
  
 /*
  *  for default cases we must verify the index fall in range
  */
  WN *wn_truebr = NULL;
  if (WN_kid_count(tree) == 3)
  {
   /*
    *  branch to default if (index >= n)
    *  because the indexN is unsigned, we do not have to compare 
    *  greater then zero
    */
    WN  *ge, *truebr;
    WN	*def  = WN_kid2(tree);

    ge =  WN_GE(Pointer_type,
		Load_Leaf(indexN),
		WN_Intconst(Pointer_type, num_entries));

    truebr = lower_truebr(WN_label_number(def), ge, &wn_truebr, actions);
    WN_INSERT_BlockLast(block, truebr);
  }

 /*
  * create fake, incomplete array type so table_st has correct size
  *
  *  generate jump for computed goto
  *    agoto ( & table + (i * sizeof(Pointer_type)));
  */
  {
    WN  *xgoto;
    WN	*gotoTable = WN_kid1(tree);

    index = WN_Iload(Pointer_type,
		     0,
		     MTYPE_To_TY(Pointer_type),
		     Load_Leaf(addN));

    index = lower_expr(block, index, actions);

    xgoto = WN_CreateXgoto(num_entries, index, gotoTable, table_st);
    WN_Set_Linenum (xgoto, WN_Get_Linenum(tree));

    // Lower feedback info
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_compgoto( tree, xgoto, wn_truebr );

    return xgoto;
  }
}




/* ====================================================================
 *
 * WN *lower_assert(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on OPC_ASSERT
 * node <tree>, returning structured if
 *
 * ==================================================================== */

static WN *lower_assert(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN  *if_else, *if_then, *IF, *trap, *cond;

  Is_True(WN_opcode(tree) == OPC_ASSERT,
	  ("expected ASSERT node, not %s", OPCODE_name(WN_opcode(tree))));

  WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);

  if (NotAction(LOWER_ASSERT))
    return tree;

  if_then = WN_CreateBlock();
  if_else = WN_CreateBlock();
  cond = lower_expr(block, WN_kid0(tree), actions);

 /*
  * generate intrinsic call 
  */
  switch ( WN_offset(tree) ) {
    case WN_TRAP_F77_BOUNDS_ERROR:
      {
	char *name;
	WN **kids = (WN **) alloca(4 * sizeof(WN *));

	/* add blank at end of string cause s_rnge wants one */
	kids[0] = WN_LdaString ( "? ", 0, 2 );
	kids[1] = WN_Intconst ( MTYPE_I4, -1 );	/* cause of s_rnge weirdness */

	/* s_rnge expects name with _ at end */
	name = ST_name(&St_Table[PU_Info_proc_sym(Current_PU_Info)]);
	kids[2] = WN_LdaString ( name, 0, strlen(name)+1 );

	kids[3] = WN_Intconst(MTYPE_I4, Srcpos_To_Line(WN_Get_Linenum(tree)));
	trap = WN_Create_Intrinsic ( OPC_VINTRINSIC_CALL,
				     INTRN_F77_BOUNDS_ERR, 4, kids );
      } 
      break;

    case WN_TRAP_C_BOUNDS_ERROR:
      /* Check DEBUG_Verbose_Runtime.  If set, call __C_runtime_error;
       * otherwise emit a BRK_RANGE trap.
       */
      /* TODO PV 443095:  This doesn't have descriptive parameters due
       * to limitations on the ASSERT node.  Fix that eventually.
       */
      if ( DEBUG_Verbose_Runtime ) {
	char *name;
	WN **kids = (WN **) alloca(4 * sizeof(WN *));

	/* __C_runtime_error ( BRK_RANGE, PU_name, line_no, fmt, ...);
	 */
#if (defined(__MACH__) && defined(__APPLE__)) || defined(_AIX) || defined (sun) || defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define BRK_RANGE 0
#endif

#ifndef linux 
	kids[0] = WN_Intconst ( MTYPE_I4, BRK_RANGE );
#else
	fprintf(stderr, "Don't know how to do BRK_RANGE\n");
	exit(-1);
#endif

	/* Source routine where error occurs: */
	name = ST_name(&St_Table[PU_Info_proc_sym(Current_PU_Info)]);
	kids[1] = WN_LdaString ( name, 0, strlen(name)+1 );

	/* Source line where error occurs: */
	kids[2] = WN_Intconst ( MTYPE_I4,
				Srcpos_To_Line(WN_Get_Linenum(tree)) );

	/* Eventually, this should be a printf format and arguments,
	 * describing the case that fails (i.e. array and index).
	 * For now, the ASSERT node doesn't have the right kids.
	 */
	kids[3] = WN_LdaString ( "unknown array", 0, 14 );

	/* Make the call: */
	trap = WN_Create_Intrinsic ( OPC_VINTRINSIC_CALL,
				     INTRN_RT_ERR, 4, kids );
      } else {
#ifndef linux
	trap = WN_CreateTrap ( BRK_RANGE );
#else   
	fprintf(stderr, "Don't know how to do BRK_RANGE\n");
	exit(-1);
#endif  

	// ADD FEEDBACK INFO ??
      }
      break;

    default:
      trap = WN_CreateTrap ( WN_offset(tree) );
      // ADD FEEDBACK INFO ??
      break;
  }

  WN_Set_Linenum(trap, WN_Get_Linenum(tree));
  WN_INSERT_BlockLast(if_else, trap);

  IF = WN_CreateIf(cond, if_then, if_else);
  // ADD FEEDBACK INFO !!
  WN_Set_Linenum(IF, WN_Get_Linenum(tree));
  WN_Delete(tree);

  IF = lower_scf(block, IF, actions);
  return IF;
}


static WN *lower_branch(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True((WN_opcode(tree) == OPC_TRUEBR || WN_opcode(tree) == OPC_FALSEBR),
	  ("expected true/false"));

  WN * const kid = lower_expr(block, WN_kid0(tree), actions);
  WN_kid0(tree) = kid;

  if (WN_operator(kid) == OPR_LNOT)
  {
    switch(WN_opcode(tree))
    {
    case OPC_TRUEBR:
      WN_set_opcode(tree, OPC_FALSEBR);
      break;

    case OPC_FALSEBR:
      WN_set_opcode(tree, OPC_TRUEBR);
      break;

    default:
      break;
    }

    WN_kid0(tree) = WN_kid0(kid);
    WN_Delete(kid);
  }

  return tree;
}
 
 
/* ====================================================================
 *
 * WN *lower_return_val(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on RETURN_VAL nodes
 * turning kid0 expression into an appropriate store and the RETURN_VAL
 * into a RETURN.
 *
 * ==================================================================== */

static WN *lower_return_val(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  PREG_NUM    preg;
  ST         *preg_st;
  TYPE_ID     mtype;
  RETURN_INFO return_info;
  WN         *wn;
  
  Is_True((WN_operator(tree) == OPR_RETURN_VAL),
	  ("expected RETURN_VAL node, not %s", OPCODE_name(WN_opcode(tree))));
  
  upc_srcpos = WN_Get_Linenum(tree);
  
  TYPE_ID return_mtype = WN_rtype(tree);

  TY_IDX idx = WN_ty(WN_kid0(tree));
  idx = TY_To_Sptr_Idx(idx);
  if (idx == shared_ptr_idx || idx == pshared_ptr_idx) {
    WN *val_wn;
    if (TY_mtype(idx) == MTYPE_M) {
      /* Special case for returning a struct pointer-to-shared (see bug387) */
      return_mtype = MTYPE_M;
    }
    val_wn = WN_kid0(tree);
    if(WN_operator(val_wn) == OPR_TAS && 
       WN_operator(WN_kid0(val_wn)) == OPR_INTCONST &&
       WN_const_val(WN_kid0(val_wn)) == 0) {
      WN_kid0(tree) =
	WN_Ldid(return_mtype, 0, idx == shared_ptr_idx ? shared_null : pshared_null, idx);
    }
  }

  if (return_mtype != MTYPE_M) {
    TY_IDX return_ty_idx = Be_Type_Tbl(return_mtype);
    return_info = Get_Return_Info (return_ty_idx, Use_Simulated);

    WN_kid0(tree) = lower_expr (block, WN_kid0(tree), actions);
    if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
      // This happens, for example, if a complex result is returned for ia32.
      // Create an istore; an mstore with a single complex value seemed
      // to confuse things later on.
      WN *first_formal = WN_formal(current_function, 0);
      TY_IDX tidx = ST_type(WN_st(first_formal));
      OPCODE const_op = OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V);
      // WN *swn = WN_CreateIntconst (const_op, TY_size(return_ty_idx));
      WN *awn = WN_CreateLdid(OPR_LDID, 
			      TY_mtype(Ty_Table[tidx]), 
			      TY_mtype(Ty_Table[tidx]),
			      WN_idname_offset(first_formal), 
			      WN_st(first_formal), 
			      tidx);
      // wn  = WN_CreateMstore (0, tidx, WN_kid0(tree), awn, swn);
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V,
			   return_mtype, 0, tidx, WN_kid0(tree), awn);
    } else {  // Returned in preg
      Is_True(RETURN_INFO_count(return_info) == 1,
	      ("expected return_info count to be 1"));

      mtype   = RETURN_INFO_mtype (return_info, 0);
      preg    = RETURN_INFO_preg (return_info, 0);
      preg_st = (MTYPE_is_float(mtype) ? Float_Preg :
                 ((mtype==MTYPE_I8 || mtype==MTYPE_U8) ? MTYPE_To_PREG(mtype) : 
                  Int_Preg));

      wn  = WN_CreateStid (OPR_STID, MTYPE_V, Promoted_Mtype[mtype], 
	  		   preg, preg_st, Be_Type_Tbl(mtype), WN_kid0(tree));
      WN_Set_Linenum(wn, upc_srcpos);
    }
    WN_INSERT_BlockLast (block, wn);
  }
  else { // MTYPE_M; rhs is one of MLDID or MILOAD or MLOAD

    if (Compile_Upc && Action(LOWER_RETURN_VAL)) {
      /* Whirl2c has some problem with this lowering; since whirl2c supports 
	 RETURN_VAL stmts anyway, don't see why this needs to be lowered into Return Pregs 
      */
      WN_Set_Linenum(tree, upc_srcpos);
      return tree;
    }

    WN *o_rhs = WN_kid0(tree); 
    OPERATOR rhs_opr = WN_operator(o_rhs);
    TY_IDX ty_idx = WN_ty(o_rhs);
    if (OPERATOR_is_load(rhs_opr) && WN_field_id(o_rhs) != 0) {
      if (rhs_opr == OPR_LDID) 
        ty_idx = get_field_type(ty_idx, WN_field_id(o_rhs));
      else { // OPR_MLOAD
        ty_idx = TY_pointed(Ty_Table[ty_idx]);
        ty_idx = get_field_type(ty_idx, WN_field_id(o_rhs));
      }
    }
    return_info = Get_Return_Info(ty_idx, Complex_Not_Simulated);

    if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
      Is_True(RETURN_INFO_count(return_info) == 0, 
	      ("expected RETURN_INFO to have 0 count"));
      WN *n_rhs;

      // fix rhs
      if (WN_operator(o_rhs) == OPR_LDID)
        //n_rhs = lower_mldid(block, o_rhs, LOWER_MLDID_MSTID);
	n_rhs = lower_mldid(block, o_rhs, actions);
      else if (WN_operator(o_rhs) == OPR_ILOAD) 
        //n_rhs = lower_miload(block, o_rhs, LOWER_MLDID_MSTID);
	n_rhs = lower_miload(block, o_rhs, actions);
      else n_rhs = o_rhs; 		// MLOAD

      // create an mstore
      WN *first_formal = WN_formal(current_function, 0);
      TY_IDX tidx = ST_type(WN_st(first_formal));
      WN *awn = WN_CreateLdid(OPR_LDID, 
			      TY_mtype(Ty_Table[tidx]), 
			      TY_mtype(Ty_Table[tidx]),
			      WN_idname_offset(first_formal), 
			      WN_st(first_formal), 
			      tidx);
      WN *swn = WN_CopyNode(WN_kid1(n_rhs));
      wn  = WN_CreateMstore (0, tidx, n_rhs, awn, swn);

      WN_INSERT_BlockLast (block, wn);
    }
    else { // return in return registers
      INT32 i;
      WN *n_rhs;
      UINT algn;
      TY_IDX ty_idx_used;

      if (WN_operator(o_rhs) == OPR_LDID) {
	Is_True(WN_rtype(o_rhs) == MTYPE_M,
		("expected RETURN_VAL kid not type M"));

	algn = TY_align(ty_idx);
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
	  mtype = RETURN_INFO_mtype(return_info, i);
	  ty_idx_used = Be_Type_Tbl(mtype);
	  Set_TY_align(ty_idx_used, algn);
	  n_rhs = WN_CreateLdid (OPR_LDID, mtype, mtype, 
				 WN_load_offset(o_rhs)
				   + i * MTYPE_byte_size(mtype),
				 WN_st_idx(o_rhs), ty_idx_used);
	  preg    = RETURN_INFO_preg (return_info, i);
	  preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
	  wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, preg, preg_st, 
			     Be_Type_Tbl(mtype), n_rhs);
	  WN_INSERT_BlockLast (block, wn);
	}
      }
      else if (WN_operator(o_rhs) == OPR_ILOAD) {
	Is_True(WN_rtype(o_rhs) == MTYPE_M,
		("expected RETURN_VAL kid not type M"));

	algn = TY_align(ty_idx);
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
          mtype = RETURN_INFO_mtype(return_info, i);
	  ty_idx_used = Be_Type_Tbl(mtype);
	  Set_TY_align(ty_idx_used, algn);
	  if (i == 0)
	    n_rhs = WN_kid0(o_rhs);
	  else n_rhs = WN_COPY_Tree(WN_kid0(o_rhs));
	  n_rhs = WN_CreateIload(OPR_ILOAD, mtype, mtype,
				 WN_load_offset(o_rhs)
				   + i * MTYPE_byte_size(mtype),
				 ty_idx_used,
				 Make_Pointer_Type(ty_idx_used), n_rhs);
	  n_rhs = lower_expr(block, n_rhs, actions);
	  preg    = RETURN_INFO_preg (return_info, i);
          preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
	  wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, preg, preg_st, 
			     Be_Type_Tbl(mtype), n_rhs);
	  WN_INSERT_BlockLast (block, wn);
	}
      }
      else { // MLOAD
	Is_True(WN_operator(WN_kid1(o_rhs)) == OPR_INTCONST,
		("expected RETURN_VAL's MLOAD kid to be of constant size"));
	algn = TY_align(TY_pointed(WN_load_addr_ty(o_rhs)));
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
          mtype = RETURN_INFO_mtype(return_info, i);
	  ty_idx_used = Be_Type_Tbl(mtype);
	  Set_TY_align(ty_idx_used, algn);
	  if (i == 0)
	    n_rhs = WN_kid0(o_rhs);
	  else n_rhs = WN_COPY_Tree(WN_kid0(o_rhs));
	  n_rhs = WN_CreateIload(OPR_ILOAD, mtype, mtype,
				 WN_load_offset(o_rhs)
				   + i * MTYPE_byte_size(mtype),
				 ty_idx_used,
				 Make_Pointer_Type(ty_idx_used), n_rhs);
	  n_rhs = lower_expr(block, n_rhs, actions);
	  preg    = RETURN_INFO_preg (return_info, i);
          preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
	  wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, preg, preg_st, 
			     Be_Type_Tbl(mtype), n_rhs);
	  WN_INSERT_BlockLast (block, wn);
	}
      }

      WN_Delete (o_rhs);
    }
  }

  WN *wn_return = WN_CreateReturn ();
  if ( Cur_PU_Feedback )
    Cur_PU_Feedback->FB_lower_return_val( tree, wn_return );
  tree = wn_return;
  WN_Set_Linenum(tree, upc_srcpos);
  return tree;
}


//Since Parm node allows only integer type, we need to 
//promote char and short to int 
static TYPE_ID get_parm_type(int rtype) {

  if (rtype == MTYPE_I1 || rtype == MTYPE_I2) {
    return MTYPE_I4;
  }
  if (rtype == MTYPE_U1 || rtype == MTYPE_U2) {
    return MTYPE_U4;
  }
  return rtype;
}

static WN* get_TLD_call(TYPE_ID rtype, WN* param, TY_IDX ty_idx) {
  
  WN* kids[1];
  WN* var = WN_CreateParm(rtype, param, ty_idx, WN_PARM_BY_VALUE);
  kids[0] = var;
  return WN_Create_Intrinsic(OPR_INTRINSIC_OP, TY_mtype(Make_Pointer_Type(ty_idx)), MTYPE_V, INTRN_TLD_ADDR, 1, kids);
}

static bool TY_not_shared(TY_IDX idx) {
  
  return !(TY_is_shared(idx) || idx == shared_ptr_idx || idx == pshared_ptr_idx);
}

/*
 * Perform lowering for TLD variables, which must be converted to TLD_ADDR(...) in the output C file
 */
static WN* lower_TLD(WN* block, WN* wn, LOWER_ACTIONS actions) {

  if (!Action(LOWER_UPC_TO_INTR)) {
    return wn;
  }

  ST_IDX idx = WN_st_idx(wn);
  ST st = St_Table[idx];
  TY_IDX ty_idx = ST_type(idx);
  TYPE_ID rtype = TY_mtype(ty_idx);

  int sclass = ST_sclass(st);
  if (TY_not_shared(ty_idx) && 
      ST_class(st) != CLASS_CONST &&
      (sclass == SCLASS_PSTATIC || sclass == SCLASS_FSTATIC || 
       sclass == SCLASS_COMMON || sclass == SCLASS_EXTERN ||
       sclass == SCLASS_UGLOBAL || sclass == SCLASS_DGLOBAL)) {
    //variable is TLD
    

    TY_IDX ptr_ty = Make_Pointer_Type(ty_idx);
    if (WN_field_id(wn) != 0) {
      //storing into a field, set type appropriately
      TY_IDX fld_idx = get_field_type(ty_idx, WN_field_id(wn));
      ptr_ty = Make_Pointer_Type(fld_idx);
      rtype = TY_mtype(fld_idx);
    }

    rtype = get_parm_type(rtype);
    WN* res;
    WN* ld = WN_Ldid(rtype, WN_offset(wn), idx, ty_idx, WN_field_id(wn)); 
    res = get_TLD_call(rtype, ld, ty_idx);
    switch (WN_operator(wn)) {
    case OPR_STID: {
      WN* val = lower_expr(block, WN_kid(wn, 0), actions); 
      return WN_CreateIstore(OPR_ISTORE, MTYPE_V, WN_rtype(val), 0, ptr_ty, val, res, 0); 
    }
    case OPR_LDID:
      return WN_CreateIload(OPR_ILOAD, rtype, rtype, 0, TY_pointed(ptr_ty), ptr_ty, res, 0); 
    default:
      Is_True(0, ("Unexpected Operator in lower_TLD: %d", WN_operator(wn)));
    }
  }
  return wn;
}


/* ====================================================================
 *
 * WN *lower_stmt(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statement <tree>,
 * returning lowered tree.  Usually a single statement is returned, but
 * in some cases, lowering (e.g. of complex stores) produces a BLOCK of
 * statements.
 *
 * ==================================================================== */

static WN *lower_stmt(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  CURRENT_STATE stmtState = pushCurrentState(tree, actions);

  upc_srcpos = WN_Get_Linenum(tree);

  Is_True(OPCODE_is_stmt(WN_opcode(tree)),
	  ("expected statement node, not %s", OPCODE_name(WN_opcode(tree))));
  switch(WN_operator(tree))
  {

  case OPR_PRAGMA:
    if (Action(LOWER_UPC_TO_INTR)) {
      Enter_Consistency_Info(WN_pragma(tree));
      return tree;
    }
    break;
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
    tree = lower_call(block, tree, actions);
    break;

  case OPR_INTRINSIC_CALL:
    if (Action(LOWER_UPC_TO_INTR) && WN_intrinsic(tree) == INTRN_WAIT_SYNC_MEM) {
      //This is the placeholder for the sync() calls inserted by the optimizer
      //We find the correct handle (supplied by the init_call) from the map
      //and generate the sync call here
      extern INTRINSIC WN_Type_To_SyncIntrinsic(TYPE_ID mtype);
      sync_handle_t* handle = (sync_handle_t*) WN_MAP_Get(upc_comm_map, tree);
      if (handle == NULL) {
	fprintf(TFile, "!!!!!!should have a valid handle here: %p\n!!!!", tree);
	//fdump_tree(stderr, tree);
	return NULL;
      } else if (handle->st == NULL) {
	//FIXME:  Having orphaned syncs should be an error, but this currently happen with
	//shared stores whose value is a preg (a blocking call is created for it)
	//Fail_FmtAssertion("sync handle should have been set by this point\n");
	return NULL;
      } else {
	TY_IDX handle_ty_idx = ST_type(handle->st);
	WN* call_wn = WN_Create (OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 1);
	WN_intrinsic(call_wn) = WN_Type_To_SyncIntrinsic(TY_mtype(handle_ty_idx));
	WN_kid0(call_wn) = WN_CreateParm(TY_mtype(handle_ty_idx),
					 WN_Ldid(TY_mtype(handle_ty_idx), 0, handle->st, handle_ty_idx),
					 handle_ty_idx, WN_PARM_BY_VALUE);
	WN* block = WN_CreateBlock();
	WN_INSERT_BlockLast(block, call_wn);
	if (handle->invalidate) {
	  WN* invalidate = WN_Stid(TY_mtype(handle_ty_idx), 0, handle->st, handle_ty_idx, 
				   WN_Ldid(TY_mtype(handle_ty_idx), 0, invalid_handle, handle_ty_idx), 0);
	  WN_INSERT_BlockLast(block, invalidate);
	}
	WN_DELETE_Tree(tree);
	tree = block;
	break;
      }
    }
    
    tree = lower_intrinsic_call(block, tree, actions);
    break;

  case OPR_ISTORE:
  case OPR_STID:
  case OPR_MSTORE: {
    tree = lower_store(block, tree, actions);
    break;
  }

  case OPR_COMPGOTO:
    tree = lower_compgoto(block, tree, actions);
    break;

  case OPR_IO:
    if (Action(LOWER_IO_STATEMENT))
    {
      if (traceIO)
      {
          fputs(DBar, TFile);
          fprintf(TFile, "Io Lower: Before Lowering\n");
          fdump_tree(TFile, tree);
          fputs(DBar, TFile);
      }

      tree = lower_io_statement(tree, actions);

      if (traceIO)
      {
          fputs(DBar, TFile);
          fprintf(TFile, "Io Lower: After Lowering\n");
          fdump_tree(TFile, tree);
          fputs(DBar, TFile);
      }
    }
    break;
  case OPR_ALTENTRY:
    tree = lower_entry(tree, actions);
    break;

  case OPR_EVAL:
    tree = lower_eval(block, tree, actions);
    break;

  case OPR_LABEL:
   /*
    *  lower loop info for CG (it will process these opt>0
    */
    if (WN_label_loop_info(tree))
    {
      WN  *infoblock= WN_CreateBlock();
      WN  *info=      WN_label_loop_info(tree);

      info = lower_expr(infoblock, info, actions);
      WN_set_label_loop_info(tree, info);
    }
    break;

  case OPR_EXC_SCOPE_BEGIN:
   /*
    *  We don't want to lower the kids, which are not used to generate code,
    * but only to provide information to the optimizer. 
    */
    break;

  case OPR_REGION_EXIT:
   /*
    *  convert REGION_EXIT to GOTO
    */
    if (Action(LOWER_REGION_EXITS)) 
      WN_set_opcode(tree, OPC_GOTO);
    break;

  case OPR_ASSERT:
    tree = lower_assert(block, tree, actions);
    break;

  case OPR_TRUEBR:
  case OPR_FALSEBR:
    tree = lower_branch(block, tree, actions);
    break;

  case OPR_RETURN_VAL:
    if (Action(LOWER_RETURN_VAL))
      tree = lower_return_val(block, tree, actions);
    else
      WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
    break;

  case OPR_ASM_STMT:
    {
      // Lower only the input expressions
      INT16 i;
      for (i = 2; i < WN_kid_count(tree); i++) {
	WN_kid0(WN_kid(tree,i)) = lower_expr(block, WN_kid0(WN_kid(tree,i)), actions);
      }
      break;
    }



  default: 
    {
      INT16 i;
      for (i = 0; i < WN_kid_count(tree); i++)
  	WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
    }
  }

  popCurrentState(stmtState);
  return tree;
}

/* ====================================================================
 *
 * WN *replace_ldidPreg(WN *expr, PREG_NUM reg, WN *replacement)
 * 
 *  walk expr looking for an ldid of preg reg.
 *  if found (expect only one of them), replace the ldid with replacement
 *
 *  if replacement is NULL we are doing a dryrun to test if there is a value
 *
 * ==================================================================== */

static WN *replace_ldidPreg(WN *expr, PREG_NUM reg, WN *replacement)
{
  INT16 i;

  if (expr==NULL)
    return NULL;

  for (i = 0; i < WN_kid_count(expr); i++)
  {
    WN	*child= WN_kid(expr, i);

    if ((WN_operator_is(child, OPR_LDID))	&&
	(WN_class(child) == CLASS_PREG)		&&
	WN_load_offset(child) == reg)
    {
      if (replacement)
      {
	WN_kid(expr, i) = replacement;
      }
      return child;
    }

    //UPC functions returning shared pointers have a return mtype of MTYPE_M,
    //which can confuse replace_ldidpreg() since we can't use a preg to store
    //its return value.
    if (Compile_Upc && WN_operator_is(child, OPR_LDID) &&
	WN_st(child) == Return_Val_Preg) {
      if (replacement && !MTYPE_is_m(WN_rtype(child)))
      {
	WN_kid(expr, i) = replacement;
      }
      return child;
    }

    {
      WN  *tree;
      if (tree = replace_ldidPreg(child, reg, replacement))
	return tree;
    }
  }
  return NULL;
}

/* ====================================================================
 *
 * 
 * WN *lower_intrinsic_call(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * intrinsic calls have the special problem of defining a return
 * register that is used in the next stmt
 *
 * It is required that the emulation routine either
 *  [1]		passes the corresponding call node back
 *  	or
 *  [2]		returns a value (typically ldid preg)
 *		this value must replace the return register which
 *		should be in the next whirl statement (fchow says so)
 *
 * ==================================================================== */
static WN *lower_intrinsic_call(WN *block, WN *tree, LOWER_ACTIONS actions)
{ 
  WN		*next, *em, *newBlock;
  INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);
  SRCPOS srcpos = WN_Get_Linenum(tree);
  TYPE_ID	type;
  PREG_NUM	reg1, reg2;
  BOOL intrinsic_lowered;

  Is_True(OPCODE_is_stmt(WN_opcode(tree)),
	  ("expected statement node, not %s", OPCODE_name(WN_opcode(tree))));
  {
   /*
    *  first lower the children
    */
    INT16 i;
    for (i = 0; i < WN_kid_count(tree); i++)
      WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
  }

  if (INTRN_cg_intrinsic(id))
    return tree;

  if (NotAction(LOWER_INTRINSIC |
		LOWER_INLINE_INTRINSIC |
		LOWER_INL_STACK_INTRINSIC)) {
    return tree;
  }
  else if (NotAction(LOWER_INL_STACK_INTRINSIC)) {
    INTRINSIC intr = (INTRINSIC) WN_intrinsic(tree);
    if (intr == INTRN_U4I4SETSTACKPOINTER ||
	intr == INTRN_U8I8SETSTACKPOINTER ||
	intr == INTRN_U4READSTACKPOINTER ||
	intr == INTRN_U8READSTACKPOINTER ||
	intr == INTRN_U4I4ALLOCA ||
	intr == INTRN_U8I8ALLOCA) {
      return tree;
    }
  }

  next = WN_next(tree);
  type = WN_rtype(tree);
  newBlock = WN_CreateBlock();

 /*
  *  see if statement return value exists in the next statement
  *  but don't replace it
  */
  {
    TYPE_ID	ty1, ty2;
    BOOL	returnValueUnused = FALSE;

    if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (MTYPE_To_TY(type),
                                                 Complex_Not_Simulated);

      if (RETURN_INFO_count(return_info) <= 2) {

        ty1  = RETURN_INFO_mtype (return_info, 0);
        ty2  = RETURN_INFO_mtype (return_info, 1);
        reg1 = RETURN_INFO_preg (return_info, 0);
        reg2 = RETURN_INFO_preg (return_info, 1);
      }

      else
        ErrMsg (EC_Unimplemented,
                "lower_intrinsic_call: more than 2 return registers");
    }

    else {
      Get_Return_Mtypes(MTYPE_To_TY(type), Complex_Not_Simulated, &ty1, &ty2);
      Get_Return_Pregs(ty1, ty2, &reg1, &reg2);
    }

    if (MTYPE_is_void(type))
    {
      returnValueUnused = TRUE;
    }
    if (replace_ldidPreg(next, reg1, NULL)==NULL)
    {
      returnValueUnused = TRUE;
    }

    if (returnValueUnused && INTRN_has_no_side_effects(id))
    {
      DevWarn("lower_intrinsic_call(): function %s is void or unused and has"
	      " no_side_effects. It will be removed", get_intrinsic_name(id));

      return newBlock;
    }
  }

  em = lower_emulation(newBlock, tree, actions, intrinsic_lowered);

 /*
  *  Nothing happened (ie. the intrinsic was not an INLINE
  *  and we should go no further
  */
  if ((Action(LOWER_INLINE_INTRINSIC) ||
       Action(LOWER_INL_STACK_INTRINSIC)) && 
      NotAction(LOWER_INTRINSIC)	  &&
      tree == em)
  {
    WN_Delete(newBlock);
    return tree;
  }

  WN_Delete(tree);

  if (OPCODE_is_call(WN_opcode(em)))
  {
    WN_Set_Linenum (em, srcpos);
    WN_INSERT_BlockLast(block, newBlock);
    return em;
  }
  else if (WN_is_block(em))
  {
    WN_INSERT_BlockLast(block, newBlock);
    return em;
  }
  else if (MTYPE_is_void(type))
  {
    if (OPCODE_is_stmt(WN_opcode(em)))
    {
      WN_INSERT_BlockLast(newBlock, em);
    }
    return newBlock;
  }
  else
  {
    /*
     *	so far we only expand __builtin_alloca() and the memset routines.
     *  This code will need to change based on different return sequences
     */
    Is_True((reg1 != 0), ("expected return value from intrinsic_call"));
    Is_True((reg2 == 0), ("cannot evaluate 2 regs into an expression"));
    Is_True((OPCODE_is_expression(WN_opcode(em))), ("expected expression"));
    {
      WN	*replaced = NULL;

      /*
       *  In the next statement replace an ldid of preg reg1 with em
       *  The use of the function may have been deleted, but the function
       *  still may have side effects!
       *
       *  We are guaranteed the result is in the "next" statement BTW
       */
      replaced = replace_ldidPreg(next, reg1, em);

      if (replaced)
      { /* CVTL-RELATED */
        Is_True(Types_are_compatible(WN_rtype(replaced),type),
		("return reg mismatch type"));
      }
      return newBlock;
    }
  }
}




/* ====================================================================
 *
 * WN *lower_block(WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in BLOCK
 * node <tree>, returning (possibly) modified BLOCK.
 *
 * ==================================================================== */

WN *lower_block(WN *tree, LOWER_ACTIONS actions)
{
  WN *out, *node, *next_node;
  CURRENT_STATE blockState;

  Is_True(WN_opcode(tree) == OPC_BLOCK,
	  ("expected BLOCK node, not %s", OPCODE_name(WN_opcode(tree))));

  blockState = pushCurrentState(tree, actions);

  out = WN_CreateBlock();
  WN_Set_Linenum (out, current_srcpos);

  for (node = WN_first(tree); node; node = next_node)
  {

    /*	MP pragmas are handled in a very special manner.  There are three forms
	of parallel constructs - standalone parallel directive, parallel loop
	(doacross) and parallel region.
	A standalone parallel directive starts with an MP pragma node and is
	followed by (depending on the directive) a series of statements ended
	with another, specific MP pragma node.
	Parallel loops start with an MP pragma node followed by a series of MP
	pragmas and assignment statements ended with a DO_LOOP node.
	A parallel region is an OPC_REGION created by the FE which encapsulates
	all statements within the region.  It will always have a func_pragma
	list containing MP pragmas.  To all but the MP lowerer, this region is
	transparent.
	If MP lowering is in effect, then this entire set of nodes needs to be
	removed from the statement chain, translated and replaced with one or
	more statement/scf nodes.  Because a set of nodes must be processed
	as a unit, the usual lowering scheme of passing in a single node (tree)
	and returning a single node (tree) won't work.  */

    while (Action(LOWER_MP) && node &&
      ((((WN_opcode(node) == OPC_PRAGMA) || (WN_opcode(node) == OPC_XPRAGMA))
	&& (WN_pragmas[WN_pragma(node)].users & PUSER_MP)) ||
       ((WN_opcode(node) == OPC_REGION) && WN_first(WN_region_pragmas(node)) &&
        ((WN_opcode(WN_first(WN_region_pragmas(node))) == OPC_PRAGMA) ||
	 (WN_opcode(WN_first(WN_region_pragmas(node))) == OPC_XPRAGMA)) &&
        (WN_pragmas[WN_pragma(WN_first(WN_region_pragmas(node)))].users &
	 PUSER_MP))))
	node = lower_mp(out, node, actions);
    if (node == NULL) break;

    /*
     *  Must read WN_next(node) now since it may be overwritten
     */
    next_node = WN_next(node);
    setCurrentState(node, actions);

    /* WEI: remove PREFETCHS when they're no longer needed 
     *  (they're used in LOWER_UPC_TO_INTR to perform communication overlapping)
     */
    if (Action(LOWER_UPC_INTRINSIC) && WN_operator(node) == OPR_PREFETCH) {
      continue;
    }

    if (OPCODE_is_stmt(WN_opcode(node)))
    {
	node = lower_stmt(out, node, actions);
    }
    else if (OPCODE_is_scf(WN_opcode(node)))
    {
	node = lower_scf(out, node, actions);
    }
    else
    {
	Fail_FmtAssertion("expected statement or SCF node, not %s",
			  OPCODE_name(WN_opcode(tree)));
	/*NOTREACHED*/
    }

    if (node == NULL) // lowering an MSTID of Return_Val_Preg can cause NULL
      continue;

    WN_INSERT_BlockLast(out, node);
  
    // if STID(ALLOCA) and trapuv, insert a mstore
    if ( Action(LOWER_INL_STACK_INTRINSIC) && DEBUG_Trap_Uv
        && WN_operator(node) == OPR_STID
        && WN_operator(WN_kid0(node)) == OPR_ALLOCA)
    {
        lower_trapuv_alloca (out, WN_kid0(node), actions);
    }
  }

 /*
  *  Once we have the integrated block, remove any stmt after
  *  a return or goto, until we hit a label or ALTENTRY
  *  only if we are not called by lower_scf_non_recursive()
  *
  *  Curiously enough, this is needed to prevent dead blocks (-O0)
  *  which causes exception handling to fail ...
  *  
  */
  if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    for (node = WN_first(out); node; node = next_node)
    {
      next_node = WN_next(node);
  
      if (WN_unconditional_goto(node))
      {
        for(node = next_node; node; node = next_node)
        {
	  next_node = WN_next(node);
  
	  if (WN_operator_is(node, OPR_LABEL))
	    break;
	  else if (WN_operator_is(node, OPR_ALTENTRY))
	    break;
	  else if (WN_operator_is(node, OPR_EXC_SCOPE_BEGIN))
	    continue;
	  else if (WN_operator_is(node, OPR_EXC_SCOPE_END))
	    continue;
	  else if (WN_operator_is(node, OPR_PRAGMA))
	    continue;
	  else if (WN_operator_is(node, OPR_XPRAGMA))
	    continue;
	  else if (WN_operator_is(node, OPR_REGION_EXIT))
	    continue;
	  if (OPCODE_is_stmt(WN_opcode(node)))
	  {
	    WN_DELETE_FromBlock(out, node);
	  }
	  else
	    break;
        }
      }
    }
  }

  WN_Delete(tree);

  popCurrentState(blockState);
  return out;
}


/* ====================================================================
 *
 * WN *lower_speculate(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * try to speculate CAND/CIOR to LAND/LIOR
 * Edge frequency data is discarded
 *
 * ==================================================================== */

static WN *lower_speculate(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  INT32	n;
  BOOL  speculate = TRUE;

  if (OPT_Lower_Speculate == FALSE)
    return tree;

  switch(WN_operator(tree))
  {
  case OPR_CAND:
  case OPR_CIOR:
  case OPR_CSELECT:
    for (n = 0; n < WN_kid_count(tree); n++)
    {
      WN_kid(tree, n) =	lower_speculate(block, WN_kid(tree, n), actions);
      speculate &=	expr_is_speculative(WN_kid(tree, n));
    }

    if (speculate)
    {
      if (WN_operator_is(tree, OPR_CAND))
      {
	WN *land = WN_LAND( WN_kid0(tree), WN_kid1(tree) );
	WN_Delete(tree);
	return land;
      }
      else if (WN_operator_is(tree, OPR_CIOR))
      {
	WN *lior = WN_LIOR( WN_kid0(tree), WN_kid1(tree) );
	WN_Delete(tree);
	return lior;
      }
      else if (WN_operator_is(tree, OPR_CSELECT))
      {
	WN *select = WN_Select(WN_rtype(tree), WN_kid0(tree), WN_kid1(tree),
			       WN_kid2(tree));
  
	WN_Delete(tree);
	return select;
      }
    }
    break;
  }

  return tree;
}

/* ====================================================================
 *
 * WN *lower_conditional(WN *block, WN *tree, BOOL branchType,
 *                       LOWER_ACTIONS actions)
 *
 * lower CAND/CIOR to scf
 *
 * ==================================================================== */

static WN *
lower_conditional(WN *block, WN *tree, LABEL_IDX trueLabel,
		  LABEL_IDX falseLabel, BOOL branchType,
		  LOWER_ACTIONS actions)
{
  WN *shortcircuit, *left_branch = NULL, *right_branch = NULL;

  switch(WN_operator(tree))
  {
  case OPR_CAND:
   /*
    *	Process the left child.
    *   We need a label for the children to branch to
    */
    if (WN_operator_is(WN_kid0(tree), OPR_CAND)	||
	WN_operator_is(WN_kid0(tree), OPR_CIOR))
    {
      shortcircuit = WN_NewLabel();

      lower_conditional(block, WN_kid0(tree), WN_label_number(shortcircuit),
			falseLabel, FALSE, actions);
      WN_INSERT_BlockLast(block, shortcircuit);
    }
    else
    {
      left_branch = WN_Falsebr(falseLabel, WN_kid0(tree));
      WN_INSERT_BlockLast(block, left_branch);
    }

   /*
    *	Process the right child.
    */
    if (WN_operator_is(WN_kid1(tree), OPR_CAND) ||
	WN_operator_is(WN_kid1(tree), OPR_CIOR))
    {
      lower_conditional(block, WN_kid1(tree), trueLabel, falseLabel,
			branchType, actions);
    }
    else {
      if ( branchType /*==TRUE*/ ) {
	right_branch = WN_Truebr(trueLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
      else {
	/* branchType == FALSE */
	right_branch = WN_Falsebr(falseLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
    }
    if (Cur_PU_Feedback) {
      Cur_PU_Feedback->FB_lower_circuit( tree, left_branch, right_branch );
    }
    return NULL;

  case OPR_CIOR:
   /*
    *	Process the left child.
    *   We need a label for the children to branch to
    */
    if (WN_operator_is(WN_kid0(tree), OPR_CAND)	||
	WN_operator_is(WN_kid0(tree), OPR_CIOR))
    {
      shortcircuit = WN_NewLabel();

      lower_conditional(block, WN_kid0(tree), trueLabel,
			WN_label_number(shortcircuit), TRUE, actions);
      WN_INSERT_BlockLast(block, shortcircuit);
    }
    else
    {
      left_branch = WN_Truebr(trueLabel, WN_kid0(tree));
      WN_INSERT_BlockLast(block, left_branch);
    }

   /*
    *	Process the right child.
    */
    if (WN_operator_is(WN_kid1(tree), OPR_CAND) ||
	WN_operator_is(WN_kid1(tree), OPR_CIOR))
    {
      lower_conditional(block, WN_kid1(tree), trueLabel, falseLabel,
			branchType, actions);
    }
    else {
      if ( branchType /*==TRUE*/ ) {
	right_branch = WN_Truebr(trueLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
      else {
	/* branchType == FALSE */
	right_branch = WN_Falsebr(falseLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
    }
    if (Cur_PU_Feedback) {
      Cur_PU_Feedback->FB_lower_circuit( tree, left_branch, right_branch );
    }
    return NULL;

  default:
    tree = lower_expr(block, tree, actions);
    break;
  }

  return tree;
}

static WN *lower_branch_condition(BOOL branchType, LABEL_IDX label, WN *cond,
				  WN **branch, LOWER_ACTIONS actions)
{
  WN *condBlock = WN_CreateBlock();

  cond = lower_speculate(condBlock, cond, actions);

  *branch = NULL;

  switch(WN_operator(cond))
  {
  case OPR_CAND:
  case OPR_CIOR:
    {
      LABEL_IDX shortcircuit_lbl;
      shortcircuit_lbl = NewLabel();

      if (branchType == TRUE)
      {
	cond = lower_conditional(condBlock, cond, label, shortcircuit_lbl,
				 branchType, actions);
      }
      else
      {
	cond = lower_conditional(condBlock, cond, shortcircuit_lbl, label,
				 branchType, actions);
      }

      condBlock = lower_block(condBlock, actions);
      WN_INSERT_BlockLast(condBlock, WN_Label(shortcircuit_lbl));
    }
    break;
  default:
    {
      cond = lower_expr(condBlock, cond, actions);

      if (branchType)
	*branch = WN_Truebr(label, cond);
      else
	*branch = WN_Falsebr(label, cond);
      WN_INSERT_BlockLast(condBlock, *branch);
    }
  }
  return condBlock;
}

#ifdef SHORTCIRCUIT_HACK
/* return TRUE if the expression <tree> has a CAND/CIOR that cannot
 * be converted into LAND/LIOR respectively.
 */
static BOOL tree_has_cand_cior (WN *tree)
{
  WN_ITER *wni;
  WN *wn;

  for (wni = WN_WALK_TreeIter (tree); 
       wni != NULL;
       wni = WN_WALK_TreeNext (wni))
  {
    wn = WN_ITER_wn (wni);
    /* TODO: check if the CAND/CIOR can be converted to LAND/LIOR */
    if (WN_operator_is(wn, OPR_CAND)	||
	WN_operator_is(wn, OPR_CIOR)	||
	WN_operator_is(wn, OPR_CSELECT))
      return TRUE;
  }
  return FALSE;
}
#endif

/* ====================================================================
 *
 * WN *lower_if(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in IF
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_if(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_IF,
	  ("expected IF node, not %s", OPCODE_name(WN_opcode(tree))));

  if (WN_Is_If_MpVersion(tree))
    return lower_block(lower_mp(block, tree, actions), actions);

#ifndef SHORTCIRCUIT_HACK
  if (Action(LOWER_IF))
#else
  if (Action(LOWER_IF) || 
      (Action(LOWER_SHORTCIRCUIT) && tree_has_cand_cior(WN_if_test(tree))))
#endif
  {
   /*
    *  Lower IF.  This is done differently depending on
    *  whether the "then" or "else" clauses are empty.
    * 
    *  Pay close attention to the block scope and state while creating nodes
    */
    WN *body = WN_CreateBlock();
    WN *wn_branch = NULL;
    LABEL_IDX cont_lbl;
    cont_lbl = NewLabel();

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }

    if (WN_block_nonempty(WN_then(tree)))
    {
      if (WN_block_nonempty(WN_else(tree)))
      {
       /*
        * Both "then" and "else" clauses exist.  Generate:
        * 	(FALSEBR <cond> <else_lbl>)
        *	<then_clause>
        *	(GOTO <cont_lbl>)
        *	(LABEL <else_lbl>)
        *	<else_clause>
        *	(LABEL <cont_lbl>)
        */
	LABEL_IDX else_lbl;
	else_lbl = NewLabel();

	WN *falsebr_block = lower_falsebr(else_lbl, WN_if_test(tree),
					  &wn_branch, actions);
	WN_INSERT_BlockFirst(body, falsebr_block);

	setCurrentStateBlockLast(WN_then(tree), actions);
	WN_INSERT_BlockLast(body, lower_block(WN_then(tree), actions));
	WN *wn_goto = WN_Goto(cont_lbl);
	WN_INSERT_BlockLast(body, wn_goto);

	WN *wn_else_lbl = WN_Label(else_lbl);
	WN_INSERT_BlockLast(body, wn_else_lbl);
	setCurrentStateBlockLast(WN_else(tree), actions);
	WN_INSERT_BlockLast(body, lower_block(WN_else(tree), actions));
	WN *wn_cont_lbl = WN_Label(cont_lbl);
	WN_INSERT_BlockLast(body, wn_cont_lbl);
      } 
      else
      {
       /*
        * Only "then" clause exists.  Generate:
        *	(FALSEBR <cond> <cont_lbl>)
        *	<then_clause>
        *	(LABEL <cont_lbl>)
        */
	WN *falsebr_block = lower_falsebr(cont_lbl, WN_if_test(tree),
					  &wn_branch, actions);
	WN_INSERT_BlockFirst(body, falsebr_block);

	setCurrentStateBlockLast(WN_then(tree), actions);
	WN_INSERT_BlockLast(body, lower_block(WN_then(tree), actions));
	WN *wn_cont_lbl = WN_Label(cont_lbl);
	WN_INSERT_BlockLast(body, wn_cont_lbl);
      }

      if (Cur_PU_Feedback) {
	Cur_PU_Feedback->FB_lower_branch( tree, wn_branch );
      }

      WN_Delete(tree);
      return body;
    } 
    else if (WN_block_nonempty(WN_else(tree)))
    {
     /*
      * Only "else" clause exists.  Generate:
      *		(TRUEBR <cond> <cont_lbl>)
      *		<else_clause>
      *		(LABEL <cont_lbl>)
      */
      WN *truebr_block = lower_truebr(cont_lbl, WN_if_test(tree),
				      &wn_branch, actions);
      WN_INSERT_BlockFirst(body, truebr_block);

      setCurrentStateBlockLast(WN_else(tree), actions);
      WN_INSERT_BlockLast(body, lower_block(WN_else(tree), actions));
      WN *wn_cont_lbl = WN_Label(cont_lbl);
      WN_INSERT_BlockLast(body, wn_cont_lbl);

      if (Cur_PU_Feedback) {
	Cur_PU_Feedback->FB_lower_branch( tree, wn_branch );
      }

      WN_Delete(tree);
      return body;
    }
    else
    {
     /*
      * Neither "then" or "else" clause exists.  Generate:
      *		(EVAL <cond>)
      */
      WN *eval;

      eval = WN_CreateExp1(OPC_EVAL, lower_expr(block, WN_if_test(tree),
						actions));
      WN_Set_Linenum (eval, current_srcpos);

      if (Cur_PU_Feedback) {
	Cur_PU_Feedback->FB_lower_branch( tree, NULL );
      }

      WN_DELETE_Tree(WN_then(tree));
      WN_DELETE_Tree(WN_else(tree));
      WN_Delete(tree);
      return eval;
    }
  }
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN_if_test(tree) = lower_expr(block, WN_if_test(tree),
				  RemoveShortCircuitAction(actions));
    WN_then(tree) = lower_block(WN_then(tree), actions);
    WN_else(tree) = lower_block(WN_else(tree), actions);
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_do_loop(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in DO_LOOP
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_do_loop(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN *loop_info;

  Is_True(WN_opcode(tree) == OPC_DO_LOOP,
	  ("expected DO_LOOP node, not %s", OPCODE_name(WN_opcode(tree))));

  loop_info = WN_do_loop_info(tree);  
  loop_nest_depth = loop_info ? WN_loop_depth(loop_info) : loop_nest_depth+1;

  if (Action(LOWER_DO_LOOP))
  {
   /*
    *  Lower DO_LOOP.  Generate:
    *	<start>
    *	(FALSEBR <end> <cont_lbl>)		; unless nz_trip flag set
    *	(LABEL <top_lbl> <loop_info>)
    *	<body>
    *	<step>
    *	(TRUEBR <end> <top_lbl>)
    *  (LABEL <cont_lbl>)			; unless nz_trip flag set
    */
    BOOL nz_trip = loop_info && WN_Loop_Nz_Trip(loop_info);
    WN *wn_top_branch = NULL, *wn_back_branch = NULL;
    WN *body = WN_CreateBlock();

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }

    contains_a_loop = FALSE;
   /*
    *  create loop info for CG
    *  it must be lowered as CG processes this
    */
    if (loop_info == NULL)
    {
      WN *infoblock = WN_CreateBlock();
      WN *trip_count = WN_LOOP_TripCount(tree);

      /* Set the nz_trip if we can */
      if (trip_count && WN_operator_is(trip_count, OPR_INTCONST) &&
	  WN_const_val(trip_count) > 0) {
	 nz_trip = TRUE;
      }
      loop_info = WN_CreateLoopInfo(WN_index(tree),
				    trip_count,
				    0,
				    loop_nest_depth,
				    contains_a_loop ? 0 : WN_LOOP_INNERMOST);
      loop_info = lower_expr(infoblock, loop_info, actions);
      WN_Set_Loop_Nz_Trip(loop_info);
      WN_DELETE_Tree(infoblock);
    }
    else if (WN_loop_induction(loop_info) != WN_index(tree))
    {
      WN_DELETE_Tree(WN_index(tree));
    }

    WN_INSERT_BlockLast(body, lower_stmt(block, WN_start(tree), actions));

    WN *cont_lbl;
    if (nz_trip == FALSE)
    {
      WN *end = lower_copy_tree(WN_end(tree), actions);

      if (Cur_PU_Feedback)
	Cur_PU_Feedback->FB_clone_loop_test( WN_end(tree), end, tree );

      cont_lbl = WN_NewLabel();
      WN *top_branch_block = lower_falsebr(WN_label_number(cont_lbl), end,
					   &wn_top_branch, actions);
      WN_INSERT_BlockLast(body, top_branch_block);
    }

    setCurrentState(WN_do_body(tree), actions);

    LABEL_IDX top_lbl_idx;
    top_lbl_idx = NewLabel();

    WN *top_lbl = WN_CreateLabel(ST_IDX_ZERO, top_lbl_idx, 0, loop_info);
    WN_INSERT_BlockLast(body, top_lbl);

    WN_INSERT_BlockLast(body, lower_block(WN_do_body(tree), actions));

    setCurrentState(WN_step(tree), actions);
    WN_INSERT_BlockLast(body, lower_stmt(block, WN_step(tree), actions));
    WN *back_branch_block = lower_truebr(WN_label_number(top_lbl),
					 WN_end(tree), &wn_back_branch,
					 actions);
    WN_INSERT_BlockLast(body, back_branch_block);

    if (nz_trip == FALSE) {
      WN_INSERT_BlockLast(body, cont_lbl);
    }

    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_loop( tree, wn_top_branch, wn_back_branch );

    WN_Delete(tree);

    tree = body;
  }
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN_start(tree) = lower_stmt(block, WN_start(tree), actions);
    WN_end(tree) = lower_expr(block, WN_end(tree), actions);
    WN_step(tree) = lower_stmt(block, WN_step(tree), actions);
    WN_do_body(tree) = lower_block(WN_do_body(tree), actions);
  }

  --loop_nest_depth;
  contains_a_loop = TRUE;
  return tree;
}



/* ====================================================================
 *
 * WN *lower_do_while(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in DO_WHILE
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_do_while(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_DO_WHILE,
	  ("expected DO_WHILE node, not %s", OPCODE_name(WN_opcode(tree))));

  ++loop_nest_depth;
#ifndef SHORTCIRCUIT_HACK
  if (Action(LOWER_DO_WHILE))
#else
  if (Action(LOWER_DO_WHILE) || 
      (Action(LOWER_SHORTCIRCUIT) && tree_has_cand_cior (WN_while_test(tree))))
#endif
  {
   /*
    * Lower DO_WHILE.  Generate:
    *	(LABEL <top_lbl>)
    *	<body>
    *	(TRUEBR <test> <top_lbl>)
    */
    WN *body = WN_CreateBlock();
    WN *top_lbl = WN_NewLabel();

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }
    WN_INSERT_BlockFirst(body, top_lbl);
    WN_INSERT_BlockLast(body, lower_block(WN_while_body(tree), actions));

    WN *wn_back_branch = NULL;
    WN *back_branch_block
      = lower_truebr(WN_label_number(top_lbl), WN_while_test(tree),
		     &wn_back_branch, actions);
    WN_INSERT_BlockLast(body, back_branch_block);

    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_loop( tree, NULL, wn_back_branch );

    WN_Delete(tree);
    tree = body;
  }
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN *testBlock = WN_CreateBlock();

    WN_while_body(tree) = lower_block(WN_while_body(tree), actions);

    WN_while_test(tree) = lower_expr(testBlock, WN_while_test(tree), actions);

    WN_INSERT_BlockLast(WN_while_body(tree), testBlock);
  }

  --loop_nest_depth;
  contains_a_loop = TRUE;
  return tree;
}

//map the old label name to the new one
static map<int, int> label_map;
static hash_set<int> labels;

static WN *lower_while_do(WN *block, WN *tree, LOWER_ACTIONS actions);

/**
 *
 * rename recursively all labels in the wn 
 *
 * Questionable ones: REGION, REGION_EXIT, AGOTO, COMMA
 */
static WN* rename_labels(WN* wn) {
  SRCPOS srcpos = WN_Get_Linenum(wn);

  WN* result;
  switch ( WN_operator(wn) ) {
    
  case OPR_BLOCK: {
    result = WN_CreateBlock();
    WN_Set_Linenum(result, srcpos);
    WN* next = WN_first(wn);
    while (next != NULL) {
      WN_INSERT_BlockLast(result, rename_labels(next));
      next = WN_next(next);
    }
    break;
  }

  case OPR_DO_WHILE:
    result = WN_CreateDoWhile(WN_COPY_Tree(WN_while_test(wn)), rename_labels(WN_while_body(wn)));
    WN_Set_Linenum(result, srcpos);
    break;

  case OPR_WHILE_DO:
    result = WN_CreateWhileDo(WN_COPY_Tree(WN_while_test(wn)), rename_labels(WN_while_body(wn)));
    WN_Set_Linenum(result, srcpos);
    break;

  case OPR_IF:
    result = WN_CreateIf(WN_COPY_Tree(WN_if_test(wn)), rename_labels(WN_then(wn)), rename_labels(WN_else(wn)));
    WN_Set_Linenum(result, srcpos);
    break;

  case OPR_CASEGOTO:
  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_GOTO:
  case OPR_LABEL: {
    result = WN_COPY_Tree(wn);
    int label = WN_label_number(wn);
    if (WN_operator(wn) == OPR_GOTO) {
      //catch the break out of affinity loop case
      if (labels.find(label) == labels.end()) {
	//cout << "label not present: " << label << endl;
	break;
	//this is a goto to the end of loop, so we don't rename it
      }
    }

    if (label_map[label] == 0) {
      LABEL_IDX new_label; 
      LABEL_Init (New_LABEL (CURRENT_SYMTAB, new_label), 0, LKIND_DEFAULT);
      label_map[label] = new_label;
    }
    //update the label number
    WN_label_number(result) = label_map[label];
    break;
  }

  case OPR_SWITCH: 
    result = WN_COPY_Tree(wn);
    for (int i = 0; i < WN_kid_count(result); i++) {
      WN_kid(result, i) = rename_labels(WN_kid(result, i));
    }
    break;

  default:
    result = WN_COPY_Tree(wn);
  }

  return result;
}

/**
 *
 * find all labels the wn tree
 */
static void find_labels(WN* wn) {
 
  switch ( WN_operator(wn) ) {
    
  case OPR_BLOCK: {
    WN* next = WN_first(wn);
    while (next != NULL) {
      find_labels(next);
      next = WN_next(next);
    }
    break;
  }

  case OPR_DO_WHILE:
    find_labels(WN_while_body(wn));
    break;

  case OPR_WHILE_DO:
    find_labels(WN_while_body(wn));
    break;

  case OPR_IF:
    find_labels(WN_then(wn));
    find_labels(WN_else(wn));
    break;

  case OPR_SWITCH: 
    for (int i = 0; i < WN_kid_count(wn); i++) {
      find_labels(WN_kid(wn, i));
    }
    break;

  case OPR_LABEL: 
    labels.insert(WN_label_number(wn));
    break;
  default:
    break;
  }
}

/***
 *
 * Replace in wn all references of the form shared[subscript] into *arr
 * , which is an equivalent local pointer for shared 
 */
static WN* use_private(WN* wn, ST_IDX arr, ST_IDX shared, WN* subscript) {

  WN* result;
  switch (WN_operator(wn)) {
  case OPR_BLOCK: {
    result = WN_CreateBlock();
    WN* next = WN_first(wn);
    while (next != NULL) {
      WN* tmp = WN_next(next);
      WN_INSERT_BlockLast(result, use_private(next, arr, shared, subscript));
      next = tmp;
    }
    return result;
  }
  case OPR_DO_WHILE:
    return WN_CreateDoWhile(WN_COPY_Tree(WN_while_test(wn)), use_private(WN_while_body(wn), arr, shared, subscript));
  case OPR_WHILE_DO:
    return WN_CreateWhileDo(WN_COPY_Tree(WN_while_test(wn)), use_private(WN_while_body(wn), arr, shared, subscript));
  case OPR_IF:
    return WN_CreateIf(WN_COPY_Tree(WN_if_test(wn)), use_private(WN_then(wn), arr, shared, subscript), 
			 use_private(WN_else(wn), arr, shared, subscript));
  case OPR_ISTORE: 
    // the case of A[i] = ...
    //fall thru to iload case
  case OPR_ILOAD: {
    result = WN_COPY_Tree(wn);
    if (WN_operator(result) == OPR_ISTORE) {
      WN_kid0(result) = use_private(WN_kid0(result), arr, shared, subscript);
    }
    //the case of exp = A[i] ...
    WN* addr = (WN_operator(result) == OPR_ISTORE) ? WN_kid1(result) : WN_kid0(result);
    if (WN_operator(addr) == OPR_ARRAY) {
      ST_IDX st = WN_st_idx(WN_kid0(addr));
      int dim = WN_kid_count(addr) >> 1;
      if (dim > 1) {
	/* don't support two dimension for now */
	return result;
      }
      WN* exp = WN_kid2(addr);
      if (st == shared && WN_Equiv(exp, subscript)) { /* FIXME: this does not compare children */
	WN_kid0(addr) = WN_Ldid(Pointer_Mtype, 0, ST_ptr(arr), ST_type(ST_ptr(arr)), 0);
	WN_kid2(addr) = WN_Intconst(MTYPE_I4,0);
	if (WN_operator(result) == OPR_ISTORE) {
	  WN_set_ty(result, ST_type(ST_ptr(arr)));
	} else {
	  WN_set_load_addr_ty(result, ST_type(ST_ptr(arr)));
	}
      }
    }
    return result;
  }
  default:
    result = WN_COPY_Tree(wn);
    for (int i = 0; i < WN_kid_count(result); i++) {
      WN_kid(result, i) = use_private(WN_kid(result, i), arr, shared, subscript);
    }
    return result;
  }
}

/* 
 * when aff_wn is an array reference of the form
 *  Add
 *    Lda arr
 *    Some integer expression
 *
 * replace all identical references
 * in the loop with an equiv private access
 */
static WN* fix_While_Body(WN* while_body, WN* affinity_control_wn) {

  WN* result;
  if (affinity_control_wn == NULL) {
    return while_body;
  }

  WN* aff_wn = Strip_TAS(affinity_control_wn);

  if (WN_operator(aff_wn) != OPR_ADD ||
      WN_operator(WN_kid0(aff_wn)) != OPR_LDA)  {
    return while_body;
  }
  ST* addr_st = ST_ptr(WN_st_idx(WN_kid0(aff_wn))); 
  TY_IDX ty = ST_type(addr_st);
  if (TY_kind(ty) != KIND_ARRAY) {
    /* for now we only operate on global arrays */
    return while_body;
  }
  TY_IDX elt_type = Get_Inner_Array_Type(ty);

  TY_IDX lty_idx;
  TY& lty = New_TY(lty_idx); /* the equiv non-shared type */
  TY_Init (lty, TY_size(elt_type), TY_kind(elt_type), TY_mtype(elt_type),
	   TY_name_idx(elt_type));
  Set_TY_flags(lty_idx, TY_flags(elt_type));
  Set_TY_align(lty_idx, TY_align(elt_type));
  Clear_TY_is_shared(lty_idx);
  ST* local = Gen_Temp_Symbol(Make_Pointer_Type(lty_idx), ".local");
  result = use_private(while_body, ST_st_idx(local), WN_st_idx(WN_kid0(aff_wn)), WN_kid1(affinity_control_wn));

  WN* cast_stmt = WN_Create(OPR_TAS, Pointer_Mtype, MTYPE_V, 1);
  //WN_kid0(cast_stmt) = WN_Lda(Pointer_Mtype, 0, addr_st, 0);
  WN_kid0(cast_stmt) = aff_wn;
  WN_set_ty(cast_stmt, ST_type(local));
  cast_stmt = WN_Stid(Pointer_Mtype, 0, local, lty_idx, cast_stmt, 0);
  WN_INSERT_BlockFirst(result, cast_stmt);

  return result;
}

static WN* get_Aff_exp(WN* wn) {

  WN* tmp = wn;
  if (tmp == NULL) {
    return NULL;
  } else if (WN_operator(tmp) == OPR_INTRINSIC_CALL &&
      WN_intrinsic(tmp) == INTRN_AFF_S || WN_intrinsic(tmp) == INTRN_AFF_P) {
    return WN_kid0(WN_kid0(tmp));
  } else {
    return get_Aff_exp(WN_prev(tmp));
  }
}

/**
 *
 * wn is a while_do loop of the form
 * i = S;
 * while (test) {
 *   aff_exp
 *   if (aff) {
 *      loop_body
 *   }
 *   #pragma
 *   cont_label:
 *   loop_incr
 * }
 *
 * Which we tranform to
 * if (forall_ctrl) {
 *    i = S;
 *    while (test) {
 *      loop_body_1
 *    }
 *    c_label1:
 *    loop_incr
 * } else {
 *    forall_ctrl = 1;
 *    i = S;
 *    while (test) {
 *      aff_exp
 *      if (aff) {
 *         loop_body
 *      }
 *      c_label:
 *      loop_incr
 *    }
 *    forall_ctrl = 0;
 * }
 *
 * Where body_1 and body are identical except the labels in 1 are renamed to avoid duplicate labels
 * Note that we need replicate the assignment to loop induction variable so that the optimizer
 * can recognize the whiledo as a do loop later
 * 
 */

static WN* lower_forall_ctrl(WN* block, WN* tree, WN* prag, LOWER_ACTIONS actions) {

  //return tree;
  //reinit the data structures
  label_map.clear();
  labels.clear();

  SRCPOS srcpos = WN_Get_Linenum(tree);

  /* Search backwards for the WN_PRAGMA_UPC_FORALL (see bug 2690) */
  WN *forall_pragma = tree;
  do {
    forall_pragma = WN_prev(forall_pragma);
    FmtAssert(forall_pragma != NULL,
	      ("pragma forall should appear before a forall loop"));
  } while (WN_operator(forall_pragma) != OPR_PRAGMA ||
	   WN_pragma(forall_pragma) != WN_PRAGMA_UPC_FORALL);

  /* Init expression, if any, preceeds the pragma */
  WN* init = WN_prev(forall_pragma);
  if (init == NULL || WN_operator(init) != OPR_STID) {
    //the forall loop does not contain a initialization expression
    // In the event of some unrelated OPR_STID preceeding a upc_forall()
    // it will be harmlessly duplicated in both the 'then' and 'else' blocks.
    init = NULL;
  }

  //fdump_tree(stderr, tree);

  WN* test = WN_while_test(tree);
  //This relies on the front end always inserting the pragma right after the affinity test
  WN* aff_body = WN_prev(prag);
  while (aff_body != NULL && WN_operator(aff_body) != OPR_IF) {
    aff_body = WN_prev(aff_body);
  }
  FmtAssert(WN_opcode(aff_body) == OPC_IF,
	    ("expected IF node for affinity test, not %s", OPCODE_name(WN_opcode(tree))));

  WN* loop_body = WN_then(aff_body);
  WN* label = WN_next(WN_next(aff_body));  
  WN* incr = WN_next(label);

  WN* if_expr = WN_Ldid(MTYPE_I4, 0, upc_forall_control_st, ST_type(upc_forall_control_st));

  find_labels(tree);
  loop_body = rename_labels(loop_body);

  label = WN_COPY_Tree(label);
  int label_num = WN_label_number(label);
  if (label_map[label_num] == 0) {
    LABEL_IDX new_label; 
    LABEL_Init (New_LABEL (CURRENT_SYMTAB, new_label), 0, LKIND_DEFAULT);
    label_map[label_num] = new_label;
  }
  //update the label number
  WN_label_number(label) = label_map[label_num];  
  
  WN_INSERT_BlockLast(loop_body, label);
  for (; incr != NULL; incr = WN_next(incr)) {
    WN_INSERT_BlockLast(loop_body, WN_COPY_Tree(incr));
  }

  WN* new_loop = WN_CreateWhileDo(WN_COPY_Tree(test), loop_body);
  new_loop = lower_while_do(block, new_loop, actions);
  WN_Set_Linenum(new_loop, srcpos);
  WN* then_body = WN_CreateBlock();
  WN_Set_Linenum(then_body,srcpos);
  if (init != NULL) {
    WN_INSERT_BlockLast(then_body, WN_COPY_Tree(init));
  }
  WN_INSERT_BlockLast(then_body, new_loop);
  
  WN* else_body = WN_CreateBlock();
  WN_Set_Linenum(else_body,srcpos);
  WN* control_val = WN_Intconst(MTYPE_I4, 1); 
  WN* set_control = WN_Stid(MTYPE_I4, 0, upc_forall_control_st, ST_type(upc_forall_control_st), control_val);
  WN_Set_Linenum(set_control,srcpos);
  WN_INSERT_BlockLast(else_body, set_control);
  if (init != NULL) {
    WN_INSERT_BlockLast(else_body, WN_COPY_Tree(init));
  }
  WN_INSERT_BlockLast(else_body, WN_COPY_Tree(forall_pragma));
  WN_INSERT_BlockLast(else_body, tree);
  control_val = WN_Intconst(MTYPE_I4, 0);
  set_control = WN_Stid(MTYPE_I4, 0, upc_forall_control_st, ST_type(upc_forall_control_st), control_val);
  WN_Set_Linenum(set_control,srcpos);
  WN_INSERT_BlockLast(else_body, set_control);

  WN_DELETE_FromBlock(block, forall_pragma);
  if (init != NULL) {
    WN_DELETE_FromBlock(block, init);
  }

  WN* result = WN_CreateIf(if_expr, then_body, else_body);
  WN_Set_Linenum(result,srcpos);
  return result;
} 


/* ====================================================================
 *
 * WN *lower_while_do(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Lower WHILE_DO.  Generate:
 *	(FALSEBR (<test>) <cont_lbl>)
 *	(LABEL <top_lbl>)
 *	<body>
 *	(TRUEBR <test> <top_lbl>)
 *  (LABEL <cont_lbl>)
 *
 *
 * Perform lowering (see WN_Lower description) on statements in WHILE_DO
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_while_do(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_WHILE_DO,
	  ("expected WHILE_DO node, not %s", OPCODE_name(WN_opcode(tree))));

  ++loop_nest_depth;

  //WEI: see if we have a forall_ctrl loop
  if (Action(LOWER_UPC_FORALL)) {
    WN* body = WN_while_body(tree);
    for (WN* prag = WN_last(body); prag != NULL; prag = WN_prev(prag)) {
      if (WN_opcode(prag) == OPC_PRAGMA && 
	  WN_pragma(prag) == WN_PRAGMA_UPC_FORALL_AFFINITY) {
	tree = lower_forall_ctrl(block, tree, prag, actions);
	break;
      }
    }
  }

#ifndef SHORTCIRCUIT_HACK
  if (Action(LOWER_WHILE_DO))
#else
  if (Action(LOWER_WHILE_DO) || 
      (Action(LOWER_SHORTCIRCUIT) && tree_has_cand_cior (WN_while_test(tree))))
#endif
  {
   /*
    *	(FALSEBR <test> <cont_lbl>) into block
    *	(LABEL <top_lbl>)
    *	<body>
    *	(TRUEBR <test> <top_lbl>)
    *  (LABEL <cont_lbl>)
    */
    LABEL_IDX top_lbl, cont_lbl;
    top_lbl  = NewLabel();
    cont_lbl = NewLabel();

    WN *test = lower_copy_tree( WN_while_test(tree), actions);
    WN *body = WN_CreateBlock();
    if (Cur_PU_Feedback)
      Cur_PU_Feedback->FB_clone_loop_test( WN_while_test(tree), test, tree );

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }

    WN *wn_top_branch = NULL;
    WN *top_branch_block = lower_falsebr(cont_lbl, test, &wn_top_branch,
					 actions);
    WN_INSERT_BlockLast(block, top_branch_block);

    setCurrentState(WN_while_body(tree), actions);
    WN *wn_top_lbl = WN_Label(top_lbl);
    WN_INSERT_BlockLast(body, wn_top_lbl);

    setCurrentStateBlockLast(WN_while_body(tree), actions);
    WN_INSERT_BlockLast(body, lower_block(WN_while_body(tree), actions));

    WN *wn_back_branch = NULL;
    WN *back_branch_block = lower_truebr(top_lbl, WN_while_test(tree),
					 &wn_back_branch, actions);
    WN_INSERT_BlockLast(body, back_branch_block);
    WN *wn_cont_lbl = WN_Label(cont_lbl);
    WN_INSERT_BlockLast(body, wn_cont_lbl);

    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_loop( tree, wn_top_branch, wn_back_branch );

    WN_Delete(tree);

    tree = body;
  }
 /*
  * We're not lowering WHILE_DOs, so just lower children.
  * The semantics of the WHILE_DO require any statement level whirl
  * created during the lowering of the while test be copied to both the
  * block (before the WHILE_DO) and in the end of while_body
  */
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN *copytestBlock;
    WN *testBlock = WN_CreateBlock();

    WN_while_body(tree) = lower_block(WN_while_body(tree), actions);

    WN_while_test(tree) = lower_expr(testBlock, WN_while_test(tree), actions);
    copytestBlock = lower_copy_tree(testBlock, actions);

    if (Cur_PU_Feedback)
      Cur_PU_Feedback->FB_clone_loop_test( testBlock, copytestBlock, tree );

    WN_INSERT_BlockLast(block, copytestBlock);
    WN_INSERT_BlockLast(WN_while_body(tree), testBlock);
  }

  --loop_nest_depth;
  contains_a_loop = TRUE;
  return tree;
}



/* ====================================================================
 *
 *  The parameter types may not be correct, and as usual, we have to
 *  make up for it. Integral types are canonicalized [I,U][1,2] types
 *  (to [I,U]4) and do not require explicit conversion
 *  (the conversion happens when the value is loaded)
 *
 *  Mostly floats are passed as doubles and must be converted 
 *
 * ==================================================================== */
static WN *lower_promoted_formal(PLOC ploc, ST *formalST)
{
  WN		*ldid, *cvt;
  TYPE_ID	regType, formalType;

  formalType = TY_mtype(Ty_Table[ST_type(formalST)]);

  regType = Mtype_comparison(TY_mtype(Promoted_Parm_Type(formalST)));

  if (!PLOC_on_stack(ploc))
  {
    formalST = MTYPE_To_PREG(regType);
  }

  ldid = WN_Ldid(regType, PLOC_reg(ploc), formalST, ST_type(formalST));

  cvt =	WN_Type_Conversion(ldid, formalType);

  return cvt;
}

/* ====================================================================
 *
 * PLOC lower_formals(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 *  lower formal tree returning a ploc
 *
 * ==================================================================== */

static void lower_formals(WN *block, WN *formal, PLOC ploc,
			  LOWER_ACTIONS actions)
{
  BOOL    altentry = PU_has_altentry(Get_Current_PU());
  ST     *sym      = WN_st(formal);
  TY_IDX  formalTY = WN_type(formal);

  if (PLOC_on_stack(ploc))
  {
    /* on stack already */
    if (altentry)
    {
      ST	*upformal;
      WN	*ldid, *stid;

      upformal = Get_Altentry_UpFormal_Symbol (sym, ploc);

      ldid = WN_Ldid(TY_mtype(Ty_Table[formalTY]), 0, upformal, formalTY);

      stid = WN_Stid(TY_mtype(Ty_Table[formalTY]), 0, sym, formalTY, ldid);

      stid = lower_store(block, stid, actions);
      WN_Set_Linenum (stid, current_srcpos);
      WN_INSERT_BlockLast(block, stid);
    }
    else if (ST_promote_parm(sym))
    {
      if (MTYPE_is_float(TY_mtype(Ty_Table[formalTY])))
      {
	WN	*cvt;
	WN	*stid;

	cvt  = lower_promoted_formal(ploc, sym);
	stid = WN_Stid(TY_mtype(Ty_Table[formalTY]), 0, sym,
				formalTY, cvt);

	WN_Set_Linenum (stid, current_srcpos);
	WN_INSERT_BlockLast(block, stid);
      }
    }
  }
  else if (MTYPE_is_m(TY_mtype(Ty_Table[formalTY])))
  {
    /* structure parameter */
    lower_mload_formal (block, formal, ploc, actions);
  }
  else
  {
    WN	*cvt, *stid;

    cvt  = lower_promoted_formal(ploc, sym);

    stid = WN_Stid(TY_mtype(Ty_Table[formalTY]), 0, sym, formalTY, cvt);

    WN_Set_Linenum (stid, current_srcpos);
    WN_INSERT_BlockLast(block, stid);
  }
}



/* ====================================================================
 *
 * PLOC lower_entry_formals(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on ENTRY and ALTENTRY nodes
 *
 * for LOWER_ENTRY_EXIT
 *		lower all but sclass formal refs
 * for LOWER_ENTRY_FORMAL_REF
 *		lower just sclass formal refs
 *
 * ==================================================================== */

static PLOC lower_entry_formals(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  PLOC	ploc;
  INT32	i, n;

  ploc = Setup_Input_Parameter_Locations(ST_pu_type(WN_st(tree)));

  if (WN_opcode(tree) == OPC_ALTENTRY)
  {
    n =	WN_kid_count(tree);
    Reset_UPFORMAL_Segment();
  }
  else
  {
    n =	WN_num_formals(tree);
  }

  for (i = 0; i < n; i++)
  {
    WN *formal = WN_formal(tree, i);
	
    if (WN_sclass(formal) == SCLASS_FORMAL_REF)
    {
      ST *base;

      base = Get_ST_formal_ref_base(WN_st(formal));
      ploc = Get_Input_Parameter_Location(ST_type(base));

      if (NotAction(LOWER_ENTRY_FORMAL_REF))
      {
	continue;
      }
      else
      {
	formal = WN_CreateIdname(WN_idname_offset(formal), base);
      }
    }
    else
    {
      ploc = Get_Input_Parameter_Location(WN_type(formal));

      if (NotAction(LOWER_ENTRY_EXIT))
      {
	continue;
      }
    }
    lower_formals(block, formal, ploc, actions);
  }

  return ploc;
}

static WN *
Create_Mcount_Call (void)
{
    // don't do anything here,
    // because ia64 mcount call is non-standard calling convention,
    // so implement it in target-dependent cg.
    return NULL;
}

/* ====================================================================
 *
 * WN *lower_entry(WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on ENTRY and ALTENTRY nodes
 *
 * for ENTRY
 *		return entry node
 * for ALTENTRY
 *		return block with lowered formals 
 *
 * for LOWER_ENTRY_EXIT
 *		create trapuv code (uninitialized variable)
 *		create varargs register assignment
 *		create slink sym initialization
 *
 * for LOWER_RETURN_VAL
 *		create fake first parameter for struct return if needed
 *
 * ==================================================================== */

static WN *lower_entry(WN *tree, LOWER_ACTIONS actions)
{
  PLOC	ploc;
  WN	*block;

  Is_True(((WN_opcode(tree) == OPC_FUNC_ENTRY)
	   || (WN_opcode(tree) == OPC_ALTENTRY)),
          ("expected ENTRY/ALTENTRY node, not %s",
	   OPCODE_name(WN_opcode(tree))));

  if (WHIRL_Return_Val_On && Action(LOWER_RETURN_VAL) && 
      WN_operator(tree) == OPR_FUNC_ENTRY && !Compile_Upc)
  { // create fake first parameter for struct return if needed 
    ST_IDX func_stidx = WN_st_idx(tree);
    PU_IDX puidx = ST_pu(St_Table[func_stidx]);
    TY_IDX prototype = PU_prototype(Pu_Table[puidx]);
    RETURN_INFO return_info = Get_Return_Info(TY_ret_type(prototype), 
					      Complex_Not_Simulated);
    if (RETURN_INFO_return_via_first_arg(return_info)) {
      ST *return_st = Gen_Temp_Symbol(
			      Make_Pointer_Type(TY_ret_type(prototype), FALSE),
			      Index_To_Str(Save_Str2(".return.",
						       ST_name(func_stidx))));
      Set_ST_sclass(return_st, SCLASS_FORMAL);
      Set_ST_is_value_parm(return_st);
      WN *idname = WN_CreateIdname(0, return_st);
      // create the new func_entry node
      WN *n_tree = WN_CreateEntry(WN_num_formals(tree)+1, func_stidx,
				  WN_func_body(tree), WN_func_pragmas(tree),
				  WN_func_varrefs(tree));
      WN_Set_Linenum(n_tree, WN_Get_Linenum(tree));
      WN_map_id(n_tree) = WN_map_id(tree);
      WN_kid0(n_tree) = idname;
      for (INT32 i = 0; i < WN_kid_count(tree); i++)
	WN_kid(n_tree, i+1) = WN_kid(tree, i);
      // fix pu pointer from RID
      if (RID_map != WN_MAP_UNDEFINED) {
        RID *rid = (RID *)WN_MAP_Get(RID_map, tree);
        if (RID_rwn(rid) == tree)
	  RID_rwn(rid) = n_tree;
      }
      WN_Delete(tree);
      tree = n_tree;
    }
  }

  setCurrentState(tree, actions);
  block =		WN_CreateBlock();
  
  if (Action(LOWER_ENTRY_EXIT))
  {
    ploc = lower_entry_formals(block, tree, actions);

    if (TY_is_varargs(Ty_Table[PU_prototype(Pu_Table[ST_pu(WN_st(tree))])]))
    {
     /*
      *  For varargs, the func-entry just has the list of fixed
      *  parameters, so also have to store the vararg registers. 
      */
      TYPE_ID	type = Def_Int_Mtype;

      if (PLOC_is_nonempty(ploc) && !PLOC_on_stack(ploc)) {
	/* don't do if already reached stack params */
        ploc = Get_Vararg_Input_Parameter_Location (ploc);
      }

      while (!PLOC_on_stack(ploc))
      {
       /*
        *  vararg registers must be integer registers
        */
	WN	*wn;
	ST	*st;

	wn = WN_Ldid (type, PLOC_reg(ploc), Int_Preg, ST_type(Int_Preg));
       /*
	*  get the symbol for the vararg formal
	*/
	st = Get_Vararg_Symbol (ploc);

	wn = WN_Stid (type, 0, st, ST_type(st), wn);
	WN_Set_Linenum (wn, current_srcpos);
	WN_INSERT_BlockLast(block, wn);

	ploc = Get_Vararg_Input_Parameter_Location (ploc);
      }
    }

   /*
    * add initialization code for trapuv
    */
    {
      WN *trapuvBlock = WN_CreateBlock();

      trapuvBlock = lower_trapuv(trapuvBlock, tree, actions);

      trapuvBlock= lower_block(trapuvBlock, actions);

      WN_INSERT_BlockLast(block, trapuvBlock);
    }
  }
  else if (Action(LOWER_ENTRY_FORMAL_REF))
  {
    ploc = lower_entry_formals(block, tree, actions);
  }

  if (Action(LOWER_SPLIT_SYM_ADDRS))
  {
    /*
     *  Initialize the static link if required ($2 should have callers $fp)
     */
    ST *slink = Find_Slink_Symbol(CURRENT_SYMTAB);
    if (slink)
    {
      WN	*ld, *wn;

      ld = WN_LdidPreg(Pointer_type, Static_Link_Preg_Offset);

      wn = WN_Stid (Pointer_type, 0, slink, ST_type(slink), ld);

      WN_Set_Linenum (wn, current_srcpos);
      WN_INSERT_BlockFirst(block, wn);
    }
  }

  if (WN_opcode(tree) == OPC_FUNC_ENTRY)
  {
    /* Process any PU-scope MP pragmas */
    if (WN_func_pragmas(tree) && Action(LOWER_MP)) {
      WN *wn;
      for (wn = WN_first(WN_func_pragmas(tree)); wn; wn = WN_next(wn)) {
	if (((WN_opcode(wn) == OPC_PRAGMA) || (WN_opcode(wn) == OPC_XPRAGMA))
	    && (WN_pragmas[WN_pragma(wn)].users & PUSER_MP)) {
	  (void) lower_mp(NULL, wn, actions);
	}
      }
    }

    if (Call_Mcount && Action(LOWER_ENTRY_EXIT)) {
	WN *profcall = Create_Mcount_Call ();
	if (profcall) {	// possible that we won't create call yet
		WN_Set_Linenum (profcall, current_srcpos);
		WN_INSERT_BlockLast(block, profcall);
	}
    }

    WN_INSERT_BlockFirst(WN_func_body(tree), block);

    /* Lower statements in function body. */
    WN_func_body(tree) = lower_block(WN_func_body(tree), actions);
    return tree;
  }
  else
  {
    block  = lower_block(block, actions);
    WN_INSERT_BlockFirst(block, tree);
  }
  return block;
}




/* ====================================================================
 *
 * WN *lower_region(WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on REGION node
 * <tree> returning lowered REGION tree.
 *
 * ==================================================================== */

static WN *lower_region(WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(WN_region_body(tree)) == OPC_BLOCK,
	  ("kid of REGION should be OPC_BLOCK, not %s",
	   OPCODE_name(WN_opcode(WN_region_body(tree)))));

  setCurrentState(tree, actions);
  if (current_function == NULL) {
    // haven't seen FUNC_ENTRY yet
    current_function = PU_Info_tree_ptr(Current_PU_Info);
  }

  if (Action(LOWER_REGION))
  {
    RID *rid = REGION_get_rid( tree );

    Is_True(rid, ("expected valid region id"));

    max_region++;

   /*
    *  first time thru. Set region lowered flags
    *  otherwise remove flags that have already been processed.
    */
    if (RID_lowered(rid) == (LOWER_ACTIONS) NULL)
    {
      RID_lowered(rid) = actions;
    }
    else
    {
      actions ^= (RID_lowered(rid) & (LOWER_CALL | LOWER_ENTRY_EXIT));
    }

    if (actions)
    {
      WN_region_body(tree) = lower_block(WN_region_body(tree), 
					 actions | LOWER_REGION);
    }
  }
  else
  {
    WN_region_body(tree) = lower_block(WN_region_body(tree), actions);
  }

  return tree;
}

WN *lower_scf_non_recursive(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  return lower_scf(block,tree,actions | LOWER_TOP_LEVEL_ONLY);
}

/* ====================================================================
 *
 * WN *lower_scf(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on structured control
 * flow node <tree>.  Returned tree will always have a structured
 * control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_scf(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  CURRENT_STATE scfState = pushCurrentState(tree, actions);

  upc_srcpos = WN_Get_Linenum(tree);

  switch (WN_opcode(tree))
  {
  case OPC_DO_WHILE:
    block = lower_do_while(block, tree, actions);
    break;

  case OPC_WHILE_DO:
    block = lower_while_do(block, tree, actions);
    break;

  case OPC_DO_LOOP:
    block = lower_do_loop(block, tree, actions);
    break;
      
  case OPC_IF:
    block = lower_if(block, tree, actions);
    break;

  case OPC_BLOCK:
    block = lower_block(tree, actions);
    break;
    
  case OPC_REGION:
    block = lower_region(tree, actions);
    break;
  }

  popCurrentState(scfState);
  return block;
}


/* ====================================================================
 *
 * WN *lower_trapuv(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Initialize stack variables as per trapuv
 *
 * ==================================================================== */

static WN *lower_trapuv(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  if ( DEBUG_Trap_Uv == FALSE)
    return block;

  {
    ST	*st;
    ST  *slink = Find_Slink_Symbol(CURRENT_SYMTAB);

    INT32 i;
    FOREACH_SYMBOL(CURRENT_SYMTAB, st, i)
    {
      TY_IDX  type;
      TYPE_ID btype;
      INT32   size;

      if (ST_class(st) != CLASS_VAR)
	continue;

      if (ST_sclass(st) != SCLASS_AUTO)
	continue;

      if (Has_Base_Block(st))
	continue;

      if (ST_is_uplevelTemp(st) || st == slink)
	continue;

      if (ST_is_not_used(st)) 
	continue;
      if (Is_Allocated(st))
	continue;

      type  = ST_type(st);
      btype = TY_mtype(Ty_Table[type]);
      size  = TY_size(type);

      Is_True(ST_pu_defined(st), ("trapuv auto or temp not ST_pu_defined"));

      switch(TY_kind(type))
      {
      case KIND_SCALAR:
      case KIND_POINTER:
      case KIND_FUNCTION:
        {
	  WN  *con, *stid;

	  Is_True((MTYPE_RegisterSize(btype) == size),
		  ("bad size for scalar/pointer"));;

	  con = WN_UVConst(btype);

	  stid = WN_Stid(btype, 0, st, type, con);
	  WN_Set_Linenum(stid, WN_Get_Linenum(tree));
	  WN_INSERT_BlockLast(block, stid);
        }
	break;

      case KIND_ARRAY:
      case KIND_STRUCT:
	{
	 /*
	  *  Assign bit pattern just on basis of size.
	  *  We cannot truncate the pattern, so we set ST_use_reg_align();
	  *  We always start at offset 0 (we know it is aligned)
	  *  and replicate the following pattern  (0xFFFA5A5A)
	  *
	  *	size	right-justify		left-justify
	  *	----	-------------		------------
	  *	1	5A			FF
	  *	2	5A5A			FFFA
	  *	3	FA5A5A			FFFA5A
	  *	4	FFFA5A5A		FFFA5A5A
	  *	5	5AFFFA5A 5A		FFFA5A5A FF
	  *	6	5A5AFFFA 5A5A		FFFA5A5A FFFA
	  *	7	FA5A5AFF FA5A5A		FFFA5A5A FFFA5A
	  *
	  *  We do the assignment in chunks, special casing the
	  *  last assigment.
	  *  These weird patterns are only relevant for character
	  *  and structure assignment
	  */
	  TY_IDX	ptype = Make_Pointer_Type(type);
	  WN           *conUV4 = NULL;
	  INT32		todo = size;
	  INT32		offset = 0;
	  UINT32        ncon;
	  TYPE_ID       q;

	  for (q= Max_Uint_Mtype; q!=MTYPE_UNKNOWN; q= Mtype_prev_alignment(q))
	  {
	    WN	   *con, *lda, *num, *store;
	    INT32  qSize   = MTYPE_RegisterSize(q);
	    INT32  qBits   = MTYPE_size_reg(q);
	    INT32  nMoves  = todo / qSize;
	    INT32  residue = todo % qSize;

	    if (q >= MTYPE_U4)
	    { 
	      if (nMoves)
	      {
		con = WN_UVConst(q);

  		if ( DEBUG_Trap_Uv_Rjustify )
  		{
		  con = WN_RotateIntconst(con, residue*8);
  		}

		lda = WN_Lda(Pointer_type, 0, st);
		num = WN_Intconst(MTYPE_I4, nMoves * qSize);

		store = WN_CreateMstore(offset, ptype, con, lda, num);

		WN_Set_Linenum(store, WN_Get_Linenum(tree));
		WN_INSERT_BlockLast(block, store);

		todo   -= (nMoves * qSize);
		offset += (nMoves * qSize);
	      }
	    }
	    else
	    {
	     /*
	      *  very tricky residue code (size 1,2,3).
	      */
	      if (todo > 0)
	      {
		if (conUV4==NULL)
		{
 		  conUV4 = WN_UVConst(MTYPE_U4);
		  ncon   = WN_const_val(conUV4);
		}
		if (nMoves)
		{
		  if ( DEBUG_Trap_Uv_Rjustify )
		  {
		    con = WN_Intconst(MTYPE_U4, ncon>>(todo*8 - qBits));
		  }
		  else
		  {
		    con = WN_Intconst(MTYPE_U4,
				      ncon>>(MTYPE_size_reg(MTYPE_U4)-qBits));
		    ncon <<= qBits;
		  }

		  store = WN_Stid(q, offset, st, type, con);
		  WN_Set_Linenum(store, WN_Get_Linenum(tree));
		  WN_INSERT_BlockLast(block, store);
		
		  todo   -= (nMoves * qSize);
		  offset += (nMoves * qSize);
		}
	      }
	    }
	  }
	  if (conUV4)
	    WN_Delete(conUV4);

	}
	break;
      }
    }
  }
  return block;
}


static void lower_actions_fprintf(FILE *f, LOWER_ACTIONS actions)
{
  LOWER_ACTIONS i = 1;

  while (actions)
  {
    if (Action(i))
    {
      fprintf(f, "%s ", LOWER_ACTIONS_name(i));
      actions = actions ^ i;
    }
    i <<= 1;
  }
  fprintf(f, "\n");
}


/* ====================================================================
 *
 * const char * LOWER_ACTIONS_name(LOWER_ACTIONS actions)
 *
 * Exported.  See interface description in "wn_lower.h".
 *
 * ==================================================================== */
const char * LOWER_ACTIONS_name(LOWER_ACTIONS actions)
{
  if ((actions-1) & actions)
  {
    DevWarn("LOWER_ACTION_name(0x%llx): expected only one flag at a time",
	    actions);
  }

  switch(actions)
  {
  case LOWER_NULL:			return "LOWER_NULL";
  case LOWER_DO_LOOP:			return "LOWER_DO_LOOP";
  case LOWER_DO_WHILE:			return "LOWER_DO_WHILE";
  case LOWER_WHILE_DO:			return "LOWER_WHILE_DO";
  case LOWER_IF:			return "LOWER_IF";
  case LOWER_COMPLEX:			return "LOWER_COMPLEX";
  case LOWER_ARRAY:			return "LOWER_ARRAY";
  case LOWER_SPLIT_CONST_OFFSETS:	return "LOWER_SPLIT_CONST_OFFSETS";
  case LOWER_ENTRY_EXIT:		return "LOWER_ENTRY_EXIT";
  case LOWER_CALL:			return "LOWER_CALL";
  case LOWER_SPLIT_SYM_ADDRS:		return "LOWER_SPLIT_SYM_ADDRS";
  case LOWER_IO_STATEMENT:		return "LOWER_IO_STATEMENT";
  case LOWER_MSTORE:			return "LOWER_MSTORE";
  case LOWER_CVT:			return "LOWER_CVT";
  case LOWER_MP:			return "LOWER_MP";
  case LOWER_8X_ARRAY:			return "LOWER_8X_ARRAY";
  case LOWER_INTRINSIC:			return "LOWER_INTRINSIC";
  case LOWER_INLINE_INTRINSIC:		return "LOWER_INLINE_INTRINSIC";
  case LOWER_INL_STACK_INTRINSIC:	return "LOWER_INL_STACK_INTRINSIC";
  case LOWER_REGION:			return "LOWER_REGION";
  case LOWER_QUAD:			return "LOWER_QUAD";
  case LOWER_COMPGOTO:			return "LOWER_COMPGOTO";
  case LOWER_MADD:			return "LOWER_MADD";
  case LOWER_TOP_LEVEL_ONLY:		return "LOWER_TOP_LEVEL_ONLY";
  case LOWER_PREFETCH_MAPS:		return "LOWER_PREFETCH_MAPS";
  case LOWER_ALIAS_MAPS:		return "LOWER_ALIAS_MAPS";
  case LOWER_DEPGRAPH_MAPS:		return "LOWER_DEPGRAPH_MAPS";
  case LOWER_PARITY_MAPS:		return "LOWER_PARITY_MAPS";
    // NOTE: Delete LOWER_FREQUENCY_MAPS
  case LOWER_FREQUENCY_MAPS:		return "LOWER_FREQUENCY_MAPS";
  case LOWER_PICCALL:			return "LOWER_PICCALL";
  case LOWER_BASE_INDEX:		return "LOWER_BASE_INDEX";
  case LOWER_TO_CG:			return "LOWER_TO_CG";
  case LOWER_ASSERT:			return "LOWER_ASSERT";
  case LOWER_FORMAL_REF:		return "LOWER_FORMAL_REF";
  case LOWER_UPLEVEL:			return "LOWER_UPLEVEL";
  case LOWER_ENTRY_FORMAL_REF:		return "LOWER_ENTRY_FORMAL_REF";
  case LOWER_SHORTCIRCUIT:		return "LOWER_SHORTCIRCUIT";
  case LOWER_TREEHEIGHT:		return "LOWER_TREEHEIGHT";
  case LOWER_RETURN_VAL:		return "LOWER_RETURN_VAL";
  case LOWER_MLDID_MSTID:		return "LOWER_MLDID_MSTID";
  case LOWER_BIT_FIELD_ID:		return "LOWER_BIT_FIELD_ID";
  case LOWER_BITS_OP:			return "LOWER_BITS_OP";
  default:				return "<unrecognized>";
  }
}

static const char * MSTORE_ACTIONS_name(MSTORE_ACTIONS actions)
{
  switch(actions)
  {
  case MSTORE_aggregate:		return "scalar moves    ";
  case MSTORE_loop:			return "generate loop   ";
  case MSTORE_intrinsic_bzero:		return "intrinsic bzero ";
  case MSTORE_intrinsic_memset:		return "intrinsic memset";
  case MSTORE_intrinsic_bcopy:		return "intrinsic bcopy";
  default:				return "<unrecognized>";
  }
}


void WN_Lower_Checkdump(char *msg, WN *tree, LOWER_ACTIONS actions)
{
  traceAlignment   = Get_Trace(TP_LOWER, 0x004);
  traceSplitSymOff = Get_Trace(TP_LOWER, 0x010);
  traceIO          = Get_Trace(TP_LOWER, 0x020);
  traceSpeculate   = Get_Trace(TP_LOWER, 0x040);
  traceTreeHeight  = Get_Trace(TP_LOWER, 0x080);
  traceMload       = Get_Trace(TP_LOWER, 0x100);
  // traceUplevel    = Get_Trace(TP_LOWER, 0x200);

  if (Get_Trace(TP_LOWER, 0x008))
  {
    enable_tree_freq_display();
  }

  if (Get_Trace(TKIND_IR, TP_LOWER))
  {
    fputs(DBar, TFile);
    fprintf(TFile, "WN_Lower: \"%s\"\n", msg);
    if (actions)
    {
      fprintf(TFile, "flags are:\n");
      lower_actions_fprintf(TFile, actions);
    }
    fdump_tree(TFile, tree);
    fputs(DBar, TFile);
  }

  if (Get_Trace(TP_LOWER, 0x001))
  {
    IR_dump_map_info = TRUE;

    fprintf(TFile, "WN_Lower: LNO DEP GRAPH\n");
    LNOPrintDepGraph(TFile);
  }
  if (Get_Trace(TP_LOWER, 0x002))
  {
    IR_dump_map_info = TRUE;

    fprintf(TFile, "WN_Lower: WOPT ALIAS INFO\n");
    fdump_dep_tree(TFile, tree, alias_manager);
  }

  if (Get_Trace(TKIND_SYMTAB,TP_LOWER)) {
    fprintf(TFile,"\n\n========== Symbol tables after Lowering ==========\n");
    Print_symtab (TFile, GLOBAL_SYMTAB);
    Print_symtab (TFile, CURRENT_SYMTAB);
  }

  /*
   * these options can lead to infinite regress
   */
  if (Action(LOWER_SPLIT_SYM_ADDRS))
  {
    if (Enable_WN_Simp && WN_Simp_Fold_ILOAD)
    {
      DevWarn("disabling option WN_Simp_Fold_ILOAD"
	      " while lowering action LOWER_SPLIT_SYM_ADDRS");
      WN_Simp_Fold_ILOAD = FALSE;
    }
  }
  /*
   * get any relevant pragmas
   * I need to know if wopt was run to generate some eval warnings
   */
  {
    WN	*pragma=	NULL;
	  
    traceWoptFinishedOpt=	FALSE;

    switch(WN_operator(tree))
    {
    case OPR_FUNC_ENTRY:
      pragma=	WN_func_pragmas(tree);
      break;
    case OPR_REGION:
      pragma=	WN_region_pragmas(tree);
      break;
    }
    if (pragma)
    {
      WN	*wn;
      for(wn= WN_first(pragma); wn; wn= WN_next(wn))
      {
	if (WN_pragma(wn) == WN_PRAGMA_WOPT_FINISHED_OPT)
	  traceWoptFinishedOpt=	TRUE;
      }
    }
  }
}


/* ====================================================================
 *
 * LOWER_ACTIONS lower_to_cg(LOWER_ACTIONS, LOWER_ACTIONS)
 *
 * The last lowering is special in that it may require lowering
 * already specified and lowering that has been avoided
 * 
 * Some lowering should not be done more than once (ENTRY_EXIT, CALL)
 *
 * Keep track and add/subtract any lowering already processed
 *
 * ==================================================================== */

static LOWER_ACTIONS lower_actions(WN *pu, LOWER_ACTIONS actions)
{
  if (OPT_Lower_Treeheight && Action(LOWER_TO_CG))
    actions |= LOWER_TREEHEIGHT;

  if (Action(LOWER_TO_CG))
  {
   /*
    *  remove/add one-time-only and must-apply flags
    */
    actions |=  lowering_actions ^ (LOWER_ENTRY_EXIT		|
				    LOWER_ENTRY_FORMAL_REF	|
				    LOWER_FORMAL_REF		|
				    LOWER_UPLEVEL		|
				    LOWER_SPLIT_SYM_ADDRS	|
				    LOWER_CALL			|
				    LOWER_RETURN_VAL);

   /*
    * remove these always
    *
    * disabling SPLIT_CONST will allow the lowerer to put together addresses
    * that have been pulled apart
    */
    actions &=  ~(LOWER_BASE_INDEX | LOWER_SPLIT_CONST_OFFSETS);

   /*
    * these form the major LOWER_TO_CG actions
    */
    actions |=	LOWER_SCF		  |
		LOWER_ARRAY		  |
		LOWER_MP		  |
		LOWER_IO_STATEMENT	  |
		LOWER_MSTORE		  |
		LOWER_CVT		  |
		LOWER_COMPGOTO		  |
		LOWER_COMPLEX		  |
		LOWER_QUAD		  |
		LOWER_MADD		  |
		LOWER_INTRINSIC		  |
		LOWER_ASSERT		  |
		LOWER_PICCALL		  |
		LOWER_ALL_MAPS		  |
		LOWER_SHORTCIRCUIT	  |
		LOWER_INL_STACK_INTRINSIC |
		LOWER_INLINE_INTRINSIC	  |
		LOWER_BIT_FIELD_ID;
   /*
    *  do not split divides into mul/recip at the CG lowering
    */
    save_Div_Split_Allowed = Div_Split_Allowed;
    Div_Split_Allowed = FALSE;;
  }
  if (WN_opcode(pu) == OPC_FUNC_ENTRY)
  {
    lowering_actions |= actions;
  }

  if (Action(LOWER_BITS_OP))
    actions |= LOWER_BIT_FIELD_ID;

  current_actions = actions;

  lower_maps_init(actions);

  setCurrentState(pu, actions);

  return actions;
}




/* ====================================================================
 *
 * Lower_Init(void)
 *
 * lowering specific initialization
 * ==================================================================== */

void Lower_Init(void)
{
   static CURRENT_STATE current_state_NULL;
   /*
    *  create map for marking parity
    */
   lowering_parity_map = WN_MAP32_Create(MEM_pu_pool_ptr);

   lowering_actions = 0;
   current_state = current_state_NULL;

   // save parity_map in an array becuse it may change in nested PUs
   parity_map_index++;
   FmtAssert(0 <= parity_map_index && parity_map_index < PARITY_MAP_ARRAY_SIZE,
             ("Lower_Init: Index into parity map array is out of range"));
   parity_map_array[parity_map_index] = lowering_parity_map;
}

void Lowering_Finalize(void)
{
  /* free lowering_parity_map */
  WN_MAP_Delete(lowering_parity_map);
  
  parity_map_index--;
  if (parity_map_index >= 0) {
    // if there is a saved parity map, then restore it
    lowering_parity_map = parity_map_array[parity_map_index];
  }
  else {
    // otherwise, set it to undefined
    lowering_parity_map = WN_MAP_UNDEFINED;
  }
}




static void lower_end(WN *tree, LOWER_ACTIONS actions)
{
  lower_maps_reset(actions);

  if (Action(LOWER_TO_CG))
  {
    Div_Split_Allowed = save_Div_Split_Allowed; /* reset */
  }

  popCurrentState(current_state);
}

/* ====================================================================
 *
 * WN *WN_Lower(WN *tree, LOWER_ACTIONS actions)
 *
 * Exported.  See interface description in "wn_lower.h".
 *
 * ==================================================================== */

WN *WN_Lower(WN *tree, LOWER_ACTIONS actions, struct ALIAS_MANAGER *alias,
	     char *msg)
{
#ifdef BACK_END
  Start_Timer(T_Lower_CU);
#endif
  alias_manager =	alias;
  loop_nest_depth =	0;

  // Don't do any lowering on trees that merely wrap up file-scope
  // assembly language code.
  if (WN_operator(tree) == OPR_FUNC_ENTRY &&
      ST_asm_function_st(*WN_st(tree))) {
    return tree;
  }

  actions = lower_actions(tree, actions);

  if (Action(LOWER_MP)) {
    /* Initialize the MP lowerer.
     * Note: We're assuming that for MP-lowering,
     * this routine (WN_Lower) is called once per PU.
     */
    LowerMP_PU_Init ();
  }

  /*
  if (Action(LOWER_UPC_CONSISTENCY)) {
    LowerUPC_Init_Consistency();
  }
  */

  WN_Lower_Checkdump(msg, tree, actions);

  if (WN_opcode(tree) == OPC_FUNC_ENTRY)
  {
    tree = lower_entry(tree, actions);
  }
  else if (OPCODE_is_scf(WN_opcode(tree)))
  {
    tree = lower_scf(NULL, tree, actions);
  }
  else if (OPCODE_is_stmt(WN_opcode(tree)))
  {
    tree = lower_stmt(NULL, tree, actions);
  }
  else if (OPCODE_is_expression(WN_opcode(tree)))
  {
    tree = lower_expr(NULL, tree, actions);
  }

  lower_end(tree, actions);

  WN_Lower_Checkdump("After lowering", tree, 0);

#ifdef BACK_END
  Stop_Timer(T_Lower_CU);
#endif
  
  if(!Action(LOWER_UPC_TO_INTR) && !Action(LOWER_UPC_MFIELD))
    WN_verifier(tree);

  return tree;
}
