/* UPC shared pointer expression semantics, for GNU compiler.
   Copyright (C) 1988, 92-99, 2001 Free Software Foundation, Inc.
  Original Implementation by Jesse M. Draper <jdraper@super.org>
  and William W. Carlson <wwc@super.org>.
  Ported to SGI Irix 6.5 and the gcc 2.95.2 baseline by
  Gary Funck <gary@intrepid.com> and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "machmode.h"
#include "rtl.h"
#include "tree.h"
#include "c-tree.h"
#include "obstack.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "except.h"
#include "function.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "insn-config.h"
/* Include upc-expr.h after insn-config.h so we get HAVE_conditional_move. */
/* #include "upc-expr.h" */
#include "recog.h"
#include "output.h"
#include "typeclass.h"
#include "defaults.h"
#include "toplev.h"
#include "upc-act.h"
#include "upc-share.h"
#include "wfe_misc.h"

/* Chain of all pending loops.  */
extern struct nesting *loop_stack;	/* from stmt.c */

static rtx expand_get	PARAMS((enum machine_mode, rtx, rtx, tree));
static rtx expand_put	PARAMS((enum machine_mode, rtx, rtx, tree));
static rtx expand_remote_address	PARAMS((tree, int, tree,
					       enum machine_mode));
static int is_valid_shared_ptr_p	PARAMS((tree));
static int static_lvalue_p		PARAMS((tree));
static int strict_p		PARAMS((tree));
static tree find_base_type		PARAMS((tree));


/* TODO - replace this with command line parsing */
int threads_int = 0;
tree threads_log =  NULL_TREE;

/* An rtx sequence used to fetch the value of THREADS */
rtx threads_rtx;

/* UPC support functions */
rtx upc_barrier_libfunc;
rtx upc_notify_libfunc;
rtx upc_wait_libfunc;
rtx upc_getaddr_libfunc;



/* return true if current consistency state is "strict" */

static
int
strict_p (exp)
    tree exp;
{
  /* for now, always strict */
  return 1;
}

rtx
expand_upc_shared_component_ref (exp, target, mode, modifier)
    tree exp;
    rtx target;
    enum machine_mode mode;
    enum expand_modifier modifier;
{
  
  fatal("expand_upc_shared_component_ref()  not implemented yet\n");
  return NULL_RTX;
}

int
upc_shared_ptr_cvt_op_p (exp)
    tree exp;
{
  return TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == POINTER_TYPE
      && (TYPE_SHARED (TREE_TYPE (TREE_TYPE (exp)))
	  != TYPE_SHARED (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0)))));
}

rtx
expand_upc_shared_ptr_cvt (exp, target, modifier)
    tree exp;
    rtx target;
    enum expand_modifier modifier;
{

  //fatal("%s  not implemented yet\n", __FUNCTION__);
  //wei: replace __FUNCTION__ with __func__, which is part of C99 standard
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

tree
upc_shared_pointer_int_sum (resultcode, ptrop, intop)
     enum tree_code resultcode;
     tree ptrop;
     tree intop;
{
     tree size_exp;

  register tree result;
  register tree folded;
  /* The result type is a pointer of the same type that is being added,
     after dropping the shared qualifier (for PTS's that happen
     to live in shared memory). */
  tree ttype = TREE_TYPE (ptrop);
  int shared_quals = (TYPE_QUAL_SHARED | TYPE_QUAL_STRICT | TYPE_QUAL_RELAXED);
  int quals_minus_shared = TYPE_QUALS (ttype) & ~shared_quals;
  tree result_type = c_build_qualified_type (ttype, quals_minus_shared);
  tree newptrop = ptrop;

  STRIP_NOPS(newptrop);
  if (! (TREE_CODE (newptrop) == ADDR_EXPR
         && TREE_CODE (TREE_OPERAND (newptrop, 0)) == COMPONENT_REF))
    /* then we have a pointer to a shared object.  For pointers to
       simple objects, just build a "resultcode" tree with the intop and
       let expand_expr handle the arithmetic correctly.  For pointers to
       arrays, compute the number of elements represented by the intop
       and build a "resultcode" tree with the ptrop and that number. */
    {
      tree result;
      tree base_type = get_inner_array_type (result_type);
      if (TREE_TYPE (result_type) != base_type
          && TREE_CODE (TYPE_SIZE (TREE_TYPE (result_type))) == INTEGER_CST)
        {
          tree elt_cnt;
          int size = TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (result_type)));
          int elt_size = TREE_INT_CST_LOW (
                          TYPE_SIZE (get_inner_array_type (result_type)));
          elt_cnt = size_int (size / elt_size);
	  //wei: for multi-dimensional arrays 
	  //we want to know if the specific dimension has a THREADS multiplier,
	  //not whether the high-level type has one
          //if (UPC_TYPE_USES_THREADS (TREE_TYPE (result_type))) {	  
	  if (UPC_TYPE_HAS_THREADS (TREE_TYPE (result_type))) {
            elt_cnt = build (MULT_EXPR, sizetype, elt_cnt,
                             lookup_name (get_identifier ("THREADS")));
	  }
          intop = convert (sizetype, intop);
          intop = build (MULT_EXPR, sizetype, intop, elt_cnt);
        }
      else if (TREE_TYPE (result_type) != base_type)
        {
          tree elt_cnt;
          tree size = TYPE_SIZE (TREE_TYPE (result_type));
          tree elt_size = TYPE_SIZE (get_inner_array_type (result_type));
          elt_cnt = build (EXACT_DIV_EXPR, sizetype, size, elt_size);
          intop = convert (sizetype, intop);
          intop = build (MULT_EXPR, sizetype, intop, elt_cnt);
        }
      result = build (resultcode, result_type, ptrop, intop);
      return fold (result);
    }
  else
    {
      tree size;
      if (TREE_CODE (TREE_TYPE (newptrop)) == POINTER_TYPE
          && TREE_CODE (TREE_TYPE (TREE_TYPE (newptrop))) == ARRAY_TYPE)
        /* Then we have an array that is a field in a shared
           structure.  We have to scale the intop here by the size of
           the array.  Subsequent dimensions are handled by special code
           in build_array_ref. */
        {
	  tree inner = TREE_TYPE(TREE_TYPE(TREE_TYPE(newptrop)));
	  size = size_in_bytes (TREE_TYPE (result_type));
	  if(TREE_CODE(inner) == ARRAY_TYPE) {
	    inner = get_inner_array_type (result_type);
	    inner = size_in_bytes(inner);
	    size = build (EXACT_DIV_EXPR, sizetype, size, inner);
	  } else {
	    size = size_one_node;
	  }
	  
	  
          intop = convert (sizetype, intop);
          intop = fold (build (MULT_EXPR, sizetype, intop, size)); 
        }
      /* The main point of importance here is that this addition is
         normal because we are just calculating the correct offset within
         a structure whose shared address is computed elsewhere. */
      return (build (resultcode, result_type, ptrop, intop));
    }

}

tree
upc_shared_pointer_diff (op0, op1)
     tree op0;
     tree op1;
{
 register tree result, folded;
 tree restype = ptrdiff_type_node;
 
  tree target_type = TREE_TYPE (TREE_TYPE (op0));
  /* then the two pointers must both point to shared objects, and we
     have to perform the reverse of addition on shared pointers */
  tree thread0, thread1, thread_diff, offset_diff, mask, off0, off1;
  if ( (TYPE_SHARED (target_type)
        && ! TYPE_SHARED (TREE_TYPE (TREE_TYPE (op1))))
      || (TYPE_SHARED (TREE_TYPE (TREE_TYPE (op1)))
          && ! TYPE_SHARED (target_type)))
    {
      warning ("Attempt to take the difference of pointer-to-shared and pointer-to-private");
      return error_mark_node;
    }

  /* check to see that the base types match and the block_sizes match */
  comptypes(target_type, TREE_TYPE(TREE_TYPE(op1)));

  result = build_binary_op (MINUS_EXPR, convert (restype, op0), convert (restype, op1), 0);
  
  /* This generates an error if op1 is pointer to incomplete type.  */
  if (!COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (TREE_TYPE (op1))))
    error ("arithmetic on pointer to an incomplete type");
  
  /* This generates an error if op0 is pointer to incomplete type.  */
  op1 = c_size_in_bytes (target_type);

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
  return folded;
}

rtx
expand_upc_shared_assignment (to, from, want_value, suggest_reg)
     tree to;
     tree from;
     int want_value;
     int suggest_reg;
{
  fatal("%s  not implemented yet\n", __func__);  
  return NULL_RTX;
}

rtx
expand_upc_shared_bit_field_assign (to, from, field_ref, bitpos, offset,
			            want_value, suggest_reg)
     tree to;
     tree from;
     tree field_ref;
     int bitpos;
     tree offset;
     int want_value;
     int suggest_reg;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

rtx
expand_upc_fetch_shared_scalar (exp, target, mode)
    tree exp;
    rtx target;
    enum machine_mode mode;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
 
}

static int
is_valid_shared_ptr_p (exp)
    tree exp;
{
  tree type = TREE_TYPE (exp);
  return ((TREE_CODE (type) == POINTER_TYPE)
       && TYPE_SHARED (TREE_TYPE (type))
       && !(TREE_CODE (exp) == NOP_EXPR
	     && TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	     && TREE_CODE (TREE_OPERAND
		   (TREE_OPERAND (exp, 0), 0)) == COMPONENT_REF));
}

int
upc_shared_ptr_op_p (exp)
    tree exp;
{
  return is_valid_shared_ptr_p (TREE_OPERAND (exp, 0))
         || is_valid_shared_ptr_p (TREE_OPERAND (exp, 1));
}

rtx
expand_upc_get_indirect (exp, target, mode)
    tree exp;
    rtx target;
    enum machine_mode mode;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

rtx
expand_upc_shared_bit_field_ref (field_ref, bitpos, offset, target, mode)
    tree field_ref;
    int bitpos;
    tree offset;
    rtx target;
    enum machine_mode mode;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

rtx
expand_upc_shared_ptr_sum (exp, modifier)
    tree exp;
    enum expand_modifier modifier;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

rtx
expand_upc_shared_ptr_diff (exp, modifier)
    tree exp;
    enum expand_modifier modifier;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

/* Determine if an expression is static. Used to detect assignment
   of local addresses to shared pointers. */
static int
static_lvalue_p (exp)
     tree exp;
{
  register enum tree_code code = TREE_CODE (exp);
  
  switch (code)
    {
    case VAR_DECL:
      return TREE_STATIC (exp);
    case INDIRECT_REF:
    case ARRAY_REF:
    case ADDR_EXPR:
    case NOP_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
      return static_lvalue_p (TREE_OPERAND (exp, 0));
    case PARM_DECL:
    default:
      return 0;
    }
}

static rtx
expand_remote_address (exp, bitpos, offset, finalmode)
     tree exp;
     int bitpos;
     tree offset;
     enum machine_mode finalmode;
{
  
  return NULL_RTX;
}

static tree
find_base_type (type)
     tree type;
{
  while (TREE_TYPE (type))
    if (TREE_CODE (TREE_TYPE (type)) == ERROR_MARK)
      return TREE_TYPE (type);
    else
      type = TREE_TYPE (type);
  return type;
}

/* Values for sync:  1 == notify; 2 == wait; 3 == barrier; 4 == fence . See c-parse.y */
void 
upc_expand_sync (int sync, tree exp)
{
  extern void WFE_Expand_Upc_Sync(int, tree);
  if (exp) exp = upc_expand_sync_arg(sync,exp);
  if (exp != error_mark_node) WFE_Expand_Upc_Sync(sync, exp);
}

static rtx
expand_cond (exp, target, mode)
     register tree exp;
     rtx target;
     enum machine_mode mode;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

/* Fetch the shared object designated by `exp', and pointed to by
   `get_loc' of mode `mode', and copy it into `target'.

   In this implementation, a library function is always called. The
   library function is responsible for decoding the shared address,
   fetching the necessary data, and for synchronizing the data.

   In this implementation, strict synchronization is assumed.
   If both the strict and relaxed models were implemented, the
   library routine would require an addtional parameter designating
   the current synchronization mode.
   
   There are two library routine interfaces: (1) for non-block
   objects, the access routine accepts a single parameter which is
   the shared address of the object; it returns the fetched data as
   a function call result, (2) The block-mode library routine does a
   memory-to-memory copy, between the remote location pointed to by
   `shared', and the destination, given by `target'. */

static rtx
expand_get (mode, target, get_loc, exp)
    enum machine_mode mode;
    rtx target;
    rtx get_loc;
    tree exp;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

static rtx
expand_put (mode, put_loc, src, exp)
     enum machine_mode mode;
     rtx put_loc;
     rtx src;
     tree exp;
{
  fatal("%s  not implemented yet\n", __func__);
  return NULL_RTX;
}

extern int get_TY_wrapper(tree);
extern void debug_type_wrapper(int, char*);

/**
 *
 *  Checks whether the two types are equivalent in UPC
 *  1 -- equivalent
 *  0 -- not equivalent (a warning is also printed)
 * 
 */
tree bupc_assert_type (type1, type2) 
     tree type1, type2;
{ 
  int ty1 = get_TY_wrapper(type1);
  int ty2 = get_TY_wrapper(type2);
  char s1[1024], s2[1024];
  int is_equiv;
  debug_type_wrapper(ty1,s1);
  debug_type_wrapper(ty2,s2);
  is_equiv = (ty1 == ty2) || strncmp(s1, s2, 1024) == 0;
  if (!is_equiv) {
    warning("bupc_assert_type detects unequivalent types. ");
    debug_tree(type1);
    fprintf(stderr, "type1: id = %d, %s", ty1, s1);
    fprintf(stderr, "\n");
    debug_tree(type2);
    fprintf(stderr, "type2: id = %d, %s", ty2, s2);
    fprintf(stderr, "\n");
  }
  return is_equiv ? integer_one_node : integer_zero_node;
}


tree 
upc_blocksizeof (exp)
     tree exp;
{
  int objectp = !TYPE_P(exp);
  tree type = objectp ? TREE_TYPE (exp) : exp;
  enum tree_code code = TREE_CODE (type);
  tree t, blocksize, elt_type, t_type;

  if (code == FUNCTION_TYPE)
    {
      error ("upc_blocksizeof applied to a function type");
      //if (pedantic || warn_pointer_arith)
      //  pedwarn ("upc_blocksizeof applied to a function type");
      return size_one_node;
    }
  if (code == VOID_TYPE)
    {
      error ("upc_blocksizeof applied to a void type");
      //if (pedantic || warn_pointer_arith)
      //  pedwarn ("upc_blocksizeof applied to a void type");
      return size_one_node;
    }

  if (!TYPE_SHARED (type) && code != ARRAY_TYPE)
    {
      error("upc_blocksizeof applied to a nonshared type");
      return size_one_node;
    }

  if (code == ERROR_MARK)
    return size_one_node;

  /* At this point we already know the type is shared.  If the block size
     is NULL, then return the default block size: 1. */
  elt_type = (code == ARRAY_TYPE) ? get_inner_array_type(type) : type;

  if(!TYPE_SHARED(elt_type)) {
    error("upc_blocksizeof applied to a nonshared type");
    return size_one_node;
  }
    
  blocksize = TYPE_BLOCK_SIZE (elt_type);
  if (blocksize == 0)
    return size_one_node;

  /* If the block size is the integer constant 0, then the entire block
     resides on one thread; we return 0. */
  if (blocksize == integer_zero_node)
    return size_zero_node;

  if (blocksize == star_layout_node) {
    /* WEI: we have the star case here, need to do more work to get the correct blocksize */
    t = size_binop(CEIL_DIV_EXPR, TYPE_SIZE(type), TYPE_SIZE (elt_type));
    if (threads_int != 0) {
      tree num_threads = build_int_2(threads_int, 0);
      /* FIXME: what does the last arg mean!!!! */
      t = build_binary_op(PLUS_EXPR, t, num_threads, 1);
      t = build_binary_op(MINUS_EXPR, t, integer_one_node, 1);
      t = build_binary_op(FLOOR_DIV_EXPR, t, num_threads, 1);
    }
  } else {
    // Must *copy* blocksize rather than modify it in place (e.g. bug819)
    t = copy_node(blocksize);
  }

  TREE_TYPE(t) = sizetype;

  /* size_binop does not put the constant in range, so do it now.  */
  if (TREE_CODE (t) == INTEGER_CST && force_fit_type (t, 0))
    TREE_CONSTANT_OVERFLOW (t) = TREE_OVERFLOW (t) = 1;
  return t;

}

extern tree size_ll(unsigned long long sz);

tree 
upc_localsizeof (exp)
     tree exp;
{
  int objectp = !TYPE_P(exp);
  tree type = objectp ? TREE_TYPE (exp) : exp;
  enum tree_code code = TREE_CODE (type);
  tree t, blocksize, totalsize;
  
  
  if (code == FUNCTION_TYPE)
    {
      error("upc_localsizeof applied to a function type");
      //if (pedantic || warn_pointer_arith)
      //  pedwarn ("upc_localsizeof applied to a function type");
      return size_one_node;
    }
  if (code == VOID_TYPE)
    {
      error("upc_localsizeof applied to a void type");
      //if (pedantic || warn_pointer_arith)
      //  pedwarn ("upc_localsizeof applied to a void type");
      return size_one_node;
    }
  if (!TYPE_SHARED (type))
    {
      error ("upc_localsizeof applied to a nonshared type");
      return size_one_node;
    }

  if (code != ARRAY_TYPE) {
    return c_sizeof(type);
  }
  
  if (code == ERROR_MARK)
    return size_one_node;

  /* At this point we already know the type is shared. */

  blocksize = upc_blocksizeof(type);
  totalsize = c_sizeof (type);
  
  if (blocksize == size_zero_node)
    /* per upc 1.1, it should always be the totalsize */
    {
      return totalsize;
    }

  if (threads_int != 0) {
    /* static environment */

    // NOTE: we perform this in host 64-bit arithmetic to avoid the possible
    // overflow of the target size_t on 32-bit targets for cases like
    //    upc_localsizeof(shared int [256][1024][1024*THREADS])
    // since THREADS is already expanded in the static threads environment
    // Caveat: we probably still have other issues w/ such large arrays

    const UINT64 blksz = TREE_INT_CST_LOW(blocksize);  
    const UINT64 totsz =  get_real_size(get_TY_wrapper(type));
    const UINT64 esize = get_real_size(get_TY_wrapper(get_inner_array_type(type)));
    const UINT64 totelem = totsz / esize; // total elements
    const UINT64 nblks = totelem / (blksz * threads_int); // FULL blocks per thread
    const UINT64 elems = nblks * blksz; // elems per thread contained in FULL blocks
    const UINT64 rem = totelem - (elems * threads_int); // remainder elems not in full blocks
    const UINT64 rem0 = MIN(rem, blksz); // remainder elems on thread 0
    const UINT64 elems0 = elems + rem0; // total elems on thread 0
  
    t = size_ll( esize * elems0 );
  } else /* dynamic env  */
  if (TREE_CODE(totalsize) == INTEGER_CST) {
    // a pointer type "shared [B] T (*ptr)[SIZE]" with B > 0
    // We are currently operating as indefinite in this case, which is a LOT of "padding"!
    // See comment #2 (et. seq) in bug2960 for some "market research"
    t = totalsize;
  } else {
    // an array declaration "shared [B] T arr[S*THREADS]" with B > 0

    // totalsize is "(S * sizeof(T))* THREADS", but for (S=1, T=char) will be just "THREADS".
    // Therefore, this effectively divides totalsize by THREADS.
    tree thsize = (TREE_CODE(totalsize) == MULT_EXPR) ? TREE_OPERAND(totalsize, 1) : size_one_node;

    // Since spec requires a CONSTANT we round up to a multiple of B.
    // A non-rounded result would contain a non-constant THREADS factor
    tree esize = c_sizeof(get_inner_array_type(type));
    t = size_binop(EXACT_DIV_EXPR, thsize, esize); // max elements per-thread
    t = size_binop(CEIL_DIV_EXPR, t, blocksize); // blocks on thread 0 (incl. partial blocks)
    t = size_binop(MULT_EXPR, t, blocksize); // elem on thread 0 (incl. padding to full block)
    t = size_binop(MULT_EXPR, t, esize); // bytes on thread 0 (also incl. padding)
  }
  
  return t;
}

/* WEI: compute the value of the upc_elemsizeof operator.  Only type nodes can be passed
   here
 */
tree upc_elemsizeof(tree type) {

  enum tree_code code = TREE_CODE (type);
  tree t;
  if (code == FUNCTION_TYPE)
    {
      error ("upc_elemsizeof applied to a function type");
      //if (pedantic || warn_pointer_arith)
      //  pedwarn ("upc_elemsizeof applied to a function type");
      return size_one_node;
    }
  if (code == VOID_TYPE)
    {
      error ("upc_elemsizeof applied to a void type");
      //if (pedantic || warn_pointer_arith)
      //  pedwarn ("upc_elemsizeof applied to a void type");
      return size_one_node;
    }
  if (!TYPE_SHARED (type))
    {
      error("calling upc_elemsizeof() on a non-shared type");
      return size_one_node;
    }

  if (TREE_CODE(type) != ARRAY_TYPE) {
    t = c_sizeof(type);
  } else {
    /* return size of the leftmost non-array type */
    //t = build_binary_op(FLOOR_DIV_EXPR, TYPE_SIZE(get_inner_array_type(type)), build_int_2(8,0), 1);
    t = c_sizeof(get_inner_array_type(type));
  }
  return t;
}

extern void WFE_Expand_Affinity_Test(tree);
void 
expand_affinity_test (x)
  tree x;
{
  WFE_Expand_Affinity_Test (x);
}



/* Given a UPC layout specifier, calculate the blocksize in bytes, and
   add this into the type field of `type'.  Return the new, augmented type */

/* ??? layout specifier is a TREE_LIST in the new grammar?                  */
/* ??? we could change the grammar not to use a UPC_LAYOUT_SPEC node type   */

tree
set_upc_blocksize (type, layout_specifier)
    tree type ATTRIBUTE_UNUSED;
    tree layout_specifier ATTRIBUTE_UNUSED;
{

  tree layout_value;
  tree blocksize;

  if (!layout_specifier)
    return type;

  /* The grammar disallows multiple layout specifiers appearing
     together.  In an older syntax this was allowed.
     If we do find another layout specifiers, something is wrong. */

  if (TREE_CHAIN (layout_specifier)) {
    abort ();
  }
  layout_value = TREE_VALUE (layout_specifier);

  if (layout_value) {
    STRIP_NOPS(layout_value);
    blocksize = layout_value;
    if (blocksize == star_layout_node) {
      /*  just leave block size as is */
    } else { 
      /* don't mult blocksize by elt size, since it may break recursive types */
      //blocksize =  build_binary_op(MULT_EXPR, layout_value, elt_size, 1); 
      if (TREE_CODE(blocksize) != INTEGER_CST) {
	error("Block Size is not a compile time constant");
      }
      else if (TREE_INT_CST_LOW (blocksize) > max_bsize) {
	error ("Maximum block size in this implementation is %lu", max_bsize);    
      }
    }
  } else {
    blocksize = integer_zero_node;
  }

  type = build_type_copy (type);
  TYPE_BLOCK_SIZE (type) = blocksize;
  TYPE_TY_IDX (type) = 0;
  return type;
}


void
set_threads_value (s)
    char *s;
{
    int v = read_integral_parameter (s, "-fupc-threads-", 0);
    int p2;
    if (v > 1048576) /* 1024 x 1024 */
      {
        error ("THREADS value exceeds implementation limit of 1048576");
        v = 1;
      }
    threads_int = v;
}

/* Return non-zero power of 2, if `n' is an exact power of 2
   greater than zero */

static
int
log_2_of_n (n)
    int n;
{
  int power_of_2;
  unsigned int v = n;
  for (power_of_2 = 0; v && !(v & 1); ++power_of_2, v >>=1 ) ;
  return (v == 1) ? power_of_2 : 0;
}

/* Return the value N if -upc-threads=N was supplied on the command line */

int
upc_get_threads_val ()
{
  return threads_int;
}




/****** UPC tree-related checks, and operations **************/

/* Traverse the expression and return the number of times
   THREADS is referenced.  */

static
int
recursive_count_upc_thread_refs (expr)
    tree expr;
{
  enum tree_code code;
  register int i;
  int first_rtl;
  int count = 0;
  
  if (expr == NULL_TREE)
    return 0;
  code = TREE_CODE (expr); 
  switch (TREE_CODE_CLASS (code))
    {
    case 'e':  /* an expression */
    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
    case '1':  /* a unary arithmetic expression */
      first_rtl = first_rtl_op (code);
      for (i = first_rtl - 1; i >= 0; i--)
        count += recursive_count_upc_thread_refs (TREE_OPERAND (expr, i));
      break;
    default:
      if (expr == lookup_name (get_identifier ("THREADS")))
        count = 1;
    }
  return count;
}   



/* Count the number of references to THREADS inside `expr'. */

int
count_upc_thread_refs (expr)
    tree expr ATTRIBUTE_UNUSED;
{
  return recursive_count_upc_thread_refs (expr);
}

int
is_multiple_of_upc_threads (expr)
    tree expr ATTRIBUTE_UNUSED;
{
   enum tree_code code;
  if (expr == NULL_TREE)
    return 0;
  if (expr == lookup_name (get_identifier ("THREADS")))
    return 1;
  code = TREE_CODE (expr);
  if (code == MULT_EXPR) {
    return is_multiple_of_upc_threads (TREE_OPERAND (expr, 0))
           || is_multiple_of_upc_threads (TREE_OPERAND (expr, 1));
  } else if (code == NOP_EXPR || code == NON_LVALUE_EXPR) {
    /* could happen due to type promotion to that of the integer operand */
    return is_multiple_of_upc_threads (TREE_OPERAND(expr, 0));
  }
  return 0;

}


/* Find all references to THREADS and change them into the constant `1'.
   This is done so that fold () when applied to the dimension of a
   shared array will yield the local size of the array */


void
set_upc_thread_refs_to_one (expr)
    tree *expr ATTRIBUTE_UNUSED;
{
  enum tree_code code;
  register int i;
  int first_rtl;

  if (*expr == NULL_TREE)
    return;
  code = TREE_CODE (*expr);
  switch (TREE_CODE_CLASS (code))
    {
    case 'e':  /* an expression */
    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
    case '1':  /* a unary arithmetic expression */
      first_rtl = first_rtl_op (code);
      for (i = first_rtl - 1; i >= 0; i--)
        set_upc_thread_refs_to_one (&TREE_OPERAND (*expr, i));
      break;
    default:
      if (*expr == lookup_name (get_identifier ("THREADS")))
        *expr = integer_one_node;
    }
  return;

}


void
init_upc ()
{
    int p2;
    p2 = log_2_of_n (threads_int);
    if (threads_int)
        threads_rtx = gen_rtx (CONST_INT, Pmode, threads_int);
    else
        threads_rtx = gen_rtx_SYMBOL_REF (Pmode, "THREADS");
    if (p2)
      {
        /* threads_int is a power of 2.  Set threads_log to the log
           of threads_int so that we can use it for shifts and masks. */
        threads_log = build_int_2 (p2, 0);
      }
  /* UPC library functions */
  upc_barrier_libfunc = gen_rtx_SYMBOL_REF (Pmode, UPC_BARRIER_LIBCALL);
  upc_notify_libfunc = gen_rtx_SYMBOL_REF (Pmode, UPC_NOTIFY_LIBCALL);
  upc_wait_libfunc = gen_rtx_SYMBOL_REF (Pmode, UPC_WAIT_LIBCALL);
  upc_getaddr_libfunc = gen_rtx_SYMBOL_REF (Pmode, UPC_GETADDR_LIBCALL);
}


tree 
build_comp_ref_shared_type(tree type, BOOL indef) {
  
  tree  result = build_type_copy(type);
  tree inner = result;

  switch(TREE_CODE(type)) {
  case POINTER_TYPE:
    inner = build_type_copy(TREE_TYPE(result));
    TYPE_SHARED(inner) = 1;
    TREE_TYPE(result) = inner;
    break;
  case ARRAY_TYPE: {
    tree last = result;
    while(TREE_CODE(last) == ARRAY_TYPE &&
	  TREE_CODE(TREE_TYPE(last)) == ARRAY_TYPE) {
      last = TREE_TYPE(last);
    }
    inner = build_type_copy(TREE_TYPE(last));
    TREE_TYPE(last) = inner;
    indef = TRUE; // Array ref is always indefinite
  }
    break;
  } 

  if (indef)
    TYPE_BLOCK_SIZE(inner) = integer_zero_node;
  return result;
}


int Can_Strip_NOP(tree t1, tree t2) 
{
  switch(TREE_CODE(t1)) {
  case INTEGER_TYPE:
    if(TREE_CODE(t2) == POINTER_TYPE)
      return !TREE_SHARED(t2) && !TREE_SHARED(TREE_TYPE(t2));
    break;
  case POINTER_TYPE:
    if(TREE_CODE(t2) == POINTER_TYPE) {
      if (TREE_SHARED(t1) || TREE_SHARED(t2) ||
	  TREE_SHARED(TREE_TYPE(t1)) || TREE_SHARED(TREE_TYPE(t2))) {
	//Fix bug1094
	//Do not strip NOPS involving pointer-to-shared
	return 0;
      }
      /*
      if( TREE_SHARED(t1) != TREE_SHARED(t2) ||
	  TREE_SHARED(TREE_TYPE(t1)) != TREE_SHARED(TREE_TYPE(t2)))
	return 0;
      */
    }
    break;
  default:
    return 1;
  }
  return 1;
}

int x_x_simp_safe(tree arg0, tree arg1) {
  if(!compiling_upc)
    return 1;
  if(arg0 && (TREE_CODE(arg0) == VAR_DECL || TREE_CODE(arg0) == PARM_DECL) &&
     TREE_CODE(TREE_TYPE(arg0)) == POINTER_TYPE && TREE_CODE(TREE_TYPE(TREE_TYPE(arg0))) == RECORD_TYPE)
    return 0;
  if(arg0 && (TREE_CODE(arg0) == VAR_DECL || TREE_CODE(arg0) == PARM_DECL) &&
     TREE_CODE(TREE_TYPE(arg0)) == POINTER_TYPE &&  TREE_CODE(TREE_TYPE(TREE_TYPE(arg0))) == POINTER_TYPE && 
     TYPE_SHARED(TREE_TYPE(TREE_TYPE(TREE_TYPE(arg0)))))
    return 0;
  if(arg1 && (TREE_CODE(arg1) == VAR_DECL || TREE_CODE(arg1) == PARM_DECL) &&
     TREE_CODE(TREE_TYPE(arg1)) == POINTER_TYPE && TREE_CODE(TREE_TYPE(TREE_TYPE(arg1))) == RECORD_TYPE)
    return 0;
  if(arg1 && (TREE_CODE(arg1) == VAR_DECL || TREE_CODE(arg1) == PARM_DECL) &&
     TREE_CODE(TREE_TYPE(arg1)) == POINTER_TYPE &&  TREE_CODE(TREE_TYPE(TREE_TYPE(arg0))) == POINTER_TYPE &&
     TYPE_SHARED(TREE_TYPE(TREE_TYPE(TREE_TYPE(arg1)))))
    return 0;
  return 1;
}


