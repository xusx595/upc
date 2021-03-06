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


/* ====================================================================
 * ====================================================================
 *
 * Module: ty2c.c
 * $Revision: 1.42 $
 * $Date: 2013/12/17 06:08:30 $
 * $Author: phargrov $
 * $Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/be/whirl2c/ty2c.cxx $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    See ty2c.h for a description of the exported functions and 
 *    variables.
 *
 * ====================================================================
 * ====================================================================
 */
#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/be/whirl2c/ty2c.cxx $ $Revision: 1.42 $";
#endif /* _KEEP_RCS_ID */

#include "whirl2c_common.h"
#include "PUinfo.h"
#include "ty2c.h"
#include "tcon2c.h"
#include "upc_mangle_name.h"

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

/*------------------ macros, types, and local state -------------------*/
/*---------------------------------------------------------------------*/


/* Scalar_C_Names[] is a mapping from a scalar mtype to the builtin
 * C type we have chosen to represent it.  This mapping relies on
 * a certain ordinal numbering between the MTYPE definitions in 
 * common/tdt/mtypes.h, and any changes in that ordering must be
 * reflected in a corresponding change here.
 */
typedef struct SCALAR_C_NAME
{
   const char *real_name;
   const char *pseudo_name;
} SCALAR_C_NAME;


/* This name is used to wrap a union around a block containing
 * equivalence fields.  Should only occur when translating
 * Fortran to C.
 */
const char TY2C_Aligned_Block_Name[] = "__block";

#define MTYPE_PREDEF MTYPE_F16

static char Name_Unknown_Type[] = "__UNKNOWN_TYPE";
static const SCALAR_C_NAME Scalar_C_Names[MTYPE_PREDEF+1] =
   {{"void",               Name_Unknown_Type},  /* MTYPE_UNKNOWN 0 */
    {"char",               "_BOOLEAN"},         /* MTYPE_B = 1 */
    //{"signed char",        "_INT8"},            /* MTYPE_I1 = 2 */
    {"signed char",        "char"},  // We use "char" to represnet I1, since the translator frequently uses 
                                     // a pointer to the predefined I1 whenever it wants to generate a cast to (char *).
                                     // explicit use of "signed char" should be handled by the front end
                                     // and should not use the predefined I1.
    {"signed short",       "_INT16"},           /* MTYPE_I2 = 3 */
    {"signed int",         "_INT32"},           /* MTYPE_I4 = 4 */
    {"signed long long",   "_INT64"},           /* MTYPE_I8 = 5 */
    {"unsigned char",      "_UINT8"},           /* MTYPE_U1 = 6 */
    {"unsigned short",     "_UINT16"},          /* MTYPE_U2 = 7 */
    {"unsigned int",       "_UINT32"},          /* MTYPE_U4 = 8 */
    {"unsigned long long", "_UINT64"},          /* MTYPE_U8 = 9 */
    {"float",              "_IEEE32"},          /* MTYPE_F4 = 10 */
    {"double",             "_IEEE64"},          /* MTYPE_F8 = 11 */
    {Name_Unknown_Type,    "_IEEE80"},          /* MTYPE_F10 = 12 */
    {Name_Unknown_Type,    "_IEEE128"}  /* MTYPE_F16 = 13 = MTYPE_PREDEF */
   }; /* Scalar_C_Names */


/* The following is a mapping from some special types to their
 * names.  This allows us to predefine them and give them a
 * name of our choice (by means of a typedef).  These variables 
 * are initialized by TY2C_initialize().  GET_SPECIAL_TYPENAME
 * returns the type name if the given mtype is one that we
 * treat specially; otherwise, it returns NULL.
 */
const char Special_Void_TypeName[] = "void";
const char Special_String_TypeName[] = "_STRING";
const char Special_Quad_TypeName[] = "_QUAD";
const char Special_Complex32_TypeName[] = "_COMPLEX32";
const char Special_Complex64_TypeName[] = "_COMPLEX64";
const char Special_ComplexQD_TypeName[] = "_COMPLEXQD";

#define GET_SPECIAL_TYPENAME(mtype)\
   ((mtype) == MTYPE_V? Special_Void_TypeName : \
    ((mtype) == MTYPE_STR? Special_String_TypeName : \
     ((mtype) == MTYPE_FQ? Special_Quad_TypeName : \
      ((mtype) == MTYPE_C4? Special_Complex32_TypeName : \
       ((mtype) == MTYPE_C8? Special_Complex64_TypeName : \
	((mtype) == MTYPE_CQ? Special_ComplexQD_TypeName : \
	 (const char *)NULL))))))

/*
#define PTR_OR_ALIGNED_WITH_STRUCT(fld_ty, struct_align) \
   (TY_Is_Pointer(fld_ty) || TY_align(fld_ty) <= struct_align)
*/

#define PTR_OR_ALIGNED_WITH_STRUCT(fld_ty, struct_align) true


/* Some forward declarations */
static void TY2C_scalar(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);
static void TY2C_array(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);
static void TY2C_struct(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);
static void TY2C_function(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);
static void TY2C_pointer(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);
static void TY2C_void(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);
static void TY2C_invalid(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context);


/* TY2C_Handle[] maps a TY_kind to a function that translates
 * a type of the given type into C code.  Should the ordinal
 * numbering of the KIND change in "../common/com/stab.h", then
 * a corresponding change must be made here.
 */
typedef void (*TY2C_HANDLER_FUNC)(TOKEN_BUFFER, TY_IDX, CONTEXT);

static const TY2C_HANDLER_FUNC 
   TY2C_Handle[KIND_LAST/*TY_KIND*/] =
{
   &TY2C_invalid,   /* KIND_INVALID */
   &TY2C_scalar,    /* KIND_SCALAR */
   &TY2C_array,     /* KIND_ARRAY */
   &TY2C_struct,    /* KIND_STRUCT */
   &TY2C_pointer,   /* KIND_POINTER */
   &TY2C_function,  /* KIND_FUNCTION */
   &TY2C_void,      /* KIND_VOID */
};


/*------------------------- exported objects --------------------------*/
/*---------------------------------------------------------------------*/


/* These must match the names given in "stab.c" for the complex types. 
 */
const char *TY2C_Complex_Realpart_Name = "realpart";
const char *TY2C_Complex_Imagpart_Name = "imagpart";
extern int compiling_upc_flag ;

/*--------------------- hidden utility routines -----------------------*/
/*---------------------------------------------------------------------*/

static void 
Write_Scalar_Typedef(FILE       *ofile, 
		     const char *real_type, 
		     const char *new_name)
{
   TOKEN_BUFFER tdef = New_Token_Buffer();

   Append_Token_String(tdef, "typedef");
   Append_Token_String(tdef, real_type);
   Append_Token_String(tdef, new_name);
   Append_Token_Special(tdef, ';');
   Append_Indented_Newline(tdef, 1);
   Write_And_Reclaim_Tokens(ofile, NULL, &tdef);
} /* Write_Scalar_Typedef */


static void 
Write_Typedef(FILE       *ofile, 
	      TY_IDX      ty, 
	      const char *type_name)
{
   TOKEN_BUFFER tdef = New_Token_Buffer();
   CONTEXT      context;
   
   CONTEXT_reset(context);
   
   Append_Token_String(tdef, type_name);
   TY2C_Handle[(TY_KIND)TY_kind(ty)](tdef, ty, context);
   Prepend_Token_String(tdef, "typedef");
   Append_Token_Special(tdef, ';');
   Append_Indented_Newline(tdef, 1);

   /* Put an empty line before this declaration and write it out */
   Prepend_Indented_Newline(tdef, 1);
   Write_And_Reclaim_Tokens(ofile, NULL, &tdef);
} /* Write_Typedef */


static void 
TY2C_prepend_qualifiers(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
  if (!CONTEXT_unqualified_ty2c(context))
    {
      if (TY_is_const(ty) && CONTEXT_const(context)) {
	Prepend_Token_String(decl_tokens, "const");
      }
      if (TY_is_volatile(ty))
	Prepend_Token_String(decl_tokens, "volatile");
      if (TY_is_restrict(ty)) {
	TY_IDX real_ty = TY_To_Sptr_Idx(ty);
	if (real_ty == pshared_ptr_idx) {
	  Prepend_Token_String(decl_tokens, "UPCR_RESTRICT_PSHARED");
	} else if (real_ty == shared_ptr_idx) {
	  Prepend_Token_String(decl_tokens, "UPCR_RESTRICT_SHARED");
	} else {
	  Prepend_Token_String(decl_tokens, "UPCR_RESTRICT");
	}
      }
    }
} /* TY2C_prepend_qualifiers */


static INT64
get_field_gap(FLD_HANDLE this_fld, FLD_HANDLE next_fld, INT64 max_size,
	      BOOL is_union) 
{
   /* Return the minimum of the actual size of this_field and
    * the difference in offsets between this and the next field, 
    * unless there is no next_fld or we are dealing with a union 
    * of fields, in which case we assume the field-size must be 
    * less than or equal to the given max_size.
    */
   INT64 field_gap = TY_size(FLD_type(this_fld));
   
   if (!is_union        && 
       !next_fld.Is_Null() &&
       field_gap != FLD_ofst(next_fld)-FLD_ofst(this_fld))
   {
      field_gap = FLD_ofst(next_fld) - FLD_ofst(this_fld);
   }
   if (field_gap > max_size || (!is_union && next_fld.Is_Null ()))
   {
      field_gap = max_size;
   }
   return field_gap;
} /* get_field_gap */


static FLD_HANDLE
skip_till_next_field(FLD_HANDLE  this_fld, 
		     const INT64 struct_align,
		     const INT64 struct_size,
		     BOOL        is_union)
{
   /* Walk through the field-list until we find a field with
    * a larger offset than the first field, which is aligned
    * commensurate with its type but not with stricter (higher)
    * alignment requirements than the struct-type as a whole.  
    * I.e. until we find a field which can safely be accessed 
    * using C "->" or "." notation.
    */
   FLD_HANDLE next_fld = FLD_next(this_fld);
   
   if (!is_union)
   {
      /* Skip until we get to a next_fld which is neither packed
       * tighter than the struct itself nor is misaligned (we only 
       * allow this for pointers to handle INITVKIND_SYMOFF 
       * initializers).
       */
      while (!next_fld.Is_Null () &&
	     (FLD_ofst(this_fld) >= FLD_ofst(next_fld) ||
	      !PTR_OR_ALIGNED_WITH_STRUCT(FLD_type(next_fld), struct_align) ||
	      //bug1452 -- on PPC we have situations where a type's natural alignemnt 
	      //is different from when it's the inside a struct, so we'd have a "misaligned"
	      //field here that still needs to be output
	      (!Compile_Upc && !TY_Is_Pointer(FLD_type(next_fld)) &&
	       FLD_ofst(next_fld) % TY_align(FLD_type(next_fld)) != 0) ||
	      (!is_union &&
	       FLD_Is_Bitfield(next_fld, 
			       FLD_next(next_fld),
			       struct_size - FLD_ofst(next_fld)))))
      {
	 next_fld = FLD_next(next_fld);
      }
   }
   return next_fld;
} /* skip_till_next_field */

static void
TY2C_prepend_filler_field(TOKEN_BUFFER decl_tokens, INT64 byte_size)
{

  if (Compile_Upc) {
    return;  //Do not output filler field to aid debugging 
  }
   /* Declare a filler field, uniquely named "fill".
    */
   TOKEN_BUFFER field_tokens;
   
   field_tokens = New_Token_Buffer();
   Append_Token_String(field_tokens,
		       Scalar_C_Names[MTYPE_U1].pseudo_name);
   Append_Token_String(field_tokens, W2CF_Symtab_Unique_Name("fill"));
   Append_Token_Special(field_tokens, '[');
   TCON2C_translate(field_tokens, Host_To_Targ(MTYPE_I8, byte_size));
   Append_Token_Special(field_tokens, ']');
   Append_Token_Special(field_tokens, ';');
   Prepend_Indented_Newline(field_tokens, 1);
   Prepend_And_Reclaim_Token_List(decl_tokens, &field_tokens);
} /* TY2C_prepend_filler_field */


extern int debug_requested;
static void
TY2C_prepend_FLD_list(TOKEN_BUFFER decl_tokens,
		      FLD_HANDLE   fld,
		      const BOOL   is_union,
		      const INT64  struct_align,
		      const INT64  struct_size,
		      CONTEXT      context)
{
   /* Add the fields in the field-list (fld) in the correct order,
    * where we handle bitfields and packed structures by declaring
    * container or "filler" fields.  Such fillers must never be 
    * referenced, so we must recognize them uniformly both at places
    * of reference and here at the point of declaration.  Insert an
    * indented newline before each field.  Note that for bitfields we 
    * cannot assume anything about the structure alignment, since 
    * this is dependent on the container types, so we pass the 
    * structure alignment in here explicitly.  Assert(fld != NULL)!!
    */
   TOKEN_BUFFER fld_tokens;
   INT64        fld_gap;
   const INT64  remaining_bytes = struct_size - FLD_ofst(fld);
   
   FLD_HANDLE next_fld = skip_till_next_field(fld,
					      struct_align,
					      struct_size,
					      is_union);
   fld_gap = get_field_gap(fld, next_fld, remaining_bytes, is_union);
      
   /* Do the next field before this one, since fields are prepended 
    * (rather than appended) to the token buffer.  Note that filler-
    * fields to precede the next_fld should be prepended after having
    * prepended the next_fld.
    */
   if (!next_fld.Is_Null ())
   {
      if (is_union)
	 TY2C_prepend_FLD_list(decl_tokens, 
			       next_fld,
			       TRUE,  /* is_union */
			       struct_align,
			       struct_size,
			       context);
      else
	 TY2C_prepend_FLD_list(decl_tokens,
			       next_fld,
			       FALSE, /* is_union */
			       struct_align,
			       struct_size, 
			       context);
   }
 
   /* Add the tokens for the field declaration in a right-to-left
    * manner.  Note that we cannot redeclare bitfields accurately,
    * since the TY information is too incomplete to allow this.
    */

   if (!PTR_OR_ALIGNED_WITH_STRUCT(FLD_type(fld), struct_align) ||
       (!is_union &&
	FLD_Is_Bitfield(fld, FLD_next(fld), remaining_bytes)))
   {
      /* Fill up this space with a filler-field */
      if (fld_gap > 0)
	 TY2C_prepend_filler_field(decl_tokens, fld_gap);

#if 0
// This is for debugging possible future output of bitfields
// Current state shows that information is at least sometimes incorrect.
      if (fld_gap > 0) {
	 TOKEN_BUFFER tmp_tokens = New_Token_Buffer();
	 for (FLD_HANDLE tmp = fld; tmp != next_fld; tmp = FLD_next(tmp)) {
	    TY_IDX tmp_ty = FLD_type(tmp);
	    char buf[128];
	    Append_Indented_Newline(tmp_tokens, 1);
	    Append_Token_String(tmp_tokens, "/* Skipped output of bitfield");
	    sprintf(buf, "ofst=%d bofst=%d :", (int)FLD_ofst(tmp), (int)FLD_bofst(tmp));
	    Append_Token_String(tmp_tokens, buf);
	    Increment_Indentation();
	    Append_Indented_Newline(tmp_tokens, 1);

	    Append_Token_String(tmp_tokens, Scalar_C_Names[TY_mtype(tmp_ty)].pseudo_name);
	    Append_Token_String(tmp_tokens, W2CF_Symtab_Nameof_Fld(tmp));
	    Append_Token_Special(tmp_tokens, ':');
	    sprintf(buf, "%d", (int)FLD_bsize(tmp)), Append_Token_String(tmp_tokens, buf);
	    Append_Token_Special(tmp_tokens, ';');

	    Decrement_Indentation();
	    Append_Indented_Newline(tmp_tokens, 1);
	    Append_Token_String(tmp_tokens, " */");
	 }
	 Prepend_And_Reclaim_Token_List(decl_tokens, &tmp_tokens);
      }
#endif
   }
   else /* A regular field, at least we think so */
   {
      /* Insert filler to succede this fld, since such fillers are
       * not handled in the processing of the next field.
       */
      if (!is_union && fld_gap > TY_size(FLD_type(fld)))
      {
	 TY2C_prepend_filler_field(decl_tokens, 
				   fld_gap - TY_size(FLD_type(fld)));
      }
      
      /* Emit this_fld declaration */
      fld_tokens = New_Token_Buffer();
      Append_Token_String(fld_tokens, W2CF_Symtab_Nameof_Fld(fld));
      if(debug_requested && Type_Is_Shared_Ptr(FLD_orig_type(fld)))
	  TY2C_translate(fld_tokens, FLD_orig_type(fld), context);
      else  
	TY2C_translate(fld_tokens, FLD_type(fld), context);
      Append_Token_Special(fld_tokens, ';');
      Prepend_And_Reclaim_Token_List(decl_tokens, &fld_tokens);
      Prepend_Indented_Newline(decl_tokens, 1);
   } /*if*/
} /* TY2C_prepend_FLD_list */


static void
TY2C_Prepend_Alignment_Type(TOKEN_BUFFER tokens, INT64 align)
{
   CONTEXT c;

   CONTEXT_reset(c);
   switch (align)
   {
   case 1:
      TY2C_translate(tokens, Stab_Mtype_To_Ty(MTYPE_U1), c);
      break;
   case 2:
      TY2C_translate(tokens, Stab_Mtype_To_Ty(MTYPE_U2), c);
      break;
   case 4:
      TY2C_translate(tokens, Stab_Mtype_To_Ty(MTYPE_U4), c);
      break;
   case 8:
      TY2C_translate(tokens, Stab_Mtype_To_Ty(MTYPE_U8), c);
      break;
   case 16:
      TY2C_translate(tokens, Stab_Mtype_To_Ty(MTYPE_FQ), c);
      break;
   default:
      Is_True(FALSE, 
	      ("Unexpected alignment (%lld) in TY2C_Prepend_Alignment_Type()",
	       align));
   }
} /* TY2C_Prepend_Alignment_Type */


static void
TY2C_prototype_params(TOKEN_BUFFER decl_tokens, 
		      TYLIST_IDX   params, 
		      CONTEXT      context)
{
   TOKEN_BUFFER param_tokens;
   /* We want the const qualifer for function prototypes */
   CONTEXT_set_const(context);
   if (Tylist_Table[params] == 0)
   {
      Append_Token_String(decl_tokens, "void");
   }
   else
   {
      while (Tylist_Table[params] != 0)
      {
	 param_tokens = New_Token_Buffer();
	 TY2C_translate(param_tokens, TYLIST_item(Tylist_Table[params]),
                        context);
	 Append_And_Reclaim_Token_List(decl_tokens, &param_tokens);
	 params = TYLIST_next(params);
         if (Tylist_Table[params] != 0)
	    Append_Token_Special(decl_tokens, ',');
      }
   }
   CONTEXT_reset_const(context);
} /* TY2C_prototype_params */


/*---------- hidden routines to handle each kind of type --------------*/
/*---------------------------------------------------------------------*/

static void 
TY2C_scalar(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
   Is_True(TY_Is_String(ty) ||
	   TY_mtype(ty) <= MTYPE_PREDEF, ("Illegal type in TY2C_scalar()"));
   Is_True(TY_mtype(ty) != MTYPE_UNKNOWN, ("Unknown type in TY2C_scalar()"));

   if (TY_Is_String(ty))
   {
      Prepend_Token_String(decl_tokens,
			   Scalar_C_Names[MTYPE_U1].pseudo_name);
      Append_Token_Special(decl_tokens, '[');
      Append_Token_Special(decl_tokens, ']');
   }
   else
   {
     //WEI:
     //For shared_ptr_idx, we want to print the name directly
     if ((TY_is_shared(ty) && !TY_is_pshared(ty)) || strcmp(TY_name(ty),"shared_ptr_struct") == 0) {
       Prepend_Token_String(decl_tokens, "upcr_shared_ptr_t");
     } else if ((TY_is_shared(ty) && TY_is_pshared(ty)) || strcmp(TY_name(ty),"pshared_ptr_struct") == 0) {
       Prepend_Token_String(decl_tokens, "upcr_pshared_ptr_t");
     } else if (Type_Is_Shared_Ptr(ty)) {
       Prepend_Token_String(decl_tokens, 
			    (TY_To_Sptr_Idx(ty) == pshared_ptr_idx) ? "upcr_pshared_ptr_t" : "upcr_shared_ptr_t");
     } else if (ty == upc_hsync_mem_ty) {
       Prepend_Token_String(decl_tokens, "upcr_handle_t");
     } else if (Compile_Upc && TY_is_logical(ty)) {
       Prepend_Token_String(decl_tokens, TY_name(ty));
     } else {
	 Prepend_Token_String(decl_tokens, 
			      Scalar_C_Names[TY_mtype(ty)].pseudo_name);
     }
   }
   TY2C_prepend_qualifiers(decl_tokens, ty, context);
} /* TY2C_scalar */


static void 
TY2C_array(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
   if (Stab_Array_Has_Dynamic_Bounds(ty))
   {
      /* The array has dynamic bounds, unknown at compile-time,
       * so declare it as a pointer type in a context that cannot
       * accept incomplete types, and declare it as an incomplete
       * array otherwise. 
       *
       * Is_True(TY_size(ty) == 0,
       *      ("Expected size of dynamic array to be zero (size=%llu)",
       *       (UINT64)TY_size(ty)));
       */
      if (CONTEXT_incomplete_ty2c(context))
      {
	 /* We can have incomplete C types here, so just declare it as 
	  * an incomplete array of one dimension.  Dynamic arrays are
	  * indexed as though they have one dimension, so this should
	  * work just fine.
	  */
	 Append_Token_Special(decl_tokens, '[');
	 Append_Token_Special(decl_tokens, ']');

	 /* In C, the element type of an incomplete array must be 
	  * a complete type
	  */
	 CONTEXT_reset_incomplete_ty2c(context);
      }
      else
      {
	 /* Declare it as a pointer type, since it must be a dynamic or 
	  * allocatable Fortran array.
	  */
	 Prepend_Token_Special(decl_tokens, '*');
	 if (TY_Is_Array_Or_Function(TY_AR_etype(ty)))
	    WHIRL2C_parenthesize(decl_tokens);

	 /* Allow incomplete element type */
	 CONTEXT_set_incomplete_ty2c(context);
      }
   }
   else
   {
      INT32 dim;
      INT64 num_elts;
   
      /* Emit the array dimensions.  TODO:  If the stride is different
       * from the element type size, need to adjust the num_elts.
       */
      for (dim = 0; dim < TY_AR_ndims(ty); dim++)
      {
	 Append_Token_Special(decl_tokens, '[');

	 /* If we need to take the stride (in bytes) into account, then we
	  * must divide the following by:
	  *
	  *    (TY_AR_stride_val(ty, dim)/TY_size(TY_AR_etype(ty)))
	  */
	 num_elts = (TY_AR_ubnd_val(ty, dim) - TY_AR_lbnd_val(ty, dim) + 1);

	 if (num_elts > 0)
	    TCON2C_translate(decl_tokens, 
			     Host_To_Targ(MTYPE_I8, num_elts));

	 Append_Token_Special(decl_tokens, ']');
      }
	 
      /* In C, the element type of an incomplete array must be 
       * a complete type
       */
      CONTEXT_reset_incomplete_ty2c(context);
   }

   //WEI: if elt type is a struct, make sure it's output as incomplete
   if (TY_kind(TY_AR_etype(ty)) == KIND_STRUCT) {
     CONTEXT_set_incomplete_ty2c(context);
   }
   
   /* Add the element type tokens and combine the array and element
    * qualifiers.
    */
   TY2C_translate(decl_tokens, TY_AR_etype(ty), context);
   TY2C_prepend_qualifiers(decl_tokens, ty, context);
} /* TY2C_array */


static void TY2C_complete_struct(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context) {

   /* 
    *  We need to get the alignment right for structs containing
    * equivalence fields; i.e. overlapping fields, only one of which
    * will be declared by whirl2f.  One way to accomplish this is
    * to wrap the whole struct within a union and give the union the
    * name of the struct, the struct field an anonymous type and
    * a name "__block", and enforce the alignment with the a
    * second union field with the name "__align".  As an example,
    * consider the struct S (having an equivalence field):
    *
    *   struct S1 {....}
    *
    * We translate this into:
    *
    *   union S1 {
    *     struct {
    *       ....
    *     } __block;
    *     UINT64 __align;
    *   }
    *
    * Access expressions to this struct must accordingly denote
    * the "__block" portion of the union.
    */

    CONTEXT fld_context;
    BOOL    is_equivalenced = Stab_Is_Equivalenced_Struct(ty);
    BOOL    is_anonymous = (strncmp (TY_name(ty), "T ", 2) == 0); //whether struct is anonymous
    BOOL    do_write = FALSE;
    if (!TY_flist(Ty_Table[ty]).Is_Null ()) {
      do_write = TRUE;
      fld_context = context;
      CONTEXT_reset_unqualified_ty2c(fld_context);
      CONTEXT_reset_incomplete_ty2c(fld_context);
      
      if (is_equivalenced)
	{
	  /* Prepend the end-of-union character, on a new line following
	   * the list of its two fields, and force the alignment
	   * with a new field.
	   */
	  Prepend_Token_Special(decl_tokens, '}');
	  Prepend_Indented_Newline(decl_tokens, 1);
	  Increment_Indentation();
	  Prepend_Token_Special(decl_tokens, ';');
	  Prepend_Token_String(decl_tokens, "__align");
	  TY2C_Prepend_Alignment_Type(decl_tokens, TY_align(ty));
	  Prepend_Indented_Newline(decl_tokens, 1);
	  Prepend_Token_Special(decl_tokens, ';');
	  Prepend_Token_String(decl_tokens, TY2C_Aligned_Block_Name);
	}
      
      /* Prepend the end-of-struct character, on a new line following
       * the list of fields.
       */
      Prepend_Token_Special(decl_tokens, '}');
      Prepend_Indented_Newline(decl_tokens, 1);
      
      //fix bug579.
      //In the front end, a forward declared struct has an align of 1,
      //and here we need to fix it back to the correct alignment
      if (TY_align(ty) == 1) {
	FLD_HANDLE first_fld = TY_flist(Ty_Table[ty]);
	if (!first_fld.Is_Null()) {
	  Set_TY_align(ty, TY_align(FLD_type(first_fld)));
	}
      }

      /* prepend fields, where each field is preceeded by a newline 
       * and is indented.
       */
      Increment_Indentation();
      TY2C_prepend_FLD_list(decl_tokens, 
			    TY_flist(Ty_Table[ty]),
			    TY_is_union(ty),
			    TY_align(ty),
			    TY_size(ty),
			    fld_context);
      Decrement_Indentation();

      /* Prepend the start-of-struct character*/
      Prepend_Token_Special(decl_tokens, '{');

      if (is_equivalenced)
      {
	 /* Prepend the start-of-union character, followed by a newline
	  * to precede the struct and alignment fields.
	  */
	 Prepend_Token_String(decl_tokens, TY_is_union(ty)? "union": "struct");
	 Prepend_Indented_Newline(decl_tokens, 1);
	 Decrement_Indentation();
	 Prepend_Token_Special(decl_tokens, '{');
      }
      if (!is_anonymous) {
	Prepend_Token_String(decl_tokens, W2CF_Symtab_Nameof_Ty(ty));
      }
    } else if (TY_size(ty) == 1) {
      /* A special struct with no fields, which may sometimes
       * occur for C++ lowered into C.
       */
      Prepend_Token_Special(decl_tokens, '}');
      Prepend_Indented_Newline(decl_tokens, 1);
      
      Prepend_Token_Special(decl_tokens, ';');
      Prepend_Token_String(decl_tokens, W2CF_Symtab_Unique_Name("dummy"));
      TY2C_translate(decl_tokens, Stab_Mtype_To_Ty(MTYPE_U1), context);
      
      Increment_Indentation();
      Prepend_Indented_Newline(decl_tokens, 1);
      Prepend_Token_Special(decl_tokens, '{');
      Prepend_Token_String(decl_tokens, W2CF_Symtab_Nameof_Ty(ty));
      Decrement_Indentation();
      do_write = TRUE;
    } else if (TY_size(ty) == 0) {
      //special case of incomplete struct (see bug1323)
      if (!is_anonymous) {
	Prepend_Token_String(decl_tokens, W2CF_Symtab_Nameof_Ty(ty));
	do_write = TRUE;
      }
    }

    if(do_write) { 
      if (TY_is_union(ty) || is_equivalenced)
	Prepend_Token_String(decl_tokens, "union");
      else
	Prepend_Token_String(decl_tokens, "struct");
    }
    if (is_anonymous) {
      Prepend_Token_String(decl_tokens, "typedef ");
      Append_Token_String(decl_tokens, " ");
      Append_Token_String(decl_tokens, TY_name(ty) + 2);
    }
}

/* code to avoid duplication of struct output.  This could happen e.g. if the struct type is used both for 
   shaerd and non-shared variables */

static hash_set<STR_IDX> struct_names;

//If the given type is a user-defined struct, 
//This function outputs it complete declaration to the w2c.h file
static void TY2C_Output_Struct_Type(TY_IDX ty,
			     INT lines_between_decls,
			     CONTEXT context) {

  if (struct_names.find(TY_name_idx(ty)) != struct_names.end()
      || !TY_is_written(ty)) {
    //don't output duplicate struct definitions
    return;
  }

  //it seems that the intention here is to avoid writing duplicate declarations
  //for structs that appear as both local and shared types.
  //However - if  they appear only as shared we do not get any types printed - we
  //need them for debug. The above test should be strong enough ...
  //if (!TY_is_shared(ty)) {
    Set_TY_is_translated_to_c(ty);  //need to force all later struct type decl to be incomplete
    struct_names.insert(TY_name_idx(ty));

    TOKEN_BUFFER tmp_tokens = New_Token_Buffer(); 
    CONTEXT_reset_incomplete_ty2c(context); 
    TY2C_complete_struct(tmp_tokens, ty, context);
    Append_Token_Special(tmp_tokens, ';'); 
    Append_Indented_Newline(tmp_tokens, lines_between_decls);
    Write_And_Reclaim_Tokens(W2C_File[W2C_DOTH_FILE], 
			     NULL,
			     &tmp_tokens);
    //}
}


/*
 * WEI: This function is modified to output only incomplete struct types.
 * Complete struct type will be output by TY2C_complete_struct, through TY2C_Output_Struct_Type
 */
static void 
TY2C_struct(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
  
  if (!TY_is_translated_to_c(ty)) {
    //Add this struct type to the global w2c.h
    CONTEXT_reset_incomplete_ty2c (context);
    TY2C_Output_Struct_Type(ty, 1, context);
  }
  
  if (Compile_Upc) {
    //WEI: special case for shared types
    if (strcmp(W2CF_Symtab_Nameof_Ty(ty),"pshared_ptr_struct") == 0) {
      Prepend_Token_String(decl_tokens, "upcr_pshared_ptr_t");
      TY2C_prepend_qualifiers(decl_tokens, ty, context);
      return;
    }
    if (strcmp(W2CF_Symtab_Nameof_Ty(ty),"shared_ptr_struct") == 0) {
      Prepend_Token_String(decl_tokens, "upcr_shared_ptr_t");
      TY2C_prepend_qualifiers(decl_tokens, ty, context);
      return;
    }
  }

  BOOL is_anonymous = (strncmp(TY_name(ty), "T ", 2) == 0);
  if (!is_anonymous) {
    /* the normal case */
    Prepend_Token_String(decl_tokens, W2CF_Symtab_Nameof_Ty(ty));
  
    BOOL    is_equivalenced = Stab_Is_Equivalenced_Struct(ty);
    if (TY_is_union(ty) || is_equivalenced)
      Prepend_Token_String(decl_tokens, "union");
    else
      Prepend_Token_String(decl_tokens, "struct");
  } else {
    Prepend_Token_String(decl_tokens, TY_name(ty) + 2);
  }
    
  TY2C_prepend_qualifiers(decl_tokens, ty, context);
} /* TY2C_struct */


static void 
TY2C_function(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
   TYLIST_IDX params = TY_parms(ty);

   /* A function cannot be qualified! */
   CONTEXT_reset_unqualified_ty2c(context);

   /* WEI: struct types in a function had better be incomplete */
   CONTEXT_set_incomplete_ty2c(context);

   /* Append the parameter type list to the right of the decl_tokens */
   Append_Token_Special(decl_tokens, '(');
   if (TY_has_prototype(ty))
   {
      TY2C_prototype_params(decl_tokens, params, context);
      if (TY_is_varargs(ty))
      {
	 Append_Token_Special(decl_tokens, ',');
	 Append_Token_String(decl_tokens, "...");
      }
   }
   Append_Token_Special(decl_tokens, ')');

   /* Prepend the return type as the type of the decl_tokens */
   TY2C_translate(decl_tokens, Func_Return_Type(ty), context);
} /* TY2C_function */


static void 
TY2C_pointer(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{

  TY_IDX sptr_idx = 0;
   /* Add qualifiers to the rhs of the '*' and to the left of the
    * decl_tokens.
    */
   TY2C_prepend_qualifiers(decl_tokens, ty, context);

   //WEI: If ty is a local pointer to shared, don't output *
   //i.e., we want shared_ptr_t, not shared_ptr_t*
   if (!TY_is_shared(TY_pointed(ty))) {
     Prepend_Token_Special(decl_tokens, '*');
   } else
     //Shouldn't see shared ptr types in this stage - BE bug
     //However, until we fix it, patch these here.
     if(Type_Is_Shared_Ptr(ty)) {
       sptr_idx = TY_To_Sptr_Idx(ty);
     }


   if (TY_Is_Array_Or_Function(TY_pointed(ty)) && 
       !TY_is_shared(TY_pointed(ty)) &&
       !(sptr_idx && Compile_Upc && debug_requested) )
      WHIRL2C_parenthesize(decl_tokens);
   
   CONTEXT_reset_unqualified_ty2c(context); /* Always qualify pointee type */
   CONTEXT_set_incomplete_ty2c(context); /* Pointee can be incomplete */
   bool old_const = CONTEXT_const(context);
   CONTEXT_set_const(context);
   if(!sptr_idx)
     TY2C_translate(decl_tokens, TY_pointed(ty), context);
   else {
     if(Compile_Upc && debug_requested) {
       static char mangled_name[256];
       strcpy(mangled_name,"__BMN_");
       strcat(mangled_name, Mangle_Type(ty).data());
       Prepend_Token_String(decl_tokens, mangled_name);
     }
     else 
       TY2C_translate(decl_tokens, sptr_idx, context);
   }
   if (!old_const) {
     CONTEXT_reset_const(context);
   }
} /* TY2C_pointer */


static void 
TY2C_void(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
   Prepend_Token_String(decl_tokens, Special_Void_TypeName);
   TY2C_prepend_qualifiers(decl_tokens, ty, context);
} /* TY2C_void */


static void 
TY2C_invalid(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
   Is_True(FALSE, ("Invalid TY kind (%d) for translation to C", TY_kind(ty)));
} /* TY2C_invalid */


/*------------------------ exported routines --------------------------*/
/*---------------------------------------------------------------------*/

void 
TY2C_initialize(CONTEXT context)
{
   /* Since /usr/include/whirl2c.h has declared typedefs for the 
    * complex and string types, we need to ensure that they are 
    * not declared again:
    */
   Set_TY_is_translated_to_c(Be_Type_Tbl(MTYPE_STRING));
   Set_TY_is_translated_to_c(Be_Type_Tbl(MTYPE_C4));
   Set_TY_is_translated_to_c(Be_Type_Tbl(MTYPE_C8));
   Set_TY_is_translated_to_c(Be_Type_Tbl(MTYPE_CQ));
} /* TY2C_initialize */


void 
TY2C_finalize(void)
{
   return; /* nothing to do */
} /* TY2C_finalize */


void 
TY2C_translate(TOKEN_BUFFER decl_tokens, TY_IDX ty, CONTEXT context)
{
   /* Note that "decl_tokens" can either be the variable name or a
    * string representing an enclosing type.
    */
   const char *special_name = GET_SPECIAL_TYPENAME(TY_mtype(ty));

   if (special_name != NULL)
   {
      Prepend_Token_String(decl_tokens, special_name);
      TY2C_prepend_qualifiers(decl_tokens, ty, context);
   }
   else
   {
      Is_True((TY_KIND)TY_kind(ty) < KIND_LAST && TY_kind(ty) > KIND_INVALID, 
	      ("Unexpected TY_kind (%d) in TY2C_translate()", TY_kind(ty)));
      TY2C_Handle[(TY_KIND)TY_kind(ty)](decl_tokens, ty, context);
   }
} /* TY2C_translate */


void 
TY2C_translate_unqualified(TOKEN_BUFFER decl_tokens, TY_IDX ty)
{
   /* Any qualification on the top-level type should be ignored.
    */
   CONTEXT context;

   CONTEXT_reset(context);
   CONTEXT_set_unqualified_ty2c(context);
   //WEI: in this cast the struct type should definitely be incomplete
   if (Compile_Upc && TY_kind(ty) == KIND_STRUCT) {
     CONTEXT_set_incomplete_ty2c(context);
   }

   TY2C_translate(decl_tokens, ty, context);
} /* TY2C_translate_unqualified */

/* This function is derived from TY2C_get_field_info to handle bug2642.
 * Better to merge with TY2C_get_field_info function once it is stable.
 */
TY2C_FLD_INFO
TY2C_get_field(TY_IDX struct_ty,      /* base type */
        TY_IDX desired_ty,     /* preferred field type */
        MTYPE  desired_mty,    /* preferred field mtype */
        INT64  desired_offset) /* required field mtype */
{
	/* Try to find a field at any level of nesting, which is at
	 * the given offset from the base of the struct_ty and
	 * (preferably) of type field_ty or (less preferably) of mtype 
	 * field_mty.
	 *
	 * possibly (?) TODO:  enhance this to be more similar to 
	 * Construct_Fld_Path in ty2f.h.
	 */
	TY_IDX        this_ty;
	FLD_HANDLE       this_fld;
	FLD_HANDLE       next_fld;
	TY2C_FLD_INFO this_fld_info;
	const INT64   struct_size = TY_size(struct_ty);
	const INT64   struct_align = TY_align(struct_ty);
	BOOL          found_ty = FALSE, found_mty = FALSE;

	/* Search for a field at, or just before, the given offset */
	this_fld = TY_flist(Ty_Table[struct_ty]);
	if (!this_fld.Is_Null () && !TY_is_union(struct_ty))
	{
	/* Set "this_field" to the field closest to the given offset, such
	 * that FLD_ofst(this_field) <= desired_offset and next_fld==NULL
	 * or FLD_ofst(next_field) > desired_offset.
	 */
		for ( next_fld = skip_till_next_field(this_fld, struct_align, struct_size, FALSE);
					!next_fld.Is_Null() && FLD_ofst(next_fld) <= desired_offset;
					next_fld = skip_till_next_field(next_fld, struct_align, struct_size, FALSE))
		{
			this_fld = next_fld;
		}
	}
	else if (!this_fld.Is_Null ())
	{
		next_fld = skip_till_next_field(this_fld, struct_align, struct_size, TRUE/*is_union*/);
	}

	/* Search for a field of the correct type at the *exact* offset.
	 * If a struct/union/class field is encountered, of a different
	 * type than the one we are looking for, then try recursively if
	 * possible.
	 */
	while (!found_ty && !this_fld.Is_Null () && FLD_ofst(this_fld) <= desired_offset)
	{
		this_ty = FLD_type(this_fld);

		TY_IDX array_ty;
		if ( TY_kind(this_ty) == KIND_ARRAY ) {
			array_ty = Get_Inner_Array_Type(this_ty);
		}

		if (!PTR_OR_ALIGNED_WITH_STRUCT(this_ty, struct_align) || 
				(!TY_is_union(struct_ty) &&
				FLD_Is_Bitfield(this_fld, FLD_next(this_fld), struct_size - FLD_ofst(this_fld))))
		{
			/* A bitfield: Note that we cannot redeclare bitfields
			 * accurately, and we therefore cannot access them directly
			 * either.  Continue down the list of fields to see if
			 * there is a non-bitfield unioned with this one.
			 */
		}
		else if ( desired_offset == FLD_ofst(this_fld) &&
							(desired_ty != NULL && Stab_Identical_Types(desired_ty, this_ty,
								FALSE, /* check_quals */
								TRUE,  /* check_scalars */
								TRUE)) /* ptrs_as_scalars */
						)
		{
			/* A perfect match, so initiate the field_info */
			this_fld_info.found_fld = this_fld;
			this_fld_info.select_tokens = New_Token_Buffer();
			Append_Token_String(this_fld_info.select_tokens, W2CF_Symtab_Nameof_Fld(this_fld));
			found_ty = TRUE;
		}
		else if (TY_Is_Structured(this_ty))
		{
			/* Try to find a nested field that matches.  Subtract the
			 * offset of this field from the offset searched for in
			 * the nested struct.
			 */
			this_fld_info = TY2C_get_field(this_ty,
												desired_ty,
												desired_mty,
												desired_offset-FLD_ofst(this_fld));
			if (!this_fld_info.found_fld.Is_Null ())
			{
				/* prepend this name to select the path returned */
				Prepend_Token_Special(this_fld_info.select_tokens, '.');
				Prepend_Token_String(this_fld_info.select_tokens,
				W2CF_Symtab_Nameof_Fld(this_fld));
				found_ty = TRUE;
			}
		}
		/* the following "else if" case is added to fix bug2642
		 * it tries to do the similar thing as the above case where TY_Is_Structured(this_ty)
		 * is true. This case detects the case where this_ty is a pointer to an array of
		 * structs
		 * "desired_offset > FLD_ofst(this_fld)" is tested to not to go into the structure
		 * again if this TY2C_get_field function already reached the desired fld.  
		 */
		else if ( desired_offset > FLD_ofst(this_fld) &&
							TY_kind(this_ty) == KIND_ARRAY  && TY_Is_Structured(array_ty) )
		{
			/* Try to find a nested field that matches.  Subtract the
		 	 * offset of this field from the offset searched for in
			 * the nested struct.
			 */
			this_fld_info = TY2C_get_field(array_ty,
												desired_ty,
												desired_mty,
												desired_offset-FLD_ofst(this_fld));
			if (!this_fld_info.found_fld.Is_Null ())
			{
				/* prepend this name to select the path returned */
				Prepend_Token_Special(this_fld_info.select_tokens, '.');
				Prepend_Token_String(this_fld_info.select_tokens, W2CF_Symtab_Nameof_Fld(this_fld));
				found_ty = TRUE;
			}
		}
		else if (desired_offset == FLD_ofst(this_fld) &&
							(TY_Is_Structured(this_ty)) ||		
							(TY_kind(this_ty) == KIND_ARRAY  && TY_Is_Structured(array_ty) ) )
		{
			this_fld_info.found_fld = this_fld;
			found_mty = TRUE;
		}
		/* if struct, let's not check the mtype */
		else if (desired_offset == FLD_ofst(this_fld) &&
						(desired_mty!=NULL && desired_mty == TY_mtype(this_ty)) || 
						(TY_Is_Structured(this_ty)) ||		/* if struct, let's not check the mtype */
						(TY_kind(this_ty) == KIND_ARRAY  && TY_Is_Structured(array_ty) ) )
		{
			/* An imperfect match, so initiate the found_fld, but keep 
			 * searching ...
			 */
			this_fld_info.found_fld = this_fld;
			found_mty = TRUE;
		}

		if (!found_ty)
		{
			this_fld = next_fld;
			if (!this_fld.Is_Null ())
				next_fld = skip_till_next_field(this_fld, struct_align, struct_size, TY_is_union(struct_ty));
		}
	} /* while */

	if (!found_ty && found_mty)
	{
		/* An imperfect match was the best we found, so initiate the
		* field_info.
		*/
		this_fld_info.select_tokens = New_Token_Buffer();
		Append_Token_String(this_fld_info.select_tokens, W2CF_Symtab_Nameof_Fld(this_fld_info.found_fld));
	}
	if (found_ty || found_mty)
	{
		/* If we have an equivalence field, we must dereference through
		 * the artificial union we have created to declare the struct
		 * of appropriate alignment.  See also "TY2C_struct()".
		 */
		if (Stab_Is_Equivalenced_Struct(struct_ty))
		{
			Prepend_Token_Special(this_fld_info.select_tokens, '.');
			Prepend_Token_String(this_fld_info.select_tokens, TY2C_Aligned_Block_Name);
		}
	}
	else if (!found_ty)
	{
		this_fld_info.found_fld = FLD_HANDLE(0);
	}
	return this_fld_info;
} /* TY2C_get_field */

TY2C_FLD_INFO
TY2C_get_field_info(TY_IDX struct_ty,      /* base type */
		    TY_IDX desired_ty,     /* preferred field type */
		    MTYPE  desired_mty,    /* preferred field mtype */
		    INT64  desired_offset) /* required field mtype */
{
   /* Try to find a field at any level of nesting, which is at
    * the given offset from the base of the struct_ty and
    * (preferably) of type field_ty or (less preferably) of mtype 
    * field_mty.
    *
    * possibly (?) TODO:  enhance this to be more similar to 
    * Construct_Fld_Path in ty2f.h.
    */
   TY_IDX        this_ty;
   FLD_HANDLE       this_fld;
   FLD_HANDLE       next_fld;
   TY2C_FLD_INFO this_fld_info;
   const INT64   struct_size = TY_size(struct_ty);
   const INT64   struct_align = TY_align(struct_ty);
   BOOL          found_ty = FALSE, found_mty = FALSE;

   /* Search for a field at, or just before, the given offset */
   this_fld = TY_flist(Ty_Table[struct_ty]);
   if (!this_fld.Is_Null () && !TY_is_union(struct_ty))
   {
      /* Set "this_field" to the field closest to the given offset, such
       * that FLD_ofst(this_field) <= desired_offset and next_fld==NULL
       * or FLD_ofst(next_field) > desired_offset.
       */
      for (next_fld = skip_till_next_field(this_fld, 
					   struct_align,
					   struct_size,
					   FALSE/*is_union*/);
	   !next_fld.Is_Null() && FLD_ofst(next_fld) <= desired_offset;
	   next_fld = skip_till_next_field(next_fld, 
					   struct_align,
					   struct_size,
					   FALSE/*is_union*/))
      {
	 this_fld = next_fld;
      }
   }
   else if (!this_fld.Is_Null ())
   {
      next_fld = skip_till_next_field(this_fld, 
				      struct_align,
				      struct_size,
				      TRUE/*is_union*/);
   }
   
   /* Search for a field of the correct type at the *exact* offset.
    * If a struct/union/class field is encountered, of a different
    * type than the one we are looking for, then try recursively if
    * possible.
    */
   while (!found_ty && !this_fld.Is_Null () &&
	  FLD_ofst(this_fld) <= desired_offset)
   {
      this_ty = FLD_type(this_fld);

      if (!PTR_OR_ALIGNED_WITH_STRUCT(this_ty, struct_align) ||
	  (!TY_is_union(struct_ty) &&
	   FLD_Is_Bitfield(this_fld, FLD_next(this_fld), 
			   struct_size - FLD_ofst(this_fld))))
      {
	 /* A bitfield: Note that we cannot redeclare bitfields
	  * accurately, and we therefore cannot access them directly
	  * either.  Continue down the list of fields to see if
	  * there is a non-bitfield unioned with this one.
	  */
      }
      else if (desired_offset == FLD_ofst(this_fld) && 
	       Stab_Identical_Types(desired_ty, this_ty, 
				    FALSE, /* check_quals */
				    TRUE,  /* check_scalars */
				    TRUE)) /* ptrs_as_scalars */
      {
	 /* A perfect match, so initiate the field_info */
	 this_fld_info.found_fld = this_fld;
	 this_fld_info.select_tokens = New_Token_Buffer();
	 Append_Token_String(this_fld_info.select_tokens,
			     W2CF_Symtab_Nameof_Fld(this_fld));
	 found_ty = TRUE;
      }
      else if (TY_Is_Structured(this_ty))
      {
	 /* Try to find a nested field that matches.  Subtract the
	  * offset of this field from the offset searched for in
	  * the nested struct.
	  */
	 this_fld_info = 
	    TY2C_get_field_info(this_ty, 
				desired_ty, 
				desired_mty, 
				desired_offset-FLD_ofst(this_fld));
	 if (!this_fld_info.found_fld.Is_Null ())
	 {
	    /* prepend this name to select the path returned */
	    Prepend_Token_Special(this_fld_info.select_tokens, '.');
	    Prepend_Token_String(this_fld_info.select_tokens,
				 W2CF_Symtab_Nameof_Fld(this_fld));
	    found_ty = TRUE;
	 }
      }
      else if (desired_offset == FLD_ofst(this_fld) && 
	       desired_mty == TY_mtype(this_ty))
      {
	 /* An imperfect match, so initiate the found_fld, but keep 
	  * searching ...
	  */
	 this_fld_info.found_fld = this_fld;
	 found_mty = TRUE;
      }
      if (!found_ty)
      {
	 this_fld = next_fld;
	 if (!this_fld.Is_Null ())
	    next_fld = skip_till_next_field(this_fld,
					    struct_align,
					    struct_size,
					    TY_is_union(struct_ty));
      }
   } /* while */
   
   if (!found_ty && found_mty)
   {
      /* An imperfect match was the best we found, so initiate the
       * field_info.
       */
      this_fld_info.select_tokens = New_Token_Buffer();
      Append_Token_String(this_fld_info.select_tokens,
			  W2CF_Symtab_Nameof_Fld(this_fld_info.found_fld));
   }
   if (found_ty || found_mty)
   {
      /* If we have an equivalence field, we must dereference through
       * the artificial union we have created to declare the struct
       * of appropriate alignment.  See also "TY2C_struct()".
       */
      if (Stab_Is_Equivalenced_Struct(struct_ty))
      {
	 Prepend_Token_Special(this_fld_info.select_tokens, '.');
	 Prepend_Token_String(this_fld_info.select_tokens, 
			      TY2C_Aligned_Block_Name);
      }
   }
   else if (!found_ty)
   {
      this_fld_info.found_fld = FLD_HANDLE(0);
   }
   return this_fld_info;
} /* TY2C_get_field_info */


BOOL
TY2C_builtin(TY_IDX ty)
{
   const char *name = GET_SPECIAL_TYPENAME(TY_mtype(ty));

   return (name != NULL ||
	   (TY_mtype(ty) <= MTYPE_PREDEF && 
	    (TY_mtype(ty) != MTYPE_UNKNOWN || TY_kind(ty) == KIND_INVALID) &&
	    Scalar_C_Names[TY_mtype(ty)].pseudo_name != NULL));
} /* TY2C_builtin */
