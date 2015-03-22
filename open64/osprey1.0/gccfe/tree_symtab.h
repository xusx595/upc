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

#ifndef tree_symtab_INCLUDED
#define tree_symtab_INCLUDED

#include <cmplrs/rcodes.h>
#include <map>
#include <string>
#include <vector>
#include <set>
#include <upc_symtab_utils.h>

#include <limits>
#ifdef PATH_MAX
 #define MAX_PATH PATH_MAX
#else
 #define MAX_PATH 4096
#endif

extern int Scope_level;
extern int For_scope;

extern int compiling_upc;

/* mark the "char" type, which should be distinct from MTYPE_I1 or MTYPE_U1 */
extern TY_IDX char_ty;

/* Calculate the block size of a type */
extern int Type_Tree_Block_Size(tree type_tree);
extern "C" void Pad_Field_for_UPC_Align(tree type,
					unsigned int actual_align, unsigned int known_align, 
					unsigned int desired_align);

extern TY_IDX Create_TY_For_Tree (tree, TY_IDX idx = TY_IDX_ZERO);
extern "C" ST* Create_ST_For_Tree (tree);

extern std::map<ST_IDX, int> upc_st_orig_ty;
extern std::set<ST_IDX> upc_tld;

extern string getTypeStr(TY_IDX idx);



/* 
 * either return a previously created TY_IDX associated with a type,
 * or create a new one.
 */
inline TY_IDX
Get_TY (tree type_tree)
{
  TY_IDX result = 0; 
  
  if (TREE_CODE(type_tree) == ERROR_MARK)
    exit (RC_USER_ERROR);
  TY_IDX idx = TYPE_TY_IDX(type_tree);
  if (idx != 0) { 
    bool shared_conflict = compiling_upc && TYPE_SHARED(type_tree) && !TY_is_shared(idx);
    // The following catches pointer-to-function for bug 2867
    // However, perhaps TYPE_SHARED() should NOT have been set in the first place?
    shared_conflict = shared_conflict && (TREE_CODE(type_tree) != FUNCTION_TYPE);
    if (shared_conflict) {
      //create the equivalent shared type
      result = Make_Shared_Type (idx, Type_Tree_Block_Size(type_tree), 
				 TYPE_STRICT(type_tree) ? STRICT_CONSISTENCY :
				 TYPE_RELAXED(type_tree) ? RELAXED_CONSISTENCY : NO_CONSISTENCY);
    } else {
      if (TREE_CODE(type_tree) == RECORD_TYPE ||
	  TREE_CODE(type_tree) == UNION_TYPE) {
	FLD_HANDLE elt_fld = TY_fld(idx);
	if (elt_fld.Is_Null() && TREE_PURPOSE(type_tree) /* not the same incomplete type*/) 
	  result = Create_TY_For_Tree (type_tree, idx); // forward declared
	else 
	  result = idx;
      } else {
	//can safely reuse the type entry
	result = idx;
      }
    } 
  } else { 
    result = Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
  }
  return result;
}

/*
 * either return a previously created ST associated with a
 * var-decl/parm-decl/function_decl, or create a new one.
 */
inline ST *
Get_ST (tree decl_tree)
{
	ST *st = DECL_ST(decl_tree);
        if (st != NULL) {
		if (TREE_CODE(decl_tree) == VAR_DECL &&
		    ST_sclass(st) == SCLASS_EXTERN   &&
		    !ST_is_weak_symbol(st)           &&
		    !DECL_EXTERNAL(decl_tree)        &&
		    !DECL_INITIAL(decl_tree))
		  ;
		  //Set_ST_sclass (st, SCLASS_UGLOBAL);
        }
	else
		st = Create_ST_For_Tree (decl_tree);
	if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
	    ((TREE_CODE(decl_tree) == VAR_DECL) ||
	     (TREE_CODE(decl_tree) == PARM_DECL)) &&
	    (ST_level(st) < CURRENT_SYMTAB) &&
	    (ST_level(st) > GLOBAL_SYMTAB)) {
		Set_ST_has_nested_ref (st);
		ST *base_st = st;
		while (base_st != ST_base (base_st)) {
			base_st = ST_base (base_st);
			Set_ST_has_nested_ref (base_st);
		}
	}
	return st;
}

/* #if (defined(__MACH__) && defined(__APPLE__)) || defined(__CYGWIN__) || defined(__FreeBSD__) */
#ifndef BITSPERBYTE
#define BITSPERBYTE CHAR_BIT
#endif

#endif
