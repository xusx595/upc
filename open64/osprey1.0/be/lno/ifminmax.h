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


// -*-C++-*-

/**
***  Exported types and functions:
***
***    void If_MinMax(WN* func_nd)
***
***	Convert expressions of the form: 
***	  if (expr1 .relop. expr2) then
***	    result = expr1
***	  else 
***	    result = expr2
***	  end if 
***	to expressions of the form:
***	  result = min(expr1, expr2)
***	or
***	  result = max(expr1, expr2)
***	where .relop. is one of .LT., .GT., .LE., and .GE.
***
***	Attempt sinking converted expressions into loop bounds where
***	  appropriate.
**/

#ifndef ifminmax_INCLUDED
#define ifminmax_INCLUDED "cache_model.h"

extern void If_MinMax(WN* func_nd); 
extern WN* Store_Expr(WN* wn_store);


#endif // ifminmax_INCLUDED
