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
//                     Array Dead-Store and Partial Dead-Store Elimination
//                     --------------------------------------------------
//
// Description:
//
// 	In loops given
//	do i
//	  a[i] = ...
//	      ...
//	  a[i] = ...
//
//      dead store eliminate the first store
//
//      also, convert
//
//	do i
//	   a[i] = ...
//	   ...
//	   if (...)
//           a[i] = ...
//
//	into
//
//	do i
//	  t = ...
//	  ...
//	  if (...)
//	    a[i] = ...
//	  else 
//	     a[i] = t
// Exported functions:
//
// void Dead_Store_Eliminate_Arrays(ARRAY_DIRECTED_GRAPH16 *dep_graph)
//
//
//


/**
*** $Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/be/lno/dead.h $
**/

#ifndef DEAD_RCS_ID
#define DEAD_RCS_ID
#ifdef _KEEP_RCS_ID
static char *dead = "$Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/be/lno/dead.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */
#endif

#ifndef DEAD_DECLARE

#define DEAD_DECLARE

void Dead_Store_Eliminate_Arrays(ARRAY_DIRECTED_GRAPH16 *dep_graph);
void Process_Store(WN *, VINDEX16 , ARRAY_DIRECTED_GRAPH16 *);

#endif  // DEAD_DECLARE

