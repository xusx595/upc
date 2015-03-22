/* Declarations for upc-act.c.
   Copyright (C) 2001 Free Software Foundation, Inc.
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

/*** Public Interface (procedures) ***/

/* used by yyparse */
extern tree upc_blocksizeof			PARAMS((tree));
extern tree upc_localsizeof			PARAMS((tree));
extern tree upc_elemsizeof                      PARAMS((tree));
extern void upc_expand_sync                     PARAMS((int, tree));
extern tree bupc_assert_type                    PARAMS((tree, tree));

/* used by UPC-specific routines (ie, upc-share.c) */

/* Non-zero if THREADS is specified at compile-time via the
   "-upc-threads" switch. */
extern int threads_int;

/* Non-zero if THREADS is specified at compile-time, and its value
   is an integral power of 2 greater than 0. */
extern tree threads_log;

#ifdef RTX_CODE
/* An rtx sequence used to fetch the value of THREADS */
extern rtx threads_rtx;

/* UPC support functions */
extern rtx upc_barrier_libfunc;
extern rtx upc_notify_libfunc;
extern rtx upc_wait_libfunc;
extern rtx upc_getaddr_libfunc;

#endif
