/* UPC shared memory access routines
   Copyright (C) 1987, 91-98, 1999 Free Software Foundation, Inc.
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

/* This is the 4th arg to `expand_expr'.
   EXPAND_SUM means it is ok to return a PLUS rtx or MULT rtx.
   EXPAND_INITIALIZER is similar but also record any labels on forced_labels.
   EXPAND_CONST_ADDRESS means it is ok to return a MEM whose address
    is a constant that is not a legitimate address.
   EXPAND_MEMORY_USE_* are explained below.
   EXPAND_SHARED_ADDR_ONLY means that only the remote address is needed,
    to avoid access of the remote data */
enum expand_modifier {EXPAND_NORMAL, EXPAND_SUM,
                      EXPAND_CONST_ADDRESS, EXPAND_INITIALIZER,
                      EXPAND_MEMORY_USE_WO, EXPAND_MEMORY_USE_RW,
                      EXPAND_MEMORY_USE_BAD, EXPAND_MEMORY_USE_DONT,
                      EXPAND_SHARED_ADDR_ONLY};





extern int upc_shared_ptr_cvt_op_p PARAMS((tree));
extern int upc_shared_ptr_op_p PARAMS((tree));
extern rtx expand_upc_shared_component_ref PARAMS((tree, rtx,
				   enum machine_mode, enum expand_modifier));
extern rtx expand_upc_shared_ptr_cvt PARAMS((tree, rtx, enum expand_modifier));
extern rtx expand_upc_shared_assignment PARAMS((tree, tree, int, int));
extern rtx expand_upc_shared_bit_field_assign PARAMS((tree, tree, tree,
						     int, tree, int, int));
extern rtx expand_upc_fetch_shared_scalar PARAMS((tree, rtx, enum machine_mode));
extern rtx expand_upc_get_indirect PARAMS((tree, rtx, enum machine_mode));
extern rtx expand_upc_shared_bit_field_ref PARAMS((tree, int, tree, rtx, enum machine_mode));
extern rtx expand_upc_shared_ptr_sum PARAMS((tree, enum expand_modifier));
extern rtx expand_upc_shared_ptr_diff PARAMS((tree, enum expand_modifier));
extern void set_threads_value PARAMS((char *));
int x_x_simp_safe PARAMS((tree, tree));


#define UPC_MAX_BLOCK_SIZE 0xffffff


#ifndef UPC_BARRIER_LIBCALL
#define UPC_BARRIER_LIBCALL "__upc_barrier"
#endif
#ifndef UPC_NOTIFY_LIBCALL
#define UPC_NOTIFY_LIBCALL "__upc_notify"
#endif
#ifndef UPC_WAIT_LIBCALL
#define UPC_WAIT_LIBCALL "__upc_wait"
#endif
#ifndef UPC_GETADDR_LIBCALL
#define UPC_GETADDR_LIBCALL "__getaddr"
#endif
