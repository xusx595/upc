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


/***********************************************************************
 *
 * This file provides three cache related classes.
 *
 * Exported type:
 *
 *  CACHE_PARAMETERS: store the parameters of the cache(s)
 *  
 * Exported function
 *
 *  void Initialize ()
 *      Initialize the cache data parameters
 *
 *  INT LineSize (INT level)
 *      Return the cache line size for the specified level (1 or 2)
 *
 *  INT64 EffSize (INT level)
 *      Return the effective cache size for the specified level
 *
 *  INT Levels ()
 *      Return the number of levels in the cache
 *
 *  BOOL Level1_Really_Level2 ()
 *      Return TRUE if L1 was disabled for prefetching or for Mhd.
 *
 * Exported type:
 *
 * VEC_VOLUME:        the volume of data accessed by a loop
 *
 * Exported functions
 *
 *  VEC_VOLUME ()
 *      constructor, initialize to 0
 *
 *  VEC_VOLUME (const VEC_VOLUME& vol)
 *      constructor, initialize to supplied volume
 *
 *  VEC_VOLUME (INT vol_1, INT vol_2)
 *      constructor, initialize to supplied values
 *
 *  VEC_VOLUME&    operator *= (INT lines)
 *      multiply the values by the supplied number of lines
 *
 *  VEC_VOLUME&    operator += (const VEC_VOLUME& vol)
 *      add the supplied volume to this volume
 *
 *  BOOL Localized ()
 *      return TRUE if the volume is localized in either level of the cache
 *
 *  void Print (FILE* fp)
 *      print the volume to FILE*
 *
 *
 * Exported type:
 *
 *  VEC_LOCLOOP:       based on the volume of data, is a loop localized or not
 *
 * Exported functions
 *
 *  VEC_LOCLOOP ()
 *      constructor, initialize to not-localized
 *
 *  VEC_LOCLOOP* operator= (const VEC_LOCLOOP& locloop)
 *      copy given locloop into this, return this
 *
 *  BOOL Update (mINT16 depth, VEC_VOLUME vol)
 *      Called with the depth and volume of the current loop on the
 *      incoming locloop of the outer loops.
 *      Update the locloop based on the supplied vol -- i.e. if the volume
 *      didn't fit in the cache before, but does for this loop, store
 *      it's depth. Do this for both L1 and L2 caches as appropriate.
 *      Return TRUE if this is the outermost loop to become localized
 *      in the furthest cache level (typically L2).
 *      
 *  BOOL Localized ()
 *      Return true if localized in any cache level, false otherwise
 *
 *  BOOL Localized_1L ()
 *      Return true if localized in L1
 *
 *  BOOL Localized_2L ()
 *      Return true if localized in L2
 *
 *  mINT16 Loop_1L ()
 *      Return depth of outermost loop that is localized in L1, -1 otherwise
 *
 *  mINT16 Loop_2L ()
 *      Return depth of outermost loop that is localized in L2, -1 otherwise
 *
 *  void Print (FILE* fp)
 *      Print to FILE*
 *
 *
 ***********************************************************************/

#ifndef vec_cache_INCLUDED
#define vec_cache_INCLUDED

#include "config_targ.h"
#include "config_cache.h"
#include "config_lno.h"
#include "wn.h"
#include "pf_common.h"
#include "pf_cache.h"

extern CACHE_PARAMETERS Cache;

class VEC_VOLUME {
  VEC_VOLUME* operator= (const VEC_VOLUME&);
public:
  UINT32    vol_1L;
  UINT32    vol_2L;
  VEC_VOLUME () {
    vol_1L = 0;
    vol_2L = 0;
  }
  VEC_VOLUME (const VEC_VOLUME& vol) {
    vol_1L = vol.vol_1L;
    vol_2L = vol.vol_2L;
  }
  VEC_VOLUME (INT vol_1, INT vol_2) {
    vol_1L = vol_1;
    vol_2L = vol_2;
  }
  ~VEC_VOLUME (void) {}
  VEC_VOLUME&    operator *= (INT lines) {
    vol_1L *= lines;
    vol_2L *= lines;
    return *this;
  }
  VEC_VOLUME&    operator += (const VEC_VOLUME& vol) {
    vol_1L += vol.vol_1L;
    vol_2L += vol.vol_2L;
    return *this;
  }
  BOOL Localized () {
    if (vol_1L <= Cache.EffSize(1)) return TRUE;
    if ((Cache.EffSize(2) > 0) && (vol_2L <= Cache.EffSize(2)))
      return TRUE;
    return FALSE;
  }
  BOOL Localized_1L () { return (vol_1L <= Cache.EffSize(1)); }
  BOOL Localized_2L () { return (vol_2L <= Cache.EffSize(2)); }
  void Print (FILE* fp) {
    fprintf (fp, "Vol: L1 %6d bytes", vol_1L);
    if (Cache.Levels()>1)
      fprintf (fp, ", L2 %6d bytes", vol_2L);
    fprintf (fp, "\n");
  }
};

class VEC_LOCLOOP {
  mINT16    _loop_1L;    // depth of loop localized in 1st-level cache, else -1
  mINT16    _loop_2L;    // depth of loop localized in 2nd-level cache, else -1
  mINT16    _confidence_1L; // confidence of that loop
  mINT16    _confidence_2L; // ditto
  BOOL      _while_temporal_1L;    // temporal locality coz inside a while-loop
  BOOL      _while_temporal_2L;    // temporal locality coz inside a while-loop
public:
  VEC_LOCLOOP () {
    _loop_1L = _loop_2L = -1;
    _confidence_1L = _confidence_2L = 3;
    _while_temporal_1L = _while_temporal_2L = FALSE;
  }
  VEC_LOCLOOP* operator= (const VEC_LOCLOOP& locloop) {
    _loop_1L = locloop._loop_1L;
    _loop_2L = locloop._loop_2L;
    _confidence_1L = locloop._confidence_1L;
    _confidence_2L = locloop._confidence_2L;
    _while_temporal_1L = locloop._while_temporal_1L;
    _while_temporal_2L = locloop._while_temporal_2L;
    return this;
  }
  /* Return TRUE if the outermost cache level has only now become
   * localized, FALSE otherwise.
   */
  BOOL Update (mINT16 depth, VEC_VOLUME vol, mINT16 confidence) {
    BOOL updated = FALSE;
    switch (Cache.Levels()) {
    case 1:
      if ((_loop_1L == -1) && (vol.vol_1L < Cache.EffSize(1))) {
        _loop_1L = depth;
        _confidence_1L = confidence;
        updated = TRUE;
      }
      break;
    case 2:
      // if it fits in L1, must also fit in L2
      Is_True ((_loop_1L == -1) || (_loop_2L != -1),
               ("Funny loop -- localized in L1 but not in L2\n"));
      if ((_loop_1L == -1) && (vol.vol_1L < Cache.EffSize(1))) {
        _loop_1L = depth;
        _confidence_1L = confidence;
      }
      if ((_loop_2L == -1) && (vol.vol_2L < Cache.EffSize(2))) {
        _loop_2L = depth;
        _confidence_2L = confidence;
        updated = TRUE;
      }
      break;
    default:
      Cache.Print(stdout);
      FmtAssert (0, ("Cache has (%d) more than 2 levels\n", Cache.Levels()));
    }
    return updated;
  }
  BOOL Localized () {
    return ((_loop_2L != -1) || (_loop_1L != -1));
  }
  BOOL Localized_1L () { return (_loop_1L != -1); }
  BOOL Localized_2L () { return (_loop_2L != -1); }
  mINT16 Loop_1L ()    const { return _loop_1L; }
  mINT16 Loop_2L ()    const { return _loop_2L; }
  mINT16 Confidence_1L () const { return _confidence_1L; }
  mINT16 Confidence_2L () const { return _confidence_2L; }
  BOOL While_Temporal_1L () const { return _while_temporal_1L; }
  BOOL While_Temporal_2L () const { return _while_temporal_2L; }
  void Set_While_Temporal_1L () { _while_temporal_1L = TRUE; }
  void Set_While_Temporal_2L () { _while_temporal_2L = TRUE; }
  void Print (FILE* fp) {
    fprintf (fp, "Localized loops: _1L --> %d (conf: %d%s)",
             _loop_1L, _confidence_1L,
             (_while_temporal_1L?" while-temporal":""));
    if (Cache.Levels()>1)
      fprintf (fp, ", _2L --> %d (conf: %d%s)", _loop_2L, _confidence_2L,
               (_while_temporal_2L?" while-temporal":""));
    fprintf (fp, "\n");
  }
};

#endif // pf_cache_INCLUDED
