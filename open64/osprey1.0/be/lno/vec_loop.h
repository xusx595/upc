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


// c-or-c++
/*
 *      Data structures for a do loop node
 *      ----------------------------------
 *
 * Exported Type:
 *
 *  VEC_SPLIT_VECTOR
 *
 *      Store how loops got versioned.
 *
 * Exported Functions
 *
 *  VEC_SPLIT_VECTOR ()
 *  
 *      Constructor. Initialize to NULL.
 *
 *  VEC_SPLIT_VECTOR (mINT16 depth, 
 *                   mINT16 count, 
 *                   mINT16* vec, 
 *                   VEC_LOOPNODE* loopnode)
 *
 *      Constructor given the various values.
 *
 *  BOOL Empty ()
 *
 *      Return whether splits are empty or not.
 *
 *  void Update (VEC_SPLIT_VECTOR split_vec)
 *
 *      Given another split_vector, update this to be current best
 *      based on the one that is deepest, and with highest count.
 *      CXX_DELETE the incoming split_vec. ALWAYS.
 *
 *  VEC_LOOPNODE* Get_Loop ()
 *
 *      Return the lowest (deepest) loop in the chosen version vector.
 *      This allows us to uniquely identify the loops to be versioned by 
 *      walking up the tree of loops from this leaf loop.
 *
 *  mINT16       Get_Depth ()
 *
 *      Return the depth of the split vector.
 *
 *  mINT16       *Get_Vector ()
 *
 *      Return the split/version vector.
 *
 *  void Print (FILE* fp)
 *
 *      Print the split vector.
 *
 *
 * Exported Type
 *
 *  VEC_LOOPNODE
 *      A node corresponding to a do loop in the original code
 *      Contains a parent pointer to the closent enclosing do loop,
 *      and contains children pointers to the immediately nested 
 *      do loops within this do loop.
 *
 * Private Functions
 *
 *  void   Add_Ref (WN* wn_array)
 *
 *      Add a reference to this loop, if it is well-behaved.
 *
 *  void  Process_Refs (const WN* wn)
 *
 *      Walk the whirl code looking for array references,
 *      adding them to the references in this loop.
 *
 *  VEC_VOLUME Volume ()
 *
 *      Compute the volume of data referenced in this loop.
 *
 *  VEC_VOLUME Volume_For_Outer (mINT16 depth)
 *
 *      Find the volume of the references in this loop, but for an outer
 *      loop at the given depth.
 *
 *  void      Find_Loc_Loops (VEC_LOCLOOP locloop)
 *
 *      locloop is the localized status of the outer loops. This routine 
 *      updates the localized status including this loop, and then 
 *      walks the localized loops  to compute the prefetch vectors within 
 *      the localized space. Also count the number of lines for the various 
 *      UGSs, that are then used to determine ultimate split.
 *
 *  VEC_SPLIT_VECTOR*   Find_Split_Vector ()
 *
 *      Find the best split vector, based on deepest, with highest count.
 *
 * Exported Functions
 *
 *  VEC_LOOPNODE(VEC_LOOPNODE* parent, WN* code, mINT16 depth)
 *
 *      Constructor, given parent and actual do loop whirl tree
 *
 *  ~VEC_LOOPNODE ()
 *
 *      Destructor -- delete all the children loops and the bases.
 *
 *  void Add_Child (VEC_LOOPNODE *childnode)
 *
 *      add childnode as a child do loop
 *
 *  void Process_Refs ()
 *
 *      Process all the references within this do loop
 *
 *  void Process_Loop ()
 *
 *      First process all the references in this loop,
 *      then process the children (nested) loops
 *
 *  void  Build_Base_LGs ()
 *
 *      Build the base LGs for all base arrays, all UGSs
 *
 *  void  Process_PU_Volume ()
 *
 *      Do volume computation for all the nested loops.
 *
 *  DO_LOOP_INFO* Get_LoopInfo ()
 *
 *      get the do loop info
 *
 *  void  Gen_Prefetch (VEC_SPLIT_VECTOR*)
 *
 *      Given how the loops were versioned, generate prefetches for all refs
 *      in this loop.
 *
 *  void  Process_Prefetch ()
 *
 *      Call Gen_Prefetch for all the children nodes in this PU.
 *
 *  void  Split_Loops  (VEC_SPLIT_VECTOR* split_vec)
 *
 *      Given how loops should be versioned, actually do the versioning.
 *
 *  void  Process_Loc_Loops ()
 *  
 *      Given localized loops, for each loop in PU calculate the desired split.
 *
 *  mINT16   Get_Depth ()
 *
 *      Return the depth of this loop, outermost loop 0, PU is -1.
 *
 *  VEC_LOOPNODE* Get_Parent ()
 *
 *      Return the parent of this loopnode
 *
 *  VEC_LOCLOOP    Get_locloop ()
 *
 *      Return the info for localized loops stored in each loop.
 *
 *  void Print (FILE*)
 *
 *      standard print routine
 *
 *
 *
 *  mINT16 Loop_Confidence (DO_LOOP_INFO* dli)
 *
 *      Return confidence based on the given loop-info.
 *
 ***********************************************************************/


#ifndef vec_loop_INCLUDED
#define vec_loop_INCLUDED

#include "defs.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "wn.h"
#include "lnopt_main.h"
#include "pf_common.h"
#include "vec_volume.h"
#include "vec_ref.h"

extern MEM_POOL *VEC_mpool;

class VEC_BASE_ARRAY;
class VEC_LOOPNODE;


class VEC_LOOPNODE;

class VEC_SPLIT_VECTOR {
  mINT16    _depth;         // depth of the split vector
  mINT16    _count;         // number of lines
  mINT16    *_vec;          // the split vector itself, of depth _depth
  VEC_LOOPNODE* _loopnode;   /* the lowest level loopnode in the chosen split
                             * This is useful since there may be many loops
                             * at the same level. Storing the "leaf" loopnode 
                             * allows us to walk up the tree of loops, 
                             * identifying the ones that should get versioned.
                             */

  // VEC_SPLIT_VECTOR (void);
  VEC_SPLIT_VECTOR (const VEC_SPLIT_VECTOR&);
  VEC_SPLIT_VECTOR* operator= (const VEC_SPLIT_VECTOR&);
public:
  VEC_SPLIT_VECTOR () {
    _depth = _count = 0;
    _vec = NULL;
    _loopnode = NULL;
  }
  VEC_SPLIT_VECTOR (mINT16 depth,
                   mINT16 count,
                   mINT16* vec,
                   VEC_LOOPNODE* loopnode) {
    _depth = depth;
    _count = count; 
    _vec = vec;
    _loopnode = loopnode;
  }
  void Copy (VEC_SPLIT_VECTOR* split_vec) {
    _depth = split_vec->_depth;
    _count = split_vec->_count;
    _vec = split_vec->_vec;
    _loopnode = split_vec->_loopnode;
  }
    
  BOOL Empty () {
    if (_vec == NULL) return TRUE;
    for (INT i=0; i<_depth; i++)
      if (_vec[i] > 1) return FALSE;
    return TRUE;
  }
  void Update (VEC_SPLIT_VECTOR* split_vec) {
    if (split_vec == NULL) return;
    Is_True (split_vec->_vec,
             ("Split_vec: Update - got an empty split_vec\n"));
    // make sure that some split is required
    INT i;
    for (i=0; i<split_vec->_depth-1; i++)
      if (split_vec->_vec[i] !=  0) break;
    if (i == (split_vec->_depth-1)) {
      Is_True (FALSE, ("split_vec:Update - got an empty vector\n"));
      CXX_DELETE (split_vec, VEC_mpool);
      return;
    }
    // ok, so this is a real split vector
    // if I am empty, update
    if (Empty()) {
      Is_True (FALSE, ("split_vec: update - why am i empty?\n"));
      return;
    }
    // these are the only real cases
    if (split_vec->_depth < _depth) {
      CXX_DELETE (split_vec, VEC_mpool);
      return;
    }
    if ((split_vec->_depth > _depth) ||
        (split_vec->_count > _count)) {
      // use the new split vector
      Copy (split_vec);
      CXX_DELETE (split_vec, VEC_mpool);
      return;
    }
  }
  VEC_LOOPNODE* Get_Loop () const { return _loopnode; }
  mINT16       Get_Depth () const { return _depth; }
  mINT16       *Get_Vector () const { return _vec; }
  void Print (FILE* fp) {
    if (Empty()) 
      fprintf (fp, "Split vector is Empty\n");
    else {
      fprintf (fp, "Split Vector: depth - %d, count - %d, loopnode - 0x%p, Vector - ",
               _depth, _count, _loopnode);
      for (INT i=0; i<_depth; i++) fprintf (fp, " %3d ", _vec[i]);
      fprintf (fp, "\n");
    }
  }
};


typedef STACK<VEC_LOOPNODE*> VEC_LOOPNODE_DA;
typedef STACK<VEC_BASE_ARRAY*> VEC_BASE_ARRAY_DA;

class VEC_LOOPNODE {
 protected:
  VEC_LOOPNODE       *_parent;       // pointer to parent loop in this graph
  VEC_LOOPNODE_DA    _child;         // stack of pointers to children
  VEC_BASE_ARRAY_DA  _bases;         // references directly within this loop
  WN                *_code;         // pointer to whirl node for this loop
  INT               _num_bad;
  mINT16            _depth;         // depth in loop nest (outermost loop is 0)
  mINT16            _volume_confidence;
                         // confidence in vol computation
                         // 3 == very high
                         // 2 == symbolic, but maxiters not too diff
                         // 1 == symbolic
                         // This value is basically minimum of values for
                         // each nested loop, and does NOT include "this" loop.
                         // This is coz localization depends on single-iter vol

  VEC_VOLUME         _single_iter;   // volume (in bytes) in a single iteration
  VEC_VOLUME         _total_iter;    // volume (in bytes) of all iterations
  INT               _manual_volume; /* volume (bytes) of manually prefetched
                                     * data
                                     */

  VEC_LOCLOOP        _locloop;       // the localized loops
  mINT16            _split_num;     // splitting factor for this loop, if any
                                    // used just for verbose printing
  VEC_SPLIT_VECTOR*  _split_vec;     // store the chosen versioning
  // private methods
  virtual void              Add_Ref (WN* wn_array);
  virtual void              Process_Refs (const WN* wn);
 
  VEC_SPLIT_VECTOR*   Find_Split_Vector ();

  VEC_LOOPNODE (void);
  VEC_LOOPNODE (const VEC_LOOPNODE&);
  VEC_LOOPNODE* operator= (const VEC_LOOPNODE&);
public:
  VEC_VOLUME         Volume_For_Outer (mINT16 depth);
  VEC_LOOPNODE (VEC_LOOPNODE *parent, WN *code, mINT16 depth) :
    _child(VEC_mpool), _bases(VEC_mpool) {
    _parent     = parent;
    _code       = code;
    _num_bad    = 0;
    _depth      = depth;
    _volume_confidence = 3;
    _manual_volume = 0;
    _split_vec  = NULL;
    _split_num  = 0;
  }
  ~VEC_LOOPNODE ();
  virtual void Add_Child (VEC_LOOPNODE *childnode) { 
    _child.Push (childnode);
  }
  INT       Num_Children () { return _child.Elements(); }
  VEC_LOOPNODE *Get_Child (INT i) { return _child.Bottom_nth(i); }
  virtual void      Process_Loop ();
  void      Build_Base_LGs ();
  virtual VEC_VOLUME Volume ();
  VEC_VOLUME Volume_Within_While (WN* while_wn);
  void      Find_Loc_Loops (VEC_LOCLOOP locloop);
  void      Process_PU_Volume () {
    for (INT i=0; i<_child.Elements(); i++) _child.Bottom_nth(i)->Volume ();
  }

  DO_LOOP_INFO* Get_LoopInfo () {
    DO_LOOP_INFO* dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map, _code);
    FmtAssert(dli, ("Get_LoopInfo(): Unmarked do loop\n"));
    return dli;
  }

  void  Gen_Prefetch (VEC_SPLIT_VECTOR*);
  void  Process_Prefetch () {
    for (INT i=0; i<_child.Elements(); i++)
      _child.Bottom_nth(i)->Gen_Prefetch(NULL);
  }
  void  Split_Loops  (VEC_SPLIT_VECTOR* split_vec);
  void  Process_Loc_Loops () {
    VEC_LOCLOOP tmp;
    for (INT i=0; i<_child.Elements(); i++) {
      _child.Bottom_nth(i)->Find_Loc_Loops (tmp);
    }
  }
  mINT16        Get_Depth ()  const { return _depth; }
  VEC_LOOPNODE   *Get_Parent () const { return _parent; }
  VEC_LOCLOOP    Get_locloop () const { return _locloop; }
  mINT16        Get_Confidence () const { return _volume_confidence; }
  VEC_VOLUME     Get_Total () const { return _total_iter; }
  WN            *Get_Code () const { return _code; }
  void Print (FILE *fp);
  void Print_Structure ();
  void Print_Volume ();
  void Print_Splits ();
};

extern mINT16 Loop_Confidence (DO_LOOP_INFO* dli);
extern BOOL Check_Version_Map (WN* body_orig, WN* body_new);
extern WN* While_Before_Do (WN* do_wn);
#endif // vec_loop_INCLUDED
