//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_upc_cse.h
// $Revision: 1.16 $
// $Date: 2006/10/04 06:37:31 $
// $Author: wychen $
// $Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/be/opt/opt_upc_cse.h $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
// ====================================================================


#ifndef opt_upc_cseINCLUDED
#define opt_upc_cseINCLUDED	"opt_upc_cse.h"

// forward declaration
class CFG;
class BB_NODE;
class OPT_STAB;
class CODEMAP;
class CODEREP;

typedef struct {
  BB_NODE* bb;
  STMTREP* stmt;
} seq_point_t;


class CSE_NODE {
private:
  int num_uses; 
  TY_IDX _local_ty;
  CODEREP* _shared_cr;
  bool _merged;
  MEM_POOL* _pool;

public:

  //the program point where the shared load value becomes defined
  //stmt == NULL means the point is at beginning of block (defined by phi node)
  //bb == NULL means optimization is not profitable
  seq_point_t _def_point;

  //Keep track of stmts where the shared_cr appears in
  //Note the length of the list may be less than num_uses,
  //since one stmt may contain multiple uses.
  DYN_ARRAY<STMTREP*> _use_stmts;

  CSE_NODE(CODEREP* shared_cr, MEM_POOL *pool): _use_stmts(pool) 
  {
    num_uses = 0;
    _shared_cr = shared_cr;
    _local_ty = 0;
    _pool = pool;
    _merged = false;
  }

  ~CSE_NODE() {
    _use_stmts.Free_array();
  }

  MEM_POOL * Loc_pool() {
    return _pool;
  }

  //Node is the result of coalescing two field accesses to same struct
  bool Is_merged() {
    return _merged;
  }
  
  void Set_merged(bool merged) {
    _merged = merged;
  }

  void Set_shared_cr(CODEREP *cr) {
    _shared_cr = cr;
  }

  CODEREP *Shared_cr() {
    return _shared_cr;
  }

  void Set_local_ty(TY_IDX ty) {
    _local_ty = ty;
  }

  TY_IDX Local_ty() {
    return _local_ty;
  }
  
  void Add_use(STMTREP* stmt) {
      if (_use_stmts.Elements() == 0 || _use_stmts[_use_stmts.Lastidx()] != stmt) {
	  _use_stmts.AddElement(stmt);
      }
      num_uses++;
  }

  void Adjust_def_point();
  
  void Fix_speculative_load();

  bool Is_add() {
    return _shared_cr->Kind() == CK_OP;
  }

  bool Is_profitable();

  bool Is_local();

  bool Overlaps(CSE_NODE* node);

  void Print(FILE* file = stderr);  
};


class WRITE_SYNCS {
public:
  DYN_ARRAY<seq_point_t> _syncs;
  sync_handle_t* _handle;
  STMTREP* _write; //the write stmt
  
  WRITE_SYNCS(sync_handle_t* handle, STMTREP* write, MEM_POOL *pool): _syncs(pool)
  {
    _handle = handle;
    _write = write;
  }

  ~WRITE_SYNCS() 
  {
    _syncs.Free_array();
  }

  //
  //  If stmt != NULL, add sync right before the statement.
  //  else add sync as the last statement of bb
  //  Return false if a sync is already inserted in the same place.
  //
  void Add_sync(STMTREP* stmt, BB_NODE* bb) {
    seq_point_t pt;
    for (int i = 0; i < _syncs.Elements(); i++) {
      pt = _syncs[i];
      if (stmt != NULL && stmt == pt.stmt) {
	return;
      }
      if (bb != NULL && bb == pt.bb) {
	return;
      }
    }
    pt.stmt = stmt;
    pt.bb = bb;
    _syncs.AddElement(pt);
    if (_syncs.Elements() > 1) {
      _handle->invalidate = true;
    }
  }

  bool Is_profitable();

  int Num_syncs() {
    return _syncs.Elements();
  }

};

class UPC_CSE {
private:
  OPT_STAB   *_opt_stab;        // the optimizer symtab
  CFG        *_cfg;             // the control flow graph
  CODEMAP    *_htable;          // the hash table
  MEM_POOL   *_loc_pool;        // the permenant pool for new nodes
  UINT counter; //some debuging counter for split-phase calls
  
  DYN_ARRAY<CSE_NODE*> _new_cses;
  DYN_ARRAY<CSE_NODE*> _pre_adds;
  DYN_ARRAY<WRITE_SYNCS*> _write_syncs;  //one for each write

  OPT_STAB *Opt_stab(void)const { return _opt_stab; }
  CFG      *Cfg(void) const     { return _cfg; }
  CODEMAP  *Htable(void) const 	{ return _htable; }
  MEM_POOL *Loc_pool(void)const { return _loc_pool; }

  UPC_CSE(void);               // REQUIRED UNDEFINED UNWANTED methods
  UPC_CSE(const UPC_CSE&);    // REQUIRED UNDEFINED UNWANTED methods
  UPC_CSE& operator = (const UPC_CSE&); // REQUIRED UNDEFINED UNWANTED methods

public:

  CONSISTENCY_class *_bb_consistency; //mark strict/relaxed for each block

  UPC_CSE( CODEMAP *htable, OPT_STAB *opt_stab, CFG *cfg, MEM_POOL *lpool):
    _opt_stab(opt_stab), _htable(htable), _cfg(cfg), _loc_pool(lpool), 
    _new_cses(lpool), _pre_adds(lpool), _write_syncs(lpool)
    {
      counter = 0;
      _bb_consistency = CXX_NEW_ARRAY(CONSISTENCY_class, _cfg->Total_bb_count(), _loc_pool);
      for (int i = 0; i < _cfg->Total_bb_count(); i++) {
	_bb_consistency[i] = NO_CONSISTENCY;
      }
    }
  ~UPC_CSE(void) { 
    _new_cses.Free_array();
    _pre_adds.Free_array();
    _write_syncs.Free_array();
  }

  seq_point_t Find_cr_def_point(CODEREP* cr);
  CODEREP *Gen_temp_cr(TY_IDX ty, char* name);
  void     Mark_shared_load(CODEREP *cr, STMTREP* stmt);
  void     Mark_shared_load_rec(CODEREP *cr, STMTREP* stmt);
  void     Comp_usecnt(CODEREP *cr);
  void     Mark_cse_stmt(STMTREP* stmt);
  void     Insert_new_def(seq_point_t def_point, STMTREP* store);
  void     Code_gen(bool is_add);
  void     Mark_shared_add_rec(CODEREP* cr, STMTREP* stmt);
  void     Insert_shared_add_defs();
  void     Insert_sync(STMTREP* stmt, sync_handle_t* handle, BB_NODE* bb = NULL);
  void     Split_phase_write(STMTREP* write, AUX_ID var);
  void     Write_code_gen();
  void     Merge_node();
  sync_handle_t *Create_sync_handle(ST* st);
  void     Nbi_bulk_call_in_loop(BB_LOOP* loop);
  void     Do_Auto_NB(BB_NODE* bb);
  void     Set_consistency_info();
 
}; // end of class UPC_CSE

#endif  // opt_upc_cse_INCLUDED
