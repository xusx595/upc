#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"       // for Cur_PU_Name                                                                                  
#include "mempool.h"
#include "tracing.h"    /* for TFile */
#include "stab.h"
#include "irbdata.h"
#include "cxx_memory.h"
#include "be_symtab.h"
#include "upc_wn_util.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_main.h"
#include "bb_node_set.h"
#include "opt_util.h"
#include "opt_fold.h"
#include "opt_mu_chi.h"
#include "opt_sym.h"
#include "opt_alias_rule.h"
#include "opt_cvtl_rule.h"
#include "opt_upc_cse.h"


/*
 *  Perform redundancy elimination on UPC shared memory load. 
 *  
 *  While open64 includes a preSSA phase in WOPT, it does not raelly fit into 
 *  the source-to-source compilation scheme for our compiler,
 *  because in my experience the output of WOPT is generally too low level (type info stripped, etc.) 
 *  to be converted back to legal C code.  
 *  Instead, our strategy is to leave the local variables alone but 
 *  focus on eliminating redundant shared loads, which are translated into function calls 
 *  and therefore not subject to backend compiler's optimizations.
 *  The algorithm sketch:
 *
 *  for every shared vaiable v, create a new local temporary variable lv;
 *  replace all occurences of v with lv in the code;
 *  insert the assignment lv = v right after the definition point of v
 *    (for var this is a single point because of SSA;
 *     for ivar we need to compare the definition point of all variables that appear in the ivar's address,
 *     and pick the one that comes last.
 *    )
 *
 *  In the code generation phase, the assignment is converted into a nonblocking read call
 *  (upcr_get{p}_nb_shared), and a corresponding sync() call is generated right before each time 
 *  the value is used (with redundant sync calls eliminated).
 *
 *  The implementation is done in two phases.  The first phase marks, for each unique shared value
 *  in the program, its defintion point as well as a list of all uses for its value.  The second 
 *  phase performs the actual code generation, based on some heuristics that calculates the 
 *  optimization's proffitability.
 *
 *  Due to the limitation of the network interface, we currently do not perform speculative code motion
 *  that would place a remote read on a path that does not execute it in the original program.  
 *  Even if the safety for such speculative operations could be maintained (e.g., by checking pointer 
 *  validity before performing the read), the tranformation likely would never be profitable due to
 *  extra communication cost.
 *
 *  Some words on code generation for split-phase accesses:
 *  After the sync calls are inserted, later at code generation stage we need to patch 
 *  them with the correct handle coming from the init call.
 *  The problem is that the code generation is done on WHIRL nodes later during backend lowering,
 *  while the analysis is performed in WOPT on the SSA representation.  
 *  To pass the analysis information, we create a handle struct that links the init call with its 
 *  corresponding (possible mutliple) sync calls.  For sync calls the handle can be directly mapped 
 *  to their WN nodes using a WN_MAP.  For the init call, we store the handle as part of the CODEREP
 *  for the shared variable; mapping information is generated when converting SSA back to WHIRL at the 
 *  end of WOPT (opt_emit_template.h).  
 *  Later during backend lowering, when we encounter the init calls, we do the code gen 
 *  and store the sync handle into the handle struct, so that the later sync calls
 *  will get the right sync handle.
 *
 *
 */

bool Type_is_strict(TY_IDX ty) {

  if (!TY_is_shared(ty)) {
    return false;
  }

  if (TY_is_strict(ty)) {
    return true;
  }
  if (TY_is_relaxed(ty)) {
    return false;
  }

  return false;
}

//
//given two stmts, 
//return 1 if s1 must occur later than s2
//return -1 if s2 must occur later than s1
//return 0 if neither dominates another
//
static int Later_seq_point(seq_point_t s1, seq_point_t s2) {

  FmtAssert(s1.bb != NULL && s2.bb != NULL, (""));

  if (s1.bb == s2.bb) {
    if (s1.stmt == NULL) {
      return -1;
    } 
    if (s2.stmt == NULL) {
      return 1;
    }
    STMTREP_ITER stmt_iter(s1.bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt == s1.stmt) {
	return -1;
      }
      if (stmt == s2.stmt) {
	return 1;
      }
    }
  } else if (s1.bb->Dominates(s2.bb)) {
    return -1;
  } else if (s2.bb->Dominates(s1.bb)) {
    return 1;
  }
  
  //fprintf(stderr, "neither s1 nor s2 dominates the other\n");
  return 0;
}

//check if this coderep contains a strict access
//block level consistency pragmas are handled by checking the 
//_bb_consistency array in UPC_CSE
bool CODEREP::Has_strict_access() {

  switch (Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    return false;
  case CK_VAR:
    return Type_is_strict(Lod_ty());
  case CK_IVAR:
    return Type_is_strict(Ilod_ty());
  case CK_OP:
    for (int i = 0; i < Kid_count(); i++) {
      if (Get_opnd(i)->Has_strict_access()) {
	return true;
      }
    }
  }
  return false;
}

//
// The function is meaningful only when localization analysis is enabled.
// For shared loads, returns true if the load is always local, i.e., 
// the load is a IVAR whose base is a pointer that points to MYTHREAD.
// For shared ptr arithmetic, returns true if the pointer-to-shared always points to local data
//
bool CSE_NODE::Is_local() {

  if (!run_ptr_locality) {
    return false;
  }
  CODEREP* ptr_exp = NULL;
  if (_shared_cr->Kind() == CK_IVAR) {
    ptr_exp = _shared_cr->Ilod_base();
  } else if (_shared_cr->Kind() == CK_OP) {
    ptr_exp = _shared_cr;
  }
  if (ptr_exp != NULL) {
    while (ptr_exp->Kind() == CK_OP && ptr_exp->Opr() == OPR_TAS) {
      ptr_exp = ptr_exp->Opnd(0);
    }
    return ptr_exp->Thread_id() == POINTS_TO::LOCALITY_MYTHREAD; 
  }

  return false;
}

//
//check if it is profitable to perform the CSE by 
//emitting the shared load of cr right after the defintion stmt
//
bool CSE_NODE::Is_profitable() {

  if (Is_add()) {
    return num_uses > 1;
  }

  if (num_uses > 1) {
    //we've got potential redundancy.
    //FIXME: we could be more precise here by checking whether the loads
    //will be on the same path
    return true;
  }

  if (Is_local()) {
    //saving a runtime branch is probably always a good idea
    return true;
  }

  BB_NODE* def_bb = _def_point.bb;
  STMTREP* def_stmt = _def_point.stmt;
  STMTREP* use = _use_stmts[0];

  if (def_bb == use->Bb()) {
    STMTREP_ITER stmt_iter(def_bb->Stmtlist());
    stmt_iter.Init();
    if (def_stmt != NULL) {
      stmt_iter.Set_Cur(def_stmt);
      if (stmt_iter.Next() == use) {
	return false;
      }
    } else {
      if (stmt_iter.First() == use) {
	return false;
      }
    }
  }

  return true;  
}

//
//  Find the definition statement of a var/ivar
//
seq_point_t UPC_CSE::Find_cr_def_point(CODEREP* cr) {

  seq_point_t point;
  point.bb = Cfg()->Entry_bb();
  point.stmt = NULL;
  
  switch(cr->Kind()) {
  case CK_VAR:
    point.bb = cr->Defbb();
    point.stmt = cr->Get_defstmt();
    if (point.bb == NULL && !cr->Is_var_volatile()) {
      FmtAssert(0, ("no def for st: %s\n", Opt_stab()->Aux_stab_entry(cr->Aux_id())->Base_name()));
    }
    break;
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    break;
  case CK_IVAR: 
    {
      //for shared indirect loads, we have to check not only the def of the ivar itself,
      //but also the def of all variables that appear in the address
      CODEREP *vsym = cr->Get_ivar_vsym();
      //fprintf(stderr,"vsym for ivar:\n");
      point.bb = vsym->Defbb();
      point.stmt = vsym->Get_defstmt();    
      if (point.bb == NULL && !cr->Is_ivar_volatile()) {
	//can't find definition of the iload (this can happen for volatile pointers)
	if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	  fprintf(TFile, "no def for ivar\n");
	  cr->Print(0, TFile);
	}
	break;
      }
      seq_point_t addr = Find_cr_def_point(cr->Ilod_base());
      if (addr.bb == NULL || Later_seq_point(addr, point) == 1) {
	point = addr;
      }
      break;
    }
  case CK_OP:
     for (int i = 0; i < cr->Kid_count(); i++) {
       seq_point_t pt = Find_cr_def_point(cr->Get_opnd(i));
       if (pt.bb == NULL) {
	 return pt;
       }
       if (Later_seq_point(pt, point) == 1) {
	 point = pt;
       }
     }
     break;
  }
  return point;

}

CODEREP *UPC_CSE::Gen_temp_cr(TY_IDX ty, char * name) {

  AUX_ID aux = Opt_stab()->Create_temp(ty, name);
  return Htable()->Add_def(aux, 1, NULL, TY_mtype(ty), TY_mtype(ty), 0, ty, 0, FALSE);
}


// 
//  UPC_CSE::Mark_shared_load
//  Given a cr that is either a VAR or IVAR of some shared variable
//  Either create a new CSE_NODE or find an existing one where it belongs to
//
void UPC_CSE::Mark_shared_load(CODEREP* old, STMTREP* stmt) {

  for (int i = 0; i <=_new_cses.Lastidx(); i++) {
    CSE_NODE* node = _new_cses[i];
    if (node->Shared_cr() == old) {
      if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	fprintf(TFile, "found a reuse\n");
      }
      node->Add_use(stmt);
      return;
    }
  }

  bool field_in_shared_struct = false;
  //we need to create a new CSE_NODE
  seq_point_t def_point;
  TY_IDX ty;
  switch(old->Kind()) {
  case CK_VAR:
      if (old->Is_var_volatile()) {
	  return;
      }
    def_point = Find_cr_def_point(old);
    ty = old->Lod_ty();
    if (TY_kind(ty) == KIND_STRUCT && old->Field_id() != 0) {
      ty = Get_Field_Type(ty, old->Field_id());
      //The front end does not mark fields of shared structs as shared
      field_in_shared_struct = true;
    }
    break;
  case CK_IVAR:
      if (old->Is_ivar_volatile()) {
	  return;
      }
    def_point = Find_cr_def_point(old);
    ty = old->Ilod_ty();
    break;
  default:
    Fail_FmtAssertion(("Expected type %d for coderep"), old->Kind()); 
  }
  
  if (def_point.bb == NULL) {
    //missing def-use chain info for the shared variable, 
    //abort optimization
    return;
  }

  CSE_NODE* node = CXX_NEW(CSE_NODE(old, Loc_pool()), Loc_pool());
  if(old->Kind() == CK_IVAR && old->Ilod_base_ty()) {
    if (TY_is_shared(ty) && TY_kind(ty) == KIND_ARRAY) 
      ty = Get_Inner_Array_Type(ty);
  }
  node->Set_local_ty(field_in_shared_struct ? ty : Shared_To_Private_Type(ty));
  node->_def_point = def_point;
  node->Add_use(stmt);
  _new_cses.AddElement(node);
}

// 
// UPC_CSE::Mark_shared_load_rec
// Iterate through the cr to find and mark all shared relaxed loads
//
void UPC_CSE::Mark_shared_load_rec(CODEREP* cr, STMTREP* stmt) {
  
  TY_IDX ty;
  switch(cr->Kind()) {
  case CK_VAR: 
    ty = cr->Lod_ty();
    if (TY_is_shared(ty) && !Type_is_strict(ty)) {
      Mark_shared_load(cr, stmt);
    }
    break;
  case CK_IVAR:
    ty = cr->Ilod_ty();    
    if (cr->Opr() == OPR_PARM) {
      //IVAR may include PARM nodes, so need a special case here
      if (!TY_is_shared(ty)) {
        break;
      }
      Mark_shared_load_rec(cr->Ilod_base(), stmt);
    } else if (TY_is_shared(ty) && !Type_is_strict(ty)) {
      Mark_shared_load(cr, stmt);
    } else {
      Mark_shared_load_rec(cr->Ilod_base(), stmt);
    }
    break;
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    return;
  case CK_OP: 
    for (int i = 0; i < cr->Kid_count(); i++) {
      Mark_shared_load_rec(cr->Get_opnd(i), stmt);
    }
  }
}

void UPC_CSE::Mark_cse_stmt(STMTREP* stmt) {

  CODEREP* rhs = stmt->Rhs();
  if (rhs) {
    Mark_shared_load_rec(rhs, stmt);
  }

}

//Replace all occurrences of old_cr with new_cr in cr
static CODEREP* Replace_cr_rec(CODEREP* cr, CODEREP* old_cr, CODEREP* new_cr, bool is_add) {

  switch(cr->Kind()) {
  case CK_VAR: 
    if (!is_add && cr == old_cr)
      return new_cr;
    return NULL;
  case CK_IVAR: 
    {
      if (!is_add && cr == old_cr) {
	return new_cr;
      }
      if (cr->Ilod_base() != NULL) {
	CODEREP* kid_cr = Replace_cr_rec(cr->Ilod_base(), old_cr, new_cr, is_add);
	if (kid_cr != NULL) {
	  cr->Set_ilod_base(kid_cr);
	  if (!is_add) 
	    cr->Set_ilod_ty(new_cr->Lod_ty());
	  return cr;
	}
      } else {
	CODEREP* kid_cr = Replace_cr_rec(cr->Istr_base(), old_cr, new_cr, is_add);
	if (kid_cr != NULL) {
	  cr->Set_istr_base(kid_cr);
	  return cr;
	}
      }
      return NULL;
    }
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
    return NULL;
  case CK_OP: 
    {
      bool changed = false;
      for (int i = 0; i < cr->Kid_count(); i++) {
	CODEREP *kid = Replace_cr_rec(cr->Get_opnd(i), old_cr, new_cr, is_add);
	if (kid != NULL) {
	  changed = true;
	  cr->Set_opnd(i, kid);
	}
      }

      if (is_add && cr == old_cr) {
	  //if the node has a TAS, we have to keep it since it may be a 
	  //shared to pshared conversion
	  if (cr->Opr() == OPR_TAS) {
	      cr->Set_opnd(0, new_cr);
	      return cr;
	  } else {
	      return new_cr;
	  }
      }

      if (changed) {
	return cr;
      } else {
	return NULL;
      }
    }
  }
  return NULL;
}

void UPC_CSE::Mark_shared_add_rec(CODEREP* cr, STMTREP *stmt) {

  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
  case CK_VAR:
    return;
  case CK_IVAR:
    if (cr->Ilod_base() != NULL) 
      Mark_shared_add_rec(cr->Ilod_base(), stmt);
    else 
      Mark_shared_add_rec(cr->Istr_base(), stmt);
    return;
  case CK_OP:
    for (int i = 0; i < cr->Kid_count(); i++) {
      Mark_shared_add_rec(cr->Get_opnd(i), stmt);
    }
    
    if (cr->Opr() == OPR_TAS && cr->Get_opnd(0)->Kind() == CK_OP && cr->Get_opnd(0)->Opr() == OPR_ADD) {
      if (Type_Is_Shared_Ptr(cr->Ty_index(), true)) {
	//fprintf(stderr, "found shared add: %p\n", cr);
	//cr->Print(0, stderr);
	  CODEREP* add_cr = cr->Get_opnd(0);
	for (int i = 0; i < _pre_adds.Elements(); i++) {
	    if (_pre_adds[i]->Shared_cr() == add_cr) {
		_pre_adds[i]->Add_use(stmt);
		return;
	    }
	}
	CSE_NODE* node = CXX_NEW(CSE_NODE(add_cr, Loc_pool()), Loc_pool());
	node->_def_point = Find_cr_def_point(add_cr);
	if (node->_def_point.bb != NULL) {
	  node->Set_local_ty(cr->Ty_index());
	  node->Add_use(stmt);
	  _pre_adds.AddElement(node);
	}
	return;
      }
    }
  }
}

//
//  Check if the two shared loads may overlap each other (i.e.,
//  they may both be in flight at the same time)
//  Note that we assume here that this always starts first.
//
bool CSE_NODE::Overlaps(CSE_NODE* node) {

  STMTREP_ITER stmt_iter(_def_point.bb->Stmtlist());
  stmt_iter.Init();

  //FIXME: for now, only coalesce two reads if they belong to the same block
  if (_def_point.bb == node->_def_point.bb) {
    if (_def_point.stmt == NULL && node->_def_point.stmt == NULL) {
      return true;
    }
    for (STMTREP* stmt = stmt_iter.First(); stmt != NULL; stmt = stmt_iter.Next()) {
      if (stmt == node->_def_point.stmt) {
	return true; 
      }
      for (int i = 0; i < _use_stmts.Elements(); i++) {
	if (stmt == _use_stmts[i]) {
	  return false;
	}
      }
    }
  }

  return false;

}

//disallow speculative code motion for shared load,
//by pushing down the def_point until it's in a block that dominates all uses
//FIXME: not complete yet as it doesn't handle the case when we can't
//just use a single def point without introducing speculation.
//We should handle this case by splitting the uses.
void CSE_NODE::Fix_speculative_load() {

    BB_NODE* bbs[64];
    int bbs_length = 0;
    bbs[bbs_length++] = _use_stmts[0]->Bb();

    for (int i = 1; i < _use_stmts.Elements(); i++) {
      bbs[bbs_length++] = _use_stmts[i]->Bb();
      for (int j = 0; j < bbs_length-1; j++) {
	if (bbs[j]->Dominates( _use_stmts[i]->Bb())) {
	  //no need to add this block (this relies on that uses are added in program order)
	  bbs_length--;
	  break;
	}
      }
    }

    if (bbs_length == 1) {
      //All uses are dominated by a single block
      BB_NODE* use_bb = bbs[0];
      while (!use_bb->Postdominates(_def_point.bb)) {
	_def_point.stmt = NULL;
	_def_point.bb = _def_point.bb->Ipdom();
	if (!_def_point.bb->Dominates(use_bb)) {
	  _def_point.bb = use_bb;
	  break;
	}
      }
    }

}

void CSE_NODE::Adjust_def_point() {

  int use_in_loop = 0;
  int use_after_loop = 0;

  //if the statement at def_point is a function call that returns something,
  //we need to plact the new defintion after the statement that reads the return value
  if (_def_point.stmt != NULL) {
    OPERATOR opr = _def_point.stmt->Opr();
    if (opr == OPR_CALL || opr == OPR_INTRINSIC_CALL ||
	opr == OPR_ICALL || opr == OPR_INTRINSIC_OP) {
      if (_def_point.stmt->Rtype() != MTYPE_V) {
	STMTREP_ITER stmt_iter(_def_point.bb->Stmtlist());
	STMTREP* stmt = NULL;
	FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	  if (stmt == _def_point.stmt) {
	    stmt = stmt_iter.Next();
	    break;
	  }
	}
	if (stmt != NULL) {
	  _def_point.stmt = stmt;
	}
      }
    }
  }

  switch (_def_point.bb->Kind()) {

    /*
      case BB_WHILEEND: 
      {
      STMTREP* branch = _def_point.bb->Branch_stmtrep();
      if (branch->Rhs()->Contains(_shared_cr)) {
      //fprintf(stderr, "shared variable used in loop branch\n");
      //we should leave the nonblocking call at the merge node
      return;
      }
      }
      //fallthru
      */

  case BB_DOEND: 
    //We have to distinguish the case where the cr is in the loop body vs.
    //where the cr is in bbs after the loop
    STMTREP* in_loop[256];
    STMTREP* after_loop[256];
    for (int i = 0; i <=_use_stmts.Lastidx(); i++) {
      BB_NODE* cur_bb = _use_stmts[i]->Bb();
      BB_NODE* body = _def_point.bb->Loopbody();
      if (body->Dominates(cur_bb)) {
	//cr is part of the loop
	in_loop[use_in_loop++] = _use_stmts[i];
      } else {
	after_loop[use_after_loop++] = _use_stmts[i];
      }
    }

    if (use_in_loop != 0) {
      _def_point.bb = _def_point.bb->Loopbody();
    } else {
      _def_point.bb = _def_point.bb->Looptail();
    }
    //Fixme: we still have the case where a variable is used both in and out of the loop
    //In this case we should probably create a new CSE_NODE (i.e., have one nonblocking call
    //inside the loop, and one at loop exit)
    if (use_in_loop != 0 && use_after_loop != 0) {
      _use_stmts.Resetidx();
      for (int i = 0; i < use_in_loop; i++) {
	Add_use(in_loop[i]);
      }
    }
    break;
  }

  
  if (!Is_add()) {
    Fix_speculative_load();
  }
}

sync_handle_t *UPC_CSE::Create_sync_handle(ST* st) { 

  sync_handle_t *handle = CXX_NEW(sync_handle_t, &upc_mem_pool);
  memset(handle, 0, sizeof(sync_handle_t));
  if (st != NULL) {
    handle->st = st;
  }
  return handle;
}


void UPC_CSE::Merge_node() {

  for (int i = 0; i < _new_cses.Elements(); i++) {
    CSE_NODE *node = _new_cses[i];
    CODEREP* cr = node->Shared_cr();
    if (cr->Kind() == CK_IVAR && TY_kind(TY_pointed(cr->Ilod_base_ty())) == KIND_STRUCT) {
      CODEREP* base = cr->Ilod_base();
      for (int j = i+1; j < _new_cses.Elements(); j++) {
	CSE_NODE* node2 = _new_cses[j];
	CODEREP* cr2 = node2->Shared_cr();
	if (cr2->Kind() == CK_IVAR && TY_kind(TY_pointed(cr2->Ilod_base_ty())) == KIND_STRUCT) {
	  if (base == cr2->Ilod_base() && node->Overlaps(node2)) {
	    //two shared crs have the same base
	    //we have to determine whether their range overlaps or not.
	    if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	      fprintf(TFile, "two nodes can be merged: \n");
	    }
	    node->Set_merged(true);  
	    node->Set_local_ty(Shared_To_Private_Type(TY_pointed(cr->Ilod_base_ty())));
	    //Add the uses of the second node to the first one
	    for (int k = 0; k < node2->_use_stmts.Elements(); k++) {
	      node->Add_use(node2->_use_stmts[k]);
	    }
	    //node->Set_shared_cr(base);
	    //Delete the second node by shifting everything in _new_cse by 1
	    for (int k = j+1; k < _new_cses.Elements(); k++) {
	      _new_cses[k-1] = _new_cses[k];
	    }
	    _new_cses.Decidx();
	  }
	}
      } 
    }
  }
}



//
// UPC_CSE::Code_gen()
// Perform the actual split phase code generation
// For each CSE_NODE, we perform the following:
//   Insert the nonblocking read right after the variable's defintion point
//   Eeplace all occurrences of the shared cr with the newly created local var
//   Insert the necessary syncs for each use of the variable
//
void UPC_CSE::Code_gen(bool is_add) {
 

  DYN_ARRAY<CSE_NODE*> *array;
  array = (is_add) ? &_pre_adds : &_new_cses;

  for (int i = array->Lastidx(); i >= 0; i--) {
    CSE_NODE *node = (*array)[i];
    node->Adjust_def_point();
    if (!node->Is_profitable()) {
      continue;
    }

    if (!is_add)
      if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	fprintf(TFile, "Optimize a shared get\n");
      }

    TY_IDX ty = node->Local_ty();
    CODEREP *local_cr = Gen_temp_cr(ty, (char*) (is_add ? "UPC_ADD" : "UPC_CSE"));
    CODEREP* rhs = node->Shared_cr();
    //fprintf(stderr, "local:\n");
    //local_cr->Print(0, stderr);
    //fprintf(stderr, "shared:\n");
    //node->Shared_cr()->Print(0, stderr);

    if (node->Is_local()) {
      if (is_add) {
	//propagate locality information to the newly created temp
	fprintf(stderr, "got here\n");
	local_cr->Set_thread_id(POINTS_TO::LOCALITY_MYTHREAD);
      } else {
	//for local loads we can bypass the runtime
	fprintf(stderr, "localizing\n");
	//rhs->Print(0, stderr);
	//cast the Ilod_base to local, then perform an iload
	CODEREP* tas = Alloc_stack_cr(0);
	tas->Init_expr(Pointer_Mtype == MTYPE_U4 ? OPC_U4TAS : OPC_U8TAS, node->Shared_cr()->Ilod_base());
	tas->Set_ty_index(Make_Pointer_Type(ty));
	tas = Htable()->Hash_Op(tas);
	
	CODEREP* load = Alloc_stack_cr(0);
	load->Init_ivar(rhs->Op(), rhs->Dtyp(), rhs->Ivar_occ(), rhs->Dsctyp(), ty,
			tas, NULL, rhs->Offset(), rhs->Mstore_size(), rhs->I_field_id());
	load->Set_ivar_mu_node(rhs->Ivar_mu_node());
	load->Set_ilod_base_ty(tas->Ty_index());
	rhs = Htable()->Hash_New_ivar(load);
      }
    }

    //Insert the non-blocking init call
    OPCODE opc = OPCODE_make_op(OPR_STID, MTYPE_V, TY_mtype(ty));
    STMTREP* store_st = CXX_NEW(STMTREP(opc), Loc_pool());
    store_st->Init(local_cr, rhs, opc);
    local_cr->Set_defstmt(store_st);
    Insert_new_def(node->_def_point, store_st);

    //replace all occurrences of the shared cr with the newly created local var
    for (int i = 0; i <= node->_use_stmts.Lastidx(); i++) {
      STMTREP* use = node->_use_stmts[i];
      CODEREP *new_rhs = Replace_cr_rec(use->Rhs(), node->Shared_cr(), local_cr, is_add);
      if (new_rhs != NULL) {
	use->Set_rhs(new_rhs);
      }
      if (is_add && use->Lhs() != NULL) {
	//shared add may also be in lhs 
	CODEREP* new_lhs = Replace_cr_rec(use->Lhs(), node->Shared_cr(), local_cr, is_add);
	if (new_lhs != NULL) {
	  use->Set_lhs(new_lhs);
	}
      }
    }

    if (is_add) {
      if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	fprintf(TFile, "PRE shared add\n");
      }
      //No need of syncs
      continue;
    } else if (node->Is_local()) {
      //don't need syncs for local loads
      continue;
    }

    //now generate all of the necessary syncs
    sync_handle_t *handle = Create_sync_handle(_opt_stab->Aux_stab_entry(local_cr->Aux_id())->St());
    local_cr->Set_handle(handle);

    if (node->_use_stmts.Elements() > 1) {
      //If more than one sync is generated for the shared access,
      //we conservatively assume we have to invalidate the handle after each sync.
      handle->invalidate = true;
    }

    for (int i = 0; i <= node->_use_stmts.Lastidx(); i++) {
      bool need_sync = true;
      STMTREP* use = node->_use_stmts[i];
      for (int j = i-1; j >= 0; j--) {
	//try to get rid of redundant syncs if we can
	STMTREP* old_sync = node->_use_stmts[j];
	if (old_sync->Bb()->Dominates(use->Bb())) {
	  if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	    fprintf(TFile, "eliminate a redundant sync\n");
	  }
	  need_sync = false;
	  break;
	}
      }

      if (need_sync) {
	Insert_sync(use, handle);
      }
    }
    
    //bug2510 - if there is a path from the new definition point to the function exit (post-dominator), 
    //insert a sync there, otherwise we leak handles
   //  BB_NODE *bby;
//     BB_NODE_SET_ITER bns_iter;
//     FOR_ALL_ELEM (bby, bns_iter, Init(bb->Rcfg_dom_frontier()))
//       Find_and_mark_cd_branch_live(bby);

    
  }
}

//
// Insert a sync() call before the given stmt.
// handle is used to connect the sync call to the earlier init statement,
// so that we can later supply the sync with the correct synchronization handle
// Stmt may be null, in which case we insert the sync as the last stmt in the bb
//
void UPC_CSE::Insert_sync(STMTREP* stmt, sync_handle_t* handle, BB_NODE* bb) {

  WN* wn = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 0);
  WN_intrinsic(wn) = INTRN_WAIT_SYNC_MEM;
  STMTREP* sync = CXX_NEW(STMTREP(WN_opcode(wn)), Loc_pool());
  sync->Set_wn(wn);
  BOOL proped = false;
  sync->Set_rhs(_htable->Add_expr(wn, _opt_stab, sync, &proped, NULL));
  if (stmt != NULL) {
    stmt->Bb()->Insert_stmtrep_before(sync, stmt);
  } else {
    //we put the sync as the last stmt in the block, except when
    //the block ends in branches
    STMTREP* last = bb->Last_stmtrep();
    if (last != NULL && 
	(last->Opr() == OPR_FALSEBR ||
	 last->Opr() == OPR_TRUEBR ||
	 last->Opr() == OPR_GOTO ||
	 last->Opr() == OPR_RETURN )) {
      bb->Insert_stmtrep_before(sync, last);
    } else {
      bb->Append_stmtrep(sync);
    }
  }


  WN_MAP_Set(upc_comm_map, wn, handle);
  sync->Set_wn(wn);
}

//
// Insert the assignments of the form
//   local_temp = shared_load
// into the program, right after the def stmt of the shared var/ivar.
// If the shared variable is defined at entry point, we insert the assignment at the top
// of the function.  The inserted statements must be after the UPC consistency pragmas
// (located in at the start of the second block) so that the shared loads can be lowered correctly
//

void UPC_CSE::Insert_new_def(seq_point_t def, STMTREP* local_init) {

  if (def.bb != Cfg()->Entry_bb()) {
    if (def.stmt != NULL) {
      def.bb->Insert_stmtrep_after(local_init, def.stmt);
    } else {
      //defined by a phi node
      def.bb->Prepend_stmtrep(local_init);
    }
  } else {
    //shared load is never redefined in function
    BB_NODE* def_bb = def.bb->Nth_succ(0);
    STMTREP_ITER stmt_iter(def_bb->Stmtlist());
    stmt_iter.Init();
    STMTREP* stmt;
    //upc pragmas should be at the beginning of a block
    for(stmt = stmt_iter.First(); stmt != NULL && stmt->Opr() == OPR_PRAGMA; stmt = stmt_iter.Next());
    
    if (stmt != NULL) {
      def_bb->Insert_stmtrep_before(local_init, stmt);
    } else {
      //Add to the end of the block
      def_bb->Append_stmtrep(local_init);
    }
  }
}

static bool Stmt_references_cr(STMTREP* stmt, CODEREP* cr) {

  CODEREP *rhs = stmt->Rhs(), *lhs = stmt->Lhs();

  //cr is read
  if ((rhs != NULL && rhs->Contains(cr))) {
    return true;
  }

  //cr may be refeerenced (it's in the stmt's mu-list)
  if (stmt->Has_mu()) {
    MU_LIST *mus = stmt->Mu_list();
    MU_LIST_ITER mu_iter;
    MU_NODE *mnode;
    FOR_ALL_NODE(mnode, mu_iter, Init(mus)) {
      //mnode->Print(stderr);
      if (mnode->OPND() == cr)
	return true;
    }
  }
  
  //cr may be redefined (check stmt's chi-list)
  if (stmt->Has_chi()) {
    CHI_LIST *chis = stmt->Chi_list();
    CHI_LIST_ITER chi_iter;
    CHI_NODE *cnode;
    FOR_ALL_NODE(cnode, chi_iter, Init(chis)) {
      //cnode->Print(stderr);
      if (cnode->OPND() == cr)
	return true;
    }
  }
  
  return false;

}

//
// Implement split-phase communication for a shared write,
// by finding and placing all the necessary syncs for it
// Basically we need a sync right before any program point where 
// the variable may be used or redefined, including the exit.
void UPC_CSE::Split_phase_write(STMTREP* write, AUX_ID var) {

  BB_NODE* write_bb = write->Bb();
  STMTREP_ITER stmt_iter(write_bb->Stmtlist());
  stmt_iter.Init();
  stmt_iter.Set_Cur(write);
  STMTREP* stmt;

  //First, check if there's any potential at all for spliting
  //FIXME: we should use a performance model later
  stmt = stmt_iter.Next();
  if (stmt != NULL && stmt->References_var(var)) {
    //split phase is definitely not profitable, 
    //since we can not separate init and sync at all
    return;
  }

  sync_handle_t *handle = Create_sync_handle(NULL); 

  WRITE_SYNCS* syncs = CXX_NEW(WRITE_SYNCS(handle, write, Loc_pool()), Loc_pool());
  _write_syncs.AddElement(syncs);

  //Check if a sync is needed in the same block
  for (; stmt != NULL; stmt = stmt_iter.Next()) {
    if (stmt->References_var(var)) {
      //fprintf(stderr, "%p needs sync before %p, in the same block\n", write, stmt);
      syncs->Add_sync(stmt, NULL);
      return;
    }
  }

  if (write_bb == Cfg()->Exit_bb()) {
    //no successors, add sync to end of write's block
    syncs->Add_sync(NULL, write->Bb());
    return;
  }

  STACK<BB_NODE*> bbs(Loc_pool());

  BB_LIST_ITER bb_iter;
  BB_NODE *succ;
  FOR_ALL_ELEM( succ, bb_iter, Init(write_bb->Succ()) ) {
    if (succ->Kind() == BB_DOSTEP || succ->Kind() == BB_WHILEEND ||
	succ->Kind() == BB_DOSTART) {
      //We need to be careful about loops: 
      //1. If we're at the end of a loop (DOSTEP or WHILEEND), we stop code motion,
      //   since we can not push a sync before its init call
      //2. If we're at the beginning of a loop (DOSTART or WHILEEND), we also stop code motion,
      //   since pushing the sync into the loop may make the performance worse
      syncs->Add_sync(NULL, write->Bb());
      goto ret_point;
    }
    bbs.Push(succ);
  }

 outer:
  while (bbs.Elements() != 0) {
    BB_NODE* bb = bbs.Pop();
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    stmt_iter.Init();
    //check if this block needs a sync
    for (stmt = stmt_iter.First(); stmt != NULL; stmt = stmt_iter.Next()) {
      if (stmt->References_var(var)) {
	//fprintf(stderr, "%p needs sync before %p, in block %d\n", write, stmt, bb->Id());
	syncs->Add_sync(stmt, NULL);
	goto outer;
      }
    }

    if (bb == Cfg()->Exit_bb()) {
      //fprintf(stderr, "%p needs sync at function exit\n", write);
      syncs->Add_sync(NULL, bb);
      continue;
    }

    FOR_ALL_ELEM(succ, bb_iter, Init(bb->Succ()) ) {
      if (succ->Kind() == BB_DOSTEP || succ->Kind() == BB_WHILEEND ||
	  succ->Kind() == BB_DOSTART) {
	//We're either hitting a new loop, or about to exit a loop
	syncs->Add_sync(NULL, bb);
	goto outer;
      }
      bbs.Push(succ);
    }
  } 

 ret_point:
  if (write_bb->Loopdepth() > 0) {
      //if stmt is inside a loop and we have pushed the sync out of the write's basic block,
      //the handle must be invalidated so it will start with a fresh value the next iteration
      handle->invalidate = true;
  }
  bbs.Free();
}

//Checks if the split phase write can be profitable
bool WRITE_SYNCS::Is_profitable() {

  if (Num_syncs() > 1) {
    //Assume it's profitable if we have more then one sync
    return true;
  }

  FmtAssert(Num_syncs() == 1, ("There should at least be one sync for every write"));
  seq_point_t sync = _syncs[0];

  if (sync.bb == _write->Bb() &&
      _write->Bb()->Last_stmtrep() == _write) {
    //write is at the end of block, and we've failed to push it down further
    return false;
  }

  
  if (_write->Bb()->Last_stmtrep() == _write) {
    BB_LIST_ITER bb_iter;
    BB_NODE *succ;
    FOR_ALL_ELEM( succ, bb_iter, Init(_write->Bb()->Succ()) ) {
      if (sync.bb == succ && succ->First_stmtrep() == sync.stmt) {
	//fprintf(stderr, "write/sync in different blocks, but not profitable\n");
	return false;
      }
    }
  }
  
  return true;

}

//
//A simplified version of code generation can be used for writes
//Since we actually look at every path to determine the possible placement
//for write syncs, at this point no further analysis is necessary 
//(i.e., we should have the optimal sync placement)
void UPC_CSE::Write_code_gen() {

  for (int i = 0; i < _write_syncs.Elements(); i++) {
    WRITE_SYNCS *syncs = _write_syncs[i];
    if (!syncs->Is_profitable()) {
      continue;
    }
    if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
      fprintf(TFile, "split-phase one write: %p\n", syncs->_write);
    }
    syncs->_write->Set_write_handle(syncs->_handle);
    for (int j = 0; j < syncs->Num_syncs(); j++) {
      Insert_sync(syncs->_syncs[j].stmt, syncs->_handle, syncs->_syncs[j].bb);
    }
  }
}


static bool Conflicting_stmt(STMTREP* upcmem, STMTREP* stmt) {

  
  if (stmt->Opr() == OPR_FORWARD_BARRIER || stmt->Opr() == OPR_BACKWARD_BARRIER)
    return true;

  if (upcmem->Opr() == OPR_INTRINSIC_CALL) {
    MU_LIST* mus = upcmem->Mu_list();
    CHI_LIST* chis = upcmem->Chi_list();
    if (stmt != upcmem) {
      CHI_NODE *cnode;
      CHI_LIST_ITER citer;
      FOR_ALL_NODE(cnode, citer, Init(chis)) {
	if (stmt->References_var(cnode->Aux_id())) {
	    //fprintf(stderr, "stmt conflict with chi: \n");
	    //cnode->Print(stderr);
	  return true;
	}
    }
      
      MU_NODE *mnode;
      MU_LIST_ITER miter;
      FOR_ALL_NODE(mnode, miter, Init(mus)) {
	if (stmt->Redefines_var(mnode->Aux_id())) {
	    //fprintf(stderr, "stmt conflict with mu: \n");
	    //mnode->Print(stderr);
	  return true;
	}
      }
    }
  } else {
    //case for scalar shared writes
    AUX_ID aux = upcmem->Lhs()->Kind() == CK_VAR ? upcmem->Lhs()->Aux_id() : upcmem->Lhs()->Ivar_occ()->Aux_id();
    if (stmt->References_var(aux)) {
      return true;
    }
  }
   
  return false;
}
  
//Check the mu and chi list for stmts in the loop body to see 
//if any of it may conflict with the upc memory transfer call
static bool Find_conflicting_stmt(BB_NODE_SET* body, STMTREP* upcmem) {

  //check for stmts in same block
  STMTREP_ITER stmt_iter(upcmem->Bb()->Stmtlist());
  stmt_iter.Init();
  stmt_iter.Set_Cur(upcmem);
  STMTREP* stmt;

  for (stmt = stmt_iter.Next(); stmt != NULL; stmt = stmt_iter.Next()) {
    if (Conflicting_stmt(upcmem, stmt)) {
      return true;
    }
  }

  //check for subsequent blocks in the same loop
  for (BB_NODE *bb = body->Choose(); bb != BB_NODE_SET_CHOOSE_FAILURE; bb = body->Choose_Next(bb)) {
    if (bb->Id() > upcmem->Bb()->Id()) {
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP* stmt;
      FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	if (Conflicting_stmt(upcmem, stmt)) {
	  return true;
	}
      }
    }
  }
  return false;
}

static bool Has_strict_pointer_in_call(CODEREP *call) {
  
  INTRINSIC intrn = call->Intrinsic();
  //check dst
  if (intrn == INTRN_UPC_MEMPUT || intrn == INTRN_UPC_MEMCPY) {
    TY_IDX idx = call->Opnd(0)->Get_ty();
    if (Type_is_strict(TY_pointed(idx))) 
      return true;
  }

  //check src
  if (intrn == INTRN_UPC_MEMGET || intrn == INTRN_UPC_MEMCPY) {
    TY_IDX idx = call->Opnd(1)->Get_ty();
    if (Type_is_strict(TY_pointed(idx))) 
      return true;
  }

  return false;
}

/*                                                                                                                         
 *  For loops containing upc_memget/memput functions, if the loop                                          
 *  is free of dependencies we can convert the memegets into nonblocking                                                   
 *  ones, and sync them outside the loop.  See NAS FT/IS for examples of loops                                             
 *  (all-to-all communication) that could benefit from this transformation.                                                
 *  fine-grained accesses are not affected, since it's unlikely that there won't be any dependences                        
 *  for them in the loop body (in such cases, loop invariant code motion should take care of them).   
 *  In the absence of more sophisticated analysis, we currently rely on user-supplied     
 *  ivdeps to establish the non-existence of cross-iteration dependencies.                                                 
 *  We do check here that there's no dependency within same iteration.                                                     
 *                                                                                                                         
 */

void UPC_CSE::Nbi_bulk_call_in_loop(BB_LOOP* loop) {

  bool has_nbi_call = false;
  BB_NODE_SET *body = loop->Body_set();
  for (BB_NODE *bb = body->Choose(); bb != BB_NODE_SET_CHOOSE_FAILURE; bb = body->Choose_Next(bb)) {
    if (_bb_consistency[bb->Id()] == STRICT_CONSISTENCY) {
      continue;
    }
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP* stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt->Opr() == OPR_INTRINSIC_CALL) {
	INTRINSIC intrn = stmt->Rhs()->Intrinsic();
	if (intrn == INTRN_UPC_MEMGET || intrn == INTRN_UPC_MEMPUT ||
	    intrn == INTRN_UPC_MEMCPY) {
	  bool has_strict = false;
	  if (!Has_strict_pointer_in_call(stmt->Rhs()) && !Find_conflicting_stmt(body, stmt)) {
	    //we can nbi the call
	    if (intrn == INTRN_UPC_MEMGET) 
	      stmt->Rhs()->Set_intrinsic(INTRN_MEMGET_NBI);
	    if (intrn == INTRN_UPC_MEMPUT) 
	      stmt->Rhs()->Set_intrinsic(INTRN_MEMPUT_NBI);
	    if (intrn == INTRN_UPC_MEMCPY) 
	      stmt->Rhs()->Set_intrinsic(INTRN_MEMCPY_NBI);
	    has_nbi_call = true;
	  }
	}
      } else {
	CODEREP* lhs = stmt->Lhs();
	if (lhs != NULL) {
	  switch (lhs->Kind()) {
	  case CK_VAR:
	    // X = expr
	    if (TY_is_shared(lhs->Lod_ty()) && !Type_is_strict(lhs->Lod_ty())) {
	      if (!Find_conflicting_stmt(body, stmt)) {
		if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
		  fprintf(TFile, "stid can be nbi-ed\n");
		}
		has_nbi_call = true;
		stmt->Set_nbi_write();
	      }
	    }
	    break;
	  case CK_IVAR:
	    // *X = expr
	    if (TY_is_shared(lhs->Ilod_ty()) && !Type_is_strict(lhs->Ilod_ty())) {
	      if (!Find_conflicting_stmt(body, stmt)) {
		if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
		  fprintf(TFile, "istore can be nbi-ed\n");
		}
		has_nbi_call = true;
		stmt->Set_nbi_write();
	      }
	    }
	  }
	}
      }
    }
  }

      if (has_nbi_call) {
	if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
	  fprintf(TFile, "performing nbi loop optimization\n");
	}
	WN* wn = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 0);
	WN_intrinsic(wn) = INTRN_WAIT_SYN_ALL;
	STMTREP* sync = CXX_NEW(STMTREP(WN_opcode(wn)), Loc_pool());
	sync->Set_wn(wn);
	BOOL proped = false;
	sync->Set_rhs(_htable->Add_expr(wn, _opt_stab, sync, &proped, NULL));
	BB_NODE* exit = loop->Dotail();
	exit->Prepend_stmtrep(sync);
      }
}

//Iterate through the cfg and associate strict/relaxed attributes
//with every single block
void UPC_CSE::Set_consistency_info() {

  BB_NODE *bb;
  CFG_ITER cfg_iter(Cfg());

  FOR_ALL_NODE (bb, cfg_iter, Init()) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt = bb->First_stmtrep();
    
    if (stmt != NULL && stmt->Opr() == OPR_PRAGMA) {
      Enter_Consistency_Info(WN_pragma(stmt->Orig_wn()));
    }
   
   
    if (!consistency_stack.empty()) {
      _bb_consistency[bb->Id()] = consistency_stack.top();
    }  
      
    
    
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt != bb->First_stmtrep() && stmt->Opr() == OPR_PRAGMA) {
	Enter_Consistency_Info(WN_pragma(stmt->Orig_wn()));
      }
    }
  }
}

static bool Mark_NB_Stmt(STMTREP *stmt, bool last_profitable, bool is_get) {

  bool nb_profitable = last_profitable;
  if (stmt->Rhs() != NULL && stmt->Rhs()->Has_strict_access()) {
    nb_profitable = false;
  } else if (stmt->Lhs() != NULL && stmt->Lhs()->Has_strict_access()) {
    nb_profitable = false;
  } else if (stmt->Opr() == OPR_INTRINSIC_CALL) {
    INTRINSIC intrn = stmt->Rhs()->Intrinsic();
    if (INTRN_Is_Upc_Sync(intrn) || Has_strict_pointer_in_call(stmt->Rhs())) {
      nb_profitable = false;
    } else if (stmt->Rhs()->Intrinsic() == INTRN_UPC_MEMGET) {
      if (nb_profitable && is_get) {
	stmt->Rhs()->Set_intrinsic(INTRN_AUTO_NB_MEMGET);
	fprintf(stderr, "find one memget as prefetching candidate\n");
      }
      nb_profitable = true;
    } else if (stmt->Rhs()->Intrinsic() == INTRN_UPC_MEMPUT) {
      if (nb_profitable && !is_get) {
	stmt->Rhs()->Set_intrinsic(INTRN_AUTO_NB_MEMPUT);
	fprintf(stderr, "convert one memput to non-blocking\n");
      }
      nb_profitable = true;
    } else if (stmt->Rhs()->Intrinsic() == INTRN_UPC_MEMCPY) {
      nb_profitable = true;
    }
    // other intrinsics that we care about??
  } else if (stmt->Opr() == OPR_CALL) {
    //optimistically assume the function contains enough computation that 
    //overlapping would be profitable
    //IPA would allow us to be more precise here
    nb_profitable = true;
  } 

  //FIXME: we could also check for fine-grained accesses as a source of overlap,
  //but in practice they're never mixed with bulk calls

  return nb_profitable;
  
}

//Convert blocking upc_memput calls into the nonblocking ones,
//which are to be automatically synchronized at runtime
//the memgets will be prefetched starting from the last barrier
void UPC_CSE::Do_Auto_NB(BB_NODE *bb) {

  if (_bb_consistency[bb->Id()] == STRICT_CONSISTENCY) {
    return;
  }

  bool nb_profitable = true; //assume any get/put that can be pushed out of the block is profitable

  //nonblocking gets
  for (STMTREP *stmt = bb->First_stmtrep(); stmt != NULL; stmt = stmt->Next()) {
    nb_profitable = Mark_NB_Stmt(stmt, nb_profitable, true);
  }

  nb_profitable = true;

  //nonblocking puts
  for (STMTREP *stmt = bb->Last_stmtrep(); stmt != NULL; stmt = stmt->Prev()) {
    nb_profitable = Mark_NB_Stmt(stmt, nb_profitable, false);
  }
}


bool Bb_has_barriers( BB_NODE *bb)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if(stmt->Opr() == OPR_FORWARD_BARRIER || stmt->Opr() == OPR_BACKWARD_BARRIER)
      return true;
  }
  return false;
}

// ====================================================================
//  COMP_UNIT::Do_UPC_CSE - perform CSE on UPC shared loads for this PU
// ====================================================================
void
COMP_UNIT::Do_UPC_CSE(MEM_POOL* pool)
{
  
  Clear_Consistency_Info();

  UPC_CSE upc_cse(Htable(), Opt_stab(), Cfg(), pool);
  BB_NODE* bb;
  CFG_ITER cfg_iter(Cfg());

  if (Get_Trace(TP_WOPT2, UPC_OPT_FLAG)) {
    fprintf(TFile, "======== UPC communication optimizations==============\n");
  }
 
  upc_cse.Set_consistency_info();

#if 0
  //not needed once we have auto nonblocking optimizations
  //find all do loops with ivdeps
  FOR_ALL_NODE(bb, cfg_iter, Init()) {
    if (bb->Kind() == BB_DOSTART) {
      BB_NODE* pred = bb->Nth_pred(0);
      STMTREP* stmt = pred->Last_stmtrep();
      if (stmt != NULL && stmt->Opr() == OPR_PRAGMA &&
	  WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_IVDEP) {
	upc_cse.Nbi_bulk_call_in_loop(bb->Loop());
      }
    }
  }
#endif

  if (run_auto_nb) {
    //take advantage of runtime-directed synchronization of memputs if it's available
    FOR_ALL_NODE(bb, cfg_iter, Init()) {
      upc_cse.Do_Auto_NB(bb);
    }
  }


  if (run_pre_add) {
    //First optimize the shared pointer arithmetic
    FOR_ALL_NODE (bb, cfg_iter, Init()) {
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP *stmt;
      FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	CODEREP* tmp;
	if (stmt->Rhs() != NULL) {
	  upc_cse.Mark_shared_add_rec(stmt->Rhs(), stmt);
	}
	if (stmt->Lhs() != NULL) {
	  upc_cse.Mark_shared_add_rec(stmt->Lhs(), stmt);
	}
      }
    }
    
    upc_cse.Code_gen(true);
  }

  //With symmetric pointers, we can derefernce the pointer directly,
  //and there's no need for split-phase optimization
  //PRE can still be useful though, so don't disable that
  if (run_split_phase && !Everything_Local) {

    //Go through every statement in the program,
    //and mark all of the shared loads
    FOR_ALL_NODE (bb, cfg_iter, Init()) {
      if (upc_cse._bb_consistency[bb->Id()] == STRICT_CONSISTENCY || Bb_has_barriers(bb)) {
	 //bug2882 - strict accesses are not captured
	// In opt_alias_analysis, the code inserts OPC_FORWARD_BARRIERS (or BACKWARD) whenever
	// a strict access is detected. Treat these conservatively as strict scope for the bounding basic block.
	continue;
      }
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP *stmt;
      FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	upc_cse.Mark_cse_stmt(stmt);
      }
    }
    
    upc_cse.Code_gen(false);

#if 1
    
    //Find all locations where we need to add syncs for split-phase writes
    FOR_ALL_NODE (bb, cfg_iter, Init()) {
      if (upc_cse._bb_consistency[bb->Id()] == STRICT_CONSISTENCY || Bb_has_barriers(bb)) {
	continue;
      }
      STMTREP_ITER stmt_iter(bb->Stmtlist());
      STMTREP *stmt;
      FOR_ALL_NODE(stmt, stmt_iter, Init()) {
	CODEREP* lhs = stmt->Lhs();
	if (lhs != NULL) {
	  switch (lhs->Kind()) {
	  case CK_VAR:
	    // X = expr
	    if (TY_is_shared(lhs->Lod_ty()) && !Type_is_strict(lhs->Lod_ty())) {
	      upc_cse.Split_phase_write(stmt, lhs->Aux_id());
	    }
	    break;
	  case CK_IVAR:
	    // *X = expr
	    if (TY_is_shared(lhs->Ilod_ty()) && !Type_is_strict(lhs->Ilod_ty())) {
	      upc_cse.Split_phase_write(stmt, lhs->Ivar_occ()->Aux_id());
	    }
	  }
	}
      }
    }
    
    upc_cse.Write_code_gen();
#endif
  }

  if ( Get_Trace(TP_GLOBOPT, PROP_DUMP_FLAG)) {
    fprintf(TFile, "%sAfter COMP_UNIT::Do_UPC_CSE\n%s",
	     DBar, DBar );
    Cfg()->Print(TFile);
  }

}

void
CSE_NODE::Print(FILE* file) {

  fprintf(file, "---------------------------------\n");
  fprintf(file, "shared cr: %p\n", _shared_cr);
  _shared_cr->Print(0, file);
  fprintf(file, "definition point: BB %d\n", _def_point.bb->Id());
  //_def_point.bb->Print(file);
  fprintf(file, "%d uses:\n", num_uses);
  for (int i=0; i <= _use_stmts.Lastidx(); i++) {
    _use_stmts[i]->Print(file);
    fprintf(file, "\n");
  }
  fprintf(file, "---------------------------------\n");
}
