#include "string.h"

#include "wn.h"
#include "ir_reader.h"
#include "tracing.h"
#include "be_util.h"
#include "wintrinsic.h"
#include "cxx_hash.h"

#include "opt_main.h"
#include "opt_cfg.h"
#include "opt_bb.h"
#include "opt_etable.h"
#include "opt_htable.h"
#include "opt_stmt.h"
#include "opt_sym.h"
#include "opt_mu_chi.h"
#include "vector"


static COMP_UNIT *cu;

/*
  Store the bbs that are guaranteed to be only executed by one thread at anytime
  Currently handles:
  -- if (MYTHREAD ==/!= CONST) {...}
  -- forall loops
  -- critical section
  The second id points to the IF/WHILEND bb that guards the bb
*/
static HASH_TABLE<int, int> *single_thread_bb;

/* 
   Check if the given cr is a load of MYTHREAD
 */
static bool is_mythread (CODEREP * cr) {

  if (cr->Kind() == CK_VAR) {
    IDTYPE id = cr->Aux_id();
    ST* st = cu->Opt_stab()->St(id);
    return strncmp(ST_name(st), "MYTHREAD", 8) == 0;
  }
  return false;
}

static bool is_shared_lhs(CODEREP * cr) {
  switch (cr->Kind()) {
  case CK_VAR: /* ldid */
    return TY_is_shared(cr->Lod_ty());
  case CK_IVAR:
    return Type_Is_Shared_Ptr(cr->Ilod_ty());
  case CK_OP: {
    OPERATOR op = cr->Opr();
    if (op == OPR_INTRINSIC_CALL) {
      INTRINSIC intrn = cr->Intrinsic();
      return intrn == INTRN_UPC_MEMCPY || intrn == INTRN_UPC_MEMGET || 
	intrn == INTRN_UPC_MEMPUT || intrn == INTRN_UPC_MEMSET; 
    }
  }
  default:
    return false;
  }
}

static void report_conflict(STMTREP* s1, STMTREP* s2, AUX_ID id) {

  fprintf(stderr, "-----------------------------------\n");
  fprintf(stderr, "MAY CONFLICT: \n");
  fprintf(stderr, "shared variable: %s\n", ST_name(cu->Opt_stab()->St(id)));
  fprintf(stderr, "Stmt 1: line %d\n", Srcpos_To_Line(s1->Linenum()));
  s1->Print(stderr);
  fprintf(stderr, "*****************\n");
  fprintf(stderr, "Stmt 2: line %d\n", Srcpos_To_Line(s2->Linenum()));
  s2->Print(stderr);
}

/* 
   Check for conflicts between stmt and any shared accesses in bb and bb's successors, starting from statement start.
   bb must be a successor of stmt's bb
 */
static void check_conflict(vector<AUX_ID> *mod_vars, STMTREP* stmt, BB_NODE* bb, STMTREP* start, HASH_TABLE<BB_NODE*, int> *htable) {

  
  if (htable->Find(bb)) {
    if (stmt->Bb() != bb) {
      /* We've processed this bb */
      return;
    }
  }

  bool single_thread = single_thread_bb->Find(stmt->Bb()->Id()) &&
    single_thread_bb->Find(stmt->Bb()->Id()) == single_thread_bb->Find(bb->Id());

  if (!single_thread) {
    /* check for conflicts within the BB */
    for (STMTREP* succs = start; succs != NULL; succs = succs->Next()) {
      /* should we check for self-conflict??? */
      OPERATOR opr = succs->Opr();
      if (opr == OPR_INTRINSIC_CALL) {
	INTRINSIC intrn = succs->Rhs()->Intrinsic();
	if (intrn == INTRN_UPCBAR ||
	    intrn == INTRN_UPCWAIT) {
	  /* Barrier means all subsequent accesses can't be concurrent */
	  return;
	} else if (intrn == INTRN_UPC_MEMCPY || intrn == INTRN_UPC_MEMGET || 
		   intrn == INTRN_UPC_MEMPUT || intrn == INTRN_UPC_MEMSET) {
	  CODEREP* call = succs->Rhs();
	  for (int i = 0; i < call->Kid_count(); i++) {
	    CODEREP* params = call->Get_opnd(i);
	    for (int j = 0; j < mod_vars->size(); j++) {
	      if (params->References_var((*mod_vars)[j])) {
		report_conflict(stmt, succs, (*mod_vars)[j]);
	      }
	    }
	  }
	} else {
	  /* can we assume no other intrinsics will touch shared val? */
	  continue;
	}
      } else if (opr == OPR_RETURN) {
	continue;
      } else if (opr == OPR_CALL) {
	CODEREP* call = succs->Rhs();
	if (!call->Is_flag_set(CF_IS_USER_FUNC)) {
	  /* C functions can not cause races */
	  continue;
	}

	for (int i = 0; i < mod_vars->size(); i++) {
	  ST* st = cu->Opt_stab()->St((*mod_vars)[i]);
	  if (ST_sclass(st) != SCLASS_PSTATIC) {
	    /* In the absence of interprocedural analysis,
	       assume every global variable could be modified */
	    report_conflict(stmt, succs, (*mod_vars)[i]);
	  }
	}

	for (int i = 0; i < call->Kid_count(); i++) {
	  CODEREP* params = call->Get_opnd(i);
	  for (int j = 0; j < mod_vars->size(); j++) {
	    if (params->References_var((*mod_vars)[j])) {
	      report_conflict(stmt, succs, (*mod_vars)[j]);
	    }
	  }
	}
      } else {
	/* default case */
	for (int i = 0; i < mod_vars->size(); i++) {
	  if (succs->References_var((*mod_vars)[i])) {
	    report_conflict(stmt, succs, (*mod_vars)[i]);
	    break;
	  }
	}
      }
    }
  }

  htable->Enter(bb, 1);

  /* check for all of bb's successors */
  BB_LIST* succ_bbs = bb->Succ();
  while (succ_bbs != NULL) {
    check_conflict (mod_vars, stmt, succ_bbs->Node(), succ_bbs->Node()->First_stmtrep(), htable);
    succ_bbs = succ_bbs->Next();
  }
} 

static void do_detection(BB_NODE* bb) {

  bool no_internal = single_thread_bb->Find(bb->Id());
	  
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMT_LIST swrites;
  STMTREP* stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    CODEREP* lhs = stmt->Lhs();
    if (lhs && is_shared_lhs(lhs)) {
      //fprintf(stderr, "Shared lhs: \n");
      //lhs->Print(1, stderr);
      vector<AUX_ID> mod_vars;
      HASH_TABLE<BB_NODE*, int> htable(500, cu->Loc_pool());
      if (OPERATOR_is_scalar_store(stmt->Opr())) {
	mod_vars.push_back(lhs->Aux_id());
      } 
      if (stmt->Has_chi()) {
	CHI_NODE* cnode;
	CHI_LIST_ITER chi_iter;
	CHI_LIST *chi_list = stmt->Chi_list();
	FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
	  ST* st = cu->Opt_stab()->St(cnode->Aux_id());
	  if (st && TY_is_shared(ST_type(st))) {
	    mod_vars.push_back(cnode->Aux_id());
	  }
	}
      }
      for (int i = 0; i < mod_vars.size(); i++) {
	//fprintf(stderr, "modified var: %s\n", ST_name(cu->Opt_stab()->St(mod_vars[i])));
      }
      if (!no_internal) {
	/* check for conflicts within the BB */
	check_conflict(&mod_vars, stmt, bb, stmt->Next(), &htable);
      } else {
	check_conflict(&mod_vars, stmt, bb, NULL, &htable);
      }
    }
  }
}

static void find_single_thread_block() {

  CFG* cfg = cu->Cfg();
  CFG_ITER cfg_iter(cfg);
  BB_NODE *bb;
  FOR_ALL_NODE(bb, cfg_iter, Init() ) {
    if (single_thread_bb->Find(bb->Id())) {
      continue;
    }
    if (bb->Kind() == BB_LOGIF) {
      BB_NODE* if_test = bb->If_cond();
      STMTREP * stmt = if_test->Last_stmtrep();
      CODEREP* cr = stmt->Rhs();
      if (cr->Kind() == CK_OP && (cr->Opr() == OPR_EQ || cr->Opr() == OPR_NE)) {
	CODEREP *op1 = cr->Opnd(0), *op2 = cr->Opnd(1);
	if ((is_mythread(op1) && op2->Kind() == CK_CONST) ||
	    (is_mythread(op2) && op1->Kind() == CK_CONST)) {    
	  BB_NODE* dom_bb = (cr->Opr() == OPR_EQ) ? bb->If_then() : bb->If_else();
	  /* Why is there no faster way to do this? ugh */
	  BB_NODE* tmp;
	  FOR_ALL_NODE(tmp, cfg_iter, Init() ) {
	    if (dom_bb->Dominates(tmp)) {
	      single_thread_bb->Enter(tmp->Id(), bb->Id());
	      //fprintf(stderr, "single_T: IF %d %d\n", tmp->Id(), bb->Id());
	    }
	  }
	}
      }
    }

    if (bb->Kind() == BB_WHILEEND) {
      BB_LIST* preds = bb->Pred();
      for (; preds != NULL; preds = preds->Next()) {
	BB_NODE *parent = preds->Node();
	STMTREP* stmt = parent->Last_stmtrep();
	if (stmt != NULL && stmt->Opr() == OPR_PRAGMA &&
	    WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_UPC_FORALL) {
	  /* forall loop */
	  BB_NODE_SET* loop = bb->Loop()->Body_set();
	  for (BB_NODE* tmp = loop->Choose(); tmp != BB_NODE_SET_CHOOSE_FAILURE; tmp = loop->Choose_Next(tmp)) {
	    single_thread_bb->Enter(tmp->Id(), bb->Id());
	    //fprintf(stderr, "single_T: FORALL %d %d\n", tmp->Id(), bb->Id());
	  }
	}
      }
    }
  }
}

/***
 *
 * Perform race detection for a CFG (representing a procedure)
 * 
 * Alg sketch:
 *   for each BB, find a list of all shared memory writes;
     for each shared write { 
 *     if BB could be run by any threads {
 *        check for race in the BB
 *     check for races recursively in successors of BB
 *   }
 *   
 *   Where a race is defined as a shared write reaching 
 *   any other shared memory operation, with no intervening synchronization operations
 *
 * TODO: interprocedural support
 * TODO: symbolic analysis for MYTHREAD
 * TODO: add support for arrays (use array dependence graphs from lno???)
 */   

void race_detection(COMP_UNIT* comp_unit) {

  MEM_POOL_Push (comp_unit->Loc_pool());
  cu = comp_unit;
  HASH_TABLE<int, int> htable(500, cu->Loc_pool());
  single_thread_bb = &htable;
  find_single_thread_block();
  CFG* cfg = comp_unit->Cfg();
  CFG_ITER cfg_iter(cfg);
  BB_NODE *bb;
  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    //bb->Print(stderr);
    do_detection(bb);
  }
  MEM_POOL_Pop (comp_unit->Loc_pool());
}

