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
//#include "opt_upc_locality.h"

/******************************
 * 
 *  Locality analysis for UPC
 *
 *  Idea: for every UPC pointer-to-shared, during the points-to analysis we compute extra information 
 *  about the thread that it points to.
 *
 *  TH(p) : {0, 1, ?, T-1} U {M, Top}.  
 *   M is a symbolic value indicating that the pointer points to local memory (MYTHREAD), 
 *   while Top has the standard data-flow lattice connotation (i.e., we don't know anything)
 *
 *  Initially, every pointer-to-shared is initialized to Top.  Transitions occur for the following statements:
 *
 *  -- p = upc_alloc()
 *
 *  TH(p) = Mythread.  // p is guaranteed to be local after this
 *
 *  -- p = upc_global_alloc() / upc_all_alloc() 
 *
 *  TH(p) = 0   //p is at thread zero
 *
 *  -- p = q 
 *
 *  TH(p) = TH(q)
 *
 *   -- p = &q
 *
 *  TH(p) = Threadof(q), where Threadof(q) is the thread that q belongs to
 *
 *  -- p = NULL
 *
 *  TH(p) = Top
 *
 *  -- (cast to local) p
 *
 *  TH(p) = M  //this is essentially an assert that p is local
 *
 *  -- p = q op c  (ptr arithmetic)
 *
 *  This is an interesting case.  We'll need symbolic values to handle the generic cases, which 
 *  doesn't seem like it'd be worth it.  We probably should handle the case where c is constant, 
 *  though (the common p++ case):
 *
 *  TH(p) = (TH(q) + c) mod T
 *
 *  Another special case is when c == MYTHREAD:
 *
 *  TH(p) =  M  if TH(q) == 0
 *  TH(p) =  Top  otherwise
 *
 *  In both cases I'm assuming cyclic arrays.
 *  My feeling is that we don't need to pay special attention to ptr arithmetic, since they're bound
 *  to occur in loops, and a separate loop framework should be used to take care of them.  The job
 *  of the locality analysis is instead to assert at the entrance of the loop which thread the base 
 *  pointer points to.
 *
 *  -- call()
 *
 *  We distinguish between C and UPC functions, as determined by detect-upc; the former may never 
 *  modify pointer-to-shared.  For the latter we look up the MOD set of the function, and for the pointer-to-
 *  shared in them set their thread id to TOP
 *
 *
 *  Merge operation:
 *
 *  Merging happens at the phi nodes in the SSA.  For every phi definition, we check if 
 *  its operands have the same thread id; if not, the phi node's thread id is set back to Top
 *
 *  Implementation:
 *
 *  This is a flow-sensitive (and hopefully context sensitive once we get IPA working) analysis, 
 *  and I plan to build up the information as part of the flow-sensitive alias analysis.
 *  The analysis is performed on the SSA, and the extra TH information is stored as part of the CODEREP,
 *  so that variables with different versions can have different thread ids
 *
 *  Application:
 *
 *  -- One application of the analysis that I've implemented is to use it to help the localization optimizations
 *     in UPC forall loops.  For example, with the following loop
 *
 *     shared int * a = upc_all_alloc(...)
 *     ...
 *     forall (i = 0; i < N; i++; i)
 *        a[i] = ...
 *
 *     the analysis determines that a points to thread 0 in the loop, and a[i] thus can be localized
 *
 */


static void find_base_and_offset(CODEREP* rhs, CODEREP **base, CODEREP **offset) {

    CODEREP *cr1, *cr2;
    cr1 = rhs->Get_opnd(0);
    cr2 = rhs->Get_opnd(1);
    if ((cr1->Kind() == CK_VAR || cr1->Kind() == CK_IVAR) &&
	Type_Is_Shared_Ptr(cr1->Get_ty(), true)) {
	*base = cr1;
	*offset = cr2; 
    } else if ((cr2->Kind() == CK_VAR || cr2->Kind() == CK_IVAR) &&
	       Type_Is_Shared_Ptr(cr2->Get_ty(), true)) {
	*base = cr2;
	*offset = cr1;
    } else {
	//fprintf(stderr, "unrecognized pointer exp\n");
	//rhs->Print(0, stderr);
    }
}

//For ptr arithmetic, use pattern matching to handle some common special cases 
void OPT_STAB::Analyze_Locality_Ptr_Arith(CODEREP* rhs) {

    FmtAssert(rhs->Kind() == CK_OP && rhs->Opr() == OPR_ADD, ("Rhs must be a ADD"));

    if (!Type_Is_Shared_Ptr(rhs->Get_ty(), true)) {
	return;
    }
    CODEREP *base = NULL, *offset = NULL;
    find_base_and_offset(rhs, &base, &offset);
    if (base != NULL && offset != NULL) {
	//base->Print(0, stderr);
	Analyze_Locality_Cr(base);
	TY_IDX obj_ty = TY_pointed(base->Get_ty());
	if (TY_block_size(obj_ty) == 0) {
	    rhs->Set_thread_id(base->Thread_id());
	    return;
	} else if (TY_block_size(obj_ty) == 1) {
	    switch (offset->Kind()) {
		case CK_VAR: {
		    ST* st = Aux_stab_entry(offset->Aux_id())->St();
		    if (st == upc_mythread_st && base->Thread_id() == 0) {
			fprintf(stderr, "case of &a[MYTHREAD]\n");
			rhs->Set_thread_id(POINTS_TO::LOCALITY_MYTHREAD);
			return;
		    } 
		    break;
		}
		case CK_CONST:
		    if (base->Thread_id() != POINTS_TO::LOCALITY_TOP &&
			base->Thread_id() != POINTS_TO::LOCALITY_MYTHREAD) {
			int thread = base->Thread_id() + offset->Const_val();
			if (upc_num_threads != 0) {
			  thread = thread >= 0 ? thread % upc_num_threads : thread % upc_num_threads + upc_num_threads;
			}
			rhs->Set_thread_id(thread);  
			return;
		    }
	    }
	}
	//can't do anything for block cyclic arrays unless we track their phase
    }
    
    rhs->Print(0, stderr);
    //at this point we don't know anything about the ptr add expression and give up
    rhs->Set_thread_id(POINTS_TO::LOCALITY_TOP);
    
}

//Do a bottom up pass to mark the locality information for 
//all expressions in the given cr
void OPT_STAB::Analyze_Locality_Cr(CODEREP* cr) {

  switch(cr->Kind()) {
      case CK_VAR:
	  if (cr->Has_thread_id()) {
	      ST* st = Aux_stab_entry(cr->Aux_id())->St();	      
	      TY_IDX ty = ST_type(st);
	      if (TY_kind(ty) == KIND_ARRAY && TY_block_size(ty) != 0) {
		  fprintf(stderr, "base array case\n");
		  cr->Set_thread_id(0);
	      }
	  }
	  break;
  case CK_OP:
    if (cr->Opr() == OPR_TAS) {
      CODEREP* kid = cr->Opnd(0);
      //strip off all other TAS except the top level one
      while (kid->Kind() == CK_OP && kid->Opr() == OPR_TAS) {
	  kid = kid->Opnd(0);
      }
      if (!Type_Is_Shared_Ptr(cr->Get_ty(), true) &&
	  kid->Has_thread_id()) {
	//shared to local cast
	kid->Set_thread_id(POINTS_TO::LOCALITY_MYTHREAD);
      } else {
	  Analyze_Locality_Cr(kid);
      }
    } else if (cr->Opr() == OPR_ADD) {
	//case of ptr arithmetic
	Analyze_Locality_Ptr_Arith(cr);	
    } else {
	for (int i = 0; i < cr->Kid_count(); i++) {
	    Analyze_Locality_Cr(cr->Get_opnd(i));
	}
    }
    break;
  case CK_IVAR:
    Analyze_Locality_Cr(cr->Ilod_base());
  }
}

void OPT_STAB::Analyze_Locality_Stmt(CODEREP *pts, CODEREP* rhs) {

  Analyze_Locality_Cr(rhs);

  if (!pts->Has_thread_id()) {
    return;
  }
  switch(rhs->Kind()) {
  case CK_LDA: {
    //fprintf(stderr, "case of p = &q\n");
    TY_IDX pointed_ty = TY_pointed(rhs->Lda_ty());
    if (TY_kind(pointed_ty) != KIND_POINTER) {
      //all scalars are on thread 0
      pts->Set_thread_id(0);
    } else {
      //for shared pointer we currently have no way of telling
      pts->Set_thread_id(POINTS_TO::LOCALITY_TOP);
    }
    break;
  }
  case CK_VAR: 
  case CK_IVAR: 
    //the case of p = q
    //fprintf(stderr, "case of ptr assignment %p = %p\n", pts, rhs);
    pts->Set_thread_id(rhs->Thread_id());
    break;
  case CK_OP:
      //only tas and ptr arith nodes may have locality info
    if (rhs->Opr() == OPR_TAS) {
      Analyze_Locality_Stmt(pts, rhs->Get_opnd(0));
    } else if (rhs->Opr() == OPR_ADD) {
	pts->Set_thread_id(rhs->Thread_id());
    }
    break;
  case CK_CONST:
    //fprintf(stderr, "case of null ptr assignment\n");
    //case of NULL assignment, set to TOP I guess
    pts->Set_thread_id(POINTS_TO::LOCALITY_TOP);
    break;
  default:
    FmtAssert(FALSE, ("Unexpected Coderep type %d", rhs->Kind()));
  }
}

void OPT_STAB::Analyze_Locality_Alloc(CODEREP *pts, ST* func_st) {

    fprintf(stderr, "in %s: %p\n", __func__, pts);
    if (func_st == upc_all_alloc_st) {
	pts->Set_thread_id(0);
    } else if (func_st == upc_global_alloc_st) {
	pts->Set_thread_id(0);
    } else if (func_st == upc_alloc_st) {
	pts->Set_thread_id(POINTS_TO::LOCALITY_MYTHREAD);
    } else {
	FmtAssert(FALSE, ("Unexpected function %s", ST_name(func_st)));
    }
}


void OPT_STAB::Analyze_Locality_Block(BB_NODE* bb) {

  //analyze the phi nodes first
  PHI_LIST_ITER phi_iter;
  PHI_NODE *phi;
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live() && phi->Res_is_cr()) {
      //update the thread id info for the result
      CODEREP* pts = phi->RESULT();
      if (pts->Has_thread_id()) {
	//phi->Print(stderr);
	INT16 thread_id = phi->OPND(0)->Has_thread_id() ? phi->OPND(0)->Thread_id() : POINTS_TO::LOCALITY_TOP;
	for (int i = 1; i < phi->Size(); i++) {
	  if (thread_id != phi->OPND(i)->Thread_id()) {
	    thread_id = POINTS_TO::LOCALITY_TOP;
	  }
	}
	pts->Set_thread_id(thread_id);
      }
    }
  }

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  ST* last_alloc_call = NULL;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    //stmt->Print(stderr);
    CODEREP *lhs, *rhs;
    if (stmt->Has_chi()) {
      //stmt->Print(stderr);
    }
    //first analyze the rhs (cast to locals, ptr arith, etc.)
    if (stmt->Rhs()) {
	Analyze_Locality_Cr(stmt->Rhs());
    }

    switch (stmt->Opr()) {
    case OPR_STID: 
      lhs = stmt->Lhs();
      if (Type_Is_Shared_Ptr(lhs->Lod_ty(), true)) {
	if (last_alloc_call != NULL) {
	  Analyze_Locality_Alloc(lhs, last_alloc_call);
	  last_alloc_call = NULL;
	} else {
	  Analyze_Locality_Stmt(lhs, stmt->Rhs());
	}
      }
      break;
    case OPR_ISTORE:
      lhs = stmt->Lhs();
      if (Type_Is_Shared_Ptr(lhs->Get_ty(), true)) {
	if (last_alloc_call != NULL) {
	  Analyze_Locality_Alloc(lhs, last_alloc_call);
	  last_alloc_call = NULL;
	} else {
	  Analyze_Locality_Stmt(lhs, stmt->Rhs());
	}
      } 	  
      break;
    case OPR_CALL:
      if (stmt->St() != NULL && 
	  (stmt->St() == upc_all_alloc_st ||
	   stmt->St() == upc_global_alloc_st ||
	   stmt->St() == upc_alloc_st)) {
	last_alloc_call = stmt->St();
      } else if (!stmt->Rhs()->Is_flag_set(CF_IS_USER_FUNC) && stmt->Has_chi()) {
	//in the absence of IPA, we have to reset every pointer variable that
	//may be modified by the call
	
	CHI_LIST *chis = stmt->Chi_list();
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE(cnode, chi_iter, Init(chis)) {
	  if (cnode->Live()) {
	    CODEREP* cr = cnode->RESULT();
	    if (cr->Has_thread_id()) {
	      cr->Set_thread_id(POINTS_TO::LOCALITY_TOP);
	    }
	  }
	}
      }
      break;
    default:
	;
    }
  }
}

  
//  Proprocess all BBs in the reverse dominator_tree order                                                                 
void OPT_STAB::Analyze_Locality(CFG* cfg) {

  CFG_ITER cfg_iter(cfg);
  BB_NODE* bb;

  //Print(stderr);
  FOR_ALL_NODE(bb, cfg_iter, Init()) {
      //  bb->Print(stderr);
    Analyze_Locality_Block(bb);
  }
}
