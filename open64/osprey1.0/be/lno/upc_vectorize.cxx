#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "defs.h"
#include "config_cache.h"
#include "config_list.h"
#include "config_lno.h"
#include "erbe.h"
#include "glob.h"		    /* Irb_File_Name, Cur_PU_Name */
#include "wn.h"
#include "timing.h"
#include "wn_simp.h"
#include "ir_reader.h"
#include "lnoptimizer.h"
#include "opt_du.h"			/* Du_Built() */
#include "wn_pragmas.h"

#include "lwn_util.h"
#include "lnoutils.h"
#include "graph_template.h"

#include "dep_graph.h"
#include "fission.h"
#include "fusion.h"
#include "ff_utils.h"
#include "ff_pragmas.h"
#include "fiz_fuse.h"
#include "fis_gthr.h"
#include "inner_fission.h"
#include "snl.h"
#include "prefetch.h"
#include "reduc.h"
#include "soe.h"
#include "cond.h"
#include "lnopt_main.h"
#include "config.h"
#include "be_util.h"
#include "aequiv.h"
#include "sclrze.h"
#include "dead.h"
#include "minvariant.h"
#include "outer.h"
#include "lego.h"
#include "lego_opts.h"
#include "cxx_graph.h"
#include "model.h"
#include "forward.h"
#include "debug.h"
#include "cse.h"
#include "stblock.h"
#include "strtab.h"
#include "reverse.h"
#include "tile.h"
#include "permute.h"
#include "lego_skew.h"
#include "array_bounds.h"
#include "small_trips.h"
#include "parallel.h"
#include "doacross.h"
#include "autod.h"
#include "prompf.h" 
#include "anl_driver.h"
#include "parids.h"
#include "call_info.h"
#include "ifminmax.h"
#include "shackle.h"
#include "ipa_lno_info.h"
#include "ipa_lno_file.h"
#include "ipa_lno_summary.h"
#include "ipa_section.h"
#include "lnodriver.h"
#include "ipa_lno_read.h"
#include "upc_vectorize.h"
#include "prefetch.h"
#include "vec_loop.h"
#include "math.h"
#include "upc_wn_util.h"

#include <sys/time.h>
#include <sys/resource.h>
#include <tracing.h>

MEM_POOL VEC_memory_pool, VEC_CG_mpool,*VEC_mpool;
DOLOOP_STACK *dls;
extern MEM_POOL *PF_mpool;
int LNO_Upc_Vectorize = 1;
static WN_MAP version_map = 0;

static ARRAY_DIRECTED_GRAPH16* dg;
static DU_MANAGER* du; 
static REDUCTION_MANAGER* rm;
static BOOL vec_mempools_initialized = FALSE; 

static TY_IDX shared_void_ptr = 0;

extern WN* Lego_Index_From_Access_Vector(ACCESS_VECTOR* av,       
                                         WN* wn_index,
                                         DU_MANAGER* du);

static ST* vec_temp = NULL;
FIZ_FUSE_INFO *loops;


//the amount of stack temporary arrays that we have allocated 
static UINT total_stack_size;
static UINT max_stack_size = 1024*1024;

//from symtab.cxx
extern TY_IDX Shared_To_Private_Type(TY_IDX ty_idx);

//check if the type of the symbol in question can be vectorized
//FIXME: For now we do not handle vectorizing shared pointer-to-shared
static bool Is_Upc_Vect(TY_IDX ty) {
  BOOL result;
  result =  (Type_Is_Shared_Ptr(ty, true) && !TY_is_shared(ty)) || (TY_is_shared(ty) && TY_kind(ty) == KIND_ARRAY)  ;
  
  return result && (Get_Type_Block_Size(ty) == 0);
}

static bool Is_Shared_Array(WN* wn, ST* st) {

  if (WN_operator(wn) == OPR_ARRAY) {
    WN* base = WN_array_base(wn);
    for (; WN_operator(base) == OPR_ARRAY; base = WN_array_base(base)); 
    return (WN_operator(base) == OPR_LDID || WN_operator(base) == OPR_LDA) &&
      ST_st_idx(WN_st(base)) == ST_st_idx(st);
  }
  return false;
}


void Print_Vec_Mesg(SRCPOS srcpos, char *repo) {
  INT32  line = Srcpos_To_Line(srcpos);
  const char   *fname = NULL;
  const char   *dname;
  IR_Srcpos_Filename(srcpos, &fname, &dname);
  
  fprintf(stderr, "%s: \t %s at line %d\n", fname, repo, line);
}

#define MOD(x)  (x < 0 ? -x : x)

enum DEP_SUMMARY {
  NO_DEP = 1, 
  CIRC_DEP = 2,
  TRUE_DEP = 3
};


int Deps_Say_Is_Vectorizable(ARRAY_DIRECTED_GRAPH16 *adg)
{
  
  DEP_SUMMARY result = NO_DEP;
  VINDEX16 i;
  EINDEX16 e;
  
  if(adg->_type != DEPV_ARRAY_ARRAY_GRAPH)
    return TRUE_DEP;
  
  if(adg->Get_Edge_Count() == 0)
    return NO_DEP;

  for(i=1; i < adg->Get_Vertex_Count(); i++) {
    //skip the local pointers
    BOOL is_load = FALSE;
    BOOL is_call = FALSE;
    WN *wn = adg->_v[i].Wn;
    if (OPCODE_is_load(WN_opcode(wn))) {
      is_load = TRUE;
      if (WN_kid_count(wn) >= 1) {
	wn = WN_kid0(wn); // not an ldid
      }
    } else if (OPCODE_is_store(WN_opcode(wn))) {
      if (WN_kid_count(wn) >= 2) {
	wn = WN_kid1(wn); // not an stid
      }
    } else {
      is_call = TRUE;
    }
    if (WN_operator(wn) == OPR_ARRAY) {
      WN *base = WN_array_base(wn);
      if(!Type_Is_Shared_Ptr(WN_ty(base)))
	continue;
    }  

    //not local pointer, check for no self edges
    e = adg->_v[i].Get_Out_Edge();
    while (e) {
      switch(result) {
      case NO_DEP:
	if(i == adg->_e[e].Get_Sink()) {
	  //circular dep
	} else
	  return TRUE_DEP;
	break;
      case CIRC_DEP:
	if(i == adg->_e[e].Get_Sink()) {
	  //circular dep
	} else
	  return TRUE_DEP;
	break;
      case TRUE_DEP:
	return TRUE_DEP;
	break;
      }
      e = adg->_e[e].Get_Next_Out_Edge();
    }
  }
} 

void Print_ADG(FILE *fp, ARRAY_DIRECTED_GRAPH16 *adg)
{
  VINDEX16 i;
  EINDEX16 e;
  if (adg->_type==DEPV_ARRAY_ARRAY_GRAPH) {
    fprintf(fp,"Printing an ARRAY_DIRECTED_GRAPH16 of type DEPV_ARRAY \n");
  } else if (adg->_type == LEVEL_ARRAY_GRAPH) {
    fprintf(fp,"Printing an ARRAY_DIRECTED_GRAPH16 of type level \n");
  } else {
    fprintf(fp,"Printing an ARRAY_DIRECTED_GRAPH16 of type DEP \n");
  }
  for (i=1; i<adg->_v.Lastidx()+1; i++) {
   if (!adg->_v[i].Is_Free()) {
    if (adg->_type==DEPV_ARRAY_ARRAY_GRAPH) {
#ifdef LNO
      BOOL is_load = FALSE;
      BOOL is_call = FALSE;
      WN *wn = adg->_v[i].Wn;
      if (OPCODE_is_load(WN_opcode(wn))) {
	is_load = TRUE;
	if (WN_kid_count(wn) >= 1) {
	  wn = WN_kid0(wn); // not an ldid
        }
      } else if (OPCODE_is_store(WN_opcode(wn))) {
	if (WN_kid_count(wn) >= 2) {
	  wn = WN_kid1(wn); // not an stid
        }
      } else {
	is_call = TRUE;
      }
      if (WN_operator(wn) == OPR_ARRAY) {
	WN *base = WN_array_base(wn);
	if (OPCODE_has_sym(WN_opcode(base)) && WN_st(base)) {
          if (is_load) {
            fprintf(fp,"Vertex %d for load from Wn = %s",i,
          		ST_name(WN_st(WN_array_base(wn))));
          } else {
            fprintf(fp,"Vertex %d for store into Wn = %s",i,
          		ST_name(WN_st(WN_array_base(wn))));
          }
        } else {
	  if (is_load) {
            fprintf(fp,"Vertex %d for load from Wn = ??? ",i);
          } else {
            fprintf(fp,"Vertex %d for store into Wn = ??? ",i);
          }
        }
        ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,wn);
        array->Print(fp);
      } else {
	if (is_load) {
          fprintf(fp,"Vertex %d for load from Wn = ",i);
        } else if (is_call) {
          fprintf(fp,"Vertex %d for call into Wn = ",i);
        } else {
          fprintf(fp,"Vertex %d for store into Wn = ",i);
        }
        Dump_WN(wn,fp,TRUE,0,0);
      }
#endif
    } else {
      fprintf(fp,"Vertex %d for Wn = 0x%p",i,adg->_v[i].Wn);
#ifdef LNO
      // this is LNO specific but other groups might want something similar
      Dump_WN(adg->_v[i].Wn,fp,TRUE,0,0);
#endif
      fprintf(fp,"\n");
    }
    e = adg->_v[i].Get_Out_Edge();
    while (e) {
      fprintf(fp,"Edge %d to vertex %d ",e,adg->_e[e].Get_Sink());
      if (adg->_type==DEPV_ARRAY_ARRAY_GRAPH) {
#ifdef LNO
        fprintf(fp," has DEPV_ARRAY = ");
        adg->_e[e].Depv_Array->Print(fp);
#endif
      } else if (adg->_type == LEVEL_ARRAY_GRAPH) {
#ifdef LNO
	fprintf(fp," has level %d \n",adg->_e[e].Level_Info.Level);
#endif
      } else {
	fprintf(fp," has dep ");
	DEP_Print(adg->_e[e].DEP_Struct.Dep,fp); 
	fprintf(fp," and Is_Must is %d\n",adg->_e[e].DEP_Struct.Is_Must);
      }
      e = adg->_e[e].Get_Next_Out_Edge();
    }
   }
  }

}



static void Generate_Void_Intrinsic(WN *bblock, INTRINSIC iop, BOOL first)
{
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 0);
  WN_intrinsic(call) = iop;
 
  WN_INSERT_BlockLast(block, call); 
  if(first)
    WN_INSERT_BlockFirst(bblock, block);
  else
    WN_INSERT_BlockLast(bblock, block);
  LWN_Set_Parent(block, bblock);
}


static void Generate_End_Call(WN *bblock, ST* ldesc) 
{
  
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 1);
  WN_intrinsic(call) = INTRN_VEC_ENEST;
  WN_kid0(call) = WN_CreateParm(Pointer_type, WN_Ldid(Pointer_type,0,ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
 
  WN_INSERT_BlockLast(block, call);
  WN_INSERT_BlockLast(bblock, block);
  LWN_Set_Parent(block, bblock);
}

static void Generate_FinDim_Call(WN *bblock, ST* ldesc, int dim) 
{
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 2);
  WN_intrinsic(call) = INTRN_VEC_FIND;
  WN_kid0(call) = WN_CreateParm(Pointer_type, WN_Ldid(Pointer_type,0,ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type,dim), 
				MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
 
  WN_INSERT_BlockLast(block, call);
  WN_INSERT_BlockLast(bblock, block);
  LWN_Set_Parent(block, bblock);
}

static void Generate_AdvDim_Call(WN *bblock, ST* ldesc, int dim, BOOL first = FALSE) 
{
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 2);
  WN_intrinsic(call) = INTRN_VEC_ADVD;
  WN_kid0(call) = WN_CreateParm(Pointer_type, WN_Ldid(Pointer_type,0,ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type,dim), 
				MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
 
 
  WN_INSERT_BlockLast(block, call);
   if(first)
    WN_INSERT_BlockFirst(bblock, block);
  else
    WN_INSERT_BlockLast(bblock, block);
  LWN_Set_Parent(block, bblock);
}


static void Generate_Get_Coeff(WN *bblock, ST* res, ST* ldesc, ST* rdesc, ST* lmad, int dim)
{
  WN *block, *call;
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, 4);
  WN_intrinsic(call) = INTRN_VEC_GET_COEFF;
  WN_kid0(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, rdesc, ST_type(rdesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, lmad, ST_type(lmad)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Integer_type,
				WN_Intconst(Integer_type, dim),
				MTYPE_To_TY(Integer_type),
				WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  call = WN_Ldid(Integer_type, -1, Return_Val_Preg, MTYPE_To_TY(Integer_type));
  call =  WN_CreateComma (OPR_COMMA, Integer_type, MTYPE_V,
			  block, call);
  block = WN_Stid(Integer_type, 0, res, MTYPE_To_TY(Integer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);

}


static void Generate_Get_Laddr_Call(WN *bblock, ST* res, ST* ldesc, ST* rdesc, ST* lmad, INTRINSIC iop = INTRINSIC_LAST)
{
  WN *block, *call;
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, (iop == INTRINSIC_LAST) ? 3 : 0);
  if(iop == INTRINSIC_LAST) {
    WN_intrinsic(call) = INTRN_VEC_GET_LADDR;
    WN_kid0(call) = WN_CreateParm(Pointer_type, 
				  WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				  MTYPE_To_TY(Pointer_type), 
				  WN_PARM_BY_VALUE);
    WN_kid1(call) = WN_CreateParm(Pointer_type, 
				  WN_Ldid(Pointer_type, 0, rdesc, ST_type(rdesc)), 
				  MTYPE_To_TY(Pointer_type), 
				  WN_PARM_BY_VALUE);
    WN_kid2(call) = WN_CreateParm(Pointer_type, 
				  WN_Ldid(Pointer_type, 0, lmad, ST_type(lmad)), 
				  MTYPE_To_TY(Pointer_type), 
				  WN_PARM_BY_VALUE);
  } else 
    WN_intrinsic(call) = iop;

  WN_INSERT_BlockLast(block, call);
  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
			  block, call);
  block = WN_Stid(Pointer_type, 0, res, ST_type(res), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);

}

static ST* Generate_Analyze_Call(WN *bblock, ST *ldesc, INTRINSIC iop = INTRINSIC_LAST)
{
  WN *call, *block;
 
  ST *res = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), (char*)"Vres");

  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Integer_type, MTYPE_V, (iop == INTRINSIC_LAST) ? 1 : 0);
  if(iop == INTRINSIC_LAST) {
    WN_intrinsic(call) = INTRN_VEC_ANAL;
    WN_kid0(call) = WN_CreateParm(Pointer_type,
				  WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				  MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  } else {
    WN_intrinsic(call) = iop;
  }
  WN_INSERT_BlockLast(block, call);
  call = WN_Ldid(Integer_type, -1, Return_Val_Preg, MTYPE_To_TY(Integer_type));
  call =  WN_CreateComma (OPR_COMMA, Integer_type, MTYPE_V,
				block, call);
  block = WN_Stid(Integer_type, 0, res, MTYPE_To_TY(Integer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
 
  return res;
}

//Generate a unique loop hash key for upcri_start_nest() 
static UINT64 Get_Loop_Hash_Key() {
    //FIXME: we should use the full path name here for the hash
    UINT64 hash = string_hash(Src_File_Name);
    static int counter = 0;
    counter++;
    return hash + counter;
}

static void Generate_Add_SosD_Call(WN *bblock, ST* ldesc, ST* rdesc, 
				   ST* lmad, 
				   WN *dim, WN *stride, WN* elc) {
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 6);
  WN_intrinsic(call) = INTRN_VEC_ADD_SDIM;
  WN_kid0(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, rdesc, ST_type(rdesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, lmad, ST_type(lmad)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Pointer_type, dim, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  
  WN_kid(call,4) = WN_CreateParm(Pointer_type, stride, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid(call,5) = WN_CreateParm(Pointer_type, elc, 
				 MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE); 
  WN_INSERT_BlockLast(block, call);
 
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
}
				   


static ST* Generate_New_Lmad_Call_With_Targ(WN *bblock, ST* ldesc, ST* rdesc, WN* base, int type)
{
  ST *lmad  = Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), (char*)"lmad");
  WN *call, *block;

 

  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, 4);
  WN_intrinsic(call) = INTRN_VEC_NEW_LMAD_TARG;
  WN_kid0(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, rdesc, ST_type(rdesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
 
  WN_kid2(call) = WN_CreateParm(WN_rtype(base), base, 
				WN_ty(base), WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, type), 
				  MTYPE_To_TY(Integer_type),WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
				block, call);
  block = WN_Stid(Pointer_type, 0, lmad, MTYPE_To_TY(Pointer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
  
  return lmad;

}

static ST* Generate_New_Lmad_Call(WN *bblock, ST* ldesc, ST* rdesc, WN* base, int type)
{
  ST *lmad  = Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), (char*)"lmad");
  WN *call, *block;

  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, 4);
  WN_intrinsic(call) = INTRN_VEC_NEW_LMAD;
  WN_kid0(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, rdesc, ST_type(rdesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  
  if(TY_kind(WN_ty(base))  == KIND_POINTER &&  TY_is_pshared(TY_pointed(WN_ty(base)))) {
    WN *tmp, *tmp1;
    
    tmp1  = WN_CreateBlock();
    tmp = WN_Create(OPR_INTRINSIC_CALL, TY_mtype(shared_ptr_idx), MTYPE_V, 1);
    WN_intrinsic(tmp) = INTRN_P_TO_S;
    WN_kid0(tmp) = 
    WN_CreateParm(TY_mtype(shared_ptr_idx), WN_COPY_Tree(base), WN_ty(base), WN_PARM_BY_VALUE);
    
    
    WN_INSERT_BlockLast(tmp1, tmp);
    WN *wn1 = WN_Ldid(TY_mtype(shared_ptr_idx), -1, Return_Val_Preg, shared_ptr_idx);
   
    ST *ret_st = Gen_Temp_Symbol(shared_ptr_idx, (char*) ".Mstopcvt.");
    wn1 = WN_Stid (TY_mtype(shared_ptr_idx), 0, ret_st, shared_ptr_idx, wn1);
    WN_INSERT_BlockLast(tmp1, wn1);
    // LWN_Set_Parent(wn1, bblock);
    WN_INSERT_BlockLast(bblock, tmp1);
    // LWN_Set_Parent(tmp1,bblock);
    
    WN_kid2(call) = WN_CreateParm(TY_mtype(shared_ptr_idx), 
				  WN_Ldid(TY_mtype(shared_ptr_idx), 0, ret_st, shared_ptr_idx),
				  shared_ptr_idx, WN_PARM_BY_VALUE);

  } else {

    WN_kid2(call) = WN_CreateParm(WN_rtype(base), base, 
				  WN_ty(base), WN_PARM_BY_VALUE);
  }
  WN_kid3(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, type), 
				  MTYPE_To_TY(Integer_type),WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
				block, call);
  block = WN_Stid(Pointer_type, 0, lmad, MTYPE_To_TY(Pointer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
  
  return lmad;

}


static ST* Generate_New_Redist_Ref_Call(WN *bblock, ST *ldesc, ST *peer_ref, ST *peer_lmad, 
					int type, int alias, int esize) 
{
  ST *rdesc =  Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), (char*)"rr");
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, 5);
  WN_intrinsic(call) = INTRN_VEC_NEW_RR;
  WN_kid0(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, peer_ref, ST_type(peer_ref)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, peer_lmad, ST_type(peer_lmad)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,alias), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid(call,4) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,esize), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  LWN_Set_Parent(call, block);
  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
				block, call);
  block = WN_Stid(Pointer_type, 0, rdesc, MTYPE_To_TY(Pointer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
  return rdesc;
}



static ST* Generate_New_Base_Ref_Call(WN *bblock, ST *ldesc, int type, int alias, int esize) 
{
  ST *rdesc =  Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), (char*)"br");
  WN *call, *block;
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, 3);
  WN_intrinsic(call) = INTRN_VEC_NEW_BR;
  WN_kid0(call) = WN_CreateParm(Pointer_type, 
				WN_Ldid(Pointer_type, 0, ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), 
				WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,alias), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,esize), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  LWN_Set_Parent(call, block);
  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
				block, call);
  block = WN_Stid(Pointer_type, 0, rdesc, MTYPE_To_TY(Pointer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
  return rdesc;
}


static void Generate_Loop1R(WN *bblock, DO_LOOP_INFO *dli, WN *base_addr, int type, int esize, int Nref, int Nop, WN *redist)
{
  WN *llb, *lub, *step;
  const WN *loop =  dli->ARA_Info->Loop();
  WN *block, *call;
  
  
  llb =   WN_COPY_Tree(Store_Expr(WN_start(loop)));
  lub =  WN_COPY_Tree(UBexp(WN_end(loop)));
  step = WN_COPY_Tree(dli->Step->Get_Base_WN());
  
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 8);
  WN_intrinsic(call) = INTRN_VEC_LOOP_1RS1;
  WN_kid0(call) = WN_CreateParm(Pointer_type, llb, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, lub, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Pointer_type, step, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,type), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid(call, 4) = WN_CreateParm(WN_rtype(base_addr), base_addr, 
				  WN_ty(base_addr), WN_PARM_BY_VALUE);
  WN_kid(call, 5) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,esize), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid(call, 6) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,Nref), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid(call, 7) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,Nop), 
				  MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  // WN_kid(call, 8) = WN_CreateParm(WN_rtype(redist), redist, 
				 //  WN_operator(redist) != OPR_INTCONST ? WN_ty(redist) : MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  LWN_Set_Parent(call, block);
  WN_INSERT_BlockLast(bblock, block);
  LWN_Set_Parent(block, bblock); 
}

static void Generate_Add_Pdim(WN *bblock, DO_LOOP_INFO *dli, ST *ldesc, int dim)
{
  WN *llb, *lub, *step;
  const WN *loop =  dli->ARA_Info->Loop();
  WN *block, *call;


  llb =   WN_COPY_Tree(Store_Expr(WN_start(loop)));
  lub =  WN_COPY_Tree(UBexp(WN_end(loop)));
  step = WN_COPY_Tree(dli->Step->Get_Base_WN());

  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 5);
  WN_intrinsic(call) = INTRN_VEC_ADD_PDIM;
  WN_kid0(call) = WN_CreateParm(Pointer_type, WN_Ldid(Pointer_type,0,ldesc, ST_type(ldesc)), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,dim), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid2(call) = WN_CreateParm(Pointer_type, llb, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
   WN_kid3(call) = WN_CreateParm(Pointer_type, lub, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
   WN_kid(call,4) = WN_CreateParm(Pointer_type, step, 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  LWN_Set_Parent(call, block);
  WN_INSERT_BlockLast(bblock, block);
  LWN_Set_Parent(block, bblock);
  
}

static ST* Generate_Get_Strips(WN *bblock, ST *ldesc, INTRINSIC iop = INTRINSIC_LAST) 
{
  
  WN *block, *call;
  ST *nelems;
  WN *llb, *lub, *step;
  ST *sdesc;

  sdesc = Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), (char*)"sd");
  block = WN_CreateBlock();
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, (iop == INTRINSIC_LAST)? 1 : 0);
  if(iop == INTRINSIC_LAST) {
    
    WN_intrinsic(call) = INTRN_VEC_GETSTR;
    WN_kid0(call) = WN_CreateParm(TY_mtype(ST_type(ldesc)), 
				  WN_Ldid(TY_mtype(ST_type(ldesc)), 0, ldesc, ST_type(ldesc)),
				  ST_type(ldesc), WN_PARM_BY_VALUE);
  } else 
    WN_intrinsic(call) = iop;
  WN_INSERT_BlockLast(block, call);
  LWN_Set_Parent(call, block);

  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, ST_type(sdesc));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
			  block, call);
  block = WN_Stid(Pointer_type, 0, sdesc, MTYPE_To_TY(Pointer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
  
  return sdesc;
  
}



// This code requires that the loop has been normalized i.e.
// for(i=0; i < U; i++)
static ST* Generate_Loop_Description(WN *bblock, DO_LOOP_INFO *dli, int isredist) 
{
  
  WN *block, *call;
  ST *nelems;
  WN *llb, *lub, *step;
  ST *ldesc;

  const WN *loop =  dli->ARA_Info->Loop();

  UINT64 hash_key = Get_Loop_Hash_Key();

  block = WN_CreateBlock();
  ldesc = Gen_Temp_Symbol(MTYPE_To_TY(Pointer_type), (char*)"ln");
  call = WN_Create(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V, 2);
  WN_intrinsic(call) = INTRN_VEC_START_LN;
  WN_kid0(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,Get_Loop_Hash_Key()), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Pointer_type, WN_Intconst(Pointer_type,isredist), 
				MTYPE_To_TY(Pointer_type), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  LWN_Set_Parent(call, block);
  call = WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));
  call =  WN_CreateComma (OPR_COMMA, Pointer_type, MTYPE_V,
				block, call);
  block = WN_Stid(Pointer_type, 0, ldesc, MTYPE_To_TY(Pointer_type), call);
  WN_INSERT_BlockLast(bblock, block); 
  LWN_Set_Parent(block, bblock);
  
  return ldesc;
  
}

static void Generate_Base_Addr(WN *bblock, ST *ast, WN *disp, ST *temp, 
			      int esize, int bsize, int phaseless) 
{
  WN *block, *call;
  ST *nelems;
  WN *llb, *lub, *step;
  TYPE_ID mtype;
  TY_IDX idx;
  WN *ld;

  idx = ST_type(ast);
  mtype = TY_mtype(idx);
  
  ld =  WN_Ldid(mtype, 0, ast, idx);
  if(Type_Is_Shared_Ptr(idx))
    call = WN_Add(Pointer_Mtype, ld, WN_COPY_Tree(disp));
  else
    call = WN_Add(Pointer_Mtype, ld, WN_Mpy(Integer_type, disp, WN_Intconst(Integer_type, esize))/*WN_COPY_Tree(disp)*/);

  block = WN_Create(OPR_TAS, mtype, MTYPE_V, 1);
  WN_kid0(block) = call;
  WN_set_ty(block, ST_type(temp));
  call = WN_Stid(mtype, 0, temp, idx, block);
  WN_INSERT_BlockLast(bblock, call);
  
  LWN_Set_Parent(call, bblock);
}

static void Generate_Test_Vect(WN *prefix, WN *loop, ST *nrefs, int srefs, WN *old, WN *xtra, WN *adim = 0)
{
 
  WN *bthen, *belse, *next;
  WN *block, *call, *bblock;
  
  bblock = (WN*)LWN_Get_Parent(loop);
  if(xtra)
    WN_EXTRACT_FromBlock(bblock, xtra);
  WN_INSERT_BlockBefore(bblock, loop, prefix);
  LWN_Set_Parent(prefix, bblock);
  next = WN_next(loop);
  WN_EXTRACT_FromBlock(bblock, loop);
  bthen = WN_CreateBlock();
  WN_INSERT_BlockLast(bthen, loop);
  if(adim) 
    WN_INSERT_BlockLast(bthen, adim);
  if(xtra)
    WN_INSERT_BlockLast(bthen, xtra);
  
  

  belse = WN_CreateBlock();
  WN_INSERT_BlockLast(belse, old);
  
  block = WN_CreateIf(WN_EQ(Integer_type,
			    WN_Ldid(Integer_type, 0, nrefs, ST_type(nrefs)),
			    WN_Intconst(Integer_type, srefs)),
		      bthen, belse);
  WN_INSERT_BlockBefore(bblock, next, block);
 
  LWN_Set_Parent(block, bblock);
  LWN_Set_Parent((WN*)loop, bthen);
  if(xtra)
    LWN_Set_Parent(xtra, bthen);
  if(adim)
    LWN_Set_Parent(adim, bthen);
}




WN*  REMOTE_REF_DESCR::Region_Base(REGION *reg, AXLE_NODE *axle, STACK<WN *> *ind_vars) 
{
  WN *idx;
  ACCESS_ARRAY *acc;
  if(axle) {
    idx = axle->lo->_ac_v->Get_Base_WN();
    return idx;
  } 

  acc = (ACCESS_ARRAY*)  WN_MAP_Get(LNO_Info_Map,reg->_wn_list.Bottom_nth(0));
  Is_True(acc->Num_Vec() == 1, (""));
 return  acc->Dim(0)->Get_Base_WN(ind_vars);
  
}


WN* Find_Loop_Coeff(AXLE_NODE *axle, int level, ST *ist)
{
  return WN_Intconst(Integer_type, axle->lo->_ac_v->Loop_Coeff(level));

}


WN *Contrib_Index_Level(AXLE_NODE* axle, ARA_LOOP_INFO* ali, int level) 
{
  WN *result = NULL;
  WN *index;
  ST *ist;
  int i;
  
  index = WN_index(ali->Do_Stack().Bottom_nth(level));
  ist = WN_st(index);
  index =  WN_COPY_Tree(Find_Loop_Coeff(axle, level, ist));
  if(index)
    result = WN_Mpy(Integer_type, 
		    WN_Ldid(TY_mtype(ST_type(ist)), 0, ist, ST_type(ist)),
		    index);
  
  return result;
  
}

WN * REMOTE_REF_DESCR::Region_Span(REGION *reg, AXLE_NODE *axle, 
				   WN *loop, WN *stride) 
{
  WN *ub;
  WN *idx;
  int coeff;
  ub = UBexp(WN_end(loop));
  
  return WN_COPY_Tree(ub);
  //return WN_Mpy(Integer_type, WN_COPY_Tree(ub), WN_COPY_Tree(stride));
 
}


BOOL Same_Ref(WN *l, WN *r)
{
  fprintf(stderr, "Same_Ref NOT IMPLEMENTED - Faking it! ....\n");
  return TRUE;
}


/* This function walks through the reference lists and groups references
   to fields of the same STRUCT */
void REMOTE_REF::Collapse_Similar() 
{
  int i, j;
  DYN_ARRAY<REMOTE_REF_DESCR*> c(VEC_mpool);
  REMOTE_REF_DESCR *l, *r;
  ARA_REF *Al, *Ar;
  REGION *Rl, *Rr;
  WN *Wl, *Wr;
  int active[64];
  int last_insert = -1;
  if(level.Elements() > 64)
    Fail_FmtAssertion("Not enough temp space Collapse_Similar\n");

  if(level.Elements() == 1)
    return;

  for(i=0; i < level.Elements(); i++)
    active[i] = 1;
  for(i=level.Lastidx(); i >= 0; i--){
    if(!active[i]) continue;
    l = level[i];
    for(j=i-1; j >= 0; j--) {
      if(!active[j])
	continue;
      r = level[j];
      if(l->use != 0 && r->use != 0) {
	Is_True(l->def == 0 && r->def == 0, (("")));
	Al = l->use;
	Ar = r->use;
      } else if(l->def != 0 && r->def !=0) {
	Is_True(l->use == 0 && r->use == 0, (("")));
	Al = l->def;
	Ar = r->def;
      } else Fail_FmtAssertion("");
      
      if(Al->Image().Len() == 1 && Ar->Image().Len() == 1) {
	if(TY_kind(Get_Inner_Array_Type(ST_type(Al->Array().St()))) == KIND_STRUCT &&
	   TY_kind(Get_Inner_Array_Type(ST_type(Ar->Array().St()))) == KIND_STRUCT) {
	  Wl = Al->Image().Any_Wn();
	  Wr = Ar->Image().Any_Wn();
	  active[j] = 0;
	  if(Same_Ref(Wl,Wr)) {
	    if(last_insert != i) {
	      last_insert = i;
	      c.AddElement(l);
	    }
	    l->Add_Similar_Wn(Wr);
	  }
	}
      }  else {
	 if(last_insert != i) {
	   c.AddElement(l);
	   last_insert = i;
	 }
      }
    }
    if(last_insert != i) {
      c.AddElement(l);
      last_insert = i;
    }
  }
  
  level.Resetidx();
  for(i = c.Elements()-1; i >= 0; i--)
    level.AddElement(c[i]);
}

BOOL REMOTE_REF::Analyze()
{
  BOOL result = TRUE;
  int i;
  
  DYN_ARRAY<REGION*> pr(VEC_mpool);

  // We do not allow nests where an array reference appears at
  // different levels so we need to analyze only the deepest references.
  Collapse_Similar();
  for(i = level.Elements()-1; i >= 0; i--) {
   //  result = level[level.Elements()-1]->Analyze(pr);
//     total_refs += (level[level.Elements()-1]->stride_use.Elements() + 
// 		   level[level.Elements()-1]->stride_def.Elements());
    result &= level[i]->Analyze(pr);
    total_refs += (level[i]->stride_use.Elements() + 
		   level[i]->stride_def.Elements());
  }
   
  return result;
}


BOOL REMOTE_REF_DESCR::Check_Deps(ARRAY_DIRECTED_GRAPH16 *adg)
{
  VINDEX16 vertex, sink;
  EINDEX16 edge;
  int j;
  WN *ref, *parent;
  
 

  if(def) {
    REGION_UN & img = def->Image();
    REGION_ITER iter(&img);
    for (REGION *cur = iter.First();  !iter.Is_Empty(); cur = iter.Next()) {
      for(j = 0; j < cur->_wn_list.Elements(); j++) {
	ref = cur->_wn_list.Bottom_nth(j);
	parent = LWN_Get_Parent(ref);
	if(WN_kid1(parent) == ref) {
	  vertex = adg->Get_Vertex(parent);
	  if(vertex) {
	    edge = adg->Get_In_Edge(vertex);
	    while(edge) {
	      sink = adg->Get_Source(edge);
	      if(sink != vertex)
		return TRUE;
	      edge = adg->Get_Next_Edge(edge);
	    }
	    
	    edge = adg->Get_Out_Edge(vertex);
	    while(edge) {
	      sink = adg->Get_Sink(edge);
	      if(sink != vertex)
		return TRUE;
	      edge = adg->Get_Next_Edge(edge);
	    }
	  }
	}
      }
    }
  } else return FALSE;
  
  return FALSE;
}


BOOL Reg_Processed(REGION *reg, DYN_ARRAY<REGION*> &pr)
{
  int i;
  
  for(i=0; i < pr.Elements(); i++) {
    if(reg == pr[i])
      return TRUE;
  }
  return FALSE;
  

}


BOOL Base_Is_Def(SYMBOL *sym, ARA_LOOP_INFO *ali)
{
  int i;
  
  for(i=0; i < ali->SCALAR_DEF().Elements(); i++) {
    if(*sym == ali->SCALAR_DEF().Bottom_nth(i)->_scalar)
      return TRUE;
  }
  
  return FALSE;
}


// For each family of array references, the analysis tries
// to summarize as many as possible with the same region.
// E.g. a[i+j+1] and a[i+j] will be represented by the same region.
// also a[3*i+j+1]  and a[3*i+j] will be represented by the same region
//
// The first implementation of Analyze() does the following:
//  - for each WN summarized by a region:
//     > find the original region  and generate the LMAD info for the WN
//     > generate the call for the base address
//     > the index transformation becomes local[index]
//
// This approach introduces unnecessary run-time overhead because it forces analysis
// for each base reference, but it simplifies greatly the index generation logic.
//
// If the runtime overhead is prohibitive, we could generate one LMAD for each array region and 
// compute at compile time  the index transformation for each summarized reference.
// In order to do this, get the access vector for each wn, call Subtract() on the pairs and
// do a symbolic sort of the differences in the index variable.  There are some extra conditions
// for this to work (coeff1 == coeff2, disp2-disp1 % coeff == 0 or coeff == 1)
// Also, in this case, everything is conditioned by the loop bounds.
// Example: for(i = 0; i < N..)
//             .. = a[i] + a[i+100] 
//
// produces a conservative union [0, N+100, 1] 
// If N < 100, the remote regions are disjoint and we want to bring them separately.

BOOL  REMOTE_REF_DESCR::Analyze(DYN_ARRAY<REGION*> &processed_regs) 
{
  int i,j;
  WN *aref;
  REGION  *temp;
  WN *rstep, *lstep, *wn;
  BOOL ldec, rdec;
  BOOL result = FALSE;
  STACK<REGION*> all_uses(VEC_mpool);
  STACK<REGION*> all_defs(VEC_mpool);
  REGION *cur_use, *cur_def;

  if((use && use->Is_Too_Messy()) || (def && def->Is_Too_Messy())) {
    if(trace_msg_vect)
      Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Use/Def too messy"); 
     
  }
 
  if(check_deps)
    deps = Check_Deps(Array_Dependence_Graph);

  if(deps) {
    if(trace_msg_vect)
      fprintf(stderr, "Loop with inter-nest dependencies at line %d.\n",  
	      Srcpos_To_Line(WN_Get_Linenum(ali->Loop())));
    return result;
  }

  if(use) {
    if(Base_Is_Def((SYMBOL*)&use->Array(), ali)) {
      if(trace_msg_vect)
	Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Array base is killed");
      return FALSE;
    }
    REGION_UN & img = use->Image();
    //img.Print(stderr);
    REGION_ITER iter(&img);
    for (REGION *cur = iter.First();  !iter.Is_Empty(); cur = iter.Next()) {
      if(Reg_Processed(cur, processed_regs))
	continue;
      else
	processed_regs.AddElement(cur);
      for(j = 0; j < cur->_wn_list.Elements(); j++) {
	aref = cur->_wn_list.Bottom_nth(j);
	temp = CXX_NEW(REGION(aref, ali), VEC_mpool);
	//temp->Print(stderr);
	AXLE_NODE &axle = temp->Dim(0); 
	for(i = ali->Depth() - depth; i <= ali->Depth(); i++) {
	  switch(temp->_type) {
	  case  ARA_TOO_MESSY:
	  case ARA_NORMAL:
	    rstep = NULL;
	    rdec = FALSE;
	    ldec = FALSE;
	    if(!Can_Vectorize_on_Axle(ali, temp, &axle, ldec, rdec, &rstep, &lstep, i, depth, ind_vars)) {
	      //  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,aref);
// 	       array->Print(stderr);
	      if(trace_msg_vect)
		Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Can't vectorize on one axle");
	      return FALSE;
	    } else {
	      result = TRUE;
	      all_uses.Push(temp);
	      if(rstep != NULL) {
		stride_use.AddElement(rstep);
		span_use.AddElement(Region_Span(temp, &axle, ali->Do_Stack().Bottom_nth(i),rstep));	  
		
	      } else { 
		Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Missing index expression\n");
		stride_use.AddElement(WN_Intconst(Integer_type,0));
		span_use.AddElement(WN_Intconst(Integer_type,0));
	      }
	    }
	    break;
	  case ARA_TOP:
	  case ARA_BOTTOM:
	    result = FALSE;
	    if(trace_msg_vect) 
	      Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()),  "ARA_TOP/BOTTOM in Analyze");
	     
	    break;
	  }
	}
	if(result) {
	  WN *base, *tmp;
	  base = Region_Base(temp, &axle, ind_vars);
	  for(i=0; i < ali->Depth()-depth; i++) {
	    tmp = Contrib_Index_Level(&axle, ali, i);
	    if(tmp)
	      base = WN_Add(Integer_type, base, tmp );
	  }
	  base_use.AddElement(base);
	}
      } 
    }
  }

  if(def) {
    if(Base_Is_Def((SYMBOL*)&def->Array(), ali)) {
	  if(trace_msg_vect)
	    Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Array base is killed");
	    
	  return FALSE;
    }
    REGION_UN & img = def->Image();
    //img.Print(stderr);
    REGION_ITER iter(&img);
    for (REGION *cur = iter.First();  !iter.Is_Empty(); cur = iter.Next()) {
      if(Reg_Processed(cur, processed_regs))
	continue;
      else
	processed_regs.AddElement(cur);
      for(j = 0; j < cur->_wn_list.Elements(); j++) {
	aref = cur->_wn_list.Bottom_nth(j);
	temp = CXX_NEW(REGION(aref, ali), VEC_mpool);
	//temp->Print(stderr);
	AXLE_NODE &axle = temp->Dim(0); 
	for(i = ali->Depth() - depth; i <= ali->Depth(); i++) {
	  switch(temp->_type) {
	  case  ARA_TOO_MESSY:
	  case ARA_NORMAL:
	    rstep = NULL;
	    rdec = FALSE;
	    ldec = FALSE;
	    if(!Can_Vectorize_on_Axle(ali, temp, &axle, ldec, rdec, &rstep, &lstep,i, depth, ind_vars)) {
	      if(trace_msg_vect)
		Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Can't vectorize on one axle");
	
	      return FALSE;
	    } else {
	      result = TRUE;
	      all_defs.Push(temp);
	      if(rstep != NULL) {
		stride_def.AddElement(rstep);
		span_def.AddElement(Region_Span(temp, &axle, ali->Do_Stack().Bottom_nth(i), rstep));	  
	      } else {
		Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Missing index expression\n");
		stride_def.AddElement(rstep);
		span_def.AddElement(WN_Intconst(Integer_type,0));
		Fail_FmtAssertion("NULL step in Analyze\n");
	      }
	    }
	    break;
	  case ARA_TOP:
	  case ARA_BOTTOM:
	    result = FALSE;
	    if(trace_msg_vect) 
	      Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()),  "ARA_TOP/BOTTOM in Analyze");
	      
	    break;
	  }
	  
	}
	if(result) {
	  WN *base, *tmp;
	  base = Region_Base(temp, &axle, ind_vars);
	  for(i=0; i < ali->Depth()-depth; i++) {
	    tmp =  Contrib_Index_Level(&axle, ali, i);
	    if(tmp)
	      base = WN_Add(Integer_type, base, tmp);
	  }
	  base_def.AddElement(base);
	}
      } 
    }
  }
  
  // For the time being we vectorize only loops where the read/write sets
  // are described by UGSes. This means that at runtime either both reads
  // or writes are represented by one summary LMAD or they are disjoint
  // This can be relaxed a little if we perform more checks. Things like
  //      a[2*i] = a[i]
  // can be handled. Don't see the utility right now.
  ACCESS_VECTOR *av;
  for(i=0; i < all_uses.Elements(); i++) {
    cur_use = all_uses.Bottom_nth(i);
    for(j = 0; j < all_defs.Elements(); j++) {
      cur_def =  all_defs.Bottom_nth(j);
      av = Subtract(cur_use->Dim(0).lo->_ac_v, cur_def->Dim(0).lo->_ac_v, VEC_mpool);
      if(!av || !av->Has_Only_Zero_Coeff()) return FALSE; 
    }
  }
 


  if(redist_targ) {
    ARA_REF *tmp = 0;
    if(use && def) {
      Is_True(0, ("Loop marked for redistribution has R/W conflicts"));
      return FALSE;
    }
    
    tmp = redist_targ;

    REGION_UN & img = tmp->Image();
    REGION_ITER iter(&img);
    for (REGION *cur = iter.First();  !iter.Is_Empty(); cur = iter.Next()) {
      if(Reg_Processed(cur, processed_regs))
	continue;
      else
	processed_regs.AddElement(cur);
      for(j = 0; j < cur->_wn_list.Elements(); j++) {
	aref = cur->_wn_list.Bottom_nth(j);
	if(aref != redist_wn)
	  continue;
	temp = CXX_NEW(REGION(aref, ali), VEC_mpool);
	AXLE_NODE &axle = temp->Dim(0); 
	for(i = ali->Depth() - depth; i <= ali->Depth(); i++) {
	  switch(temp->_type) {
	  case  ARA_TOO_MESSY:
	  case ARA_NORMAL:
	    rstep = NULL;
	    rdec = FALSE;
	    ldec = FALSE;
	    if(!Can_Vectorize_on_Axle(ali, temp, &axle, ldec, rdec, &rstep, &lstep, i, depth, ind_vars)) {
	     
	      if(trace_msg_vect)
		Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()), "Can't vectorize on one axle");
	      return FALSE;
	    } else {
	      result = TRUE;
	      all_uses.Push(temp);
	      if(rstep != NULL) {
		stride_redist.AddElement(rstep);
		span_redist.AddElement(Region_Span(temp, &axle, ali->Do_Stack().Bottom_nth(i),rstep));	  
	
	      } else 
		Fail_FmtAssertion("NULL step in Analyze\n");
	    }
	    break;
	  case ARA_TOP:
	  case ARA_BOTTOM:
	    result = FALSE;
	    if(trace_msg_vect) 
	      Print_Vec_Mesg(WN_Get_Linenum(ali->Loop()),  "ARA_TOP/BOTTOM in Analyze");
	     
	    break;
	  }
	}
	if(result) {
	  WN *base, *tmp;
	  base = Region_Base(temp, &axle, ind_vars);
	  for(i=0; i < ali->Depth()-depth; i++) {
	    tmp = Contrib_Index_Level(&axle, ali, i);
	    if(tmp)
	      base = WN_Add(Integer_type, base, tmp );
	  }
	  base_redist.AddElement(base);
	}
      } 
    }

  }

  return result;
}


REMOTE_REF_DESCR::REMOTE_REF_DESCR(ARA_REF *_use, ARA_REF *_def, ARA_LOOP_INFO *li, 
				   BOOL check, int level, MEM_POOL *mpool) {
  
  int i;
  int top;
  use = _use;
  def = _def;
  ali = li;
  redist_targ = 0;
  redist_wn = 0;
  check_deps = check;
  deps = FALSE;
  depth = level;
  stride_use.Set_Mem_Pool(mpool);
  span_use.Set_Mem_Pool(mpool);
  base_use.Set_Mem_Pool(mpool);
  stride_def.Set_Mem_Pool(mpool);
  span_def.Set_Mem_Pool(mpool);
  base_def.Set_Mem_Pool(mpool);
  stride_redist.Set_Mem_Pool(mpool);
  span_redist.Set_Mem_Pool(mpool);
  base_redist.Set_Mem_Pool(mpool);
  similar_wn.Set_Mem_Pool(mpool);

  ind_vars = CXX_NEW(STACK<WN *> (VEC_mpool), VEC_mpool);
  
  top = li->Do_Stack().Elements();
  for(i = 0; i < level; i++) {
    ind_vars->Push(WN_index(li->Do_Stack().Bottom_nth(i)));
  } 

  ind_vars->Push(WN_index(li->Loop()));

  
}


void REMOTE_REF_DESCR::Do_Code_Gen(ST *ldesc, WN *prefix, WN *laddr_init, BOOL is_redist, int total_refs = 0)
{
  DO_LOOP_INFO *dli = (DO_LOOP_INFO*)ali->Info();
  int i, esize, bsize,j, type,k;
  ST *rdesc, *lmad;
  ST *tbase, *local_ptr;
  ST *tbase_redist;
  int num_dims;
  ST * a_st;
  TY_IDX a_ty, a_ty_redist;
  TY_IDX ety;
  ST *nrefs;
  ST *new_coeff;
  int cur_pos;
  BOOL use_simple_interface = FALSE;
  if(use)
    a_st = use->Array().St();
  else if(def)
    a_st = def->Array().St();
  else 
    Fail_FmtAssertion("Empty ARA_REF in VEC\n");


  a_ty = ST_type(a_st);
  STACK<ST*>  lstack(VEC_mpool);
  
  if(TY_kind(a_ty) == KIND_POINTER) { 
    ety = TY_pointed(a_ty);
    esize =  TY_adjusted_size(ety);
  } else if(TY_kind(a_ty) == KIND_ARRAY) {
    ety = Get_Inner_Array_Type(a_ty);
    esize = TY_adjusted_size(ety);
  }
  bsize = Get_Type_Block_Size(ety);
  num_dims = (depth+1 < ali->Depth()+1) ? depth+1 : ali->Depth()+1;
  tbase = Gen_Temp_Symbol(a_ty, "rbase");   
 
  use_simple_interface = ((num_dims == 1) &&  (stride_use.Elements() + stride_def.Elements() == 1) && (total_refs == 1));
  if(use_simple_interface)
    is_redist = 0;
  if(!use_simple_interface) {
    rdesc =  Generate_New_Base_Ref_Call(prefix, ldesc, 0, 0, esize);
    //This test does not hold 
    // Is_True(stride_use.Elements() >= num_dims ||
// 	    (use == NULL && stride_use.Elements() == 0), 
// 	    (("Mismatch between dims and strides")));
  }
  
  if(stride_use.Elements() != 0)
    num_dims = (num_dims > stride_use.Elements()) ? stride_use.Elements() : num_dims;
  for(i=0; i < stride_use.Elements()/num_dims; i++) {
    Generate_Base_Addr(prefix, a_st, base_use[i], tbase, esize, bsize, (bsize == 0));
    
    if(!use_simple_interface) {
      lmad = Generate_New_Lmad_Call(prefix, ldesc, rdesc, 
				    WN_Ldid(TY_mtype(a_ty), 0, tbase, ST_type(tbase)), 
				    REF_READ);
      for(j=0; j < num_dims; j++) {
	Generate_Add_SosD_Call(prefix, ldesc, rdesc, lmad, WN_Intconst(Integer_type,j), 
			       stride_use[i*num_dims+j], 
			       WN_Mpy(Integer_type, WN_COPY_Tree(span_use[i*num_dims+j]),
				      WN_COPY_Tree(stride_use[i*num_dims+j])));
      }
    }
    if(!is_redist) {
      local_ptr = Gen_Temp_Symbol(Make_Pointer_Type(Shared_To_Private_Type(ety)), "lbase");
      lstack.Push(local_ptr); 
      Generate_Get_Laddr_Call(laddr_init, local_ptr, ldesc, rdesc, lmad, 
			      use_simple_interface ? INTRN_VEC_GET_LADDR_1RS1 : INTRINSIC_LAST); 
    }  
  }
  
  
  if(use && !is_redist) {
    REGION_UN & img = use->Image();
    REGION_ITER iter(&img);
    i = 0;
    cur_pos = 0;
    
    for (REGION *cur = iter.First();  !iter.Is_Empty(); cur = iter.Next()) {
      for(j=0; j < cur->_wn_list.Elements(); j++) {
	new_coeff = Generate_New_Coeff(prefix, laddr_init, ldesc, rdesc, lmad, cur_pos, 
				       num_dims < stride_use.Elements()  ? num_dims : stride_use.Elements(), 
				       TRUE);
	if(cur->_wn_list.Elements() > j && lstack.Elements() > i) {
	  Replace_Shared_Access(cur->_wn_list.Bottom_nth(j), a_st, lstack.Bottom_nth(i), 
				WN_st(WN_index(ali->Loop())), 
				new_coeff );
	  for(k = 0; k < similar_wn.Elements(); k++) {
	    Replace_Shared_Access(similar_wn[k], a_st, lstack.Bottom_nth(i), 
				  WN_st(WN_index(ali->Loop())), 
				  new_coeff );
	  }
	}
	i++;
      }
    }
  }
  
  lstack.Clear();
  Is_True(stride_def.Elements() >= num_dims ||
	  (def == NULL && stride_def.Elements() == 0), (("Mismatch between dims and strides")));
  
  if(stride_def.Elements() != 0)
    num_dims = (num_dims > stride_def.Elements()) ? stride_def.Elements() : num_dims;
  
  for(i=0; i < stride_def.Elements()/num_dims; i++) {
      Generate_Base_Addr(prefix, a_st, base_def[i], tbase, esize, bsize, (bsize == 0));

      if(!use_simple_interface) {
	lmad = Generate_New_Lmad_Call(prefix, ldesc, rdesc, 
				      WN_Ldid(TY_mtype(a_ty), 0, tbase, ST_type(tbase)), 
				      REF_WRITE);
	for(j=0; j < num_dims; j++) {
	  Generate_Add_SosD_Call(prefix, ldesc, rdesc, lmad, WN_Intconst(Integer_type,j), 
				 stride_def[i*num_dims+j], 
				 WN_Mpy(Integer_type, WN_COPY_Tree(span_def[i*num_dims+j]),
					WN_COPY_Tree(stride_def[i*num_dims+j]))); 
	}
      }
      if(!is_redist) {
	local_ptr = Gen_Temp_Symbol(Make_Pointer_Type(Shared_To_Private_Type(ety)), "lbase");
	lstack.Push(local_ptr);
	Generate_Get_Laddr_Call(laddr_init, local_ptr, ldesc, rdesc, lmad,
				use_simple_interface ? INTRN_VEC_GET_LADDR_1RS1 : INTRINSIC_LAST);
      } 
    }
 
  
  if(def && !is_redist) {
    REGION_UN & dimg = def->Image();
    REGION_ITER diter(&dimg);
    i = 0;
    cur_pos = 0;
    if(!use_simple_interface) {
      for (REGION *cur = diter.First();  !diter.Is_Empty(); cur = diter.Next()) {
	for(j=0; j < cur->_wn_list.Elements(); j++) {
	  new_coeff = Generate_New_Coeff(prefix, laddr_init, ldesc, rdesc, lmad, cur_pos, num_dims, FALSE);
	  Replace_Shared_Access(cur->_wn_list.Bottom_nth(j), a_st, lstack.Bottom_nth(i), 
				WN_st(WN_index(ali->Loop())), 
				new_coeff);
	  for(k = 0; k < similar_wn.Elements(); k++) {
	    Replace_Shared_Access(similar_wn[k], a_st, lstack.Bottom_nth(i), 
				  WN_st(WN_index(ali->Loop())), 
				  new_coeff );
	  }
	  i++;
	}
      }
    }  else {
      //Is_True(0, (("Not implemented\n"))); 
    }
  }

 
  tbase_redist = 0;
  if(use_simple_interface)
    is_redist = 0;
  // on Elan - redist not good; on Infiniband - redist good
  
  if(is_redist) {
    int redist_type;
    if(use == 0)
      redist_type = REF_READ;
    else if (def == 0) 
      redist_type = REF_WRITE;
   
    
    if(!(redist_targ &&(use  == 0 || def == 0))) {
      Print_Vec_Mesg(WN_Get_Linenum((WN*)ali->Loop()), "Target not set in loop marked for redistribution or loop erroneously marked for redistribution");
      Fail_FmtAssertion(0);
    }
    Is_True(redist_targ && (use == 0 || def == 0), ("Target not set in loop marked for redistribution or loop erroneously marked for redistribution"));
    if(!use_simple_interface)
      rdesc =  Generate_New_Redist_Ref_Call(prefix, ldesc, rdesc, lmad, 0, 0, esize);
    a_st = redist_targ->Array().St();
    a_ty_redist = ST_type(a_st);
    tbase_redist = Gen_Temp_Symbol(a_ty_redist, "Rrbase"); 
    num_dims = (depth+1 < ali->Depth()+1) ? depth+1 : ali->Depth()+1;
    
    Is_True(stride_redist.Elements() >= num_dims ||
	    (redist_targ == NULL && stride_redist.Elements() == 0), 
	    (("Mismatch between dims and strides")));
    for(i=0; i < stride_redist.Elements()/num_dims; i++) {
      Generate_Base_Addr(prefix, a_st, base_redist[i], tbase_redist, esize, bsize, (bsize == 0));
      if(!use_simple_interface) {
	lmad = Generate_New_Lmad_Call_With_Targ(prefix, ldesc, rdesc, 
						WN_Ldid(TY_mtype(a_ty_redist), 0, 
							tbase_redist, ST_type(tbase_redist)), 
						redist_type);
	for(j=0; j < num_dims; j++) {
	  Generate_Add_SosD_Call(prefix, ldesc, rdesc, lmad, WN_Intconst(Integer_type,j), 
				 stride_redist[i*num_dims+j], 
				 WN_Mpy(Integer_type, WN_COPY_Tree(span_redist[i*num_dims+j]),
					WN_COPY_Tree(stride_redist[i*num_dims+j])));
	} 
      }
    }
  }
  
   if(use_simple_interface) {
    if(stride_use.Elements() == 1)
      type = REF_READ;
    else 
      type = REF_WRITE;
    
    Generate_Loop1R(prefix, dli, 
		    WN_Ldid(TY_mtype(a_ty), 0, tbase, ST_type(tbase)), type, esize, 1, 2, 
		    tbase_redist ? WN_Ldid(TY_mtype(a_ty_redist), 0, tbase_redist, ST_type(tbase_redist)) :
		    WN_Intconst(Pointer_type, 0));
  } 
}




void REMOTE_REF::Do_Code_Gen(ST *ldesc, WN *prefix, WN *laddr_init, BOOL is_redist)
{
  level[level.Elements()-1]->Do_Code_Gen(ldesc, prefix, laddr_init, is_redist);
  //Is_True(level.Elements()==1,(("Oops!")));
}

WN * REMOTE_REF_DESCR::Try_Static_Coeff(int cur_pos, int num_dim, BOOL is_use) 
{
  WN *result = NULL;
  WN* stride0, *span0, *stride1, *span1, *stride2, *span2;
  WN *index1, *index2, *index0;
  ARA_LOOP_INFO *parent;
  WN *term;
  ST *indv;
  TYPE_ID indv_mtype;
  int is0, is1, i;
  BOOL prev_was_eq = FALSE;
  
  if(is_use) {
    stride0 = stride_use[cur_pos+num_dim-1];
    span0 = span_use[cur_pos+num_dim-1];
    stride1 = stride_use[cur_pos+num_dim-2];
    span1 = span_use[cur_pos+num_dim-2];
  } else {
    stride0 = stride_def[cur_pos+num_dim-1];
    span0 = span_def[cur_pos+num_dim-1];
    stride1 = stride_def[cur_pos+num_dim-2];
    span1 = span_def[cur_pos+num_dim-2];
  }
  
  indv  = WN_st(WN_index(ali->Loop()));
  indv_mtype = TY_mtype(ST_type(indv));
  index0 = WN_Ldid(indv_mtype, 0, indv, ST_type(indv));
  indv = WN_st(WN_index(ali->Parent()->Loop()));
  index1 = WN_Ldid(indv_mtype, 0, indv, ST_type(indv));
  parent = ali->Parent();
  
  if(WN_operator(stride0) == OPR_INTCONST) {
    is0 = WN_const_val(stride0);
    switch(is0) {
    case -1:  
    case 1:
      if(WN_operator(stride1) == OPR_INTCONST && WN_const_val(stride1) == 0) {
	result = WN_COPY_Tree(index0);
	
      } else {
	result = WN_Add(indv_mtype, WN_COPY_Tree(span0), WN_Intconst(indv_mtype,1));
	result =  WN_CreateExp2 (OPCODE_make_op (OPR_MIN,
						 indv_mtype,
						 MTYPE_V),
				 result, WN_COPY_Tree(stride1));
	term = result;
	result = WN_Mpy(indv_mtype,index1,result);
	result = WN_Add(indv_mtype, index0, result);
	
      }
      if(num_dim == 2)
	return result;
      break;
    case 0:
      result = WN_Mpy(indv_mtype, index1, WN_COPY_Tree(stride1));
      if(num_dim == 2) {
	return result;
      } 
      break;
    default:
      
      if(WN_operator(stride1) == OPR_INTCONST && WN_const_val(stride1) >= is0 
	 && (WN_const_val(stride1) % is0) == 0) {
	result = WN_Add(indv_mtype, WN_COPY_Tree(span0), WN_Intconst(indv_mtype,1));

	result =  WN_CreateExp2 (OPCODE_make_op (OPR_MIN,
						 indv_mtype,
						 MTYPE_V),
				 result, WN_COPY_Tree(stride1));
	term = result;
				 
	result = WN_Add(indv_mtype, index0, 
			WN_Mpy(indv_mtype, index1, result));
	if(num_dim == 2)
	  return result;
      } 
      break;
    }
  }
  
  if(!result)
    return NULL;

  if(WN_operator(stride1) == OPR_INTCONST && WN_const_val(stride1) == is0)
    prev_was_eq = TRUE;

  //num_dim > 2
  if(WN_operator(stride1) != OPR_INTCONST) {
    WN_DELETE_Tree(result);
    return NULL;
  }

  for(i = 0; i < num_dim-2; i++) {
    if(is_use) {
      stride2 = stride_use[cur_pos+num_dim-3-i];
      span2 =  span_use[cur_pos+num_dim-3-i];
    } else {
      stride2 = stride_def[cur_pos+num_dim-3-i];
      span2 =  span_def[cur_pos+num_dim-3-i];
    }
   //  fdump_tree(stderr, WN_end(parent->Loop()));
    parent =  parent->Parent();
    indv = WN_st(WN_index(parent->Loop()));
    index2 = WN_Ldid(indv_mtype, 0, indv, ST_type(indv));
   //  fdump_tree(stderr, WN_end(parent->Loop()));
    if(WN_operator(stride2) != OPR_INTCONST) {
      WN_DELETE_Tree(result);
      return NULL;
    } else {
      is0 = WN_const_val(stride1);
      is1 = WN_const_val(stride2);
      if(is1 >= is0 && (is1 % is0 == 0)) {
	if(prev_was_eq && is1 != is0) {
	  WN_DELETE_Tree(result);
	  return NULL;
	}

	span0 =  WN_Binary(OPR_DIV, indv_mtype, WN_COPY_Tree(stride2),
			   WN_COPY_Tree(stride1));
	stride0 =  WN_CreateExp2 (OPCODE_make_op (OPR_MIN,
						  indv_mtype,
						  MTYPE_V),
				  WN_COPY_Tree(span0), 
				  WN_Add(indv_mtype, WN_COPY_Tree(span1), 
					 WN_Intconst(indv_mtype,1))
				  );
        if(is1 != is0 || !prev_was_eq) {
	    term = WN_Mpy(indv_mtype, stride0, WN_COPY_Tree(term));
	    result = WN_Add(indv_mtype, result, 
			    WN_Mpy( indv_mtype, index2, term));
	
	} else {
	  result = WN_Add(indv_mtype, result, index2);
	  prev_was_eq = TRUE;
	}
	
      } else {
	WN_DELETE_Tree(result);
	return NULL;
      }
      
      stride1 = stride2;
      span1 = span2;
      
    }
    
  }

 
  return result;
    
  
}


ST* REMOTE_REF_DESCR::Generate_New_Coeff(WN *bblock, WN *cinit, ST *ldesc, 
					 ST *rdesc, ST* lmad, 
					 int cur_pos, int num_dims, BOOL is_use)
{
  ST *result;
  WN *coeff = NULL;
  WN *body;
  int i;
  ST *vcd;
  WN *term;
  if(num_dims == 1)
    return WN_st(WN_index(ali->Loop()));
  else {
    result = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "vcoeff");
    coeff = Try_Static_Coeff(cur_pos, num_dims, is_use);
    if(!coeff) {
      for(i = 0; i < ind_vars->Elements(); i++) {
	vcd = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "vcd");
	Generate_Get_Coeff(cinit, vcd, ldesc, rdesc, lmad, i);
	term = WN_Mpy(Integer_type, 
		       WN_Ldid(Integer_type, 0, WN_st(ind_vars->Bottom_nth(i)), 
			       MTYPE_To_TY(Integer_type)),
		       WN_Ldid(Integer_type, 0, vcd, ST_type(vcd)));
	if(coeff) 
	 coeff  = WN_Add(Integer_type, coeff, term);
	else 
	  coeff = term;
      }
    }
    coeff = WN_Stid(Integer_type, 0, result, ST_type(result), coeff,0);
   
    body = WN_do_body(ali->Loop());
    WN_INSERT_BlockFirst(body, coeff);
    LWN_Set_Parent(coeff, body);
  }
  
  return result;
}




WN *Find_Comp_Value(WN *comp, WN *arg) 
{
  ST *st = WN_st(arg);
  WN *kid0, *kid1;

  kid0 = WN_kid0(comp);
  kid1 = WN_kid1(comp);
  
  if(WN_operator(kid0) == OPR_LDID && WN_st(kid0) == st)
    return kid1;
  else if(WN_operator(kid1) == OPR_LDID && WN_st(kid1) == st)
    return kid0;
  else FmtAssert(0, ("Comparison node not handled"));
  
  return NULL;

}

void Print_All_Region_Info (REGION *reg) 
{
  fprintf(stderr, "\n START REGION INFO =================\n");
  fprintf(stderr, " DIM = %d, DEPTH = %d, COUPLED = %d \n", reg->Num_Dim(), reg->_depth, 
	  reg->Is_Coupled());
  for (INT i = 0; i < reg->_wn_list.Elements(); ++i) {
    fprintf(stderr, "WNs =====================\n");
    fdump_tree(stderr, reg->_wn_list.Bottom_nth(i));
  
    fprintf(stderr, "=====================\n");
  }
  
  for (INT i = 0; i < reg->Num_Dim(); ++i) {
    fprintf(stderr, "CONDS =====================\n");
    if( reg->_conditions)
      reg->_conditions[i].Print(stderr);
    fprintf(stderr, "=====================\n");
  }
  
  
  fprintf(stderr, "KERNEL  =====================\n");
  if(reg->_kernel)
    reg->_kernel->Get_Kernel()->Print(stderr);
  fprintf(stderr, "=====================\n");
 
  
  
  if(reg->_axle) {
    fprintf(stderr, "AXLE  =====================\n");
    reg->_axle->Print(stderr, 0);
    fprintf(stderr, "=====================\n");
  }
  
  fprintf(stderr, "\n END REGION INFO =================\n");
}


//The stride is
//
//  reg_stride = (reg_ub - reg_lb) * loop_stride
//               -------------------------------    (depends of is_decreasing)
//               (loop_ub - loop_lb)  
//
//The function is called only for the case where all the bounds are constants since this is the
//only one that cannot be solved in Can_Vectorize 
//
static void Find_Region_Stride(ARA_LOOP_INFO *ai, 
			       WN *reg_lb, WN *reg_ub, WN **reg_step, BOOL reg_is_decreasing,
			       WN **loop_lb, WN **loop_ub, WN *loop_step, BOOL loop_is_decreasing) 
{
  
  const WN *loop = ai->Loop();  
  WN *wn, *wn1;
  TYPE_ID mtype = WN_rtype(loop_step);
  TY_IDX ity = MTYPE_To_TY(mtype);
  

  if(*loop_lb == NULL) {
    if( loop_is_decreasing) {
      *loop_lb =  WN_COPY_Tree(Find_Comp_Value(WN_end(loop), WN_index(loop)));
    } else {
      *loop_lb =   WN_COPY_Tree(Store_Expr(WN_start(loop))); 
    }
  }

  if(*loop_ub == NULL) {
    if(loop_is_decreasing) {
      *loop_ub = WN_COPY_Tree(Store_Expr(WN_start(loop)));
    } else {
      *loop_ub =  WN_COPY_Tree(UBexp(WN_end(loop)));
    }
  }

  FmtAssert(reg_lb  && *reg_step && *loop_lb && *loop_ub && loop_step,(""));

  if(WN_operator(reg_lb) == OPR_INTCONST && reg_ub && WN_operator(reg_ub) == OPR_INTCONST &&
     WN_operator(*loop_lb) == OPR_INTCONST && WN_operator(*loop_ub) == OPR_INTCONST) {
    wn = WN_Sub(mtype, WN_COPY_Tree(reg_ub), WN_COPY_Tree(reg_lb));
    wn1 = WN_Sub(mtype, WN_COPY_Tree(*loop_ub), WN_COPY_Tree(*loop_lb));
    wn = WN_Div(mtype, wn, wn1);
    wn = WN_Mpy(mtype, wn, WN_COPY_Tree(loop_step));
    *reg_step = wn;
  } else 
    *reg_step = WN_Mpy(mtype, WN_COPY_Tree(*reg_step), WN_COPY_Tree(loop_step));

}



// Gen_Compute_BB should be called ahead of any other vectorization codegen code
// since it sets correctly the values of the lower and upper bound wns
// The generated code is 
// nelems = upcr_compute_bb(lo, up, step, is_eq);
// and it is inserted before the loop of interest
// The function returns ldid(nelems) to be used by the caller

static WN* Gen_Compute_BB(ARA_LOOP_INFO *ai, DO_LOOP_INFO *li, AXLE_NODE &axle, 
			  WN **loop_lb, WN **loop_ub, WN *reg_lb, WN* reg_ub, WN *bblock, 
			  WN *reg_stride, BOOL neg_stride) 
{
  const WN *loop = ai->Loop();  
  
  WN *block, *call;
  ST *nelems;
  WN *llb, *lub, *ls, *ilb, *is;

  BOOL is_equality = (WN_operator(WN_end(loop)) == OPR_LE || WN_operator(WN_end(loop)) == OPR_GE);

  if(neg_stride) {
    llb =  WN_COPY_Tree(Find_Comp_Value(WN_end(loop), WN_index(loop))); 
    lub = WN_COPY_Tree(Store_Expr(WN_start(loop)));
  } else {
    llb =   WN_COPY_Tree(Store_Expr(WN_start(loop)));
    lub =  WN_COPY_Tree(UBexp(WN_end(loop)));
  }
 
    ls = reg_stride;

  if(*loop_lb == NULL)
    *loop_lb = llb;
  
  if(*loop_ub == NULL)
    *loop_ub = lub;

    if(axle.lo && axle.lo->_ac_v) {
      llb = axle.lo->_ac_v->Get_Base_WN();
      fdump_tree(stderr, *loop_lb);
    }
    if(axle.up && axle.up->_ac_v) {
      lub = axle.up->_ac_v->Get_Base_WN();
      fdump_tree(stderr, *loop_ub);
    }

  block = WN_CreateBlock();
  
  call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_I4, MTYPE_V, 4);
  WN_intrinsic(call) = INTRN_BB;
  WN_kid0(call) = WN_CreateParm(Integer_type, WN_COPY_Tree(llb), 
				MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid1(call) = WN_CreateParm(Integer_type, WN_COPY_Tree(lub), 
				MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  
  WN_kid2(call) = WN_CreateParm(Integer_type, WN_COPY_Tree(ls), 
				MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid3(call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, is_equality ? 1 : 0), 
				MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(block, call);
  call = WN_Ldid(Integer_type, -1, Return_Val_Preg, MTYPE_To_TY(Integer_type));
  call =  WN_CreateComma (OPR_COMMA, Integer_type, MTYPE_V,
				block, call);
  
  nelems =  Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), (char*) "nelems");
  block = WN_Stid(Integer_type, 0, nelems, MTYPE_To_TY(Integer_type), call);
 
  WN_INSERT_BlockBefore(bblock, (WN*) loop, block);
  
  return WN_Ldid(Integer_type, 0, nelems, MTYPE_To_TY(Integer_type)); ;
}

//Add a new induction variable to the loop, to be used when accessing the new 
//vectorization buffer
//The induction var is initialized to be zero right before the loop, 
//and incremented by 1 every iteration
static ST* Add_Ind_Var_to_Loop(WN* block, WN* loop, WN* init_val) {
 
  ST* ind_var = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "ind_var");
  if (init_val == NULL) {
    //incrementing 
    WN_INSERT_BlockBefore(block, loop, WN_Stid(Integer_type, 0, ind_var, ST_type(ind_var),
					       WN_Intconst(Integer_type, 0)));
    
    WN_INSERT_BlockLast(WN_do_body(loop),
			WN_Stid(Integer_type, 0, ind_var, ST_type(ind_var),
				WN_Add(Integer_type, WN_Intconst(Integer_type, 1),
				       WN_Ldid(Integer_type, 0, ind_var, ST_type(ind_var)))));
  } else {
    //decrementing
    WN_INSERT_BlockBefore(block, loop, WN_Stid(Integer_type, 0, ind_var, ST_type(ind_var), init_val));
    
    WN_INSERT_BlockLast(WN_do_body(loop),
			WN_Stid(Integer_type, 0, ind_var, ST_type(ind_var),
				WN_Sub(Integer_type, WN_Intconst(Integer_type, 1),
				       WN_Ldid(Integer_type, 0, ind_var, ST_type(ind_var)))));
  }
    
  return ind_var;
}

/* For a shared ptr sold that's been vectorized, replace all of its read
   accesses.  new_index is the ST of the new induction variable that should be used
   for the array access.

*/
void Replace_Shared_Access(WN* wn, ST* sold, ST* snew, ST* old_idx, ST* new_index) {

  FmtAssert(Type_Is_Shared_Ptr(ST_type(sold)), ("Expecting shared pointer symbols"));
  
  switch (WN_operator(wn)) {
  case OPR_BLOCK:
    for (WN* body_wn = WN_first(wn); body_wn != NULL; body_wn = WN_next(body_wn)) {
      Replace_Shared_Access(body_wn, sold, snew, old_idx, new_index);
    }
    return;
  case OPR_ILOAD:

    if (Is_Shared_Array(WN_kid0(wn), sold)) {
     
      WN* ar_base = WN_array_base(WN_kid0(wn));
      WN* parent = WN_kid0(wn);
      while (WN_operator(ar_base) == OPR_ARRAY) {
	parent = ar_base;
	ar_base = WN_array_base(ar_base);
      }
      WN* new_base = WN_Ldid(TY_mtype(ST_type(snew)), WN_offset(ar_base), snew, 
			     ST_type(snew), 0);
      FmtAssert(WN_operator(parent) == OPR_ARRAY && WN_array_base(parent) == ar_base, ("Something wrong here")); 

      if (TY_is_shared(WN_ty(wn))) {
	WN_set_ty(wn, Shared_To_Private_Type(WN_ty(wn)));
      } 
      if (Type_Is_Shared_Ptr(WN_load_addr_ty(wn), true)) {
	TY_IDX idx;
	idx = WN_load_addr_ty(wn);
	switch(TY_kind(idx)) {
	case KIND_POINTER:
	  WN_set_load_addr_ty(wn, Make_Pointer_Type(Shared_To_Private_Type(TY_pointed(idx))));
	  break;
	case KIND_ARRAY:
	  Fail_FmtAssertion("Unimplemented feature replace ARRAY type");
	  //WN_set_load_addr_ty(wn, Make_Pointer_Type(WN_ty(wn)));
	  break;
	}
	

      }
     
      Replace_Symbol(WN_kid2(WN_kid0(wn)), SYMBOL(old_idx,0, TY_mtype(ST_type(old_idx))),
		     SYMBOL(new_index, 0,TY_mtype(ST_type(old_idx))), 
		     WN_Ldid(Integer_type, 0, new_index, ST_type(new_index)),
		     WN_kid2(WN_kid0(wn)));
      WN* new_array = WN_Ternary(OPR_ARRAY, Pointer_Mtype, new_base, WN_kid1(WN_kid0(wn)),
				 WN_kid2(WN_kid0(wn)));
      WN_element_size(new_array) = TY_size(WN_ty(wn));
      WN_kid0(wn) = new_array;
     
      return;
    }
    break;
  case OPR_ISTORE:
    //vectorizing writes
    Replace_Shared_Access(WN_kid0(wn), sold, snew, old_idx, new_index);
    if (Is_Shared_Array(WN_kid1(wn), sold)) {
      WN* ar_base = WN_array_base(WN_kid1(wn));
      WN* parent = WN_kid1(wn);
      while (WN_operator(ar_base) == OPR_ARRAY) {
	parent = ar_base;
	ar_base = WN_array_base(ar_base);
      }
      WN* new_base = WN_Ldid(TY_mtype(ST_type(snew)), WN_offset(ar_base), snew, 
			     ST_type(snew), 0);
      WN_array_base(parent) = new_base;
      if (Type_Is_Shared_Ptr(WN_ty(wn), true)) {
	WN_set_ty(wn, Make_Pointer_Type(Shared_To_Private_Type(TY_pointed(WN_ty(wn)))));
      } 

      return;
    }
    break;
  case OPR_ARRAY: {
    WN *parent;
    WN_array_base(wn) = WN_Ldid(TY_mtype(ST_type(snew)),0, snew, ST_type(snew));
    WN_array_index(wn,0) = WN_Ldid(TY_mtype(ST_type(new_index)), 0, new_index, ST_type(new_index));
    parent = LWN_Get_Parent(wn);
    if(WN_operator(parent) == OPR_ILOAD  ) {
      if (WN_field_id(parent) != 0) {
	WN_set_ty(parent, Get_Field_Type(TY_pointed(ST_type(snew)), WN_field_id(parent)));
      } else {
	WN_set_ty(parent, TY_pointed(ST_type(snew)));
      }
      WN_set_load_addr_ty(parent, ST_type(snew));
    } else  
      if (WN_operator(parent) == OPR_ISTORE) {
	WN_set_ty(parent, ST_type(snew));
      } else
	Fail_FmtAssertion("Unexpected parent for array node\n");
    break;
  }
  }
  
  for (int i =0; i < WN_kid_count(wn); i++) {
    Replace_Shared_Access(WN_kid(wn, i), sold, snew, old_idx, new_index);
  }
}





static WN* Get_Free(ST* st) {

  FmtAssert(TY_kind(ST_type(st)) == KIND_POINTER, ("Attempt to free a non-pointer variable"));
  WN* free_wn = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 1);
  WN_intrinsic(free_wn) = INTRN_VECT_FREE;
  WN_kid0(free_wn) = WN_CreateParm(Pointer_Mtype, WN_Ldid(Pointer_Mtype, 0, st, ST_type(st), 0),
				   ST_type(st), WN_PARM_BY_VALUE);
  return free_wn;
}

/*
  Get the allocation call that creates the private array
  If size is intconst, a stack array is returned
  Otherwise, alloca (should malloc be used instead?) is used
*/
static WN* Get_Alloc(WN* size, TY_IDX ptr_ty, char* var_name) {

  WN* alloc_call;

  //generate the alloc call
  WN* tmp_block = WN_CreateBlock();
  WN* malloc_wn = WN_Create(OPR_INTRINSIC_CALL, Pointer_Mtype, MTYPE_V, 1);
  WN_intrinsic(malloc_wn) = INTRN_VECT_ALLOC;
  WN_kid0(malloc_wn) = WN_CreateParm(TY_mtype(WN_rtype(size)), WN_COPY_Tree(size),
				     MTYPE_To_TY(WN_rtype(size)), WN_PARM_BY_VALUE);
  WN_INSERT_BlockLast(tmp_block, malloc_wn);
  alloc_call = WN_Ldid(Pointer_Mtype, -1, Return_Val_Preg, ptr_ty);
  alloc_call  = WN_CreateComma (OPR_COMMA, Pointer_Mtype, MTYPE_V,
				tmp_block, alloc_call);
  return alloc_call;
}



//get the starting address of the shared array, when performing the memget
static WN* Get_Start_Addr (ST* st, TY_IDX idx, WN *nelems,  
			   WN *loop_lb, WN* loop_ub, WN* loop_stride, BOOL loop_neg_stride,
			   WN* reg_lo, WN *reg_up = 0, WN *reg_stride = 0,  BOOL reg_neg_stride = FALSE, 
			   BOOL is_eq = FALSE) 
{
  WN* tmp;
  WN *btu;
  TYPE_ID mtype = WN_rtype(reg_stride);

  if (reg_neg_stride == loop_neg_stride) {
    if(reg_neg_stride ) {
      //the displacement is 
      // (reg_up - reg_lo) - (reg_up+idx_coeff*loop_up) == -(reg_lo + idx_coeff*loop_up)
      // loop_up is negative for non_const_loops and positive otherwise,
      //idx_coeff = reg_stride/loop_stride
      if(WN_operator(reg_stride) == OPR_INTCONST && WN_const_val(reg_stride) != 1) {
	btu = WN_Div(mtype,  WN_COPY_Tree(reg_stride), WN_COPY_Tree(loop_stride));
	btu = WN_Mpy(mtype,  WN_COPY_Tree(loop_ub), btu);
      } else
	btu = WN_COPY_Tree(loop_ub);
      
      if(WN_operator(loop_ub) == OPR_INTCONST)
	btu = WN_Sub(mtype, WN_COPY_Tree(reg_lo), btu);
      else 
	btu = WN_Add(mtype, WN_COPY_Tree(reg_lo), btu); 
      //bug 1307 - delete this code when bug fixed
      fprintf(stderr,"SPILL IN LNO FOR bug1307 WORKAROUND \n");
      ST *st =  Gen_Temp_Symbol(MTYPE_To_TY(mtype), (char*) "bug1307");
      WN *st1 = WN_Stid(mtype, 0, st, MTYPE_To_TY(WN_rtype(btu)), btu);
      WN *st3 = WN_CreateBlock();
      WN_INSERT_BlockLast(st3, st1);
      st1 = st3;
      WN *st2 = WN_Ldid(mtype, 0, st, MTYPE_To_TY(mtype));
      btu =  WN_CreateComma (OPR_COMMA, mtype, MTYPE_V, st1, st2);
      //end delete me
    } else 
      btu = WN_COPY_Tree(reg_lo);
  } else {
    if(loop_neg_stride) {
      //the  displacement is something like this
      // if (loop_lb == intconst) 
      //    trip_cnt = (loop_up  + loop_lb)/loop_stride
      //else 
      //    trip_cnt = (loop_up - loop_lb)/loop_stride
      //
      // init_disp = trip_cnt*reg_stride + const_contrib_for_init_val
      // where const_contrib for something like [j+k+2] : j--
      // is k+2 - which can be obtained from the axle - NOT IMPLEMENTED YET
    }
    FmtAssert(0, (" Decreasing loop index, increasing  array index not implemented yet"));
    btu = WN_Intconst(mtype, 0);
  }
  

  WN *addr = (TY_kind(ST_type(st)) == KIND_ARRAY) ? WN_Lda(Pointer_Mtype, 0, st, 0) :
    WN_Ldid(Pointer_Mtype, 0, st, idx);
  
  // start = base_addr + disp  
  tmp = WN_Add(mtype, addr, btu); 
  addr = LWN_Get_Tas(tmp, idx);
  
  return addr;
}

/*
 *  Vector code generation for 1D indefinite array with non-unit stride
 *  The memget_fstrided call is used to perform the communication.
 *
 *  Required info from the analysis:  lower bound, upper bound, stride of the indefinite array
 *
 */
static void IndefBSize_Stride_Code_Gen(ARA_LOOP_INFO* ai, ARA_REF *reg, 
				       AXLE_NODE &axle,  BOOL is_write,
				       WN* reg_lb, WN* reg_ub, WN* reg_step, BOOL reg_is_decr,  
				       WN* loop_lb, WN* loop_ub, WN *loop_step, BOOL loop_is_decr)
{

  const SYMBOL &arr = reg->Array();
  ST * a_st = arr.St();
  //create new induction variable for accessing the private array...
  TY_IDX a_ty = ST_type(a_st);
  INT esize = Get_Type_Inner_Size(a_ty);
  const DO_LOOP_INFO *li = ai->Info();
  WN *loop = (WN*)ai->Loop();
  WN *bbox, *bblock, *stride;
  WN *src_addr, *src_num_elt;
  ST *nelems;

  

  if(is_write)
  FmtAssert(is_write, ("Strided memput not implemented yet"));
  if (TY_kind(a_ty) == KIND_ARRAY) {
    //for shared array, convert it to the equivalent pointer type
    for (; TY_kind(a_ty) == KIND_ARRAY; a_ty = TY_etype(a_ty));
    a_ty = Make_Pointer_Type(a_ty);
  }  
  FmtAssert(Type_Is_Shared_Ptr(a_ty, true), ("Error promoting array type to ptr type"));
  TY_IDX local_ptr_ty = Make_Pointer_Type(Shared_To_Private_Type(TY_pointed(a_ty)));
  ST *h_tmp =  Gen_Temp_Symbol(local_ptr_ty, (char*) ".vectmp");  

  WN* block = (WN*) LWN_Get_Parent(loop);
  //create and insert the compute_bb
  src_num_elt = Gen_Compute_BB(ai, (DO_LOOP_INFO*)li, axle, 
			       &loop_lb, &loop_ub, reg_lb, reg_ub,  block, reg_step, 
			       loop_is_decr);
  src_addr = Get_Start_Addr(a_st, a_ty, src_num_elt, 
			    loop_lb, loop_ub, loop_step, loop_is_decr,
			    reg_lb, reg_ub, reg_step, reg_is_decr);
  
  src_num_elt = WN_Add(Integer_type, src_num_elt, WN_Intconst(Integer_type, 1));

  // need to make sure that I adjust the step here  to be positive - TO DO
  stride = WN_COPY_Tree(reg_step);
  WN* src_chunklen = WN_Intconst(Integer_type, esize);
  WN* src_chunkstride = WN_Mpy(Integer_type, stride,
			       WN_Intconst(Integer_type, esize));
  if(loop_is_decr || reg_is_decr) 
    src_chunkstride = WN_Neg(Integer_type, src_chunkstride);
  WN* dst_chunklen =  WN_Mpy(Integer_type, WN_COPY_Tree(src_num_elt), WN_Intconst(Integer_type, esize));
  WN* alloc_call = Get_Alloc(dst_chunklen, local_ptr_ty, ".vectmp");
  alloc_call = WN_Stid (Pointer_Mtype, 0, h_tmp, local_ptr_ty, alloc_call);

  //generate the stride call
  WN *fstride_call; 
  fstride_call = WN_Create (OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 8);
  WN_intrinsic(fstride_call) = INTRN_MEMGET_STRIDE;
  WN_kid0(fstride_call) = WN_CreateParm(Pointer_Mtype, WN_Ldid(Pointer_Mtype, 0, h_tmp, local_ptr_ty),
					local_ptr_ty, WN_PARM_BY_VALUE);
  WN_kid1(fstride_call) = WN_CreateParm(Integer_type, dst_chunklen, MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid2(fstride_call) = WN_CreateParm(Integer_type, WN_COPY_Tree(dst_chunklen), MTYPE_To_TY(Integer_type),
					WN_PARM_BY_VALUE);
  WN_kid3(fstride_call) = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, 1), MTYPE_To_TY(Integer_type),
					WN_PARM_BY_VALUE);
  WN_kid(fstride_call, 4) = WN_CreateParm(Pointer_Mtype, src_addr, a_ty, WN_PARM_BY_VALUE);
  WN_kid(fstride_call, 5) = WN_CreateParm(Integer_type, src_chunklen, MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid(fstride_call, 6) = WN_CreateParm(Integer_type, src_chunkstride, MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN_kid(fstride_call, 7) = WN_CreateParm(Integer_type, src_num_elt, MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);

  {
    //insert the calls...
    WN* block = (WN*) LWN_Get_Parent(loop);

    WN_INSERT_BlockBefore(block, (WN*) loop, alloc_call);
    WN_INSERT_BlockBefore(block, (WN*) loop, fstride_call);
    WN_INSERT_BlockAfter(block, (WN*) loop, Get_Free(h_tmp));
    ST* ind_var = Add_Ind_Var_to_Loop(block, (WN*) loop, NULL);
    Replace_Shared_Access(WN_do_body(loop), a_st, h_tmp, WN_st(WN_index(loop)), ind_var);
    
  }

}


/*
  Given a 1d array's lower and upper bound in a loop, generate the vectorized code.
  The array symbol to be vectorized can either be a shared pointer or a shared array.
  for vectorizing reads, the C code looks like the following:


  size = (up - lo + 1 ) * esize;
  DATA* vec_tmp = malloc(size);
  upcr_memget(vec_tmp, ar, size);
  the original loop, with array accesses to ar replaced by accesses to vec_tmp;
  free (vec_tmp);

  If size is a compile-time constant, we use a stack temporary array instead of malloc
  for the private array.

  If lo is non-zero, ar is adjusted to be ar + lo, and references to vec_tmp in the array
  are adjusted by -lo.

  For vectorizing writes, an additional memput is inserted before the free call:
  upcr_memput(ar, vec_tmp, size);

 
*/

static void IndefBSize_Vect_Code_Gen(ARA_LOOP_INFO *ai, ARA_REF *reg, 
				     AXLE_NODE &axle, bool is_write,
				     WN *reg_step, BOOL reg_is_decr, 
				     WN* loop_step, BOOL loop_is_decr) 
{

  const SYMBOL &arr = reg->Array();
  ST * a_st = arr.St();
  TY_IDX a_ty = ST_type(a_st);
  INT esize = Get_Type_Inner_Size(a_ty);
  const DO_LOOP_INFO *li = ai->Info();
  WN *loop = (WN*)ai->Loop();
  WN *bbox, *bblock;
  WN *ld_alias;
  WN *reg_lb, *reg_ub;
  WN *loop_lb, *loop_ub;

  if(!loop_step)
    loop_step = li->Step->Get_Base_WN();
  else 
    loop_step = WN_COPY_Tree(loop_step);
  

  loop_lb = li->LB->Dim(0)->Get_Base_WN();
  if(li->LB->Dim(0)->Too_Messy) {
    li->LB->Dim(0)->Print(stderr, 1);
    return;
  }
  
  if(li->UB && li->UB->Dim(0)) {
    if(li->UB->Dim(0)->Too_Messy) {
      fprintf(stderr, "MESSY UB in Vectorize \n");
      return;
    }
    loop_ub = li->UB->Dim(0)->Get_Base_WN();
  }
  //Upper_Bound_Standardize(WN_end(loop), TRUE);
  
  
  reg_lb  = axle.lo->Access_Vector()->Get_Base_WN();
  if(axle.lo->Access_Vector()->Has_Loop_Coeff() && 
     !axle.lo->Access_Vector()->Has_Only_Zero_Coeff()) {
    FmtAssert(0,("Axle with loop coefficient not patched")); 
  }

  reg_ub = NULL;
  if(axle.up) { 
    FmtAssert(!axle.up->Access_Vector()->Has_Loop_Coeff() ||  
	      axle.lo->Access_Vector()->Has_Only_Zero_Coeff(),
	      ("Axle with loop coefficient not patched"));
    reg_ub = axle.up->Access_Vector()->Get_Base_WN();

  }

  //At this point we need to compute the region stride based on the already computed
  //bounds.

  Find_Region_Stride(ai,
		     reg_lb, reg_ub, &reg_step, reg_is_decr, 
		     &loop_lb, &loop_ub, loop_step, loop_is_decr);
  
  if(
     (WN_operator(loop_step) != OPR_INTCONST || 
     (WN_operator(loop_step) == OPR_INTCONST && MOD(WN_const_val(loop_step)) != 1)) ||
     (WN_operator(reg_step) != OPR_INTCONST || 
     (WN_operator(reg_step) == OPR_INTCONST && MOD(WN_const_val(reg_step)) != 1))
     ) {
    IndefBSize_Stride_Code_Gen(ai, reg, axle, is_write, 
			       reg_lb, reg_ub, reg_step, reg_is_decr, 
			       loop_lb, loop_ub, loop_step, loop_is_decr);
    return;
  }

  if (TY_kind(a_ty) == KIND_ARRAY) {
    //for shared array, convert it to the equivalent pointer type
    for (; TY_kind(a_ty) == KIND_ARRAY; a_ty = TY_etype(a_ty));
    a_ty = Make_Pointer_Type(a_ty);
  }
  FmtAssert(Type_Is_Shared_Ptr(a_ty, true), ("Error promoting array type to ptr type"));
  
 


  TY_IDX local_ptr_ty = Make_Pointer_Type(Shared_To_Private_Type(TY_pointed(a_ty)));
  ST *h_tmp =  Gen_Temp_Symbol(local_ptr_ty, (char*) ".vectmp");
  ST *nelems;
  //generate the memget_call
  WN *call_wn; 
  call_wn = WN_Create (OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 3);
  WN_intrinsic(call_wn) = INTRN_UPC_MEMGET;
  WN *ld_shadow = WN_Ldid(TY_mtype(local_ptr_ty), 0, h_tmp, local_ptr_ty);
  
  WN* block = (WN*) LWN_Get_Parent(loop);
  //create and insert the compute_bb
  bbox = Gen_Compute_BB(ai, (DO_LOOP_INFO*)li, axle, 
			&loop_lb, &loop_ub,  reg_lb, reg_ub, block, 
			loop_step, loop_is_decr);
  ld_alias = Get_Start_Addr(a_st, a_ty, bbox,
			    loop_lb, loop_ub, loop_step, loop_is_decr,
			    reg_lb, reg_ub, reg_step, reg_is_decr);
  WN *size =  WN_Mpy(Integer_type,
		     WN_Intconst(Integer_type, esize),
		     WN_Add(Integer_type, bbox, WN_Intconst(Integer_type, 1)));

  WN_kid0(call_wn) = WN_CreateParm(TY_mtype(local_ptr_ty),
				   ld_shadow,
				   local_ptr_ty, WN_PARM_BY_VALUE);
  WN* arg1;
  TY_IDX arg1_ty = a_ty;
  //Need an tas to shared void *
  arg1 = WN_Create(OPR_TAS, Pointer_Mtype, MTYPE_V, 1);
  WN_kid0(arg1) = ld_alias;
  arg1_ty = Make_Pointer_Type(Make_Shared_Type(MTYPE_To_TY(MTYPE_V), 1, RELAXED_CONSISTENCY));
  WN_set_ty(arg1, arg1_ty);

  WN_kid1(call_wn) = WN_CreateParm(Pointer_Mtype, 
				   arg1,
				   arg1_ty, WN_PARM_BY_VALUE);
  //size (in bytes) is (up - lo + 1) * esize
  WN_kid2(call_wn) = WN_CreateParm(Integer_type,
				   size,
				   MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);

  

  WN* alloc_call = Get_Alloc(size, local_ptr_ty, "stack_temp");
  WN* alloc_stmt = WN_Stid (Pointer_Mtype, 0, h_tmp, local_ptr_ty, alloc_call);
 
  WN* local_init = WN_CreateBlock();
  WN_INSERT_BlockLast(local_init, alloc_stmt);
  WN_INSERT_BlockLast(local_init, call_wn);
  
  WN_INSERT_BlockBefore(block, (WN*) loop, local_init);

  //free call is the last x
  WN_INSERT_BlockAfter(block, (WN*) loop, Get_Free(h_tmp));
  ST* ind_var = Add_Ind_Var_to_Loop(block, (WN*) loop, NULL);
  Replace_Shared_Access(WN_do_body(loop), a_st, h_tmp, WN_st(WN_index(loop)), ind_var);


  if (is_write) {
    //generate a memput call to write back the temp array
    WN* put_call = WN_Create (OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 3);
    WN_intrinsic(put_call) = INTRN_UPC_MEMPUT;
    WN_kid0(put_call) = WN_COPY_Tree(WN_kid1(call_wn));
    WN_kid1(put_call) = WN_COPY_Tree(WN_kid0(call_wn));
    WN_kid2(put_call) = WN_COPY_Tree(WN_kid2(call_wn));
    WN_INSERT_BlockAfter(block, (WN*) loop, put_call);
  }
}


WN* Find_Coeff_in_Index_Expr(WN *idx, SYMBOL *ind_var, WN *loop_idx) 
{
  int i;
  WN *result;
  WN *kid;
  if(WN_operator(idx) == OPR_MPY) {
    kid = WN_kid0(idx);
    if(WN_operator(kid) == OPR_LDID && WN_st(kid) == ind_var->ST_Base() && WN_offset(kid) == WN_offset(loop_idx))
      return WN_kid1(idx);
  }
  
  for(i = 0; i < WN_kid_count(idx); i++) {
    result = Find_Coeff_in_Index_Expr(WN_kid(idx,i), ind_var, loop_idx);
    if(result)
      return result;
  }
  return NULL;
}


WN *Find_Loop_Coeff(REGION *reg, SYMBOL *ind_var, WN *loop_idx) 
{
  WN *result;
  
  for(INT i = 0; i < reg->_wn_list.Elements(); i++) {
    result = Find_Coeff_in_Index_Expr(reg->_wn_list.Bottom_nth(i), ind_var, loop_idx);
    if(result)
      return result;
  } 
  
  return NULL;
  
}


//returns true for the cases the calling vectorization code
//knows how to handle.
//Can_Vectorize_on_Ax just passes up this  return value
BOOL  Analyze_Ind_Var_Update(WN *def, BOOL &is_decreasing, ST *var = NULL, WN**step = NULL) 
{
  is_decreasing = FALSE;
  WN *rhs;
  WN *kid0, *kid1;
  BOOL result = FALSE;

  BOOL lid;

  if(def && WN_operator(def) == OPR_STID) {
    rhs = WN_kid0(def);
    var = WN_st(def);
  } else {
    if(def)
      rhs = def;
    else 
      rhs = *step;
  }
  
  FmtAssert((var || rhs) && step , ("Analysis should set ST and step "));
  
  switch(WN_operator(rhs)) {
    //we get add for either var + var or var +/- ct
  case  OPR_ADD:
    kid0 = WN_kid0(rhs);
    kid1 = WN_kid1(rhs);
    if(WN_operator(kid0) == OPR_LDID && WN_st(kid0) == var) {
      if(WN_operator(kid1) == OPR_INTCONST && WN_const_val(kid1) < 0) 
	is_decreasing = TRUE;
      *step = kid1;
      result = TRUE;
    } else if(WN_operator(kid1) == OPR_LDID && WN_st(kid1) == var) {
      if(WN_operator(kid0) == OPR_INTCONST && WN_const_val(kid0) < 0)
	is_decreasing = TRUE;
      *step = kid0;
      result = TRUE;
    } else {
      //result = Analyze_Ind_Var_Update(kid0, is_decreasing, var);
      //result |= Analyze_Ind_Var_Update(kid1, lid, var);
      //for the time being make it unvectorizable
      result = FALSE;
      is_decreasing = is_decreasing | lid; //is this right?
    } 
      
    break;
    //sub is only for var - var
  case OPR_SUB:
    if(WN_operator(kid0) == OPR_LDID && WN_st(kid0) == var) {
      result = TRUE;
      *step = kid1;
    } else if(WN_operator(kid1) == OPR_LDID && WN_st(kid1) == var) {
      //make it unvectorizable
      result = FALSE;
    } else {
      //result = Analyze_Ind_Var_Update(kid0, is_decreasing, var);
      //result |= Analyze_Ind_Var_Update(kid1, lid, var);
      //for the time being make it unvectorizable
      result = FALSE;
      is_decreasing = is_decreasing | lid; //is this right?
    } 
    break;

    
  case OPR_INTCONST:
    is_decreasing = (WN_const_val(rhs) < 0);
    result = TRUE;
    break;

  default:
    result = FALSE;
    break;
  }
  
  return result;
  
}


BOOL Can_Vectorize_On_Messy_Axle(ARA_LOOP_INFO *ai, REGION *reg, AXLE_NODE *a, WN **reg_step, int depth, 
				 int nest_depth, STACK<WN* > *ind_vars)
{
  WN *loop = (WN*)ai->Loop();
  SYMBOL ind_var(ind_vars->Bottom_nth(/*nest_depth - */depth));
  ACCESS_ARRAY *acc;
  ACCESS_VECTOR *dim;

  WN *temp;
  
  if(reg->Num_Dim() > 1) {
    if(trace_msg_vect)
      Print_Vec_Mesg(WN_Get_Linenum(loop), "Proper multi-dimensional array not implemented yet");
    return FALSE;
  }
  acc = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,reg->_wn_list.Bottom_nth(0));
  Is_True(acc, (""));

  Is_True(acc->Num_Vec() == 1 , ("Proper multi-dimensional arrays not implemented yet"));

  dim = acc->Dim(0);
  
  if(dim->Has_Loop_Coeff()) {
    if(dim->Loop_Coeff(// nest_depth -
		       depth))
      *reg_step = WN_Intconst(Integer_type, dim->Loop_Coeff(// nest_depth -
							    depth));
  }
  
  if (*reg_step == NULL)
    *reg_step = Find_Loop_Coeff(reg, &ind_var, WN_index(loop));
  
  //now we need to look for the occurrence of the same index var in the 
  //linear terms 
  
  if(dim->Lin_Symb != NULL && !dim->Lin_Symb->Is_Empty()) 
    {
      INTSYMB_CONST_ITER iter(dim->Lin_Symb);
      const INTSYMB_NODE* first = iter.First();
      
      for (const INTSYMB_NODE *node=first; !iter.Is_Empty(); node = iter.Next()) { 
	if(*(SYMBOL*)&node->Symbol == ind_var) {
	  fprintf(stderr, "FOUND ONE %d \n", depth);
	}
      }
    }

  if(dim->Non_Lin_Symb &&  !dim->Non_Lin_Symb->Is_Empty()) {
    SUMPROD_CONST_ITER iter(dim->Non_Lin_Symb);
    const SUMPROD_NODE *first = iter.First();
    
    for(const SUMPROD_NODE* node = first; !iter.Is_Empty(); node = iter.Next()) {
      
      if(node->Prod_List->Contains(&ind_var)) {
	temp = WN_Intconst(Integer_type, node->Coeff);
	SYMBOL_CONST_ITER term_iter(node->Prod_List);
	SYMBOL_NODE *ft = (SYMBOL_NODE*) term_iter.First();
	for(SYMBOL_NODE* term = ft; !term_iter.Is_Empty(); term = (SYMBOL_NODE*)term_iter.Next()) {
	  //over here should check if the symbol varies in the loop
	  if(term->Is_Loop_Var)
	    if(term->Symbol != ind_var) {
	      if(trace_msg_vect)
		Print_Vec_Mesg(WN_Get_Linenum(loop), "Coefficient is not loop constant");
	      return FALSE;
	    }
	  TY_IDX  tidx = ST_type(term->Symbol.St());
	  if(term->Symbol.St() != ind_var.St())
	    temp = WN_Mpy(Integer_type, temp, WN_Ldid(TY_mtype(tidx), 0, term->Symbol.St(), tidx));
	 
	} 
	if(*reg_step)
	  *reg_step = WN_Add(Integer_type, *reg_step,  temp);
	else 
	  *reg_step = temp;
      }
    }
  }
  
  // if(*reg_step)
//     fdump_tree(stderr, *reg_step);
  return TRUE;
}

// Return true if the region induced by this axle can be vectorized
// Conditions:

//   * Any vbles appearing in the index expression are either loop invariants
//     or proper induction vbles (loop induction or assigned only once var +/-= step)
//
//  A region is classified as Too_Messy in the following circumstances
//
//  1. Non linear expressions for the loop bounds, e.g. var*var. 
//     We can extend the test later for this case. Easy enough for the 
//     developers to replace it with Bound = var*var or have a pass that replaces
//     automatically these expressions. Very low priority.
//
//  2. Loop step is not a constant. For the time being we do not vectorize this case since
//     the innermost loop is likely to have a constant step. Need to poll application
//     developers to see if there is any case where they might need variable steps in the
//     innermost loop. Also, for loop nests, the outermost loops is likely to have the step
//     expressed as a variable (i+= var). For this case, message strip mining is the preferred
//     optimization, so we again care only about the innermost loop. The only exception is when
//     strip mining does not pay off (~ < 2K  total volume accross the nest) and we need to
//     vectorize the whole nest together. Doable to extend for this case, but we need to make sure the
//     region described by the nest is contiguous. Will do only if I have a very motivating example. 
//  3. Messy array access, aka var*index or index + var*var. var*var - might as well be hoisted
//     outside by the user or by the a pass of loop invariant code  motion.
//     Var*index is covered by Can_Vectorize_Messy. 
//
//  4. Non-similar access vectors, i.e. a[i+1] and a[c*i+d] appear in the loop

extern STACK<WN*>* Scalar_Defs(SYMBOL* sym,
                               WN* wn_loop);

BOOL Can_Vectorize_on_Axle(ARA_LOOP_INFO *ai, REGION *reg, AXLE_NODE *a, 
			   BOOL &loop_is_decreasing, BOOL& reg_is_decreasing, 
			   WN**reg_step, WN **loop_step, int depth, int nest_depth, STACK<WN*> *ind_vars)
{
  WN *loop = (WN*)ai->Loop();
  int i;
  BOOL result = FALSE, one_var = FALSE;
  BOOL decr = FALSE;
  BOOL first_var = TRUE;
  BOOL only_loop_invariants = TRUE;
  SYMBOL ind_var(WN_index(loop));
  BOOL saw_ind_var = FALSE;
  
  

  // fprintf(stderr, "\n+++++++++++++++++++\n");
  //   reg->Print(stderr);
  //   fprintf(stderr, "\n+++++++++++++++++++\n");
  // Print_All_Region_Info(reg);
  
  if(reg->Is_Too_Messy()) {
    return Can_Vectorize_On_Messy_Axle(ai, reg, a, reg_step, depth, nest_depth, ind_vars);
  }
  
  
  if(a->lo == NULL)
    Fail_FmtAssertion("Null axle\n");
  
  Is_True(reg->Num_Dim() == 1, (""));

  if(a->lo->_ac_v->Has_Loop_Coeff()) {
    if(a->lo->_ac_v->Loop_Coeff(depth))
      *reg_step = WN_Intconst(Integer_type, a->lo->_ac_v->Loop_Coeff(depth));
  }
  if (*reg_step == NULL)
    *reg_step = Find_Loop_Coeff(reg, &ind_var, WN_index(loop));

  //For each symbol participating in the axle, check for loop invariance or
  //"induction behavior"
  if(a->lo && a->lo->_ac_v && a->lo->_ac_v->Lin_Symb != NULL && 
     !a->lo->_ac_v->Lin_Symb->Is_Empty()) 
    {
      INTSYMB_CONST_ITER iter(a->lo->_ac_v->Lin_Symb);
      const INTSYMB_NODE* first = iter.First();
      
      for (const INTSYMB_NODE *node=first; !iter.Is_Empty(); node = iter.Next()) { 
	STACK<WN*> *Defs = Scalar_Defs((SYMBOL*)&node->Symbol, loop);
	
	//ignore everything that's assigned to more than once
	if(Defs->Elements() > 1) { //multiple definitions
	  return FALSE;
	} else {
	  //get the statement and see what it is.
	  for(i=0; i < Defs->Elements(); i++) {
	    only_loop_invariants = FALSE;
	    WN *def = Defs->Bottom_nth(i);
	    if(WN_operator(def) != OPR_STID)
	      return FALSE;
	    //for sure stid
	    //make sure that a[i+k] is not vectorizable for the time being
	    //where i,k vary in the same loop body
	    one_var = Analyze_Ind_Var_Update(def, decr, NULL, reg_step);
	    if(ind_var.ST_Base() == WN_st(def) && (WN_offset(WN_index(loop)) == WN_offset(def))) {
	      //it's not enough to test for the equality of ST's since PREG's have
	      //the same ST entry but different offsets
	      loop_is_decreasing = decr;
	      saw_ind_var = TRUE;
	    }
	    if(first_var) {
	      result = one_var;
	      reg_is_decreasing = decr;
	      first_var = FALSE;
	    } else {
	      if(reg_is_decreasing != decr || !result || !one_var) 
		return FALSE;
	    }   
	  }
	}
      }
      //do not vectorize a[i+k] where  k is not loop constant 
      //we can extend the test to accept a[i+k] where both vary in the
      //same direction, e.g. (increasing,increasing)
      //For the time being let the user rewrite the loop
      for(i=0; i < a->lo->_ac_v->Nest_Depth(); i++) {
	if( a->lo->_ac_v->Loop_Coeff(i) != 0) {
	  //is_induction = TRUE;
	  if (only_loop_invariants)
	    goto  loop_invar;
	} else {
	  //for constant bounds we get the coefficient for the loop
	  //induction variable = 0 and the region is described 
	  // as( [k+lbct] :  [k+ubct] : step)
	  //use the step to tell in this case
	  if(a->step > 0) {
	    //is_induction = FALSE;
	    if(*reg_step == NULL)
	      *reg_step = WN_Intconst(MTYPE_I4, 1); 
	    if (only_loop_invariants)
	      goto  loop_invar1;
	  }
	} 
      }
      if(!saw_ind_var) {
	WN *tmp = ai->Info()->Step->Get_Base_WN();
	if((WN_operator(tmp) == OPR_INTCONST && WN_const_val(tmp) < 0) ||
	   WN_operator(tmp) == OPR_NEG)
	  loop_is_decreasing = TRUE;
      } 
      if(reg_is_decreasing == loop_is_decreasing)
	return result;
      else 
	return FALSE;
    }

loop_invar:
  
 loop_invar1:
  //by now we have to deal only with the induction variable
  *loop_step = ai->Info()->Step->Get_Base_WN();
  //Why default to 1? TO DO: 
   if(!*reg_step)
     *reg_step = WN_Intconst(WN_rtype(*loop_step), 0);
  
  result =  Analyze_Ind_Var_Update(0, loop_is_decreasing, 0, loop_step);
  reg_is_decreasing = loop_is_decreasing;
  
  return result;  
}


BOOL Stmt_Is_Redist_Assign(WN *stmt) 
{

  //Note : in this function we need to check the types of the
  // operands in order to make sure that a upc_memcpy is not erroneously
  //classified as a redist. 
  Is_True(WN_operator(stmt) == OPR_ISTORE,(""));
  //also structs confuse the hell out of redistribution, don't mark them
  return (WN_operator(WN_kid0(stmt)) == OPR_ILOAD && 
	  TY_kind(TY_pointed(WN_load_addr_ty(WN_kid0(stmt)))) != KIND_STRUCT) && 
    (Type_Is_Shared_Ptr(WN_ty(stmt)) !=  Type_Is_Shared_Ptr(WN_ty(WN_kid0(stmt))));
}


BOOL No_Shared_Refs(WN* stmt) 
{
  BOOL result = TRUE;
  int num_kids;
  switch(WN_operator(stmt)) {
  case OPR_LDID:
  case OPR_ILOAD: 
  case OPR_MSTORE:
  case OPR_STID:
  case OPR_ISTORE:
  case OPR_ARRAY:
    if(Type_Is_Shared_Ptr(WN_ty(stmt)))
       return FALSE;
  default:
    for(num_kids = 0; num_kids < WN_kid_count(stmt); num_kids++)
       result &= No_Shared_Refs(WN_kid(stmt, num_kids));
  }

  return result;
}

BOOL Check_Loop_Is_Redist(WN *body)
{
  
  BOOL is_redist = TRUE;
  BOOL child_is_redist = TRUE;
  
  switch(WN_operator(body)) {
  case OPR_BLOCK: 
    {
      WN  *treeStmt = WN_first(body);
      while(treeStmt)
	{
	  is_redist &= Check_Loop_Is_Redist(treeStmt);
	  if(!is_redist)
	    return FALSE;
	  treeStmt = WN_next(treeStmt);
	}
      break;
    }
  case OPR_DO_LOOP:
    is_redist &= Check_Loop_Is_Redist(WN_do_body(body));
    break;
  case OPR_ISTORE:
    is_redist &= (Stmt_Is_Redist_Assign(body) || No_Shared_Refs(body));
    break;
  case OPR_INTRINSIC_CALL:  
  case OPR_CALL:  
  case OPR_MSTORE:
  case OPR_STID:
    return No_Shared_Refs(body);
  default:
    return FALSE;
    Fail_FmtAssertion("Operator is not statement.");
  }
  
  return is_redist;
}

BOOL Check_One_Loop_Canon(WN *loop, BOOL &loop_is_redist) 
{
  WN *start, *lb, *end, *step;
  
  start = WN_start(loop);
  lb = WN_kid0(start);
  end = WN_end(loop);
  step = WN_step(loop);
  
  loop_is_redist = FALSE;

  if(WN_operator(start) != OPR_STID ||  WN_operator(lb) != OPR_INTCONST 
     || WN_const_val(lb) != 0) {
    if(trace_msg_vect)
      Print_Vec_Mesg(WN_Get_Linenum(loop), "Loop \"start\" not in canonical form");
      
    return FALSE;
  }
  
  if(WN_operator(end) != OPR_LE &&  WN_operator(end) != OPR_GE &&
     WN_operator(end) != OPR_LT && WN_operator(end) != OPR_GT) {
    if(trace_msg_vect)
      Print_Vec_Mesg(WN_Get_Linenum(loop), "Loop test not in canonical form");
      
    return FALSE;
  }
  
  step = WN_kid0(step);
  if(WN_operator(step) != OPR_ADD ||
     WN_operator(WN_kid1(step)) != OPR_INTCONST ||
     WN_const_val(WN_kid1(step)) != 1) {
    
    if(trace_msg_vect)
      Print_Vec_Mesg(WN_Get_Linenum(loop), "Loop increment not in canonical form");
    return FALSE;
  }
  
  loop_is_redist = Check_Loop_Is_Redist(WN_do_body(loop));
  
  return TRUE;
}


REMOTE_REF* VECT_INFO::Find_Ref(const SYMBOL& s)
{
  REMOTE_REF *result = NULL;
  REMOTE_REF *cur;
  int i;
  for(i=0; i < _refs.Elements(); i++) {
    cur = _refs[i];
    if(s == cur->base) {
      result = cur;
      break;
    }
  }
  return result;
}

void Add_Refs(VECT_INFO *vinfo, ARA_LOOP_INFO *li, int level, BOOL check_deps, BOOL loop_is_redist = FALSE)
{
  int i,j;
  BOOL do_vectorize = TRUE;
  REMOTE_REF *rref = NULL;
  ARA_REF_ST & use = li->USE();
  ARA_REF_ST & may_def = li->MAY_DEF();
  ARA_REF_ST & def = li->DEF();
   WN *wn, *to;
   
  //  fprintf(stderr,"////////////////////////////////////////////////////////// \n");
//    Get_Do_Loop_Info(li->Loop())->Print(stderr);
//    li->Print(stderr);
//    fprintf(stderr,"////////////////////////////////////////////////////////// \n");
   
  for(i = 0; i < use.Elements(); i++) {
    ARA_REF *read = use.Bottom_nth(i);
    if(Is_Upc_Vect(ST_type(read->Array().St()))) {
      rref = vinfo->Find_Ref(read->Array());
      if(rref == NULL) {
	rref = CXX_NEW(REMOTE_REF(li, level, (SYMBOL&)read->Array(), VEC_mpool), VEC_mpool);
	vinfo->Refs().AddElement(rref);
      }
     
      rref->level.AddElement(CXX_NEW(REMOTE_REF_DESCR(read,NULL, li, check_deps, level, 
						      VEC_mpool), VEC_mpool));
      do_vectorize = TRUE;
      
    } else { 
      //the reference is local, let the def code deal with it
      rref = NULL;
    }

    //mark shortcuts for memory to memory operations
    //in this case do not use temporary buffers, use the targets directly.
    if(loop_is_redist && rref) {
      //for any shared ref that gets assigned into a scalar ref
      //find that ref and mark the region for redistribution
      if(read->Image().Len() == 1) {
	wn = read->Image().Any_Wn();
	to = LWN_Get_Parent(LWN_Get_Parent(wn));
	Is_True(WN_operator(to) == OPR_ISTORE, (""));
      } else {
	Fail_FmtAssertion("Redistribution of multiple collapsed references not implemented\n");
      }
    }
    if (rref) {
      for(j = 0; j < def.Elements(); j++) {
	ARA_REF *write = def.Bottom_nth(j);
	if(read->Array() == write->Array() && rref) {
	  rref->level[level]->def = write;
	} else 
	  if(loop_is_redist) {
	    if(write->Image().Len() == 1 && LWN_Get_Parent(write->Image().Any_Wn()) == to) {
	      rref->level[level]->redist_targ = write;
	      rref->level[level]->redist_wn = WN_kid1(to);
	      break;
	    }
	  }
      }
    } 
  }
  
  for(i = 0; i < def.Elements(); i++) {
    ARA_REF *write = def.Bottom_nth(i);
    if(Is_Upc_Vect(ST_type(write->Array().St()))) {
      rref = vinfo->Find_Ref(write->Array());
      if(rref == NULL) {
	rref = CXX_NEW(REMOTE_REF(li, level, (SYMBOL&)write->Array(),VEC_mpool), VEC_mpool);
	vinfo->Refs().AddElement(rref);
      }
      rref->level.AddElement(CXX_NEW(REMOTE_REF_DESCR(NULL, write, li, check_deps, level, VEC_mpool), VEC_mpool));
      do_vectorize = TRUE;
    } else 
      rref = NULL;
    
    if(rref && loop_is_redist) {
      if(write->Image().Len() == 1) {
	wn = write->Image().Any_Wn();
	to = WN_kid0(WN_kid0(LWN_Get_Parent(wn)));
	Is_True(WN_operator(to) == OPR_ARRAY, (""));
      } else {
	Fail_FmtAssertion("Redistribution of multiple collapsed references not implemented\n");
      }
      for(j = 0; j < use.Elements(); j++) {
	ARA_REF *read = use.Bottom_nth(j);
	if(read->Image().Len() == 1 && 
	   (read->Image().Any_Wn() == to || read->Image().Contains(to))) {
	  rref->level[level]->redist_targ = read;
	  rref->level[level]->redist_wn = to; 
	}
      }
    }
     
  } 
}


static WN *Strip_Mine_Loop(WN *prefix, WN *loop, ST *sdesc, ST *ldesc, WN ** xtra_loop)
{
  WN *new_loop, *rem_loop, *wn1, *wn2;
  WN *index, *start, *end, *step, *body, *ub;
  WN *parent;

  index = WN_index(loop);
  end = WN_end(loop);
  body = WN_do_body(loop);
  start = WN_start(loop);
  step = WN_step(loop);
  parent = LWN_Get_Parent(loop);

  
  rem_loop = LWN_Copy_Tree(loop);
  ub = UBexp(WN_end(rem_loop));
  wn1 = WN_start(rem_loop);
  WN_kid0(wn1) = WN_Sub(Integer_type, 
			  LWN_Copy_Tree(ub),
			  WN_CreateExp2 (OPCODE_make_op (OPR_REM,
							 WN_rtype(ub),
							 MTYPE_V),
					 LWN_Copy_Tree(ub), 
					 WN_Ldid(TY_mtype(ST_type(sdesc)), 
						 0, sdesc, ST_type(sdesc))) 
			  );
  
  ST* outer_index = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "oidx");
  ST *inner_index = Gen_Temp_Symbol(MTYPE_To_TY(Integer_type), "iidx");
  WN * outer_start = LWN_Copy_Tree(start);
  
  Replace_Symbol(outer_start, SYMBOL(WN_st(index), 0, TY_mtype(ST_type(WN_st(index)))),
		 SYMBOL(outer_index, 0, TY_mtype(ST_type(outer_index))), 
		 WN_Ldid(Integer_type, 0, outer_index, ST_type(outer_index)),
		 outer_start);
  WN *outer_step = LWN_Copy_Tree(step);
  Replace_Symbol(outer_step, SYMBOL(WN_st(index), 0, TY_mtype(ST_type(WN_st(index)))),
		 SYMBOL(outer_index, 0, TY_mtype(ST_type(outer_index))), 
		 WN_Ldid(Integer_type, 0, outer_index, ST_type(outer_index)),
		 outer_step);
  WN *outer_end = LWN_Copy_Tree(end);
 


  Replace_Symbol(outer_end, SYMBOL(WN_st(index), 0, TY_mtype(ST_type(WN_st(index)))),
		 SYMBOL(outer_index, 0, TY_mtype(ST_type(outer_index))), 
		 WN_Ldid(Integer_type, 0, outer_index, ST_type(outer_index)),
		 outer_end);
  WN_kid1(outer_end) = WN_Sub(Integer_type, WN_Div(Integer_type, LWN_Copy_Tree(UBexp(outer_end)), 
			      WN_Ldid(Integer_type, 0, sdesc, ST_type(sdesc))), WN_Intconst(Integer_type,1));
  
  
  
  Replace_Symbol(step, SYMBOL(WN_st(index), 0, TY_mtype(ST_type(WN_st(index)))),
		 SYMBOL(inner_index, 0, TY_mtype(ST_type(inner_index))), 
		 WN_Ldid(Integer_type, 0, inner_index, ST_type(inner_index)),
		 step);
  Replace_Symbol(start, SYMBOL(WN_st(index), 0, TY_mtype(ST_type(WN_st(index)))),
		  SYMBOL(inner_index, 0, TY_mtype(ST_type(inner_index))), 
		  WN_Ldid(Integer_type, 0, inner_index, ST_type(inner_index)),
		 start);
 
   Replace_Symbol(end, SYMBOL(WN_st(index), 0, TY_mtype(ST_type(WN_st(index)))),
		 SYMBOL(inner_index, 0, TY_mtype(ST_type(inner_index))), 
		 WN_Ldid(Integer_type, 0, inner_index, ST_type(inner_index)),
		 end);
  WN_kid1(end) = WN_Sub(Integer_type, 
			WN_Ldid(Integer_type, 0, sdesc, ST_type(sdesc)),
			WN_Intconst(Integer_type,1)); // should it be -1?
 

  //new

  wn1 = WN_Stid(Integer_type, 0, WN_st(index), ST_type(WN_st(index)),
		WN_Add(Integer_type, WN_Ldid(Integer_type, 0, inner_index, ST_type(inner_index)),
		       WN_Mpy(Integer_type, 
			      WN_Ldid(Integer_type, 0, outer_index, ST_type(outer_index)),
			      WN_Ldid(Integer_type, 0, sdesc, ST_type(sdesc)))
		       )
		);
  WN_st_idx(index) = ST_st_idx(inner_index);
 
  
  WN_INSERT_BlockFirst(body, wn1);
  
  
  WN_INSERT_BlockBefore(parent, loop, rem_loop);
  WN_EXTRACT_FromBlock(parent, loop);

  wn1 = WN_CreateBlock();
  WN_INSERT_BlockFirst(wn1, loop);
  
  WN *outer_loop = LWN_CreateDO(WN_CreateIdname(0, ST_st_idx(outer_index)),
			       outer_start, outer_end, outer_step, wn1); 
  
  
  
  
  WN_INSERT_BlockBefore(parent, rem_loop, outer_loop);
 

  LWN_Set_Parent(outer_loop, parent);  
  LWN_Set_Parent(rem_loop, parent);
  
  *xtra_loop = rem_loop;
  
  return outer_loop;
 
}



void Walk_Inner_Loop_Refs(VECT_INFO *vinfo, ARA_LOOP_INFO  *li, int level, 
			  BOOL &complicated_nest,
			  BOOL check_deps,
			  STACK<DO_LOOP_INFO*> &dli_stack)
{
  int i;
  BOOL loop_is_redist = FALSE;
  
  if(li->Children().Elements() > 1) {
    complicated_nest = TRUE;
    return;
  }
  
  if(!Check_One_Loop_Canon((WN*)li->Loop(), loop_is_redist)) {
    complicated_nest = TRUE;
    return;
  }

  Is_True(vinfo->loop_is_redist == loop_is_redist, (""));

  Add_Refs(vinfo, li, level, check_deps, loop_is_redist);
  dli_stack.Push((DO_LOOP_INFO*)li->Info());
  for(i=0; i < li->Children().Elements(); i++) {
    Walk_Inner_Loop_Refs(vinfo, li->Children().Bottom_nth(i), level+1, 
			 complicated_nest, check_deps, dli_stack);
    if(complicated_nest)
      return;
  }
}




BOOL Check_Loops_Canon(ARA_LOOP_INFO *ali, int DEPTH) 
{
  int i;
  WN *start, *lb, *loop, *end, *step;
  BOOL loop_is_redist;
  if(DEPTH > ali->Depth()) {
    // top loop is good, go down the stack
    return Check_One_Loop_Canon((WN*)ali->Loop(), loop_is_redist);
    
  }  else {
    Is_True(0,(""));

  } 

  return TRUE;
}


void Vectorize_Loop(DO_LOOP_INFO *dli, BOOL check_deps, int DEPTH, BOOL *changed = NULL) {
  INT i, j;
  BOOL reg_is_decr = FALSE, loop_is_decr = FALSE;
  WN *reg_step = NULL, *loop_step = NULL;
  BOOL no_deps = FALSE;
  BOOL deps_known = FALSE;
  BOOL scalar_to_vect = FALSE;
  BOOL loop_is_redist = FALSE;
  BOOL do_vectorize = TRUE;
  VECT_INFO *vinfo = CXX_NEW(VECT_INFO(VEC_mpool),VEC_mpool);
  ST *sdesc;
  WN *fin;
  int level = 0;
  REMOTE_REF *rref = NULL;
  BOOL complicated_nest = FALSE;
  ARA_LOOP_INFO *li = dli->ARA_Info;
  LNO_Analysis = stdout;
  STACK<DO_LOOP_INFO*> dli_stack(VEC_mpool);
  WN *new_loop;
  ARA_LOOP_INFO *parent;

  SRCPOS srcpos = WN_Get_Linenum(li->Loop());
  int total_refs = 0;
  BOOL use_simple_interface = FALSE;
  
  //fprintf(stderr,"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++/ \n");
  //li->Print_Analysis_Info();
  //fprintf(stderr,"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++/ \n");

  if(!Check_One_Loop_Canon((WN*)li->Loop(), vinfo->loop_is_redist)) {
    return;
  }
 
  Add_Refs(vinfo, li, level, check_deps, vinfo->loop_is_redist);
  
  if(vinfo->_refs.Elements() == 0) {
    if(trace_msg_vect)
      Print_Vec_Mesg(WN_Get_Linenum(li->Loop()), "Nest not vectorized: no ARRAY nodes detected ");
    return;
  }

  dli_stack.Push(dli);

  if(DEPTH == 1) {
    //single loop
    
    if(vinfo->_refs.Elements() == 0)
      return;

    for(i=0; i < vinfo->_refs.Elements(); i++) {
      rref =vinfo->_refs[i];
      if(!rref->Analyze())
	return;
      total_refs += rref->total_refs;
    } 
    
    
    use_simple_interface = (total_refs == 1);
    WN *prefix, *laddr;
    WN *original_loop;
    original_loop = WN_COPY_Tree((WN*)li->Loop());
    vinfo->refs_vect = 0;
    prefix = WN_CreateBlock();
    laddr = WN_CreateBlock();
 
   
    if(!use_simple_interface) {
      vinfo->ldesc = Generate_Loop_Description(prefix,(DO_LOOP_INFO*)li->Info(), vinfo->loop_is_redist);
      Generate_Add_Pdim(prefix, (DO_LOOP_INFO*)li->Info(), vinfo->ldesc, 0);
    }
    for(i = 0; i < vinfo->Refs().Elements(); i++) {
      //vinfo->Refs()[i]->Collapse_Similar();
      for(j=0; j < vinfo->Refs()[i]->level.Elements(); j++) {
	vinfo->Refs()[i]->level[j]->Do_Code_Gen(vinfo->ldesc, prefix, laddr, vinfo->loop_is_redist, total_refs);
	vinfo->refs_vect += vinfo->Refs()[i]->level[j]->stride_use.Elements() + 
	  vinfo->Refs()[i]->level[j]->stride_def.Elements();
	//was level[0]
      }
    }
    ST *nrefs;
    

    if(use_simple_interface) { 
      nrefs = Generate_Analyze_Call(prefix, vinfo->ldesc, INTRN_VEC_ANAL_1RS1);
      sdesc = Generate_Get_Strips(laddr, vinfo->ldesc, INTRN_VEC_GETSTR_1RS1);
    } else { 
      nrefs = Generate_Analyze_Call(prefix, vinfo->ldesc);
      sdesc = Generate_Get_Strips(laddr, vinfo->ldesc);
    }

    WN *tblock;
    WN *temp = WN_CreateBlock();
    new_loop = Strip_Mine_Loop(tblock, (WN*)li->Loop(), sdesc, vinfo->ldesc, &tblock);


    if(use_simple_interface) {
      Generate_Void_Intrinsic(WN_do_body(new_loop), INTRN_VEC_ADVD_1RS1, TRUE);  
      Generate_Void_Intrinsic(temp, INTRN_VEC_ADVD_1RS1, FALSE);
    } else {
      Generate_AdvDim_Call(WN_do_body(new_loop), vinfo->ldesc, 0, TRUE);  
      Generate_AdvDim_Call(temp, vinfo->ldesc, 0, FALSE);
    }
    

    Generate_Test_Vect(prefix, new_loop, nrefs, vinfo->refs_vect, original_loop, tblock, temp);
    WN_INSERT_BlockBefore(LWN_Get_Parent(new_loop), new_loop, laddr);
    LWN_Set_Parent(laddr, new_loop);

    fin = WN_CreateBlock();
    if(use_simple_interface) {
      Generate_Void_Intrinsic(fin, INTRN_VEC_FIND_1RS1, TRUE);
    }  else {
      Generate_FinDim_Call(fin, vinfo->ldesc, 0);
      Generate_End_Call(fin, vinfo->ldesc);
    }
   
    WN_INSERT_BlockLast(LWN_Get_Parent(new_loop), fin);
  
  } else {
    
    //multiple nest
    if(li->Children().Elements() > 1) {
      if(trace_msg_vect)
	Print_Vec_Mesg(WN_Get_Linenum(li->Loop()), "Nest is not SNL (multiple inner loops on the same level)");
      return;
    }

    level = 1;
    for(i = 0; i < li->Children().Elements(); i++)
      {
      Walk_Inner_Loop_Refs(vinfo, li->Children().Bottom_nth(i), level, complicated_nest, check_deps, dli_stack);
      if(complicated_nest) {
	if(trace_msg_vect)
	  Print_Vec_Mesg(WN_Get_Linenum(li->Loop()), "Can't handle nest" );
	return;
      }
      //At this point if one inner loop is not in canonical form
      //the whole nest does not get vectorized
      //Maybe we should restart the process  on the inner loops.
     //  parent = parent->Parent();
    }
    
    for(i=0; i < vinfo->_refs.Elements(); i++) {
      rref =vinfo->_refs[i];
      if(!rref->Analyze())
	return;
    }
    
    WN *prefix, *laddr;
    WN *original_loop;
    ST *nrefs;
    original_loop = WN_COPY_Tree((WN*)li->Loop());
    prefix = WN_CreateBlock();
    laddr = WN_CreateBlock();
    vinfo->refs_vect = 0;
    vinfo->ldesc = Generate_Loop_Description(prefix,(DO_LOOP_INFO*)li->Info(), vinfo->loop_is_redist);
    for(i = 0; i < dli_stack.Elements(); i++)
      Generate_Add_Pdim(prefix, dli_stack.Bottom_nth(i), vinfo->ldesc, i);   
   
    for(i = 0; i < vinfo->Refs().Elements(); i++) {
      vinfo->Refs()[i]->Do_Code_Gen(vinfo->ldesc, prefix, laddr, vinfo->loop_is_redist);
      int num_dims = vinfo->Refs()[i]->level.Elements();
      vinfo->refs_vect += 
	vinfo->Refs()[i]->level[num_dims-1]->stride_use.Elements()/num_dims + 
	vinfo->Refs()[i]->level[num_dims-1]->stride_def.Elements()/num_dims;
    }
    
    vinfo->refs_vect = 1;
    Generate_AdvDim_Call(laddr, vinfo->ldesc, 0); 
    nrefs = Generate_Analyze_Call(prefix, vinfo->ldesc);
    Generate_Test_Vect(prefix, (WN*)li->Loop(), nrefs, vinfo->refs_vect, original_loop, NULL);
    WN_INSERT_BlockBefore(LWN_Get_Parent((WN*)li->Loop()), (WN*)li->Loop(), laddr);
    /* this is just a hack */
    for(i = 0; i < li->Children().Elements(); i++) {
      ARA_LOOP_INFO  *tmp = li->Children().Bottom_nth(i);
      WN * wn = (WN*)tmp->Loop();
      WN *parent = LWN_Get_Parent(wn);
      WN *block = WN_CreateBlock();
      Generate_AdvDim_Call(block, vinfo->ldesc, tmp->Depth());
      WN_INSERT_BlockBefore(parent, wn, block);
      
     //  block = WN_CreateBlock();
//       Generate_FinDim_Call(block, vinfo->ldesc, tmp->Depth());
//       WN_INSERT_BlockAfter(parent, wn, block);

    }
    
    LWN_Set_Parent(laddr, (WN*)li->Loop());
    fin = WN_CreateBlock();
    Generate_FinDim_Call(fin, vinfo->ldesc, 0);
    Generate_End_Call(fin, vinfo->ldesc);
    prefix = LWN_Get_Parent((WN*)li->Loop());
    if(vinfo->loop_is_redist) {
      WN_DELETE_FromBlock(prefix, (WN*)li->Loop());
    }
    WN_INSERT_BlockLast(prefix, fin);
  } //multiple nest
  
  if(trace_msg_vect) {
    Print_Vec_Mesg(srcpos, "Vectorized nest" );
  }
}

//Set up the loop info that will be used during upc-specific  optimizations
ARA_LOOP_INFO * Get_Loop_Info(WN* func_nd) {

  ARA_LOOP_INFO *root = 0;
  /* du-chain-debug  Du_Sanity_Check (func_nd); */
  /* du-chain-debug  Print_Def_Use (func_nd, stdout); */

  if (!vec_mempools_initialized) {
    MEM_POOL_Initialize (&VEC_memory_pool, "Vectorize_pool", FALSE);
    MEM_POOL_Initialize (&VEC_CG_mpool, "Vectorize_to_cg_mpool", FALSE);
    MEM_POOL_Push_Freeze (&VEC_CG_mpool);
    PF_mpool = &(VEC_memory_pool);
    VEC_mpool = &(VEC_memory_pool);
    vec_mempools_initialized = TRUE;
  }
  
  VEC_mpool = &(VEC_memory_pool);

  // the following is to avoid passing it as an argument all over the place
  // MAT<FRAC>::Set_Default_Pool(&VEC_memory_pool);
  
  root =  CXX_NEW(ARA_LOOP_INFO(func_nd, NULL, TRUE), &VEC_memory_pool);
  LWN_Parentize(func_nd);
  ARA_Initialize_Loops(func_nd, root);
  // Perform array region analysis
  ARA_Walk_Loops(root);

  // Perform liveness analysis
  root->Create_Live_Use();
  
  // Determine last value of private arrays
  root->Determine_Last_Value();
  // Print their ARA info
  
  Walk_Loop_Dependence(func_nd);
  extern void ARA_Print_Loops(ARA_LOOP_INFO *root_info);
  
 //  ARA_Print_Loops(root);  
  return root;
}



void VEC_Init(WN* func_nd) 
{

  if (!vec_mempools_initialized) {
    MEM_POOL_Initialize (&VEC_memory_pool, "Vectorize_pool", FALSE);
    MEM_POOL_Initialize (&VEC_CG_mpool, "Vectorize_to_cg_mpool", FALSE);
    MEM_POOL_Push_Freeze (&VEC_CG_mpool);
    PF_mpool = &(VEC_memory_pool);
    VEC_mpool = &(VEC_memory_pool);
    vec_mempools_initialized = TRUE;
  }
  
  VEC_mpool = &(VEC_memory_pool);
  // the following is to avoid passing it as an argument all over the place
  MAT<FRAC>::Set_Default_Pool(&VEC_memory_pool);
  
  //very likely that I need to set here shorthands for the dependence graph and the du_manager
  loops =  CXX_NEW(FIZ_FUSE_INFO(&LNO_default_pool),&LNO_default_pool);

  if (shared_void_ptr == 0) {
    shared_void_ptr = Make_Pointer_Type(Make_Shared_Type(MTYPE_To_TY(MTYPE_V), 1, RELAXED_CONSISTENCY));
  }

  //need to replace all ptr based accesses to fields with a temp holding the address

}



static void VEC_Extract_Loops(WN *wn_tree) 
{
  loops->Build(wn_tree, TRUE);
}


void VEC_Analyze(FIZ_FUSE_INFO *loops) 
{
  ARA_LOOP_INFO *root = 0;
  int i;
  int num_loops = loops->Num_Snl();

  if (!vec_mempools_initialized) {
    MEM_POOL_Initialize (&VEC_memory_pool, "Vectorize_pool", FALSE);
    MEM_POOL_Initialize (&VEC_CG_mpool, "Vectorize_to_cg_mpool", FALSE);
    MEM_POOL_Push_Freeze (&VEC_CG_mpool);
    PF_mpool = &(VEC_memory_pool);
    VEC_mpool = &(VEC_memory_pool);
    vec_mempools_initialized = TRUE;
  }
  
  VEC_mpool = &(VEC_memory_pool);

  // the following is to avoid passing it as an argument all over the place
  // MAT<FRAC>::Set_Default_Pool(&VEC_memory_pool);
  for(i = 1; i < num_loops; i++) {
    WN *top_loop = loops->Get_Wn(i);
    root =  CXX_NEW(ARA_LOOP_INFO(top_loop, NULL, TRUE), &VEC_memory_pool);
    LWN_Parentize(top_loop);
    ARA_Initialize_Loops(top_loop, root);
    // Perform array region analysis
    ARA_Walk_Loops(root);
    
    // Perform liveness analysis
    root->Create_Live_Use();
    
    // Determine last value of private arrays
    root->Determine_Last_Value();
   
    Walk_Loop_Dependence(top_loop);
  }
    extern void ARA_Print_Loops(ARA_LOOP_INFO *root_info);
  
    //  ARA_Print_Loops(root);  
  
}


void VEC_Do_Codegen(FIZ_FUSE_INFO *loops) 
{
  int num_loops = loops->Num_Snl();
  int i;
  DO_LOOP_INFO *dli;
  int j;
 //  loops->Print(stderr);
 
  for(i = 0; i < num_loops; i++ ) { 
    WN *top_loop = loops->Get_Wn(i);
    if(loops->Get_Type(i) == Invalid ||WN_opcode(top_loop) == OPC_IF ||
       WN_opcode(top_loop) == OPC_REGION || 
       WN_opcode(top_loop) == OPC_DO_WHILE ||
       WN_opcode(top_loop) == OPC_WHILE_DO  ) {
    
      continue;
    }
    INT nloops = loops->Get_Depth(i);
    if (nloops < 1 || loops->Get_Type(i) != Inner) {
    
      continue;
    } 
    
    
    // for(j = 0; j < loops->Get_Depth(i)-1; j++)
//       top_loop = Get_Only_Loop_Inside(top_loop,FALSE);
    
    dli = Get_Do_Loop_Info(top_loop);
    
    if(dli->Needs_Vectorization) { 
      if(dli->ARA_Info->Has_Upc_Sync() /*|| li->Info()->Has_Bad_Mem*/) {
	//Has_Bad_Mem I think it is too strong since it gets
	//initialized to TRUE whenever an ILOAD/ISTORE is encountered 
	if(trace_msg_vect) 
	  Print_Vec_Mesg(WN_Get_Linenum(top_loop),"Loop with strict access" );

	continue ;
      }
      
     // Print_ADG(stderr, Array_Dependence_Graph);
     Vectorize_Loop(dli, !(dli->Is_Ivdep || dli->No_RL_Alias), nloops ); 
      
    }
  }
  
}
 



void Upc_Vectorize(WN *func_nd) 
{

  total_stack_size = 0;
  struct rlimit r;

 
  VEC_Init(func_nd);
  LNO_Analysis = stdout;
  VEC_Extract_Loops(func_nd);
  VEC_Analyze(loops);
  VEC_Do_Codegen(loops);
 
 
  Current_LNO->Run_prefetch = 0;
  return;
}

void Create_ARA_Info(WN* wn) {

  ARA_LOOP_INFO *ara_root =
    CXX_NEW(ARA_LOOP_INFO(wn, NULL, TRUE), &ARA_memory_pool);
  ARA_Initialize_Loops(wn, ara_root);
  
  ARA_Walk_Loops(ara_root);
  
  // Perform liveness analysis
  ara_root->Create_Live_Use();
  
  // Determine last value of private arrays
  ara_root->Determine_Last_Value();
  
  Walk_Loop_Dependence(wn);
  /*
  fprintf(stdout,"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  ara_root->Print_Analysis_Info();
  fprintf(stdout ,"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  */
}





// Nice loops have constant coefficients in the index expressions of remote variables.
WN * SNL_Vectorize_Loops(WN *wn, INT nloops, SNL_NEST_INFO *ni, 
			  SNL_ANAL_INFO *ai, 
			  BOOL *changed, BOOL nice) 
{
  int i;
  ARRAY_DIRECTED_GRAPH16 *adg = Array_Dependence_Graph;
  DEP_SUMMARY problem;
  
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  fprintf(stdout, "===========================================================\n");
  if(!dli->ARA_Info) {
    Create_ARA_Info(wn);
    /*
      ARA_LOOP_INFO *ara_root =
      CXX_NEW(ARA_LOOP_INFO(wn, NULL, TRUE), &ARA_memory_pool);
      ARA_Initialize_Loops(wn, ara_root);
      
      
      ARA_Walk_Loops(ara_root);
      
      // Perform liveness analysis
      ara_root->Create_Live_Use();
      
      // Determine last value of private arrays
      ara_root->Determine_Last_Value();
      
      Walk_Loop_Dependence(wn);
      fprintf(stdout,"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
      ara_root->Print_Analysis_Info();
      fprintf(stdout ,"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
    */
  } 

  DOLOOP_STACK &stack = ni->Dostack();
  for(i = 0; i < stack.Elements(); i++) {
    WN *cl = stack.Bottom_nth(i);
    fprintf(stderr,"//////////////////////////////////////////\n");
    DO_LOOP_INFO *dlip = Get_Do_Loop_Info(cl);
    dlip->Print(stderr);
    fprintf(stderr,"//////////////////////////////////////////\n");
  }
  // Print_ADG(stderr, adg);
  problem = (DEP_SUMMARY)Deps_Say_Is_Vectorizable(adg);
  if(problem == TRUE_DEP) {
    if(trace_msg_vect)
      fprintf(stderr, "Found remote dep in nest at %d\n",  Srcpos_To_Line((WN_Get_Linenum(wn))));    return wn;
  }
 
  Vectorize_Loop(dli, !(dli->Is_Ivdep || dli->No_RL_Alias), 0,changed);
 
  
  
  return wn;
  
}

