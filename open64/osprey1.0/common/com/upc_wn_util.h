#ifndef upc_wn_util_INCLUDED
#define upc_wn_util_INCLUDED

#include "wn.h"
#include <stack>
#include "mempool.h"
#include "wn_map.h"
#include "upc_symtab_utils.h"


typedef std::stack<WN*> SPTR_OFFSET_TERM_STACK;
typedef std::stack<SPTR_OFFSET_TERM_STACK*> SPTR_OFFSET_ACCUMULATION_STACK;
extern  SPTR_OFFSET_ACCUMULATION_STACK sptr_off_accumulation_stack;

typedef enum {
  ACCUMULATION,
  ARRAY_ACCUMULATION,
  NO_ARRAY_ACCUMULATION,
  NO_ACCUMULATION
} SPTR_ACCUMULATION_STATE;
extern SPTR_ACCUMULATION_STATE sptr_accumulation_state;
  

extern std::stack<CONSISTENCY_class> consistency_stack;

extern BOOL Fold_Keep_Shared_Tas;
extern WN* WN_Create_Shared_Load (WN *ld, 
				  WN *dest = 0, 
				  WN_OFFSET xtra_offst = 0,
				  TY_IDX  access_ty = 0,
				  BOOL has_off = FALSE,  WN *off_wn = 0);
extern WN* WN_Create_Shared_Store (WN *st, 
				   BOOL src_is_shared = FALSE,
				   WN_OFFSET xtra_offst = 0,
				   BOOL has_offt = FALSE, WN *offt = 0, BOOL spill = TRUE);
extern WN* WN_Create_Shared_Ptr_Arithmetic( WN *base, WN *disp, OPERATOR opr, 
					    UINT esize = 0, UINT bsize = 0, int phaseless = 0);
extern WN* WN_Convert_Shared_To_Local ( WN *ptr, TY_IDX ty = 0, ST* st = 0);
extern WN* WN_Convert_Shared_To_Int ( WN *ptr );
extern WN* WN_Create_PtrEq_Test(OPERATOR op, WN *awn0, WN *awn1, TY_IDX idx0, TY_IDX idx1);
extern WN* WN_Create_StoP_Cvt(WN *init_wn, INTRINSIC iop); 

extern CONSISTENCY_class Get_Access_Consistency (TY_IDX idx);
extern void              Enter_Consistency_Info (mUINT16 pragma_id);
extern void              Clear_Consistency_Info ();

//extern WN* WN_Create_Shared_Ptr_Diff( WN *op0, WN *op1, UINT esize, UINT bsize);
extern WN* WN_Create_Shared_Ptr_Diff( WN *op0, WN *op1, TY_IDX t1, TY_IDX t2);
extern WN* WN_SetNull_Sptr(WN *st);
extern WN* Strip_TAS(WN * wn, TY_IDX ty_idx = 0);
extern WN* Spill_Shared_Load( WN *ld);
//extern void LowerUPC_Init_Consistency();
extern WN *Combine_Offset_Terms(SPTR_OFFSET_TERM_STACK &stack);
extern void Adjust_Consts_For_Ptr_Arithmetic(WN *ptr, WN*off);
extern TY_IDX Get_Array_Type(TY_IDX etyp, UINT64 size);

//For symmetric pointers, indicate we can cast a point-to-shared
//directly to a local pointer
extern BOOL Everything_Local;

//WN MAPs for UPC optimizations
//stores the mapping between an init and a sync call
//We need this so that the sync() will have the correct handle
extern WN_MAP upc_comm_map;

//mark whether a shared access should be performed using a nbi call
extern WN_MAP upc_nbi_map;  

extern MEM_POOL upc_mem_pool;

extern void Create_UPC_Maps();
extern void Destroy_UPC_Maps();


typedef struct {
  BOOL invalidate;  //whether we need to invalidate the sync handle after the call
  ST* st;  // st for the sync handle, to be filled by the init call
} sync_handle_t;

#endif
                                                              
