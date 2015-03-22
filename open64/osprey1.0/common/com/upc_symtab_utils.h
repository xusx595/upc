/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef upc_symtab_utils_INCLUDED
#define upc_symtab_utils_INCLUDED

/* runtime spec version in use (update whenever a API change in upcr.h is
 * used: see the README.developers file in the runtime tree for details) */
#ifndef UCPR_SPEC_MAJOR
#define UPCR_SPEC_MAJOR 3
#endif
#ifndef UPCR_SPEC_MINOR
#define UPCR_SPEC_MINOR 6
#endif


#include <wintrinsic.h>
//#include <string>

class WN;

typedef enum {
  STRICT_CONSISTENCY,
  RELAXED_CONSISTENCY,
  NO_CONSISTENCY
} CONSISTENCY_class;


/* UPC specific */
extern TY_IDX shared_ptr_idx;
extern TY_IDX pshared_ptr_idx;
//extern int compiling_upc;


/* synchronization */

#define  UPCR_BARRIERFLAG_ANONYMOUS 1 /* keep this consistent with upcr.h */
#define UPCR_BARRIERVAL_ANONYMOUS 0xdeadbeef

#define MAX_LINE_LEN_UPC 100

extern char *shared_ptr_name;
extern ST* upc_memget_st;
extern ST* upc_memput_st;
extern ST* upc_memcpy_st;
extern ST* upc_nulleq_st;
extern ST* upc_nullneq_st;
extern ST* upc_ptreqtest_st;
extern ST* upc_all_alloc_st;
extern ST* upc_global_alloc_st;
extern ST* upc_alloc_st;

extern TY_IDX upc_hsync_reg_ty;
extern TY_IDX upc_hsync_mem_ty;
extern ST* upc_forall_control_st;
extern ST *shared_null;
extern ST *pshared_null;
extern ST *upc_threads_st;
extern ST *upc_mythread_st;
extern ST *invalid_handle;

extern UINT64 string_hash(const char* str);

extern void Upc_Translate_Name(char *from, char **to);
extern void Initialize_Upc_Vars();
extern void Find_Upc_Vars();
extern void Initialize_Upc_Types(char *, UINT, UINT, 
				 char *, UINT, UINT,
				 char*, UINT, UINT,
				 char*, UINT, UINT);
extern void Upc_Lower_SymbolTable();
extern void Create_Special_Shared_Global_Symbols();
extern UINT64 Get_Type_Inner_Size (TY_IDX idx, BOOL follow_s = FALSE);
extern TY_IDX Get_Inner_Array_Type( TY_IDX idx);
extern TY_IDX TY_To_Sptr_Idx (TY_IDX);
extern UINT Get_Type_Block_Size (TY_IDX idx);
extern BOOL WN_Type_Is_Shared_Ptr (const WN* wn, BOOL real_ptr = FALSE);
extern BOOL Type_Is_Shared_Ptr (TY_IDX idx, BOOL real_ptr = FALSE);
extern TY_IDX Fix_Intrinsic_Return_Type(INTRINSIC i);
extern BOOL Upc_Intrinsic(INTRINSIC i);
extern BOOL Types_Are_Equiv(TY_IDX idx1, TY_IDX idx2);
extern BOOL Need_StoP_Cvt(TY_IDX src_idx, TY_IDX dest_idx, INTRINSIC *iop);

extern BOOL TY_is_pshared(TY_IDX idx);
extern BOOL Use_32_Bit(const char* filename);
extern INT Adjust_Field_Offset(TY_IDX struct_idx, UINT field_id, int offset);
extern INT Adjust_Field_Offset(TY_IDX struct_idx, UINT field_id);
extern INT Get_Field_Offset_From_Id(TY&  struct_idx, UINT field_id);
extern TY_IDX MTYPE_TO_Shared_TY_array[MTYPE_LAST+1];
extern TY_IDX Get_Field_Type (TY_IDX struct_type, UINT field_id);
extern UINT Adjusted_Type_Size(TY_IDX idx);
extern bool is_upcr_ptr(TY_IDX ty);
extern BOOL WN_Has_Valid_Type(WN *wn);
extern TY_IDX WN_Get_Ref_TY(WN *wn);
extern UINT Get_Field_Id(TY_IDX struct_ty, TY_IDX fld_ty, UINT offset);
extern bool Get_Field_By_Offset(TY_IDX struct_ty, TY_IDX fld_ty, UINT offset, UINT& cur_field_id);
extern TY_IDX Shared_To_Private_Type(TY_IDX actual_ty) ;
extern BOOL Type_Not_Mangled(TY_IDX ty) ;
extern void Change_Type_To_Shared(ST *st, ST_ATTR_TAB *st_attr_tab, int lexical_level);
extern BOOL INTRN_Is_Upc_Sync(INTRINSIC op);
#define MTYPE_To_Shared_TY(t)  MTYPE_TO_Shared_TY_array[t]

//Used by both front end and whirl2c to print escaped characters (\n, etc)
extern char *append_char(char* str, char ch);

//number of threads (== 0 in dynamic thread environment)
extern int upc_num_threads;

//Whether split-phase optimization should be performed
//(enabled by default)
extern int run_pre_add;
extern int run_split_phase;
extern int run_msg_vect;
extern int trace_msg_vect;
extern int run_forall_opt;
extern int run_ptr_coalescing;
extern int run_ptr_locality;
extern int run_auto_nb;
#endif /* upc_symtab_utils_INCLUDED */ 
