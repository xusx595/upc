#include <ctype.h>
#include <symtab.h>
#include <symtab_utils.h>
#include <upc_symtab_utils.h>
#include <wintrinsic.h>
#include <set>
#include "wn.h"

char *shared_ptr_name = "shared_ptr_struct";
char *shared_null_name = "upcr_null_shared";
char *pshared_null_name = "upcr_null_pshared";

char *hsync_reg_name;
char *hsync_mem_name;

//List of command line flags that can be passed to the translator's 
//backend to control UPC specific optimizations.
//All of the flags have a common structure;
//"-do-[option]" enables the optimization, while 
//"-no-[option]" disables it.

int run_pre_add = 1;
int run_split_phase = 0;
int run_forall_opt = 0;
int run_msg_vect = 0;  
int trace_msg_vect = 0;
int run_ptr_coalescing = 1;
int run_ptr_locality = 0;
int run_auto_nb = 0;

TY_IDX shared_ptr_idx = 0; 
TY_IDX pshared_ptr_idx = 0;
TY_IDX upc_hsync_reg_ty = 0;
TY_IDX upc_hsync_mem_ty = 0;

ST *upc_mythread_st = 0;
ST *upc_memget_st = 0;
ST *upc_memput_st = 0;
ST *upc_memcpy_st = 0;
ST* upc_forall_control_st = 0;
ST *shared_null = 0;
ST *pshared_null = 0;
ST *upc_threads_st = 0;
ST *invalid_handle = 0;

ST *upc_all_alloc_st = 0;
ST *upc_global_alloc_st = 0;
ST *upc_alloc_st = 0;

int upc_num_threads = 0;

TYPE_ID MTYPE_TO_Shared_TY_array[MTYPE_LAST+1];

extern void fdump_tree(FILE*, WN*);

/* bug 1109: all of the following goop exists solely to translate user calls
 * to the UPC library using the upc_* name prefix into the identical upcr_*
 * prefix function names in the runtime interface
 * starting in runtime version 2.1.14, the runtime's upcr_proxy.h also provides
 * this renaming service, so performing this renaming in the translator is
 * no longer necessary
 * this code can all be deleted once runtime v2.2 becomes widely deployed 
 */
void Upc_Translate_Name(char *from, char **to) {
  return; /* no-op */
}

TY_IDX Get_Field_Type (TY_IDX struct_type, UINT field_id)
{
  FmtAssert(TY_kind (struct_type) == KIND_STRUCT, ("expecting KIND_STRUCT in Get_Field_Type"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (struct_type, field_id, cur_field_id);
  FmtAssert(! fld.Is_Null(), ("Invalid field id %d for type 0x%x",
			      field_id, struct_type));
  return FLD_type (fld);
}

//Recover a field's id from its offset and type.
//If a field with the matching offset and type is found,
//the function returns true, and cur_field_id stores the correct field id.
//otherwise it returns false, indicating an error occured somewhere.
//This is used to recover the field id after the SSA conversion
bool 
Get_Field_By_Offset (TY_IDX struct_ty_idx, TY_IDX wanted_type, UINT offset, UINT &cur_field_id)
{
  UINT cur_offset = 0;
  if (TY_are_equivalent(struct_ty_idx, wanted_type)) {
    return true;
  }
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    TY_IDX fld_ty = FLD_type(fld);
    cur_offset = FLD_ofst(fld);
    cur_field_id++;
    if (cur_offset > offset) {
      break;   //no match    
    }

    if (offset == cur_offset) {
      if (TY_are_equivalent(fld_ty, wanted_type)) {
        return true;  //bingo
      }
    }
    if (TY_kind(fld_ty) == KIND_ARRAY)
      fld_ty = Get_Inner_Array_Type(fld_ty);
    //WEI: if the field is a {p}shared_ptr_t, make sure we don't try to traverse its fields                             
    if (TY_kind(fld_ty) == KIND_STRUCT && !is_upcr_ptr(fld_ty) &&
	TY_fld(fld_ty) != FLD_HANDLE()) {
      bool found_fld = Get_Field_By_Offset(fld_ty, wanted_type, offset - cur_offset, cur_field_id);
      if (found_fld)
	return found_fld;
    }
  } while (!FLD_last_field(fld_iter++));
  return false;
} // FLD_get_to_field           

bool is_upcr_ptr(TY_IDX ty) {
  
  return ty == shared_ptr_idx ||
    ty == pshared_ptr_idx ||
    //WEI: shouldn't need the following two, but for some reason the two above don't seem to be enough
    strcmp(TY_name(ty), "shared_ptr_struct") == 0 || 
    strcmp(TY_name(ty), "pshared_ptr_struct") == 0;
}
  

TY_IDX Get_Inner_Array_Type( TY_IDX idx) {
  
  Is_True(TY_kind(idx) == KIND_ARRAY,("",""));
  TY_IDX eidx = TY_etype(idx);
  while (TY_kind(eidx) == KIND_ARRAY)
    eidx = TY_etype(eidx);

  return eidx;
  
}

UINT Get_Type_Block_Size (TY_IDX idx) {

  UINT result;
  switch(TY_kind(idx)) {
  case KIND_STRUCT:
  case KIND_SCALAR:
  case KIND_VOID:
    Is_True(TY_is_shared(idx),("Request for the block_size of a non-shared type",""));
    result =  TY_block_size(idx);
    break;
  case KIND_POINTER: {
    if(TY_is_shared(idx))
      result =  TY_block_size(idx);
    else  if(TY_is_shared(TY_pointed(idx))) {
      if(TY_kind(TY_pointed(idx)) == KIND_ARRAY)
	result = Get_Type_Block_Size(TY_pointed(idx));
      else
	result =   TY_block_size(TY_pointed(idx));
    }
    else 
       Is_True(0, ("Request for the block_size of a non-shared type",""));
    }
    break;

  case KIND_ARRAY:
    result =  TY_block_size(Get_Inner_Array_Type(idx));
    break;
  default:
    Is_True(0, ("Request for the block_size of a non-shared type",""));
  }
  return result;
}


UINT64 Get_Type_Inner_Size (TY_IDX idx, BOOL follow_s) {
  
  switch(TY_kind(idx)) {
  case KIND_STRUCT:
    return Adjusted_Type_Size(idx);
  case KIND_SCALAR:
    return TY_size(idx);
  case KIND_POINTER:
    if(TY_is_shared(idx)  && !follow_s) {
      return (TY_block_size(idx) <= 1) ? TY_size(pshared_ptr_idx) : TY_size(shared_ptr_idx); 
    } else 
      if (TY_kind(TY_pointed(idx)) == KIND_ARRAY)
	return Adjusted_Type_Size(Get_Inner_Array_Type(TY_pointed(idx)));
      else { 
	return Adjusted_Type_Size(TY_pointed(idx));
      }
    break;
  case KIND_ARRAY:
      return Adjusted_Type_Size(Get_Inner_Array_Type(idx));
  default:
    Is_True(0,("Unexpected type for shared ptr arithmetic",""));
  }
  return 0;
}

UINT Adjusted_Type_Size(TY_IDX idx) {
  
  UINT result = 0;
  UINT alignment = TY_align(idx);

  switch(TY_kind(idx)) {
  case KIND_SCALAR:
    return TY_size(idx);
  case KIND_POINTER:
    if(!Type_Is_Shared_Ptr(idx)) {
      return TY_size(idx);
    } else {
      return TY_size(TY_To_Sptr_Idx(idx));
    }
  case KIND_STRUCT: {
    if(idx == pshared_ptr_idx || idx == shared_ptr_idx)
      return TY_size(idx);
   else 
     return TY_adjusted_size(idx);
  }
  case KIND_ARRAY:
   //  return TY_adjusted_size(idx);
    if(Type_Is_Shared_Ptr(TY_etype(idx), TRUE)) 
      return (TY_size(idx) / TY_size(TY_etype(idx))) * TY_size(TY_To_Sptr_Idx(TY_etype(idx)));
    else
      return TY_size(idx); 
    
    break;
  case KIND_VOID:
    return 0;
  }
  
  return result;
}



// This function assumes that the field offsets within
// a struct are already adjusted to account for the 
// shared pointers.
// The adjusted type size is offset(last_field) + sizeof(last_field)
void Adjust_Type_Size(TY *ty) {

  if (strncmp(TY_name(*ty), "shared_ptr_struct", 17) == 0 ||
      strncmp(TY_name(*ty), "pshared_ptr_struct", 18) == 0) {
    /* Don't change the type of the special symbols */
    return;
  }
  
 
  
  if(TY_kind(*ty) == KIND_STRUCT) {
    FLD_IDX field = ty->Fld();
    TY_IDX last_fld_idx = 0;
    UINT field_id = 1;
    UINT offset;
    UINT alignment  = 0;
      ;

    FLD_ITER  fiter  = Make_fld_iter(FLD_HANDLE(field));
    FLD_ITER last = Fld_Table.end ();
    FLD_HANDLE fh;
    do {
      fh = FLD_HANDLE(fiter);
      last_fld_idx = FLD_type(fh);
      if(alignment == 0) {
	if(Type_Is_Shared_Ptr(last_fld_idx) )
	  alignment = TY_size(TY_To_Sptr_Idx(last_fld_idx));
	else
	  alignment = TY_align(last_fld_idx);
      }
      offset = FLD_ofst(fh);
    } while (!FLD_last_field(fiter) && ++fiter != last);
   
    if(field) {
      offset += Adjusted_Type_Size(last_fld_idx);
      
      if(offset  % alignment == 0)
	Set_TY_size(*ty, offset);
      else 
	Set_TY_size(*ty, offset + (alignment - (offset % alignment)));
    }
   }
 
  if(TY_size(*ty) != 0 && TY_adjusted_size(*ty))
    Set_TY_size(*ty, TY_adjusted_size(*ty)); 
}

template <class T>
struct adjust_type_size_op
{
  TY *t;
  adjust_type_size_op (TY *it) : t(it) {}
  
  void operator () (UINT idx, TY *entry) const;
}; // adjust_type_size_op


template <class T>
inline void
adjust_type_size_op<T>::operator () (UINT idx, TY *entry) const {
  Adjust_Type_Size(entry);
}



struct adjust_size 
{
  void operator() (UINT32 idx, TY*  ty) const {
    FLD_HANDLE fh;
    if( TY_kind(*ty) == KIND_STRUCT &&
	!TY_is_union(*ty) &&
	strncmp(TY_name(*ty), "shared_ptr_struct", 17) != 0 &&
	strncmp(TY_name(*ty), "pshared_ptr_struct", 18) != 0) {
      
      FLD_IDX field = ty->Fld();
      UINT field_id = 1;
      FLD_ITER  fiter  = Make_fld_iter(FLD_HANDLE(field));
      FLD_ITER last = Fld_Table.end ();
      
      do {
	fh = FLD_HANDLE(fiter);
	TY_IDX fidx = FLD_type(fh);
	if(FLD_adjusted_ofst(fh) != 0) {
	  Set_FLD_ofst(fh, FLD_adjusted_ofst(fh));
	} else 
	  Set_FLD_ofst(fh, Get_Field_Offset_From_Id(*ty, field_id));
	field_id++;
      } while (!FLD_last_field(fiter) && ++fiter != last);

      
      field_id = 1;
      fiter =  Make_fld_iter(FLD_HANDLE(field));
      do {
	FLD_HANDLE fh = FLD_HANDLE(fiter);
	TY_IDX fidx = FLD_type(fh);
	if(Type_Is_Shared_Ptr(fidx)) {
	  Set_FLD_orig_type(fh, fidx);
	  Set_FLD_type(fh, TY_To_Sptr_Idx(fidx));
	}
	field_id++;
      } while (!FLD_last_field(fiter) && ++fiter != last);
      if(TY_size(*ty) != 0 && TY_adjusted_size(*ty))
	Set_TY_size(*ty, TY_adjusted_size(*ty));
    } else {
      if(TY_size(*ty) != 0 && TY_adjusted_size(*ty))
	Set_TY_size(*ty, TY_adjusted_size(*ty));
    }
  } 

} ;


int debug_requested;

static std::set<TY_IDX> retyped;

void Change_Type_To_Shared(ST *st, ST_ATTR_TAB *st_attr_tab, int lexical_level) 
{
  int i;
 
  TY_IDX st_ty  = ST_class(st) == CLASS_VAR ? ST_type(st) :
    ST_class(st) == CLASS_FUNC ? ST_pu_type(st) : ST_type(st);

  /* Do not lower private pointer-to-shared, 
     so whirl2c can tell them from shared variables */
  if (ST_class(st) == CLASS_VAR) {
    if (TY_is_shared(st_ty) ||
	(TY_kind(st_ty) == KIND_ARRAY && TY_is_shared(Get_Inner_Array_Type(st_ty)))) { 
      if(!debug_requested) {
	retyped.insert(st_ty);
	Set_ST_type (st, TY_To_Sptr_Idx(st_ty)); 
      }
    }
  }
  else if (ST_class(st) == CLASS_FUNC) {
    TYLIST_IDX idx = TY_tylist(ST_pu_type(st));
    ST_ATTR_TAB *pu_attr_tab =  
      Scope_tab[PU_lexical_level (Pu_Table[ST_pu (st)])].st_attr_tab;
    ST *lst;
    int i;
    
    int arg = 0;
    int has_shared = 0;
    while(Tylist_Table [idx]) {
      TY_IDX tidx = Tylist_Table[idx];
      if (Type_Is_Shared_Ptr(tidx)) {
	if(!debug_requested) {
	  retyped.insert(tidx);
	  Set_TYLIST_type (Tylist_Table [idx], TY_To_Sptr_Idx(tidx));
        }
	idx++;  
	has_shared = 1;
      }
      else idx++;
    }

    idx = TY_tylist(ST_pu_type(st));
    while(has_shared && Tylist_Table [idx]) { 
      TY_IDX tidx = Tylist_Table[idx];
      ST_ATTR_IDX st_attr_idx;
      ST_ATTR&    st_attr = New_ST_ATTR (lexical_level, st_attr_idx);
      if(!arg) {
	
	ST_ATTR_Init (st_attr, ST_st_idx (st),  ST_ATTR_FN_RET_TY_IDX,
		      tidx);
	arg = 1;
      } else {
	ST_ATTR_Init (st_attr, ST_st_idx (st),  ST_ATTR_FN_ARG_TY_IDX,
		      tidx);
      }
      idx++;
    }
    
  }
}




void Upc_Lower_SymbolTable() {
  int level, i;
  ST *st;
  ST_ATTR_TAB *cur_tab;
  Is_True (shared_ptr_idx, ("Shared ptr type not initialized",""));
  cur_tab  = Scope_tab[GLOBAL_SYMTAB].st_attr_tab;
  FOREACH_SYMBOL(GLOBAL_SYMTAB, st, i)
    Change_Type_To_Shared (st, cur_tab, GLOBAL_SYMTAB);

  // Bugs 2308 and 2967 - create symbols to reference types that
  // have been displaced at least once by [p]shared_ptr_idx
  for(std::set<TY_IDX>::iterator i = retyped.begin(); i != retyped.end(); ++i) {
    TY_IDX ty_idx = *i;
    st = New_ST(GLOBAL_SYMTAB);
    ST_Init (st, Save_Str2i("_bupc_retyped","_",(int)ty_idx),
	     CLASS_VAR, SCLASS_EXTERN, EXPORT_LOCAL, ty_idx);
  }
  retyped.clear();

  For_all_entries(Ty_tab, adjust_size(), 1);
  For_all <adjust_type_size_op<TY> > (Ty_Table, adjust_type_size_op <TY>(0) );
 
}
  


INT Get_Field_Offset_From_Id(TY& struct_ty, UINT field_id) {
  
  UINT off = 0;
  UINT cur_id = 0;
  UINT aligned = 0;
  UINT displacement;
  UINT alignment = 1;
  TY_IDX fld_ty;
  FmtAssert(TY_kind(struct_ty) == KIND_STRUCT, ("",""));

  if(field_id  == 0|| field_id == 1)
    return 0;

  FLD_IDX idx = struct_ty.Fld();
  FLD_ITER fiter = Make_fld_iter(FLD_HANDLE(idx));
  FLD_HANDLE prev_fld = FLD_HANDLE(fiter++);
  FLD_HANDLE fh;
  cur_id = 2;  
  while(cur_id++ <= field_id) {
    fh = FLD_HANDLE(fiter);
    fld_ty = FLD_type(fh);
    alignment = TY_align(fld_ty);
    if(Type_Is_Shared_Ptr(fld_ty))
      alignment = TY_align(TY_To_Sptr_Idx(fld_ty));
    displacement = off + Adjusted_Type_Size(FLD_type(prev_fld)) ;
    if(displacement % alignment == 0) 
      off = displacement;
    else 
      off = displacement + (alignment - displacement % alignment);
    prev_fld = fh;
    ++fiter;
  } ;

  return off;
}



/**
 *
 * Adjust the offset of field accesses, for structs that have shared pointers 
 * offset > sizeof (struct_ty) can happen for expressions like p[i+1].y,
 * where 1 is folded into the offset of the iload/istore.
 */

INT Adjust_Field_Offset(TY_IDX struct_ty, UINT field_id, int offset) {
  
  INT off = 0;
  TY_IDX fld_ty;
  UINT displacement = 0;
  UINT alignment;
  UINT cur_id;
  int num_struct = 0; 
  int off_orig = 0;
  BOOL inner_struct = FALSE;

  if(TY_kind(struct_ty) != KIND_STRUCT) {
    Ty_Table[struct_ty].Print(stderr);
    Fail_FmtAssertion("Expected struct type: %s  \n", Index_To_Str(TY_name_idx(Ty_Table[struct_ty])));
  }
  if(field_id == 0 || field_id == 1) {
    return (offset/TY_size(struct_ty))*TY_adjusted_size(struct_ty) + offset%TY_size(struct_ty);
  }

  // The case of incomplete type is now caught in the front end
  if (TY_size(struct_ty) == 0) {
    return 0;
  }
  
  if(offset > 0)
    num_struct = offset / TY_size(struct_ty);
  
  //bug 401
  //handle negative offsets
  if(offset < 0) {
    off_orig = offset;
    offset = TY_size(struct_ty) + offset;
  }

  UINT cur_field_id = 0;
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_field_id++;
    if (cur_field_id == field_id) {
      off = Adjust_Field_Offset (struct_ty, FLD_ofst(fld));
      if (FLD_is_bit_field(fld)) {
	// At least until we can output bitfield types;
	Fail_FmtAssertion("Unable to adjust offset for bitfield %s in struct %s",
			  FLD_name(fld), TY_name(struct_ty));
#if 0
	INT bofst = FLD_bofst(fld);
	if (bofst % 8)
	  Fail_FmtAssertion("Unable to adjust offset for unaligned bitfield %s in struct %s",
			    FLD_name(fld), TY_name(struct_ty));
	off += bofst / 8;
#endif
      }
      break;
    }
    if (TY_kind(FLD_type(fld)) == KIND_STRUCT && !is_upcr_ptr(FLD_type(fld)) &&
	TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
      UINT nested_field_id = field_id - cur_field_id;
      FLD_HANDLE inner_fld = FLD_get_to_field(FLD_type(fld), field_id, cur_field_id);
      if (inner_fld != FLD_HANDLE()) {
	//the field is in the nested struct
	off = Adjust_Field_Offset(struct_ty, FLD_ofst(fld)) +
	  Adjust_Field_Offset(FLD_type(fld), nested_field_id, 0);
	inner_struct = TRUE;
	break;
      }
    }
  } while (!FLD_last_field(fld_iter++));

  if(!TY_is_union(struct_ty) && !( inner_struct && field_id == 2)) {
    FmtAssert(off != 0, ("can't find the corresponding field id: %d", field_id)); 
  }
  if (num_struct != 0) {
    return off + num_struct * Adjusted_Type_Size(struct_ty);
  } else {
    if(off_orig < 0) {
      off = off - Adjusted_Type_Size(struct_ty);
    }
    return off;
  }
}

static TYPE_ID
Size_To_Mtype (UINT32 size) 
{
  
  if (size == TY_size(MTYPE_To_TY(MTYPE_I1)))
     return MTYPE_I1;
  else if (size == TY_size(MTYPE_To_TY(MTYPE_I2)))
     return MTYPE_I2;
  else if (size == TY_size(MTYPE_To_TY(MTYPE_I4)))
     return MTYPE_I4;
  else if (size == TY_size(MTYPE_To_TY(MTYPE_I8)))
     return MTYPE_I8;
  else 
    return MTYPE_M;

}


static void 
Fill_Structure_Type (TY &ty, UINT size, UINT align) {
  
  TY_IDX bogus_idx = 0;
  TY &bogus = New_TY (bogus_idx);
  TY_Init (bogus, size, KIND_ARRAY, MTYPE_M, Save_Str("bogus_name"));
  Set_TY_etype (bogus, MTYPE_To_TY(MTYPE_I1));
  Set_TY_align (bogus_idx, align);
  ARB_HANDLE arb = New_ARB (); 
  ARB_Init (arb, 0, 0, 0);
  Set_TY_arb (bogus, arb);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_ARB_dimension (arb, 1);
  Set_ARB_const_stride (arb);
  Set_ARB_stride_val (arb, size);
  Set_ARB_const_lbnd (arb);
  Set_ARB_lbnd_val (arb, 0);
  Set_ARB_const_ubnd (arb);
  Set_ARB_ubnd_val (arb,size-1);
  
  //Ty_Table[bogus_idx].Print(stderr);
  
  FLD_HANDLE fld = New_FLD();
  FLD_Init(fld, Save_Str("bogus_field_name"), bogus_idx, 0);
  Set_FLD_orig_type(fld, bogus_idx);
  Set_TY_fld(ty, fld);
  Set_FLD_last_field(fld);
}           

void Create_Special_Shared_Global_Symbols()
{
  TY_IDX ty_idx;
  TY_KIND ty_kind = KIND_SCALAR;
  ST *s;
  
#define LAST_SCALAR_SHARED_TYPE MTYPE_F16

  int j;

  /* Make predefined types for the machine types: */
  for (TYPE_ID i = MTYPE_I1; i  <= LAST_SCALAR_SHARED_TYPE; ++i) {
    
    TY &ty = New_TY (ty_idx);
    ty_kind = KIND_SCALAR;
    Set_TY_align (ty_idx, TY_align(MTYPE_To_TY(i)));
    TY_Init (ty, MTYPE_byte_size (i), ty_kind, i,
	     Save_Str2(".predef_shared_", MTYPE_name (i)));
    MTYPE_To_Shared_TY (i) = ty_idx;
    Set_TY_is_shared(ty_idx);
  }
  
  s = New_ST(GLOBAL_SYMTAB);
  ty_idx = Make_Shared_Type(MTYPE_To_TY(MTYPE_V), 1, STRICT_CONSISTENCY);
  ty_idx = Make_Pointer_Type(ty_idx);
  ST_Init (s, Save_Str(shared_null_name), CLASS_VAR, SCLASS_EXTERN , EXPORT_PREEMPTIBLE, ty_idx);
  Clear_ST_keep_name_w2f(s);

  s = New_ST(GLOBAL_SYMTAB);
  ty_idx = Make_Shared_Type(MTYPE_To_TY(MTYPE_I4), 1, STRICT_CONSISTENCY);
  ty_idx = Make_Pointer_Type(ty_idx);
  ST_Init (s, Save_Str(pshared_null_name), CLASS_VAR, SCLASS_EXTERN , EXPORT_PREEMPTIBLE, ty_idx);
  Clear_ST_keep_name_w2f(s);
}

class TY_find {
private:
  char name[256];

public:
  TY_find(char * name) {
    strncpy(this->name, name, 256);
  }

  bool operator() (UINT, const TY* ty) const {
    return strcmp(TY_name(*ty), this->name) == 0;
  }
};
  
void Find_Upc_Vars () {

  ST* s;
  int i;

  //for source-to-source translation there's no reason 
  //to restrict load/stores to have no offsets
  //This affects WOPT in the SSA representaion of iload/istores
  Use_Load_Store_Offset = true;

  FOREACH_SYMBOL(GLOBAL_SYMTAB, s, i) {
    if (strcmp(ST_name(*s),"upcr_forall_control") == 0)  {
      upc_forall_control_st = s;
    } else if (strcmp(ST_name(*s),shared_null_name) == 0)  {
      shared_null = s;
    } else if (strcmp(ST_name(*s),pshared_null_name) == 0)  {
      pshared_null = s;
    } else if (strcmp(ST_name(*s), "THREADS") == 0) {
      upc_threads_st = s;
    } else if (strcmp(ST_name(*s), "MYTHREAD") == 0) {
      upc_mythread_st = s;
    } else if (strcmp(ST_name(*s), "UPCR_INVALID_HANDLE") == 0) {
      invalid_handle = s;
    } else if (strcmp(ST_name(*s), "upc_memget") == 0) {
	upc_memget_st = s;
    } else if (strcmp(ST_name(*s), "upc_memput") == 0) {
	upc_memput_st = s;
    } else if (strcmp(ST_name(*s), "upc_memcpy") == 0) {
	upc_memcpy_st = s;
    } else if (strcmp(ST_name(*s), "upc_all_alloc") == 0) {
	upc_all_alloc_st = s; 
    } else if (strcmp(ST_name(*s), "upc_global_alloc") == 0) {
	upc_global_alloc_st = s;
    } else if (strcmp(ST_name(*s), "upc_alloc") == 0) {
	upc_alloc_st = s;
    }
  }
  FmtAssert(upc_forall_control_st != NULL && shared_null != NULL && pshared_null != NULL,
	    ("Can't find required UPC symbols in the symbol table"));
	    
  //it's possible that THREADS/MYTHREAD are undefined,
  //when the program never refers to them
  if (upc_threads_st == NULL) {
    upc_threads_st = New_ST(GLOBAL_SYMTAB);
    ST_Init (upc_threads_st, Save_Str("THREADS"), CLASS_VAR, SCLASS_UGLOBAL, 
	     EXPORT_PREEMPTIBLE, MTYPE_To_TY(Integer_type));
  }
  if (upc_mythread_st == NULL) {
    upc_mythread_st = New_ST(GLOBAL_SYMTAB);
    ST_Init (upc_mythread_st, Save_Str("MYTHREAD"), CLASS_VAR, SCLASS_UGLOBAL, 
	     EXPORT_PREEMPTIBLE, MTYPE_To_TY(Integer_type));
  }

  //find the static number of threads from the INITO of upc_threads, if it exists
  INITV_IDX initv = INITV_index(GLOBAL_SYMTAB, ST_st_idx(upc_threads_st));
  if (initv != INITV_Table_Size() + 1) {
    switch (INITV_kind(initv)) {
    case INITVKIND_ONE:
      upc_num_threads = 1;
      break;
    case INITVKIND_VAL:
      upc_num_threads = TCON_ival(INITV_tc_val(initv));
      break;
    default:
      FmtAssert(0, ("unrecongized INTIV kind for number of upc threads"));
    }
  }
  shared_ptr_idx = For_all_until(Ty_Table, TY_find("shared_ptr_struct"));
  pshared_ptr_idx = For_all_until(Ty_Table, TY_find("pshared_ptr_struct"));
  upc_hsync_reg_ty = For_all_until(Ty_Table, TY_find("reg_handle_t"));
  upc_hsync_mem_ty = For_all_until(Ty_Table, TY_find("mem_handle_t"));
 
  Is_True(shared_ptr_idx != 0 && pshared_ptr_idx != 0, ("Can't find shared_ptr_struct in symbol table", ""));
  Is_True(upc_hsync_reg_ty != 0 && upc_hsync_mem_ty != 0, ("Can't find reg/mem handle_t in symbol table", ""));

  if (invalid_handle == NULL) {
    invalid_handle = New_ST(GLOBAL_SYMTAB);
    ST_Init(invalid_handle, Save_Str("UPCR_INVALID_HANDLE"), CLASS_VAR, SCLASS_UGLOBAL,
	    EXPORT_PREEMPTIBLE, upc_hsync_mem_ty);
    Clear_ST_keep_name_w2f(invalid_handle);
  }
  
  /* Symbol table apparently does not preserve type alignment, 
     so reset them there */
  Set_TY_align(shared_ptr_idx, TY_align(MTYPE_To_TY(Pointer_Mtype)));
  Set_TY_align(pshared_ptr_idx, TY_align(MTYPE_To_TY(Pointer_Mtype)));
}

BOOL Use_32_Bit(const char* filename) {

  FILE* config_file = fopen(filename, "r");
  char line[MAX_LINE_LEN_UPC];
  int size;
  char param[MAX_LINE_LEN_UPC];
  while (fgets(line, MAX_LINE_LEN_UPC, config_file) != NULL) {
    if (sscanf(line, "%s\t%d", param, &size) != 2) {
      continue;
    }
    if (strcmp(param, "ptr_size") == 0 && size == 4) {
      return TRUE;
    }
  }
  return FALSE;
}

void Initialize_Upc_Vars () {

  upc_forall_control_st = New_ST(GLOBAL_SYMTAB);
  TY_IDX idx = MTYPE_To_TY(MTYPE_I4);
  ST_Init(upc_forall_control_st, 
	  Save_Str("upcr_forall_control"),
	  CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, idx);
}

//sizes are in bytes
void Initialize_Upc_Types (char *sptr_name, UINT sptr_size, UINT sptr_align,
			   char *psptr_name, UINT psptr_size, UINT psptr_align,
			   char *hreg_name, UINT hreg_size, UINT hreg_align,  // reg_handle_t 
			   char *hmem_name, UINT hmem_size, UINT hmem_align)  // mem_handle_t

{
  
  TYPE_ID mtype;
  TY_KIND ty_kind;
  UINT align;

  TY &sty = New_TY(shared_ptr_idx); 
  TY &psty = New_TY(pshared_ptr_idx); 
  TY &rty = New_TY (upc_hsync_reg_ty);
  TY &mty = New_TY (upc_hsync_mem_ty);
  BOOL _64_bit_target = TY_size(MTYPE_To_TY(Pointer_type)) > 4;
  UINT talign = _64_bit_target ? 8 : 4;
  UINT fac;

  mtype = Size_To_Mtype(sptr_size);
  if(!_64_bit_target && mtype == MTYPE_I8) {
    mtype = MTYPE_M;
  }
  ty_kind = (mtype < MTYPE_M) ? KIND_SCALAR : KIND_STRUCT;
  
 
  TY_Init(sty, sptr_size, ty_kind, mtype, Save_Str (sptr_name));
  Set_TY_adjusted_size(sty, sptr_size);
  Set_TY_align(shared_ptr_idx, sptr_align);
  if (mtype == MTYPE_M)
    Fill_Structure_Type (sty, sptr_size, sptr_align);
  
  mtype = Size_To_Mtype(psptr_size);
  if(!_64_bit_target && mtype == MTYPE_I8) {
    mtype = MTYPE_M;
  }
  ty_kind = (mtype < MTYPE_M) ? KIND_SCALAR : KIND_STRUCT;
   
  
  TY_Init(psty, psptr_size, ty_kind, mtype, Save_Str (psptr_name));
  Set_TY_align(pshared_ptr_idx, psptr_align);
  Set_TY_adjusted_size(psty, psptr_size);
  if (mtype == MTYPE_M)
    Fill_Structure_Type (psty, psptr_size, psptr_align);

  
  mtype = Size_To_Mtype(hreg_size);
  if(!_64_bit_target && mtype == MTYPE_I8) {
    mtype = MTYPE_M;
  }
  ty_kind = (mtype < MTYPE_M) ? KIND_SCALAR : KIND_STRUCT;
  TY_Init(rty, hreg_size, ty_kind, mtype, Save_Str (hreg_name));
  Set_TY_adjusted_size(rty, hreg_size);
  Set_TY_align(upc_hsync_reg_ty, hreg_align);
  if (mtype == MTYPE_M)
    Fill_Structure_Type (rty, hreg_size, hreg_align);
  
  mtype = Size_To_Mtype(hmem_size);
  if(!_64_bit_target && mtype == MTYPE_I8) {
    mtype = MTYPE_M;
  }
  ty_kind = (mtype < MTYPE_M) ? KIND_SCALAR : KIND_STRUCT;
  TY_Init(mty, hmem_size, ty_kind, mtype, Save_Str (hmem_name));
   Set_TY_adjusted_size(mty, hmem_size);
  Set_TY_align(upc_hsync_mem_ty, hmem_align);
  if (mtype == MTYPE_M)
    Fill_Structure_Type (mty, hmem_size, hmem_align);

}

//It's illegal to call WN_ty() on a OPR_CONST, so we need this wrapper here 
//(may apply to other nodes too??)
BOOL WN_Type_Is_Shared_Ptr(const WN* wn, BOOL real_ptr) {

  switch (WN_operator(wn)) {
  case OPR_CONST:
    return false;
  default:
    return Type_Is_Shared_Ptr(WN_ty(wn), real_ptr);
  }
}

BOOL Type_Is_Shared_Ptr (TY_IDX idx, BOOL real_ptr) {

  return real_ptr ?  
    TY_kind(idx) == KIND_POINTER  && TY_is_shared(TY_pointed(idx)) :
    
    (TY_is_shared(idx) ||
     (TY_kind(idx) == KIND_POINTER  && TY_is_shared(TY_pointed(idx))) ||
     (TY_kind(idx) == KIND_ARRAY && 
      TY_is_shared(Get_Inner_Array_Type(idx)))) ; 
}

BOOL TY_is_pshared(TY_IDX idx) {

  //NOTE: TY_is_shared must be true as a precondition of this method
  FmtAssert(TY_is_shared(idx), ("Calling TY_is_pshared with a non-shared type",""));

  if (TY_kind(idx) == KIND_ARRAY) {
    return TY_is_pshared(TY_etype(idx));
  }
  if (TY_kind(idx) != KIND_VOID) {
    return TY_block_size(idx) <= 1;
  }
  return false;
}



TY_IDX TY_To_Sptr_Idx (TY_IDX idx) 
{

  if (TY_is_shared(idx)) {
    return TY_is_pshared(idx) ? pshared_ptr_idx : shared_ptr_idx;
  }
  if (Type_Is_Shared_Ptr(idx, true)) {
    return TY_is_pshared(TY_pointed(idx)) ? pshared_ptr_idx : shared_ptr_idx;
  }
  return idx;
}

BOOL Upc_Intrinsic(INTRINSIC op) {
  switch(op) {
  case INTRN_ADD_PI:
  case INTRN_ADD_P1:
  case INTRN_S_TO_P:
  case INTRN_SPTRDIFF:
  case INTRN_S_RESET:
  case INTRN_ADD_S:
  case INTRN_P_TO_S:
  case INTRN_SPTRADD:
    return TRUE;
  default:
    return FALSE;
  }
  return FALSE;
}

TY_IDX Fix_Intrinsic_Return_Type(INTRINSIC op) 
{
  switch(op) {
  case INTRN_ADD_PI:
  case INTRN_ADD_P1:
  case INTRN_S_TO_P:
    return pshared_ptr_idx;
  case INTRN_SPTRADD:
  case INTRN_SPTRDIFF:
  case INTRN_S_RESET:
  case INTRN_ADD_S:
  case INTRN_P_TO_S:
    return shared_ptr_idx;
  }
}

BOOL INTRN_Is_Upc_Sync(INTRINSIC op) {

  if (op == INTRN_UPCFENCE || op == INTRN_UPCBAR ||
      op == INTRN_UPCNTFY || op == INTRN_UPCWAIT ||
      op == INTRN_LOCK || op == INTRN_LOCK_ATTEMPT || op == INTRN_UNLOCK) {
    return TRUE;
  }
  return FALSE;
      

}

BOOL Need_StoP_Cvt(TY_IDX src_idx, TY_IDX dest_idx, INTRINSIC *iop)
{
  BOOL result = FALSE;
  *iop = INTRINSIC_LAST;
  if(src_idx == 0 || dest_idx == 0 ||
     TY_kind(src_idx) == KIND_SCALAR ||
     TY_kind(dest_idx) == KIND_SCALAR)
    return FALSE;

  //No casts needed if we have shared pointer-to-private
  if (TY_kind(src_idx) == KIND_POINTER && !TY_is_shared(TY_pointed(src_idx))) {
    return FALSE;
  }

  if (TY_kind(dest_idx) == KIND_POINTER && !TY_is_shared(TY_pointed(dest_idx))) {
    return FALSE;
  }

  TY_IDX src_real = TY_To_Sptr_Idx(src_idx);
  TY_IDX dst_real = TY_To_Sptr_Idx(dest_idx);

  UINT src_blk = Get_Type_Block_Size(src_idx);
  UINT dest_blk = Get_Type_Block_Size(dest_idx);
  UINT src_sz = Get_Type_Inner_Size(src_idx, TRUE);
  UINT dest_sz = Get_Type_Inner_Size(dest_idx, TRUE);
  
  if(TY_is_shared(src_idx)) {
    switch(TY_kind(src_idx)) {
    case KIND_POINTER:
      src_blk = Get_Type_Block_Size(TY_pointed(src_idx));
      src_real = TY_To_Sptr_Idx(TY_pointed(src_idx));
      break;
    }
  }

  if(TY_is_shared(dest_idx)) {
    switch(TY_kind(dest_idx)) {
    case KIND_POINTER:
      dest_blk = Get_Type_Block_Size(TY_pointed(dest_idx));
      dst_real = TY_To_Sptr_Idx(TY_pointed(dest_idx));
      break;
    }
  }

  Is_True(((TY_kind(src_idx) == KIND_POINTER || TY_kind(src_idx) == KIND_ARRAY) &&
	  (TY_kind(dest_idx) == KIND_POINTER || TY_kind(dest_idx) == KIND_ARRAY)) ||
	  (TY_kind(dest_idx) == KIND_SCALAR && TY_kind(src_idx) == KIND_SCALAR),
	  ("Incorrect type combination in StoP",""));

  if (src_real == pshared_ptr_idx) {
    if (dst_real == shared_ptr_idx) {
      *iop = INTRN_P_TO_S;
      result = TRUE;
    }
  } else if (src_real == shared_ptr_idx) {
    if (dst_real == pshared_ptr_idx) {
      *iop = INTRN_S_TO_P;
      result = TRUE;
    } else if (dst_real == shared_ptr_idx) {
      //both are pointer-to-shared, may need to reset phase
      if (   !(TY_kind(src_idx)  == KIND_POINTER && TY_kind(TY_pointed(src_idx))  == KIND_VOID)
  	  && !(TY_kind(dest_idx) == KIND_POINTER && TY_kind(TY_pointed(dest_idx)) == KIND_VOID)) {
	if (src_blk != dest_blk || src_sz != dest_sz || !src_sz || !dest_sz) {
	  *iop = INTRN_S_RESET;
	  result = TRUE;
	}
      }
    }
  }

  return result;
}

BOOL Types_Are_Equiv(TY_IDX idx1, TY_IDX idx2)
{
  

  if (idx1 == idx2)
    return TRUE;
  if(TY_kind(idx1) == KIND_POINTER) 
    idx1 = TY_pointed(idx1);
  else 
    if(TY_kind(idx1) == KIND_ARRAY)
      idx1 = Get_Inner_Array_Type(idx1);
  
  if(TY_kind(idx2) == KIND_POINTER) 
    idx2 = TY_pointed(idx2);
  else 
    if(TY_kind(idx2) == KIND_ARRAY)
      idx2 = Get_Inner_Array_Type(idx2);
  return (idx1 == idx2) || 
    (Get_Type_Inner_Size(idx1) == Get_Type_Inner_Size(idx2)) ||
    (Get_Type_Inner_Size(idx1) == 0) || (Get_Type_Inner_Size(idx2) == 0)
    || (Get_Type_Inner_Size(idx2) == 1);
 ;
  
}


BOOL WN_Has_Valid_Type(WN *wn) {
  
  OPERATOR op = WN_operator(wn);
  return OPERATOR_is_load(op) || op == OPR_TAS || op == OPR_LDA; 

}

//Get the type of the whirl node
//This is mainly a convenience wrapper for checking whether the wn
//may be shared, and thus may ignore operator types are known to never 
//be shared (e.g. consts).
TY_IDX WN_Get_Ref_TY(WN *wn) 
{
  TY_IDX result = 0;
  switch(WN_operator(wn)) {
  case OPR_TAS: return WN_ty(wn);
  case OPR_LDID:
  case OPR_STID:
    if (WN_field_id(wn)) {
      result = Get_Field_Type(WN_ty(wn), WN_field_id(wn));
    } else return WN_ty(wn); 
    break;
  case OPR_ISTORE:
  case OPR_MLOAD:
  case OPR_MSTORE:
    if(WN_field_id(wn)) {
      result = Get_Field_Type(TY_pointed(WN_ty(wn)), WN_field_id(wn));
    } else
      return TY_pointed(WN_ty(wn));
      break;
  case OPR_ILOAD:
    //wei: the opt branch has been fixed to agree with the whirl spec
    // That's fine, but the front-end generates for fields of shared structs
    // fake types to assist the ptr arithmetic generation. There's a lot
   // 	of code that depends on the Get_Field_Type being called. 

    //if(WN_field_id(wn)) {
    //  Print_TY(stderr, TY_pointed(WN_load_addr_ty(wn)));
    //  result = Get_Field_Type(TY_pointed(WN_load_addr_ty(wn)), WN_field_id(wn));
    //  Print_TY(stderr, result);
    //} else
      return WN_ty(wn);
    break;
  case OPR_LDA:
    if(WN_field_id(wn) && TY_kind(TY_pointed(WN_ty(wn))) == KIND_STRUCT) {
      //bug912:  for code like a[i].b[j], we get from front end an LDA with a field id,
      //but the pointed type is not a struct.  WN_ty(wn) has the correct type for the LDA
      return Make_Pointer_Type(Get_Field_Type(TY_pointed(WN_ty(wn)), WN_field_id(wn)));
    } else return WN_ty(wn);
    break;
  case OPR_ARRAY: {
    WN* base = WN_array_base(wn);
    while (WN_operator(base) == OPR_ARRAY) {
      base = WN_array_base(base);
    }
    result = WN_Get_Ref_TY(base);
    break;
  }
  case OPR_ADD:
  case OPR_SUB:
    //we mainly care about pointer-to-shared
    result = WN_Get_Ref_TY(WN_kid0(wn));
    if (TY_kind(result) == KIND_POINTER) {
      return result;
    } 
    result = WN_Get_Ref_TY(WN_kid1(wn));
    break;
  case OPR_COMMA:
    result = WN_Get_Ref_TY(WN_kid1(wn));
    break;
  default: 
    ;
  }
  return result;
}


INT Adjust_Field_Offset(TY_IDX struct_ty, UINT offset) 
{
  UINT adj=0;
  UINT items, size, ofst, displ, adj_displ = 0;
  TY_IDX fld_ty;
  size = TY_size(struct_ty);
  static int may_fail = 0; // for recursive calls on union variants

  if (offset == 0) {
    return 0;
  }

  switch (TY_kind(struct_ty)) {
    
  case KIND_STRUCT:
    {
    
    items = offset / size;
    displ = offset % size;
    adj_displ = 0;
    
    
    FLD_ITER fiter = Make_fld_iter(TY_fld(struct_ty));
    FLD_HANDLE fh;
    FLD_HANDLE f2;

    if (TY_is_union(struct_ty)) {
      // Try each union variant until a match is found
      INT result;
  
      ++may_fail;
      do  {
        fh = FLD_HANDLE(fiter);
        fld_ty = FLD_type(fh);
	FmtAssert(FLD_ofst(fh) == 0, ("Unexpected non-zero union field offset %d", ofst));
	FmtAssert(FLD_adjusted_ofst(fh) == 0, ("Unexpected non-zero adjusted union field offset %d", ofst));
        result = Adjust_Field_Offset(fld_ty, displ);
        fiter++;
      } while ((result < 0) && !FLD_last_field(fh));
      --may_fail;

      //When compiling with optimizations enabled for gcc, some of the
      //intrinsics are written in "assembly" style and use unions 
      // This check is probably too generic ...  (bug 1080)
      // This moved here w/ the fix for bug 2890, but may no longer be needed.
      if (!may_fail && (result < 0)) return offset;

      //FmtAssert(may_fail || (result >= 0), ("Could not find field at the given offset %d", offset));
      return (result < 0) ? result : (items * Adjusted_Type_Size(struct_ty) + result);
    }
    
    //find the field that starts at or covers the current displacement
    
    do  {
      fh = FLD_HANDLE(fiter);
      fld_ty = FLD_type(fh);
      ofst = FLD_ofst(fh);
      adj_displ = FLD_adjusted_ofst(fh);
      fiter++;
    } while (ofst < displ && ofst + TY_size(fld_ty) <= displ && !FLD_last_field(fh));
    
    // At least until we can output bit field types:
    if (FLD_is_bit_field(fh))
      Fail_FmtAssertion("Cannot perform offset adjustment for bitfield at offset %d", offset);

    if (displ  && ofst != displ && ofst + TY_size(fld_ty) > displ) {
      //we are indexing with a const inside a field, need to take into account 
      // the adjusted type size of the field elements; -> fld_ty
      if(TY_kind(fld_ty) == KIND_ARRAY) {
	fld_ty = TY_etype(fld_ty);
	adj_displ += ((displ - ofst) / TY_size(fld_ty)) * Adjusted_Type_Size(fld_ty);
	//for C style direct ptr arithmetic - need to keep the max around
	//see bug 1360
	if(adj_displ < displ)
	  adj_displ += (displ - adj_displ);
      } else if(TY_kind(fld_ty) == KIND_STRUCT) {
	INT result = Adjust_Field_Offset(fld_ty, displ-ofst); 
        FmtAssert(may_fail || (result >= 0), ("Unexpected negative result from Adjust_Field_Offset()"));
	return (result < 0) ? result : (adj_displ + items * Adjusted_Type_Size(struct_ty) + result);
#if 0 // Lightly tested, but currently unreachable.  See bug2112 comment #15
      } else if (FLD_is_bit_field(fh)) {
	//for bitfield other than the first in the same "unit"
	UINT bdispl = 8 * displ;
	UINT bofst;
	do {
	  fh = FLD_HANDLE(fiter);
	  FmtAssert(FLD_is_bit_field(fh), ("Expecting bitfield but found something else"));
	  ofst = FLD_ofst(fh);
	  adj_displ = FLD_adjusted_ofst(fh);
	  bofst = 8 * ofst + FLD_bofst(fh);
	  fiter++;
	} while (bofst < bdispl && bofst + FLD_bsize(fh) <= bdispl && !FLD_last_field(fh));
	// Fail if field does not start on a byte boundary
	if (bofst % 8)
	  Fail_FmtAssertion("Unable to adjust offset for unaligned bitfield %s in struct %s",
			    FLD_name(fh), TY_name(struct_ty));
	// Fail if offset is ambibuous
	if (!FLD_last_field(fh) && (ofst == FLD_ofst(FLD_HANDLE(fiter))))
	  Fail_FmtAssertion("Unable to adjust offset for bitfield at ambiguous offset %d in struct %s",
			    ofst, TY_name(struct_ty));
	adj_displ += (FLD_bofst(fh) / 8);
#endif
      } else {
	if (may_fail)
	  return -1;
	else
	  Fail_FmtAssertion("Could not find field at the given offset %d", offset);
      }
    } else if(FLD_last_field(fh) && ofst + TY_size(fld_ty) == displ) {
      //must be last field and must be padding
      adj_displ = displ;
    }
    
    return adj_displ + items * Adjusted_Type_Size(struct_ty);
    break;
    }
  case KIND_ARRAY:
    // for inititalizers and arrays that are indexed with
    // constant
    size = TY_size(Get_Inner_Array_Type(struct_ty));
    ofst = Adjusted_Type_Size(Get_Inner_Array_Type(struct_ty));
    return (offset / size) * ofst;
    break;
  case KIND_POINTER:
    size = TY_size(TY_pointed(struct_ty));
    ofst = Adjusted_Type_Size(TY_pointed(struct_ty));
    return (offset / size) * ofst;
  default:
    break;
  }

  return offset;
}




TY_IDX lequiv = 0;


//operator returns true when it finds the first occurence of a aggregate type
//that is local and equivalent to the inner type (must be a STRUCT) 
//wei: overload it to also search for name-matching int types (i.e., 
//int != INT32 even if they have the same size)
class TY_find_lequiv {
private:
  TY_IDX ity;

public:
  TY_find_lequiv(TY_IDX idx) {
    ity = idx;
  }

  bool operator() (UINT, const TY* ty) const {
    if(TY_kind(*ty) != TY_kind(ity))
      return FALSE;
    else {
      switch (TY_kind(ity)) {
      case KIND_STRUCT:
      case KIND_SCALAR:
	if(strcmp(TY_name(*ty), TY_name(Ty_Table[ity])) == 0 && !TY_is_shared(*ty)
	   && TY_size(ity) != 0) 
	  return TRUE;
	else return FALSE;
	break;
      default: 
	{
	  Fail_FmtAssertion(0,("",""));
	  return TRUE;
	}
      }
    }
  }
};


// Finds the local equivalent of a shared aggregate type.
// This is useful for struct types as well as integral types
// when we need to distinguish between types with different size (e.g. int v. long).
// It scans the whole type table the first time, and uses 
// cached value later on
#include <map>

static TY_IDX Shared_To_Local_Type_Equiv(TY_IDX actual_ty) 
{
  static std::map<TY_IDX, TY_IDX> shared_to_private_map;
  lequiv = 0;

  if (shared_to_private_map[actual_ty] != 0) {
    return shared_to_private_map[actual_ty];
  }

  lequiv = For_all_until (Ty_Table, TY_find_lequiv(actual_ty));
  if (lequiv != 0) {
    shared_to_private_map[actual_ty] = lequiv;
  }

  return lequiv;
}


//used by the optimizer to convert a shared expression into 
//equivalent private ones
//Note that the function does not handle shared array types
TY_IDX Shared_To_Private_Type(TY_IDX ty_idx) {
  	     
  FmtAssert(TY_is_shared(ty_idx), ("must be a shared type"));
  switch(TY_kind(ty_idx)) {
  case KIND_SCALAR:
    {
      TY_IDX idx = Shared_To_Local_Type_Equiv(ty_idx);
      if (idx == 0) {
	idx = MTYPE_To_TY(TY_mtype(ty_idx));
      }
      return idx;
    }
  case KIND_VOID:
    return MTYPE_To_TY(TY_mtype(ty_idx));
  case KIND_POINTER:
    return Make_Pointer_Type((TY_pointed(ty_idx)));
  case KIND_STRUCT: {
    //search the type table to find the equivalent private type
    TY_IDX idx;
    idx = Shared_To_Local_Type_Equiv(ty_idx);
    if (idx) 
      return idx;
    else {
      idx = Copy_TY(ty_idx);
      Clear_TY_is_shared(idx);
      return idx;
    }
  }
  case KIND_ARRAY:
    FmtAssert(0,("can't convert shared struct/array into private types"));
  }
  return 0;
}

char* append_char(char* str, char ch)
{
  BOOL escape;
  char escaped_ch;
  
  switch (ch)
  {
  case '\n':
     escaped_ch = 'n';
     escape = TRUE;
     break;
  case '\t':
    escaped_ch = 't';
    escape = TRUE;
    break;
  case '\a':
    escaped_ch = 'a';
    escape = TRUE;
    break;
  case '\b':
     escaped_ch = 'b';
     escape = TRUE;
     break;
  case '\r':
     escaped_ch = 'r';
     escape = TRUE;
     break;
  case '\f':
     escaped_ch = 'f';
     escape = TRUE;
     break;
  case '\v':
     escaped_ch = 'v';
     escape = TRUE;
     break;
  case '\\':
     escaped_ch = '\\';
     escape = TRUE;
     break;
  case '\'':
     escaped_ch = '\'';
     escape = TRUE;
     break;
  case '\"':
     escaped_ch = '\"';
     escape = TRUE;
     break;
  default: 
    if (!isprint(ch)) {
      /* special handling for binary output */
      *str++ = '\\';
      //const char * val = utoa(ch).c_str();
      char val[4];
      sprintf(val, "%o", ch);
      for (int i = 0; i < strlen(val); i++) {
	*str++ = val[i];
      }
      return str;
    } else {
      escaped_ch = ch;
      escape = FALSE;
    }
     break;
  }
  if (escape)
     *str++ = '\\';
  *str++ = escaped_ch;

  return str;
} 

/* 
 *  the dbj2 string hash function 
 */
UINT64
string_hash(const char *str)
{
    UINT64 hash = 5381;
    int c;

    while (c = *(unsigned char *)(str++))
	hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}
