#ifndef UPC_FORALL_INCLUDED
#define UPC_FORALL_INCLUDED
#include "ara.h"

extern void Upc_Forall_Opt(WN *);

//This class represents UPC affinity expressions of the form &a[i + exp],
//where i the induction variable of the forall loop, and exp is invariant 
//within the forall loop.  integer affinity expressions are modeled as if 
//they have a a cyclic base array

class UPC_AFF_EXP {
 private:
  WN* _aff_exp;
  WN* _base_arr;
  //the part of expression without the base array.
  //For int affinity expression it is the same as aff_exp
  WN* _index_exp; 
  WN* _ind_var; //expression of the induction variable
  int _bsize;
  int _scale;
 
 public:
  
  UPC_AFF_EXP(WN* aff_exp, WN* ind_var, int scale) {

    _aff_exp = aff_exp;
    Find_base_and_index();
    if (_base_arr == NULL) {
      _bsize = 1;
    } else {
      TY_IDX ty = WN_Get_Ref_TY(_base_arr);
      FmtAssert(TY_kind(ty) == KIND_POINTER && TY_is_shared(TY_pointed(ty)), 
		("base array type is not shared pointer"));
      _bsize = Get_Type_Block_Size(TY_pointed(ty));
    }
    _ind_var = ind_var;
    _scale = scale;
  }

  WN* Aff_exp() { return _aff_exp; }
  WN* Base_arr() { return _base_arr; }
  WN* Index_exp() { return _index_exp; }
  ST* Ind_var() { return WN_st(_ind_var); }
  WN_OFFSET Ind_var_ofst() { return WN_idname_offset(_ind_var); }
  int Bsize() { return _bsize; } 
  int Scale() { return _scale; }
  void Find_base_and_index();
  bool Base_aligned(WN* arr);

  void Print(FILE* file = stderr);
  
};

#endif
