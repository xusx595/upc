#include <symtab.h>
#include <symtab_utils.h>
#include <upc_coalescing.h>
#include <wintrinsic.h>
#include "wn.h"
#include "wn_util.h"
#include "ir_reader.h" // for fdump_tree
#include "tracing.h"

GET::GET(WN* wn, sync_handle_t* handle) {

  FmtAssert(WN_operator(wn) == OPR_STID, (""));
  _size = 0;
  _offset = INT_MAX;
  _get = wn;
  _handle = handle;
  WN* call = WN_first(WN_kid0(WN_kid0(_get)));
  FmtAssert(WN_operator(call) == OPR_INTRINSIC_CALL, (""));
  _is_pshared = WN_intrinsic(call) == INTRN_SMLD_NB ? false : true;
  _dst = WN_st(WN_kid0(WN_kid0(call)));
  _src = WN_kid0(WN_kid1(call));

  if (WN_operator(WN_kid0(WN_kid2(call))) == OPR_INTCONST) {
    _offset = WN_const_val(WN_kid0(WN_kid2(call)));
  } 

  //check for pointer add of the form p + c,
  //where p is an indefinite pointer and c is a constant.
  //For this special case, we set the _src = p and _offset = c
  //to allow for more opportunities for coalescing.
  //unfortunately the expression (p+c) has been fully expanded into 
  //upc_add_psharedI() at this point, so pattern matching becomes more complex here
  //FIXME: can we delay the upc add lowering to simplify the pattern matching
  //(and hopefully allow coalescing on non constant index)?
  if (WN_operator(_src) == OPR_COMMA) {
    WN* stmt = WN_first(WN_kid0(_src));
    if (WN_operator(stmt) == OPR_INTRINSIC_CALL &&
	WN_intrinsic(stmt) == INTRN_ADD_PI) {
      //fdump_tree(stderr, stmt);
      WN* base = WN_kid0(WN_kid0(stmt));
      WN* size = WN_kid0(WN_kid1(stmt));
      WN* offst = WN_kid0(WN_kid2(stmt));
      if (WN_operator(size) == OPR_INTCONST &&
	  WN_operator(offst) == OPR_INTCONST) {
	if (_offset != INT_MAX) {
	  //we do have a valid integer byte offset here
	  _offset += WN_const_val(size) * WN_const_val(offst);
	  _src = base;
	}
      }
    }
  }
      

  if (WN_operator(WN_kid0(WN_kid3(call))) == OPR_INTCONST) {
    _size = WN_const_val(WN_kid0(WN_kid3(call)));
  } 
  
}

void GET::Print(FILE* out) {

  fprintf(out, "offset = %d\n", _offset);
  fprintf(out, "size = %d\n", _size);
  fprintf(out, "local dst: %s\n", ST_name(_dst));
  fprintf(out, "src wn: \n");
  fdump_tree(out, _src);
    
  //fprintf(out, "WN for upc_get:\n");
  //fdump_tree(out, _get);
}

bool
COMM::Coalesce(WN* block) {

  if (_gets.Elements() == 1) {
    return false;
  }

  int begin = INT_MAX, end = INT_MIN;
  for (int i = 0; i < _gets.Elements(); i++) {
    if (begin > _gets[i]->Offset()) {
      begin = _gets[i]->Offset();
    }
    if (end < _gets[i]->Offset() + _gets[i]->Size()) {
      end = _gets[i]->Offset() + _gets[i]->Size();
    }
  }

  //For now, do not coalesce if the bounding box is > 1024 bytes
  //Eventually we should have a network-specific performance model to
  //determine the size of the bounding box
  if (end - begin > 1024) {
    return false;
  }
  _offset = begin;
  _size = end - begin;
  fprintf(TFile, "bounding box: [%d, %d]\n", begin, end);

  //generate the coalesced get
  WN* ofst = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, _offset),
			   MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN* size = WN_CreateParm(Integer_type, WN_Intconst(Integer_type, _size),
			   MTYPE_To_TY(Integer_type), WN_PARM_BY_VALUE);
  WN* src_addr = WN_CreateParm(Pointer_Mtype, WN_COPY_Tree(_gets[0]->Src()),
			       WN_ty(_gets[0]->Src()), WN_PARM_BY_VALUE);
  // For bug 2945 - preserve alignment of size.  This is NOT sufficient to
  // entirely fix the problem if, for instance, an 8-byte type is part of
  // a 16byte = (4 + 8 + 4) transfer.
  TYPE_ID dst_mtype;
  if (!(_size % 8)) {
    dst_mtype = MTYPE_I8;
  } else if (!(_size % 4)) {
    dst_mtype = MTYPE_I4;
  } else if (!(_size % 2)) {
    dst_mtype = MTYPE_I2;
  } else {
    dst_mtype = MTYPE_I1;
  }
  TY_IDX dst_type = Get_Array_Type(MTYPE_To_TY(dst_mtype), _size);
  _dst = Gen_Temp_Symbol(dst_type, "COALESCED");
  WN* dst_addr = WN_CreateParm(Pointer_Mtype, WN_Lda(Pointer_Mtype, 0, _dst, 0),
			       Make_Pointer_Type(MTYPE_To_TY(MTYPE_I1)), WN_PARM_BY_VALUE);

  //For testing purposes, use blocking call for now  
  //WN* coalesced_call = WN_Create(OPR_INTRINSIC_CALL, TY_mtype(upc_hsync_mem_ty), MTYPE_V, 4);
  //WN_intrinsic(coalesced_call) = _gets[0]->Is_pshared() ? INTRN_GET_NB_P : INTRN_SMLD_NB;
  WN* coalesced_call = WN_Create(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V, 4);
  WN_intrinsic(coalesced_call) = _gets[0]->Is_pshared() ? INTRN_GET_P : INTRN_SMLD;
  WN_kid0(coalesced_call) = dst_addr;
  WN_kid1(coalesced_call) = src_addr;
  WN_kid2(coalesced_call) = ofst;
  WN_kid3(coalesced_call) = size;
  
  //fdump_tree(stderr, coalesced_call);
  WN_INSERT_BlockAfter(block, _gets[0]->Get_stmt(), coalesced_call);

  //Now need to fix up the local destination
  for (int i = 0; i < _gets.Elements(); i++) {
    int diff = _gets[i]->Offset() - _offset;
    TY_IDX ty = ST_type(_gets[i]->Dst());
    WN* wn = WN_Add(Pointer_Mtype, WN_Lda(Pointer_Mtype, 0, _dst, 0),
		    WN_Intconst(Integer_type, diff));
    wn = WN_Iload(TY_mtype(ty), 0, ty, wn, 0);
    wn = WN_Stid(TY_mtype(ty), 0, _gets[i]->Dst(), ty, wn, 0);
    //fdump_tree(stderr, wn);
    WN_INSERT_BlockAfter(block, coalesced_call, wn);
    _gets[i]->Update_sync_handle(NULL);
  }

  return true;


}


//Both wn1 and wn2 must be shared addresses
//Note that since the get calls (wn1 and wn2) are next to each other,
//wn1 and wn2 are guaranteed to produce the same address as long as 
//they have the same whirl tree
bool Get_addr_are_equivalent (WN* wn1, WN* wn2) {


  if (WN_opcode(wn1) != WN_opcode(wn2)) {
    return false;
  }

  switch (WN_operator(wn1)) {
  case OPR_LDID:
    if (WN_st(wn1) == WN_st(wn2) && WN_load_offset(wn1) == WN_load_offset(wn2)) {
      return true;
    }
    return false;
  case OPR_ILOAD:
  case OPR_MLOAD:
    if (WN_load_offset(wn1) != WN_load_offset(wn2)) {
      return false;
    }
    break;
  case OPR_TAS:
    if (WN_ty(wn1) != WN_ty(wn2)) {
      return false;
    }
    break;
  case OPR_INTCONST:
    return WN_const_val(wn1) == WN_const_val(wn2);
  }

  for (int i = 0; i < WN_kid_count(wn1); i++) {
    if (!Get_addr_are_equivalent(WN_kid(wn1, i), WN_kid(wn2, i))) {
      return false;
    } 
  }
  
  return true;
}
