#include "upc_mangle_name.h"
#include <map>

static string mangle_type_quals(TY_IDX ty_idx, int do_shared = 1) 
{
  
  string result;
  char buf[64];
  buf[0] = '\0';
  if(TY_is_const(ty_idx))
    result = "K" + result;
  if(TY_is_volatile(ty_idx))
    result = "V" + result;
  if(TY_is_restrict(ty_idx))
    result = "r" + result;
  
  if(do_shared && TY_is_shared(ty_idx)) {
    sprintf(buf, "%d", TY_block_size(ty_idx)); 
    result = "_" + result;;
    result = buf  + result;
    if(TY_is_strict(ty_idx)) {
      result = "S" + result;
    } else 
      result = "R" + result;
  }

  
  return result;
}

static string mangle_base_scalar(TY_IDX ty_idx) 
{
  string result;
  
  switch(TY_mtype(ty_idx)) {
  case MTYPE_V:
    result += "v";
    break;
  case MTYPE_I1:
    result += "c"; 
    break;
  case MTYPE_U1:
    result += "h";
    break; 
  case MTYPE_I2:
    result += "s";
    break;
  case MTYPE_U2:
    result += "t";
    break;
  case MTYPE_I4:
    result += "i";
    break;
  case MTYPE_A4:
  case MTYPE_U4:
    result += "j";
    break;
  case MTYPE_I8:
    result += "x";
    break;
  case MTYPE_A8:
  case MTYPE_U8:
    result += "y";
    break;
  case MTYPE_I16:
    result += "n";
    break;
  case MTYPE_U16:
    result += "o";
    break;
  case MTYPE_B:
    result += "b";
    break;
  case MTYPE_F4:
    result += "f";
    break;
  case MTYPE_F8:
    result += "d";
    break;
  case MTYPE_F10:
    result += "e";
    break;
  case MTYPE_F16:
    result += "g";
    break;
  default:
    result = "Invalid.mtype";
  }
  
  return mangle_type_quals(ty_idx) + result;
} 


class CMP_NAME {
public:
  BOOL operator() (const string&, const string&) const;
};


BOOL CMP_NAME::operator() (const string &n1, const string  &n2) const 
{
  return strcmp(n1.data(), n2.data()) < 0;
}


map<TY_IDX, string> ty_mangle_map;
map<string, int, CMP_NAME> string_mangle_map;

TY_IDX  TY_For_Name(string name) {
  return string_mangle_map[name];
}



//for any TY_IDX factor the alignment out
BOOL Type_Not_Mangled(TY_IDX ty) 
{
  return ty_mangle_map[ty & (~TY_ALIGN) ].empty();
}


string Mangle_Type(TY_IDX ty_idx) 
{
  string result;
  char *name;
  char len[32];
  TY_IDX ity = ty_idx & (~TY_ALIGN);
  result = ty_mangle_map[ity];
  if(!Type_Is_Shared_Ptr(ty_idx)) {
    //Fail_FmtAssertion("Type is not shared in Mangle_Shared_Type");
  }

  if(result.empty()) {
    
    switch(TY_kind(ty_idx)) {
    case KIND_SCALAR:
    case KIND_VOID:
      result = mangle_base_scalar(ty_idx);
      break;
    case KIND_POINTER:
      result = Mangle_Type(TY_pointed(ty_idx));
      result = 'P' + result;
      result =  mangle_type_quals(ty_idx) +  result; 
      break;
    case KIND_ARRAY: {
      TY_IDX ety = TY_etype(ty_idx);
      sprintf(len, "%lld", TY_adjusted_size(ty_idx)/TY_adjusted_size(ety));
      if(TY_uses_threads(ty_idx))
	strcat(len,"H");
      result = mangle_type_quals(ty_idx, 0 /*TY_kind(ety) != KIND_ARRAY*/) + "A" + len + "_" + Mangle_Type(ety);
      } break;
    case KIND_STRUCT:
      name = TY_name(ty_idx);
      //check for typedef names whose underlying type is not written to the
      // output file (looks like "T orig_name")
      if(/*!TY_is_written(ty_idx) &&*/ name[0] == 'T' && name[1] == ' ')
	name = name+2;
      sprintf(len,"%d", (int) strlen(name));
      result = mangle_type_quals(ty_idx) + "N" + len + name;
      break;
   
    default:
      break;
    }
    
    ty_mangle_map[ity] +=  result; 
    if(string_mangle_map[result] == 0) {
      string_mangle_map[result] = ity;
    }
    
  } 

  return result;
}
