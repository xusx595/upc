/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

//Implement the variable name mangling capabilities for Totalview support  

#ifndef upc_mangle_name_INCLUDED
#define upc_mangle_name_INCLUDED

#include "symtab.h"
#include <string>

extern string Mangle_Type(TY_IDX idx);
extern BOOL Type_Not_Mangled(TY_IDX ty) ;
extern TY_IDX  TY_For_Name(string name);

#endif /* upc_mangle_name_INCLUDED */

