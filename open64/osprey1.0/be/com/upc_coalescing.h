#ifndef upc_coalescing_INCLUDED
#define upc_coalescing_INCLUDED

/****
 *
 *  Implement coalescing for individual upcr_get calls
 *  The optimization operates on a "communication point" basis (a 
 *  communication point is defined as a sequence of get calls).  If two get
 *  calls from the same communication point access contiguous memory, we combine 
 *  them into a single call.  The corresponding synchronization handles are updated,
 *  as well as the temporary variables that store the value.  e.g., 
 *    h1 = get (tmp1, a[0], 4);
 *    h2 = get (tmp2, a[1], 4);
 *    ...
 *    sync(h1);
 *    ...tmp1;
 *  is converted to
 *    h = get (tmp, a[0], 8);
 *    ...
 *   sync (h);
 *   ...tmp
 *
 *
 *   Note that we don't perform any symbolic analysis at this point, so the size and offset of the get call
 *   must be compile time constants for them to be coalesced.  In practice this means only pointer accesses 
 *   (e.g. p->x and p->y) may be coalesced;  array accesses (e.g a[i] and a[i+1]) can't be coalesced 
 *   since a) they may not be on the same processor and b) they have different base addresses (offset is zero 
 *   for both)
 */

#include <symtab_utils.h>
#include <upc_symtab_utils.h>
#include <upc_wn_util.h>
#include "wn.h"
#include <cxx_template.h>
#include "limits.h"


//Test whether two WN trees representing the base address in get() calls 
//are structurally equivalent (i.e., they are from the same C expressions)
bool Get_addr_are_equivalent(WN* wn1, WN* wn2);

class GET {
 private:
  WN* _get;
  sync_handle_t *_handle;
  int _offset;
  int _size;
  ST* _dst; //the local tmp var being stored to
  WN* _src; //the remote source xaddress
  bool _is_pshared;

 public:
  //wn must be of the form sync = get(...)
  GET(WN* wn, sync_handle_t *handle);

  WN* Get_stmt() { return _get; }
  sync_handle_t *Sync_handle() { return _handle; }
  void Update_sync_handle(ST* st) { _handle->st = st; }
  int Offset() { return _offset; }
  int Size() { return _size; }
  //Either the offset or size is not a compile time constant
  //Not much we can do about them
  bool Has_const_bound() { return _size != 0 && _offset != INT_MAX; }
  bool Is_pshared() { return _is_pshared; }
  ST* Dst() { return _dst; }
  WN* Src() { return _src; }

  void Print(FILE* out);
};

class COMM {
  
 private:
  ST* _dst;  //ST for the local destination  
  int _offset;
  int _size;
  DYN_ARRAY<GET*> _gets; //list of gets with the same base address
  
 public:

  COMM(MEM_POOL *pool) : _gets(pool) {
    _size = 0;
  }

  ~COMM() { 
    for (int i = 0; i < _gets.Elements(); i++) {
      delete _gets[i];
    }
    _gets.Free_array(); 
  }

  WN* Base() { return _gets[0]->Src(); }
  int Offset() { return _offset; }
  int Size() { return _size; }
  ST* Dst() { return _dst; }
  void Set_dst(ST* st) { _dst = st; }
  bool Is_coalesced() { return _size > 0; }
  void Add_get(GET *get) { _gets.AddElement(get); }
  int Num_get() { return _gets.Elements(); }
  GET* Nth_get(int i) { return _gets[i]; }

  //Performs coalescing on the chunks listed in _gets
  bool Coalesce(WN* block);  
};
  
#endif //upc_coalescing_INCLUDED
 
    
