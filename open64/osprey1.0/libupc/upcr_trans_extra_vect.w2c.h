#ifndef __UPCR_VECT_INTERNAL_W2C_H__
#define __UPCR_VECT_INTERNAL_W2C_H__

#include <upcr.h>
#include <string.h>
#include <assert.h> 

#define upcrt_assert assert


#define UPCRV_INLINE GASNETT_INLINE



#define UPCRT_MAX_DIM 10
#define UPCRT_QUEUE_DEPTH 32

#define UPCRT_T_CONTIG 0
#define UPCRT_T_FSTRIDE 1
#define UPCRT_T_STRIDEN 2
#define UPCRT_T_ILIST 3

#define UPCRT_T_PERSISTENT_MEM 0x100 /* don't free local buffers at the end
					of the lifetime of a transfer, the buffer
					is supplied by the application */
#define UPCRT_T_TRANSIENT_MEM 0x200 
#define UPCRT_T_TYPE_MASK 0xf

#define UPCRT_REDIST_REF 1
#define UPCRT_DEFAULT_PEER_REF -1
#define UPCRT_INVALID_REF -1

typedef struct upcrt_ttype {
  int type;
  upcr_shared_ptr_t local;
  upcr_shared_ptr_t remote;
  int islocal;
  int op_type; 
  bupc_handle_t handles[UPCRT_QUEUE_DEPTH];
  int active;
  int size;
  int esize;
  int Nref;
  int Nop;
} upcrt_TransDesc;


typedef struct upcrt_contigtrans {
  int type;
  upcr_shared_ptr_t local;
  upcr_shared_ptr_t remote;
  int islocal;
  int op_type; /*read,write; */
  bupc_handle_t handles[UPCRT_QUEUE_DEPTH];
  int active;
  int size;
  int esize;
  int Nref;
  int Nop;
  int dir; /*up,down*/
  int nstrips;  
  int pad; 
  int span; /*span=nstrips*size + pad*/
  int init_comm;
  int wait_comm;
  int burst;
  int skip_init; /*  how many iterations to skip before initiating 
		any other communication */
  int stage_init;
  int skip_sync;
  int stage_sync;
} upcrt_ContigTrans;


typedef struct upcrt_fstridetrans {
  int type;
  upcr_shared_ptr_t local;
  upcr_shared_ptr_t remote;
  int islocal;
  int op_type; /*read,write;*/  
  bupc_handle_t handles[UPCRT_QUEUE_DEPTH];
  int active;
  int size;
  int esize;
  int Nref;
  int Nop;
  int dir; /*up,down*/
  int nstrips;
  int pad; 
  int span; /*span=nstrips*size + pad*/ 
  int cur_strip;
  size_t dstchunnklen;
  size_t dstchunkstride;
  size_t dstchunkcount;
  size_t srcchunnklen;
  size_t srcchunkstride;
  size_t  srcchunkcount;
  
  /*need to  add something to describe the padding;*/

}  upcrt_FstrideTrans;


typedef struct upcrt_strideNtrans {
  int type;
  upcr_shared_ptr_t local;
  upcr_shared_ptr_t remote;
  int islocal;
  int op_type; /*read,write;*/
  bupc_handle_t handles[UPCRT_QUEUE_DEPTH];
  int active;
  int size;
  int dir; /*up,down*/
  int nstrips;
  int pad; 
  int span; /*span=nstrips*size + pad*/
 
  int cur_strip;
   
  size_t dststride[UPCRT_MAX_DIM];
  size_t srcstride[UPCRT_MAX_DIM];
  size_t count[UPCRT_MAX_DIM];
  size_t stridelevels;
  int try_model;
  int contig; /* holds the length in bytes of the contiguous part */
  int chunks;
}  upcrt_StrideNTrans;



typedef struct upcrt_vec {
  int cnt;
  void *dummy;  
}  upcrt_Vec;

typedef intptr_t   upcr_key_t;
typedef int   upcr_nest_descr_t;
typedef int  upcr_ref_descr_t;
typedef int  upcr_lmad_descr_t;

typedef struct upcrt_dim {
  int stride;
  int span;
  int init_pos; /* original position in the descriptor, */
                /* corresponds also to the loop nest order*/ 
} upcrt_Dim;

typedef struct upcrt_Dim_Vec {
  int cnt;
  upcrt_Dim elem[UPCRT_MAX_DIM];  
}  upcrt_DimVec;

typedef struct upcrt_Int_Vec {
  int cnt;
  int elem[UPCRT_MAX_DIM];
}  upcrt_IntVec;


#define UPCRT_STRIDE_UNKNOWN 0
#define UPCRT_STRIDE_INCR 1   /* e.g. (1,N) large stride gets traversed first */
#define UPCRT_STRIDE_DECR 2   /* e.g (N,1) small stride gets traversed first */
#define UPCRT_STRIDE_INTV 3   /* e.g (N,1,2) */
 

typedef struct upcrt_lmad {
  upcrt_DimVec dvec; /*original descriptor*/
  int depth; /*depth in the nest*/
  int  disp;
  int total_span;
  upcr_shared_ptr_t local;
  upcr_shared_ptr_t remote;
  int init_pos;
  int order; /* order of dimensions for dvec */
  int type; /* read/write*/
  upcrt_IntVec equiv_read;
  upcrt_IntVec equiv_write;
  upcr_ref_descr_t redist_ref;
}  upcrt_Lmad;

typedef struct upcrt_Lmad_Vec {
  int cnt;
   upcrt_Lmad *elem[UPCRT_MAX_DIM];  
}  upcrt_LmadVec;



/* All references that have the same base address in the */
/* original program should go within the same descriptor*/
/* if we know that they are unaliased.*/
/* All base references for which there's no precise alias information*/
/* go within the same desc. Question is how to deal efficiently with them.*/
typedef struct upcrt_Ref_Desc {
  upcrt_LmadVec desc;
  upcrt_LmadVec comm;
  size_t esize;
  int alias;                   /* whether the base pointer is restrict*/
  upcr_ref_descr_t peer_ref;  /* for redistribution refs this points to the 
				 peer reference class */
  int type;                   /* redist refs have this set so we don't waste time analyzing */
}  upcrt_RefDesc; 

typedef struct upcrt_Ref_Vec {
  int cnt;
  upcrt_RefDesc elem[UPCRT_MAX_DIM];
}  upcrt_RefVec;

typedef struct upcrt_Bnds {
  long lb;
  long ub;
  long stride;
} upcrt_Bounds;

typedef struct upcrt_polytope {
  int cnt;
  upcrt_Bounds elem[UPCRT_MAX_DIM];
}  upcrt_Polytope;


typedef struct upcrt_Comm_Vec {
  int cnt;
  upcrt_TransDesc *elem[UPCRT_MAX_DIM];
}  upcrt_CommVec;



typedef struct upcrt_Loop_Nest {
  upcrt_RefVec refs;
  upcrt_Polytope bounds;
  upcrt_CommVec comm_read[UPCRT_MAX_DIM];
  upcrt_CommVec comm_write[UPCRT_MAX_DIM];
  int isredist;
  int Nref;
  int Nop;
}  upcrt_LoopNest;

typedef struct upcrt_Loop_Vec {
  int cnt;
  upcrt_LoopNest elem[UPCRT_MAX_DIM];
}  upcrt_LoopNestVec; 


typedef int upcrt_pair[2];

#define UPCRT_REF_READ 1
#define UPCRT_REF_WRITE 2
#define UPCRT_REF_RDWR 3

void _upcrt_add_sos_dim(upcr_nest_descr_t nest, upcr_ref_descr_t ref, upcr_lmad_descr_t lmad, 
			long dim, long stride, long elc);
int _upcrt_analyze_transfers( upcr_nest_descr_t loopnest );
int _upcrt_analyze_redist( upcr_nest_descr_t loopnest);
void _upcrt_finalize_dim(upcr_nest_descr_t, int);
size_t upcrt_get_strips(upcr_nest_descr_t);
int upcrt_get_coeff(upcr_nest_descr_t loopnest, upcr_ref_descr_t lref, upcr_lmad_descr_t lmad, 
		    int dim);
void *upcrt_get_address_1RS1(void);
/* #define upcrt_loop_1R(A,B,C,D,E,F,G,H,I) _upcrt_loop_1R((A),(B),(C),(D),(E),(F),(G),(H),(I)) */
void upcrt_loop_1R(int LB, int UB, int stride, int type, upcr_shared_ptr_t remote, int esize, int Nref, int Nop);
void _upcrt_finalize_1RS1(void); 
int _upcrt_analyze_1RS1(void);
int upcrt_strips_1RS1(void);
void _upcrt_advance_1RS1(void);
extern void upcrt_set_a2a(int);
void upcrt_Init_Comm_StrideN(upcrt_StrideNTrans *);
void upcrt_Init_Comm_StrideNWrite(upcrt_StrideNTrans *);
void upcrt_Advance_Transfer_Contig(upcrt_ContigTrans *);
upcr_nest_descr_t upcrt_start_nest(upcr_key_t key, int isredist);
void _upcrt_end_nest( upcr_nest_descr_t loopnest);
void _upcrt_start_transfers(upcr_nest_descr_t nest);
void _upcrt_advance_dim(upcr_nest_descr_t loopnest, int dim);
upcr_ref_descr_t  upcrt_new_redist_ref(upcr_nest_descr_t nest, upcr_ref_descr_t peer_ref,
				       upcr_lmad_descr_t peer_lmad,
				       int alias, size_t esize);
upcr_lmad_descr_t _upcrt_new_lmad_local(upcr_nest_descr_t nest, 
				 upcr_ref_descr_t ref, 
					void *base, int type);
upcr_lmad_descr_t upcrt_new_lmad(upcr_nest_descr_t nest, 
				 upcr_ref_descr_t ref, 
				 upcr_shared_ptr_t base, int type);
upcr_ref_descr_t  upcrt_new_base_ref(upcr_nest_descr_t nest, int alias, size_t esize);
void upcrt_add_polytope_dim(upcr_nest_descr_t nest, long depth, long lbd, long ubnd, 
			    long stride);
void* upcrt_get_local_address(upcr_nest_descr_t loopnest, 
			     upcr_ref_descr_t lref, 
			      upcr_lmad_descr_t lmad);
#define upcrt_add_sos_dim( nest, ref, lmad, dim, stride, elc) \
 _upcrt_add_sos_dim((nest), (ref), (lmad), (dim), (stride), (elc))
#define upcrt_analyze_transfers(ln) _upcrt_analyze_transfers((ln))
#define upcrt_analyze_redist(ln) _upcrt_analyze_redist((ln))
#define upcrt_finalize_dim(ln, dim) _upcrt_finalize_dim((ln), (dim)) 
#define upcrt_analyze_1RS1() _upcrt_analyze_1RS1()
#define upcrt_advance_1RS1() _upcrt_advance_1RS1()
#define upcrt_finalize_1RS1() _upcrt_finalize_1RS1()
#define upcrt_new_lmad_local(nest, ref, base, type) \
  _upcrt_new_lmad_local((nest), (ref), (base), (type)) 
#define upcrt_advance_dim(ln, dim) _upcrt_advance_dim((ln), (dim))
#define upcrt_end_nest(ln) _upcrt_end_nest((ln)) 
#define upcrt_start_transers(ln) _upcrt_start_transfers((ln)) 
#define  upcrt_get_local_address(ln, lref,lmad) \
       _upcrt_get_local_address((ln), (lref),(lmad))  


/* extern upcrt_LoopNestVec upcrt_program; */
/* extern int upcrt_print_targets; */
/* extern int upcrt_all2all; */

/*The cur_* variables are valid only during the */
/*construction phase. They are used to cache the address*/
/*of the current insertion point. */

/* extern upcrt_LoopNest *upcrt_cur_nest;  */
/* extern upcrt_RefDesc  *upcrt_cur_ref; */
/* extern upcrt_Lmad     *upcrt_cur_lmad; */
/* extern upcrt_DimVec      *upcrt_cur_dim; */
/* extern bupc_tick_t start_comm, end_comm; */
/* extern bupc_tick_t start_desc, end_desc; */
/* extern int total_calls; */


GASNETT_FORMAT_PRINTF(upcrt_err,1,2,
static void upcrt_err(const char *msg, ...) GASNETT_NORETURN);
static void upcrt_err(const char *msg, ...)
{
    va_list args;
    char buf[1000];

    va_start(args, msg);
    vsprintf(buf, msg, args);
    va_end(args);

    fflush(stdout);
    /* NOTE: the test harness scans for this string */
    fprintf(stderr, "UPC Runtime error: %s\n", buf);
    fflush(stderr);
    upcr_global_exit(-1);
}



UPCRV_INLINE(Set_TransDesc_Lifetime)
     void Set_TransDesc_Lifetime(upcrt_TransDesc* t, int scope)
{
  t->type |= scope;
}

UPCRV_INLINE(Set_TransDesc_Lifetime)
     void Set_TransDesc_Type(upcrt_TransDesc* t, int type)
{
  t->type |= type;
}


UPCRV_INLINE(Inc_Cnt)
void Inc_Cnt(upcrt_Vec *v) {
  v->cnt++;
}

UPCRV_INLINE(Elems)
int Elems(upcrt_Vec *v)
{
  return v->cnt;
}



UPCRV_INLINE(Dec_Cnt)
void Dec_Cnt(upcrt_Vec *v) {
  v->cnt--;
}

UPCRV_INLINE(Add_Element)
int  Add_Element(upcrt_Vec *v, void *elem, size_t elsz)
{
  char *mem; 
  upcrt_assert(v->cnt < UPCRT_MAX_DIM);
  
  mem = (char*)(&v->dummy);
  mem =  mem + elsz*v->cnt;
  memcpy(mem, elem, elsz);
  
  return v->cnt++;
}



UPCRV_INLINE(Add_IntElement)
int  Add_IntElement(upcrt_IntVec *v, void *elem, size_t elsz)
{
  
  upcrt_assert(v->cnt < UPCRT_MAX_DIM);
 
  v->elem[v->cnt] = *(int*)elem;
  return v->cnt++;
}


UPCRV_INLINE(Summary_Order)
int Summary_Order (int o1, int o2)
{
  if(o1 == o2)
    return o1;
  else 
    return UPCRT_STRIDE_UNKNOWN;
}

UPCRV_INLINE(Comp_Dims)
int Comp_Dims(const void *e1, const void *e2)
{
  upcrt_Dim *d1 = (upcrt_Dim*)e1;
  upcrt_Dim *d2 = (upcrt_Dim*)e2;

  if(d1->stride < d2->stride) 
    return -1;
  if(d1->stride == d2->stride)
    return 0;
  if(d1->stride > d2->stride)
    return 1;
  
  return 0;
}

UPCRV_INLINE(Comp_Base_Addr)
int Comp_Base_Addr(const void *e1, const void *e2)
{
  upcrt_Lmad *l1, *l2;
  ptrdiff_t result;
  l1 = *(upcrt_Lmad **)e1;
  l2 = *(upcrt_Lmad **)e2;
   
  result =  upcr_sub_psharedI(upcr_shared_to_pshared(l1->remote), upcr_shared_to_pshared(l2->remote),1);
  if(result < 0)
    return -1;
  else if (result > 0)
    return 1;
  else
    return 0;
}

UPCRV_INLINE(Can_Coalesce)
int Can_Coalesce(upcrt_Dim *d1, upcrt_Dim* d2) 
{
  
  if((d2->stride % d1->stride == 0) && (d2->stride <= d1->stride+d1->span))
    return 1;

  return 0;
  
}



UPCRV_INLINE(Init_Comm_ContigWrite)
void Init_Comm_ContigWrite(upcrt_ContigTrans *t)
{
  UPCR_BEGIN_FUNCTION();
  t->handles[t->active] = bupc_memput_async(t->remote, 
					    upcr_shared_to_local(t->local),
					    t->size);
  t->active++;
  upcrt_assert(t->active < UPCRT_QUEUE_DEPTH);
}

UPCRV_INLINE(Init_Comm_Contig)
void Init_Comm_Contig(upcrt_ContigTrans *t)
{
  UPCR_BEGIN_FUNCTION();
  upcrt_assert((t->type & UPCRT_T_TYPE_MASK)  == UPCRT_T_CONTIG);
 
  if(UPCRT_REF_READ == t->op_type ) {
    t->handles[t->active] = bupc_memget_async(upcr_shared_to_processlocal(t->local),
					      t->remote, 
					      t->size);   
  } else  
    if(UPCRT_REF_WRITE ==  t->op_type) {
      if(t->type & UPCRT_T_PERSISTENT_MEM) return; 
      t->handles[t->active] = bupc_memput_async(t->remote, 
					     upcr_shared_to_local(t->local),
					     t->size);
       
    } else upcrt_err("Hybrid transfer request \n");
  
  t->active++;
  upcrt_assert(t->active < UPCRT_QUEUE_DEPTH);
  
}



UPCRV_INLINE(Update_Lmad_Local_Address)
void Update_Lmad_Local_Address(int l, upcrt_RefDesc *ref, upcr_shared_ptr_t addr, size_t esize)
{
  upcrt_assert(l < ref->desc.cnt);
  
  ref->desc.elem[l]->local = upcr_pshared_to_shared(
                                    upcr_add_psharedI(upcr_shared_to_pshared(addr), 
								     esize,
								     ref->desc.elem[l]->disp));
}

UPCRV_INLINE(Update_Peer_Local_Address)
void Update_Peer_Local_Address(upcrt_Lmad *l, upcrt_RefDesc *ref, 
				      upcr_shared_ptr_t addr)
{
  UPCR_BEGIN_FUNCTION();
  int i;
  int peer;
  for(i = 0; i < Elems((upcrt_Vec*)&l->equiv_read); i++) {
    peer = l->equiv_read.elem[i];
    Update_Lmad_Local_Address(peer, ref, addr, ref->esize);
  }
  for(i = 0; i < Elems((upcrt_Vec*)&l->equiv_write); i++) {
    peer = l->equiv_write.elem[i];
    Update_Lmad_Local_Address(peer, ref, addr, ref->esize);
  }
}



UPCRV_INLINE(Advance_Transfer)
void Advance_Transfer(upcrt_TransDesc *t) 
{
  UPCR_BEGIN_FUNCTION();
  int i;
  
  for(i = 0; i < t->active; i++) {
    bupc_waitsync(t->handles[i]);
    /* end_comm += (bupc_ticks_now() -  start_comm); */
  }
  
  t->active = 0;
}

upcrt_LoopNestVec*  UPCR_TLD_DEFINE_TENTATIVE(upcrt_program, sizeof(void*), 8);
upcrt_LoopNest*    UPCR_TLD_DEFINE_TENTATIVE(upcrt_cur_nest, sizeof(void*), 8);
upcrt_RefDesc*     UPCR_TLD_DEFINE_TENTATIVE(upcrt_cur_ref, sizeof(void*),8);
upcrt_Lmad*        UPCR_TLD_DEFINE_TENTATIVE(upcrt_cur_lmad, sizeof(void*), 8);
upcrt_DimVec*      UPCR_TLD_DEFINE_TENTATIVE(upcrt_cur_dim, sizeof(void*), 8);
upcrt_ContigTrans*  UPCR_TLD_DEFINE_TENTATIVE(upcrt_1RS1, sizeof(void *), 8);
upcrt_FstrideTrans* UPCR_TLD_DEFINE_TENTATIVE(upcrt_1RSN, sizeof(void *), 8);

extern void upcrt_vect_thread_init(void);

bupc_tick_t UPCR_TLD_DEFINE_TENTATIVE(start_anal, 8,8);
bupc_tick_t UPCR_TLD_DEFINE_TENTATIVE(end_anal, 8,8);
bupc_tick_t UPCR_TLD_DEFINE_TENTATIVE(start_comm,8,8);
bupc_tick_t UPCR_TLD_DEFINE_TENTATIVE(end_comm, 8,8);
bupc_tick_t  UPCR_TLD_DEFINE_TENTATIVE(start_desc, 8,8);
bupc_tick_t UPCR_TLD_DEFINE_TENTATIVE(end_desc, 8,8);
int UPCR_TLD_DEFINE_TENTATIVE(total_calls, sizeof(int),8);

#endif 
