/* ------- extensions for nonblocking memputs ----------- */

#define UPCRT_AUTO_NB_STATS 1

typedef struct _upcrt_nb_put {
  upcr_handle_t handle;
  upcr_shared_ptr_t dst;
  size_t nbytes;
#ifdef UPCRT_AUTO_NB_STATS
  bupc_tick_t start;
#endif
} upcrt_put_t;

typedef struct _upcrt_nb_put_array {
  unsigned num_puts;
  upcrt_put_t *arr;
} upcrt_nb_put_array_t;

/* Put all global variables in a struct to reduce the TLD overhead */
typedef struct {

  /* The main data structure, where the ith element 
   * is an array of outstanding memputs to the destination node i.
   */
  upcrt_nb_put_array_t *upcrt_nb_puts;

  /* An auxiliary list that records all of the nb_put_arrays.  
   * It is useful for improving the scalability 
   * of the clear_all_puts() operation, so that its overhead is O(active nodes) 
   * instead of O(all nodes)
   */
  upcrt_nb_put_array_t* *upcrt_nb_puts_aux;

  /* Store the number of destination nodes with active memputs */
  unsigned int active_nodes;  

#ifdef UPCRT_AUTO_NB_STATS

  /* Performance counters for nonblocking memputs */
  unsigned int total_memput;
  unsigned int num_conflict_check;
  unsigned int num_clear;
  unsigned int max_outstanding_puts;
  unsigned int total_outstanding_puts;
  bupc_tick_t max_overlap;
  bupc_tick_t total_overlap;
  unsigned int max_put_size;
  unsigned int min_put_size;
  uint64_t total_put_size;
  unsigned int total_conflict;

#endif /* UPCRT_AUTO_NB_STATS */

} UPCRT_GLOBAL_PUT;

UPCRT_GLOBAL_PUT UPCR_TLD_DEFINE_TENTATIVE(global_vars, sizeof(UPCRT_GLOBAL_PUT), 8);

/*
 * The max number of outstanding puts allowed per node.
 * When the threshold is reached, we must synchronize the puts in the list
 * This needs not be TLD because it's set once at initialization
 */  
static unsigned int upcrt_max_puts;

/*  The default number of puts allowed per thread per destination node */
#define UPCRT_MAX_PUT_DEFAULT 20




extern void upcrt_report_auto_nb_stats(void) {

  UPCR_BEGIN_FUNCTION();

  UPCRT_GLOBAL_PUT *my_vars = (UPCRT_GLOBAL_PUT *) UPCR_TLD_ADDR(global_vars); 
  
#ifdef UPCRT_AUTO_NB_STATS
  if (my_vars->total_memput > 0 ) {
    /* report only when there's at least one nonblocking memput */
    printf("==========Auto nonblocking statistics for node %u============\n", gasnet_mynode());
    printf("Total number of nonblocking memputs: %u\n", my_vars->total_memput);
    printf("Min/Max/Avg message size: %u/%u/%u bytes\n", 
	   my_vars->min_put_size, my_vars->max_put_size, (unsigned) my_vars->total_put_size / my_vars->total_memput);
    printf("Total number of conflict checks: %u\n", my_vars->num_conflict_check);
    printf("Total number of actual conflicts: %u\n", my_vars->total_conflict);
    printf("Average list length for a conflict check: %f\n", 
	   ((double) my_vars->total_outstanding_puts) / my_vars->num_conflict_check);
    printf("Max list length for a conflict check: %u\n", my_vars->max_outstanding_puts);
    printf("Number of list clear operations: %u\n", my_vars->num_clear);
    printf("Max amount of available overlap: %llu us\n", (unsigned long long) bupc_ticks_to_us(my_vars->max_overlap));
    printf("Average amount of available overlap: %llu us\n", 
	   (unsigned long long) bupc_ticks_to_us(my_vars->total_overlap / my_vars->total_memput));
    
    printf("==============================================\n");
  }
#endif
}


extern void upcrt_auto_nb_proc_init(void) {

  char * max_puts;
  max_puts = upcr_getenv("UPC_MAX_NB_PUT");
  if (max_puts != NULL) {
    upcrt_max_puts = atoi(max_puts);
  } else {
    upcrt_max_puts = UPCRT_MAX_PUT_DEFAULT;
  }
}

extern void upcrt_auto_nb_thread_init(void) {

  UPCR_BEGIN_FUNCTION();
  UPCRT_GLOBAL_PUT *my_vars = (UPCRT_GLOBAL_PUT *) UPCR_TLD_ADDR(global_vars);   

  memset(my_vars, 0, sizeof(UPCRT_GLOBAL_PUT));

  my_vars->upcrt_nb_puts = (upcrt_nb_put_array_t*) upcri_malloc(gasnet_nodes() * sizeof(upcrt_nb_put_array_t));
  memset(my_vars->upcrt_nb_puts, 0, gasnet_nodes() * sizeof(upcrt_nb_put_array_t));
  
  my_vars->upcrt_nb_puts_aux = (upcrt_nb_put_array_t**) upcri_malloc(gasnet_nodes() * sizeof(upcrt_nb_put_array_t));
  my_vars->min_put_size = 1e7;
  
  /* the individual nb put queues to each node are initialized on demand */
}

static void upcrt_clear_put_list(upcrt_nb_put_array_t *put_ar) {

    int i;

    for (i = 0; i < put_ar->num_puts; i++) {  
	upcr_wait_syncnb(put_ar->arr[i].handle); 
    } 
    put_ar->num_puts = 0;
} 

/*
 * Remove a put from the list of outstanding put (synchronizing it if necessary), 
 * and shift all of the later puts up by one
 */
static void upcrt_remove_nb_put(upcrt_nb_put_array_t *put_ar, int index, int need_sync) {

  UPCR_BEGIN_FUNCTION();

    int i;
    UPCRT_GLOBAL_PUT *my_vars = (UPCRT_GLOBAL_PUT *) UPCR_TLD_ADDR(global_vars);   
    upcri_assert(put_ar->num_puts > 0 && index < put_ar->num_puts);

#ifdef UPCRT_AUTO_NB_STATS
    {
      bupc_tick_t tick;
      tick = bupc_ticks_now() - put_ar->arr[index].start;
      if (tick > my_vars->max_overlap) {
	my_vars->max_overlap = tick;
      }
      my_vars->total_overlap += my_vars->max_overlap;
    }
#endif

    if (need_sync) {
	upcr_wait_syncnb(put_ar->arr[index].handle);
    }

    put_ar->arr[index] = put_ar->arr[put_ar->num_puts-1];
    put_ar->num_puts--;
} 

/* search through the list of outstanding puts, searching for conflicts */
static void upcrt_check_conflict_nbput(gasnet_node_t node, void *addr, size_t nbytes) {

  UPCR_BEGIN_FUNCTION();

    int i;
    UPCRT_GLOBAL_PUT *my_vars = (UPCRT_GLOBAL_PUT *) UPCR_TLD_ADDR(global_vars);   
    upcrt_nb_put_array_t *put_ar = &my_vars->upcrt_nb_puts[node];
    /* fprintf(stderr, "%d in %s: %d %d\n", gasnet_mynode(), __func__, node, put_ar->num_puts); */

#ifdef UPCRT_AUTO_NB_STATS
    my_vars->num_conflict_check++;
    if (put_ar->num_puts > my_vars->max_outstanding_puts) {
      my_vars->max_outstanding_puts = put_ar->num_puts;
    }
    my_vars->total_outstanding_puts += put_ar->num_puts;
#endif

    for (i = 0; i < put_ar->num_puts; i++) {
      /*
	fprintf(stderr, "%d in %s: put(%d) = <%d, %llu, %d>\n", 
	      gasnet_mynode(), __func__, i, put_ar->arr[i].handle,
	      put_ar->arr[i].dst, put_ar->arr[i].nbytes);
      */
      upcr_shared_ptr_t put_addr = put_ar->arr[i].dst;
      char* addrfield1 = (char*) upcri_shared_to_remote(put_addr);
      char* addrfield2 = (char*) addr;	
      size_t size1 = put_ar->arr[i].nbytes;
      
      if ((addrfield2 > addrfield1 && addrfield2 < addrfield1 + size1) ||
	  (addrfield1 > addrfield2 && addrfield1 < addrfield2 + nbytes)) {
	/* there's overlap */
	upcrt_remove_nb_put(put_ar, i, 1);
	i--; /* since the list has shrunk by one, we also need to decrement the index */
#ifdef UPCRT_AUTO_NB_STATS
	fprintf(stderr, "Conflict on node %d: <%p, %p> and <%p, %p>\n", 
		gasnet_mynode(), addrfield1, addrfield1+size1, addrfield2, addrfield2+nbytes);
	my_vars->total_conflict++;
#endif
      } else if (upcr_try_syncnb(put_ar->arr[i].handle)) {
	/* may as well remove it */
	upcrt_remove_nb_put(put_ar, i, 0);
	i--;
      }
    }
    /*    fprintf(stderr, "%d out %s\n", gasnet_mynode(), __func__); */
}


void upcrt_clear_all_puts(void) {

  int i;
  UPCRT_GLOBAL_PUT *my_vars = (UPCRT_GLOBAL_PUT *) UPCR_TLD_ADDR(global_vars);     

  for (i = 0; i < my_vars->active_nodes; i++) {
    upcrt_nb_put_array_t *put_ar = my_vars->upcrt_nb_puts_aux[i];
    upcrt_clear_put_list(put_ar);
  }

#ifdef UPCRT_AUTO_NB_STATS
    my_vars->num_clear++;
#endif

}



/***********  Interface exposed to the runtime *************/

extern void upcrt_check_conflict_remote_put_pshared(upcr_pshared_ptr_t dest, ptrdiff_t destoffset, 
						    const void *src, size_t nbytes, int isstrict) {

  if (isstrict) {
    upcrt_clear_all_puts();
  } else if (gasnet_mynode() != upcri_pshared_nodeof(dest)) {
    upcrt_check_conflict_nbput(upcri_pshared_nodeof(dest), upcri_pshared_to_remote_off(dest, destoffset), nbytes);
  }
}

extern void upcrt_check_conflict_remote_put_shared(upcr_shared_ptr_t dest, ptrdiff_t destoffset, 
						   const void *src, size_t nbytes, int isstrict) {

  if (isstrict) {
    upcrt_clear_all_puts();
  } else if (gasnet_mynode() != upcri_shared_nodeof(dest)) {
    upcrt_check_conflict_nbput(upcri_shared_nodeof(dest), upcri_shared_to_remote_off(dest, destoffset), nbytes);
  }
}

extern void upcrt_check_conflict_remote_get_pshared(void *dest, upcr_pshared_ptr_t src, ptrdiff_t srcoffset,
						    size_t nbytes, int isstrict) {

  if (isstrict) {
    upcrt_clear_all_puts();
  } else if (gasnet_mynode() != upcri_pshared_nodeof(src)) {
    upcrt_check_conflict_nbput(upcri_pshared_nodeof(src), upcri_pshared_to_remote_off(src, srcoffset), nbytes);
  }
}

extern void upcrt_check_conflict_remote_get_shared(void *dest, upcr_shared_ptr_t src, ptrdiff_t srcoffset,
						   size_t nbytes, int isstrict) {
  
  if (isstrict) {
    upcrt_clear_all_puts();
  } else if (gasnet_mynode() != upcri_shared_nodeof(src)) {
    upcrt_check_conflict_nbput(upcri_shared_nodeof(src), upcri_shared_to_remote_off(src, srcoffset), nbytes);
  }
}

extern void upcrt_before_barrier(int barrierval, int flag) {

  upcrt_clear_all_puts();
}

extern void upcrt_after_barrier(int barrierval, int flag) {

  /* nop for puts, need to issue prefetch calls for gets */
}

extern void upcrt_sync_allnb(void) {

  upcrt_clear_all_puts();
}


/************* Interface exposed to the translator *****************/

extern void _upcrt_memput_nb(upcr_shared_ptr_t dst, const void *src, size_t n) {

  UPCR_BEGIN_FUNCTION();

  int i;
  UPCRT_GLOBAL_PUT *my_vars = (UPCRT_GLOBAL_PUT *) UPCR_TLD_ADDR(global_vars);     
  gasnet_node_t node = upcri_shared_nodeof(dst);
  upcrt_nb_put_array_t *put_ar;
  upcrt_put_t* new_put;
  
  put_ar = &my_vars->upcrt_nb_puts[node];
  
  if (node == gasnet_mynode()) {
    upcr_memput(dst, (void*) src, n);
    return;
  }
  
  if (put_ar->arr == NULL) {
    /* communication to a new destination node */
    put_ar->arr = (upcrt_put_t*) upcri_malloc(upcrt_max_puts * sizeof(upcrt_put_t));
    my_vars->upcrt_nb_puts_aux[my_vars->active_nodes++] = put_ar;
  } else if (put_ar->num_puts == upcrt_max_puts) {
    /* sync all of the outstanding puts.  this should hopefully be rare */
    upcrt_clear_put_list(put_ar);
  }
  
  new_put = &put_ar->arr[put_ar->num_puts];
  /* the no buffer version */
  /* new_put->handle = upcr_nb_memput(dst, (void*) src, n); */
  /* the buffer version */
  new_put->handle = upcr_put_nb_shared(dst, 0, src, n);
  new_put->dst = dst;
  new_put->nbytes = n;
  put_ar->num_puts++;
  
  
#ifdef UPCRT_AUTO_NB_STATS 
    my_vars->total_memput++;
    if (n > my_vars->max_put_size) {
      my_vars->max_put_size = n;
    }
    if (n < my_vars->min_put_size) {
      my_vars->min_put_size = n;
    }
    my_vars->total_put_size += n;
    new_put->start = bupc_ticks_now();    
#endif

    /*
      fprintf(stderr, "%d out %s: adding put(%d): <%d, %llu, %d>\n", gasnet_mynode(), __func__,
      put_ar->num_puts-1, new_put->handle, new_put->dst, new_put->nbytes);
    */
}




