/* UPC Translator runtime helpers
 * ------------------------------
 * Rules for this interface:
 * - these helper code files must be named matching the pattern upcr_trans_extra* 
 *   and output by the translator along with *any* UPC-to-C translation,
 *   although optimization options may be used to decide which files to send
 * - must use only the upcr_* public interfaces and must *not* rely upon any 
 *   upcri_* symbols, to avoid breakage with future runtime versions
 * - any global symbols should be named upcrt_* to prevent name collisions
 * - must follow all the regular upcr coding rules (strict ANSI C, no C++ comments, etc)
 * - should not include any headers - upcr.h will be automatically included, and it 
 *   already includes most of the standard headers with appropriate autoconf magic
 * - any functions from this file used by generated code should also be declared in 
 *   upcr_trans_extra.w2c.h so they'll show up in w2c.h (or other w2c.h files that the
 *   translator appends to the translated file)
 */

/* ------ Initialization callbacks that can be enabled if necessary ------ */

/* called once-per-process, before global var setup */
static void upcrt_proc_init(void) { 

  /* upcrt_auto_nb_proc_init(); */
} 
void (*UPCRL_trans_extra_procinit)(void) = &upcrt_proc_init;

#ifdef UPCRT_VECT_INCLUDE
extern void upcrt_vect_thread_init(void);
#endif 

/* called once-per-thread, after global var setup */
static void upcrt_thread_init(void) { 

#ifdef UPCRT_VECT_INCLUDE   
  upcrt_vect_thread_init();
#endif

  /* upcrt_auto_nb_thread_init(); */
} 
void (*UPCRL_trans_extra_threadinit)(void) = &upcrt_thread_init;


/* ------ Helper functions for UPC forall optimizations -------- */

/*
 *  Compute the GCD of two integers
 *  FIXME: does this work for negative ints?
 */
int upcrt_gcd(int a, int b) {
  return (b == 0) ? a : upcrt_gcd(b, a % b);
}

/* Compute val such that (i = val) is the first iteration in the forall loop 
 * that MYTHREAD will execute.
 * start_thread is the thread that executes the first ieration (i.e., i = lower_bound)
 * step is the step of the loop.
 * scale is the multiplier for the affinity expression (i.e., for 2*i scale is 2)
 * the affinity expression is thus incremented by scale * step in each iteration
 * lo is the lower bound of the loop (the initial value of i)
 */
int _upcrt_forall_start(int start_thread, int step, int lo, int scale) {
  UPCR_BEGIN_FUNCTION();
  int threads = upcr_threads();
  int mythread = upcr_mythread();
  int gcd_val = upcrt_gcd(step, threads);
  int dist = start_thread - mythread;
  int ans;
  if (dist % gcd_val != 0) {
    /* this thread never executes the loop */
    return step > 0 ? (int)~(((unsigned int)1) << (8*sizeof(int)-1)) /*INT_MAX*/ :
                      (int)(((unsigned int)1) << (8*sizeof(int)-1)) /*INT_MIN*/;
  }
  if (step > 0) {
    for (ans = lo; ans < lo + step * threads; ans += step) {
      if (dist == 0) {
        /* fprintf(stderr, "th %d: forall_start(%d, %d, %d) = %d\n", mythread, start_thread, step, lo, ans); */
        return ans;
      }
      dist = (dist + step * scale) % threads;
    }
  } else {
    for (ans = lo; ans > lo + step * threads; ans += step) {
      if (dist == 0) {
        /* fprintf(stderr, "th %d: forall_start(%d, %d, %d) = %d\n", mythread, start_thread, step, lo, ans); */
        return ans;
      }
      dist = (dist + step * scale) % threads;
    }
  }

  abort(); /* shouldn't reach here */
  return 0;
}


