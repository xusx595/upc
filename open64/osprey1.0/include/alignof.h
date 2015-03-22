/* $Id: alignof.h,v 1.2 2004/01/25 22:43:13 wychen Exp $ */
/* -*-Mode: C;-*- */
/* * BeginRiceCopyright *****************************************************
 * 
 * ******************************************************* EndRiceCopyright */

/* ====================================================================
 * ====================================================================
 *
 * $Source: bitbucket.org:berkeleylab/upc-translator.git/open64/osprey1.0/include/alignof.h $
 * $Revision: 1.2 $
 * $Date: 2004/01/25 22:43:13 $
 *
 * Nathan Tallent.
 *
 * Description:
 *
 * Standard interface to alignof() function.
 *
 * ====================================================================
 * ==================================================================== */

#ifndef alignof_h
#define alignof_h

/*************************** System Include Files ***************************/

/**************************** User Include Files ****************************/

/*************************** Forward Declarations ***************************/

/****************************************************************************/

/* ALIGNOF(type): return a size_t giving alignment information for type. */
#if defined(__GNUC__)

  /* GCC provides as a compiler builtin */
# define ALIGNOF(x) __alignof__(x)

#else

# if defined(__sun)
   /* Sun compiler provides as builtin */
#  define ALIGNOF(x) __alignof(x)
# elif defined(__sgi)
   /* SGI compiler provides as builtin */
#  define ALIGNOF(x) __builtin_alignof(x)
# elif defined(__alpha)
   /* Compaq compiler does not provide as a builtin */
#  include <alignof_replacement.h> /* for ALIGNOF */
# else
#  error "Please verify definition for alignof()." 
# endif

#endif


/****************************************************************************/

#endif
