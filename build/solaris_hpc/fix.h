/*$Id: fix.h,v 1.1 2001/11/15 23:10:20 dneckels Exp $*/

/*
    This fixes various things in system files that are incomplete, for 
  instance many systems don't properly prototype all system functions.
  It is not intended to DUPLICATE anything in the system include files;
  if the compiler reports a conflict between a prototye in a system file
  and this file then the prototype in this file should be removed.

    This is included by files in src/sys/src
*/

#if !defined(ESMC_FIX_H)
#define ESMC_FIX_H

/*
  This prototype lets us resolve the datastructure 'rusage' only in
  the source files using getrusage, and not in other source files.
*/
typedef struct rusage* s_rusage;

/* -----------------------linux ------------------------------------------*/
#if defined(__cplusplus)
extern "C" {
extern void* memalign (int,int);
}
#endif
#endif


