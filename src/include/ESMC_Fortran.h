/* esmc_fortran.h */

#include "conf.h"

/* Macro utilities for connecting fortran and C */


/* String utilities */
#ifdef ESMC_HAVE_MIXED_LEN
#define ESMC_END_LEN(len)
#define ESMC_MIXED_LEN(len)  ,int len
#else
#define ESMC_END_LEN(len)   ,int len
#define ESMC_MIXED_LEN(len)
#endif


#define ESMC_FIXCHAR(a,n,b) \
{\
  if (a == (char*) 0) { \
    b = a = 0; \
  } else { \
    while((n > 0) && (a[n-1] == ' ')) n--; \
    if (a[n] != 0) { \
      b = (char*) malloc((n+1)*sizeof(char)); \
      strncpy(b, a, n); \
      b[n] = 0; \
    } else b = a;\
  } \
}



#define ESMC_FREECHAR(a, b) if (a != b) free(b);
