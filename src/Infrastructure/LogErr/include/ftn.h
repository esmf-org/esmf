#include "conf.h"


#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FTN(func) func##_
#else
#define FTN(func) func
#endif
