/* Basic Utilities */

#ifndef ESMC_BASIC_UTIL_H
#define ESMC_BASIC_UTIL_H

/*----------------------------------------------------------------------*/
/* Basic constants */

#define ESMC_MAX_CHARS    128
#define ESMC_NULL             0 
#define ESMC_COMM_WORLD       0
#define ESMC_SUCCESS          0

typedef enum {ESMC_FALSE, ESMC_TRUE} ESMC_Bool;

#define ESMC_EQUAL(v1,v2)          ((v1>=(v2-ESMC_EPS)) && (v1<v2+ESMC_EPS))

#endif














