/* $Id: ESMC_TimeF.c,v 1.1 2002/11/15 21:32:21 jwolfe Exp $ */

/* Fortran interface file */

#include "ESMO.h"
#include "ESMO_Time.h"

#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FORTRANUNDERSCORE
#endif

#ifdef POINTER_64_BITS
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *ESMC_ToPointer();
extern int ESMC_FromPointer();
extern void ESMC_RmPointer();
#if defined(__cplusplus)
} 
#endif 

#else

#define ESMC_ToPointer(a) ((long*)(a))
#define ESMC_FromPointer(a) (long)(a)
#define ESMC_RmPointer(a)
#endif


#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timeinitis_ PESMC_TIMEINITIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeinitis_ pesmc_timeinitis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeinitis_ pesmc_timeinitis
#else

#define esmc_timeinitis_ pesmc_timeinitis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timeinitis_ ESMC_TIMEINITIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeinitis_ esmc_timeinitis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeinitis_ esmc_timeinitis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timecopyinit_ PESMC_TIMECOPYINIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timecopyinit_ pesmc_timecopyinit__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timecopyinit_ pesmc_timecopyinit
#else

#define esmc_timecopyinit_ pesmc_timecopyinit_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timecopyinit_ ESMC_TIMECOPYINIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timecopyinit_ esmc_timecopyinit__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timecopyinit_ esmc_timecopyinit
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timeinitundefined_ PESMC_TIMEINITUNDEFINED
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeinitundefined_ pesmc_timeinitundefined__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeinitundefined_ pesmc_timeinitundefined
#else

#define esmc_timeinitundefined_ pesmc_timeinitundefined_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timeinitundefined_ ESMC_TIMEINITUNDEFINED
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeinitundefined_ esmc_timeinitundefined__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeinitundefined_ esmc_timeinitundefined
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timecopy_ PESMC_TIMECOPY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timecopy_ pesmc_timecopy__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timecopy_ pesmc_timecopy
#else

#define esmc_timecopy_ pesmc_timecopy_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timecopy_ ESMC_TIMECOPY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timecopy_ esmc_timecopy__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timecopy_ esmc_timecopy
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timegetdays_ PESMC_TIMEGETDAYS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timegetdays_ pesmc_timegetdays__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timegetdays_ pesmc_timegetdays
#else
#define esmc_timegetdays_ pesmc_timegetdays_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timegetdays_ ESMC_TIMEGETDAYS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timegetdays_ esmc_timegetdays__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timegetdays_ esmc_timegetdays
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timegetis_ PESMC_TIMEGETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timegetis_ pesmc_timegetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timegetis_ pesmc_timegetis
#else
#define esmc_timegetis_ pesmc_timegetis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timegetis_ ESMC_TIMEGETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timegetis_ esmc_timegetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timegetis_ esmc_timegetis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timesetis_ PESMC_TIMESETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timesetis_ pesmc_timesetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timesetis_ pesmc_timesetis
#else
#define esmc_timesetis_ pesmc_timesetis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timesetis_ ESMC_TIMESETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timesetis_ esmc_timesetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timesetis_ esmc_timesetis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timeincrementis_ PESMC_TIMEINCREMENTIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeincrementis_ pesmc_timeincrementis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeincrementis_ pesmc_timeincrementis
#else
#define esmc_timeincrementis_ pesmc_timeincrementis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timeincrementis_ ESMC_TIMEINCREMENTIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeincrementis_ esmc_timeincrementis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeincrementis_ esmc_timeincrementis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timediff_ PESMC_TIMEDIFF
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timediff_ pesmc_timediff__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timediff_ pesmc_timediff
#else
#define esmc_timediff_ pesmc_timediff_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timediff_ ESMC_TIMEDIFF
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timediff_ esmc_timediff__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timediff_ esmc_timediff
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timedecrementis_ PESMC_TIMEDECREMENTIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timedecrementis_ pesmc_timedecrementis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timedecrementis_ pesmc_timedecrementis
#else
#define esmc_timedecrementis_ pesmc_timedecrementis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timedecrementis_ ESMC_TIMEDECREMENTIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timedecrementis_ esmc_timedecrementis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timedecrementis_ esmc_timedecrementis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timeprint_ PESMC_TIMEPRINT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeprint_ pesmc_timeprint__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeprint_ pesmc_timeprint
#else
#define esmc_timeprint_ pesmc_timeprint_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timeprint_ ESMC_TIMEPRINT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timeprint_ esmc_timeprint__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timeprint_ esmc_timeprint
#endif
#endif

/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif


void   esmc_timeinitis_(ESMC_Time this, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeConstructIS(this, *days, *seconds);
}



void   esmc_timecopyinit_(ESMC_Time this, ESMC_Time orig, int *rc)
{
  *rc = ESMC_TimeCopyConstruct(this, orig);
}



void   esmc_timeinitundefined_(ESMC_Time this, int *rc)
{
  *rc = ESMC_TimeConstructUndefined(this);
}



void   esmc_timecopy_(ESMC_Time this, ESMC_Time orig, int *rc)
{
  *rc = ESMC_TimeCopy(this, orig);
}



void   esmc_timegetis_(ESMC_Time this, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeGetIS(this, days, seconds);
}



void   esmc_timesetis_(ESMC_Time this, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeSetIS(this, *days, *seconds);
}



void   esmc_timegetdays_(ESMC_Time this, double *days, int *rc)
{
  *rc = ESMC_TimeGetDays(this, days);
}



void  esmc_timeincrementis_(ESMC_Time this, ESMC_Time incTime, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeIncrementIS(this, incTime, *days, *seconds);
}



void  esmc_timediff_(ESMC_Time earlyTime, ESMC_Time lateTime, ESMC_Time diff,
		   ESMC_Bool *isLater, int *rc)
{
  *rc = ESMC_TimeDiff(earlyTime, lateTime, diff, isLater);
}



void esmc_timedecrementis_(ESMC_Time this, ESMC_Time decTime, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeDecrementIS(this, decTime, *days, *seconds);
}



void esmc_timeprint_(ESMC_Time this, int *rc)
{
  *rc = ESMC_TimePrint(this);
}



#if defined(__cplusplus)
}
#endif




















