/* $Id: ESMC_ErrorF.c,v 1.1 2003/03/11 03:17:51 cdeluca Exp $ */

/* Fortran interface file */

#include "ESMO.h"
#include "ESMO_Error.h"

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
#define esmc_errhandlersettype_ PESMC_ERRHANDLERSETTYPE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_errhandlersettype_ pesmc_errhandlersettype__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_errhandlersettype_ pesmc_errhandlersettype
#else

#define esmc_errhandlersettype_ pesmc_errhandlersettype_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_errhandlersettype_ ESMC_ERRHANDLERSETTYPE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_errhandlersettype_ esmc_errhandlersettype__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_errhandlersettype_ esmc_errhandlersettype
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_errprint_ PESMC_ERRPRINT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_errprint_ pesmc_errprint__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_errprint_ pesmc_errprint
#else

#define esmc_errprint_ pesmc_errprint_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_errprint_ ESMC_ERRPRINT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_errprint_ esmc_errprint__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_errprint_ esmc_errprint
#endif
#endif

/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

void   esmc_errhandlersettype_(ESMC_ErrHandlerType *type)
{
  ESMC_ErrHandlerSetType(*type);
}



void   esmc_errprint_(int *rc)
{
  ESMC_ErrPrint(*rc);
}














