/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: zoltan_mem.h,v $
 *    $Author: w6ws $
 *    $Date: 2008/12/09 23:09:10 $
 *    Revision: 1.9 $
 ****************************************************************************/


#ifndef __MEM_CONST_H
#define __MEM_CONST_H


#include <string.h>

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


#ifndef HAVE_PROTOTYPES
#   if defined(__STDC__) || defined(__GNUC__) || defined(__cplusplus) || defined(c_plusplus)
#       define	HAVE_PROTOTYPES
#   endif
#endif

#undef PROTO
#ifdef HAVE_PROTOTYPES
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#define ZOLTAN_MALLOC(a) Zoltan_Malloc((a), __FILE__, __LINE__)
#define ZOLTAN_CALLOC(a, b) Zoltan_Calloc((a), (b), __FILE__, __LINE__)
#define ZOLTAN_REALLOC(a, b) Zoltan_Realloc((a), (b), __FILE__, __LINE__)
#define ZOLTAN_FREE(a) Zoltan_Free((void **) (a), __FILE__, __LINE__)

#define ZOLTAN_MEM_STAT_TOTAL   0
#define ZOLTAN_MEM_STAT_MAXIMUM 1

/* function declarations for dynamic array allocation */

#if defined (__STDC__) || defined (__cplusplus)
extern double *Zoltan_Array_Alloc(const char *file, int lineno, int numdim, ...);
#else
extern double *Zoltan_Array_Alloc();
#endif

extern void Zoltan_Memory_Debug(int);
extern void Zoltan_Free(void **ptr, const char *file, int lineno);
extern double *Zoltan_Calloc (size_t num, int size, const char *filename, int lineno);
extern double *Zoltan_Malloc(size_t n, const char *file, int lineno);
extern double *Zoltan_Realloc(void *ptr, size_t n, const char *filename, int lineno);
extern void Zoltan_Memory_Stats(void);
extern int Zoltan_Memory_Usage(int);

#if defined (__STDC__) || defined (__cplusplus)
extern void Zoltan_Multifree(const char *, int, int n, ...);
#else
extern void Zoltan_Multifree();
#endif

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
