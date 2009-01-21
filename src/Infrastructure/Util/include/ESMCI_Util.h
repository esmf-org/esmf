// $Id: ESMCI_Util.h,v 1.3.2.5 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Util C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_UTIL_H
#define ESMCI_UTIL_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

#include "stdio.h"

#include "ESMC_Start.h"

#include "ESMF_InitMacros.inc"

#include "ESMC_Util.h"

//-----------------------------------------------------------------------------
//BOP
// This file contains generic utility routines and a few minor classes.
//
// !DESCRIPTION:
// General purpose utility code.
//
//-----------------------------------------------------------------------------
//
// !USES:

// !PUBLIC TYPES:

// WARNING:  the values of these enums MUST match the values defined
//  in ../interface/ESMF_Util.F90

// general logical value - MUST MATCH F90
enum ESMC_Logical { ESMF_TRUE=1,
                    ESMF_FALSE };

// ESMF class states
enum ESMC_Status { ESMF_STATUS_UNINIT=1,
                   ESMF_STATUS_READY,
                   ESMF_STATUS_UNALLOCATED,
                   ESMF_STATUS_ALLOCATED,
                   ESMF_STATUS_BUSY,
                   ESMF_STATUS_INVALID,
                   ESMF_STATUS_NOT_READY };

#define ESMC_NULL_POINTER (0)
#define ESMC_BAD_POINTER (-1)

// comm types
enum ESMC_BlockingFlag { ESMF_BLOCKING=1,
                         ESMF_VASBLOCKING,
                         ESMF_NONBLOCKING};

// context flag
enum ESMC_ContextFlag { ESMF_CHILD_IN_NEW_VM=1,
                        ESMF_CHILD_IN_PARENT_VM};

// de pin flag
enum ESMC_DePinFlag { ESMF_DE_PIN_PET=1,
                        ESMF_DE_PIN_VAS};

// direction type
enum ESMC_Direction { ESMF_MODE_FORWARD=1,
                      ESMF_MODE_REVERSE};

// indexflag type
enum ESMC_IndexFlag { ESMF_INDEX_DELOCAL=0,
                      ESMF_INDEX_GLOBAL,
                      ESMF_INDEX_USER};

// regionflag type
enum ESMC_RegionFlag { ESMF_REGION_TOTAL=0,
                       ESMF_REGION_SELECT,
                       ESMF_REGION_EMPTY};

// ESMF platform-dependent data types
#ifdef ESMF_IS_32BIT_MACHINE
  typedef long long ESMC_I8;
  typedef int       ESMC_I4;
  typedef short     ESMC_I2;
  typedef char      ESMC_I1;
  typedef double    ESMC_R8;
  typedef float     ESMC_R4;
  typedef unsigned long      ESMC_POINTER;
#else // 64-bit or larger machine
  typedef long      ESMC_I8;
  typedef int       ESMC_I4;
  typedef short     ESMC_I2;
  typedef char      ESMC_I1;
  typedef double    ESMC_R8;
  typedef float     ESMC_R4;
  typedef unsigned long long ESMC_POINTER;
#endif

// are the index numbers relative to a local chunk or the overall
// combined igrid?
typedef enum {
    ESMF_LOCAL  = 1, 
    ESMF_GLOBAL
} ESMC_LocalGlobalFlag;

// what kind of data chunk are the index numbers describing?
typedef enum {
    ESMC_DOMAIN_EXCLUSIVE = 1,
    ESMC_DOMAIN_COMPUTATIONAL,
    ESMC_DOMAIN_TOTAL,
    ESMC_DOMAIN_ALLOCATED,
    ESMC_DOMAIN_OLDEXCLUSIVE,
    ESMC_DOMAIN_OLDCOMPUTATIONAL,
    ESMC_DOMAIN_OLDTOTAL
} ESMC_DomainType;

// max/min macros if they don't already exist
#ifndef MAX
#define MAX(a,b)  (((a)>(b))?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b)  (((a)<(b))?(a):(b))
#endif

// unique id/name string for all esmf objects.
typedef struct ESMC_ObjectID { 
   int objectID; 
   char objectName[32]; 
} ESMC_ObjectID;


// TODO:
// the domain below should be renamed AxisIndexList
// or the AxisIndex itself could morph into a rank and
// a list of mins, maxs and lengths.

// elemental index for axis decompositions
class ESMC_AxisIndex {
 public:
    int     min;
    int     max;
    int  stride;
    int     pad;    // insure F90/C++ memory alignment
    ESMF_INIT_C_PAD 

};

// collection of AxisIndices per axis, to describe an n-dim cube
struct ESMC_Domain {
    int DE;
    int rank;
    ESMC_AxisIndex ai_list[ESMF_MAXDIM];

    ESMF_INIT_C_PAD 
};


// collection of AxisIndices per axis, to describe an n-dim cube
class ESMC_DomainList {
  public:   // TODO: fix this to be private?
    int num_domains;
    int current_size;
    int total_points;
    int pad_for_64;
    ESMC_Domain *domains;

    int  ESMC_DomainListGetDE(int domainnum);
    ESMC_AxisIndex ESMC_DomainListGetAI(int domainnum, int ainum);

    ESMF_INIT_C_PAD 

};

// these should all become class methods
int ESMC_AxisIndexSet(ESMC_AxisIndex *ai, int min, int max);
int ESMC_AxisIndexSet(ESMC_AxisIndex *ai, int min, int max, int stride);
int ESMC_AxisIndexGet(ESMC_AxisIndex *ai, int *min, int *max, int *stride);
int ESMC_AxisIndexCopy(ESMC_AxisIndex *src, ESMC_AxisIndex *dst);
bool ESMC_AxisIndexIntersect(int ndims, 
                             ESMC_AxisIndex *src1, ESMC_AxisIndex *src2, 
                             ESMC_AxisIndex *dst);
int ESMC_AxisIndexLocalToGlobal(ESMC_AxisIndex *srclocal, 
                                     ESMC_AxisIndex *global, 
                                     int *globalStarts, int ndims, 
                                     ESMC_AxisIndex *dstglobal);
int ESMC_AxisIndexGlobalToLocal(int ndims, 
                                ESMC_AxisIndex *srcglobal, 
                                ESMC_AxisIndex *globalref, 
                                ESMC_AxisIndex *dstlocal);
int ESMC_AxisIndexPrint(ESMC_AxisIndex *ai);
ESMC_Logical ESMC_AxisIndexEqual(ESMC_AxisIndex *ai1, ESMC_AxisIndex *ai2);

// string utility functions:
// the first set are for converting an F90 char buffer + max count into a 
// null terminated C string.  
// the first routine allocates space for the return string and the caller
//  must delete the buffer when finished.
// the second routine copies up to dlen bytes into the dst buffer.  the order
//  of the arguments are logical for a caller in C++.  see the following call
//  for something that can be called straight from fortran (where the counts
//  are added by the compiler as hidden arguments at the end of the arg list).
//
// the second set takes in a C string and fortran buffer with max length
//  (which is added by the compiler for a call straight from fortran)
//  and copies the contents of the src to the dst, padding the remainder
//  with spaces and no null terminator.
//
char *ESMC_F90toCstring(char *src, int slen);
int  ESMC_F90toCstring(char *src, int slen, char *dst, int dlen);
int  ESMC_CtoF90string(char *src, char *dst, int dlen);
extern "C" {
void  FTN(esmf_f90tocstring)(char *src, char *dst, int *rc, 
                             /* hidden */ int *slen, int *dlen);
void  FTN(esmf_ctof90string)(char *src, char *dst, int *rc, 
                             /* hidden */ int *slen, int *dlen);
}

// return byte counts for TypeKinds
int ESMC_TypeKindSize(ESMC_TypeKind dk);
// return a static string name for various enums
const char *ESMC_StatusString(ESMC_Status stat);
const char *ESMC_TypeKindString(ESMC_TypeKind dk);
const char *ESMC_LogicalString(ESMC_Logical tf);

extern "C" {
void FTN(f_esmf_domainlistgetde)(ESMC_DomainList *, int *, int *, int *);
void FTN(f_esmf_domainlistgetai)(ESMC_DomainList *, int *, int *, 
                                                    ESMC_AxisIndex *ai, int *);
void FTN(esmf_pointertoint)(int *n, short *s, ESMC_POINTER *len);
void FTN(esmf_pointerdifference)(int *n, short *s1, short *s2, int *len);

}

// general reduction operator value - MUST MATCH F90
enum ESMC_Operation { ESMF_SUM=1, ESMF_MIN, ESMF_MAX};

#endif  // ESMCI_UTIL_H
