// $Id: ESMCI_Util.h,v 1.22.2.1 2010/02/05 20:01:03 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
//  in ../src/ESMF_UtilTypes.F90


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

// inquireonly flag type
enum ESMC_InquireFlag { ESMF_INQUIREONLY=ESMF_TRUE,
                        ESMF_NOINQUIRE=ESMF_FALSE};

// proxy flag type
enum ESMC_ProxyFlag { ESMF_PROXYYES=1,
                      ESMF_PROXYNO};

// regionflag type
enum ESMC_RegionFlag { ESMF_REGION_TOTAL=0,
                       ESMF_REGION_SELECT,
                       ESMF_REGION_EMPTY};

// Attribute reconcile type
enum ESMC_AttCopyFlag { ESMC_ATTCOPY_HYBRID=0,
                        ESMC_ATTCOPY_REFERENCE,
                        ESMC_ATTCOPY_VALUE};

// attgetcount flag type
enum ESMC_AttGetCountFlag { ESMC_ATTGETCOUNT_ATTRIBUTE=0,
                            ESMC_ATTGETCOUNT_ATTPACK,
                            ESMC_ATTGETCOUNT_ATTLINK,
                            ESMC_ATTGETCOUNT_TOTAL};
                        
// nested Attribute package flag type
enum ESMC_AttPackNestFlag { ESMC_ATTPACKNEST_OFF=0,
                            ESMC_ATTPACKNEST_ON};

// Attribute reconcile type
enum ESMC_AttReconcileFlag { ESMC_ATTRECONCILE_OFF=0,
                            ESMC_ATTRECONCILE_ON};

// Attribute reconcile type
enum ESMC_AttTreeFlag { ESMC_ATTTREE_OFF=0,
                        ESMC_ATTTREE_ON};

// attwrite flag type
enum ESMC_AttWriteFlag { ESMC_ATTWRITE_TAB=0,
                         ESMC_ATTWRITE_XML};

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
char *ESMC_F90toCstring(const char *src, ESMCI_FortranStrLenArg slen);
int  ESMC_F90toCstring(const char *src, ESMCI_FortranStrLenArg slen, char *dst, ESMCI_FortranStrLenArg dlen);
int  ESMC_CtoF90string(const char *src, char *dst, ESMCI_FortranStrLenArg dlen);
extern "C" {
void  FTN(esmf_f90tocstring)(const char *src, char *dst, int *rc, 
                             /* hidden */ ESMCI_FortranStrLenArg slen, ESMCI_FortranStrLenArg dlen);
void  FTN(esmf_ctof90string)(const char *src, char *dst, int *rc, 
                             /* hidden */ ESMCI_FortranStrLenArg slen, ESMCI_FortranStrLenArg dlen);
}

// return byte counts for TypeKinds
int ESMC_TypeKindSize(ESMC_TypeKind dk);
// return a static string name for various enums
const char *ESMC_StatusString(ESMC_Status stat);
const char *ESMC_TypeKindString(ESMC_TypeKind dk);
const char *ESMC_LogicalString(ESMC_Logical tf);

extern "C" {
void FTN(esmf_pointertoint)(int *n, short *s, ESMC_POINTER *len);
void FTN(esmf_pointerdifference)(int *n, short *s1, short *s2, int *len);

}

// general reduction operator value - MUST MATCH F90
enum ESMC_Operation { ESMF_SUM=1, ESMF_MIN, ESMF_MAX};


extern ESMC_ObjectID ESMC_ID_BASE;
extern ESMC_ObjectID ESMC_ID_IOSPEC;
extern ESMC_ObjectID ESMC_ID_LOGERR;
extern ESMC_ObjectID ESMC_ID_TIME;
extern ESMC_ObjectID ESMC_ID_CALENDAR;
extern ESMC_ObjectID ESMC_ID_TIMEINTERVAL;
extern ESMC_ObjectID ESMC_ID_ALARM;
extern ESMC_ObjectID ESMC_ID_CLOCK;
extern ESMC_ObjectID ESMC_ID_ARRAYSPEC;
extern ESMC_ObjectID ESMC_ID_LOCALARRAY;
extern ESMC_ObjectID ESMC_ID_ARRAYBUNDLE;
extern ESMC_ObjectID ESMC_ID_VM;
extern ESMC_ObjectID ESMC_ID_DELAYOUT;
extern ESMC_ObjectID ESMC_ID_CONFIG;
extern ESMC_ObjectID ESMC_ID_ARRAY;
extern ESMC_ObjectID ESMC_ID_PHYSGRID;
extern ESMC_ObjectID ESMC_ID_IGRID;
extern ESMC_ObjectID ESMC_ID_EXCHANGEPACKET;
extern ESMC_ObjectID ESMC_ID_COMMTABLE;
extern ESMC_ObjectID ESMC_ID_ROUTETABLE;
extern ESMC_ObjectID ESMC_ID_ROUTE;
extern ESMC_ObjectID ESMC_ID_ROUTEHANDLE;
extern ESMC_ObjectID ESMC_ID_FIELDDATAMAP;
extern ESMC_ObjectID ESMC_ID_FIELD;
extern ESMC_ObjectID ESMC_ID_BUNDLEDATAMAP;
extern ESMC_ObjectID ESMC_ID_FIELDBUNDLE;
extern ESMC_ObjectID ESMC_ID_GEOMBASE;
extern ESMC_ObjectID ESMC_ID_REGRID;
extern ESMC_ObjectID ESMC_ID_LOCSTREAM;
extern ESMC_ObjectID ESMC_ID_STATE;
extern ESMC_ObjectID ESMC_ID_GRIDCOMPONENT;
extern ESMC_ObjectID ESMC_ID_CPLCOMPONENT;
extern ESMC_ObjectID ESMC_ID_COMPONENT;
extern ESMC_ObjectID ESMC_ID_NONE;


#endif  // ESMCI_UTIL_H
