// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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

#include <string>
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

// Maximum length of a file name, including its path.
#define ESMC_MAXPATHLEN 1024

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

// pin flag
enum ESMC_Pin_Flag { ESMF_PIN_DE_TO_PET=1,
                     ESMF_PIN_DE_TO_VAS,
                     ESMF_PIN_DE_TO_SSI,
                     ESMF_PIN_DE_TO_SSI_CONTIG};

// direction type
enum ESMC_Direction { ESMF_DIRECTION_FORWARD=1,
                      ESMF_DIRECTION_REVERSE};

// inquireonly flag type
enum ESMC_InquireFlag { ESMF_INQUIREONLY=ESMF_TRUE,
                        ESMF_NOINQUIRE=ESMF_FALSE};

// proxy flag type
enum ESMC_ProxyFlag { ESMF_PROXYYES=1,  // Object is a proxy
                      ESMF_PROXYNO,     // Object is not a proxy
                      ESMF_PROXYANY};   // For queries

// halostartregionflag type
enum ESMC_HaloStartRegionFlag { ESMF_REGION_EXCLUSIVE=0,
                                ESMF_REGION_COMPUTATIONAL};
// commflag type
enum ESMC_CommFlag { ESMF_COMM_BLOCKING=0,
                     ESMF_COMM_NBSTART,
                     ESMF_COMM_NBTESTFINISH,
                     ESMF_COMM_NBWAITFINISH,
                     ESMF_COMM_CANCEL};

// Attribute reconcile type
enum ESMC_AttCopyFlag { ESMF_ATTCOPY_REFERENCE=0,
                        ESMF_ATTCOPY_VALUE,
                        ESMF_ATTCOPY_HYBRID};

// attgetcount flag type
enum ESMC_AttGetCountFlag { ESMC_ATTGETCOUNT_ATTRIBUTE=0,
                            ESMC_ATTGETCOUNT_ATTPACK,
                            ESMC_ATTGETCOUNT_ATTLINK,
                            ESMC_ATTGETCOUNT_TOTAL};
                        
// attnest flag type
enum ESMC_AttNest_Flag { ESMC_ATTNEST_OFF=0,
                         ESMC_ATTNEST_ON};

// Attribute reconcile type
enum ESMC_AttReconcileFlag { ESMC_ATTRECONCILE_OFF=0,
                             ESMC_ATTRECONCILE_ON};

// attwrite flag type
enum ESMC_AttWriteFlag { ESMC_ATTWRITE_TAB=0,
                         ESMC_ATTWRITE_XML};

// Item order when retrieving item lists from a Container object
enum ESMC_ItemOrder_Flag { ESMC_ITEMORDER_ABC=0,
                           ESMC_ITEMORDER_ADDORDER};

// Source term order in the destination sums during SMM
enum ESMC_TermOrder_Flag { ESMC_TERMORDER_SRCSEQ=0,
                           ESMC_TERMORDER_SRCPET,
                           ESMC_TERMORDER_FREE};
// Mesh spatial operator
enum ESMC_MeshOp_Flag { ESMC_MESHOP_DIFFERENCE=0 };

// TODO: investigate why this is a macro instead of an enum
#define ESMC_GRIDITEM_INVALID -2
#define ESMC_GRIDITEM_UNINIT  -1
#define ESMC_GRIDITEM_MASK     0
#define ESMC_GRIDITEM_AREA     1
#define ESMC_GRIDITEM_AREAM    2
#define ESMC_GRIDITEM_FRAC     3
#define ESMC_GRIDITEM_COUNT    4

// Needs to be kept in line with ESMF_ATT_GRIDDED_DIM_LABELS and
// ESMF_ATT_UNGRIDDED_DIM_LABELS in ../src/ESMCI_UtilTypes.F90
#define ESMC_ATT_GRIDDED_DIM_LABELS   "ESMF:gridded_dim_labels"
#define ESMC_ATT_UNGRIDDED_DIM_LABELS "ESMF:ungridded_dim_labels"

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
size_t ESMC_F90lentrim (const char *src, ESMCI_FortranStrLenArg slen);
char *ESMC_F90toCstring(const char *src, ESMCI_FortranStrLenArg slen);
int  ESMC_F90toCstring(const char *src, ESMCI_FortranStrLenArg slen, char *dst, ESMCI_FortranStrLenArg dlen);
int  ESMC_CtoF90string(const char *src, char *dst, ESMCI_FortranStrLenArg dlen);
void ESMC_cxxtoF90string(const std::string &src, char *dst, int *rc, 
                             /* hidden */ ESMCI_FortranStrLenArg dlen);
extern "C" {
void  FTN_X(esmf_f90tocstring)(const char *src, char *dst, int *rc, 
                             /* hidden */ ESMCI_FortranStrLenArg slen, ESMCI_FortranStrLenArg dlen);
void  FTN_X(esmf_ctof90string)(const char *src, char *dst, int *rc, 
                             /* hidden */ ESMCI_FortranStrLenArg slen, ESMCI_FortranStrLenArg dlen);
}

// return the objectName given the objectId
const char *ESMC_ObjectID_Name(int objectId);
// return byte counts for TypeKinds
int ESMC_TypeKind_FlagSize(ESMC_TypeKind_Flag dk);
// return a static string name for various enums
const char *ESMC_StatusString(ESMC_Status stat);
const char *ESMC_TypeKind_FlagString(ESMC_TypeKind_Flag dk);
const char *ESMC_LogicalString(ESMC_Logical tf);

extern "C" {
void FTN_X(esmf_pointertoint)(int *n, short *s, ESMC_POINTER *len);
void FTN_X(esmf_pointerdifference)(long *n, short *s1, short *s2, int *len);
void FTN_X(c_esmc_getcwd)(char *pathname, int *rc, ESMCI_FortranStrLenArg pathname_l);
void FTN_X(c_esmc_makedirectory)(const char *pathname, int *mode, ESMC_Logical *relaxedFlag,
      int *rc, ESMCI_FortranStrLenArg pathname_l);
}

// generate a Globally Unique ID (GUID) in a platform independent way (e.g.
//   does not require UUID library).
int ESMC_InitializeGUID(void);
int ESMC_GenerateGUID(std::string &guid);

// Find and replace all occurrences of a string in a given string with another.
// TODO:  When C++11 STL becomes supported in all our supported
// compilers/versions, replace with std::tr1::regex_replace() or simply
// std::regex_replace().
int ESMC_FindAndReplaceAll(std::string &subjectStr,
                           const std::string& searchStr,
                           const std::string& replaceStr);

// general reduction operator value - MUST MATCH F90
enum ESMC_Operation { ESMF_SUM=1, ESMF_MIN, ESMF_MAX};


extern ESMC_ObjectID ESMC_ID_BASE;
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
extern ESMC_ObjectID ESMC_ID_XGRID;
extern ESMC_ObjectID ESMC_ID_XGRIDGEOMBASE;
extern ESMC_ObjectID ESMC_ID_NONE;

#endif  // ESMCI_UTIL_H
