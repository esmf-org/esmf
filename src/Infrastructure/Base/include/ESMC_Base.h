// $Id: ESMC_Base.h,v 1.37 2004/01/26 17:42:11 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMF Base C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMC_BASE_H
#define ESMC_BASE_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

#include "stdio.h"
#include <ESMC.h>

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Base - all ESMF classes inherit from ESMC_Base
//
// !DESCRIPTION:
// The code in this file implements the Base defined type
// and methods which operate on all derived types.
//
//-----------------------------------------------------------------------------
//
// !USES:

// !PUBLIC TYPES:
 class ESMC_Base;

// ESMF class states
enum ESMC_Status { ESMF_STATE_UNINIT=1,
                   ESMF_STATE_READY,
                   ESMF_STATE_UNALLOCATED,
                   ESMF_STATE_ALLOCATED,
                   ESMF_STATE_BUSY,
                   ESMF_STATE_INVALID };

#define ESMC_NULL_POINTER (0)
#define ESMC_BAD_POINTER (-1)

// ESMF data types and kinds
enum ESMC_DataType { ESMF_DATA_INTEGER=1,
                     ESMF_DATA_REAL,
                     ESMF_DATA_LOGICAL,
                     ESMF_DATA_CHARACTER };

enum ESMC_DataKind { ESMF_I1=1,
                     ESMF_I2,
                     ESMF_I4,
                     ESMF_I8,
                     ESMF_R4,
                     ESMF_R8,
                     ESMF_C8,
                     ESMF_C16 };

// ESMF platform-dependent data types
#ifdef ESMF_IS_32BIT_MACHINE
  typedef long long ESMF_KIND_I8;
  typedef int       ESMF_KIND_I4;
  typedef short     ESMF_KIND_I2;
  typedef char      ESMF_KIND_I1;
  typedef double    ESMF_KIND_R8;
  typedef float     ESMF_KIND_R4;
#else // 64-bit or larger machine
  typedef long      ESMF_KIND_I8;
  typedef int       ESMF_KIND_I4;
  typedef short     ESMF_KIND_I2;
  typedef char      ESMF_KIND_I1;
  typedef double    ESMF_KIND_R8;
  typedef float     ESMF_KIND_R4;
#endif

// general logical value - MUST MATCH F90
enum ESMC_Logical { ESMF_TRUE=1,
                    ESMF_FALSE };

// max/min macros if they don't already exist
#ifndef MAX
#define MAX(a,b)  (((a)>(b))?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b)  (((a)<(b))?(a):(b))
#endif

// union to hold a value of any type
struct ESMC_DataValue {
  //private:
    ESMC_DataType dt;
    int rank;
    union {         // overload pointers to conserve space 
      int     vi;               // could be an integer,
      int    *vip;              // pointer to integer,
      double  vr;               // double (real),
      double *vrp;              // pointer to double (real),
      bool    vl;               // boolean (logical),
      bool   *vlp;              // pointer to boolean (logical),
      char    vc[ESMF_MAXSTR];  // character string,
      char   *vcp;              // or a pointer to a character string
      void   *voidp;            // cannot be dereferenced
    };
};


// elemental attribute
struct ESMC_Attribute {
  //private:
    char           attrName[ESMF_MAXSTR];
    //ESMC_DataType  attrType;   // redundant w/ dt in datavalue
    ESMC_DataValue attrValue;
};

// elemental index for axis decompositions
struct ESMC_AxisIndex {
    int     min;
    int     max;
    int  stride;
};

// collection of AxisIndices per axis, to describe an n-dim cube
struct ESMC_Domain {
    int DE;
    int rank;
    struct ESMC_AxisIndex ai_list[ESMF_MAXDIM];
};

// collection of AxisIndices per axis, to describe an n-dim cube
class ESMC_DomainList {
  public:   // TODO: fix this
    int num_domains;
    int current_size;
    int total_points;
    int pad_for_64;
    struct ESMC_Domain *domains;

    int  ESMC_DomainListGetDE(int domainnum);
    struct ESMC_AxisIndex ESMC_DomainListGetAI(int domainnum, int ainum);
    
};

// !PRIVATE TYPES:

 // class declaration type.  WARNING: this must match layout in memory
 // corresponding F90 base derived type.  do not change one without
 // changing the other.
class ESMC_Base
{
  protected:
    int           ID;         // unique ID of this instance
    int           refCount;   // number of references to this instance
    ESMC_Status   baseStatus; // status of an instance of Base derived class
    char          baseName[ESMF_MAXSTR];  // object name

  private:
    int attrCount;            // number of attributes in use in list
    int attrAlloc;            // number of attributes currently allocated
    ESMC_Attribute *attr;     // attribute list

// !PUBLIC MEMBER FUNCTIONS:
  
  public:
    // required & optional standard interface methods for all ESMF classes.
    // should not instantiate a ESMC_Base object directly; must sub-class first.

    // for shallow classes
//  int ESMC_BaseInit(void);

    // for deep classes
//  ESMC_Base *ESMC_BaseCreate(int *rc);
//  int        ESMC_BaseDestroy(void);
//  int        ESMC_BaseConstruct(void);
//  int        ESMC_BaseDestruct(void);

    // configuration methods for any ESMF class
//  int ESMC_BaseGetConfig(void) const;
//  int ESMC_BaseSetConfig(void);

    // accessor to number of class instances
    int  ESMC_BaseGetInstCount(void) const;

    // accessors to unique ID
    void ESMC_BaseSetID(int id);
    int  ESMC_BaseGetID(void) const;

    // accessors to reference count
    void ESMC_BaseSetRefCount(int count);
    int  ESMC_BaseGetRefCount(void) const;

    // accessors to base status
    void        ESMC_BaseSetStatus(ESMC_Status status);
    ESMC_Status ESMC_BaseGetStatus(void) const;
 
    // accessors to base name
    int  ESMC_BaseSetName(char *name, char *context);
    char *ESMC_BaseGetName(void) const;

    // optional Read/Write methods for any ESMF class
    virtual int ESMC_Read(void);
    virtual int ESMC_Write(void) const;

    // required Validate and Print methods
    //  (would be pure virtual =0; but some will be implemented in F90)
    virtual int ESMC_Validate(const char *options=0) const;
    virtual int ESMC_Print(const char *options=0) const;

    // attribute methods
    int ESMC_AttributeSet(char *name, ESMC_DataValue value);
    int ESMC_AttributeGet(char *name, ESMC_DataType *type,
                          ESMC_DataValue *value) const;
    int ESMC_AttributeAlloc(int adding);

    int ESMC_AttributeGetCount(void) const;
    int ESMC_AttributeGetbyNumber(int number, char *name, ESMC_DataType *type,
                                  ESMC_DataValue *value) const;
    int ESMC_AttributeGetNameList(int *count, char **namelist) const;

    int ESMC_AttributeSetList(char **namelist, ESMC_DataValue *values);
    int ESMC_AttributeGetList(char **namelist, ESMC_DataType *typelist,
                              ESMC_DataValue *valuelist) const;

    int ESMC_AttributeCopy(char *name, ESMC_Base *destination);
    int ESMC_AttributeCopyAll(ESMC_Base *destination);

    // constructor/destructor
    ESMC_Base(void);
    ~ESMC_Base(void);

};   // end class ESMC_Base

// class utility functions, not methods, since they operate on
//  multiple objects
int ESMC_AttributeSetObjectList(ESMC_Base *anytypelist, char *name,
                                ESMC_DataValue *value);
int ESMC_AttributeGetObjectList(ESMC_Base *anytypelist, char *name,
                                ESMC_DataType *typelist,
                                ESMC_DataValue *valuelist);

int ESMC_AxisIndexSet(ESMC_AxisIndex *ai, int min, int max, int stride);
int ESMC_AxisIndexGet(ESMC_AxisIndex *ai, int *min, int *max, int *stride);
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

// return byte counts for DataKinds
int ESMC_DataKindSize(ESMC_DataKind dk);

extern "C" {
void FTN(f_esmf_domainlistgetde)(ESMC_DomainList *, int *, int *, int *);
void FTN(f_esmf_domainlistgetai)(ESMC_DomainList *, int *, int *, 
                                                    ESMC_AxisIndex *ai, int *);
}

#endif  // ESMC_BASE_H
