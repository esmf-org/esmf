// $Id: ESMC_Base.h,v 1.50 2004/04/23 17:31:39 nscollins Exp $
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
#include <ESMC_Start.h>

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

// WARNING:  the values of these enums MUST match the values defined
//  in ../interface/ESMF_Base.F90

// general logical value - MUST MATCH F90
enum ESMC_Logical { ESMF_TRUE=1,
                    ESMF_FALSE };

// ESMF class states
enum ESMC_Status { ESMF_STATE_UNINIT=1,
                   ESMF_STATE_READY,
                   ESMF_STATE_UNALLOCATED,
                   ESMF_STATE_ALLOCATED,
                   ESMF_STATE_BUSY,
                   ESMF_STATE_INVALID };

#define ESMC_NULL_POINTER (0)
#define ESMC_BAD_POINTER (-1)

// comm types
enum ESMC_BlockingFlag { ESMF_BLOCKING=1,
                         ESMF_NONBLOCKING};

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

// max/min macros if they don't already exist
#ifndef MAX
#define MAX(a,b)  (((a)>(b))?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b)  (((a)<(b))?(a):(b))
#endif

// Single Attribute, (name, value) pair which can contain:
//  int / Integer*4, single value or list
//  double / Real*8, single value or list
//  char / Character - single character string (lists not allowed)
//  ESMC_Logical - single value or arrays (note: not bool or .TRUE.)
struct ESMC_Attribute {
 //private:   // TODO: fix the interfaces and then make this private again
    char attrName[ESMF_MAXSTR]; // inline to reduce memory thrashing
    ESMC_DataType dt;           // type for selecting the right pointer below
    int items;                  // number of items (NOT byte count) for lists
    int slen;                   // for string, length, inc trailing NULL. 
    union {                     // overload pointers to conserve space 
      int     vi;               // integer, or
      int    *vip;              // pointer to integer list, or
      double  vr;               // double (real), or
      double *vrp;              // pointer to double (real) list, or
      ESMC_Logical    vl;       // boolean (logical), or
      ESMC_Logical   *vlp;      // pointer to boolean (logical) list, or
      char   *vcp;              // pointer to a NULL term character string, or
      void   *voidp;            // cannot be dereferenced, but generic.
      // ESMC_Array *ap,        // pointer to an ESMC_Array object (someday?)
    };

 public:
    int ESMC_Print(void);
    ESMC_Attribute& operator=(const ESMC_Attribute &);
    ESMC_Attribute(void);
    ESMC_Attribute(char *name, ESMC_DataType datatype, int numitems, 
                   void *datap);
    ~ESMC_Attribute(void);


  friend class ESMC_Base;

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
  public:   // TODO: fix this to be private?
    int num_domains;
    int current_size;
    int total_points;
    int pad_for_64;
    struct ESMC_Domain *domains;

    int  ESMC_DomainListGetDE(int domainnum);
    struct ESMC_AxisIndex ESMC_DomainListGetAI(int domainnum, int ainum);
    
};

// !PRIVATE TYPES:

 // class declaration type.
class ESMC_Base
{
  protected:
    int           ID;           // unique ID for any object in the system
    int           refCount;     // number of references to this instance
    int           classID;      // unique ID relative to this class
    ESMC_Status   baseStatus;   // status of an instance of Base derived class
    char          baseName[ESMF_MAXSTR];    // object name, unique over class 
    char          baseNameF90[ESMF_MAXSTR]; // same name, non-null terminated
    char          className[ESMF_MAXSTR];   // object class

  private:
    int attrCount;              // number of attributes in use in list
    int attrAlloc;              // number of attributes currently allocated
    ESMC_Attribute **attrList;  // attributes - array of pointers

// !PUBLIC MEMBER FUNCTIONS:
  
  public:
    // required & optional standard interface methods for all ESMF classes.
    // should not instantiate a ESMC_Base object directly; must sub-class first.

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
    int   ESMC_BaseSetName(char *name, char *classname);
    char *ESMC_BaseGetName(void) const;
    int   ESMC_BaseSetF90Name(char *name, int nlen);
    char *ESMC_BaseGetF90Name(void) const;

    // accessors to class name
    int   ESMC_BaseSetClassName(char *classname);
    char *ESMC_BaseGetClassName(void) const;
    int   ESMC_BaseSetF90ClassName(char *name, int nlen);
    int   ESMC_BaseGetF90ClassName(char *name, int nlen) const;

    // optional Read/Write methods for any ESMF class
    virtual int ESMC_Read(void);
    virtual int ESMC_Write(void) const;

    // required Validate and Print methods
    //  (would be pure virtual =0; but some will be implemented in F90)
    virtual int ESMC_Validate(const char *options=0) const;
    virtual int ESMC_Print(const char *options=0) const;

    // attribute methods - set
    int ESMC_AttributeSet(char *name, int value);
    int ESMC_AttributeSet(char *name, int count, int *value);
    int ESMC_AttributeSet(char *name, double value);
    int ESMC_AttributeSet(char *name, int count, double *value);
    int ESMC_AttributeSet(char *name, ESMC_Logical value);
    int ESMC_AttributeSet(char *name, int count, ESMC_Logical *value);
    int ESMC_AttributeSet(char *name, char *value);
    int ESMC_AttributeSet(char *name, ESMC_DataType dt, int count, void *value);

    // attribute methods - get
    int ESMC_AttributeGet(char *name, int *value) const;
    int ESMC_AttributeGet(char *name, int *count, int *value) const;
    int ESMC_AttributeGet(char *name, double *value) const;
    int ESMC_AttributeGet(char *name, int *count, double *value) const;
    int ESMC_AttributeGet(char *name, ESMC_Logical *value) const;
    int ESMC_AttributeGet(char *name, int *count, ESMC_Logical *value) const;
    int ESMC_AttributeGet(char *name, char *value) const;
    int ESMC_AttributeGet(char *name, ESMC_DataType *dt, int *count, void *value) const;
    int ESMC_AttributeGet(int num, char *name, ESMC_DataType *dt, int *count, void *value) const;

    // count of attributes on an object
    int ESMC_AttributeGetCount(void) const;


    // setting when you have an attribute already assembled
    int ESMC_AttributeSet(ESMC_Attribute *attr);

    // getting either by name or number directly return attribute ptr
    ESMC_Attribute *ESMC_AttributeGet(char *name) const;
    ESMC_Attribute *ESMC_AttributeGet(int num) const;

    // extend pointer list
    int ESMC_AttributeAlloc(int adding);

    // not implemented yet
    int ESMC_AttributeGetNameList(int *count, char **namelist) const;
    int ESMC_AttributeSetList(int count, ESMC_Attribute *valuelist);
    int ESMC_AttributeGetList(char **namelist, ESMC_Attribute *valuelist) const;
    int ESMC_AttributeCopy(char *name, ESMC_Base *destination);
    int ESMC_AttributeCopyAll(ESMC_Base *destination);

    // constructors/destructor
    ESMC_Base(void);
    ESMC_Base(char *superclass, char *name, int nattrs);
    ~ESMC_Base(void);

};   // end class ESMC_Base

// fortran interface functions to base objects
extern "C" {
  void FTN(c_esmc_basecreate)(ESMC_Base **base, char *superclass, char *name,
                              int *nattrs, int *rc, int sclen, int nlen);
  void FTN(c_esmc_basedestroy)(ESMC_Base **base, int *rc);

  void FTN(c_esmc_baseprint)(ESMC_Base **base, char *opts, int *rc, int nlen);
  void FTN(c_esmc_getclassname)(ESMC_Base **base, char *name, int *rc, int nlen);
  void FTN(c_esmc_getname)(ESMC_Base **base, char *name, int *rc, int nlen);
  void FTN(c_esmc_setname)(ESMC_Base **base, char *classname, char *objname, int *rc, int clen, int olen);

  void FTN(c_esmc_attributesetvalue)(ESMC_Base **base, char *name, ESMC_DataType *dt, int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_attributesetchar)(ESMC_Base **base, char *name, char *value, int *rc, int nlen, int vlen);

  void FTN(c_esmc_attributegetvalue)(ESMC_Base **base, char *name, ESMC_DataType *dt, int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_attributegetchar)(ESMC_Base **base, char *name, char *value, int *rc, int nlen, int vlen);
  void FTN(c_esmc_attributegetattrinfoname)(ESMC_Base **base, char *name, ESMC_DataType *dt, int *count, int *rc, int nlen);
  void FTN(c_esmc_attributegetattrinfonum)(ESMC_Base **base, int num, char *name, ESMC_DataType *dt, int *count, int *rc, int nlen);
  void FTN(c_esmc_attributegetcount)(ESMC_Base **base, int *count, int *rc);

}

// class utility functions, not methods, since they operate on
//  multiple objects
int ESMC_AttributeSetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);
int ESMC_AttributeGetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);

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
// return a static string name for various enums
const char *ESMC_StatusString(ESMC_Status stat);
const char *ESMC_DataTypeString(ESMC_DataType dt);
const char *ESMC_DataKindString(ESMC_DataKind dk);
const char *ESMC_LogicalString(ESMC_Logical tf);

extern "C" {
void FTN(f_esmf_domainlistgetde)(ESMC_DomainList *, int *, int *, int *);
void FTN(f_esmf_domainlistgetai)(ESMC_DomainList *, int *, int *, 
                                                    ESMC_AxisIndex *ai, int *);
}

// general reduction operator value - MUST MATCH F90
enum ESMC_newOp { ESMF_newSUM=1, ESMF_newMIN, ESMF_newMAX};

#endif  // ESMC_BASE_H
