// $Id: ESMC_Base.h,v 1.3 2003/02/03 05:51:52 eschwab Exp $
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

#include <ESMF_Macros.inc>

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
                   ESMF_STATE_ALLOCATED,
                   ESMF_STATE_BUSY,
                   ESMF_STATE_INVALID };

// ESMF data types and kinds
enum ESMC_DataType { ESMF_DATA_INTEGER=1,
                     ESMF_DATA_REAL,
                     ESMF_DATA_LOGICAL,
                     ESMF_DATA_CHARACTER };

enum ESMC_DataKind { ESMF_KIND_I1=1,
                     ESMF_KIND_I2,
                     ESMF_KIND_I4,
                     ESMF_KIND_I8,
                     ESMF_KIND_R4,
                     ESMF_KIND_R8,
                     ESMF_KIND_C8,
                     ESMF_KIND_C16 };

// ESMF platform-dependent data types
#ifdef ESMF_IS_32BIT_MACHINE
  typedef long long ESMF_IKIND_I8;
  typedef int       ESMF_IKIND_I4;
  typedef short     ESMF_IKIND_I2;
  typedef char      ESMF_IKIND_I1;
  typedef double    ESMF_IKIND_R8;
  typedef float     ESMF_IKIND_R4;
#else // 64-bit or larger machine
  typedef long      ESMF_IKIND_I8;
  typedef int       ESMF_IKIND_I4;
  typedef short     ESMF_IKIND_I2;
  typedef char      ESMF_IKIND_I1;
  typedef double    ESMF_IKIND_R8;
  typedef float     ESMF_IKIND_R4;
#endif

// general logical value
enum ESMC_Logical { ESMF_TF_UNKNOWN=1,
                    ESMF_TF_TRUE,
                    ESMF_TF_FALSE };

// union to hold a value of any type
struct ESMC_DataValue {
  private:
    ESMC_DataType dt;
    int rank;
    union {   // can't do in F90 ?? EQUIVALENCE statement too limited ?? TODO
      int     vi;               // could be an integer,
      int    *vip;              // pointer to integer,
      double  vr;               // double (real),
      double *vrp;              // pointer to double (real),
      bool    vl;               // boolean (logical),
      bool   *vlp;              // pointer to boolean (logical),
      char    vc[ESMF_MAXSTR];  // character string,
      char   *vcp;              // or a pointer to a character string
    };
};

// elemental attribute
struct ESMC_Attribute {
  private:
    char           attrName[ESMF_MAXSTR];
    ESMC_DataType  attrType;
    ESMC_DataValue attrValue;
};

// !PRIVATE TYPES:

 // class declaration type
class ESMC_Base
{
  private:
    int attrCount;            // number of attributes in list
    ESMC_Attribute *attr;     // attribute list

  protected:
    static int    instCount;  // number of instances of this class
    int           ID;         // unique ID of this instance
    int           refCount;   // number of references to this instance
    ESMC_Status   baseStatus; // status of an instance of Base derived class

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
 
    // optional Read/Write methods for any ESMF class
    virtual int ESMC_Read(void);
    virtual int ESMC_Write(void) const;

    // required Validate and Print methods
    //  (would be pure virtual =0; but some will be implemented in F90)
    virtual int ESMC_Validate(void) const;
    virtual int ESMC_Print(void) const;

    // attribute methods
    int ESMC_AttributeSet(char *name, ESMC_DataValue value);
    int ESMC_AttributeGet(char *name, ESMC_DataType *type,
                          ESMC_DataValue *value) const;

    int ESMC_AttributeGetCount(int count) const;
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

#endif  // ESMC_BASE_H
