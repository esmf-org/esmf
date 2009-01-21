// $Id: ESMC_Base.h,v 1.76.2.3 2009/01/21 21:25:19 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Base C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMC_BASE_H
#define ESMC_BASE_H

//-----------------------------------------------------------------------------

 // Generic constants have been moved to Util.h - the constants in this 
 // file relate directly to the Base class now.

//-----------------------------------------------------------------------------

#include "stdio.h"

#include "ESMCI_Util.h"
#include "ESMC_VM.h"

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
 class ESMC_Attribute;


// Single Attribute, (name, value) pair which can contain:
//  int / Integer*4, single value or list
//  double / Real*8, single value or list
//  char / Character - single character string (lists not allowed)
//  ESMC_Logical - single value or arrays (note: not bool or .TRUE.)
class ESMC_Attribute 
{
 private:
    char attrName[ESMF_MAXSTR]; // inline to reduce memory thrashing
    ESMC_TypeKind tk;           // typekind indicator
    int items;                  // number of items (NOT byte count) for lists
    int slen;                   // for string, length, inc trailing NULL. 
    union {                     // overload pointers to conserve space 
      ESMC_I4    vi;       // integer, or
      ESMC_I4  *vip;       // pointer to integer list, or
      ESMC_I8   vtl;       // long, or
                  //  ^  TODO: change back to vl when Cray X1 compiler fixed
      ESMC_I8  *vlp;       // pointer to long list, or
      ESMC_R4    vf;       // float (real*4), or
      ESMC_R4  *vfp;       // pointer to float (real*4) list, or
      ESMC_R8    vd;       // double (real*8), or
      ESMC_R8  *vdp;       // pointer to double (real*8) list, or
      ESMC_Logical    vb;       // boolean (logical), or
      ESMC_Logical  *vbp;       // pointer to boolean (logical) list, or
      char          *vcp;       // pointer to a NULL term character string, or
      void        *voidp;       // cannot be dereferenced, but generic.
      // ESMC_Array  *ap;       // pointer to an ESMC_Array object (someday?)
    };

 public:
    int ESMC_Print(void) const;
    int ESMC_Serialize(char *buffer, int *length, int *offset) const;
    int ESMC_Deserialize(char *buffer, int *offset);
    ESMC_Attribute& operator=(const ESMC_Attribute &);
    ESMC_Attribute(void);
    ESMC_Attribute(char *name, ESMC_TypeKind tk, int numitems, void *datap);
    ~ESMC_Attribute(void);


  friend class ESMC_Base;

};


// !PRIVATE TYPES:

 // class declaration type.
class ESMC_Base
{
  protected:
    int           ID;           // unique ID for any object in this VM context
    ESMCI::VMId   *vmID;        // unique vmID for any VM in the system
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

    // accessors to object's vmID    
    void ESMC_BaseSetVMId(ESMCI::VMId *vmID);
    ESMCI::VMId *ESMC_BaseGetVMId(void) const;
    
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

    // flatten an object into a byte stream, and reconstitute it again
    int ESMC_Serialize(char *buffer, int *length, int *offset) const;
    int ESMC_Deserialize(char *buffer, int *offset);
    
    // optional Read/Write methods for any ESMF class
    virtual int ESMC_Read(void);
    virtual int ESMC_Write(void) const;

    // required Validate and Print methods
    //  (would be pure virtual =0; but some will be implemented in F90)
    virtual int ESMC_Validate(const char *options=0) const;
    virtual int ESMC_Print(const char *options=0) const;

    // attribute methods - set
    int ESMC_BaseAttSet(char *name, ESMC_I4 value);
    int ESMC_BaseAttSet(char *name, int count, ESMC_I4 *value);
    int ESMC_BaseAttSet(char *name, ESMC_I8 value);
    int ESMC_BaseAttSet(char *name, int count, ESMC_I8 *value);
    int ESMC_BaseAttSet(char *name, ESMC_R4 value);
    int ESMC_BaseAttSet(char *name, int count, ESMC_R4 *value);
    int ESMC_BaseAttSet(char *name, ESMC_R8 value);
    int ESMC_BaseAttSet(char *name, int count, ESMC_R8 *value);
    int ESMC_BaseAttSet(char *name, ESMC_Logical value);
    int ESMC_BaseAttSet(char *name, int count, ESMC_Logical *value);
    int ESMC_BaseAttSet(char *name, char *value);
    int ESMC_BaseAttSet(char *name, ESMC_TypeKind tk, int count, void *value);

    // attribute methods - get
    int ESMC_BaseAttGet(char *name, ESMC_I4 *value) const;
    int ESMC_BaseAttGet(char *name, int *count, ESMC_I4 *value) const;
    int ESMC_BaseAttGet(char *name, ESMC_I8 *value) const;
    int ESMC_BaseAttGet(char *name, int *count, ESMC_I8 *value) const;
    int ESMC_BaseAttGet(char *name, ESMC_R4 *value) const;
    int ESMC_BaseAttGet(char *name, int *count, ESMC_R4 *value) const;
    int ESMC_BaseAttGet(char *name, ESMC_R8 *value) const;
    int ESMC_BaseAttGet(char *name, int *count, ESMC_R8 *value) const;
    int ESMC_BaseAttGet(char *name, ESMC_Logical *value) const;
    int ESMC_BaseAttGet(char *name, int *count, ESMC_Logical *value) const;
    int ESMC_BaseAttGet(char *name, char *value) const;
    int ESMC_BaseAttGet(char *name, ESMC_TypeKind *tk, int *count, 
      void *value) const;
    int ESMC_BaseAttGet(int num, char *name, ESMC_TypeKind *tk, int *count,
      void *value) const;

    // count of attributes on an object
    int ESMC_BaseAttGetCount(void) const;


    // setting when you have an attribute already assembled
    int ESMC_BaseAttSet(ESMC_Attribute *attr);

    // getting either by name or number directly return attribute ptr
    ESMC_Attribute *ESMC_BaseAttGet(char *name) const;
    ESMC_Attribute *ESMC_BaseAttGet(int num) const;

    // extend pointer list
    int ESMC_AttributeAlloc(int adding);

    // not implemented yet
    int ESMC_BaseAttGetNameList(int *count, char **namelist) const;
    int ESMC_BaseAttSetList(int count, ESMC_Attribute *valuelist);
    int ESMC_BaseAttGetList(char **namelist, ESMC_Attribute *valuelist) const;
    int ESMC_BaseAttCopy(char *name, ESMC_Base *destination);
    int ESMC_BaseAttCopyAll(ESMC_Base *destination);

    // constructors/destructor
    ESMC_Base(void);
    ESMC_Base(char *superclass, char *name, int nattrs);
    virtual ~ESMC_Base(void);

};   // end class ESMC_Base

// fortran interface functions to base objects
extern "C" {
  void FTN(c_esmc_basecreate)(ESMC_Base **base, char *superclass, char *name,
                              int *nattrs, int *rc, int sclen, int nlen);
  void FTN(c_esmc_basedestroy)(ESMC_Base **base, int *rc);

  void FTN(c_esmc_baseserialize)(ESMC_Base **base, char *buf, int *length,
                                 int *offset, int *rc);
  void FTN(c_esmc_basedeserialize)(ESMC_Base **base, char *buf,
                                   int *offset, int *rc);

  void FTN(c_esmc_baseprint)(ESMC_Base **base, char *opts, int *rc, int nlen);
  void FTN(c_esmc_basevalidate)(ESMC_Base **base, char *opts, int *rc, int nlen);
  void FTN(c_esmc_getclassname)(ESMC_Base **base, char *name, int *rc, int nlen);
  void FTN(c_esmc_getname)(ESMC_Base **base, char *name, int *rc, int nlen);
  void FTN(c_esmc_setname)(ESMC_Base **base, char *classname, char *objname, 
                           int *rc, int clen, int olen);

  void FTN(c_esmc_baseattsetvalue)(ESMC_Base **base, char *name, 
                                    ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_baseattsetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);

  void FTN(c_esmc_baseattgetvalue)(ESMC_Base **base, char *name, 
                                    ESMC_TypeKind *tk,
                                    int *count, void *value, int *rc, int nlen);
  void FTN(c_esmc_baseattgetchar)(ESMC_Base **base, char *name, char *value, 
                                    int *rc, int nlen, int vlen);
  void FTN(c_esmc_baseattgetattrinfoname)(ESMC_Base **base, char *name, 
                                           ESMC_TypeKind *tk,
                                           int *count, int *rc, int nlen);
  void FTN(c_esmc_baseattgetattrinfonum)(ESMC_Base **base, int *num, 
                                           char *name,
                                           ESMC_TypeKind *tk, int *count, 
                                           int *rc, int nlen);
  void FTN(c_esmc_baseattgetcount)(ESMC_Base **base, int *count, int *rc);

}

// class utility functions, not methods, since they operate on
//  multiple objects
int ESMC_BaseAttSetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);
int ESMC_BaseAttGetObjectList(ESMC_Base *anytypelist, ESMC_Attribute *valuelist);

#endif  // ESMC_BASE_H
