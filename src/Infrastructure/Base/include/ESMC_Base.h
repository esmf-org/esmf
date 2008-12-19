// $Id: ESMC_Base.h,v 1.99 2008/12/19 00:31:28 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
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
#include "ESMCI_VM.h"
#include "ESMCI_Attribute.h"

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
 class Attribute;

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
    
  
    // prevent accidental copying
    //ESMC_Base& operator=(const ESMC_Base&);
    ESMC_Base(const ESMC_Base&);
  
  public:
    ESMCI::Attribute root;

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
  }

#endif  // ESMC_BASE_H
