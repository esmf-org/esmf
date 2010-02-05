// $Id: ESMC_Base.C,v 1.128.2.1 2010/02/05 19:54:06 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

#define ESMF_FILENAME "ESMC_Base.C"

// ESMC Base method implementation (body) file

// single blank line to make protex happy.
//BOP

//EOP
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Base methods declared
// in the companion file ESMC_Base.h
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include <vector>
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base.C,v 1.128.2.1 2010/02/05 19:54:06 svasquez Exp $";
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMC_Base routines
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PRIVATE:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
/*
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Base_operator="
//BOPI
// !IROUTINE:  ESMC_Base_operator= - empty private operator =
//
// !INTERFACE:
      ESMC_Base& ESMC_Base::operator=(
//
// !ARGUMENTS:
      const ESMC_Base&) {
// 
// !RETURN VALUE:
//    Base object.
// 
// !DESCRIPTION:
//    Empty private operator =.
//
//EOPI

}  // end ESMC_Base_operator=
//-----------------------------------------------------------------------------
*/
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Base"
//BOPI
// !IROUTINE:  ESMC_Base - empty private copy constructor
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(
//
// !ARGUMENTS:
      const ESMC_Base&) {
// 
// !RETURN VALUE:
//    Base object.
// 
// !DESCRIPTION:
//    Empty private copy constructor.
//
//EOPI

}  // end ESMC_Base
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// PUBLIC:
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetClassName"
//BOPI
// !IROUTINE:  ESMC_BaseGetClassName - Get Base class name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetClassName(
//
// !ARGUMENTS:
      void)  const {
// 
// !RETURN VALUE:
//    Character pointer to class name.
// 
// !DESCRIPTION:
//    Accessor method for the class name of the object.
//
//EOPI

  return (char * ) className;

}  // end ESMC_BaseGetClassName

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetF90ClassName"
//BOPI
// !IROUTINE:  ESMC_BaseGetF90ClassName - Get Base class name in Fortran format
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetF90ClassName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
     char *name,         // in - Location to copy name into
     int nlen) const {   // in - Maximum length of string buffer
// 
// !DESCRIPTION:
//     Return a separate copy of the base class name, in Fortran friendly
//     format, which means not null terminated, and space filled.  
//     Will not copy more than {\tt nlen} bytes into {\tt name} string.
//
//EOPI

  // the (char *) cast is to try to make the compiler happy:
  return ESMC_CtoF90string((char *)className, name, nlen);

}  // end ESMC_BaseGetF90ClassName

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetID"
//BOPI
// !IROUTINE:  ESMC_BaseGetID - Get Base class unique ID
//  
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetID(
// 
// !ARGUMENTS:
      void) const {
//  
// !RETURN VALUE:
//    Unique object ID.
//  
// !DESCRIPTION:
//    Returns the unique object ID.
//  
//EOPI

  return ID;

} // end ESMC_BaseGetID

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetVMId"
//BOPI
// !IROUTINE:  ESMC_BaseGetVMId - Get Base class VMId
//  
// !INTERFACE:
      ESMCI::VMId *ESMC_Base::ESMC_BaseGetVMId(
// 
// !ARGUMENTS:
      void) const {
//  
// !RETURN VALUE:
//    Unique VMId of the context in which this base object was created
//  
// !DESCRIPTION:
//    Returns the object's VMId.
//  
//EOPI

  return vmID;

} // end ESMC_BaseGetVMId

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetVMId"
//BOPI
// !IROUTINE:  ESMC_BaseSetVMId - Set Base class VMId
//  
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetVMId(
// 
// !ARGUMENTS:
      ESMCI::VMId *vmID) {
//  
//  
// !DESCRIPTION:
//    Set the unique VMId of the context in which this base object was created
//  
//EOPI
  int localrc;
  
  this->vmID = new ESMCI::VMId;             // allocate space for this VMId
  *(this->vmID) = ESMCI::VMIdCreate(&localrc);// allocate internal VMId memory
  ESMCI::VMIdCopy(this->vmID, vmID);  // copy content of vmID to this->vmID.
  vmIDCreator = true;  // Base object is responsible for vmID deallocation

} // end ESMC_BaseSetVMId

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetName"
//BOPI
// !IROUTINE:  ESMC_BaseGetName - Get Base object name
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetName(
// 
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//    Character pointer to {\tt ESMC\_Base} name.
// 
// !DESCRIPTION:
//    Accessor method for the {\tt ESMC\_Base} name.
//
//EOPI

  return (char * )baseName;

}  // end ESMC_BaseGetName

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetF90Name"
//BOPI
// !IROUTINE:  ESMC_BaseGetF90Name - Get Base object name in Fortran format
//
// !INTERFACE:
      char *ESMC_Base::ESMC_BaseGetF90Name(
// 
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//     Pointer to object name, not null terminated and space filled.
// 
// !DESCRIPTION:
//     Accessor to base class name returned in Fortran friendly format, which
//     means not null terminated, and space filled.
//
//EOPI

  return (char * )baseNameF90;

}  // end ESMC_BaseGetF90Name

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetRefCount"
//BOPI
// !IROUTINE:  ESMC_BaseGetRefCount - Get Base class reference count
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetRefCount(
// 
// !ARGUMENTS:
      void) const {
//
// !RETURN VALUE:
//    Integer reference count.
//
// !DESCRIPTION:
//    Accessor method for base class reference count.
//
//EOPI

  return refCount;
} // end ESMC_BaseGetRefCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetClassName"
//BOPI
// !IROUTINE:  ESMC_BaseSetClassName - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetClassName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *classname) {    // in - context in which name should be unique
// 
// !DESCRIPTION:
//    Accessor method to set base class name.
//
//EOPI

  int rc, len;
  char msgbuf[ESMF_MAXSTR];
 
    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  if (classname) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "Error: object type %d bytes longer than limit of %d\n",
                          len, ESMF_MAXSTR-1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
     }
  }

  strcpy(className, classname ? classname : "global");

  return ESMF_SUCCESS;

}  // end ESMC_BaseSetClassName
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetF90ClassName"
//BOPI
// !IROUTINE:  ESMC_BaseSetF90ClassName - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetF90ClassName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,      // in - contains name to set in fortran format
      int nlen) {      // in - length of the input name buffer
// 
// !DESCRIPTION:
//    Accessor method to set base class name.
//
//EOPI
  int rc;
  char msgbuf[ESMF_MAXSTR];

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  if (nlen > ESMF_MAXSTR) {
       sprintf(msgbuf, "string name %d bytes longer than limit of %d bytes\n",
                       nlen, ESMF_MAXSTR);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
  }

  return ESMC_F90toCstring(name, nlen, className, ESMF_MAXSTR);

}  // end ESMC_BaseSetF90ClassName

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetID"
//BOPI
// !IROUTINE:  ESMC_BaseSetID - Set Base class unique ID
//  
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetID(
//  
// !RETURN VALUE:
//    none
//  
// !ARGUMENTS:
      int id) {   // in - ID to set
//  
// !DESCRIPTION: 
//     override default ID (see constructor)
//  
//EOPI

  ID = id;

}  // end ESMC_BaseSetID

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetName"
//BOPI
// !IROUTINE:  ESMC_BaseSetName - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetName(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      const char *name,           // in - base name to set
      const char *classname) {    // in - context in which name should be unique
// 
// !DESCRIPTION:
//     Accessor method for base class name.
//
//EOPI

  int len, rc;
  int defname, defclass;
  char msgbuf[ESMF_MAXSTR];
 
    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  // no name, no context:  generate a name "globalXXX" where xxx is a seq num
  // no name, but a context: name is contextXXX with the seq num again
  // name given: use it as is
  defname = 1;
  defclass = 1;

  // simple error checks first
  if (name && (name[0]!='\0')) { 
     len = strlen(name);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "object name %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
     }
     defname = 0;
  } 

  if (classname && (classname[0]!='\0')) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "object type %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
     }
     defclass = 0;
  }

  strcpy(className, defclass ? "global" : classname);
  if (defname) 
      sprintf(baseName, "%s%03d", className, ID); 
  else
      strcpy(baseName, name);

  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  return ESMF_SUCCESS;

}  // end ESMC_BaseSetName
 
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetF90Name"
//BOPI
// !IROUTINE:  ESMC_BaseSetF90Name - Set Base class name
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseSetF90Name(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      char *name,     // in - class name to set, in fortran format
      int nlen) {     // in - length of class name buffer
// 
// !DESCRIPTION:
//     Accessor method to set base class name.
//
//EOPI
  int rc;
  char msgbuf[ESMF_MAXSTR];

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  if (nlen > ESMF_MAXSTR) {
       sprintf(msgbuf, "string name %d bytes longer than limit of %d bytes\n",
                       nlen, ESMF_MAXSTR);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
  }

  memcpy(baseNameF90, name, nlen);
  if (nlen < ESMF_MAXSTR) 
      memset(baseNameF90 + nlen, (int)' ', ESMF_MAXSTR-nlen);

  ESMC_F90toCstring(baseNameF90, ESMF_MAXSTR-1, baseName, ESMF_MAXSTR);
  return ESMF_SUCCESS;

}  // end ESMC_BaseSetF90Name


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetRefCount"
//BOPI
// !IROUTINE:  ESMC_BaseSetRefCount - Set Base class reference count
//
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetRefCount(
// 
// !RETURN VALUE:
//    none
// 
// !ARGUMENTS:
      int count) {
// 
// !DESCRIPTION:
//     Accessor method for reference count.
//
//EOPI

  refCount = count;

} // end ESMC_BaseSetRefCount

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Deserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset,           // inout - original offset
      const ESMC_AttReconcileFlag &attreconflag) {  // in - attreconcile flag
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
    
    int *ip, i, nbytes;
    ESMC_Status *sp;
    ESMC_ProxyFlag *pfp;
    char *cp;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    ip = (int *)(buffer + *offset);
    ID = *ip++;
    refCount = *ip++;  
    classID = *ip++;  
    sp = (ESMC_Status *)ip;
    baseStatus = *sp++;
    status = *sp++;
    pfp = (ESMC_ProxyFlag *)sp;
    proxyflag = *pfp++;
    cp = (char *)pfp;
    memcpy(baseName, cp, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    memcpy(baseNameF90, cp, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    memcpy(className, cp, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    ip = (int *)cp;
    cp = (char *)ip;

    // update offset to point to past the current obj
    *offset = (cp - buffer);

    // setup the root Attribute, passing the address of this
    root.setBase(this);

    // Deserialize the Attribute hierarchy
    if (attreconflag == ESMC_ATTRECONCILE_ON) {
      localrc = root.ESMC_Deserialize(buffer,offset);
    }
        
  return ESMF_SUCCESS;

 } // end ESMC_Deserialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOPI
// !IROUTINE:  ESMC_Print - Print contents of a Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Print(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const char *options) const {     //  in - print options
//
// !DESCRIPTION:
//    Print the contents of an {\tt ESMC\_Base} object.  Expected to be
//    called internally from the object-specific print routines.
//
//EOPI

  char msgbuf[ESMF_MAXSTR];
  int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // PLI -- 10/4/2007 -- use this function to print attribute lists in various ESMF classes
    // No need to print the base object ID, Name or Class because these information will be
    // printed at the derived class
 
    //  sprintf(msgbuf,
    //   "Base object ID: %d, Ref count: %d, Status=%s, Name=%s, Class=%s\n", 
    //       ID, refCount, ESMC_StatusString(baseStatus), baseName, className);
    // printf(msgbuf);
    // ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
    
  // root Attribute
  sprintf(msgbuf, "   Root Attribute\n");
  printf(msgbuf);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOG_INFO);
  
  // traverse the Attribute hierarchy, printing as we go
  root.ESMC_Print();

  return ESMF_SUCCESS;

 } // end ESMC_Print

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Read"
//BOPI
// !IROUTINE:  ESMC_Read - Read in contents of a Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Read(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//    Base class provides stubs for optional read/write methods.
//
//EOPI

  return ESMC_RC_NOT_IMPL;

 } // end ESMC_Read

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Serialize"
//BOPI
// !IROUTINE:  ESMC_Serialize - Turn the object information into a byte stream
//
// !INTERFACE:
      int ESMC_Base::ESMC_Serialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset,           // inout - original offset
      const ESMC_AttReconcileFlag &attreconflag,     // in - attreconcile flag
      const ESMC_InquireFlag &inquireflag) const {   // in - inquire flag
//
// !DESCRIPTION:
//    Turn info in base class into a stream of bytes.
//
//EOPI
    int fixedpart;
    int *ip, i, rc;
    ESMC_Status *sp;
    ESMC_ProxyFlag *pfp;
    char *cp;

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    fixedpart = sizeof(ESMC_Base);
    if (inquireflag == ESMF_INQUIREONLY) {
      *offset += fixedpart;
    } else {
      if ((*length - *offset) < fixedpart) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                               "Buffer too short to add a Base object", &rc);
        return ESMF_FAILURE; 
        //buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
        //*length += 2 * fixedpart;
      }

      ip = (int *)(buffer + *offset);
      *ip++ = ID;
      *ip++ = refCount;  
      *ip++ = classID;  

      sp = (ESMC_Status *)ip;
      *sp++ = baseStatus;
      *sp++ = status;

      pfp = (ESMC_ProxyFlag *)sp;
      *pfp++ = proxyflag;

      cp = (char *)pfp;
      memcpy(cp, baseName, ESMF_MAXSTR);
      cp += ESMF_MAXSTR;
      memcpy(cp, baseNameF90, ESMF_MAXSTR);
      cp += ESMF_MAXSTR;
      memcpy(cp, className, ESMF_MAXSTR);
      cp += ESMF_MAXSTR;

      ip = (int *)cp;
      cp = (char *)ip;

      // update the offset before calling AttributeSerialize
      *offset = (cp - buffer);
    }

    // Serialize the Attribute hierarchy
    if (attreconflag == ESMC_ATTRECONCILE_ON) { 
      rc = root.ESMC_Serialize(buffer,length,offset, inquireflag);
    }

  return ESMF_SUCCESS;

 } // end ESMC_Serialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Validate"
//BOPI
// !IROUTINE:  ESMC_Validate - Internal consistency check for Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Validate(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//     Validation of the {\tt ESMC\_Base} object.  Expected to be called
//     internally from the object-specific validation methods.
//
//EOPI

  int localrc;

   // Initialize local return code; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

  if (baseStatus != ESMF_STATUS_READY) 
    return ESMF_FAILURE;

  return ESMF_SUCCESS;

 } // end ESMC_Validate


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Write"
//BOP
// !IROUTINE:  ESMC_Write - Write out contents of a Base object
//
// !INTERFACE:
      int ESMC_Base::ESMC_Write(
// 
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
// 
// !ARGUMENTS:
      void) const {
// 
// !DESCRIPTION:
//    Base class provides stubs for optional read/write methods.
//
//EOP

  return ESMC_RC_NOT_IMPL;

} // end ESMC_Write


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Base()"
//BOPI
// !IROUTINE:  ESMC_Base - native C++ constructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//   default initialization 
//
//EOPI
  int rc;
  
  vmID = ESMCI::VM::getCurrentID(&rc);  // get vmID of current VM context
//  ESMCI::VMIdPrint(vmID);
  vmIDCreator = false;  // vmID points into global table
  
  // set ID
  ID = ESMCI::VM::getBaseIDAndInc(vmID);
  
  // add object to list for automatic garbage collection
  ESMCI::VM::addObject(this, vmID);

  refCount = 1;
  strcpy(className, "global");
  sprintf(baseName, "%s%3d", "unnamed", ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);
  
  // setup the root Attribute, passing the address of this
  root.setBase(this);
  
  baseStatus  = ESMF_STATUS_READY;
  status      = ESMF_STATUS_READY;
  proxyflag   = ESMF_PROXYNO;

 } // end ESMC_Base

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Base()"
//BOPI
// !IROUTINE:  ESMC_Base - native C++ constructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(int id) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//   default initialization 
//
//EOPI
  int rc;
  
  vmID = ESMCI::VM::getCurrentID(&rc);  // get vmID of current VM context
//  ESMCI::VMIdPrint(vmID);
  vmIDCreator = false;  // vmID points into global table
  
  // set ID to objectCount;
  ID = id;
  
  // add object to list for automatic garbage collection
  ESMCI::VM::addObject(this, vmID);

  refCount = 1;
  strcpy(className, "global");
  sprintf(baseName, "%s%3d", "unnamed", ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);
  
  // setup the root Attribute, passing the address of this
  root.setBase(this);
  
  baseStatus  = ESMF_STATUS_READY;
  status      = ESMF_STATUS_READY;
  proxyflag   = ESMF_PROXYNO;

 } // end ESMC_Base

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Base()"
//BOPI
// !IROUTINE:  ESMC_Base - native C++ constructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::ESMC_Base(char *superclass, char *name, int nattrs) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//   initialization with known class name, object name, initial number
//   of attributes to make space for.
//
//EOPI
  int rc;
  
  vmID = ESMCI::VM::getCurrentID(&rc);  // get vmID of current VM context
//  ESMCI::VMIdPrint(vmID);
  vmIDCreator = false;  // vmID points into global table
  
  // set ID to objectCount
  ID = ESMCI::VM::getBaseIDAndInc(vmID);
  
  // add object to list for automatic garbage collection
  ESMCI::VM::addObject(this, vmID);

  refCount = 1;
  strcpy(className, superclass ? superclass : "global");
  if (name && (name[0]!='\0')) 
      // TODO: make sure this name is unique in this namespace.  This means
      // some sort of registry utility.
      strcpy(baseName, name);
  else
      sprintf(baseName, "%s%3d", className, ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  // setup the root Attribute, passing the address of this
  root.setBase(this);
  
  baseStatus  = ESMF_STATUS_READY;
  status      = ESMF_STATUS_READY;
  proxyflag   = ESMF_PROXYNO;

 } // end ESMC_Base

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_Base()"
//BOPI
// !IROUTINE:  ~ESMC_Base - native C++ destructor for ESMC_Base class
//
// !INTERFACE:
      ESMC_Base::~ESMC_Base(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOPI
  int i, rc;
  
//fprintf(stderr, "final garbage collection in ~ESMC_Base() for %p\n", this);
  
  if (vmIDCreator){
    // Base object is responsible for vmID deallocation
    ESMCI::VMIdDestroy(vmID, &rc);
    delete vmID;
  }

  baseStatus  = ESMF_STATUS_INVALID;
  status      = ESMF_STATUS_INVALID;
  
  // setup the root Attribute, passing the address of this
  //delete root;
  
  // if we have to support reference counts someday,
  // test if (refCount > 0) and do something if true;

 } // end ~ESMC_Base

