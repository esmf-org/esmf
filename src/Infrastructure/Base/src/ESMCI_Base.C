// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Base.C"
//==============================================================================
//
// Base class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Base methods declared
// in the companion file ESMCI_Base.h
//
//-----------------------------------------------------------------------------

// single blank line to make protex happy.
//BOP

//EOP


// include associated header file
#include "ESMCI_Base.h"

// include higher level, 3rd party or system headers
#include <iostream>
#include <vector>

// include ESMF headers
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
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
  
  // first deal with old vmID if this is the creator  
  if (vmIDCreator){
    // responsible for vmID deallocation
    localrc = this->vmID->destroy();
    delete this->vmID;
  }
  // now create the new vmID
  this->vmID = new ESMCI::VMId;       // allocate space for this VMId
  localrc = this->vmID->create();     // allocate internal VMId memory
  ESMCI::VMIdCopy(this->vmID, vmID);  // copy content of vmID to this->vmID.
  vmIDCreator = true;  // Base object is responsible for vmID deallocation

} // end ESMC_BaseSetVMId

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetVM"
//BOPI
// !IROUTINE:  ESMC_BaseGetVM - Get Base class VM
//  
// !INTERFACE:
      ESMCI::VM *ESMC_Base::ESMC_BaseGetVM(
// 
// !ARGUMENTS:
      void) const {
//  
// !RETURN VALUE:
//    Unique VM of the context in which this base object was created
//  
// !DESCRIPTION:
//    Returns the object's VM.
//  
//EOPI

  return vm;

} // end ESMC_BaseGetVM

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
      const char *classname) { // in - context in which name should be unique
// 
// !DESCRIPTION:
//    Accessor method to set base class name.
//
//EOPI

  int rc;
 
    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  if (classname) {
     int len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       char msgbuf[ESMF_MAXSTR];
       sprintf(msgbuf, "Error: object type %d bytes longer than limit of %d",
                          len, ESMF_MAXSTR-1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, 
         &rc);
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
      const char *name,// in - contains name to set in fortran format
      int nlen) {      // in - length of the input name buffer
// 
// !DESCRIPTION:
//    Accessor method to set base class name.
//
//EOPI
  int rc;

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  if (nlen > ESMF_MAXSTR) {
       char msgbuf[ESMF_MAXSTR];
       sprintf(msgbuf, "string name %d bytes longer than limit of %d bytes",
                       nlen, ESMF_MAXSTR);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, 
            &rc);
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
       char msgbuf[ESMF_MAXSTR];
       sprintf(msgbuf, "object name %d bytes longer than limit of %d bytes",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, 
           &rc);
       return rc;
     }
     // look for slash in name.  Conflicts with syntax used in StateGet for items in
     // nested States.
     if (strchr (name, '/') != NULL) {
       char msgbuf[ESMF_MAXSTR];
       sprintf(msgbuf, "%s must not have a slash (/) in its name", name);
       ESMC_LogDefault.MsgFoundError (ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT,
           &rc);
       return rc;
     }
     defname = 0;
  } 

  if (classname && (classname[0]!='\0')) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       char msgbuf[ESMF_MAXSTR];
       sprintf(msgbuf, "object type %d bytes longer than limit of %d bytes",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, 
           &rc);
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
  
  
  //printf("%s\n", baseName);

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
      const char *name,// in - class name to set, in fortran format
      int nlen) {      // in - length of class name buffer
// 
// !DESCRIPTION:
//     Accessor method to set base class name.
//
//EOPI
  int rc;

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

  if (nlen > ESMF_MAXSTR) {
       std::string msgbuf;
       msgbuf = "Base name " + std::string(name, nlen) + " is longer than ESMF_MAXSTR";
       ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT, 
           &rc);
       return rc;
  }
  // look for slash in name.  Conflicts with syntax used in StateGet for items in
  // nested States.
  if (memchr (name, '/', nlen) != NULL) {
    std::string msgbuf;
    msgbuf = "Base name " + std::string (name, nlen) + " must not have a slash (/) in its name";
    ESMC_LogDefault.MsgFoundError (ESMC_RC_ARG_VALUE, msgbuf, ESMC_CONTEXT,
        &rc);
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
    ESMCI::VMId *vmIDp;
    char *cp;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    int r=*offset%8;
    if (r!=0) *offset += 8-r;  // alignment

    ip = (int *)(buffer + *offset);
    ID = *ip++;
    refCount = *ip++;  
    classID = *ip++;

    sp = (ESMC_Status *)ip;
    baseStatus = *sp++;
    status = *sp++;

    pfp = (ESMC_ProxyFlag *)sp;
    proxyflag = *pfp++;
    proxyflag = ESMF_PROXYYES;  // deserialize means this is a proxy object

    vmIDp = (ESMCI::VMId *)pfp;
    vmIDp++;

    cp = (char *)vmIDp;
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

    // Update the offset
    if (*offset%8 != 0)
      *offset += 8 - *offset%8;
    localrc = vmID_remote->create();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;
    localrc = vmID_remote->deserialize (buffer, offset, false);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;

    // setup the root Attribute, passing the address of this
    root = new ESMCI::Attribute(ESMF_TRUE);
    root->setBase(this);
    rootalias = false;

    // Deserialize the Attribute hierarchy
    if (attreconflag == ESMC_ATTRECONCILE_ON) {
      if (*offset%8 != 0)
        *offset += 8 - *offset%8;
      localrc = root->ESMC_Deserialize(buffer,offset);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, 
            ESMC_CONTEXT, &localrc)) return localrc;
    }
        
  return ESMF_SUCCESS;

 } // end ESMC_Deserialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - ID and vmID inquiry of a serialized object
//
// !INTERFACE:
      // static
      int ESMC_Base::ESMC_Deserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      const char *buffer,   // in - byte stream to read
      const int *offset,    // in - original offset
      int *ID,              // out - Object ID
      ESMCI::VMId *vmID,    // in/out - VMId
      std::string &objname) { // out - base name
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.  VMId is assumed to have the
//    vmkey array pre-allocated.
//
//EOPI

    int *ip, i, nbytes;
    int offset_local = *offset;
    ESMC_Status *sp;
    ESMC_ProxyFlag *pfp;
    ESMCI::VMId *vmIDp;
    char *cp;
    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    int r=offset_local%8;
    if (r!=0) offset_local += 8-r;  // alignment

    ip = (int *)(buffer + offset_local);
    *ID = *ip;
    ip+=3;

    sp = (ESMC_Status *)ip;
    sp+=2;

    pfp = (ESMC_ProxyFlag *)sp;
    pfp++;

    vmIDp = (ESMCI::VMId *)pfp;
    vmIDp++;

    cp = (char *)vmIDp;
    objname = cp;
    cp += 3*ESMF_MAXSTR;

    ip = (int*)cp;
    cp = (char *)ip;

    offset_local = (cp - buffer);

#if 1
    if (!vmID) {
      if (ESMC_LogDefault.MsgFoundError(ESMF_RC_PTR_NULL, "vmID must be initialized",
            ESMC_CONTEXT, &localrc)) return localrc;
    }
#endif

    // Turn on full deserialize for inquiries.
    if (offset_local%8 != 0)
      offset_local += 8 - offset_local%8;
// std::cout << ESMC_METHOD << ": calling vmID deserialize inquiry at offset: " << offset_local << std::endl;
    vmID->deserialize (buffer, &offset_local, false);

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
      int level,                    //  in - print level for recursive prints
      const char *options,          //  in - print options
      bool tofile,                  //  in - from file flag
      const char *filename,         //  in - filename
      bool append) const {          //  in - append
//
// !DESCRIPTION:
//    Print the contents of an {\tt ESMC\_Base} object.  Expected to be
//    called internally from the object-specific print routines.
//
//EOPI

  int localrc;
  int lpet = 0;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    // PLI -- 10/4/2007 -- use this function to print attribute lists in various ESMF classes
    // No need to print the base object ID, Name or Class because these information will be
    // printed at the derived class
 
    //  sprintf(msgbuf,
    //   "Base object ID: %d, Ref count: %d, Status=%s, Name=%s, Class=%s\n", 
    //       ID, refCount, ESMC_StatusString(baseStatus), baseName, className);
    // printf(msgbuf);
    // ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);
    
  // root Attribute
  if (level > 0) {
    std::cout << " ";
    for (int i=0; i<level; i++)
      std::cout << "->";
  }
  std::cout << " Base name    = " << baseName << std::endl;
  std::cout << " Status: Base = " << ESMC_StatusString(baseStatus) << ", "
      << " object = " << ESMC_StatusString(status) << std::endl;
  std::cout << " Proxy        = " << ((proxyflag == ESMF_PROXYYES)?"yes":"no") << std::endl;
  if (options) {
    if (strcmp (options, "debug") == 0) {
      std::cout << " Base ID = " << ID << ", vmID:" << std::endl;
      vmID->print ();
      if (proxyflag == ESMF_PROXYYES) {
        if (vmID_remote) {
          std::cout << " Remote vmID:" << std::endl;
          vmID_remote->print ();
        } else
          std::cout << " Proxy with NO remote vmID!  (Probable error)" << std::endl;
      }
    }
  }
  if ((root->getCountAttr() > 0 || root->getCountPack() > 0) && tofile) {
    std::cout << " Root Attributes:" << std::endl;
    // ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);

    // traverse the Attribute hierarchy, printing as we go
    if (append)
      root->ESMC_Print(tofile, filename, true);
    else
      root->ESMC_Print(tofile, filename, false);
  }
  fflush (NULL);

  return ESMF_SUCCESS;

 } // end ESMC_Print

//-----------------------------------------------------------------------------

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
      const char *options) const { //  in - print options
//
// !DESCRIPTION:
//    Print the contents of an {\tt ESMC\_Base} object.  Expected to be
//    called internally from the object-specific print routines.
//
//EOPI

  return ESMC_Print(0, options, false, "", false);

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
    int *ip, i, localrc;
    ESMC_Status *sp;
    ESMC_ProxyFlag *pfp;
    ESMCI::VMId *vmIDp;
    char *cp;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    int r=*offset%8;
    if (r!=0) *offset += 8-r;  // alignment

    fixedpart = sizeof(ESMC_Base);
    if (inquireflag == ESMF_INQUIREONLY) {
      *offset += fixedpart;
    } else {
      if ((*length - *offset) < fixedpart) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
                               "Buffer too short to add a Base object", 
            ESMC_CONTEXT, &localrc);
        return localrc; 
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

      vmIDp = (ESMCI::VMId *)pfp;
      *vmIDp++ = *vmID;

      cp = (char *)vmIDp;
      memcpy(cp, baseName, ESMF_MAXSTR);
      cp += ESMF_MAXSTR;
      memcpy(cp, baseNameF90, ESMF_MAXSTR);
      cp += ESMF_MAXSTR;
      memcpy(cp, className, ESMF_MAXSTR);
      cp += ESMF_MAXSTR;

      ip = (int *)cp;
      cp = (char *)ip;

      // update the offset before calling vmID and Attribute serialize
      *offset = (cp - buffer);
    }

    // serialize vmID for inquiries when deserializing proxy objects
    if (*offset%8 != 0)
      *offset += 8 - *offset%8;
// std::cout << ESMC_METHOD << ": serializing vmID at offset: " << *offset << std::endl;
    localrc = vmID->serialize (buffer, length, offset, inquireflag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &localrc)) return localrc;

    // Serialize the Attribute hierarchy
    if (attreconflag == ESMC_ATTRECONCILE_ON) {
      if (*offset%8 != 0)
        *offset += 8 - *offset%8;
      localrc = root->ESMC_Serialize(buffer,length,offset, inquireflag);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, 
            ESMC_CONTEXT, &localrc)) return localrc;
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
      ESMC_Base::ESMC_Base(ESMCI::VM *vmArg) {
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
  
  if (vmArg==NULL){
    // no VM passed in -> get vmID of the current VM context
    vmID = ESMCI::VM::getCurrentID(&rc);
    vm = ESMCI::VM::getCurrent(&rc);
  }else{
    // VM was passed in -> get vmID of the specified VM context
    vmID = vmArg->getVMId(&rc);
    vm = vmArg;
  }
    
  //ESMCI::VMIdPrint(vmID);
  vmIDCreator = false;  // vmID points into global table
  
  // set ID
  ID = ESMCI::VM::getBaseIDAndInc(vmID);
  classID = 0;

  ID_remote = 0;
  vmID_remote = NULL;

  refCount = 1;
  strcpy(className, "global");
  sprintf(baseName, "%s%03d", "unnamed", ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);
  
#if 0
  char msgbuf[ESMF_MAXSTR];
  sprintf(msgbuf, "ESMC_Base constructor: %p, %s, %s", this, 
    this->ESMC_BaseGetClassName(), this->ESMC_BaseGetName());
  ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);
#endif
  
  // add object to list for automatic garbage collection
  ESMCI::VM::addObject(this, vmID);

  // setup the root Attribute, passing the address of this
  root = new ESMCI::Attribute(ESMF_TRUE);
  root->setBase(this);
  rootalias = false;

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
//   Initialization with a specified id. If id==-1 then this is a proxy member.
//
//EOPI
  int rc;
  
  vmID = ESMCI::VM::getCurrentID(&rc);  // get vmID of current VM context
  if (id==-1){
    // proxy members hold NULL for the vm and space for a remote VMId
    vm = NULL;
    vmID_remote = new ESMCI::VMId;
  }else {
    vm = ESMCI::VM::getCurrent(&rc);
    vmID_remote = NULL;
  }
//  ESMCI::VMIdPrint(vmID);
  vmIDCreator = false;  // vmID points into global table
  
  // set ID to objectCount;
  ID = id;
  classID = 0;

  ID_remote = 0;

  refCount = 1;
  strcpy(className, "global");
  sprintf(baseName, "%s%03d", "unnamed", ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);
  
#if 0
  char msgbuf[ESMF_MAXSTR];
  sprintf(msgbuf, "ESMC_Base constructor: %p, %s, %s", this, 
    this->ESMC_BaseGetClassName(), this->ESMC_BaseGetName());
  ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);
#endif
  
  // add object to list for automatic garbage collection
  ESMCI::VM::addObject(this, vmID);

  // setup the root Attribute, passing the address of this
  if (id==-1){
    rootalias = true; // protect root Attribute from being used in delete
  }else{
    root = new ESMCI::Attribute(ESMF_TRUE);
    root->setBase(this);
    rootalias = false;
  }

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
      ESMC_Base::ESMC_Base(const char *superclass, const char *name, int nattrs,
        ESMCI::VM *vmArg){
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
  
  if (vmArg==NULL){
    // no VM passed in -> get vmID of the current VM context
    vmID = ESMCI::VM::getCurrentID(&rc);
    vm = ESMCI::VM::getCurrent(&rc);
  }else{
    // VM was passed in -> get vmID of the specified VM context
    vmID = vmArg->getVMId(&rc);
    vm = vmArg;
  }

  //ESMCI::VMIdPrint(vmID);
  vmIDCreator = false;  // vmID points into global table
  
  // set ID to objectCount
  ID = ESMCI::VM::getBaseIDAndInc(vmID);
  classID = 0;

  ID_remote = 0;
  vmID_remote = NULL;

  refCount = 1;
  strcpy(className, superclass ? superclass : "global");
  if (name && (name[0]!='\0')) 
      // TODO: make sure this name is unique in this namespace.  This means
      // some sort of registry utility.
      strcpy(baseName, name);
  else
      sprintf(baseName, "%s%03d", className, ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

#if 0
  char msgbuf[ESMF_MAXSTR];
  sprintf(msgbuf, "ESMC_Base constructor: %p, %s, %s", this, 
    this->ESMC_BaseGetClassName(), this->ESMC_BaseGetName());
  ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);
#endif
  
  // add object to list for automatic garbage collection
  ESMCI::VM::addObject(this, vmID);

  // setup the root Attribute, passing the address of this
  root = new ESMCI::Attribute(ESMF_TRUE);
  root->setBase(this);
  rootalias = false;

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

#if 0
  char msgbuf[ESMF_MAXSTR];
  sprintf(msgbuf, "In ~ESMC_Base() for %p", this);
  ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);
  sprintf(msgbuf, " -> Base name: %s, classname: %s", this->ESMC_BaseGetName(),
    this->ESMC_BaseGetClassName());
  ESMC_LogDefault.Write(msgbuf, ESMC_LOGMSG_INFO);
#endif
  
  if (vmIDCreator){
    // Base object is responsible for vmID deallocation
    rc = vmID->destroy();
    delete vmID;
  }

  if (vmID_remote) {
    // Base object is responsible for vmID_remote deallocation
    rc = vmID_remote->destroy();
    delete vmID_remote;
  }

  baseStatus  = ESMF_STATUS_INVALID;
  status      = ESMF_STATUS_INVALID;

#if 0
  std::stringstream debugmsg;
  debugmsg << "From ~ESMC_Base(): rootalias=" << rootalias;
  ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
#endif

  // delete the root Attribute
  if (!rootalias)
    delete root;

  // if we have to support reference counts someday,
  // test if (refCount > 0) and do something if true;

 } // end ~ESMC_Base

