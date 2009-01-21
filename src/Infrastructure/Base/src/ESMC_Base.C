// $Id: ESMC_Base.C,v 1.83.2.3 2009/01/21 21:25:19 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"
#include "ESMC_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base.C,v 1.83.2.3 2009/01/21 21:25:19 cdeluca Exp $";
//-----------------------------------------------------------------------------

// initialize class-wide instance counter
static int globalCount = 0;   //TODO: this should be a counter per VM context


//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the ESMC_Base routines
//
//

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

} // end ESMC_BaseSetVMId

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseGetInstCount"
//BOPI
// !IROUTINE:  ESMC_BaseGetInstCount - Get number of Base class instances
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseGetInstCount(
// 
// !ARGUMENTS:
      void) const {
//
// !RETURN VALUE:
//    Integer instance count.
//
// !DESCRIPTION:
//    Return a count of how many instances of the {\tt ESMC_Base} class
//    have been instantiated.
//
//EOPI

  return globalCount;

} // end ESMC_BaseGetInstCount

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
#define ESMC_METHOD "ESMC_BaseGetStatus"
//BOPI
// !IROUTINE:  ESMC_BaseGetStatus - Get Base class status
//
// !INTERFACE:
      ESMC_Status ESMC_Base::ESMC_BaseGetStatus(
// 
// !ARGUMENTS:
      void) const {
// 
// !RETURN VALUE:
//    {\tt ESMC\_Status} object containing the {\tt ESMC\_Base} status.
// 
// !DESCRIPTION:
//    Accessor method for base class status.
//
//EOPI

  return baseStatus;

}  // end ESMC_BaseGetStatus

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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
      char *name,           // in - base name to set
      char *classname) {    // in - context in which name should be unique
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
       return rc;
     }
     defname = 0;
  } 

  if (classname && (classname[0]!='\0')) {
     len = strlen(classname);
     if (len >= ESMF_MAXSTR) {
       sprintf(msgbuf, "object type %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR-1);
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseSetStatus"
//BOPI
// !IROUTINE:  ESMC_BaseSetStatus - Set Base class status
//
// !INTERFACE:
      void ESMC_Base::ESMC_BaseSetStatus(
// 
// !RETURN VALUE:
//    none
// 
// !ARGUMENTS:
      ESMC_Status status) {   // in - base status to set
// 
// !DESCRIPTION:
//     Accessor method for base class status.
//
//EOPI

  baseStatus = status;

}  // end ESMC_BaseSetStatus
 

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
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
    
    int *ip, i, nbytes;
    ESMC_Status *sp;
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
    cp = (char *)sp;
    memcpy(baseName, cp, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    memcpy(baseNameF90, cp, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    memcpy(className, cp, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    ip = (int *)cp;
    attrCount = *ip++;
    attrAlloc = *ip++;
    cp = (char *)ip;

    // update offset to point to past the current obj
    *offset = (cp - buffer);

    if (attrAlloc > 0) {
         nbytes = attrAlloc * sizeof(ESMC_Attribute *);
         attrList = (ESMC_Attribute **)malloc(nbytes);
         for (i=0; i<attrCount; i++) {
            attrList[i] = new ESMC_Attribute;
            attrList[i]->ESMC_Attribute::ESMC_Deserialize(buffer, offset);
            //attrList[i] = NULL;
         }
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

  int i;
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
    // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  

  sprintf(msgbuf, "   Number of Attributes: %d\n", attrCount);
  printf(msgbuf);
    // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  for (i=0; i<attrCount; i++) {
      sprintf(msgbuf, " Attr %d: ", i);
      printf(msgbuf);
        // ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      attrList[i]->ESMC_Print();
  }
                         
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
      int *offset) const {   // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in base class into a stream of bytes.
//
//EOPI
    int fixedpart;
    int *ip, i, rc;
    ESMC_Status *sp;
    char *cp;

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    fixedpart = sizeof(ESMC_Base);
    if ((*length - *offset) < fixedpart) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
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
    cp = (char *)sp;
    memcpy(cp, baseName, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    memcpy(cp, baseNameF90, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    memcpy(cp, className, ESMF_MAXSTR);
    cp += ESMF_MAXSTR;
    ip = (int *)cp;
    *ip++ = attrCount;
    *ip++ = attrAlloc;
    cp = (char *)ip;

    // update the offset before calling AttributeSerialize
    *offset = (cp - buffer);

    if (attrCount > 0) {
         for (i=0; i<attrCount; i++)
             attrList[i]->ESMC_Attribute::ESMC_Serialize(buffer, length, offset);
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
//-----------------------------------------------------------------------------
// Attribute methods
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_AttributeAlloc"
//BOPI
// !IROUTINE:  ESMC_AttributeAlloc - ensure the attribute list is long enough
//
// !INTERFACE:
      int ESMC_Base::ESMC_AttributeAlloc(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int adding) {             // in - number of attributes being added
// 
// !DESCRIPTION:
//     Ensure there is enough space to add nattr more attributes.
//
//EOPI

#define ATTR_CHUNK  4           // allocate and realloc in units of this

  void *saveme;   // in case of error
  int rc;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  if ((attrCount + adding) <= attrAlloc) 
      return ESMF_SUCCESS;

  // FIXME: this should be arrays of *attrs, not whole size, right?
  saveme = (void *)attrList;
  attrList = (ESMC_Attribute **)realloc((void *)attrList, 
                           (attrAlloc + ATTR_CHUNK) * sizeof(ESMC_Attribute *));
  if (attrList == NULL) {
      free(saveme);   // although at this point, the heap is probably boffed
      return ESMF_FAILURE;
  }
  attrAlloc += ATTR_CHUNK;

  return ESMF_SUCCESS;

}  // end ESMC_AttributeAlloc


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOPI
// !IROUTINE:  ESMC_BaseAttSet - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Attribute *attr) {   // in - attribute name, type, value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//     This version of set is used when the caller has already allocated
//     an attribute object and filled it, and the attribute is simply
//     added to the list belonging to this object.  The caller must not
//     delete the attribute.  Generally used internally - see below for
//     individual attribute set routines for each supported type.
//
//EOPI

  int i, rc;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!attr) || (!attr->attrName) || (attr->attrName[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute object", &rc);
       return rc;
  }

  // first, see if you are replacing an existing attribute
  for (i=0; i<attrCount; i++) {
      if (strcmp(attr->attrName, attrList[i]->attrName))
          continue;

      // FIXME: we might want an explicit flag saying that this is what
      // is wanted, instead of an error if a previous value not expected.

      // if you get here, you found a match.  replace previous copy.

      // delete old attribute, including possibly freeing a list
      attrList[i]->~ESMC_Attribute();

      attrList[i] = attr;
      return ESMF_SUCCESS;
  }   

  // new attribute name, make sure there is space for it.
  rc = ESMC_AttributeAlloc(1);
  if (rc != ESMF_SUCCESS)
      return rc;
  
  attrList[attrCount] = attr;   
  attrCount++;
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttSet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOPI
// !IROUTINE:  ESMC_BaseAttSet(int) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      ESMC_I4 value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOPI

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(int)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOPI
// !IROUTINE:  ESMC_BaseAttSet(int *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      int count,               // in - number of ints in list
      ESMC_I4 *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOPI

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(int *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOPI
// !IROUTINE:  ESMC_BaseAttSet(ESMC_I8) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      ESMC_I8 value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOPI

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(ESMC_I8)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOPI
// !IROUTINE:  ESMC_BaseAttSet(ESMC_I8 *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      int count,               // in - number of ints in list
      ESMC_I8 *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOPI

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_I8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(ESMC_I8 *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(ESMC_R4) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      ESMC_R4 value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R4, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(ESMC_R4)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(ESMC_R4 *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      int count,               // in - number of ESMC_R4s in list
      ESMC_R4 *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R4, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(ESMC_R4 *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(ESMC_R8) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      ESMC_R8 value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R8, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(ESMC_R8)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(ESMC_R8 *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      int count,               // in - number of ESMC_R8s in list
      ESMC_R8 *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_R8, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(ESMC_R8 *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(bool) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      ESMC_Logical value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_LOGICAL, 1, &value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(bool)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(bool *) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,              // in - attribute name
      int count,               // in - number of logicals in list
      ESMC_Logical *value) {   // in - attribute values
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_LOGICAL, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(bool *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet(char) - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,       // in - attribute name
      char *value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, ESMC_TYPEKIND_CHARACTER, 1, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet(char)


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSet"
//BOP
// !IROUTINE:  ESMC_BaseAttSet - set attribute on an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSet(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,       // in - attribute name
      ESMC_TypeKind tk, // in - typekind
      int count,        // in - number of values
      void *value) {    // in - attribute value
// 
// !DESCRIPTION:
//     Associate a (name,value) pair with any type in the system.
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = new ESMC_Attribute(name, tk, count, value);  
  if (!attr)
    return ESMF_FAILURE;
 
  rc = ESMC_BaseAttSet(attr);

  return rc;

}  // end ESMC_BaseAttSet


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet - get attribute from an ESMF type
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    pointer to requested attribute
// 
// !ARGUMENTS:
      char *name) const {        // in - attr name to retrieve
// 
// !DESCRIPTION:
//
//EOP

  int i;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                               "bad attribute name", NULL);
       return NULL;
  }

  for (i=0; i<attrCount; i++) {
      if (strcmp(name, attrList[i]->attrName))
          continue;

      // if you get here, you found a match. 
      return attrList[i]; 
  }   

  // bad news - you get here if no matches found
  return NULL;

}  // end ESMC_BaseAttGet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(int) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_I4 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_I4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind I4", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vi;
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(int)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(int *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_I4 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_I4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind I4", &rc);
       return rc;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vi;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vip[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(int *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(ESMC_I8) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_I8 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_I8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind I8", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vtl;
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(ESMC_I8)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(ESMC_I8 *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_I8 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_I8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind I8", &rc);
       return rc;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vtl;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vlp[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(ESMC_I8 *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(ESMC_R4) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_R4 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_R4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind R4", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vf;
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(ESMC_R4)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(ESMC_R4 *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_R4 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_R4) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind R4", &rc);
       return rc;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vf;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vfp[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(ESMC_R4 *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(ESMC_R8) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_R8 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_R8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind R8", &rc);
       return rc;
  }
  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vd;
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(ESMC_R8)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(ESMC_R8 *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_R8 *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_R8) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind R8", &rc);
       return rc;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vd;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vdp[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(ESMC_R8 *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(bool) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_Logical *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind LOGICAL", &rc);
       return rc;
  }

  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  *value = attr->vb;
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(bool)


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(bool *) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      int *count,                    // out - number of values in list
      ESMC_Logical *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_LOGICAL) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind LOGICAL", &rc);
       return rc;
  }

  if (count) 
      *count = attr->items;

  if (value) {
      if (attr->items == 1)
          value[0] = attr->vb;
      else for (i=0; i<attr->items; i++)
          value[i] = attr->vbp[i];
  }

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(bool *)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(char) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,            // in - name of attribute to retrieve
      char *value) const {   // out - attribute value
// 
// !DESCRIPTION:
//
//EOP

  int rc;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad attribute name", &rc);
       return rc;
  }
  if (!value) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "bad value return argument", &rc);
      return ESMF_FAILURE;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not found", &rc);
       return rc;
  }

  if (attr->tk != ESMC_TYPEKIND_CHARACTER) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not typekind CHARACTER", &rc);
       return rc;
  }

  if (attr->items != 1) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                  "attribute not single value", &rc);
       return rc;
  }

  strcpy(value, attr->vcp);
  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(char)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(name) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                    // in - name of attribute to retrieve
      ESMC_TypeKind *tk,             // out - typekind
      int *count,                    // out - number of values in list
      void *value) const {           // out - attribute value(s)
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;


  // simple sanity checks
  if ((!name) || (name[0] == '\0')) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "bad attribute name", &rc);
       return rc;
  }

  attr = ESMC_BaseAttGet(name);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "attribute not found", &rc);
       return rc;
  }

  if (tk) 
      *tk = attr->tk;

   if (count) {
     if (attr->tk == ESMC_TYPEKIND_CHARACTER)
         *count = attr->slen;
     else
         *count = attr->items;
   }


  if (value) {
      if (attr->items == 1) {
              if (attr->tk == ESMC_TYPEKIND_I4)
                  *(ESMC_I4 *)value = attr->vi; 
              else if (attr->tk == ESMC_TYPEKIND_I8)
                  *(ESMC_I8 *)value = attr->vtl; 
              else if (attr->tk == ESMC_TYPEKIND_R4)
                  *(ESMC_R4 *)value = attr->vf; 
              else if (attr->tk == ESMC_TYPEKIND_R8)
                  *(ESMC_R8 *)value = attr->vd; 
              else if (attr->tk == ESMC_TYPEKIND_LOGICAL)
                  *(ESMC_Logical *)value = attr->vb;
              else{
                   ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
                                       "unknown typekind",
                                       &rc);
                   return rc;
               }

 
      } else {
              if (attr->tk == ESMC_TYPEKIND_I4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I4 *)value)[i] = attr->vip[i];
              } else if (attr->tk == ESMC_TYPEKIND_I8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I8 *)value)[i] = attr->vlp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R4 *)value)[i] = attr->vfp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R8 *)value)[i] = attr->vdp[i];
              } else if (attr->tk == ESMC_TYPEKIND_LOGICAL) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_Logical *)value)[i] = attr->vbp[i];
              } else{
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown typekind", 
                                       &rc);
              return rc;
              }
       }
  }          //value

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(name)

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet(num) - get attribute from an ESMF type
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int num,                       // in - number of attribute to retrieve
      char *name,                    // out - attribute name
      ESMC_TypeKind *tk,             // out - typekind
      int *count,                    // out - number of values in list
      void *value) const {           // out - attribute value(s)
// 
// !DESCRIPTION:
//
//EOP

  int rc, i;
  ESMC_Attribute *attr;

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  attr = ESMC_BaseAttGet(num);
  if (!attr) {
       ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "attribute not found", &rc);
       return rc;
  }

  if (name)
       strcpy(name, attr->attrName);

  if (tk) 
      *tk = attr->tk;

if (count) {
     if (attr->tk == ESMC_TYPEKIND_CHARACTER)
         *count = attr->slen;
     else
         *count = attr->items;
   }


  if (value) {
      if (attr->items == 1) {
              if (attr->tk == ESMC_TYPEKIND_I4)
                  *(ESMC_I4 *)value = attr->vi; 
              else if (attr->tk == ESMC_TYPEKIND_I8)
                  *(ESMC_I8 *)value = attr->vtl; 
              else if (attr->tk == ESMC_TYPEKIND_R4)
                  *(ESMC_R4 *)value = attr->vf; 
              else if (attr->tk == ESMC_TYPEKIND_R8)
                  *(ESMC_R8 *)value = attr->vd; 
              else if (attr->tk == ESMC_TYPEKIND_LOGICAL)
                  *(ESMC_Logical *)value = attr->vb;
              else{
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown typekind", 
                                       &rc);
                  return rc;
              }
 
      } else {
              if (attr->tk == ESMC_TYPEKIND_I4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I4 *)value)[i] = attr->vip[i];
              } else if (attr->tk == ESMC_TYPEKIND_I8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_I8 *)value)[i] = attr->vlp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R4) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R4 *)value)[i] = attr->vfp[i];
              } else if (attr->tk == ESMC_TYPEKIND_R8) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_R8 *)value)[i] = attr->vdp[i];
              } else if (attr->tk == ESMC_TYPEKIND_LOGICAL) {
                  for (i=0; i<attr->items; i++)
                      ((ESMC_Logical *)value)[i] = attr->vbp[i];
              } else {
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                                       "unknown typekind", 
                                       &rc);
                  return rc;
              }
      }
  }

  return ESMF_SUCCESS;

}  // end ESMC_BaseAttGet(num)


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGetCount"
//BOP
// !IROUTINE:  ESMC_BaseAttGetCount - get an ESMF object's number of attributes
// 
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGetCount(
// 
// !RETURN VALUE:
//    int attribute count
// 
// !ARGUMENTS:
      void) const {  
// 
// !DESCRIPTION:
//      Returns number of attributes present
//
//EOP

  
  return attrCount;

} // end ESMC_BaseAttGetCount

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGet"
//BOP
// !IROUTINE:  ESMC_BaseAttGet - get an ESMF object's attribute by number
//
// !INTERFACE:
      ESMC_Attribute *ESMC_Base::ESMC_BaseAttGet(
// 
// !RETURN VALUE:
//    int return code.
// 
// !ARGUMENTS:
      int number) const {             // in - attribute number
// 
// !DESCRIPTION:
//     Allows the caller to get attributes by number instead of by name.
//     This can be useful in iterating through all attributes in a loop.

//
//EOP

  char msgbuf[ESMF_MAXSTR];

  // simple sanity check
  if ((number < 0) || (number >= attrCount)) {
      sprintf(msgbuf, "attribute number must be  0 < N <= %d\n", attrCount-1);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, NULL);
      return NULL;
  }

  return attrList[number];

}  // end ESMC_BaseAttGet

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGetNameList"
//BOP
// !IROUTINE:  ESMC_BaseAttGetNameList - get the list of attribute names
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGetNameList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      int *count,               // out - number of attributes
      char **namelist) const {  // out - namelist
// 
// !DESCRIPTION:
//     Return a list of all attribute names without returning the values.
//
//EOP

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_BaseAttGetNameList

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSetList"
//BOP
// !IROUTINE:  ESMC_BaseAttSetList - set multiple attributes at once
// 
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttSetList(
// 
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
      int count,                   // in - number of attributes to set
      ESMC_Attribute *valuelist) { // in - list of attribute values
// 
// !DESCRIPTION:
//    Set multiple attributes on an object in one call. 
//
//EOP

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_BaseAttSetList

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGetList"
//BOP
// !IROUTINE:  ESMC_BaseAttGetList - get multiple attributes at once
// 
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttGetList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char **namelist,                   // in - null term list of names
      ESMC_Attribute *valuelist) const { // out - list of attribute values
// 
// !DESCRIPTION:
//     Get multiple attributes from an object in a single call
//
//EOP

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_BaseAttGetList

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttCopy"
//BOP
// !IROUTINE:  ESMC_BaseAttCopy - copy an attribute between two objects
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttCopy(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      char *name,                 // in - attribute to copy
      ESMC_Base *destination) {   // in - the destination object
// 
// !DESCRIPTION:
//     The specified attribute associated with the source object (this) is
//     copied to the destination object. 

//EOP

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_BaseAttCopy

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttCopyAll"
//BOP
// !IROUTINE:  ESMC_BaseAttCopyAll - copy attributes between two objects 
//
// !INTERFACE:
      int ESMC_Base::ESMC_BaseAttCopyAll(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *source) {  // in - the source object
// 
// !DESCRIPTION:
//     All attributes associated with the source object are copied to the
//     destination object (this).  Some attributes might have to be considered
//     {\tt read only} and won't be updated by this call. 

//EOP

  return ESMC_RC_NOT_IMPL;

}  // end ESMC_BaseAttCopyAll

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Serialize"
//BOPI
// !IROUTINE:  ESMC_Serialize - Turn the object information into a byte stream
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Serialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset) const {   // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in attribute class into a stream of bytes.
//
//EOPI
    int nbytes, rc;
    char *cp;

    // Initialize local return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;

    nbytes = sizeof(ESMC_Attribute);
    if (items > 1) 
        nbytes += items * ESMC_TypeKindSize(tk);
    
    if ((*length - *offset) < nbytes) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                               "Buffer too short to add an Attr object", &rc);
        return rc; 
        //buffer = (char *)realloc((void *)buffer, *length + nbytes);
        //*length += nbytes;
    }

    cp = (buffer + *offset);
    memcpy(cp, this, sizeof(ESMC_Attribute));
    cp += sizeof(ESMC_Attribute);

    if (items > 1) {
        nbytes = items * ESMC_TypeKindSize(tk);
        memcpy(cp, voidp, nbytes);
        cp += nbytes;
    }
    
    *offset = (cp - buffer);
   
    
  return ESMF_SUCCESS;

 } // end ESMC_Serialize

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_Deserialize - Turn a byte stream into an object
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Deserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // in - byte stream to read
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
    int nbytes;
    char *cp;

   int localrc;
   // Initialize local return code; assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

    cp = (buffer + *offset);
    memcpy(this, cp, sizeof(ESMC_Attribute));
    cp += sizeof(ESMC_Attribute);

    if (items > 1) {
        nbytes = items * ESMC_TypeKindSize(tk);
        memcpy(voidp, cp, nbytes);
        cp += nbytes;
    }
   
    *offset = (cp - buffer);
   
   localrc = ESMF_SUCCESS;
   return localrc;

 } // end ESMC_Deserialize
//-----------------------------------------------------------------------------

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Print"
//BOP
// !IROUTINE:  ESMC_Attribute::ESMC_Print - print the Attribute contents
//
// !INTERFACE:
      int ESMC_Attribute::ESMC_Print(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      void) const {                    // could add options at some point
// 
// !DESCRIPTION:
//     Print the contents of a Attribute object
//
//EOP
  int rc;
  char msgbuf[ESMF_MAXSTR];

  // Initialize local return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  if (tk != ESMF_NOKIND) 
      sprintf(msgbuf, "name '%s',  typekind %s", 
              attrName,  ESMC_TypeKindString(tk));
  else
      sprintf(msgbuf, "name '%s'",  attrName);
 
  printf(msgbuf);
  //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);

  if (items <= 0) {
      printf("\n");
      //ESMC_LogDefault.ESMC_LogWrite("\n", ESMC_LOG_INFO);
  }

  if (items == 1) {
      printf(", value: ");
      //ESMC_LogDefault.ESMC_LogWrite(", value: ", ESMC_LOG_INFO);
             if (tk == ESMC_TYPEKIND_I4)
                 sprintf(msgbuf, "%d\n", vi); 
             else if (tk == ESMC_TYPEKIND_I8)
                 sprintf(msgbuf, "%ld\n", vtl); 
             else if (tk == ESMC_TYPEKIND_R4)
                 sprintf(msgbuf, "%f\n", vf); 
             else if (tk == ESMC_TYPEKIND_R8)
                 sprintf(msgbuf, "%g\n", vd); 
             else if (tk == ESMC_TYPEKIND_LOGICAL)
                 sprintf(msgbuf, "%s\n", ESMC_LogicalString(vb)); 
             else if (tk == ESMC_TYPEKIND_CHARACTER)
                 sprintf(msgbuf, "%s\n", vcp);
             else{ 
                 ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "unknown value", &rc);
                 return rc;
             }
      printf(msgbuf);
      //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  if (items > 1) { 
      sprintf(msgbuf, ", %d items, values:\n", items);
      printf(msgbuf);
      //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
      for (int i=0; i<items; i++) {
                if (tk == ESMC_TYPEKIND_I4) {
                    sprintf(msgbuf, "\t item %d: %d\n", i, vip[i]); 
                } else if (tk == ESMC_TYPEKIND_I8) {
                    sprintf(msgbuf, "\t item %d: %ld\n", i, vlp[i]); 
                } else if (tk == ESMC_TYPEKIND_R4) {
                    sprintf(msgbuf, "\t item %d: %f\n", i, vfp[i]); 
                } else if (tk == ESMC_TYPEKIND_R8) {
                    sprintf(msgbuf, "\t item %d: %g\n", i, vdp[i]); 
                } else if (tk == ESMC_TYPEKIND_LOGICAL) {
                    sprintf(msgbuf, "\t item %d: %s\n", i,
                      ESMC_LogicalString(vbp[i]));
                } else{
                    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, 
                             "\t unknown value", &rc);
                    return rc;
                }
		printf(msgbuf);
      }
      //ESMC_LogDefault.ESMC_LogWrite(msgbuf, ESMC_LOG_INFO);
  }

  return ESMF_SUCCESS;

}  // end ESMC_Print


//-----------------------------------------------------------------------------
// ESMC_Base class utility functions, not methods, since they operate on
//   multiple objects at once
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttSetObjectList"
//BOP
// !IROUTINE:  ESMC_BaseAttSetObjectList - set an attribute on multiple ESMF objects
//
// !INTERFACE:
      int ESMC_BaseAttSetObjectList(
//
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,    // in - list of ESMC objects
      ESMC_Attribute *value) {   // in - attribute value
// 
// !DESCRIPTION:
//     Set the same attribute on multiple objects in one call
//
//EOP

    int localrc;

    // Initialize local return code; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end ESMC_BaseAttSetObjectList

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttGetObjectList"
//BOP
// !IROUTINE:  ESMC_BaseAttGetObjectList - get an attribute from multiple ESMF objects 
//
// !INTERFACE:
      int ESMC_BaseAttGetObjectList(
// 
// !RETURN VALUE:
//    int return code
// 
// !ARGUMENTS:
      ESMC_Base *anytypelist,            // in - list of ESMC objects
      char *name,                        // in - attribute name
      ESMC_Attribute *valuelist) {       // out - list of attribute values
// 
// !DESCRIPTION:
//     Get the same attribute name from multiple objects in one call
//
//EOP
    int localrc;

    // Initialize local return code ; assume routine not implemented
    localrc = ESMC_RC_NOT_IMPL;

    return localrc;

}  // end ESMC_BaseAttGetObjectList

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_BaseAttCopy(=)"
//BOP
// !IROUTINE:  ESMC_BaseAttCopy(=) - assignment operator for attributes
//
// !INTERFACE:
      ESMC_Attribute& ESMC_Attribute::operator=(
//
// !RETURN VALUE:
//    new attribute object with allocations done if lists or char strings
//    must be stored.
//
// !ARGUMENTS:
        const ESMC_Attribute &source) {   // in - ESMC_Attribute
//
// !DESCRIPTION:
//   copy an attribute, including contents, to current object (this)
//
//EOP
  int i;

  memcpy(attrName, source.attrName, ESMF_MAXSTR);

  tk = source.tk;
  items = source.items;
  slen = source.slen;
  
  if (items == 0)
    voidp = NULL;
 
  else if (items == 1) {
        if (tk == ESMC_TYPEKIND_I4)
            vi = source.vi;  
        else if (tk == ESMC_TYPEKIND_I8)
            vtl = source.vtl;  
        else if (tk == ESMC_TYPEKIND_R4)
            vf = source.vf;  
        else if (tk == ESMC_TYPEKIND_R8)
            vd = source.vd;  
        else if (tk == ESMC_TYPEKIND_LOGICAL)
            vb = source.vb;
        else if (tk == ESMC_TYPEKIND_CHARACTER){
            vcp = new char[slen];   // includes trailing null
            memcpy(vcp, (char *)source.vcp, slen);
        }else
            voidp = NULL;
  } else {
    // items > 1, alloc space for a list and do the copy
          if (tk == ESMC_TYPEKIND_I4) {
              vip = new ESMC_I4[items];      
              for (i=0; i<items; i++)
                  vip[i] = source.vip[i];  
          } else if (tk == ESMC_TYPEKIND_I8) {
              vlp = new ESMC_I8[items];      
              for (i=0; i<items; i++)
                  vlp[i] = source.vlp[i];  
          } else if (tk == ESMC_TYPEKIND_R4) {
              vfp = new ESMC_R4[items];      
              for (i=0; i<items; i++)
                  vfp[i] = source.vfp[i];  
          } else if (tk == ESMC_TYPEKIND_R8) {
              vdp = new ESMC_R8[items];      
              for (i=0; i<items; i++)
                  vdp[i] = source.vdp[i];  
          } else if (tk == ESMC_TYPEKIND_LOGICAL){
              vbp = new ESMC_Logical[items];      
              for (i=0; i<items; i++)
                vbp[i] = source.vbp[i];  
          }else
          // error - arrays of char strings not allowed
             voidp = NULL;
  }

  return (*this);

 } // end ESMC_Attribute::operator=

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Attribute()"
//BOP
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     create an empty attribute structure
//
//EOP

  attrName[0] = '\0';
  items = 0;
  slen = 0;
  voidp = NULL;

 } // end ESMC_Attribute

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Attribute()"
//BOP
// !IROUTINE:  ESMC_Attribute - native C++ constructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::ESMC_Attribute(
//
// !RETURN VALUE:
//    new attribute object
//
// !ARGUMENTS:
        char *name,                // attribute name
        ESMC_TypeKind typekind,    // typekind
        int numitems,              // single or list
        void *datap) {             // generic pointer to values
//
// !DESCRIPTION:
//   initialize an attribute, and make a copy of the data if items > 1
//
//EOP
  int i, len, rc;
  char msgbuf[ESMF_MAXSTR];

  if (!name)
      attrName[0] = '\0';
  else {
      len = strlen(name)+1;   // strlen doesn't count trailing null
      if (len > ESMF_MAXSTR) {
        sprintf(msgbuf, "attr name %d bytes longer than limit of %d bytes\n",
                       len, ESMF_MAXSTR);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, msgbuf, &rc);
      }
      memcpy(attrName, name, len);
  }

  tk = typekind;
  items = numitems;
  slen = 0;          // only used for string values
  
  if (items == 0)
      voidp = NULL;
 
  else if (items == 1) {
      if (!datap) 
          voidp = NULL;
      else  {
            if (tk == ESMC_TYPEKIND_I4)
                vi = *(ESMC_I4 *)datap;  
            else if (tk == ESMC_TYPEKIND_I8)
                vtl = *(ESMC_I8 *)datap;  
            else if (tk == ESMC_TYPEKIND_R4)
                vf = *(ESMC_R4 *)datap;  
            else if (tk == ESMC_TYPEKIND_R8)
                vd = *(ESMC_R8 *)datap;  
            else if (tk == ESMC_TYPEKIND_LOGICAL)
                vb = *(ESMC_Logical *)datap;  
            else if (tk == ESMC_TYPEKIND_CHARACTER){
                slen = strlen((char *)datap) + 1;
                vcp = new char[slen];
                strncpy(vcp, (char *)datap, slen);
           }else
                voidp = NULL;
    }

  } else {
    // items > 1, alloc space for a list and do the copy
        if (tk == ESMC_TYPEKIND_I4) {
            vip = new ESMC_I4[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vip[i] = ((ESMC_I4 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_I8) {
            vlp = new ESMC_I8[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vlp[i] = ((ESMC_I8 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_R4) {
            vfp = new ESMC_R4[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vfp[i] = ((ESMC_R4 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_R8) {
            vdp = new ESMC_R8[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vdp[i] = ((ESMC_R8 *)datap)[i];  
        } else if (tk == ESMC_TYPEKIND_LOGICAL) {
            vbp = new ESMC_Logical[items];      
            if (datap) 
              for (i=0; i<items; i++)
                vbp[i] = ((ESMC_Logical *)datap)[i];  

        } else
           // error - arrays of char strings not allowed
                voidp = NULL;
  }

 } // end ESMC_Attribute

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "~ESMC_Attribute()"
//BOP
// !IROUTINE:  ~ESMC_Attribute - native C++ destructor for ESMC_Attribute class
//
// !INTERFACE:
      ESMC_Attribute::~ESMC_Attribute(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//
//EOP

  if (tk == ESMC_TYPEKIND_CHARACTER) delete [] vcp;

  if (items > 1) {
        if (tk == ESMC_TYPEKIND_I4) delete [] vip;
        else if (tk == ESMC_TYPEKIND_I8) delete [] vlp;
        else if (tk == ESMC_TYPEKIND_R4) delete [] vfp;
        else if (tk == ESMC_TYPEKIND_R8) delete [] vdp;  
        else if (tk == ESMC_TYPEKIND_LOGICAL) delete [] vbp;
  }

 } // end ~ESMC_Attribute

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
  ID = ++globalCount;
  refCount = 1;
  strcpy(className, "global");
  sprintf(baseName, "%s%3d", "unnamed", ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;

  baseStatus = ESMF_STATUS_READY;

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
  ID = ++globalCount;
  refCount = 1;
  strcpy(className, superclass ? superclass : "global");
  if (name && (name[0]!='\0')) 
      // TODO: make sure this name is unique in this namespace.  This means
      // some sort of registry utility.
      strcpy(baseName, name);
  else
      sprintf(baseName, "%s%3d", className, ID);
  ESMC_CtoF90string(baseName, baseNameF90, ESMF_MAXSTR);

  attrCount = 0;
  attrAlloc = 0;
  attrList = ESMC_NULL_POINTER;
  if (nattrs > 0) {
      if (ESMC_AttributeAlloc(nattrs) != ESMF_SUCCESS) {
          baseStatus = ESMF_STATUS_INVALID;   // can't return err, but can
          return;                            // try to indicate unhappiness
      }
  }

  baseStatus = ESMF_STATUS_READY;

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
  int i;

  baseStatus = ESMF_STATUS_INVALID;

  // if attribute lists, delete them.
  for (i=0; i<attrCount; i++) 
      delete attrList[i];
                         
  if (attrList) delete [] attrList;

  // if we have to support reference counts someday,
  // test if (refCount > 0) and do something if true;

 } // end ~ESMC_Base

