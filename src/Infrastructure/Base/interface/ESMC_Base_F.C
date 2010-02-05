// $Id: ESMC_Base_F.C,v 1.76.2.1 2010/02/05 19:53:46 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Base method interface (from F90 to C++) file
#define ESMF_FILENAME "ESMC_Base_F.C"

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the Fortran callable 
// interfaces to the C++ Base methods.
//
//-----------------------------------------------------------------------------
//
 // associated class definition file and others
#include <string.h>
#include <stdlib.h>
#include "ESMC_Base.h"
#include "ESMCI_LogErr.h"  // will this work?

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Base_F.C,v 1.76.2.1 2010/02/05 19:53:46 svasquez Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes ESMC_Base routine interfaces
//
//

extern "C" {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  Base object methods
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseCreate - create and initialize a new Base object 
//
// !INTERFACE:
      void FTN(c_esmc_basecreate)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *superclass,         // in - F90, non-null terminated string
      char *name,               // in (opt) - F90, non-null terminated string
      int *nattrs,              // in - number of initial attributes to alloc
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg sclen,  // hidden/in - strlen count for superclass
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for name
// 
// !DESCRIPTION:
//     Create a new Base object.
//
//EOP

  int status;
  char *cname = NULL;
  char *scname = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // copy and convert F90 strings to null terminated ones
  if (superclass && (sclen > 0) && (superclass[0] != '\0')) {
      scname = ESMC_F90toCstring(superclass, sclen);
      if (!scname) {
           ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                         "bad attribute name", &status);
          if (rc) *rc = status;
          return;
      }
  }
  if (name && (nlen > 0) && (name[0] != '\0')) {
      cname = ESMC_F90toCstring(name, nlen);
      if (!cname) {
          delete [] scname;
          if (rc) *rc = status;
          return;
      }
  } 

  (*base) = new ESMC_Base(scname, cname, *nattrs);
  if (*base != NULL)
      *rc = ESMF_SUCCESS;
  else
      *rc = ESMF_FAILURE;

  if (scname) delete [] scname;
  if (cname)  delete [] cname;
  return;

}  // end c_ESMC_BaseCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseDestroy - release resources from a Base object
//
// !INTERFACE:
      void FTN(c_esmc_basedestroy)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Free resources associated with a base object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  // nothing to be done, because automatic garbage collection takes care
  // of Base delete

  // return successfully
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BasePrint - print Base object 
//
// !INTERFACE:
      void FTN(c_esmc_baseprint)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *opts,               // in - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for options
// 
// !DESCRIPTION:
//     Print the contents of a base object.
//
//EOP

  char *copts = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
    // for Print, it's not a failure for an uninit object to be printed
  }

  // copy and convert F90 string to null terminated one
  if (opts && (nlen > 0) && (opts[0] != '\0')) {
      copts = ESMC_F90toCstring(opts, nlen);
      if (!copts) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  *rc = (*base)->ESMC_Print(copts);

  if (copts)
      delete [] copts;
  return;

}  // end c_ESMC_BasePrint

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseSerialize - Serialize Base object 
//
// !INTERFACE:
      void FTN(c_esmc_baseserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *buf,                // in/out - really a byte stream
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      ESMC_AttReconcileFlag *attreconflag, // in - attreconcile flag
      ESMC_InquireFlag *inquireflag,       // in - inquire-only flag
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a base object.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;


  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*base)->ESMC_Serialize(buf, length, offset, *attreconflag,
                                *inquireflag);

  return;

}  // end c_ESMC_BaseSerialize

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseDeserialize - Deserialize Base object 
//
// !INTERFACE:
      void FTN(c_esmc_basedeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      ESMC_AttReconcileFlag *attreconflag, // in - attreconcile flag
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a base object.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // CAUTION:
  // Allocate Base object in glue code because the ESMCI implementation
  // of deserialize() does _not_ allocate, since it is meant to be called
  // on an object of a class derived from Base. On the Fortran side, however,
  // there is no such thing as a derived class, and Base is held as a data
  // member in each class -> need allocation here! *gjt*
  // Must explicitly set dummy ID (here -1) to prevent inconsistency in
  // Base object counting. ID will be overwritten by StateReconcile() anyway.
  *base = new ESMC_Base(-1);
  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object error", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *rc = (*base)->ESMC_Deserialize(buf, offset, *attreconflag);

  return;

}  // end c_ESMC_BaseDeserialize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseValidate - print Base object 
//
// !INTERFACE:
      void FTN(c_esmc_basevalidate)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *opts,               // in - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for options
// 
// !DESCRIPTION:
//     Validate the contents of a base object.
//
//EOP

  char *copts = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  // copy and convert F90 string to null terminated one
  if (opts && (nlen > 0) && (opts[0] != '\0')) {
      copts = ESMC_F90toCstring(opts, nlen);
      if (!copts) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  *rc = (*base)->ESMC_Validate(copts);

  if (copts)
      delete [] copts;
  return;

}  // end c_ESMC_BaseValidate

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetName - return the object name to a Fortran caller
//
// !INTERFACE:
      void FTN(c_esmc_getname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *name,               // out - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     return the name to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  if (ESMF_MAXSTR < nlen){
    strncpy(name, (*base)->ESMC_BaseGetF90Name(), ESMF_MAXSTR);
    memset(name+ESMF_MAXSTR, ' ', nlen-ESMF_MAXSTR);  // fill rest with spaces
  }else{
    strncpy(name, (*base)->ESMC_BaseGetF90Name(), nlen);
  }

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_GetName

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetName - set the object name from an F90 caller
//
// !INTERFACE:
      void FTN(c_esmc_setname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *classname,          // in - F90, non-null terminated string
      char *objname,            // in - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg clen,   // hidden/in - max strlen count for classname
      ESMCI_FortranStrLenArg olen) { // hidden/in - max strlen count for objname
// 
// !DESCRIPTION:
//     set the name from an F90 caller.
//
//EOPI

  char *oname = NULL;
  char *cname = NULL;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }
 
  if (classname && (clen > 0) && (classname[0] != '\0')) {
      // copy and convert F90 string to null terminated one
      cname = ESMC_F90toCstring(classname, clen);
      if (!cname) {
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  if (objname && (olen > 0) && (objname[0] != '\0')) {
      // copy and convert F90 string to null terminated one
      oname = ESMC_F90toCstring(objname, olen);
      if (!oname) {
          if (!cname)
              delete [] cname;
          if (rc) *rc = ESMF_FAILURE;
          return;
      }
  }

  (*rc) = (*base)->ESMC_BaseSetName(oname, cname);

  delete [] oname;
  delete [] cname; 

  return;

}  // end c_ESMC_SetName

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetClassName - return the object name to a Fortran caller
//
// !INTERFACE:
      void FTN(c_esmc_getclassname)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *classname,          // out - Fortran, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - max strlen count for name
// 
// !DESCRIPTION:
//     return the name to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *rc = (*base)->ESMC_BaseGetF90ClassName(classname, nlen);

  return;

}  // end c_ESMC_GetClassName

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetID - return the object id to the caller
//
// !INTERFACE:
      void FTN(c_esmc_getid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      int *id,                  // out - Fortran, integer address
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     return the object ID to a Fortran caller.
//
//EOPI

  int i, status;

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base || !id) {
    printf("in c_ESMC_GetID, base or id bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *id = (*base)->ESMC_BaseGetID();
  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_GetID

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetID - set an object id 
//
// !INTERFACE:
      void FTN(c_esmc_setid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      int *id,                  // in - Fortran, integer address
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     set an object ID from a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base || !id) {
    printf("in c_ESMC_SetID, base or id bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  (*base)->ESMC_BaseSetID(*id);

  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_SetID

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetVMId - return the object's VMId to the caller
//
// !INTERFACE:
      void FTN(c_esmc_getvmid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      ESMCI::VMId **vmid,       // out - Fortran, ESMF_VMId
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     return the object's VMId to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    printf("in c_ESMC_GetVMId, base is bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *vmid = (*base)->ESMC_BaseGetVMId();
  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_GetVMId

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetVMId - allocate space and set the object's VMId 
//
// !INTERFACE:
      void FTN(c_esmc_setvmid)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      ESMCI::VMId **vmid,       // in - Fortran, ESMF_VMId
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     allocate space and set the object's VMId.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    printf("in c_ESMC_SetVMId, base is bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  (*base)->ESMC_BaseSetVMId(*vmid);
  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_SetVMId


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseSetBaseStatus - set baseStatus in Base object
//
// !INTERFACE:
      void FTN(c_esmc_basesetbasestatus)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      ESMC_Status *baseStatus,  // in - baseStatus
      int *rc                   // out - return code
      ){
// 
// !DESCRIPTION:
//     set baseStatus in Base object
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  (*base)->ESMC_BaseSetBaseStatus(*baseStatus);

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseSetBaseStatus

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseGetBaseStatus - get baseStatus from Base object
//
// !INTERFACE:
      void FTN(c_esmc_basegetbasestatus)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      ESMC_Status *baseStatus,  // in - baseStatus
      int *rc                   // out - return code
      ){
// 
// !DESCRIPTION:
//     get baseStatus from Base object
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  *baseStatus = (*base)->ESMC_BaseGetBaseStatus();

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseGetBaseStatus


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseSetStatus - set status in Base object
//
// !INTERFACE:
      void FTN(c_esmc_basesetstatus)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      ESMC_Status *status,      // in - status
      int *rc                   // out - return code
      ){
// 
// !DESCRIPTION:
//     set status in Base object
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  (*base)->ESMC_BaseSetStatus(*status);

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseSetStatus

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseGetStatus - get status from Base object
//
// !INTERFACE:
      void FTN(c_esmc_basegetstatus)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      ESMC_Status *status,      // in - status
      int *rc                   // out - return code
      ){
// 
// !DESCRIPTION:
//     get status from Base object
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  *status = (*base)->ESMC_BaseGetStatus();

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseGetStatus


} // extern "C"
