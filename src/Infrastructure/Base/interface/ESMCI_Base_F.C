// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMF_FILENAME "ESMCI_Base_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------

#include "ESMCI_Base.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"

#include <string>
using namespace std;

#include <stdlib.h>

// the interface subroutine names MUST be in lower case by ESMF convention
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
      void FTN_X(c_esmc_basecreate)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basecreate()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      const char *superclass,   // in - F90, non-null terminated string
      const char *name,         // in (opt) - F90, non-null terminated string
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

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  string scname = string (superclass, ESMC_F90lentrim (superclass, sclen));
  string cname  = string (name, ESMC_F90lentrim (name, nlen));
  (*base) = new ESMC_Base(scname.c_str(), cname.c_str(), *nattrs);
  if (*base != NULL)
      *rc = ESMF_SUCCESS;
  else
      *rc = ESMF_FAILURE;

  return;

}  // end c_ESMC_BaseCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BaseDestroy - release resources from a Base object
//
// !INTERFACE:
      void FTN_X(c_esmc_basedestroy)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basedestroy()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      ESMC_Logical *noGarbage,
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Free resources associated with a base object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
  
  // convert to bool
  bool noGarbageOpt = false;  // default
  if (noGarbage != NULL)
    if (*noGarbage == ESMF_TRUE) noGarbageOpt = true;

  if (noGarbageOpt){
    delete *base;
  }
  
  // return successfully
  *rc = ESMF_SUCCESS;
  return;

}  // end c_ESMC_BaseDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  c_ESMC_BasePrint - print Base object 
//
// !INTERFACE:
      void FTN_X(c_esmc_baseprint)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_baseprint()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      int *level,               // in - print level for recursive prints
      const char *opts,         // in - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for options
// 
// !DESCRIPTION:
//     Print the contents of a base object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object uninitialized", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);
    if (rc) *rc = ESMF_SUCCESS;
    return;
    // for Print, it's not a failure for an uninit object to be printed
  }

  string copts = string (opts, ESMC_F90lentrim (opts, nlen));
  *rc = (*base)->ESMC_Print(*level, copts.c_str());
  fflush (stdout);

  return;

}  // end c_ESMC_BasePrint

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_BaseSerialize - Serialize Base object 
//
// !INTERFACE:
      void FTN_X(c_esmc_baseserialize)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_baseserialize()"
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
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg buf_l) { // hidden/in - buffer length
// 
// !DESCRIPTION:
//     Serialize the contents of a base object.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;


  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object uninitialized", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);
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
      void FTN_X(c_esmc_basedeserialize)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basedeserialize()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      ESMC_AttReconcileFlag *attreconflag, // in - attreconcile flag
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg buf_l) { // hidden/in - buffer length
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
    ESMC_LogDefault.Write("Base object error", ESMC_LOGMSG_INFO, ESMC_CONTEXT);
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
      void FTN_X(c_esmc_basevalidate)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basevalidate()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      const char *opts,         // in - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg nlen) { // hidden/in - strlen count for options
// 
// !DESCRIPTION:
//     Validate the contents of a base object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    //printf("uninitialized Base object\n");
    ESMC_LogDefault.Write("Base object uninitialized", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  string copts = string (opts, ESMC_F90lentrim (opts, nlen));
  *rc = (*base)->ESMC_Validate(copts.c_str());

}  // end c_ESMC_BaseValidate

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetName - return the object name to a Fortran caller
//
// !INTERFACE:
      void FTN_X(c_esmc_getname)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getname()"
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
// !IROUTINE:  c_ESMC_GetVM - return the object's VM to the caller
//
// !INTERFACE:
      void FTN_X(c_esmc_getvm)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getvm()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      ESMCI::VM **vm,           // out - Fortran, ESMF_VM
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     return the object's VM to a Fortran caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    printf("in c_ESMC_GetVM, base is bad, returning failure\n");
    if (rc) *rc = ESMF_FAILURE;
    return;
  }

  *vm = (*base)->ESMC_BaseGetVM();
  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_GetVM

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_SetName - set the object name from an F90 caller
//
// !INTERFACE:
      void FTN_X(c_esmc_setname)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setname()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in/out - base object
      const char *classname,    // in - F90, non-null terminated string
      const char *objname,      // in - F90, non-null terminated string
      int *rc,                  // out - return code
      ESMCI_FortranStrLenArg clen,   // hidden/in - max strlen count for classname
      ESMCI_FortranStrLenArg olen) { // hidden/in - max strlen count for objname
// 
// !DESCRIPTION:
//     set the name from an F90 caller.
//
//EOPI

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!base) {
    if (rc) *rc = ESMF_FAILURE;
    return;
  }
 
  string oname = string (objname, ESMC_F90lentrim (objname, olen));
  string cname = string (classname, ESMC_F90lentrim (classname, clen));
  (*rc) = (*base)->ESMC_BaseSetName(oname.c_str(), cname.c_str());

  return;

}  // end c_ESMC_SetName

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  c_ESMC_GetClassName - return the object name to a Fortran caller
//
// !INTERFACE:
      void FTN_X(c_esmc_getclassname)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getclassname()"
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
      void FTN_X(c_esmc_getid)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getid()"
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
      void FTN_X(c_esmc_setid)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setid()"
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
      void FTN_X(c_esmc_getvmid)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_getvmid()"
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
      void FTN_X(c_esmc_setvmid)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_setvmid()"
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
      void FTN_X(c_esmc_basesetbasestatus)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basesetbasestatus()"
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
      void FTN_X(c_esmc_basegetbasestatus)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basegetbasestatus()"
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
      void FTN_X(c_esmc_basesetstatus)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basesetstatus()"
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
      void FTN_X(c_esmc_basegetstatus)(
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_basegetstatus()"
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Base **base,         // in - base object
      ESMC_Status *status,      // out - status
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
