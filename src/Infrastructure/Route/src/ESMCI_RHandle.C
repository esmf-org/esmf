// $Id: ESMCI_RHandle.C,v 1.4.2.1 2010/02/05 19:59:59 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMF_FILENAME "ESMCI_RHandle.C"
//==============================================================================
//
// ESMCI RouteHandle method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RouteHandle methods declared
// in the companion file ESMCI_RHandle.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_RHandle.h"

// include higher level, 3rd party or system headers
#include <cstdlib>

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMCI_Array.h"
#include "ESMCI_ArrayBundle.h"

// LogErr headers
#include "ESMCI_LogErr.h"                  // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = 
  "$Id: ESMCI_RHandle.C,v 1.4.2.1 2010/02/05 19:59:59 svasquez Exp $";
//-----------------------------------------------------------------------------


namespace ESMCI {

  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::create()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::create - Create a new RouteHandle
//
// !INTERFACE:
RouteHandle *RouteHandle::create(
//
// !RETURN VALUE:
//  pointer to newly allocated RouteHandle
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Allocate memory for a new RouteHandle object and initialize it. 
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  RouteHandle *routehandle;
  try{

    routehandle = new RouteHandle;

    localrc = routehandle->construct();
    if (ESMC_LogDefault.MsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc)){
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return NULL;
    }
    
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::Array.", rc);  
    routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return routehandle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::destroy()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::destroy - free a RouteHandle
//
// !INTERFACE:
int RouteHandle::destroy(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    RouteHandle *routehandle) {    // in - RouteHandle object to destroy
//
// !DESCRIPTION:
//  ESMF routine which destroys a RouteHandle object.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // return with errors for NULL pointer
  if (routehandle == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to RouteHandle", &rc);
    return rc;
  }
  
  // destruct DistGrid object
  localrc = routehandle->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // mark as invalid object
  routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::construct()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::construct - fill a RouteHandle
//
// !INTERFACE:
int RouteHandle::construct(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    void){ 
// 
// !DESCRIPTION: 
//  ESMF routine which fills in the contents of an already
//  allocated RouteHandle object
//
//EOP
//-----------------------------------------------------------------------------
  htype = ESMC_UNINITIALIZEDHANDLE;
  storage = NULL;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::destruct()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::destruct - release RouteHandle resources
//
// !INTERFACE:
int RouteHandle::destruct(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  void){
//
// !DESCRIPTION:
//  Deallocates any space allocated by ESMF_RouteHandleConstruct.
//
//EOP
//-----------------------------------------------------------------------------
  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    switch (htype){
    case ESMC_ARRAYXXE:
      ESMCI::Array::sparseMatMulRelease(this);
      break;
    case ESMC_ARRAYBUNDLEXXE:
      ESMCI::ArrayBundle::sparseMatMulRelease(this);
      break;
    default:
      break;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::validate()"
//BOP
// !IROUTINE: ESMCI::RouteHandle::validate - validate a RouteHandle
//
// !INTERFACE:
int RouteHandle::validate(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//  Validates that a RouteHandle is internally consistent.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::print()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::print - print contents of a RouteHandle
//
// !INTERFACE:
int RouteHandle::print(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  const char *options)const{     //  in - print options
//
// !DESCRIPTION:
//  Print information about a RouteHandle.  The options control the
//  type of information and level of detail.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  return rc;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI
