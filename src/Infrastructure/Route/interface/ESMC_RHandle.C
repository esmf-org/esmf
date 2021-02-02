// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_RHandle.C"
//==============================================================================
//
// ESMC RouteHandle method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C RouteHandle methods declared
// in the companion file ESMC_RHandle.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMC_RHandle.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_RHandle.h"
#include "ESMC_Interface.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleCreateFromFile()"
ESMC_RouteHandle ESMC_RouteHandleCreateFromFile(char *filename, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;
  
  // typecast into ESMCI type
  ESMC_RouteHandle rh;
  rh.ptr = NULL;

  // call into ESMCI method  
  rh.ptr = reinterpret_cast<void *>(ESMCI::RouteHandle::create(filename, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return rh;  // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return rh;
}  
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandlePrint()"
int ESMC_RouteHandlePrint(ESMC_RouteHandle rh){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::RouteHandle *rhp = (ESMCI::RouteHandle *)(rh.ptr);

  // call into ESMCI method  
  localrc = rhp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_RouteHandleWrite()"
int ESMC_RouteHandleWrite(ESMC_RouteHandle rh, char *filename){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::RouteHandle *rhp = (ESMCI::RouteHandle *)(rh.ptr);

  // call into ESMCI method  
  localrc = rhp->write(filename);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------

 
}; // extern "C"
