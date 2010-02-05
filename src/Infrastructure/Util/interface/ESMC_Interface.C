// $Id: ESMC_Interface.C,v 1.4.4.1 2010/02/05 20:01:06 svasquez Exp $
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
#define ESMC_FILENAME "ESMC_Interface.C"
//==============================================================================
//
// ESMC Interface method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Interface methods declared
// in the companion file ESMC_Interface.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Interface.h"

// include ESMF headers
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Interface.C,v 1.4.4.1 2010/02/05 20:01:06 svasquez Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_InterfaceInt ESMC_InterfaceIntCreate(int *arrayArg, int lenArg, int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_InterfaceInt interfaceInt;
  
  interfaceInt.ptr = (void *)(new ESMCI::InterfaceInt(arrayArg, lenArg));

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return interfaceInt;
}

int ESMC_InterfaceIntDestroy(ESMC_InterfaceInt *interfaceIntArg){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::InterfaceInt *iip = (ESMCI::InterfaceInt *)(interfaceIntArg->ptr);

  // call into ESMCI method
  delete iip;
  
  // invalidate pointer
  interfaceIntArg->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
