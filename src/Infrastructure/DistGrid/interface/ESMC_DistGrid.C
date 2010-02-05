// $Id: ESMC_DistGrid.C,v 1.8.2.1 2010/02/05 19:55:14 svasquez Exp $
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
#define ESMC_FILENAME "ESMC_DistGrid.C"
//==============================================================================
//
// ESMC DistGrid method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C DistGrid methods declared
// in the companion file ESMC_DistGrid.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_DistGrid.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_DistGrid.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_DistGrid.C,v 1.8.2.1 2010/02/05 19:55:14 svasquez Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_DistGrid ESMC_DistGridCreate(ESMC_InterfaceInt minIndexInterfaceArg,
  ESMC_InterfaceInt maxIndexInterfaceArg, int *rc){ //TODO: complete this API
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridCreate()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_DistGrid distgrid;
  
  // typecast into ESMCI types
  ESMCI::InterfaceInt *minIndexInterface =
    (ESMCI::InterfaceInt *)(minIndexInterfaceArg.ptr);
  ESMCI::InterfaceInt *maxIndexInterface =
    (ESMCI::InterfaceInt *)(maxIndexInterfaceArg.ptr);
  
  distgrid.ptr = (void *)
    ESMCI::DistGrid::create(minIndexInterface, maxIndexInterface, NULL,
      NULL, 0, NULL, NULL, NULL, NULL, NULL, (ESMCI::DELayout*)NULL, NULL,
      &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    distgrid.ptr = NULL;
    return distgrid;  // bail out
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}

int ESMC_DistGridPrint(ESMC_DistGrid distgrid){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridPrint()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::DistGrid *dgp = (ESMCI::DistGrid *)(distgrid.ptr);

  // call into ESMCI method  
  localrc = dgp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

int ESMC_DistGridDestroy(ESMC_DistGrid *distgrid){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridDestroy()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::DistGrid *dgp = (ESMCI::DistGrid *)(distgrid->ptr);

  // call into ESMCI method  
  localrc = ESMCI::DistGrid::destroy(&dgp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
  
  // invalidate pointer
  distgrid->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
