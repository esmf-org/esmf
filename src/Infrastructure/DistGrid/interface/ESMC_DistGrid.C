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
#include "ESMCI_DistGrid.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_DistGrid ESMC_DistGridCreate(ESMC_InterArrayInt minIndexInterfaceArg,
  ESMC_InterArrayInt maxIndexInterfaceArg, int *rc){ //TODO: complete this API
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridCreate()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_DistGrid distgrid;
  
  // typecast into ESMCI types
  ESMCI::InterArray<int> *minIndexInterface =
    (ESMCI::InterArray<int> *)&minIndexInterfaceArg;
  ESMCI::InterArray<int> *maxIndexInterface =
    (ESMCI::InterArray<int> *)&maxIndexInterfaceArg;
  
  distgrid.ptr = (void *)
    ESMCI::DistGrid::create(minIndexInterface, maxIndexInterface, NULL,
      NULL, 0, NULL, NULL, NULL, NULL, NULL, (ESMCI::DELayout*)NULL, NULL,
      &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, rc)){
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
  
  // check input
  if (distgrid.ptr == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
    "- Not a valid ESMC_DistGrid object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }
  
  // typecast into ESMCI type
  ESMCI::DistGrid *dgp = (ESMCI::DistGrid *)(distgrid.ptr);
  // test for NULL pointer via macro before calling any class methods
  ESMCI_NULL_CHECK_RC(dgp, rc)
  
  // call into ESMCI method  
  localrc = dgp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, &rc)) return rc;  // bail out
    
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
  
  // test for NULL pointer via macro before calling any class methods
  ESMCI_NULL_CHECK_RC(distgrid, rc)
  
  // typecast into ESMCI type
  ESMCI::DistGrid *dgp = (ESMCI::DistGrid *)(distgrid->ptr);
  // test for NULL pointer via macro before calling any class methods
  ESMCI_NULL_CHECK_RC(dgp, rc)

  // call into ESMCI method  
  localrc = ESMCI::DistGrid::destroy(&dgp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, &rc)) return rc;  // bail out
  
  // invalidate pointer
  distgrid->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
