// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_DynamicMask.C"
//==============================================================================
//
// ESMC DynamicMask method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C DynamicMask methods declared
// in the companion file ESMC_DynamicMask.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMC_DynamicMask.h"

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h" 
#include "ESMCI_DynamicMask.h" 

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version =
  "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_DynamicMaskPredefinedSetR8R8R8(ESMC_DynamicMask *DynamicMask,
  ESMC_PredefinedDynamicMask_Flag mask_flag, int *handleAllElements,
  ESMC_R8 *dynamicSrcMaskValue, ESMC_R8 *dynamicDstMaskValue) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DynamicMaskSetR8R8R8()"
    
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // call into ESMCI interface
  localrc = ((ESMCI::DynamicMask *)DynamicMask)->setR8R8R8(mask_flag, handleAllElements, dynamicSrcMaskValue, dynamicDstMaskValue);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

} //extern "C"
