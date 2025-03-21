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
#define ESMC_FILENAME "ESMCI_DynamicMask.C"
//==============================================================================
//
// !DESCRIPTION:
//
// The code in this file implements the internal C++ DynamicMask API declared
// in the companion file ESMCI_DynamicMask.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_DynamicMask.h"

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h" 

extern "C" {
  
// Prototypes of the Fortran interface functions.
void FTN_X(f_esmf_dynamicmaskpredefinedsetr8r8r8)(ESMCI::DynamicMask *DynamicMask, ESMC_PredefinedDynamicMask_Flag *maskType, int *handleAllElements, int *haepresent, 
  ESMC_R8 *dynamicSrcMaskValue, int *dsmpresent, 
  ESMC_R8 *dynamicDstMaskValue, int *ddmpresent, int *rc);


}; // extern "C"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

int DynamicMask::setR8R8R8(
  ESMC_PredefinedDynamicMask_Flag maskType, int *handleAllElements,
  ESMC_R8 *dynamicSrcMaskValue, ESMC_R8 *dynamicDstMaskValue) {
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DynamicMask::setR8R8R8()"
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  int haepresent = 0;
  int dsmpresent = 0;
  int  ddmpresent = 0;

  if (handleAllElements != ESMC_NULL_POINTER) {
	  int templ = *handleAllElements;
     haepresent = 1;
  }
  if (dynamicSrcMaskValue != ESMC_NULL_POINTER) {
     ESMC_R8 rtemp;
     rtemp = *dynamicSrcMaskValue;
     dsmpresent = 1;
  }
  if (dynamicDstMaskValue != ESMC_NULL_POINTER) {
     ddmpresent = 1;
  }
  FTN_X(f_esmf_dynamicmaskpredefinedsetr8r8r8)(this, &maskType, handleAllElements, &haepresent,
  dynamicSrcMaskValue, &dsmpresent, dynamicDstMaskValue, &ddmpresent, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
  
} // namespace ESMCI
