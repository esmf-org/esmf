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
#define ESMC_FILENAME "ESMCI_ArraySpec.C"
//==============================================================================
//
// !DESCRIPTION:
//
// The code in this file implements the internal C++ ArraySpec API declared
// in the companion file ESMCI_ArraySpec.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_ArraySpec.h"

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h" 

extern "C" {
  
// Prototypes of the Fortran interface functions.
void FTN_X(f_esmf_arrayspecset)(ESMCI::ArraySpec *arrayspec, int *rank,
  ESMC_TypeKind_Flag *typekind, int *rc);

void FTN_X(f_esmf_arrayspecgetrank)(ESMCI::ArraySpec *arrayspec, int *rank,
  int *rc);

void FTN_X(f_esmf_arrayspecgettypekind)(ESMCI::ArraySpec *arrayspec,
  ESMC_TypeKind_Flag *typekind, int *rc);

}; // extern "C"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

int ArraySpec::set(int rank, ESMC_TypeKind_Flag typekind){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArraySpec::set()"
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  FTN_X(f_esmf_arrayspecset)(this, &rank, &typekind, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
  
int ArraySpec::getRank(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArraySpec::getRank()"
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  int rank;
  FTN_X(f_esmf_arrayspecgetrank)(this, &rank, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return -1;  // bail out with invalid rank
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return rank;
}

ESMC_TypeKind_Flag ArraySpec::getTypeKind(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArraySpec::getTypeKind()"
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  ESMC_TypeKind_Flag typekind;
  FTN_X(f_esmf_arrayspecgettypekind)(this, &typekind, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMF_NOKIND;  // bail out with invalid typekind
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return typekind;
}
  
} // namespace ESMCI
