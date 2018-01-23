// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_ArraySpec.C"
//==============================================================================
//
// ESMC ArraySpec method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C ArraySpec methods declared
// in the companion file ESMC_ArraySpec.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMC_ArraySpec.h"

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h" 
#include "ESMCI_ArraySpec.h" 

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version =
  "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_ArraySpecSet(ESMC_ArraySpec *arrayspec,int rank,
  ESMC_TypeKind_Flag typekind){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySpecSet()"
    
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // call into ESMCI interface
  localrc = ((ESMCI::ArraySpec *)arrayspec)->set(rank, typekind);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

int ESMC_ArraySpecGet(ESMC_ArraySpec arrayspec, int *rank,
  ESMC_TypeKind_Flag *typekind){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySpecGet()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // call into ESMCI interface
  *rank = ((ESMCI::ArraySpec *)&arrayspec)->getRank(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  *typekind = ((ESMCI::ArraySpec *)&arrayspec)->getTypeKind(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

} //extern "C"
