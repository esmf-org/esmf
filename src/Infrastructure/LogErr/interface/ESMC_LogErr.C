// $Id: ESMC_LogErr.C,v 1.10 2011/11/08 21:27:26 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_LogErr.C"
//==============================================================================
//
// ESMC LogErr method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C LogErr methods declared
// in the companion file ESMC_LogErr.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_LogErr.h"

// include ESMF headers
#include "ESMCI_LogErr.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_LogErr.C,v 1.10 2011/11/08 21:27:26 rokuingh Exp $";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_LogSet(bool flush){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_LogSet()"

  // Initialize return code. Assume routine not implemented
  int rc = ESMF_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  // Call into ESMCI method
  localrc = ESMC_LogDefault.Set(flush);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                    ESMC_CONTEXT, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc; 

} // ESMC_LogSet

int ESMC_LogWrite(const char msg[], int msgtype){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_LogWrite()"

//TODO: Add context arguments as optional arguments

  // Initialize return code. Assume routine not implemented
  int rc = ESMF_RC_NOT_IMPL;
  int localrc = ESMC_RC_NOT_IMPL;

  // Call into ESMCI method
  localrc = ESMC_LogDefault.Write(msg, msgtype);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                    ESMC_CONTEXT, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc; 

} // ESMC_LogWrite

} // extern "C"
