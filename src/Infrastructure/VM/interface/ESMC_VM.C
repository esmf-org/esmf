// $Id: ESMC_VM.C,v 1.2 2008/07/29 01:34:56 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_VM.C"
//==============================================================================
//
// ESMC VM method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C VM methods declared
// in the companion file ESMC_VM.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_VM.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_VM.C,v 1.2 2008/07/29 01:34:56 rosalind Exp $";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_VMPrint(ESMC_VM vm){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMPrint()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::VM *vmp = (ESMCI::VM *)(vm.ptr);

  // call into ESMCI method  
  localrc = vmp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
