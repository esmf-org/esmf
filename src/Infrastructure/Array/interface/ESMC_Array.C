// $Id: ESMC_Array.C,v 1.2 2008/02/16 06:04:37 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Array.C"
//==============================================================================
//
// ESMC Array method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Array methods declared
// in the companion file ESMC_Array.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Array.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMC_LogErr.h"
#include "ESMCI_Array.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Array.C,v 1.2 2008/02/16 06:04:37 theurich Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_Array ESMC_ArrayCreate(int *rc){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Array array;
  
  array.ptr = NULL;  // this dummy prototype returns invalidated ESMC_Array

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return array;
}

int ESMC_ArrayPrint(ESMC_Array array){
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  ESMCI::Array *ap = (ESMCI::Array *)(array.ptr); // typecast into ESMCI type

  // call into ESMCI method  
  localrc = ap->print();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
