// $Id: ESMC_Array.C,v 1.8 2008/04/02 02:58:38 rosalind Exp $
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

// include generic header files
#include <string.h>

// include associated header file
#include "ESMC_Array.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMC_LogErr.h"
#include "ESMCI_Array.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Array.C,v 1.8 2008/04/02 02:58:38 rosalind Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_Array ESMC_ArrayCreate(ESMC_ArraySpec arrayspec, ESMC_DistGrid distgrid,
  char* name, int *rc){ //TODO: complete this API
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayCreate()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Array array;

  array.ptr = (void *)
    ESMCI::Array::create((ESMCI::ArraySpec *)&arrayspec,
    (ESMCI::DistGrid *)(distgrid.ptr),
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    &localrc);

  // Set name in newly created Array
  localrc = ((ESMCI::Array*)(array.ptr))->ESMCI::Array::setName(name);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    array.ptr = NULL;
    return array;  // bail out
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return array;
}


char* ESMC_ArrayGetName(ESMC_Array array, int *rc){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetName()"

  // initialize return code; assume routine not implemented
  *rc = ESMC_RC_NOT_IMPL;              // final return code

  ESMCI::Array *ap = (ESMCI::Array *)(array.ptr); // typecast into ESMCI type

  // call into ESMCI method 
  char* name;
  name = new char;
  memset(name, ' ', ESMF_MAXSTR);
  strncpy(name, ap->getName(), ESMF_MAXSTR);

//printf("In ESMC_ArrayGetName, name= %s \n",name);

  *rc = ESMF_SUCCESS;
  return name;
} 


int ESMC_ArrayPrint(ESMC_Array array){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayPrint()"

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

int ESMC_ArrayDestroy(ESMC_Array *array){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayDestroy()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::Array *ap = (ESMCI::Array *)(array->ptr);

  // call into ESMCI method  
  localrc = ESMCI::Array::destroy(&ap);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
  
  // invalidate pointer
  array->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
