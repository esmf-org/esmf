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
#include "ESMCI_LogErr.h"
#include "ESMCI_Array.h"
#include "ESMC_Interface.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayCreate()"
ESMC_Array ESMC_ArrayCreate(ESMC_ArraySpec arrayspec, ESMC_DistGrid distgrid,
  const char* name, int *rc){ //TODO: complete this API

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Array array;

  // call into ESMCI method 
  array.ptr = (void *)
    ESMCI::Array::create((ESMCI::ArraySpec *)&arrayspec,
    (ESMCI::DistGrid *)(distgrid.ptr),
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    &localrc);

  // Set name in newly created Array
  localrc = ((ESMCI::Array*)(array.ptr))->ESMCI::Array::setName(name);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)){
    array.ptr = NULL;
    return array;  // bail out
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return array;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayDestroy()"
int ESMC_ArrayDestroy(ESMC_Array *array){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::Array *ap = (ESMCI::Array *)(array->ptr);

  // call into ESMCI method  
  localrc = ESMCI::Array::destroy(&ap);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
  
  // invalidate pointer
  array->ptr = NULL;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayPrint()"
int ESMC_ArrayPrint(ESMC_Array array){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::Array *ap = (ESMCI::Array *)(array.ptr);

  // call into ESMCI method  
  localrc = ap->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetName()"
const char *ESMC_ArrayGetName(ESMC_Array array, int *rc){

  // initialize return code; assume routine not implemented
  *rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  ESMCI::Array *ap = (ESMCI::Array *)(array.ptr);

  // call into ESMCI method 
  const char *name = ap->getName();

  // return successfully
  *rc = ESMF_SUCCESS;
  return name;
} 
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetPtr()"
void *ESMC_ArrayGetPtr(ESMC_Array array, int localDe, int *rc){

  // initialize return code; assume routine not implemented
  *rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (array.ptr == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
    "- Not a valid ESMC_Array object", ESMC_CONTEXT, rc);
    return NULL;  // bail out
  }
  
  // typecast into ESMCI type
  ESMCI::Array *ap = (ESMCI::Array *)(array.ptr);

  // check localDe input
  int localDeCount = ap->getDELayout()->getLocalDeCount();  
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
    "- localDe argument out of range", ESMC_CONTEXT, rc);
    return NULL;  // bail out
  }

  // call into ESMCI method 
  ESMCI::LocalArray **localarrayList = ap->getLocalarrayList();
  void *ptr = localarrayList[localDe]->getBaseAddr();

  // return successfully
  *rc = ESMF_SUCCESS;
  return ptr;
} 
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySetLWidth()"
int ESMC_ArraySetLWidth(ESMC_Array array,
  ESMC_InterArrayInt computationalLWidthArg){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
 
  // typecast into ESMCI type
  ESMCI::Array *ap = (ESMCI::Array *)(array.ptr);
 
  // call into ESMCI method
  localrc = ap->setComputationalLWidth(
    (ESMCI::InterArray<int> *)&computationalLWidthArg);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------
  
}; // extern "C"
