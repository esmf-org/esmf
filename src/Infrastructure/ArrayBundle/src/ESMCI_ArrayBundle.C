// $Id: ESMCI_ArrayBundle.C,v 1.1.2.1 2008/04/24 00:15:54 theurich Exp $
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
#define ESMC_FILENAME "ESMCI_ArrayBundle.C"
//==============================================================================
//
// ESMCI ArrayBundle method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ArrayBundle methods declared
// in the companion file ESMCI_ArrayBundle.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_ArrayBundle.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMC_Start.h"

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_ArrayBundle.C,v 1.1.2.1 2008/04/24 00:15:54 theurich Exp $";
//-----------------------------------------------------------------------------


namespace ESMCI {

  //-----------------------------------------------------------------------------
//
// constructor and destructor
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::ArrayBundle()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::ArrayBundle    - constructor
//
// !INTERFACE:
ArrayBundle::ArrayBundle(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  Array **arrayListArg,                   // (in)
  int arrayCountArg,                      // (in)
  int *rc                                 // (out)
  ){
//
// !DESCRIPTION:
//    Construct the internal structure of an ESMC\_ArrayBundle object.
//    No error checking wrt consistency of input arguments is needed because
//    this ArrayBundle constructor is only to be called by ArrayCreate()
//    interfaces which are responsible for providing consistent arguments to
//    this layer.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  try{  

  // fill in the ArrayBundle object
  arrayCount = arrayCountArg;
  arrayList = new Array*[arrayCount];
  memcpy(arrayList, arrayListArg, arrayCount*sizeof(Array *));
  
  // invalidate the name for this ArrayBundle object in the Base class
  ESMC_BaseSetName(NULL, "ArrayBundle");
   
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::~ArrayBundle()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::~ArrayBundle   - destructor
//
// !INTERFACE:
ArrayBundle::~ArrayBundle(){
//
// !DESCRIPTION:
//    Destruct the internal structure of an ESMC\_ArrayBundle object.
//
//EOPI
//-----------------------------------------------------------------------------
  // garbage collection
  if (arrayList != NULL)
    delete [] arrayList;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::create()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::create
//
// !INTERFACE:
ArrayBundle *ArrayBundle::create(
//
// !RETURN VALUE:
//    ArrayBundle * to newly allocated ArrayBundle
//
// !ARGUMENTS:
//
  Array **arrayListArg,                       // (in)
  int arrayCount,                             // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ArrayBundle} object from list of Arrays.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  ArrayBundle *arraybundle;
  try{
  
  // check the input and get the information together to call creator

  // call class constructor
  try{
    arraybundle = new ArrayBundle(arrayListArg, arrayCount, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::ArrayBundle.", rc);  
    return ESMC_NULL_POINTER;
  }
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return ESMC_NULL_POINTER;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arraybundle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::destroy()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::destroy
//
// !INTERFACE:
int ArrayBundle::destroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle **arraybundle){  // in - ArrayBundle to destroy
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{

  // return with errors for NULL pointer
  if (arraybundle == ESMC_NULL_POINTER || *arraybundle == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ArrayBundle", &rc);
    return rc;
  }

  // delete ArrayBundle object
  delete *arraybundle;
  *arraybundle = ESMC_NULL_POINTER;
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// misc.
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::print()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::print
//
// !INTERFACE:
int ArrayBundle::print()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of ArrayBundle object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ArrayBundle", &rc);
    return rc;
  }

  // print info about the ESMCI::ArrayBundle object
  printf("--- ESMCI::ArrayBundle::print start ---\n");
  printf("ArrayBundle: %s\n", getName());
  printf("arrayCount = %d\n", arrayCount);
  for (int i=0; i<arrayCount; i++)
    printf("arrayList[%d]: %s\n", i, arrayList[i]->getName());
  printf("--- ESMCI::ArrayBundle::print end ---\n");
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
