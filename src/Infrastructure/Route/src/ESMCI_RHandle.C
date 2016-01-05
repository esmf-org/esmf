// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMF_FILENAME "ESMCI_RHandle.C"
//==============================================================================
//
// RouteHandle class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ RouteHandle methods declared
// in the companion file ESMCI_RHandle.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_RHandle.h"

// include higher level, 3rd party or system headers
#include <cstdlib>
#include <cstdio>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_Array.h"
#include "ESMCI_ArrayBundle.h"

// LogErr headers
#include "ESMCI_LogErr.h"                  // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = 
  "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI {

  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::create()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::create - Create a new RouteHandle
//
// !INTERFACE:
RouteHandle *RouteHandle::create(
//
// !RETURN VALUE:
//  pointer to newly allocated RouteHandle
//
// !ARGUMENTS:
    int *rc) {           // out - return code
//
// !DESCRIPTION:
//  Allocate memory for a new RouteHandle object and initialize it. 
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  RouteHandle *routehandle;
  try{

    routehandle = new RouteHandle;

    localrc = routehandle->construct();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return NULL;
    }
    
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::Array.", ESMC_CONTEXT, rc);  
    routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return routehandle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::destroy()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::destroy - free a RouteHandle
//
// !INTERFACE:
int RouteHandle::destroy(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    RouteHandle *routehandle) {    // in - RouteHandle object to destroy
//
// !DESCRIPTION:
//  ESMF routine which destroys a RouteHandle object.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // return with errors for NULL pointer
  if (routehandle == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to RouteHandle", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // destruct RouteHandle object
  localrc = routehandle->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
  // mark as invalid object
  routehandle->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::construct()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::construct - fill a RouteHandle
//
// !INTERFACE:
int RouteHandle::construct(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
    void){ 
// 
// !DESCRIPTION: 
//  ESMF routine which fills in the contents of an already
//  allocated RouteHandle object
//
//EOP
//-----------------------------------------------------------------------------
  htype = ESMC_UNINITIALIZEDHANDLE;
  for (int i=0; i<RHSTORAGECOUNT; i++)
    storage[i] = NULL;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::destruct()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::destruct - release RouteHandle resources
//
// !INTERFACE:
int RouteHandle::destruct(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  void){
//
// !DESCRIPTION:
//  Deallocates any space allocated by ESMF_RouteHandleConstruct.
//
//EOP
//-----------------------------------------------------------------------------
  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    switch (htype){
    case ESMC_ARRAYXXE:
      ESMCI::Array::sparseMatMulRelease(this);
      break;
    case ESMC_ARRAYBUNDLEXXE:
      ESMCI::ArrayBundle::sparseMatMulRelease(this);
      break;
    default:
      break;
    }
  }

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::validate()"
//BOP
// !IROUTINE: ESMCI::RouteHandle::validate - validate a RouteHandle
//
// !INTERFACE:
int RouteHandle::validate(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  )const{
//
// !DESCRIPTION:
//  Validates that a RouteHandle is internally consistent.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::print()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::print - print contents of a RouteHandle
//
// !INTERFACE:
int RouteHandle::print(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  )const{
//
// !DESCRIPTION:
//  Print information about a RouteHandle.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{
  
    // get XXE from routehandle
    XXE *xxe = (XXE *)getStorage();
    
    if (xxe == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc);
      return rc;
    }
  
    // write XXE stream profile to file
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    int localPet = vm->getLocalPet();
    int petCount = vm->getPetCount();
    char file[160];
    sprintf(file, "xxeprofile.%05d", localPet);
    FILE *fp = fopen(file, "a");
    fprintf(fp, "\n=================================================="
      "==============================\n");
    fprintf(fp, "=================================================="
      "==============================\n\n");
    for (int pet=0; pet<petCount; pet++){
      if (pet==localPet){
        localrc = xxe->printProfile(fp);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
      }
      vm->barrier();
    }
    fclose(fp);
    
    // -------------------------------------------------------------------------
#define PRINTCOMMMATRIX_disable
#ifdef PRINTCOMMMATRIX
    // get the communication matrix from routehandle
    std::vector<int> *commMatrixDstPet       =(std::vector<int> *)getStorage(1);
    std::vector<int> *commMatrixDstDataCount =(std::vector<int> *)getStorage(2);
    std::vector<int> *commMatrixSrcPet       =(std::vector<int> *)getStorage(3);
    std::vector<int> *commMatrixSrcDataCount =(std::vector<int> *)getStorage(4);
    
    if (commMatrixDstPet == NULL
      || commMatrixDstDataCount == NULL
      || commMatrixSrcPet == NULL
      || commMatrixSrcDataCount == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc);
      return rc;
    }
  
    // construct the communication matrix as an ESMF Array
    ArraySpec as;
    as.set(2, ESMC_TYPEKIND_I4);
    std::vector<int> minIndexV;
    minIndexV.push_back(1);
    minIndexV.push_back(1);
    InterfaceInt *minIndex = new InterfaceInt(minIndexV);
    std::vector<int> maxIndexV;
    maxIndexV.push_back(petCount);
    maxIndexV.push_back(petCount);
    InterfaceInt *maxIndex = new InterfaceInt(maxIndexV);
    DELayout *delayout = NULL;
    DistGrid *dg = DistGrid::create(minIndex, maxIndex, NULL, NULL, 0, NULL,
      NULL, NULL, NULL, NULL, delayout, NULL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    Array *commArrayPETMap = Array::create(&as, dg,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    Array *commArrayDataCount = Array::create(&as, dg,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
    LocalArray **localarrayList;
    int *entry;

    // fill the Array with comm matrix values
    localarrayList = commArrayPETMap->getLocalarrayList();
    entry = (int *)localarrayList[0]->getBaseAddr();
    for (int i=0; i<petCount; i++)
      entry[i] = 0; // reset
    for (int i=0; i<(*commMatrixDstPet).size(); i++){
      int pet = (*commMatrixDstPet)[i];
      ++entry[pet];
    }
    localrc = commArrayPETMap->setName("src-to-dst-PET-mapping");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // fill the Array with comm matrix values
    localarrayList = commArrayDataCount->getLocalarrayList();
    entry = (int *)localarrayList[0]->getBaseAddr();
    for (int i=0; i<petCount; i++)
      entry[i] = 0; // reset
    for (int i=0; i<(*commMatrixDstPet).size(); i++){
      int pet = (*commMatrixDstPet)[i];
      entry[pet] += (*commMatrixDstDataCount)[i];
    }
    localrc = commArrayDataCount->setName("dataCount");
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
#if 1
    // finally write the Arrays to file -- using Array::write()
    bool overwrite = true;
    localrc = commArrayPETMap->write("commMatrix.nc",
      NULL, &overwrite, NULL, NULL, NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = commArrayDataCount->write("commMatrix.nc",
      NULL, &overwrite, NULL, NULL, NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    
#else
    // finally write the Arrays to file -- using ArrayBundle::write()
    Array *arrayList[2];
    arrayList[0] = commArrayPETMap;
    arrayList[1] = commArrayDataCount;
    ArrayBundle *ab = ArrayBundle::create(arrayList, 2);
    bool singleFile = true;
    bool overwrite = true;
    localrc = ab->write("commMatrix.nc", &singleFile, &overwrite, NULL, NULL,
      NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
#endif
#endif
  
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::RouteHandle::optimize()"
//BOP
// !IROUTINE:  ESMCI::RouteHandle::optimize - optimize for communication pattern
//
// !INTERFACE:
int RouteHandle::optimize(
//
// !RETURN VALUE:
//  int error return code
//
// !ARGUMENTS:
  )const{
//
// !DESCRIPTION:
//  Optimize for the communication pattern stored in the RouteHandle.
//
//EOP
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{
    
    ESMC_LogDefault.Write("Entering RouteHandle::optimize()", ESMC_LOGMSG_INFO,
      ESMC_CONTEXT);

    // get the communication matrix from routehandle
    std::vector<int> *commMatrixDstPet       =(std::vector<int> *)getStorage(1);
    std::vector<int> *commMatrixDstDataCount =(std::vector<int> *)getStorage(2);
    std::vector<int> *commMatrixSrcPet       =(std::vector<int> *)getStorage(3);
    std::vector<int> *commMatrixSrcDataCount =(std::vector<int> *)getStorage(4);
    
    if (commMatrixDstPet == NULL
      || commMatrixDstDataCount == NULL
      || commMatrixSrcPet == NULL
      || commMatrixSrcDataCount == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc);
      return rc;
    }
  
    
        
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
