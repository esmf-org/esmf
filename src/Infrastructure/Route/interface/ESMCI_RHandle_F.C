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
#define ESMC_FILENAME "ESMCI_RHandle_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_Macros.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_DELayout.h"
#include "ESMCI_LogErr.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt RouteHandle} class functions.
//
//EOP
//------------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case
extern "C" {

  void FTN_X(c_esmc_routehandlecreate)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandlecreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::RouteHandle::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandledestroy)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandledestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = ESMCI::RouteHandle::destroy(*ptr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandleprepxxe)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandleprepxxe()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // set routehandle type for XXE comms
    localrc = (*ptr)->setType(ESMCI::ESMC_ARRAYBUNDLEXXE);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // allocate XXE and attach to RouteHandle
    ESMCI::VM *vm = ESMCI::VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    ESMCI::XXE *xxe;
    try{
      xxe = new ESMCI::XXE(vm, 100, 10, 1000);
    }catch (...){
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    localrc = (*ptr)->setStorage(xxe);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
       ESMC_NOT_PRESENT_FILTER(rc))) return;
    
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandleappendclear)(ESMCI::RouteHandle **ptr, 
    ESMCI::RouteHandle **rh, int *rraShift, int *vectorLengthShift, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandleappendclear()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // get a handle on the XXE stored in rh
    ESMCI::XXE *xxeSub = (ESMCI::XXE *)(*rh)->getStorage();
    // delete the temporary routehandle w/o deleting the xxeSub
    localrc = (*rh)->setType(ESMCI::ESMC_UNINITIALIZEDHANDLE);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    localrc = ESMCI::RouteHandle::destroy(*rh);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // append the xxeSub to the xxe object with RRA offset info
    ESMCI::XXE *xxe = (ESMCI::XXE *)(*ptr)->getStorage();
    localrc = xxe->appendXxeSub(0x0, xxeSub, *rraShift, *vectorLengthShift);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // keep track of xxeSub for xxe garbage collection
    localrc = xxe->storeXxeSub(xxeSub);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandlegettype)(ESMCI::RouteHandle **ptr, int *htype,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandlegettype()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    *(ESMCI::RouteHandleType *)htype = (*ptr)->getType();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandlesettype)(ESMCI::RouteHandle **ptr, int *htype,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandlesettype()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->setType(*(ESMCI::RouteHandleType *)htype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandlevalidate)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandlevalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->validate();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandleprint)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandleprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->print();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    fflush (stdout);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_routehandleoptimize)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandleoptimize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->optimize();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

};


