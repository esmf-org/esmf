// $Id: ESMCI_RHandle_F.C,v 1.2.2.1 2010/02/05 19:59:58 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMC_LogMacros.inc"             // for LogErr
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

  void FTN(c_esmc_routehandlecreate)(ESMCI::RouteHandle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_routehandlecreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::RouteHandle::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_routehandledestroy)(ESMCI::RouteHandle **ptr, int *rc){
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = ESMCI::RouteHandle::destroy(*ptr);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_routehandlegettype)(ESMCI::RouteHandle **ptr, int *htype,
    int *rc){
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    *(ESMCI::RouteHandleType *)htype = (*ptr)->getType();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_routehandlesettype)(ESMCI::RouteHandle **ptr, int *htype,
    int *rc){
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->setType(*(ESMCI::RouteHandleType *)htype);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_routehandlevalidate)(ESMCI::RouteHandle **ptr, char *opts,
    int *rc){
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->validate(opts);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_routehandleprint)(ESMCI::RouteHandle **ptr, char *opts, 
    int *rc){
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->print(opts);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

};


