// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_HConfig_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <cstring>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_HConfig.h"
#include "ESMCI_LogErr.h"

using namespace std;

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt HConfig} class functions.
//
//EOP
//-------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN_X(c_esmc_hconfigcreate)(ESMCI::HConfig **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigdestroy)(ESMCI::HConfig **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigdestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError(ESMCI::HConfig::destroy(ptr),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, 
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_hconfigload)(ESMCI::HConfig **ptr,
    const char *content, int *rc, ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigload()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // call into C++
    localrc = (*ptr)->load(std::string(content,strLen));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigloadfile)(ESMCI::HConfig **ptr,
    const char *filename, int *rc, ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigload()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // call into C++
    localrc = (*ptr)->loadFile(std::string(filename,strLen));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigtoconfig)(ESMCI::HConfig **ptr, ESMCI_Config **ptr2,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigtoconfig()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // call into C++
    localrc = (*ptr)->toConfig(*ptr2);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
}

