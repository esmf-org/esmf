// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research, 
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
#include <string>
#include <iostream>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_HConfig.h"
#include "ESMCI_LogErr.h"

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
        
  void FTN_X(c_esmc_hconfigcreate)(ESMCI::HConfig *ptr, int *rc){
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

  void FTN_X(c_esmc_hconfigdestroy)(ESMCI::HConfig *ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigdestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMC_LogDefault.MsgFoundError(ESMCI::HConfig::destroy(ptr),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, 
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_hconfigload)(ESMCI::HConfig *ptr,
    const char *content, int *rc, ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigload()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->load(std::string(content,strLen));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigloadfile)(ESMCI::HConfig *ptr,
    const char *filename, int *rc, ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigload()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->loadFile(std::string(filename,strLen));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisnull)(ESMCI::HConfig *ptr, ESMC_Logical *flag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisnull()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isNull(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisscalar)(ESMCI::HConfig *ptr, ESMC_Logical *flag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisscalar()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isScalar(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigissequence)(ESMCI::HConfig *ptr, ESMC_Logical *flag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigissequence()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isSequence(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigismap)(ESMCI::HConfig *ptr, ESMC_Logical *flag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigismap()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isMap(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisdefined)(ESMCI::HConfig *ptr, ESMC_Logical *flag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisdefined()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isDefined(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstringlen)(ESMCI::HConfig *ptr, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstring()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->asString(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstring)(ESMCI::HConfig *ptr,
    char *string, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstring()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->asString(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    strncpy(string, value.c_str(), string_l);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigiterbegin)(ESMCI::HConfig *ptr,
    ESMCI::HConfig *iter, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiterbegin()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(iter, rc)
    // call into C++
    *iter = ptr->iterBegin(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigiterend)(ESMCI::HConfig *ptr,
    ESMCI::HConfig *iter, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiterend()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(iter, rc)
    // call into C++
    *iter = ptr->iterEnd(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigiternext)(ESMCI::HConfig *ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiternext()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->iterNext();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigtoconfig)(ESMCI::HConfig *ptr, ESMCI_Config **ptr2,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigtoconfig()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(ptr2, rc)
    // call into C++
    localrc = ptr->toConfig(ptr2);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
}

