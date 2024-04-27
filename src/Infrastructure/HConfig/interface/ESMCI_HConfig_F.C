// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
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

  void FTN_X(c_esmc_hconfigcreatei4)(ESMCI::HConfig *ptr, ESMC_I4 *content, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreatei4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(*content, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreatei8)(ESMCI::HConfig *ptr, ESMC_I8 *content, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreatei8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(*content, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreater4)(ESMCI::HConfig *ptr, ESMC_R4 *content, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreater4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(*content, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreater8)(ESMCI::HConfig *ptr, ESMC_R8 *content, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreater8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(*content, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreatei4seq)(ESMCI::HConfig *ptr, ESMC_I4 *content,
    int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreatei4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(content, *count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreatei8seq)(ESMCI::HConfig *ptr, ESMC_I8 *content,
    int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreatei8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(content, *count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreater4seq)(ESMCI::HConfig *ptr, ESMC_R4 *content,
    int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreater4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(content, *count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreater8seq)(ESMCI::HConfig *ptr, ESMC_R8 *content,
    int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreater8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    *ptr = ESMCI::HConfig::create(content, *count, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  // Check for equality
  // Since this is used as equality operator above, there isn't a place to pass
  // a return code, so just pass false for an error instead (as done with
  // equality operators elsewhere in ESMF).
  void FTN_X(c_esmc_hconfigequal)(ESMCI::HConfig *hconfig1,
    ESMCI::HConfig *hconfig2, ESMC_Logical *isEqual){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigequal()"
    // Init return
    *isEqual = ESMF_FALSE;
    // Test for NULL pointers
    if (hconfig1 == NULL) return;
    if (hconfig2 == NULL) return;
    // call into C++
    bool equal = ESMCI::HConfig::equal(hconfig1, hconfig2);
    // Convert to ESMF Logical
    if (equal) *isEqual=ESMF_TRUE;
    else  *isEqual = ESMF_FALSE;
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

  void FTN_X(c_esmc_hconfigfileload)(ESMCI::HConfig *ptr,
    const char *filename, int *doc, int *rc,
    ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigload()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->loadFile(std::string(filename,strLen), doc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigfilesave)(ESMCI::HConfig *ptr,
    const char *filename, int *doc, int *rc, ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigsave()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->saveFile(std::string(filename,strLen),doc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreateatkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *at,
    ESMCI::HConfig *key, int *doc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreateatkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(at, rc)
    ESMCI_NULL_CHECK_PRC(key, rc)
    // call into C++
    *at = ptr->createAtKey(key, doc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreateat)(ESMCI::HConfig *ptr, ESMCI::HConfig *at,
    int *index, int *doc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreateat()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(at, rc)
    // call into C++
    *at = ptr->createAt(index, doc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreateatmapkeykey)(ESMCI::HConfig *ptr, ESMCI::HConfig *at,
    ESMCI::HConfig *key, int *doc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreateatmapkeykey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(at, rc)
    ESMCI_NULL_CHECK_PRC(key, rc)
    // call into C++
    *at = ptr->createAtMapKeyKey(key, doc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreateatmapkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *at,
    int *index, int *doc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreateatmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(at, rc)
    // call into C++
    *at = ptr->createAtMapKey(index, doc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreateatmapvalkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *at,
    ESMCI::HConfig *key, int *doc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreateatmapvalkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(at, rc)
    ESMCI_NULL_CHECK_PRC(key, rc)
    // call into C++
    *at = ptr->createAtMapValKey(key, doc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigcreateatmapval)(ESMCI::HConfig *ptr, ESMCI::HConfig *at,
    int *index, int *doc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigcreateatmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(at, rc)
    // call into C++
    *at = ptr->createAtMapVal(index, doc, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigadd)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigadd()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->add(value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaddkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    ESMCI::HConfig *key, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaddkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->add(key, value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaddmapkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaddmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->addMapKey(value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaddkeymapkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    ESMCI::HConfig *key, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaddkeymapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->addMapKey(key, value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaddmapval)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaddmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->addMapVal(value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaddkeymapval)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    ESMCI::HConfig *key, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaddkeymapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->addMapVal(key, value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggetdoccount)(ESMCI::HConfig *ptr, int *count, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggetdoccount()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    *count = ptr->getDocCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggetsize)(ESMCI::HConfig *ptr, int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggetsize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    *size = ptr->getSize(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggetsizemapkey)(ESMCI::HConfig *ptr, int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggetsizemapkeys()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    *size = ptr->getSizeMapKey(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggetsizemapval)(ESMCI::HConfig *ptr, int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggetsizemapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    *size = ptr->getSizeMapVal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggettaglen)(ESMCI::HConfig *ptr, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggettaglen()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->getTag(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggettag)(ESMCI::HConfig *ptr,
    char *string, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggettag()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->getTag(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
#if (__GNUC__ == 9)
    string_l = value.size();
#endif
    strncpy(string, value.c_str(), string_l);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggettagmapkeylen)(ESMCI::HConfig *ptr, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggettagmapkeylen()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->getTagMapKey(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggettagmapkey)(ESMCI::HConfig *ptr,
    char *string, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggettagmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->getTagMapKey(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
#if (__GNUC__ == 9)
    string_l = value.size();
#endif
    strncpy(string, value.c_str(), string_l);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggettagmapvallen)(ESMCI::HConfig *ptr, int *len, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggettagmapvallen()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->getTagMapVal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiggettagmapval)(ESMCI::HConfig *ptr,
    char *string, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiggettagmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    std::string value = ptr->getTagMapVal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
#if (__GNUC__ == 9)
    string_l = value.size();
#endif
    strncpy(string, value.c_str(), string_l);
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

  void FTN_X(c_esmc_hconfigisnullmapkey)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisnullmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isNullMapKey(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisscalarmapkey)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisscalarmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isScalarMapKey(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigissequencemapkey)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigissequencemapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isSequenceMapKey(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigismapmapkey)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigismapmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isMapMapKey(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisdefinedmapkey)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisdefinedmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isDefinedMapKey(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisnullmapval)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisnullmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isNullMapVal(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisscalarmapval)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisscalarmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isScalarMapVal(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigissequencemapval)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigissequencemapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isSequenceMapVal(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigismapmapval)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigismapmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isMapMapVal(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisdefinedmapval)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisdefinedmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isDefinedMapVal(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisiterator)(ESMCI::HConfig *ptr, ESMC_Logical *flag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisiterator()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isIterator(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigisseqiterator)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigisseqiterator()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isSequenceIterator(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigismapiterator)(ESMCI::HConfig *ptr,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigismapiterator()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool _flag;
    localrc = ptr->isMapIterator(&_flag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (_flag) *flag = ESMF_TRUE;
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

  void FTN_X(c_esmc_hconfigiterbeginmapkey)(ESMCI::HConfig *ptr,
    ESMCI::HConfig *iter, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiterbeginmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(iter, rc)
    // call into C++
    *iter = ptr->iterBeginMapKey(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigiterendmapkey)(ESMCI::HConfig *ptr,
    ESMCI::HConfig *iter, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiterendmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(iter, rc)
    // call into C++
    *iter = ptr->iterEndMapKey(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigiterbeginmapval)(ESMCI::HConfig *ptr,
    ESMCI::HConfig *iter, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiterbeginmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(iter, rc)
    // call into C++
    *iter = ptr->iterBeginMapVal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigiterendmapval)(ESMCI::HConfig *ptr,
    ESMCI::HConfig *iter, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigiterendmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(iter, rc)
    // call into C++
    *iter = ptr->iterEndMapVal(&localrc);
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

  void FTN_X(c_esmc_hconfigasstringlen)(ESMCI::HConfig *ptr, int *len,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstringlen()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    std::string value = ptr->as<std::string>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstring)(ESMCI::HConfig *ptr,
    char *string, ESMC_Logical *flag, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstring()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    std::string value = ptr->as<std::string>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
#if (__GNUC__ == 9)
    string_l = value.size();
#endif
    strncpy(string, value.c_str(), string_l);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstringmapkeylen)(ESMCI::HConfig *ptr, int *len,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstringmapkeylen()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    std::string value = ptr->asMapKey<std::string>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstringmapkey)(ESMCI::HConfig *ptr,
    char *string, ESMC_Logical *flag, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstringmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    std::string value = ptr->asMapKey<std::string>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
#if (__GNUC__ == 9)
    string_l = value.size();
#endif
    strncpy(string, value.c_str(), string_l);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstringmapvallen)(ESMCI::HConfig *ptr, int *len,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstringmapvallen()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    std::string value = ptr->asMapVal<std::string>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    *len = value.size();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasstringmapval)(ESMCI::HConfig *ptr,
    char *string, ESMC_Logical *flag, int *rc, ESMCI_FortranStrLenArg string_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasstringmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    std::string value = ptr->asMapVal<std::string>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
#if (__GNUC__ == 9)
    string_l = value.size();
#endif
    strncpy(string, value.c_str(), string_l);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaslogical)(ESMCI::HConfig *ptr, ESMC_Logical *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaslogical()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *value = ESMF_FALSE;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    bool _value = ptr->as<bool>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    if (_value) *value = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaslogicalmapkey)(ESMCI::HConfig *ptr,
    ESMC_Logical *value, ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaslogicalmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *value = ESMF_FALSE;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    bool _value = ptr->asMapKey<bool>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    if (_value) *value = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigaslogicalmapval)(ESMCI::HConfig *ptr,
    ESMC_Logical *value, ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigaslogicalmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *value = ESMF_FALSE;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    bool _value = ptr->asMapVal<bool>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    if (_value) *value = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasi4)(ESMCI::HConfig *ptr, ESMC_I4 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasi4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->as<ESMC_I4>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasi4mapkey)(ESMCI::HConfig *ptr, ESMC_I4 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasi4mapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapKey<ESMC_I4>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasi4mapval)(ESMCI::HConfig *ptr, ESMC_I4 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasi4mapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapVal<ESMC_I4>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasi8)(ESMCI::HConfig *ptr, ESMC_I8 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasi8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->as<ESMC_I8>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasi8mapkey)(ESMCI::HConfig *ptr, ESMC_I8 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasi8mapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapKey<ESMC_I8>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasi8mapval)(ESMCI::HConfig *ptr, ESMC_I8 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasi8mapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapVal<ESMC_I8>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasr4)(ESMCI::HConfig *ptr, ESMC_R4 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasr4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->as<ESMC_R4>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasr4mapkey)(ESMCI::HConfig *ptr, ESMC_R4 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasr4mapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapKey<ESMC_R4>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasr4mapval)(ESMCI::HConfig *ptr, ESMC_R4 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasr4mapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapVal<ESMC_R4>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasr8)(ESMCI::HConfig *ptr, ESMC_R8 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasr8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->as<ESMC_R8>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasr8mapkey)(ESMCI::HConfig *ptr, ESMC_R8 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasr8mapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapKey<ESMC_R8>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigasr8mapval)(ESMCI::HConfig *ptr, ESMC_R8 *value,
    ESMC_Logical *flag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigasr8mapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *flag = ESMF_FALSE;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    bool asOkay;
    *value = ptr->asMapVal<ESMC_R8>(&asOkay, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (asOkay) *flag = ESMF_TRUE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigset)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigset()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->set(value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigsetmapkey)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigsetmapkey()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->setMapKey(value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigsetmapval)(ESMCI::HConfig *ptr, ESMCI::HConfig *value,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigsetmapval()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(value, rc)
    // call into C++
    localrc = ptr->setMapVal(value);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigremoveindex)(ESMCI::HConfig *ptr, int *index,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigremoveindex()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->remove(*index);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfigremovekeystring)(ESMCI::HConfig *ptr,
    const char *keyString, int *rc, ESMCI_FortranStrLenArg strLen){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfigremoveindex()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // call into C++
    localrc = ptr->remove(std::string(keyString,strLen));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_hconfiglog)(ESMCI::HConfig *ptr,
    char *prefix, ESMC_LogMsgType_Flag *logMsgFlag, int *doc, int *rc,
    ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_hconfiglog()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
      ptr->log(prefixStr, *logMsgFlag, doc);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
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

