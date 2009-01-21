// $Id: ESMCI_ArrayBundle_F.C,v 1.1.2.8 2009/01/21 21:25:19 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_ArrayBundle_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <string.h>

#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_RHandle.h"
#include "ESMCI_Array.h"

#include "ESMCI_ArrayBundle.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ArrayBundle} class functions.
//
//EOP
//-------------------------------------------------------------------------

// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:

  void FTN(c_esmc_arraybundlecreate)(ESMCI::ArrayBundle **ptr, 
    ESMCI::Array **arrayList, int *arrayCount, char *name, int *len_name,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlecreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::ArrayBundle::create(arrayList, *arrayCount, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the ArrayBundle object
    char *cname = ESMC_F90toCstring(name, *len_name);
    if (cname){
      (*ptr)->setName(cname);
      delete [] cname;
    }else if(*len_name){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid string", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }
  
  void FTN(c_esmc_arraybundledestroy)(ESMCI::ArrayBundle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundledestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundleget)(ESMCI::ArrayBundle **ptr, int *arrayCount,  
    ESMCI::Array **opt_arrayList, int *len_arrayList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values
    if (ESMC_NOT_PRESENT_FILTER(arrayCount) != ESMC_NULL_POINTER)
      *arrayCount = (*ptr)->getArrayCount();
    // fill arrayList
    if (*len_arrayList != 0){
      // opt_arrayList was provided
      if (*len_arrayList < (*ptr)->getArrayCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- opt_arrayList must provide arrayCount elements", rc);
        return;
      }
      // opt_arrayList has correct number of elements
      for (int i=0; i<(*ptr)->getArrayCount(); i++)
        opt_arrayList[i] = ((*ptr)->getArrayList())[i];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
 
  void FTN(c_esmc_arraybundleprint)(ESMCI::ArrayBundle **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlerediststore)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMC_RouteHandle **routehandle, 
    ESMCI::InterfaceInt **srcToDstTransposeMap, ESMC_TypeKind *typekind,
    void *factor, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlerediststore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::redistStore(
      *srcArraybundle, *dstArraybundle, routehandle, *srcToDstTransposeMap,
      *typekind, factor),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlerediststorenf)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMC_RouteHandle **routehandle,
    ESMCI::InterfaceInt **srcToDstTransposeMap, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlerediststorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::redistStore(
      *srcArraybundle, *dstArraybundle, routehandle, *srcToDstTransposeMap),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundleredist)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMC_RouteHandle **routehandle,
    ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleredist()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::redist(
      *srcArraybundle, *dstArraybundle, routehandle, *checkflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlesmmstore)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMC_RouteHandle **routehandle, 
    ESMC_TypeKind *typekind, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmmstore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::sparseMatMulStore(
      *srcArraybundle, *dstArraybundle, routehandle, *typekind, factorList,
      *factorListCount, *factorIndexList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlesmmstorenf)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMC_RouteHandle **routehandle,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmmstorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::sparseMatMulStore(
      *srcArraybundle, *dstArraybundle, routehandle),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlesmm)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMC_RouteHandle **routehandle,
    ESMC_RegionFlag *zeroflag, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::ArrayBundle::sparseMatMul(
      *srcArraybundle, *dstArraybundle, routehandle, *zeroflag, *checkflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundleserialize)(ESMCI::ArrayBundle **arraybundle, 
    char *buf, int *length, int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*arraybundle)->serialize(
      buf, length, offset),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundledeserialize)(ESMCI::ArrayBundle **arraybundle,
    char *buf, int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundledeserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    *arraybundle = new ESMCI::ArrayBundle;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*arraybundle)->deserialize(
      buf, offset),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
#undef  ESMC_METHOD
}
