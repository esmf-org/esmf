// $Id: ESMCI_ArrayBundle_F.C,v 1.12.2.1 2010/02/05 19:53:02 svasquez Exp $
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
#include "ESMCI_RHandle.h"
#include "ESMCI_Array.h"

#include "ESMCI_ArrayBundle.h"

#include "ESMCI_LogErr.h"                  // for LogErr
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
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the ArrayBundle object
    char *cname = ESMC_F90toCstring(name, *len_name);
    if (cname){
      (*ptr)->setName(cname);
      delete [] cname;
    }else if(*len_name){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
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
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::destroy(ptr),
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
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
    ESMC_LogDefault.MsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlerediststore)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle, 
    ESMCI::InterfaceInt **srcToDstTransposeMap, ESMC_TypeKind *typekind,
    void *factor, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlerediststore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::redistStore(
      *srcArraybundle, *dstArraybundle, routehandle, *srcToDstTransposeMap,
      *typekind, factor),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlerediststorenf)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMCI::InterfaceInt **srcToDstTransposeMap, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlerediststorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::redistStore(
      *srcArraybundle, *dstArraybundle, routehandle, *srcToDstTransposeMap),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundleredist)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleredist()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::redist(
      *srcArraybundle, *dstArraybundle, routehandle, *checkflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlesmmstore)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle, 
    ESMC_TypeKind *typekindFactors, void *factorList, int *factorListCount,
    ESMCI::InterfaceInt **factorIndexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmmstore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    
    try{
    
    // check argument consistency
    if (*factorListCount > 0){
      // must provide valid factorList and factorIndexList args
      if (*factorIndexList == NULL){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
          "- Not a valid pointer to factorIndexList array", rc);
        return;
      }
      if ((*factorIndexList)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- factorIndexList array must be of rank 2", rc);
        return;
      }
      if ((*factorIndexList)->extent[0] != 2 && 
        (*factorIndexList)->extent[0] != 4){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of factorIndexList array must be of size 2 or 4",
          rc);
        return;
      }
      if ((*factorIndexList)->extent[1] != *factorListCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dimension of factorIndexList does not match factorListCount",
          rc);
        return;
      }
    }
    // prepare SparseMatrix vector
    vector<ESMCI::SparseMatrix> sparseMatrix;
    int srcN = (*factorIndexList)->extent[0]/2;
    int dstN = (*factorIndexList)->extent[0]/2;
    sparseMatrix.push_back(ESMCI::SparseMatrix(*typekindFactors, factorList,
      *factorListCount, srcN, dstN, (*factorIndexList)->array));
    // Call into the actual C++ method wrapped inside LogErr handling
    if (ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::sparseMatMulStore(
      *srcArraybundle, *dstArraybundle, routehandle, sparseMatrix ),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "- Caught exception", rc);
      return;
    }
  
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_arraybundlesmmstorenf)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmmstorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // prepare empty SparseMatrix vector
    vector<ESMCI::SparseMatrix> sparseMatrix;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::sparseMatMulStore(
      *srcArraybundle, *dstArraybundle, routehandle, sparseMatrix),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundlesmm)(ESMCI::ArrayBundle **srcArraybundle,
    ESMCI::ArrayBundle **dstArraybundle, ESMCI::RouteHandle **routehandle,
    ESMC_RegionFlag *zeroflag, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundlesmm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::ArrayBundle::sparseMatMul(
      *srcArraybundle, *dstArraybundle, routehandle, *zeroflag, *checkflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundleserialize)(ESMCI::ArrayBundle **arraybundle, 
    char *buf, int *length, int *offset, ESMC_AttReconcileFlag *attreconflag,
    ESMC_InquireFlag *inquireflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundleserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*arraybundle)->serialize(
      buf,length,offset,*attreconflag,*inquireflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_arraybundledeserialize)(ESMCI::ArrayBundle **arraybundle,
    char *buf, int *offset, ESMC_AttReconcileFlag *attreconflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraybundledeserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    *arraybundle = new ESMCI::ArrayBundle(-1);  // prevent baseID counter incr.
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*arraybundle)->deserialize(
      buf,offset,*attreconflag),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
#undef  ESMC_METHOD
}
