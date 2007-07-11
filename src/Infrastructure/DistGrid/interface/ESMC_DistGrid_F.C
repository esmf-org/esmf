// $Id: ESMC_DistGrid_F.C,v 1.20 2007/07/11 19:08:57 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_DistGrid_F.C"
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
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"

#include "ESMC_DistGrid.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt DistGrid} class functions.
//
//EOP
//-------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN(c_esmc_distgridcreaterd)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount, 
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterd()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreatedb)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **deBlockList,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedb()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *deBlockList,
      *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreaterdfa)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount, 
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    int *fastAxis, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdfa()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *deLabelList,
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, *fastAxis, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridcreaterdp)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount1, int *decompflagCount2,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransformList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdp()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount1, *decompflagCount2, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransformList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgriddestroy)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddestroy()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::DistGrid::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridget)(ESMCI::DistGrid **ptr,
    ESMCI::DELayout **delayout, int *patchCount,
    ESMCI::InterfaceInt **patchList, int *dimCount,
    ESMCI::InterfaceInt **dimExtent, ESMC_Logical *regDecompFlag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // fill simple return values
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDELayout();
    if (ESMC_NOT_PRESENT_FILTER(patchCount) != ESMC_NULL_POINTER)
      *patchCount = (*ptr)->getPatchCount();
    if (ESMC_NOT_PRESENT_FILTER(dimCount) != ESMC_NULL_POINTER)
      *dimCount = (*ptr)->getDimCount();
    if (ESMC_NOT_PRESENT_FILTER(regDecompFlag) != ESMC_NULL_POINTER)
      *regDecompFlag = (*ptr)->getRegDecompFlag();
    // fill dimExtent
    if (*dimExtent != NULL){
      // dimExtent was provided -> do some error checking
      if ((*dimExtent)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- dimExtent array must be of rank 2", rc);
        return;
      }
      if ((*dimExtent)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of dimExtent array must be of size 'dimCount'", rc);
        return;
      }
      if ((*dimExtent)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of dimExtent array must be of size 'deCount'", rc);
        return;
      }
      // fill in the values: The interface allows to pass in dimExtent arrays
      // which are larger than dimCount x deCount. Consequently it is necessary
      // to memcpy strips of contiguous data since it cannot be assumed that
      // all data ends up contiguous in the dimExtent array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(&((*dimExtent)->array[i*((*dimExtent)->extent[0])]),
          &(((*ptr)->getDimExtent())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill patchList
    if (*patchList != NULL){
      // patchList was provided -> do some error checking
      if ((*patchList)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- patchList array must be of rank 1", rc);
        return;
      }
      if ((*patchList)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of patchList array must be of size 'deCount'", rc);
        return;
      }
      // fill in values
      memcpy((*patchList)->array, (*ptr)->getDePatchList(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_distgridgetpdepdim)(ESMCI::DistGrid **ptr, int *localDeArg,
    int *dimArg, ESMCI::InterfaceInt **localIndexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetpdepdim()"
    int localrc = ESMC_RC_NOT_IMPL;
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // shift input indices to base zero
    int localDe = *localDeArg - 1;
    int dim = *dimArg - 1;
    // fill indexList
    if (*localIndexList != NULL){
      // indexList provided -> get localIndexListPtr & do some error checking
      const int *localIndexListPtr =
        (*ptr)->getLocalIndexList(localDe, dim+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
      if ((*localIndexList)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- localIndexList array must be of rank 1", rc);
        return;
      }
      if ((*localIndexList)->extent[0] <
        ((*ptr)->getDimExtent())[(*ptr)->getDELayout()->
        getLocalDeList()[localDe] * (*ptr)->getDimCount()+dim]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of localIndexList array size insufficiently", rc);
        return;
      }
      // fill in the values
      memcpy((*localIndexList)->array, localIndexListPtr,
        sizeof(int) * (*ptr)->getDimExtent()[((*ptr)->getDELayout()->
        getLocalDeList()[localDe] * (*ptr)->getDimCount()+dim)]);
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_distgridprint)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridvalidate)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridvalidate()"
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->validate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_connection)(
    ESMCI::InterfaceInt **connection, int *patchIndexA,
    int *patchIndexB, ESMCI::InterfaceInt **positionVector,
    ESMCI::InterfaceInt **orientationVector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connection()"
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMCI::DistGrid::connection(*connection, *patchIndexA,
      *patchIndexB, *positionVector, *orientationVector), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgrid_storeabidx)(
    ESMCI::DistGrid **dptr,
    ESMCI::InterfaceInt **abidx, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgrid_storeabidx()"
    
    if (rc!=NULL)
      *rc = ESMC_RC_NOT_IMPL;
      
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      (*dptr)->setArbIdx(*abidx),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
#undef  ESMC_METHOD


}

