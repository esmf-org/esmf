// $Id: ESMCI_DELayout_F.C,v 1.1.2.4 2009/01/21 21:25:20 cdeluca Exp $
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
#define ESMC_FILENAME "ESMCI_DELayout_F.C"
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

#include "ESMCI_DELayout.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt DELayout} class functions.
//
//EOP
//-------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case
extern "C" {

  // - ESMF-public methods:

  void FTN(c_esmc_delayoutcreatefrompetmap)(ESMCI::DELayout **ptr, int *petMap, 
    int *petMapCount, ESMC_DePinFlag *dePinFlag, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatefrompetmap()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DELayout::create(petMap, *petMapCount,
      ESMC_NOT_PRESENT_FILTER(dePinFlag), 
      opt_vm, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutcreatedefault)(ESMCI::DELayout **ptr, int *deCount,
    ESMCI::InterfaceInt **deGrouping, ESMC_DePinFlag *dePinFlag, 
    ESMCI::InterfaceInt **petList, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatedefault()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DELayout::create(
      ESMC_NOT_PRESENT_FILTER(deCount), 
      *deGrouping, 
      ESMC_NOT_PRESENT_FILTER(dePinFlag),
      *petList, opt_vm, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutcreatend)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *deCountList, int *deCountListCount, int *petList, int *petListCount,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatend()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMC_Logical cyclic = ESMF_TRUE;  // TODO: fix API
    // call into C++
    *ptr = ESMCI::DELayout::create(**vm, deCountList, *deCountListCount,
      petList, *petListCount, &cyclic, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutdestroy)(ESMCI::DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::DELayout::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutget)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *deCount, ESMCI::InterfaceInt **petMap, ESMCI::InterfaceInt **vasMap,
    ESMC_Logical *oneToOneFlag, ESMC_DePinFlag *dePinFlag,
    int *localDeCount, ESMCI::InterfaceInt **localDeList,
    int *vasLocalDeCount, ESMCI::InterfaceInt **vasLocalDeList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(vm) != ESMC_NULL_POINTER)
      *vm = (*ptr)->getVM();
    if (ESMC_NOT_PRESENT_FILTER(deCount) != ESMC_NULL_POINTER)
      *deCount = (*ptr)->getDeCount();
    if (*petMap != NULL){
      // petMap was provided -> do some error checking
      if ((*petMap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- petMap array must be of rank 1", rc);
        return;
      }
      if ((*petMap)->extent[0] < (*ptr)->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of petMap array must be of size 'deCount'",
          rc);
        return;
      }
      // fill in values
      for (int i=0; i<(*ptr)->getDeCount(); i++)
        ((*petMap)->array)[i] = (*ptr)->getPet(i);
    }
    if (*vasMap != NULL){
      // vasMap was provided -> do some error checking
      if ((*vasMap)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- vasMap array must be of rank 1", rc);
        return;
      }
      if ((*vasMap)->extent[0] < (*ptr)->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of vasMap array must be of size 'deCount'",
          rc);
        return;
      }
      // fill in values
      for (int i=0; i<(*ptr)->getDeCount(); i++)
        ((*vasMap)->array)[i] = (*ptr)->getVas(i);
    }
    if (ESMC_NOT_PRESENT_FILTER(oneToOneFlag) != ESMC_NULL_POINTER)
      *oneToOneFlag = (*ptr)->getOneToOneFlag();
    if (ESMC_NOT_PRESENT_FILTER(dePinFlag) != ESMC_NULL_POINTER)
      *dePinFlag = (*ptr)->getDePinFlag();
    if (ESMC_NOT_PRESENT_FILTER(localDeCount) != ESMC_NULL_POINTER)
      *localDeCount = (*ptr)->getLocalDeCount();
    if (*localDeList != NULL){
      // localDeList was provided -> do some error checking
      if ((*localDeList)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- localDeList array must be of rank 1", rc);
        return;
      }
      if ((*localDeList)->extent[0] < (*ptr)->getLocalDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of localDeList array must be of size 'localDeCount'",
          rc);
        return;
      }
      // fill in values
      memcpy((*localDeList)->array, (*ptr)->getLocalDeList(),
        sizeof(int)*(*ptr)->getLocalDeCount());
    }
    if (ESMC_NOT_PRESENT_FILTER(vasLocalDeCount) != ESMC_NULL_POINTER)
      *vasLocalDeCount = (*ptr)->getVasLocalDeCount();
    if (*vasLocalDeList != NULL){
      // vasLocalDeList was provided -> do some error checking
      if ((*vasLocalDeList)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- vasLocalDeList array must be of rank 1", rc);
        return;
      }
      if ((*vasLocalDeList)->extent[0] < (*ptr)->getVasLocalDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of vasLocalDeList array must be of size 'vasLocalDeCount'",
          rc);
        return;
      }
      // fill in values
      memcpy((*vasLocalDeList)->array, (*ptr)->getVasLocalDeList(),
        sizeof(int)*(*ptr)->getVasLocalDeCount());
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutgetdematchde)(ESMCI::DELayout **ptr,
    int *DEid, ESMCI::DELayout **ptrMatch, int *deMatchCount, int *deMatchList,
    int *len_deMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematchde()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->getDEMatchDE(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(deMatchCount),
      deMatchList, *len_deMatchList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutgetdematchpet)(ESMCI::DELayout **ptr,
    int *DEid, ESMCI::VM **ptrMatch, int *petMatchCount, int *petMatchList,
    int *len_petMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematchpet()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->getDEMatchPET(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(petMatchCount),
      petMatchList, *len_petMatchList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutgetdeprecated)(ESMCI::DELayout **ptr,
    int *deCount, int *dimCount, int *localDeCount, int *localDeList,
    int *len_localDeList, int *localDe, ESMC_Logical *oneToOneFlag, 
    ESMC_Logical *logRectFlag, int *deCountPerDim, int *len_deCountPerDim,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdeprecated()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->getDeprecated(
      ESMC_NOT_PRESENT_FILTER(deCount), 
      ESMC_NOT_PRESENT_FILTER(dimCount), 
      ESMC_NOT_PRESENT_FILTER(localDeCount),
      localDeList, *len_localDeList, 
      ESMC_NOT_PRESENT_FILTER(localDe), 
      ESMC_NOT_PRESENT_FILTER(oneToOneFlag), 
      ESMC_NOT_PRESENT_FILTER(logRectFlag),
      deCountPerDim, *len_deCountPerDim),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutgetdelocalinfo)(ESMCI::DELayout **ptr,
    int *DEid, int *DEcoord, int *len_coord, int *DEcde, int *len_cde, 
    int *DEcw, int *len_cw, int *nDEc, int *pid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdelocalinfo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->getDELocalInfo(
      *DEid, DEcoord, *len_coord, DEcde, *len_cde, DEcw, *len_cw,
      ESMC_NOT_PRESENT_FILTER(nDEc),
      ESMC_NOT_PRESENT_FILTER(pid)), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutprint)(ESMCI::DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_delayoutvalidate)(ESMCI::DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->validate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_delayoutserviceoffer)(ESMCI::DELayout **ptr, int *de,
    ESMCI::DELayoutServiceReply *reply, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserviceoffer()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *reply = (*ptr)->serviceOffer(*de, &localrc);
//TODO: enable LogErr once it is thread-safe
    *rc=localrc;  
//    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
//      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_delayoutservicecomplete)(ESMCI::DELayout **ptr, int *de,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutservicecomplete()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->serviceComplete(*de),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_delayoutserialize)(ESMCI::DELayout **delayout, char *buf,
    int *length, int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserialize"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      (*delayout)->serialize(buf, length, offset),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_delayoutdeserialize)(ESMCI::DELayout **delayout, char *buf,
    int *offset, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdeserialize"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    (*delayout) = ESMCI::DELayout::deserialize(buf, offset);
    if (*delayout == NULL) 
      localrc = ESMC_RC_INTNRL_BAD;
    else
      localrc = ESMF_SUCCESS;
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
}
