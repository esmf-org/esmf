// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
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

#include <cstring>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"
#include "ESMCI_LogErr.h"

using namespace std;

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

  void FTN_X(c_esmc_delayoutcreatefrompetmap)(ESMCI::DELayout **ptr, int *petMap, 
    int *petMapCount, ESMC_Pin_Flag *pinFlag, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatefrompetmap()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    bool actualFlag = true;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      opt_vm = NULL;
    else{
      opt_vm = *vm;
      if (opt_vm == NULL)
        actualFlag = false; // not an actual member because VM present but NULL
    }
#if 0
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      printf("VM NOT PRESENT: opt_vm=%p, actualFlag=%d\n", opt_vm, actualFlag);
    else
      printf("VM is present: opt_vm=%p, actualFlag=%d\n", opt_vm, actualFlag);
#endif
    if (actualFlag){
      // on PETs with actual members call into C++
      // test for NULL pointer via macro before calling any class methods
      ESMCI_NULL_CHECK_PRC(ptr, rc)
      *ptr = ESMCI::DELayout::create(petMap, *petMapCount,
        ESMC_NOT_PRESENT_FILTER(pinFlag), 
        opt_vm, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutcreatedefault)(ESMCI::DELayout **ptr, int *deCount,
    ESMCI::InterArray<int> *deGrouping, ESMC_Pin_Flag *pinFlag, 
    ESMCI::InterArray<int> *petList, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatedefault()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    bool actualFlag = true;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      opt_vm = NULL;
    else{
      opt_vm = *vm;
      if (opt_vm == NULL)
        actualFlag = false; // not an actual member because VM present but NULL
    }
#if 0
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      printf("VM NOT PRESENT: opt_vm=%p, actualFlag=%d\n", opt_vm, actualFlag);
    else
      printf("VM is present: opt_vm=%p, actualFlag=%d\n", opt_vm, actualFlag);
#endif
    if (actualFlag){
      // on PETs with actual members call into C++
      // test for NULL pointer via macro before calling any class methods
      ESMCI_NULL_CHECK_PRC(ptr, rc)
      *ptr = ESMCI::DELayout::create(
        ESMC_NOT_PRESENT_FILTER(deCount), 
        deGrouping,
        ESMC_NOT_PRESENT_FILTER(pinFlag),
        petList, opt_vm, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutcreatend)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *deCountList, int *deCountListCount, int *petList, int *petListCount,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatend()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMC_Logical cyclic = ESMF_TRUE;  // TODO: fix API
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    *ptr = ESMCI::DELayout::create(**vm, deCountList, *deCountListCount,
      petList, *petListCount, &cyclic, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutdestroy)(ESMCI::DELayout **ptr, 
    ESMC_Logical *noGarbage, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool noGarbageOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(noGarbage) != ESMC_NULL_POINTER)
      if (*noGarbage == ESMF_TRUE) noGarbageOpt = true;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError(ESMCI::DELayout::destroy(ptr, 
      noGarbageOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutget)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *deCount, ESMCI::InterArray<int> *petMap,
    ESMCI::InterArray<int> *vasMap, ESMC_Logical *oneToOneFlag,
    ESMC_Pin_Flag *pinFlag, int *localDeCount, 
    ESMCI::InterArray<int> *localDeToDeMap, int *vasLocalDeCount,
    ESMCI::InterArray<int> *vasLocalDeToDeMap, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(vm) != ESMC_NULL_POINTER)
      *vm = (*ptr)->getVM();
    if (ESMC_NOT_PRESENT_FILTER(deCount) != ESMC_NULL_POINTER)
      *deCount = (*ptr)->getDeCount();
    if (present(petMap)){
      // petMap was provided -> do some error checking
      if ((petMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "petMap array must be of rank 1", ESMC_CONTEXT, rc);
        return; // bail out
      }
      if ((petMap)->extent[0] < (*ptr)->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of petMap array must be of size 'deCount'",
          ESMC_CONTEXT, rc);
        return; // bail out
      }
      // fill in values
      for (int i=0; i<(*ptr)->getDeCount(); i++)
        ((petMap)->array)[i] = (*ptr)->getPet(i);
    }
    if (present(vasMap)){
      // vasMap was provided -> do some error checking
      if ((vasMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "vasMap array must be of rank 1", ESMC_CONTEXT, rc);
        return; // bail out
      }
      if ((vasMap)->extent[0] < (*ptr)->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of vasMap array must be of size 'deCount'", ESMC_CONTEXT,
          rc);
        return; // bail out
      }
      // fill in values
      for (int i=0; i<(*ptr)->getDeCount(); i++)
        ((vasMap)->array)[i] = (*ptr)->getVas(i);
    }
    if (ESMC_NOT_PRESENT_FILTER(oneToOneFlag) != ESMC_NULL_POINTER)
      *oneToOneFlag = (*ptr)->getOneToOneFlag();
    if (ESMC_NOT_PRESENT_FILTER(pinFlag) != ESMC_NULL_POINTER)
      *pinFlag = (*ptr)->getPinFlag();
    if (ESMC_NOT_PRESENT_FILTER(localDeCount) != ESMC_NULL_POINTER)
      *localDeCount = (*ptr)->getLocalDeCount();
    if (present(localDeToDeMap)){
      // localDeToDeMap was provided -> do some error checking
      if ((localDeToDeMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "localDeToDeMap array must be of rank 1", ESMC_CONTEXT, rc);
        return; // bail out
      }
      if ((localDeToDeMap)->extent[0] < (*ptr)->getLocalDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of localDeToDeMap array must be of size 'localDeCount'",
          ESMC_CONTEXT, rc);
        return; // bail out
      }
      // fill in values
      memcpy((localDeToDeMap)->array, (*ptr)->getLocalDeToDeMap(),
        sizeof(int)*(*ptr)->getLocalDeCount());
    }
    if (ESMC_NOT_PRESENT_FILTER(vasLocalDeCount) != ESMC_NULL_POINTER)
      *vasLocalDeCount = (*ptr)->getVasLocalDeCount();
    if (present(vasLocalDeToDeMap)){
      // vasLocalDeToDeMap was provided -> do some error checking
      if ((vasLocalDeToDeMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "vasLocalDeToDeMap array must be of rank 1", ESMC_CONTEXT, rc);
        return; // bail out
      }
      if ((vasLocalDeToDeMap)->extent[0] < (*ptr)->getVasLocalDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of vasLocalDeToDeMap array must be of size "
          "'vasLocalDeCount'", ESMC_CONTEXT, rc);
        return; // bail out
      }
      // fill in values
      memcpy((vasLocalDeToDeMap)->array, (*ptr)->getVasLocalDeToDeMap(),
        sizeof(int)*(*ptr)->getVasLocalDeCount());
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutgetdematchde)(ESMCI::DELayout **ptr,
    int *DEid, ESMCI::DELayout **ptrMatch, int *deMatchCount, int *deMatchList,
    int *len_deMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematchde()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->getDEMatchDE(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(deMatchCount),
      deMatchList, *len_deMatchList),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutgetdematchpet)(ESMCI::DELayout **ptr,
    int *DEid, ESMCI::VM **ptrMatch, int *petMatchCount, int *petMatchList,
    int *len_petMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematchpet()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->getDEMatchPET(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(petMatchCount),
      petMatchList, *len_petMatchList),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutgetdeprecated)(ESMCI::DELayout **ptr,
    int *deCount, int *dimCount, int *localDeCount, int *localDeToDeMap,
    int *len_localDeToDeMap, int *localDe, ESMC_Logical *oneToOneFlag, 
    ESMC_Logical *logRectFlag, int *deCountPerDim, int *len_deCountPerDim,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdeprecated()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->getDeprecated(
      ESMC_NOT_PRESENT_FILTER(deCount), 
      ESMC_NOT_PRESENT_FILTER(dimCount), 
      ESMC_NOT_PRESENT_FILTER(localDeCount),
      localDeToDeMap, *len_localDeToDeMap, 
      ESMC_NOT_PRESENT_FILTER(localDe), 
      ESMC_NOT_PRESENT_FILTER(oneToOneFlag), 
      ESMC_NOT_PRESENT_FILTER(logRectFlag),
      deCountPerDim, *len_deCountPerDim),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutgetdelocalinfo)(ESMCI::DELayout **ptr,
    int *DEid, int *DEcoord, int *len_coord, int *DEcde, int *len_cde, 
    int *DEcw, int *len_cw, int *nDEc, int *pid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdelocalinfo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->getDELocalInfo(
      *DEid, DEcoord, *len_coord, DEcde, *len_cde, DEcw, *len_cw,
      ESMC_NOT_PRESENT_FILTER(nDEc),
      ESMC_NOT_PRESENT_FILTER(pid)), 
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutprint)(ESMCI::DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->print(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_delayoutvalidate)(ESMCI::DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->validate(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_delayoutserviceoffer)(ESMCI::DELayout **ptr, int *de,
    ESMCI::ServiceReply *reply, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserviceoffer()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    *reply = (*ptr)->serviceOffer(*de, &localrc);
//TODO: enable LogErr once it is thread-safe
    *rc=localrc;  
//    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
//      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_delayoutservicecomplete)(ESMCI::DELayout **ptr, int *de,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutservicecomplete()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    if (ESMC_LogDefault.MsgFoundError((*ptr)->serviceComplete(*de),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_delayoutserialize)(ESMCI::DELayout **delayout, char *buf,
    int *length, int *offset, ESMC_InquireFlag *inquireflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserialize"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(delayout, rc)
    ESMCI_NULL_CHECK_PRC(*delayout, rc)
    if (ESMC_LogDefault.MsgFoundError(
      (*delayout)->serialize(buf, length, offset,*inquireflag),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_delayoutdeserialize)(ESMCI::DELayout **delayout, char *buf,
    int *offset, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdeserialize"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(delayout, rc)
    *delayout = ESMCI::DELayout::deserialize(buf, offset);
    if (*delayout == NULL) 
      localrc = ESMC_RC_INTNRL_BAD;
    else
      localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
}
