// $Id: ESMC_DELayout_F.C,v 1.32 2007/06/20 01:29:20 theurich Exp $
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
#define ESMC_FILENAME "ESMC_DELayout_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"

#include "ESMC_DELayout.h"

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
    int localrc;
    ESMCI::VM *opt_vm;
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatefrompetmap()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DELayout::create(petMap, *petMapCount,
      ESMC_NOT_PRESENT_FILTER(dePinFlag), 
      opt_vm, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutcreatedefault)(ESMCI::DELayout **ptr, int *deCount,
    ESMCI::InterfaceInt **deGrouping, ESMC_DePinFlag *dePinFlag, 
    ESMCI::InterfaceInt **petList, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatedefault()"
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
  }

  void FTN(c_esmc_delayoutcreatend)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *deCountList, int *deCountListCount, int *petList, int *petListCount,
    int *rc){
    int localrc;
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    ESMC_Logical cyclic = ESMF_TRUE;  // TODO: fix API
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatend()"
    // call into C++
    *ptr = ESMCI::DELayout::create(**vm, deCountList, *deCountListCount,
      petList, *petListCount, &cyclic, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
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
  }

  void FTN(c_esmc_delayoutget)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *deCount, int *petMap, int *petMapCount, int *vasMap,
    int *vasMapCount, ESMC_Logical *oneToOneFlag, ESMC_DePinFlag *dePinFlag,
    int *localDeCount, int *localDeList, int *localDeListCount,
    int *vasLocalDeCount, int *vasLocalDeList, int *vasLocalDeListCount, 
    int *rc){
    ESMCI::VM **opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = vm;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->get(
      opt_vm,
      ESMC_NOT_PRESENT_FILTER(deCount),
      petMap, *petMapCount, vasMap, *vasMapCount,
      ESMC_NOT_PRESENT_FILTER(oneToOneFlag),
      ESMC_NOT_PRESENT_FILTER(dePinFlag),
      ESMC_NOT_PRESENT_FILTER(localDeCount),
      localDeList, *localDeListCount, 
      ESMC_NOT_PRESENT_FILTER(vasLocalDeCount),
      vasLocalDeList, *vasLocalDeListCount), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
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
  }

  void FTN(c_esmc_delayoutgetvm)(ESMCI::DELayout **ptr, ESMCI::VM **vm,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetvm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->getVM(
      vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));  
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
  }

  
  void FTN(c_esmc_delayoutserviceoffer)(ESMCI::DELayout **ptr, int *de,
    ESMCI::DELayoutServiceReply *reply, int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserviceoffer()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *reply = (*ptr)->serviceOffer(*de, &localrc);
//TODO: enable LogErr once it is thread-safe
    *rc=localrc;  
//    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
//      ESMC_NOT_PRESENT_FILTER(rc));
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
  }
  
  
  
  
  
  
  
    
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserialize"
//BOP
// !IROUTINE:  c_ESMC_DELayoutSerialize - Serialize DELayout object 
//
// !INTERFACE:
      void FTN(c_esmc_delayoutserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMCI::DELayout **delayout,       // in/out - delayout object
      char *buf,                // in/out - really a byte stream
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a delayout object.
//     Warning!!  Not completely implemented yet.
//
//EOP

    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      (*delayout)->serialize(buf, length, offset),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_DELayoutSerialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdeserialize"
//BOP
// !IROUTINE:  c_ESMC_DELayoutDeserialize - Deserialize DELayout object 
//
// !INTERFACE:
      void FTN(c_esmc_delayoutdeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMCI::DELayout **delayout,       // in/out - delayout object
      char *buf,                // in/out - really a byte stream
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a delayout object.
//
//EOP
    int localrc;

    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    (*delayout) = ESMCI::DELayout::deserialize(buf, offset);
    if (*delayout == NULL) 
        localrc = ESMF_FAILURE;
    else
        localrc = ESMF_SUCCESS;

    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));

}  // end c_ESMC_DELayoutDeserialize



#undef  ESMC_METHOD
}

