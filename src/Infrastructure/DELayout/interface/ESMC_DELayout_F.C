// $Id: ESMC_DELayout_F.C,v 1.31 2007/05/01 15:35:36 rosalind Exp $
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

  void FTN(c_esmc_delayoutcreatefrompetmap)(ESMC_DELayout **ptr, int *petMap, 
    int *petMapCount, ESMC_DePinFlag *dePinFlag, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_VM *opt_vm;
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatefrompetmap()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DELayoutCreate(petMap, *petMapCount,
      ESMC_NOT_PRESENT_FILTER(dePinFlag), 
      opt_vm, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutcreatedefault)(ESMC_DELayout **ptr, int *deCount,
    ESMC_InterfaceInt **deGrouping, ESMC_DePinFlag *dePinFlag, 
    ESMC_InterfaceInt **petList, ESMC_VM **vm, int *rc){
    int localrc;
    ESMC_VM *opt_vm;
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreatedefault()"
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMC_DELayoutCreate(
      ESMC_NOT_PRESENT_FILTER(deCount), 
      *deGrouping, 
      ESMC_NOT_PRESENT_FILTER(dePinFlag),
      *petList, opt_vm, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutcreatend)(ESMC_DELayout **ptr, ESMC_VM **vm,
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
    *ptr = ESMC_DELayoutCreate(**vm, deCountList, *deCountListCount, petList,
      *petListCount, &cyclic, &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutdestroy)(ESMC_DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_DELayoutDestroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutget)(ESMC_DELayout **ptr, ESMC_VM **vm,
    int *deCount, int *petMap, int *petMapCount, int *vasMap,
    int *vasMapCount, ESMC_Logical *oneToOneFlag, ESMC_DePinFlag *dePinFlag,
    int *localDeCount, int *localDeList, int *localDeListCount,
    int *vasLocalDeCount, int *vasLocalDeList, int *vasLocalDeListCount, 
    int *rc){
    ESMC_VM **opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = vm;
    // call into C++, dealing with optional arguments 
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGet(
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

  void FTN(c_esmc_delayoutgetdeprecated)(ESMC_DELayout **ptr,
    int *deCount, int *dimCount, int *localDeCount, int *localDeList,
    int *len_localDeList, int *localDe, ESMC_Logical *oneToOneFlag, 
    ESMC_Logical *logRectFlag, int *deCountPerDim, int *len_deCountPerDim,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdeprecated()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetDeprecated(
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

  void FTN(c_esmc_delayoutgetvm)(ESMC_DELayout **ptr, ESMC_VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetvm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetVM(
      vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));  
  }

  void FTN(c_esmc_delayoutgetdelocalinfo)(ESMC_DELayout **ptr,
    int *DEid, int *DEcoord, int *len_coord, int *DEcde, int *len_cde, 
    int *DEcw, int *len_cw, int *nDEc, int *pid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdelocalinfo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetDELocalInfo(
      *DEid, DEcoord, *len_coord, DEcde, *len_cde, DEcw, *len_cw,
      ESMC_NOT_PRESENT_FILTER(nDEc),
      ESMC_NOT_PRESENT_FILTER(pid)), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutgetdematchde)(ESMC_DELayout **ptr,
    int *DEid, ESMC_DELayout **ptrMatch, int *deMatchCount, int *deMatchList,
    int *len_deMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematchde()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetDEMatchDE(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(deMatchCount),
      deMatchList, *len_deMatchList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutgetdematchpet)(ESMC_DELayout **ptr,
    int *DEid, ESMC_VM **ptrMatch, int *petMatchCount, int *petMatchList,
    int *len_petMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematchpet()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetDEMatchPET(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(petMatchCount),
      petMatchList, *len_petMatchList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutprint)(ESMC_DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutPrint(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_delayoutvalidate)(ESMC_DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutValidate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  

  
  void FTN(c_esmc_delayoutserviceoffer)(ESMC_DELayout **ptr, int *de,
    ESMC_DELayoutServiceReply *reply, int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutserviceoffer()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *reply = (*ptr)->ESMC_DELayoutServiceOffer(*de, &localrc);
//TODO: enable LogErr once it is thread-safe
    *rc=localrc;  
//    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
//      ESMC_NOT_PRESENT_FILTER(rc));
  }


  void FTN(c_esmc_delayoutservicecomplete)(ESMC_DELayout **ptr, int *de,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutservicecomplete()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutServiceComplete(
      *de),
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
      ESMC_DELayout **delayout,       // in/out - delayout object
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
      (*delayout)->ESMC_DELayoutSerialize(buf, length, offset),
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
      ESMC_DELayout **delayout,       // in/out - delayout object
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

    (*delayout) = ESMC_DELayoutDeserialize(buf, offset);
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

