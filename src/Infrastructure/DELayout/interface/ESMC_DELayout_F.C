// $Id: ESMC_DELayout_F.C,v 1.21 2004/11/05 08:14:49 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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

  void FTN(c_esmc_delayoutcreate)(ESMC_DELayout **ptr, ESMC_VM **vm,
    int *deCountList, int *len_deCountList, int *dePetList, int *len_dePetList,
    int *rc){
    int localrc;
    ESMC_Logical cyclic = ESMF_TRUE;  // TODO: fix API
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcreate()"
    *ptr = ESMC_DELayoutCreate(**vm, deCountList, *len_deCountList, dePetList,
      *len_dePetList, &cyclic, &localrc);
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutdestroy)(ESMC_DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdestroy()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_DELayoutDestroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
       
  void FTN(c_esmc_delayoutgetvm)(ESMC_DELayout **ptr, ESMC_VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetvm()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetVM(
      vm),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));  
}

  void FTN(c_esmc_delayoutget)(ESMC_DELayout **ptr,
    int *deCount, int *dimCount, int *localDeCount, int *localDeList,
    int *len_localDeList, int *localDe, ESMC_Logical *oneToOneFlag, 
    ESMC_Logical *logRectFlag, int *deCountPerDim, int *len_deCountPerDim,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutget()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGet(
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

  void FTN(c_esmc_delayoutgetdelocalinfo)(ESMC_DELayout **ptr,
    int *DEid, int *DEcoord, int *len_coord, int *DEcde, int *len_cde, 
    int *DEcw, int *len_cw, int *nDEc, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdelocalinfo()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetDELocalInfo(
      *DEid, DEcoord, *len_coord, DEcde, *len_cde, DEcw, *len_cw,
      ESMC_NOT_PRESENT_FILTER(nDEc)),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutgetdematchde)(ESMC_DELayout **ptr,
    int *DEid, ESMC_DELayout **ptrMatch, int *deMatchCount, int *deMatchList,
    int *len_deMatchList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgetdematch()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutGetDEMatchDE(
      *DEid, **ptrMatch, 
      ESMC_NOT_PRESENT_FILTER(deMatchCount),
      deMatchList, *len_deMatchList),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutprint)(ESMC_DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutprint()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutPrint(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_delayoutvalidate)(ESMC_DELayout **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutvalidate()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_DELayoutValidate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  // - ESMF-private methods:
       
  void FTN(c_esmc_delayoutallfullreduce)(ESMC_DELayout **ptr, 
    void ***datain, void *result, int *len, ESMC_DataKind *dtk,
    ESMC_Operation *op, ESMC_Logical *oneToOneFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutallfullreduce()"
    int localrc;
    if (*oneToOneFlag == ESMF_TRUE){
      localrc = (*ptr)->ESMC_DELayoutAllFullReduce((void **)datain, result,
        *len, *dtk, *op, *oneToOneFlag);
    }else{
      localrc = (*ptr)->ESMC_DELayoutAllFullReduce(*datain, result,
        *len, *dtk, *op, *oneToOneFlag);
    }
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutcopy)(ESMC_DELayout **ptr,
    void ***datain, void ***dataout, int *blen, int *src, int *dest,
    ESMC_Logical *oneToOneFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutcopy()"
    int localrc;
    if (*oneToOneFlag == ESMF_TRUE){
      localrc = (*ptr)->ESMC_DELayoutCopy((void **)datain, (void **)dataout,
        *blen, *src, *dest, *oneToOneFlag);
    }else{
      localrc = (*ptr)->ESMC_DELayoutCopy(*datain, *dataout, *blen,
        *src, *dest, *oneToOneFlag);
    }
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_delayoutexchange)(ESMC_DELayout **ptr,
    void ***datain1, void ***datain2, void ***dataout1, void ***dataout2, 
    int *blen1, int *blen2, int *de1, int *de2,
    ESMC_Logical *oneToOneFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutexchange()"
    int localrc;
    if (*oneToOneFlag == ESMF_TRUE){
      localrc = (*ptr)->ESMC_DELayoutExchange((void **)datain1, 
        (void **)datain2, (void **)dataout1, (void **)dataout2,
        *blen1, *blen2, *de1, *de2, *oneToOneFlag);
    }else{
      localrc = (*ptr)->ESMC_DELayoutExchange(*datain1, *datain2, *dataout1,
        *dataout2, *blen1, *blen2, *de1, *de2, *oneToOneFlag);
    }
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_delayoutgather)(ESMC_DELayout **ptr,
    void ***datain, void ***dataout, int *blen, int *root, 
    ESMC_Logical *oneToOneFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutgather()"
    int localrc;
    if (*oneToOneFlag == ESMF_TRUE){
      localrc = (*ptr)->ESMC_DELayoutGather((void **)datain, 
        (void **)dataout, *blen, *root, *oneToOneFlag);
    }else{
      localrc = (*ptr)->ESMC_DELayoutGather(*datain, *dataout, *blen,
        *root, *oneToOneFlag);
    }
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutscatter)(ESMC_DELayout **ptr,
    void ***datain, void ***dataout, int *blen, int *root, 
    ESMC_Logical *oneToOneFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutscatter()"
    int localrc;
    if (*oneToOneFlag == ESMF_TRUE){
      localrc = (*ptr)->ESMC_DELayoutScatter((void **)datain, 
        (void **)dataout, *blen, *root, *oneToOneFlag);
    }else{      
      localrc = (*ptr)->ESMC_DELayoutScatter(*datain, *dataout, *blen,
        *root, *oneToOneFlag);
    }
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_delayoutdatacreate)(void ***ptr, int *n, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdatacreate()"
    int localrc;
    *ptr = ESMC_DELayoutDataCreate(*n, &localrc);
    // Use LogErr to handle return code
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_delayoutdataadd)(void ***ptr, void *a, int *index, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdataadd()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMC_DELayoutDataAdd(*ptr, a, *index-1),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_delayoutdatadestroy)(void ***ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_delayoutdatadestroy()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMC_DELayoutDataDestroy(*ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
};
