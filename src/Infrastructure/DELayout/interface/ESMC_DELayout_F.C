// $Id: ESMC_DELayout_F.C,v 1.9 2004/04/28 23:11:49 cdeluca Exp $
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
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"
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

  void FTN(c_esmc_ndelayoutcreate)(ESMC_DELayout **ptr, ESMC_VM **vm,
    int *nDEs, int *ndim, int *DEtoPET, int *len, int *status){
    ESMC_Logical cyclic = ESMF_TRUE;
    *ptr = ESMC_DELayoutCreate(**vm, nDEs, *ndim, DEtoPET, *len, &cyclic,
      status);
  }

  void FTN(c_esmc_ndelayoutdestroy)(ESMC_DELayout **ptr, int *status){
    *status = ESMC_DELayoutDestroy(*ptr);
  }
       
  void FTN(c_esmc_ndelayoutget)(ESMC_DELayout **ptr,
    int *deCount, int *dimCount, int *localDeCount, int *localDeList,
    int *len_localDeList, int *localDe, ESMC_Logical *oneToOneFlag, 
    ESMC_Logical *logRectFlag, int *deCountPerDim, int *len_deCountPerDim,
    int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)deCount  == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : deCount;
    (void*)dimCount == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : dimCount;
    (void*)localDeCount
      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : localDeCount;
    (void*)localDe  == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : localDe;
    (void*)oneToOneFlag 
      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : oneToOneFlag;
    (void*)logRectFlag 
      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : logRectFlag;
    // Done sorting out non-present F90 optional arguments.
    int rc = (*ptr)->ESMC_DELayoutGet(deCount, dimCount, localDeCount,
      localDeList, *len_localDeList, localDe, oneToOneFlag, logRectFlag,
      deCountPerDim, *len_deCountPerDim);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_ndelayoutgetde)(ESMC_DELayout **ptr,
    int *DEid, int *DEcoord, int *len_coord, int *DEcde, int *len_cde, 
    int *DEcw, int *len_cw, int *nDEc, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)nDEc      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : nDEc;
    // Done sorting out non-present F90 optional arguments.
    int rc = (*ptr)->ESMC_DELayoutGetDE(*DEid, DEcoord, *len_coord, 
      DEcde, *len_cde, DEcw, *len_cw, nDEc);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_ndelayoutgetdematch)(ESMC_DELayout **ptr,
    int *DEid, ESMC_DELayout **ptrMatch, int *deMatchCount, int *deMatchList,
    int *len_deMatchList, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)deMatchCount 
      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : deMatchCount;
    // Done sorting out non-present F90 optional arguments.
    int rc = (*ptr)->ESMC_DELayoutGetDEMatch(*DEid, **ptrMatch, deMatchCount,
      deMatchList, *len_deMatchList);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_ndelayoutprint)(ESMC_DELayout **ptr, int *status){
    *status = (*ptr)->ESMC_DELayoutPrint();
  }
  
  // ~~~ Communications ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       
  void FTN(c_esmc_ndelayoutcopy)(ESMC_DELayout **ptr,
    void ***datain, void ***dataout, int *blen, int *src, int *dest,
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_DELayoutCopy((void **)datain, (void **)dataout,
        *blen, *src, *dest, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_DELayoutCopy(*datain, *dataout, *blen,
        *src, *dest, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_ndelayoutcopycopy)(ESMC_DELayout **ptr,
    void ***datain1, void ***datain2, void ***dataout1, void ***dataout2, 
    int *blen1, int *blen2, int *de1, int *de2,
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_DELayoutCopyCopy((void **)datain1, 
        (void **)datain2, (void **)dataout1, (void **)dataout2,
        *blen1, *blen2, *de1, *de2, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_DELayoutCopyCopy(*datain1, *datain2, *dataout1,
        *dataout2, *blen1, *blen2, *de1, *de2, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_ndelayoutscatter)(ESMC_DELayout **ptr,
    void ***datain, void ***dataout, int *blen, int *root, 
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_DELayoutScatter((void **)datain, 
        (void **)dataout, *blen, *root, *oneToOneFlag);
    }else{      
      *status = (*ptr)->ESMC_DELayoutScatter(*datain, *dataout, *blen,
        *root, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_ndelayoutgather)(ESMC_DELayout **ptr,
    void ***datain, void ***dataout, int *blen, int *root, 
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_DELayoutGather((void **)datain, 
        (void **)dataout, *blen, *root, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_DELayoutGather(*datain, *dataout, *blen,
        *root, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_ndelayoutglobreduce)(ESMC_DELayout **ptr, 
    void ***datain, void *result, int *len, ESMC_DataKind *dtk,
    ESMC_newOp *op, ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_DELayoutAllGlobalReduce((void **)datain, result,
        *len, *dtk, *op, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_DELayoutAllGlobalReduce(*datain, result,
        *len, *dtk, *op, *oneToOneFlag);
    }
  }

  // ~~~ DELayoutData ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_ndelayoutdatacreate)(void ***ptr, int *n, int *status){
    *ptr = ESMC_DELayoutDataCreate(*n, status);
  }
  
  void FTN(c_esmc_ndelayoutdataadd)(void ***ptr, void *a, int *index, 
    int *status){
    *status = ESMC_DELayoutDataAdd(*ptr, a, *index-1);
  }

  void FTN(c_esmc_ndelayoutdatadestroy)(void ***ptr, int *status){
    *status = ESMC_DELayoutDataDestroy(*ptr);
  }
  
};
