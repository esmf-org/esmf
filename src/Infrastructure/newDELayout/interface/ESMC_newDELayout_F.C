// $Id: ESMC_newDELayout_F.C,v 1.9 2004/04/05 17:59:45 theurich Exp $
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
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
#include "ESMC_newDELayout.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt newDELayout} class functions.
//
//EOP
//-------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case
extern "C" {

  void FTN(c_esmc_newdelayoutcreate)(ESMC_newDELayout **ptr, ESMC_VM **vm,
    int *nDEs, int *ndim, int *DEtoPET, int *len, int *status){
    ESMC_Logical cyclic = ESMF_TRUE;
    *ptr = ESMC_newDELayoutCreate(**vm, nDEs, *ndim, DEtoPET, *len, &cyclic,
      status);
  }

  void FTN(c_esmc_newdelayoutdestroy)(ESMC_newDELayout **ptr, int *status){
    *status = ESMC_newDELayoutDestroy(*ptr);
  }
       
  void FTN(c_esmc_newdelayoutget)(ESMC_newDELayout **ptr,
    int *nDEs, int *ndim, int *nmyDEs, int *myDEs, int *len, int *localDe,
    ESMC_Logical *oneToOneFlag, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)nDEs      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : nDEs;
    (void*)ndim      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : ndim;
    (void*)nmyDEs   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : nmyDEs;
    (void*)localDe   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : localDe;
    (void*)oneToOneFlag == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : oneToOneFlag;
    // Done sorting out non-present F90 optional arguments.
    int rc = (*ptr)->ESMC_newDELayoutGet(nDEs, ndim, nmyDEs, myDEs, *len,
      localDe, oneToOneFlag);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_newdelayoutgetde)(ESMC_newDELayout **ptr,
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
    int rc = (*ptr)->ESMC_newDELayoutGetDE(*DEid, DEcoord, *len_coord, 
      DEcde, *len_cde, DEcw, *len_cw, nDEc);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_newdelayoutmyde)(ESMC_newDELayout **ptr, 
    int *DE, ESMC_Logical *value, int *status){
    *status = (*ptr)->ESMC_newDELayoutMyDE(*DE, value);
  }
  
  void FTN(c_esmc_newdelayoutprint)(ESMC_newDELayout **ptr, int *status){
    *status = (*ptr)->ESMC_newDELayoutPrint();
  }
  
  // ~~~ Communications ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       
  void FTN(c_esmc_newdelayoutcopy)(ESMC_newDELayout **ptr,
    void ***datain, void ***dataout, int *len, int *src, int* dest,
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_newDELayoutCopy((void **)datain, (void **)dataout,
        *len, *src, *dest, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_newDELayoutCopy(*datain, *dataout, *len,
        *src, *dest, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_newdelayoutscatter)(ESMC_newDELayout **ptr,
    void ***datain, void ***dataout, int *len, int *root, 
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_newDELayoutScatter((void **)datain, 
        (void **)dataout, *len, *root, *oneToOneFlag);
    }else{      
      *status = (*ptr)->ESMC_newDELayoutScatter(*datain, *dataout, *len,
        *root, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_newdelayoutgather)(ESMC_newDELayout **ptr,
    void ***datain, void ***dataout, int *len, int *root, 
    ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_newDELayoutGather((void **)datain, 
        (void **)dataout, *len, *root, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_newDELayoutGather(*datain, *dataout, *len,
        *root, *oneToOneFlag);
    }
  }
  
  void FTN(c_esmc_newdelayoutallglobalreduce)(ESMC_newDELayout **ptr, 
    void ***datain, void *result, int *len, ESMC_DataKind *dtk,
    ESMC_newOp *op, ESMC_Logical *oneToOneFlag, int *status){
    if (*oneToOneFlag == ESMF_TRUE){
      *status = (*ptr)->ESMC_newDELayoutAllGlobalReduce((void **)datain, result,
        *len, *dtk, *op, *oneToOneFlag);
    }else{
      *status = (*ptr)->ESMC_newDELayoutAllGlobalReduce(*datain, result,
        *len, *dtk, *op, *oneToOneFlag);
    }
  }

  // ~~~ DELayoutData ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_newdelayoutdatacreate)(void ***ptr, int *n, int *status){
    *ptr = ESMC_newDELayoutDataCreate(*n, status);
  }
  
  void FTN(c_esmc_newdelayoutdataadd)(void ***ptr, void *a, int *index, 
    int *status){
    *status = ESMC_newDELayoutDataAdd(*ptr, a, *index-1);
  }

  void FTN(c_esmc_newdelayoutdatadestroy)(void ***ptr, int *status){
    *status = ESMC_newDELayoutDataDestroy(*ptr);
  }
  
};
