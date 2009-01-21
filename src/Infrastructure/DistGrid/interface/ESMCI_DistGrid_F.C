// $Id: ESMCI_DistGrid_F.C,v 1.1.2.9 2009/01/21 21:25:20 cdeluca Exp $
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
#define ESMC_FILENAME "ESMCI_DistGrid_F.C"
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

#include "ESMCI_DistGrid.h"

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
    ESMCI::InterfaceInt **regDecompFirstExtra,
    ESMCI::InterfaceInt **regDecompLastExtra,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterd()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *regDecompFirstExtra, *regDecompLastExtra,
      *deLabelList, ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreatedb)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **deBlockList,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
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
      *connectionList, *connectionTransList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridcreaterdfa)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount,
    ESMCI::InterfaceInt **regDecompFirstExtra,
    ESMCI::InterfaceInt **regDecompLastExtra,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransList,
    int *fastAxis, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdfa()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount, *regDecompFirstExtra, *regDecompLastExtra,
      *deLabelList, ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransList, *fastAxis, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridcreaterdp)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::DecompFlag *decompflag, int *decompflagCount1, int *decompflagCount2,
    ESMCI::InterfaceInt **regDecompFirstExtra,
    ESMCI::InterfaceInt **regDecompLastExtra,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
    ESMCI::InterfaceInt **connectionTransList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdp()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*minIndex, *maxIndex, *regDecomp,
      decompflag, *decompflagCount1, *decompflagCount2, 
      *regDecompFirstExtra, *regDecompLastExtra, *deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag),
      *connectionList, *connectionTransList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgriddestroy)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMCI::DistGrid::destroy(ptr),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridget)(ESMCI::DistGrid **ptr,
    int *dimCount, int *patchCount,
    ESMCI::InterfaceInt **minIndexPDimPPatch,
    ESMCI::InterfaceInt **maxIndexPDimPPatch,
    ESMCI::InterfaceInt **elementCountPPatch,
    ESMCI::InterfaceInt **minIndexPDimPDe,
    ESMCI::InterfaceInt **maxIndexPDimPDe,
    ESMCI::InterfaceInt **elementCountPDe,
    ESMCI::InterfaceInt **patchListPDe,
    ESMCI::InterfaceInt **indexCountPDimPDe,
    ESMC_Logical *regDecompFlag, ESMCI::DELayout **delayout, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDELayout();
    if (ESMC_NOT_PRESENT_FILTER(patchCount) != ESMC_NULL_POINTER)
      *patchCount = (*ptr)->getPatchCount();
    if (ESMC_NOT_PRESENT_FILTER(dimCount) != ESMC_NULL_POINTER)
      *dimCount = (*ptr)->getDimCount();
    if (ESMC_NOT_PRESENT_FILTER(regDecompFlag) != ESMC_NULL_POINTER)
      *regDecompFlag = (*ptr)->getRegDecompFlag();
    // fill minIndexPDimPPatch
    if (*minIndexPDimPPatch != NULL){
      // minIndexPDimPPatch was provided -> do some error checking
      if ((*minIndexPDimPPatch)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- minIndexPDimPPatch array must be of rank 2", rc);
        return;
      }
      if ((*minIndexPDimPPatch)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of minIndexPDimPPatch array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*minIndexPDimPPatch)->extent[1] < (*ptr)->getPatchCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of minIndexPDimPPatch array must be of size 'patchCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in minIndexPDimPPatch
      // arrays which are larger than dimCount x patchCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the minIndexPDimPPatch
      // array.
      for (int i=0; i<(*ptr)->getPatchCount(); i++)
        memcpy(
          &((*minIndexPDimPPatch)->array[i*((*minIndexPDimPPatch)->extent[0])]),
          &(((*ptr)->getMinIndexPDimPPatch())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill maxIndexPDimPPatch
    if (*maxIndexPDimPPatch != NULL){
      // maxIndexPDimPPatch was provided -> do some error checking
      if ((*maxIndexPDimPPatch)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- maxIndexPDimPPatch array must be of rank 2", rc);
        return;
      }
      if ((*maxIndexPDimPPatch)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of maxIndexPDimPPatch array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*maxIndexPDimPPatch)->extent[1] < (*ptr)->getPatchCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of maxIndexPDimPPatch array must be of size 'patchCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in maxIndexPDimPPatch
      // arrays which are larger than dimCount x patchCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the maxIndexPDimPPatch
      // array.
      for (int i=0; i<(*ptr)->getPatchCount(); i++)
        memcpy(
          &((*maxIndexPDimPPatch)->array[i*((*maxIndexPDimPPatch)->extent[0])]),
          &(((*ptr)->getMaxIndexPDimPPatch())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill elementCountPPatch
    if (*elementCountPPatch != NULL){
      // elementCountPPatch was provided -> do some error checking
      if ((*elementCountPPatch)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- elementCountPPatch array must be of rank 1", rc);
        return;
      }
      if ((*elementCountPPatch)->extent[0] < (*ptr)->getPatchCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of elementCountPPatch array must be of size 'patchCount'",
          rc);
        return;
      }
      // fill in values
      memcpy((*elementCountPPatch)->array, (*ptr)->getElementCountPPatch(),
        sizeof(int)*(*ptr)->getPatchCount());
    }
    // fill minIndexPDimPDe
    if (*minIndexPDimPDe != NULL){
      // minIndexPDimPDe was provided -> do some error checking
      if ((*minIndexPDimPDe)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- minIndexPDimPDe array must be of rank 2", rc);
        return;
      }
      if ((*minIndexPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of minIndexPDimPDe array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*minIndexPDimPDe)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of minIndexPDimPDe array must be of size 'deCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in minIndexPDimPDe
      // arrays which are larger than dimCount x deCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the minIndexPDimPDe
      // array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(
          &((*minIndexPDimPDe)->array[i*((*minIndexPDimPDe)->extent[0])]),
          &(((*ptr)->getMinIndexPDimPDe())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill maxIndexPDimPDe
    if (*maxIndexPDimPDe != NULL){
      // maxIndexPDimPDe was provided -> do some error checking
      if ((*maxIndexPDimPDe)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- maxIndexPDimPDe array must be of rank 2", rc);
        return;
      }
      if ((*maxIndexPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of maxIndexPDimPDe array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*maxIndexPDimPDe)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of maxIndexPDimPDe array must be of size 'deCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in maxIndexPDimPDe
      // arrays which are larger than dimCount x deCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the maxIndexPDimPDe
      // array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(
          &((*maxIndexPDimPDe)->array[i*((*maxIndexPDimPDe)->extent[0])]),
          &(((*ptr)->getMaxIndexPDimPDe())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill elementCountPDe
    if (*elementCountPDe != NULL){
      // elementCountPDe was provided -> do some error checking
      if ((*elementCountPDe)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- elementCountPDe array must be of rank 1", rc);
        return;
      }
      if ((*elementCountPDe)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of elementCountPDe array must be of size 'deCount'",
          rc);
        return;
      }
      // fill in values
      memcpy((*elementCountPDe)->array, (*ptr)->getElementCountPDe(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
    }
    // fill patchListPDe
    if (*patchListPDe != NULL){
      // patchListPDe was provided -> do some error checking
      if ((*patchListPDe)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- patchListPDe array must be of rank 1", rc);
        return;
      }
      if ((*patchListPDe)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of patchListPDe array must be of size 'deCount'", rc);
        return;
      }
      // fill in values
      memcpy((*patchListPDe)->array, (*ptr)->getPatchListPDe(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
    }
    // fill indexCountPDimPDe
    if (*indexCountPDimPDe != NULL){
      // indexCountPDimPDe was provided -> do some error checking
      if ((*indexCountPDimPDe)->dimCount != 2){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- indexCountPDimPDe array must be of rank 2", rc);
        return;
      }
      if ((*indexCountPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of indexCountPDimPDe array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*indexCountPDimPDe)->extent[1] <
        (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of indexCountPDimPDe array must be of size 'deCount'", rc);
        return;
      }
      // fill in the values: The interface allows to pass in indexCountPDimPDe
      // arrays which are larger than dimCount x deCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the indexCountPDimPDe
      // array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(
          &((*indexCountPDimPDe)->array[i*((*indexCountPDimPDe)->extent[0])]),
          &(((*ptr)->getIndexCountPDimPDe())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_distgridgetplocalde)(ESMCI::DistGrid **ptr,
    int *localDeArg, ESMCI::InterfaceInt **seqIndexList, int *elementCount, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetplocalde()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // shift input indices
    int localDe = *localDeArg;  // already base 0
    // check input values
    int localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
    if (localDe < 0 || localDe > localDeCount-1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- Specified local DE out of bounds", rc);
      return;
    }
    // fill seqIndexList
    if (*seqIndexList != NULL){
      // seqIndexList provided -> error checking
      if ((*seqIndexList)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- seqIndexList array must be of rank 1", rc);
        return;
      }
      if ((*seqIndexList)->extent[0] <
        ((*ptr)->getElementCountPDe())[(*ptr)->getDELayout()->
        getLocalDeList()[localDe]]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of seqIndexList array size insufficiently", rc);
        return;
      }
      // check for arbitrary sequence indices
      const int *arbSeqIndexCountPLocalDe =
        (*ptr)->getArbSeqIndexCountPLocalDe();
      if (arbSeqIndexCountPLocalDe[localDe]){
        // arbitrary seq indices -> get arbSeqIndexListPLocalDe and fill in
        const int *arbSeqIndexListPLocalDe =
          (*ptr)->getArbSeqIndexListPLocalDe(localDe, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          ESMC_NOT_PRESENT_FILTER(rc))) return;
        memcpy((*seqIndexList)->array, arbSeqIndexListPLocalDe,
          sizeof(int) * arbSeqIndexCountPLocalDe[localDe]);
      }else{
        // default seq indices
        int dimCount = (*ptr)->getDimCount();
        int *ii = new int[dimCount];     // index tuple basis 0
        const int *iiEnd = (*ptr)->getIndexCountPDimPDe() + dimCount*
          (*ptr)->getDELayout()->getLocalDeList()[localDe];
        // reset counters
        int index = 0;
        for (int j=0; j<dimCount; j++)
          ii[j] = 0;  // reset
        // loop over all elements in exclusive region for localDe
        while(ii[dimCount-1] < iiEnd[dimCount-1]){
          (*seqIndexList)->array[index] =
            (*ptr)->getSequenceIndexLocalDe(localDe, ii);
          ++index;
          // multi-dim index increment
          ++ii[0];
          for (int j=0; j<dimCount-1; j++){
            if (ii[j] == iiEnd[j]){
              ii[j] = 0;  // reset
              ++ii[j+1];
            }
          }
        }
        delete [] ii;
      }
    }
    // set elementCount
    if (ESMC_NOT_PRESENT_FILTER(elementCount) != ESMC_NULL_POINTER){
      const int *elementCountPDe = (*ptr)->getElementCountPDe();
      *elementCount =
        elementCountPDe[(*ptr)->getDELayout()->getLocalDeList()[localDe]];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_distgridgetplocaldepdim)(ESMCI::DistGrid **ptr,
    int *localDeArg, int *dimArg, ESMCI::InterfaceInt **indexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetplocaldepdim()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // shift input indices
    int localDe = *localDeArg;  // already base 0
    int dim = *dimArg - 1;      // shift to base 0
    // fill indexList
    if (*indexList != NULL){
      // indexList provided -> get indexListPtr & do some error checking
      // getIndexListPDimPLocalDe() checks localDe and dim for range!
      const int *indexListPtr =
        (*ptr)->getIndexListPDimPLocalDe(localDe, dim+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
      if ((*indexList)->dimCount != 1){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
          "- indexList array must be of rank 1", rc);
        return;
      }
      if ((*indexList)->extent[0] <
        ((*ptr)->getIndexCountPDimPDe())[(*ptr)->getDELayout()->
        getLocalDeList()[localDe] * (*ptr)->getDimCount()+dim]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of indexList array size insufficiently", rc);
        return;
      }
      // fill in the values
      memcpy((*indexList)->array, indexListPtr,
        sizeof(int) * (*ptr)->getIndexCountPDimPDe()[((*ptr)->getDELayout()->
        getLocalDeList()[localDe] * (*ptr)->getDimCount()+dim)]);
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_distgridmatch)(ESMCI::DistGrid **ptr1, ESMCI::DistGrid **ptr2,
    ESMC_Logical *matchResult, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridmatch()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    bool matchBool = ESMCI::DistGrid::match(*ptr1, *ptr2, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (matchBool) *matchResult = ESMF_TRUE;
    else *matchResult = ESMF_FALSE;
  }
  
  void FTN(c_esmc_distgridprint)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridvalidate)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->validate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_distgridconnection)(
    ESMCI::InterfaceInt **connection, int *patchIndexA,
    int *patchIndexB, ESMCI::InterfaceInt **positionVector,
    ESMCI::InterfaceInt **orientationVector,
    ESMCI::InterfaceInt **repetitionVector, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connection()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      ESMCI::DistGrid::connection(*connection, *patchIndexA,
      *patchIndexB, *positionVector, *orientationVector, *repetitionVector), 
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN(c_esmc_distgridsetarbseqindex)(
    ESMCI::DistGrid **ptr, ESMCI::InterfaceInt **arbSeqIndex, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridsetarbseqindex()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError(
      (*ptr)->setArbSeqIndex(*arbSeqIndex),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
#undef  ESMC_METHOD
}

