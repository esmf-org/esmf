// $Id: ESMCI_DistGrid_F.C,v 1.39 2012/01/06 20:16:30 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research, 
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

#include "ESMF_LogMacros.inc" // TODO: remove once this comes through ESMCI_LogErr.h

#include <cstring>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"

#include "ESMCI_DistGrid.h"

#include "ESMCI_LogErr.h"

using namespace std;

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
        
  void FTN_X(c_esmc_distgridcreatedg)(ESMCI::DistGrid **ptr, 
    ESMCI::DistGrid **dg, ESMCI::InterfaceInt **firstExtra,
    ESMCI::InterfaceInt **lastExtra, ESMC_IndexFlag *indexflag,
    ESMCI::InterfaceInt **connectionList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedg()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::DistGrid::create(*dg, *firstExtra, *lastExtra,
      ESMC_NOT_PRESENT_FILTER(indexflag), *connectionList, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridcreaterd)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::Decomp_Flag *decompflag, int *decompflagCount, 
    ESMCI::InterfaceInt **regDecompFirstExtra,
    ESMCI::InterfaceInt **regDecompLastExtra,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
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
      *connectionList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridcreatedb)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **deBlockList,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
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
      *connectionList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridcreaterdfa)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::Decomp_Flag *decompflag, int *decompflagCount,
    ESMCI::InterfaceInt **regDecompFirstExtra,
    ESMCI::InterfaceInt **regDecompLastExtra,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
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
      *connectionList, *fastAxis, opt_vm,
      &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridcreaterdp)(ESMCI::DistGrid **ptr, 
    ESMCI::InterfaceInt **minIndex, ESMCI::InterfaceInt **maxIndex,
    ESMCI::InterfaceInt **regDecomp,
    ESMCI::Decomp_Flag *decompflag,
    int *decompflagCount1, int *decompflagCount2,
    ESMCI::InterfaceInt **regDecompFirstExtra,
    ESMCI::InterfaceInt **regDecompLastExtra,
    ESMCI::InterfaceInt **deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterfaceInt **connectionList,
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
      *connectionList, opt_delayout, opt_vm,
      &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgriddestroy)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // call into C++
    ESMC_LogDefault.MsgFoundError(ESMCI::DistGrid::destroy(ptr),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridget)(ESMCI::DistGrid **ptr,
    int *dimCount, int *tileCount,
    ESMCI::InterfaceInt **minIndexPDimPTile,
    ESMCI::InterfaceInt **maxIndexPDimPTile,
    ESMCI::InterfaceInt **elementCountPTile,
    ESMCI::InterfaceInt **minIndexPDimPDe,
    ESMCI::InterfaceInt **maxIndexPDimPDe,
    ESMCI::InterfaceInt **elementCountPDe,
    ESMCI::InterfaceInt **tileListPDe,
    ESMCI::InterfaceInt **indexCountPDimPDe,
    ESMCI::InterfaceInt **collocationPDim,
    ESMC_Logical *regDecompFlag, ESMCI::DELayout **delayout, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDELayout();
    if (ESMC_NOT_PRESENT_FILTER(tileCount) != ESMC_NULL_POINTER)
      *tileCount = (*ptr)->getTileCount();
    if (ESMC_NOT_PRESENT_FILTER(dimCount) != ESMC_NULL_POINTER)
      *dimCount = (*ptr)->getDimCount();
    if (ESMC_NOT_PRESENT_FILTER(regDecompFlag) != ESMC_NULL_POINTER){
      if ((*ptr)->getRegDecomp())
        *regDecompFlag = ESMF_TRUE;
      else
        *regDecompFlag = ESMF_FALSE;
    }
    // fill minIndexPDimPTile
    if (*minIndexPDimPTile != NULL){
      // minIndexPDimPTile was provided -> do some error checking
      if ((*minIndexPDimPTile)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- minIndexPDimPTile array must be of rank 2", rc);
        return;
      }
      if ((*minIndexPDimPTile)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of minIndexPDimPTile array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*minIndexPDimPTile)->extent[1] < (*ptr)->getTileCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of minIndexPDimPTile array must be of size 'tileCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in minIndexPDimPTile
      // arrays which are larger than dimCount x tileCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the minIndexPDimPTile
      // array.
      for (int i=0; i<(*ptr)->getTileCount(); i++)
        memcpy(
          &((*minIndexPDimPTile)->array[i*((*minIndexPDimPTile)->extent[0])]),
          &(((*ptr)->getMinIndexPDimPTile())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill maxIndexPDimPTile
    if (*maxIndexPDimPTile != NULL){
      // maxIndexPDimPTile was provided -> do some error checking
      if ((*maxIndexPDimPTile)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- maxIndexPDimPTile array must be of rank 2", rc);
        return;
      }
      if ((*maxIndexPDimPTile)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of maxIndexPDimPTile array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*maxIndexPDimPTile)->extent[1] < (*ptr)->getTileCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 2nd dim of maxIndexPDimPTile array must be of size 'tileCount'",
          rc);
        return;
      }
      // fill in the values: The interface allows to pass in maxIndexPDimPTile
      // arrays which are larger than dimCount x tileCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the maxIndexPDimPTile
      // array.
      for (int i=0; i<(*ptr)->getTileCount(); i++)
        memcpy(
          &((*maxIndexPDimPTile)->array[i*((*maxIndexPDimPTile)->extent[0])]),
          &(((*ptr)->getMaxIndexPDimPTile())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill elementCountPTile
    if (*elementCountPTile != NULL){
      // elementCountPTile was provided -> do some error checking
      if ((*elementCountPTile)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- elementCountPTile array must be of rank 1", rc);
        return;
      }
      if ((*elementCountPTile)->extent[0] < (*ptr)->getTileCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of elementCountPTile array must be of size 'tileCount'",
          rc);
        return;
      }
      // fill in values
      memcpy((*elementCountPTile)->array, (*ptr)->getElementCountPTile(),
        sizeof(int)*(*ptr)->getTileCount());
    }
    // fill minIndexPDimPDe
    if (*minIndexPDimPDe != NULL){
      // minIndexPDimPDe was provided -> do some error checking
      if ((*minIndexPDimPDe)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- minIndexPDimPDe array must be of rank 2", rc);
        return;
      }
      if ((*minIndexPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of minIndexPDimPDe array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*minIndexPDimPDe)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- maxIndexPDimPDe array must be of rank 2", rc);
        return;
      }
      if ((*maxIndexPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of maxIndexPDimPDe array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*maxIndexPDimPDe)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- elementCountPDe array must be of rank 1", rc);
        return;
      }
      if ((*elementCountPDe)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of elementCountPDe array must be of size 'deCount'",
          rc);
        return;
      }
      // fill in values
      memcpy((*elementCountPDe)->array, (*ptr)->getElementCountPDe(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
    }
    // fill tileListPDe
    if (*tileListPDe != NULL){
      // tileListPDe was provided -> do some error checking
      if ((*tileListPDe)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- tileListPDe array must be of rank 1", rc);
        return;
      }
      if ((*tileListPDe)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of tileListPDe array must be of size 'deCount'", rc);
        return;
      }
      // fill in values
      memcpy((*tileListPDe)->array, (*ptr)->getTileListPDe(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
    }
    // fill indexCountPDimPDe
    if (*indexCountPDimPDe != NULL){
      // indexCountPDimPDe was provided -> do some error checking
      if ((*indexCountPDimPDe)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- indexCountPDimPDe array must be of rank 2", rc);
        return;
      }
      if ((*indexCountPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of indexCountPDimPDe array must be of size 'dimCount'",
          rc);
        return;
      }
      if ((*indexCountPDimPDe)->extent[1] <
        (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
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
    // fill collocationPDim
    if (*collocationPDim != NULL){
      // collocationPDim was provided -> do some error checking
      if ((*collocationPDim)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- collocationPDim array must be of rank 1", rc);
        return;
      }
      int dimCount = (*ptr)->getDimCount();
      if ((*collocationPDim)->extent[0] < dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dim of collocationPDim array must be of size 'dimCount'", rc);
        return;
      }
      // fill in values
      memcpy((*collocationPDim)->array, (*ptr)->getCollocationPDim(),
        sizeof(int)*dimCount);
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_distgridgetplocalde)(ESMCI::DistGrid **ptr,
    int *localDeArg, int *collocationArg, ESMC_Logical *arbSeqIndexFlag,
    ESMCI::InterfaceInt **seqIndexList, int *elementCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetplocalde()"
    // Initialize return code; assume routine not implemented
    if (ESMC_NOT_PRESENT_FILTER(rc)) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // shift input indices
    int localDe = *localDeArg;  // already base 0
    // check input values
    int localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
    if (localDe < 0 || localDe > localDeCount-1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- Specified local DE out of bounds", ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
    // check incoming collocation argument
    int diffCollocationCount = (*ptr)->getDiffCollocationCount();
    const int *collocationTable = (*ptr)->getCollocationTable();
    int collocation;
    int collIndex;
    if (ESMC_NOT_PRESENT_FILTER(collocationArg) != ESMC_NULL_POINTER){    
      collocation = *collocationArg;
      int i;
      for (i=0; i<diffCollocationCount; i++)
        if (collocationTable[i]==collocation) break;
      if (i==diffCollocationCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "- specified collocation not valid", ESMC_NOT_PRESENT_FILTER(rc));
        return;
      }
      collIndex = i;
    }else{
      collocation = collocationTable[0]; // default to first collocation 
      collIndex = 0;
    }
    const int *arbSeqIndexList =
      (*ptr)->getArbSeqIndexList(localDe, collocation, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (ESMC_NOT_PRESENT_FILTER(arbSeqIndexFlag) != ESMC_NULL_POINTER){  
      if (arbSeqIndexList)
        *arbSeqIndexFlag = ESMF_TRUE;
      else
        *arbSeqIndexFlag = ESMF_FALSE;
    }
    // fill seqIndexList
    localrc = (*ptr)->fillSeqIndexList(*seqIndexList, localDe, collocation);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set elementCount
    if (ESMC_NOT_PRESENT_FILTER(elementCount) != ESMC_NULL_POINTER){
      int *const *elementCountPCollPLocalDe =
        (*ptr)->getElementCountPCollPLocalDe();
      *elementCount = elementCountPCollPLocalDe[collIndex][localDe];
    }
    // return successfully
    if (ESMC_NOT_PRESENT_FILTER(rc)) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_distgridgetplocaldepdim)(ESMCI::DistGrid **ptr,
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
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
      if ((*indexList)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- indexList array must be of rank 1", rc);
        return;
      }
      if ((*indexList)->extent[0] <
        ((*ptr)->getIndexCountPDimPDe())[(*ptr)->getDELayout()->
        getLocalDeToDeMap()[localDe] * (*ptr)->getDimCount()+dim]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of indexList array size insufficiently", rc);
        return;
      }
      // fill in the values
      memcpy((*indexList)->array, indexListPtr,
        sizeof(int) * (*ptr)->getIndexCountPDimPDe()[((*ptr)->getDELayout()->
        getLocalDeToDeMap()[localDe] * (*ptr)->getDimCount()+dim)]);
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_distgridmatch)(ESMCI::DistGrid **ptr1, ESMCI::DistGrid **ptr2,
    ESMCI::DistGridMatch_Flag *matchResult, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridmatch()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *matchResult = ESMCI::DistGrid::match(*ptr1, *ptr2, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }
  
  void FTN_X(c_esmc_distgridprint)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->print(),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
  }
  
  void FTN_X(c_esmc_distgridvalidate)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->validate(),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridconnection)(
    ESMCI::InterfaceInt **connection, int *tileIndexA,
    int *tileIndexB, ESMCI::InterfaceInt **positionVector,
    ESMCI::InterfaceInt **orientationVector,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connection()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(
      ESMCI::DistGrid::connection(*connection, *tileIndexA,
      *tileIndexB, *positionVector, *orientationVector), 
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridset)(
    ESMCI::DistGrid **ptr, ESMCI::InterfaceInt **collocationPDim, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridset()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(
      (*ptr)->setCollocationPDim(*collocationPDim),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridsetarbseqindex)(
    ESMCI::DistGrid **ptr, ESMCI::InterfaceInt **arbSeqIndex, 
      int *localDe, int *collocation, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridsetarbseqindex()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(
      (*ptr)->setArbSeqIndex(*arbSeqIndex, *localDe, *collocation),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }


  void FTN_X(c_esmc_distgridserialize)(ESMCI::DistGrid **distgrid, char *buf, int *length,
    int *offset, ESMC_InquireFlag *inquireflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridserialize()"

    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(
      (*distgrid)->serialize(buf, length, offset, *inquireflag),
      ESMCI_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgriddeserialize)(ESMCI::DistGrid **distgrid, char *buf,
    int *offset, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddeserialize()"

    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // Deserialize the distgrid
    *distgrid=ESMCI::DistGrid::deserialize(buf, offset);

    // Return success
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }


#undef  ESMC_METHOD
}

