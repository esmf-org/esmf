// $Id: ESMC_DistGrid.C,v 1.36 2007/09/26 18:46:52 theurich Exp $
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
#define ESMC_FILENAME "ESMC_DistGrid.C"
//==============================================================================
//
// ESMC DistGrid method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DistGrid methods declared
// in the companion file ESMC_DistGrid.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_DistGrid.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMC_Base.h" 
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"

// LogErr headers
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_DistGrid.C,v 1.36 2007/09/26 18:46:52 theurich Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid:create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid:create
//
// !INTERFACE:
DistGrid *DistGrid::create(
//
// !RETURN VALUE:
//    ESMCI::DistGrid * to newly allocated DistGrid
//
// !ARGUMENTS:
//
  InterfaceInt *minIndex,                 // (in)
  InterfaceInt *maxIndex,                 // (in)
  InterfaceInt *regDecomp,                // (in)
  DecompFlag *decompflag,                 // (in)
  int decompflagCount,                    // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
  InterfaceInt *connectionTransformList,  // (in)
  DELayout *delayout,                     // (in)
  VM *vm,                                 // (in)
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // allocate the new DistGrid object
  DistGrid *distgrid;
  try{
    distgrid = new DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call construct()
  if (minIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minIndex array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxIndex array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (minIndex->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- minIndex array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxIndex->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- maxIndex array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int dimCount = minIndex->extent[0];
  if (maxIndex->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = VM::getCurrent(&localrc);  // get current VM for default
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  int petCount = vm->getNpets();
  int deCount=1;  // reset
  if (regDecomp != ESMC_NULL_POINTER){
    if (regDecomp->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- regDecomp array must be of rank 1", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (regDecomp->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of regDecomp array must be of size dimCount", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    // regDecomp was provided -> determine number of DEs according to regDecomp
    for (int i=0; i<regDecomp->extent[0]; i++)
      deCount *= regDecomp->array[i]; 
  }else{
    // regDecomp was not provided -> set deCount = petCount for default
    deCount = petCount;
  }
  if (delayout == ESMC_NULL_POINTER){
    // delayout was not provided -> create default DELayout with deCount DEs
    delayout = DELayout::create(&deCount, NULL, NULL, NULL, vm, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> check deCount
    deCount = delayout->getDeCount();
  }
  int *dummy;
  int regDecompDeleteFlag = 0;  // reset
  if (regDecomp == ESMC_NULL_POINTER){
    // regDecomp was not provided -> create a temporary default regDecomp
    regDecompDeleteFlag = 1;  // set
    dummy = new int[dimCount];
    // set default decomposition
    dummy[0] = deCount;
    for (int i=1; i<dimCount; i++)
      dummy[i] = 1;
    regDecomp = new InterfaceInt(dummy, 1, &dimCount);
  }
  if (regDecomp->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecomp array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int decompflagDeleteFlag = 0; // reset
  if (decompflagCount==0){
    // decompflag was not provided -> set up default decompflag
    decompflagDeleteFlag = 1; // set
    decompflagCount = dimCount;
    decompflag = new DecompFlag[dimCount];
    for (int i=0; i<dimCount; i++)
      decompflag[i] = DECOMP_DEFAULT;
  }
  if (decompflagCount != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minIndex and maxIndex arrays", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int deLabelListDeleteFlag = 0;  // reset
  if (deLabelList == ESMC_NULL_POINTER){
    // deLabelList was not provided -> create a temporary default deLabelList
    deLabelListDeleteFlag = 1;  // set
    dummy = new int[deCount];
    // set default sequence
    for (int i=0; i<deCount; i++)
      dummy[i] = i;
    deLabelList = new InterfaceInt(dummy, 1, &deCount);
  }
  if (deLabelList->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (deLabelList->extent[0] < deCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must provide deCount DE labels", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  for (int i=0; i<deCount; i++){
    if (deLabelList->array[i] < 0 || deLabelList->array[i] >= deCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
        "- deLabelList array contains invalid DE labels", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  
  // setup temporary indexCountPDimPDe, indexListPDimPLocalDe and dimCongtigFlag
  // arrays for construct()
  int localDeCount = delayout->getLocalDeCount();
  const int *deList = delayout->getDeList();
  int *indexCountPDimPDe = new int[dimCount*deCount];
  int **indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  int *contigFlagPDimPDe = new int[dimCount*deCount];
  int *minIndexPDimPDe = new int[dimCount*deCount];
  int *maxIndexPDimPDe = new int[dimCount*deCount];
  int deDivider = 1;  // reset
  for (int i=0; i<dimCount; i++){
    int dimLength = maxIndex->array[i] - minIndex->array[i] + 1;
    int chunkLength = dimLength/regDecomp->array[i];  // basic chunk size
    int chunkRest = dimLength%regDecomp->array[i];    // left over points
    int de, decompChunk, extentIndex;
    switch (decompflag[i]){
      case DECOMP_DEFAULT:
      case DECOMP_HOMOGEN:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          indexCountPDimPDe[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest)
            ++indexCountPDimPDe[extentIndex]; // dist. rest
          contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk < chunkRest) indexStart += decompChunk;
          else indexStart += chunkRest;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + indexCountPDimPDe[extentIndex] - 1;
          if (deList[de] > -1){
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // block structure
              indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
            }
          }
        }
        break;
      case DECOMP_RESTLAST:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          indexCountPDimPDe[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk == regDecomp->array[i]-1) 
            indexCountPDimPDe[extentIndex] += chunkRest; // add rest
          contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + indexCountPDimPDe[extentIndex] - 1;
          if (deList[de] > -1){
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // block structure
              indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
            }
          }
        }
        break;
      case DECOMP_RESTFIRST:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          indexCountPDimPDe[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk == 0) 
            indexCountPDimPDe[extentIndex] += chunkRest; // add rest
          contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk > 0) indexStart += chunkRest;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + indexCountPDimPDe[extentIndex] - 1;
          if (deList[de] > -1){
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // block structure
              indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
            }
          }
        }
        break;
      case DECOMP_CYCLIC:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          indexCountPDimPDe[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest)
            ++indexCountPDimPDe[extentIndex]; // distr. rest
          contigFlagPDimPDe[extentIndex] = 0; // flag non-contiguous dimension
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + (indexCountPDimPDe[extentIndex] - 1) * regDecomp->array[i];
          if (deList[de] > -1){
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // cyclic
              indexListPDimPLocalDe[localExtentIndex][k] = 
                indexStart + k * regDecomp->array[i];
            }
          }
        }
        break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
          "- this decompflag is currently not implemented", rc);
        delete distgrid;
        distgrid = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
        break;
    }
    deDivider *= regDecomp->array[i];
  }
  // set up patchListPDe
  int *patchListPDe = new int[deCount];
  for (int i=0; i<deCount; i++)
    patchListPDe[i] = 1;
  
  // call into construct()
  localrc = distgrid->construct(dimCount, 1, patchListPDe,
    minIndex->array, maxIndex->array, minIndexPDimPDe, maxIndexPDimPDe,
    contigFlagPDimPDe, indexCountPDimPDe, indexListPDimPLocalDe, ESMF_TRUE,
    connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] contigFlagPDimPDe;
  delete [] minIndexPDimPDe;
  delete [] maxIndexPDimPDe;
  delete [] indexCountPDimPDe;
  for (int i=0; i<dimCount*localDeCount; i++)
    delete [] indexListPDimPLocalDe[i];
  delete [] indexListPDimPLocalDe;
  if (regDecompDeleteFlag){
    delete [] regDecomp->array;
    delete regDecomp;
  }
  if (decompflagDeleteFlag){
    delete [] decompflag;
  }
  if (deLabelListDeleteFlag){
    delete [] deLabelList->array;
    delete deLabelList;
  }
  delete [] patchListPDe;
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid:create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid:create
//
// !INTERFACE:
DistGrid *DistGrid::create(
//
// !RETURN VALUE:
//    DistGrid * to newly allocated DistGrid
//
// !ARGUMENTS:
//
  InterfaceInt *minIndex,                 // (in)
  InterfaceInt *maxIndex,                 // (in)
  InterfaceInt *deBlockList,              // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
  InterfaceInt *connectionTransformList,  // (in)
  DELayout *delayout,                     // (in)
  VM *vm,                                 // (in)
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // allocate the new DistGrid object
  DistGrid *distgrid;
  try{
    distgrid = new DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call construct()
  if (minIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minIndex array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxIndex array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (minIndex->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- minIndex array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxIndex->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- maxIndex array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int dimCount = minIndex->extent[0];
  if (maxIndex->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = VM::getCurrent(&localrc);  // get current VM for default
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  int petCount = vm->getNpets();
  if (deBlockList == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to deBlockList array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (deBlockList->dimCount != 3){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- deBlockList array must be of rank 3", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (deBlockList->extent[0] < dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- deBlockList array must provide dimCount elements in first dimension",
      rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (deBlockList->extent[1] < 2){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- deBlockList array must provide 2 elements in second dimension", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int deCount = deBlockList->extent[2]; // the 3rd dimension runs through DEs
  if (delayout == ESMC_NULL_POINTER){
    // delayout was not provided -> create default DELayout with deCount DEs
    delayout = DELayout::create(&deCount, NULL, NULL, NULL, vm, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> check deCount
    deCount = delayout->getDeCount();
  }
  int *dummy;
  int deLabelListDeleteFlag = 0;  // reset
  if (deLabelList == ESMC_NULL_POINTER){
    // deLabelList was not provided -> create a temporary default deLabelList
    deLabelListDeleteFlag = 1;  // set
    dummy = new int[deCount];
    // set default sequence
    for (int i=0; i<deCount; i++)
      dummy[i] = i;
    deLabelList = new InterfaceInt(dummy, 1, &deCount);
  }
  if (deLabelList->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (deLabelList->extent[0] < deCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- deLabelList array must provide deCount DE labels", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  for (int i=0; i<deCount; i++){
    if (deLabelList->array[i] < 0 || deLabelList->array[i] >= deCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
        "- deLabelList array contains invalid DE labels", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  
  // setup temporary indexCountPDimPDe, indexListPDimPLocalDe and dimCongtigFlag
  // arrays for construct()
  int localDeCount = delayout->getLocalDeCount();
  const int *deList = delayout->getDeList();
  int *indexCountPDimPDe = new int[dimCount*deCount];
  int **indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  int *contigFlagPDimPDe = new int[dimCount*deCount];
  int *minIndexPDimPDe = new int[dimCount*deCount];
  int *maxIndexPDimPDe = new int[dimCount*deCount];
  for (int i=0; i<dimCount; i++){
    int dimLength = maxIndex->array[i] - minIndex->array[i] + 1;
    int de, extentIndex, deBlockIndexMin, deBlockIndexMax;
    for (int j=0; j<deCount; j++){
      de = deLabelList->array[j];
      extentIndex = de*dimCount+i;  // index into temp. arrays
      deBlockIndexMin = j*deBlockList->extent[0]*deBlockList->extent[1] + i;
      deBlockIndexMax = deBlockIndexMin + deBlockList->extent[0];
      indexCountPDimPDe[extentIndex] = deBlockList->array[deBlockIndexMax]
        - deBlockList->array[deBlockIndexMin] + 1;
      contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
      // determine min and max
      minIndexPDimPDe[extentIndex] = deBlockList->array[deBlockIndexMin];
      maxIndexPDimPDe[extentIndex] = deBlockList->array[deBlockIndexMax];
      if (deList[j] > -1){
        int localExtentIndex = deList[j]*dimCount+i;
        indexListPDimPLocalDe[localExtentIndex] =
          new int[indexCountPDimPDe[extentIndex]];
        for (int k=0; k<indexCountPDimPDe[extentIndex]; k++)
          indexListPDimPLocalDe[localExtentIndex][k] =
            deBlockList->array[deBlockIndexMin] + k;
      }
    }
  }
  // set up patchListPDe
  int *patchListPDe = new int[deCount];
  for (int i=0; i<deCount; i++)
    patchListPDe[i] = 1;

  // todo: check for overlapping deBlocks!!
  // call into construct()
  localrc = distgrid->construct(dimCount, 1, patchListPDe, 
    minIndex->array, maxIndex->array, minIndexPDimPDe, maxIndexPDimPDe,
    contigFlagPDimPDe, indexCountPDimPDe, indexListPDimPLocalDe, ESMF_FALSE,
    connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
    
  // garbage collection
  delete [] contigFlagPDimPDe;
  delete [] minIndexPDimPDe;
  delete [] maxIndexPDimPDe;
  delete [] indexCountPDimPDe;
  for (int i=0; i<dimCount*localDeCount; i++)
    delete [] indexListPDimPLocalDe[i];
  delete [] indexListPDimPLocalDe;
  if (deLabelListDeleteFlag){
    delete [] deLabelList->array;
    delete deLabelList;
  }
  delete [] patchListPDe;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid:create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid:create
//
// !INTERFACE:
DistGrid *DistGrid::create(
//
// !RETURN VALUE:
//    DistGrid * to newly allocated DistGrid
//
// !ARGUMENTS:
//
  InterfaceInt *minIndex,                 // (in)
  InterfaceInt *maxIndex,                 // (in)
  InterfaceInt *regDecomp,                // (in)
  DecompFlag *decompflag,                 // (in)
  int decompflagCount,                    // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
  InterfaceInt *connectionTransformList,  // (in)
  int fastAxis,                           // (in)
  VM *vm,                                 // (in)
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // create a DELayout according to fastAxis argument
  // todo: once DELayout functions exist to determine communication capabilities
  // this is the place to use it to create a DELayout according to fastAxis. For
  // now indicate that a default DELayout is to be created in the following call
  DELayout *delayout = NULL;
  
  // use DistGrid::create() with DELayout to create a suitable DistGrid object
  DistGrid *distgrid = 
    create(minIndex, maxIndex, regDecomp, decompflag,
      decompflagCount, deLabelList, indexflag, connectionList,
      connectionTransformList, delayout, vm, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return distgrid;
  
  // return successfully
  //if (rc!=NULL) *rc = ESMF_SUCCESS; TODO: override ESMC_RC_NOT_IMPL
  
  return distgrid;
}
//-----------------------------------------------------------------------------

  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid:create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid:create
//
// !INTERFACE:
DistGrid *DistGrid::create(
//
// !RETURN VALUE:
//    DistGrid * to newly allocated DistGrid
//
// !ARGUMENTS:
//
  InterfaceInt *minIndex,                 // (in)
  InterfaceInt *maxIndex,                 // (in)
  InterfaceInt *regDecomp,                // (in)
  DecompFlag *decompflag,                 // (in)
  int decompflagCount1,                   // (in)
  int decompflagCount2,                   // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
  InterfaceInt *connectionTransformList,  // (in)
  DELayout *delayout,                     // (in)
  VM *vm,                                 // (in)
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // allocate the new DistGrid object
  DistGrid *distgrid;
  try{
    distgrid = new DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call construct()
  if (minIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minIndex array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxIndex array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (minIndex->dimCount != 2){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- minIndex array must be of rank 2", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxIndex->dimCount != 2){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- maxIndex array must be of rank 2", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int dimCount = minIndex->extent[0];
  if (maxIndex->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch in dimCount", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int patchCount = minIndex->extent[1];
  if (maxIndex->extent[1] != patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch in patchCount", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = VM::getCurrent(&localrc);  // get current VM for default
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  int petCount = vm->getNpets();
  int deCount=0;  // reset
  int *deCountPPatch;
  if (regDecomp != ESMC_NULL_POINTER){
    if (regDecomp->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- regDecomp array must be of rank 2", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (regDecomp->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of regDecomp array must be of size dimCount", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (regDecomp->extent[1] != patchCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of regDecomp array must be of size patchCount", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    // regDecomp was provided -> determine number of DEs according to regDecomp
    deCountPPatch = new int[patchCount];
    for (int i=0; i<patchCount; i++){
      int localProduct = 1; // reset
      for (int j=0; j<dimCount; j++)
        localProduct *= regDecomp->array[i*dimCount+j];
      deCountPPatch[i] = localProduct;
      deCount += localProduct;
    }
  }else{
    // regDecomp was not provided -> set deCount = patchCount for default
    deCountPPatch = new int[patchCount];
    for (int i=0; i<patchCount; i++)
      deCountPPatch[i] = 1;
    deCount = patchCount;
  }
  if (delayout == ESMC_NULL_POINTER){
    // delayout was not provided -> create default DELayout with deCount DEs
    delayout = DELayout::create(&deCount, NULL, NULL, NULL, vm, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> check deCount
    deCount = delayout->getDeCount();
  }
  int *dummy, dummyLen[2];
  int regDecompDeleteFlag = 0;  // reset
  if (regDecomp == ESMC_NULL_POINTER){
    // regDecomp was not provided -> create a temporary default regDecomp
    regDecompDeleteFlag = 1;  // set
    dummy = new int[dimCount*patchCount];
    // set default decomposition
    for (int i=0; i<dimCount*patchCount; i++)
      dummy[i] = 1;
    dummyLen[0] = dimCount;
    dummyLen[1] = patchCount;
    regDecomp = new InterfaceInt(dummy, 2, dummyLen);
  }
  if (regDecomp->dimCount != 2){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecomp array must be of rank 2", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int decompflagDeleteFlag = 0; // reset
  if (decompflagCount1==0 || decompflagCount2==0){
    // decompflag was not provided -> set up default decompflag
    decompflagDeleteFlag = 1; // set
    decompflagCount1 = dimCount;
    decompflagCount2 = patchCount;
    decompflag = new DecompFlag[dimCount*patchCount];
    for (int i=0; i<dimCount*patchCount; i++)
      decompflag[i] = DECOMP_DEFAULT;
  }
  if (decompflagCount1 != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minIndex and maxIndex arrays", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (decompflagCount2 != patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minIndex and maxIndex arrays", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int deLabelListDeleteFlag = 0;  // reset
  if (deLabelList == ESMC_NULL_POINTER){
    // deLabelList was not provided -> create a temporary default deLabelList
    deLabelListDeleteFlag = 1;  // set
    dummy = new int[deCount];
    // set default sequence
    for (int i=0; i<deCount; i++)
      dummy[i] = i;
    deLabelList = new InterfaceInt(dummy, 1, &deCount);
  }
  if (deLabelList->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (deLabelList->extent[0] < deCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must provide deCount DE labels", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  for (int i=0; i<deCount; i++){
    if (deLabelList->array[i] < 0 || deLabelList->array[i] >= deCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
        "- deLabelList array contains invalid DE labels", rc);
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
    
  // setup temporary indexCountPDimPDe, indexListPDimPLocalDe and dimCongtigFlag
  // arrays for construct()
  int localDeCount = delayout->getLocalDeCount();
  const int *deList = delayout->getDeList();
  int *indexCountPDimPDe = new int[dimCount*deCount];
  int **indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  int *contigFlagPDimPDe = new int[dimCount*deCount];
  int *minIndexPDimPDe = new int[dimCount*deCount];
  int *maxIndexPDimPDe = new int[dimCount*deCount];
  
  // the following differs from the single patch case in that there is an
  // extra outer loop over patches. The indexCountPDimPDe and
  // indexListPDimPLocalDe arrays are on DE basis, independent of patches.

  int dePatchStart = 0;  // reset  
  for (int patch=0; patch<patchCount; patch++){
    int deDivider = 1;  // reset
    for (int ii=0; ii<dimCount; ii++){
      int i = patch*dimCount + ii;  // work in the current patch
      int dimLength = maxIndex->array[i] - minIndex->array[i] + 1;
      int chunkLength = dimLength/regDecomp->array[i];  // basic chunk size
      int chunkRest = dimLength%regDecomp->array[i];    // left over points
      int de, decompChunk, extentIndex;
      switch (decompflag[i]){
        case DECOMP_DEFAULT:
        case DECOMP_HOMOGEN:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest)
              ++indexCountPDimPDe[extentIndex]; //distr. rest
            contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart +
              indexCountPDimPDe[extentIndex] - 1;
            if (deList[de] > -1){
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
          }
          break;
        case DECOMP_RESTLAST:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == regDecomp->array[i]-1) 
              indexCountPDimPDe[extentIndex] += chunkRest; // add rest
            contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart +
              indexCountPDimPDe[extentIndex] - 1;
            if (deList[de] > -1){
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
          }
          break;
        case DECOMP_RESTFIRST:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == 0) 
              indexCountPDimPDe[extentIndex] += chunkRest; // add rest
            contigFlagPDimPDe[extentIndex] = 1; // flag contiguous dimension
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart +
              indexCountPDimPDe[extentIndex] - 1;
            if (deList[de] > -1){
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
          }
          break;
        case DECOMP_CYCLIC:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest)
              ++indexCountPDimPDe[extentIndex]; //distr. rest
            contigFlagPDimPDe[extentIndex] = 0; // flag non-contiguous dimension
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart +
              indexCountPDimPDe[extentIndex] - 1;
            if (deList[de] > -1){
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
          }
          break;
        default:
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
            "- this decompflag is currently not implemented", rc);
          delete distgrid;
          distgrid = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
          break;
      }
      deDivider *= regDecomp->array[i];
    } // i-loop
    dePatchStart += deCountPPatch[patch];
  } // patch-loop
  // set up patchListPDe
  dePatchStart = 0;  // reset  
  int *patchListPDe = new int[deCount];
  for (int patch=0; patch<patchCount; patch++){
    for (int jj=0; jj<deCountPPatch[patch]; jj++){
      int j = dePatchStart + jj;
      int de = deLabelList->array[j];
      patchListPDe[de] = patch + 1;  // patch ids are basis 1
    }
    dePatchStart += deCountPPatch[patch];
  }

  // call into construct()
  localrc = distgrid->construct(dimCount, patchCount, patchListPDe,
    minIndex->array, maxIndex->array, minIndexPDimPDe, maxIndexPDimPDe,
    contigFlagPDimPDe, indexCountPDimPDe, indexListPDimPLocalDe, ESMF_TRUE,
    connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] contigFlagPDimPDe;
  delete [] minIndexPDimPDe;
  delete [] maxIndexPDimPDe;
  delete [] deCountPPatch;
  delete [] indexCountPDimPDe;
  for (int i=0; i<dimCount*localDeCount; i++)
    delete [] indexListPDimPLocalDe[i];
  delete [] indexListPDimPLocalDe;
  if (regDecompDeleteFlag){
    delete [] regDecomp->array;
    delete regDecomp;
  }
  if (decompflagDeleteFlag){
    delete [] decompflag;
  }
  if (deLabelListDeleteFlag){
    delete [] deLabelList->array;
    delete deLabelList;
  }
  delete [] patchListPDe;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------
  
  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI:DistGrid::destroy()"
//BOPI
// !IROUTINE:  ESMCI:DistGrid::destroy
//
// !INTERFACE:
int DistGrid::destroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  DistGrid **distgrid){  // in - DistGrid to destroy
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (distgrid == ESMC_NULL_POINTER || *distgrid == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", &rc);
    return rc;
  }

  // destruct() and delete DistGrid object
  localrc = (*distgrid)->destruct();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  delete *distgrid;
  *distgrid = ESMC_NULL_POINTER;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// construct() and destruct()
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::construct()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::construct
//
// !INTERFACE:
int DistGrid::construct(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int dimCountArg,                      // (in)
  int patchCountArg,                    // (in)
  int *patchListPDeArg,                 // (in)
  int *minIndexArg,                     // (in)
  int *maxIndexArg,                     // (in)
  int *minIndexPDimPDeArg,              // (in)
  int *maxIndexPDimPDeArg,              // (in)
  int *contigFlagPDimPDeArg,            // (in)
  int *indexCountPDimPDeArg,            // (in)
  int **indexListPDimPLocalDeArg,       // (in)
  ESMC_Logical regDecompFlagArg,        // (in)
  InterfaceInt *connectionListArg,      // (in)
  DELayout *delayoutArg,                // (in) DELayout
  VM *vmArg                             // (in) VM context
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMCI::DistGrid object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // fill in the DistGrid object
  dimCount = dimCountArg;
  patchCount = patchCountArg;
  regDecompFlag = regDecompFlagArg;
  if (connectionListArg != NULL){
    // connectionList was provided
    int elementSize = 2*dimCount+2;
    if (connectionListArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- connectionListArg array must be of rank 2", &rc);
      return rc;
    }
    if (connectionListArg->extent[0] != elementSize){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of connectionListArg array must be of size "
        "(2*dimCount+2)", &rc);
      return rc;
    }
    // fill in the connectionList member
    connectionCount = connectionListArg->extent[1];
    connectionList = new int*[connectionCount];
    for (int i=0; i<connectionCount; i++){
      connectionList[i] = new int[elementSize];
      memcpy(connectionList[i],
        &(connectionListArg->array[elementSize*i]), sizeof(int)*elementSize);
    }
  }else{
    // connectionList was not provided -> nullify
    connectionCount = 0;
    connectionList = NULL;
  }
  delayout = delayoutArg;
  vm = vmArg;
  // fill in the rest
  minIndexPDimPPatch = new int[dimCount*patchCount];
  memcpy(minIndexPDimPPatch, minIndexArg, sizeof(int)*dimCount*patchCount);
  maxIndexPDimPPatch = new int[dimCount*patchCount];
  memcpy(maxIndexPDimPPatch, maxIndexArg, sizeof(int)*dimCount*patchCount);
  int deCount = delayout->getDeCount();
  minIndexPDimPDe = new int[dimCount*deCount];
  memcpy(minIndexPDimPDe, minIndexPDimPDeArg, sizeof(int)*dimCount*deCount);
  maxIndexPDimPDe = new int[dimCount*deCount];
  memcpy(maxIndexPDimPDe, maxIndexPDimPDeArg, sizeof(int)*dimCount*deCount);
  contigFlagPDimPDe = new int[dimCount*deCount];
  memcpy(contigFlagPDimPDe, contigFlagPDimPDeArg, sizeof(int)*dimCount*deCount);
  indexCountPDimPDe = new int[dimCount*deCount];
  memcpy(indexCountPDimPDe, indexCountPDimPDeArg, sizeof(int)*dimCount*deCount);
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeList = delayout->getLocalDeList();
  indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    for (int k=0; k<dimCount; k++){
      indexListPDimPLocalDe[i*dimCount+k] =
        new int[indexCountPDimPDe[de*dimCount+k]];
      memcpy(indexListPDimPLocalDe[i*dimCount+k],
        indexListPDimPLocalDeArg[i*dimCount+k],
        sizeof(int)*indexCountPDimPDe[de*dimCount+k]);
    }
  }
  // determine the cellCountPPatch
  cellCountPPatch = new int[patchCount];
  for (int i=0; i<patchCount; i++){
    cellCountPPatch[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      cellCountPPatch[i] *=
        maxIndexPDimPPatch[i*dimCount+j] - minIndexPDimPPatch[i*dimCount+j] + 1;
  }
  patchListPDe = new int[deCount];
  memcpy(patchListPDe, patchListPDeArg, sizeof(int)*deCount);
  cellCountPDe = new int[deCount];
  for (int i=0; i<deCount; i++){
    cellCountPDe[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      cellCountPDe[i] *= indexCountPDimPDe[i*dimCount+j];
    // mark in patchListPDe DEs that have no cells as not being on any patch
    if (cellCountPDe[i]==0) patchListPDe[i]=0;
  }
  // no arbitrary sequence indices by default
  arbSeqIndexCountPLocalDe = new int[localDeCount];
  arbSeqIndexListPLocalDe = new int*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    arbSeqIndexCountPLocalDe[i] = 0;
    arbSeqIndexListPLocalDe[i] = NULL;
  }
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::destruct()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::destruct
//
// !INTERFACE:
int DistGrid::destruct(void){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure of an ESMCI::DistGrid object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // garbage collection
  delete [] indexCountPDimPDe;
  delete [] minIndexPDimPPatch;
  delete [] maxIndexPDimPPatch;
  delete [] minIndexPDimPDe;
  delete [] maxIndexPDimPDe;
  delete [] cellCountPPatch;
  delete [] patchListPDe;
  delete [] cellCountPDe;
  delete [] contigFlagPDimPDe;
  int localDeCount = delayout->getLocalDeCount();
  for (int i=0; i<dimCount*localDeCount; i++)
    delete [] indexListPDimPLocalDe[i];
  delete [] indexListPDimPLocalDe;
  for (int i=0; i<connectionCount; i++)
    delete [] connectionList[i];
  if (connectionList)
    delete [] connectionList;
  delete [] arbSeqIndexCountPLocalDe;
  for (int i=0; i<localDeCount; i++)
    if (arbSeqIndexListPLocalDe[i])
      delete [] arbSeqIndexListPLocalDe[i];
  delete [] arbSeqIndexListPLocalDe;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// fill()
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::fillIndexListPDimPDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::fillIndexListPDimPDe
//
// !INTERFACE:
int DistGrid::fillIndexListPDimPDe(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int *indexList,                   // out - only on rootPet
  int de,                           // in  - DE   = {0, ..., deCount-1}
  int dim,                          // in  - dim  = {1, ..., dimCount}
  vmk_commhandle **commh,           // inout -
  int rootPet,                      // in  -
  VM *vm                            // [in] - default: currentVM
  )const{
//
// !DESCRIPTION:
//    Fill indexList with values on rootPet for de and dim.
//
//    Only rootPet must supply a sufficiently allocated indexList pointer.
//    All PETs are allowed to call into this method once for each DE/dim
//    request. Only the rootPet and the PET on which DE is local _must_ issue
//    a call to this method. If on input *commh == NULL a new commhandle will
//    be allocated internally and *commh will point to this new allocation on
//    return. This behavior can be used by the calling code to test if
//    the method did issue communications on behalf of the calling PET.
//    If on input *commh != NULL the method will reuse the already
//    allocated commhandle. In either case it is the caller's responsibility to
//    issue the appropriate wait calls and delete the commhandles when finished.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
  }
  
  // query the VM
  int localPet = vm->getLocalPet();
  // query lower classes
  const int *deList = delayout->getDeList();
      
  if (localPet == rootPet){
    // check if this DE is local or not            
    if (deList[de] == -1){
      // this DE is _not_ local -> receive indexList from respective Pet
      int srcPet;
      delayout->getDEMatchPET(de, *vm, NULL, &srcPet, 1);
      if (*commh == NULL) *commh = new vmk_commhandle;
      localrc = vm->vmk_recv(indexList,
        sizeof(int)*indexCountPDimPDe[de*dimCount+dim-1], srcPet, commh);
      if (localrc){
        char *message = new char[160];
        sprintf(message, "VMKernel/MPI error #%d\n", localrc);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
          message, &rc);
        delete [] message;
        return rc;
      }
    }else{
      // this DE _is_ local -> look up indexList locally
      const int *localIndexList =
        getIndexListPDimPLocalDe(deList[de], dim, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      memcpy(indexList, localIndexList, sizeof(int)*
        indexCountPDimPDe[de*dimCount+dim-1]);
    }
  }else{
    if (deList[de] != -1){
      // this DE _is_ local -> send indexList to rootPet
      const int *localIndexList =
        getIndexListPDimPLocalDe(deList[de], dim, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      if (*commh == NULL) *commh = new vmk_commhandle;
      localrc = vm->vmk_send(localIndexList,
        sizeof(int)*indexCountPDimPDe[de*dimCount+dim-1], rootPet, commh);
      if (localrc){
        char *message = new char[160];
        sprintf(message, "VMKernel/MPI error #%d\n", localrc);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
          message, &rc);
        delete [] message;
        return rc;
      }
    }
  }
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// print and validation class methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::print()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::print
//
// !INTERFACE:
int DistGrid::print()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of DistGrid object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", &rc);
    return rc;
  }

  // print info about the ESMC_DistGrid object
  printf("--- ESMCI::DistGrid::print start ---\n");
  printf("dimCount = %d\n", dimCount);
  printf("patchCount = %d\n", patchCount);
  printf("cellCountPPatch: ");
  for (int i=0; i<patchCount; i++)
    printf("%d ", cellCountPPatch[i]);
  printf("\n");
  printf("regDecompFlag = %s\n", ESMC_LogicalString(regDecompFlag));
  printf("patchListPDe: ");
  int deCount = delayout->getDeCount();
  for (int i=0; i<deCount; i++)
    printf("%d ", patchListPDe[i]);
  printf("\n");
  printf("cellCountPDe: ");
  for (int i=0; i<deCount; i++)
    printf("%d ", cellCountPDe[i]);
  printf("\n");
  printf("contigFlagPDimPDe (dims separated by / ):\n");
  for (int i=0; i<deCount; i++){
    printf(" for DE %d: ", i);
    for (int j=0; j<dimCount; j++){
      printf("%d / ", contigFlagPDimPDe[i*dimCount+j]);
    }
    printf("\n");
  }
  printf("(min,max)IndexPDimPDe (dims separated by / ):\n");
  for (int i=0; i<deCount; i++){
    printf(" for DE %d: ", i);
    for (int j=0; j<dimCount; j++){
      printf("(%d, %d) / ", minIndexPDimPDe[i*dimCount+j],
        maxIndexPDimPDe[i*dimCount+j]);
    }
    printf("\n");
  }
  printf("indexListPDimPLocalDe (dims separated by / ):\n");
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeList = delayout->getLocalDeList();
  for (int i=0; i<localDeCount; i++){
    printf(" for localDE %d - DE %d: ", i, localDeList[i]);
    for (int j=0; j<dimCount; j++){
      printf(" (");
      for (int k=0; k<indexCountPDimPDe[localDeList[i]*dimCount+j]; k++){
        if (k!=0) printf(", ");
        printf("%d", indexListPDimPLocalDe[i*dimCount+j][k]);
      }
      printf(") /");
    }
    printf("\n");
  }
  printf("arbSeqIndexListPLocalDe:\n");
  for (int i=0; i<localDeCount; i++){
    printf(" for localDE %d - DE %d: ", i, localDeList[i]);
    if (arbSeqIndexCountPLocalDe[i]){
      printf("(");
      for (int j=0; j<arbSeqIndexCountPLocalDe[i]; j++){
        if (j!=0) printf(", ");
        printf("%d", arbSeqIndexListPLocalDe[i][j]);
      }
      printf(")\n");
    }else
      printf(" default\n");
  }
      
  printf("connectionCount = %d\n", connectionCount);
  printf("~ lower class' values ~\n");
  printf("deCount = %d\n", deCount);
  printf("localPet = %d\n", vm->getLocalPet());
  printf("petCount = %d\n", vm->getPetCount());
  printf("--- ESMCI::DistGrid::print end ---\n");

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::validate()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::validate
//
// !INTERFACE:
int DistGrid::validate()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Validate details of DistGrid object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check against NULL pointer
  if (this == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
       " - 'this' pointer is NULL.", &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// get() and set()
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getContigFlagPDimPDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getContigFlagPDimPDe
//
// !INTERFACE:
int DistGrid::getContigFlagPDimPDe(
//
// !RETURN VALUE:
//    int contigFlag for de and dim
//
// !ARGUMENTS:
//
  int de,                           // in  - DE   = {0, ..., deCount-1}
  int dim,                          // in  - dim  = {1, ..., dimCount}
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  int deCount = delayout->getDeCount();
  if (de < 0 || de > deCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified DE out of bounds", rc);
    return NULL;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified dim out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return contigFlagPDimPDe[de*dimCount+(dim-1)];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getCellCountPDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getCellCountPDe
//
// !INTERFACE:
int DistGrid::getCellCountPDe(
//
// !RETURN VALUE:
//    int cellCount for DE
//
// !ARGUMENTS:
//
  int de,                               // in  - DE   = {0, ..., deCount-1}
  int *rc                               // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  int deCount = delayout->getDeCount();
  if (de < 0 || de > deCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified DE out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return cellCountPDe[de];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getSequenceIndex()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getSequenceIndex
//
// !INTERFACE:
int DistGrid::getSequenceIndex(
//
// !RETURN VALUE:
//    int sequence index
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  int *index,                       // in  - DE-local index tupple in exclusive
                                    //       region basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", rc);
    return -1;
  }
  
  int seqindex;
  if (arbSeqIndexCountPLocalDe[localDe]){
    // determine the sequentialized index by arbSeqIndexListPLocalDe look-up
    int linExclusiveIndex = index[dimCount-1];  // initialize
    for (int i=dimCount-2; i>=0; i--){
      linExclusiveIndex *= indexCountPDimPDe[localDe*dimCount + i];
      linExclusiveIndex += index[i];
    }
    seqindex = arbSeqIndexListPLocalDe[localDe][linExclusiveIndex];
  }else{
    // determine the sequentialized index by construction of default patch rule
    const int *localDeList = delayout->getLocalDeList();
    int patch = patchListPDe[localDeList[localDe]];  // patches are basis 1 !!!!
    // add up cells from patch in which DE is located
    seqindex =
      indexListPDimPLocalDe[localDe*dimCount+(dimCount-1)][index[dimCount-1]]
      - minIndexPDimPPatch[(patch-1)*dimCount+(dimCount-1)]; // initialize
    for (int i=dimCount-2; i>=0; i--){
      seqindex *= maxIndexPDimPPatch[(patch-1)*dimCount+i] 
        - minIndexPDimPPatch[(patch-1)*dimCount+i] + 1;
      seqindex += indexListPDimPLocalDe[localDe*dimCount+i][index[i]] 
        - minIndexPDimPPatch[(patch-1)*dimCount+i];
    }
    // add all the cells of previous patches
    for (int i=0; i<patchListPDe[localDeList[localDe]]-2; i++)
      seqindex += cellCountPPatch[i];
    ++seqindex;  // shift sequentialized index to basis 1 !!!!
  }
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqindex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMinIndexPDimPPatch()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMinIndexPDimPPatch
//
// !INTERFACE:
const int *DistGrid::getMinIndexPDimPPatch(
//
// !RETURN VALUE:
//    int *minIndex for patch
//
// !ARGUMENTS:
//
  int patch,                            // in  - patch   = {1, ..., patchCount}
  int *rc                               // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified patch out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &minIndexPDimPPatch[(patch-1)*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMaxIndexPDimPPatch()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMaxIndexPDimPPatch
//
// !INTERFACE:
const int *DistGrid::getMaxIndexPDimPPatch(
//
// !RETURN VALUE:
//    int *maxIndex for patch
//
// !ARGUMENTS:
//
  int patch,                            // in  - patch   = {1, ..., patchCount}
  int *rc                               // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified patch out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &maxIndexPDimPPatch[(patch-1)*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMinIndexPDimPDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMinIndexPDimPDe
//
// !INTERFACE:
const int *DistGrid::getMinIndexPDimPDe(
//
// !RETURN VALUE:
//    int *minIndex for de
//
// !ARGUMENTS:
//
  int de,                               // in  - de   = {0, ..., deCount-1}
  int *rc                               // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  if (de < 0 || de > delayout->getDeCount()-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified de out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &minIndexPDimPDe[de*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMaxIndexPDimPDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMaxIndexPDimPDe
//
// !INTERFACE:
const int *DistGrid::getMaxIndexPDimPDe(
//
// !RETURN VALUE:
//    int *maxIndex for de
//
// !ARGUMENTS:
//
  int de,                               // in  - de   = {0, ..., deCount-1}
  int *rc                               // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check input
  if (de < 0 || de > delayout->getDeCount()-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified de out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &maxIndexPDimPDe[de*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getIndexListPDimPLocalDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getIndexListPDimPLocalDe
//
// !INTERFACE:
const int *DistGrid::getIndexListPDimPLocalDe(
//
// !RETURN VALUE:
//    int *indexListPDimPLocalDe for localDe, dim
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  int dim,                          // in  - dim  = {1, ..., dimCount}
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", rc);
    return NULL;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified dim out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return indexListPDimPLocalDe[localDe*dimCount+(dim-1)];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getArbSeqIndexListPLocalDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getArbSeqIndexListPLocalDe
//
// !INTERFACE:
const int *DistGrid::getArbSeqIndexListPLocalDe(
//
// !RETURN VALUE:
//    int *arbSeqIndexListPLocalDe for localDe
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arbSeqIndexListPLocalDe[localDe];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// Serialize/Deserialize functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::serialize()"
//BOPI
// !IROUTINE:  ESMC::DistGrid::serialize - Turn distgrid into a byte stream
//
// !INTERFACE:
int DistGrid::serialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length; realloc'd here if needed
  int *offset            // inout - original offset, updated to point 
                             //  to first free byte after current obj info
  )const{
//
// !DESCRIPTION:
//    Turn info in distgrid object into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int fixedpart, nbytes;
  int i, j;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  VM **vp;
  ESMC_DePinFlag *dp;
  de_type *dep;

  // TODO: we cannot reallocate from C++ if the original buffer is
  //  allocated on the f90 side.  change the code to make the allocate
  //  happen in C++; then this will be fine.  (for now make sure buffer
  //  is always big enough so realloc is not needed.)
  fixedpart = sizeof(DistGrid);
  if ((*length - *offset) < fixedpart) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
      "Buffer too short to add a DistGrid object", &rc);
    return rc;
  }

  // fixedpart = sizeof(DELayout);
  // if ((*length - *offset) < fixedpart) {
  //     buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
  //     *length += 2 * fixedpart;
  //  }

  // first set the base part of the object
  localrc = this->ESMC_Base::ESMC_Serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // serialize the DELayout
  localrc = delayout->serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // figure current position in buffer
  cp = (char *)(buffer + *offset);
  
  // serialize scalar members
  ip = (int *)cp;
  *ip++ = dimCount;
  *ip++ = patchCount;
  *ip++ = connectionCount;
  lp = (ESMC_Logical *)ip;
  *lp++ = regDecompFlag;
  
  int deCount = delayout->getDeCount();
  // serialize array members
  ip = (int *)lp;
  for (int i=0; i<patchCount; i++)
    *ip++ = cellCountPPatch[i];
  for (int i=0; i<deCount; i++)
    *ip++ = patchListPDe[i];
  for (int i=0; i<dimCount*patchCount; i++)
    *ip++ = minIndexPDimPPatch[i];
  for (int i=0; i<dimCount*patchCount; i++)
    *ip++ = maxIndexPDimPPatch[i];
  for (int i=0; i<dimCount*deCount; i++)
    *ip++ = indexCountPDimPDe[i];
  
  cp = (char *)ip;
  *offset = (cp - buffer);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::deserialize - Turn a byte stream into an object
//
// !INTERFACE:
DistGrid *DistGrid::deserialize(
//
// !RETURN VALUE:
//    DistGrid * to deserialized proxy object
//
// !ARGUMENTS:
  char *buffer,          // in - byte stream to read
  int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  DistGrid *a = new DistGrid;
  int fixedpart, nbytes;
  int i, j;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  VM **vp;
  ESMC_DePinFlag *dp;
  de_type *dep;

  // first get the base part of the object
  localrc = a->ESMC_Base::ESMC_Deserialize(buffer, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return NULL;
  a->delayout = DELayout::deserialize(buffer, offset);
  a->vm = NULL; // VM must be reset

  // now the rest
  cp = (char *)(buffer + *offset);
  
  // deserialize scalar members
  ip = (int *)cp;
  a->dimCount = *ip++;
  a->patchCount = *ip++;
  a->connectionCount = *ip++;
  lp = (ESMC_Logical *)ip;
  a->regDecompFlag = *lp++;
      
  int deCount = a->delayout->getDeCount();
  // deserialize array members
  ip = (int *)lp;
  a->cellCountPPatch = new int[a->patchCount];
  for (int i=0; i<a->patchCount; i++)
    a->cellCountPPatch[i] = *ip++;
  a->patchListPDe = new int[deCount];
  for (int i=0; i<deCount; i++)
    a->patchListPDe[i] = *ip++;
  a->minIndexPDimPPatch = new int[a->dimCount*a->patchCount];
  for (int i=0; i<a->dimCount*a->patchCount; i++)
    a->minIndexPDimPPatch[i] = *ip++;
  a->maxIndexPDimPPatch = new int[a->dimCount*a->patchCount];
  for (int i=0; i<a->dimCount*a->patchCount; i++)
    a->maxIndexPDimPPatch[i] = *ip++;
  a->indexCountPDimPDe = new int[a->dimCount*deCount];
  for (int i=0; i<a->dimCount*deCount; i++)
    a->indexCountPDimPDe[i] = *ip++;
  
  cp = (char *)ip;
  *offset = (cp - buffer);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return a;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// Connection functions
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::connection()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::connection
//
// !INTERFACE:
int DistGrid::connection(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *connection,         // out -
  int patchIndexA,                  // in  -
  int patchIndexB,                  // in  -
  InterfaceInt *positionVector,     // in -
  InterfaceInt *orientationVector   // in -
  ){    
//
// !DESCRIPTION:
//    Construct a connection element
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check connetion argument
  if (connection == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to connection array", &rc);
    return rc;
  }
  if (connection->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- connection array must be of rank 1", &rc);
    return rc;
  }
  int dimCount = (connection->extent[0]-2)/2;
  if (dimCount <= 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dimension of connection array must be of size "
      "(2 * dimCount + 2)", &rc);
    return rc;
  }
  
  // fill in the patch indices
  connection->array[0] = patchIndexA;
  connection->array[1] = patchIndexB;
  
  // check positionVector argument
  if (positionVector == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to positionVector array", &rc);
    return rc;
  }
  if (positionVector->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- positionVector array must be of rank 1", &rc);
    return rc;
  }
  if (positionVector->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dimension of positionVector array must be of size dimCount", &rc);
    return rc;
  }
  
  // fill in the positionVector
  memcpy(&(connection->array[2]), positionVector->array,
    sizeof(int)*dimCount);
  
  // check on orientationVector
  if (orientationVector != NULL){
    // orientationVector was provided
    if (orientationVector->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- orientationVector array must be of rank 1", &rc);
      return rc;
    }
    if (orientationVector->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of orientationVector array must be of size dimCount",
        &rc);
      return rc;
    }
    // fill in the orientationVector
    memcpy(&(connection->array[2+dimCount]), orientationVector->array,
      sizeof(int)*dimCount);
  }else{
    // orientationVector was not provided -> fill in default orientation
    for (int i=0; i<dimCount; i++)
      connection->array[2+dimCount+i] = i+1;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::setArbSeqIndex()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::setArbSeqIndex
//
// !INTERFACE:
//
int DistGrid::setArbSeqIndex(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *arbSeqIndex // in
  ){
//
// !DESCRIPTION:
//    Set the array of arbitrary indicies
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  //TODO: This routine is kind of strange and needs to be refactored:
  //      1) InterfaceInt types should not make it into Set() or Get() methods.
  //      2) The routine assumes special case 1 DE per PET.
  
  // check that the conditions are met for this routine to make sense
  int localDeCount = delayout->getLocalDeCount();
  if (localDeCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
      "- This routine is only implemented for 1 DE per PET case", &rc);
    return rc;
  }

  // check input
  if (arbSeqIndex == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to arbSeqIndex array", &rc);
    return rc;
  }
  if (arbSeqIndex->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- arbSeqIndex array must be of rank 1", &rc);
    return rc;
  }
  const int *localDeList = delayout->getLocalDeList();
  if (arbSeqIndex->extent[0] != cellCountPDe[localDeList[0]]){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- arbSeqIndex array must supply one value for each cell in local DE",
      &rc);
    return rc;
  }

  // set arbSeqIndexCountPLocalDe[] and arbSeqIndexListPLocalDe[][]
  arbSeqIndexCountPLocalDe[0] = arbSeqIndex->extent[0];
  if (arbSeqIndexListPLocalDe[0])
    delete [] arbSeqIndexListPLocalDe[0];  // delete previous index list
  arbSeqIndexListPLocalDe[0] = new int[arbSeqIndex->extent[0]];
  memcpy(arbSeqIndexListPLocalDe[0], arbSeqIndex->array, 
    sizeof(int)*arbSeqIndex->extent[0]);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI
