// $Id: ESMC_DistGrid.C,v 1.20 2007/06/22 20:52:31 theurich Exp $
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
static const char *const version = "$Id: ESMC_DistGrid.C,v 1.20 2007/06/22 20:52:31 theurich Exp $";
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
  // local vars
  int localrc;                // local return code
   
  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new DistGrid object
  DistGrid *distgrid;
  try{
    distgrid = new DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call DistGridConstruct
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
  
  // setup temporary dimExtent and indexList arrays for DistGridConstruct()
  // also setup temporary dimCongtigFlag array for DistGridConstruct()
  int *dimExtent = new int[dimCount*deCount];
  int **indexList = new int*[dimCount*deCount];
  int *dimContigFlag = new int[dimCount*deCount];
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
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest) ++dimExtent[extentIndex]; // distr. rest
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk < chunkRest) indexStart += decompChunk;
          else indexStart += chunkRest;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            indexList[extentIndex][k] = indexStart + k; // block structure
          }
          dimContigFlag[extentIndex] = 1; // flag contiguous dimension
        }
        break;
      case DECOMP_RESTLAST:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk == regDecomp->array[i]-1) 
            dimExtent[extentIndex] += chunkRest; // add rest to last chunk
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            indexList[extentIndex][k] = indexStart + k; // block structure
          }
          dimContigFlag[extentIndex] = 1; // flag contiguous dimension
        }
        break;
      case DECOMP_RESTFIRST:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk == 0) 
            dimExtent[extentIndex] += chunkRest; // add rest to first chunk
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk > 0) indexStart += chunkRest;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            indexList[extentIndex][k] = indexStart + k; // block structure
          }
          dimContigFlag[extentIndex] = 1; // flag contiguous dimension
        }
        break;
      case DECOMP_CYCLIC:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest) ++dimExtent[extentIndex]; // distr. rest
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minIndex->array[i] + decompChunk;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            // cyclic
            indexList[extentIndex][k] = indexStart + k * regDecomp->array[i];
          }
          dimContigFlag[extentIndex] = 0; // flag non-contiguous dimension
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
  // set up dePatchList
  int *dePatchList = new int[deCount];
  for (int i=0; i<deCount; i++)
    dePatchList[i] = 1;
  
  // call into construct()
  localrc = distgrid->construct(dimCount, 1, dePatchList,
    minIndex->array, maxIndex->array, dimContigFlag, dimExtent, indexList,
    ESMF_TRUE, connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] dimContigFlag;
  delete [] dimExtent;
  for (int i=0; i<dimCount*deCount; i++)
    delete [] indexList[i];
  delete [] indexList;
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
  delete [] dePatchList;
    
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
  // local vars
  int localrc;                // local return code
   
  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new DistGrid object
  DistGrid *distgrid;
  try{
    distgrid = new DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call DistGridConstruct
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
  
  // setup temporary dimExtent and indexList arrays for DistGridConstruct()
  // also setup temporary dimCongtigFlag array for DistGridConstruct()
  int *dimExtent = new int[dimCount*deCount];
  int **indexList = new int*[dimCount*deCount];
  int *dimContigFlag = new int[dimCount*deCount];
  for (int i=0; i<dimCount; i++){
    int dimLength = maxIndex->array[i] - minIndex->array[i] + 1;
    int de, extentIndex, deBlockIndexMin, deBlockIndexMax;
    for (int j=0; j<deCount; j++){
      de = deLabelList->array[j];
      extentIndex = de*dimCount+i;  // index into temp. arrays
      deBlockIndexMin = j*deBlockList->extent[0]*deBlockList->extent[1] + i;
      deBlockIndexMax = deBlockIndexMin + deBlockList->extent[0];
      dimExtent[extentIndex] = deBlockList->array[deBlockIndexMax]
        - deBlockList->array[deBlockIndexMin] + 1;
      indexList[extentIndex] = new int[dimExtent[extentIndex]];
      for (int k=0; k<dimExtent[extentIndex]; k++)
        indexList[extentIndex][k] =  deBlockList->array[deBlockIndexMin] + k;
    }
    dimContigFlag[extentIndex] = 1; // flag contiguous dimension
  }
  // set up dePatchList
  int *dePatchList = new int[deCount];
  for (int i=0; i<deCount; i++)
    dePatchList[i] = 1;

  // todo: check for overlapping deBlocks!!
  // call into construct()
  localrc = distgrid->construct(dimCount, 1, dePatchList, 
    minIndex->array, maxIndex->array, dimContigFlag, dimExtent, indexList,
    ESMF_FALSE, connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }

    
  // garbage collection
  delete [] dimContigFlag;
  delete [] dimExtent;
  for (int i=0; i<dimCount*deCount; i++)
    delete [] indexList[i];
  delete [] indexList;
  if (deLabelListDeleteFlag){
    delete [] deLabelList->array;
    delete deLabelList;
  }
  delete [] dePatchList;
  
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
  // local vars
  int localrc;                // local return code
   
  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
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
  // local vars
  int localrc;                // local return code
   
  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new DistGrid object
  DistGrid *distgrid;
  try{
    distgrid = new DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call DistGridConstruct
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
    
  // setup temporary dimExtent and indexList arrays for DistGridConstruct()
  // also setup temporary dimCongtigFlag array for DistGridConstruct()
  int *dimExtent = new int[dimCount*deCount];
  int **indexList = new int*[dimCount*deCount];
  int *dimContigFlag = new int[dimCount*deCount];
  
  // the following differs from the single patch case in that there is an
  // extra outer loop over patches. The dimExtent and indexList arrays are
  // on DE basis, independent of patches.

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
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest) ++dimExtent[extentIndex]; //distr. rest
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              indexList[extentIndex][k] = indexStart + k; // block structure
            }
            dimContigFlag[extentIndex] = 1; // flag contiguous dimension
          }
          break;
        case DECOMP_RESTLAST:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == regDecomp->array[i]-1) 
              dimExtent[extentIndex] += chunkRest; // add rest to last chunk
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              indexList[extentIndex][k] = indexStart + k; // block structure
            }
            dimContigFlag[extentIndex] = 1; // flag contiguous dimension
          }
          break;
        case DECOMP_RESTFIRST:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == 0) 
              dimExtent[extentIndex] += chunkRest; // add rest to first chunk
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk > 0) indexStart += chunkRest;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              indexList[extentIndex][k] = indexStart + k; // block structure
            }
            dimContigFlag[extentIndex] = 1; // flag contiguous dimension
          }
          break;
        case DECOMP_CYCLIC:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest) ++dimExtent[extentIndex]; //distr. rest
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minIndex->array[i] + decompChunk;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              // cyclic
              indexList[extentIndex][k] = indexStart + k * regDecomp->array[i];
            }
            dimContigFlag[extentIndex] = 0; // flag non-contiguous dimension
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
  // set up dePatchList
  dePatchStart = 0;  // reset  
  int *dePatchList = new int[deCount];
  for (int patch=0; patch<patchCount; patch++){
    for (int jj=0; jj<deCountPPatch[patch]; jj++){
      int j = dePatchStart + jj;
      int de = deLabelList->array[j];
      dePatchList[de] = patch + 1;  // patch ids are basis 1
    }
    dePatchStart += deCountPPatch[patch];
  }

  // call into construct()
  localrc = distgrid->construct(dimCount, patchCount, dePatchList,
    minIndex->array, maxIndex->array, dimContigFlag, dimExtent, indexList,
    ESMF_TRUE, connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] dimContigFlag;
  delete [] deCountPPatch;
  delete [] dimExtent;
  for (int i=0; i<dimCount*deCount; i++)
    delete [] indexList[i];
  delete [] indexList;
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
  delete [] dePatchList;
  
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

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
  int *dePatchListArg,                  // (in)
  int *minIndexArg,                     // (in)
  int *maxIndexArg,                     // (in)
  int *dimContigFlagArg,                // (in)
  int *dimExtentArg,                    // (in)
  int **indexListArg,                   // (in)
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

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
  // fill in cached values
  deCount = delayout->getDeCount();
  localDeCount = delayout->getLocalDeCount();
  localDeList = delayout->getLocalDeList();
  localPet = vm->getMypet();
  petCount = vm->getNpets();
  // fill in the rest
  minIndex = new int[dimCount*patchCount];
  memcpy(minIndex, minIndexArg, sizeof(int)*dimCount*patchCount);
  maxIndex = new int[dimCount*patchCount];
  memcpy(maxIndex, maxIndexArg, sizeof(int)*dimCount*patchCount);
  dimContigFlag = new int[dimCount*deCount];
  memcpy(dimContigFlag, dimContigFlagArg, sizeof(int)*dimCount*deCount);
  dimExtent = new int[dimCount*deCount];
  memcpy(dimExtent, dimExtentArg, sizeof(int)*dimCount*deCount);
  indexList = new int*[dimCount*deCount];
  for (int i=0; i<dimCount*deCount; i++){
    indexList[i] = new int[dimExtent[i]];
    memcpy(indexList[i], indexListArg[i], sizeof(int)*dimExtent[i]);
  }
  // TODO: eventually get localIndexList from DistGrid::create() routines
  // directly
  localIndexList = new int*[dimCount*localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    for (int k=0; k<dimCount; k++){
      localIndexList[i*dimCount+k] = new int[dimExtent[de*dimCount+k]];
      memcpy(localIndexList[i*dimCount+k], indexListArg[de*dimCount+k],
        sizeof(int)*dimExtent[de*dimCount+k]);
    }
  }
  // determine the patchCellCount
  patchCellCount = new int[patchCount];
  for (int i=0; i<patchCount; i++){
    patchCellCount[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      patchCellCount[i] *=
        (maxIndex[i*dimCount+j] - minIndex[i*dimCount+j] + 1);
  }
  dePatchList = new int[deCount];
  memcpy(dePatchList, dePatchListArg, sizeof(int)*deCount);
  deCellCount = new int[deCount];
  for (int i=0; i<deCount; i++){
    deCellCount[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      deCellCount[i] *= dimExtent[i*dimCount+j];
    // mark in dePatchList DEs that have no cells as not being part of any patch
    if (deCellCount[i]==0) dePatchList[i]=0;
  }

  // By default there are no arbitrary indices
  arbIdxCount = 0;
  localArbIndices = NULL;
  
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // garbage collection
  delete [] dimExtent;
  delete [] minIndex;
  delete [] maxIndex;
  delete [] patchCellCount;
  delete [] dePatchList;
  delete [] deCellCount;
  delete [] dimContigFlag;
  for (int i=0; i<dimCount*deCount; i++)
    delete [] indexList[i];
  delete [] indexList;
  for (int i=0; i<dimCount*localDeCount; i++)
    delete [] localIndexList[i];
  delete [] localIndexList;
  for (int i=0; i<connectionCount; i++)
    delete [] connectionList[i];
  if (connectionList)
    delete [] connectionList;

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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

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
  printf("patchCellCount: ");
  for (int i=0; i<patchCount; i++)
    printf("%d ", patchCellCount[i]);
  printf("\n");
  printf("regDecompFlag = %s\n", ESMC_LogicalString(regDecompFlag));
  printf("dePatchList: ");
  for (int i=0; i<deCount; i++)
    printf("%d ", dePatchList[i]);
  printf("\n");
  printf("deCellCount: ");
  for (int i=0; i<deCount; i++)
    printf("%d ", deCellCount[i]);
  printf("\n");
  printf("indexList:\n");
  for (int i=0; i<deCount; i++){
    printf("DE %d - ", i);
    for (int j=0; j<dimCount; j++){
      printf(" (");
      for (int k=0; k<dimExtent[i*dimCount+j]; k++){
        if (k!=0) printf(", ");
        printf("%d", indexList[i*dimCount+j][k]);
      }
      printf(") /");
    }
    printf("\n");
  }
  printf("connectionCount = %d\n", connectionCount);
  printf("~ cached values ~\n");
  printf("deCount = %d\n", deCount);
  printf("localPet = %d\n", localPet);
  printf("petCount = %d\n", petCount);
  printf("--- ESMCI::DistGrid::print end ---\n");

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
#define ESMC_METHOD "ESMCI::DistGrid::getDimContigFlag()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getDimContigFlag
//
// !INTERFACE:
int DistGrid::getDimContigFlag(
//
// !RETURN VALUE:
//    int dimContigFlag for de and dim
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
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // check input
  if (de < 0 || de > deCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified DE out of bounds", rc);
    return NULL;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified dim out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return dimContigFlag[de*dimCount+(dim-1)];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getDeCellCount()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getDeCellCount
//
// !INTERFACE:
int DistGrid::getDeCellCount(
//
// !RETURN VALUE:
//    int deCellCount for DE
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
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // check input
  if (de < 0 || de > deCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified DE out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return deCellCount[de];
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
  int de,                           // in - DE
  int *index                        // in - DE-local index tupple in exclusive
                                    //      region basis 0
  )const{
//
// !DESCRIPTION:
//    Get sequential index
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // determine the sequentialized index
  int patch = dePatchList[de];  // patches are basis 1 !!!!
  int seqindex = indexList[de*dimCount+(dimCount-1)][index[dimCount-1]]
    - minIndex[(patch-1)*dimCount+(dimCount-1)]; // initialize
  for (int i=dimCount-2; i>=0; i--){
    // add cells in the patch in which DE is located
    seqindex *= maxIndex[(patch-1)*dimCount+i] 
      - minIndex[(patch-1)*dimCount+i] + 1;
    seqindex += indexList[de*dimCount+i][index[i]] 
      - minIndex[(patch-1)*dimCount+i];
  }
  for (int i=0; i<dePatchList[de]-2; i++)
    seqindex += patchCellCount[i];  // add all the cells of previous patches
  
  return seqindex+1;  // shift sequentialized index to basis 1 !!!!
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getSequenceDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getSequenceDe
//
// !INTERFACE:
int DistGrid::getSequenceDe(
//
// !RETURN VALUE:
//    int DE that covers sequential index
//
// !ARGUMENTS:
//
  int seqindex                      // in - sequential index basis 1
  )const{
//
// !DESCRIPTION:
//    Get DE that covers sequential index
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  // determine the DE that covers sequentialized index
  
  // find that patch that contains seqindex
  int cellSum=0; // reset
  int p;          // this will be patch index basis 0
  for (p=0; p<patchCount; p++)
    if (seqindex <= cellSum + patchCellCount[p]) break;
  if (p >= patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Could not find seqindex in DistGrid", &rc);
    return -1;  // indicate problem
  }
  seqindex -= cellSum;  // shift into patch-local sequential index
  seqindex -= 1;        // shift into basis 0
  // find the patch-local index tuple for seqindex
  int *ii = new int[dimCount];
  for (int i=dimCount-1; i>=0; i--){
    int delta = 1; // reset
    for (int j=0; j<i; j++)
      delta *= maxIndex[p*dimCount+j] - minIndex[p*dimCount+j] + 1;
    ii[i] = seqindex / delta;
    seqindex = seqindex % delta;
  }
  // shift origin of ii[] onto patch origin
  for (int i=0; i<dimCount; i++)
    ii[i] += minIndex[p*dimCount+i];
  // find the DE that covers this patch-local index tuple
  int de;
  for (de=0; de<deCount; de++){
    if (dePatchList[de] != p+1) continue; // DE not in correct patch
    int dim;
    for (dim=0; dim<dimCount; dim++){
      int i;
      for (i=0; i<dimExtent[de*dimCount+dim]; i++){
        if (indexList[de*dimCount+dim][i] == ii[dim]) break;  // dim match
      }
      if (i==dimExtent[de*dimCount+dim])break; // did not find dim match
    }
    if (dim==dimCount) break; // found matching DE
  }
  // garbage collection
  delete [] ii;
  // error checking
  if (de == deCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Could not find DE in DistGrid that covers seqindex", &rc);
    return -1;  // indicate problem
  }
    
  return de;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMinIndex()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMinIndex
//
// !INTERFACE:
const int *DistGrid::getMinIndex(
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
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // check input
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified patch out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &minIndex[(patch-1)*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMaxIndex()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMaxIndex
//
// !INTERFACE:
const int *DistGrid::getMaxIndex(
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
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
  
  // check input
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified patch out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &maxIndex[(patch-1)*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getLocalIndexList()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getLocalIndexList
//
// !INTERFACE:
const int *DistGrid::getLocalIndexList(
//
// !RETURN VALUE:
//    int *localIndexList for de, dim
//
// !ARGUMENTS:
//
  int de,                           // in  - local DE = {0, ..., localDeCount-1}
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
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

  // check input
  if (de < 0 || de > localDeCount-1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified DE out of bounds", rc);
    return NULL;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified dim out of bounds", rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return localIndexList[de*dimCount+(dim-1)];
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

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
  *ip++ = deCount;
  lp = (ESMC_Logical *)ip;
  *lp++ = regDecompFlag;
  
  // serialize array members
  ip = (int *)lp;
  for (int i=0; i<patchCount; i++)
    *ip++ = patchCellCount[i];
  for (int i=0; i<deCount; i++)
    *ip++ = dePatchList[i];
  for (int i=0; i<dimCount*patchCount; i++)
    *ip++ = minIndex[i];
  for (int i=0; i<dimCount*patchCount; i++)
    *ip++ = maxIndex[i];
  for (int i=0; i<dimCount*deCount; i++)
    *ip++ = dimExtent[i];
  for (int i=0; i<dimCount*deCount; i++)
    for (int k=0; k<dimExtent[i]; k++)
      *ip++ = indexList[i][k];

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
    DistGrid *a = new DistGrid;
    int fixedpart, nbytes, rc;
    int i, j;
    char *cp;
    int *ip;
    ESMC_Logical *lp;
    VM **vp;
    ESMC_DePinFlag *dp;
    de_type *dep;

    // Initialize return code; assume routine not implemented
    rc = ESMC_RC_NOT_IMPL;
    // first get the base part of the object
    rc = a->ESMC_Base::ESMC_Deserialize(buffer, offset);
    a->delayout = DELayout::deserialize(buffer, offset);
    a->vm = NULL; // VM must be reset

    // now the rest
    cp = (char *)(buffer + *offset);
    
    // deserialize scalar members
    ip = (int *)cp;
    a->dimCount = *ip++;
    a->patchCount = *ip++;
    a->connectionCount = *ip++;
    a->deCount = *ip++;
    lp = (ESMC_Logical *)ip;
    a->regDecompFlag = *lp++;
    
    // set scalars that must be reset here
    a->localPet = -1;   // reset
    a->petCount = -1;   // reset
    
    // deserialize array members
    ip = (int *)lp;
    a->patchCellCount = new int[a->patchCount];
    for (int i=0; i<a->patchCount; i++)
      a->patchCellCount[i] = *ip++;
    a->dePatchList = new int[a->deCount];
    for (int i=0; i<a->deCount; i++)
      a->dePatchList[i] = *ip++;
    a->minIndex = new int[a->dimCount*a->patchCount];
    for (int i=0; i<a->dimCount*a->patchCount; i++)
      a->minIndex[i] = *ip++;
    a->maxIndex = new int[a->dimCount*a->patchCount];
    for (int i=0; i<a->dimCount*a->patchCount; i++)
      a->maxIndex[i] = *ip++;
    a->dimExtent = new int[a->dimCount*a->deCount];
    for (int i=0; i<a->dimCount*a->deCount; i++)
      a->dimExtent[i] = *ip++;
    a->indexList = new int*[a->dimCount*a->deCount];
    for (int i=0; i<a->dimCount*a->deCount; i++){
      a->indexList[i] = new int[a->dimExtent[i]];
      for (int k=0; k<a->dimExtent[i]; k++)
        a->indexList[i][k] = *ip++;
    }

    cp = (char *)ip;
    *offset = (cp - buffer);
   
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

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
#define ESMC_METHOD "ESMCI::DistGrid::setArbIdx()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::setArbIdx
//
// !INTERFACE:
//
int DistGrid::setArbIdx(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *arbIndices // in
  ){
//
// !DESCRIPTION:
//    Set the array of arbitrary indicies
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

  if (arbIndices->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- arbIndices array must be of rank 1", &rc);
    return rc;
  }

  arbIdxCount = arbIndices->extent[0];
  localArbIndices = arbIndices->array;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI
