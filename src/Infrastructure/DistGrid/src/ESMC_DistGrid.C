// $Id: ESMC_DistGrid.C,v 1.9 2007/03/28 21:55:50 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
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

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMC_Base.h" 
#include "ESMC_VM.h"
#include "ESMC_DELayout.h"

// include associated class definition
#include "ESMC_DistGrid.h"

// LogErr
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_DistGrid.C,v 1.9 2007/03/28 21:55:50 theurich Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// This section includes all the DistGrid routines
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// external Create and Destroy functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridCreate()"
//BOP
// !IROUTINE:  ESMC_DistGridCreate
//
// !INTERFACE:
ESMC_DistGrid *ESMC_DistGridCreate(
//
// !RETURN VALUE:
//    ESMC_DistGrid * to newly allocated ESMC_DistGrid
//
// !ARGUMENTS:
//
  ESMC_InterfaceInt *minCorner,          // (in)
  ESMC_InterfaceInt *maxCorner,          // (in)
  ESMC_InterfaceInt *regDecomp,          // (in)
  ESMC_DecompFlag *decompflag,                // (in)
  int decompflagCount,                        // (in)
  ESMC_InterfaceInt *deLabelList,        // (in)
  ESMC_IndexFlag *indexflag,                  // (in)
  ESMC_InterfaceInt *connectionList,     // (in)
  ESMC_InterfaceInt *connectionTransformList, // (in)
  ESMC_DELayout *delayout,                    // (in)
  ESMC_VM *vm,                                // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // allocate the new DistGrid object
  ESMC_DistGrid *distgrid;
  try{
    distgrid = new ESMC_DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call DistGridConstruct
  if (minCorner == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minCorner array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxCorner == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxCorner array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (minCorner->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- minCorner array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxCorner->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- maxCorner array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int dimCount = minCorner->extent[0];
  if (maxCorner->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minCorner and maxCorner array mismatch", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = ESMC_VMGetCurrent(&status);  // get current VM for default
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  int petCount;
  status=vm->ESMC_VMGet(NULL, &petCount, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
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
    delayout = ESMC_DELayoutCreate(&deCount, NULL, NULL, NULL, vm, &status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> check deCount
    status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
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
    regDecomp = new ESMC_InterfaceInt(dummy, 1, &dimCount);
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
    decompflag = new ESMC_DecompFlag[dimCount];
    for (int i=0; i<dimCount; i++)
      decompflag[i] = ESMF_DECOMP_DEFAULT;
  }
  if (decompflagCount != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minCorner and maxCorner arrays", rc);
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
    deLabelList = new ESMC_InterfaceInt(dummy, 1, &deCount);
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
  int *dimExtent = new int[dimCount*deCount];
  int **indexList = new int*[dimCount*deCount];
  int deDivider = 1;  // reset
  for (int i=0; i<dimCount; i++){
    int dimLength = maxCorner->array[i] - minCorner->array[i] + 1;
    int chunkLength = dimLength/regDecomp->array[i];  // basic chunk size
    int chunkRest = dimLength%regDecomp->array[i];    // left over points
    int de, decompChunk, extentIndex;
    switch (decompflag[i]){
      case ESMF_DECOMP_DEFAULT:
      case ESMF_DECOMP_HOMOGEN:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into dimExtent array
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest) ++dimExtent[extentIndex]; // distr. rest
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minCorner->array[i] + decompChunk * chunkLength;
          if (decompChunk < chunkRest) indexStart += decompChunk;
          else indexStart += chunkRest;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            indexList[extentIndex][k] = indexStart + k; // block structure
          }
        }
        break;
      case ESMF_DECOMP_RESTLAST:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into dimExtent array
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk == regDecomp->array[i]-1) 
            dimExtent[extentIndex] += chunkRest; // add rest to last chunk
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minCorner->array[i] + decompChunk * chunkLength;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            indexList[extentIndex][k] = indexStart + k; // block structure
          }
        }
        break;
      case ESMF_DECOMP_RESTFIRST:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into dimExtent array
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk == 0) 
            dimExtent[extentIndex] += chunkRest; // add rest to first chunk
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minCorner->array[i] + decompChunk * chunkLength;
          if (decompChunk > 0) indexStart += chunkRest;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            indexList[extentIndex][k] = indexStart + k; // block structure
          }
        }
        break;
      case ESMF_DECOMP_CYCLIC:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into dimExtent array
          dimExtent[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest) ++dimExtent[extentIndex]; // distr. rest
          indexList[extentIndex] = new int[dimExtent[extentIndex]];
          // fill the indexList for this dimension and DE
          int indexStart = minCorner->array[i] + decompChunk;
          for (int k=0; k<dimExtent[extentIndex]; k++){
            // cyclic
            indexList[extentIndex][k] = indexStart + k * regDecomp->array[i];
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
  // set up patchDeLookup
  int *patchDeLookup = new int[deCount];
  for (int i=0; i<deCount; i++)
    patchDeLookup[i] = 1;
  
  // call into DistGridConstruct
  status = distgrid->ESMC_DistGridConstruct(dimCount, 1, patchDeLookup,
    minCorner->array, maxCorner->array, dimExtent, indexList, ESMF_TRUE,
    connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
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
  delete [] patchDeLookup;
    
  // return successfully
  *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridCreate()"
//BOP
// !IROUTINE:  ESMC_DistGridCreate
//
// !INTERFACE:
ESMC_DistGrid *ESMC_DistGridCreate(
//
// !RETURN VALUE:
//    ESMC_DistGrid * to newly allocated ESMC_DistGrid
//
// !ARGUMENTS:
//
  ESMC_InterfaceInt *minCorner,          // (in)
  ESMC_InterfaceInt *maxCorner,          // (in)
  ESMC_InterfaceInt *deBlockList,        // (in)
  ESMC_InterfaceInt *deLabelList,        // (in)
  ESMC_IndexFlag *indexflag,                  // (in)
  ESMC_InterfaceInt *connectionList,     // (in)
  ESMC_InterfaceInt *connectionTransformList, // (in)
  ESMC_DELayout *delayout,                    // (in)
  ESMC_VM *vm,                                // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // allocate the new DistGrid object
  ESMC_DistGrid *distgrid;
  try{
    distgrid = new ESMC_DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call DistGridConstruct
  if (minCorner == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minCorner array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxCorner == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxCorner array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (minCorner->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- minCorner array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxCorner->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- maxCorner array must be of rank 1", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int dimCount = minCorner->extent[0];
  if (maxCorner->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minCorner and maxCorner array mismatch", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = ESMC_VMGetCurrent(&status);  // get current VM for default
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  int petCount;
  status=vm->ESMC_VMGet(NULL, &petCount, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
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
    delayout = ESMC_DELayoutCreate(&deCount, NULL, NULL, NULL, vm, &status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> check deCount
    status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
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
    deLabelList = new ESMC_InterfaceInt(dummy, 1, &deCount);
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
  int *dimExtent = new int[dimCount*deCount];
  int **indexList = new int*[dimCount*deCount];
  for (int i=0; i<dimCount; i++){
    int dimLength = maxCorner->array[i] - minCorner->array[i] + 1;
    int de, extentIndex, deBlockIndexMin, deBlockIndexMax;
    for (int j=0; j<deCount; j++){
      de = deLabelList->array[j];
      extentIndex = de*dimCount+i;  // index into dimExtent array
      deBlockIndexMin = j*deBlockList->extent[0]*deBlockList->extent[1] + i;
      deBlockIndexMax = deBlockIndexMin + deBlockList->extent[0];
      dimExtent[extentIndex] = deBlockList->array[deBlockIndexMax]
        - deBlockList->array[deBlockIndexMin] + 1;
      indexList[extentIndex] = new int[dimExtent[extentIndex]];
      for (int k=0; k<dimExtent[extentIndex]; k++)
        indexList[extentIndex][k] =  deBlockList->array[deBlockIndexMin] + k;
    }
  }
  // set up patchDeLookup
  int *patchDeLookup = new int[deCount];
  for (int i=0; i<deCount; i++)
    patchDeLookup[i] = 1;

  // todo: check for overlapping deBlocks!!
  // call into DistGridConstruct
  status = distgrid->ESMC_DistGridConstruct(dimCount, 1, patchDeLookup, 
    minCorner->array, maxCorner->array, dimExtent, indexList, ESMF_FALSE,
    connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
    
  // garbage collection
  delete [] dimExtent;
  for (int i=0; i<dimCount*deCount; i++)
    delete [] indexList[i];
  delete [] indexList;
  if (deLabelListDeleteFlag){
    delete [] deLabelList->array;
    delete deLabelList;
  }
  delete [] patchDeLookup;
  
  // return successfully
  *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridCreate()"
//BOP
// !IROUTINE:  ESMC_DistGridCreate
//
// !INTERFACE:
ESMC_DistGrid *ESMC_DistGridCreate(
//
// !RETURN VALUE:
//    ESMC_DistGrid * to newly allocated ESMC_DistGrid
//
// !ARGUMENTS:
//
  ESMC_InterfaceInt *minCorner,          // (in)
  ESMC_InterfaceInt *maxCorner,          // (in)
  ESMC_InterfaceInt *regDecomp,          // (in)
  ESMC_DecompFlag *decompflag,                // (in)
  int decompflagCount,                        // (in)
  ESMC_InterfaceInt *deLabelList,        // (in)
  ESMC_IndexFlag *indexflag,                  // (in)
  ESMC_InterfaceInt *connectionList,     // (in)
  ESMC_InterfaceInt *connectionTransformList, // (in)
  int fastAxis,                               // (in)
  ESMC_VM *vm,                                // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // create a DELayout according to fastAxis argument
  // todo: once DELayout functions exist to determine communication capabilities
  // this is the place to use it to create a DELayout according to fastAxis. For
  // now indicate that a default DELayout is to be created in the following call
  ESMC_DELayout *delayout = NULL;
  
  // use DistGridCreate() with DELayout to create a suitable DistGrid object
  ESMC_DistGrid *distgrid = 
    ESMC_DistGridCreate(minCorner, maxCorner, regDecomp, decompflag,
      decompflagCount, deLabelList, indexflag, connectionList,
      connectionTransformList, delayout, vm, &status);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return distgrid;
  
  // return successfully
  *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------

  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridCreate()"
//BOP
// !IROUTINE:  ESMC_DistGridCreate
//
// !INTERFACE:
ESMC_DistGrid *ESMC_DistGridCreate(
//
// !RETURN VALUE:
//    ESMC_DistGrid * to newly allocated ESMC_DistGrid
//
// !ARGUMENTS:
//
  ESMC_InterfaceInt *minCorner,          // (in)
  ESMC_InterfaceInt *maxCorner,          // (in)
  ESMC_InterfaceInt *regDecomp,          // (in)
  ESMC_DecompFlag *decompflag,                // (in)
  int decompflagCount1,                       // (in)
  int decompflagCount2,                       // (in)
  ESMC_InterfaceInt *deLabelList,        // (in)
  ESMC_IndexFlag *indexflag,                  // (in)
  ESMC_InterfaceInt *connectionList,     // (in)
  ESMC_InterfaceInt *connectionTransformList, // (in)
  ESMC_DELayout *delayout,                    // (in)
  ESMC_VM *vm,                                // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // allocate the new DistGrid object
  ESMC_DistGrid *distgrid;
  try{
    distgrid = new ESMC_DistGrid;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_DistGrid.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call DistGridConstruct
  if (minCorner == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minCorner array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxCorner == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxCorner array", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (minCorner->dimCount != 2){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- minCorner array must be of rank 2", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (maxCorner->dimCount != 2){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- maxCorner array must be of rank 2", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int dimCount = minCorner->extent[0];
  if (maxCorner->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minCorner and maxCorner array mismatch in dimCount", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int patchCount = minCorner->extent[1];
  if (maxCorner->extent[1] != patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- minCorner and maxCorner array mismatch in patchCount", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = ESMC_VMGetCurrent(&status);  // get current VM for default
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  int petCount;
  status=vm->ESMC_VMGet(NULL, &petCount, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
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
    delayout = ESMC_DELayoutCreate(&deCount, NULL, NULL, NULL, vm, &status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> check deCount
    status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete distgrid;
      distgrid = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
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
    regDecomp = new ESMC_InterfaceInt(dummy, 2, dummyLen);
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
    decompflag = new ESMC_DecompFlag[dimCount*patchCount];
    for (int i=0; i<dimCount*patchCount; i++)
      decompflag[i] = ESMF_DECOMP_DEFAULT;
  }
  if (decompflagCount1 != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minCorner and maxCorner arrays", rc);
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (decompflagCount2 != patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minCorner and maxCorner arrays", rc);
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
    deLabelList = new ESMC_InterfaceInt(dummy, 1, &deCount);
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
  int *dimExtent = new int[dimCount*deCount];
  int **indexList = new int*[dimCount*deCount];
  
  // the following differs from the single patch case in that there is an
  // extra outer loop over patches. The dimExtent and indexList arrays are
  // on DE basis, independent of patches.

  int dePatchStart = 0;  // reset  
  for (int patch=0; patch<patchCount; patch++){
    int deDivider = 1;  // reset
    for (int ii=0; ii<dimCount; ii++){
      int i = patch*dimCount + ii;  // work in the current patch
      int dimLength = maxCorner->array[i] - minCorner->array[i] + 1;
      int chunkLength = dimLength/regDecomp->array[i];  // basic chunk size
      int chunkRest = dimLength%regDecomp->array[i];    // left over points
      int de, decompChunk, extentIndex;
      switch (decompflag[i]){
        case ESMF_DECOMP_DEFAULT:
        case ESMF_DECOMP_HOMOGEN:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into dimExtent array
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest) ++dimExtent[extentIndex]; //distr. rest
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minCorner->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              indexList[extentIndex][k] = indexStart + k; // block structure
            }
          }
          break;
        case ESMF_DECOMP_RESTLAST:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into dimExtent array
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == regDecomp->array[i]-1) 
              dimExtent[extentIndex] += chunkRest; // add rest to last chunk
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minCorner->array[i] + decompChunk * chunkLength;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              indexList[extentIndex][k] = indexStart + k; // block structure
            }
          }
          break;
        case ESMF_DECOMP_RESTFIRST:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into dimExtent array
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == 0) 
              dimExtent[extentIndex] += chunkRest; // add rest to first chunk
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minCorner->array[i] + decompChunk * chunkLength;
            if (decompChunk > 0) indexStart += chunkRest;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              indexList[extentIndex][k] = indexStart + k; // block structure
            }
          }
          break;
        case ESMF_DECOMP_CYCLIC:
          for (int jj=0; jj<deCountPPatch[patch]; jj++){
            int j = dePatchStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into dimExtent array
            dimExtent[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest) ++dimExtent[extentIndex]; //distr. rest
            indexList[extentIndex] = new int[dimExtent[extentIndex]];
            // fill the indexList for this dimension and DE
            int indexStart = minCorner->array[i] + decompChunk;
            for (int k=0; k<dimExtent[extentIndex]; k++){
              // cyclic
              indexList[extentIndex][k] = indexStart + k * regDecomp->array[i];
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
  // set up patchDeLookup
  dePatchStart = 0;  // reset  
  int *patchDeLookup = new int[deCount];
  for (int patch=0; patch<patchCount; patch++){
    for (int jj=0; jj<deCountPPatch[patch]; jj++){
      int j = dePatchStart + jj;
      int de = deLabelList->array[j];
      patchDeLookup[de] = patch;
    }
    dePatchStart += deCountPPatch[patch];
  }

  // call into DistGridConstruct
  status = distgrid->ESMC_DistGridConstruct(dimCount, patchCount, patchDeLookup,
    minCorner->array, maxCorner->array, dimExtent, indexList, ESMF_TRUE,
    connectionList, delayout, vm);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete distgrid;
    distgrid = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
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
  delete [] patchDeLookup;
  
  // return successfully
  *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------
  
  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridDestroy()"
//BOP
// !IROUTINE:  ESMC_DistGridDestroy
//
// !INTERFACE:
int ESMC_DistGridDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_DistGrid **distgrid){  // in - ESMC_DistGrid to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;

  // return with errors for NULL pointer
  if (distgrid == ESMC_NULL_POINTER || *distgrid == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", rc);
    return localrc;
  }

  // destruct and delete DistGrid object
  status = (*distgrid)->ESMC_DistGridDestruct();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  delete *distgrid;
  *distgrid = ESMC_NULL_POINTER;
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// Construct and Destruct class methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridConstruct()"
//BOP
// !IROUTINE:  ESMC_DistGridConstruct
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int dimCountArg,              // (in)
  int patchCountArg,            // (in)
  int *patchDeLookupArg,        // (in)
  int *minCornerArg,            // (in)
  int *maxCornerArg,            // (in)
  int *dimExtentArg,            // (in)
  int **indexListArg,           // (in)
  ESMC_Logical regDecompFlagArg,// (in)
  ESMC_InterfaceInt *connectionListArg,     // (in)
  ESMC_DELayout *delayoutArg,   // (in) DELayout
  ESMC_VM *vmArg                // (in) VM context
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_DistGrid object.
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;

  // fill in the DistGrid object
  dimCount = dimCountArg;
  patchCount = patchCountArg;
  regDecompFlag = regDecompFlagArg;
  if (connectionListArg != NULL){
    // connectionList was provided
    int elementSize = 2*dimCount+2;
    if (connectionListArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- connectionListArg array must be of rank 2", rc);
      return localrc;
    }
    if (connectionListArg->extent[0] != elementSize){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of connectionListArg array must be of size "
        "(2*dimCount+2)", rc);
      return localrc;
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
  status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    return localrc;
  }
  status=vm->ESMC_VMGet(&localPet, &petCount, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    return localrc;
  }
  // fill in the rest
  minCorner = new int[dimCount*patchCount];
  memcpy(minCorner, minCornerArg, sizeof(int)*dimCount*patchCount);
  maxCorner = new int[dimCount*patchCount];
  memcpy(maxCorner, maxCornerArg, sizeof(int)*dimCount*patchCount);
  dimExtent = new int[dimCount*deCount];
  memcpy(dimExtent, dimExtentArg, sizeof(int)*dimCount*deCount);
  indexList = new int*[dimCount*deCount];
  for (int i=0; i<dimCount*deCount; i++){
    indexList[i] = new int[dimExtent[i]];
    memcpy(indexList[i], indexListArg[i], sizeof(int)*dimExtent[i]);
  }
  patchDeLookup = new int[deCount];
  memcpy(patchDeLookup, patchDeLookupArg, sizeof(int)*deCount);
  // determine the patchCellCount
  patchCellCount = new int[patchCount];
  for (int i=0; i<patchCount; i++){
    patchCellCount[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      patchCellCount[i] *=
        (maxCorner[i*dimCount+j] - minCorner[i*dimCount+j] + 1);
  }
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridDestruct()"
//BOP
// !IROUTINE:  ESMC_DistGridDestruct
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridDestruct(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure of an ESMC\_DistGrid object.
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // garbage collection
  delete [] dimExtent;
  delete [] minCorner;
  delete [] maxCorner;
  delete [] patchCellCount;
  delete [] patchDeLookup;
  for (int i=0; i<dimCount*deCount; i++)
    delete [] indexList[i];
  delete [] indexList;
  for (int i=0; i<connectionCount; i++)
    delete [] connectionList[i];
  if (connectionList)
    delete [] connectionList;

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// print and validation class methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridPrint()"
//BOP
// !IROUTINE:  ESMC_DistGridPrint
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridPrint(){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Print details of DistGrid object 
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  *rc = ESMF_FAILURE;

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", rc);
    return localrc;
  }

  // print info about the ESMC_DistGrid object
  printf("--- ESMC_DistGridPrint start ---\n");
  printf("dimCount = %d\n", dimCount);
  printf("patchCount = %d\n", patchCount);
  printf("regDecompFlag = %s\n", ESMC_LogicalString(regDecompFlag));
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
  printf("--- ESMC_DistGridPrint end ---\n");
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// get class methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridGet()"
//BOP
// !IROUTINE:  ESMC_DistGridGet
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_DELayout **delayoutArg,     // out - DELayout object
  int  *patchCountArg,             // out - number of patches in DistGrid
  ESMC_InterfaceInt *patchList,    // out - list of patch ID numbers
  int  *dimCountArg,               // out - DistGrid rank
  ESMC_InterfaceInt *dimExtentArg, // out - extents per dim per DE
  ESMC_Logical *regDecompFlagArg   // out - flag indicating regular decomp.
  ){    
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // fill simple return values
  if (delayoutArg != NULL)
    *delayoutArg = delayout;
  if (patchCountArg != ESMC_NULL_POINTER)
    *patchCountArg = patchCount;
  if (dimCountArg != ESMC_NULL_POINTER)
    *dimCountArg = dimCount;
  if (regDecompFlagArg != ESMC_NULL_POINTER)
    *regDecompFlagArg = regDecompFlag;

  // fill dimExtent
  if (dimExtentArg != NULL){
    // dimExtentArg was provided -> do some error checking
    if (dimExtentArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimExtentArg array must be of rank 2", rc);
      return localrc;
    }
    if (dimExtentArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of dimExtentArg array must be of size 'dimCount'", rc);
      return localrc;
    }
    if (dimExtentArg->extent[1] < deCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of dimExtentArg array must be of size 'deCount'", rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in dimExtentArg arrays
    // which are larger than dimCount x deCount. Consequently it is necessary
    // to memcpy strips of contiguous data since it cannot be assumed that
    // all data ends up contiguous in the dimExtentArg array.
    for (int i=0; i<deCount; i++)
      memcpy(&(dimExtentArg->array[i*dimExtentArg->extent[0]]),
        &(dimExtent[i*dimCount]), sizeof(int)*dimCount);
  }
  
  // fill patchList
  if (patchList != NULL){
    // patchList was provided -> do some error checking
    if (patchList->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- patchList array must be of rank 1", rc);
      return localrc;
    }
    if (patchList->extent[0] < deCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of patchList array must be of size 'deCount'", rc);
      return localrc;
    }
    // fill in values
    memcpy(patchList->array, patchDeLookup, sizeof(int)*deCount);
  }

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridGet()"
//BOP
// !IROUTINE:  ESMC_DistGridGet
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int de,                               // in  - DE   = {0, ..., deCount-1}
  int dim,                              // in  - dim  = {1, ..., dimCount}
  ESMC_InterfaceInt *indexListArg  // out - list of indices per DE per dim
  ){    
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // fill indexListArg
  if (indexListArg != NULL){
    // indexListArg was provided -> do some error checking
    if (indexListArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- indexListArg array must be of rank 1", rc);
      return localrc;
    }
    if (indexListArg->extent[0] < dimExtent[de*dimCount+(dim-1)]){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of indexListArg array size insufficiently", rc);
      return localrc;
    }
    // fill in the values
    memcpy(indexListArg->array, indexList[de*dimCount+(dim-1)],
      sizeof(int)*dimExtent[de*dimCount+(dim-1)]);
  }
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridGet()"
//BOP
// !IROUTINE:  ESMC_DistGridGet
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int de,                               // in  - DE   = {0, ..., deCount-1}
  int *cellCount                        // out
  ){    
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  //
  *cellCount = 1; // reset
  for (int i=0; i<dimCount; i++)
    *cellCount *= dimExtent[de*dimCount+i];
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridGetSequenceIndex()"
//BOP
// !IROUTINE:  ESMC_DistGridGetSequenceIndex
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridGetSequenceIndex(
//
// !RETURN VALUE:
//    int sequence index
//
// !ARGUMENTS:
//
  int de,                           // in - DE
  int *index                        // in - DE-local index tupple in exclusive
                                    //      region basis 0
  ){    
//
// !DESCRIPTION:
//    Get sequential index
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // determine the sequentialized index
  int patch = patchDeLookup[de];  // patches are basis 1 !!!!
  int seqindex = indexList[de*dimCount+(dimCount-1)][index[dimCount-1]]
    - minCorner[(patch-1)*dimCount+(dimCount-1)]; // initialize
  for (int i=dimCount-2; i>=0; i--){
    // add cells in the patch in which DE is located
    seqindex *= maxCorner[(patch-1)*dimCount+i] 
      - minCorner[(patch-1)*dimCount+i] + 1;
    seqindex += indexList[de*dimCount+i][index[i]] 
      - minCorner[(patch-1)*dimCount+i];
  }
  for (int i=0; i<patchDeLookup[de]-2; i++)
    seqindex += patchCellCount[i];  // add all the cells of previous patches
  
  return seqindex+1;  // shift sequentialized index to basis 1 !!!!
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridGetSequenceDe()"
//BOP
// !IROUTINE:  ESMC_DistGridGetSequenceDe
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridGetSequenceDe(
//
// !RETURN VALUE:
//    int DE that covers sequential index
//
// !ARGUMENTS:
//
  int seqindex                      // in - sequential index basis 1
  ){    
//
// !DESCRIPTION:
//    Get DE that covers sequential index
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // determine the DE that covers sequentialized index
  
  // find that patch that contains seqindex
  int cellSum=0; // reset
  int p;          // this will be patch index basis 0
  for (p=0; p<patchCount; p++)
    if (seqindex <= cellSum + patchCellCount[p]) break;
  if (p >= patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Could not find seqindex in DistGrid", rc);
    return -1;  // indicate problem
  }
  seqindex -= cellSum;  // shift into patch-local sequential index
  seqindex -= 1;        // shift into basis 0
  // find the patch-local index tuple for seqindex
  int *ii = new int[dimCount];
  for (int i=dimCount-1; i>=0; i--){
    int delta = 1; // reset
    for (int j=0; j<i; j++)
      delta *= maxCorner[p*dimCount+j] - minCorner[p*dimCount+j] + 1;
    ii[i] = seqindex / delta;
    seqindex = seqindex % delta;
  }
  // shift origin of ii[] onto patch origin
  for (int i=0; i<dimCount; i++)
    ii[i] += minCorner[p*dimCount+i];
  // find the DE that covers this patch-local index tuple
  int de;
  for (de=0; de<deCount; de++){
    if (patchDeLookup[de] != p+1) continue; // DE not in correct patch
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
      "- Could not find DE in DistGrid that covers seqindex", rc);
    return -1;  // indicate problem
  }
    
  return de;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridGetPatchMinMaxCorner()"
//BOP
// !IROUTINE:  ESMC_DistGridGetPatchMinMaxCorner
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridGetPatchMinMaxCorner(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int patch,                            // in  - DE   = {1, ..., patchCount}
  int *minCornerArg,                    // out
  int *maxCornerArg                     // out
  ){    
//
// !DESCRIPTION:
//    Get information about a DistGrid object
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_FAILURE,
      "- Specified patch out of bounds", rc);
    return localrc;
  }

  //
  if (minCornerArg)
    memcpy(minCornerArg, &minCorner[(patch-1)*dimCount], dimCount*sizeof(int));
  if (maxCornerArg)
    memcpy(maxCornerArg, &maxCorner[(patch-1)*dimCount], dimCount*sizeof(int));
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// Serialize/Deserialize functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridSerialize"
//BOPI
// !IROUTINE:  ESMC_DistGridSerialize - Turn distgrid information into a byte stream
//
// !INTERFACE:
int ESMC_DistGrid::ESMC_DistGridSerialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
      char *buffer,          // inout - byte stream to fill
      int *length,           // inout - buf length; realloc'd here if needed
      int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in distgrid object into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
    int fixedpart, nbytes, rc;
    int i, j;
    char *cp;
    int *ip;
    ESMC_Logical *lp;
    ESMC_VM **vp;
    ESMC_DePinFlag *dp;
    de_type *dep;

    // TODO: we cannot reallocate from C++ if the original buffer is
    //  allocated on the f90 side.  change the code to make the allocate
    //  happen in C++; then this will be fine.  (for now make sure buffer
    //  is always big enough so realloc is not needed.)
    fixedpart = sizeof(ESMC_DistGrid);
    if ((*length - *offset) < fixedpart) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                             "Buffer too short to add a DistGrid object", &rc);
        return ESMF_FAILURE; 
    }

    // fixedpart = sizeof(ESMC_DELayout);
    // if ((*length - *offset) < fixedpart) {
    //     buffer = (char *)realloc((void *)buffer, *length + 2*fixedpart);
    //     *length += 2 * fixedpart;
    //  }

    // first set the base part of the object
    rc = this->ESMC_Base::ESMC_Serialize(buffer, length, offset);
    // serialize the DELayout
    rc = delayout->ESMC_DELayoutSerialize(buffer, length, offset);
    
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
      *ip++ = patchDeLookup[i];
    for (int i=0; i<dimCount*patchCount; i++)
      *ip++ = minCorner[i];
    for (int i=0; i<dimCount*patchCount; i++)
      *ip++ = maxCorner[i];
    for (int i=0; i<dimCount*deCount; i++)
      *ip++ = dimExtent[i];
    for (int i=0; i<dimCount*deCount; i++)
      for (int k=0; k<dimExtent[i]; k++)
        *ip++ = indexList[i][k];

    cp = (char *)ip;
    *offset = (cp - buffer);
   
    return ESMF_SUCCESS;

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DistGridDeserialize"
//BOPI
// !IROUTINE:  ESMC_DistGridDeserialize - Turn a byte stream into an object
//
// !INTERFACE:
ESMC_DistGrid *ESMC_DistGridDeserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
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
    ESMC_DistGrid *a = new ESMC_DistGrid;
    int fixedpart, nbytes, rc;
    int i, j;
    char *cp;
    int *ip;
    ESMC_Logical *lp;
    ESMC_VM **vp;
    ESMC_DePinFlag *dp;
    de_type *dep;

    // first get the base part of the object
    rc = a->ESMC_Base::ESMC_Deserialize(buffer, offset);
    a->delayout = ESMC_DELayoutDeserialize(buffer, offset);
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
    a->patchDeLookup = new int[a->deCount];
    for (int i=0; i<a->deCount; i++)
      a->patchDeLookup[i] = *ip++;
    a->minCorner = new int[a->dimCount*a->patchCount];
    for (int i=0; i<a->dimCount*a->patchCount; i++)
      a->minCorner[i] = *ip++;
    a->maxCorner = new int[a->dimCount*a->patchCount];
    for (int i=0; i<a->dimCount*a->patchCount; i++)
      a->maxCorner[i] = *ip++;
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
#define ESMC_METHOD "ESMC_Connection()"
//BOP
// !IROUTINE:  ESMC_Connection
//
// !INTERFACE:
int ESMC_Connection(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_InterfaceInt *connection,  // out -
  int patchIndexA,                            // in  -
  int patchIndexB,                            // in  -
  ESMC_InterfaceInt *positionVector,     // in -
  ESMC_InterfaceInt *orientationVector   // in -
  ){    
//
// !DESCRIPTION:
//    Construct a connection element
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume failure until success is certain
  status = ESMF_FAILURE;
  if (rc!=NULL)
    *rc = ESMF_FAILURE;
  
  // check connetion argument
  if (connection == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to connection array", rc);
    return localrc;
  }
  if (connection->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- connection array must be of rank 1", rc);
    return localrc;
  }
  int dimCount = (connection->extent[0]-2)/2;
  if (dimCount <= 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dimension of connection array must be of size "
      "(2 * dimCount + 2)", rc);
    return localrc;
  }
  
  // fill in the patch indices
  connection->array[0] = patchIndexA;
  connection->array[1] = patchIndexB;
  
  // check positionVector argument
  if (positionVector == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to positionVector array", rc);
    return localrc;
  }
  if (positionVector->dimCount != 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
      "- positionVector array must be of rank 1", rc);
    return localrc;
  }
  if (positionVector->extent[0] != dimCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dimension of positionVector array must be of size dimCount", rc);
    return localrc;
  }
  
  // fill in the positionVector
  memcpy(&(connection->array[2]), positionVector->array,
    sizeof(int)*dimCount);
  
  // check on orientationVector
  if (orientationVector != NULL){
    // orientationVector was provided
    if (orientationVector->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- orientationVector array must be of rank 1", rc);
      return localrc;
    }
    if (orientationVector->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of orientationVector array must be of size dimCount",
        rc);
      return localrc;
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
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------
