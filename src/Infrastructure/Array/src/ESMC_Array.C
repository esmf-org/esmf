// $Id: ESMC_Array.C,v 1.75 2007/04/26 23:41:08 theurich Exp $
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
#define ESMC_FILENAME "ESMC_Array.C"
//==============================================================================
//
// ESMC Array method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array methods declared
// in the companion file ESMC_Array.h
//
//-----------------------------------------------------------------------------

// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include <string.h>

#include "ESMF_Pthread.h"

#include "ESMC_Start.h"

// associated class definition file
#include "ESMC_Array.h"

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
  static const char *const version = "$Id: ESMC_Array.C,v 1.75 2007/04/26 23:41:08 theurich Exp $";
//-----------------------------------------------------------------------------

#define VERBOSITY             (1)       // 0: off, 10: max

//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// external Create and Destroy functions
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayCreate()"
//BOP
// !IROUTINE:  ESMC_ArrayCreate
//
// !INTERFACE:
ESMC_Array *ESMC_ArrayCreate(
//
// !RETURN VALUE:
//    ESMC_Array * to newly allocated ESMC_Array
//
// !ARGUMENTS:
//
  ESMC_LocalArray **larrayListArg,            // (in)
  int larrayCount,                            // (in)
  ESMC_DistGrid *distgrid,                    // (in)
  ESMC_InterfaceInt *dimmap,                  // (in)
  ESMC_InterfaceInt *computationalLWidthArg,  // (in)
  ESMC_InterfaceInt *computationalUWidthArg,  // (in)
  ESMC_InterfaceInt *totalLWidthArg,          // (in)
  ESMC_InterfaceInt *totalUWidthArg,          // (in)
  ESMC_IndexFlag *indexflagArg,               // (in)
  int *staggerLocArg,                         // (in)
  int *vectorDimArg,                          // (in)
  ESMC_InterfaceInt *lboundsArg,              // (in)
  ESMC_InterfaceInt *uboundsArg,              // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Array} object from list if LocalArrays and DistGrid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new Array object
  ESMC_Array *array;
  try{
    array = new ESMC_Array;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Array.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call ArrayConstruct
  // larrayListArg -> typekind/rank
  if (larrayListArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to larrayList argument", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (larrayCount < 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- The larrayList argument must provide at least one LocalArray object",
      rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind typekind = larrayListArg[0]->ESMC_LocalArrayGetTypeKind();
  int rank = larrayListArg[0]->ESMC_LocalArrayGetRank();
  for (int i=1; i<larrayCount; i++){
    if (larrayListArg[0]->ESMC_LocalArrayGetTypeKind() != typekind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- TypeKind mismatch in the elements of larrayList argument", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (larrayListArg[0]->ESMC_LocalArrayGetRank() != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- Rank mismatch in the elements of larrayList argument", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  ESMC_DELayout *delayout;
  int dimCount;
  status = distgrid->ESMC_DistGridGet(&delayout, NULL, NULL, &dimCount, NULL,
    NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (dimCount > rank){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
      "- dimCount of distgrid argument must be <= rank of Array", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  // check for lbounds and ubounds arguments and that they match dimCount, rank
//todo: provide a default here that leaves the bounds of the incoming LocalArray
//      unchanged for tensor dimensions and don't require these arguments
  int tensorCount = rank - dimCount;  // number of tensor dimensions
  if (tensorCount > 0 && lboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid lbounds argument required to create Array with tensor dims", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (tensorCount > 0 && uboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid ubounds argument required to create Array with tensor dims", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int *lboundsArray = NULL; // reset
  if (lboundsArg != NULL){
    if (lboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (lboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, arrayspec, distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    lboundsArray = lboundsArg->array;
  }
  int *uboundsArray = NULL; // reset
  if (uboundsArg != NULL){
    if (uboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (uboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- ubounds, arrayspec, distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    uboundsArray = uboundsArg->array;
  }
  // check if dimmap was provided and matches rest of arguments
  int *dimmapArray = new int[dimCount];
  for (int i=0; i<dimCount; i++)
    dimmapArray[i] = i+1; // default  (basis 1)
  if (dimmap != NULL){
    if (dimmap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmap array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (dimmap->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (dimmap->array[i] < 1 || dimmap->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- dimmap / rank mismatch", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      dimmapArray[i] = dimmap->array[i];  // copy dimmap array element
    }
  }
  // generate inverseDimmap
  int *inverseDimmapArray = new int[rank];
  for (int i=0; i<rank; i++)
    inverseDimmapArray[i] = 0; // reset  (basis 1), 0 indicates not distr. dim
  for (int i=0; i<dimCount; i++)
    inverseDimmapArray[dimmapArray[i]-1] = i+1;
  // delayout -> deCount, localDeCount, localDeList
  int deCount;
  int localDeCount;
  status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, 0, NULL, 0,
    NULL, NULL, &localDeCount, NULL, 0, NULL, NULL, 0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (localDeCount != larrayCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Mismatch in localDeCount between larrayList argument and DELayout", 
      rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int *localDeList = new int[localDeCount];
  status=delayout->ESMC_DELayoutGet(NULL, NULL, NULL, 0, NULL, 0,
    NULL, NULL, NULL, localDeList, localDeCount, NULL, NULL, 0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  // distgrid -> dimExtent[]
  int *dimExtent = new int[dimCount*deCount];
  int dummyLen[2];
  dummyLen[0] = dimCount;
  dummyLen[1] = deCount;
  ESMC_InterfaceInt *dimExtentArg =
    new ESMC_InterfaceInt(dimExtent, 2, dummyLen);
  status = distgrid->ESMC_DistGridGet(NULL, NULL, NULL, NULL, dimExtentArg,
    NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  // check on indexflag
  ESMC_IndexFlag indexflag = ESMF_INDEX_DELOCAL;  // default
  if (indexflagArg != NULL)
    indexflag = *indexflagArg;
  // figure exclusive region
  int *exclusiveLBound = new int[dimCount*localDeCount];
  int *exclusiveUBound = new int[dimCount*localDeCount];
  for (int i=0; i<dimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at dimExtent of the associated DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    memcpy(&(exclusiveUBound[i*dimCount]), &(dimExtent[de*dimCount]),
      dimCount*sizeof(int));
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMF_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      for (int j=0; j<dimCount; j++){
        // obtain indexList for this DE and dim
        int *indexList = new int[dimExtent[de*dimCount+j]];
        ESMC_InterfaceInt *indexListArg =
          new ESMC_InterfaceInt(indexList, 1, &(dimExtent[de*dimCount+j]));
        status = distgrid->ESMC_DistGridGet(de, j+1, indexListArg);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU,
          rc)){
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
        // check that this dim has a contiguous index list
        for (int k=1; k<dimExtent[de*dimCount+j]; k++){
          if (indexList[k] != indexList[k-1]+1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
              "- Cannot use non-contiguous decomposition for pseudo global"
              " index space", rc);
            delete array;
            array = ESMC_NULL_POINTER;
            return ESMC_NULL_POINTER;
          }
        }
        // shift bounds of exclusive region to match indexList[0]
        int shift = indexList[0] - exclusiveLBound[i*dimCount+j];
        exclusiveLBound[i*dimCount+j] += shift;
        exclusiveUBound[i*dimCount+j] += shift;
        // clean-up
        delete indexListArg;
        delete [] indexList;
      } // j
    } // i
  }
  // deal with computational widths
  int *computationalLBound = new int[dimCount*localDeCount];
  int *computationalUBound = new int[dimCount*localDeCount];
  if (computationalLWidthArg != NULL){
    if (computationalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalLWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalLWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++)
        computationalLBound[j*dimCount+i] = exclusiveLBound[j*dimCount+i]
          - computationalLWidthArg->array[i];
    }
  }else{
    // set default
    memcpy(computationalLBound, exclusiveLBound,
      localDeCount*dimCount*sizeof(int));
  }
  if (computationalUWidthArg != NULL){
    if (computationalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalUWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++)
        computationalUBound[j*dimCount+i] = exclusiveUBound[j*dimCount+i]
          + computationalUWidthArg->array[i];
    }
  }else{
    // set default
    memcpy(computationalUBound, exclusiveUBound,
      localDeCount*dimCount*sizeof(int));
  }
  // deal with total widths
  int totalLBoundFlag = 0;  // reset
  int totalUBoundFlag = 0;  // reset
  int *totalLBound = new int[dimCount*localDeCount];
  int *totalUBound = new int[dimCount*localDeCount];
  if (totalLWidthArg != NULL){
    totalLBoundFlag = 1;  // set
    if (totalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalLWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalLWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalLWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*dimCount+i] = exclusiveLBound[j*dimCount+i]
          - totalLWidthArg->array[i];
        if (totalLBound[j*dimCount+i] > computationalLBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalLWidth / computationaLWidth mismatch", rc);
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
      }
    }
  }else{
    // set default
    memcpy(totalLBound, computationalLBound,
      localDeCount*dimCount*sizeof(int));
  }
  if (totalUWidthArg != NULL){
    totalUBoundFlag = 1;  // set
    if (totalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalUWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalUWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalUWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*dimCount+i] = exclusiveUBound[j*dimCount+i]
          + totalUWidthArg->array[i];
        if (totalUBound[j*dimCount+i] < computationalUBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalUWidth / computationaUWidth mismatch", rc);
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
      }
    }
  }else{
    // set default
    memcpy(totalUBound, computationalUBound,
      localDeCount*dimCount*sizeof(int));
  }
  // prepare temporary staggerLoc and vectorDim arrays
  int *staggerLoc = new int[tensorCount];
  if (staggerLocArg)
    for (int i=0; i<tensorCount; i++)
      staggerLoc[i] = *staggerLocArg;
  else
    for (int i=0; i<tensorCount; i++)
      staggerLoc[i] = 0;
  int *vectorDim = new int[tensorCount];
  if (vectorDimArg)
    for (int i=0; i<tensorCount; i++)
      vectorDim[i] = *vectorDimArg;
  else
    for (int i=0; i<tensorCount; i++)
      vectorDim[i] = 1;
    
  // allocate LocalArray list that holds all PET-local DEs and adjust elements
  ESMC_LocalArray **larrayList = new ESMC_LocalArray*[localDeCount];
  int *temp_counts = new int[rank];
  int *temp_lbounds = new int[rank];
  int *temp_ubounds = new int[rank];
  for (int i=0; i<localDeCount; i++){
    larrayListArg[i]->ESMC_LocalArrayGetCounts(rank, temp_counts);
    int jjj=0;  // reset
    for (int jj=0; jj<rank; jj++){
      if (inverseDimmapArray[jj]){
        // distributed dimension
        int j = inverseDimmapArray[jj] - 1; // shift to basis 0
        if (temp_counts[jj] < 
          totalUBound[i*dimCount+j] - totalLBound[i*dimCount+j] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- LocalArray does not accommodate requested cell count", rc);
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
        // move the total bounds according to input info
        if (totalLBoundFlag){
          // totalLBound fixed
          temp_lbounds[jj] = totalLBound[i*dimCount+j];
          if (totalUBoundFlag){
            // totalUBound fixed
            // this case allows total allocation to be larger than total region!
            temp_ubounds[jj] = totalUBound[i*dimCount+j];
          }else{
            // totalUBound not fixed
            temp_ubounds[jj] = temp_counts[jj] + totalLBound[i*dimCount+j] - 1;
          }
        }else{
          // totalLBound not fixed
          if (totalUBoundFlag){
            // totalUBound fixed
            temp_ubounds[jj] = totalUBound[i*dimCount+j];
            temp_lbounds[jj] = totalUBound[i*dimCount+j] - temp_counts[jj] + 1;
          }else{
            // totalLBound and totalUBound not fixed
            // -> shift computational region into center of total region
            temp_lbounds[jj] = computationalLBound[i*dimCount+j]
              - (0.5 * (temp_counts[jj] - 1
                        + computationalLBound[i*dimCount+j]
                        - computationalUBound[i*dimCount+j]));
            temp_ubounds[jj] = temp_counts[jj] + temp_lbounds[jj] - 1;
          }
        }
        totalLBound[i*dimCount+j] = temp_lbounds[jj]; // write back
        totalUBound[i*dimCount+j] = temp_ubounds[jj]; // write back
      }else{
        // non-distributed dimension
        if (temp_counts[jj] < 
          uboundsArray[jjj] - lboundsArray[jjj] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- LocalArray does not accommodate requested cell count", rc);
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
        temp_lbounds[jj] = lboundsArray[jjj];
        temp_ubounds[jj] = uboundsArray[jjj];
        ++jjj;
      }
    }
    // adjust LocalArray object for specific lbounds and ubounds
    larrayList[i] = larrayListArg[i]->
      ESMC_LocalArrayAdjust(temp_lbounds, temp_ubounds, &status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }
  delete [] temp_counts;
  delete [] temp_lbounds;
  delete [] temp_ubounds;
  
  // call into ArrayConstruct
  status = array->ESMC_ArrayConstruct(typekind, rank, larrayList, distgrid,
    exclusiveLBound, exclusiveUBound, computationalLBound, computationalUBound,
    totalLBound, totalUBound, tensorCount, lboundsArray, uboundsArray,
    staggerLoc, vectorDim, dimmapArray, inverseDimmapArray, indexflag);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] larrayList;
  delete [] localDeList;
  delete [] dimExtent;
  delete dimExtentArg;
  delete [] exclusiveLBound;
  delete [] exclusiveUBound;
  delete [] computationalLBound;
  delete [] computationalUBound;
  delete [] totalLBound;
  delete [] totalUBound;
  delete [] dimmapArray;
  delete [] inverseDimmapArray;
  delete [] staggerLoc;
  delete [] vectorDim;
  
  // return successfully
  *rc = ESMF_SUCCESS;
  return array;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayCreate()"
//BOP
// !IROUTINE:  ESMC_ArrayCreate
//
// !INTERFACE:
ESMC_Array *ESMC_ArrayCreate(
//
// !RETURN VALUE:
//    ESMC_Array * to newly allocated ESMC_Array
//
// !ARGUMENTS:
//
  ESMC_ArraySpec *arrayspec,                  // (in)
  ESMC_DistGrid *distgrid,                    // (in)
  ESMC_InterfaceInt *dimmap,                  // (in)
  ESMC_InterfaceInt *computationalLWidthArg,  // (in)
  ESMC_InterfaceInt *computationalUWidthArg,  // (in)
  ESMC_InterfaceInt *totalLWidthArg,          // (in)
  ESMC_InterfaceInt *totalUWidthArg,          // (in)
  ESMC_IndexFlag *indexflagArg,               // (in)
  int *staggerLocArg,                         // (in)
  int *vectorDimArg,                          // (in)
  ESMC_InterfaceInt *lboundsArg,              // (in)
  ESMC_InterfaceInt *uboundsArg,              // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Array} object from ArraySpec and DistGrid.
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new Array object
  ESMC_Array *array;
  try{
    array = new ESMC_Array;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMC_Array.", rc);  
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call ArrayConstruct
  // arrayspec -> typekind/rank
  if (arrayspec == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to arrayspec", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind typekind = arrayspec->ESMC_ArraySpecGetTypeKind();
  int rank = arrayspec->ESMC_ArraySpecGetRank();
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  ESMC_DELayout *delayout;
  int dimCount;
  status = distgrid->ESMC_DistGridGet(&delayout, NULL, NULL, &dimCount, NULL,
    NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (dimCount > rank){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
      "- dimCount of distgrid argument must be <= rank of Array", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  // check for lbounds and ubounds arguments and that they match dimCount, rank
  int tensorCount = rank - dimCount;  // number of tensor dimensions
  if (tensorCount > 0 && lboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid lbounds argument required to create Array with tensor dims", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  if (tensorCount > 0 && uboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid ubounds argument required to create Array with tensor dims", rc);
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int *lboundsArray = NULL; // reset
  if (lboundsArg != NULL){
    if (lboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (lboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, arrayspec, distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    lboundsArray = lboundsArg->array;
  }
  int *uboundsArray = NULL; // reset
  if (uboundsArg != NULL){
    if (uboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (uboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- ubounds, arrayspec, distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    uboundsArray = uboundsArg->array;
  }
  // check if dimmap was provided and matches rest of arguments
  int *dimmapArray = new int[dimCount];
  for (int i=0; i<dimCount; i++)
    dimmapArray[i] = i+1; // default  (basis 1)
  if (dimmap != NULL){
    if (dimmap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmap array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (dimmap->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (dimmap->array[i] < 1 || dimmap->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- dimmap / rank mismatch", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      dimmapArray[i] = dimmap->array[i];  // copy dimmap array element
    }
  }
  // generate inverseDimmap
  int *inverseDimmapArray = new int[rank];
  for (int i=0; i<rank; i++)
    inverseDimmapArray[i] = 0; // reset  (basis 1), 0 indicates not distr. dim
  for (int i=0; i<dimCount; i++)
    inverseDimmapArray[dimmapArray[i]-1] = i+1;
  // delayout -> deCount, localDeCount, localDeList
  int deCount;
  int localDeCount;
  status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, 0, NULL, 0,
    NULL, NULL, &localDeCount, NULL, 0, NULL, NULL, 0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  int *localDeList = new int[localDeCount];
  status=delayout->ESMC_DELayoutGet(NULL, NULL, NULL, 0, NULL, 0,
    NULL, NULL, NULL, localDeList, localDeCount, NULL, NULL, 0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  // distgrid -> dimExtent[]
  int *dimExtent = new int[dimCount*deCount];
  int dummyLen[2];
  dummyLen[0] = dimCount;
  dummyLen[1] = deCount;
  ESMC_InterfaceInt *dimExtentArg =
    new ESMC_InterfaceInt(dimExtent, 2, dummyLen);
  status = distgrid->ESMC_DistGridGet(NULL, NULL, NULL, NULL, dimExtentArg,
    NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  // check on indexflag
  ESMC_IndexFlag indexflag = ESMF_INDEX_DELOCAL;  // default
  if (indexflagArg != NULL)
    indexflag = *indexflagArg;
  // figure exclusive region
  int *exclusiveLBound = new int[dimCount*localDeCount];
  int *exclusiveUBound = new int[dimCount*localDeCount];
  for (int i=0; i<dimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at dimExtent of the associated DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    memcpy(&(exclusiveUBound[i*dimCount]), &(dimExtent[de*dimCount]),
      dimCount*sizeof(int));
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMF_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      for (int j=0; j<dimCount; j++){
        // obtain indexList for this DE and dim
        int *indexList = new int[dimExtent[de*dimCount+j]];
        ESMC_InterfaceInt *indexListArg =
          new ESMC_InterfaceInt(indexList, 1, &(dimExtent[de*dimCount+j]));
        status = distgrid->ESMC_DistGridGet(de, j+1, indexListArg);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU,
          rc)){
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
        // check that this dim has a contiguous index list
        for (int k=1; k<dimExtent[de*dimCount+j]; k++){
          if (indexList[k] != indexList[k-1]+1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
              "- Cannot use non-contiguous decomposition for pseudo global"
              " index space", rc);
            delete array;
            array = ESMC_NULL_POINTER;
            return ESMC_NULL_POINTER;
          }
        }
        // shift bounds of exclusive region to match indexList[0]
        int shift = indexList[0] - exclusiveLBound[i*dimCount+j];
        exclusiveLBound[i*dimCount+j] += shift;
        exclusiveUBound[i*dimCount+j] += shift;
        // clean-up
        delete indexListArg;
        delete [] indexList;
      } // j
    } // i
  }
  // deal with computational widths
  int *computationalLBound = new int[dimCount*localDeCount];
  int *computationalUBound = new int[dimCount*localDeCount];
  if (computationalLWidthArg != NULL){
    if (computationalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalLWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalLWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++)
        computationalLBound[j*dimCount+i] = exclusiveLBound[j*dimCount+i]
          - computationalLWidthArg->array[i];
    }
  }else{
    // set default
    memcpy(computationalLBound, exclusiveLBound,
      localDeCount*dimCount*sizeof(int));
  }
  if (computationalUWidthArg != NULL){
    if (computationalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalUWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++)
        computationalUBound[j*dimCount+i] = exclusiveUBound[j*dimCount+i]
          + computationalUWidthArg->array[i];
    }
  }else{
    // set default
    memcpy(computationalUBound, exclusiveUBound,
      localDeCount*dimCount*sizeof(int));
  }
  // deal with total widths
  int *totalLBound = new int[dimCount*localDeCount];
  int *totalUBound = new int[dimCount*localDeCount];
  if (totalLWidthArg != NULL){
    if (totalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalLWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalLWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalLWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*dimCount+i] = exclusiveLBound[j*dimCount+i]
          - totalLWidthArg->array[i];
        if (totalLBound[j*dimCount+i] > computationalLBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalLWidth / computationaLWidth mismatch", rc);
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
      }
    }
  }else{
    // set default
    memcpy(totalLBound, computationalLBound,
      localDeCount*dimCount*sizeof(int));
  }
  if (totalUWidthArg != NULL){
    if (totalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalUWidth array must be of rank 1", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalUWidth and distgrid mismatch", rc);
      delete array;
      array = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalUWidth may only contain positive values", rc);
        delete array;
        array = ESMC_NULL_POINTER;
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*dimCount+i] = exclusiveUBound[j*dimCount+i]
          + totalUWidthArg->array[i];
        if (totalUBound[j*dimCount+i] < computationalUBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalUWidth / computationaUWidth mismatch", rc);
          delete array;
          array = ESMC_NULL_POINTER;
          return ESMC_NULL_POINTER;
        }
      }
    }
  }else{
    // set default
    memcpy(totalUBound, computationalUBound,
      localDeCount*dimCount*sizeof(int));
  }
  // prepare temporary staggerLoc and vectorDim arrays
  int *staggerLoc = new int[tensorCount];
  if (staggerLocArg)
    for (int i=0; i<tensorCount; i++)
      staggerLoc[i] = *staggerLocArg;
  else
    for (int i=0; i<tensorCount; i++)
      staggerLoc[i] = 0;
  int *vectorDim = new int[tensorCount];
  if (vectorDimArg)
    for (int i=0; i<tensorCount; i++)
      vectorDim[i] = *vectorDimArg;
  else
    for (int i=0; i<tensorCount; i++)
      vectorDim[i] = 1;
  
  // allocate LocalArray list that holds all PET-local DEs
  ESMC_LocalArray **larrayList = new ESMC_LocalArray*[localDeCount];
  int *temp_counts = new int[rank];
  int *temp_lbounds = new int[rank];
  int *temp_ubounds = new int[rank];
  for (int i=0; i<localDeCount; i++){
    int jjj=0;  // reset
    for (int jj=0; jj<rank; jj++){
      if (inverseDimmapArray[jj]){
        // distributed dimension
        int j = inverseDimmapArray[jj] - 1; // shift to basis 0
        temp_counts[jj] =
          totalUBound[i*dimCount+j] - totalLBound[i*dimCount+j] + 1;
        temp_lbounds[jj] = totalLBound[i*dimCount+j];
        temp_ubounds[jj] = totalUBound[i*dimCount+j];
      }else{
        // non-distributed dimension
        temp_counts[jj] = uboundsArray[jjj] - lboundsArray[jjj] + 1;
        temp_lbounds[jj] = lboundsArray[jjj];
        temp_ubounds[jj] = uboundsArray[jjj];
        ++jjj;
      }
    }
    // allocate LocalArray object with specific lbounds and ubounds
    larrayList[i] = ESMC_LocalArrayCreate(rank, typekind, temp_counts,
      temp_lbounds, temp_ubounds);
    // TODO: need error handling for the above call
  }
  delete [] temp_counts;
  delete [] temp_lbounds;
  delete [] temp_ubounds;
  
  // call into ArrayConstruct
  status = array->ESMC_ArrayConstruct(typekind, rank, larrayList, distgrid,
    exclusiveLBound, exclusiveUBound, computationalLBound, computationalUBound,
    totalLBound, totalUBound, tensorCount, lboundsArray, uboundsArray,
    staggerLoc, vectorDim, dimmapArray, inverseDimmapArray, indexflag);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
    delete array;
    array = ESMC_NULL_POINTER;
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] larrayList;
  delete [] localDeList;
  delete [] dimExtent;
  delete dimExtentArg;
  delete [] exclusiveLBound;
  delete [] exclusiveUBound;
  delete [] computationalLBound;
  delete [] computationalUBound;
  delete [] totalLBound;
  delete [] totalUBound;
  delete [] dimmapArray;
  delete [] inverseDimmapArray;
  delete [] staggerLoc;
  delete [] vectorDim;
  
  // return successfully
  *rc = ESMF_SUCCESS;
  return array;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayDestroy()"
//BOP
// !IROUTINE:  ESMC_ArrayDestroy
//
// !INTERFACE:
int ESMC_ArrayDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_Array **array){  // in - ESMC_Array to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (array == ESMC_NULL_POINTER || *array == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", rc);
    return localrc;
  }

  // destruct and delete Array object
  status = (*array)->ESMC_ArrayDestruct();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  delete *array;
  *array = ESMC_NULL_POINTER;
  
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
#define ESMC_METHOD "ESMC_ArrayConstruct()"
//BOPI
// !IROUTINE:  ESMC_ArrayConstruct
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_TypeKind typekindArg,              // (in)
  int rankArg,                            // (in)
  ESMC_LocalArray **larrayListArg,        // (in)
  ESMC_DistGrid *distgridArg,             // (in)
  int *exclusiveLBoundArg,                // (in)
  int *exclusiveUBoundArg,                // (in)
  int *computationalLBoundArg,            // (in)
  int *computationalUBoundArg,            // (in)
  int *totalLBoundArg,                    // (in)
  int *totalUBoundArg,                    // (in)
  int tensorCountArg,                     // (in)
  int *lboundsArray,                      // (in)
  int *uboundsArray,                      // (in)
  int *staggerLocArray,                   // (in)
  int *vectorDimArray,                    // (in)
  int *dimmapArray,                       // (in)
  int *inverseDimmapArray,                // (in)
  ESMC_IndexFlag indexflagArg             // (in)
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_Array object.
//    No error checking wrt consistency of input arguments is needed because
//    ArraytConstruct() is only to be called by ArrayCreate() interfaces which
//    are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;

  // fill in the Array object
  typekind = typekindArg;
  rank = rankArg;
  distgrid = distgridArg;
  status = distgrid->ESMC_DistGridGet(&delayout, NULL, NULL, &dimCount, NULL,
    NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  status=delayout->ESMC_DELayoutGet(NULL, &deCount, NULL, 0, NULL, 0,
    NULL, NULL, &localDeCount, NULL, 0, NULL, NULL, 0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  localDeList = new int[localDeCount];
  status=delayout->ESMC_DELayoutGet(NULL, NULL, NULL, 0, NULL, 0,
    NULL, NULL, NULL, localDeList, localDeCount, NULL, NULL, 0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  // copy the PET-local LocalArray pointers
  larrayList = new ESMC_LocalArray*[localDeCount];
  memcpy(larrayList, larrayListArg, localDeCount*sizeof(ESMC_LocalArray *));
  // determine the base addresses of the local arrays:
  larrayBaseAddrList = new void*[localDeCount];
  for (int i=0; i<localDeCount; i++)
    larrayList[i]->ESMC_LocalArrayGetBaseAddr((void **)&larrayBaseAddrList[i]);
  // copy the PET-local bound arrays
  exclusiveLBound = new int[dimCount*localDeCount];
  memcpy(exclusiveLBound, exclusiveLBoundArg,
    dimCount*localDeCount*sizeof(int));
  exclusiveUBound = new int[dimCount*localDeCount];
  memcpy(exclusiveUBound, exclusiveUBoundArg,
    dimCount*localDeCount*sizeof(int));
  computationalLBound = new int[dimCount*localDeCount];
  memcpy(computationalLBound, computationalLBoundArg,
    dimCount*localDeCount*sizeof(int));
  computationalUBound = new int[dimCount*localDeCount];
  memcpy(computationalUBound, computationalUBoundArg,
    dimCount*localDeCount*sizeof(int));
  totalLBound = new int[dimCount*localDeCount];
  memcpy(totalLBound, totalLBoundArg,
    dimCount*localDeCount*sizeof(int));
  totalUBound = new int[dimCount*localDeCount];
  memcpy(totalUBound, totalUBoundArg,
    dimCount*localDeCount*sizeof(int));
  // tensor dimensions
  tensorCount = tensorCountArg;
  lbounds = new int[tensorCountArg];
  memcpy(lbounds, lboundsArray, tensorCountArg * sizeof(int));
  ubounds = new int[tensorCountArg];
  memcpy(ubounds, uboundsArray, tensorCountArg * sizeof(int));
  // staggerLoc and vectorDim
  staggerLoc = new int[tensorCountArg];
  memcpy(staggerLoc, staggerLocArray, tensorCountArg * sizeof(int));
  vectorDim = new int[tensorCountArg];
  memcpy(vectorDim, vectorDimArray, tensorCountArg * sizeof(int));
  // dimmap and inverseDimmap
  dimmap = new int[dimCount];
  memcpy(dimmap, dimmapArray, dimCount * sizeof(int));
  inverseDimmap = new int[rank];
  memcpy(inverseDimmap, inverseDimmapArray, rank * sizeof(int));
  // indexflag
  indexflag = indexflagArg;
  // contiguous flag
  contiguousFlag = new int[localDeCount];
  // deCellCount
  deCellCount = new int[deCount];
  
  int *dimExtent = new int[dimCount*deCount];
  int dummyLen[2];
  dummyLen[0] = dimCount;
  dummyLen[1] = deCount;
  ESMC_InterfaceInt *dimExtentArg =
    new ESMC_InterfaceInt(dimExtent, 2, dummyLen);
  status = distgrid->ESMC_DistGridGet(NULL, NULL, NULL, NULL, dimExtentArg);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  delete dimExtentArg;
  
  for (int i=0; i<deCount; i++){
    int distGridDeCellCount;
    status = distgrid->ESMC_DistGridGet(i, &distGridDeCellCount);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
      return localrc;
    deCellCount[i] = distGridDeCellCount;  // prime deCellCount element
    for (int k=0; k<tensorCount; k++){
      // multiply in tensor extents
      deCellCount[i] *= (ubounds[k] - lbounds[k] + 1);
    }
  }
  
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];

    contiguousFlag[i] = 1;  // initialize as contiguous

    for (int jj=0; jj<rank; jj++){
      int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor dims
      if (j){
        // decomposed dimension 
        --j;  // shift to basis 0
        if (totalLBound[i*dimCount+j] != exclusiveLBound[i*dimCount+j]
          || totalUBound[i*dimCount+j] != exclusiveUBound[i*dimCount+j]){
          contiguousFlag[i] = 0; // reset
          break;
        }
        // obtain indexList for this DE and dim
        int *indexList = new int[dimExtent[de*dimCount+j]];
        ESMC_InterfaceInt *indexListArg = new ESMC_InterfaceInt(indexList, 1,
          &(dimExtent[de*dimCount+j]));
        status = distgrid->ESMC_DistGridGet(de, j+1, indexListArg);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, 
          rc)) return localrc;
        // check if this dim has a contiguous index list
        for (int k=1; k<dimExtent[de*dimCount+j]; k++){
          if (indexList[k] != indexList[k-1]+1){
            contiguousFlag[i] = 0; // reset
            break;
          }
        }
        // clean-up
        delete indexListArg;
        delete [] indexList;
      }
    } // jj
        
  }
  delete [] dimExtent; 
  
  // invalidate the name for this Array object in the Base class
  ESMC_BaseSetName(NULL, "Array");
   
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayDestruct()"
//BOPI
// !IROUTINE:  ESMC_ArrayDestruct
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayDestruct(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure of an ESMC\_Array object.
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // garbage collection
  delete [] localDeList;
  for (int i=0; i<localDeCount; i++)
    ESMC_LocalArrayDestroy(larrayList[i]);
  delete [] larrayList;
  delete [] larrayBaseAddrList;
  delete [] exclusiveLBound;
  delete [] exclusiveUBound;
  delete [] computationalLBound;
  delete [] computationalUBound;
  delete [] totalLBound;
  delete [] totalUBound;
  delete [] lbounds;
  delete [] ubounds;
  delete [] staggerLoc;
  delete [] vectorDim;
  delete [] dimmap;
  delete [] inverseDimmap;
  delete [] contiguousFlag;
  delete [] deCellCount;
  
  // return successfully
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
#define ESMC_METHOD "ESMC_ArrayGet()"
//BOP
// !IROUTINE:  ESMC_ArrayGet
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_TypeKind *typekindArg,             // out - kind
  int *rankArg,                           // out - rank
  ESMC_LocalArray **localArrayList,       // out - localArrayList
  int localArrayListCount,                // in  - localArrayList elmt count
  ESMC_DistGrid **distgridArg,            // out - distgrid object
  ESMC_DELayout **delayoutArg,            // out - delayout object
  ESMC_IndexFlag *indexflagArg,           // out - indexflag
  ESMC_InterfaceInt *dimmapArg,           // out - dimmap
  ESMC_InterfaceInt *inverseDimmapArg,    // out - inverseDimmap
  ESMC_InterfaceInt *exclusiveLBoundArg,  // out -
  ESMC_InterfaceInt *exclusiveUBoundArg,  // out -
  ESMC_InterfaceInt *computationalLBoundArg,  // out -
  ESMC_InterfaceInt *computationalUBoundArg,  // out -
  ESMC_InterfaceInt *totalLBoundArg,      // out -
  ESMC_InterfaceInt *totalUBoundArg,      // out -
  ESMC_InterfaceInt *computationalLWidthArg,  // out -
  ESMC_InterfaceInt *computationalUWidthArg,  // out -
  ESMC_InterfaceInt *totalLWidthArg,      // out -
  ESMC_InterfaceInt *totalUWidthArg       // out -
  ){    
//
// !DESCRIPTION:
//    Get information about a Array object
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // fill simple return values
  if (typekindArg != NULL)
    *typekindArg = typekind;
  if (rankArg != NULL)
    *rankArg = rank;
  if (distgridArg != NULL)
    *distgridArg = distgrid;
  if (delayoutArg != NULL)
    *delayoutArg = delayout;
  if (indexflagArg != NULL)
    *indexflagArg = indexflag;

  // fill localArraylist
  if (localArrayListCount != 0){
    // localArrayList was provided
    if (localArrayListCount < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- localArrayList must provide localDeCount elements", rc);
      return localrc;
    }
    // localArrayListCount has correct number of elements
    for (int i=0; i<localDeCount; i++)
      localArrayList[i] = larrayList[i];
  }
  
  // fill dimmapArg
  if (dimmapArg != NULL){
    // dimmapArg was provided -> do some error checking
    if (dimmapArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmapArg array must be of rank 1", rc);
      return localrc;
    }
    if (dimmapArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of dimmapArg array must be of size 'dimCount'", rc);
      return localrc;
    }
    // fill in dimmapArg
    memcpy(dimmapArg->array, dimmap, dimCount*sizeof(int));
  }
  
  // fill inverseDimmapArg
  if (inverseDimmapArg != NULL){
    // inverseDimmapArg was provided -> do some error checking
    if (inverseDimmapArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- inverseDimmapArg array must be of rank 1", rc);
      return localrc;
    }
    if (inverseDimmapArg->extent[0] < rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of inverseDimmapArg array must be of size 'rank'", rc);
      return localrc;
    }
    // fill in dimmapArg
    memcpy(inverseDimmapArg->array, inverseDimmap, rank*sizeof(int));
  }

  // fill exclusiveLBound
  if (exclusiveLBoundArg != NULL){
    // exclusiveLBoundArg was provided -> do some error checking
    if (exclusiveLBoundArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- exclusiveLBoundArg array must be of rank 2", rc);
      return localrc;
    }
    if (exclusiveLBoundArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of exclusiveLBoundArg must be of size 'dimCount'", rc);
      return localrc;
    }
    if (exclusiveLBoundArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of exclusiveLBoundArg must be of size 'localDeCount'",
        rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in exclusiveLBoundArg
    // arrays which are larger than dimCount x localDeCount. Consequently it is
    // necessary to memcpy strips of contiguous data since it cannot be assumed
    // that all data ends up contiguous in the exclusiveLBoundArg array.
    for (int i=0; i<localDeCount; i++)
      memcpy(&(exclusiveLBoundArg->array[i*exclusiveLBoundArg->extent[0]]),
        &(exclusiveLBound[i*dimCount]), dimCount*sizeof(int));
  }
  
  // fill exclusiveUBound
  if (exclusiveUBoundArg != NULL){
    // exclusiveUBoundArg was provided -> do some error checking
    if (exclusiveUBoundArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- exclusiveUBoundArg array must be of rank 2", rc);
      return localrc;
    }
    if (exclusiveUBoundArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of exclusiveUBoundArg must be of size 'dimCount'", rc);
      return localrc;
    }
    if (exclusiveUBoundArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of exclusiveUBoundArg must be of size 'localDeCount'",
        rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in exclusiveUBoundArg
    // arrays which are larger than dimCount x localDeCount. Consequently it is
    // necessary to memcpy strips of contiguous data since it cannot be assumed
    // that all data ends up contiguous in the exclusiveUBoundArg array.
    for (int i=0; i<localDeCount; i++)
      memcpy(&(exclusiveUBoundArg->array[i*exclusiveUBoundArg->extent[0]]),
        &(exclusiveUBound[i*dimCount]), dimCount*sizeof(int));
  }

  // fill computationalLBound
  if (computationalLBoundArg != NULL){
    // computationalLBoundArg was provided -> do some error checking
    if (computationalLBoundArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLBoundArg array must be of rank 2", rc);
      return localrc;
    }
    if (computationalLBoundArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of computationalLBoundArg must be of size 'dimCount'",
        rc);
      return localrc;
    }
    if (computationalLBoundArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of computationalLBoundArg must be of size"
        " 'localDeCount'", rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in
    // computationalLBoundArg
    // arrays which are larger than dimCount x localDeCount. Consequently it is
    // necessary to memcpy strips of contiguous data since it cannot be assumed
    // that all data ends up contiguous in the computationalLBoundArg array.
    for (int i=0; i<localDeCount; i++)
      memcpy(
        &(computationalLBoundArg->array[i*computationalLBoundArg->extent[0]]),
        &(computationalLBound[i*dimCount]), dimCount*sizeof(int));
  }
  
  // fill computationalUBound
  if (computationalUBoundArg != NULL){
    // computationalUBoundArg was provided -> do some error checking
    if (computationalUBoundArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUBoundArg array must be of rank 2", rc);
      return localrc;
    }
    if (computationalUBoundArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of computationalUBoundArg must be of size 'dimCount'",
        rc);
      return localrc;
    }
    if (computationalUBoundArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of computationalUBoundArg must be of size"
        " 'localDeCount'", rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in    
    // computationalUBoundArg
    // arrays which are larger than dimCount x localDeCount. Consequently it is
    // necessary to memcpy strips of contiguous data since it cannot be assumed
    // that all data ends up contiguous in the computationalUBoundArg array.
    for (int i=0; i<localDeCount; i++)
      memcpy(
        &(computationalUBoundArg->array[i*computationalUBoundArg->extent[0]]),
        &(computationalUBound[i*dimCount]), dimCount*sizeof(int));
  }

  // fill totalLBound
  if (totalLBoundArg != NULL){
    // totalLBoundArg was provided -> do some error checking
    if (totalLBoundArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalLBoundArg array must be of rank 2", rc);
      return localrc;
    }
    if (totalLBoundArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of totalLBoundArg must be of size 'dimCount'", rc);
      return localrc;
    }
    if (totalLBoundArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of totalLBoundArg must be of size 'localDeCount'",
        rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in totalLBoundArg
    // arrays which are larger than dimCount x localDeCount. Consequently it is
    // necessary to memcpy strips of contiguous data since it cannot be assumed
    // that all data ends up contiguous in the totalLBoundArg array.
    for (int i=0; i<localDeCount; i++)
      memcpy(&(totalLBoundArg->array[i*totalLBoundArg->extent[0]]),
        &(totalLBound[i*dimCount]), dimCount*sizeof(int));
  }
  
  // fill totalUBound
  if (totalUBoundArg != NULL){
    // totalUBoundArg was provided -> do some error checking
    if (totalUBoundArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalUBoundArg array must be of rank 2", rc);
      return localrc;
    }
    if (totalUBoundArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of totalUBoundArg must be of size 'dimCount'", rc);
      return localrc;
    }
    if (totalUBoundArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of totalUBoundArg must be of size 'localDeCount'",
        rc);
      return localrc;
    }
    // fill in the values: The interface allows to pass in totalUBoundArg
    // arrays which are larger than dimCount x localDeCount. Consequently it is
    // necessary to memcpy strips of contiguous data since it cannot be assumed
    // that all data ends up contiguous in the totalUBoundArg array.
    for (int i=0; i<localDeCount; i++)
      memcpy(&(totalUBoundArg->array[i*totalUBoundArg->extent[0]]),
        &(totalUBound[i*dimCount]), dimCount*sizeof(int));
  }

  // fill computationalLWidth
  if (computationalLWidthArg != NULL){
    // computationalLWidthArg was provided -> do some error checking
    if (computationalLWidthArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLWidthArg array must be of rank 2", rc);
      return localrc;
    }
    if (computationalLWidthArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of computationalLWidthArg must be of size 'dimCount'",
        rc);
      return localrc;
    }
    if (computationalLWidthArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of computationalLWidthArg must be of size"
        " 'localDeCount'", rc);
      return localrc;
    }
    // fill in values
    for (int i=0; i<localDeCount; i++)
      for (int j=0; j<dimCount; j++)
        computationalLWidthArg->array[i*computationalLWidthArg->extent[0]+j] =
          exclusiveLBound[i*dimCount+j] - computationalLBound[i*dimCount+j];
  }
  
  // fill computationalUWidth
  if (computationalUWidthArg != NULL){
    // computationalUWidthArg was provided -> do some error checking
    if (computationalUWidthArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUWidthArg array must be of rank 2", rc);
      return localrc;
    }
    if (computationalUWidthArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of computationalUWidthArg must be of size 'dimCount'",
        rc);
      return localrc;
    }
    if (computationalUWidthArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of computationalUWidthArg must be of size"
        " 'localDeCount'", rc);
      return localrc;
    }
    // fill in values
    for (int i=0; i<localDeCount; i++)
      for (int j=0; j<dimCount; j++)
        computationalUWidthArg->array[i*computationalUWidthArg->extent[0]+j] =
          computationalUBound[i*dimCount+j] - exclusiveUBound[i*dimCount+j];
  }
  
  // fill totalLWidth
  if (totalLWidthArg != NULL){
    // totalLWidthArg was provided -> do some error checking
    if (totalLWidthArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalLWidthArg array must be of rank 2", rc);
      return localrc;
    }
    if (totalLWidthArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of totalLWidthArg must be of size 'dimCount'",
        rc);
      return localrc;
    }
    if (totalLWidthArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of totalLWidthArg must be of size"
        " 'localDeCount'", rc);
      return localrc;
    }
    // fill in values
    for (int i=0; i<localDeCount; i++)
      for (int j=0; j<dimCount; j++)
        totalLWidthArg->array[i*totalLWidthArg->extent[0]+j] =
          exclusiveLBound[i*dimCount+j] - totalLBound[i*dimCount+j];
  }
  
  // fill totalUWidth
  if (totalUWidthArg != NULL){
    // totalUWidthArg was provided -> do some error checking
    if (totalUWidthArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalUWidthArg array must be of rank 2", rc);
      return localrc;
    }
    if (totalUWidthArg->extent[0] < dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of totalUWidthArg must be of size 'dimCount'",
        rc);
      return localrc;
    }
    if (totalUWidthArg->extent[1] < localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of totalUWidthArg must be of size"
        " 'localDeCount'", rc);
      return localrc;
    }
    // fill in values
    for (int i=0; i<localDeCount; i++)
      for (int j=0; j<dimCount; j++)
        totalUWidthArg->array[i*totalUWidthArg->extent[0]+j] =
          totalUBound[i*dimCount+j] - exclusiveUBound[i*dimCount+j];
  }
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGetLinearIndexExclusive()"
//BOP
// !IROUTINE:  ESMC_ArrayGetLinearIndexExclusive
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayGetLinearIndexExclusive(
//
// !RETURN VALUE:
//    int linear index
//
// !ARGUMENTS:
//
  int localDe,                      // in - local DE
  int *index                        // in - DE-local index tupple in exclusive
                                    //      region basis 0
  ){    
//
// !DESCRIPTION:
//    Get linear index - assuming index input to be be basis 0 in excl. region
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // determine the linearized index
  //todo: the following assumes rank==dimCount -> will need to skip over not
  //      distributed dimensions when implemented
  int joff = localDe*rank;  // offset according to localDe index
  int linindex = exclusiveLBound[rank-1] - totalLBound[rank-1]
    + index[rank-1];  // initialize
  for (int j=rank-2; j>=0; j--){
    linindex *= totalUBound[joff+j] - totalLBound[joff+j] + 1;
    linindex += exclusiveLBound[j] - totalLBound[j] + index[j];
  }
      
  return linindex;    // leave this index basis 0
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
//
// print and validation class methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayPrint()"
//BOP
// !IROUTINE:  ESMC_ArrayPrint
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayPrint(){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Print details of Array object 
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", rc);
    return localrc;
  }

  // print info about the ESMC_Array object
  printf("--- ESMC_ArrayPrint start ---\n");
  printf("Array typekind/rank: %s / %d \n", ESMC_TypeKindString(typekind),
    rank);
  printf("~ cached values ~\n");
  printf("DistGrid dimCount = %d\n", dimCount);
  printf("deCount = %d\n", deCount);
  printf("localDeCount = %d\n", localDeCount);
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    printf("~ local data in LocalArray for DE %d ~\n", de);
    larrayList[i]->ESMC_LocalArrayPrint();
    if (deCellCount[de]){
      // associated DE
      int jjj=0;  // reset
      for (int jj=0; jj<rank; jj++){
        if (inverseDimmap[jj]){
          // distributed dimension
          int j = inverseDimmap[jj] - 1;  // shift to basis 0
          printf("dim %d: [%d]: [%d [%d [%d, %d] %d] %d]\n", 
            jj+1, j, 
            totalLBound[i*dimCount+j], computationalLBound[i*dimCount+j],
            exclusiveLBound[i*dimCount+j], exclusiveUBound[i*dimCount+j],
            computationalUBound[i*dimCount+j], totalUBound[i*dimCount+j]);
        }else{
          // non-distributed dimension
          printf("dim %d: lbounds[%d]=%d            ubounds[%d]=%d\n",
            jj+1, jjj, lbounds[jjj], jjj, ubounds[jjj]);
          ++jjj;
        }
      }
    }else{
      // unassociated DE
      printf("this DE is not associated with DistGrid points\n");
    }
  }
  printf("--- ESMC_ArrayPrint end ---\n");
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// serialize/deserialize class methods
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySerialize"
//BOPI
// !IROUTINE:  ESMC_ArraySerialize - Turn array information into a byte stream
//
// !INTERFACE:
int ESMC_Array::ESMC_ArraySerialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length
  int *offset) const {   // inout - original offset, updated to point 
                         //         to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in array class into a stream of bytes.
//
//EOPI
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  char *cp;
  int *ip;
  ESMC_TypeKind *dkp;
  ESMC_IndexFlag *ifp;

  if ((*length - *offset) < sizeof(ESMC_Array)){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an Array object", rc);
    return localrc;
  }

  // First, serialize the base class,
  status = ESMC_Base::ESMC_Serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  // Serialize the DistGrid
  status = distgrid->ESMC_DistGridSerialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  // Then, serialize Array meta data
  dkp = (ESMC_TypeKind *)(buffer + *offset);
  *dkp++ = typekind;
  ip = (int *)dkp;
  *ip++ = rank;
  ifp = (ESMC_IndexFlag *)ip;
  *ifp++ = indexflag;
  ip = (int *)ifp;
  *ip++ = dimCount;
  *ip++ = deCount;
  
  // fix offset  
  cp = (char *)ip;
  *offset = (cp - buffer);
  
  // return successfully
  return ESMF_SUCCESS;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_Deserialize"
//BOPI
// !IROUTINE:  ESMC_ArrayDeserialize - Turn a byte stream into an object
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayDeserialize(
//
// !RETURN VALUE:
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
  char *buffer,          // in - byte stream to read
  int *offset) {         // inout - original offset, updated to point 
                         //         to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  char *cp;
  int *ip;
  ESMC_TypeKind *dkp;
  ESMC_IndexFlag *ifp;

  // First, deserialize the base class
  status = ESMC_Base::ESMC_Deserialize(buffer, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  // Deserialize the DistGrid
  distgrid = ESMC_DistGridDeserialize(buffer, offset);
  // Pull DELayout out of DistGrid
  status = distgrid->ESMC_DistGridGet(&delayout, NULL, NULL, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  
  // Then, deserialize Array meta data
  dkp = (ESMC_TypeKind *)(buffer + *offset);
  typekind = *dkp++;
  ip = (int *)dkp;
  rank = *ip++;
  ifp = (ESMC_IndexFlag *)ip;
  indexflag = *ifp++;
  ip = (int *)ifp;
  dimCount = *ip++;
  deCount = *ip++;
  
  // fix offset
  cp = (char *)ip;
  *offset = (cp - buffer);
  
  // setting constant values for proxy objects
  localDeCount = 0;
  localDeList = NULL;
  larrayList = new ESMC_LocalArray*[localDeCount];
  larrayBaseAddrList = new void*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    larrayList[i] = NULL;
    larrayBaseAddrList[i] = NULL;
  }
  
  // return successfully
  return ESMF_SUCCESS;
}





//-----------------------------------------------------------------------------
//
// communication calls (internal)
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayScatter()"
//BOP
// !IROUTINE:  ESMC_ArrayScatter
//
// !INTERFACE:
int ESMC_Array::ESMC_ArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void *arrayArg,                       // in -
  ESMC_TypeKind typekindArg,            // in -
  int rankArg,                          // in -
  int *counts,                          // in -
  int *patchArg,                        // in -
  int rootPet,                          // in -
  ESMC_VM *vm                           // in -
  ){    
//
//
// !DESCRIPTION:
//    Scatter native array across Array object 
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", rc);
    return localrc;
  }

  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = ESMC_VMGetCurrent(&status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
      return localrc;
  }

  // query the VM for localPet and petCount
  int localPet, petCount;
  vm->ESMC_VMGet(&localPet, &petCount, NULL, NULL, NULL);
  
//printf("gjt in ArrayScatter: localPet: %d \n", localPet);
//printf("gjt in ArrayScatter: typekind/rank: %s / %d \n",
//ESMC_TypeKindString(typekindArg), rankArg);
//printf("gjt in ArrayScatter: counts: %d, %d, %d\n", counts[0], counts[1],
//counts[2]);

  // deal with optional patch argument
  int patch = 1;  // default
  if (patchArg)
    patch = *patchArg;
  int patchCount;
  int *dePatchList = new int[deCount];
  ESMC_InterfaceInt *dePatchListArg =
    new ESMC_InterfaceInt(dePatchList, 1, &deCount);
  status = distgrid->ESMC_DistGridGet(NULL, &patchCount, dePatchListArg, NULL,
    NULL, NULL);  
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  delete dePatchListArg;
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
      "- Specified patch out of bounds", rc);
    return localrc;
  }

  // check consistency of input information: shape = (typekind, rank, extents)
  int *minCorner, *maxCorner;
  minCorner = new int[dimCount];
  maxCorner = new int[dimCount];
  status = distgrid->ESMC_DistGridGetPatchMinMaxCorner(patch, minCorner,
    maxCorner);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  if (localPet == rootPet){
    if (typekindArg != typekind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between array argument and Array object", rc);
      return localrc;
    }
    if (rankArg != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Type mismatch between array argument and Array object", rc);
      return localrc;
    }
//for (int i=0; i<dimCount; i++)
//  printf("patchExtent[%d] = %d\n", i, maxCorner[i]-minCorner[i]+1);
    int tensorIndex=0;  // reset
    for (int i=0; i<rank; i++){
      int j = inverseDimmap[i];
      if (j){
        // decomposed dimension
        --j;  // shift to basis 0
        if (counts[i] != maxCorner[j] - minCorner[j] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", rc);
          return localrc;
        }
      }else{
        // tensor dimension
        if (counts[i] != ubounds[tensorIndex] - lbounds[tensorIndex] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", rc);
          return localrc;
        }
        ++tensorIndex;
      }
    }
  }

  // size in bytes of each piece of data  
  int dataSize = ESMC_TypeKindSize(typekind);

  // prepare for comms
  vmk_commhandle **commh = new vmk_commhandle*; // used by all comm calls
  
  // rootPet is the only sender
  char **sendBuffer;
  if (localPet == rootPet){
    char *array = (char *)arrayArg;
    // for each DE of the Array memcpy together a single contiguous sendBuffer
    // from "array" data and send it to the receiving PET non-blocking.
    // distgrid -> dimExtent[]
    int *dimExtent = new int[dimCount*deCount];
    int dummyLen[2];
    dummyLen[0] = dimCount;
    dummyLen[1] = deCount;
    ESMC_InterfaceInt *dimExtentArg =
      new ESMC_InterfaceInt(dimExtent, 2, dummyLen);
    status = distgrid->ESMC_DistGridGet(NULL, NULL, NULL, NULL, dimExtentArg,
      NULL);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
      return localrc;
    delete dimExtentArg;
    sendBuffer = new char*[deCount]; // contiguous sendBuffer
    int *ii = new int[rank];     // index tuple basis 0
    int *iiEnd = new int[rank];
    for (int i=0; i<deCount; i++){
      int de = i;
      if (dePatchList[de] == patch){
        // this DE is located on receiving patch
        int **indexList = new int*[rank];
        int *indexContigFlag = new int[rank];
        int tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            // obtain indexList for this DE and dim
            indexList[jj] = new int[dimExtent[de*dimCount+j]];
            ESMC_InterfaceInt *indexListArg =
              new ESMC_InterfaceInt(indexList[jj], 1,
                &(dimExtent[de*dimCount+j]));
            status = distgrid->ESMC_DistGridGet(de, j+1, indexListArg);
            if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, 
              rc)) return localrc;
            // shift basis 1 -> basis 0
            for (int k=0; k<dimExtent[de*dimCount+j]; k++)
              indexList[jj][k] -= minCorner[j];
            // check that this dim has a contiguous index list
            indexContigFlag[jj] = 1; // set
            for (int k=1; k<dimExtent[de*dimCount+j]; k++){
              if (indexList[jj][k] != indexList[jj][k-1]+1){
                indexContigFlag[jj] = 0; // reset
                break;
              }
            }
            // clean-up
            delete indexListArg;
          }else{
            // tensor dimension
            int extent = ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
            indexList[jj] = new int[extent];
            for (int k=0; k<extent; k++)
              indexList[jj][k] = k;   // basis 0
            indexContigFlag[jj] = 1;  // set
            ++tensorIndex;
          }
        } // jj
        sendBuffer[de] = new char[deCellCount[de]*dataSize]; // cont. sendBuffer
        
        int sendBufferIndex = 0;  // reset
        // reset counters for multi-dim while-loop
        tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          ii[jj] = 0;  // reset
          int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            iiEnd[jj] = dimExtent[de*dimCount+j];
          }else{
            // tensor dimension
            iiEnd[jj] = ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
            ++tensorIndex;
          }
        }
        // loop over all cells in exclusive region for this DE 
        // via multi-dim while-loop
        while(ii[rank-1] < iiEnd[rank-1]){        
          // determine linear index for this cell into array
          int linearIndex = indexList[rank-1][ii[rank-1]];  // init
          for (int j=rank-2; j>=0; j--){
            linearIndex *= counts[j];
            linearIndex += indexList[j][ii[j]];
          }
        
          // copy this element into the contiguous sendBuffer
          if (indexContigFlag[0]){
            // contiguous data in first dimension
            memcpy(sendBuffer[de]+sendBufferIndex*dataSize,
              array+linearIndex*dataSize, iiEnd[0]*dataSize);
            ii[0] = iiEnd[0]; // skip to end of 1st dimension
            sendBufferIndex += iiEnd[0];
          }else{
            // non-contiguous data in first dimension
            memcpy(sendBuffer[de]+sendBufferIndex*dataSize,
              array+linearIndex*dataSize, dataSize);
            ++ii[0];
            ++sendBufferIndex;
          }
                
          // multi-dim index increment
          for (int j=0; j<rank-1; j++){
            if (ii[j] == iiEnd[j]){
              ii[j] = 0;  // reset
              ++ii[j+1];
            }
          }
        } // multi-dim while-loop
    
        // ready to send the sendBuffer
        int dstPet;
        delayout->ESMC_DELayoutGetDEMatchPET(de, *vm, NULL, &dstPet, 1);
        *commh = NULL; // invalidate
        vm->vmk_send(sendBuffer[de], deCellCount[de]*dataSize, dstPet, commh);
        
        // clean-up
        for (int j=0; j<rank; j++)
          delete [] indexList[j];
        delete [] indexList;
        delete [] indexContigFlag;
      }
    } // i -> de
    
    delete [] ii;
    delete [] iiEnd;
  } // rootPet
  
  // - done issuing nb sends -

  // all PETs may be receivers
  char **recvBuffer = new char*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (dePatchList[de] != patch) continue; // skip to next local DE
    recvBuffer[i] = (char *)larrayBaseAddrList[i]; // default: contiguous
    if (!contiguousFlag[i])
      recvBuffer[i] = new char[deCellCount[de]*dataSize];
    *commh = NULL; // invalidate
    // receive data into recvBuffer
    vm->vmk_recv(recvBuffer[i], deCellCount[de]*dataSize, rootPet, commh);
  }
  
  // - done issuing nb receives -

  // todo: separate receive and send commhandles and separately wait!
  // wait until all the local receives are complete
  // wait until all the local sends are complete
  // for now wait on _all_ outstanding non-blocking comms for this PET
  vm->vmk_commqueuewait();
  
  // - done waiting on sends and receives -
    
  // distribute received data into non-contiguous exclusive regions
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (dePatchList[de] != patch) continue; // skip to next local DE
    if (!contiguousFlag[i]){
      // copy contiguous receive buffer into DE-local array segment piece by p.
      char *larrayBaseAddr = (char *)larrayBaseAddrList[i];
      // get contigFlag for first dimension for this de (dim is basis 1)
      int contigFlag;
      status = distgrid->ESMC_DistGridGet(de, 1, NULL, &contigFlag);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
        return localrc;
      // reset counters for multi-dim while-loop
      int *ii = new int[rank];        // index tuple basis 0
      int *iiSrt = new int[rank];
      int *iiEnd = new int[rank];
      int tensorIndex = 0;  // reset
      for (int jj=0; jj<rank; jj++){
        int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor dims
        if (j){
          // decomposed dimension 
          --j;  // shift to basis 0
          iiSrt[jj] = exclusiveLBound[i*dimCount+j] - totalLBound[i*dimCount+j];
          iiEnd[jj] = exclusiveUBound[i*dimCount+j] - totalLBound[i*dimCount+j]
            + 1;
        }else{
          // tensor dimension
          iiSrt[jj] = 0;
          iiEnd[jj] = ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
          ++tensorIndex;
        }
        ii[jj] = iiSrt[jj];
      }
      // loop over all cells in exclusive region for this DE and memcpy data
      // via multi-dim while-loop
      int recvBufferIndex = 0;  // reset
      while(ii[rank-1] < iiEnd[rank-1]){        
        // determine linear index for this cell into larrayBaseAddrList[i]
        int linearIndex = ii[rank-1];  // init
        for (int jj=rank-2; jj>=0; jj--){
          int j = inverseDimmap[jj];
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            linearIndex *= totalUBound[i*dimCount+j]
              - totalLBound[i*dimCount+j] + 1;
          }else{
            // tensor dimension
            linearIndex *= counts[jj];
          }
          linearIndex += ii[jj];
        }
        
        // copy this element from the contiguous recvBuffer into excl. region
        if (contigFlag){
          // contiguous data in first dimension
          memcpy(larrayBaseAddr+linearIndex*dataSize,
            recvBuffer[i]+recvBufferIndex*dataSize, 
              (iiEnd[0]-iiSrt[0])*dataSize);
          ii[0] = iiEnd[0]; // skip to end of 1st dimension
          recvBufferIndex += iiEnd[0] - iiSrt[0];
        }else{
          // non-contiguous data in first dimension
          memcpy(larrayBaseAddr+linearIndex*dataSize,
            recvBuffer[i]+recvBufferIndex*dataSize, dataSize);
          ++ii[0];
          ++recvBufferIndex;
        }
        
        // multi-dim index increment
        for (int j=0; j<rank-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = iiSrt[j];  // reset
            ++ii[j+1];
          }
        }
      } // multi-dim while-loop

      // clean-up
      delete [] recvBuffer[i];
      delete [] ii;
      delete [] iiEnd;
      delete [] iiSrt;
    } // !contiguousFlag
  } // i -> de
    
  // garbage collection
  delete [] minCorner;
  delete [] maxCorner;
  delete [] recvBuffer;
  delete [] dePatchList;
  if (localPet == rootPet){
    for (int i=0; i<deCount; i++)
      if (dePatchList[i] == patch)
        delete [] sendBuffer[i];
    delete [] sendBuffer;
  }
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
//
// communication calls (external)
//
//-----------------------------------------------------------------------------

//-- utility data structures: start

typedef struct{
  int factorListCount;
  ESMC_R8 *factorList;
  int *factorIndexList;
  void **receiveAddr;
  int *localSendDe;
  int *linSendIndex;
}FactorStorage;

typedef struct{
  int localDe;            // localDe this cell is located in
  int linIndex;           // linearized index into dstArray's data array
  int seqIndex;           // sequentialized DistGrid index for this cell
  int factorCount;        // number of factors in this cell's linear combination
  ESMC_R8 *factorList;     // sparse matrix factors for terms
  ESMC_R8 *valueList;      // srcArray values for terms
  //todo: for now assume that this is for double prec. data!
}TermStorage;

typedef struct{
  int count;              // number of receive calls
  int baseSize;           // base size in bytes of received data objects
  int *srcPet;            // PET on which src data resides
  void **addr;            // address of receive buffer
  vmk_commhandle **commh; // commhandles
}RecvTable;

typedef struct{
  int count;              // number of send calls
  int baseSize;           // base size in bytes of received data objects
  int *dstPet;            // PET on which destination resides
  int *localSrcDe;        // localDe index for source data
  int *linSrcIndex;       // linearized index into srcArray's data array
  vmk_commhandle **commh; // commhandles
}SendTable;

typedef struct{
  FactorStorage *factorStorage; // initial factor storage used during Store()
  int termCount;                // number of terms in term storage
  TermStorage **termStorage;    // term storage for each active local dst. cell
  RecvTable *recvTable;         // receive table
  SendTable *sendTable;         // send table
}ArraySparseMatMulStorage;

//-- utility data structures: end


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySparseMatMulStore()"
//BOP
// !IROUTINE:  ESMC_ArraySparseMatMulStore
//
// !INTERFACE:
int ESMC_ArraySparseMatMulStore(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_Array *srcArray,                 // in    -
  ESMC_Array *dstArray,                 // inout -
  ESMC_R8 *factorList,                   // in    -
  int factorListCount,                  // in    -
  ESMC_InterfaceInt *factorIndexList,   // in    -
  int rootPet,                          // in    -
  ESMC_RouteHandle **routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Store information for an Array sparse matrix multiplication operation
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  // get the current VM and VM releated information
  ESMC_VM *vm = ESMC_VMGetCurrent(&status);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  int localPet, petCount;
  status = vm->ESMC_VMGet(&localPet, &petCount, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;

  // error checking for input
  if (srcArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to srcArray", rc);
    return localrc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to dstArray", rc);
    return localrc;
  }
  if (localPet == rootPet){
    // only rootPet must provide valid factorList and factorIndexList args
    if (factorIndexList == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to factorIndexList array", rc);
      return localrc;
    }
    if (factorIndexList->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- factorIndexList array must be of rank 2", rc);
      return localrc;
    }
    if (factorIndexList->extent[0] != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- first dimension of factorIndexList array must be of size 2", rc);
      return localrc;
    }
    if (factorIndexList->extent[1] != factorListCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- second dimension of factorIndexList does not match factorListCount",
        rc);
      return localrc;
    }
  }
  
  // create and initialize the RouteHandle
  *routehandle = ESMC_RouteHandleCreate(&status);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  // todo: I have no idea what some of these settings do, just copied it for now
  // todo: from what I saw being set in ESMF_IArrayHaloStoreIndex()
  status =
    (*routehandle)->ESMC_RouteHandleSetType(ESMC_ARRAYSPARSEMATMULHANDLE);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  status = (*routehandle)->ESMC_RouteHandleSetRouteCount(1);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  status = (*routehandle)->ESMC_RouteHandleSetRMapType(ESMC_1TO1HANDLEMAP);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  status = (*routehandle)->ESMC_RouteHandleSetTVCount(0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  status = (*routehandle)->ESMC_RouteHandleSetTVMapType(ESMC_NOHANDLEMAP);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;

  // -->> here the RouteHandle needs to be filled with info!!!!  
  // 1) memory needs to be allocated to hold the factor lists for the dstArray
  //    parts that are stored on this PET.
  // 2) memory needs to be allocated to hold the srcArray elements that are
  //    needed for the multiplication

  // the "storage" variable holds all the allocations needed for sparse mat mul
  ArraySparseMatMulStorage *storage = new ArraySparseMatMulStorage;
  // attach "storage" to routehandle
  status = (*routehandle)->ESMC_RouteHandleSetStorage(storage);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
      
  // allocate and fill the factorStorage on all PETs
  storage->factorStorage = new FactorStorage;
  if (localPet == rootPet)
    storage->factorStorage->factorListCount = factorListCount;
  vm->vmk_broadcast(&(storage->factorStorage->factorListCount), sizeof(int),
    rootPet);
  storage->factorStorage->factorList =
    new ESMC_R8[storage->factorStorage->factorListCount];
  if (localPet == rootPet)
    memcpy(storage->factorStorage->factorList, factorList,
      factorListCount*sizeof(ESMC_R8));
  vm->vmk_broadcast(storage->factorStorage->factorList,
    storage->factorStorage->factorListCount*sizeof(ESMC_R8), rootPet);
  storage->factorStorage->factorIndexList =
    new int[2*storage->factorStorage->factorListCount];
  if (localPet == rootPet)
    memcpy(storage->factorStorage->factorIndexList, factorIndexList->array,
      2*factorListCount*sizeof(int));
  vm->vmk_broadcast(storage->factorStorage->factorIndexList,
    2*storage->factorStorage->factorListCount*sizeof(int), rootPet);
  // allocate and initialize memory to hold associated receive addresses
  storage->factorStorage->receiveAddr =
    new void*[storage->factorStorage->factorListCount];
  memset(storage->factorStorage->receiveAddr, 0,
    storage->factorStorage->factorListCount*sizeof(void *));
  // allocate and initialize arrays to hold associated send information
  storage->factorStorage->localSendDe =
    new int[storage->factorStorage->factorListCount];
  for (int i=0; i<storage->factorStorage->factorListCount; i++)
    storage->factorStorage->localSendDe[i] = -1;
  storage->factorStorage->linSendIndex =
    new int[storage->factorStorage->factorListCount];
  
  // determine the cells for each local DE in the dstArray that will be updated
  //todo: the following depends on rank == dimCount, i.e. all dims decomposed
  //      use the dimmap in dstArray and srcArray to skip over not decomposed
  //      dimensions
  // prepare temporary storage to hold info for all active local dst cells
  int totalLocalCellCount = 0; // reset
  for (int i=0; i<dstArray->localDeCount; i++){
    int de = dstArray->localDeList[i];
    int cellCount;
    dstArray->distgrid->ESMC_DistGridGet(de, &cellCount);
    totalLocalCellCount += cellCount;
  }
  TermStorage **tempTermStorage = new TermStorage*[totalLocalCellCount];
  int termIndex = 0;  // reset index into tempTermStorage
  int dstRank = dstArray->rank;
  int *ii = new int[dstRank];     // index tuple basis 0
  int *iiEnd = new int[dstRank];
  int totalRecvCount = 0; // reset
  for (int i=0; i<dstArray->localDeCount; i++){
    int de = dstArray->localDeList[i];
    int joff = i*dstRank; // offset according to localDe index
    // reset counters
    for (int j=0; j<dstRank; j++){
      ii[j] = 0;  // reset
      iiEnd[j] = dstArray->exclusiveUBound[joff+j]
        - dstArray->exclusiveLBound[joff+j] + 1;
    }
    // loop over all cells in exclusive region for this DE
    while(ii[dstRank-1] < iiEnd[dstRank-1]){
//      printf("DE = %d  - (", de);
//      int jjj;
//      for (jjj=0; jjj<dstRank-1; jjj++)
//        printf("%d, ", ii[jjj]);
//      printf("%d)\n", ii[jjj]);
      
      // determine the sequentialized index for cell ii[] in this DE
      // ESMC_DistGridGetSequenceIndex() expects basis 0 ii[] in excl. region
      int seqindex = dstArray->distgrid->ESMC_DistGridGetSequenceIndex(de, ii);
      // the seqindex can now be used to look-up the sparse mat factors
      int factorCount = 0;  // reset
      for (int k=0; k<storage->factorStorage->factorListCount; k++)
        if (storage->factorStorage->factorIndexList[2*k+1] == seqindex){
          ++factorCount;
          ++totalRecvCount;
        }

//      printf("sequentialized DistGrid index: %d with %d factors\n", 
//        seqindex, factorCount);
      
      if (factorCount){
        // this is an active cell -> store the sparse mat info
        tempTermStorage[termIndex] = new TermStorage;
        tempTermStorage[termIndex]->localDe = i;
        tempTermStorage[termIndex]->linIndex =
          dstArray->ESMC_ArrayGetLinearIndexExclusive(i, ii);
        tempTermStorage[termIndex]->seqIndex = seqindex;
        tempTermStorage[termIndex]->factorCount = factorCount;
        tempTermStorage[termIndex]->factorList = new ESMC_R8[factorCount];
        tempTermStorage[termIndex]->valueList = new ESMC_R8[factorCount];
        // fill in sparse mat factors
        int j=0;  // reset
        for (int k=0; k<storage->factorStorage->factorListCount; k++){
          if (storage->factorStorage->factorIndexList[2*k+1] == seqindex){
            // copy factor for this term into termStorage
            tempTermStorage[termIndex]->factorList[j] =
              storage->factorStorage->factorList[k];
            // take note of associated address of value element
            storage->factorStorage->receiveAddr[k] =
              &(tempTermStorage[termIndex]->valueList[j]);
            ++j;
          }
        }
        ++termIndex;
      }
            
      // multi-dim index increment
      ++ii[0];
      for (int j=0; j<dstRank-1; j++){
        if (ii[j] == iiEnd[j]){
          ii[j] = 0;  // reset
          ++ii[j+1];
        }
      }
    } // end while over all exclusive cells

    
  } // end for over local DEs
  delete [] ii;
  delete [] iiEnd;

//  printf("gjt: found %d cells on local PET in dstArray that are active\n"
//    "gjt: there are %d total receive calls on local PET\n", 
//    termIndex, totalRecvCount);
  
  // transfer the termStorage info into storage and delete temp. references
  storage->termCount = termIndex;
  storage->termStorage = new TermStorage*[termIndex];
  memcpy(storage->termStorage, tempTermStorage, termIndex*sizeof(TermStorage*));
  delete [] tempTermStorage;  // this does _not_ delete the termStorage elements
  
  // construct local receive table
  storage->recvTable = new RecvTable;
  storage->recvTable->count = totalRecvCount;
  storage->recvTable->baseSize = sizeof(ESMC_R8);  //todo: don't hardcode this!
  storage->recvTable->srcPet = new int[totalRecvCount];
  storage->recvTable->addr = new void*[totalRecvCount];
  storage->recvTable->commh = new vmk_commhandle*[totalRecvCount];
  for (int i=0; i<totalRecvCount; i++)
    storage->recvTable->commh[i] = new vmk_commhandle;
  int recvIndex=0;  // reset
  for (int k=0; k<storage->factorStorage->factorListCount; k++){
    if (storage->factorStorage->receiveAddr[k] != NULL){
      // found an element that must be received 
      storage->recvTable->addr[recvIndex] =
        storage->factorStorage->receiveAddr[k];
      // find the sending PET for this element out of srcArray
      int srcDe = srcArray->distgrid->ESMC_DistGridGetSequenceDe(
        storage->factorStorage->factorIndexList[2*k]);
      int srcPet;
      srcArray->delayout->ESMC_DELayoutGetDEMatchPET(srcDe, *vm, NULL, &srcPet,
        1);
      storage->recvTable->srcPet[recvIndex] = srcPet;
//printf("gjt: found element that local PET must receive from remote srcArray:"
//  " DE %d, PET %d\n", srcDe, srcPet);
      ++recvIndex;
    }
  }
  
  // determine the cells for each local DE in the srcArray that will be send
  int srcRank = srcArray->rank;
  ii = new int[srcRank];     // index tuple basis 0
  iiEnd = new int[srcRank];
  int totalSendCount = 0; // reset
  for (int i=0; i<srcArray->localDeCount; i++){
    int de = srcArray->localDeList[i];
    int joff = i*srcRank; // offset according to localDe index
    // reset counters
    for (int j=0; j<srcRank; j++){
      ii[j] = 0;  // reset
      iiEnd[j] = srcArray->exclusiveUBound[joff+j]
        - srcArray->exclusiveLBound[joff+j] + 1;
    }
    // loop over all cells in exclusive region for this DE
    while(ii[srcRank-1] < iiEnd[srcRank-1]){
//      printf("src: DE = %d  - (", de);
//      int jjj;
//      for (jjj=0; jjj<srcRank-1; jjj++)
//        printf("%d, ", ii[jjj]);
//      printf("%d)\n", ii[jjj]);
      
      // determine the sequentialized index for cell ii[] in this DE
      // ESMC_DistGridGetSequenceIndex() expects basis 0 ii[] in excl. region
      int seqindex = srcArray->distgrid->ESMC_DistGridGetSequenceIndex(de, ii);
      // the seqindex can now be used to look-up the sparse mat elements
      for (int k=0; k<storage->factorStorage->factorListCount; k++)
        if (storage->factorStorage->factorIndexList[2*k] == seqindex){
          // take note of associated address of element in srcArray
          storage->factorStorage->localSendDe[k] = i;
          storage->factorStorage->linSendIndex[k] =
            srcArray->ESMC_ArrayGetLinearIndexExclusive(i, ii);
          ++totalSendCount;
        }
      
      // multi-dim index increment
      ++ii[0];
      for (int j=0; j<srcRank-1; j++){
        if (ii[j] == iiEnd[j]){
          ii[j] = 0;  // reset
          ++ii[j+1];
        }
      }
    } // end while over all exclusive cells
  } // end for over local DEs
  delete [] ii;
  delete [] iiEnd;
  
//  printf("gjt: there are %d total send calls on local PET\n", totalSendCount);
  
  // construct local send table
  storage->sendTable = new SendTable;
  storage->sendTable->count = totalSendCount;
  storage->sendTable->baseSize = sizeof(ESMC_R8);  //todo: don't hardcode this!
  storage->sendTable->dstPet = new int[totalSendCount];
  storage->sendTable->localSrcDe = new int[totalSendCount];
  storage->sendTable->linSrcIndex = new int[totalSendCount];
  storage->sendTable->commh = new vmk_commhandle*[totalSendCount];
  for (int i=0; i<totalSendCount; i++)
    storage->sendTable->commh[i] = new vmk_commhandle;
  int sendIndex=0;  // reset
  for (int k=0; k<storage->factorStorage->factorListCount; k++){
    if (storage->factorStorage->localSendDe[k] != -1){
      // found an element that must be sent
      storage->sendTable->localSrcDe[sendIndex] =
        storage->factorStorage->localSendDe[k];
      storage->sendTable->linSrcIndex[sendIndex] =
        storage->factorStorage->linSendIndex[k];
      // find the receiving PET for this element out of dstArray
      int dstDe = dstArray->distgrid->ESMC_DistGridGetSequenceDe(
        storage->factorStorage->factorIndexList[2*k+1]);
      int dstPet;
      dstArray->delayout->ESMC_DELayoutGetDEMatchPET(dstDe, *vm, NULL, &dstPet,
        1);
      storage->sendTable->dstPet[sendIndex] = dstPet;
//printf("gjt: found element that must be sent by local PET to remote dstArray:"
//  " DE %d, PET %d\n", dstDe, dstPet);
      ++sendIndex;
    }
  }
    
    
  // maybe someday we'll use the Route code for this, right now things don't
  // really fit together ...
  
  // create the Route
//  ESMC_Route *route = ESMC_RouteCreate(vm, &status);
//  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
//    return localrc;

  // -->> here the Route needs to be filled with info!!!!  
  // 1) a route needs to be determined for the srcArray elements to be
  //    communicated to the temporary memory on the dstArray side.
    
  // store the Route in the RouteHandle
//  status = (*routehandle)->ESMC_RouteHandleSetRoute(0, route);
  
  
  // delete the local factor storage as it is not needed any longer
  delete [] storage->factorStorage->localSendDe;
  delete [] storage->factorStorage->linSendIndex;
  delete [] storage->factorStorage->receiveAddr;
  delete [] storage->factorStorage->factorIndexList;
  delete [] storage->factorStorage->factorList;
  delete [] storage->factorStorage;
    
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySparseMatMul()"
//BOP
// !IROUTINE:  ESMC_ArraySparseMatMul
//
// !INTERFACE:
int ESMC_ArraySparseMatMul(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_Array *srcArray,                 // in    -
  ESMC_Array *dstArray,                 // inout -
  ESMC_RouteHandle **routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Store information for an Array sparse matrix multiplication operation
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  // get the current VM and VM releated information
  ESMC_VM *vm = ESMC_VMGetCurrent(&status);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  int localPet, petCount;
  status = vm->ESMC_VMGet(&localPet, &petCount, NULL, NULL, NULL);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;

  // error checking for input
  if (srcArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to srcArray", rc);
    return localrc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to dstArray", rc);
    return localrc;
  }

  // get a handle on the storage in routehandle
  ArraySparseMatMulStorage *storage = 
    (ArraySparseMatMulStorage *)(*routehandle)->ESMC_RouteHandleGetStorage();

  // loop through send table and issue non-blocking sends
  int size = storage->sendTable->baseSize;
  for (int i=0; i<storage->sendTable->count; i++){
    char *element = 
      (char *)srcArray->larrayBaseAddrList[storage->sendTable->localSrcDe[i]];
    element += storage->sendTable->linSrcIndex[i] * size;
    vm->vmk_send(element, size, storage->sendTable->dstPet[i],
      &(storage->sendTable->commh[i]));
  }
  
  // loop through recv table and issue non-blocking receives
  size = storage->recvTable->baseSize;
  for (int i=0; i<storage->recvTable->count; i++){
    char *element = (char *)storage->recvTable->addr[i];
    vm->vmk_recv(element, size, storage->recvTable->srcPet[i],
      &(storage->recvTable->commh[i]));
  }

  // wait until all local receive calls have completed
  for (int i=0; i<storage->recvTable->count; i++)
    vm->vmk_commwait(&(storage->recvTable->commh[i]));
  
  // loop through termStorage and compute local results
  for (int i=0; i<storage->termCount; i++){
    TermStorage *termStorage = storage->termStorage[i];
    //todo: don't hardcode ESMC_R8 here
    ESMC_R8 *element = 
      (ESMC_R8 *)dstArray->larrayBaseAddrList[termStorage->localDe];
    element += termStorage->linIndex; // shift to correct element
    ESMC_R8 *factorList = termStorage->factorList;
    ESMC_R8 *valueList = termStorage->valueList;
    for (int j=0; j<termStorage->factorCount; j++){
      *element += factorList[j] * valueList[j]; // compute sparse mat mul term
    }
  }
  
  // wait until all local send calls have completed
  for (int i=0; i<storage->sendTable->count; i++)
    vm->vmk_commwait(&(storage->sendTable->commh[i]));
  
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArraySparseMatMulRelease()"
//BOP
// !IROUTINE:  ESMC_ArraySparseMatMulRelease
//
// !INTERFACE:
int ESMC_ArraySparseMatMulRelease(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_RouteHandle *routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Release information for an Array sparse matrix multiplication operation
//
//EOP
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  *rc = ESMC_RC_NOT_IMPL;

  // get a handle on the storage in routehandle
  ArraySparseMatMulStorage *storage = 
    (ArraySparseMatMulStorage *)routehandle->ESMC_RouteHandleGetStorage();

  // delete termStorage
  for (int i=0; i<storage->termCount; i++){
    delete [] storage->termStorage[i]->factorList;
    delete [] storage->termStorage[i]->valueList;
    delete storage->termStorage[i];
  }
  delete [] storage->termStorage;
  
  // delete recvTable
  for (int i=0; i<storage->recvTable->count; i++){
    delete storage->recvTable->commh[i];
  }
  delete [] storage->recvTable->srcPet;
  delete [] storage->recvTable->addr;
  delete [] storage->recvTable->commh;
  delete storage->recvTable;
  
  // delete sendTable
  for (int i=0; i<storage->sendTable->count; i++){
    delete storage->sendTable->commh[i];
  }
  delete [] storage->sendTable->dstPet;
  delete [] storage->sendTable->localSrcDe;
  delete [] storage->sendTable->linSrcIndex;
  delete [] storage->sendTable->commh;
  delete storage->sendTable;
  
  // delete storage handle
  delete storage;
  routehandle->ESMC_RouteHandleSetStorage(NULL);
  
  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------





//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------




//-------------------------------------------------------------------------
// The following code is for a first newArray prototype which I used
// to check out some communication ideas: DE-nonblocking paradigm!
//-------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// This section includes all the Array routines
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayConstruct()"
//BOP
// !IROUTINE:  ESMC_newArrayConstruct
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int *haloWidth,           // halo width
  ESMC_DELayout *delayout,  // DELayout
  int rootPET,              // root
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Construct the internal information structure in a new ESMC\_newArray
//
//EOP
//-----------------------------------------------------------------------------
  int localrc;
  //Initialize localrc; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET, petCount;
  vm->ESMC_VMGet(&localPET, &petCount, NULL, NULL, NULL);
  int localTid;
  vm->ESMC_VMGetPETLocalInfo(localPET, NULL, NULL, NULL, &localTid,
    &localVAS);
  // get info about the LocalArray on rootPET and broadcast it to all PETs
  int laRank;
  if (localPET == rootPET)
    laRank = larray->ESMC_LocalArrayGetRank();
  vm->vmk_broadcast(&laRank, sizeof(int), rootPET);
  int *laLength = new int[laRank];
  if (localPET == rootPET)
    larray->ESMC_LocalArrayGetLengths(laRank, laLength);
  vm->vmk_broadcast(laLength, laRank * sizeof(int), rootPET);
  int *laLbound = new int[laRank];
  if (localPET == rootPET)
    larray->ESMC_LocalArrayGetLbounds(laRank, laLbound);
  vm->vmk_broadcast(laLbound, laRank * sizeof(int), rootPET);
  // set some newArray members
  rank = laRank;  // newArray's rank is equal to that of the origin LocalArray
  int decompRank;
  this->delayout = delayout;
  delayout->ESMC_DELayoutGetDeprecated(&deCount, &decompRank, &localDeCount, NULL, 0,
    NULL, NULL, NULL, NULL, 0);
  localDeList = new int[localDeCount];
  delayout->ESMC_DELayoutGetDeprecated(NULL, NULL, NULL, localDeList,
    localDeCount, NULL, NULL, NULL, NULL, 0);
  deVASList = new int[deCount];
  for (int de=0; de<deCount; de++)
    delayout->ESMC_DELayoutGetDELocalInfo(de, NULL, 0, NULL, 0, NULL, 0, NULL,
      &(deVASList[de]));
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayConstruct: decompRank=%d, deCount=%d\n",
    decompRank, deCount);
#endif
  // determine the global min/max for all DELayout dimensions
  int *deCoordMin = new int[decompRank];
  int *deCoordMax = new int[decompRank];
  int *temp_deCoordMin = new int[decompRank];
  int *temp_deCoordMax = new int[decompRank];
//need to use the new DistGrid for this
//  delayout->ESMC_DELayoutGetDELocalCoord(0, deCoordMin, deCoordMax);
  for (int i=0; i<deCount; i++){
//need to use the new DistGrid for this
//    delayout->ESMC_DELayoutGetDELocalCoord(i, temp_deCoordMin, temp_deCoordMax);
    for (int j=0; j<decompRank; j++){
      if (temp_deCoordMin[j]<deCoordMin[j]) deCoordMin[j] = temp_deCoordMin[j];
      if (temp_deCoordMax[j]>deCoordMax[j]) deCoordMax[j] = temp_deCoordMax[j];
    }
  }
#if (VERBOSITY > 9)
  for (int j=0; j<decompRank; j++)
    printf("gjt in ESMC_newArrayConstruct: CoordMin=%d, CoordMax=%d\n",
      deCoordMin[j], deCoordMax[j]);
#endif
  // pre-chunk the decomposed larray dimensions according to DELayout axis
  int *deCoordLength = new int[decompRank];
  for (int i=0; i<decompRank; i++)
    deCoordLength[i] = deCoordMax[i] - deCoordMin[i] + 1;
  int ***arrayPreChunk = new int**[decompRank];
  for (int i=0; i<decompRank; i++){
    arrayPreChunk[i] = new int*[deCoordLength[i]];
    for (int j=0; j<deCoordLength[i]; j++){
      arrayPreChunk[i][j] = new int[2];
      arrayPreChunk[i][j][0] = deCoordMin[i] + j;
    }
  }
  
  int k=0;
  for (int i=0; i<laRank; i++){
    if (haloWidth[i]>=0){
      // this dimension may be decomposed
      for (int j=0; j<deCoordLength[k]; j++){
        // fill in evenly distributed preChunking
        arrayPreChunk[k][j][1] = laLength[i]/deCoordLength[k];
        if (j<laLength[i]%deCoordLength[k]) ++arrayPreChunk[k][j][1];
#if (VERBOSITY > 9)
        printf("gjt in ESMC_newArrayConstruct: arrayPreChunk[%d][%d][] = "
          "(%d, %d)\n", k, j, arrayPreChunk[k][j][0], arrayPreChunk[k][j][1]);
#endif
      }
      ++k;
    }
  }
  
  // fill in the newArray's meta info about the data bounds for each DE
  globalDataLBound = new int*[deCount];
  globalDataUBound = new int*[deCount];
  localFullLBound = new int*[deCount];
  localFullUBound = new int*[deCount];
  globalFullLBound = new int*[deCount];
  globalFullUBound = new int*[deCount];
  dataOffset = new int*[deCount];
  for (int de=0; de<deCount; de++){
    globalDataLBound[de] = new int[rank];
    globalDataUBound[de] = new int[rank];
    localFullLBound[de] = new int[rank];
    localFullUBound[de] = new int[rank];
    globalFullLBound[de] = new int[rank];
    globalFullUBound[de] = new int[rank];
    dataOffset[de] = new int[rank];
//need to use the new DistGrid for this
//    delayout->ESMC_DELayoutGetDELocalCoord(de, deCoordMin, deCoordMax);
    int k=0;
    for (int i=0; i<laRank; i++){
      globalDataLBound[de][i] = 0;  // first use global frame starting at 0
      if (haloWidth[i]>=0){
        // this dimension may be decomposed
        globalDataUBound[de][i] = globalDataLBound[de][i];
        for (int j=0; j<deCoordLength[k]; j++){
          if (arrayPreChunk[k][j][0] < deCoordMin[k])
            globalDataLBound[de][i] += arrayPreChunk[k][j][1];
          if (arrayPreChunk[k][j][0] <= deCoordMax[k])
            globalDataUBound[de][i] += arrayPreChunk[k][j][1];
          else break;
        }
        ++k;
      }else{
        globalDataUBound[de][i] = globalDataLBound[de][i] + laLength[i];
      }
      globalDataUBound[de][i] -= 1; // adjustment of upper bound
      globalFullLBound[de][i] = globalDataLBound[de][i]; // prepare globalFullLB
      globalFullUBound[de][i] = globalDataUBound[de][i]; // prepare globalFullUB
      if (haloWidth[i]>=0){
        // valid halo width for this dimension
        // add halo region (assuming non-periodic boundaries in all directions)
        if (globalFullLBound[de][i] > 0)
          globalFullLBound[de][i] -= haloWidth[i];
        if (globalFullUBound[de][i] < laLength[i]-1)
          globalFullUBound[de][i] += haloWidth[i];
      }
      // determine dataOffset and shift to local bounds for full box
      dataOffset[de][i] = globalDataLBound[de][i] - globalFullLBound[de][i];
      localFullLBound[de][i] = 0;
      localFullUBound[de][i] = globalFullUBound[de][i]
        - globalFullLBound[de][i];
      globalDataLBound[de][i] += laLbound[i];  // shift into global frame
      globalDataUBound[de][i] += laLbound[i];  // shift into global frame
      globalFullLBound[de][i] += laLbound[i];  // shift into global frame
      globalFullUBound[de][i] += laLbound[i];  // shift into global frame
    }
  }
  
  // now the LocalArrays for all the DEs on this PET must be created
  if (localPET == rootPET){
    kind = larray->ESMC_LocalArrayGetTypeKind();
    vm->vmk_broadcast(&kind, sizeof(ESMC_TypeKind), rootPET);
  }else{
    vm->vmk_broadcast(&kind, sizeof(ESMC_TypeKind), rootPET);
  }
  if (localTid == 0){
    // this is the master thread of an ESMF-thread group
    localArrays = new ESMC_LocalArray*[localDeCount];
    commhArray = new ESMC_newArrayCommHandle[localDeCount];
    thargArray = new ESMC_newArrayThreadArg[localDeCount];
    // need to send localArrays/commhArray pointer to other PETs in threadGroup
    for (int pet=0; pet<petCount; pet++){
      int vas;
      vm->ESMC_VMGetPETLocalInfo(pet, NULL, NULL, NULL, NULL, &vas);
      if (pet != localPET && vas == localVAS){
        // send the localArrays pointer to this PET
        vm->vmk_send(&localArrays, sizeof(ESMC_LocalArray **), pet);
        vm->vmk_send(&commhArray, sizeof(ESMC_newArrayCommHandle *), pet);
        vm->vmk_send(&thargArray, sizeof(ESMC_newArrayThreadArg *), pet);
      }
    }
  }else{
    // localPET is part of a threadGroup but is not the master thread
    int pet;
    for (pet=0; pet<petCount; pet++){
      int vas, tid;
      vm->ESMC_VMGetPETLocalInfo(pet, NULL, NULL, NULL, &tid, &vas);
      if (vas == localVAS && tid == 0) break; // found master thread
    }
    // receive the localArrays and commhArray pointer from master thread
    vm->vmk_recv(&localArrays, sizeof(ESMC_LocalArray **), pet);
    vm->vmk_recv(&commhArray, sizeof(ESMC_newArrayCommHandle *), pet);
    vm->vmk_recv(&thargArray, sizeof(ESMC_newArrayThreadArg *), pet);
  }
  int *temp_counts = new int[rank];
  int de;
  for (int i=0; i<localDeCount; i++){
    de = localDeList[i];
    if (delayout->ESMC_DELayoutServiceOffer(de, NULL)
      == ESMC_DELAYOUT_SERVICE_ACCEPT){
      for (int j=0; j<rank; j++)
        temp_counts[j] = localFullUBound[de][j] - localFullLBound[de][j] + 1;
      localArrays[i] = ESMC_LocalArrayCreate(rank, kind, temp_counts);
      commhArray[i].commhandleCount = 0;  // reset
      commhArray[i].pthidCount = 0;       // reset
      commhArray[i].buffer = NULL;        // reset
      delayout->ESMC_DELayoutServiceComplete(de);
    }
  }
  // scatter the larray across the newly constructed narray
  localrc = ESMC_newArrayScatter(larray, rootPET, vm);
  
  // garbage collection
  delete [] temp_counts;
  for (int i=0; i<decompRank; i++){
    for (int j=0; j<deCoordLength[i]; j++)
      delete [] arrayPreChunk[i][j];
    delete [] arrayPreChunk[i];
  }
  delete [] arrayPreChunk;
  delete [] deCoordLength;  
  delete [] temp_deCoordMin;
  delete [] temp_deCoordMax;
  delete [] deCoordMin;
  delete [] deCoordMax;
  delete [] laLbound;
  delete [] laLength;

  // error handling via LogErr
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
    ESMC_NULL_POINTER)) return localrc;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayDestruct()"
//BOP
// !IROUTINE:  ESMC_newArrayDestruct
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayDestruct(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure in a new ESMC\_newArray
//
//EOP
//-----------------------------------------------------------------------------
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScatter()"
//BOP
// !IROUTINE:  ESMC_newArrayScatter
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int rootPET,              // root
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. PET-based blocking paradigm.
//
//EOP
//-----------------------------------------------------------------------------
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // check that there is a valid larray on rootPET 
  if (localPET == rootPET){
    if (larray == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'larray' argument on rootPET.", &localrc);
      return localrc;
    }
  }    
  // check that t/k/r matches
  if (localPET == rootPET){
    int laRank = larray->ESMC_LocalArrayGetRank();
    if (laRank != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
        "- ranks don't match", &localrc);
      return localrc;
    }
    ESMC_TypeKind laTypeKind = larray->ESMC_LocalArrayGetTypeKind();
    if (laTypeKind != kind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
        "- kinds don't match", &localrc);
      return localrc;
    }
  }
  // broadcast some of larray's meta info
  int *laLength = new int[rank];
  if (localPET == rootPET)
    larray->ESMC_LocalArrayGetLengths(rank, laLength);
  vm->vmk_broadcast(laLength, rank * sizeof(int), rootPET);
  int *laLbound = new int[rank];
  if (localPET == rootPET)
    larray->ESMC_LocalArrayGetLbounds(rank, laLbound);
  vm->vmk_broadcast(laLbound, rank * sizeof(int), rootPET);
  int laByteCount;
  if (localPET == rootPET)
    larray->ESMC_LocalArrayGetByteCount(&laByteCount);
  vm->vmk_broadcast(&laByteCount, sizeof(int), rootPET);
  // get info out of the associated localArrays
  void **localDeArrayBase = new void*[localDeCount];
  for (int i=0; i<localDeCount; i++)
    localArrays[i]->ESMC_LocalArrayGetBaseAddr(&localDeArrayBase[i]);
  // prepare a temporary buffer
  int blockCount = 1;
  for (int i=1; i<rank; i++)
    blockCount *= laLength[i];
  int blockSize = laByteCount / blockCount;
  int elementSize = blockSize / laLength[0];
#if (VERBOSITY > 1)
  printf("gjt in ESMC_newArrayScatter: elementSize = %d\n", elementSize);
#endif
  char *buffer;
  if (localPET == rootPET)
    larray->ESMC_LocalArrayGetBaseAddr((void **)&buffer); // start of data
  else
    buffer = new char[blockSize];   // buffer for first dim.
  // setup a blockID which basically is an array holding the higher dimensions
  int *blockID = new int[rank];   // blockID[0] is unused!
  for (int i=0; i<rank; i++)
    blockID[i] = 0;
  int *blockLocalIndex = new int[rank];
  int *blockGlobalIndex = new int[rank];
  blockGlobalIndex[0] = laLbound[0];
  // now loop over all blocks
  for (int blk=0; blk<blockCount; blk++){
    for (int i=1; i<rank; i++)
      blockGlobalIndex[i] = laLbound[i] + blockID[i];
#if (VERBOSITY > 1)
    printf("gjt in ESMC_newArrayScatter: blockID: ");
    for (int i=1; i<rank-1; i++)
      printf("%d, ", blockID[i]);
    printf("%d\n", blockID[rank-1]);
#endif
    // broadcast a block of data
    vm->vmk_broadcast(buffer, blockSize, rootPET);
    // loop over local DEs
    for (int ide=0; ide<localDeCount; ide++){
      int de = localDeList[ide];
      if (delayout->ESMC_DELayoutServiceOffer(de, NULL)
        == ESMC_DELAYOUT_SERVICE_ACCEPT){
        // the localPET's offer was accepted by DELayout
        // check whether this DE's fullBox intersects the current block...
        int ii;
        for (ii=1; ii<rank; ii++){
#if (VERBOSITY > 1)
          printf("gjt in ESMC_newArrayScatter: ide=%d, blockGlobalIndex=%d\n"
            "globalXXX: %d, %d, %d, %d\n", ide, 
            blockGlobalIndex[ii], globalFullLBound[de][ii],
            globalDataLBound[de][ii], globalDataUBound[de][ii],
            globalFullUBound[de][ii]);
#endif    
          // check intersection with dataBox
          if (blockGlobalIndex[ii] >= globalDataLBound[de][ii] &&
            blockGlobalIndex[ii] <= globalDataUBound[de][ii]){
#if (VERBOSITY > 1)
            printf("gjt in ESMC_newArrayScatter: intersect dataBox\n");
#endif    
            // found intersection with dataBox
            blockLocalIndex[ii] = blockGlobalIndex[ii] 
              - globalDataLBound[de][ii]
              + dataOffset[de][ii];
            continue;
          }
          //check intersection with lower halo region
          if (globalFullLBound[de][ii] <= globalDataLBound[de][ii]){
            // this is a regular lower halo region
            if (blockGlobalIndex[ii] >= globalFullLBound[de][ii] &&
              blockGlobalIndex[ii] < globalDataLBound[de][ii]){
#if (VERBOSITY > 1)
              printf("gjt in ESMC_newArrayScatter: intersect regular lower"
                " halo\n");
#endif    
              // found intersection with regular lower halo region
              blockLocalIndex[ii] = blockGlobalIndex[ii] 
                - globalFullLBound[de][ii];
              continue;
            }
          }else{
            // this is a periodic lower halo region from the other end
            if (blockGlobalIndex[ii] >= globalFullLBound[de][ii] &&
              blockGlobalIndex[ii] <=
                (globalFullLBound[de][ii] + dataOffset[de][ii])){
#if (VERBOSITY > 1)
              printf("gjt in ESMC_newArrayScatter: intersect periodic lower"
                " halo\n");
#endif    
              // found intersection with periodic lower halo region
              blockLocalIndex[ii] = blockGlobalIndex[ii] 
                - globalFullLBound[de][ii];
              continue;
            }
          }
          //check intersection with upper halo region
          if (globalFullUBound[de][ii] >= globalDataUBound[de][ii]){
            // this is a regular upper halo region
            if (blockGlobalIndex[ii] > globalDataUBound[de][ii] &&
              blockGlobalIndex[ii] <= globalFullUBound[de][ii]){
#if (VERBOSITY > 1)
              printf("gjt in ESMC_newArrayScatter: intersect regular upper"
                " halo\n");
#endif    
              // found intersection with regular upper halo region
              blockLocalIndex[ii] = blockGlobalIndex[ii] 
                - globalDataLBound[de][ii] + dataOffset[de][ii];
              continue;
            }
          }else{
            // this is a periodic upper halo region from the other end
            int upperDataOffset = 
              localFullUBound[de][ii] - localFullLBound[de][ii]
              - dataOffset[de][ii]
              - (globalDataUBound[de][ii] - globalDataLBound[de][ii]);
            if (blockGlobalIndex[ii] >= 
              (globalFullUBound[de][ii] - upperDataOffset) &&
              blockGlobalIndex[ii] <= globalFullUBound[de][ii]){
#if (VERBOSITY > 1)
              printf("gjt in ESMC_newArrayScatter: intersect periodic upper"
                " halo\n");
#endif    
              // found intersection with periodic upper halo region
              blockLocalIndex[ii] = blockGlobalIndex[ii]
                - (globalFullUBound[de][ii] - upperDataOffset);
              continue;
            }
          }
          break;
        }        
#if (VERBOSITY > 1)
        if (ii==rank)
          printf("gjt in ESMC_newArrayScatter: block inside fullBox\n");
        else
          printf("gjt in ESMC_newArrayScatter: block outside fullBox\n");
#endif
        blockLocalIndex[0] = blockGlobalIndex[0] - globalFullLBound[de][0];
        if (ii==rank){
          // block intersects fullBox ->
          // find start address of DE's data column overlaping with block data
          char *base = (char *)localDeArrayBase[ide];
          int elementCount = 0;
          for (int i=rank-1; i>0; i--){
            elementCount += blockLocalIndex[i];
            elementCount *= (localFullUBound[de][i-1] 
              - localFullLBound[de][i-1] + 1);
          }
          base += elementCount * elementSize;
#if (VERBOSITY > 1)
          printf("gjt in ESMC_newArrayScatter: elementCount = %d, base = %p\n",
            elementCount, base);
#endif
          // determine start pointers of buffer and base overlap, overlapCount
          char *baseOverlap = base;
          char *blockOverlap = buffer;
          int overlapCount;
          int fullBoxCount = localFullUBound[de][0] - localFullLBound[de][0] 
            + 1;
          if (blockLocalIndex[0] < 0){
            blockOverlap += (-blockLocalIndex[0]) * elementSize;
            overlapCount = laLength[0] + blockLocalIndex[0];
            if (fullBoxCount < overlapCount)
              overlapCount = fullBoxCount;
          }else{
            baseOverlap += blockLocalIndex[0] * elementSize;
            overlapCount = fullBoxCount - blockLocalIndex[0];
            if (laLength[0] < overlapCount)
              overlapCount = laLength[0];
          }
          if (overlapCount < 0) 
            overlapCount = 0;
#if (VERBOSITY > 1)
          printf("gjt in ESMC_newArrayScatter: overlapCount = %d, blockOverlap"
            " = %p, baseOverlap = %p\n", overlapCount, blockOverlap,
            baseOverlap);
#endif
          // finally memcpy from buffer into DE's local array.
          memcpy(baseOverlap, blockOverlap, overlapCount * elementSize);
        }
        delayout->ESMC_DELayoutServiceComplete(de); // close window on DE
      }
    }
    // update the blockID
    ++blockID[1];
    for (int i=1; i<rank-1; i++)
      if (blockID[i] >= laLength[i]){
        blockID[i] = 0;
        ++blockID[i+1];
      }
    // shift the buffer start on rootPET to the next column
    if (localPET == rootPET)
      buffer += laLength[0] * elementSize;
  }
  
  // garbage collection
  delete [] blockGlobalIndex;
  delete [] blockLocalIndex;
  delete [] blockID;
  if (localPET != rootPET)
    delete [] buffer;
  delete [] laLength;
  delete [] localDeArrayBase;
    
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScatter()"
//BOP
// !IROUTINE:  ESMC_newArrayScatter
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int rootPET,              // root
  ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. DE-based non-blocking paradigm.
//
//EOP
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(ROOT): DE-based nb paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // if this is not the rootPET then exit because this is root side of scatter
  if (localPET != rootPET) return ESMF_SUCCESS;
  // check that the commhandle is clean, if not then exit with error
  int *cc = &(commh->commhandleCount); // to simplyfy usage during this method
  if (*cc != 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
      "- commhandle still holds outstanding communication handles.", &localrc);
    return localrc;
  }
  // check that there is a valid larray on rootPET 
  if (larray == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
      "- must supply a valid 'larray' argument on rootPET.", &localrc);
    return localrc;
  }
  // check that t/k/r matches
  int laRank = larray->ESMC_LocalArrayGetRank();
  if (laRank != rank){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- ranks don't match", &localrc);
    return localrc;
  }
  ESMC_TypeKind laTypeKind = larray->ESMC_LocalArrayGetTypeKind();
  if (laTypeKind != kind){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- kinds don't match", &localrc);
    return localrc;
  }
  // query some of larray's meta info
  int *laLength = new int[rank];
  larray->ESMC_LocalArrayGetLengths(rank, laLength);
  int laByteCount;
  larray->ESMC_LocalArrayGetByteCount(&laByteCount);
  int *laLbound = new int[rank];
  larray->ESMC_LocalArrayGetLbounds(rank, laLbound);
  // determine blockCount and allocate according number of commhandles
  int blockCount = 1;
  for (int i=1; i<rank; i++)
    blockCount *= laLength[i];
  int blockSize = laByteCount / blockCount;
  int elementSize = blockSize / laLength[0];
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(ROOT): blockCount = %d\n", blockCount);
  printf("gjt in ESMC_newArrayScatter(ROOT): elementSize = %d\n", elementSize);
#endif
  int totalHandles = deCount * (3 + blockCount);  // specific for this routine
  commh->vmk_commh = new vmk_commhandle*[totalHandles];
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(ROOT): commh=%p, vmk_commh=%p\n",
    commh, commh->vmk_commh);
  printf("gjt in ESMC_newArrayScatter(ROOT): totalHandles = %d\n",
    totalHandles);
#endif
  // send some of larray's meta info to all the DEs
  for (int de=0; de<deCount; de++){
#if (VERBOSITY > 9)
    printf("gjt in ESMC_newArrayScatter(ROOT): sending info to de = %d\n", de);
#endif
    commh->vmk_commh[*cc] = NULL; // mark as invalid element
    vm->vmk_send(laLength, rank * sizeof(int), deVASList[de],
      &(commh->vmk_commh[(*cc)++]), de+3000);
    commh->vmk_commh[*cc] = NULL; // mark as invalid element
    vm->vmk_send(&laByteCount, sizeof(int), deVASList[de],
      &(commh->vmk_commh[(*cc)++]), de+3000);
    commh->vmk_commh[*cc] = NULL; // mark as invalid element
    vm->vmk_send(laLbound, rank * sizeof(int), deVASList[de],
      &(commh->vmk_commh[(*cc)++]), de+3000);  
#if (VERBOSITY > 9)
    printf("gjt in ESMC_newArrayScatter(ROOT): done sending info to de = %d\n",
      de);
#endif
  }
  // prepare buffer pointer into data block
  char *buffer;
  larray->ESMC_LocalArrayGetBaseAddr((void **)&buffer); // start of data
#if (VERBOSITY > 9)
    fprintf(stderr, "gjt in ESMC_newArrayScatter(ROOT): buffer = %p\n",
      buffer);
#endif
  // setup a blockID which basically is an array holding the higher dimensions
  int *blockID = new int[rank];   // blockID[0] is unused!
#if (VERBOSITY > 9)
    fprintf(stderr, "gjt in ESMC_newArrayScatter(ROOT): blockID = %p\n",
      blockID);
#endif
  for (int i=0; i<rank; i++)
    blockID[i] = 0;
  int *blockLocalIndex = new int[rank];
#if (VERBOSITY > 9)
    fprintf(stderr, "gjt in ESMC_newArrayScatter(ROOT): blockLocalIndex = %p\n",
      blockLocalIndex);
#endif
  int *blockGlobalIndex = new int[rank];
#if (VERBOSITY > 9)
    fprintf(stderr, "gjt in ESMC_newArrayScatter(ROOT): blockGlobalIndex ="
      " %p\n", blockGlobalIndex);
#endif

//  blockGlobalIndex[0] = laLbound[0];
  
  
#if (VERBOSITY > 9)
  fprintf(stderr, "gjt in ESMC_newArrayScatter(ROOT): start blk loop\n");
#endif
  // now loop over all blocks
  for (int blk=0; blk<blockCount; blk++){
    for (int i=1; i<rank; i++)
      blockGlobalIndex[i] = laLbound[i] + blockID[i];
#if (VERBOSITY > 9)
    fprintf(stderr, "gjt in ESMC_newArrayScatter(ROOT): blockID: ");
    for (int i=1; i<rank-1; i++)
      fprintf(stderr, "%d, ", blockID[i]);
    fprintf(stderr, "%d\n", blockID[rank-1]);
#endif
    // send block of data to all of the DEs
    for (int de=0; de<deCount; de++){
      commh->vmk_commh[*cc] = NULL; // mark as invalid element
      vm->vmk_send(buffer, blockSize, deVASList[de],
        &(commh->vmk_commh[(*cc)++]),  de+3000);
    }
    // update the blockID
    ++blockID[1];
    for (int i=1; i<rank-1; i++)
      if (blockID[i] >= laLength[i]){
        blockID[i] = 0;
        ++blockID[i+1];
      }
    // shift the buffer start to the next column in larray
    buffer += laLength[0] * elementSize;
  }
  
  // garbage collection
  delete [] blockGlobalIndex;
  delete [] blockLocalIndex;
  delete [] blockID;
  delete [] laLbound;
  delete [] laLength;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScatter()"
//BOP
// !IROUTINE:  ESMC_newArrayScatter
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int rootPET,              // root
  int de,                   // DE for DE-based non-blocking scatter
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. DE-based non-blocking paradigm.
//
//EOP
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(DE): DE-based non-blocking paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // check that there is a valid larray on rootPET 
  if (localPET == rootPET){
    if (larray == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'larray' argument on rootPET.", &localrc);
      return localrc;
    }
  }    
  // check that t/k/r matches
  if (localPET == rootPET){
    int laRank = larray->ESMC_LocalArrayGetRank();
    if (laRank != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
        "- ranks don't match", &localrc);
      return localrc;
    }
    ESMC_TypeKind laTypeKind = larray->ESMC_LocalArrayGetTypeKind();
    if (laTypeKind != kind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
        "- kinds don't match", &localrc);
      return localrc;
    }
  }
  // determine localDE index
  int localDe;
  for (localDe=0; localDe<localDeCount; localDe++)
    if (localDeList[localDe] == de) break;
  // pack arguments to pass into ScatterThread
  thargArray[localDe].array = this;
  thargArray[localDe].vm = vm;
  thargArray[localDe].de = de;
  thargArray[localDe].rootPET = rootPET;
  // create the ScatterThread for this DE
  pthread_t *pthid =
    &(commhArray[localDe].pthid[(commhArray[localDe].pthidCount)++]);
// took the following call out so the rest would compile: 
// mpCC on AIX has trouble with friend func namespaces
//  pthread_create(pthid, NULL, ESMC_newArrayScatterThread,
//    &(thargArray[localDe]));
    
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScalarReduce()"
//BOP
// !IROUTINE:  ESMC_newArrayScalarReduce
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScalarReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void *result,             // result value (scalar)
  ESMC_TypeKind dtk,        // data type kind
  ESMC_Operation op,        // reduce operation
  int rootPET,              // root
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Reduce the data of an {\tt ESMC\_newArray} into a single scalar value.
//
//EOP
//-----------------------------------------------------------------------------
  int localrc;
  //Initialize localrc; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // check that there is a valid result argument on rootPET 
  if (localPET == rootPET){
    if (result == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'result' argument on rootPET.", &localrc);
      return localrc;
    }
  }    
  // check that t/k matches (on all PETs!)
  if (dtk != kind){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- kinds don't match", &localrc);
    return localrc;
  }
  // prepeare PET-local temporary result variable
  void *localResult;
  switch (dtk){
  case ESMC_TYPEKIND_I4:
    localResult = new ESMC_I4;
    break;
  case ESMC_TYPEKIND_R4:
    localResult = new ESMC_R4;
    break;
  case ESMC_TYPEKIND_R8:
    localResult = new ESMC_R8;
    break;
  default:
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- data type kind is unknown to VM", &localrc);
    return localrc;
  }
  // loop over local DEs
  int primeFlag = 1;
  for (int localDe=0; localDe<localDeCount; localDe++){
    int de = localDeList[localDe];
    if (delayout->ESMC_DELayoutServiceOffer(de, NULL)
      == ESMC_DELAYOUT_SERVICE_ACCEPT){
      // the localPET's offer was accepted by DELayout
      // get info out of the associated localArray
      ESMC_LocalArray *localDeArray = localArrays[localDe];
      void *localDeArrayBase;
      localDeArray->ESMC_LocalArrayGetBaseAddr(&localDeArrayBase);
      int *laLength = new int[rank];
      localDeArray->ESMC_LocalArrayGetLengths(rank, laLength);
      int laByteCount;
      localDeArray->ESMC_LocalArrayGetByteCount(&laByteCount);
      // determine how many blocks (columns) this localDeArray has
      int blockCount = 1;
      for (int i=1; i<rank; i++)
        blockCount *= laLength[i];
      int blockSize = laByteCount / blockCount;
      int elementSize = blockSize / laLength[0];
#if (VERBOSITY > 1)
      printf("gjt in ESMC_newArrayScalarReduce: de = %d, blockCount = %d\n",
        de, blockCount);
#endif
      // setup a blockID which is an array holding the higher dimensions
      int *blockID = new int[rank];   // blockID[0] is unused!
      for (int i=0; i<rank; i++)
        blockID[i] = 0;
      // now loop over all blocks
      for (int blk=0; blk<blockCount; blk++){
#if (VERBOSITY > 1)
        printf("gjt in ESMC_newArrayScalarReduce: blockID: ");
        for (int i=1; i<rank-1; i++)
          printf("%d, ", blockID[i]);
        printf("%d\n", blockID[rank-1]);
#endif
        // find start address of DE's data column for this block
        int elementCount = 0;
        int skipFlag = 0;
        for (int i=rank-1; i>0; i--){
          if (blockID[i] < dataOffset[de][i] ||
            blockID[i] > (globalDataUBound[de][i] - globalFullLBound[de][i])){
            // this block is inside halo region -> skip
            skipFlag = 1;
            break;
          }
          elementCount += blockID[i];
          elementCount *= laLength[i-1];
        }
        // update the blockID (must be done before skipping!)
        ++blockID[1];
        for (int i=1; i<rank-1; i++)
          if (blockID[i] >= laLength[i]){
            blockID[i] = 0;
            ++blockID[i+1];
          }
        if (skipFlag) continue; 
        char *base = (char *)localDeArrayBase;
        base += (elementCount + dataOffset[de][0]) * elementSize;
        int itemCount = globalDataUBound[de][0] - globalDataLBound[de][0] + 1;
#if (VERBOSITY > 1)
        printf("gjt in ESMC_newArrayScalarReduce: base = %p, itemCount = %d\n",
          base, itemCount);
#endif
        // reduce the block into localResult
        switch (dtk){
        case ESMC_TYPEKIND_I4:
          {
            ESMC_I4 *tempResult = (ESMC_I4 *)localResult;
            ESMC_I4 *tempBase = (ESMC_I4 *)base;
            switch (op){
            case ESMF_SUM:
              if (primeFlag)
                *tempResult = 0;  // prime the result variable
              for (int i=0; i<itemCount; i++){
                *tempResult += tempBase[i];
              }
              break;
            case ESMF_MIN:
              if (primeFlag)
                *tempResult = tempBase[0];          // prime the result variable
              for (int i=0; i<itemCount; i++){
                if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
              }
              break;
            case ESMF_MAX:
              if (primeFlag)
                *tempResult = tempBase[0];          // prime the result variable
              for (int i=0; i<itemCount; i++){
                if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
              }
              break;
            }
          }
          break;
        case ESMC_TYPEKIND_R4:
          {
            ESMC_R4 *tempResult = (ESMC_R4 *)localResult;
            ESMC_R4 *tempBase = (ESMC_R4 *)base;
            switch (op){
            case ESMF_SUM:
              if (primeFlag)
                *tempResult = 0.;  // prime the result variable
              for (int i=0; i<itemCount; i++){
                *tempResult += tempBase[i];
              }
              break;
            case ESMF_MIN:
              if (primeFlag)
                *tempResult = tempBase[0];          // prime the result variable
              for (int i=0; i<itemCount; i++){
                if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
              }
              break;
            case ESMF_MAX:
              if (primeFlag)
                *tempResult = tempBase[0];          // prime the result variable
              for (int i=0; i<itemCount; i++){
                if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
              }
              break;
            }
          }
          break;
        case ESMC_TYPEKIND_R8:
          {
            ESMC_R8 *tempResult = (ESMC_R8 *)localResult;
            ESMC_R8 *tempBase = (ESMC_R8 *)base;
            switch (op){
            case ESMF_SUM:
              if (primeFlag)
                *tempResult = 0.;  // prime the result variable
              for (int i=0; i<itemCount; i++){
                *tempResult += tempBase[i];
              }
              break;
            case ESMF_MIN:
              if (primeFlag)
                *tempResult = tempBase[0];          // prime the result variable
              for (int i=0; i<itemCount; i++){
                if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
              }
              break;
            case ESMF_MAX:
              if (primeFlag)
                *tempResult = tempBase[0];          // prime the result variable
              for (int i=0; i<itemCount; i++){
                if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
              }
              break;
            }
          }
          break;
        }
        primeFlag = 0;  // priming of result has been accomplished
      }      
      // DE-local garbage collection
      delete [] laLength;
      delete [] blockID;
      delayout->ESMC_DELayoutServiceComplete(de); // close window on DE
    }
  }
  // prepare for VM operation
  vmType vmt;
  switch (dtk){
  case ESMC_TYPEKIND_I4:
    vmt = vmI4;
    break;
  case ESMC_TYPEKIND_R4:
    vmt = vmR4;
    break;
  case ESMC_TYPEKIND_R8:
    vmt = vmR8;
    break;
  default:
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- data type kind is unknown to VM", &localrc);
    return localrc;
  }
  // reduce localResult across entire VM and put output into result on rootPET
  vm->vmk_reduce(localResult, result, 1, vmt, (vmOp)op, rootPET);
  
  // garbage collection
  // need to typcast back before: delete localResult;
  
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScalarReduce()"
//BOP
// !IROUTINE:  ESMC_newArrayScalarReduce
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScalarReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void *result,             // result value (scalar)
  ESMC_TypeKind dtk,        // data type kind
  ESMC_Operation op,        // reduce operation
  int rootPET,              // root
  ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Reduce the data of an {\tt ESMC\_newArray} into a single scalar value.
//
//EOP
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce(ROOT): DE-based nb paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // check that there is a valid result argument on rootPET 
  if (localPET == rootPET){
    if (result == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'result' argument on rootPET.", &localrc);
      return localrc;
    }
  }    
  // check that t/k matches (on all PETs!)
  if (dtk != kind){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- kinds don't match", &localrc);
    return localrc;
  }
  // pack arguments to pass into ScatterThread
  thargRoot.array = this;
  thargRoot.vm = vm;
  thargRoot.rootPET = rootPET;
  thargRoot.result = result;
  thargRoot.dtk = dtk;
  thargRoot.op = op;
  
  // create the ScalarReduceThread for rootPET
  pthread_t *pthid = &(commh->pthid[(commh->pthidCount)++]);
// took the following call out so the rest would compile: 
// mpCC on AIX has trouble with friend func namespaces
//  pthread_create(pthid, NULL, ESMC_newArrayScalarReduceThread, &thargRoot);
    
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScalarReduce()"
//BOP
// !IROUTINE:  ESMC_newArrayScalarReduce
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScalarReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  void *result,             // result value (scalar)
  ESMC_TypeKind dtk,        // data type kind
  ESMC_Operation op,        // reduce operation
  int rootPET,              // root
  int de,                   // DE for DE-based non-blocking reduce
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Reduce the data of an {\tt ESMC\_newArray} into a single scalar value.
//
//EOP
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce(DE): DE-based nb paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // determine localDE index
  int localDe;
  for (localDe=0; localDe<localDeCount; localDe++)
    if (localDeList[localDe] == de) break;
  // determine VAS for rootPET
  int rootVAS;
  vm->ESMC_VMGetPETLocalInfo(rootPET, NULL, NULL, NULL, NULL, &rootVAS);
  // check that t/k matches (on all PETs!)
  if (dtk != kind){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- kinds don't match", &localrc);
    return localrc;
  }
  // prepeare PET-local temporary result variable
  void *localResult;
  int size;
  switch (dtk){
  case ESMC_TYPEKIND_I4:
    localResult = new ESMC_I4;
    size = 4;
    break;
  case ESMC_TYPEKIND_R4:
    localResult = new ESMC_R4;
    size = 4;
    break;
  case ESMC_TYPEKIND_R8:
    localResult = new ESMC_R8;
    size = 8;
    break;
  default:
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- data type kind is unknown", &localrc);
    return localrc;
  }
  // get info out of the associated localArray
  ESMC_LocalArray *localDeArray = localArrays[localDe];
  void *localDeArrayBase;
  localDeArray->ESMC_LocalArrayGetBaseAddr(&localDeArrayBase);
  int *laLength = new int[rank];
  localDeArray->ESMC_LocalArrayGetLengths(rank, laLength);
  int laByteCount;
  localDeArray->ESMC_LocalArrayGetByteCount(&laByteCount);
  // determine how many blocks (columns) this localDeArray has
  int blockCount = 1;
  for (int i=1; i<rank; i++)
    blockCount *= laLength[i];
  int blockSize = laByteCount / blockCount;
  int elementSize = blockSize / laLength[0];
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce: de = %d, blockCount = %d\n",
    de, blockCount);
#endif
  // setup a blockID which is an array holding the higher dimensions
  int *blockID = new int[rank];   // blockID[0] is unused!
  for (int i=0; i<rank; i++)
    blockID[i] = 0;
  // now loop over all blocks
  int primeFlag = 1;
  for (int blk=0; blk<blockCount; blk++){
#if (VERBOSITY > 1)
    printf("gjt in ESMC_newArrayScalarReduce: blockID: ");
    for (int i=1; i<rank-1; i++)
      printf("%d, ", blockID[i]);
    printf("%d\n", blockID[rank-1]);
#endif
    // find start address of DE's data column for this block
    int elementCount = 0;
    int skipFlag = 0;
    for (int i=rank-1; i>0; i--){
      if (blockID[i] < dataOffset[de][i] ||
        blockID[i] > (globalDataUBound[de][i] - globalFullLBound[de][i])){
        // this block is inside halo region -> skip
        skipFlag = 1;
        break;
      }
      elementCount += blockID[i];
      elementCount *= laLength[i-1];
    }
    // update the blockID (must be done before skipping!)
    ++blockID[1];
    for (int i=1; i<rank-1; i++)
      if (blockID[i] >= laLength[i]){
        blockID[i] = 0;
        ++blockID[i+1];
      }
    if (skipFlag) continue; 
    char *base = (char *)localDeArrayBase;
    base += (elementCount + dataOffset[de][0]) * elementSize;
    int itemCount = globalDataUBound[de][0] - globalDataLBound[de][0] + 1;
#if (VERBOSITY > 1)
    printf("gjt in ESMC_newArrayScalarReduce: base = %p, itemCount = %d\n",
      base, itemCount);
#endif
    // reduce the block into localResult
    switch (dtk){
    case ESMC_TYPEKIND_I4:
      {
        ESMC_I4 *tempResult = (ESMC_I4 *)localResult;
        ESMC_I4 *tempBase = (ESMC_I4 *)base;
        switch (op){
        case ESMF_SUM:
          if (primeFlag)
            *tempResult = 0;                    // prime the result variable
          for (int i=0; i<itemCount; i++){
            *tempResult += tempBase[i];
          }
          break;
        case ESMF_MIN:
          if (primeFlag)
            *tempResult = tempBase[0];          // prime the result variable
          for (int i=0; i<itemCount; i++){
            if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
          }
          break;
        case ESMF_MAX:
          if (primeFlag)
            *tempResult = tempBase[0];          // prime the result variable
          for (int i=0; i<itemCount; i++){
            if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
          }
          break;
        }
      }
      break;
    case ESMC_TYPEKIND_R4:
      {
        ESMC_R4 *tempResult = (ESMC_R4 *)localResult;
        ESMC_R4 *tempBase = (ESMC_R4 *)base;
        switch (op){
        case ESMF_SUM:
          if (primeFlag)
            *tempResult = 0.;                   // prime the result variable
          for (int i=0; i<itemCount; i++){
            *tempResult += tempBase[i];
          }
          break;
        case ESMF_MIN:
          if (primeFlag)
            *tempResult = tempBase[0];          // prime the result variable
          for (int i=0; i<itemCount; i++){
            if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
          }
          break;
        case ESMF_MAX:
          if (primeFlag)
            *tempResult = tempBase[0];          // prime the result variable
          for (int i=0; i<itemCount; i++){
            if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
          }
          break;
        }
      }
      break;
    case ESMC_TYPEKIND_R8:
      {
        ESMC_R8 *tempResult = (ESMC_R8 *)localResult;
        ESMC_R8 *tempBase = (ESMC_R8 *)base;
        switch (op){
        case ESMF_SUM:
          if (primeFlag)
            *tempResult = 0.;                   // prime the result variable
          for (int i=0; i<itemCount; i++){
            *tempResult += tempBase[i];
          }
          break;
        case ESMF_MIN:
          if (primeFlag)
            *tempResult = tempBase[0];          // prime the result variable
          for (int i=0; i<itemCount; i++){
            if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
          }
          break;
        case ESMF_MAX:
          if (primeFlag)
            *tempResult = tempBase[0];          // prime the result variable
          for (int i=0; i<itemCount; i++){
            if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
          }
          break;
        }
      }
      break;
    }
    primeFlag = 0;  // priming of result has been accomplished
  }      
  // DE-local garbage collection
  delete [] laLength;
  delete [] blockID;
  // send DE-local reduction result to rootPET
  commhArray[localDe].commhandleCount = 1;
  commhArray[localDe].vmk_commh = new vmk_commhandle*[1];
  commhArray[localDe].vmk_commh[0] = NULL;  // mark as invalid
  vm->vmk_send(localResult, size, rootVAS,
    &(commhArray[localDe].vmk_commh[0]), de+5000);
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce: sent localResult=%g for de = %d\n",
    *(ESMC_R8 *)localResult, de);
#endif

  // garbage collection must be done _after_ associated wait
  commhArray[localDe].buffer = localResult;

  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------




// ---- Wait methods ---  


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayWait()"
//BOP
// !IROUTINE:  ESMC_newArrayWait
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayWait(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int rootPET,              // root
  ESMC_newArrayCommHandle *commh, // commu handle specif. non-blocking op.
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Wait for a non-blocking newArray communication to be done with data 
//    object on rootPET.
//
//EOP
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait: DE-based nb paradigm ROOT\n");
  printf("gjt in ESMC_newArrayWait(ROOT): commh=%p\n", commh);
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // if this is not the rootPET then exit because this is root side of scatter
  if (localPET != rootPET) return ESMF_SUCCESS;
  int *cc = &(commh->commhandleCount);  // to simplify usage during this method
  int *pc = &(commh->pthidCount);       // to simplify usage during this method
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait(ROOT): *cc=%d, *pc=%d\n", *cc, *pc);
#endif
  // join all of the root threads
  for (int i=0; i<*pc; i++){
    pthread_join(commh->pthid[i], NULL);
  }
  *pc = 0;  // reset
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait(ROOT): threads are joined\n");
#endif
  // wait for all the communication handles to clear
  for (int i=0; i<*cc; i++){
    vm->vmk_commwait(&(commh->vmk_commh[i]), NULL, 1);  // use nanopause=1ns to lower load
  }
  if (*cc) delete [] commh->vmk_commh;
  *cc = 0;  // reset
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait(ROOT): all commhandles are complete and deleted\n");
#endif
  
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayWait()"
//BOP
// !IROUTINE:  ESMC_newArrayWait
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayWait(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int de,                   // DE for which to wait
  ESMC_VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Wait for a non-blocking newArray communication to finish for de.
//
//EOP
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait: DE-based non-blocking paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // determine localDE index
  int localDe;
  for (localDe=0; localDe<localDeCount; localDe++)
    if (localDeList[localDe] == de) break;
  int *cc = &(commhArray[localDe].commhandleCount);  // to simplify usage
  int *pc = &(commhArray[localDe].pthidCount);       // to simplify usage
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait(DE): *cc=%d, *pc=%d\n", *cc, *pc);
#endif
  // join this DE's pthreads
  for (int i=0; i<*pc; i++){
#if (VERBOSITY > 9)
    printf("gjt in ESMC_newArrayWait: pthread=%d\n",
      commhArray[localDe].pthid[i]);
#endif
    pthread_join(commhArray[localDe].pthid[i], NULL);
  }
  *pc = 0;  // reset
  // wait for all the communication handles to clear
  for (int i=0; i<*cc; i++){
    // use nanopause=1ns to lower load
    vm->vmk_commwait(&(commhArray[localDe].vmk_commh[i]), NULL, 1);
  }
  if (*cc) delete [] commhArray[localDe].vmk_commh;
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait: all commhandles deleted\n");
#endif
  *cc = 0;  // reset
  // destroy the buffer
  if (commhArray[localDe].buffer){
    delete commhArray[localDe].buffer;
    commhArray[localDe].buffer = NULL;  // mark invalid for next time... 
  }
  
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayGet()"
//BOP
// !IROUTINE:  ESMC_newArrayGet
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  int *rank_out,                      // out - rank of newArray
  ESMC_DELayout **delayout,           // out - associated DELayout
  ESMC_LocalArray **localArrays,      // out - local arrays
  int len_localArrays,                // in  - length of localArrays array
  int *globalFullLBound,              // out - 2d array of bounds
  int *len_globalFullLBound,          // in  - lengths of the above array
  int *globalFullUBound,              // out - 2d array of bounds
  int *len_globalFullUBound,          // in  - lengths of the above array
  int *globalDataLBound,              // out - 2d array of bounds
  int *len_globalDataLBound,          // in  - lengths of the above array
  int *globalDataUBound,              // out - 2d array of bounds
  int *len_globalDataUBound,          // in  - lengths of the above array
  int *localDataLBound,               // out - 2d array of bounds
  int *len_localDataLBound,           // in  - lengths of the above array
  int *localDataUBound,               // out - 2d array of bounds
  int *len_localDataUBound            // in  - lengths of the above array
  ){
//
// !DESCRIPTION:
//    Get access to internals
//
//EOP
//-----------------------------------------------------------------------------
  if (rank_out != ESMC_NULL_POINTER)
    *rank_out = this->rank;
  if (delayout != ESMC_NULL_POINTER)
    *delayout = this->delayout;
  for (int i=0; (i<localDeCount && i<len_localArrays); i++)
    localArrays[i] = this->localArrays[i];
  for (int i=0; (i<deCount && i<len_globalFullLBound[1]); i++)
    for (int j=0; (j<rank && j<len_globalFullLBound[0]); j++)
      *(globalFullLBound + i*rank + j) = this->globalFullLBound[i][j];
  for (int i=0; (i<deCount && i<len_globalFullUBound[1]); i++)
    for (int j=0; (j<rank && j<len_globalFullUBound[0]); j++)
      *(globalFullUBound + i*rank + j) = this->globalFullUBound[i][j];
  for (int i=0; (i<deCount && i<len_globalDataLBound[1]); i++)
    for (int j=0; (j<rank && j<len_globalDataLBound[0]); j++)
      *(globalDataLBound + i*rank + j) = this->globalDataLBound[i][j];
  for (int i=0; (i<deCount && i<len_globalDataUBound[1]); i++)
    for (int j=0; (j<rank && j<len_globalDataUBound[0]); j++)
      *(globalDataUBound + i*rank + j) = this->globalDataUBound[i][j];
  for (int i=0; (i<deCount && i<len_localDataLBound[1]); i++)
    for (int j=0; (j<rank && j<len_localDataLBound[0]); j++)
      *(localDataLBound + i*rank + j) =
        (this->localFullLBound[i][j] + dataOffset[i][j]) + 1;
  for (int i=0; (i<deCount && i<len_localDataUBound[1]); i++)
    for (int j=0; (j<rank && j<len_localDataUBound[0]); j++)
      *(localDataUBound + i*rank + j) =
        (this->localFullLBound[i][j] + dataOffset[i][j]) + 1
        + (this->globalDataUBound[i][j] - this->globalDataLBound[i][j]);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayPrint()"
//BOP
// !IROUTINE:  ESMC_newArrayPrint
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayPrint(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Print ESMC\_newArray
//
//EOP
//-----------------------------------------------------------------------------
  printf("====== <ESMC_newArrayPrint> ================================\n");
  printf("deCount = %d, rank = %d\n", deCount, rank);
  for (int de=0; de<deCount; de++){
    for (int i=0; i<rank; i++)
      printf("de = %d, dim = %d, globalDataLBound = %d, "
        "globalDataUBound = %d, dataOffset = %d, "
        "localFullLBound = %d, localFullUBound = %d\n",
        de, i, globalDataLBound[de][i], globalDataUBound[de][i],
        dataOffset[de][i], localFullLBound[de][i], localFullUBound[de][i]);
  }
  for (int de=0; de<localDeCount; de++){
    localArrays[de]->ESMC_LocalArrayPrint();
  }
  printf("====== </ESMC_newArrayPrint> ===============================\n");
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayCreate()"
//BOP
// !IROUTINE:  ESMC_newArrayCreate
//
// !INTERFACE:
ESMC_newArray *ESMC_newArrayCreate(
//
// !RETURN VALUE:
//    ESMC_newArray * to newly allocated ESMC_newArray
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int *haloWidth,           // halo width
  int deCount,              // number of DEs
  int rootPET,              // root
  int *rc){                 // return code
//
// !DESCRIPTION:
//    This Create method generates a DELayout following a very simple 
//    algorithm that assumes:
//      1. that the DE vertix weight is proportional to the number of logical
//         grid points of the associated array domain - thus it is best to
//         chunk up the entire domain into equal pieces
//EOP
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize rc and localrc ; assume functions not implemented
  localrc = ESMF_RC_NOT_IMPL;
  if (*rc) *rc = ESMF_RC_NOT_IMPL;

  ESMC_newArray *array;
  // determine required information
  ESMC_VM *vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET, petCount;
  vm->ESMC_VMGet(&localPET, &petCount, NULL, NULL, NULL);
  // check that there is a valid larray on rootPET 
  if (localPET == rootPET){
    if (larray == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'larray' argument on rootPET.", rc);
      return(ESMC_NULL_POINTER);
    }
  }    
  // allocate and initialize a newArray object via native constructor
  try {
    array = new ESMC_newArray;
  }
  catch (...) {
    ESMC_LogDefault.ESMC_LogAllocError(rc);
    return(ESMC_NULL_POINTER);
  }
  // get info about the LocalArray on rootPET and broadcast it to all PETs
  int laRank, *laLength;
  if (localPET == rootPET){
    laRank = larray->ESMC_LocalArrayGetRank();
    vm->vmk_broadcast(&laRank, sizeof(int), rootPET);
    laLength = new int[laRank];
    larray->ESMC_LocalArrayGetLengths(laRank, laLength);
    vm->vmk_broadcast(laLength, laRank * sizeof(int), rootPET);
  }else{
    vm->vmk_broadcast(&laRank, sizeof(int), rootPET);
    laLength = new int[laRank];
    vm->vmk_broadcast(laLength, laRank * sizeof(int), rootPET);
  }
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayCreate: %d, %d\n", laRank, laLength[0]);
#endif
  // ensure a valid haloWidth array on rootPET
  int haloWidthAllocFlag = 0;
  if (localPET == rootPET){
    if (haloWidth == NULL){
      // need to fill in a valid haloWidth, default is a width of 0 in all dims
      haloWidthAllocFlag = 1;
      haloWidth = new int[laRank];
      for (int i=0; i<laRank; i++)
        haloWidth[i] = 0;
    }
  }    
  // broadcast rootPET's haloWidth array
  vm->vmk_broadcast(&haloWidth, sizeof(int *), rootPET);
  // don't worry, it is o.k. to overwrite local variables!
  if (haloWidth != NULL){
    if (localPET != rootPET)
      haloWidth = new int[laRank];
    vm->vmk_broadcast(haloWidth, laRank * sizeof(int), rootPET);
  }
  
  // determine the decomposition rank
  int decompRank = 0;
  if (haloWidth != NULL){
    // haloWidth array exists and needs to be evaluated
    for (int i=0; i<laRank; i++)
      if (haloWidth[i]>=0) ++decompRank;
  }else{
    // haloWidth array does not exist, by default all are decomp. dimensions
    decompRank = laRank;  // 
  }
  
  // find the haloVolume for all decomposition dimensions
  int *haloVolume = new int[decompRank];
  int *decompMap = new int[decompRank];
  if (haloWidth != NULL){
#if (VERBOSITY > 9)
    printf("gjt in ESMC_newArrayCreate: haloWidth available\n");
#endif
    // determine halo volumes of all ranks
    int k=0;
    for (int i=0; i<laRank; i++){
      if (haloWidth[i]>=0){
        decompMap[k] = i;
        haloVolume[k] = haloWidth[i];
        for (int j=0; j<i; j++)
          haloVolume[k] *= laLength[j];
        for (int j=i+1; j<laRank; j++)
          haloVolume[k] *= laLength[j];
        ++k;
      }
    }
  }else{
    // use a fake haloVolume that makes the decomposition algorithm work
    for (int i=0; i<laRank; i++){
      decompMap[i] = i;
      haloVolume[i] = laLength[i];
    }
  }
#if (VERBOSITY > 9)
  for (int i=0; i<decompRank; i++)
    printf("gjt in ESMC_newArrayCreate: haloVolume[%d]=%d\n", i, haloVolume[i]);
#endif
  // need to broadcast rootPET's deCount
  vm->vmk_broadcast(&deCount, sizeof(int), rootPET);
  // don't worry, it is o.k. to overwrite local variables!
  if (deCount == 0) deCount = petCount; // by default chose deCount = petCount

  // determine best DELayout to decompose larray data
  // todo: this is a trivial implementation that only works if the number of
  // DEs is smaller or equal the length of the dimension with the smallest
  // haloVolume. A more complete implementation would go through a factorization
  // of the deCount with as many factors as there are decompRank and pick the
  // decomposition with overall smallest sum of haloVolumes. 
  int *deLength = new int[decompRank];    // number of DEs along this dimension
  for (int i=0; i<decompRank; i++)
    deLength[i] = 1;        // reset to length one
  int minIndex=0;
  for (int i=0; i<decompRank; i++)
    if (haloVolume[i] < haloVolume[minIndex]) minIndex = i;
  if (laLength[decompMap[minIndex]]/deCount >= haloWidth[minIndex]){
    deLength[minIndex] = deCount; // set the deLength of the smallest haloVolume
  }else{
    printf("found error in ESMC_newArrayCreate\n");
  }
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayCreate: decompRank=%d\n", decompRank);
  for (int i=0; i<decompRank; i++)
    printf("gjt in ESMC_newArrayCreate: %d, deLength=%d, decompMap=%d\n", i,    
      deLength[i], decompMap[i]);
#endif
  // now there is enough information to create a DELayout
  ESMC_DELayout *delayout = ESMC_DELayoutCreate(*vm, deLength, decompRank,
    NULL, 0, NULL, &localrc);
#if (VERBOSITY > 9)
  delayout->ESMC_DELayoutPrint();
#endif
  
  // finally construct the newArray according to the DELayout
  localrc = array->ESMC_newArrayConstruct(larray, haloWidth, delayout, rootPET);

  // garbage collection
  delete [] laLength;
  if (haloWidth != NULL){
    if (localPET != rootPET || haloWidthAllocFlag)
      delete [] haloWidth;
  }
  delete [] haloVolume;
  delete [] decompMap;
  delete [] deLength;
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayCreate, array: %p, %d, %d\n", array, localPET, 
    petCount);
#endif
  
  // error handling via LogErr
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;
  // return handle to the newArray object
  return(array);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayDestroy()"
//BOP
// !IROUTINE:  ESMC_newArrayDestroy
//
// !INTERFACE:
int ESMC_newArrayDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  ESMC_newArray **array){      // in - ESMC_newArray to destroy
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------
int localrc;
//Initialize localrc; assume routine not implemented
localrc = ESMC_RC_NOT_IMPL;

  if (*array != ESMC_NULL_POINTER) {
#if 0
    (*array)->ESMC_newArrayDestruct();
#endif
    delete (*array);
    *array = ESMC_NULL_POINTER;
    return(ESMF_SUCCESS);
  }else{
    ESMC_LogDefault.ESMC_LogWrite("Cannot delete bad newArray object.", 
      ESMC_LOG_ERROR);
    return(ESMC_RC_PTR_NULL);
  }
return localrc;
}
//-----------------------------------------------------------------------------








//--------------------------
// functions that provide thread support for non-blocking comms
//--------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScatterThread()"
//BOPI
// !IROUTINE:  ESMC_newArrayScatterThread
//
// !INTERFACE:
void *ESMC_newArrayScatterThread(
//
// !RETURN VALUE:
//    return value required by pthread standard
//
// !ARGUMENTS:
//
      void *arg){               // pointer to information structure
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. DE-based non-blocking paradigm.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(THREAD): DE-based nb paradigm\n");
#endif
  int localrc;
  ESMC_newArrayThreadArg *tharg = (ESMC_newArrayThreadArg *)arg;
  // setup some useful local variables:
  ESMC_newArray *array = tharg->array;
  ESMC_VM *vm = tharg->vm;
  int de = tharg->de;
  int rootPET = tharg->rootPET;
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(THREAD): de=%d\n", de);
#endif
  // determine required information
  if (vm==NULL)
    vm = ESMC_VMGetCurrent(&localrc);  // get current VM context
  int localPET;
  vm->ESMC_VMGet(&localPET, NULL, NULL, NULL, NULL);
  // determine localDE index
  int localDeCount = array->localDeCount;
  int *localDeList = array->localDeList;
  int localDe;
  for (localDe=0; localDe<localDeCount; localDe++)
    if (localDeList[localDe] == de) break;
  // determine VAS for rootPET
  int rootVAS;
  vm->ESMC_VMGetPETLocalInfo(rootPET, NULL, NULL, NULL, NULL, &rootVAS);
  // receive some of larray's meta info from scatter root method
  int rank = array->rank;
  int *laLength = new int[rank];
  vmk_commhandle *ch_laLength = NULL; // mark as invalid
  vm->vmk_recv(laLength, rank * sizeof(int), rootVAS, &ch_laLength, de+3000);
  int laByteCount;
  vmk_commhandle *ch_laByteCount = NULL; // mark as invalid
  vm->vmk_recv(&laByteCount, sizeof(int), rootVAS, &ch_laByteCount, de+3000);
  int *laLbound = new int[rank];
  vmk_commhandle *ch_laLbound = NULL; // mark as invalid
  vm->vmk_recv(laLbound, rank * sizeof(int), rootVAS, &ch_laLbound, de+3000);
  // get info out of the associated localArrays
  ESMC_LocalArray **localArrays = array->localArrays;
  void **localDeArrayBase = new void*[localDeCount];
  for (int i=0; i<localDeCount; i++)
    localArrays[i]->ESMC_LocalArrayGetBaseAddr(&localDeArrayBase[i]);
  // prepare a temporary buffer
  vm->vmk_commwait(&ch_laLength, NULL, 1);     // need this variable in a couple of lines
  vm->vmk_commwait(&ch_laByteCount, NULL, 1);  // need this variable in a couple of lines
  int blockCount = 1;
  for (int i=1; i<rank; i++)
    blockCount *= laLength[i];
  int blockSize = laByteCount / blockCount;
  int elementSize = blockSize / laLength[0];
#if (VERBOSITY > 9)
  for (int i=0; i<rank; i++)
    printf("gjt in ESMC_newArrayScatter(THREAD): de=%d, laLenght[%d] = %d\n",
      de, i, laLength[i]);
  printf("gjt in ESMC_newArrayScatter(THREAD): elementSize = %d\n",
    elementSize);
#endif
  char *buffer;
  buffer = new char[blockSize];   // buffer for first dim.
  // setup a blockID which basically is an array holding the higher dimensions
  int *blockID = new int[rank];   // blockID[0] is unused!
  for (int i=0; i<rank; i++)
    blockID[i] = 0;
  int *blockLocalIndex = new int[rank];
  int *blockGlobalIndex = new int[rank];
  vm->vmk_commwait(&ch_laLbound, NULL, 1);
  blockGlobalIndex[0] = laLbound[0];
  // prepare for loop over all blocks
  vmk_commhandle *ch_buffer = NULL; // mark as invalid
  int **globalDataLBound = array->globalDataLBound;
  int **globalDataUBound = array->globalDataUBound;
  int **localFullLBound = array->localFullLBound; 
  int **localFullUBound = array->localFullUBound; 
  int **globalFullLBound = array->globalFullLBound;
  int **globalFullUBound = array->globalFullUBound;
  int **dataOffset = array->dataOffset;      
  // now loop over all blocks
  for (int blk=0; blk<blockCount; blk++){
    for (int i=1; i<rank; i++)
      blockGlobalIndex[i] = laLbound[i] + blockID[i];
#if (VERBOSITY > 9)
    printf("gjt in ESMC_newArrayScatter(THREAD): blockID: ");
    for (int i=1; i<rank-1; i++)
      printf("%d, ", blockID[i]);
    printf("%d\n", blockID[rank-1]);
#endif
    // use ide instead of localDe in the following lines...
    int ide = localDe;
    // receive block data from root
    vm->vmk_recv(buffer, blockSize, rootVAS, &ch_buffer, de+3000);
    // check whether this DE's fullBox intersects the current block...
    int ii;
    for (ii=1; ii<rank; ii++){
#if (VERBOSITY > 9)
      printf("gjt in ESMC_newArrayScatter(THREAD): ide=%d,"
        " blockGlobalIndex=%d\n globalXXX: %d, %d, %d, %d\n", ide, 
        blockGlobalIndex[ii], globalFullLBound[de][ii],
        globalDataLBound[de][ii], globalDataUBound[de][ii],
        globalFullUBound[de][ii]);
#endif    
      // check intersection with dataBox
      if (blockGlobalIndex[ii] >= globalDataLBound[de][ii] &&
        blockGlobalIndex[ii] <= globalDataUBound[de][ii]){
#if (VERBOSITY > 9)
        printf("gjt in ESMC_newArrayScatter(THREAD): intersect dataBox\n");
#endif    
        // found intersection with dataBox
        blockLocalIndex[ii] = blockGlobalIndex[ii] 
          - globalDataLBound[de][ii]
          + dataOffset[de][ii];
        continue;
      }
      //check intersection with lower halo region
      if (globalFullLBound[de][ii] <= globalDataLBound[de][ii]){
        // this is a regular lower halo region
        if (blockGlobalIndex[ii] >= globalFullLBound[de][ii] &&
          blockGlobalIndex[ii] < globalDataLBound[de][ii]){
#if (VERBOSITY > 9)
          printf("gjt in ESMC_newArrayScatter(THREAD): intersect regular lower"
                " halo\n");
#endif    
          // found intersection with regular lower halo region
          blockLocalIndex[ii] = blockGlobalIndex[ii] 
            - globalFullLBound[de][ii];
          continue;
        }
      }else{
        // this is a periodic lower halo region from the other end
        if (blockGlobalIndex[ii] >= globalFullLBound[de][ii] &&
          blockGlobalIndex[ii] <=
            (globalFullLBound[de][ii] + dataOffset[de][ii])){
#if (VERBOSITY > 9)
          printf("gjt in ESMC_newArrayScatter(THREAD): intersect periodic lower"
            " halo\n");
#endif    
          // found intersection with periodic lower halo region
          blockLocalIndex[ii] = blockGlobalIndex[ii] 
            - globalFullLBound[de][ii];
          continue;
        }
      }
      //check intersection with upper halo region
      if (globalFullUBound[de][ii] >= globalDataUBound[de][ii]){
        // this is a regular upper halo region
        if (blockGlobalIndex[ii] > globalDataUBound[de][ii] &&
          blockGlobalIndex[ii] <= globalFullUBound[de][ii]){
#if (VERBOSITY > 9)
          printf("gjt in ESMC_newArrayScatter(THREAD): intersect regular upper"
            " halo\n");
#endif    
          // found intersection with regular upper halo region
          blockLocalIndex[ii] = blockGlobalIndex[ii] 
            - globalDataLBound[de][ii] + dataOffset[de][ii];
          continue;
        }
      }else{
        // this is a periodic upper halo region from the other end
        int upperDataOffset = 
          localFullUBound[de][ii] - localFullLBound[de][ii]
          - dataOffset[de][ii]
          - (globalDataUBound[de][ii] - globalDataLBound[de][ii]);
        if (blockGlobalIndex[ii] >= 
          (globalFullUBound[de][ii] - upperDataOffset) &&
          blockGlobalIndex[ii] <= globalFullUBound[de][ii]){
#if (VERBOSITY > 9)
          printf("gjt in ESMC_newArrayScatter(THREAD): intersect periodic upper"
            " halo\n");
#endif    
          // found intersection with periodic upper halo region
          blockLocalIndex[ii] = blockGlobalIndex[ii]
            - (globalFullUBound[de][ii] - upperDataOffset);
          continue;
        }
      }
      break;
    }        
#if (VERBOSITY > 9)
    if (ii==rank)
      printf("gjt in ESMC_newArrayScatter(THREAD): block inside fullBox\n");
    else
      printf("gjt in ESMC_newArrayScatter(THREAD): block outside fullBox\n");
#endif
    blockLocalIndex[0] = blockGlobalIndex[0] - globalFullLBound[de][0];
    if (ii==rank){
      // block intersects fullBox ->
      // find start address of DE's data column overlaping with block data
      char *base = (char *)localDeArrayBase[ide];
      int elementCount = 0;
      for (int i=rank-1; i>0; i--){
        elementCount += blockLocalIndex[i];
        elementCount *= (localFullUBound[de][i-1] 
          - localFullLBound[de][i-1] + 1);
      }
      base += elementCount * elementSize;
#if (VERBOSITY > 9)
      printf("gjt in ESMC_newArrayScatter(THREAD): elementCount = %d, base "
        "=%p\n", elementCount, base);
#endif
      // determine start pointers of buffer and base overlap, overlapCount
      char *baseOverlap = base;
      char *blockOverlap = buffer;
      int overlapCount;
      int fullBoxCount = localFullUBound[de][0] - localFullLBound[de][0] 
        + 1;
      if (blockLocalIndex[0] < 0){
        blockOverlap += (-blockLocalIndex[0]) * elementSize;
        overlapCount = laLength[0] + blockLocalIndex[0];
        if (fullBoxCount < overlapCount)
          overlapCount = fullBoxCount;
      }else{
        baseOverlap += blockLocalIndex[0] * elementSize;
        overlapCount = fullBoxCount - blockLocalIndex[0];
        if (laLength[0] < overlapCount)
          overlapCount = laLength[0];
      }
      if (overlapCount < 0) 
        overlapCount = 0;
#if (VERBOSITY > 9)
      printf("gjt in ESMC_newArrayScatter(THREAD): overlapCount = %d,"
        " blockOverlap = %p, baseOverlap = %p\n", overlapCount, blockOverlap,
        baseOverlap);
#endif
      // going to need the data in buffer for the following memcpy call...
      vm->vmk_commwait(&ch_buffer, NULL, 1);
      // finally memcpy from buffer into DE's local array.
      memcpy(baseOverlap, blockOverlap, overlapCount * elementSize);
    }else{
      // block does not intersect fullBox
//gjt      vm->vmk_cancel(&ch_buffer);
      // gjt took the above line out again because canceling an MPI message is
      // much more involved than simply calling cancel on one side! In case the
      // cancel succeeds on the receiver side there is still the message queued
      // on the sender side and will mess up the message queue! In order to
      // cancel and remove the message correctly there will need to be
      // communication between sender and receiver! For this to be done behind
      // the scenes in a non-blocking approach will require extra threads!
      vm->vmk_commwait(&ch_buffer, NULL, 1);
    }
    // update the blockID
    ++blockID[1];
    for (int i=1; i<rank-1; i++)
      if (blockID[i] >= laLength[i]){
        blockID[i] = 0;
        ++blockID[i+1];
      }
  }
  // garbage collection
  delete [] blockGlobalIndex;
  delete [] blockLocalIndex;
  delete [] blockID;
  delete [] buffer;
  delete [] laLength;
  delete [] localDeArrayBase;

#if (VERBOSITY > 9)
      printf("gjt in ESMC_newArrayScatter(THREAD): returning\n");
#endif
    
  return NULL;
}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScalarReduceThread()"
//BOPI
// !IROUTINE:  ESMC_newArrayScalarReduceThread
//
// !INTERFACE:
void *ESMC_newArrayScalarReduceThread(
//
// !RETURN VALUE:
//    return value required by pthread standard
//
// !ARGUMENTS:
//
      void *arg){               // pointer to information structure
//
// !DESCRIPTION:
//    Reduce the contents of an {\tt ESMC\_newArray}
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce(THREAD): DE-based nb paradigm\n");
#endif
  int localrc;
  ESMC_newArrayThreadArg *tharg = (ESMC_newArrayThreadArg *)arg;
  // setup some useful local variables:
  ESMC_newArray *array = tharg->array;
  ESMC_VM *vm = tharg->vm;
  int rootPET = tharg->rootPET;
  void *result = tharg->result;
  ESMC_TypeKind dtk = tharg->dtk;
  ESMC_Operation op = tharg->op;
  // prepeare PET-local temporary result variable
  int deCount = array->deCount;
  void *localResult;
  int size;
  switch (dtk){
  case ESMC_TYPEKIND_I4:
    localResult = new ESMC_I4[deCount];
    size = 4;
    break;
  case ESMC_TYPEKIND_R4:
    localResult = new ESMC_R4[deCount];
    size = 4;
    break;
  case ESMC_TYPEKIND_R8:
    localResult = new ESMC_R8[deCount];
    size = 8;
    break;
  default:
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP, 
      "- data type kind is unknown to VM", &localrc);
    return NULL;
  }
  // loop over all DEs and issue nb receive localResult
  char *localResultChar = (char *)localResult;
  vmk_commhandle **vmk_commh = new vmk_commhandle*[deCount];
  int *deVASList = array->deVASList;
  for (int de=0; de<deCount; de++){
    vmk_commh[de] = NULL;    // mark as invalid
    vm->vmk_recv(localResultChar, size, deVASList[de], &(vmk_commh[de]),
      de+5000);
    
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce(THREAD): issued receive for de=%d"
    " in deVAS: %d, and obtained handle %p\n", de, deVASList[de],
        vmk_commh[de]);
#endif
  
    localResultChar += size;
  }
  // reduce to final result
  switch (dtk){
  case ESMC_TYPEKIND_I4:
    {
      ESMC_I4 *tempResult = (ESMC_I4 *)result;
      ESMC_I4 *tempBase = (ESMC_I4 *)localResult;
      switch (op){
      case ESMF_SUM:
        *tempResult = 0;  // prime the result variable
        for (int i=0; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          *tempResult += tempBase[i];
        }
        break;
      case ESMF_MIN:
        vm->vmk_commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
        }
        break;
      case ESMF_MAX:
        vm->vmk_commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
        }
        break;
      }
    }
    break;
  case ESMC_TYPEKIND_R4:
    {
      ESMC_R4 *tempResult = (ESMC_R4 *)result;
      ESMC_R4 *tempBase = (ESMC_R4 *)localResult;
      switch (op){
      case ESMF_SUM:
        *tempResult = 0.;  // prime the result variable
        for (int i=0; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          *tempResult += tempBase[i];
        }
        break;
      case ESMF_MIN:
        vm->vmk_commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
        }
        break;
      case ESMF_MAX:
        vm->vmk_commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
        }
        break;
      }
    }
    break;
  case ESMC_TYPEKIND_R8:
    {
      ESMC_R8 *tempResult = (ESMC_R8 *)result;
      ESMC_R8 *tempBase = (ESMC_R8 *)localResult;
      switch (op){
      case ESMF_SUM:
        *tempResult = 0.;  // prime the result variable
        for (int i=0; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          *tempResult += tempBase[i];
        }
        break;
      case ESMF_MIN:
        vm->vmk_commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
        }
        break;
      case ESMF_MAX:
        vm->vmk_commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->vmk_commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] > *tempResult) *tempResult = tempBase[i];
        }
        break;
      }
    }
    break;
  }

  // garbage collection
  delete [] localResult;
  
  return NULL;
}
//-----------------------------------------------------------------------------
