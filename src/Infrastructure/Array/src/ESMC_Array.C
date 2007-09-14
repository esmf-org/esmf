// $Id: ESMC_Array.C,v 1.131 2007/09/14 23:07:56 theurich Exp $
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

// include associated header file
#include "ESMC_Array.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMF_Pthread.h"
#include "ESMC_Start.h"

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Array.C,v 1.131 2007/09/14 23:07:56 theurich Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI {

  //-----------------------------------------------------------------------------
//
// constructor and destructor
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::Array()"
//BOPI
// !IROUTINE:  ESMCI::Array::Array    - constructor
//
// !INTERFACE:
Array::Array(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  ESMC_TypeKind typekindArg,              // (in)
  int rankArg,                            // (in)
  ESMC_LocalArray **larrayListArg,        // (in)
  DistGrid *distgridArg,                  // (in)
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
  ESMC_IndexFlag indexflagArg,            // (in)
  int *rc                                 // (out)
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
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // fill in the Array object
  typekind = typekindArg;
  rank = rankArg;
  distgrid = distgridArg;
  delayout = distgrid->getDELayout();
  // copy the PET-local LocalArray pointers
  int localDeCount = delayout->getLocalDeCount();
  larrayList = new ESMC_LocalArray*[localDeCount];
  memcpy(larrayList, larrayListArg, localDeCount*sizeof(ESMC_LocalArray *));
  // determine the base addresses of the local arrays:
  larrayBaseAddrList = new void*[localDeCount];
  for (int i=0; i<localDeCount; i++)
    larrayList[i]->ESMC_LocalArrayGetBaseAddr((void **)&larrayBaseAddrList[i]);
  // copy the PET-local bound arrays
  int dimCount = distgrid->getDimCount();
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
  int deCount = delayout->getDeCount();
  deCellCount = new int[deCount];
  
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  
  for (int i=0; i<deCount; i++){
    deCellCount[i] = 1;   // prime deCellCount element
    int tensorIndex = 0;  // reset
    for (int jj=0; jj<rank; jj++){
      int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor dims
      if (j){
        // decomposed dimension 
        --j;  // shift to basis 0
        deCellCount[i] *= indexCountPDimPDe[i*dimCount+j];
      }else{
        // tensor dimension
        deCellCount[i] *= (ubounds[tensorIndex] - lbounds[tensorIndex] + 1);
        ++ tensorIndex;
      }
    }
  }
  
  const int *localDeList = delayout->getLocalDeList();
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
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          rc)) return;
        // check if this dim has a contiguous index list
        for (int k=1; k<indexCountPDimPDe[de*dimCount+j]; k++){
          if (indexList[k] != indexList[k-1]+1){
            contiguousFlag[i] = 0; // reset
            break;
          }
        }
      }
    } // jj
  }
  
  // invalidate the name for this Array object in the Base class
  ESMC_BaseSetName(NULL, "Array");
   
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::~Array()"
//BOPI
// !IROUTINE:  ESMCI::Array::~Array   - destructor
//
// !INTERFACE:
Array::~Array(){
//
// !DESCRIPTION:
//    Destruct the internal information structure of an ESMC\_Array object.
//
//EOPI
//-----------------------------------------------------------------------------
  // garbage collection
  int localDeCount = delayout->getLocalDeCount();
  for (int i=0; i<localDeCount; i++)
    ESMC_LocalArrayDestroy(larrayList[i]);
  if (larrayList != NULL)
    delete [] larrayList;
  if (larrayBaseAddrList != NULL)
    delete [] larrayBaseAddrList;
  if (exclusiveLBound != NULL)
    delete [] exclusiveLBound;
  if (exclusiveUBound != NULL)
    delete [] exclusiveUBound;
  if (computationalLBound != NULL)
    delete [] computationalLBound;
  if (computationalUBound != NULL)
    delete [] computationalUBound;
  if (totalLBound != NULL)
    delete [] totalLBound;
  if (totalUBound != NULL)
    delete [] totalUBound;
  if (lbounds != NULL)
    delete [] lbounds;
  if (ubounds != NULL)
    delete [] ubounds;
  if (staggerLoc != NULL)
    delete [] staggerLoc;
  if (vectorDim != NULL)
    delete [] vectorDim;
  if (dimmap != NULL)
    delete [] dimmap;
  if (inverseDimmap != NULL)
    delete [] inverseDimmap;
  if (contiguousFlag != NULL)
    delete [] contiguousFlag;
  if (deCellCount != NULL)
    delete [] deCellCount;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::create()"
//BOPI
// !IROUTINE:  ESMCI::Array::create
//
// !INTERFACE:
Array *Array::create(
//
// !RETURN VALUE:
//    ESMC_Array * to newly allocated ESMC_Array
//
// !ARGUMENTS:
//
  ESMC_LocalArray **larrayListArg,            // (in)
  int larrayCount,                            // (in)
  DistGrid *distgrid,                         // (in)
  InterfaceInt *dimmap,                       // (in)
  InterfaceInt *computationalLWidthArg,       // (in)
  InterfaceInt *computationalUWidthArg,       // (in)
  InterfaceInt *totalLWidthArg,               // (in)
  InterfaceInt *totalUWidthArg,               // (in)
  ESMC_IndexFlag *indexflagArg,               // (in)
  int *staggerLocArg,                         // (in)
  int *vectorDimArg,                          // (in)
  InterfaceInt *lboundsArg,                   // (in)
  InterfaceInt *uboundsArg,                   // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Array} object from list if LocalArrays and DistGrid.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check the input and get the information together to call construct()
  // larrayListArg -> typekind/rank
  if (larrayListArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to larrayList argument", rc);
    return ESMC_NULL_POINTER;
  }
  if (larrayCount < 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- The larrayList argument must provide at least one LocalArray object",
      rc);
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind typekind = larrayListArg[0]->ESMC_LocalArrayGetTypeKind();
  int rank = larrayListArg[0]->ESMC_LocalArrayGetRank();
  for (int i=1; i<larrayCount; i++){
    if (larrayListArg[0]->ESMC_LocalArrayGetTypeKind() != typekind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- TypeKind mismatch in the elements of larrayList argument", rc);
      return ESMC_NULL_POINTER;
    }
    if (larrayListArg[0]->ESMC_LocalArrayGetRank() != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- Rank mismatch in the elements of larrayList argument", rc);
      return ESMC_NULL_POINTER;
    }
  }
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid", rc);
    return ESMC_NULL_POINTER;
  }
  const DELayout *delayout = distgrid->getDELayout();
  int dimCount = distgrid->getDimCount();
  if (dimCount > rank){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
      "- dimCount of distgrid argument must be <= rank of Array", rc);
    return ESMC_NULL_POINTER;
  }  
  // check for lbounds and ubounds arguments and that they match dimCount, rank
//todo: provide a default here that leaves the bounds of the incoming LocalArray
//      unchanged for tensor dimensions and don't require these arguments
  int tensorCount = rank - dimCount;  // number of tensor dimensions
  if (tensorCount > 0 && lboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid lbounds argument required to create Array with tensor dims", rc);
    return ESMC_NULL_POINTER;
  }
  if (tensorCount > 0 && uboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid ubounds argument required to create Array with tensor dims", rc);
    return ESMC_NULL_POINTER;
  }
  int *lboundsArray = NULL; // reset
  if (lboundsArg != NULL){
    if (lboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (lboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    lboundsArray = lboundsArg->array;
  }
  int *uboundsArray = NULL; // reset
  if (uboundsArg != NULL){
    if (uboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (uboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- ubounds, arrayspec, distgrid mismatch", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (dimmap->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (dimmap->array[i] < 1 || dimmap->array[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- dimmap / rank mismatch", rc);
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
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  if (localDeCount != larrayCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- Mismatch in localDeCount between larrayList argument and DELayout", 
      rc);
    return ESMC_NULL_POINTER;
  }
  const int *localDeList = delayout->getLocalDeList();
  // distgrid -> indexCountPDimPDe[]
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  // check on indexflag
  ESMC_IndexFlag indexflag = ESMF_INDEX_DELOCAL;  // default
  if (indexflagArg != NULL)
    indexflag = *indexflagArg;
  // figure exclusive region
  int *exclusiveLBound = new int[dimCount*localDeCount];
  int *exclusiveUBound = new int[dimCount*localDeCount];
  for (int i=0; i<dimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    memcpy(&(exclusiveUBound[i*dimCount]), &(indexCountPDimPDe[de*dimCount]),
      dimCount*sizeof(int));
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMF_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      for (int j=0; j<dimCount; j++){
        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
          return ESMC_NULL_POINTER;
        // check that this dim has a contiguous index list
        for (int k=1; k<indexCountPDimPDe[de*dimCount+j]; k++){
          if (indexList[k] != indexList[k-1]+1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
              "- Cannot use non-contiguous decomposition for pseudo global"
              " index space", rc);
            return ESMC_NULL_POINTER;
          }
        }
        // shift bounds of exclusive region to match indexList[0]
        int shift = indexList[0] - exclusiveLBound[i*dimCount+j];
        exclusiveLBound[i*dimCount+j] += shift;
        exclusiveUBound[i*dimCount+j] += shift;
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
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalLWidth may only contain positive values", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUWidth may only contain positive values", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalLWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*dimCount+i] = exclusiveLBound[j*dimCount+i]
          - totalLWidthArg->array[i];
        if (totalLBound[j*dimCount+i] > computationalLBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalLWidth / computationaLWidth mismatch", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalUWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*dimCount+i] = exclusiveUBound[j*dimCount+i]
          + totalUWidthArg->array[i];
        if (totalUBound[j*dimCount+i] < computationalUBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalUWidth / computationaUWidth mismatch", rc);
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
          return ESMC_NULL_POINTER;
        }
        temp_lbounds[jj] = lboundsArray[jjj];
        temp_ubounds[jj] = uboundsArray[jjj];
        ++jjj;
      }
    }
    // adjust LocalArray object for specific lbounds and ubounds
    larrayList[i] = larrayListArg[i]->
      ESMC_LocalArrayAdjust(temp_lbounds, temp_ubounds, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }
  delete [] temp_counts;
  delete [] temp_lbounds;
  delete [] temp_ubounds;
  
  // call class constructor
  Array *array;
  try{
    array = new Array(typekind, rank, larrayList, distgrid, exclusiveLBound,
      exclusiveUBound, computationalLBound, computationalUBound,
      totalLBound, totalUBound, tensorCount, lboundsArray, uboundsArray,
      staggerLoc, vectorDim, dimmapArray, inverseDimmapArray, indexflag,
      &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::Array.", rc);  
     return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] larrayList;
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
#define ESMC_METHOD "ESMCI::Array::create()"
//BOPI
// !IROUTINE:  ESMCI::Array::create
//
// !INTERFACE:
Array *Array::create(
//
// !RETURN VALUE:
//    ESMCI::Array * to newly allocated ESMCI::Array
//
// !ARGUMENTS:
//
  ESMC_ArraySpec *arrayspec,                  // (in)
  DistGrid *distgrid,                         // (in)
  InterfaceInt *dimmap,                       // (in)
  InterfaceInt *computationalLWidthArg,       // (in)
  InterfaceInt *computationalUWidthArg,       // (in)
  InterfaceInt *totalLWidthArg,               // (in)
  InterfaceInt *totalUWidthArg,               // (in)
  ESMC_IndexFlag *indexflagArg,               // (in)
  int *staggerLocArg,                         // (in)
  int *vectorDimArg,                          // (in)
  InterfaceInt *lboundsArg,                   // (in)
  InterfaceInt *uboundsArg,                   // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Array} object from ArraySpec and DistGrid.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // check the input and get the information together to call construct()
  // arrayspec -> typekind/rank
  if (arrayspec == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to arrayspec", rc);
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind typekind = arrayspec->ESMC_ArraySpecGetTypeKind();
  int rank = arrayspec->ESMC_ArraySpecGetRank();
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid", rc);
    return ESMC_NULL_POINTER;
  }
  const DELayout *delayout = distgrid->getDELayout();
  int dimCount = distgrid->getDimCount();
  // check if dimmap was provided and matches rest of arguments
  int *dimmapArray = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    if (i < rank)
      dimmapArray[i] = i+1; // default (basis 1)
    else
      dimmapArray[i] = 0;   // default (replicator dims beyond rank)
  }
  if (dimmap != NULL){
    if (dimmap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- dimmap array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (dimmap->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- dimmap and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (dimmapArray[i] < 0 || dimmapArray[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- invalid dimmap element", rc);
        return ESMC_NULL_POINTER;
      }
      dimmapArray[i] = dimmap->array[i];  // copy dimmap array element
    }
  }
  // determine replicatorCount
  int replicatorCount = 0;  // initialize
  for (int i=0; i<dimCount; i++)
    if (dimmapArray[i] == 0) ++replicatorCount;
  // determine tensorCount
  int tensorCount = rank - (dimCount - replicatorCount);
  if (tensorCount < 0) tensorCount = 0;
  // check for lbounds and ubounds arguments and that they match dimCount, rank
  if (tensorCount > 0 && lboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid lbounds argument required to create Array with tensor dims", rc);
    return ESMC_NULL_POINTER;
  }
  if (tensorCount > 0 && uboundsArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid ubounds argument required to create Array with tensor dims", rc);
    return ESMC_NULL_POINTER;
  }
  int *lboundsArray = NULL; // reset
  if (lboundsArg != NULL){
    if (lboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- lbounds array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (lboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- lbounds, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    lboundsArray = lboundsArg->array;
  }
  int *uboundsArray = NULL; // reset
  if (uboundsArg != NULL){
    if (uboundsArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- ubounds array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (uboundsArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- ubounds, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    uboundsArray = uboundsArg->array;
  }
  // generate inverseDimmap
  int *inverseDimmapArray = new int[rank];
  for (int i=0; i<rank; i++)
    inverseDimmapArray[i] = 0; // reset  (basis 1), 0 indicates tensor dim
  for (int i=0; i<dimCount; i++){
    if (dimmapArray[i] > 0)
      inverseDimmapArray[dimmapArray[i]-1] = i+1;
  }
  // delayout -> deCount, localDeCount, localDeList
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeList = delayout->getLocalDeList();
  // distgrid -> indexCountPDimPDe[]
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  // check on indexflag
  ESMC_IndexFlag indexflag = ESMF_INDEX_DELOCAL;  // default
  if (indexflagArg != NULL)
    indexflag = *indexflagArg;
  // figure exclusive region
  int *exclusiveLBound = new int[dimCount*localDeCount];
  int *exclusiveUBound = new int[dimCount*localDeCount];
  for (int i=0; i<dimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    memcpy(&(exclusiveUBound[i*dimCount]), &(indexCountPDimPDe[de*dimCount]),
      dimCount*sizeof(int));
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMF_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      for (int j=0; j<dimCount; j++){
        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
          return ESMC_NULL_POINTER;
        // check that this dim has a contiguous index list
        for (int k=1; k<indexCountPDimPDe[de*dimCount+j]; k++){
          if (indexList[k] != indexList[k-1]+1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
              "- Cannot use non-contiguous decomposition for pseudo global"
              " index space", rc);
            return ESMC_NULL_POINTER;
          }
        }
        // shift bounds of exclusive region to match indexList[0]
        int shift = indexList[0] - exclusiveLBound[i*dimCount+j];
        exclusiveLBound[i*dimCount+j] += shift;
        exclusiveUBound[i*dimCount+j] += shift;
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
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalLWidth may only contain positive values", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (computationalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUWidth may only contain positive values", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalLWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*dimCount+i] = exclusiveLBound[j*dimCount+i]
          - totalLWidthArg->array[i];
        if (totalLBound[j*dimCount+i] > computationalLBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalLWidth / computationaLWidth mismatch", rc);
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
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<dimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalUWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*dimCount+i] = exclusiveUBound[j*dimCount+i]
          + totalUWidthArg->array[i];
        if (totalUBound[j*dimCount+i] < computationalUBound[j*dimCount+i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- totalUWidth / computationaUWidth mismatch", rc);
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
  
  // call class constructor
  Array *array;
  try{
    array = new Array(typekind, rank, larrayList, distgrid, exclusiveLBound,
      exclusiveUBound, computationalLBound, computationalUBound,
      totalLBound, totalUBound, tensorCount, lboundsArray, uboundsArray,
      staggerLoc, vectorDim, dimmapArray, inverseDimmapArray, indexflag,
      &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::Array.", rc);  
     return ESMC_NULL_POINTER;
  }

  // garbage collection
  delete [] larrayList;
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
#define ESMC_METHOD "ESMCI::Array::destroy()"
//BOPI
// !IROUTINE:  ESMCI::Array::destroy
//
// !INTERFACE:
int Array::destroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array **array){  // in - ESMC_Array to destroy
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (array == ESMC_NULL_POINTER || *array == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", &rc);
    return rc;
  }

  // delete Array object
  delete *array;
  *array = ESMC_NULL_POINTER;
  
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
#define ESMC_METHOD "ESMCI::Array::getLinearIndexExclusive()"
//BOPI
// !IROUTINE:  ESMCI::Array::getLinearIndexExclusive
//
// !INTERFACE:
int Array::getLinearIndexExclusive(
//
// !RETURN VALUE:
//    int linear index
//
// !ARGUMENTS:
//
  int localDe,                      // in - local DE
  int *index,                       // in - DE-local index tupple in exclusive
                                    //      region basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get linear index - assuming index input to be be basis 0 in excl. region
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

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
      
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return linindex;    // leave this index basis 0
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// misc.
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::print()"
//BOPI
// !IROUTINE:  ESMCI::Array::print
//
// !INTERFACE:
int Array::print()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of Array object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", &rc);
    return rc;
  }

  // print info about the ESMCI::Array object
  printf("--- ESMCI::Array::print start ---\n");
  ESMC_Print(); // print the Base class info
  printf("Array typekind/rank: %s / %d \n", ESMC_TypeKindString(typekind),
    rank);
  printf("~ cached values ~\n");
  int dimCount = distgrid->getDimCount();
  printf("DistGrid dimCount = %d\n", dimCount);
  int deCount = delayout->getDeCount();
  printf("deCount = %d\n", deCount);
  int localDeCount = delayout->getLocalDeCount();
  printf("localDeCount = %d\n", localDeCount);
  const int *localDeList = delayout->getLocalDeList();
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
  printf("--- ESMCI::Array::print end ---\n");
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::validate()"
//BOPI
// !IROUTINE:  ESMCI::Array::validate
//
// !INTERFACE:
int Array::validate()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Validate details of Array object 
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
// serialize() and deserialize()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::serialize()"
//BOPI
// !IROUTINE:  ESMCI::Array::serialize - Turn Array into a byte stream
//
// !INTERFACE:
int Array::serialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length
  int *offset)const{     // inout - original offset, updated to point 
                         //         to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in array class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Prepare pointer variables of different types
  char *cp;
  int *ip;
  ESMC_TypeKind *dkp;
  ESMC_IndexFlag *ifp;

  // Check if buffer has enough free memory to hold object
  if ((*length - *offset) < sizeof(Array)){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an Array object", &rc);
    return rc;
  }

  // First, serialize the base class,
  localrc = ESMC_Base::ESMC_Serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Serialize the DistGrid
  localrc = distgrid->serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Then, serialize Array meta data
  dkp = (ESMC_TypeKind *)(buffer + *offset);
  *dkp++ = typekind;
  ip = (int *)dkp;
  *ip++ = rank;
  ifp = (ESMC_IndexFlag *)ip;
  *ifp++ = indexflag;
  
  // fix offset  
  cp = (char *)ifp;
  *offset = (cp - buffer);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::Array::deserialize - Turn a byte stream into an Array
//
// !INTERFACE:
int Array::deserialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // in - byte stream to read
  int *offset){          // inout - original offset, updated to point 
                         //         to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn a stream of bytes into an object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Prepare pointer variables of different types
  char *cp;
  int *ip;
  ESMC_TypeKind *dkp;
  ESMC_IndexFlag *ifp;

  // First, deserialize the base class
  localrc = ESMC_Base::ESMC_Deserialize(buffer, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;

  // Deserialize the DistGrid
  distgrid = DistGrid::deserialize(buffer, offset);
  // Pull DELayout out of DistGrid
  delayout = distgrid->getDELayout();
  // Then, deserialize Array meta data
  dkp = (ESMC_TypeKind *)(buffer + *offset);
  typekind = *dkp++;
  ip = (int *)dkp;
  rank = *ip++;
  ifp = (ESMC_IndexFlag *)ip;
  indexflag = *ifp++;
  
  // fix offset
  cp = (char *)ifp;
  *offset = (cp - buffer);
  
  // set values with local dependency
  larrayList = new ESMC_LocalArray*[0];     // no DE on proxy object
  larrayBaseAddrList = new void*[0];        // no DE on proxy object

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


//-----------------------------------------------------------------------------
//
// comms
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::scatter()"
//BOPI
// !IROUTINE:  ESMCI::Array::scatter
//
// !INTERFACE:
int Array::scatter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *arrayArg,                       // in -
  ESMC_TypeKind typekindArg,            // in -
  int rankArg,                          // in -
  int *counts,                          // in -
  int *patchArg,                        // in -
  int rootPet,                          // in -
  VM *vm                                // in -
  ){    
//
//
// !DESCRIPTION:
//    Scatter native array across Array object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", &rc);
    return rc;
  }

  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
  }

  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  
//printf("gjt in ArrayScatter: localPet: %d \n", localPet);
//printf("gjt in ArrayScatter: typekind/rank: %s / %d \n",
//ESMC_TypeKindString(typekindArg), rankArg);
//printf("gjt in ArrayScatter: counts: %d, %d, %d\n", counts[0], counts[1],
//counts[2]);

  // deal with optional patch argument
  int patch = 1;  // default
  if (patchArg)
    patch = *patchArg;
  int patchCount = distgrid->getPatchCount();
  if (patch < 1 || patch > patchCount){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
      "- Specified patch out of bounds", &rc);
    return rc;
  }
  const int *patchListPDe = distgrid->getPatchListPDe();

  // get minIndexPDimPPatch and maxIndexPDimPPatch for patch
  const int *minIndexPDimPPatch =
    distgrid->getMinIndexPDimPPatch(patch, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  const int *maxIndexPDimPPatch =
    distgrid->getMaxIndexPDimPPatch(patch, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // only on rootPet
  if (localPet == rootPet){
    // check consistency of input information: shape = (typekind, rank, extents)
    if (typekindArg != typekind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between array argument and Array object", &rc);
      return rc;
    }
    if (rankArg != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Type mismatch between array argument and Array object", &rc);
      return rc;
    }
    int tensorIndex=0;  // reset
    for (int i=0; i<rank; i++){
      int j = inverseDimmap[i];
      if (j){
        // decomposed dimension
        --j;  // shift to basis 0
        if (counts[i] != maxIndexPDimPPatch[j] - minIndexPDimPPatch[j] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", &rc);
          return rc;
        }
      }else{
        // tensor dimension
        if (counts[i] != ubounds[tensorIndex] - lbounds[tensorIndex] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", &rc);
          return rc;
        }
        ++tensorIndex;
      }
    }
  }

  // size in bytes of each piece of data
  int dataSize = ESMC_TypeKindSize(typekind);

  // prepare for comms
  vmk_commhandle **commh = new vmk_commhandle*; // used by all comm calls
  vmk_commhandle **commhList = 
    new vmk_commhandle*[rank]; // used for indexList comm
  
  // distgrid and delayout values
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  int dimCount = distgrid->getDimCount();
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeList = delayout->getLocalDeList();
  const int *deList = delayout->getDeList();
  
  // rootPet is the only sender for scatter, but may need info from other PETs
  char **sendBuffer;
  if (localPet == rootPet){
    char *array = (char *)arrayArg;
    // for each DE of the Array memcpy together a single contiguous sendBuffer
    // from "array" data and send it to the receiving PET non-blocking.
    sendBuffer = new char*[deCount]; // contiguous sendBuffer
    int *ii = new int[rank];     // index tuple basis 0
    int *iiEnd = new int[rank];
    for (int i=0; i<deCount; i++){
      int de = i;
      if (patchListPDe[de] == patch){
        // this DE is located on receiving patch
        int **indexList = new int*[rank];
        int tensorIndex=0;  // reset
        // get contigFlag for first dimension for this de (dim is basis 1)
        int contigFlag = distgrid->getContigFlagPDimPDe(de, 1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        int commhListCount = 0;  // reset
        for (int jj=0; jj<rank; jj++){
          int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            // obtain indexList for this DE and dim
            indexList[jj] = new int[indexCountPDimPDe[de*dimCount+j]];
            // check if this DE is local or not            
            if (deList[de] == -1){
              // this DE is _not_ local -> receive indexList from respective Pet
              int srcPet;
              delayout->getDEMatchPET(de, *vm, NULL, &srcPet, 1);
              commhList[commhListCount] = new vmk_commhandle;
              localrc = vm->vmk_recv(indexList[jj],
                sizeof(int)*indexCountPDimPDe[de*dimCount+j], srcPet, 
                &(commhList[commhListCount]));
              if (localrc){
                char *message = new char[160];
                sprintf(message, "VMKernel/MPI error #%d\n", localrc);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                  message, &rc);
                delete [] message;
                return rc;
              }
              commhListCount++;
            }else{
              // this DE _is_ local -> look up indexList locally
              const int *localIndexList =
                distgrid->getIndexListPDimPLocalDe(deList[de], j+1, &localrc);
              if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
                ESMF_ERR_PASSTHRU, &rc)) return rc;
              memcpy(indexList[jj], localIndexList, sizeof(int)*
                indexCountPDimPDe[de*dimCount+j]);
              // shift basis 1 -> basis 0
              for (int k=0; k<indexCountPDimPDe[de*dimCount+j]; k++)
                indexList[jj][k] -= minIndexPDimPPatch[j];
            }
          }else{
            // tensor dimension
            int extent = ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
            indexList[jj] = new int[extent];
            for (int k=0; k<extent; k++)
              indexList[jj][k] = k;   // basis 0
            ++tensorIndex;
          }
        } // jj

        // prepare contiguous sendBuffer for this DE
        sendBuffer[de] = new char[deCellCount[de]*dataSize];
        // reset counters for multi-dim while-loop
        tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          ii[jj] = 0;  // reset
          int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            iiEnd[jj] = indexCountPDimPDe[de*dimCount+j];
          }else{
            // tensor dimension
            iiEnd[jj] = ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
            ++tensorIndex;
          }
        }
        
        // wait for all outstanding indexList receives for this DE
        for (int jj=0; jj<commhListCount; jj++){
          vm->commwait(&(commhList[jj]));
          delete commhList[jj];
        }
        
        // loop over all cells in exclusive region for this DE 
        // via multi-dim while-loop
        int sendBufferIndex = 0;  // reset
        while(ii[rank-1] < iiEnd[rank-1]){        
          // determine linear index for this cell into array
          int linearIndex = indexList[rank-1][ii[rank-1]];  // init
          for (int j=rank-2; j>=0; j--){
            linearIndex *= counts[j];
            linearIndex += indexList[j][ii[j]];
          }
        
          // copy this element into the contiguous sendBuffer for this DE
          if (contigFlag){
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
        delayout->getDEMatchPET(de, *vm, NULL, &dstPet, 1);
        *commh = NULL; // invalidate
        localrc = vm->vmk_send(sendBuffer[de], deCellCount[de]*dataSize, dstPet,
          commh);
        if (localrc){
          char *message = new char[160];
          sprintf(message, "VMKernel/MPI error #%d\n", localrc);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
            message, &rc);
          delete [] message;
          return rc;
        }
        
        // clean-up
        for (int j=0; j<rank; j++)
          delete [] indexList[j];
        delete [] indexList;
      } // DE on patch
    } // i -> de
    
    delete [] ii;
    delete [] iiEnd;
  }else{
    // localPet is _not_ rootPet -> provide localIndexList to rootPet if nec.
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      if (patchListPDe[de] == patch){
        // this DE is located on receiving patch -> must send info to rootPet
        int **indexList = new int*[rank];
        for (int jj=0; jj<rank; jj++){
          int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            // obtain local indexList for this DE and dim
            indexList[jj] = new int[indexCountPDimPDe[de*dimCount+j]];
            const int *localIndexList =
              distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
            if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
              ESMF_ERR_PASSTHRU, &rc)) return rc;
            memcpy(indexList[jj], localIndexList, sizeof(int)*
              indexCountPDimPDe[de*dimCount+j]);
            // shift basis 1 -> basis 0
            for (int k=0; k<indexCountPDimPDe[de*dimCount+j]; k++)
              indexList[jj][k] -= minIndexPDimPPatch[j];
            // send indexList of local DE to rootPet
            *commh = NULL;  // invalidate
            localrc = vm->vmk_send(indexList[jj],
              sizeof(int)*indexCountPDimPDe[de*dimCount+j], rootPet, commh);
            if (localrc){
              char *message = new char[160];
              sprintf(message, "VMKernel/MPI error #%d\n", localrc);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                message, &rc);
              delete [] message;
              return rc;
            }
          }
        } // jj
        // wait for all outstanding indexList sends
        vm->commqueuewait();
        // clean-up
        for (int jj=0; jj<rank; jj++){
          int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor
          if (j)
            delete [] indexList[jj];
        }
        delete [] indexList;
      } // DE on patch
    } // i -> de
  }
  
  // - done issuing nb sends (from rootPet) -

  // all PETs may be receivers
  char **recvBuffer = new char*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (patchListPDe[de] != patch) continue; // skip to next local DE
    recvBuffer[i] = (char *)larrayBaseAddrList[i]; // default: contiguous
    if (!contiguousFlag[i])
      recvBuffer[i] = new char[deCellCount[de]*dataSize];
    *commh = NULL; // invalidate
    // receive data into recvBuffer
    localrc = vm->vmk_recv(recvBuffer[i], deCellCount[de]*dataSize, rootPet,
      commh);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
        message, &rc);
      delete [] message;
      return rc;
    }
  }
  
  // - done issuing nb receives (potentially all Pets) -

  // todo: separate receive and send commhandles and separately wait!
  // wait until all the local receives are complete
  // wait until all the local sends are complete
  // for now wait on _all_ outstanding non-blocking comms for this PET
  vm->commqueuewait();
  
  // - done waiting on sends and receives -
    
  // distribute received data into non-contiguous exclusive regions
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (patchListPDe[de] != patch) continue; // skip to next local DE
    if (!contiguousFlag[i]){
      // only if this DE has a non-contiguous decomposition the contiguous
      // receive buffer must be copied into DE-local array segment piece by p.
      char *larrayBaseAddr = (char *)larrayBaseAddrList[i];
      // reset counters for multi-dim while-loop
      int *ii = new int[rank];        // index tuple basis 0
      int *iiSrt = new int[rank];
      int *iiEnd = new int[rank];
      int *skipWidth = new int[rank];
      int tensorIndex = 0;    // reset
      int totalWidthProd = 1; // reset
      for (int jj=0; jj<rank; jj++){
        int j = inverseDimmap[jj];// j is dimIndex basis 1, or 0 for tensor dims
        if (j){
          // decomposed dimension 
          --j;  // shift to basis 0
          iiSrt[jj] = exclusiveLBound[i*dimCount+j] - totalLBound[i*dimCount+j];
          iiEnd[jj] = exclusiveUBound[i*dimCount+j] - totalLBound[i*dimCount+j]
            + 1;
          skipWidth[jj] = (totalUBound[i*dimCount+j] -totalLBound[i*dimCount+j])
            - (exclusiveUBound[i*dimCount+j] - exclusiveLBound[i*dimCount+j]);
          skipWidth[jj] *= totalWidthProd;  // multiply in previous dims
          // update totalWidthProd for next dims
          totalWidthProd *=
            totalUBound[i*dimCount+j] - totalLBound[i*dimCount+j] + 1;
        }else{
          // tensor dimension
          iiSrt[jj] = 0;
          iiEnd[jj] = ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
          skipWidth[jj] = totalWidthProd;
          totalWidthProd *= ubounds[tensorIndex] - lbounds[tensorIndex] + 1;
          ++tensorIndex;
        }
        ii[jj] = iiSrt[jj];
      }
      // loop over all cells in exclusive region for this DE and memcpy data
      // via multi-dim while-loop
      int recvBufferIndex = 0;  // reset
      // determine start linear index for this cell into larrayBaseAddrList[i]
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
      while(ii[rank-1] < iiEnd[rank-1]){
        // copy this element from the contiguous recvBuffer into excl. region
        // since the data in the recvBuffer was constructed to be contiguous
        // wrt data layout on destination DE -> contiguous data copy in 1st dim
        memcpy(larrayBaseAddr+linearIndex*dataSize,
          recvBuffer[i]+recvBufferIndex*dataSize, (iiEnd[0]-iiSrt[0])*dataSize);
        // skip to end of 1st dimension
        ii[0] = iiEnd[0];
        linearIndex += iiEnd[0] - iiSrt[0];
        recvBufferIndex += iiEnd[0] - iiSrt[0];
        
        // multi-dim index increment and linearIndex advancement
        for (int j=0; j<rank-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = iiSrt[j];  // reset
            ++ii[j+1];
            linearIndex += skipWidth[j];
          }else if (j==0){
            ++linearIndex;
          }
        }
      } // multi-dim while-loop

      // clean-up
      delete [] recvBuffer[i];
      delete [] ii;
      delete [] iiEnd;
      delete [] iiSrt;
      delete [] skipWidth;
    } // !contiguousFlag
  } // i -> de
    
  // garbage collection
  delete [] recvBuffer;
  if (localPet == rootPet){
    for (int i=0; i<deCount; i++)
      if (patchListPDe[i] == patch)
        delete [] sendBuffer[i];
    delete [] sendBuffer;
  }
  delete commh;
  delete [] commhList;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::sparseMatMulStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::sparseMatMulStore
//
// !INTERFACE:
int Array::sparseMatMulStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                      // in    - source Array
  Array *dstArray,                      // in    - destination Array
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
  ESMC_TypeKind typekindArg,            // in    - typekind of factors
  void *factorList,                     // in    - sparse matrix factors
  int factorListCount,                  // in    - number of sparse mat. indices
  InterfaceInt *factorIndexList         // in    - sparse matrix indices
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for sparse matrix multiplication
//  from srcArray to dstArray.
//
//  The implementation consists of three main phases:
//
//  - Phase I:    Check input for consistency. The sparse matrix is provided in
//                "input" distribution 
//  - Phase II:   Construct two distributed directories of sparse matrix
//                elements, one indexed by srcSeqIndex, one indexed by
//                dstSeqIndex. This takes the matrix from "input" to "work"
//                distribution.
//  - Phase III:  Use the information in "work" distribution to precompute the
//                XXE stream and take associated data into "run" distribution
//                according to the "execution pattern". Currently the
//                "dstArray execution pattern" has been implemented.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  //---------------------------------------------------------------------------
  // Phase I
  //---------------------------------------------------------------------------
  
  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  
#define ASMMSTORETIMING___disable
#ifdef ASMMSTORETIMING
  double t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10X, t10Y, t11;//gjt - profile
  double t4a, t4b, t4c, t5a, t5b, t5c;  //gjt - profile
  double t4c1=0., t4c2=0., t4c3=0., t4c4=0., t4c5=0.;  //gjt - profile
  double t4c3a, t4c3b, t4c3c, t4c3d;  //gjt - profile
  double dt4c34=0., dt4c45=0.;  //gjt - profile
  double dt4c3ab=0., dt4c3bc=0., dt4c3cd=0.;  //gjt - profile
  double t10Xa, t10Xb, t10Xc, t10Xd, t10Xe; //gjt - profile
  double t10Xc1, t10Xc2; //gjt - profile
  double t10Yf, t10Yg; //gjt - profile
  VMK::wtime(&t0);   //gjt - profile
#endif
  
  // every Pet must provide srcArray and dstArray
  if (srcArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to srcArray", &rc);
    return rc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to dstArray", &rc);
    return rc;
  }
  
  // every Pet that specifies factorListCount > 0 must be checked wrt input
  if (factorListCount > 0){
    // must provide valid factorList and factorIndexList args
    if (factorIndexList == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to factorIndexList array", &rc);
      return rc;
    }
    if (factorIndexList->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- factorIndexList array must be of rank 2", &rc);
      return rc;
    }
    if (factorIndexList->extent[0] != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- first dimension of factorIndexList array must be of size 2", &rc);
      return rc;
    }
    if (factorIndexList->extent[1] != factorListCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- second dimension of factorIndexList does not match factorListCount",
        &rc);
      return rc;
    }
    // must define a valid typekind
    if (typekindArg == ESMF_NOKIND){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- must specify valid typekindArg on PETs that provide factorList",
        &rc);
      return rc;
    }
    if (typekindArg != ESMC_TYPEKIND_I4 && typekindArg != ESMC_TYPEKIND_I8
      && typekindArg != ESMC_TYPEKIND_R4 && typekindArg != ESMC_TYPEKIND_R8){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- method not implemented for specified typekindArg", &rc);
      return rc;
    }
  }else{
    // set typekindArg to ESMF_NOKIND -> prevent this Pet to act as rootPet
    // when factors are being distributed
    typekindArg = ESMF_NOKIND;
  }

  // communicate typekindArg across all Pets
  ESMC_TypeKind *typekindList = new ESMC_TypeKind[petCount];
  vm->vmk_allgather(&typekindArg, typekindList, sizeof(ESMC_TypeKind));
  // check that all non-ESMF_NOKIND typekindList elements match,
  // set local typekindArg accordingly and keep track of Pets that have factors
  int *factorPetList = new int[petCount];
  int factorPetCount = 0; // reset
  typekindArg = ESMF_NOKIND;  // initialize
  for (int i=0; i<petCount; i++){
    if (typekindList[i] != ESMF_NOKIND)
      factorPetList[factorPetCount++] = i;
    if (typekindArg == ESMF_NOKIND)
      typekindArg = typekindList[i];  // set to first element not ESMF_NOKIND
    else{
      // check consequent elements against the set typekindArg
      if (typekindList[i] != ESMF_NOKIND && typekindArg != typekindList[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
          "- TypeKind mismatch between PETs", &rc);
        return rc;
      }
    }
  }
  delete [] typekindList;
  
  // check that factorPetCount at least 1
  if (factorPetCount < 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- at least one PET must provide a valid factorList", &rc);
    return rc;
  }
  
  // check that the typekindArg matches srcArray typekind
  if (typekindArg != srcArray->getTypekind()){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
      "- TypeKind mismatch between srcArray argument and factorList", &rc);
    return rc;
  }
  // check that the typekindArg matches dstArray typekind
  if (typekindArg != dstArray->getTypekind()){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
      "- TypeKind mismatch between dstArray argument and factorList", &rc);
    return rc;
  }

  // size in bytes of each piece of data
  int dataSize = ESMC_TypeKindSize(typekindArg);
  
  // todo: the current implementation requires (dimCount == rank)
  // todo: remove the following test if that limitation has been removed
  if (srcArray->rank != srcArray->distgrid->getDimCount()){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
      "- Currently only (rank == dimCount) supported", &rc);
    return rc;
  }
  if (dstArray->rank != dstArray->distgrid->getDimCount()){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
      "- Currently only (rank == dimCount) supported", &rc);
    return rc;
  }

  //TODO: maybe I will need these VMs to support exec() in model components?
#if 0   
  // get the srcArray VM and VM releated information
  VM *srcVm = srcArray->delayout->getVM();
  int srcLocalPet = srcVm->getLocalPet();
  int srcPetCount = srcVm->getPetCount();
  
  // get the dstArray VM and VM releated information
  VM *dstVm = dstArray->delayout->getVM();
  int dstLocalPet = dstVm->getLocalPet();
  int dstPetCount = dstVm->getPetCount();
#endif  
  
  // prepare for relative run-time addressing (RRA)
  // this is to support xxe->exec() during sparseMatMulStore()
  int rraCount = srcArray->delayout->getLocalDeCount();
  rraCount += dstArray->delayout->getLocalDeCount();
  char **rraList = new char*[rraCount];
  memcpy((char *)rraList, srcArray->larrayBaseAddrList,
    srcArray->delayout->getLocalDeCount() * sizeof(char *));
  memcpy((char *)rraList
    + srcArray->delayout->getLocalDeCount() * sizeof(char *),
    dstArray->larrayBaseAddrList,
    dstArray->delayout->getLocalDeCount() * sizeof(char *));

#ifdef ASMMSTORETIMING
  VMK::wtime(&t1);   //gjt - profile
#endif
    
  //---------------------------------------------------------------------------
  // Phase II
  //---------------------------------------------------------------------------

  // determine local srcCellCount
  int srcLocalDeCount = srcArray->delayout->getLocalDeCount();
  const int *srcLocalDeList = srcArray->delayout->getLocalDeList();
  int *srcDistGridLocalDeCellCount = new int[srcLocalDeCount];
  int srcCellCount = 0;   // initialize
  for (int i=0; i<srcLocalDeCount; i++){
    int de = srcLocalDeList[i];  // global DE index
    srcDistGridLocalDeCellCount[i] =
      srcArray->distgrid->getCellCountPDe(de, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
    srcCellCount += srcDistGridLocalDeCellCount[i];
  }
  // communicate srcCellCount across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *srcCellCountList = new int[petCount];
  vm->vmk_allgather(&srcCellCount, srcCellCountList, sizeof(int));
  // determine local dstCellCount
  int dstLocalDeCount = dstArray->delayout->getLocalDeCount();
  const int *dstLocalDeList = dstArray->delayout->getLocalDeList();
  int *dstDistGridLocalDeCellCount = new int[dstLocalDeCount];
  int dstCellCount = 0;   // initialize
  for (int i=0; i<dstLocalDeCount; i++){
    int de = dstLocalDeList[i];  // global DE index
    dstDistGridLocalDeCellCount[i] = 
      dstArray->distgrid->getCellCountPDe(de, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
    dstCellCount += dstDistGridLocalDeCellCount[i];
  }
  // communicate dstCellCount across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *dstCellCountList = new int[petCount];
  vm->vmk_allgather(&dstCellCount, dstCellCountList, sizeof(int));
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t2);   //gjt - profile
#endif
  
  // local structs to simplify code

  struct Interval{
    int min;
    int max;
    int count;
  };

  struct FactorElement{
    int partnerSeqIndex;
    int partnerDe;
    char factor[8]; // large enough for R8 and I8
  };
  
  struct SeqIndexFactorLookup{
    int de;
    int factorCount;
    FactorElement *factorList;
  };
  
  struct SeqIndexDeInfo{
    int seqIndex;
    int de;
  };

  struct AssociationElement{
    int linIndex;
    int seqIndex;
    int factorCount;
    FactorElement *factorList;
  };
  
  // determine linIndex <-> seqIndex association for all cells in exclusive 
  // region on all localDEs on srcArray
  int srcRank = srcArray->rank;
  int *ii = new int[srcRank];     // index tuple basis 0
  int *iiEnd = new int[srcRank];
  AssociationElement **srcLinSeqList = 
    new AssociationElement*[srcLocalDeCount];
  int srcSeqIndexMinMax[2]; // [0]=min, [1]=max
  for (int i=0; i<srcLocalDeCount; i++){
    int de = srcLocalDeList[i];  // global DE index
    // allocate memory in srcLinSeqList for this DE
    srcLinSeqList[i] = new AssociationElement[srcDistGridLocalDeCellCount[i]];
    // only go into the multi-dim loop if there are cells for the local DE
    if (srcDistGridLocalDeCellCount[i]){
      // reset counters
      int joff = i*srcRank; // offset according to localDe index
      for (int j=0; j<srcRank; j++){
        ii[j] = 0;  // reset
        iiEnd[j] = srcArray->exclusiveUBound[joff+j]
          - srcArray->exclusiveLBound[joff+j] + 1;
      }
      // loop over all cells in exclusive region for this DE
      int cellIndex = 0;  // reset
      while(ii[srcRank-1] < iiEnd[srcRank-1]){
        //printf("src: DE = %d  - (", de);
        //int jjj;
        //for (jjj=0; jjj<srcRank-1; jjj++)
        //  printf("%d, ", ii[jjj]);
        //printf("%d)\n", ii[jjj]);
        // determine the lin. index for cell ii[] in localArray for this DE
        int linIndex = srcArray->getLinearIndexExclusive(i, ii);
        // determine the sequentialized index for cell ii[] in this DE
        // getSequenceIndex() expects basis 0 ii[] in excl. region
        int seqIndex = srcArray->distgrid->getSequenceIndex(i, ii);
        // store linIndex and seqIndex in srcLinSeqList for this DE
        srcLinSeqList[i][cellIndex].linIndex = linIndex;
        srcLinSeqList[i][cellIndex].seqIndex = seqIndex;
        // reset factorCount
        srcLinSeqList[i][cellIndex].factorCount = 0;
        // record seqIndex min and max
        if (i==0 && cellIndex==0)
          srcSeqIndexMinMax[0] = srcSeqIndexMinMax[1] = seqIndex; // initialize
        else{
          if (seqIndex < srcSeqIndexMinMax[0]) srcSeqIndexMinMax[0] = seqIndex;
          if (seqIndex > srcSeqIndexMinMax[1]) srcSeqIndexMinMax[1] = seqIndex;
        }
        // increment
        ++cellIndex;
        // multi-dim index increment
        ++ii[0];
        for (int j=0; j<srcRank-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = 0;  // reset
            ++ii[j+1];
          }
        }
      } // end while over all exclusive cells
    } // if there are cells in localArray associated with this local DE
  } // end for over local DEs
  delete [] ii;
  delete [] iiEnd;
  
  // communicate srcSeqIndexMinMax across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *srcSeqIndexMinMaxList = new int[2*petCount];
  vm->vmk_allgather(srcSeqIndexMinMax, srcSeqIndexMinMaxList, 2*sizeof(int));

#ifdef ASMMSTOREPRINT
  for (int i=0; i<srcLocalDeCount; i++)
    for (int j=0; j<srcDistGridLocalDeCellCount[i]; j++)
      printf("gjt: localPet %d, srcLinSeqList[%d][%d] = %d, %d\n", 
        localPet, i, j, srcLinSeqList[i][j].linIndex,
        srcLinSeqList[i][j].seqIndex);
#endif

  // determine linIndex <-> seqIndex association for all cells in exclusive 
  // region on all localDEs on dstArray
  int dstRank = dstArray->rank;
  ii = new int[dstRank];     // index tuple basis 0
  iiEnd = new int[dstRank];
  AssociationElement **dstLinSeqList = 
    new AssociationElement*[dstLocalDeCount];
  int dstSeqIndexMinMax[2]; // [0]=min, [1]=max
  for (int i=0; i<dstLocalDeCount; i++){
    int de = dstLocalDeList[i];  // global DE index
    // allocate memory in dstLinSeqList for this DE
    dstLinSeqList[i] = new AssociationElement[dstDistGridLocalDeCellCount[i]];
    // only go into the multi-dim loop if there are cells for the local DE
    if (dstDistGridLocalDeCellCount[i]){
      // reset counters
      int joff = i*dstRank; // offset according to localDe index
      for (int j=0; j<dstRank; j++){
        ii[j] = 0;  // reset
        iiEnd[j] = dstArray->exclusiveUBound[joff+j]
          - dstArray->exclusiveLBound[joff+j] + 1;
      }
      // loop over all cells in exclusive region for this DE
      int cellIndex = 0;  // reset
      while(ii[dstRank-1] < iiEnd[dstRank-1]){
        //printf("dst: DE = %d  - (", de);
        //int jjj;
        //for (jjj=0; jjj<dstRank-1; jjj++)
        //  printf("%d, ", ii[jjj]);
        //printf("%d)\n", ii[jjj]);
        // determine the lin. index for cell ii[] in localArray for this DE
        int linIndex = dstArray->getLinearIndexExclusive(i, ii);
        // determine the sequentialized index for cell ii[] in this DE
        // getSequenceIndex() expects basis 0 ii[] in excl. region
        int seqIndex = dstArray->distgrid->getSequenceIndex(i, ii);
        // store linIndex and seqIndex in dstLinSeqList for this DE
        dstLinSeqList[i][cellIndex].linIndex = linIndex;
        dstLinSeqList[i][cellIndex].seqIndex = seqIndex;
        // reset factorCount
        dstLinSeqList[i][cellIndex].factorCount = 0;
        // record seqIndex min and max
        if (i==0 && cellIndex==0)
          dstSeqIndexMinMax[0] = dstSeqIndexMinMax[1] = seqIndex; // initialize
        else{
          if (seqIndex < dstSeqIndexMinMax[0]) dstSeqIndexMinMax[0] = seqIndex;
          if (seqIndex > dstSeqIndexMinMax[1]) dstSeqIndexMinMax[1] = seqIndex;
        }
        // increment
        ++cellIndex;
        // multi-dim index increment
        ++ii[0];
        for (int j=0; j<dstRank-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = 0;  // reset
            ++ii[j+1];
          }
        }
      } // end while over all exclusive cells
    } // if there are cells in localArray associated with this local DE
  } // end for over local DEs
  delete [] ii;
  delete [] iiEnd;

  // communicate dstSeqIndexMinMax across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *dstSeqIndexMinMaxList = new int[2*petCount];
  vm->vmk_allgather(dstSeqIndexMinMax, dstSeqIndexMinMaxList, 2*sizeof(int));
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t3);   //gjt - profile
#endif

#ifdef ASMMSTOREPRINT
  for (int i=0; i<dstLocalDeCount; i++)
    for (int j=0; j<dstDistGridLocalDeCellCount[i]; j++)
      printf("gjt: localPet %d, dstLinSeqList[%d][%d] = %d, %d\n", 
        localPet, i, j, dstLinSeqList[i][j].linIndex,
        dstLinSeqList[i][j].seqIndex);
#endif

  // determine the srcSeqIndexMinGlobal and MaxGlobal
  // todo: for nb-allgather(srcSeqIndexMinMaxList) here insert commwait()
  int srcSeqIndexMinGlobal, srcSeqIndexMaxGlobal;
  int pastInitFlag = 0; // reset
  for (int i=0; i<petCount; i++){
    if (srcCellCountList[i]){
      // this Pet does hold cells in srcArray
      if (pastInitFlag){
        if (srcSeqIndexMinMaxList[2*i] < srcSeqIndexMinGlobal)
          srcSeqIndexMinGlobal = srcSeqIndexMinMaxList[2*i];
        if (srcSeqIndexMinMaxList[2*i+1] > srcSeqIndexMaxGlobal)
          srcSeqIndexMaxGlobal = srcSeqIndexMinMaxList[2*i+1];
      }else{
        // initialization
        srcSeqIndexMinGlobal = srcSeqIndexMinMaxList[2*i];
        srcSeqIndexMaxGlobal = srcSeqIndexMinMaxList[2*i+1];
        pastInitFlag = 1; // set
      }
    }
  }
    
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4a);   //gjt - profile
#endif
  
  // set up a distributed directory for srcArray seqIndex look-up
  int indicesPerPet = (srcSeqIndexMaxGlobal - srcSeqIndexMinGlobal + 1)
    / petCount;
  int extraIndices = (srcSeqIndexMaxGlobal - srcSeqIndexMinGlobal + 1)
    % petCount;
  Interval *srcSeqIndexInterval = new Interval[petCount];
  srcSeqIndexInterval[0].min = srcSeqIndexMinGlobal;  // start
  for (int i=0; i<petCount-1; i++){
    srcSeqIndexInterval[i].max = srcSeqIndexInterval[i].min + indicesPerPet - 1;
    if (i<extraIndices)
      ++srcSeqIndexInterval[i].max;   // distribute extra indices homogeneously
    srcSeqIndexInterval[i].count = 
      srcSeqIndexInterval[i].max - srcSeqIndexInterval[i].min + 1;
    srcSeqIndexInterval[i+1].min = srcSeqIndexInterval[i].max + 1;
  }
  srcSeqIndexInterval[petCount-1].max = srcSeqIndexMaxGlobal;  // finish
  srcSeqIndexInterval[petCount-1].count = 
    srcSeqIndexInterval[petCount-1].max - srcSeqIndexInterval[petCount-1].min
    + 1;
  
#ifdef ASMMSTOREPRINT
  printf("gjt: localPet %d, srcCellCountList[localPet] = %d, "
    "srcSeqIndexMinMax = %d / %d, srcSeqIndexMinGlobal/MaxGlobal = %d, %d, "
    "srcSeqIndexInterval[localPet].min/.max = %d, %d\n",
    localPet, srcCellCountList[localPet], srcSeqIndexMinMax[0],
    srcSeqIndexMinMax[1], srcSeqIndexMinGlobal, srcSeqIndexMaxGlobal,
    srcSeqIndexInterval[localPet].min, srcSeqIndexInterval[localPet].max);
#endif

  // allocate local look-up table indexed by srcSeqIndex
  SeqIndexFactorLookup *srcSeqIndexFactorLookup = 
    new SeqIndexFactorLookup[srcSeqIndexInterval[localPet].count];
  for (int i=0; i<srcSeqIndexInterval[localPet].count; i++){
    srcSeqIndexFactorLookup[i].de = 0; // use during initialization as counter
    srcSeqIndexFactorLookup[i].factorCount = 0; // reset
  }

  // set up srcSeqIntervFactorListCount and srcSeqIntervFactorListIndex
  int *srcSeqIntervFactorListCount = new int[petCount];
  int **srcSeqIntervFactorListIndex = new int*[petCount];
  for (int i=0; i<petCount; i++)
    srcSeqIntervFactorListCount[i] = 0; // reset
  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and count factor towards that PETs factor list count.
    int srcSeqIndex = factorIndexList->array[j*2];
    int iMin=0, iMax=petCount-1;
    int i=petCount/2;
    int foundFlag=0;  // reset
    do{
      if (srcSeqIndex < srcSeqIndexInterval[i].min){
        iMax = i;
        i = iMin + (iMax - iMin) / 2;
        continue; 
      }
      if (srcSeqIndex > srcSeqIndexInterval[i].max){
        iMin = i;
        i = iMin + 1 + (iMax - iMin) / 2;
        continue; 
      }
      // found interval
      ++srcSeqIntervFactorListCount[i]; // count this factor for this Pet
      foundFlag = 1;  // set
      break;
    }while (iMin != iMax);
    if (!foundFlag){
      // srcSeqIndex lies outside srcArray bounds
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- Found srcSeqIndex outside srcArray bounds", &rc);
      return rc;
    }
  }
  int *srcSeqIntervFactorCounter = new int[petCount];
  for (int i=0; i<petCount; i++){
    srcSeqIntervFactorListIndex[i] = new int[srcSeqIntervFactorListCount[i]];
    srcSeqIntervFactorCounter[i] = 0;  // reset
  }
  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and factor list index
    int srcSeqIndex = factorIndexList->array[j*2];
    int iMin=0, iMax=petCount-1;
    int i=petCount/2;
    do{
      if (srcSeqIndex < srcSeqIndexInterval[i].min){
        iMax = i;
        i = iMin + (iMax - iMin) / 2;
        continue; 
      }
      if (srcSeqIndex > srcSeqIndexInterval[i].max){
        iMin = i;
        i = iMin + 1 + (iMax - iMin) / 2;
        continue; 
      }
      // found interval
        int *k = &(srcSeqIntervFactorCounter[i]);
        srcSeqIntervFactorListIndex[i][(*k)++] = j; // store factorList index
      break;
    }while (iMin != iMax);
  }
  delete [] srcSeqIntervFactorCounter;
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4b);   //gjt - profile
#endif
  
  // all Pets construct their local srcSeqIndexFactorLookup[]
  int srcSeqIndexFactorCount = 0; // reset
  for (int factorPetIndex=0; factorPetIndex<factorPetCount; factorPetIndex++){
    // each Pet in factorPetList gets to be rootPet once and provide its factors
    int rootPet = factorPetList[factorPetIndex];
    if (localPet == rootPet){
      // rootPet
      for (int i=0; i<petCount; i++){
        int *thisPetFactorCountList = new int[srcSeqIndexInterval[i].count+1];
        // the extra integer value is used to store thisPetTotalFactorCount
        // to optimize communications
        for (int j=0; j<srcSeqIndexInterval[i].count+1; j++)
          thisPetFactorCountList[j] = 0; // reset
        if (i == rootPet){
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c1);   //gjt - profile
#endif
    
          // rootPet -> rootPet "communication"
          for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's srcSeqIndex interv
            int j = srcSeqIntervFactorListIndex[i][jj];
            int srcSeqIndex = factorIndexList->array[j*2];
            int k = srcSeqIndex - srcSeqIndexInterval[i].min;
            ++thisPetFactorCountList[k];  // count this factor
          }
          // prepare srcSeqIndexFactorLookup[]
          for (int j=0; j<srcSeqIndexInterval[i].count; j++){
            if (int factorCount = thisPetFactorCountList[j]){
              srcSeqIndexFactorCount += factorCount;  // add to the total count
              int prevFactorCount = srcSeqIndexFactorLookup[j].factorCount;
              // allocate new factorList
              FactorElement *factorList = 
                new FactorElement[prevFactorCount + factorCount];
              if (prevFactorCount){
                // copy previous factorList elements into new factorList
                memcpy(factorList, srcSeqIndexFactorLookup[j].factorList,
                  prevFactorCount * sizeof(FactorElement));
                // delete previous factorList
                delete [] srcSeqIndexFactorLookup[j].factorList;
              }
              // place new factorList into look-up table and set new count
              srcSeqIndexFactorLookup[j].factorList = factorList;
              srcSeqIndexFactorLookup[j].factorCount += factorCount;
            }
          }
          // fill srcSeqIndexFactorLookup[]
          if (typekindArg == ESMC_TYPEKIND_R4){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              int k = srcSeqIndex - srcSeqIndexInterval[i].min;
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              srcSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2+1]; // dstSeqIndex
              *((ESMC_R4 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_R8){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              int k = srcSeqIndex - srcSeqIndexInterval[i].min;
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              srcSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2+1]; // dstSeqIndex
              *((ESMC_R8 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("srcArray: %d, %d, rootPet-rootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, srcSeqIndex, factorIndexList->array[j*2+1], ((ESMC_R8 *)factorList)[j]);   
#endif              
            }
          }else if (typekindArg == ESMC_TYPEKIND_I4){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              int k = srcSeqIndex - srcSeqIndexInterval[i].min;
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              srcSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2+1]; // dstSeqIndex
              *((ESMC_I4 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_I8){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              int k = srcSeqIndex - srcSeqIndexInterval[i].min;
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              srcSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2+1]; // dstSeqIndex
              *((ESMC_I8 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I8 *)factorList)[j];
            }
          } // if - typekindArg
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c2);   //gjt - profile
#endif
            
        }else{
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c3);   //gjt - profile
  VMK::wtime(&t4c3a);   //gjt - profile
#endif
          // rootPet -> not rootPet communication
          int totalCountIndex = srcSeqIndexInterval[i].count;
          for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's srcSeqIndex interv
            int j = srcSeqIntervFactorListIndex[i][jj];
            int srcSeqIndex = factorIndexList->array[j*2];
            int k = srcSeqIndex - srcSeqIndexInterval[i].min;
            // count this factor
            ++thisPetFactorCountList[k];
            ++thisPetFactorCountList[totalCountIndex];
          }
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c3b);   //gjt - profile
#endif
    
          // send info to Pet "i"
          vm->vmk_send(thisPetFactorCountList, 
            (srcSeqIndexInterval[i].count + 1) * sizeof(int), i);
            
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c3c);   //gjt - profile
#endif
  
          // prepare to send remaining information to Pet "i" in one long stream
          int thisPetTotalFactorCount =
            thisPetFactorCountList[srcSeqIndexInterval[i].count];
          int byteCount = thisPetTotalFactorCount * (2*sizeof(int) + dataSize);
          char *stream = new char[byteCount];
          int *intStream;
          if (typekindArg == ESMC_TYPEKIND_R4){
            ESMC_R4 *factorStream = (ESMC_R4 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              intStream = (int *)factorStream;
              *intStream++ = srcSeqIndex;
              *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
              factorStream = (ESMC_R4 *)intStream;
              *factorStream++ = ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_R8){
            ESMC_R8 *factorStream = (ESMC_R8 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              intStream = (int *)factorStream;
              *intStream++ = srcSeqIndex;
              *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
              factorStream = (ESMC_R8 *)intStream;
              *factorStream++ = ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("srcArray: %d, %d, rootPet-NOTrootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, srcSeqIndex, factorIndexList->array[j*2+1], ((ESMC_R8 *)factorList)[j]);   
#endif              
            }
          }else if (typekindArg == ESMC_TYPEKIND_I4){
            ESMC_I4 *factorStream = (ESMC_I4 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              intStream = (int *)factorStream;
              *intStream++ = srcSeqIndex;
              *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
              factorStream = (ESMC_I4 *)intStream;
              *factorStream++ = ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_I8){
            ESMC_I8 *factorStream = (ESMC_I8 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex = factorIndexList->array[j*2];
              intStream = (int *)factorStream;
              *intStream++ = srcSeqIndex;
              *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
              factorStream = (ESMC_I8 *)intStream;
               *factorStream++ = ((ESMC_I8 *)factorList)[j];
            }
          }
          // ready to send information to Pet "i" in one long stream
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c3d);   //gjt - profile
  VMK::wtime(&t4c4);   //gjt - profile
#endif
  
          vm->vmk_send(stream, byteCount, i);
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c5);   //gjt - profile
#endif
  
#ifdef ASMMSTORETIMING
  dt4c34 += t4c4 - t4c3;
  dt4c45 += t4c5 - t4c4;
  dt4c3ab += t4c3b - t4c3a;
  dt4c3bc += t4c3c - t4c3b;
  dt4c3cd += t4c3d - t4c3c;
#endif
  
          // garbage collection
          delete [] stream;
        }
        delete [] thisPetFactorCountList;
      } // i - petCount
    }else{
      // localPet not rootPet
      // receive info from rootPet
      int *localPetFactorCountList =
        new int[srcSeqIndexInterval[localPet].count+1];
      // the extra integer value is used to store thisPetTotalFactorCount
      // to optimize communications
      vm->vmk_recv(localPetFactorCountList, 
        (srcSeqIndexInterval[localPet].count + 1) * sizeof(int), rootPet);
      int localPetTotalFactorCount =
        localPetFactorCountList[srcSeqIndexInterval[localPet].count];
      // process localPetFactorCountList and set srcSeqIndexFactorLookup[]
      for (int j=0; j<srcSeqIndexInterval[localPet].count; j++){
        if (int factorCount = localPetFactorCountList[j]){
          srcSeqIndexFactorCount += factorCount;  // add to the total count
          int prevFactorCount = srcSeqIndexFactorLookup[j].factorCount;
          // allocate new factorList
          FactorElement *factorList = 
            new FactorElement[prevFactorCount + factorCount];
          if (prevFactorCount){
            // copy previous factorList elements into new factorList
            memcpy(factorList, srcSeqIndexFactorLookup[j].factorList,
              prevFactorCount * sizeof(FactorElement));
            // delete previous factorList
            delete [] srcSeqIndexFactorLookup[j].factorList;
          }
          // place new factorList into look-up table and set new count
          srcSeqIndexFactorLookup[j].factorList = factorList;
          srcSeqIndexFactorLookup[j].factorCount += factorCount;
        }
      }
      delete [] localPetFactorCountList;
      // receive remaining information from rootPet in one long stream
      int byteCount = localPetTotalFactorCount * (2 * sizeof(int) + dataSize);
      char *stream = new char[byteCount];
      vm->vmk_recv(stream, byteCount, rootPet);
      // process stream and set srcSeqIndexFactorLookup[] content
      int *intStream;
      if (typekindArg == ESMC_TYPEKIND_R4){
        ESMC_R4 *factorStream = (ESMC_R4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - srcSeqIndexInterval[localPet].min; // srcSeqIndex
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_R4 *)intStream;
          *((ESMC_R4 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindArg == ESMC_TYPEKIND_R8){
        ESMC_R8 *factorStream = (ESMC_R8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - srcSeqIndexInterval[localPet].min; // srcSeqIndex
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_R8 *)intStream;
          *((ESMC_R8 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindArg == ESMC_TYPEKIND_I4){
        ESMC_I4 *factorStream = (ESMC_I4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - srcSeqIndexInterval[localPet].min; // srcSeqIndex
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_I4 *)intStream;
          *((ESMC_I4 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindArg == ESMC_TYPEKIND_I8){
        ESMC_I8 *factorStream = (ESMC_I8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - srcSeqIndexInterval[localPet].min; // srcSeqIndex
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_I8 *)intStream;
          *((ESMC_I8 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }
      // garbage collection
      delete [] stream;
    } // localPet not rootPet
  } // factorPetIndex
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4c);   //gjt - profile
#endif

  // communicate between Pets to set up "de" member in srcSeqIndexFactorLookup[]
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active srcSeqIndex interval
    int seqIndexMin = srcSeqIndexInterval[i].min;
    int seqIndexMax = srcSeqIndexInterval[i].max;
    int seqIndexCount = srcSeqIndexInterval[i].count;
    if (localPet!=i){
      // localPet is a client for the active srcSeqIndex interval
      SeqIndexDeInfo *seqIndexDeInfoList = new SeqIndexDeInfo[seqIndexCount];
      int count = 0; // reset
      for (int j=0; j<srcLocalDeCount; j++){
        int de = srcLocalDeList[j];  // global DE number
        for (int k=0; k<srcDistGridLocalDeCellCount[j]; k++){
          int srcSeqIndex = srcLinSeqList[j][k].seqIndex;
          if (srcSeqIndex >= seqIndexMin && srcSeqIndex <= seqIndexMax){
            seqIndexDeInfoList[count].seqIndex = srcSeqIndex;
            seqIndexDeInfoList[count].de = de;
            ++count; // increment counter
          }
        }
      }
      // send information to the serving Pet
      vm->vmk_send(&count, sizeof(int), i);
//printf("localPet %d sending count %d to Pet %i\n", localPet, count, i);
      if (count)
        vm->vmk_send(seqIndexDeInfoList, count*sizeof(SeqIndexDeInfo), i);
      // garbage collection
      delete [] seqIndexDeInfoList;
    }else{
      // localPet serves the active srcSeqIndex interval
      // todo: this can be rewritten with nb-recv to hide latencies
      for (int ii=0; ii<petCount; ii++){
        if (ii==i){
          // server Pet itself
          for (int j=0; j<srcLocalDeCount; j++){
            int de = srcLocalDeList[j];  // global DE number
            for (int k=0; k<srcDistGridLocalDeCellCount[j]; k++){
              int srcSeqIndex = srcLinSeqList[j][k].seqIndex;
              if (srcSeqIndex >= seqIndexMin && srcSeqIndex <= seqIndexMax){
                int kk = srcSeqIndex - seqIndexMin;
                srcSeqIndexFactorLookup[kk].de = de;
              }
            }
          }
        }else{
          // receive seqIndexWithinInterval from Pet "ii"
          int count;
          vm->vmk_recv(&count, sizeof(int), ii);
          SeqIndexDeInfo *seqIndexDeInfoList = new SeqIndexDeInfo[count];
//printf("localPet %d receiving count %d from Pet %i\n", localPet, count, ii);
          if (count)
            vm->vmk_recv(seqIndexDeInfoList, count*sizeof(SeqIndexDeInfo), ii);
          // process seqIndexDeInfoList and set srcSeqIndexFactorLookup[]
          for (int j=0; j<count; j++){
            int k = seqIndexDeInfoList[j].seqIndex - seqIndexMin;
            srcSeqIndexFactorLookup[k].de = seqIndexDeInfoList[j].de;
          }
          // garbage collection
          delete [] seqIndexDeInfoList;
        }
      } // for ii
    }
  }
      
#ifdef ASMMSTORETIMING
  printf("gjt - profile for PET %d:\n"
    " t4a=%g\n t4b=%g\n t4c1=%g\n t4c2=%g\n dt4c3ab=%g\n dt4c3bc=%g\n "
    "dt4c3cd=%g\n dt4c34=%g\n dt4c45=%g\n t4c=%g\n",
    localPet, t4a-t3, t4b-t3, t4c1-t3, t4c2-t3, dt4c3ab, dt4c3bc, dt4c3cd, 
    dt4c34, dt4c45, t4c-t3);
  VMK::wtime(&t4);   //gjt - profile
#endif

  // determine the dstSeqIndexMinGlobal and MaxGlobal
  // todo: for nb-allgather(dstSeqIndexMinMaxList) here insert commwait()
  int dstSeqIndexMinGlobal, dstSeqIndexMaxGlobal;
  pastInitFlag = 0; // reset
  for (int i=0; i<petCount; i++){
    if (dstCellCountList[i]){
      // this Pet does hold cells in dstArray
      if (pastInitFlag){
        if (dstSeqIndexMinMaxList[2*i] < dstSeqIndexMinGlobal)
          dstSeqIndexMinGlobal = dstSeqIndexMinMaxList[2*i];
        if (dstSeqIndexMinMaxList[2*i+1] > dstSeqIndexMaxGlobal)
          dstSeqIndexMaxGlobal = dstSeqIndexMinMaxList[2*i+1];
      }else{
        // initialization
        dstSeqIndexMinGlobal = dstSeqIndexMinMaxList[2*i];
        dstSeqIndexMaxGlobal = dstSeqIndexMinMaxList[2*i+1];
        pastInitFlag = 1; // set
      }
    }
  }
    
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5a);   //gjt - profile
#endif
  
  // set up a distributed directory for dstArray seqIndex look-up
  indicesPerPet = (dstSeqIndexMaxGlobal - dstSeqIndexMinGlobal + 1) / petCount;
  extraIndices = (dstSeqIndexMaxGlobal - dstSeqIndexMinGlobal + 1) % petCount;
  Interval *dstSeqIndexInterval = new Interval[petCount];
  dstSeqIndexInterval[0].min = dstSeqIndexMinGlobal;  // start
  for (int i=0; i<petCount-1; i++){
    dstSeqIndexInterval[i].max = dstSeqIndexInterval[i].min + indicesPerPet - 1;
    if (i<extraIndices)
      ++dstSeqIndexInterval[i].max;   // distribute extra indices homogeneously
    dstSeqIndexInterval[i].count = 
      dstSeqIndexInterval[i].max - dstSeqIndexInterval[i].min + 1;
    dstSeqIndexInterval[i+1].min = dstSeqIndexInterval[i].max + 1;
  }
  dstSeqIndexInterval[petCount-1].max = dstSeqIndexMaxGlobal;  // finish
  dstSeqIndexInterval[petCount-1].count = 
    dstSeqIndexInterval[petCount-1].max - dstSeqIndexInterval[petCount-1].min
    + 1;
  
#ifdef ASMMSTOREPRINT
    printf("gjt: localPet %d, dstCellCountList[localPet] = %d, "
    "dstSeqIndexMinMax = %d / %d, dstSeqIndexMinGlobal/MaxGlobal = %d, %d, "
    "dstSeqIndexInterval[localPet].min/.max = %d, %d\n",
    localPet, dstCellCountList[localPet], dstSeqIndexMinMax[0],
    dstSeqIndexMinMax[1], dstSeqIndexMinGlobal, dstSeqIndexMaxGlobal,
    dstSeqIndexInterval[localPet].min, dstSeqIndexInterval[localPet].max);
#endif

  // allocate local look-up table indexed by dstSeqIndex
  SeqIndexFactorLookup *dstSeqIndexFactorLookup = 
    new SeqIndexFactorLookup[dstSeqIndexInterval[localPet].count];
  for (int i=0; i<dstSeqIndexInterval[localPet].count; i++){
    dstSeqIndexFactorLookup[i].de = 0; // use during initialization as counter
    dstSeqIndexFactorLookup[i].factorCount = 0; // reset
  }

  // set up dstSeqIntervFactorListCount and dstSeqIntervFactorListIndex
  int *dstSeqIntervFactorListCount = new int[petCount];
  int **dstSeqIntervFactorListIndex = new int*[petCount];
  for (int i=0; i<petCount; i++)
    dstSeqIntervFactorListCount[i] = 0; // reset
  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and count factor towards that PETs factor list count.
    int dstSeqIndex = factorIndexList->array[j*2+1];
    int iMin=0, iMax=petCount-1;
    int i=petCount/2;
    int foundFlag=0;  // reset
    do{
      if (dstSeqIndex < dstSeqIndexInterval[i].min){
        iMax = i;
        i = iMin + (iMax - iMin) / 2;
        continue; 
      }
      if (dstSeqIndex > dstSeqIndexInterval[i].max){
        iMin = i;
        i = iMin + 1 + (iMax - iMin) / 2;
        continue; 
      }
      // found interval
      ++dstSeqIntervFactorListCount[i]; // count this factor for this Pet
      foundFlag = 1;  // set
      break;
    }while (iMin != iMax);
    if (!foundFlag){
      // dstSeqIndex lies outside dstArray bounds
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- Found dstSeqIndex outside dstArray bounds", &rc);
      return rc;
    }
  }
  int *dstSeqIntervFactorCounter = new int[petCount];
  for (int i=0; i<petCount; i++){
    dstSeqIntervFactorListIndex[i] = new int[dstSeqIntervFactorListCount[i]];
    dstSeqIntervFactorCounter[i] = 0;  // reset
  }
  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and factor list index
    int dstSeqIndex = factorIndexList->array[j*2+1];
    int iMin=0, iMax=petCount-1;
    int i=petCount/2;
    do{
      if (dstSeqIndex < dstSeqIndexInterval[i].min){
        iMax = i;
        i = iMin + (iMax - iMin) / 2;
        continue; 
      }
      if (dstSeqIndex > dstSeqIndexInterval[i].max){
        iMin = i;
        i = iMin + 1 + (iMax - iMin) / 2;
        continue; 
      }
      // found interval
      int *k = &(dstSeqIntervFactorCounter[i]);
      dstSeqIntervFactorListIndex[i][(*k)++] = j; // store factorList index
      break;
    }while (iMin != iMax);
  }
  delete [] dstSeqIntervFactorCounter;
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b);   //gjt - profile
#endif
  
  // all Pets construct their local dstSeqIndexFactorLookup[]
  int dstSeqIndexFactorCount = 0; // reset
  for (int factorPetIndex=0; factorPetIndex<factorPetCount; factorPetIndex++){
    // each Pet in factorPetList gets to be rootPet once and provide its factors
    int rootPet = factorPetList[factorPetIndex];
    if (localPet == rootPet){
      // rootPet
      for (int i=0; i<petCount; i++){
        int *thisPetFactorCountList = new int[dstSeqIndexInterval[i].count+1];
        // the extra integer value is used to store thisPetTotalFactorCount
        // to optimize communications
        for (int j=0; j<dstSeqIndexInterval[i].count+1; j++)
          thisPetFactorCountList[j] = 0; // reset
        if (i == rootPet){
          // rootPet -> rootPet "communication"
          for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's dstSeqIndex interv
            int j = dstSeqIntervFactorListIndex[i][jj];
            int dstSeqIndex = factorIndexList->array[j*2+1];
            int k = dstSeqIndex - dstSeqIndexInterval[i].min;
            ++thisPetFactorCountList[k];  // count this factor
          }
          // prepare dstSeqIndexFactorLookup[]
          for (int j=0; j<dstSeqIndexInterval[i].count; j++){
            if (int factorCount = thisPetFactorCountList[j]){
              dstSeqIndexFactorCount += factorCount;  // add to the total count
              int prevFactorCount = dstSeqIndexFactorLookup[j].factorCount;
              // allocate new factorList
              FactorElement *factorList = 
                new FactorElement[prevFactorCount + factorCount];
              if (prevFactorCount){
                // copy previous factorList elements into new factorList
                memcpy(factorList, dstSeqIndexFactorLookup[j].factorList,
                  prevFactorCount * sizeof(FactorElement));
                // delete previous factorList
                delete [] dstSeqIndexFactorLookup[j].factorList;
              }
              // place new factorList into look-up table and set new count
              dstSeqIndexFactorLookup[j].factorList = factorList;
              dstSeqIndexFactorLookup[j].factorCount += factorCount;
            }
          }
          // fill dstSeqIndexFactorLookup[]
          if (typekindArg == ESMC_TYPEKIND_R4){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              int k = dstSeqIndex - dstSeqIndexInterval[i].min;
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              dstSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2]; // srcSeqIndex
              *((ESMC_R4 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_R8){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              int k = dstSeqIndex - dstSeqIndexInterval[i].min;
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              dstSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2]; // srcSeqIndex
              *((ESMC_R8 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("dstArray: %d, %d, rootPet-rootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, dstSeqIndex, factorIndexList->array[j*2], ((ESMC_R8 *)factorList)[j]);
#endif        
            }
          }else if (typekindArg == ESMC_TYPEKIND_I4){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              int k = dstSeqIndex - dstSeqIndexInterval[i].min;
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              dstSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2]; // srcSeqIndex
              *((ESMC_I4 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_I8){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              int k = dstSeqIndex - dstSeqIndexInterval[i].min;
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              dstSeqIndexFactorLookup[k].factorList[kk].partnerSeqIndex =
                factorIndexList->array[j*2]; // srcSeqIndex
              *((ESMC_I8 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I8 *)factorList)[j];
            }
          } // if - typekindArg
        }else{
          // rootPet -> not rootPet communication
          int totalCountIndex = dstSeqIndexInterval[i].count;
          for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's dstSeqIndex interv
            int j = dstSeqIntervFactorListIndex[i][jj];
            int dstSeqIndex = factorIndexList->array[j*2+1];
            int k = dstSeqIndex - dstSeqIndexInterval[i].min;
            // count this factor
            ++thisPetFactorCountList[k];
            ++thisPetFactorCountList[totalCountIndex];
          }
          // send info to Pet "i"
          vm->vmk_send(thisPetFactorCountList, 
            (dstSeqIndexInterval[i].count + 1) * sizeof(int), i);
          // prepare to send remaining information to Pet "i" in one long stream
          int thisPetTotalFactorCount =
            thisPetFactorCountList[dstSeqIndexInterval[i].count];
          int byteCount = thisPetTotalFactorCount * (2*sizeof(int) + dataSize);
          char *stream = new char[byteCount];
          int *intStream;
          if (typekindArg == ESMC_TYPEKIND_R4){
            ESMC_R4 *factorStream = (ESMC_R4 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              intStream = (int *)factorStream;
              *intStream++ = dstSeqIndex;
              *intStream++ = factorIndexList->array[j*2]; // srcSeqIndex
              factorStream = (ESMC_R4 *)intStream;
              *factorStream++ = ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_R8){
            ESMC_R8 *factorStream = (ESMC_R8 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              intStream = (int *)factorStream;
              *intStream++ = dstSeqIndex;
              *intStream++ = factorIndexList->array[j*2]; // srcSeqIndex
              factorStream = (ESMC_R8 *)intStream;
              *factorStream++ = ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("dstArray: %d, %d, rootPet-NOTrootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, dstSeqIndex, factorIndexList->array[j*2], ((ESMC_R8 *)factorList)[j]);
#endif
            }
          }else if (typekindArg == ESMC_TYPEKIND_I4){
            ESMC_I4 *factorStream = (ESMC_I4 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              intStream = (int *)factorStream;
              *intStream++ = dstSeqIndex;
              *intStream++ = factorIndexList->array[j*2]; // srcSeqIndex
              factorStream = (ESMC_I4 *)intStream;
              *factorStream++ = ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindArg == ESMC_TYPEKIND_I8){
            ESMC_I8 *factorStream = (ESMC_I8 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex = factorIndexList->array[j*2+1];
              intStream = (int *)factorStream;
              *intStream++ = dstSeqIndex;
              *intStream++ = factorIndexList->array[j*2]; // srcSeqIndex
              factorStream = (ESMC_I8 *)intStream;
              *factorStream++ = ((ESMC_I8 *)factorList)[j];
            }
          }
          // ready to send information to Pet "i" in one long stream
          vm->vmk_send(stream, byteCount, i);
          // garbage collection
          delete [] stream;
        }
        delete [] thisPetFactorCountList;
      } // i - petCount
    }else{
      // localPet not rootPet
      // receive info from rootPet
      int *localPetFactorCountList =
        new int[dstSeqIndexInterval[localPet].count+1];
      // the extra integer value is used to store thisPetTotalFactorCount
      // to optimize communications
      vm->vmk_recv(localPetFactorCountList, 
        (dstSeqIndexInterval[localPet].count + 1) * sizeof(int), rootPet);
      int localPetTotalFactorCount =
        localPetFactorCountList[dstSeqIndexInterval[localPet].count];
      // process localPetFactorCountList and set dstSeqIndexFactorLookup[]
      for (int j=0; j<dstSeqIndexInterval[localPet].count; j++){
        if (int factorCount = localPetFactorCountList[j]){
          dstSeqIndexFactorCount += factorCount;  // add to the total count
          int prevFactorCount = dstSeqIndexFactorLookup[j].factorCount;
          // allocate new factorList
          FactorElement *factorList = 
            new FactorElement[prevFactorCount + factorCount];
          if (prevFactorCount){
            // copy previous factorList elements into new factorList
            memcpy(factorList, dstSeqIndexFactorLookup[j].factorList,
              prevFactorCount * sizeof(FactorElement));
            // delete previous factorList
            delete [] dstSeqIndexFactorLookup[j].factorList;
          }
          // place new factorList into look-up table and set new count
          dstSeqIndexFactorLookup[j].factorList = factorList;
          dstSeqIndexFactorLookup[j].factorCount += factorCount;
        }
      }
      delete [] localPetFactorCountList;
      // receive remaining information from rootPet in one long stream
      int byteCount = localPetTotalFactorCount * (2 * sizeof(int) + dataSize);
      char *stream = new char[byteCount];
      vm->vmk_recv(stream, byteCount, rootPet);
      // process stream and set dstSeqIndexFactorLookup[] content
      int *intStream;
      if (typekindArg == ESMC_TYPEKIND_R4){
        ESMC_R4 *factorStream = (ESMC_R4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - dstSeqIndexInterval[localPet].min; // dstSeqIndex
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_R4 *)intStream;
          *((ESMC_R4 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindArg == ESMC_TYPEKIND_R8){
        ESMC_R8 *factorStream = (ESMC_R8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - dstSeqIndexInterval[localPet].min; // dstSeqIndex
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_R8 *)intStream;
          *((ESMC_R8 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindArg == ESMC_TYPEKIND_I4){
        ESMC_I4 *factorStream = (ESMC_I4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - dstSeqIndexInterval[localPet].min; // dstSeqIndex
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_I4 *)intStream;
          *((ESMC_I4 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindArg == ESMC_TYPEKIND_I8){
        ESMC_I8 *factorStream = (ESMC_I8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++ - dstSeqIndexInterval[localPet].min; // dstSeqIndex
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex =
            *intStream++; // dstSeqIndex
          factorStream = (ESMC_I8 *)intStream;
          *((ESMC_I8 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }
      // garbage collection
      delete [] stream;
    } // localPet not rootPet
  } // factorPetIndex
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5c);   //gjt - profile
#endif
  
  // communicate between Pets to set up "de" member in dstSeqIndexFactorLookup[]
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active dstSeqIndex interval
    int seqIndexMin = dstSeqIndexInterval[i].min;
    int seqIndexMax = dstSeqIndexInterval[i].max;
    int seqIndexCount = dstSeqIndexInterval[i].count;
    if (localPet!=i){
      // localPet is a client for the active dstSeqIndex interval
      SeqIndexDeInfo *seqIndexDeInfoList = new SeqIndexDeInfo[seqIndexCount];
      int count = 0; // reset
      for (int j=0; j<dstLocalDeCount; j++){
        int de = dstLocalDeList[j];  // global DE number
        for (int k=0; k<dstDistGridLocalDeCellCount[j]; k++){
          int dstSeqIndex = dstLinSeqList[j][k].seqIndex;
          if (dstSeqIndex >= seqIndexMin && dstSeqIndex <= seqIndexMax){
            seqIndexDeInfoList[count].seqIndex = dstSeqIndex;
            seqIndexDeInfoList[count].de = de;
            ++count; // increment counter
          }
        }
      }      
      // send information to the serving Pet
      vm->vmk_send(&count, sizeof(int), i);
//printf("localPet %d sending count %d to Pet %i\n", localPet, count, i);
      if (count)
        vm->vmk_send(seqIndexDeInfoList, count*sizeof(SeqIndexDeInfo), i);
      // garbage collection
      delete [] seqIndexDeInfoList;
    }else{
      // localPet serves the active dstSeqIndex interval
      // todo: this can be rewritten with nb-recv to hide latencies
      for (int ii=0; ii<petCount; ii++){
        if (ii==i){
          // server Pet itself
          for (int j=0; j<dstLocalDeCount; j++){
            int de = dstLocalDeList[j];  // global DE number
            for (int k=0; k<dstDistGridLocalDeCellCount[j]; k++){
              int dstSeqIndex = dstLinSeqList[j][k].seqIndex;
              if (dstSeqIndex >= seqIndexMin && dstSeqIndex <= seqIndexMax){
                int kk = dstSeqIndex - seqIndexMin;
                dstSeqIndexFactorLookup[kk].de = de;
              }
            }
          }
        }else{
          // receive seqIndexWithinInterval from Pet "ii"
          int count;
          vm->vmk_recv(&count, sizeof(int), ii);
          SeqIndexDeInfo *seqIndexDeInfoList = new SeqIndexDeInfo[count];
//printf("localPet %d receiving count %d from Pet %i\n", localPet, count, ii);
          if (count)
            vm->vmk_recv(seqIndexDeInfoList, count*sizeof(SeqIndexDeInfo), ii);
          // process seqIndexDeInfoList and set dstSeqIndexFactorLookup[]
          for (int j=0; j<count; j++){
            int k = seqIndexDeInfoList[j].seqIndex - seqIndexMin;
            dstSeqIndexFactorLookup[k].de = seqIndexDeInfoList[j].de;
          }
          // garbage collection
          delete [] seqIndexDeInfoList;
        }
      } // for ii
    }
  }
    
#ifdef ASMMSTORETIMING
  printf("gjt - profile for PET %d:\n"
    " t5a=%g\n t5b=%g\n t5c=%g\n", localPet, t5a-t4, t5b-t4, t5c-t4);
  VMK::wtime(&t5);   //gjt - profile
#endif

  // fill in the partnerDe member in srcSeqIndexFactorLookup using dst distdir
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active dstSeqIndex interval
    int seqIndexMin = dstSeqIndexInterval[i].min;
    int seqIndexMax = dstSeqIndexInterval[i].max;
    int seqIndexCount = dstSeqIndexInterval[i].count;
    if (localPet!=i){
      // localPet is a client for the active dstSeqIndex interval
      // construct a request from the srcSeqIndexFactorLookup for this interval
      int *requestDstSeqIndexList = new int[srcSeqIndexFactorCount];
      int *srcSeqIndexFactorLookupIndex = new int[2*srcSeqIndexFactorCount];
      int count = 0; // reset
      for (int j=0; j<srcSeqIndexInterval[localPet].count; j++)
        for (int k=0; k<srcSeqIndexFactorLookup[j].factorCount; k++){
          int partnerSeqIndex =
            srcSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex;
          if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax){
            srcSeqIndexFactorLookupIndex[2*count] = j;
            srcSeqIndexFactorLookupIndex[2*count+1] = k;
            requestDstSeqIndexList[count] = partnerSeqIndex;
            ++count;
          }
        }
      // send request to the serving Pet "i"
      vm->vmk_send(&count, sizeof(int), i);
      if (count)
        vm->vmk_send(requestDstSeqIndexList, count*sizeof(int), i);
      // receive response from the serving Pet "i"
      int *responseDstDeList = new int[count];
      if (count)
        vm->vmk_recv(responseDstDeList, count*sizeof(int), i);
      // process responseDstDeList and set partnerDe member
      for (int j=0; j<count; j++){
        int jj =  srcSeqIndexFactorLookupIndex[2*j];
        int k = srcSeqIndexFactorLookupIndex[2*j+1];
        srcSeqIndexFactorLookup[jj].factorList[k].partnerDe =
          responseDstDeList[j];
      }
      // garbage collection
      delete [] requestDstSeqIndexList;
      delete [] responseDstDeList;
      delete [] srcSeqIndexFactorLookupIndex;
    }else{
      // localPet serves the active dstSeqIndex interval
      // todo: this can be rewritten with nb-recv to hide latencies
      for (int ii=0; ii<petCount; ii++){
        if (ii==i){
          // server Pet itself
          for (int j=0; j<srcSeqIndexInterval[localPet].count; j++)
            for (int k=0; k<srcSeqIndexFactorLookup[j].factorCount; k++){
              int partnerSeqIndex = 
                srcSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex;
              if (partnerSeqIndex >= seqIndexMin &&
                partnerSeqIndex <= seqIndexMax){
                int kk = partnerSeqIndex - seqIndexMin;
                srcSeqIndexFactorLookup[j].factorList[k].partnerDe =
                  dstSeqIndexFactorLookup[kk].de;
              }
            }
        }else{
          // receive requestDstSeqIndexList from Pet "ii"
          int count;
          vm->vmk_recv(&count, sizeof(int), ii);
          int *requestDstSeqIndexList = new int[count];
          if (count)
            vm->vmk_recv(requestDstSeqIndexList, count*sizeof(int), ii);
          // process requestDstSeqIndexList and set up response
          int *responseDstDeList = new int[count];
          for (int j=0; j<count; j++){
            int k = requestDstSeqIndexList[j] - seqIndexMin;
            responseDstDeList[j] = dstSeqIndexFactorLookup[k].de;
          }
          // send response back to Pet "ii"
          if (count)
            vm->vmk_send(responseDstDeList, count*sizeof(int), ii);
          // garbage collection
          delete [] requestDstSeqIndexList;
          delete [] responseDstDeList;
        }
      } // for ii
    }
  }
  
  // fill in the partnerDe member in dstSeqIndexFactorLookup using src distdir
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active srcSeqIndex interval
    int seqIndexMin = srcSeqIndexInterval[i].min;
    int seqIndexMax = srcSeqIndexInterval[i].max;
    int seqIndexCount = srcSeqIndexInterval[i].count;
    if (localPet!=i){
      // localPet is a client for the active srcSeqIndex interval
      // construct a request from the dstSeqIndexFactorLookup for this interval
      int *requestSrcSeqIndexList = new int[dstSeqIndexFactorCount];
      int *dstSeqIndexFactorLookupIndex = new int[2*dstSeqIndexFactorCount];
      int count = 0; // reset
      for (int j=0; j<dstSeqIndexInterval[localPet].count; j++)
        for (int k=0; k<dstSeqIndexFactorLookup[j].factorCount; k++){
          int partnerSeqIndex =
            dstSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex;
          if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax){
            dstSeqIndexFactorLookupIndex[2*count] = j;
            dstSeqIndexFactorLookupIndex[2*count+1] = k;
            requestSrcSeqIndexList[count] = partnerSeqIndex;
            ++count;
          }
        }
      // send request to the serving Pet "i"
      vm->vmk_send(&count, sizeof(int), i);
      if (count)
        vm->vmk_send(requestSrcSeqIndexList, count*sizeof(int), i);
      // receive response from the serving Pet "i"
      int *responseSrcDeList = new int[count];
      if (count)
        vm->vmk_recv(responseSrcDeList, count*sizeof(int), i);
      // process responseSrcDeList and set partnerDe member
      for (int j=0; j<count; j++){
        int jj =  dstSeqIndexFactorLookupIndex[2*j];
        int k = dstSeqIndexFactorLookupIndex[2*j+1];
        dstSeqIndexFactorLookup[jj].factorList[k].partnerDe =
          responseSrcDeList[j];
      }
      // garbage collection
      delete [] requestSrcSeqIndexList;
      delete [] responseSrcDeList;
      delete [] dstSeqIndexFactorLookupIndex;
    }else{
      // localPet serves the active srcSeqIndex interval
      // todo: this can be rewritten with nb-recv to hide latencies
      for (int ii=0; ii<petCount; ii++){
        if (ii==i){
          // server Pet itself
          for (int j=0; j<dstSeqIndexInterval[localPet].count; j++)
            for (int k=0; k<dstSeqIndexFactorLookup[j].factorCount; k++){
              int partnerSeqIndex = 
                dstSeqIndexFactorLookup[j].factorList[k].partnerSeqIndex;
              if (partnerSeqIndex >= seqIndexMin &&
                partnerSeqIndex <= seqIndexMax){
                int kk = partnerSeqIndex - seqIndexMin;
                dstSeqIndexFactorLookup[j].factorList[k].partnerDe =
                  srcSeqIndexFactorLookup[kk].de;
              }
            }
        }else{
          // receive requestSrcSeqIndexList from Pet "ii"
          int count;
          vm->vmk_recv(&count, sizeof(int), ii);
          int *requestSrcSeqIndexList = new int[count];
          if (count)
            vm->vmk_recv(requestSrcSeqIndexList, count*sizeof(int), ii);
          // process requestSrcSeqIndexList and set up response
          int *responseSrcDeList = new int[count];
          for (int j=0; j<count; j++){
            int k = requestSrcSeqIndexList[j] - seqIndexMin;
            responseSrcDeList[j] = srcSeqIndexFactorLookup[k].de;
          }
          // send response back to Pet "ii"
          if (count)
            vm->vmk_send(responseSrcDeList, count*sizeof(int), ii);
          // garbage collection
          delete [] requestSrcSeqIndexList;
          delete [] responseSrcDeList;
        }
      } // for ii
    }
  }
  
#ifdef ASMMSTOREPRINT
  // some serious printing for src info
  for (int i=0; i<srcSeqIndexInterval[localPet].count; i++){
    printf("gjt srcDistDir: localPet %d, srcSeqIndex = %d, "
      "srcSeqIndexFactorLookup[%d].factorCount = %d, .de = %d\n",
      localPet, i+srcSeqIndexInterval[localPet].min, i,
      srcSeqIndexFactorLookup[i].factorCount, 
      srcSeqIndexFactorLookup[i].de);
    for (int j=0; j<srcSeqIndexFactorLookup[i].factorCount; j++)
      printf("gjt srcDistDir: localPet %d, "
        "srcSeqIndexFactorLookup[%d].factorList[%d].partnerSeqIndex = %d, "
        ".partnerDe = %d, .factor = %g\n",
        localPet, i, j,
        srcSeqIndexFactorLookup[i].factorList[j].partnerSeqIndex,
        srcSeqIndexFactorLookup[i].factorList[j].partnerDe,
        *((double *)srcSeqIndexFactorLookup[i].factorList[j].factor));
  }
  // some serious printing for dst info
  for (int i=0; i<dstSeqIndexInterval[localPet].count; i++){
    printf("gjt dstDistDir: localPet %d, dstSeqIndex = %d, "
      "dstSeqIndexFactorLookup[%d].factorCount = %d, .de = %d\n",
      localPet, i+dstSeqIndexInterval[localPet].min, i,
      dstSeqIndexFactorLookup[i].factorCount, 
      dstSeqIndexFactorLookup[i].de);
    for (int j=0; j<dstSeqIndexFactorLookup[i].factorCount; j++)
      printf("gjt dstDistDir: localPet %d, "
        "dstSeqIndexFactorLookup[%d].factorList[%d].partnerSeqIndex = %d, "
        ".partnerDe = %d, .factor = %g\n",
        localPet, i, j,
        dstSeqIndexFactorLookup[i].factorList[j].partnerSeqIndex,
        dstSeqIndexFactorLookup[i].factorList[j].partnerDe,
        *((double *)dstSeqIndexFactorLookup[i].factorList[j].factor));
  }
#endif
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t6);   //gjt - profile
#endif
  
  struct SeqIndexIndexInfo{
    int seqIndex;
    int listIndex1;
    int listIndex2;
  };
  
  // communicate between Pets to obtain complete srcArray info on localPet
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active srcSeqIndex interval
    int seqIndexMin = srcSeqIndexInterval[i].min;
    int seqIndexMax = srcSeqIndexInterval[i].max;
    int seqIndexCount = srcSeqIndexInterval[i].count;
    if (localPet!=i){
      // localPet is a client for the active srcSeqIndex interval
      SeqIndexIndexInfo *seqIndexIndexInfoList =
        new SeqIndexIndexInfo[seqIndexCount];
      int count = 0; // reset
      for (int j=0; j<srcLocalDeCount; j++){
        for (int k=0; k<srcDistGridLocalDeCellCount[j]; k++){
          int srcSeqIndex = srcLinSeqList[j][k].seqIndex;
          if (srcSeqIndex >= seqIndexMin && srcSeqIndex <= seqIndexMax){
            seqIndexIndexInfoList[count].seqIndex = srcSeqIndex;
            seqIndexIndexInfoList[count].listIndex1 = j;
            seqIndexIndexInfoList[count].listIndex2 = k;
            ++count; // increment counter
          }
        }
      }
      // send information to the serving Pet
      vm->vmk_send(&count, sizeof(int), i);
      if (count)
        vm->vmk_send(seqIndexIndexInfoList, count*sizeof(SeqIndexIndexInfo), i);
      // receive response from the serving Pet "i"
      int responseStreamSize;
      vm->vmk_recv(&responseStreamSize, sizeof(int), i);
      char *responseStream = new char[responseStreamSize];
      if (responseStreamSize)
        vm->vmk_recv(responseStream, responseStreamSize, i);
      // process responseStream and complete srcLinSeqList[][] info
      if (responseStreamSize){
        int *responseStreamInt;
        FactorElement *responseStreamFactorElement =
          (FactorElement *)responseStream;
        while ((char *)responseStreamFactorElement !=
          responseStream+responseStreamSize){
          responseStreamInt = (int *)responseStreamFactorElement;
          int j = *responseStreamInt++;
          int k = *responseStreamInt++;
          int factorCount = *responseStreamInt++;
          srcLinSeqList[j][k].factorCount = factorCount;
          srcLinSeqList[j][k].factorList = new FactorElement[factorCount];
          responseStreamFactorElement = (FactorElement *)responseStreamInt;
          memcpy(srcLinSeqList[j][k].factorList, responseStreamFactorElement,
            factorCount * sizeof(FactorElement));
          responseStreamFactorElement += factorCount;
        }
      }
      // garbage collection
      delete [] seqIndexIndexInfoList;
      delete [] responseStream;
    }else{
      // localPet serves the active srcSeqIndex interval
      // todo: this can be rewritten with nb-recv to hide latencies
      for (int ii=0; ii<petCount; ii++){
        if (ii==i){
          // server Pet itself
          for (int j=0; j<srcLocalDeCount; j++){
            for (int k=0; k<srcDistGridLocalDeCellCount[j]; k++){
              int srcSeqIndex = srcLinSeqList[j][k].seqIndex;
              if (srcSeqIndex >= seqIndexMin && srcSeqIndex <= seqIndexMax){
                int kk = srcSeqIndex - seqIndexMin;
                int factorCount = srcSeqIndexFactorLookup[kk].factorCount;
                if (factorCount){
                  srcLinSeqList[j][k].factorCount = factorCount;
                  srcLinSeqList[j][k].factorList =
                    new FactorElement[factorCount];
                  memcpy(srcLinSeqList[j][k].factorList,
                    srcSeqIndexFactorLookup[kk].factorList,
                    factorCount * sizeof(FactorElement));
                }
              }
            }
          }
        }else{
          // receive seqIndexWithinInterval from Pet "ii"
          int count;
          vm->vmk_recv(&count, sizeof(int), ii);
          SeqIndexIndexInfo *seqIndexIndexInfoList =
            new SeqIndexIndexInfo[count];
          if (count)
            vm->vmk_recv(seqIndexIndexInfoList, count*sizeof(SeqIndexIndexInfo),
              ii);
          // process seqIndexIndexInfoList and set up response
          int indexCounter = 0; // reset
          int factorElementCounter = 0; // reset
          for (int j=0; j<count; j++){
            int k = seqIndexIndexInfoList[j].seqIndex - seqIndexMin;
            int factorCount = srcSeqIndexFactorLookup[k].factorCount;
            if (factorCount){
              ++indexCounter;
              factorElementCounter += factorCount;
            }
          }
          int responseStreamSize = 3*indexCounter*sizeof(int)
            + factorElementCounter*sizeof(FactorElement);
          char *responseStream = new char[responseStreamSize];
          int *responseStreamInt;
          FactorElement *responseStreamFactorElement =
            (FactorElement *)responseStream;
          for (int j=0; j<count; j++){
            int k = seqIndexIndexInfoList[j].seqIndex - seqIndexMin;
            int factorCount = srcSeqIndexFactorLookup[k].factorCount;
            if (factorCount){
              responseStreamInt = (int *)responseStreamFactorElement;
              *responseStreamInt++ = seqIndexIndexInfoList[j].listIndex1;
              *responseStreamInt++ = seqIndexIndexInfoList[j].listIndex2;
              *responseStreamInt++ = factorCount;
              responseStreamFactorElement = (FactorElement *)responseStreamInt;
              memcpy(responseStreamFactorElement,
                srcSeqIndexFactorLookup[k].factorList,
                factorCount * sizeof(FactorElement));
              responseStreamFactorElement += factorCount;
            }
          }
          // send response back to Pet "ii"
          vm->vmk_send(&responseStreamSize, sizeof(int), ii);
          if (responseStreamSize)
            vm->vmk_send(responseStream, responseStreamSize, ii);
          // garbage collection
          delete [] seqIndexIndexInfoList;
          delete [] responseStream;
        }
      } // for ii
    }
  }
  // communicate between Pets to obtain complete dstArray info on localPet
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active dstSeqIndex interval
    int seqIndexMin = dstSeqIndexInterval[i].min;
    int seqIndexMax = dstSeqIndexInterval[i].max;
    int seqIndexCount = dstSeqIndexInterval[i].count;
    if (localPet!=i){
      // localPet is a client for the active dstSeqIndex interval
      SeqIndexIndexInfo *seqIndexIndexInfoList =
        new SeqIndexIndexInfo[seqIndexCount];
      int count = 0; // reset
      for (int j=0; j<dstLocalDeCount; j++){
        for (int k=0; k<dstDistGridLocalDeCellCount[j]; k++){
          int dstSeqIndex = dstLinSeqList[j][k].seqIndex;
          if (dstSeqIndex >= seqIndexMin && dstSeqIndex <= seqIndexMax){
            seqIndexIndexInfoList[count].seqIndex = dstSeqIndex;
            seqIndexIndexInfoList[count].listIndex1 = j;
            seqIndexIndexInfoList[count].listIndex2 = k;
            ++count; // increment counter
          }
        }
      }
      // send information to the serving Pet
      vm->vmk_send(&count, sizeof(int), i);
      if (count)
        vm->vmk_send(seqIndexIndexInfoList, count*sizeof(SeqIndexIndexInfo), i);
      // receive response from the serving Pet "i"
      int responseStreamSize;
      vm->vmk_recv(&responseStreamSize, sizeof(int), i);
      char *responseStream = new char[responseStreamSize];
      if (responseStreamSize)
        vm->vmk_recv(responseStream, responseStreamSize, i);
      // process responseStream and complete dstLinSeqList[][] info
      if (responseStreamSize){
        int *responseStreamInt;
        FactorElement *responseStreamFactorElement =
          (FactorElement *)responseStream;
        while ((char *)responseStreamFactorElement !=
          responseStream+responseStreamSize){
          responseStreamInt = (int *)responseStreamFactorElement;
          int j = *responseStreamInt++;
          int k = *responseStreamInt++;
          int factorCount = *responseStreamInt++;
          dstLinSeqList[j][k].factorCount = factorCount;
          dstLinSeqList[j][k].factorList = new FactorElement[factorCount];
          responseStreamFactorElement = (FactorElement *)responseStreamInt;
          memcpy(dstLinSeqList[j][k].factorList, responseStreamFactorElement,
            factorCount * sizeof(FactorElement));
          responseStreamFactorElement += factorCount;
        }
      }
      // garbage collection
      delete [] seqIndexIndexInfoList;
      delete [] responseStream;
    }else{
      // localPet serves the active dstSeqIndex interval
      // todo: this can be rewritten with nb-recv to hide latencies
      for (int ii=0; ii<petCount; ii++){
        if (ii==i){
          // server Pet itself
          for (int j=0; j<dstLocalDeCount; j++){
            for (int k=0; k<dstDistGridLocalDeCellCount[j]; k++){
              int dstSeqIndex = dstLinSeqList[j][k].seqIndex;
              if (dstSeqIndex >= seqIndexMin && dstSeqIndex <= seqIndexMax){
                int kk = dstSeqIndex - seqIndexMin;
                int factorCount = dstSeqIndexFactorLookup[kk].factorCount;
                if (factorCount){
                  dstLinSeqList[j][k].factorCount = factorCount;
                  dstLinSeqList[j][k].factorList =
                    new FactorElement[factorCount];
                  memcpy(dstLinSeqList[j][k].factorList,
                    dstSeqIndexFactorLookup[kk].factorList,
                    factorCount * sizeof(FactorElement));
                }
              }
            }
          }
        }else{
          // receive seqIndexWithinInterval from Pet "ii"
          int count;
          vm->vmk_recv(&count, sizeof(int), ii);
          SeqIndexIndexInfo *seqIndexIndexInfoList =
            new SeqIndexIndexInfo[count];
          if (count)
            vm->vmk_recv(seqIndexIndexInfoList, count*sizeof(SeqIndexIndexInfo),
              ii);
          // process seqIndexIndexInfoList and set up response
          int indexCounter = 0; // reset
          int factorElementCounter = 0; // reset
          for (int j=0; j<count; j++){
            int k = seqIndexIndexInfoList[j].seqIndex - seqIndexMin;
            int factorCount = dstSeqIndexFactorLookup[k].factorCount;
            if (factorCount){
              ++indexCounter;
              factorElementCounter += factorCount;
            }
          }
          int responseStreamSize = 3*indexCounter*sizeof(int)
            + factorElementCounter*sizeof(FactorElement);
          char *responseStream = new char[responseStreamSize];
          int *responseStreamInt;
          FactorElement *responseStreamFactorElement =
            (FactorElement *)responseStream;
          for (int j=0; j<count; j++){
            int k = seqIndexIndexInfoList[j].seqIndex - seqIndexMin;
            int factorCount = dstSeqIndexFactorLookup[k].factorCount;
            if (factorCount){
              responseStreamInt = (int *)responseStreamFactorElement;
              *responseStreamInt++ = seqIndexIndexInfoList[j].listIndex1;
              *responseStreamInt++ = seqIndexIndexInfoList[j].listIndex2;
              *responseStreamInt++ = factorCount;
              responseStreamFactorElement = (FactorElement *)responseStreamInt;
              memcpy(responseStreamFactorElement,
                dstSeqIndexFactorLookup[k].factorList,
                factorCount * sizeof(FactorElement));
              responseStreamFactorElement += factorCount;
            }
          }
          // send response back to Pet "ii"
          vm->vmk_send(&responseStreamSize, sizeof(int), ii);
          if (responseStreamSize)
            vm->vmk_send(responseStream, responseStreamSize, ii);
          // garbage collection
          delete [] seqIndexIndexInfoList;
          delete [] responseStream;
        }
      } // for ii
    }
  }

#ifdef ASMMSTOREPRINT
  // more serious printing
  for (int j=0; j<srcLocalDeCount; j++){
    for (int k=0; k<srcDistGridLocalDeCellCount[j]; k++){
      printf("localPet: %d, srcLinSeqList[%d][%d].linIndex = %d, "
        ".seqIndex = %d, .factorCount = %d\n",
        localPet, j, k, srcLinSeqList[j][k].linIndex,
        srcLinSeqList[j][k].seqIndex, srcLinSeqList[j][k].factorCount);
      for (int kk=0; kk<srcLinSeqList[j][k].factorCount; kk++)
        printf("factorList[%d].partnerSeqIndex = %d, .partnerDe = %d, "
          ".factor = %g\n", kk,
          srcLinSeqList[j][k].factorList[kk].partnerSeqIndex,
          srcLinSeqList[j][k].factorList[kk].partnerDe, 
          *((double *)srcLinSeqList[j][k].factorList[kk].factor));
    }
  }
    
  // more serious printing
  for (int j=0; j<dstLocalDeCount; j++){
    for (int k=0; k<dstDistGridLocalDeCellCount[j]; k++){
      printf("localPet: %d, dstLinSeqList[%d][%d].linIndex = %d, "
        ".seqIndex = %d, .factorCount = %d\n",
        localPet, j, k, dstLinSeqList[j][k].linIndex,
        dstLinSeqList[j][k].seqIndex, dstLinSeqList[j][k].factorCount);
      for (int kk=0; kk<dstLinSeqList[j][k].factorCount; kk++)
        printf("factorList[%d].partnerSeqIndex = %d, .partnerDe = %d, "
          ".factor = %g\n", kk,
          dstLinSeqList[j][k].factorList[kk].partnerSeqIndex,
          dstLinSeqList[j][k].factorList[kk].partnerDe, 
          *((double *)dstLinSeqList[j][k].factorList[kk].factor));
    }
  }
#endif

#ifdef ASMMSTORETIMING
  VMK::wtime(&t7);   //gjt - profile
#endif
  
  //---------------------------------------------------------------------------
  // Phase III
  //---------------------------------------------------------------------------

  // create and initialize the RouteHandle
  *routehandle = ESMC_RouteHandleCreate(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // todo: I have no idea what some of these settings do, just copied it for now
  // todo: from what I saw being set in ESMF_IArrayHaloStoreIndex()
  // todo: All I need here is a valid RouteHandle so I can attach an XXE object.
  localrc =
    (*routehandle)->ESMC_RouteHandleSetType(ESMC_ARRAYSPARSEMATMULHANDLE);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  localrc = (*routehandle)->ESMC_RouteHandleSetRouteCount(1);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  localrc = (*routehandle)->ESMC_RouteHandleSetRMapType(ESMC_1TO1HANDLEMAP);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  localrc = (*routehandle)->ESMC_RouteHandleSetTVCount(0);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  localrc = (*routehandle)->ESMC_RouteHandleSetTVMapType(ESMC_NOHANDLEMAP);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;

  // allocate XXE and attach to RouteHandle
  XXE *xxe;
  try{
    xxe = new XXE(1000, 10000, 1000);
  }catch (...){
    ESMC_LogDefault.ESMC_LogAllocError(&rc);
    return rc;
  }
  localrc = (*routehandle)->ESMC_RouteHandleSetStorage(xxe);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // set typekind in xxe
  xxe->typekind[0] = typekindArg;
  // prepare XXE related helper variables
  void *xxeElement;
  XXE::SendnbInfo *xxeSendnbInfo;
  XXE::RecvnbInfo *xxeRecvnbInfo;
  XXE::SendnbRRAInfo *xxeSendnbRRAInfo;
  XXE::RecvnbRRAInfo *xxeRecvnbRRAInfo;
  XXE::CommhandleInfo *xxeCommhandleInfo;
  XXE::ProductSumVectorInfo *xxeProductSumVectorInfo;
  XXE::ProductSumScalarInfo *xxeProductSumScalarInfo;
  XXE::ProductSumScalarRRAInfo *xxeProductSumScalarRRAInfo;
  XXE::ProductSumSuperScalarRRAInfo *xxeProductSumSuperScalarRRAInfo;
  XXE::ProductSumSuperScalarContigRRAInfo
    *xxeProductSumSuperScalarContigRRAInfo;
  XXE::MemCpyInfo *xxeMemCpyInfo;
  XXE::MemCpySrcRRAInfo *xxeMemCpySrcRRAInfo;
  XXE::MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo;
  XXE::XxeSubInfo *xxeSubInfo;
  XXE::XxeSubMultiInfo *xxeSubMultiInfo;
  XXE::WtimerInfo *xxeWtimerInfo;
  XXE::PrintInfo *xxePrintInfo;
  XXE::WaitOnIndexInfo *xxeWaitOnIndexInfo;
  XXE::WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo;
    
#ifdef ASMMSTORETIMING
  VMK::wtime(&t8);   //gjt - profile
#endif
    
#define ASMMPROFILE___disable
#ifdef ASMMPROFILE
  // <XXE profiling element>
  xxe->stream[xxe->count].opId = XXE::wtimer;
  xxeElement = &(xxe->stream[xxe->count]);
  xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
  xxeWtimerInfo->timerId = 0;
  xxeWtimerInfo->timerString = new char[80];
  strcpy(xxeWtimerInfo->timerString, "Wtimer 0");
  xxeWtimerInfo->actualWtimerId = 0;
  xxeWtimerInfo->relativeWtimerId = 0;
  xxeWtimerInfo->relativeWtimerXXE = NULL;
  localrc = xxe->incCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garb coll
  localrc = xxe->incStorageCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  // </XXE profiling element>
#endif
  
  // prepare arrays used in recvnb loop _and_ +=* loop
  int *diffPartnerDeCount = new int[dstLocalDeCount];
  int **recvnbIndex = new int*[dstLocalDeCount];
  int **partnerDeCount = new int*[dstLocalDeCount];
  struct DstInfo{
    int linIndex;
    int seqIndex;
    int partnerSeqIndex;
    int localDeFactorListIndex;
    static int cmp(const void *a, const void *b){
      DstInfo *aObj = (DstInfo *)a;
      DstInfo *bObj = (DstInfo *)b;
      if (aObj->partnerSeqIndex < bObj->partnerSeqIndex) return -1;
      if (aObj->partnerSeqIndex > bObj->partnerSeqIndex) return +1;
      // partnerSeqIndex must be equal
      if (aObj->seqIndex < bObj->seqIndex) return -1;
      if (aObj->seqIndex > bObj->seqIndex) return +1;
      // seqIndex must also be equal
      return 0;
    }
  };
  DstInfo ***dstInfoTable = new DstInfo**[dstLocalDeCount];
  char **localDeFactorList = new char*[dstLocalDeCount];
  char ***buffer = new char**[dstLocalDeCount];
      
  // determine recv pattern for all localDEs in dstArray and issue XXE::recvnb
  for (int j=0; j<dstLocalDeCount; j++){
    int *index2Ref = new int[dstDistGridLocalDeCellCount[j]];  // large enough
    int localDeFactorCount = 0; // reset
    int iCount = 0; // reset
    for (int k=0; k<dstDistGridLocalDeCellCount[j]; k++){
      int factorCount = dstLinSeqList[j][k].factorCount;
      if (factorCount){
        index2Ref[iCount] = k;   // store index2
        localDeFactorCount += factorCount;
        ++iCount; // increment counter
      }
    }

#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Xa);   //gjt - profile
#endif
        
#ifdef ASMMSTOREPRINT
printf("iCount: %d, localDeFactorCount: %d\n", iCount, localDeFactorCount);
#endif
    int *index2Ref2 = new int[localDeFactorCount];  // large enough
    int *factorIndexRef = new int[localDeFactorCount];  // large enough
    int *partnerDeRef = new int[localDeFactorCount];  // large enough
    int *partnerDeList = new int[localDeFactorCount];  // large enough
    partnerDeCount[j] = new int[localDeFactorCount];  // large enough
    diffPartnerDeCount[j] = 0; // reset
    int count = 0; // reset
    for (int i=0; i<iCount; i++){
      int factorCount = dstLinSeqList[j][index2Ref[i]].factorCount;
      for (int k=0; k<factorCount; k++){
        int partnerDe = dstLinSeqList[j][index2Ref[i]].factorList[k].partnerDe;
        int kk;
        for (kk=0; kk<diffPartnerDeCount[j]; kk++)
          if (partnerDeList[kk]==partnerDe) break;
        if (kk==diffPartnerDeCount[j]){
          // new entry
          partnerDeList[kk] = partnerDe;
          partnerDeCount[j][kk] = 1; // initialize
          ++diffPartnerDeCount[j];
        }else
          ++partnerDeCount[j][kk];   // increment
        index2Ref2[count] = index2Ref[i];
        factorIndexRef[count] = k;
        partnerDeRef[count] = kk;
        ++count;
      }
    }
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Xb);   //gjt - profile
#endif
        
    // invert the look-up direction
    // prepare to sort each "diffPartnerDeCount[j] group"
    // at the same time determine linIndexTermCount and linIndexTermFactorCount
    dstInfoTable[j] = new DstInfo*[diffPartnerDeCount[j]];
    int *dstInfoTableInit = new int[diffPartnerDeCount[j]];
    localDeFactorList[j] = new char[localDeFactorCount * dataSize];
    xxe->storage[xxe->storageCount] = localDeFactorList[j]; // xxe garb. coll.
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    for (int i=0; i<diffPartnerDeCount[j]; i++){
      dstInfoTable[j][i] = new DstInfo[partnerDeCount[j][i]];
      dstInfoTableInit[i] = 0;   // reset
    }
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Xc1);   //gjt - profile
#endif
    for (int i=0; i<localDeFactorCount; i++){
      int partnerDeListIndex = partnerDeRef[i];
      int index2 = dstInfoTableInit[partnerDeListIndex]++;
      dstInfoTable[j][partnerDeListIndex][index2].linIndex =
        dstLinSeqList[j][index2Ref2[i]].linIndex;
      dstInfoTable[j][partnerDeListIndex][index2].seqIndex =
        dstLinSeqList[j][index2Ref2[i]].seqIndex;
      dstInfoTable[j][partnerDeListIndex][index2].partnerSeqIndex =
        dstLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .partnerSeqIndex;
      
      memcpy(localDeFactorList[j] + i*dataSize,
        dstLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .factor, dataSize);
      dstInfoTable[j][partnerDeListIndex][index2].localDeFactorListIndex = i;
    }
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Xc2);   //gjt - profile
#endif    
    // sort each "diffPartnerDeCount[j] group" wrt partnerSeqIndex and seqIndex
    // in this order (opposite of src)
    for (int i=0; i<diffPartnerDeCount[j]; i++)
      qsort(dstInfoTable[j][i], partnerDeCount[j][i], 
        sizeof(DstInfo), DstInfo::cmp);

#ifdef ASMMSTOREPRINT
    // print:
    printf("dstArray: %d, %d\n", j, diffPartnerDeCount[j]); 
    for (int i=0; i<diffPartnerDeCount[j]; i++)
      for (int k=0; k<partnerDeCount[j][i]; k++)
        printf("dstInfoTable[%d][%d][%d].seqIndex = %d, .partnerSeqIndex[][] ="
          " %d\n", j, i, k, dstInfoTable[j][i][k].seqIndex, 
          dstInfoTable[j][i][k].partnerSeqIndex);
#endif
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Xd);   //gjt - profile
#endif
        
#ifdef ASMMPROFILE
    // <XXE profiling element>
    xxe->stream[xxe->count].opId = XXE::wtimer;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
    xxeWtimerInfo->timerId = xxe->count;
    xxeWtimerInfo->timerString = new char[80];
    strcpy(xxeWtimerInfo->timerString, "Wt: recnbL");
    xxeWtimerInfo->actualWtimerId = xxe->count;
    xxeWtimerInfo->relativeWtimerId = 0;
    xxeWtimerInfo->relativeWtimerXXE = NULL;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    // </XXE profiling element>
#endif
        
    // construct recv pattern and fill in corresponding XXE StreamElements
    recvnbIndex[j] = new int[diffPartnerDeCount[j]];
    buffer[j] = new char*[diffPartnerDeCount[j]];
    for (int i=0; i<diffPartnerDeCount[j]; i++){
      // fill in XXE StreamElements for dstArray side
      // large contiguous 1st level receive buffer
      buffer[j][i] = new char[partnerDeCount[j][i] * dataSize];
      xxe->storage[xxe->storageCount] = buffer[j][i]; // xxe garbage collection
      localrc = xxe->incStorageCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      int srcDe = partnerDeList[i];
      int srcPet;   //TODO: DE-based comms
      srcArray->delayout->getDEMatchPET(srcDe, *vm, NULL, &srcPet, 1);
#ifdef ASMMSTOREPRINT
      printf("gjt: XXE::recvnb on localPet %d from Pet %d\n", localPet, srcPet);
#endif
      recvnbIndex[j][i] = xxe->count;  // need index for the associated wait
      xxe->stream[xxe->count].opId = XXE::recvnb;
      xxeElement = &(xxe->stream[xxe->count]);
      xxeRecvnbInfo = (XXE::RecvnbInfo *)xxeElement;
      xxeRecvnbInfo->buffer = buffer[j][i];
      xxeRecvnbInfo->size = partnerDeCount[j][i] * dataSize;
      xxeRecvnbInfo->srcPet = srcPet;
      xxeRecvnbInfo->commhandle = new vmk_commhandle*;
      *(xxeRecvnbInfo->commhandle) = new vmk_commhandle;
      localrc = xxe->incCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // keep track of commhandles for xxe garbage collection
      xxe->commhandle[xxe->commhandleCount] = xxeRecvnbInfo->commhandle;
      localrc = xxe->incCommhandleCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
    } // for i - diffPartnerDeCount[j]
        
#ifdef ASMMPROFILE
    // <XXE profiling element>
    xxe->stream[xxe->count].opId = XXE::wtimer;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
    xxeWtimerInfo->timerId = xxe->count;
    xxeWtimerInfo->timerString = new char[80];
    strcpy(xxeWtimerInfo->timerString, "Wt: /recnbL");
    xxeWtimerInfo->actualWtimerId = xxe->count;
    xxeWtimerInfo->relativeWtimerId = 0;
    xxeWtimerInfo->relativeWtimerXXE = NULL;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    // </XXE profiling element>
#endif
        
    // garbage collection
    delete [] index2Ref;
    delete [] index2Ref2;
    delete [] factorIndexRef;
    delete [] partnerDeRef;
    delete [] partnerDeList;
    delete [] dstInfoTableInit;
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Xe);   //gjt - profile
    printf("gjt - profile for PET %d, j-loop %d:\n"
      " t10Xa=%g\n t10Xb=%g\n t10Xc1=%g\n t10Xc2=%g\n t10Xd=%g\n t10Xe=%g\n",
      localPet, j,
      t10Xa-t8, t10Xb-t8, t10Xc1-t8, t10Xc2-t8, t10Xd-t8, t10Xe-t8);
#endif    
    
  } // for j - dstLocalDeCount
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t10X);   //gjt - profile
#endif
  
  // determine send pattern for all localDEs in srcArray and fill in XXE
  for (int j=0; j<srcLocalDeCount; j++){
    int *index2Ref = new int[srcDistGridLocalDeCellCount[j]];  // large enough
    int localDeFactorCount = 0; // reset
    int iCount = 0; // reset
    for (int k=0; k<srcDistGridLocalDeCellCount[j]; k++){
      int factorCount = srcLinSeqList[j][k].factorCount;
      if (factorCount){
        index2Ref[iCount] = k;   // store index2
        localDeFactorCount += factorCount;
        ++iCount; // increment counter
      }
    }
    int *index2Ref2 = new int[localDeFactorCount];  // large enough
    int *factorIndexRef = new int[localDeFactorCount];  // large enough
    int *partnerDeRef = new int[localDeFactorCount];  // large enough
    int *partnerDeList = new int[localDeFactorCount];  // large enough
    int *partnerDeCount = new int[localDeFactorCount];  // large enough
    int diffPartnerDeCount = 0; // reset
    int count = 0; // reset
    for (int i=0; i<iCount; i++){
      int factorCount = srcLinSeqList[j][index2Ref[i]].factorCount;
      for (int k=0; k<factorCount; k++){
        int partnerDe = srcLinSeqList[j][index2Ref[i]].factorList[k].partnerDe;
        int kk;
        for (kk=0; kk<diffPartnerDeCount; kk++)
          if (partnerDeList[kk]==partnerDe) break;
        if (kk==diffPartnerDeCount){
          // new entry
          partnerDeList[kk] = partnerDe;
          partnerDeCount[kk] = 1; // initialize
          ++diffPartnerDeCount;
        }else
          ++partnerDeCount[kk];   // increment
        index2Ref2[count] = index2Ref[i];
        factorIndexRef[count] = k;
        partnerDeRef[count] = kk;
        ++count;
      }
    }
    // invert the look-up direction
    struct SrcInfo{
      int linIndex;
      int seqIndex;
      int partnerSeqIndex;
      static int cmp(const void *a, const void *b){
        SrcInfo *aObj = (SrcInfo *)a;
        SrcInfo *bObj = (SrcInfo *)b;
        if (aObj->seqIndex < bObj->seqIndex) return -1;
        if (aObj->seqIndex > bObj->seqIndex) return +1;
        // seqIndex must be equal
        if (aObj->partnerSeqIndex < bObj->partnerSeqIndex) return -1;
        if (aObj->partnerSeqIndex > bObj->partnerSeqIndex) return +1;
        // partnerSeqIndex must also be equal
        return 0;
      }
    };
    SrcInfo **srcInfoTable = new SrcInfo*[diffPartnerDeCount];
    int *srcInfoTableInit = new int[diffPartnerDeCount];
    for (int i=0; i<diffPartnerDeCount; i++){
      srcInfoTable[i] = new SrcInfo[partnerDeCount[i]];
      srcInfoTableInit[i] = 0;   // reset
    }
    for (int i=0; i<localDeFactorCount; i++){
      int partnerDeListIndex = partnerDeRef[i];
      int index2 = srcInfoTableInit[partnerDeListIndex]++;
      srcInfoTable[partnerDeListIndex][index2].linIndex =
        srcLinSeqList[j][index2Ref2[i]].linIndex;
      srcInfoTable[partnerDeListIndex][index2].seqIndex =
        srcLinSeqList[j][index2Ref2[i]].seqIndex;
      srcInfoTable[partnerDeListIndex][index2].partnerSeqIndex =
        srcLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .partnerSeqIndex;
    }
    // sort each "diffPartnerDeCount group" wrt seqIndex and partnerSeqIndex
    // in this order (opposite of dst)
    for (int i=0; i<diffPartnerDeCount; i++)
      qsort(srcInfoTable[i], partnerDeCount[i], sizeof(SrcInfo), SrcInfo::cmp);
#ifdef ASMMSTOREPRINT
    // print:
    for (int i=0; i<diffPartnerDeCount; i++)
      for (int k=0; k<partnerDeCount[i]; k++)
        printf("srcInfoTable[%d][%d].seqIndex = %d, .partnerSeqIndex[][] ="
          " %d\n", i, k, srcInfoTable[i][k].seqIndex, 
          srcInfoTable[i][k].partnerSeqIndex);
#endif
    
#ifdef ASMMPROFILE
    // <XXE profiling element>
    xxe->stream[xxe->count].opId = XXE::wtimer;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
    xxeWtimerInfo->timerId = xxe->count;
    xxeWtimerInfo->timerString = new char[80];
    strcpy(xxeWtimerInfo->timerString, "Wt: sendnbL");
    xxeWtimerInfo->actualWtimerId = xxe->count;
    xxeWtimerInfo->relativeWtimerId = 0;
    xxeWtimerInfo->relativeWtimerXXE = NULL;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    // </XXE profiling element>
#endif
    
    // construct send pattern and fill in corresponding XXE StreamElements    
    struct LinIndexContigBlock{
      int linIndex;
      int linIndexCount;
    };
    for (int i=0; i<diffPartnerDeCount; i++){
      // determine contiguous runs in linIndex to minimize memcpy overhead
      LinIndexContigBlock *linIndexContigBlockList =
        new LinIndexContigBlock[partnerDeCount[i]]; // large enough
      int count = 0;  // reset
      // initialize linIndexContigBlockList[]
      linIndexContigBlockList[count].linIndex = srcInfoTable[i][0].linIndex;
      linIndexContigBlockList[count].linIndexCount = 1;
      for (int k=1; k<partnerDeCount[i]; k++){
        if (srcInfoTable[i][k-1].linIndex + 1 == srcInfoTable[i][k].linIndex){
          // contiguous step in linIndex
          ++linIndexContigBlockList[count].linIndexCount;
        }else{
          // discontiguous jump in linIndex
          ++count;
          linIndexContigBlockList[count].linIndex = srcInfoTable[i][k].linIndex;
          linIndexContigBlockList[count].linIndexCount = 1;
        }
      }
      ++count;
      // fill in XXE StreamElements for srcArray side
      int dstDe = partnerDeList[i];
      int dstPet;   //TODO: DE-based comms
      dstArray->delayout->getDEMatchPET(dstDe, *vm, NULL, &dstPet, 1);
#ifdef ASMMSTOREPRINT
      printf("gjt: XXE::sendnb from localPet %d to Pet %d\n", localPet, dstPet);
#endif

      if (count == 1){
#ifdef ASMMSTOREPRINT
        printf("gjt: single contiguous linIndex run in srcArray\n");
#endif
        // sendnbRRA out of single contiguous linIndex run
        xxe->stream[xxe->count].opId = XXE::sendnbRRA;
        xxeElement = &(xxe->stream[xxe->count]);
        xxeSendnbRRAInfo = (XXE::SendnbRRAInfo *)xxeElement;
        xxeSendnbRRAInfo->rraOffset =
          linIndexContigBlockList[0].linIndex * dataSize;
        xxeSendnbRRAInfo->size = partnerDeCount[i] * dataSize;
        xxeSendnbRRAInfo->dstPet = dstPet;
        xxeSendnbRRAInfo->rraIndex = j; // localDe index into srcArray
        localrc = xxe->incCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }else{
#ifdef ASMMSTOREPRINT
        printf("gjt: non-contiguous linIndex in srcArray -> need buffer \n");
#endif
        // need intermediate buffer and memCpySrcRRAs
        char *buffer = new char[partnerDeCount[i] * dataSize];
        xxe->storage[xxe->storageCount] = buffer; // for xxe garbage collection
        localrc = xxe->incStorageCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef USEmemCpySrcRRA
        char *bufferPointer = buffer;
        for (int k=0; k<count; k++){
          int byteCount = linIndexContigBlockList[k].linIndexCount * dataSize;
          // memCpySrcRRA pieces into intermediate buffer
          xxe->stream[xxe->count].opId = XXE::memCpySrcRRA;
          xxeElement = &(xxe->stream[xxe->count]);
          xxeMemCpySrcRRAInfo = (XXE::MemCpySrcRRAInfo *)xxeElement;
          xxeMemCpySrcRRAInfo->dstMem = bufferPointer;
          xxeMemCpySrcRRAInfo->rraOffset =
            (linIndexContigBlockList[k].linIndex * dataSize);
          xxeMemCpySrcRRAInfo->size = byteCount;
          xxeMemCpySrcRRAInfo->rraIndex = j; // localDe index into srcArray
          localrc = xxe->incCount();
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
            ESMF_ERR_PASSTHRU, &rc)) return rc;
          bufferPointer += byteCount;
        }
#else
        // memGatherSrcRRA pieces into intermediate buffer
        xxe->stream[xxe->count].opId = XXE::memGatherSrcRRA;
        xxeElement = &(xxe->stream[xxe->count]);
        xxeMemGatherSrcRRAInfo = (XXE::MemGatherSrcRRAInfo *)xxeElement;
        xxeMemGatherSrcRRAInfo->dstBase = buffer;
        xxeMemGatherSrcRRAInfo->rraIndex = j; // localDe index into srcArray
        xxeMemGatherSrcRRAInfo->chunkCount = count;
        char *rraOffsetListChar = new char[count*sizeof(int)];
        int *rraOffsetList = (int *)rraOffsetListChar;
        xxeMemGatherSrcRRAInfo->rraOffsetList = rraOffsetList;
        xxe->storage[xxe->storageCount] = rraOffsetListChar; // f xxe garb coll.
        localrc = xxe->incStorageCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        char *countListChar = new char[count*sizeof(int)];
        int *countList = (int *)countListChar;
        xxeMemGatherSrcRRAInfo->countList = countList;
        xxe->storage[xxe->storageCount] = countListChar; // for xxe garb coll.
        localrc = xxe->incStorageCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        int xxeIndex = xxe->count;  // need this beyond the increment
        localrc = xxe->incCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        // try typekind specific memGatherSrcRRA
        if (typekindArg == ESMC_TYPEKIND_R4)
          xxe->stream[xxeIndex].opSubId = XXE::R4;
        else if (typekindArg == ESMC_TYPEKIND_R8)
          xxe->stream[xxeIndex].opSubId = XXE::R8;
        else if (typekindArg == ESMC_TYPEKIND_I4)
          xxe->stream[xxeIndex].opSubId = XXE::I4;
        else if (typekindArg == ESMC_TYPEKIND_I8)
          xxe->stream[xxeIndex].opSubId = XXE::I8;
        for (int k=0; k<count; k++){
          rraOffsetList[k] = linIndexContigBlockList[k].linIndex * dataSize;
          countList[k] = linIndexContigBlockList[k].linIndexCount;
        }
        double dt_tk;
        localrc = xxe->exec(rraCount, rraList, &dt_tk, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // try byte option for memGatherSrcRRA
        xxe->stream[xxeIndex].opSubId = XXE::BYTE;
        for (int k=0; k<count; k++){
          rraOffsetList[k] = linIndexContigBlockList[k].linIndex * dataSize;
          countList[k] = linIndexContigBlockList[k].linIndexCount * dataSize;
        }
        double dt_byte;
        localrc = xxe->exec(rraCount, rraList, &dt_byte, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
printf("gjt - on localPet %d memGatherSrcRRA took dt_tk=%g s and"
  " dt_byte=%g s for count=%d\n", localPet, dt_tk, dt_byte, count);
        // decide for the fastest option
        if (dt_byte < dt_tk){
          // use byte option for memGatherSrcRRA
          xxe->stream[xxeIndex].opSubId = XXE::BYTE;
          for (int k=0; k<count; k++){
            rraOffsetList[k] = linIndexContigBlockList[k].linIndex * dataSize;
            countList[k] = linIndexContigBlockList[k].linIndexCount * dataSize;
          }
        }else{
          // use typekind specific memGatherSrcRRA
          if (typekindArg == ESMC_TYPEKIND_R4)
            xxe->stream[xxeIndex].opSubId = XXE::R4;
          else if (typekindArg == ESMC_TYPEKIND_R8)
            xxe->stream[xxeIndex].opSubId = XXE::R8;
          else if (typekindArg == ESMC_TYPEKIND_I4)
            xxe->stream[xxeIndex].opSubId = XXE::I4;
          else if (typekindArg == ESMC_TYPEKIND_I8)
            xxe->stream[xxeIndex].opSubId = XXE::I8;
          for (int k=0; k<count; k++){
            rraOffsetList[k] = linIndexContigBlockList[k].linIndex * dataSize;
            countList[k] = linIndexContigBlockList[k].linIndexCount;
          }
        }
#endif
        // sendnb out of contiguous intermediate buffer
        xxe->stream[xxe->count].opId = XXE::sendnb;
        xxeElement = &(xxe->stream[xxe->count]);
        xxeSendnbInfo = (XXE::SendnbInfo *)xxeElement;
        xxeSendnbInfo->buffer = buffer;
        xxeSendnbInfo->size = partnerDeCount[i] * dataSize;
        xxeSendnbInfo->dstPet = dstPet;
        localrc = xxe->incCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }
      // add commhandle to last xxe element (sendnbRRA or sendnb) and keep track
      // of commhandle for xxe garbage collection
      xxeCommhandleInfo = (XXE::CommhandleInfo *)xxeElement;
      xxeCommhandleInfo->commhandle = new vmk_commhandle*;
      *(xxeCommhandleInfo->commhandle) = new vmk_commhandle;
      xxe->commhandle[xxe->commhandleCount] = xxeCommhandleInfo->commhandle;
      localrc = xxe->incCommhandleCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      delete [] linIndexContigBlockList;
    } // for i - diffPartnerDeCount
    // garbage collection
    delete [] index2Ref;
    delete [] index2Ref2;
    delete [] factorIndexRef;
    delete [] partnerDeRef;
    delete [] partnerDeList;
    delete [] partnerDeCount;
    for (int i=0; i<diffPartnerDeCount; i++){
      delete [] srcInfoTable[i];
    }
    delete [] srcInfoTable;
    delete [] srcInfoTableInit;
    
#ifdef ASMMPROFILE
    // <XXE profiling element>
    xxe->stream[xxe->count].opId = XXE::wtimer;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
    xxeWtimerInfo->timerId = xxe->count;
    xxeWtimerInfo->timerString = new char[80];
    strcpy(xxeWtimerInfo->timerString, "Wt: /sendnbL");
    xxeWtimerInfo->actualWtimerId = xxe->count;
    xxeWtimerInfo->relativeWtimerId = 0;
    xxeWtimerInfo->relativeWtimerXXE = NULL;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    // </XXE profiling element>
#endif
        
  } // for j - srcLocalDeCount

  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t9);   //gjt - profile
#endif
    
  // use recv pattern for all localDEs in dstArray and issue XXE::"+=*"
  for (int j=0; j<dstLocalDeCount; j++){

#ifdef ASMMPROFILE
    // <XXE profiling element>
    xxe->stream[xxe->count].opId = XXE::wtimer;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
    xxeWtimerInfo->timerId = xxe->count;
    xxeWtimerInfo->timerString = new char[80];
    strcpy(xxeWtimerInfo->timerString, "Wt: w+=*L");
    xxeWtimerInfo->actualWtimerId = xxe->count;
    xxeWtimerInfo->relativeWtimerId = 0;
    xxeWtimerInfo->relativeWtimerXXE = NULL;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    // </XXE profiling element>
#endif
      
    // use waitOnAnyIndexSub to wait on and process the incoming data
    xxe->stream[xxe->count].opId = XXE::waitOnAnyIndexSub;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWaitOnAnyIndexSubInfo = (XXE::WaitOnAnyIndexSubInfo *)xxeElement;
    xxeWaitOnAnyIndexSubInfo->count = diffPartnerDeCount[j];
    char *xxeChar = new char[diffPartnerDeCount[j]*sizeof(XXE *)];
    xxeWaitOnAnyIndexSubInfo->xxe = (XXE **)xxeChar;
    char *indexChar = new char[diffPartnerDeCount[j]*sizeof(int)];
    xxeWaitOnAnyIndexSubInfo->index = (int *)indexChar;
    char *completeFlagChar = new char[diffPartnerDeCount[j]*sizeof(int)];
    xxeWaitOnAnyIndexSubInfo->completeFlag = (int *)completeFlagChar;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeChar; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = indexChar; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = completeFlagChar; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    
    for (int k=0; k<diffPartnerDeCount[j]; k++){
        
      // register the associated recvnb XXE element in xxeWaitOnAnyIndexSubInfo
      xxeWaitOnAnyIndexSubInfo->index[k] = recvnbIndex[j][k];
      
      // allocate sub XXE stream and attach to xxeWaitOnAnyIndexSubInfo
      xxeWaitOnAnyIndexSubInfo->xxe[k] = new XXE(1000, 1000, 1000);
      XXE *xxeSub = xxeWaitOnAnyIndexSubInfo->xxe[k];
      xxe->xxeSubList[xxe->xxeSubCount] = xxeSub; // xxe garbage collection
      localrc = xxe->incXxeSubCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;

#ifdef ASMMPROFILE
      // <XXE profiling element>
      xxeSub->stream[xxeSub->count].opId = XXE::wtimer;
      xxeElement = &(xxeSub->stream[xxeSub->count]);
      xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
      xxeWtimerInfo->timerId = 0;
      xxeWtimerInfo->timerString = new char[80];
      sprintf(xxeWtimerInfo->timerString, "xxeSub k=%d", k);
      xxeWtimerInfo->actualWtimerId = 0;
      xxeWtimerInfo->relativeWtimerId = 0;
      xxeWtimerInfo->relativeWtimerXXE = xxe; // reference back into main XXE
      localrc = xxeSub->incCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      xxeSub->storage[xxeSub->storageCount] =
        xxeWtimerInfo->timerString; // xxe garbCo
      localrc = xxeSub->incStorageCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // </XXE profiling element>
#endif
        
#ifdef ASMMPROFILE
      // <XXE profiling element>
      xxeSub->stream[xxeSub->count].opId = XXE::wtimer;
      xxeElement = &(xxeSub->stream[xxeSub->count]);
      xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
      xxeWtimerInfo->timerId = xxeSub->count;
      xxeWtimerInfo->timerString = new char[80];
      strcpy(xxeWtimerInfo->timerString, "Wt: pSSRRA");
      xxeWtimerInfo->actualWtimerId = xxeSub->count;
      xxeWtimerInfo->relativeWtimerId = 0;
      xxeWtimerInfo->relativeWtimerXXE = xxe; // reference back into main XXE
      localrc = xxeSub->incCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // xxe garb coll
      xxeSub->storage[xxeSub->storageCount] = xxeWtimerInfo->timerString;
      localrc = xxeSub->incStorageCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // </XXE profiling element>
#endif
      
#ifdef USEproductSumScalarRRA
      for (int kk=0; kk<partnerDeCount[j][k]; kk++){
        DstInfo *dstInfo = &(dstInfoTable[j][k][kk]);
        int linIndex = dstInfo->linIndex;
        // enter scalar "+=*" operation into XXE stream
        xxeSub->stream[xxeSub->count].opId = XXE::productSumScalarRRA;
        // todo: replace the following with using ESMC_TypeKind in XXE!
        if (typekindArg == ESMC_TYPEKIND_R4)
          xxeSub->stream[xxeSub->count].opSubId = XXE::R4;
        else if (typekindArg == ESMC_TYPEKIND_R8)
          xxeSub->stream[xxeSub->count].opSubId = XXE::R8;
        else if (typekindArg == ESMC_TYPEKIND_I4)
          xxeSub->stream[xxeSub->count].opSubId = XXE::I4;
        else if (typekindArg == ESMC_TYPEKIND_I8)
          xxeSub->stream[xxeSub->count].opSubId = XXE::I8;
        xxeElement = &(xxeSub->stream[xxeSub->count]);
        xxeProductSumScalarRRAInfo = (XXE::ProductSumScalarRRAInfo *)xxeElement;
        xxeProductSumScalarRRAInfo->rraOffset = linIndex * dataSize;
        xxeProductSumScalarRRAInfo->factor = (void *)
          (localDeFactorList[j] + (dstInfo->localDeFactorListIndex) * dataSize);
        xxeProductSumScalarRRAInfo->value = (void *)
          (buffer[j][k] + kk*dataSize);
        xxeProductSumScalarRRAInfo->rraIndex = srcLocalDeCount
          + j; // localDe index into dstArray shifted by srcArray localDeCount
        localrc = xxeSub->incCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      } // kk - partnerDeCount[j][k]
#else
      int xxeIndex = xxeSub->count;  // need this beyond the increment
      localrc = xxeSub->incCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;

      // try super-scalar "+=*" operation in XXE stream
      xxeSub->stream[xxeIndex].opId = XXE::productSumSuperScalarRRA;
      // todo: replace the following with using ESMC_TypeKind in XXE!
      if (typekindArg == ESMC_TYPEKIND_R4)
        xxeSub->stream[xxeIndex].opSubId = XXE::R4;
      else if (typekindArg == ESMC_TYPEKIND_R8)
        xxeSub->stream[xxeIndex].opSubId = XXE::R8;
      else if (typekindArg == ESMC_TYPEKIND_I4)
        xxeSub->stream[xxeIndex].opSubId = XXE::I4;
      else if (typekindArg == ESMC_TYPEKIND_I8)
        xxeSub->stream[xxeIndex].opSubId = XXE::I8;
      xxeElement = &(xxeSub->stream[xxeIndex]);
      xxeProductSumSuperScalarRRAInfo =
        (XXE::ProductSumSuperScalarRRAInfo *)xxeElement;
      xxeProductSumSuperScalarRRAInfo->rraIndex = srcLocalDeCount
        + j; // localDe index into dstArray shifted by srcArray localDeCount
      int termCount = partnerDeCount[j][k];
      xxeProductSumSuperScalarRRAInfo->termCount = termCount;
      char *rraOffsetListChar = new char[termCount*sizeof(int)];
      int *rraOffsetList = (int *)rraOffsetListChar;
      xxeProductSumSuperScalarRRAInfo->rraOffsetList = rraOffsetList;
      xxeSub->storage[xxeSub->storageCount] = rraOffsetListChar; // garb coll.
      localrc = xxeSub->incStorageCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      char *factorListChar = new char[termCount*sizeof(void *)];
      void **factorList = (void **)factorListChar;
      xxeProductSumSuperScalarRRAInfo->factorList = factorList;
      xxeSub->storage[xxeSub->storageCount] = factorListChar; // garb coll.
      localrc = xxeSub->incStorageCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      char *valueListChar = new char[termCount*sizeof(void *)];
      void **valueList = (void **)valueListChar;
      xxeProductSumSuperScalarRRAInfo->valueList = valueList;
      for (int kk=0; kk<termCount; kk++){
        DstInfo *dstInfo = &(dstInfoTable[j][k][kk]);
        int linIndex = dstInfo->linIndex;
        rraOffsetList[kk] = linIndex * dataSize;
        factorList[kk] = (void *)
          (localDeFactorList[j] + (dstInfo->localDeFactorListIndex) * dataSize);
        valueList[kk] = (void *)(buffer[j][k] + kk*dataSize);
        // need to fill in sensible values or else timing will be bogus
        *(ESMC_R4 *)(rraList[srcLocalDeCount]+rraOffsetList[kk]) = 0.; //element
        *(ESMC_R4 *)valueList[kk] = 0.01; // value
      } // for kk - termCount
      xxeSub->optimizeElement(xxeIndex);
      double dt_sScalar;
      localrc = xxeSub->exec(rraCount, rraList, &dt_sScalar, xxeIndex,
        xxeIndex);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      
      // try super-scalar contig "+=*" operation in XXE stream
      xxeSub->stream[xxeIndex].opId = XXE::productSumSuperScalarContigRRA;
      xxeProductSumSuperScalarContigRRAInfo =
        (XXE::ProductSumSuperScalarContigRRAInfo *)xxeElement;
      // only change members that are different compared to super-scalar
      xxeProductSumSuperScalarContigRRAInfo->valueList = (void *)(buffer[j][k]);
      xxeSub->optimizeElement(xxeIndex);
      double dt_sScalarC;
      localrc = xxeSub->exec(rraCount, rraList, &dt_sScalarC, xxeIndex,
        xxeIndex);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
printf("gjt - on localPet %d sumSuperScalar<>RRA took dt_sScalar=%g s and"
  " dt_sScalarC=%g s for termCount=%d\n", localPet, dt_sScalar, dt_sScalarC,
  termCount);
      // decide for the fastest option
      if (dt_sScalar < dt_sScalarC){
        // use productSumSuperScalarRRA
        xxeSub->stream[xxeIndex].opId = XXE::productSumSuperScalarRRA;
        xxeProductSumSuperScalarRRAInfo->valueList = valueList;
        xxeSub->storage[xxeSub->storageCount] = valueListChar; // garb coll.
        localrc = xxeSub->incStorageCount();
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }else{
        // use productSumSuperScalarContigRRA
        // already set from last trial, just need to remove some garbage:
        delete [] valueListChar;  // garbage from productSumSuperScalarRRA try
      }
      
#endif
            
#ifdef ASMMPROFILE
      // <XXE profiling element>
      xxeSub->stream[xxeSub->count].opId = XXE::wtimer;
      xxeElement = &(xxeSub->stream[xxeSub->count]);
      xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
      xxeWtimerInfo->timerId = xxeSub->count;
      xxeWtimerInfo->timerString = new char[80];
      strcpy(xxeWtimerInfo->timerString, "Wt: /pSSRRA");
      xxeWtimerInfo->actualWtimerId = xxeSub->count;
      xxeWtimerInfo->relativeWtimerId = 0;
      xxeWtimerInfo->relativeWtimerXXE = xxe; // reference back into main XXE
      localrc = xxeSub->incCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // xxe garb coll
      xxeSub->storage[xxeSub->storageCount] = xxeWtimerInfo->timerString;
      localrc = xxeSub->incStorageCount();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // </XXE profiling element>
#endif
      
    } // k - diffPartnerDeCount[j]    
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Yf);   //gjt - profile
#endif
        
#ifdef ASMMPROFILE
    // <XXE profiling element>
    xxe->stream[xxe->count].opId = XXE::wtimer;
    xxeElement = &(xxe->stream[xxe->count]);
    xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
    xxeWtimerInfo->timerId = xxe->count;
    xxeWtimerInfo->timerString = new char[80];
    strcpy(xxeWtimerInfo->timerString, "Wt: /w+=*L");
    xxeWtimerInfo->actualWtimerId = xxe->count;
    xxeWtimerInfo->relativeWtimerId = 0;
    xxeWtimerInfo->relativeWtimerXXE = NULL;
    localrc = xxe->incCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garbCo
    localrc = xxe->incStorageCount();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    // </XXE profiling element>
#endif
    
    // garbage collection
    delete [] partnerDeCount[j];
    for (int i=0; i<diffPartnerDeCount[j]; i++){
      delete [] dstInfoTable[j][i];
    }
    delete [] dstInfoTable[j];
    delete [] recvnbIndex[j];
    delete [] buffer[j];
        
#ifdef ASMMSTORETIMING
    VMK::wtime(&t10Yg);   //gjt - profile
  printf("gjt - profile for PET %d, j-loop %d:\n"
    " t10Yf=%g\n t10Yg=%g\n", localPet, j,
    t10Yf-t9, t10Yg-t9);
#endif
      
  } // for j - dstLocalDeCount

  // garbage collection
  delete [] diffPartnerDeCount;
  delete [] recvnbIndex;
  delete [] partnerDeCount;
  delete [] dstInfoTable;
  delete [] localDeFactorList;
  delete [] buffer;
    
#ifdef ASMMSTORETIMING
  VMK::wtime(&t10Y);   //gjt - profile
#endif
  
#ifdef ASMMPROFILE
  // <XXE profiling element>
  xxe->stream[xxe->count].opId = XXE::wtimer;
  xxeElement = &(xxe->stream[xxe->count]);
  xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
  xxeWtimerInfo->timerId = xxe->count;
  xxeWtimerInfo->timerString = new char[80];
  strcpy(xxeWtimerInfo->timerString, "Wt: bef wOAS");
  xxeWtimerInfo->actualWtimerId = xxe->count;
  xxeWtimerInfo->relativeWtimerId = 0;
  xxeWtimerInfo->relativeWtimerXXE = NULL;
  localrc = xxe->incCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garb coll
  localrc = xxe->incStorageCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  // </XXE profiling element>
#endif
  
  // post XXE::waitOnAllSendnb
  xxe->stream[xxe->count].opId = XXE::waitOnAllSendnb;
  localrc = xxe->incCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
    ESMF_ERR_PASSTHRU, &rc)) return rc;

#ifdef ASMMPROFILE
  // <XXE profiling element>
  xxe->stream[xxe->count].opId = XXE::wtimer;
  xxeElement = &(xxe->stream[xxe->count]);
  xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
  xxeWtimerInfo->timerId = xxe->count;
  xxeWtimerInfo->timerString = new char[80];
  strcpy(xxeWtimerInfo->timerString, "Wtimer End");
  xxeWtimerInfo->actualWtimerId = xxe->count;
  xxeWtimerInfo->relativeWtimerId = 0;
  xxeWtimerInfo->relativeWtimerXXE = NULL;
  localrc = xxe->incCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garb coll
  localrc = xxe->incStorageCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  // </XXE profiling element>
#endif
  
#ifdef ASMMPROFILE
  // <XXE profiling element>
  xxe->stream[xxe->count].opId = XXE::wtimer;
  xxeElement = &(xxe->stream[xxe->count]);
  xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
  xxeWtimerInfo->timerId = xxe->count;
  xxeWtimerInfo->timerString = new char[80];
  strcpy(xxeWtimerInfo->timerString, "Wtimer End2");
  xxeWtimerInfo->actualWtimerId = xxe->count;
  xxeWtimerInfo->relativeWtimerId = 0;
  xxeWtimerInfo->relativeWtimerXXE = NULL;
  localrc = xxe->incCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  xxe->storage[xxe->storageCount] = xxeWtimerInfo->timerString; // xxe garb coll
  localrc = xxe->incStorageCount();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;
  // </XXE profiling element>
#endif
  
#if 0  
  // optimize the XXE entire stream
  localrc = xxe->optimize();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
#endif 
  
  // get XXE ready for execution
  localrc = xxe->execReady();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // garbage collection
  for (int i=0; i<srcLocalDeCount; i++){
    for (int j=0; j<srcDistGridLocalDeCellCount[i]; j++)
      if (srcLinSeqList[i][j].factorCount)
        delete [] srcLinSeqList[i][j].factorList;
    delete [] srcLinSeqList[i];
  }
  delete [] srcLinSeqList;
  delete [] srcDistGridLocalDeCellCount;
  for (int i=0; i<dstLocalDeCount; i++){
    for (int j=0; j<dstDistGridLocalDeCellCount[i]; j++)
      if (dstLinSeqList[i][j].factorCount)
        delete [] dstLinSeqList[i][j].factorList;
    delete [] dstLinSeqList[i];
  }
  delete [] dstLinSeqList;
  delete [] dstDistGridLocalDeCellCount;
  delete [] srcCellCountList;
  delete [] dstCellCountList;
  delete [] srcSeqIndexMinMaxList;
  delete [] dstSeqIndexMinMaxList;
  for (int i=0; i<srcSeqIndexInterval[localPet].count; i++)
    if (srcSeqIndexFactorLookup[i].factorCount)
      delete [] srcSeqIndexFactorLookup[i].factorList;
  delete [] srcSeqIndexFactorLookup;
  delete [] srcSeqIndexInterval;
  for (int i=0; i<dstSeqIndexInterval[localPet].count; i++)
    if (dstSeqIndexFactorLookup[i].factorCount)
      delete [] dstSeqIndexFactorLookup[i].factorList;
  delete [] dstSeqIndexFactorLookup;
  delete [] dstSeqIndexInterval;
  delete [] factorPetList;
  delete [] srcSeqIntervFactorListCount;
  for (int i=0; i<petCount; i++)
    delete [] srcSeqIntervFactorListIndex[i];
  delete [] srcSeqIntervFactorListIndex;
  delete [] dstSeqIntervFactorListCount;
  for (int i=0; i<petCount; i++)
    delete [] dstSeqIntervFactorListIndex[i];
  delete [] dstSeqIntervFactorListIndex;
  delete [] rraList;
    
#ifdef ASMMSTORETIMING
  VMK::wtime(&t11);   //gjt - profile
  printf("gjt - profile for PET %d:\n"
    " t1=%g\n t2=%g\n t3=%g\n t4=%g\n t5=%g\n t6=%g\n"
    " t7=%g\n t8=%g\n t10X=%g\n t9=%g\n t10Y=%g\n t11=%g\n", localPet,
    t1-t0, t2-t0, t3-t0, t4-t0, 
    t5-t0, t6-t0, t7-t0, t8-t0, t10X-t0, t9-t0, t10Y-t0, t11-t0);
#endif
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::sparseMatMul()"
//BOPI
// !IROUTINE:  ESMCI::Array::sparseMatMul
//
// !INTERFACE:
int Array::sparseMatMul(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                      // in    - source Array
  Array *dstArray,                      // inout - destination Array
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
  ESMC_Logical zeroflag,                // in    - ESMF_FALSE: don't zero dstA.
                                        //         ESMF_TRUE: (def.) zero dstA.
  ESMC_Logical checkflag                // in    - ESMF_FALSE: (def.) bas. chcks
                                        //         ESMF_TRUE: full input check
  ){    
//
// !DESCRIPTION:
//    Execute an Array sparse matrix multiplication operation
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#define ASMMTIMING___disable
#ifdef ASMMTIMING
  double t0, t1, t2, t3, t4, t5, t6;            //gjt - profile
  VMK::wtime(&t0);      //gjt - profile
#endif

  // basic error checking for input
  if (srcArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to srcArray", &rc);
    return rc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to dstArray", &rc);
    return rc;
  }
  
#ifdef ASMMTIMING
  VMK::wtime(&t1);      //gjt - profile
#endif
  
  // get a handle on the XXE stored in routehandle
  XXE *xxe = (XXE *)(*routehandle)->ESMC_RouteHandleGetStorage();

  // conditionally perform full input checks
  if (checkflag==ESMF_TRUE){
    // check that XXE's typekind matches srcArray typekind
    if (xxe->typekind[0] != srcArray->getTypekind()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between srcArray argument and precomputed XXE",
        &rc);
      return rc;
    }
    // check that XXE's typekind matches dstArray typekind
    if (xxe->typekind[0] != dstArray->getTypekind()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between dstArray argument and precomputed XXE",
        &rc);
      return rc;
    }
    // todo: need to check DistGrid-conformance and congruence of local tiles
    //       between argument Array pair and Array pair used during XXE precomp.
  }
  
#ifdef ASMMTIMING
  VMK::wtime(&t2);      //gjt - profile
#endif
  
  // conditionally zero out total regions of the dstArray for all localDEs
  if (zeroflag==ESMF_TRUE){
    for (int i=0; i<dstArray->delayout->getLocalDeCount(); i++){
      char *start = (char *)(dstArray->larrayBaseAddrList[i]);
      int dataSize = ESMC_TypeKindSize(dstArray->typekind);
      int cellCount =
        dstArray->deCellCount[dstArray->delayout->getLocalDeList()[i]];
      memset(start, 0, dataSize * cellCount);
    }
  }
  
#ifdef ASMMTIMING
  VMK::wtime(&t3);      //gjt - profile
#endif
  
  // prepare for relative run-time addressing (RRA)
  int rraCount = srcArray->delayout->getLocalDeCount();
  rraCount += dstArray->delayout->getLocalDeCount();
  char **rraList = new char*[rraCount];
  memcpy((char *)rraList, srcArray->larrayBaseAddrList,
    srcArray->delayout->getLocalDeCount() * sizeof(char *));
  memcpy((char *)rraList
    + srcArray->delayout->getLocalDeCount() * sizeof(char *),
    dstArray->larrayBaseAddrList,
    dstArray->delayout->getLocalDeCount() * sizeof(char *));

#ifdef ASMMTIMING
  VMK::wtime(&t4);      //gjt - profile
#endif
  
  // execute XXE stream
  localrc = xxe->exec(rraCount, rraList);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;

#ifdef ASMMTIMING
  VMK::wtime(&t5);      //gjt - profile
#endif
  
  // garbage collection
  delete [] rraList;
  
#ifdef ASMMTIMING
  VMK::wtime(&t6);      //gjt - profile
  int localPet = 999;   // for now in order to save on overhead
  printf("gjt - exec() profile for PET %d: "
    "dt1 = %g\tdt2 = %g\tdt3 = %g\tdt4 = %g\tdt5 = %g\tdt6 = %g\n",
    localPet, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0);
#endif
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::sparseMatMulRelease()"
//BOPI
// !IROUTINE:  ESMCI::Array::sparseMatMulRelease
//
// !INTERFACE:
int Array::sparseMatMulRelease(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ESMC_RouteHandle *routehandle        // inout -
  ){    
//
// !DESCRIPTION:
//    Release information for an Array sparse matrix multiplication operation
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // get XXE from routehandle
  XXE *xxe = (XXE *)routehandle->ESMC_RouteHandleGetStorage();
  
#define XXEPROFILEPRINT
#ifdef XXEPROFILEPRINT
  // print XXE stream profile
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  for (int pet=0; pet<petCount; pet++){
    if (pet==localPet){
      localrc = xxe->printProfile();
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc))
      return rc;
    }
    vm->vmk_barrier();
  }
#endif
  
  // delete xxe
  delete xxe;

  // mark storage pointer in RouteHandle as invalid  
  routehandle->ESMC_RouteHandleSetStorage(NULL);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

} // namespace ESMCI





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
//BOPI
// !IROUTINE:  ESMC_newArrayConstruct
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayConstruct(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int *haloWidth,           // halo width
  ESMCI::DELayout *delayout,// DELayout
  int rootPET,              // root
  ESMCI::VM *vm){           // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Construct the internal information structure in a new ESMC\_newArray
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  int localTid = vm->getTid(localPet);
  localVAS = vm->getVas(localPet);
  // get info about the LocalArray on rootPET and broadcast it to all PETs
  int laRank;
  if (localPet == rootPET)
    laRank = larray->ESMC_LocalArrayGetRank();
  vm->vmk_broadcast(&laRank, sizeof(int), rootPET);
  int *laLength = new int[laRank];
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLengths(laRank, laLength);
  vm->vmk_broadcast(laLength, laRank * sizeof(int), rootPET);
  int *laLbound = new int[laRank];
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLbounds(laRank, laLbound);
  vm->vmk_broadcast(laLbound, laRank * sizeof(int), rootPET);
  // set some newArray members
  rank = laRank;  // newArray's rank is equal to that of the origin LocalArray
  int decompRank;
  this->delayout = delayout;
  delayout->getDeprecated(&deCount, &decompRank, &localDeCount, NULL, 0,
    NULL, NULL, NULL, NULL, 0);
  localDeList = new int[localDeCount];
  delayout->getDeprecated(NULL, NULL, NULL, localDeList, localDeCount, NULL,
    NULL, NULL, NULL, 0);
  deVASList = new int[deCount];
  for (int de=0; de<deCount; de++)
    delayout->getDELocalInfo(de, NULL, 0, NULL, 0, NULL, 0, NULL,
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
//  delayout->getDELocalCoord(0, deCoordMin, deCoordMax);
  for (int i=0; i<deCount; i++){
//need to use the new DistGrid for this
//    delayout->getDELocalCoord(i, temp_deCoordMin, temp_deCoordMax);
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
//    delayout->getDELocalCoord(de, deCoordMin, deCoordMax);
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
  if (localPet == rootPET){
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
      int vas = vm->getVas(pet);
      if (pet != localPet && vas == localVAS){
        // send the localArrays pointer to this PET
        vm->vmk_send(&localArrays, sizeof(ESMC_LocalArray **), pet);
        vm->vmk_send(&commhArray, sizeof(ESMC_newArrayCommHandle *), pet);
        vm->vmk_send(&thargArray, sizeof(ESMC_newArrayThreadArg *), pet);
      }
    }
  }else{
    // localPet is part of a threadGroup but is not the master thread
    int pet;
    for (pet=0; pet<petCount; pet++){
      int tid = vm->getTid(pet);
      int vas = vm->getVas(pet);
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
    if (delayout->serviceOffer(de, NULL) == ESMCI::DELAYOUT_SERVICE_ACCEPT){
      for (int j=0; j<rank; j++)
        temp_counts[j] = localFullUBound[de][j] - localFullLBound[de][j] + 1;
      localArrays[i] = ESMC_LocalArrayCreate(rank, kind, temp_counts);
      commhArray[i].commhandleCount = 0;  // reset
      commhArray[i].pthidCount = 0;       // reset
      commhArray[i].buffer = NULL;        // reset
      delayout->serviceComplete(de);
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
//BOPI
// !IROUTINE:  ESMC_newArrayDestruct
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayDestruct(void){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure in a new ESMC\_newArray
//
//EOPI
//-----------------------------------------------------------------------------
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScatter()"
//BOPI
// !IROUTINE:  ESMC_newArrayScatter
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScatter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int rootPET,              // root
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. PET-based blocking paradigm.
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // check that there is a valid larray on rootPET 
  if (localPet == rootPET){
    if (larray == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'larray' argument on rootPET.", &localrc);
      return localrc;
    }
  }    
  // check that t/k/r matches
  if (localPet == rootPET){
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
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLengths(rank, laLength);
  vm->vmk_broadcast(laLength, rank * sizeof(int), rootPET);
  int *laLbound = new int[rank];
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLbounds(rank, laLbound);
  vm->vmk_broadcast(laLbound, rank * sizeof(int), rootPET);
  int laByteCount;
  if (localPet == rootPET)
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
  if (localPet == rootPET)
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
      if (delayout->serviceOffer(de, NULL) == ESMCI::DELAYOUT_SERVICE_ACCEPT){
        // the localPet's offer was accepted by DELayout
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
        delayout->serviceComplete(de); // close window on DE
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
    if (localPet == rootPET)
      buffer += laLength[0] * elementSize;
  }
  
  // garbage collection
  delete [] blockGlobalIndex;
  delete [] blockLocalIndex;
  delete [] blockID;
  if (localPet != rootPET)
    delete [] buffer;
  delete [] laLength;
  delete [] localDeArrayBase;
    
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newArrayScatter()"
//BOPI
// !IROUTINE:  ESMC_newArrayScatter
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScatter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int rootPET,              // root
  ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. DE-based non-blocking paradigm.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(ROOT): DE-based nb paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // if this is not the rootPET then exit because this is root side of scatter
  if (localPet != rootPET) return ESMF_SUCCESS;
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
//BOPI
// !IROUTINE:  ESMC_newArrayScatter
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScatter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ESMC_LocalArray *larray,  // pointer to ESMC_LocalArray object
  int rootPET,              // root
  int de,                   // DE for DE-based non-blocking scatter
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Scatter the contents of an {\tt ESMC\_LocalArray} across the
//    {\tt ESMC\_newArray}. DE-based non-blocking paradigm.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(DE): DE-based non-blocking paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // check that there is a valid larray on rootPET 
  if (localPet == rootPET){
    if (larray == ESMC_NULL_POINTER){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
        "- must supply a valid 'larray' argument on rootPET.", &localrc);
      return localrc;
    }
  }    
  // check that t/k/r matches
  if (localPet == rootPET){
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
//BOPI
// !IROUTINE:  ESMC_newArrayScalarReduce
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScalarReduce(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *result,             // result value (scalar)
  ESMC_TypeKind dtk,        // data type kind
  ESMC_Operation op,        // reduce operation
  int rootPET,              // root
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Reduce the data of an {\tt ESMC\_newArray} into a single scalar value.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // check that there is a valid result argument on rootPET 
  if (localPet == rootPET){
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
    if (delayout->serviceOffer(de, NULL) == ESMCI::DELAYOUT_SERVICE_ACCEPT){
      // the localPet's offer was accepted by DELayout
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
      delayout->serviceComplete(de); // close window on DE
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
//BOPI
// !IROUTINE:  ESMC_newArrayScalarReduce
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScalarReduce(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *result,             // result value (scalar)
  ESMC_TypeKind dtk,        // data type kind
  ESMC_Operation op,        // reduce operation
  int rootPET,              // root
  ESMC_newArrayCommHandle *commh, // commu handle for non-blocking mode
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Reduce the data of an {\tt ESMC\_newArray} into a single scalar value.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce(ROOT): DE-based nb paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // check that there is a valid result argument on rootPET 
  if (localPet == rootPET){
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
//BOPI
// !IROUTINE:  ESMC_newArrayScalarReduce
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayScalarReduce(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *result,             // result value (scalar)
  ESMC_TypeKind dtk,        // data type kind
  ESMC_Operation op,        // reduce operation
  int rootPET,              // root
  int de,                   // DE for DE-based non-blocking reduce
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Reduce the data of an {\tt ESMC\_newArray} into a single scalar value.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScalarReduce(DE): DE-based nb paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // determine localDE index
  int localDe;
  for (localDe=0; localDe<localDeCount; localDe++)
    if (localDeList[localDe] == de) break;
  // determine VAS for rootPET
  int rootVAS = vm->getVas(rootPET);
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
//BOPI
// !IROUTINE:  ESMC_newArrayWait
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayWait(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int rootPET,              // root
  ESMC_newArrayCommHandle *commh, // commu handle specif. non-blocking op.
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Wait for a non-blocking newArray communication to be done with data 
//    object on rootPET.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait: DE-based nb paradigm ROOT\n");
  printf("gjt in ESMC_newArrayWait(ROOT): commh=%p\n", commh);
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // if this is not the rootPET then exit because this is root side of scatter
  if (localPet != rootPET) return ESMF_SUCCESS;
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
    vm->commwait(&(commh->vmk_commh[i]), NULL, 1);  // use nanopause=1ns to lower load
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
//BOPI
// !IROUTINE:  ESMC_newArrayWait
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayWait(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int de,                   // DE for which to wait
  ESMCI::VM *vm){             // optional VM argument to speed up things
//
// !DESCRIPTION:
//    Wait for a non-blocking newArray communication to finish for de.
//
//EOPI
//-----------------------------------------------------------------------------
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayWait: DE-based non-blocking paradigm\n");
#endif
  int localrc;
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
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
    vm->commwait(&(commhArray[localDe].vmk_commh[i]), NULL, 1);
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
//BOPI
// !IROUTINE:  ESMC_newArrayGet
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayGet(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int *rank_out,                      // out - rank of newArray
  ESMCI::DELayout **delayout,         // out - associated DELayout
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
//EOPI
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
//BOPI
// !IROUTINE:  ESMC_newArrayPrint
//
// !INTERFACE:
int ESMC_newArray::ESMC_newArrayPrint(void){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print ESMC\_newArray
//
//EOPI
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
//BOPI
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
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize rc and localrc ; assume functions not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (*rc) *rc = ESMC_RC_NOT_IMPL;

  ESMC_newArray *array;
  // determine required information
  ESMCI::VM *vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  // check that there is a valid larray on rootPET 
  if (localPet == rootPET){
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
  if (localPet == rootPET){
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
  if (localPet == rootPET){
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
    if (localPet != rootPET)
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
  ESMCI::DELayout *delayout = ESMCI::DELayout::create(*vm, deLength, decompRank,
    NULL, 0, NULL, &localrc);
#if (VERBOSITY > 9)
  delayout->ESMC_DELayoutPrint();
#endif
  
  // finally construct the newArray according to the DELayout
  localrc = array->ESMC_newArrayConstruct(larray, haloWidth, delayout, rootPET);

  // garbage collection
  delete [] laLength;
  if (haloWidth != NULL){
    if (localPet != rootPET || haloWidthAllocFlag)
      delete [] haloWidth;
  }
  delete [] haloVolume;
  delete [] decompMap;
  delete [] deLength;
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayCreate, array: %p, %d, %d\n", array, localPet, 
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
//BOPI
// !IROUTINE:  ESMC_newArrayDestroy
//
// !INTERFACE:
int ESMC_newArrayDestroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ESMC_newArray **array){      // in - ESMC_newArray to destroy
//
// !DESCRIPTION:
//
//EOPI
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
  ESMCI::VM *vm = tharg->vm;
  int de = tharg->de;
  int rootPET = tharg->rootPET;
#if (VERBOSITY > 9)
  printf("gjt in ESMC_newArrayScatter(THREAD): de=%d\n", de);
#endif
  // determine required information
  if (vm==NULL)
    vm = ESMCI::VM::getCurrent(&localrc);  // get current VM context
  int localPet = vm->getLocalPet();
  // determine localDE index
  int localDeCount = array->localDeCount;
  int *localDeList = array->localDeList;
  int localDe;
  for (localDe=0; localDe<localDeCount; localDe++)
    if (localDeList[localDe] == de) break;
  // determine VAS for rootPET
  int rootVAS = vm->getVas(rootPET);
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
  vm->commwait(&ch_laLength, NULL, 1);     // need this variable in a couple of lines
  vm->commwait(&ch_laByteCount, NULL, 1);  // need this variable in a couple of lines
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
  vm->commwait(&ch_laLbound, NULL, 1);
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
      vm->commwait(&ch_buffer, NULL, 1);
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
      vm->commwait(&ch_buffer, NULL, 1);
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
  ESMCI::VM *vm = tharg->vm;
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
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          *tempResult += tempBase[i];
        }
        break;
      case ESMF_MIN:
        vm->commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
        }
        break;
      case ESMF_MAX:
        vm->commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
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
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          *tempResult += tempBase[i];
        }
        break;
      case ESMF_MIN:
        vm->commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
        }
        break;
      case ESMF_MAX:
        vm->commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
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
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          *tempResult += tempBase[i];
        }
        break;
      case ESMF_MIN:
        vm->commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
          if (tempBase[i] < *tempResult) *tempResult = tempBase[i];
        }
        break;
      case ESMF_MAX:
        vm->commwait(&(vmk_commh[0]), NULL, 1);   // complete receive, nanopause=1ns
        *tempResult = tempBase[0];          // prime the result variable
        for (int i=1; i<deCount; i++){
          vm->commwait(&(vmk_commh[i]), NULL, 1);  // complete receive, nanopause=1ns
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
