// $Id: ESMCI_Array.C,v 1.1.2.53 2009/01/21 21:25:19 cdeluca Exp $
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
#define ESMC_FILENAME "ESMCI_Array.C"
//==============================================================================
//
// ESMC Array method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array methods declared
// in the companion file ESMCI_Array.h
//
//-----------------------------------------------------------------------------

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>
#include <vector>
#include <algorithm>

// include associated header file
#include "ESMCI_Array.h"

// include ESMF headers
#include "ESMC_Start.h"

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Array.C,v 1.1.2.53 2009/01/21 21:25:19 cdeluca Exp $";
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
  bool distgridCreatorArg,                // (in)
  int *exclusiveLBoundArg,                // (in)
  int *exclusiveUBoundArg,                // (in)
  int *computationalLBoundArg,            // (in)
  int *computationalUBoundArg,            // (in)
  int *totalLBoundArg,                    // (in)
  int *totalUBoundArg,                    // (in)
  int tensorCountArg,                     // (in)
  int tensorElementCountArg,              // (in)
  int *undistLBoundArray,                 // (in)
  int *undistUBoundArray,                 // (in)
  int *staggerLocArray,                   // (in)
  int *vectorDimArray,                    // (in)
  int *distgridToArrayMapArray,           // (in)
  int *arrayToDistGridMapArray,           // (in)
  int *distgridToPackedArrayMapArray,     // (in)
  ESMC_IndexFlag indexflagArg,            // (in)
  int *rc                                 // (out)
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_Array object.
//    No error checking wrt consistency of input arguments is needed because
//    ArrayConstruct() is only to be called by ArrayCreate() interfaces which
//    are responsible for providing consistent arguments to this layer.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  try{  

  // fill in the Array object
  typekind = typekindArg;
  rank = rankArg;
  distgrid = distgridArg;
  distgridCreator = distgridCreatorArg;
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
  int redDimCount = rank - tensorCountArg; // reduced dimCount w/o repl. dims
  exclusiveLBound = new int[redDimCount*localDeCount];
  memcpy(exclusiveLBound, exclusiveLBoundArg,
    redDimCount*localDeCount*sizeof(int));
  exclusiveUBound = new int[redDimCount*localDeCount];
  memcpy(exclusiveUBound, exclusiveUBoundArg,
    redDimCount*localDeCount*sizeof(int));
  computationalLBound = new int[redDimCount*localDeCount];
  memcpy(computationalLBound, computationalLBoundArg,
    redDimCount*localDeCount*sizeof(int));
  computationalUBound = new int[redDimCount*localDeCount];
  memcpy(computationalUBound, computationalUBoundArg,
    redDimCount*localDeCount*sizeof(int));
  totalLBound = new int[redDimCount*localDeCount];
  memcpy(totalLBound, totalLBoundArg,
    redDimCount*localDeCount*sizeof(int));
  totalUBound = new int[redDimCount*localDeCount];
  memcpy(totalUBound, totalUBoundArg,
    redDimCount*localDeCount*sizeof(int));
  // tensor dimensions
  tensorCount = tensorCountArg;
  tensorElementCount = tensorElementCountArg;
  undistLBound = new int[tensorCountArg];
  memcpy(undistLBound, undistLBoundArray, tensorCountArg * sizeof(int));
  undistUBound = new int[tensorCountArg];
  memcpy(undistUBound, undistUBoundArray, tensorCountArg * sizeof(int));
  // staggerLoc and vectorDim
  staggerLoc = new int[tensorElementCount];
  memcpy(staggerLoc, staggerLocArray, tensorElementCount * sizeof(int));
  vectorDim = new int[tensorElementCount];
  memcpy(vectorDim, vectorDimArray, tensorElementCount * sizeof(int));
  // distgridToArrayMap, arrayToDistGridMap and distgridToPackedArrayMap
  int dimCount = distgrid->getDimCount();
  distgridToArrayMap = new int[dimCount];
  memcpy(distgridToArrayMap, distgridToArrayMapArray, dimCount * sizeof(int));
  arrayToDistGridMap = new int[rank];
  memcpy(arrayToDistGridMap, arrayToDistGridMapArray, rank * sizeof(int));
  distgridToPackedArrayMap = new int[dimCount];
  memcpy(distgridToPackedArrayMap, distgridToPackedArrayMapArray,
    dimCount * sizeof(int));
  // indexflag
  indexflag = indexflagArg;
  // contiguous flag
  contiguousFlag = new int[localDeCount];
  // exclusiveElementCountPDe
  int deCount = delayout->getDeCount();
  exclusiveElementCountPDe = new int[deCount];
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  for (int i=0; i<deCount; i++){
    exclusiveElementCountPDe[i] = 1;   // prime exclusiveElementCountPDe element
    for (int jj=0; jj<rank; jj++){
      int j = arrayToDistGridMap[jj];// j is dimIndex basis 1, or 0 for tensor d
      if (j){
        // decomposed dimension 
        --j;  // shift to basis 0
        exclusiveElementCountPDe[i] *= indexCountPDimPDe[i*dimCount+j];
      }
    }
  }
  // totalElementCountPLocalDe
  totalElementCountPLocalDe = new int[localDeCount];
  for (int i=0; i<localDeCount; i++){
    totalElementCountPLocalDe[i] = 1;   // prime totalElementCountPLocalDe elem
    for (int j=0; j<redDimCount; j++){
      totalElementCountPLocalDe[i] *=
        totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
    }
  }
  
  const int *localDeList = delayout->getLocalDeList();
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    contiguousFlag[i] = 1;  // initialize as contiguous
    int pI = 0; // initialize packed index
    for (int jj=0; jj<rank; jj++){
      int j = arrayToDistGridMap[jj];// j is dimIndex basis 1, or 0 for tensor d
      if (j){
        // decomposed dimension 
        if (totalLBound[i*redDimCount+pI] != exclusiveLBound[i*redDimCount+pI]
          || totalUBound[i*redDimCount+pI] !=exclusiveUBound[i*redDimCount+pI]){
          contiguousFlag[i] = 0; // reset
          break;
        }
        // obtain indexList for this DE and dim
        --j;  // shift to basis 0
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
        ++pI;
      }
    } // jj
  }
  
  // invalidate the name for this Array object in the Base class
  ESMC_BaseSetName(NULL, "Array");
   
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return;
  }
  
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
    ESMC_LocalArray::ESMC_LocalArrayDestroy(larrayList[i]);
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
  if (undistLBound != NULL)
    delete [] undistLBound;
  if (undistUBound != NULL)
    delete [] undistUBound;
  if (staggerLoc != NULL)
    delete [] staggerLoc;
  if (vectorDim != NULL)
    delete [] vectorDim;
  if (distgridToArrayMap != NULL)
    delete [] distgridToArrayMap;
  if (arrayToDistGridMap != NULL)
    delete [] arrayToDistGridMap;
  if (distgridToPackedArrayMap != NULL)
    delete [] distgridToPackedArrayMap;
  if (contiguousFlag != NULL)
    delete [] contiguousFlag;
  if (exclusiveElementCountPDe != NULL)
    delete [] exclusiveElementCountPDe;
  if (totalElementCountPLocalDe != NULL)
    delete [] totalElementCountPLocalDe;
  if (distgridCreator)
    DistGrid::destroy(&distgrid); 
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
  ESMC_DataCopy copyflag,                     // (in)
  InterfaceInt *distgridToArrayMap,           // (in)
  InterfaceInt *computationalEdgeLWidthArg,   // (in)
  InterfaceInt *computationalEdgeUWidthArg,   // (in)
  InterfaceInt *computationalLWidthArg,       // (in)
  InterfaceInt *computationalUWidthArg,       // (in)
  InterfaceInt *totalLWidthArg,               // (in)
  InterfaceInt *totalUWidthArg,               // (in)
  ESMC_IndexFlag *indexflagArg,               // (in)
  int *staggerLocArg,                         // (in)
  int *vectorDimArg,                          // (in)
  InterfaceInt *undistLBoundArg,              // (in)
  InterfaceInt *undistUBoundArg,              // (in)
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
  
  Array *array;
  try{
  
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
  // check if distgridToArrayMap was provided and matches rest of arguments
  int *distgridToArrayMapArray = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    if (i < rank)
      distgridToArrayMapArray[i] = i+1; // default (basis 1)
    else
      distgridToArrayMapArray[i] = 0;   // default (replicator dims beyond rank)
  }
  if (distgridToArrayMap != NULL){
    if (distgridToArrayMap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- distgridToArrayMap array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (distgridToArrayMap->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- distgridToArrayMap and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(distgridToArrayMapArray, distgridToArrayMap->array,
      dimCount*sizeof(int));
  }
  {
    // check distgridToArrayMapArray
    bool *check = new bool[rank];
    for (int i=0; i<rank; i++)
      check[i] = false; // initialize
    for (int i=0; i<dimCount; i++){
      if (distgridToArrayMapArray[i] < 0 || distgridToArrayMapArray[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- invalid distgridToArrayMap element", rc);
        return ESMC_NULL_POINTER;
      }
      if (distgridToArrayMapArray[i] > 0){
        if(check[distgridToArrayMapArray[i]-1]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- invalid distgridToArrayMap element", rc);
          return ESMC_NULL_POINTER;
        }
        check[distgridToArrayMapArray[i]-1] = true;
      }
    }
    delete [] check;
  }
  // determine replicatorCount
  int replicatorCount = 0;  // initialize
  for (int i=0; i<dimCount; i++)
    if (distgridToArrayMapArray[i] == 0) ++replicatorCount;
  // determine reduced dimCount -> redDimCount
  int redDimCount = dimCount - replicatorCount;
  // determine tensorCount
  int tensorCount = rank - redDimCount;
  if (tensorCount < 0) tensorCount = 0;
  // generate arrayToDistGridMap
  int *arrayToDistGridMapArray = new int[rank];
  for (int i=0; i<rank; i++)
    arrayToDistGridMapArray[i] = 0; // reset  (basis 1), 0 indicates tensor dim
  for (int i=0; i<dimCount; i++)
    if (int j=distgridToArrayMapArray[i])
      arrayToDistGridMapArray[j-1] = i+1;
  // generate distgridToPackedArrayMap - labels the distributed Array dims 1,2,.
  int *distgridToPackedArrayMap = new int[dimCount];
  for (int i=0; i<dimCount; i++)
    distgridToPackedArrayMap[i] = 0; // reset  (basis 1), 0 indicates repl. dim
  {
    int k=1;  // reset
    for (int i=0; i<rank; i++){
      if (int j=arrayToDistGridMapArray[i]){
        distgridToPackedArrayMap[j-1] = k;
        ++k;
      }
    }
  }
  // check for undistLBound and undistUBound arguments and that they match
  // tensorCount
  int undistLBoundArrayAllocFlag = 0;  // reset
  int *undistLBoundArray = NULL; // reset
  if (undistLBoundArg != NULL){
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistLBound array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (undistLBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- undistLBound, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    undistLBoundArray = undistLBoundArg->array;
  }else if (tensorCount > 0){
    // tensor dimensions are present, but no explicit bounds provided'
    // -> set to bounds of incoming array at DE 0 (should be same across DEs!)
    int *undistLBound = new int[rank];
    localrc = larrayListArg[0]->ESMC_LocalArrayGetLbounds(rank, undistLBound);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
      return ESMC_NULL_POINTER;
    undistLBoundArrayAllocFlag = 1;  // set
    undistLBoundArray = new int[tensorCount];
    int tensorIndex = 0;  // reset
    for (int i=0; i<rank; i++)
      if (arrayToDistGridMapArray[i] == 0){
        undistLBoundArray[tensorIndex] = undistLBound[i];
        ++tensorIndex;
      }
    delete [] undistLBound;
  }
  int undistUBoundArrayAllocFlag = 0;  // reset
  int *undistUBoundArray = NULL; // reset
  if (undistUBoundArg != NULL){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (undistUBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- undistUBound, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    undistUBoundArray = undistUBoundArg->array;
  }else if (tensorCount > 0){
    // tensor dimensions are present, but no explicit bounds provided
    // -> set to bounds of incoming array at DE 0
    // -> set to bounds of incoming array at DE 0 (should be same across DEs!)
    int *undistUBound = new int[rank];
    localrc = larrayListArg[0]->ESMC_LocalArrayGetUbounds(rank, undistUBound);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
      return ESMC_NULL_POINTER;
    undistUBoundArrayAllocFlag = 1;  // set
    undistUBoundArray = new int[tensorCount];
    int tensorIndex = 0;  // reset
    for (int i=0; i<rank; i++)
      if (arrayToDistGridMapArray[i] == 0){
        undistUBoundArray[tensorIndex] = undistUBound[i];
        ++tensorIndex;
      }
    delete [] undistUBound;
  }
  // tensorElementCount
  int tensorElementCount = 1;  // prime tensorElementCount
  for (int i=0; i<tensorCount; i++)
    tensorElementCount *= (undistUBoundArray[i] - undistLBoundArray[i] + 1);
  // prepare temporary staggerLoc and vectorDim arrays
  int *staggerLoc = new int[tensorElementCount];
  if (staggerLocArg)
    for (int i=0; i<tensorElementCount; i++)
      staggerLoc[i] = *staggerLocArg;
  else
    for (int i=0; i<tensorElementCount; i++)
      staggerLoc[i] = 0;
  int *vectorDim = new int[tensorElementCount];
  if (vectorDimArg)
    for (int i=0; i<tensorElementCount; i++)
      vectorDim[i] = *vectorDimArg;
  else
    for (int i=0; i<tensorElementCount; i++)
      vectorDim[i] = 1;
  
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
  int *exclusiveLBound = new int[redDimCount*localDeCount];
  int *exclusiveUBound = new int[redDimCount*localDeCount];
  for (int i=0; i<redDimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    for (int j=0; j<dimCount; j++)
      if (int k=distgridToPackedArrayMap[j])
        exclusiveUBound[i*redDimCount+k-1] = indexCountPDimPDe[de*dimCount+j];
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMF_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      for (int j=0; j<dimCount; j++){
        // check that this DE/dim has a contiguous index list
        const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
        if (!contigFlagPDimPDe[de*dimCount+j]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
            "- Cannot use non-contiguous decomposition for pseudo global"
            " index space", rc);
          return ESMC_NULL_POINTER;
        }
        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
          return ESMC_NULL_POINTER;
        // shift bounds of exclusive region to match indexList[0]
        if (indexCountPDimPDe[de*dimCount+j]){
          // only shift if there are indices associated
          if (int k=distgridToPackedArrayMap[j]){
            // only shift if this isn't a replicated dim
            int shift = indexList[0] - exclusiveLBound[i*redDimCount+k-1];
            exclusiveLBound[i*redDimCount+k-1] += shift;
            exclusiveUBound[i*redDimCount+k-1] += shift;
          }
        }
      } // j
    } // i
  }
  // deal with computationalEdge widths
  int *computationalEdgeLWidth = new int[redDimCount];
  int *computationalEdgeUWidth = new int[redDimCount];
  if (computationalEdgeLWidthArg != NULL){
    if (computationalEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalEdgeLWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalEdgeLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(computationalEdgeLWidth, computationalEdgeLWidthArg->array,
      redDimCount*sizeof(int));
  }else{
    // set default
    for (int i=0; i<redDimCount; i++)
      computationalEdgeLWidth[i] = 0;
  }
  if (computationalEdgeUWidthArg != NULL){
    if (computationalEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalEdgeUWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalEdgeUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(computationalEdgeUWidth, computationalEdgeUWidthArg->array,
      redDimCount*sizeof(int));
  }else{
    // set default
    for (int i=0; i<redDimCount; i++)
      computationalEdgeUWidth[i] = 0;
  }
  // deal with computational widths
  int *computationalLBound = new int[redDimCount*localDeCount];
  int *computationalUBound = new int[redDimCount*localDeCount];
  if (computationalLWidthArg != NULL){
    if (computationalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalLBound[j*redDimCount+i] = exclusiveLBound[j*redDimCount+i]
          - computationalLWidthArg->array[i];
  }else{
    // set default
    memcpy(computationalLBound, exclusiveLBound,
      localDeCount*redDimCount*sizeof(int));
  }
  if (computationalUWidthArg != NULL){
    if (computationalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalUBound[j*redDimCount+i] = exclusiveUBound[j*redDimCount+i]
          + computationalUWidthArg->array[i];
  }else{
    // set default
    memcpy(computationalUBound, exclusiveUBound,
      localDeCount*redDimCount*sizeof(int));
  }
  // modify computational bounds on patch edges
  for (int j=0; j<localDeCount; j++){
    for (int i=0; i<dimCount; i++){
      bool onEdgeL = distgrid->isLocalDeOnEdgeL(j, i+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
        return ESMC_NULL_POINTER;
      if (onEdgeL)
        if (int k=distgridToPackedArrayMap[i])
          computationalLBound[j*redDimCount+k-1]
            -= computationalEdgeLWidth[k-1];
      bool onEdgeU = distgrid->isLocalDeOnEdgeU(j, i+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
        return ESMC_NULL_POINTER;
      if (onEdgeU)
        if (int k=distgridToPackedArrayMap[i])
          computationalUBound[j*redDimCount+k-1]
            += computationalEdgeUWidth[k-1];
    }
  }
  // clean-up
  delete [] computationalEdgeLWidth;
  delete [] computationalEdgeUWidth;
  // deal with total widths
  int totalLBoundFlag = 0;  // reset
  int totalUBoundFlag = 0;  // reset
  int *totalLBound = new int[redDimCount*localDeCount];
  int *totalUBound = new int[redDimCount*localDeCount];
  if (totalLWidthArg != NULL){
    totalLBoundFlag = 1;  // set
    if (totalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalLWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalLWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*redDimCount+i] = computationalLBound[j*redDimCount+i]
          - totalLWidthArg->array[i];
      }
    }
  }else{
    // set default
    for (int i=0; i<localDeCount*redDimCount; i++)
      totalLBound[i] = computationalLBound[i];
  }
  if (totalUWidthArg != NULL){
    totalUBoundFlag = 1;  // set
    if (totalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalUWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalUWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*redDimCount+i] = computationalUBound[j*redDimCount+i]
          + totalUWidthArg->array[i];
      }
    }
  }else{
    // set default
    for (int i=0; i<localDeCount*redDimCount; i++)
      totalUBound[i] = computationalUBound[i];
  }
  // total region _must_ cover exclusive region -> adjust total region if not so
  for (int i=0; i<localDeCount*redDimCount; i++){
    if (exclusiveLBound[i] < totalLBound[i])
      totalLBound[i] = exclusiveLBound[i];
    if (exclusiveUBound[i] > totalUBound[i])
      totalUBound[i] = exclusiveUBound[i];
  }
  
  // check computational bounds against total bounds
  for (int i=0; i<localDeCount*redDimCount; i++){
    if (totalLBound[i] <= totalUBound[i]){
      // DE/dim is associated with DistGrid elements
      if (computationalLBound[i] < totalLBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaLBound / totalLBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalLBound[i] > totalUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaLBound / totalUBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] < totalLBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUBound / totalLBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] > totalUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUBound / totalUBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
    }
  }

  // allocate LocalArray list that holds all PET-local DEs and adjust elements
  ESMC_LocalArray **larrayList = new ESMC_LocalArray*[localDeCount];
  int *temp_counts = new int[rank];
  int *temp_larrayLBound = new int[rank];
  int *temp_larrayUBound = new int[rank];
  for (int i=0; i<localDeCount; i++){
    if (indexflag == ESMF_INDEX_USER){
      // don't adjust dope vector, use F90 pointers directly and their bounds
      larrayListArg[i]->ESMC_LocalArrayGetCounts(rank, temp_counts);
      int j=0;    // reset distributed index
      int jjj=0;  // reset undistributed index
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          if (temp_counts[jj] < 
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
              "- LocalArray does not accommodate requested element count", rc);
            return ESMC_NULL_POINTER;
          }
          // move the total bounds according to input info
          if (totalLBoundFlag){
            // totalLBound fixed
            temp_larrayLBound[jj] = totalLBound[i*redDimCount+j];
            if (totalUBoundFlag){
              // totalUBound fixed
              temp_larrayUBound[jj] = totalUBound[i*redDimCount+j];
            }else{
              // totalUBound not fixed
              temp_larrayUBound[jj] = temp_counts[jj]
                + totalLBound[i*redDimCount+j] - 1;
            }
          }else{
            // totalLBound not fixed
            if (totalUBoundFlag){
              // totalUBound fixed
              temp_larrayUBound[jj] = totalUBound[i*redDimCount+j];
              temp_larrayLBound[jj] = totalUBound[i*redDimCount+j]
                - temp_counts[jj] + 1;
            }else{
              // totalLBound and totalUBound not fixed
              // -> shift computational/excl. region into center of total region
              int lBound = computationalLBound[i*redDimCount+j];
              if (exclusiveLBound[i*redDimCount+j] 
                < computationalLBound[i*redDimCount+j])
                lBound = exclusiveLBound[i*redDimCount+j];
              int uBound = computationalUBound[i*redDimCount+j];
              if (exclusiveUBound[i*redDimCount+j]
                > computationalUBound[i*redDimCount+j])
                uBound = exclusiveUBound[i*redDimCount+j];
              temp_larrayLBound[jj] = lBound
                - (int)(0.5 * (temp_counts[jj] - 1 + lBound - uBound));
              temp_larrayUBound[jj] = temp_counts[jj] + temp_larrayLBound[jj]
                - 1;
            }
          }
          totalLBound[i*redDimCount+j] = temp_larrayLBound[jj]; // write back
          totalUBound[i*redDimCount+j] = temp_larrayUBound[jj]; // write back
          // now bounds must match exactly or it's an error
          if (temp_counts[jj] !=
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
              "- LocalArray does not match requested element count", rc);
            return ESMC_NULL_POINTER;
          }
          ++j;
        }else{
          // non-distributed dimension
          if (temp_counts[jj] !=
            undistUBoundArray[jjj] - undistLBoundArray[jjj] + 1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
              "- LocalArray does not match requested element count", rc);
            return ESMC_NULL_POINTER;
          }
          ++jjj;
        }
      }
      // adjust all of the bounds to match absolute bounds of F90 pointers
      int *larrayLBound = new int[rank];
      localrc = larrayListArg[i]->ESMC_LocalArrayGetLbounds(rank, larrayLBound);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
        return ESMC_NULL_POINTER;
      j=0;    // reset distributed index
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          int shift = larrayLBound[jj] - totalLBound[i*redDimCount+j];
          totalLBound[i*redDimCount+j] += shift;
          totalUBound[i*redDimCount+j] += shift;
          computationalLBound[i*redDimCount+j] += shift;
          computationalUBound[i*redDimCount+j] += shift;
          exclusiveLBound[i*redDimCount+j] += shift;
          exclusiveUBound[i*redDimCount+j] += shift;
          ++j;
        }
      }      
      delete [] larrayLBound;
      // use F90 pointers directly
      larrayList[i] = larrayListArg[i];
    }else{
      // prepare to adjust dope vector
      larrayListArg[i]->ESMC_LocalArrayGetCounts(rank, temp_counts);
      int j=0;    // reset distributed index
      int jjj=0;  // reset undistributed index
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          if (temp_counts[jj] < 
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
              "- LocalArray does not accommodate requested element count", rc);
            return ESMC_NULL_POINTER;
          }
          // move the total bounds according to input info
          if (totalLBoundFlag){
            // totalLBound fixed
            temp_larrayLBound[jj] = totalLBound[i*redDimCount+j];
            if (totalUBoundFlag){
              // totalUBound fixed
              // this case allows total alloc. to be larger than total region,
              // but LocalArrayAdjust() method will ultimately prevent this by
              // returning an error if counts are not equal to
              // ubounds-lbounds+1.
              temp_larrayUBound[jj] = totalUBound[i*redDimCount+j];
            }else{
              // totalUBound not fixed
              temp_larrayUBound[jj] = temp_counts[jj]
                + totalLBound[i*redDimCount+j] - 1;
            }
          }else{
            // totalLBound not fixed
            if (totalUBoundFlag){
              // totalUBound fixed
              temp_larrayUBound[jj] = totalUBound[i*redDimCount+j];
              temp_larrayLBound[jj] = totalUBound[i*redDimCount+j]
                - temp_counts[jj] + 1;
            }else{
              // totalLBound and totalUBound not fixed
              // -> shift computational/excl. region into center of total region
              int lBound = computationalLBound[i*redDimCount+j];
              if (exclusiveLBound[i*redDimCount+j] 
                < computationalLBound[i*redDimCount+j])
                lBound = exclusiveLBound[i*redDimCount+j];
              int uBound = computationalUBound[i*redDimCount+j];
              if (exclusiveUBound[i*redDimCount+j]
                > computationalUBound[i*redDimCount+j])
                uBound = exclusiveUBound[i*redDimCount+j];
              temp_larrayLBound[jj] = lBound
                - (int)(0.5 * (temp_counts[jj] - 1 + lBound - uBound));
              temp_larrayUBound[jj] = temp_counts[jj] + temp_larrayLBound[jj]
                - 1;
            }
          }
          totalLBound[i*redDimCount+j] = temp_larrayLBound[jj]; // write back
          totalUBound[i*redDimCount+j] = temp_larrayUBound[jj]; // write back
          ++j;
        }else{
          // non-distributed dimension
          if (temp_counts[jj] !=
            undistUBoundArray[jjj] - undistLBoundArray[jjj] + 1){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
              "- LocalArray does not match requested element count", rc);
            return ESMC_NULL_POINTER;
          }
          temp_larrayLBound[jj] = undistLBoundArray[jjj];
          temp_larrayUBound[jj] = undistUBoundArray[jjj];
          ++jjj;
        }
      }
      // Adjust LocalArray object for specific undistLBound and undistUBound.
      // This will alloc. memory for a _new_ LocalArray object for each element.
      // Depending on copyflag the original memory used for data storage will be
      // referenced or a copy of the data will be made.
      larrayList[i] = larrayListArg[i]->
        ESMC_LocalArrayAdjust(copyflag, temp_larrayLBound, temp_larrayUBound,
          &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
        return ESMC_NULL_POINTER;
    }        
  }
  delete [] temp_counts;
  delete [] temp_larrayLBound;
  delete [] temp_larrayUBound;
  
  // call class constructor
  try{
    array = new Array(typekind, rank, larrayList, distgrid, false,
      exclusiveLBound, exclusiveUBound, computationalLBound,
      computationalUBound, totalLBound, totalUBound, tensorCount,
      tensorElementCount, undistLBoundArray, undistUBoundArray, staggerLoc,
      vectorDim, distgridToArrayMapArray, arrayToDistGridMapArray,
      distgridToPackedArrayMap, indexflag, &localrc);
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
  delete [] distgridToArrayMapArray;
  delete [] arrayToDistGridMapArray;
  delete [] distgridToPackedArrayMap;
  delete [] staggerLoc;
  delete [] vectorDim;
  if (undistLBoundArrayAllocFlag) delete [] undistLBoundArray;
  if (undistUBoundArrayAllocFlag) delete [] undistUBoundArray;
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return ESMC_NULL_POINTER;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
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
  ArraySpec *arrayspec,                       // (in)
  DistGrid *distgrid,                         // (in)
  InterfaceInt *distgridToArrayMap,           // (in)
  InterfaceInt *computationalEdgeLWidthArg,   // (in)
  InterfaceInt *computationalEdgeUWidthArg,   // (in)
  InterfaceInt *computationalLWidthArg,       // (in)
  InterfaceInt *computationalUWidthArg,       // (in)
  InterfaceInt *totalLWidthArg,               // (in)
  InterfaceInt *totalUWidthArg,               // (in)
  ESMC_IndexFlag *indexflagArg,               // (in)
  InterfaceInt *distLBoundArg,                // (in)
  int *staggerLocArg,                         // (in)
  int *vectorDimArg,                          // (in)
  InterfaceInt *undistLBoundArg,              // (in)
  InterfaceInt *undistUBoundArg,              // (in)
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
  
  Array *array;
  try{
  
  // check the input and get the information together to call construct()
  // arrayspec -> typekind/rank
  if (arrayspec == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to arrayspec", rc);
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind typekind = arrayspec->getTypeKind(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
    return ESMC_NULL_POINTER;
  int rank = arrayspec->getRank(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
    return ESMC_NULL_POINTER;
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to distgrid", rc);
    return ESMC_NULL_POINTER;
  }
  const DELayout *delayout = distgrid->getDELayout();
  int dimCount = distgrid->getDimCount();
  // check if distgridToArrayMap was provided and matches rest of arguments
  int *distgridToArrayMapArray = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    if (i < rank)
      distgridToArrayMapArray[i] = i+1; // default (basis 1)
    else
      distgridToArrayMapArray[i] = 0;   // default (replicator dims beyond rank)
  }
  if (distgridToArrayMap != NULL){
    if (distgridToArrayMap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- distgridToArrayMap array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (distgridToArrayMap->extent[0] != dimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- distgridToArrayMap and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(distgridToArrayMapArray, distgridToArrayMap->array,
      dimCount*sizeof(int));
  }
  {
    // check distgridToArrayMapArray
    bool *check = new bool[rank];
    for (int i=0; i<rank; i++)
      check[i] = false; // initialize
    for (int i=0; i<dimCount; i++){
      if (distgridToArrayMapArray[i] < 0 || distgridToArrayMapArray[i] > rank){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- invalid distgridToArrayMap element", rc);
        return ESMC_NULL_POINTER;
      }
      if (distgridToArrayMapArray[i] > 0){
        if(check[distgridToArrayMapArray[i]-1]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
            "- invalid distgridToArrayMap element", rc);
          return ESMC_NULL_POINTER;
        }
        check[distgridToArrayMapArray[i]-1] = true;
      }
    }
    delete [] check;
  }
  // determine replicatorCount
  int replicatorCount = 0;  // initialize
  for (int i=0; i<dimCount; i++)
    if (distgridToArrayMapArray[i] == 0) ++replicatorCount;
  // determine reduced dimCount -> redDimCount
  int redDimCount = dimCount - replicatorCount;
  // determine tensorCount
  int tensorCount = rank - redDimCount;
  if (tensorCount < 0) tensorCount = 0;
  // generate arrayToDistGridMap
  int *arrayToDistGridMapArray = new int[rank];
  for (int i=0; i<rank; i++)
    arrayToDistGridMapArray[i] = 0; // reset  (basis 1), 0 indicates tensor dim
  for (int i=0; i<dimCount; i++)
    if (int j=distgridToArrayMapArray[i])
      arrayToDistGridMapArray[j-1] = i+1;
  // generate distgridToPackedArrayMap - labels the distributed Array dims 1,2,.
  int *distgridToPackedArrayMap = new int[dimCount];
  for (int i=0; i<dimCount; i++)
    distgridToPackedArrayMap[i] = 0; // reset  (basis 1), 0 indicates repl. dim
  {
    int k=1;  // reset
    for (int i=0; i<rank; i++){
      if (int j=arrayToDistGridMapArray[i]){
        distgridToPackedArrayMap[j-1] = k;
        ++k;
      }
    }
  }
  // check for undistLBound and undistUBound arguments and that they match
  // tensorCount
  if (tensorCount > 0 && undistLBoundArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid undistLBound argument required to create Array with tensor dims",
      rc);
    return ESMC_NULL_POINTER;
  }
  if (tensorCount > 0 && undistUBoundArg == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Valid undistUBound argument required to create Array with tensor dims",
      rc);
    return ESMC_NULL_POINTER;
  }
  int *undistLBoundArray = NULL; // reset
  if (undistLBoundArg != NULL){
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistLBound array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (undistLBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- undistLBound, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    undistLBoundArray = undistLBoundArg->array;
  }
  int *undistUBoundArray = NULL; // reset
  if (undistUBoundArg != NULL){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- undistUBound array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (undistUBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- undistUBound, arrayspec, distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    undistUBoundArray = undistUBoundArg->array;
  }
  // tensorElementCount
  int tensorElementCount = 1;  // prime tensorElementCount
  for (int i=0; i<tensorCount; i++)
    tensorElementCount *= (undistUBoundArray[i] - undistLBoundArray[i] + 1);
  // prepare temporary staggerLoc and vectorDim arrays
  int *staggerLoc = new int[tensorElementCount];
  if (staggerLocArg)
    for (int i=0; i<tensorElementCount; i++)
      staggerLoc[i] = *staggerLocArg;
  else
    for (int i=0; i<tensorElementCount; i++)
      staggerLoc[i] = 0;
  int *vectorDim = new int[tensorElementCount];
  if (vectorDimArg)
    for (int i=0; i<tensorElementCount; i++)
      vectorDim[i] = *vectorDimArg;
  else
    for (int i=0; i<tensorElementCount; i++)
      vectorDim[i] = 1;
  
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
  // check that presence of distLBoundArg is consistent with indexflag
  if(indexflag == ESMF_INDEX_USER){
    if (distLBoundArg == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- distLBoundArg required in ESMF_INDEX_USER mode", rc);
      return ESMC_NULL_POINTER;
    }
  }else{
    if (distLBoundArg != NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- distLBoundArg must only be specified in ESMF_INDEX_USER mode", rc);
      return ESMC_NULL_POINTER;
    }
  }
  // figure exclusive region
  int *exclusiveLBound = new int[redDimCount*localDeCount];
  int *exclusiveUBound = new int[redDimCount*localDeCount];
  for (int i=0; i<redDimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    for (int j=0; j<dimCount; j++)
      if (int k=distgridToPackedArrayMap[j])
        exclusiveUBound[i*redDimCount+k-1] = indexCountPDimPDe[de*dimCount+j];
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMF_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      for (int j=0; j<dimCount; j++){
        // check that this DE/dim has a contiguous index list
        const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
        if (!contigFlagPDimPDe[de*dimCount+j]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
            "- Cannot use non-contiguous decomposition for pseudo global"
            " index space", rc);
          return ESMC_NULL_POINTER;
        }
        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
          return ESMC_NULL_POINTER;
        // shift bounds of exclusive region to match indexList[0]
        if (indexCountPDimPDe[de*dimCount+j]){
          // only shift if there are indices associated
          if (int k=distgridToPackedArrayMap[j]){
            // only shift if this isn't a replicated dim
            int shift = indexList[0] - exclusiveLBound[i*redDimCount+k-1];
            exclusiveLBound[i*redDimCount+k-1] += shift;
            exclusiveUBound[i*redDimCount+k-1] += shift;
          }
        }
      } // j
    } // i
  }
  // deal with computationalEdge widths
  int *computationalEdgeLWidth = new int[redDimCount];
  int *computationalEdgeUWidth = new int[redDimCount];
  if (computationalEdgeLWidthArg != NULL){
    if (computationalEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalEdgeLWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalEdgeLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(computationalEdgeLWidth, computationalEdgeLWidthArg->array,
      redDimCount*sizeof(int));
  }else{
    // set default
    for (int i=0; i<redDimCount; i++)
      computationalEdgeLWidth[i] = 0;
  }
  if (computationalEdgeUWidthArg != NULL){
    if (computationalEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalEdgeUWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalEdgeUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(computationalEdgeUWidth, computationalEdgeUWidthArg->array,
      redDimCount*sizeof(int));
  }else{
    // set default
    for (int i=0; i<redDimCount; i++)
      computationalEdgeUWidth[i] = 0;
  }
  // deal with computational widths
  int *computationalLBound = new int[redDimCount*localDeCount];
  int *computationalUBound = new int[redDimCount*localDeCount];
  if (computationalLWidthArg != NULL){
    if (computationalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalLBound[j*redDimCount+i] = exclusiveLBound[j*redDimCount+i]
          - computationalLWidthArg->array[i];
  }else{
    // set default
    memcpy(computationalLBound, exclusiveLBound,
      localDeCount*redDimCount*sizeof(int));
  }
  if (computationalUWidthArg != NULL){
    if (computationalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- computationalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalUBound[j*redDimCount+i] = exclusiveUBound[j*redDimCount+i]
          + computationalUWidthArg->array[i];
  }else{
    // set default
    memcpy(computationalUBound, exclusiveUBound,
      localDeCount*redDimCount*sizeof(int));
  }
  // modify computational bounds on patch edges
  for (int j=0; j<localDeCount; j++){
    for (int i=0; i<dimCount; i++){
      bool onEdgeL = distgrid->isLocalDeOnEdgeL(j, i+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
        return ESMC_NULL_POINTER;
      if (onEdgeL)
        if (int k=distgridToPackedArrayMap[i])
          computationalLBound[j*redDimCount+k-1]
            -= computationalEdgeLWidth[k-1];
      bool onEdgeU = distgrid->isLocalDeOnEdgeU(j, i+1, &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,ESMF_ERR_PASSTHRU,rc))
        return ESMC_NULL_POINTER;
      if (onEdgeU)
        if (int k=distgridToPackedArrayMap[i])
          computationalUBound[j*redDimCount+k-1]
            += computationalEdgeUWidth[k-1];
    }
  }
  // clean-up
  delete [] computationalEdgeLWidth;
  delete [] computationalEdgeUWidth;
  // deal with total widths
  int *totalLBound = new int[redDimCount*localDeCount];
  int *totalUBound = new int[redDimCount*localDeCount];
  if (totalLWidthArg != NULL){
    if (totalLWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalLWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalLWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalLWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*redDimCount+i] = computationalLBound[j*redDimCount+i]
          - totalLWidthArg->array[i];
      }
    }
  }else{
    // set default
    for (int i=0; i<localDeCount*redDimCount; i++)
      totalLBound[i] = computationalLBound[i];
  }
  if (totalUWidthArg != NULL){
    if (totalUWidthArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- totalUWidth array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- totalUWidth and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- totalUWidth may only contain positive values", rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*redDimCount+i] = computationalUBound[j*redDimCount+i]
          + totalUWidthArg->array[i];
      }
    }
  }else{
    // set default
    for (int i=0; i<localDeCount*redDimCount; i++)
      totalUBound[i] = computationalUBound[i];
  }
  // total region _must_ cover exclusive region -> adjust total region if not so
  for (int i=0; i<localDeCount*redDimCount; i++){
    if (exclusiveLBound[i] < totalLBound[i])
      totalLBound[i] = exclusiveLBound[i];
    if (exclusiveUBound[i] > totalUBound[i])
      totalUBound[i] = exclusiveUBound[i];
  }
  
  // check computational bounds against total bounds
  for (int i=0; i<localDeCount*redDimCount; i++){
    if (totalLBound[i] <= totalUBound[i]){
      // DE/dim is associated with DistGrid elements
      if (computationalLBound[i] < totalLBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaLBound / totalLBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalLBound[i] > totalUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaLBound / totalUBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] < totalLBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUBound / totalLBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] > totalUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationalUBound / totalUBound mismatch", rc);
        return ESMC_NULL_POINTER;
      }
    }
  }
  
  // optionally shift to supplied lower bounds
  if (distLBoundArg != NULL){
    if (distLBoundArg->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- distLBoundArg array must be of rank 1", rc);
      return ESMC_NULL_POINTER;
    }
    if (distLBoundArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- distLBoundArg and distgrid mismatch", rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<localDeCount; i++){
      int j=0;    // reset distributed index
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          // shift all bounds to be consistent with supplied lower bounds
          int shift = distLBoundArg->array[jj] - totalLBound[i*redDimCount+j];
          totalLBound[i*redDimCount+j] += shift;
          totalUBound[i*redDimCount+j] += shift;
          computationalLBound[i*redDimCount+j] += shift;
          computationalUBound[i*redDimCount+j] += shift;
          exclusiveLBound[i*redDimCount+j] += shift;
          exclusiveUBound[i*redDimCount+j] += shift;
          ++j;
        }
      }
    }
  }
  
  // allocate LocalArray list that holds all PET-local DEs
  ESMC_LocalArray **larrayList = new ESMC_LocalArray*[localDeCount];
  int *temp_counts = new int[rank];
  int *temp_larrayLBound = new int[rank];
  int *temp_larrayUBound = new int[rank];
  for (int i=0; i<localDeCount; i++){
    int j=0;    // reset distributed index
    int jjj=0;  // reset undistributed index
    for (int jj=0; jj<rank; jj++){
      if (arrayToDistGridMapArray[jj]){
        // distributed dimension
        temp_counts[jj] =
          totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
        temp_larrayLBound[jj] = totalLBound[i*redDimCount+j];
        temp_larrayUBound[jj] = totalUBound[i*redDimCount+j];
        ++j;
      }else{
        // non-distributed dimension
        temp_counts[jj] = undistUBoundArray[jjj] - undistLBoundArray[jjj] + 1;
        temp_larrayLBound[jj] = undistLBoundArray[jjj];
        temp_larrayUBound[jj] = undistUBoundArray[jjj];
        ++jjj;
      }
    }
    // allocate LocalArray object with specific undistLBound and undistUBound
    larrayList[i] = ESMC_LocalArray::ESMC_LocalArrayCreate(rank, typekind,
      temp_counts, temp_larrayLBound, temp_larrayUBound, NULL, ESMC_DATA_REF,
      NULL, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }
  delete [] temp_counts;
  delete [] temp_larrayLBound;
  delete [] temp_larrayUBound;
  
  // call class constructor
  try{
    array = new Array(typekind, rank, larrayList, distgrid, false,
      exclusiveLBound, exclusiveUBound, computationalLBound,
      computationalUBound, totalLBound, totalUBound, tensorCount,
      tensorElementCount, undistLBoundArray, undistUBoundArray, staggerLoc,
      vectorDim, distgridToArrayMapArray, arrayToDistGridMapArray,
      distgridToPackedArrayMap, indexflag, &localrc);
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
  delete [] distgridToArrayMapArray;
  delete [] arrayToDistGridMapArray;
  delete [] distgridToPackedArrayMap;
  delete [] staggerLoc;
  delete [] vectorDim;
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return ESMC_NULL_POINTER;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
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
//    ESMC_Array * to newly allocated ESMC_Array
//
// !ARGUMENTS:
//
  Array *arrayIn,                             // (in) Array to copy
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMC\_Array} object as copy of an existing
//    {\tt ESMC\_Array} object.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  Array *arrayOut;
  
  try{
    // get an allocation for the new Array object
    try{
      arrayOut = new Array();
    }catch(...){
      // allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::Array.", rc);  
      return ESMC_NULL_POINTER;
    }
    // copy all scalar members and reference members
    arrayOut->typekind = arrayIn->typekind;
    int rank =
      arrayOut->rank = arrayIn->rank;
    arrayOut->indexflag = arrayIn->indexflag;
    int tensorCount =
      arrayOut->tensorCount = arrayIn->tensorCount;
    int tensorElementCount = 
      arrayOut->tensorElementCount = arrayIn->tensorElementCount;
    arrayOut->distgrid = arrayIn->distgrid; // copy reference
    arrayOut->distgridCreator = false;      // not a locally created object
    arrayOut->delayout = arrayIn->delayout; // copy reference
    // deep copy of members with allocations
    // copy the PET-local LocalArray pointers
    int localDeCount = arrayIn->delayout->getLocalDeCount();
    arrayOut->larrayList = new ESMC_LocalArray*[localDeCount];
    for (int i=0; i<localDeCount; i++){
      arrayOut->larrayList[i] =
        ESMC_LocalArray::ESMC_LocalArrayCreate(arrayIn->larrayList[i],
        &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
        return ESMC_NULL_POINTER;
    }
    // determine the base addresses of the local arrays:
    arrayOut->larrayBaseAddrList = new void*[localDeCount];
    for (int i=0; i<localDeCount; i++)
      arrayOut->larrayList[i]
        ->ESMC_LocalArrayGetBaseAddr((void **)
        &(arrayOut->larrayBaseAddrList[i]));
    // copy the PET-local bound arrays
    int redDimCount = rank - tensorCount;
    arrayOut->exclusiveLBound = new int[redDimCount*localDeCount];
    memcpy(arrayOut->exclusiveLBound, arrayIn->exclusiveLBound,
      redDimCount*localDeCount*sizeof(int));
    arrayOut->exclusiveUBound = new int[redDimCount*localDeCount];
    memcpy(arrayOut->exclusiveUBound, arrayIn->exclusiveUBound,
      redDimCount*localDeCount*sizeof(int));
    arrayOut->computationalLBound = new int[redDimCount*localDeCount];
    memcpy(arrayOut->computationalLBound, arrayIn->computationalLBound,
      redDimCount*localDeCount*sizeof(int));
    arrayOut->computationalUBound = new int[redDimCount*localDeCount];
    memcpy(arrayOut->computationalUBound, arrayIn->computationalUBound,
      redDimCount*localDeCount*sizeof(int));
    arrayOut->totalLBound = new int[redDimCount*localDeCount];
    memcpy(arrayOut->totalLBound, arrayIn->totalLBound,
      redDimCount*localDeCount*sizeof(int));
    arrayOut->totalUBound = new int[redDimCount*localDeCount];
    memcpy(arrayOut->totalUBound, arrayIn->totalUBound,
      redDimCount*localDeCount*sizeof(int));
    // tensor dimensions
    arrayOut->undistLBound = new int[tensorCount];
    memcpy(arrayOut->undistLBound, arrayIn->undistLBound,
      tensorCount * sizeof(int));
    arrayOut->undistUBound = new int[tensorCount];
    memcpy(arrayOut->undistUBound, arrayIn->undistUBound,
      tensorCount * sizeof(int));
    // staggerLoc and vectorDim
    arrayOut->staggerLoc = new int[tensorElementCount];
    memcpy(arrayOut->staggerLoc, arrayIn->staggerLoc,
      tensorElementCount * sizeof(int));
    arrayOut->vectorDim = new int[tensorElementCount];
    memcpy(arrayOut->vectorDim, arrayIn->vectorDim,
      tensorElementCount * sizeof(int));
    // distgridToArrayMap, arrayToDistGridMap and distgridToPackedArrayMap
    int dimCount = arrayIn->distgrid->getDimCount();
    arrayOut->distgridToArrayMap = new int[dimCount];
    memcpy(arrayOut->distgridToArrayMap, arrayIn->distgridToArrayMap,
      dimCount * sizeof(int));
    arrayOut->arrayToDistGridMap = new int[rank];
    memcpy(arrayOut->arrayToDistGridMap, arrayIn->arrayToDistGridMap,
      rank * sizeof(int));
    arrayOut->distgridToPackedArrayMap = new int[dimCount];
    memcpy(arrayOut->distgridToPackedArrayMap,
      arrayIn->distgridToPackedArrayMap, dimCount * sizeof(int));
    // contiguous flag
    arrayOut->contiguousFlag = new int[localDeCount];
    memcpy(arrayOut->contiguousFlag, arrayIn->contiguousFlag,
      localDeCount * sizeof(int));
    // exclusiveElementCountPDe
    int deCount = arrayIn->delayout->getDeCount();
    arrayOut->exclusiveElementCountPDe = new int[deCount];
    memcpy(arrayOut->exclusiveElementCountPDe,
      arrayIn->exclusiveElementCountPDe, deCount * sizeof(int));
    // totalElementCountPLocalDe
    arrayOut->totalElementCountPLocalDe = new int[localDeCount];
    memcpy(arrayOut->totalElementCountPLocalDe,
      arrayIn->totalElementCountPLocalDe, localDeCount * sizeof(int));
    // invalidate the name for this Array object in the Base class
    arrayOut->ESMC_BaseSetName(NULL, "Array");
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return ESMC_NULL_POINTER;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arrayOut;
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
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{

  // return with errors for NULL pointer
  if (array == ESMC_NULL_POINTER || *array == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", &rc);
    return rc;
  }

  // delete Array object
  delete *array;
  *array = ESMC_NULL_POINTER;
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
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
  int *index,                       // in - DE-local index tuple in exclusive
                                    //      region basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get linear index - assuming index input to be basis 0 in excl. region
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // determine the linearized index
  int redDimCount = rank - tensorCount;
  int joff = localDe*redDimCount;   // offset according to localDe index
  int linIndex = 0;                 // reset
  int tensorIndex = tensorCount-1;  // reset
  int packedIndex = redDimCount-1;  // reset
  for (int jj=rank-1; jj>=0; jj--){
    if (arrayToDistGridMap[jj]){
      // decomposed dimension 
      int j = packedIndex;
      // first time multiply with zero intentionally:
      linIndex *= totalUBound[joff+j] - totalLBound[joff+j] + 1;
      linIndex += exclusiveLBound[joff+j] - totalLBound[joff+j] + index[jj];    
      --packedIndex;
    }else{
      // tensor dimension
      // first time multiply with zero intentionally:
      linIndex *= undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1;
      linIndex += index[jj];
      --tensorIndex;
    }
  }
   
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return linIndex;    // leave this index basis 0
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getSequenceIndexExclusive()"
//BOPI
// !IROUTINE:  ESMCI::Array::getSequenceIndexExclusive
//
// !INTERFACE:
SeqIndex Array::getSequenceIndexExclusive(
//
// !RETURN VALUE:
//    SeqIndex sequence index
//
// !ARGUMENTS:
//
  int localDe,                      // in - local DE
  int *index,                       // in - DE-local index tuple in exclusive
                                    //      region basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index - assuming index input to be basis 0 in excl. region
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // initialize seqIndex
  SeqIndex seqIndex;
  seqIndex.decompSeqIndex = seqIndex.tensorSeqIndex = -1;

  // prepare decompIndex for decomposed dimensions in the DistGrid order
  int dimCount = distgrid->getDimCount();
  int *decompIndex = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    if (distgridToArrayMap[i] > 0){
      // DistGrid dim associated with Array dim
      decompIndex[i] = index[distgridToArrayMap[i]-1];
    }else{
      // DistGrid dim _not_ associated with Array dim
      decompIndex[i] = 0; // use smallest seq index for replicated dims
    }
  }
  // determine the sequentialized index for decomposed dimensions
  int decompSeqIndex;
  decompSeqIndex = distgrid->getSequenceIndexLocalDe(localDe, decompIndex,
    &localrc);  
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return seqIndex;
  seqIndex.decompSeqIndex = decompSeqIndex;
  
  // garbage collection
  delete [] decompIndex;
        
  // determine the sequentialized index for tensor dimensions
  int tensorSeqIndex = 0;             // reset
  int tensorIndex = tensorCount - 1;  // reset
  for (int jj=rank-1; jj>=0; jj--){
    if (arrayToDistGridMap[jj]==0){
      // tensor dimension
      // first time multiply with zero intentionally:
      tensorSeqIndex *= undistUBound[tensorIndex] - undistLBound[tensorIndex]
      + 1;
      tensorSeqIndex += index[jj];
      --tensorIndex;
    }
  }
  ++tensorSeqIndex; // shift tensor sequentialized index to basis 1 !!!!
  seqIndex.tensorSeqIndex = tensorSeqIndex;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqIndex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getSequenceIndexPatch()"
//BOPI
// !IROUTINE:  ESMCI::Array::getSequenceIndexPatch
//
// !INTERFACE:
SeqIndex Array::getSequenceIndexPatch(
//
// !RETURN VALUE:
//    SeqIndex sequence index
//
// !ARGUMENTS:
//
  int patch,                        // in - patch = {1,..., patchCount}
  int *index,                       // in - index tuple within patch 
                                    //    - basis min (not basis 0)
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index - assuming index input to be basis 0 in patch region
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  // initialize seqIndex
  SeqIndex seqIndex;
  seqIndex.decompSeqIndex = seqIndex.tensorSeqIndex = -1;

  // prepare decompIndex for decomposed dimensions in the DistGrid order
  int dimCount = distgrid->getDimCount();
  int *decompIndex = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    if (distgridToArrayMap[i] > 0){
      // DistGrid dim associated with Array dim
      decompIndex[i] = index[distgridToArrayMap[i]-1];
    }else{
      // DistGrid dim _not_ associated with Array dim
      decompIndex[i] = distgrid->getMinIndexPDimPPatch()[(patch-1)*dimCount+i];
      // use smallest seq index for replicated dims
    }
  }
  // determine the sequentialized index for decomposed dimensions
  int decompSeqIndex;
  decompSeqIndex = distgrid->getSequenceIndexPatch(patch, decompIndex,
    &localrc);  
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return seqIndex;
  seqIndex.decompSeqIndex = decompSeqIndex;
  
  // garbage collection
  delete [] decompIndex;
        
  // determine the sequentialized index for tensor dimensions
  int tensorSeqIndex = 0;             // reset
  int tensorIndex = tensorCount - 1;  // reset
  for (int jj=rank-1; jj>=0; jj--){
    if (arrayToDistGridMap[jj]==0){
      // tensor dimension
      // first time multiply with zero intentionally:
      tensorSeqIndex *= undistUBound[tensorIndex] - undistLBound[tensorIndex]
      + 1;
      tensorSeqIndex += index[jj] - undistLBound[tensorIndex];
      --tensorIndex;
    }
  }
  ++tensorSeqIndex; // shift tensor sequentialized index to basis 1 !!!!
  seqIndex.tensorSeqIndex = tensorSeqIndex;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqIndex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::setComputationalLWidth()"
//BOPI
// !IROUTINE:  ESMCI::Array::setComputationalLWidth
//
// !INTERFACE:
int Array::setComputationalLWidth(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *computationalLWidthArg        // (in)
  ){
//
// !DESCRIPTION:
//    Set computationalLWidth for all local DEs.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check computationalLWidthArg input and process
  if (computationalLWidthArg != NULL){
    if (computationalLWidthArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalLWidth array must be of rank 2", &rc);
      return rc;
    }
    int redDimCount = rank - tensorCount;
    if (computationalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dim of computationalLWidth argument must be of size redDimCount",
        &rc);
      return rc;
    }
    int localDeCount = delayout->getLocalDeCount();
    if (computationalLWidthArg->extent[1] != localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dim of computationalLWidth argument must be of size "
        "localDeCount", &rc);
      return rc;
    }
    int *computationalLBoundNew = new int[redDimCount*localDeCount];
    for (int i=0; i<localDeCount*redDimCount; i++){
      computationalLBoundNew[i] = exclusiveLBound[i]
        - computationalLWidthArg->array[i];
      if (computationalLBoundNew[i] < totalLBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaLWidth below totalLBound -> not updated", &rc);
        return rc;
      }
      if (computationalLBoundNew[i] > totalUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaLWidth above totalUBound -> not updated", &rc);
        return rc;
      }
    }
    // update computationalLBound
    memcpy(computationalLBound, computationalLBoundNew,
      redDimCount*localDeCount*sizeof(int));
    delete [] computationalLBoundNew;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::setComputationalUWidth()"
//BOPI
// !IROUTINE:  ESMCI::Array::setComputationalUWidth
//
// !INTERFACE:
int Array::setComputationalUWidth(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *computationalUWidthArg        // (in)
  ){
//
// !DESCRIPTION:
//    Set computationalUWidth for all local DEs.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check computationalUWidthArg input and process
  if (computationalUWidthArg != NULL){
    if (computationalUWidthArg->dimCount != 2){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- computationalUWidth array must be of rank 2", &rc);
      return rc;
    }
    int redDimCount = rank - tensorCount;
    if (computationalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dim of computationalUWidth argument must be of size redDimCount",
        &rc);
      return rc;
    }
    int localDeCount = delayout->getLocalDeCount();
    if (computationalUWidthArg->extent[1] != localDeCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dim of computationalUWidth argument must be of size "
        "localDeCount", &rc);
      return rc;
    }
    int *computationalLBoundNew = new int[redDimCount*localDeCount];
    for (int i=0; i<localDeCount*redDimCount; i++){
      computationalLBoundNew[i] = exclusiveLBound[i]
        - computationalUWidthArg->array[i];
      if (computationalLBoundNew[i] < totalLBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaUWidth below totalLBound -> not updated", &rc);
        return rc;
      }
      if (computationalLBoundNew[i] > totalUBound[i]){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- computationaUWidth above totalUBound -> not updated", &rc);
        return rc;
      }
    }
    // update computationalLBound
    memcpy(computationalLBound, computationalLBoundNew,
      redDimCount*localDeCount*sizeof(int));
    delete [] computationalLBoundNew;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// misc.
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::match()"
//BOPI
// !IROUTINE:  ESMCI::Array::match
//
// !INTERFACE:
bool Array::match(
//
// !RETURN VALUE:
//    bool according to match
//
// !ARGUMENTS:
//
  Array *array1,                          // in
  Array *array2,                          // in
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//    Determine if array1 and array2 match.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // initialize return value
  bool matchResult = false;
  
  // return with errors for NULL pointer
  if (array1 == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", rc);
    return matchResult;
  }
  if (array2 == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to Array", rc);
    return matchResult;
  }
  
  // check if Array pointers are identical
  if (array1 == array2){
    // pointers are identical -> nothing more to check
    matchResult = true;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  
  // check typekind, rank match
  if (array1->typekind != array2->typekind){
    matchResult = false;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  if (array1->rank != array2->rank){
    matchResult = false;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }

  // compare the distgrid members
  matchResult = DistGrid::match(array1->getDistGrid(), array2->getDistGrid(),
    &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return rc;
  if (matchResult==false){
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  
  // compare Array members to ensure DE-local tiles are congruent
  if (array1->tensorCount != array2->tensorCount){
    matchResult = false;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  if (array1->tensorElementCount != array2->tensorElementCount){
    matchResult = false;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  int localDeCount = array1->getDistGrid()->getDELayout()->getLocalDeCount();
  int *int1 = array1->totalElementCountPLocalDe;
  int *int2 = array2->totalElementCountPLocalDe;
  for (int i=0; i<localDeCount; i++){
    if (int1[i] != int2[i]){
      matchResult = false;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }

  // return successfully indicating match
  matchResult = true;
  if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
  return matchResult;
}
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
  printf("~ lower class' values ~\n");
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
    if (exclusiveElementCountPDe[de]){
      // associated DE
      int j=0;    // reset
      int jjj=0;  // reset
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMap[jj]){
          // distributed dimension
          printf("dim %d: [%d]: [%d [%d [%d, %d] %d] %d]\n", 
            jj+1, j, 
            totalLBound[i*dimCount+j], computationalLBound[i*dimCount+j],
            exclusiveLBound[i*dimCount+j], exclusiveUBound[i*dimCount+j],
            computationalUBound[i*dimCount+j], totalUBound[i*dimCount+j]);
          ++j;
        }else{
          // non-distributed dimension
          printf("dim %d: undistLBound[%d]=%d            undistUBound[%d]=%d\n",
            jj+1, jjj, undistLBound[jjj], jjj, undistUBound[jjj]);
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
  int r;

  // Check if buffer has enough free memory to hold object
  if ((*length - *offset) < sizeof(Array)){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an Array object", &rc);
    return rc;
  }

  // Serialize the Base class,
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Serialize the DistGrid
  localrc = distgrid->serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Serialize Array meta data
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  dkp = (ESMC_TypeKind *)(buffer + *offset);
  *dkp++ = typekind;
  ip = (int *)dkp;
  *ip++ = rank;
  ifp = (ESMC_IndexFlag *)ip;
  *ifp++ = indexflag;
  ip = (int *)ifp;
  *ip++ = tensorCount;
  for (int i=0; i<tensorCount; i++){
    *ip++ = undistLBound[i];
    *ip++ = undistUBound[i];
    *ip++ = staggerLoc[i];
    *ip++ = vectorDim[i];
  }
  for (int i=0; i<distgrid->getDimCount(); i++)
    *ip++ = distgridToArrayMap[i];
  for (int i=0; i<rank; i++)
    *ip++ = arrayToDistGridMap[i];
  for (int i=0; i<distgrid->getDimCount(); i++)
    *ip++ = distgridToPackedArrayMap[i];
  *ip++ = tensorElementCount;
  for (int i=0; i<delayout->getDeCount(); i++)
    *ip++ = exclusiveElementCountPDe[i];
  
  // fix offset  
  cp = (char *)ip;
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
  int r;

  // Deserialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Deserialize(buffer, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Deserialize the DistGrid
  distgrid = DistGrid::deserialize(buffer, offset);
  distgridCreator = true;  // deserialize creates a local object
  // Pull DELayout out of DistGrid
  delayout = distgrid->getDELayout();
  // Deserialize Array meta data
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  dkp = (ESMC_TypeKind *)(buffer + *offset);
  typekind = *dkp++;
  ip = (int *)dkp;
  rank = *ip++;
  ifp = (ESMC_IndexFlag *)ip;
  indexflag = *ifp++;
  ip = (int *)ifp;
  tensorCount = *ip++;
  undistLBound = new int[tensorCount];
  undistUBound = new int[tensorCount];
  staggerLoc = new int[tensorCount];
  vectorDim = new int[tensorCount];
  for (int i=0; i<tensorCount; i++){
    undistLBound[i] = *ip++;
    undistUBound[i] = *ip++;
    staggerLoc[i] = *ip++;
    vectorDim[i] = *ip++;
  }
  distgridToArrayMap = new int[distgrid->getDimCount()];
  for (int i=0; i<distgrid->getDimCount(); i++)
    distgridToArrayMap[i] = *ip++;
  arrayToDistGridMap = new int[rank];
  for (int i=0; i<rank; i++)
    arrayToDistGridMap[i] = *ip++;
  distgridToPackedArrayMap = new int[distgrid->getDimCount()];
  for (int i=0; i<distgrid->getDimCount(); i++)
    distgridToPackedArrayMap[i] = *ip++;
  tensorElementCount = *ip++;
  exclusiveElementCountPDe = new int[delayout->getDeCount()];
  for (int i=0; i<delayout->getDeCount(); i++)
    exclusiveElementCountPDe[i] = *ip++;
  
  // fix offset
  cp = (char *)ip;
  *offset = (cp - buffer);
  
  // set values with local dependency
  larrayList = new ESMC_LocalArray*[0];     // no DE on proxy object
  larrayBaseAddrList = new void*[0];        // no DE on proxy object
  totalElementCountPLocalDe = NULL;         // no De on proxy object

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
#define ESMC_METHOD "ESMCI::Array::gather()"
//BOPI
// !IROUTINE:  ESMCI::Array::gather
//
// !INTERFACE:
int Array::gather(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *arrayArg,                       // out -
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
//    Gather Array object into native array.
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

  // get minIndexPDim and maxIndexPDim for patch
  const int *minIndexPDim = distgrid->getMinIndexPDimPPatch(patch, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  const int *maxIndexPDim = distgrid->getMaxIndexPDimPPatch(patch, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // only rootPet checks for consistency of input, others listen
  if (localPet == rootPet){
    // check consistency of input information: shape = (typekind, rank, extents)
    if (typekindArg != typekind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between array argument and Array object", &rc);
      vm->broadcast(&rc, sizeof(int), rootPet);
      return rc;
    }
    if (rankArg != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Type mismatch between array argument and Array object", &rc);
      vm->broadcast(&rc, sizeof(int), rootPet);
      return rc;
    }
    int tensorIndex=0;  // reset
    for (int i=0; i<rank; i++){
      int j = arrayToDistGridMap[i];
      if (j){
        // decomposed dimension
        --j;  // shift to basis 0
        if (counts[i] != maxIndexPDim[j] - minIndexPDim[j] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", &rc);
          vm->broadcast(&rc, sizeof(int), rootPet);
          return rc;
        }
      }else{
        // tensor dimension
        if (counts[i] != undistUBound[tensorIndex] - undistLBound[tensorIndex]
          + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", &rc);
          vm->broadcast(&rc, sizeof(int), rootPet);
          return rc;
        }
        ++tensorIndex;
      }
    }
    int success = ESMF_SUCCESS;
    vm->broadcast(&success, sizeof(int), rootPet);
  }else{
    // not rootPet receive status from rootPet
    vm->broadcast(&localrc, sizeof(int), rootPet);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      "- rootPet exited with error", &rc)) return rc;
  }

  // size in bytes of each piece of data
  int dataSize = ESMC_TypeKindSize(typekind);

  
  // distgrid and delayout values
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
  const int *minIndexPDimPDe = distgrid->getMinIndexPDimPDe();
  int dimCount = distgrid->getDimCount();
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeList = delayout->getLocalDeList();
  
  int redDimCount = rank - tensorCount;
  
  // prepare for comms
  VMK::commhandle **commh = new VMK::commhandle*; // used by all comm calls
  VMK::commhandle **commhList = 
    new VMK::commhandle*[dimCount]; // used for indexList comm
  
  // rootPet is the only receiver for gather
  char **recvBuffer;
  if (localPet == rootPet){
    // for each DE of the Array ceceive a single contiguous recvBuffer
    // from the associated PET non-blocking and memcpy it into the right 
    // location in "array".
    recvBuffer = new char*[deCount]; // contiguous recvBuffer
    for (int i=0; i<deCount; i++){
      int de = i;
      if (patchListPDe[de] == patch){
        // this DE is located on sending patch
        // prepare contiguous recvBuffer for this DE and issue non-blocking recv
        int recvSize =
          exclusiveElementCountPDe[de]*tensorElementCount*dataSize;  // bytes
        recvBuffer[de] = new char[recvSize];
        int srcPet;
        delayout->getDEMatchPET(de, *vm, NULL, &srcPet, 1);
        *commh = NULL; // invalidate
        localrc = vm->recv(recvBuffer[de], recvSize, srcPet, commh);
        if (localrc){
          char *message = new char[160];
          sprintf(message, "VMKernel/MPI error #%d\n", localrc);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
            message, &rc);
          delete [] message;
          return rc;
        }
      } // DE on patch
    } // i -> de
  }
  
  // all PETs may be senders
  char **sendBuffer = new char*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (patchListPDe[de] != patch) continue; // skip to next local DE
    sendBuffer[i] = (char *)larrayBaseAddrList[i]; // default: contiguous
    int sendSize =
      exclusiveElementCountPDe[de]*tensorElementCount*dataSize;  // bytes
    if (!contiguousFlag[i]){
      // only if this DE has a non-contiguous decomposition the contiguous
      // send buffer must be compiled from DE-local array segment piece by p.
      sendBuffer[i] = new char[sendSize];
      char *larrayBaseAddr = (char *)larrayBaseAddrList[i];
      // reset counters for multi-dim while-loop
      int *ii = new int[rank];        // index tuple basis 0
      int *iiSrt = new int[rank];
      int *iiEnd = new int[rank];
      int *skipWidth = new int[rank];
      int packedIndex = 0;    // reset
      int tensorIndex = 0;    // reset
      int totalWidthProd = 1; // reset
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMap[jj]){
          // decomposed dimension 
          int j = packedIndex;
          iiSrt[jj] =
            exclusiveLBound[i*redDimCount+j] - totalLBound[i*redDimCount+j];
          iiEnd[jj] =
            exclusiveUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
          skipWidth[jj] =
            (totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j])
            - (exclusiveUBound[i*redDimCount+j]
            - exclusiveLBound[i*redDimCount+j]);
          skipWidth[jj] *= totalWidthProd;  // multiply in previous dims
          // update totalWidthProd for next dims
          totalWidthProd *=
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
          ++packedIndex;
        }else{
          // tensor dimension
          iiSrt[jj] = 0;
          iiEnd[jj] = undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1;
          totalWidthProd *=
            undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1;
          skipWidth[jj] = totalWidthProd;
          ++tensorIndex;
        }
        ii[jj] = iiSrt[jj];
      }
      // loop over all elements in exclusive region for this DE and memcpy data
      // via multi-dim while-loop
      int sendBufferIndex = 0;  // reset
      // determine start lin. index for this element into larrayBaseAddrList[i]
      int linearIndex = ii[rank-1];   // init
      packedIndex = redDimCount-1;   // reset
      if (arrayToDistGridMap[rank-1])
        --packedIndex;  // adjust starting index
      for (int jj=rank-2; jj>=0; jj--){
        if (arrayToDistGridMap[jj]){
          // decomposed dimension 
          int j = packedIndex;
          linearIndex *= totalUBound[i*redDimCount+j]
            - totalLBound[i*redDimCount+j] + 1;
          --packedIndex;
        }else{
          // tensor dimension
          linearIndex *= iiEnd[jj];
        }
        linearIndex += ii[jj];
      }
      while(ii[rank-1] < iiEnd[rank-1]){
        // copy this element from excl. region into the contiguous sendBuffer
        // contiguous data copy in 1st dim
        memcpy(sendBuffer[i]+sendBufferIndex*dataSize,
          larrayBaseAddr+linearIndex*dataSize, (iiEnd[0]-iiSrt[0])*dataSize);
        // skip to end of 1st dimension
        ii[0] = iiEnd[0];
        linearIndex += iiEnd[0] - iiSrt[0];
        sendBufferIndex += iiEnd[0] - iiSrt[0];
        
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
      delete [] ii;
      delete [] iiEnd;
      delete [] iiSrt;
      delete [] skipWidth;
    } // !contiguousFlag
    
    // ready to send the sendBuffer to rootPet
    *commh = NULL; // invalidate
    localrc = vm->send(sendBuffer[i], sendSize, rootPet, commh);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
        message, &rc);
      delete [] message;
      return rc;
    }
  } // i -> de
  
  // todo: separate receive and send commhandles and separately wait!
  // wait until all the local receives are complete
  // wait until all the local sends are complete
  // for now wait on _all_ outstanding non-blocking comms for this PET
  vm->commqueuewait();
  
  if (localPet == rootPet){
    char *array = (char *)arrayArg;
    int *ii = new int[rank];     // index tuple basis 0
    int *iiEnd = new int[rank];
    for (int i=0; i<deCount; i++){
      int de = i;
      if (patchListPDe[de] == patch){
        // this DE is located on sending patch
        int **indexList = new int*[dimCount];
        int tensorIndex=0;  // reset
        int commhListCount = 0;  // reset
        for (int j=0; j<dimCount; j++){
          if(distgridToArrayMap[j]!=0 && contigFlagPDimPDe[de*dimCount+j]==0){
            // associated and non-contiguous dimension
            // -> obtain indexList for this DE and dim
            indexList[j] = new int[indexCountPDimPDe[de*dimCount+j]];
            commhList[commhListCount] = NULL; // prime for later test
            localrc = distgrid->fillIndexListPDimPDe(indexList[j], de, j+1, 
              &(commhList[commhListCount]), localPet, vm);
            if (commhList[commhListCount] != NULL)
              ++commhListCount;
            if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
              ESMF_ERR_PASSTHRU, &rc)) return rc;
          }
        } // j

        // reset counters for multi-dim while-loop
        tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          ii[jj] = 0;  // reset
          int j = arrayToDistGridMap[jj];// j is dimIndex basis 1, or 0 f tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            iiEnd[jj] = indexCountPDimPDe[de*dimCount+j];
          }else{
            // tensor dimension
            iiEnd[jj] = undistUBound[tensorIndex] - undistLBound[tensorIndex]
              + 1;
            ++tensorIndex;
          }
        }
        
        // wait for all outstanding receives issued by fillIndexListPDimPDe()
        for (int j=0; j<commhListCount; j++){
          vm->commwait(&(commhList[j]));
          delete commhList[j];
        }
        
        // loop over all elements in exclusive region for this DE 
        // via multi-dim while-loop
        int recvBufferIndex = 0;  // reset
        while(ii[rank-1] < iiEnd[rank-1]){        
          // determine linear index for this element into array
          int linearIndex = 0;  // reset
          for (int jj=rank-1; jj>=0; jj--){
            linearIndex *= counts[jj];  // first time zero o.k.
            int j = arrayToDistGridMap[jj];// j is dimIndex bas 1, or 0 f tensor
            if (j){
              // decomposed dimension 
              --j;  // shift to basis 0
              if (contigFlagPDimPDe[de*dimCount+j])
                linearIndex += minIndexPDimPDe[de*dimCount+j] + ii[jj];
              else
                linearIndex += indexList[j][ii[jj]];
              // shift basis 1 -> basis 0
              linearIndex -= minIndexPDim[j];
            }else{
              // tensor dimension
              linearIndex += ii[jj];
            }
          }
          // copy this element from the contiguous recvBuffer for this DE
          if (contigFlagPDimPDe[de*dimCount]){
            // contiguous data in first dimension
            memcpy(array+linearIndex*dataSize, 
              recvBuffer[de]+recvBufferIndex*dataSize, iiEnd[0]*dataSize);
            ii[0] = iiEnd[0]; // skip to end of 1st dimension
            recvBufferIndex += iiEnd[0];
          }else{
            // non-contiguous data in first dimension
            memcpy(array+linearIndex*dataSize, 
              recvBuffer[de]+recvBufferIndex*dataSize, dataSize);
            ++ii[0];
            ++recvBufferIndex;
          }
          // multi-dim index increment
          for (int j=0; j<rank-1; j++){
            if (ii[j] == iiEnd[j]){
              ii[j] = 0;  // reset
              ++ii[j+1];
            }
          }
        } // multi-dim while-loop
            
        // clean-up
        for (int j=0; j<dimCount; j++)
          if(contigFlagPDimPDe[de*dimCount+j]==0)
            delete [] indexList[j];
        delete [] indexList;
      } // DE on patch
    } // i -> de
    delete [] ii;
    delete [] iiEnd;
  }else{
    // localPet is _not_ rootPet -> provide localIndexList to rootPet if nec.
    int commhListCount = 0;  // reset
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      if (patchListPDe[de] == patch){
        // this DE is located on receiving patch -> must send info to rootPet
        for (int j=0; j<dimCount; j++){
          if(distgridToArrayMap[j]!=0 && contigFlagPDimPDe[de*dimCount+j]==0){
            // associated and non-contiguous dimension
            // -> send local indexList for this DE and dim to rootPet
            commhList[commhListCount] = NULL; // prime for later test
            localrc = distgrid->fillIndexListPDimPDe(NULL, de, j+1, 
              &(commhList[commhListCount]), rootPet, vm);
            if (commhList[commhListCount] != NULL)
              ++commhListCount;
            if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
              ESMF_ERR_PASSTHRU, &rc)) return rc;
          }
        } // j
      } // DE on patch
    } // i -> de
    // wait for all outstanding indexList sends issued by fillIndexListPDimPDe()
    for (int j=0; j<commhListCount; j++){
      vm->commwait(&(commhList[j]));
        delete commhList[j];
    }
  }
  
  // garbage collection
  if (localPet == rootPet){
    for (int i=0; i<deCount; i++){
      int de = i;
      if (patchListPDe[de] == patch)
        delete [] recvBuffer[de];
    }    
    delete [] recvBuffer;
  }
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (patchListPDe[de] != patch) continue; // skip to next local DE
    if (!contiguousFlag[i])
      delete [] sendBuffer[i];
  }
  delete [] sendBuffer;
  delete commh;
  delete [] commhList;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
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

  // query the VM
  int localPet = vm->getLocalPet();
  
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

  // get minIndexPDim and maxIndexPDim for patch
  const int *minIndexPDim = distgrid->getMinIndexPDimPPatch(patch, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  const int *maxIndexPDim = distgrid->getMaxIndexPDimPPatch(patch, &localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // only rootPet checks for consistency of input, others listen
  if (localPet == rootPet){
    // check consistency of input information: shape = (typekind, rank, extents)
    if (typekindArg != typekind){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between array argument and Array object", &rc);
      vm->broadcast(&rc, sizeof(int), rootPet);
      return rc;
    }
    if (rankArg != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- Type mismatch between array argument and Array object", &rc);
      vm->broadcast(&rc, sizeof(int), rootPet);
      return rc;
    }
    int tensorIndex=0;  // reset
    for (int i=0; i<rank; i++){
      int j = arrayToDistGridMap[i];
      if (j){
        // decomposed dimension
        --j;  // shift to basis 0
        if (counts[i] != maxIndexPDim[j] - minIndexPDim[j] + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", &rc);
          vm->broadcast(&rc, sizeof(int), rootPet);
          return rc;
        }
      }else{
        // tensor dimension
        if (counts[i] != undistUBound[tensorIndex] - undistLBound[tensorIndex]
          + 1){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Extent mismatch between array argument and Array object", &rc);
          vm->broadcast(&rc, sizeof(int), rootPet);
          return rc;
        }
        ++tensorIndex;
      }
    }
    int success = ESMF_SUCCESS;
    vm->broadcast(&success, sizeof(int), rootPet);
  }else{
    // not rootPet receive status from rootPet
    vm->broadcast(&localrc, sizeof(int), rootPet);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      "- rootPet exited with error", &rc)) return rc;
  }

  // size in bytes of each piece of data
  int dataSize = ESMC_TypeKindSize(typekind);

  // distgrid and delayout values
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
  const int *minIndexPDimPDe = distgrid->getMinIndexPDimPDe();
  int dimCount = distgrid->getDimCount();
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeList = delayout->getLocalDeList();
  
  int redDimCount = rank - tensorCount;
  
  // prepare for comms
  VMK::commhandle **commh = new VMK::commhandle*; // used by all comm calls
  VMK::commhandle **commhList = 
    new VMK::commhandle*[dimCount]; // used for indexList comm
  
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
        int **indexList = new int*[dimCount];
        int tensorIndex=0;  // reset
        int commhListCount = 0;  // reset
        for (int j=0; j<dimCount; j++){
          if(distgridToArrayMap[j]!=0 && contigFlagPDimPDe[de*dimCount+j]==0){
            // associated and non-contiguous dimension
            // -> obtain indexList for this DE and dim
            indexList[j] = new int[indexCountPDimPDe[de*dimCount+j]];
            commhList[commhListCount] = NULL; // prime for later test
            localrc = distgrid->fillIndexListPDimPDe(indexList[j], de, j+1, 
              &(commhList[commhListCount]), localPet, vm);
            if (commhList[commhListCount] != NULL)
              ++commhListCount;
            if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
              ESMF_ERR_PASSTHRU, &rc)) return rc;
          }
        } // j

        // prepare contiguous sendBuffer for this DE
        int sendSize =
          exclusiveElementCountPDe[de]*tensorElementCount*dataSize;  // bytes
        sendBuffer[de] = new char[sendSize];
        // reset counters for multi-dim while-loop
        tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          ii[jj] = 0;  // reset
          int j = arrayToDistGridMap[jj];// j is dimIndex basis 1, or 0 f tensor
          if (j){
            // decomposed dimension 
            --j;  // shift to basis 0
            iiEnd[jj] = indexCountPDimPDe[de*dimCount+j];
          }else{
            // tensor dimension
            iiEnd[jj] = undistUBound[tensorIndex] - undistLBound[tensorIndex]
              + 1;
            ++tensorIndex;
          }
        }
        
        // wait for all outstanding receives issued by fillIndexListPDimPDe()
        for (int j=0; j<commhListCount; j++){
          vm->commwait(&(commhList[j]));
          delete commhList[j];
        }
        
        // loop over all elements in exclusive region for this DE 
        // via multi-dim while-loop
        int sendBufferIndex = 0;  // reset
        while(ii[rank-1] < iiEnd[rank-1]){        
          // determine linear index for this element into array
          int linearIndex = 0;  // reset
          for (int jj=rank-1; jj>=0; jj--){
            linearIndex *= counts[jj];  // first time zero o.k.
            int j = arrayToDistGridMap[jj];// j is dimIndex bas 1, or 0 f tensor
            if (j){
              // decomposed dimension 
              --j;  // shift to basis 0
              if (contigFlagPDimPDe[de*dimCount+j])
                linearIndex += minIndexPDimPDe[de*dimCount+j] + ii[jj];
              else
                linearIndex += indexList[j][ii[jj]];
              // shift basis 1 -> basis 0
              linearIndex -= minIndexPDim[j];
            }else{
              // tensor dimension
              linearIndex += ii[jj];
            }
          }
          // copy this element into the contiguous sendBuffer for this DE
          if (contigFlagPDimPDe[de*dimCount]){
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
        localrc = vm->send(sendBuffer[de], sendSize, dstPet, commh);
        if (localrc){
          char *message = new char[160];
          sprintf(message, "VMKernel/MPI error #%d\n", localrc);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
            message, &rc);
          delete [] message;
          return rc;
        }
        
        // clean-up
        for (int j=0; j<dimCount; j++)
          if(contigFlagPDimPDe[de*dimCount+j]==0)
            delete [] indexList[j];
        delete [] indexList;
      } // DE on patch
    } // i -> de
    
    delete [] ii;
    delete [] iiEnd;
  }else{
    // localPet is _not_ rootPet -> provide localIndexList to rootPet if nec.
    int commhListCount = 0;  // reset
    for (int i=0; i<localDeCount; i++){
      int de = localDeList[i];
      if (patchListPDe[de] == patch){
        // this DE is located on receiving patch -> must send info to rootPet
        for (int j=0; j<dimCount; j++){
          if(distgridToArrayMap[j]!=0 && contigFlagPDimPDe[de*dimCount+j]==0){
            // associated and non-contiguous dimension
            // -> send local indexList for this DE and dim to rootPet
            commhList[commhListCount] = NULL; // prime for later test
            localrc = distgrid->fillIndexListPDimPDe(NULL, de, j+1, 
              &(commhList[commhListCount]), rootPet, vm);
            if (commhList[commhListCount] != NULL)
              ++commhListCount;
            if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
              ESMF_ERR_PASSTHRU, &rc)) return rc;
          }
        } // j
      } // DE on patch
    } // i -> de
    // wait for all outstanding indexList sends issued by fillIndexListPDimPDe()
    for (int j=0; j<commhListCount; j++){
      vm->commwait(&(commhList[j]));
        delete commhList[j];
    }
  }
  
  // - done issuing nb sends (from rootPet) -

  // all PETs may be receivers
  char **recvBuffer = new char*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeList[i];
    if (patchListPDe[de] != patch) continue; // skip to next local DE
    recvBuffer[i] = (char *)larrayBaseAddrList[i]; // default: contiguous
    int recvSize =
      exclusiveElementCountPDe[de]*tensorElementCount*dataSize; // bytes
    if (!contiguousFlag[i])
      recvBuffer[i] = new char[recvSize];
    *commh = NULL; // invalidate
    // receive data into recvBuffer
    localrc = vm->recv(recvBuffer[i], recvSize, rootPet, commh);
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
      int packedIndex = 0;    // reset
      int tensorIndex = 0;    // reset
      int totalWidthProd = 1; // reset
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMap[jj]){
          // decomposed dimension 
          int j = packedIndex;
          iiSrt[jj] =
            exclusiveLBound[i*redDimCount+j] - totalLBound[i*redDimCount+j];
          iiEnd[jj] =
            exclusiveUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
          skipWidth[jj] =
            (totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j])
            - (exclusiveUBound[i*redDimCount+j]
            - exclusiveLBound[i*redDimCount+j]);
          skipWidth[jj] *= totalWidthProd;  // multiply in previous dims
          // update totalWidthProd for next dims
          totalWidthProd *=
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
          ++packedIndex;
        }else{
          // tensor dimension
          iiSrt[jj] = 0;
          iiEnd[jj] = undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1;
          totalWidthProd *=
            undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1;
          skipWidth[jj] = totalWidthProd;
          ++tensorIndex;
        }
        ii[jj] = iiSrt[jj];
      }
      // loop over all elements in exclusive region for this DE and memcpy data
      // via multi-dim while-loop
      int recvBufferIndex = 0;  // reset
      // determine start lin. index for this element into larrayBaseAddrList[i]
      int linearIndex = ii[rank-1];  // init
      packedIndex = redDimCount-1;  // reset
      if (arrayToDistGridMap[rank-1])
        --packedIndex;  // adjust starting index
      for (int jj=rank-2; jj>=0; jj--){
        if (arrayToDistGridMap[jj]){
          // decomposed dimension 
          int j = packedIndex;
          linearIndex *= totalUBound[i*redDimCount+j]
            - totalLBound[i*redDimCount+j] + 1;
          --packedIndex;
        }else{
          // tensor dimension
          linearIndex *= iiEnd[jj];
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
#define ESMC_METHOD "ESMCI::Array::redistStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::redistStore
//
// !INTERFACE:
int Array::redistStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                      // in    - source Array
  Array *dstArray,                      // in    - destination Array
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
  InterfaceInt *srcToDstTransposeMap,   // in    - mapping src -> dst dims
  ESMC_TypeKind typekindFactor,         // in    - typekind of factor
  void *factor                          // in    - redist factor
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for redistribution
//  from srcArray to dstArray.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

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
  // srcArray and dstArray may not point to the identical Array object
  if (srcArray == dstArray){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- srcArray and dstArray must not be identical", &rc);
    return rc;
  }
  
  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  // determine consistent typekindFactor and factor across all PETs
  if (factor != NULL){
    // must define a valid typekind
    if (typekindFactor == ESMF_NOKIND){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- must specify valid typekindFactor on PETs that provide factor", &rc);
      return rc;
    }
    if (typekindFactor != ESMC_TYPEKIND_I4
      && typekindFactor != ESMC_TYPEKIND_I8
      && typekindFactor != ESMC_TYPEKIND_R4
      && typekindFactor != ESMC_TYPEKIND_R8){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- method not implemented for specified typekindFactor", &rc);
      return rc;
    }
  }else{
    // set typekindFactor to ESMF_NOKIND 
    // -> this Pet will not provide factor
    typekindFactor = ESMF_NOKIND;
  }
  
  // communicate typekindFactor across all Pets
  ESMC_TypeKind *typekindList = new ESMC_TypeKind[petCount];
  vm->allgather(&typekindFactor, typekindList, sizeof(ESMC_TypeKind));
  // Check that all non-ESMF_NOKIND typekindList elements match,
  // set local typekindFactor accordingly and keep track of Pets that have
  // factors.
  int *factorPetList = new int[petCount];
  int factorPetCount = 0; // reset
  typekindFactor = ESMF_NOKIND;  // initialize
  for (int i=0; i<petCount; i++){
    if (typekindList[i] != ESMF_NOKIND)
      factorPetList[factorPetCount++] = i;
    if (typekindFactor == ESMF_NOKIND){
      typekindFactor = typekindList[i];  // set to 1st element not ESMF_NOKIND
    }else{
      // check following elements against the typekindFactors and tensorMixFlag
      if (typekindList[i] != ESMF_NOKIND){
        if (typekindFactor != typekindList[i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- TypeKind mismatch between PETs", &rc);
          return rc;
        }
      }
    }
  }
  delete [] typekindList;
  // set typekindFactor default in case there were no factors provided
  if (factorPetCount == 0)
    typekindFactor = srcArray->getTypekind();
  
  // set factorLocal
  void *factorLocal;
  if (typekindFactor == ESMC_TYPEKIND_R4){
    ESMC_R4 *factorLocalT = new ESMC_R4;
    if (factor)
      *factorLocalT = *(ESMC_R4 *)factor;
    else
      *factorLocalT = 1.;
    factorLocal = (void *)factorLocalT;
  }else if (typekindFactor == ESMC_TYPEKIND_R8){
    ESMC_R8 *factorLocalT = new ESMC_R8;
    if (factor)
      *factorLocalT = *(ESMC_R8 *)factor;
    else
      *factorLocalT = 1.;
    factorLocal = (void *)factorLocalT;
  }else if (typekindFactor == ESMC_TYPEKIND_I4){
    ESMC_I4 *factorLocalT = new ESMC_I4;
    if (factor)
      *factorLocalT = *(ESMC_I4 *)factor;
    else
      *factorLocalT = 1;
    factorLocal = (void *)factorLocalT;
  }else if (typekindFactor == ESMC_TYPEKIND_I8){
    ESMC_I8 *factorLocalT = new ESMC_I8;
    if (factor)
      *factorLocalT = *(ESMC_I8 *)factor;
    else
      *factorLocalT = 1;
    factorLocal = (void *)factorLocalT;
  }
  
  if (factorPetCount > 0){
    // communicate factorLocal variables and check for consistency
    int factorSize = ESMC_TypeKindSize(typekindFactor);
    char *factorLocalList = new char[petCount*factorSize];
    vm->allgather(factorLocal, factorLocalList, factorSize);
    // prime factorLocal with value from first Pet with factor
    memcpy(factorLocal, factorLocalList + factorSize * factorPetList[0],
      factorSize);
    // check against all other factorLocal entries
    for (int i=1; i<factorPetCount; i++){
      char *factorPtr = factorLocalList + factorSize * factorPetList[i];
      if (memcmp(factorLocal, factorPtr, factorSize) != 0){
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
          "- Factor mismatch between PETs", &rc);
        return rc;
      }
    }
    delete [] factorLocalList;
  }
  delete [] factorPetList;
  
  // set up local factorList and factorIndexList
  int factorListCount;
  int *factorIndexListAlloc;
  InterfaceInt *factorIndexList;
  
  if (srcToDstTransposeMap == NULL){
    // srcToDstTransposeMap not specified -> default mode
    
    // src and dst Arrays must have identical number of exclusive elements
    int srcElementCount = 0; // init
    const int *srcElementCountPPatch =
      srcArray->distgrid->getElementCountPPatch();
    for (int i=0; i<srcArray->distgrid->getPatchCount(); i++)
      srcElementCount += srcElementCountPPatch[i];
    int dstElementCount = 0; // init
    const int *dstElementCountPPatch =
      dstArray->distgrid->getElementCountPPatch();
    for (int i=0; i<dstArray->distgrid->getPatchCount(); i++)
      dstElementCount += dstElementCountPPatch[i];
    if (srcElementCount != dstElementCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- srcArray and dstArray must provide identical number of exclusive"
        " elements", &rc);
      return rc;
    }
  
    // implemented via sparseMatMul using identity matrix
    const int *srcElementCountPDe = srcArray->distgrid->getElementCountPDe();
    const int *srcArbSeqIndexCountPLocalDe =
      srcArray->distgrid->getArbSeqIndexCountPLocalDe();
    const int *srcLocalDeList = srcArray->delayout->getLocalDeList();
    int srcLocalDeCount = srcArray->delayout->getLocalDeCount();
    factorListCount = 0;  // init
    for (int i=0; i<srcLocalDeCount; i++)
      factorListCount += srcElementCountPDe[srcLocalDeList[i]];
    // set up factorIndexList
    factorIndexListAlloc = new int[2*factorListCount];
    int *extent = new int[2];
    extent[0] = 2;
    extent[1] = factorListCount;
    factorIndexList = new InterfaceInt(factorIndexListAlloc, 2, extent);
    delete [] extent;
    int jj = 0; // reset
    for (int i=0; i<srcLocalDeCount; i++){
      int de = srcLocalDeList[i];
      int arbSeqIndexCount = srcArbSeqIndexCountPLocalDe[i];
      if (arbSeqIndexCount==srcElementCountPDe[de]){
        const int *srcArbSeqIndexListPLocalDe =
          srcArray->distgrid->getArbSeqIndexListPLocalDe(i);
        for (int j=0; j<arbSeqIndexCount; j++){
          factorIndexListAlloc[2*jj] = factorIndexListAlloc[2*jj+1] =
            srcArbSeqIndexListPLocalDe[j];
          ++jj;
        }
      }else{
        int seqIndexOffset = 1; // reset, seqIndex is basis 1
        for (int j=0; j<de; j++)
          seqIndexOffset += srcElementCountPDe[j];
        for (int j=0; j<srcElementCountPDe[de]; j++){
          factorIndexListAlloc[2*jj] = factorIndexListAlloc[2*jj+1] =
            seqIndexOffset + j;
          ++jj;
        }
      }
    }  
    
  }else{
    // srcToDstTransposeMap specified -> transpose mode
    
    // src and dst Arrays must be of same rank
    int rank = srcArray->getRank();
    if (rank != dstArray->getRank()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- in transpose mode srcArray and dstArray must be of same rank", &rc);
      return rc;
    }

    // src and dst Arrays must be have same number of patches
    int patchCount = srcArray->distgrid->getPatchCount();
    if (patchCount != dstArray->distgrid->getPatchCount()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- in transpose mode srcArray and dstArray must have same number of"
        " patches", &rc);
      return rc;
    }

    // check srcToDstTransposeMap input
    if (srcToDstTransposeMap->dimCount != 1){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_RANK,
        "- srcToDstTransposeMap must be of rank 1", &rc);
      return rc;
    }
    if (srcToDstTransposeMap->extent[0] != rank){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- srcToDstTransposeMap must provide rank values", &rc);
      return rc;
    }
    int *srcToDstTMap = new int[rank];
    for (int i=0; i<rank; i++){
      srcToDstTMap[i] = srcToDstTransposeMap->array[i] - 1; // shift to base 0
      int j;
      for (j=0; j<rank; j++)
        if (srcToDstTransposeMap->array[j] == i+1) break;
      if (j==rank){
        // did not find (i+1) value in transpose map
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE,
          "- srcToDstTransposeMap values must be unique and within range:"
          " [1,..,rank].", &rc);
        return rc;
      }
    }
    
    // size of dims in src and dst Arrays must pairwise match in each patch
    const int *srcArrayToDistGridMap = srcArray->getArrayToDistGridMap();
    const int *dstArrayToDistGridMap = dstArray->getArrayToDistGridMap();
    const int *srcMinIndexPDimPPatch =
      srcArray->distgrid->getMinIndexPDimPPatch();
    const int *dstMinIndexPDimPPatch =
      dstArray->distgrid->getMinIndexPDimPPatch();
    const int *srcMaxIndexPDimPPatch =
      srcArray->distgrid->getMaxIndexPDimPPatch();
    const int *dstMaxIndexPDimPPatch =
      dstArray->distgrid->getMaxIndexPDimPPatch();
    int srcDimCount = srcArray->distgrid->getDimCount();
    int dstDimCount = dstArray->distgrid->getDimCount();
    // prepare dstArrayToTensorMap
    int *dstArrayToTensorMap = new int[rank];
    int tensorIndex = 0;
    for (int jj=0; jj<rank; jj++){
      if (dstArrayToDistGridMap[jj]==0){
        // tensor dimension
        dstArrayToTensorMap[jj] = tensorIndex;
        ++tensorIndex;
      }
    }
    factorListCount = 0;
    int *localStart = new int[patchCount];  // localPet's start index
    int *localSize = new int[patchCount];   // localPet's number of elements
    for (int i=0; i<patchCount; i++){
      int patchFactorListCount = 1;
      // prepare decomposition along last distributed dim in srcArray
      //TODO: this assumes that srcArray has at least one distr. dim
      int lastDimSize = srcMaxIndexPDimPPatch[i*srcDimCount+srcDimCount-1]
        - srcMinIndexPDimPPatch[i*srcDimCount+srcDimCount-1] + 1;
      int intervalSize = lastDimSize/petCount;
      int extraElements = lastDimSize%petCount;
      localStart[i] = srcMinIndexPDimPPatch[i*srcDimCount+srcDimCount-1];
      localStart[i] += localPet * intervalSize;
      localSize[i] = intervalSize;
      if (localPet < extraElements){
        localStart[i] += localPet;
        localSize[i] += 1;
      }
#if 0
fprintf(stderr, "%d start:%d, size:%d\n", localPet, localStart[i], localSize[i]);
#endif
      // check every patch
      int srcTensorIndex = 0;
      int dstTensorIndex = 0;
      for (int jj=0; jj<rank; jj++){
        int srcSize;
        int dstSize;
        int j = srcArrayToDistGridMap[jj];  // j is dimIndex bas 1, or 0 undist.
        if (j){
          // decomposed dimension 
          --j;  // shift to basis 0
          srcSize = srcMaxIndexPDimPPatch[i*srcDimCount+j]
            - srcMinIndexPDimPPatch[i*srcDimCount+j] + 1;
          if (j == srcDimCount-1)
            patchFactorListCount *= localSize[i];
          else
            patchFactorListCount *= srcSize;
        }else{
          // tensor dimension
          srcSize = srcArray->undistUBound[srcTensorIndex]
            - srcArray->undistLBound[srcTensorIndex] + 1;
          ++srcTensorIndex;
          patchFactorListCount *= srcSize;
        }
        int jjj = srcToDstTMap[jj];         // src -> dst dimension mapping
        j = dstArrayToDistGridMap[jjj];     // j is dimIndex bas 1, or 0 undist.
        if (j){
          // decomposed dimension 
          --j;  // shift to basis 0
          dstSize = dstMaxIndexPDimPPatch[i*dstDimCount+j]
            - dstMinIndexPDimPPatch[i*dstDimCount+j] + 1;
        }else{
          // tensor dimension
          dstTensorIndex = dstArrayToTensorMap[jjj];
          dstSize = dstArray->undistUBound[dstTensorIndex]
            - dstArray->undistLBound[dstTensorIndex] + 1;
        }
#if 0
fprintf(stderr, "%d, %d\n", srcSize, dstSize);
#endif
        if (srcSize != dstSize){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
            "- in transpose mode the size of srcArray and dstArray dimensions"
            " must pairwise match", &rc);
          return rc;
        }
      }
      factorListCount += patchFactorListCount;
    }
#if 0
fprintf(stderr, "factorListCount = %d\n", factorListCount);
#endif
    // set up factorIndexList
    factorIndexListAlloc = new int[4*factorListCount];
    int *extent = new int[2];
    extent[0] = 4;
    extent[1] = factorListCount;
    factorIndexList = new InterfaceInt(factorIndexListAlloc, 2, extent);
    delete [] extent;
    // prepare to fill in factorIndexList elements
    int factorIndexListIndex = 0; // reset
    int *srcTuple = new int[rank];
    int *srcTupleStart = new int[rank];
    int *srcTupleEnd = new int[rank];
    int *dstTuple = new int[rank];
    // fill in factorIndexList elements
    for (int i=0; i<patchCount; i++){
      // prepare for multi-dim loop through dims in srcArray patch
      int tensorIndex = 0;  // reset
      for (int jj=0; jj<rank; jj++){
        int j = srcArrayToDistGridMap[jj];  // j is dimIndex bas 1, or 0 undist.
        if (j){
          // decomposed dimension 
          --j;  // shift to basis 0
          if (j == srcDimCount-1){
            srcTupleStart[jj] = localStart[i];
            srcTuple[jj] = srcTupleStart[jj];
            srcTupleEnd[jj] = localStart[i] + localSize[i];
          }else{
            srcTupleStart[jj] = srcMinIndexPDimPPatch[i*srcDimCount+j];
            srcTuple[jj] = srcTupleStart[jj];
            srcTupleEnd[jj] = srcMaxIndexPDimPPatch[i*srcDimCount+j] + 1;
          }
        }else{
          // tensor dimension
          srcTupleStart[jj] = srcArray->undistLBound[tensorIndex];
          srcTuple[jj] = srcTupleStart[jj];
          srcTupleEnd[jj] = srcArray->undistUBound[tensorIndex] + 1;
          ++tensorIndex;
        }
      }
      // multi-dim loop through dims in srcArray patch
      while (srcTuple[rank-1] < srcTupleEnd[rank-1]){
        // srcTuple --srcToDstTMap--> dstTuple
        for (int j=0; j<rank; j++)
          dstTuple[srcToDstTMap[j]] = srcTuple[j];
        // determine seq indices
        SeqIndex srcSeqIndex = srcArray->getSequenceIndexPatch(i+1, srcTuple);
        SeqIndex dstSeqIndex = dstArray->getSequenceIndexPatch(i+1, dstTuple);
        // fill this info into factorIndexList
        int fili = 4*factorIndexListIndex;
        factorIndexListAlloc[fili]   = srcSeqIndex.decompSeqIndex;
        factorIndexListAlloc[fili+1] = srcSeqIndex.tensorSeqIndex;
        factorIndexListAlloc[fili+2] = dstSeqIndex.decompSeqIndex;
        factorIndexListAlloc[fili+3] = dstSeqIndex.tensorSeqIndex;

#if 0
printf("factorIndexListIndex=%d (%d, %d) (%d, %d, %d, %d)\n",
  factorIndexListIndex,
  srcTuple[0], srcTuple[1],
  srcSeqIndex.decompSeqIndex, srcSeqIndex.tensorSeqIndex,
  dstSeqIndex.decompSeqIndex, dstSeqIndex.tensorSeqIndex);
#endif   

        ++factorIndexListIndex;        

        // multi-dim index increment
        ++srcTuple[0];
        for (int j=0; j<rank-1; j++){
          if (srcTuple[j] == srcTupleEnd[j]){
            srcTuple[j] = srcTupleStart[j];  // reset
            ++srcTuple[j+1];
          }
        }
      }
    }
    // garbage collection    
    delete [] srcToDstTMap;
    delete [] dstArrayToTensorMap;
    delete [] localStart;
    delete [] localSize;
    delete [] srcTuple;
    delete [] srcTupleEnd;
    delete [] dstTuple;
  }
  
  // load type specific factorList with "1"
  void *factorList;
  if (typekindFactor == ESMC_TYPEKIND_R4){
    ESMC_R4 *factorListT = new ESMC_R4[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_R4 *)factorLocal;
    factorList = (void *)factorListT;
    delete (ESMC_R4 *)factorLocal;
  }else if (typekindFactor == ESMC_TYPEKIND_R8){
    ESMC_R8 *factorListT = new ESMC_R8[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_R8 *)factorLocal;
    factorList = (void *)factorListT;
    delete (ESMC_R8 *)factorLocal;
  }else if (typekindFactor == ESMC_TYPEKIND_I4){
    ESMC_I4 *factorListT = new ESMC_I4[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_I4 *)factorLocal;
    factorList = (void *)factorListT;
    delete (ESMC_I4 *)factorLocal;
  }else if (typekindFactor == ESMC_TYPEKIND_I8){
    ESMC_I8 *factorListT = new ESMC_I8[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_I8 *)factorLocal;
    factorList = (void *)factorListT;
    delete (ESMC_I4 *)factorLocal;
  }
  
  // precompute sparse matrix multiplication
  localrc = sparseMatMulStore(srcArray, dstArray, routehandle, typekindFactor,
    factorList, factorListCount, factorIndexList);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // garbage collection
  delete [] factorIndexListAlloc;
  delete factorIndexList;
  if (typekindFactor == ESMC_TYPEKIND_R4){
    ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
    delete [] factorListT;
  }else if (typekindFactor == ESMC_TYPEKIND_R8){
    ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
    delete [] factorListT;
  }else if (typekindFactor == ESMC_TYPEKIND_I4){
    ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
    delete [] factorListT;
  }else if (typekindFactor == ESMC_TYPEKIND_I8){
    ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
    delete [] factorListT;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::redist()"
//BOPI
// !IROUTINE:  ESMCI::Array::redist
//
// !INTERFACE:
int Array::redist(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                      // in    - source Array
  Array *dstArray,                      // inout - destination Array
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
  ESMC_Logical checkflag                // in    - ESMF_FALSE: (def.) bas. chcks
                                        //         ESMF_TRUE: full input check
  ){    
//
// !DESCRIPTION:
//    Execute an Array redistribution
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // implemented via sparseMatMul
  localrc = sparseMatMul(srcArray, dstArray, routehandle, ESMF_REGION_TOTAL,
    checkflag);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::redistRelease()"
//BOPI
// !IROUTINE:  ESMCI::Array::redistRelease
//
// !INTERFACE:
int Array::redistRelease(
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
//    Release information for an Array redistribution
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // implemented via sparseMatMul
  localrc = sparseMatMulRelease(routehandle);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
bool operator==(SeqIndex a, SeqIndex b){
  if (a.decompSeqIndex != b.decompSeqIndex) return false;
  // decompSeqIndex must be equal
  return (a.tensorSeqIndex == b.tensorSeqIndex);
}
bool operator<(SeqIndex a, SeqIndex b){
  if (a.decompSeqIndex < b.decompSeqIndex) return true;
  if (a.decompSeqIndex > b.decompSeqIndex) return false;
  // decompSeqIndex must be equal
  return (a.tensorSeqIndex < b.tensorSeqIndex);
}
//-----------------------------------------------------------------------------


#define ASMMPROFILE___disable

namespace ArrayHelper{
  
#define MSG_DST_CONTIG

  struct DstInfo{
    int linIndex;               // if vector element then this is start
    int vectorLength;           // ==1 single element, > 1 vector element
    SeqIndex seqIndex;          // if vector element then this is start
    SeqIndex partnerSeqIndex;   // if vector element then this is start
    void *factor;               // if vector element then this factor for all
  };
  bool scalarOrderDstInfo(DstInfo a, DstInfo b){
#ifdef MSG_DST_CONTIG
    if (a.seqIndex == b.seqIndex)
      return (a.partnerSeqIndex < b.partnerSeqIndex);
    else
      return (a.seqIndex < b.seqIndex);
#else
    if (a.partnerSeqIndex == b.partnerSeqIndex)
      return (a.seqIndex < b.seqIndex);
    else
      return (a.partnerSeqIndex < b.partnerSeqIndex);
#endif
  }
  bool vectorOrderDstInfo(DstInfo a, DstInfo b){
    if (a.seqIndex.decompSeqIndex == b.seqIndex.decompSeqIndex)
      if (a.partnerSeqIndex.decompSeqIndex == b.partnerSeqIndex.decompSeqIndex)
        return (a.seqIndex.tensorSeqIndex < b.seqIndex.tensorSeqIndex);
      else
        return
          (a.partnerSeqIndex.decompSeqIndex < b.partnerSeqIndex.decompSeqIndex);
    else
      return (a.seqIndex.decompSeqIndex < b.seqIndex.decompSeqIndex);
  }

  struct SrcInfo{
    int linIndex;               // if vector element then this is start
    int vectorLength;           // ==1 single element, > 1 vector element
    SeqIndex seqIndex;          // if vector element then this is start
    SeqIndex partnerSeqIndex;   // if vector element then this is start
    void *factor;               // if vector element then this factor for all
  };
  bool scalarOrderSrcInfo(SrcInfo a, SrcInfo b){
#ifdef MSG_DST_CONTIG
    if (a.partnerSeqIndex == b.partnerSeqIndex)
      return (a.seqIndex < b.seqIndex);
    else
      return (a.partnerSeqIndex < b.partnerSeqIndex);
#else
    if (a.seqIndex == b.seqIndex)
      return (a.partnerSeqIndex < b.partnerSeqIndex);
    else
      return (a.seqIndex < b.seqIndex);
#endif
  }
  bool vectorOrderSrcInfo(SrcInfo a, SrcInfo b){
    if (a.partnerSeqIndex.decompSeqIndex == b.partnerSeqIndex.decompSeqIndex)
      if (a.seqIndex.decompSeqIndex == b.seqIndex.decompSeqIndex)
        return
          (a.partnerSeqIndex.tensorSeqIndex < b.partnerSeqIndex.tensorSeqIndex);
      else
        return (a.seqIndex.decompSeqIndex < b.seqIndex.decompSeqIndex);
    else
      return
        (a.partnerSeqIndex.decompSeqIndex < b.partnerSeqIndex.decompSeqIndex);
  }
  
  struct RecvnbElement{
    int srcPet;
    int srcDe;        // global DE index of src DE in src DELayout
    int srcLocalDe;   // local enumeration of srcDe
    int dstDe;        // global DE index of dst DE in local DELayout
    int dstLocalDe;   // local enumeration of dstDe
    char *buffer;
    int partnerDeDataCount;
    int recvnbIndex;
    int vectorLength; // each element in dstInfoTable is a vector of this length
    vector<DstInfo> dstInfoTable;
    int localPet;
    int petCount;
    //
    int appendRecvnb(XXE *xxe, int srcTermProcessing, int dataSizeSrc, int k);
    int appendZeroSuperScalar(XXE *xxe, int srcLocalDeCount,
      XXE::TKId elementTK);
    int appendProductSum(XXE *xxe, int srcTermProcessing, int srcLocalDeCount,
      XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK,
      int dataSizeDst, int dataSizeSrc, int dataSizeFactors,
      char **rraList, int rraCount);
    int appendWaitProductSum(XXE *xxe, int srcTermProcessing,
      int srcLocalDeCount, XXE::TKId elementTK, XXE::TKId valueTK,
      XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc, int dataSizeFactors,
      char **rraList, int rraCount, int k);
  };
  bool operator<(RecvnbElement a, RecvnbElement b){
    int aSrcPet = (a.localPet - a.srcPet + a.petCount) % a.petCount;
    int bSrcPet = (b.localPet - b.srcPet + b.petCount) % b.petCount;
    if (aSrcPet == bSrcPet)
      if (a.srcDe == b.srcDe)
        return (a.dstDe < b.dstDe);
      else
        return (a.srcDe < b.srcDe);
    else
      return (aSrcPet < bSrcPet);
  }
  int RecvnbElement::appendRecvnb(XXE *xxe, int srcTermProcessing,
    int dataSizeSrc, int k){
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
#ifdef ASMMSTOREPRINT
    printf("gjt: XXE::recvnb on localPet %d from Pet %d\n", localPet, srcPet);
#endif
    recvnbIndex = xxe->count;  // store index for the associated wait
    int tag = 0;  // no need for special tags - messages are ordered to match
    // determine bufferItemCount according to srcTermProcessing
    int bufferItemCount = 0; // reset
    if (srcTermProcessing == 0)
      bufferItemCount = partnerDeDataCount;
    else{
      vector<ArrayHelper::DstInfo>::iterator pp = dstInfoTable.begin();
      while (pp != dstInfoTable.end()){
        SeqIndex seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
    }
    // append the recvnb operation
    localrc = xxe->appendRecvnb(0x0, buffer, bufferItemCount * dataSizeSrc
      * vectorLength, srcPet, tag);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef ASMMPROFILE
    char *tempString = new char[100];
    sprintf(tempString, "<(%04d/%04d)-Rnb(%d/%d)-(%04d/%04d)> ",
      srcDe, srcPet, k, recvnbIndex, dstDe, localPet);
    localrc = xxe->appendProfileMessage(0x0, tempString);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  int RecvnbElement::appendZeroSuperScalar(XXE *xxe, int srcLocalDeCount,
    XXE::TKId elementTK){
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int j = dstLocalDe;
    int rraIndex = srcLocalDeCount + j; // localDe index into dstArray shifted 
                                        // by srcArray localDeCount
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x2, "Wt: select zero", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
    int xxeIndex = xxe->count;  // need this beyond the increment
    localrc = xxe->appendZeroSuperScalarRRA(0x2, elementTK, rraIndex,
      dstInfoTable.size(), vectorLength);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    XXE::ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo =
      (XXE::ZeroSuperScalarRRAInfo *)&(xxe->stream[xxeIndex]);
    // fill rraOffsetList[]
    int k=0;  // reset
    for (vector<ArrayHelper::DstInfo>::iterator pp = dstInfoTable.begin();
      pp != dstInfoTable.end(); ++pp){
      xxeZeroSuperScalarRRAInfo->rraOffsetList[k] = pp->linIndex;
      ++k;
    }
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x2, "Wt: /select zero", xxe->count,
      xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  int RecvnbElement::appendProductSum(XXE *xxe, int srcTermProcessing,
    int srcLocalDeCount, XXE::TKId elementTK, XXE::TKId valueTK,
    XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc, int dataSizeFactors,
    char **rraList, int rraCount){
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int j = dstLocalDe;
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      // use super-scalar "+=*" operation containing all terms
      int rraIndex = srcLocalDeCount + j; // localDe index into dstArray
                                          // shifted by srcArray localDeCount
      int termCount = partnerDeDataCount;
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarDstRRA(0x0, elementTK, valueTK,
        factorTK, rraIndex, termCount, vectorLength);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      XXE::ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo =
        (XXE::ProductSumSuperScalarDstRRAInfo *)&(xxe->stream[xxeIndex]);
      int *rraOffsetList = xxeProductSumSuperScalarDstRRAInfo->rraOffsetList;
      void **factorList = xxeProductSumSuperScalarDstRRAInfo->factorList;
      void **valueList = xxeProductSumSuperScalarDstRRAInfo->valueList;
      // fill in rraOffsetList, factorList, valueList
      vector<ArrayHelper::DstInfo>::iterator pp = dstInfoTable.begin();
      for (int kk=0; kk<termCount; kk++){
        int linIndex = pp->linIndex;
        rraOffsetList[kk] = linIndex * dataSizeDst;
        factorList[kk] = (void *)(pp->factor);
        valueList[kk] = (void *)(buffer + kk * dataSizeSrc * vectorLength);
        ++pp;
      } // for kk - termCount
      // need to fill in sensible elements and values or timing will be bogus
      switch (elementTK){
      case XXE::R4:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_R4 *)(rraList[srcLocalDeCount]+rraOffsetList[kk]) =
            (ESMC_R4)0.; //element
        break;
      case XXE::R8:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_R8 *)(rraList[srcLocalDeCount]+rraOffsetList[kk]) =
            (ESMC_R8)0.; //element
        break;
      case XXE::I4:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_I4 *)(rraList[srcLocalDeCount]+rraOffsetList[kk]) =
            (ESMC_I4)0; //element
        break;
      case XXE::I8:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_I8 *)(rraList[srcLocalDeCount]+rraOffsetList[kk]) =
            (ESMC_I8)0; //element
        break;
      default:
        break;
      }
      switch (valueTK){
      case XXE::R4:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_R4 *)valueList[kk] = (ESMC_R4)0.01; // value
        break;
      case XXE::R8:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_R4 *)valueList[kk] = (ESMC_R8)0.01; // value
        break;
      case XXE::I4:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_R4 *)valueList[kk] = (ESMC_I4)0.01; // value
        break;
      case XXE::I8:
        for (int kk=0; kk<termCount; kk++)
          *(ESMC_R4 *)valueList[kk] = (ESMC_I8)0.01; // value
        break;
      default:
        break;
      }
      xxe->optimizeElement(xxeIndex);
      double dt_sScalar;
      localrc = xxe->exec(rraCount, rraList, 0x0, &dt_sScalar, xxeIndex,
        xxeIndex);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      // try super-scalar contig "+=*" operation in XXE stream
      xxe->stream[xxeIndex].opId = XXE::productSumSuperScalarContigRRA;
      XXE::ProductSumSuperScalarContigRRAInfo
        *xxeProductSumSuperScalarContigRRAInfo =
        (XXE::ProductSumSuperScalarContigRRAInfo *)&(xxe->stream[xxeIndex]);
      // only change members that are different wrt super-scalar operation
      xxeProductSumSuperScalarContigRRAInfo->valueList = (void *)(buffer);
      xxe->optimizeElement(xxeIndex);
      double dt_sScalarC;
      localrc = xxe->exec(rraCount, rraList, 0x0, &dt_sScalarC, xxeIndex,
        xxeIndex);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
#ifdef ASMMPROFILE
      char *tempString = new char[80];
      vector<int> sortOffsetList(termCount);
      copy(rraOffsetList, rraOffsetList+termCount, sortOffsetList.begin());
      sort(sortOffsetList.begin(), sortOffsetList.end());
      int diffElements = 1;
      for (int i=1; i<termCount; i++)
        if (sortOffsetList[i] != sortOffsetList[i-1]) ++diffElements;
#endif
      // decide for the fastest option
      if (dt_sScalar < dt_sScalarC){
        // use productSumSuperScalarDstRRA
#ifdef ASMMPROFILE
        sprintf(tempString, "use productSumSuperScalarDstRRA for %d terms"
          " (diffElements=%d)", termCount, diffElements);
#endif
        xxe->stream[xxeIndex].opId = XXE::productSumSuperScalarDstRRA;
        xxeProductSumSuperScalarDstRRAInfo->valueList = valueList;
      }else{
        // use productSumSuperScalarContigRRA
#ifdef ASMMPROFILE
        sprintf(tempString, "use productSumSuperScalarContigRRA for %d terms"
          " (diffElements=%d)", termCount, diffElements);
#endif
        // nothing to be done -> already set from last trial
      }
#ifdef ASMMPROFILE
      localrc = xxe->appendProfileMessage(0x0, tempString);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      delete [] tempString;
#endif
    }else{
      // do some processing on the src side
      // use super-scalar "+=" operation containing all terms
      int rraIndex = srcLocalDeCount + j; // localDe index into dstArray
                                          // shifted by srcArray localDeCount
      // determine bufferItemCount according to srcTermProcessing
      int bufferItemCount = 0; // reset
      vector<ArrayHelper::DstInfo>::iterator pp = dstInfoTable.begin();
      while (pp != dstInfoTable.end()){
        SeqIndex seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendSumSuperScalarDstRRA(0x0, elementTK, valueTK,
        rraIndex, bufferItemCount, vectorLength);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      XXE::SumSuperScalarDstRRAInfo *xxeSumSuperScalarDstRRAInfo =
        (XXE::SumSuperScalarDstRRAInfo *)&(xxe->stream[xxeIndex]);
      int *rraOffsetList = xxeSumSuperScalarDstRRAInfo->rraOffsetList;
      void **valueList = xxeSumSuperScalarDstRRAInfo->valueList;
      // fill in rraOffsetList, valueList
      int bufferItem = 0; // reset
      pp = dstInfoTable.begin();  // reset
      while (pp != dstInfoTable.end()){
        SeqIndex seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          rraOffsetList[bufferItem] = pp->linIndex * dataSizeDst;
          valueList[bufferItem] = (void *)(buffer + bufferItem * dataSizeSrc
            * vectorLength);
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItem;
      }
#ifdef ASMMPROFILE
      char *tempString = new char[160];
      vector<int> sortOffsetList(bufferItem);
      copy(rraOffsetList, rraOffsetList+bufferItem, sortOffsetList.begin());
      sort(sortOffsetList.begin(), sortOffsetList.end());
      int diffElements = 1;
      for (int i=1; i<bufferItem; i++)
        if (sortOffsetList[i] != sortOffsetList[i-1]) ++diffElements;
      sprintf(tempString, "use sumSuperScalarDstRRA for termCount=%d -> reduced"
        " to %d terms (diffElements=%d)", partnerDeDataCount, bufferItem,
        diffElements);
      localrc = xxe->appendProfileMessage(0x0, tempString);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      delete [] tempString;
#endif
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  int RecvnbElement::appendWaitProductSum(XXE *xxe, int srcTermProcessing,
    int srcLocalDeCount, XXE::TKId elementTK, XXE::TKId valueTK,
    XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc, int dataSizeFactors,
    char **rraList, int rraCount, int k){
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
#ifdef ASMMPROFILE
    char *tempString = new char[80];
    sprintf(tempString, "Wt: pSSRRA %d", k);
    localrc = xxe->appendWtimer(0x0, tempString, xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    delete [] tempString;
#endif
    localrc = xxe->appendWaitOnIndex(0x0, recvnbIndex);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef ASMMPROFILE
    tempString = new char[80];
    sprintf(tempString, "Wt: done WaitOnIndex: %d", recvnbIndex);
    localrc = xxe->appendWtimer(0x0, tempString, xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    delete [] tempString;
#endif
    localrc = appendProductSum(xxe, srcTermProcessing, srcLocalDeCount,
      elementTK, valueTK, factorTK, dataSizeDst, dataSizeSrc, dataSizeFactors,
      rraList, rraCount);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x0, "Wt: /pSSRRA", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  
  struct LinIndexContigBlock{
    int linIndex;
    int linIndexCount;
  };
  struct SendnbElement{
    int dstPet;
    int dstDe;        // global DE index of dst DE in dst DELayout
    int dstLocalDe;   // local enumeration of dstDe
    int srcDe;        // global DE index of src DE in local DELayout
    int srcLocalDe;   // local enumeration of srcDe
    char *buffer;
    int partnerDeDataCount;
    int sendnbIndex;
    int vectorLength; // each element in srcInfoTable is a vector of this length
    vector<SrcInfo> srcInfoTable;
    vector<LinIndexContigBlock> linIndexContigBlockList;
    int localPet;
    int petCount;
    //
    int appendSendnb(XXE *xxe, int srcTermProcessing, XXE::TKId elementTK,
      XXE::TKId valueTK, XXE::TKId factorTK, int dataSizeSrc, char **rraList,
      int rraCount, int k);
  };
  bool operator<(SendnbElement a, SendnbElement b){
    int aDstPet = (a.dstPet - a.localPet + a.petCount) % a.petCount;
    int bDstPet = (b.dstPet - b.localPet + b.petCount) % b.petCount;
    if (aDstPet == bDstPet)
      if (a.srcDe == b.srcDe)
        return (a.dstDe < b.dstDe);
      else
        return (a.srcDe < b.srcDe);
    else
      return (aDstPet < bDstPet);
  }
  int SendnbElement::appendSendnb(XXE *xxe, int srcTermProcessing,
    XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK, int dataSizeSrc,
    char **rraList, int rraCount, int k){
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int tag = 0;  // no need for special tags - messages are ordered to match
    int j = srcLocalDe;
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      int count = linIndexContigBlockList.size();
#ifdef ASMMSTOREPRINT
      printf("gjt: XXE::sendnb from localPet %d to Pet %d\n", localPet, dstPet);
#endif
      if (count == 1){
        // sendnbRRA out of single contiguous linIndex run
#ifdef ASMMSTOREPRINT
        printf("gjt: single contiguous linIndex run on src side\n");
#endif
        sendnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendnbRRA(0x0, linIndexContigBlockList[0].linIndex
          * dataSizeSrc, partnerDeDataCount * dataSizeSrc * vectorLength,
          dstPet, j, tag);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }else{
        // use intermediate buffer
#ifdef ASMMSTOREPRINT
        printf("gjt: non-contiguous linIndex on src side -> need buffer \n");
#endif
        // use intermediate buffer
        // memGatherSrcRRA pieces into intermediate buffer
        int xxeIndex = xxe->count;  // need this beyond the increment
        localrc = xxe->appendMemGatherSrcRRA(0x0, buffer, valueTK, j, count,
          vectorLength);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        XXE::MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo =
          (XXE::MemGatherSrcRRAInfo *) &(xxe->stream[xxeIndex]);
        // try typekind specific memGatherSrcRRA
        for (int k=0; k<count; k++){
          xxeMemGatherSrcRRAInfo->rraOffsetList[k] =
            linIndexContigBlockList[k].linIndex * dataSizeSrc;
          xxeMemGatherSrcRRAInfo->countList[k] =
            linIndexContigBlockList[k].linIndexCount;
        }
        double dt_tk;
        localrc = xxe->exec(rraCount, rraList, 0x0, &dt_tk, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // try byte option for memGatherSrcRRA
        xxeMemGatherSrcRRAInfo->dstBaseTK = XXE::BYTE;
        for (int k=0; k<count; k++)
          xxeMemGatherSrcRRAInfo->countList[k] *= dataSizeSrc;  // scale to byte
        double dt_byte;
        localrc = xxe->exec(rraCount, rraList, 0x0, &dt_byte, xxeIndex, 
          xxeIndex);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
#ifdef ASMMSTOREPRINT
        printf("gjt - on localPet %d memGatherSrcRRA took dt_tk=%g s and"
          " dt_byte=%g s for count=%d\n", localPet, dt_tk, dt_byte, count);
#endif
        // decide for the fastest option
        if (dt_byte < dt_tk){
          // use byte option for memGatherSrcRRA
          // -> nothing to do because this was the last mode tested
        }else{
          // use typekind specific memGatherSrcRRA
          xxeMemGatherSrcRRAInfo->dstBaseTK = valueTK;
          for (int k=0; k<count; k++){
            xxeMemGatherSrcRRAInfo->countList[k] =
              linIndexContigBlockList[k].linIndexCount;
          }
        }
        // sendnb out of contiguous intermediate buffer
        sendnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendnb(0x0, buffer, partnerDeDataCount
          * dataSizeSrc * vectorLength, dstPet, tag);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }
    }else{
      // do some processing on the src side
      // determine bufferItemCount according to srcTermProcessingS
      int bufferItemCount = 0; // reset
      vector<ArrayHelper::SrcInfo>::iterator pp = srcInfoTable.begin();
      while (pp != srcInfoTable.end()){
        SeqIndex partnerSeqIndex = pp->partnerSeqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == srcInfoTable.end()) ||
            !(partnerSeqIndex == pp->partnerSeqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
      // zero out intermediate buffer
      localrc = xxe->appendZeroVector(0x0, buffer, bufferItemCount * dataSizeSrc
        * vectorLength);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      // use super-scalar "+=*" operation containing all terms
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarSrcRRA(0x0, valueTK, valueTK,
        factorTK, j, partnerDeDataCount, vectorLength);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      XXE::ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo =
        (XXE::ProductSumSuperScalarSrcRRAInfo *)&(xxe->stream[xxeIndex]);
      int *rraOffsetList = xxeProductSumSuperScalarSrcRRAInfo->rraOffsetList;
      void **factorList = xxeProductSumSuperScalarSrcRRAInfo->factorList;
      void **elementList = xxeProductSumSuperScalarSrcRRAInfo->elementList;
      // fill in rraOffsetList, factorList, valueList
      int bufferItem = 0; // reset
      int kk = 0; // reset
      pp = srcInfoTable.begin();  // reset
      while (pp != srcInfoTable.end()){
        SeqIndex partnerSeqIndex = pp->partnerSeqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          rraOffsetList[kk] = pp->linIndex * dataSizeSrc;
          factorList[kk] = (void *)(pp->factor);
          elementList[kk] = (void *)(buffer + bufferItem * dataSizeSrc
            * vectorLength);
          ++pp;
          ++kk;
          if ((pp == srcInfoTable.end()) ||
            !(partnerSeqIndex == pp->partnerSeqIndex)) break;
        } // for srcTermProcessing
        ++bufferItem;
      }
      // sendnb out of contiguous intermediate buffer
      sendnbIndex = xxe->count;  // store index for the associated wait
      localrc = xxe->appendSendnb(0x0, buffer, bufferItemCount * dataSizeSrc
        * vectorLength, dstPet, tag);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
    }
#ifdef ASMMPROFILE
    char *tempString = new char[100];
    sprintf(tempString, "<(%04d/%04d)-Snb(%d/%d)-(%04d/%04d)> ",
      srcDe, localPet, k, sendnbIndex, dstDe, dstPet);
    localrc = xxe->appendProfileMessage(0x0, tempString);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
      
  // more efficient allocation scheme for many little pieces of memory
  class MemHelper{
    class MemHelper *next;
    void *memAlloc;
    char *memPtr;
    int rest;
  public:
    MemHelper(){
      next = NULL;
      memAlloc = NULL;
      memPtr = NULL;
      rest = 0;
    }
    ~MemHelper(){
      if (memAlloc)
        std::free(memAlloc);
      if (next)
        delete next;
    }
    void *malloc(int bytes){
      const int limit=1024;     // 1kByte
      const int block=1048576;  // 1MByte
      if (bytes > limit)
        return std::malloc(bytes);
      if (memPtr==NULL){
        memAlloc = std::malloc(block);
        memPtr = (char *)memAlloc;
        rest = block;
      }
      if (rest >= bytes){
        rest -= bytes;
        void *mem = (void *)memPtr;
        memPtr += bytes;
        return mem;
      }else if(next)
        return next->malloc(bytes);
      else{
        next = new MemHelper();
        return next->malloc(bytes);
      }
    }
  };
  
} // ArrayHelper


namespace DD{
  struct Interval{
    int min;
    int max;
    int count;
    int countEff;
  };
  struct FactorElement{
    SeqIndex partnerSeqIndex;
    int partnerDe;
    int padding;    // padding for 8-byte alignment
    char factor[8]; // large enough for R8 and I8
  };
  struct SeqIndexFactorLookup{
    int de;
    int factorCount;
    FactorElement *factorList;
  };
  
  // the following structs could be moved out of this namespace
  struct AssociationElement{
    int linIndex;
    SeqIndex seqIndex;
    int factorCount;
    FactorElement *factorList;
  };
  struct FillLinSeqListInfo{
    AssociationElement **linSeqList;
    int localPet;
    int localDeCount;
    const int *localDeElementCount;
    const Interval *seqIndexInterval;
    const SeqIndexFactorLookup *seqIndexFactorLookup;
    bool tensorMixFlag;
    ArrayHelper::MemHelper *memHelper;
  };
  struct FillPartnerDeInfo{
    int localPet;
    const Interval *seqIndexIntervalIn;
    const Interval *seqIndexIntervalOut;
    const SeqIndexFactorLookup *seqIndexFactorLookupIn;
    const SeqIndexFactorLookup *seqIndexFactorLookupOut;
    bool tensorMixFlag;
  };
  struct FillSelfDeInfo{
    AssociationElement **linSeqList;
    int localPet;
    int localDeCount;
    const int *localDeElementCount;
    const int *localDeList;
    const Interval *seqIndexInterval;
    SeqIndexFactorLookup *seqIndexFactorLookup;
    bool tensorMixFlag;
  };

// -- accessLookup()

template<typename T>
int requestSizeFactor(T *t);

template<typename T>
void clientRequest(T *t, int i, char **requestStreamClient);

template<typename T>
void localClientServerExchange(T *t);

template<typename T>
int serverResponseSize(T *t, int count, int i, char **requestStreamServer);

template<typename T>
void serverResponse(T *t, int count, int i, char **requestStreamServer,
  char **responseStreamServer);

template<typename T>
void clientProcess(T *t, char *responseStream, int responseStreamSize);

template<typename T>
void accessLookup(
  ESMCI::VM *vm,
  int petCount,
  int localPet,
  int *localIntervalPerPetCount,
  int *localElementsPerIntervalCount,
  T *t
  ){
  // access look up table
  VMK::commhandle **send1commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **send2commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **send3commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recv1commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recv2commhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recv3commhList = new VMK::commhandle*[petCount];
  char **requestStreamClient = new char*[petCount];
  char **requestStreamServer = new char*[petCount];
  int *responseStreamSizeClient = new int[petCount];
  int *responseStreamSizeServer = new int[petCount];
  char **responseStreamClient = new char*[petCount];
  char **responseStreamServer = new char*[petCount];
  // t-specific routine
  int requestFactor = requestSizeFactor(t);
  // localPet acts as server, posts non-blocking recvs for all client requests
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    // receive request from Pet "i"
    int count = localIntervalPerPetCount[i];
    if (count>0){
      requestStreamServer[i] = new char[requestFactor*count];
      recv3commhList[i] = NULL;
      vm->recv(requestStreamServer[i], requestFactor*count, i,
        &(recv3commhList[i]));
    }
  }
  // localPet acts as a client, sends its requests to the appropriate servers
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet "i"
      requestStreamClient[i] =
        new char[requestFactor*localElementsPerIntervalCount[i]];
      // t-specific client routine
      clientRequest(t, i, requestStreamClient);
      // send information to the serving Pet
      send1commhList[i] = NULL;
      vm->send(requestStreamClient[i],
        requestFactor*localElementsPerIntervalCount[i], i, 
        &(send1commhList[i]));
      // post receive to obtain response size from server Pet
      recv1commhList[i] = NULL;
      vm->recv(&(responseStreamSizeClient[i]), sizeof(int), i,
        &(recv1commhList[i]));
    }
  }
  // localPet locally acts as server and client to fill its own request
  // t-specific client-server routine
  localClientServerExchange(t);
  // localPet acts as server, processing requests from clients, send response sz
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
      // wait for request from Pet "i"
      vm->commwait(&(recv3commhList[i]));
      // t-specific server routine
      int responseStreamSize =
        serverResponseSize(t, count, i, requestStreamServer);
      // send response size to client Pet "i"
      responseStreamSizeServer[i] = responseStreamSize;
      send2commhList[i] = NULL;
      vm->send(&(responseStreamSizeServer[i]), sizeof(int), i,
        &(send2commhList[i]));
    }
  }
  // localPet acts as a client, waits for response size from server and posts
  // receive for response stream
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
      // wait to receive response size from the serving Pet "i"
      vm->commwait(&(recv1commhList[i]));
      int responseStreamSize = responseStreamSizeClient[i];
      if (responseStreamSize>0){
        responseStreamClient[i] = new char[responseStreamSize];
        recv2commhList[i] = NULL;
        vm->recv(responseStreamClient[i], responseStreamSize, i,
          &(recv2commhList[i]));
      }
    }
  }
  // localPet acts as server, send response stream
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
      int responseStreamSize = responseStreamSizeServer[i];
      if (responseStreamSize>0){
        // construct response stream
        responseStreamServer[i] = new char[responseStreamSize];
        // t-specific server routine
        serverResponse(t, count, i, requestStreamServer, responseStreamServer);
        // send response stream to client Pet "i"
        send3commhList[i] = NULL;
        vm->send(responseStreamServer[i], responseStreamSize, i,
          &(send3commhList[i]));
        // garbage collection
        delete [] requestStreamServer[i];
      }
    }
  }
  // localPet acts as a client, waits for response stream from server, process
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
      int responseStreamSize = responseStreamSizeClient[i];
      if (responseStreamSize>0){
        // wait to receive response stream from the serving Pet "i"
        vm->commwait(&(recv2commhList[i]));
        // process responseStream and complete t[][] info
        char *responseStream = responseStreamClient[i];
        // t-specific client routine
        clientProcess(t, responseStream, responseStreamSize);
        // garbage collection
        delete [] responseStreamClient[i];
      }
    }
  }
  // localPet acts as a client, wait for sends to complete and collect garbage
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
      // wait for send
      vm->commwait(&(send1commhList[i]));
      // garbage collection
      delete [] requestStreamClient[i];
    }
  }
  // localPet acts as server, wait for sends to complete and collect garbage
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
      vm->commwait(&(send2commhList[i]));
      if (responseStreamSizeServer[i]>0){
        vm->commwait(&(send3commhList[i]));
        // garbage collection
        delete [] responseStreamServer[i];
      }
    }
  }  
  // garbage collection
  delete [] requestStreamClient;
  delete [] requestStreamServer;
  delete [] responseStreamClient;
  delete [] responseStreamServer;
  delete [] responseStreamSizeClient;
  delete [] responseStreamSizeServer;
  delete [] send1commhList;
  delete [] send2commhList;
  delete [] send3commhList;
  delete [] recv1commhList;
  delete [] recv2commhList;
  delete [] recv3commhList;
}

// FillLinSeqListInfo-specific accessLookup routines

int requestSizeFactor(FillLinSeqListInfo *fillLinSeqListInfo){
  return 3*sizeof(int); // lookupIndex, j, k
}

void clientRequest(FillLinSeqListInfo *fillLinSeqListInfo, int i,
  char **requestStreamClient){
  const int localDeCount = fillLinSeqListInfo->localDeCount;
  const int *localDeElementCount = fillLinSeqListInfo->localDeElementCount;
  AssociationElement **linSeqList = fillLinSeqListInfo->linSeqList;
  const Interval *seqIndexInterval = fillLinSeqListInfo->seqIndexInterval;
  const bool tensorMixFlag = fillLinSeqListInfo->tensorMixFlag;
  // fill the requestStreamClient[i] element
  int seqIndexMin = seqIndexInterval[i].min;
  int seqIndexMax = seqIndexInterval[i].max;
  int seqIndexCount = seqIndexInterval[i].count;
  int jj = 0; // reset
  for (int j=0; j<localDeCount; j++){
    for (int k=0; k<localDeElementCount[j]; k++){
      int seqIndex = linSeqList[j][k].seqIndex.decompSeqIndex;
      if (seqIndex >= seqIndexMin && seqIndex <= seqIndexMax){
        int lookupIndex = seqIndex - seqIndexMin;
        if (tensorMixFlag){
          lookupIndex +=
            (linSeqList[j][k].seqIndex.tensorSeqIndex - 1) * seqIndexCount;
        }
        int *requestStreamClientInt = (int *)requestStreamClient[i];
        requestStreamClientInt[3*jj] = lookupIndex;
        requestStreamClientInt[3*jj+1] = j;
        requestStreamClientInt[3*jj+2] = k;
        ++jj; // increment counter
      }
    }
  }
}

void localClientServerExchange(FillLinSeqListInfo *fillLinSeqListInfo){
  const int localPet = fillLinSeqListInfo->localPet;
  const int localDeCount = fillLinSeqListInfo->localDeCount;
  const int *localDeElementCount = fillLinSeqListInfo->localDeElementCount;
  AssociationElement **linSeqList = fillLinSeqListInfo->linSeqList;
  const Interval *seqIndexInterval = fillLinSeqListInfo->seqIndexInterval;
  const bool tensorMixFlag = fillLinSeqListInfo->tensorMixFlag;
  ArrayHelper::MemHelper *memHelper = fillLinSeqListInfo->memHelper;
  const SeqIndexFactorLookup *seqIndexFactorLookup =
    fillLinSeqListInfo->seqIndexFactorLookup;
  // localPet locally acts as server and client
  int seqIndexMin = seqIndexInterval[localPet].min;
  int seqIndexMax = seqIndexInterval[localPet].max;
  int seqIndexCount = seqIndexInterval[localPet].count;
  for (int j=0; j<localDeCount; j++){
    for (int k=0; k<localDeElementCount[j]; k++){
      int seqIndex = linSeqList[j][k].seqIndex.decompSeqIndex;
      if (seqIndex >= seqIndexMin && seqIndex <= seqIndexMax){
        int kk = seqIndex - seqIndexMin;
        if (tensorMixFlag){
          kk += (linSeqList[j][k].seqIndex.tensorSeqIndex - 1)
            * seqIndexCount;
        }
        int factorCount = seqIndexFactorLookup[kk].factorCount;
        if (factorCount){
          linSeqList[j][k].factorCount = factorCount;
          linSeqList[j][k].factorList = (FactorElement *)
            memHelper->malloc(sizeof(FactorElement)*factorCount);
          memcpy(linSeqList[j][k].factorList, 
            seqIndexFactorLookup[kk].factorList,
            factorCount * sizeof(FactorElement));
        }
      }
    }
  }
}

int serverResponseSize(FillLinSeqListInfo *fillLinSeqListInfo, int count,
  int i, char **requestStreamServer){
  const SeqIndexFactorLookup *seqIndexFactorLookup =
    fillLinSeqListInfo->seqIndexFactorLookup;
  // process requestStreamServer[i] and return response stream size
  int indexCounter = 0; // reset
  int factorElementCounter = 0; // reset
  int *requestStreamServerInt = (int *)requestStreamServer[i];
  for (int j=0; j<count; j++){
    int lookupIndex = requestStreamServerInt[3*j];
    int factorCount = seqIndexFactorLookup[lookupIndex].factorCount;
    if (factorCount){
      ++indexCounter;
      factorElementCounter += factorCount;
    }
  }
  int responseStreamSize = 4*indexCounter*sizeof(int)
    + factorElementCounter*sizeof(FactorElement);
  return responseStreamSize;
}
      
void serverResponse(FillLinSeqListInfo *fillLinSeqListInfo, int count,
  int i, char **requestStreamServer, char **responseStreamServer){
  const SeqIndexFactorLookup *seqIndexFactorLookup =
    fillLinSeqListInfo->seqIndexFactorLookup;
  // construct response stream
  int *responseStreamInt;
  FactorElement *responseStreamFactorElement =
    (FactorElement *)responseStreamServer[i];
  int *requestStreamServerInt = (int *)requestStreamServer[i];
  for (int jj=0; jj<count; jj++){
    int lookupIndex = requestStreamServerInt[3*jj];
    int factorCount = seqIndexFactorLookup[lookupIndex].factorCount;
    if (factorCount){
      responseStreamInt = (int *)responseStreamFactorElement;
      *responseStreamInt++ = requestStreamServerInt[3*jj+1];  // j
      *responseStreamInt++ = requestStreamServerInt[3*jj+2];  // k
      *responseStreamInt++ = factorCount;
      *responseStreamInt++ = 0; // padding for 8-byte alignment
      responseStreamFactorElement = (FactorElement *)responseStreamInt;
      memcpy(responseStreamFactorElement,
        seqIndexFactorLookup[lookupIndex].factorList,
        factorCount * sizeof(FactorElement));
      responseStreamFactorElement += factorCount;
    }
  }
}        
        
void clientProcess(FillLinSeqListInfo *fillLinSeqListInfo,
  char *responseStream, int responseStreamSize){
  AssociationElement **linSeqList = fillLinSeqListInfo->linSeqList;
  ArrayHelper::MemHelper *memHelper = fillLinSeqListInfo->memHelper;
  // process responseStream and complete linSeqList[][] info
  int *responseStreamInt;
  FactorElement *responseStreamFactorElement =
    (FactorElement *)responseStream;
  while ((char *)responseStreamFactorElement !=
    responseStream+responseStreamSize){
    responseStreamInt = (int *)responseStreamFactorElement;
    int j = *responseStreamInt++;
    int k = *responseStreamInt++;
    int factorCount = *responseStreamInt++;
    responseStreamInt++;  // skip padding
    linSeqList[j][k].factorCount = factorCount;
    linSeqList[j][k].factorList = (FactorElement *)
      memHelper->malloc(sizeof(FactorElement)*factorCount);
    responseStreamFactorElement = (FactorElement *)responseStreamInt;
    memcpy(linSeqList[j][k].factorList, responseStreamFactorElement,
      factorCount * sizeof(FactorElement));
    responseStreamFactorElement += factorCount;
  }
}
        
// FillPartnerDeInfo-specific accessLookup routines

int requestSizeFactor(FillPartnerDeInfo *fillPartnerDeInfo){
  return 3*sizeof(int); // lookupIndex, j, k
}

void clientRequest(FillPartnerDeInfo *fillPartnerDeInfo, int i,
  char **requestStreamClient){
  const int localPet = fillPartnerDeInfo->localPet;
  const Interval *seqIndexIntervalIn = fillPartnerDeInfo->seqIndexIntervalIn;
  const Interval *seqIndexIntervalOut = fillPartnerDeInfo->seqIndexIntervalOut;
  const SeqIndexFactorLookup *seqIndexFactorLookupOut =
    fillPartnerDeInfo->seqIndexFactorLookupOut;
  const bool tensorMixFlag = fillPartnerDeInfo->tensorMixFlag;
  // fill the requestStreamClient[i] element
  int seqIndexMin = seqIndexIntervalIn[i].min;
  int seqIndexMax = seqIndexIntervalIn[i].max;
  int seqIndexCount = seqIndexIntervalIn[i].count;
  int jj = 0; // reset
  for (int j=0; j<seqIndexIntervalOut[localPet].countEff; j++){
    for (int k=0; k<seqIndexFactorLookupOut[j].factorCount; k++){
      int partnerSeqIndex = seqIndexFactorLookupOut[j].factorList[k]
        .partnerSeqIndex.decompSeqIndex;
      if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax){
        int lookupIndex = partnerSeqIndex - seqIndexMin;
        if (tensorMixFlag){
          lookupIndex += (seqIndexFactorLookupOut[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex - 1) * seqIndexCount;
        }
        int *requestStreamClientInt = (int *)requestStreamClient[i];
        requestStreamClientInt[3*jj] = lookupIndex;
        requestStreamClientInt[3*jj+1] = j;
        requestStreamClientInt[3*jj+2] = k;
        ++jj; // increment counter
      }
    }
  }
}

void localClientServerExchange(FillPartnerDeInfo *fillPartnerDeInfo){
  const int localPet = fillPartnerDeInfo->localPet;
  const Interval *seqIndexIntervalIn = fillPartnerDeInfo->seqIndexIntervalIn;
  const Interval *seqIndexIntervalOut = fillPartnerDeInfo->seqIndexIntervalOut;
  const SeqIndexFactorLookup *seqIndexFactorLookupIn =
    fillPartnerDeInfo->seqIndexFactorLookupIn;
  const SeqIndexFactorLookup *seqIndexFactorLookupOut =
    fillPartnerDeInfo->seqIndexFactorLookupOut;
  const bool tensorMixFlag = fillPartnerDeInfo->tensorMixFlag;
  // localPet locally acts as server and client
  int seqIndexMin = seqIndexIntervalIn[localPet].min;
  int seqIndexMax = seqIndexIntervalIn[localPet].max;
  int seqIndexCount = seqIndexIntervalIn[localPet].count;
  for (int j=0; j<seqIndexIntervalOut[localPet].countEff; j++){
    for (int k=0; k<seqIndexFactorLookupOut[j].factorCount; k++){
      int partnerSeqIndex = seqIndexFactorLookupOut[j].factorList[k]
        .partnerSeqIndex.decompSeqIndex;
      if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax){
        int kk = partnerSeqIndex - seqIndexMin;
        if (tensorMixFlag){
          kk += (seqIndexFactorLookupOut[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex - 1) * seqIndexCount;
        }
        seqIndexFactorLookupOut[j].factorList[k].partnerDe =
          seqIndexFactorLookupIn[kk].de;
      }
    }
  }
}

int serverResponseSize(FillPartnerDeInfo *fillPartnerDeInfo, int count,
  int i, char **requestStreamServer){
  int responseStreamSize = 3*count*sizeof(int);
  return responseStreamSize;
}
      
void serverResponse(FillPartnerDeInfo *fillPartnerDeInfo, int count,
  int i, char **requestStreamServer, char **responseStreamServer){
  const SeqIndexFactorLookup *seqIndexFactorLookupIn =
    fillPartnerDeInfo->seqIndexFactorLookupIn;
  // construct response stream
  int *responseStreamInt = (int *)responseStreamServer[i];
  int *requestStreamServerInt = (int *)requestStreamServer[i];
  for (int jj=0; jj<count; jj++){
    int lookupIndex = requestStreamServerInt[3*jj];
    *responseStreamInt++ = seqIndexFactorLookupIn[lookupIndex].de;  // de
    *responseStreamInt++ = requestStreamServerInt[3*jj+1];  // j
    *responseStreamInt++ = requestStreamServerInt[3*jj+2];  // k
  }
}        
        
void clientProcess(FillPartnerDeInfo *fillPartnerDeInfo,
  char *responseStream, int responseStreamSize){
  const SeqIndexFactorLookup *seqIndexFactorLookupOut =
    fillPartnerDeInfo->seqIndexFactorLookupOut;
  // process responseStream and complete seqIndexFactorLookupOut info
  int *responseStreamInt = (int *)responseStream;
  while ((char *)responseStreamInt != responseStream+responseStreamSize){
    int de = *responseStreamInt++;
    int j = *responseStreamInt++;
    int k = *responseStreamInt++;
    seqIndexFactorLookupOut[j].factorList[k].partnerDe = de;
  }
}

// -- updateLookup()

template<typename T>
int updateSizeFactor(T *t);

template<typename T>
void clientUpdate(T *t, int i, char **updateStreamClient);

template<typename T>
void localClientServerUpdate(T *t);

template<typename T>
int serverUpdate(T *t, int count, int i, char **updateStreamServer);

template<typename T>
void updateLookup(
  ESMCI::VM *vm,
  int petCount,
  int localPet,
  int *localIntervalPerPetCount,
  int *localElementsPerIntervalCount,
  T *t
  ){
  // update look up table
  VMK::commhandle **sendcommhList = new VMK::commhandle*[petCount];
  VMK::commhandle **recvcommhList = new VMK::commhandle*[petCount];
  char **updateStreamClient = new char*[petCount];
  char **updateStreamServer = new char*[petCount];
  // t-specific routine
  int updateFactor = updateSizeFactor(t);
  // localPet acts as server, posts non-blocking recvs for all client updates
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    // receive update from Pet "i"
    int count = localIntervalPerPetCount[i];
    if (count>0){
      updateStreamServer[i] = new char[updateFactor*count];
      recvcommhList[i] = NULL;
      vm->recv(updateStreamServer[i], updateFactor*count, i,
        &(recvcommhList[i]));
    }
  }
  // localPet acts as a client, sends its updates to the appropriate servers
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet "i"
      updateStreamClient[i] =
        new char[updateFactor*localElementsPerIntervalCount[i]];
      // t-specific client routine
      clientUpdate(t, i, updateStreamClient);
      // send information to the serving Pet
      sendcommhList[i] = NULL;
      vm->send(updateStreamClient[i],
        updateFactor*localElementsPerIntervalCount[i], i, 
        &(sendcommhList[i]));
    }
  }
  // localPet locally acts as server and client to do update
  // t-specific client-server routine
  localClientServerUpdate(t);
  // localPet acts as server, processing updates from clients
  for (int ii=localPet+1; ii<localPet+petCount; ii++){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    int count = localIntervalPerPetCount[i];
    if (count>0){
      // wait for update from Pet "i"
      vm->commwait(&(recvcommhList[i]));
      // t-specific server routine
      serverUpdate(t, count, i, updateStreamServer);
    }
  }
  // localPet acts as a client, wait for sends to complete and collect garbage
  for (int ii=localPet+petCount-1; ii>localPet; ii--){
    // localPet-dependent shifted loop reduces communication contention
    int i = ii%petCount;  // fold back into [0,..,petCount-1] range
    if (localElementsPerIntervalCount[i]>0){
      // localPet has elements that are located in interval of server Pet i
      // wait for send
      vm->commwait(&(sendcommhList[i]));
      // garbage collection
      delete [] updateStreamClient[i];
    }
  }
  // garbage collection
  delete [] updateStreamClient;
  delete [] updateStreamServer;
  delete [] sendcommhList;
  delete [] recvcommhList;
}

// FillSelfDeInfo-specific updateLookup routines

int updateSizeFactor(FillSelfDeInfo *fillSelfDeInfo){
  return 2*sizeof(int); // lookupIndex, de
}

void clientUpdate(FillSelfDeInfo *fillSelfDeInfo, int i,
  char **updateStreamClient){
  const int localDeCount = fillSelfDeInfo->localDeCount;
  const int *localDeList = fillSelfDeInfo->localDeList;
  const int *localDeElementCount = fillSelfDeInfo->localDeElementCount;
  const Interval *seqIndexInterval = fillSelfDeInfo->seqIndexInterval;
  AssociationElement **linSeqList = fillSelfDeInfo->linSeqList;
  const bool tensorMixFlag = fillSelfDeInfo->tensorMixFlag;
  // fill the updateStreamClient[i] element
  int seqIndexMin = seqIndexInterval[i].min;
  int seqIndexMax = seqIndexInterval[i].max;
  int seqIndexCount = seqIndexInterval[i].count;
  int jj = 0; // reset
  for (int j=0; j<localDeCount; j++){
    int de = localDeList[j];  // global DE number
    for (int k=0; k<localDeElementCount[j]; k++){
      int seqIndex = linSeqList[j][k].seqIndex.decompSeqIndex;
      if (seqIndex >= seqIndexMin && seqIndex <= seqIndexMax){
        int lookupIndex = seqIndex - seqIndexMin;
        if (tensorMixFlag){
          lookupIndex += (linSeqList[j][k].seqIndex.tensorSeqIndex - 1)
            * seqIndexCount;
        }
        int *updateStreamClientInt = (int *)updateStreamClient[i];
        updateStreamClientInt[2*jj] = lookupIndex;
        updateStreamClientInt[2*jj+1] = de;
        ++jj; // increment counter
      }
    }
  }
}

void localClientServerUpdate(FillSelfDeInfo *fillSelfDeInfo){
  const int localPet = fillSelfDeInfo->localPet;
  const int localDeCount = fillSelfDeInfo->localDeCount;
  const int *localDeList = fillSelfDeInfo->localDeList;
  const int *localDeElementCount = fillSelfDeInfo->localDeElementCount;
  const Interval *seqIndexInterval = fillSelfDeInfo->seqIndexInterval;
  AssociationElement **linSeqList = fillSelfDeInfo->linSeqList;
  const bool tensorMixFlag = fillSelfDeInfo->tensorMixFlag;
  SeqIndexFactorLookup *seqIndexFactorLookup =
    fillSelfDeInfo->seqIndexFactorLookup;
  // localPet locally acts as server and client
  int seqIndexMin = seqIndexInterval[localPet].min;
  int seqIndexMax = seqIndexInterval[localPet].max;
  int seqIndexCount = seqIndexInterval[localPet].count;
  for (int j=0; j<localDeCount; j++){
    int de = localDeList[j];  // global DE number
    for (int k=0; k<localDeElementCount[j]; k++){
      int seqIndex = linSeqList[j][k].seqIndex.decompSeqIndex;
      if (seqIndex >= seqIndexMin && seqIndex <= seqIndexMax){
        int lookupIndex = seqIndex - seqIndexMin;
        if (tensorMixFlag){
          lookupIndex += (linSeqList[j][k].seqIndex.tensorSeqIndex - 1)
            * seqIndexCount;
        }
        seqIndexFactorLookup[lookupIndex].de = de;
      }
    }
  }
}

void serverUpdate(FillSelfDeInfo *fillSelfDeInfo, int count,
  int i, char **updateStreamServer){
  SeqIndexFactorLookup *seqIndexFactorLookup =
    fillSelfDeInfo->seqIndexFactorLookup;
  // update server according to update stream
  int *updateStreamServerInt = (int *)updateStreamServer[i];
  for (int jj=0; jj<count; jj++){
    int lookupIndex = updateStreamServerInt[2*jj];
    seqIndexFactorLookup[lookupIndex].de = updateStreamServerInt[2*jj+1];
  }
}        
        
} // namespace DD


#define ASMMSTORETIMING___disable

int sparseMatMulStoreEncodeXXE(VM *vm, DELayout *srcDelayout,
  DELayout *dstDelayout, bool tensorMixFlag, 
  int srcTensorContigLength, int dstTensorContigLength,
  ESMC_TypeKind typekindFactors, ESMC_TypeKind typekindSrc,
  ESMC_TypeKind typekindDst,
  const int *srcLocalDeElementCount, const int *dstLocalDeElementCount,
  DD::AssociationElement **srcLinSeqList,
  DD::AssociationElement **dstLinSeqList,
  const int *dstLocalDeTotalElementCount,
  char **rraList, int rraCount, ESMC_RouteHandle **routehandle
#ifdef ASMMSTORETIMING
  , double *t8, double *t9, double *t10, double *t11, double *t12, double *t13,
  double *t14
#endif
  );

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
  ESMC_TypeKind typekindFactors,        // in    - typekind of factors
  void *factorList,                     // in    - sparse matrix factors
  int factorListCount,                  // in    - number of sparse mat. indices
  InterfaceInt *factorIndexList         // in    - sparse matrix indices
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for sparse matrix multiplication
//  from srcArray to dstArray.
//
//  The implementation consists of four main phases:
//
//  - Phase I:    Check input for consistency. The sparse matrix is provided in
//                "input" distribution 
//  - Phase II:   Construct two distributed directories of sparse matrix
//                elements, one indexed by srcSeqIndex, one indexed by
//                dstSeqIndex. This takes the matrix from "input" to "work"
//                distribution.
//  - Phase III:  Use the information in "work" distribution and take it into 
//                "run" distribution, i.e. src and dst DEs have access to all
//                the local data they may operate on.
//  - Phase IV:   Use the information in "run" distribution to encode an
//                optimized XXE stream, balancing src/dst work-loads and
//                pipelining overlapping communications and computation.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{
  
  //---------------------------------------------------------------------------
  // Phase I
  //---------------------------------------------------------------------------
  
  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  
#ifdef ASMMSTORETIMING
  double t0, t1, t2, t3, t4, t5, t6, t7;  //gjt - profile
  double t8, t9, t10, t11, t12, t13, t14, t15; //gjt - profile
  double t4a, t4b;  //gjt - profile
  double t4a1, t4a2, t4a3;  //gjt - profile  
  double t4b1, t4b2, t4b3;  //gjt - profile
  double t5a, t5b, t5c, t5d, t5e, t5f;  //gjt - profile
  double t5a1, t5a2, t5a3;  //gjt - profile  
  double t5b1, t5b2, t5b3, t5b4, t5b5;  //gjt - profile
  double t5b3a, t5b3b, t5b3c, t5b3d;  //gjt - profile
  double dt5b34=0., dt5b45=0.;  //gjt - profile
  double dt5b3ab=0., dt5b3bc=0., dt5b3cd=0.;  //gjt - profile
  vm->barrier();      //synchronize start time across all PETs
  VMK::wtime(&t0);    //gjt - profile
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
  
  // srcArray and dstArray may not point to the identical Array object
  if (srcArray == dstArray){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- srcArray and dstArray must not be identical", &rc);
    return rc;
  }
  
  // every Pet that specifies factorListCount > 0 must be checked wrt input
  bool tensorMixFlag = false;     // default
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
    if (factorIndexList->extent[0] != 2 && factorIndexList->extent[0] != 4){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- first dimension of factorIndexList array must be of size 2 or 4",
        &rc);
      return rc;
    }
    if (factorIndexList->extent[1] != factorListCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
        "- second dimension of factorIndexList does not match factorListCount",
        &rc);
      return rc;
    }
    // must define a valid typekind
    if (typekindFactors == ESMF_NOKIND){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- must specify valid typekindFactors on PETs that provide factorList",
        &rc);
      return rc;
    }
    if (typekindFactors != ESMC_TYPEKIND_I4
      && typekindFactors != ESMC_TYPEKIND_I8
      && typekindFactors != ESMC_TYPEKIND_R4
      && typekindFactors != ESMC_TYPEKIND_R8){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- method not implemented for specified typekindFactors", &rc);
      return rc;
    }
    // check if tensorMixFlag must be set
    if (factorIndexList->extent[0] == 4)
      tensorMixFlag = true;
  }else{
    // set typekindFactors to ESMF_NOKIND 
    // -> this Pet will not be entered into the factorPetList and thus will
    // not act as rootPet when factors are being distributed
    typekindFactors = ESMF_NOKIND;
  }

  // communicate typekindFactors across all Pets
  ESMC_TypeKind *typekindList = new ESMC_TypeKind[petCount];
  vm->allgather(&typekindFactors, typekindList, sizeof(ESMC_TypeKind));
  // communicate tensorMixFlag across all Pets
  bool *tensorMixFlagList = new bool[petCount];
  vm->allgather(&tensorMixFlag, tensorMixFlagList, sizeof(bool));
  // Check that all non-ESMF_NOKIND typekindList elements match,
  // set local typekindFactors accordingly and keep track of Pets that have
  // factors. At the same time check that tensorMixFlag matches across Pets
  // that provide factors.
  int *factorPetList = new int[petCount];
  int factorPetCount = 0; // reset
  typekindFactors = ESMF_NOKIND;  // initialize
  for (int i=0; i<petCount; i++){
    if (typekindList[i] != ESMF_NOKIND)
      factorPetList[factorPetCount++] = i;
    if (typekindFactors == ESMF_NOKIND){
      typekindFactors = typekindList[i];  // set to 1st element not ESMF_NOKIND
      tensorMixFlag = tensorMixFlagList[i];
    }else{
      // check following elements against the typekindFactors and tensorMixFlag
      if (typekindList[i] != ESMF_NOKIND){
        if (typekindFactors != typekindList[i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- TypeKind mismatch between PETs", &rc);
          return rc;
        }
        if (tensorMixFlag != tensorMixFlagList[i]){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- Mismatch between PETs in size of first dimension of"
            " factorIndexList array", &rc);
          return rc;
        }
      }
    }
  }
  delete [] typekindList;
  delete [] tensorMixFlagList;
  
  // check that factorPetCount at least 1
  if (factorPetCount < 1){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "- at least one PET must provide a valid factorList", &rc);
    // garbage collection before bail out
    delete [] factorPetList;
    return rc;
  }
  
  // set dataSize for factors
  int dataSizeFactors = ESMC_TypeKindSize(typekindFactors);
  
  // check that if tensorMixFlag is not set that the tensorElementCount matches
  if (!tensorMixFlag){
    if (srcArray->getTensorElementCount() != dstArray->getTensorElementCount()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- factorIndexList without tensor mixing requires matching srcArray and"
        " dstArray tensorElementCount", &rc);
      // garbage collection before bail out
      delete [] factorPetList;
      return rc;
    }
  }
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t1);   //gjt - profile
#endif
    
  //---------------------------------------------------------------------------
  // Phase II
  //---------------------------------------------------------------------------

  // determine local srcElementCount
  int srcLocalDeCount = srcArray->delayout->getLocalDeCount();
  const int *srcLocalDeList = srcArray->delayout->getLocalDeList();
  int *srcLocalDeElementCount = new int[srcLocalDeCount];
  int srcElementCount = 0;   // initialize
  for (int i=0; i<srcLocalDeCount; i++){
    int de = srcLocalDeList[i];  // global DE index
    srcLocalDeElementCount[i] = srcArray->exclusiveElementCountPDe[de]
      * srcArray->tensorElementCount;
    srcElementCount += srcLocalDeElementCount[i];
  }
  // communicate srcElementCount across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *srcElementCountList = new int[petCount];
  vm->allgather(&srcElementCount, srcElementCountList, sizeof(int));
  // determine local dstElementCount
  int dstLocalDeCount = dstArray->delayout->getLocalDeCount();
  const int *dstLocalDeList = dstArray->delayout->getLocalDeList();
  int *dstLocalDeElementCount = new int[dstLocalDeCount];
  int dstElementCount = 0;   // initialize
  for (int i=0; i<dstLocalDeCount; i++){
    int de = dstLocalDeList[i];  // global DE index
    dstLocalDeElementCount[i] = dstArray->exclusiveElementCountPDe[de]
      * dstArray->tensorElementCount;
    dstElementCount += dstLocalDeElementCount[i];
  }
  // communicate dstElementCount across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *dstElementCountList = new int[petCount];
  vm->allgather(&dstElementCount, dstElementCountList, sizeof(int));
  
  // set the effective tensorElementCount for src and dst Arrays
  int srcTensorElementCountEff = srcArray->tensorElementCount;  // default
  int dstTensorElementCountEff = dstArray->tensorElementCount;  // default
  if (!tensorMixFlag){
    // if there is not tensor mixing then default into tensor for tensor mode
    srcTensorElementCountEff = 1;
    dstTensorElementCountEff = 1;
  }    

#ifdef ASMMSTORETIMING
  VMK::wtime(&t2);   //gjt - profile
#endif
  
  // determine linIndex <-> seqIndex association for all elements in exclusive 
  // region on all localDEs on srcArray
  int srcRank = srcArray->rank;
  int *ii = new int[srcRank];     // index tuple basis 0
  int *iiEnd = new int[srcRank];
  DD::AssociationElement **srcLinSeqList = 
    new DD::AssociationElement*[srcLocalDeCount];
  int srcSeqIndexMinMax[2]; // [0]=min, [1]=max
  for (int i=0; i<srcLocalDeCount; i++){
    // allocate memory in srcLinSeqList for this DE
    srcLinSeqList[i] = new DD::AssociationElement[srcLocalDeElementCount[i]];
    // only go into the multi-dim loop if there are elements for the local DE
    if (srcLocalDeElementCount[i]){
      // reset counters
      int joff = i*srcArray->distgrid->getDimCount();  // offset into bounds
      int packedIndex = 0;    // reset
      int tensorIndex = 0;    // reset
      for (int jj=0; jj<srcRank; jj++){
        ii[jj] = 0;  // reset
        if (srcArray->arrayToDistGridMap[jj]){
          // decomposed dimension 
          int j = packedIndex;
          iiEnd[jj] = srcArray->exclusiveUBound[joff+j]
            - srcArray->exclusiveLBound[joff+j] + 1;
          ++packedIndex;
        }else{
          // tensor dimension
          iiEnd[jj] = srcArray->undistUBound[tensorIndex]
            - srcArray->undistLBound[tensorIndex] + 1;
          ++tensorIndex;
        }
      }
      // loop over all elements in exclusive region for this DE
      int elementIndex = 0;  // reset
      while(ii[srcRank-1] < iiEnd[srcRank-1]){
        //printf("src: DE = %d  - (", de);
        //int jjj;
        //for (jjj=0; jjj<srcRank-1; jjj++)
        //  printf("%d, ", ii[jjj]);
        //printf("%d)\n", ii[jjj]);
        // determine the lin. index for element ii[] in localArray for this DE
        int linIndex = srcArray->getLinearIndexExclusive(i, ii);
        // determine the sequentialized index for element ii[] in this DE
        // getSequenceIndexExclusive() expects basis 0 ii[] in excl. region
        SeqIndex seqIndex = srcArray->getSequenceIndexExclusive(i, ii);
        // store linIndex and seqIndex in srcLinSeqList for this DE
        srcLinSeqList[i][elementIndex].linIndex = linIndex;
        srcLinSeqList[i][elementIndex].seqIndex = seqIndex;
        // reset factorCount
        srcLinSeqList[i][elementIndex].factorCount = 0;
        // record seqIndex min and max
        if (i==0 && elementIndex==0)
          srcSeqIndexMinMax[0] = srcSeqIndexMinMax[1]
            = seqIndex.decompSeqIndex; // initialize
        else{
          if (seqIndex.decompSeqIndex < srcSeqIndexMinMax[0])
            srcSeqIndexMinMax[0] = seqIndex.decompSeqIndex;
          if (seqIndex.decompSeqIndex > srcSeqIndexMinMax[1])
            srcSeqIndexMinMax[1] = seqIndex.decompSeqIndex;
        }
        // increment
        ++elementIndex;
        // multi-dim index increment
        ++ii[0];
        for (int j=0; j<srcRank-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = 0;  // reset
            ++ii[j+1];
          }
        }
      } // end while over all exclusive elements
    } // if there are elements in localArray associated with this local DE
  } // end for over local DEs
  delete [] ii;
  delete [] iiEnd;
  
  // communicate srcSeqIndexMinMax across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *srcSeqIndexMinMaxList = new int[2*petCount];
  vm->allgather(srcSeqIndexMinMax, srcSeqIndexMinMaxList, 2*sizeof(int));

#ifdef ASMMSTOREPRINT
  for (int i=0; i<srcLocalDeCount; i++)
    for (int j=0; j<srcLocalDeElementCount[i]; j++)
      printf("gjt: localPet %d, srcLinSeqList[%d][%d] = %d, %d\n", 
        localPet, i, j, srcLinSeqList[i][j].linIndex,
        srcLinSeqList[i][j].seqIndex);
#endif

  // determine linIndex <-> seqIndex association for all elements in exclusive 
  // region on all localDEs on dstArray
  int dstRank = dstArray->rank;
  ii = new int[dstRank];     // index tuple basis 0
  iiEnd = new int[dstRank];
  DD::AssociationElement **dstLinSeqList = 
    new DD::AssociationElement*[dstLocalDeCount];
  int dstSeqIndexMinMax[2]; // [0]=min, [1]=max
  for (int i=0; i<dstLocalDeCount; i++){
    // allocate memory in dstLinSeqList for this DE
    dstLinSeqList[i] = new DD::AssociationElement[dstLocalDeElementCount[i]];
    // only go into the multi-dim loop if there are elements for the local DE
    if (dstLocalDeElementCount[i]){
      // reset counters
      int joff = i*dstArray->distgrid->getDimCount();  // offset into bounds
      int packedIndex = 0;    // reset
      int tensorIndex = 0;    // reset
      for (int jj=0; jj<dstRank; jj++){
        ii[jj] = 0;  // reset
        if (dstArray->arrayToDistGridMap[jj]){
          // decomposed dimension 
          int j = packedIndex;
          iiEnd[jj] = dstArray->exclusiveUBound[joff+j]
            - dstArray->exclusiveLBound[joff+j] + 1;
          ++packedIndex;
        }else{
          // tensor dimension
          iiEnd[jj] = dstArray->undistUBound[tensorIndex]
            - dstArray->undistLBound[tensorIndex] + 1;
          ++tensorIndex;
        }
      }
      // loop over all elements in exclusive region for this DE
      int elementIndex = 0;  // reset
      while(ii[dstRank-1] < iiEnd[dstRank-1]){
        //printf("dst: DE = %d  - (", de);
        //int jjj;
        //for (jjj=0; jjj<dstRank-1; jjj++)
        //  printf("%d, ", ii[jjj]);
        //printf("%d)\n", ii[jjj]);
        // determine the lin. index for element ii[] in localArray for this DE
        int linIndex = dstArray->getLinearIndexExclusive(i, ii);
        // determine the sequentialized index for element ii[] in this DE
        // getSequenceIndexExclusive() expects basis 0 ii[] in excl. region
        SeqIndex seqIndex = dstArray->getSequenceIndexExclusive(i, ii);
        // store linIndex and seqIndex in dstLinSeqList for this DE
        dstLinSeqList[i][elementIndex].linIndex = linIndex;
        dstLinSeqList[i][elementIndex].seqIndex = seqIndex;
        // reset factorCount
        dstLinSeqList[i][elementIndex].factorCount = 0;
        // record seqIndex min and max
        if (i==0 && elementIndex==0)
          dstSeqIndexMinMax[0] = dstSeqIndexMinMax[1]
            = seqIndex.decompSeqIndex; // initialize
        else{
          if (seqIndex.decompSeqIndex < dstSeqIndexMinMax[0])
            dstSeqIndexMinMax[0] = seqIndex.decompSeqIndex;
          if (seqIndex.decompSeqIndex > dstSeqIndexMinMax[1])
            dstSeqIndexMinMax[1] = seqIndex.decompSeqIndex;
        }
        // increment
        ++elementIndex;
        // multi-dim index increment
        ++ii[0];
        for (int j=0; j<dstRank-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = 0;  // reset
            ++ii[j+1];
          }
        }
      } // end while over all exclusive elements
    } // if there are elements in localArray associated with this local DE
  } // end for over local DEs
  delete [] ii;
  delete [] iiEnd;

  // communicate dstSeqIndexMinMax across all Pets
  // todo: use nb-allgather and wait right before needed below
  int *dstSeqIndexMinMaxList = new int[2*petCount];
  vm->allgather(dstSeqIndexMinMax, dstSeqIndexMinMaxList, 2*sizeof(int));
  
#ifdef ASMMSTOREPRINT
  for (int i=0; i<dstLocalDeCount; i++)
    for (int j=0; j<dstLocalDeElementCount[i]; j++)
      printf("gjt: localPet %d, dstLinSeqList[%d][%d] = %d, %d\n", 
        localPet, i, j, dstLinSeqList[i][j].linIndex,
        dstLinSeqList[i][j].seqIndex);
#endif

#ifdef ASMMSTORETIMING
  VMK::wtime(&t3);   //gjt - profile
#endif
  
  // set up structure and intervals of src and dst distributed directories
  
  // determine the srcSeqIndexMinGlobal and MaxGlobal
  // todo: for nb-allgather(srcSeqIndexMinMaxList) here insert commwait()
  int srcSeqIndexMinGlobal, srcSeqIndexMaxGlobal;
  int pastInitFlag = 0; // reset
  for (int i=0; i<petCount; i++){
    if (srcElementCountList[i]){
      // this Pet does hold elements in srcArray
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
  VMK::wtime(&t4a1);   //gjt - profile
#endif

  // set up a distributed directory for srcArray seqIndex look-up
  int indicesPerPet = (srcSeqIndexMaxGlobal - srcSeqIndexMinGlobal + 1)
    / petCount;
  int extraIndices = (srcSeqIndexMaxGlobal - srcSeqIndexMinGlobal + 1)
    % petCount;
  DD::Interval *srcSeqIndexInterval = new DD::Interval[petCount];
  srcSeqIndexInterval[0].min = srcSeqIndexMinGlobal;  // start
  for (int i=0; i<petCount-1; i++){
    srcSeqIndexInterval[i].max = srcSeqIndexInterval[i].min + indicesPerPet - 1;
    if (i<extraIndices)
      ++srcSeqIndexInterval[i].max;   // distribute extra indices homogeneously
    srcSeqIndexInterval[i].count = 
      srcSeqIndexInterval[i].max - srcSeqIndexInterval[i].min + 1;
    srcSeqIndexInterval[i].countEff =
      srcSeqIndexInterval[i].count * srcTensorElementCountEff;
    srcSeqIndexInterval[i+1].min = srcSeqIndexInterval[i].max + 1;
  }
  srcSeqIndexInterval[petCount-1].max = srcSeqIndexMaxGlobal;  // finish
  srcSeqIndexInterval[petCount-1].count = 
    srcSeqIndexInterval[petCount-1].max - srcSeqIndexInterval[petCount-1].min
    + 1;
  srcSeqIndexInterval[petCount-1].countEff =
    srcSeqIndexInterval[petCount-1].count * srcTensorElementCountEff;
  
#ifdef ASMMSTOREPRINT
  printf("gjt: localPet %d, srcElementCountList[localPet] = %d, "
    "srcSeqIndexMinMax = %d / %d, srcSeqIndexMinGlobal/MaxGlobal = %d, %d, "
    "srcSeqIndexInterval[localPet].min/.max = %d, %d\n",
    localPet, srcElementCountList[localPet], srcSeqIndexMinMax[0],
    srcSeqIndexMinMax[1], srcSeqIndexMinGlobal, srcSeqIndexMaxGlobal,
    srcSeqIndexInterval[localPet].min, srcSeqIndexInterval[localPet].max);
#endif

#ifdef ASMMSTORETIMING
  VMK::wtime(&t4a2);   //gjt - profile
#endif
  
  int *srcLocalElementsPerIntervalCount = new int[petCount];
  {
    // prepare temporary seqIndexList for sorting
    vector<int> seqIndexList(srcElementCount);
    int jj=0;
    for (int j=0; j<srcLocalDeCount; j++){
      for (int k=0; k<srcLocalDeElementCount[j]; k++){
        seqIndexList[jj] = srcLinSeqList[j][k].seqIndex.decompSeqIndex;
        ++jj;
      }
    }
    sort(seqIndexList.begin(), seqIndexList.end());
    jj=0;
    for (int i=0; i<petCount; i++){
      int seqIndexMax = srcSeqIndexInterval[i].max;
      int count = 0; // reset
      while (jj<srcElementCount && seqIndexList[jj]<=seqIndexMax){
        ++count;  // increment counter
        ++jj;
      }
      srcLocalElementsPerIntervalCount[i] = count;
    }
  }
  
  int *srcLocalIntervalPerPetCount = new int[petCount];
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4a3);   //gjt - profile
#endif
  
  vm->alltoall(srcLocalElementsPerIntervalCount, sizeof(int),
    srcLocalIntervalPerPetCount, sizeof(int), vmBYTE);
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4a);   //gjt - profile
#endif
  
  // determine the dstSeqIndexMinGlobal and MaxGlobal
  // todo: for nb-allgather(dstSeqIndexMinMaxList) here insert commwait()
  int dstSeqIndexMinGlobal, dstSeqIndexMaxGlobal;
  pastInitFlag = 0; // reset
  for (int i=0; i<petCount; i++){
    if (dstElementCountList[i]){
      // this Pet does hold elements in dstArray
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
  VMK::wtime(&t4b1);   //gjt - profile
#endif

  // set up a distributed directory for dstArray seqIndex look-up
  indicesPerPet = (dstSeqIndexMaxGlobal - dstSeqIndexMinGlobal + 1) / petCount;
  extraIndices = (dstSeqIndexMaxGlobal - dstSeqIndexMinGlobal + 1) % petCount;
  DD::Interval *dstSeqIndexInterval = new DD::Interval[petCount];
  dstSeqIndexInterval[0].min = dstSeqIndexMinGlobal;  // start
  for (int i=0; i<petCount-1; i++){
    dstSeqIndexInterval[i].max = dstSeqIndexInterval[i].min + indicesPerPet - 1;
    if (i<extraIndices)
      ++dstSeqIndexInterval[i].max;   // distribute extra indices homogeneously
    dstSeqIndexInterval[i].count = 
      dstSeqIndexInterval[i].max - dstSeqIndexInterval[i].min + 1;
    dstSeqIndexInterval[i].countEff =
      dstSeqIndexInterval[i].count * dstTensorElementCountEff;
    dstSeqIndexInterval[i+1].min = dstSeqIndexInterval[i].max + 1;
  }
  dstSeqIndexInterval[petCount-1].max = dstSeqIndexMaxGlobal;  // finish
  dstSeqIndexInterval[petCount-1].count = 
    dstSeqIndexInterval[petCount-1].max - dstSeqIndexInterval[petCount-1].min
    + 1;
  dstSeqIndexInterval[petCount-1].countEff =
    dstSeqIndexInterval[petCount-1].count * dstTensorElementCountEff;
  
#ifdef ASMMSTOREPRINT
    printf("gjt: localPet %d, dstElementCountList[localPet] = %d, "
    "dstSeqIndexMinMax = %d / %d, dstSeqIndexMinGlobal/MaxGlobal = %d, %d, "
    "dstSeqIndexInterval[localPet].min/.max = %d, %d\n",
    localPet, dstElementCountList[localPet], dstSeqIndexMinMax[0],
    dstSeqIndexMinMax[1], dstSeqIndexMinGlobal, dstSeqIndexMaxGlobal,
    dstSeqIndexInterval[localPet].min, dstSeqIndexInterval[localPet].max);
#endif

#ifdef ASMMSTORETIMING
  VMK::wtime(&t4b2);   //gjt - profile
#endif

  int *dstLocalElementsPerIntervalCount = new int[petCount];
  {
    // prepare temporary seqIndexList for sorting
    vector<int> seqIndexList(dstElementCount);
    int jj=0;
    for (int j=0; j<dstLocalDeCount; j++){
      for (int k=0; k<dstLocalDeElementCount[j]; k++){
        seqIndexList[jj] = dstLinSeqList[j][k].seqIndex.decompSeqIndex;
        ++jj;
      }
    }
    sort(seqIndexList.begin(), seqIndexList.end());
    jj=0;
    for (int i=0; i<petCount; i++){
      int seqIndexMax = dstSeqIndexInterval[i].max;
      int count = 0; // reset
      while (jj<dstElementCount && seqIndexList[jj]<=seqIndexMax){
        ++count;  // increment counter
        ++jj;
      }
      dstLocalElementsPerIntervalCount[i] = count;
    }
  }
  
  int *dstLocalIntervalPerPetCount = new int[petCount];
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4b3);   //gjt - profile
#endif

  vm->alltoall(dstLocalElementsPerIntervalCount, sizeof(int),
    dstLocalIntervalPerPetCount, sizeof(int), vmBYTE);
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t4b);   //gjt - profile
#endif

#ifdef ASMMSTORETIMING
  VMK::wtime(&t4);   //gjt - profile
#endif
  
  // set up srcSeqIntervFactorListCount and srcSeqIntervFactorListIndex
  int *srcSeqIntervFactorListCount = new int[petCount];
  int **srcSeqIntervFactorListIndex = new int*[petCount];
  for (int i=0; i<petCount; i++)
    srcSeqIntervFactorListCount[i] = 0; // reset

#ifdef ASMMSTORETIMING
  VMK::wtime(&t5a1);   //gjt - profile
#endif

  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and count factor towards that PETs factor list count.
    int srcSeqIndex;
    int srcTensorSeqIndex;
    if (tensorMixFlag){
      srcSeqIndex = factorIndexList->array[j*4];
      srcTensorSeqIndex = factorIndexList->array[j*4+1];
    }else{
      srcSeqIndex = factorIndexList->array[j*2];
      srcTensorSeqIndex = 1;  // dummy
    }
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
      // found interval -> check if srcTensorSeqIndex is within bounds
      if (srcTensorSeqIndex < 1 || srcTensorSeqIndex >
        srcTensorElementCountEff){
        // srcTensorSeqIndex outside srcArray bounds
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
          "- factorIndexList contains srcTensorSeqIndex outside srcArray"
          " bounds", &rc);
        return rc;
      }
      // count this factor for this Pet
      ++srcSeqIntervFactorListCount[i];
      foundFlag = 1;  // set
      break;
    }while (iMin != iMax);
    if (!foundFlag){
      // srcSeqIndex lies outside srcArray bounds
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- factorIndexList contains srcSeqIndex outside srcArray bounds", &rc);
      return rc;
    }
  }

#ifdef ASMMSTORETIMING
  VMK::wtime(&t5a2);   //gjt - profile
#endif
  
  int *srcSeqIntervFactorCounter = new int[petCount];
  for (int i=0; i<petCount; i++){
    srcSeqIntervFactorListIndex[i] = new int[srcSeqIntervFactorListCount[i]];
    srcSeqIntervFactorCounter[i] = 0;  // reset
  }

#ifdef ASMMSTORETIMING
  VMK::wtime(&t5a3);   //gjt - profile
#endif
  
  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and factor list index
    int srcSeqIndex;
    if (tensorMixFlag)
      srcSeqIndex = factorIndexList->array[j*4];
    else
      srcSeqIndex = factorIndexList->array[j*2];
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
  VMK::wtime(&t5a);   //gjt - profile
#endif
  
  // allocate local look-up table indexed by srcSeqIndex
  DD::SeqIndexFactorLookup *srcSeqIndexFactorLookup = 
    new DD::SeqIndexFactorLookup[srcSeqIndexInterval[localPet].countEff];
  for (int i=0; i<srcSeqIndexInterval[localPet].countEff; i++){
    srcSeqIndexFactorLookup[i].de = 0; // use during initialization as counter
    srcSeqIndexFactorLookup[i].factorCount = 0; // reset
  }

  // all Pets construct their local srcSeqIndexFactorLookup[]
  ArrayHelper::MemHelper *memHelperFactorLookup = new ArrayHelper::MemHelper();
  int srcSeqIndexFactorCount = 0; // reset
  for (int factorPetIndex=0; factorPetIndex<factorPetCount; factorPetIndex++){
    // each Pet in factorPetList gets to be rootPet once and provide its factors
    int rootPet = factorPetList[factorPetIndex];
    if (localPet == rootPet){
      // rootPet
      for (int i=0; i<petCount; i++){
        int *thisPetFactorCountList =
          new int[srcSeqIndexInterval[i].countEff+1];
        // the extra integer value is used to store thisPetTotalFactorCount
        // to optimize communications
        for (int j=0; j<srcSeqIndexInterval[i].countEff+1; j++)
          thisPetFactorCountList[j] = 0; // reset
        if (i == rootPet){
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b1);   //gjt - profile
#endif
    
          // rootPet -> rootPet "communication"
          for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's srcSeqIndex interv
            int j = srcSeqIntervFactorListIndex[i][jj];
            int srcSeqIndex;
            int k;
            if (tensorMixFlag){
              srcSeqIndex = factorIndexList->array[j*4];
              k = srcSeqIndex - srcSeqIndexInterval[i].min;
              k += (factorIndexList->array[j*4+1]-1)
                * srcSeqIndexInterval[i].count;
            }else{
              srcSeqIndex = factorIndexList->array[j*2];
              k = srcSeqIndex - srcSeqIndexInterval[i].min;
            }
            ++thisPetFactorCountList[k];  // count this factor
          }
          // prepare srcSeqIndexFactorLookup[]
          for (int j=0; j<srcSeqIndexInterval[i].countEff; j++){
            if (int factorCount = thisPetFactorCountList[j]){
              srcSeqIndexFactorCount += factorCount;  // add to the total count
              int prevFactorCount = srcSeqIndexFactorLookup[j].factorCount;
              // allocate new factorList
              DD::FactorElement *factorList =
                (DD::FactorElement*)memHelperFactorLookup->
                malloc(sizeof(DD::FactorElement)*(prevFactorCount+factorCount));
              if (prevFactorCount){
                // copy previous factorList elements into new factorList
                memcpy(factorList, srcSeqIndexFactorLookup[j].factorList,
                  prevFactorCount * sizeof(DD::FactorElement));
              }
              // place new factorList into look-up table and set new count
              srcSeqIndexFactorLookup[j].factorList = factorList;
              srcSeqIndexFactorLookup[j].factorCount += factorCount;
            }
          }
          // fill srcSeqIndexFactorLookup[]
          if (typekindFactors == ESMC_TYPEKIND_R4){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4+2]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+3]; // dstTensorSeqIndex
              }else{
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2+1]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_R4 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_R8){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4+2]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+3]; // dstTensorSeqIndex
              }else{
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2+1]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_R8 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("srcArray: %d, %d, rootPet-rootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, srcSeqIndex, factorIndexList->array[j*2+1], ((ESMC_R8 *)factorList)[j]);   
#endif              
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I4){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4+2]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+3]; // dstTensorSeqIndex
              }else{
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2+1]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_I4 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I8){
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              int kk = srcSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4+2]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+3]; // dstTensorSeqIndex
              }else{
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2+1]; // dstSeqIndex
                srcSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_I8 *)srcSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I8 *)factorList)[j];
            }
          } // if - typekindFactors
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b2);   //gjt - profile
#endif
            
        }else{
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b3);   //gjt - profile
  VMK::wtime(&t5b3a);   //gjt - profile
#endif
          // rootPet -> not rootPet communication
          int totalCountIndex = srcSeqIndexInterval[i].countEff; // last element
          for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's srcSeqIndex interv
            int j = srcSeqIntervFactorListIndex[i][jj];
            int srcSeqIndex;
            int k;
            if (tensorMixFlag){
              srcSeqIndex = factorIndexList->array[j*4];
              k = srcSeqIndex - srcSeqIndexInterval[i].min;
              k += (factorIndexList->array[j*4+1]-1)
                * srcSeqIndexInterval[i].count;
            }else{
              srcSeqIndex = factorIndexList->array[j*2];
              k = srcSeqIndex - srcSeqIndexInterval[i].min;
            }
            // count this factor
            ++thisPetFactorCountList[k];
            ++thisPetFactorCountList[totalCountIndex];
          }
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b3b);   //gjt - profile
#endif
    
          // send info to Pet "i"
          vm->send(thisPetFactorCountList, 
            (srcSeqIndexInterval[i].countEff + 1) * sizeof(int), i);
            
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b3c);   //gjt - profile
#endif
  
          // prepare to send remaining information to Pet "i" in one long stream
          int thisPetTotalFactorCount = thisPetFactorCountList[totalCountIndex];
          int byteCount = thisPetTotalFactorCount
            * (4*sizeof(int) + dataSizeFactors);
          char *stream = new char[byteCount];
          int *intStream;
          if (typekindFactors == ESMC_TYPEKIND_R4){
            ESMC_R4 *factorStream = (ESMC_R4 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4+2]; // dstSeqIndex
                *intStream++ = factorIndexList->array[j*4+3]; // dstTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
                *intStream++ = -1; // dummy dstTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_R4 *)intStream;
              *factorStream++ = ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_R8){
            ESMC_R8 *factorStream = (ESMC_R8 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4+2]; // dstSeqIndex
                *intStream++ = factorIndexList->array[j*4+3]; // dstTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
                *intStream++ = -1; // dummy dstTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_R8 *)intStream;
              *factorStream++ = ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("srcArray: %d, %d, rootPet-NOTrootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, srcSeqIndex, factorIndexList->array[j*2+1], ((ESMC_R8 *)factorList)[j]);   
#endif              
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I4){
            ESMC_I4 *factorStream = (ESMC_I4 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4+2]; // dstSeqIndex
                *intStream++ = factorIndexList->array[j*4+3]; // dstTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
                *intStream++ = -1; // dummy dstTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_I4 *)intStream;
              *factorStream++ = ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I8){
            ESMC_I8 *factorStream = (ESMC_I8 *)stream;
            for (int jj=0; jj<srcSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's srcSeqIndex interv
              int j = srcSeqIntervFactorListIndex[i][jj];
              int srcSeqIndex;
              int k;
              if (tensorMixFlag){
                srcSeqIndex = factorIndexList->array[j*4];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+1]-1)
                  * srcSeqIndexInterval[i].count;
              }else{
                srcSeqIndex = factorIndexList->array[j*2];
                k = srcSeqIndex - srcSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4+2]; // dstSeqIndex
                *intStream++ = factorIndexList->array[j*4+3]; // dstTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2+1]; // dstSeqIndex
                *intStream++ = -1; // dummy dstTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_I8 *)intStream;
               *factorStream++ = ((ESMC_I8 *)factorList)[j];
            }
          }
          // ready to send information to Pet "i" in one long stream
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b3d);   //gjt - profile
  VMK::wtime(&t5b4);   //gjt - profile
#endif
  
          vm->send(stream, byteCount, i);
          
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5b5);   //gjt - profile
#endif
  
#ifdef ASMMSTORETIMING
  dt5b34 += t5b4 - t5b3;
  dt5b45 += t5b5 - t5b4;
  dt5b3ab += t5b3b - t5b3a;
  dt5b3bc += t5b3c - t5b3b;
  dt5b3cd += t5b3d - t5b3c;
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
        new int[srcSeqIndexInterval[localPet].countEff+1];
      // the extra integer value is used to store thisPetTotalFactorCount
      // to optimize communications
      vm->recv(localPetFactorCountList, 
        (srcSeqIndexInterval[localPet].countEff + 1) * sizeof(int), rootPet);
      int localPetTotalFactorCount =
        localPetFactorCountList[srcSeqIndexInterval[localPet].countEff];
      // process localPetFactorCountList and set srcSeqIndexFactorLookup[]
      for (int j=0; j<srcSeqIndexInterval[localPet].countEff; j++){
        if (int factorCount = localPetFactorCountList[j]){
          srcSeqIndexFactorCount += factorCount;  // add to the total count
          int prevFactorCount = srcSeqIndexFactorLookup[j].factorCount;
          // allocate new factorList
          DD::FactorElement *factorList =
            (DD::FactorElement*)memHelperFactorLookup->
            malloc(sizeof(DD::FactorElement)*(prevFactorCount+factorCount));
          if (prevFactorCount){
            // copy previous factorList elements into new factorList
            memcpy(factorList, srcSeqIndexFactorLookup[j].factorList,
              prevFactorCount * sizeof(DD::FactorElement));
          }
          // place new factorList into look-up table and set new count
          srcSeqIndexFactorLookup[j].factorList = factorList;
          srcSeqIndexFactorLookup[j].factorCount += factorCount;
        }
      }
      delete [] localPetFactorCountList;
      // receive remaining information from rootPet in one long stream
      int byteCount = localPetTotalFactorCount
        * (4*sizeof(int) + dataSizeFactors);
      char *stream = new char[byteCount];
      vm->recv(stream, byteCount, rootPet);
      // process stream and set srcSeqIndexFactorLookup[] content
      int *intStream;
      if (typekindFactors == ESMC_TYPEKIND_R4){
        ESMC_R4 *factorStream = (ESMC_R4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // dstSeqIndex
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // dstTensorSeqIndex
          intStream++;  // skip padding
          factorStream = (ESMC_R4 *)intStream;
          *((ESMC_R4 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindFactors == ESMC_TYPEKIND_R8){
        ESMC_R8 *factorStream = (ESMC_R8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // dstSeqIndex
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // dstTensorSeqIndex
          intStream++;  // skip padding
          factorStream = (ESMC_R8 *)intStream;
          *((ESMC_R8 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindFactors == ESMC_TYPEKIND_I4){
        ESMC_I4 *factorStream = (ESMC_I4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // dstSeqIndex
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // dstTensorSeqIndex
          intStream++;  // skip padding
          factorStream = (ESMC_I4 *)intStream;
          *((ESMC_I4 *)srcSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindFactors == ESMC_TYPEKIND_I8){
        ESMC_I8 *factorStream = (ESMC_I8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=srcSeqIndexFactorLookup[j].de++;// counter during initialization
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // dstSeqIndex
          srcSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // dstTensorSeqIndex
          intStream++;  // skip padding
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
  VMK::wtime(&t5b);   //gjt - profile
#endif

  // communicate between Pets to set up "de" member in srcSeqIndexFactorLookup[]
  {
    DD::FillSelfDeInfo *fillSelfDeInfo = new DD::FillSelfDeInfo;
    fillSelfDeInfo->linSeqList = srcLinSeqList;
    fillSelfDeInfo->localPet = localPet;
    fillSelfDeInfo->localDeCount = srcLocalDeCount;
    fillSelfDeInfo->localDeElementCount = srcLocalDeElementCount;
    fillSelfDeInfo->localDeList = srcLocalDeList;
    fillSelfDeInfo->seqIndexInterval = srcSeqIndexInterval;
    fillSelfDeInfo->seqIndexFactorLookup = srcSeqIndexFactorLookup;
    fillSelfDeInfo->tensorMixFlag = tensorMixFlag;
      
    DD::updateLookup(vm, petCount, localPet, srcLocalIntervalPerPetCount,
      srcLocalElementsPerIntervalCount, fillSelfDeInfo);
    delete fillSelfDeInfo;
  }
  
//vm->barrier();  /// only for profiling tests
      
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5c);   //gjt - profile
#endif

  // set up dstSeqIntervFactorListCount and dstSeqIntervFactorListIndex
  int *dstSeqIntervFactorListCount = new int[petCount];
  int **dstSeqIntervFactorListIndex = new int*[petCount];
  for (int i=0; i<petCount; i++)
    dstSeqIntervFactorListCount[i] = 0; // reset
  for (int j=0; j<factorListCount; j++){
    // loop over all factorList entries, find matching interval via bisection
    // and count factor towards that PETs factor list count.
    int dstSeqIndex;
    int dstTensorSeqIndex;
    if (tensorMixFlag){
      dstSeqIndex = factorIndexList->array[j*4+2];
      dstTensorSeqIndex = factorIndexList->array[j*4+3];
    }else{
      dstSeqIndex = factorIndexList->array[j*2+1];
      dstTensorSeqIndex = 1;  // dummy
    }
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
      // found interval -> check if dstTensorSeqIndex is within bounds
      if (dstTensorSeqIndex < 1 || dstTensorSeqIndex >
        dstTensorElementCountEff){
        // dstTensorSeqIndex outside dstArray bounds
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
          "- factorIndexList contains dstTensorSeqIndex outside dstArray"
          " bounds", &rc);
        return rc;
      }
      // count this factor for this Pet
      ++dstSeqIntervFactorListCount[i];
      foundFlag = 1;  // set
      break;
    }while (iMin != iMax);
    if (!foundFlag){
      // dstSeqIndex lies outside dstArray bounds
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- factorIndexList contains dstSeqIndex outside dstArray bounds", &rc);
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
    int dstSeqIndex;
    if (tensorMixFlag)
      dstSeqIndex = factorIndexList->array[j*4+2];
    else
      dstSeqIndex = factorIndexList->array[j*2+1];
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
  VMK::wtime(&t5d);   //gjt - profile
#endif
  
  // allocate local look-up table indexed by dstSeqIndex
  DD::SeqIndexFactorLookup *dstSeqIndexFactorLookup = 
    new DD::SeqIndexFactorLookup[dstSeqIndexInterval[localPet].countEff];
  for (int i=0; i<dstSeqIndexInterval[localPet].countEff; i++){
    dstSeqIndexFactorLookup[i].de = 0; // use during initialization as counter
    dstSeqIndexFactorLookup[i].factorCount = 0; // reset
  }

  // all Pets construct their local dstSeqIndexFactorLookup[]
  int dstSeqIndexFactorCount = 0; // reset
  for (int factorPetIndex=0; factorPetIndex<factorPetCount; factorPetIndex++){
    // each Pet in factorPetList gets to be rootPet once and provide its factors
    int rootPet = factorPetList[factorPetIndex];
    if (localPet == rootPet){
      // rootPet
      for (int i=0; i<petCount; i++){
        int *thisPetFactorCountList =
          new int[dstSeqIndexInterval[i].countEff+1];
        // the extra integer value is used to store thisPetTotalFactorCount
        // to optimize communications
        for (int j=0; j<dstSeqIndexInterval[i].countEff+1; j++)
          thisPetFactorCountList[j] = 0; // reset
        if (i == rootPet){
          // rootPet -> rootPet "communication"
          for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's dstSeqIndex interv
            int j = dstSeqIntervFactorListIndex[i][jj];
            int dstSeqIndex;
            int k;
            if (tensorMixFlag){
              dstSeqIndex = factorIndexList->array[j*4+2];
              k = dstSeqIndex - dstSeqIndexInterval[i].min;
              k += (factorIndexList->array[j*4+3]-1)
                * dstSeqIndexInterval[i].count;
            }else{
              dstSeqIndex = factorIndexList->array[j*2+1];
              k = dstSeqIndex - dstSeqIndexInterval[i].min;
            }
            ++thisPetFactorCountList[k];  // count this factor
          }
          // prepare dstSeqIndexFactorLookup[]
          for (int j=0; j<dstSeqIndexInterval[i].countEff; j++){
            if (int factorCount = thisPetFactorCountList[j]){
              dstSeqIndexFactorCount += factorCount;  // add to the total count
              int prevFactorCount = dstSeqIndexFactorLookup[j].factorCount;
              // allocate new factorList
              DD::FactorElement *factorList =
                (DD::FactorElement*)memHelperFactorLookup->
                malloc(sizeof(DD::FactorElement)*(prevFactorCount+factorCount));
              if (prevFactorCount){
                // copy previous factorList elements into new factorList
                memcpy(factorList, dstSeqIndexFactorLookup[j].factorList,
                  prevFactorCount * sizeof(DD::FactorElement));
              }
              // place new factorList into look-up table and set new count
              dstSeqIndexFactorLookup[j].factorList = factorList;
              dstSeqIndexFactorLookup[j].factorCount += factorCount;
            }
          }
          // fill dstSeqIndexFactorLookup[]
          if (typekindFactors == ESMC_TYPEKIND_R4){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+1];  // srcTensorSeqIndex
              }else{
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_R4 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_R8){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+1];  // srcTensorSeqIndex
              }else{
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_R8 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("dstArray: %d, %d, rootPet-rootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, dstSeqIndex, factorIndexList->array[j*2], ((ESMC_R8 *)factorList)[j]);
#endif        
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I4){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+1];  // srcTensorSeqIndex
              }else{
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_I4 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I8){
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              int kk = dstSeqIndexFactorLookup[k].de++;// counter during init
              if (tensorMixFlag){
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*4];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex =
                  factorIndexList->array[j*4+1];  // srcTensorSeqIndex
              }else{
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.decompSeqIndex =
                  factorIndexList->array[j*2];    // srcSeqIndex
                dstSeqIndexFactorLookup[k].factorList[kk]
                  .partnerSeqIndex.tensorSeqIndex = -1; // dummy
              }
              *((ESMC_I8 *)dstSeqIndexFactorLookup[k].factorList[kk].factor) =
                ((ESMC_I8 *)factorList)[j];
            }
          } // if - typekindFactors
        }else{
          // rootPet -> not rootPet communication
          int totalCountIndex = dstSeqIndexInterval[i].countEff; // last element
          for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
            // loop over factorList entries in this Pet's dstSeqIndex interv
            int j = dstSeqIntervFactorListIndex[i][jj];
            int dstSeqIndex;
            int k;
            if (tensorMixFlag){
              dstSeqIndex = factorIndexList->array[j*4+2];
              k = dstSeqIndex - dstSeqIndexInterval[i].min;
              k += (factorIndexList->array[j*4+3]-1)
                * dstSeqIndexInterval[i].count;
            }else{
              dstSeqIndex = factorIndexList->array[j*2+1];
              k = dstSeqIndex - dstSeqIndexInterval[i].min;
            }
            // count this factor
            ++thisPetFactorCountList[k];
            ++thisPetFactorCountList[totalCountIndex];
          }
          // send info to Pet "i"
          vm->send(thisPetFactorCountList, 
            (dstSeqIndexInterval[i].countEff + 1) * sizeof(int), i);
          // prepare to send remaining information to Pet "i" in one long stream
          int thisPetTotalFactorCount = thisPetFactorCountList[totalCountIndex];
          int byteCount = thisPetTotalFactorCount
            * (4*sizeof(int) + dataSizeFactors);
          char *stream = new char[byteCount];
          int *intStream;
          if (typekindFactors == ESMC_TYPEKIND_R4){
            ESMC_R4 *factorStream = (ESMC_R4 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4];   // srcSeqIndex
                *intStream++ = factorIndexList->array[j*4+1]; // srcTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2];   // srcSeqIndex
                *intStream++ = -1; // dummy srcTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_R4 *)intStream;
              *factorStream++ = ((ESMC_R4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_R8){
            ESMC_R8 *factorStream = (ESMC_R8 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4];   // srcSeqIndex
                *intStream++ = factorIndexList->array[j*4+1]; // srcTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2];   // srcSeqIndex
                *intStream++ = -1; // dummy srcTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_R8 *)intStream;
              *factorStream++ = ((ESMC_R8 *)factorList)[j];
#ifdef ASMMSTOREPRINT
printf("dstArray: %d, %d, rootPet-NOTrootPet R8: partnerSeqIndex %d, factor: %g\n", factorListCount, dstSeqIndex, factorIndexList->array[j*2], ((ESMC_R8 *)factorList)[j]);
#endif
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I4){
            ESMC_I4 *factorStream = (ESMC_I4 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4];   // srcSeqIndex
                *intStream++ = factorIndexList->array[j*4+1]; // srcTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2];   // srcSeqIndex
                *intStream++ = -1; // dummy srcTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_I4 *)intStream;
              *factorStream++ = ((ESMC_I4 *)factorList)[j];
            }
          }else if (typekindFactors == ESMC_TYPEKIND_I8){
            ESMC_I8 *factorStream = (ESMC_I8 *)stream;
            for (int jj=0; jj<dstSeqIntervFactorListCount[i]; jj++){
              // loop over factorList entries in this Pet's dstSeqIndex interv
              int j = dstSeqIntervFactorListIndex[i][jj];
              int dstSeqIndex;
              int k;
              if (tensorMixFlag){
                dstSeqIndex = factorIndexList->array[j*4+2];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
                k += (factorIndexList->array[j*4+3]-1)
                  * dstSeqIndexInterval[i].count;
              }else{
                dstSeqIndex = factorIndexList->array[j*2+1];
                k = dstSeqIndex - dstSeqIndexInterval[i].min;
              }
              intStream = (int *)factorStream;
              *intStream++ = k; // index into distr. dir lookup table
              if (tensorMixFlag){
                *intStream++ = factorIndexList->array[j*4];   // srcSeqIndex
                *intStream++ = factorIndexList->array[j*4+1]; // srcTen.SeqIndex
              }else{
                *intStream++ = factorIndexList->array[j*2];   // srcSeqIndex
                *intStream++ = -1; // dummy srcTensorSeqIndex
              }
              *intStream++ = 0; // padding for 8-byte alignment
              factorStream = (ESMC_I8 *)intStream;
              *factorStream++ = ((ESMC_I8 *)factorList)[j];
            }
          }
          // ready to send information to Pet "i" in one long stream
          vm->send(stream, byteCount, i);
          // garbage collection
          delete [] stream;
        }
        delete [] thisPetFactorCountList;
      } // i - petCount
    }else{
      // localPet not rootPet
      // receive info from rootPet
      int *localPetFactorCountList =
        new int[dstSeqIndexInterval[localPet].countEff+1];
      // the extra integer value is used to store thisPetTotalFactorCount
      // to optimize communications
      vm->recv(localPetFactorCountList, 
        (dstSeqIndexInterval[localPet].countEff + 1) * sizeof(int), rootPet);
      int localPetTotalFactorCount =
        localPetFactorCountList[dstSeqIndexInterval[localPet].countEff];
      // process localPetFactorCountList and set dstSeqIndexFactorLookup[]
      for (int j=0; j<dstSeqIndexInterval[localPet].countEff; j++){
        if (int factorCount = localPetFactorCountList[j]){
          dstSeqIndexFactorCount += factorCount;  // add to the total count
          int prevFactorCount = dstSeqIndexFactorLookup[j].factorCount;
          // allocate new factorList
          DD::FactorElement *factorList =
            (DD::FactorElement*)memHelperFactorLookup->
            malloc(sizeof(DD::FactorElement)*(prevFactorCount+factorCount));
          if (prevFactorCount){
            // copy previous factorList elements into new factorList
            memcpy(factorList, dstSeqIndexFactorLookup[j].factorList,
              prevFactorCount * sizeof(DD::FactorElement));
          }
          // place new factorList into look-up table and set new count
          dstSeqIndexFactorLookup[j].factorList = factorList;
          dstSeqIndexFactorLookup[j].factorCount += factorCount;
        }
      }
      delete [] localPetFactorCountList;
      // receive remaining information from rootPet in one long stream
      int byteCount = localPetTotalFactorCount
        * (4*sizeof(int) + dataSizeFactors);
      char *stream = new char[byteCount];
      vm->recv(stream, byteCount, rootPet);
      // process stream and set dstSeqIndexFactorLookup[] content
      int *intStream;
      if (typekindFactors == ESMC_TYPEKIND_R4){
        ESMC_R4 *factorStream = (ESMC_R4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // srcSeqIndex
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // srcTensorSeqIndex
          intStream++;  // skip padding
          factorStream = (ESMC_R4 *)intStream;
          *((ESMC_R4 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindFactors == ESMC_TYPEKIND_R8){
        ESMC_R8 *factorStream = (ESMC_R8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // srcSeqIndex
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // srcTensorSeqIndex
          intStream++;  // skip padding
          factorStream = (ESMC_R8 *)intStream;
          *((ESMC_R8 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindFactors == ESMC_TYPEKIND_I4){
        ESMC_I4 *factorStream = (ESMC_I4 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // srcSeqIndex
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // srcTensorSeqIndex
          intStream++;  // skip padding
          factorStream = (ESMC_I4 *)intStream;
          *((ESMC_I4 *)dstSeqIndexFactorLookup[j].factorList[k].factor) =
            *factorStream++;
        }
      }else if (typekindFactors == ESMC_TYPEKIND_I8){
        ESMC_I8 *factorStream = (ESMC_I8 *)stream;
        for (int i=0; i<localPetTotalFactorCount; i++){
          intStream = (int *)factorStream;
          int j=*intStream++;                   // index into lookup table
          int k=dstSeqIndexFactorLookup[j].de++;// counter during initialization
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.decompSeqIndex = *intStream++; // srcSeqIndex
          dstSeqIndexFactorLookup[j].factorList[k]
            .partnerSeqIndex.tensorSeqIndex = *intStream++; // srcTensorSeqIndex
          intStream++;  // skip padding
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
  VMK::wtime(&t5e);   //gjt - profile
#endif
  
  // communicate between Pets to set up "de" member in dstSeqIndexFactorLookup[]
  {
    DD::FillSelfDeInfo *fillSelfDeInfo = new DD::FillSelfDeInfo;
    fillSelfDeInfo->linSeqList = dstLinSeqList;
    fillSelfDeInfo->localPet = localPet;
    fillSelfDeInfo->localDeCount = dstLocalDeCount;
    fillSelfDeInfo->localDeElementCount = dstLocalDeElementCount;
    fillSelfDeInfo->localDeList = dstLocalDeList;
    fillSelfDeInfo->seqIndexInterval = dstSeqIndexInterval;
    fillSelfDeInfo->seqIndexFactorLookup = dstSeqIndexFactorLookup;
    fillSelfDeInfo->tensorMixFlag = tensorMixFlag;
      
    DD::updateLookup(vm, petCount, localPet, dstLocalIntervalPerPetCount,
      dstLocalElementsPerIntervalCount, fillSelfDeInfo);
    delete fillSelfDeInfo;
  }

//vm->barrier();  /// only for profiling tests
      
#ifdef ASMMSTORETIMING
  VMK::wtime(&t5f);   //gjt - profile
#endif
  
  // garbage collection  
  delete [] srcElementCountList;
  delete [] dstElementCountList;
  delete [] srcSeqIndexMinMaxList;
  delete [] dstSeqIndexMinMaxList;
  delete [] factorPetList;
  delete [] srcSeqIntervFactorListCount;
  for (int i=0; i<petCount; i++)
    delete [] srcSeqIntervFactorListIndex[i];
  delete [] srcSeqIntervFactorListIndex;
  delete [] dstSeqIntervFactorListCount;
  for (int i=0; i<petCount; i++)
    delete [] dstSeqIntervFactorListIndex[i];
  delete [] dstSeqIntervFactorListIndex;

#ifdef ASMMSTORETIMING
  VMK::wtime(&t5);   //gjt - profile
#endif
  
  // prepare count arrays for src partner look-up in dstSeqIndexFactorLookup
  int *srcLocalPartnerElementsPerIntervalCount = new int[petCount];
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active srcSeqIndex interval
    int seqIndexMin = dstSeqIndexInterval[i].min;
    int seqIndexMax = dstSeqIndexInterval[i].max;
    int count = 0; // reset
    for (int j=0; j<srcSeqIndexInterval[localPet].countEff; j++){
      for (int k=0; k<srcSeqIndexFactorLookup[j].factorCount; k++){
        int partnerSeqIndex = srcSeqIndexFactorLookup[j].factorList[k]
          .partnerSeqIndex.decompSeqIndex;
        if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax)
          ++count; // increment counter
      }
    }
    srcLocalPartnerElementsPerIntervalCount[i] = count;
  }
  int *dstLocalPartnerIntervalPerPetCount = new int[petCount];
  vm->alltoall(srcLocalPartnerElementsPerIntervalCount, sizeof(int),
    dstLocalPartnerIntervalPerPetCount, sizeof(int), vmBYTE);
  
  // fill partnerDe in srcSeqIndexFactorLookup using dstSeqIndexFactorLookup
  {
    DD::FillPartnerDeInfo *fillPartnerDeInfo = new DD::FillPartnerDeInfo;
    fillPartnerDeInfo->localPet = localPet;
    fillPartnerDeInfo->seqIndexIntervalIn = dstSeqIndexInterval;
    fillPartnerDeInfo->seqIndexIntervalOut = srcSeqIndexInterval;
    fillPartnerDeInfo->seqIndexFactorLookupIn = dstSeqIndexFactorLookup;
    fillPartnerDeInfo->seqIndexFactorLookupOut = srcSeqIndexFactorLookup;
    fillPartnerDeInfo->tensorMixFlag = tensorMixFlag;
      
    DD::accessLookup(vm, petCount, localPet, dstLocalPartnerIntervalPerPetCount,
      srcLocalPartnerElementsPerIntervalCount, fillPartnerDeInfo);
    delete fillPartnerDeInfo;
  }

  // garbage collection
  delete [] srcLocalPartnerElementsPerIntervalCount;
  delete [] dstLocalPartnerIntervalPerPetCount;
      
  // prepare count arrays for dst partner look-up in srcSeqIndexFactorLookup
  int *dstLocalPartnerElementsPerIntervalCount = new int[petCount];
  for (int i=0; i<petCount; i++){
    // Pet "i" is the active dstSeqIndex interval
    int seqIndexMin = srcSeqIndexInterval[i].min;
    int seqIndexMax = srcSeqIndexInterval[i].max;
    int count = 0; // reset
    for (int j=0; j<dstSeqIndexInterval[localPet].countEff; j++){
      for (int k=0; k<dstSeqIndexFactorLookup[j].factorCount; k++){
        int partnerSeqIndex = dstSeqIndexFactorLookup[j].factorList[k]
          .partnerSeqIndex.decompSeqIndex;
        if (partnerSeqIndex >= seqIndexMin && partnerSeqIndex <= seqIndexMax)
          ++count; // increment counter
      }
    }
    dstLocalPartnerElementsPerIntervalCount[i] = count;
  }
  int *srcLocalPartnerIntervalPerPetCount = new int[petCount];
  vm->alltoall(dstLocalPartnerElementsPerIntervalCount, sizeof(int),
    srcLocalPartnerIntervalPerPetCount, sizeof(int), vmBYTE);
  
  // fill partnerDe in dstSeqIndexFactorLookup using srcSeqIndexFactorLookup
  {
    DD::FillPartnerDeInfo *fillPartnerDeInfo = new DD::FillPartnerDeInfo;
    fillPartnerDeInfo->localPet = localPet;
    fillPartnerDeInfo->seqIndexIntervalIn = srcSeqIndexInterval;
    fillPartnerDeInfo->seqIndexIntervalOut = dstSeqIndexInterval;
    fillPartnerDeInfo->seqIndexFactorLookupIn = srcSeqIndexFactorLookup;
    fillPartnerDeInfo->seqIndexFactorLookupOut = dstSeqIndexFactorLookup;
    fillPartnerDeInfo->tensorMixFlag = tensorMixFlag;
      
    DD::accessLookup(vm, petCount, localPet, srcLocalPartnerIntervalPerPetCount,
      dstLocalPartnerElementsPerIntervalCount, fillPartnerDeInfo);
    delete fillPartnerDeInfo;
  }

  // garbage collection
  delete [] dstLocalPartnerElementsPerIntervalCount;
  delete [] srcLocalPartnerIntervalPerPetCount;

#ifdef ASMMSTOREPRINT
  // some serious printing for src info
  for (int i=0; i<srcSeqIndexInterval[localPet].count; i++){
    printf("gjt srcDistDir: localPet %d, srcSeqIndex = %d, "
      "srcSeqIndexFactorLookup[%d].factorCount = %d, .de = %d\n",
      localPet, i+srcSeqIndexInterval[localPet].min, i,
      srcSeqIndexFactorLookup[i].factorCount, 
      srcSeqIndexFactorLookup[i].de);
    for (int j=0; j<srcSeqIndexFactorLookup[i].factorCount; j++)
      printf("gjt srcDistDir: localPet %d, srcSeqIndexFactorLookup[%d]."
        "factorList[%d].partnerSeqIndex.decompSeqIndex = %d, "
        ".partnerDe = %d, .factor = %g\n", localPet, i, j,
        srcSeqIndexFactorLookup[i].factorList[j].partnerSeqIndex.decompSeqIndex,
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
      printf("gjt dstDistDir: localPet %d, dstSeqIndexFactorLookup[%d]."
        "factorList[%d].partnerSeqIndex.decompSeqIndex = %d, "
        ".partnerDe = %d, .factor = %g\n", localPet, i, j,
        dstSeqIndexFactorLookup[i].factorList[j].partnerSeqIndex.decompSeqIndex,
        dstSeqIndexFactorLookup[i].factorList[j].partnerDe,
        *((double *)dstSeqIndexFactorLookup[i].factorList[j].factor));
  }
#endif
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t6);   //gjt - profile
#endif
  
  //---------------------------------------------------------------------------
  // Phase III
  //---------------------------------------------------------------------------

  ArrayHelper::MemHelper *memHelper = new ArrayHelper::MemHelper();

  // access srcSeqIndexFactorLookup to obtain complete srcLinSeqList
  {  
    DD::FillLinSeqListInfo *fillLinSeqListInfo = new DD::FillLinSeqListInfo;
    fillLinSeqListInfo->linSeqList = srcLinSeqList;
    fillLinSeqListInfo->localPet = localPet;
    fillLinSeqListInfo->localDeCount = srcLocalDeCount;
    fillLinSeqListInfo->localDeElementCount = srcLocalDeElementCount;
    fillLinSeqListInfo->seqIndexInterval = srcSeqIndexInterval;
    fillLinSeqListInfo->seqIndexFactorLookup = srcSeqIndexFactorLookup;
    fillLinSeqListInfo->tensorMixFlag = tensorMixFlag;
    fillLinSeqListInfo->memHelper = memHelper;

    DD::accessLookup(vm, petCount, localPet, srcLocalIntervalPerPetCount,
      srcLocalElementsPerIntervalCount, fillLinSeqListInfo);
    delete fillLinSeqListInfo;
  }
  
  // access dstSeqIndexFactorLookup to obtain complete dstLinSeqList
  {
    DD::FillLinSeqListInfo *fillLinSeqListInfo = new DD::FillLinSeqListInfo;
    fillLinSeqListInfo->linSeqList = dstLinSeqList;
    fillLinSeqListInfo->localPet = localPet;
    fillLinSeqListInfo->localDeCount = dstLocalDeCount;
    fillLinSeqListInfo->localDeElementCount = dstLocalDeElementCount;
    fillLinSeqListInfo->seqIndexInterval = dstSeqIndexInterval;
    fillLinSeqListInfo->seqIndexFactorLookup = dstSeqIndexFactorLookup;
    fillLinSeqListInfo->tensorMixFlag = tensorMixFlag;
    fillLinSeqListInfo->memHelper = memHelper;

    DD::accessLookup(vm, petCount, localPet, dstLocalIntervalPerPetCount,
      dstLocalElementsPerIntervalCount, fillLinSeqListInfo);
    delete fillLinSeqListInfo;
  }

#ifdef ASMMSTOREPRINT
  // more serious printing
  for (int j=0; j<srcLocalDeCount; j++){
    for (int k=0; k<srcLocalDeElementCount[j]; k++){
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
    for (int k=0; k<dstLocalDeElementCount[j]; k++){
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
  
  // garbage colletion
  delete [] srcLocalElementsPerIntervalCount;
  delete [] srcLocalIntervalPerPetCount;
  delete [] dstLocalElementsPerIntervalCount;
  delete [] dstLocalIntervalPerPetCount;
  delete [] srcSeqIndexFactorLookup;
  delete [] srcSeqIndexInterval;
  delete [] dstSeqIndexFactorLookup;
  delete [] dstSeqIndexInterval;
  delete memHelperFactorLookup;
  
#ifdef ASMMSTORETIMING
  VMK::wtime(&t7);   //gjt - profile
#endif
  
  //---------------------------------------------------------------------------
  // Phase IV
  //---------------------------------------------------------------------------

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
  // obtain typekindSrc
  ESMC_TypeKind typekindSrc = srcArray->getTypekind();
  // obtain typekindDst
  ESMC_TypeKind typekindDst = dstArray->getTypekind();
  
  // prepare dstLocalDeTotalElementCount
  int *dstLocalDeTotalElementCount = new int[dstLocalDeCount];
  for (int i=0; i<dstLocalDeCount; i++)
    dstLocalDeTotalElementCount[i] = 
      dstArray->totalElementCountPLocalDe[i] * dstArray->tensorElementCount;
  
  // prepare tensorContigLength arguments
  int srcTensorContigLength = 1;  // init
  for (int jj=0; jj<srcRank; jj++){
    if (srcArray->arrayToDistGridMap[jj])
      // decomposed dimension
      break;
    else
      // tensor dimension
      srcTensorContigLength *= srcArray->undistUBound[jj]
        - srcArray->undistLBound[jj] + 1;
  }
  int dstTensorContigLength = 1;  // init
  for (int jj=0; jj<dstRank; jj++){
    if (dstArray->arrayToDistGridMap[jj])
      // decomposed dimension
      break;
    else
      // tensor dimension
      dstTensorContigLength *= dstArray->undistUBound[jj]
        - dstArray->undistLBound[jj] + 1;
  }
  
  // encode sparseMatMul communication pattern into XXE stream
  localrc = sparseMatMulStoreEncodeXXE(vm,
    srcArray->delayout, dstArray->delayout,
    tensorMixFlag, srcTensorContigLength, dstTensorContigLength,
    typekindFactors, typekindSrc, typekindDst,
    srcLocalDeElementCount, dstLocalDeElementCount,
    srcLinSeqList, dstLinSeqList, dstLocalDeTotalElementCount,
    rraList, rraCount, routehandle
#ifdef ASMMSTORETIMING
    , &t8, &t9, &t10, &t11, &t12, &t13, &t14
#endif
  );
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;

  // garbage collection
  delete [] rraList;
  for (int i=0; i<srcLocalDeCount; i++)
    delete [] srcLinSeqList[i];
  delete [] srcLinSeqList;
  delete [] srcLocalDeElementCount;
  for (int i=0; i<dstLocalDeCount; i++)
    delete [] dstLinSeqList[i];
  delete [] dstLinSeqList;
  delete [] dstLocalDeElementCount;
  delete [] dstLocalDeTotalElementCount;
  delete memHelper;  

#ifdef ASMMSTORETIMING
  VMK::wtime(&t15);   //gjt - profile
  printf("gjt - profile for PET %d:\n"
    " t4a1=%g\n t4a2=%g\n t4a3=%g\n t4a=%g\n"
    " t4b1=%g\n t4b2=%g\n t4b3=%g\n t4b=%g\n",
    localPet, t4a1-t3, t4a2-t3, t4a3-t3, t4a-t3, t4b1-t3, t4b2-t3, t4b3-t3,
    t4b-t3);
  printf("gjt - profile for PET %d:\n"
    " t5a1=%g\n t5a2=%g\n t5a3=%g\n t5a=%g\n"
    " t5b1=%g\n t5b2=%g\n t5b3=%g\n t5b4=%g\n t5b5=%g\n t5b=%g\n"
    " t5c=%g\n t5d=%g\n t5e=%g\n t5f=%g\n",
    localPet, t5a1-t4, t5a2-t4, t5a3-t4, t5a-t4,
    t5b1-t4, t5b2-t4, t5b3-t4, t5b4-t4, t5b5-t4, t5b-t4, 
    t5c-t4, t5d-t4, t5e-t4, t5f-t4);
  printf("gjt - profile for PET %d:\n"
    " t1=%g\n t2=%g\n t3=%g\n t4=%g\n t5=%g\n t6=%g\n"
    " t7=%g\n t8=%g\n t9=%g\n t10=%g\n t11=%g\n t12=%g\n t13=%g\n t14=%g\n"
    " t15=%g\n",
    localPet, t1-t0, t2-t0, t3-t0, t4-t0, 
    t5-t0, t6-t0, t7-t0, t8-t0, t9-t0, t10-t0, t11-t0, t12-t0, t13-t0, t14-t0,
    t15-t0);
#endif
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


int sparseMatMulStoreEncodeXXEStream(VM *vm,
  vector<ArrayHelper::RecvnbElement> recvnbVector,
  vector<ArrayHelper::SendnbElement> sendnbVector,
  int srcTermProcessing, int pipelineDepth, XXE::TKId elementTK,
  XXE::TKId valueTK, XXE::TKId factorTK,
  int dataSizeSrc, int dataSizeDst, int dataSizeFactors, int srcLocalDeCount,
  int dstLocalDeCount, const int *dstLocalDeTotalElementCount, char **rraList,
  int rraCount, XXE *xxe);

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::sparseMatMulStoreEncodeXXE()"
//BOPI
// !IROUTINE:  ESMCI::sparseMatMulStoreEncodeXXE
//
// !INTERFACE:
int sparseMatMulStoreEncodeXXE(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vm,                                 // in
  DELayout *srcDelayout,                  // in
  DELayout *dstDelayout,                  // in
  bool tensorMixFlag,                     // in
  int srcTensorContigLength,              // in
  int dstTensorContigLength,              // in
  ESMC_TypeKind typekindFactors,          // in
  ESMC_TypeKind typekindSrc,              // in
  ESMC_TypeKind typekindDst,              // in
  const int *srcLocalDeElementCount,      // in
  const int *dstLocalDeElementCount,      // in
  DD::AssociationElement **srcLinSeqList, // in
  DD::AssociationElement **dstLinSeqList, // in
  const int *dstLocalDeTotalElementCount, // in
  char **rraList,                         // in
  int rraCount,                           // in
  ESMC_RouteHandle **routehandle          // inout - handle to precomputed comm
#ifdef ASMMSTORETIMING
  , double *t8, double *t9, double *t10, double *t11, double *t12, double *t13,
  double *t14
#endif
  ){    
//
// !DESCRIPTION:
//    Take the incoming sparse matrix information in "work distribution" and
//    use it to encode an XXE stream for the sparseMatMul. 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{
  // create and initialize the RouteHandle
  *routehandle = ESMC_RouteHandleCreate(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // todo: I have no idea what some of these settings do, just copied it for now
  // todo: from what I saw being set in ESMF_IArrayHaloStoreIndex()
  // todo: All I need here is a valid RouteHandle so I can attach an XXE object.
  localrc =
    (*routehandle)->ESMC_RouteHandleSetType(ESMC_ARRAYXXE);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  localrc = (*routehandle)->ESMC_RouteHandleSetRouteCount(0);
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
    xxe = new XXE(vm, 1000, 10000, 1000);
  }catch (...){
    ESMC_LogDefault.ESMC_LogAllocError(&rc);
    return rc;
  }
  localrc = (*routehandle)->ESMC_RouteHandleSetStorage(xxe);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // set typekind in xxe which is used to check Arrays before ASMM execution 
  xxe->typekind[0] = typekindFactors;
  xxe->typekind[1] = typekindSrc;
  xxe->typekind[2] = typekindDst;
  // prepare XXE type variables
  XXE::TKId elementTK;
  switch (typekindDst){
  case ESMC_TYPEKIND_R4:
    elementTK = XXE::R4;
    break;
  case ESMC_TYPEKIND_R8:
    elementTK = XXE::R8;
    break;
  case ESMC_TYPEKIND_I4:
    elementTK = XXE::I4;
    break;
  case ESMC_TYPEKIND_I8:
    elementTK = XXE::I8;
    break;
  default:
    break;
  }
  XXE::TKId valueTK;
  switch (typekindSrc){
  case ESMC_TYPEKIND_R4:
    valueTK = XXE::R4;
    break;
  case ESMC_TYPEKIND_R8:
    valueTK = XXE::R8;
    break;
  case ESMC_TYPEKIND_I4:
    valueTK = XXE::I4;
    break;
  case ESMC_TYPEKIND_I8:
    valueTK = XXE::I8;
    break;
  default:
    break;
  }
  XXE::TKId factorTK;
  switch (typekindFactors){
  case ESMC_TYPEKIND_R4:
    factorTK = XXE::R4;
    break;
  case ESMC_TYPEKIND_R8:
    factorTK = XXE::R8;
    break;
  case ESMC_TYPEKIND_I4:
    factorTK = XXE::I4;
    break;
  case ESMC_TYPEKIND_I8:
    factorTK = XXE::I8;
    break;
  default:
    break;
  }
  // prepare other local variables
  int dataSizeFactors = ESMC_TypeKindSize(typekindFactors);
  
  int dataSizeSrc = ESMC_TypeKindSize(typekindSrc);
  int srcLocalDeCount = srcDelayout->getLocalDeCount();
  const int *srcLocalDeList = srcDelayout->getLocalDeList();
  
  int dataSizeDst = ESMC_TypeKindSize(typekindDst);
  int dstLocalDeCount = dstDelayout->getLocalDeCount();
  const int *dstLocalDeList = dstDelayout->getLocalDeList();
  
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
    
#ifdef ASMMSTORETIMING
  double t9a, t9b, t9d, t9e; //gjt - profile
  double t9c1, t9c2; //gjt - profile
  VMK::wtime(t8);   //gjt - profile
#endif
    
  // determine recv pattern for all localDEs on dst side
  vector<ArrayHelper::RecvnbElement> recvnbVector;
  for (int j=0; j<dstLocalDeCount; j++){
    int *index2Ref = new int[dstLocalDeElementCount[j]];  // large enough
    int localDeFactorCount = 0; // reset
    int iCount = 0; // reset
    for (int k=0; k<dstLocalDeElementCount[j]; k++){
      int factorCount = dstLinSeqList[j][k].factorCount;
      if (factorCount){
        index2Ref[iCount] = k;   // store element index
        localDeFactorCount += factorCount;
        ++iCount; // increment counter
      }
    }

#ifdef ASMMSTORETIMING
    VMK::wtime(&t9a);   //gjt - profile
#endif
        
#ifdef ASMMSTOREPRINT
printf("iCount: %d, localDeFactorCount: %d\n", iCount, localDeFactorCount);
#endif
    int *index2Ref2 = new int[localDeFactorCount];  // large enough
    int *factorIndexRef = new int[localDeFactorCount];  // large enough
    int *partnerDeRef = new int[localDeFactorCount];  // large enough
    vector<int> recvnbPartnerDeList(localDeFactorCount);  // large enough
    vector<int> recvnbPartnerDeCount(localDeFactorCount);  // large enough
    int recvnbDiffPartnerDeCount = 0; // reset
    int count = 0; // reset
    for (int i=0; i<iCount; i++){
      int factorCount = dstLinSeqList[j][index2Ref[i]].factorCount;
      for (int k=0; k<factorCount; k++){
        int partnerDe = dstLinSeqList[j][index2Ref[i]].factorList[k].partnerDe;
        int kk;
        for (kk=0; kk<recvnbDiffPartnerDeCount; kk++)
          if (recvnbPartnerDeList[kk]==partnerDe) break;
        if (kk==recvnbDiffPartnerDeCount){
          // new entry
          recvnbPartnerDeList[kk] = partnerDe;
          recvnbPartnerDeCount[kk] = 1; // initialize
          ++recvnbDiffPartnerDeCount;
        }else
          ++recvnbPartnerDeCount[kk];   // increment
        index2Ref2[count] = index2Ref[i];
        factorIndexRef[count] = k;
        partnerDeRef[count] = kk;
        ++count;
      }
    }
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t9b);   //gjt - profile
#endif
        
    // invert the look-up direction
    vector<vector<ArrayHelper::DstInfo> >
      dstInfoTable(recvnbDiffPartnerDeCount);
    vector<int> dstInfoTableInit(recvnbDiffPartnerDeCount);
    for (int i=0; i<recvnbDiffPartnerDeCount; i++){
      dstInfoTable[i].resize(recvnbPartnerDeCount[i]);
      dstInfoTableInit[i] = 0;   // reset
    }
    char *localDeFactorBuffer = new char[localDeFactorCount * dataSizeFactors];
    localrc = xxe->storeStorage(localDeFactorBuffer); // XXE garbage collec.
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef ASMMSTORETIMING
    VMK::wtime(&t9c1);   //gjt - profile
#endif
    for (int i=0; i<localDeFactorCount; i++){
      int partnerDeListIndex = partnerDeRef[i];
      int index2 = dstInfoTableInit[partnerDeListIndex]++;
      dstInfoTable[partnerDeListIndex][index2].linIndex =
        dstLinSeqList[j][index2Ref2[i]].linIndex;
      dstInfoTable[partnerDeListIndex][index2].vectorLength = 1;  // default
      dstInfoTable[partnerDeListIndex][index2].seqIndex =
        dstLinSeqList[j][index2Ref2[i]].seqIndex;
      dstInfoTable[partnerDeListIndex][index2].partnerSeqIndex
        = dstLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .partnerSeqIndex;
      if (!tensorMixFlag){
        // default into tensor for tensor src/dst mode
        dstInfoTable[partnerDeListIndex][index2].partnerSeqIndex
          .tensorSeqIndex
          = dstInfoTable[partnerDeListIndex][index2].seqIndex.tensorSeqIndex;
      }
      char *localDeFactorBufferEntry = localDeFactorBuffer + i*dataSizeFactors;
      memcpy(localDeFactorBufferEntry,
        dstLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .factor, dataSizeFactors);
      dstInfoTable[partnerDeListIndex][index2].factor =
        (void *)(localDeFactorBufferEntry);
    }
    
    // garbage collection
    delete [] index2Ref;
    delete [] index2Ref2;
    delete [] factorIndexRef;
    delete [] partnerDeRef;
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t9c2);   //gjt - profile
#endif    
    // sort each "recvnbDiffPartnerDeCount group" (opposite of src)
    if (tensorMixFlag || (srcTensorContigLength != dstTensorContigLength) ||
      (srcTensorContigLength == 1)){
      // sort for scalar optimization
      for (int i=0; i<recvnbDiffPartnerDeCount; i++)
        sort(dstInfoTable[i].begin(), dstInfoTable[i].end(),
          ArrayHelper::scalarOrderDstInfo);
    }else{
      // sort vector optimization
      for (int i=0; i<recvnbDiffPartnerDeCount; i++){
        sort(dstInfoTable[i].begin(), dstInfoTable[i].end(),
          ArrayHelper::vectorOrderDstInfo);
        // vectorize -> deflate dstInfoTable 
        vector<ArrayHelper::DstInfo>::iterator rangeStart =
          dstInfoTable[i].begin();
        vector<ArrayHelper::DstInfo>::iterator rangeStop = rangeStart;
        vector<ArrayHelper::DstInfo>::iterator rangeWrite = rangeStart;
        while (rangeStart != dstInfoTable[i].end()){
          int vectorLength = 1; // initialize
          int decompSeqIndex = rangeStart->seqIndex.decompSeqIndex;
          int linIndex = rangeStart->linIndex;
          rangeStop++;
          while((rangeStop != dstInfoTable[i].end())
            && (rangeStop->seqIndex.decompSeqIndex == decompSeqIndex)
            && (rangeStop->linIndex == (linIndex + 1))){
            linIndex = rangeStop->linIndex;
            ++vectorLength;
            rangeStop++;
          }
          if ((rangeWrite != dstInfoTable[i].begin())
            && ((rangeWrite-1)->vectorLength != vectorLength)){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_INCONS,
            "- vectorization failed", &rc);
            return rc;
          }
          *rangeWrite = *rangeStart;  // copy table element
          rangeWrite->vectorLength = vectorLength;
          rangeWrite++;
          rangeStart = rangeStop;
        }
        dstInfoTable[i].erase(rangeWrite, dstInfoTable[i].end());
      }
    }

#ifdef ASMMSTOREPRINT
    // print:
    printf("dstArray: %d, %d\n", j, recvnbDiffPartnerDeCount); 
    for (int i=0; i<recvnbDiffPartnerDeCount; i++)
      for (int k=0; k<dstInfoTable[i].size(); k++)
        printf("dstInfoTable[%d][%d].seqIndex = %d/%d, .partnerSeqIndex[][] ="
          " %d/%d\n", i, k,
          dstInfoTable[i][k].seqIndex.decompSeqIndex, 
          dstInfoTable[i][k].seqIndex.tensorSeqIndex, 
          dstInfoTable[i][k].partnerSeqIndex.decompSeqIndex, 
          dstInfoTable[i][k].partnerSeqIndex.tensorSeqIndex);
#endif
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t9d);   //gjt - profile
#endif

    // construct recv elements
    for (int i=0; i<recvnbDiffPartnerDeCount; i++){
      // large contiguous 1st level receive buffer
      char *buffer = new char[recvnbPartnerDeCount[i] * dataSizeSrc];
      localrc = xxe->storeStorage(buffer); // XXE garbage collec.
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      int srcDe = recvnbPartnerDeList[i];
      int srcPet;   //TODO: DE-based comms
      srcDelayout->getDEMatchPET(srcDe, *vm, NULL, &srcPet, 1);
      int dstDe = dstLocalDeList[j];
      // fill values into recvnbVector
      ArrayHelper::RecvnbElement recvnbElement;
      recvnbElement.srcPet = srcPet;
      recvnbElement.srcDe = srcDe;
      recvnbElement.srcLocalDe = i;
      recvnbElement.dstDe = dstDe;
      recvnbElement.dstLocalDe = j;
      recvnbElement.buffer = buffer;
      recvnbElement.partnerDeDataCount = dstInfoTable[i].size();
      recvnbElement.vectorLength = dstInfoTable[i].begin()->vectorLength;
      recvnbElement.dstInfoTable = dstInfoTable[i];
      recvnbElement.localPet = localPet;
      recvnbElement.petCount = petCount;
      recvnbVector.push_back(recvnbElement);
#ifdef ASMMSTOREPRINT
      printf("gjt: recvnbElement localPet %d, srcPet %d, vectorLength=%d\n",
        localPet, srcPet, recvnbElement.vectorLength);
#endif
    } // for i - recvnbDiffPartnerDeCount
    
#ifdef ASMMSTORETIMING
    VMK::wtime(&t9e);   //gjt - profile
    printf("gjt - profile for PET %d, j-loop %d:\n"
      " t9a=%g\n t9b=%g\n t9c1=%g\n t9c2=%g\n t9d=%g\n t9e=%g\n", localPet, j,
      t9a-*t8, t9b-*t8, t9c1-*t8, t9c2-*t8, t9d-*t8, t9e-*t8);
#endif
    
  } // for j - dstLocalDeCount
    
#ifdef ASMMSTORETIMING
  VMK::wtime(t9);   //gjt - profile
#endif
  
  // determine send pattern for all localDEs on src side
  vector<ArrayHelper::SendnbElement> sendnbVector;
  for (int j=0; j<srcLocalDeCount; j++){
    int *index2Ref = new int[srcLocalDeElementCount[j]];  // large enough
    int localDeFactorCount = 0; // reset
    int iCount = 0; // reset
    for (int k=0; k<srcLocalDeElementCount[j]; k++){
      int factorCount = srcLinSeqList[j][k].factorCount;
      if (factorCount){
        index2Ref[iCount] = k;   // store element index
        localDeFactorCount += factorCount;
        ++iCount; // increment counter
      }
    }
    int *index2Ref2 = new int[localDeFactorCount];  // large enough
    int *factorIndexRef = new int[localDeFactorCount];  // large enough
    int *partnerDeRef = new int[localDeFactorCount];  // large enough
    int *sendnbPartnerDeList = new int[localDeFactorCount];  // large enough
    int *sendnbPartnerDeCount = new int[localDeFactorCount];  // large enough
    int sendnbDiffPartnerDeCount = 0; // reset
    int count = 0; // reset
    for (int i=0; i<iCount; i++){
      int factorCount = srcLinSeqList[j][index2Ref[i]].factorCount;
      for (int k=0; k<factorCount; k++){
        int partnerDe = srcLinSeqList[j][index2Ref[i]].factorList[k].partnerDe;
        int kk;
        for (kk=0; kk<sendnbDiffPartnerDeCount; kk++)
          if (sendnbPartnerDeList[kk]==partnerDe) break;
        if (kk==sendnbDiffPartnerDeCount){
          // new entry
          sendnbPartnerDeList[kk] = partnerDe;
          sendnbPartnerDeCount[kk] = 1; // initialize
          ++sendnbDiffPartnerDeCount;
        }else
          ++sendnbPartnerDeCount[kk];   // increment
        index2Ref2[count] = index2Ref[i];
        factorIndexRef[count] = k;
        partnerDeRef[count] = kk;
        ++count;
      }
    }
    // invert the look-up direction
    vector<vector<ArrayHelper::SrcInfo> >
      srcInfoTable(sendnbDiffPartnerDeCount);
    vector<int> srcInfoTableInit(sendnbDiffPartnerDeCount);
    for (int i=0; i<sendnbDiffPartnerDeCount; i++){
      srcInfoTable[i].resize(sendnbPartnerDeCount[i]);
      srcInfoTableInit[i] = 0;   // reset
    }
    char *localDeFactorBuffer = new char[localDeFactorCount * dataSizeFactors];
    localrc = xxe->storeStorage(localDeFactorBuffer); // XXE garbage collec.
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
    for (int i=0; i<localDeFactorCount; i++){
      int partnerDeListIndex = partnerDeRef[i];
      int index2 = srcInfoTableInit[partnerDeListIndex]++;
      srcInfoTable[partnerDeListIndex][index2].linIndex =
        srcLinSeqList[j][index2Ref2[i]].linIndex;
      srcInfoTable[partnerDeListIndex][index2].vectorLength = 1;
      srcInfoTable[partnerDeListIndex][index2].seqIndex =
        srcLinSeqList[j][index2Ref2[i]].seqIndex;
      srcInfoTable[partnerDeListIndex][index2].partnerSeqIndex =
        srcLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .partnerSeqIndex;
      if (!tensorMixFlag){
        // default into tensor for tensor src/dst mode
        srcInfoTable[partnerDeListIndex][index2].partnerSeqIndex
          .tensorSeqIndex =
          srcInfoTable[partnerDeListIndex][index2].seqIndex.tensorSeqIndex;
      }
      char *localDeFactorBufferEntry = localDeFactorBuffer + i*dataSizeFactors;
      memcpy(localDeFactorBufferEntry,
        srcLinSeqList[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .factor, dataSizeFactors);
      srcInfoTable[partnerDeListIndex][index2].factor =
        (void *)(localDeFactorBufferEntry);
    }
    
    // garbage collection
    delete [] index2Ref;
    delete [] index2Ref2;
    delete [] factorIndexRef;
    delete [] partnerDeRef;
    
    // sort each "sendnbDiffPartnerDeCount group" (opposite of dst)
    if (tensorMixFlag || (srcTensorContigLength != dstTensorContigLength) ||
      (srcTensorContigLength == 1)){
      // sort for scalar optimization
      for (int i=0; i<sendnbDiffPartnerDeCount; i++)
        sort(srcInfoTable[i].begin(), srcInfoTable[i].end(),
          ArrayHelper::scalarOrderSrcInfo);
    }else{
      // sort vector optimization
      for (int i=0; i<sendnbDiffPartnerDeCount; i++){
        sort(srcInfoTable[i].begin(), srcInfoTable[i].end(),
          ArrayHelper::vectorOrderSrcInfo);
        // vectorize -> deflate srcInfoTable 
        vector<ArrayHelper::SrcInfo>::iterator rangeStart =
          srcInfoTable[i].begin();
        vector<ArrayHelper::SrcInfo>::iterator rangeStop = rangeStart;
        vector<ArrayHelper::SrcInfo>::iterator rangeWrite = rangeStart;
        while (rangeStart != srcInfoTable[i].end()){
          int vectorLength = 1; // initialize
          int decompSeqIndex = rangeStart->seqIndex.decompSeqIndex;
          int linIndex = rangeStart->linIndex;
          rangeStop++;
          while((rangeStop != srcInfoTable[i].end())
            && (rangeStop->seqIndex.decompSeqIndex == decompSeqIndex)
            && (rangeStop->linIndex == (linIndex + 1))){
            linIndex = rangeStop->linIndex;
            ++vectorLength;
            rangeStop++;
          }
          if ((rangeWrite != srcInfoTable[i].begin())
            && ((rangeWrite-1)->vectorLength != vectorLength)){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_INCONS,
            "- vectorization failed", &rc);
            return rc;
          }
          *rangeWrite = *rangeStart;  // copy table element
          rangeWrite->vectorLength = vectorLength;
          rangeWrite++;
          rangeStart = rangeStop;
        }
        srcInfoTable[i].erase(rangeWrite, srcInfoTable[i].end());
      }
    }
        
#ifdef ASMMSTOREPRINT
    // print:
    for (int i=0; i<sendnbDiffPartnerDeCount; i++)
      for (int k=0; k<srcInfoTable[i].size(); k++)
        printf("srcInfoTable[%d][%d].seqIndex = %d/%d, .partnerSeqIndex[][] ="
          " %d/%d\n", i, k,
          srcInfoTable[i][k].seqIndex.decompSeqIndex, 
          srcInfoTable[i][k].seqIndex.tensorSeqIndex, 
          srcInfoTable[i][k].partnerSeqIndex.decompSeqIndex, 
          srcInfoTable[i][k].partnerSeqIndex.tensorSeqIndex);
#endif
    
    // construct send elements
    for (int i=0; i<sendnbDiffPartnerDeCount; i++){
      // determine contiguous runs in linIndex to minimize memcpy overhead
      vector<ArrayHelper::LinIndexContigBlock> linIndexContigBlockList;
      // initialize linIndexContigBlockList[]
      ArrayHelper::LinIndexContigBlock block;
      block.linIndex = srcInfoTable[i][0].linIndex;
      block.linIndexCount = 1;
      linIndexContigBlockList.push_back(block);
      for (int k=1; k<srcInfoTable[i].size(); k++){
        if (srcInfoTable[i][k-1].linIndex + 1 ==
          srcInfoTable[i][k].linIndex){
          // contiguous step in linIndex
          ++(linIndexContigBlockList.back().linIndexCount);
        }else{
          // discontiguous jump in linIndex
          block.linIndex = srcInfoTable[i][k].linIndex;
          block.linIndexCount = 1;
          linIndexContigBlockList.push_back(block);
        }
      }
      // intermediate buffer (in case it is needed)
      char *buffer = new char[sendnbPartnerDeCount[i] * dataSizeSrc];
      localrc = xxe->storeStorage(buffer); // XXE garbage collec.
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
      int srcDe = srcLocalDeList[j];
      int dstDe = sendnbPartnerDeList[i];
      int dstPet;   //TODO: DE-based comms
      dstDelayout->getDEMatchPET(dstDe, *vm, NULL, &dstPet, 1);
      // fill values into sendnbVector
      ArrayHelper::SendnbElement sendnbElement;
      sendnbElement.dstPet = dstPet;
      sendnbElement.dstDe = dstDe;
      sendnbElement.dstLocalDe = i;
      sendnbElement.srcDe = srcDe;
      sendnbElement.srcLocalDe = j;
      sendnbElement.partnerDeDataCount = srcInfoTable[i].size();
      sendnbElement.vectorLength = srcInfoTable[i].begin()->vectorLength;
      sendnbElement.srcInfoTable = srcInfoTable[i];
      sendnbElement.linIndexContigBlockList = linIndexContigBlockList;
      sendnbElement.buffer = buffer;
      sendnbElement.localPet = localPet;
      sendnbElement.petCount = petCount;
      sendnbVector.push_back(sendnbElement);
#ifdef ASMMSTOREPRINT
      printf("gjt: sendnbElement localPet %d, dstPet %d, vectorLength=%d\n",
        localPet, dstPet, sendnbElement.vectorLength);
#endif
    } // for i - sendnbDiffPartnerDeCount
    // garbage collection
    delete [] sendnbPartnerDeList;
    delete [] sendnbPartnerDeCount;
    
      
  } // for j - srcLocalDeCount
  
#ifdef ASMMSTORETIMING
  VMK::wtime(t10);   //gjt - profile
#endif
  
  // --------------------------------------------------------------
  // recv and send patterns have been determined, ready to use them
  // --------------------------------------------------------------
  
  // sort recv and send vectors to lower communication contention
  // sorting also ensures correct ordering of sendnb and recvnb calls w/o tags
  sort(recvnbVector.begin(), recvnbVector.end());
  sort(sendnbVector.begin(), sendnbVector.end());

#ifdef ASMMSTORETIMING
  VMK::wtime(t11);   //gjt - profile
#endif

  // store current XXE parameter in order to efficiently rewrite multiple times
  const int startCount = xxe->count;
  const int startStorageCount = xxe->storageCount;
  const int startCommhandleCount = xxe->commhandleCount;
  const int startXxeSubCount = xxe->xxeSubCount;
  
  double dtMin;           // to find minimum time
  
#define ASMMSTOREOPTPRINT___disable

  // optimize srcTermProcessing
  int pipelineDepth = 4;  // safe value during srcTermProcessing optimization
  int srcTermProcessingOpt;
  const int srcTermProcMax = 6;
  const int srcTermProcList[] = {0, 1, 2, 3, 4, 20};  // settings to be tried
  for (int srcTermProc=0; srcTermProc<srcTermProcMax; srcTermProc++){
    int srcTermProcessing=srcTermProcList[srcTermProc];
    // start writing a fresh XXE stream
    xxe->clearReset(startCount, startStorageCount, startCommhandleCount,
      startXxeSubCount);
    localrc = sparseMatMulStoreEncodeXXEStream(vm, recvnbVector, sendnbVector,
      srcTermProcessing, pipelineDepth, elementTK, valueTK, factorTK,
      dataSizeSrc, dataSizeDst, dataSizeFactors, srcLocalDeCount,
      dstLocalDeCount, dstLocalDeTotalElementCount, rraList, rraCount, xxe);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
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
    // obtain timing
    double dtAverage = 0.;
    double dtStart, dtEnd;
    const int dtCount = 10;
    for (int i=0; i<dtCount; i++){
      vm->barrier();
      vm->wtime(&dtStart);
        localrc = xxe->exec(rraCount, rraList, 0x0);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
      vm->barrier();
      vm->wtime(&dtEnd);
      dtAverage += dtEnd - dtStart;
    }
    dtAverage /= dtCount;
#ifdef ASMMSTOREOPTPRINT
    printf("localPet: %d, srcTermProcessing=%d -> dtAverage=%gs\n", 
      localPet, srcTermProcessing, dtAverage);
#endif
    // determine optimum srcTermProcessing  
    if (srcTermProcessing==0){
      // first time through -> initialize to find minimum
      dtMin = dtAverage;
      srcTermProcessingOpt = srcTermProcessing;
    }else{
      // compare to current minimum
      if (dtAverage < dtMin){
        // found better time
        dtMin = dtAverage;
        srcTermProcessingOpt = srcTermProcessing;
      }
    }
  } // srcTermProcessing

#ifdef ASMMSTOREOPTPRINT
  printf("localPet: %d, srcTermProcessingOpt=%d -> dtMin=%gs\n", 
    localPet, srcTermProcessingOpt, dtMin);
#endif
    
  // all PETs vote on srcTermProcessingOpt
  vector<int> srcTermProcessingOptList(petCount);
  vm->allgather(&srcTermProcessingOpt, &srcTermProcessingOptList[0],
    sizeof(int));
  sort(srcTermProcessingOptList.begin(), srcTermProcessingOptList.end());
  int votes = 1; // initialize
  int votesMax = 0; // initialize
  for (int i=1; i<petCount; i++){
    if (srcTermProcessingOptList[i-1] == srcTermProcessingOptList[i]){
      // same vote
      ++votes;
    }else{
      // different vote
      if (votes > votesMax){
        // new high vote found
        votesMax = votes;
        srcTermProcessingOpt = srcTermProcessingOptList[i-1];
      }
      votes = 1;
    }
  }
  // check last votes
  if (votes > votesMax){
    // new high vote found
    srcTermProcessingOpt = srcTermProcessingOptList[petCount-1];
  }
  
#ifdef ASMMSTOREOPTPRINT
  printf("localPet: %d, srcTermProcessingOpt=%d -> dtMin=%gs (after vote)\n", 
    localPet, srcTermProcessingOpt, dtMin);
#endif
    
#ifdef ASMMSTORETIMING
  VMK::wtime(t12);   //gjt - profile
#endif

  // optimize pipeline depth
  int pipelineDepthOpt;   // optimium pipeline depth
  for (pipelineDepth=1; pipelineDepth<=petCount; pipelineDepth*=2){
    // start writing a fresh XXE stream
    xxe->clearReset(startCount, startStorageCount, startCommhandleCount,
      startXxeSubCount);
    localrc = sparseMatMulStoreEncodeXXEStream(vm, recvnbVector, sendnbVector,
      srcTermProcessingOpt, pipelineDepth, elementTK, valueTK, factorTK,
      dataSizeSrc, dataSizeDst, dataSizeFactors, srcLocalDeCount,
      dstLocalDeCount, dstLocalDeTotalElementCount, rraList, rraCount, xxe);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
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
    // obtain timing
    double dtAverage = 0.;
    double dtStart, dtEnd;
    const int dtCount = 10;
    for (int i=0; i<dtCount; i++){
      vm->barrier();
      vm->wtime(&dtStart);
        localrc = xxe->exec(rraCount, rraList, 0x0);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
      vm->barrier();
      vm->wtime(&dtEnd);
      dtAverage += dtEnd - dtStart;
    }
    dtAverage /= dtCount;
#ifdef ASMMSTOREOPTPRINT
    printf("localPet: %d, pipelineDepth=%d -> dtAverage=%gs\n", 
      localPet, pipelineDepth, dtAverage);
#endif
    // determine optimum pipelineDepth  
    if (pipelineDepth==1){
      // first time through -> initialize to find minimum
      dtMin = dtAverage;
      pipelineDepthOpt = pipelineDepth;
    }else{
      // compare to current minimum
      if (dtAverage < dtMin){
        // found better time
        dtMin = dtAverage;
        pipelineDepthOpt = pipelineDepth;
      }
    }
  } // pipelineDepth
  
#ifdef ASMMSTOREOPTPRINT
  printf("localPet: %d, pipelineDepthOpt=%d -> dtMin=%gs\n", 
    localPet, pipelineDepthOpt, dtMin);
#endif
    
  // all PETs vote on pipelineDepthOpt
  vector<int> pipelineDepthOptList(petCount);
  vm->allgather(&pipelineDepthOpt, &pipelineDepthOptList[0], sizeof(int));
  sort(pipelineDepthOptList.begin(), pipelineDepthOptList.end());
  votes = 1; // initialize
  votesMax = 0; // initialize
  for (int i=1; i<petCount; i++){
    if (pipelineDepthOptList[i-1] == pipelineDepthOptList[i]){
      // same vote
      ++votes;
    }else{
      // different vote
      if (votes > votesMax){
        // new high vote found
        votesMax = votes;
        pipelineDepthOpt = pipelineDepthOptList[i-1];
      }
      votes = 1;
    }
  }
  // check last votes
  if (votes > votesMax){
    // new high vote found
    pipelineDepthOpt = pipelineDepthOptList[petCount-1];
  }

#ifdef ASMMSTOREOPTPRINT
  printf("localPet: %d, pipelineDepthOpt=%d -> dtMin=%gs (after vote)\n", 
    localPet, pipelineDepthOpt, dtMin);
#endif
      
#ifdef ASMMSTORETIMING
  VMK::wtime(t13);   //gjt - profile
#endif

  // encode with the majority voted pipelineDepthOpt
  xxe->clearReset(startCount, startStorageCount, startCommhandleCount,
    startXxeSubCount);
  localrc = sparseMatMulStoreEncodeXXEStream(vm, recvnbVector, sendnbVector,
    srcTermProcessingOpt, pipelineDepthOpt, elementTK, valueTK, factorTK,
    dataSizeSrc, dataSizeDst, dataSizeFactors, srcLocalDeCount,
    dstLocalDeCount, dstLocalDeTotalElementCount, rraList, rraCount, xxe);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
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
  
#ifdef ASMMSTORETIMING
  VMK::wtime(t14);   //gjt - profile
#endif

  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::sparseMatMulStoreEncodeXXEStream()"
//BOPI
// !IROUTINE:  ESMCI::sparseMatMulStoreEncodeXXEStream
//
// !INTERFACE:
int sparseMatMulStoreEncodeXXEStream(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vm,                                 // in
  vector<ArrayHelper::RecvnbElement> recvnbVector,  // in
  vector<ArrayHelper::SendnbElement> sendnbVector,  // in
  int srcTermProcessing,                  // in
  int pipelineDepth,                      // in
  XXE::TKId elementTK,                    // in
  XXE::TKId valueTK,                      // in
  XXE::TKId factorTK,                     // in
  int dataSizeSrc,                        // in
  int dataSizeDst,                        // in
  int dataSizeFactors,                    // in
  int srcLocalDeCount,                    // in
  int dstLocalDeCount,                    // in
  const int *dstLocalDeTotalElementCount, // in
  char **rraList,                         // in
  int rraCount,                           // in
  XXE *xxe                                // inout - XXE stream
  ){    
//
// !DESCRIPTION:
//    Encode a pipelined XXE stream for the sparseMatMul. 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
    
  try{

#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x0, "Wtimer 0", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
  
    // use pipeline approach
    vector<ArrayHelper::RecvnbElement>::iterator pRecv    =recvnbVector.begin();
    vector<ArrayHelper::RecvnbElement>::iterator pRecvWait=recvnbVector.begin();
    vector<ArrayHelper::SendnbElement>::iterator pSend    =sendnbVector.begin();
    vector<ArrayHelper::SendnbElement>::iterator pSendWait=sendnbVector.begin();

    // prepare pipeline
    for (int i=0; i<pipelineDepth; i++){
      if (pRecv != recvnbVector.end()){
        int k = pRecv - recvnbVector.begin();
        localrc = pRecv->appendRecvnb(xxe, srcTermProcessing, dataSizeSrc, k);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        ++pRecv;
      }
      if (pSend != sendnbVector.end()){
        int k = pSend - sendnbVector.begin();
        localrc = pSend->appendSendnb(xxe, srcTermProcessing, elementTK,
          valueTK, factorTK, dataSizeSrc, rraList, rraCount, k);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        ++pSend;
      }
    }
    
    // append predicated zero operations for the total region
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x1, "Wt: total zero", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
    for (int i=0; i<dstLocalDeCount; i++){
      localrc = xxe->appendZeroVectorRRA(0x1,
        dstLocalDeTotalElementCount[i] * dataSizeDst, srcLocalDeCount + i);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
        ESMF_ERR_PASSTHRU, &rc)) return rc;
    }
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x1, "Wt: /total zero", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
  
    // append predicated zero operations for targeted dst elements
    for(vector<ArrayHelper::RecvnbElement>::iterator pzero=recvnbVector.begin();
      pzero != recvnbVector.end(); ++pzero){
      localrc = pzero->appendZeroSuperScalar(xxe, srcLocalDeCount, elementTK);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
        ESMF_ERR_PASSTHRU, &rc)) return rc;
    }
    
    // fill pipeline
    bool recvnbOK = true; // initialize
    bool sendnbOK = true; // initialize
    while ((pRecv != recvnbVector.end()) || (pSend != sendnbVector.end())){
      if ((pRecv != recvnbVector.end()) && recvnbOK){
        int k = pRecv - recvnbVector.begin();
        localrc = pRecv->appendRecvnb(xxe, srcTermProcessing, dataSizeSrc, k);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        ++pRecv;
      }
      if ((pSend != sendnbVector.end()) && sendnbOK){
        int k = pSend - sendnbVector.begin();
        localrc = pSend->appendSendnb(xxe, srcTermProcessing, elementTK,
          valueTK, factorTK, dataSizeSrc, rraList, rraCount, k);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
        ++pSend;
      }
      if (pRecvWait != recvnbVector.end()){
        // prevent deadlock by staging waits correctly
        int recvnbStage = (localPet - pRecvWait->srcPet + petCount) % petCount;
        int sendnbStage = recvnbStage + 1;  // initialize to allow wait
        if (pSend != sendnbVector.end())
          sendnbStage = (pSend->dstPet - localPet + petCount) % petCount;//actu.
        if (recvnbStage < sendnbStage){
          // wait will not cause deadlock in the staggered Pet pattern
          int k = pRecvWait-recvnbVector.begin();
          localrc = pRecvWait->appendWaitProductSum(xxe, srcTermProcessing,
            srcLocalDeCount, elementTK, valueTK, factorTK, dataSizeDst,
            dataSizeSrc, dataSizeFactors, rraList, rraCount, k);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
          ++pRecvWait;
          recvnbOK = true;  // o.k. to post next recvnb call next iteration
        }else if (recvnbStage == sendnbStage)
          recvnbOK = true;  // this prevents multi DE/PET case from hanging
        else
          recvnbOK = false; // not o.k. to post next recvnb call next iteration
      }
      if (pSendWait != sendnbVector.end()){
        // prevent deadlock by staging waits correctly
        int sendnbStage = (pSendWait->dstPet - localPet + petCount) % petCount;
        int recvnbStage = sendnbStage + 1;  // initialize to allow wait
        if (pRecv != recvnbVector.end())
          recvnbStage = (localPet - pRecv->srcPet + petCount) % petCount;//actu.
        if (sendnbStage < recvnbStage){
          // wait will not cause deadlock in the staggered Pet pattern
          localrc = xxe->appendWaitOnIndex(0x0, pSendWait->sendnbIndex);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef ASMMPROFILE
          char *tempString = new char[80];
          sprintf(tempString, "Wt: done WaitOnIndex: %d",
            pSendWait->sendnbIndex);
          localrc = xxe->appendWtimer(0x0, tempString, xxe->count, xxe->count);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
          delete [] tempString;
#endif
          ++pSendWait;
          sendnbOK = true;  // o.k. to post next sendnb call next iteration
        }else if (sendnbStage == recvnbStage)
          sendnbOK = true;  // this prevents multi DE/PET case from hanging
        else
          sendnbOK = false; // not o.k. to post next sendnb call next iteration
      }
    }
    
    // drain pipeline
    while ((pRecvWait!=recvnbVector.end()) || (pSendWait!=sendnbVector.end())){
      if (pRecvWait != recvnbVector.end()){
        // prevent deadlock by staging waits correctly
        int recvnbStage = (localPet - pRecvWait->srcPet + petCount) % petCount;
        int sendnbStage = recvnbStage + 1;  // initialize to allow wait
        if (pSend != sendnbVector.end())
          sendnbStage = (pSend->dstPet - localPet + petCount) % petCount;//actu.
        if (recvnbStage < sendnbStage){
          // wait will not cause deadlock in the staggered Pet pattern
          int k = pRecvWait-recvnbVector.begin();
          localrc = pRecvWait->appendWaitProductSum(xxe, srcTermProcessing,
            srcLocalDeCount, elementTK, valueTK, factorTK, dataSizeDst,
            dataSizeSrc, dataSizeFactors, rraList, rraCount, k);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
          ++pRecvWait;
        }
      }
      if (pSendWait != sendnbVector.end()){
        // prevent deadlock by staging waits correctly
        int sendnbStage = (pSendWait->dstPet - localPet + petCount) % petCount;
        int recvnbStage = sendnbStage + 1;  // initialize to allow wait
        if (pRecv != recvnbVector.end())
          recvnbStage = (localPet - pRecv->srcPet + petCount) % petCount;//actu.
        if (sendnbStage < recvnbStage){
          // wait will not cause deadlock in the staggered Pet pattern
          localrc = xxe->appendWaitOnIndex(0x0, pSendWait->sendnbIndex);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
#ifdef ASMMPROFILE
          char *tempString = new char[80];
          sprintf(tempString, "Wt: done WaitOnIndex: %d",
            pSendWait->sendnbIndex);
          localrc = xxe->appendWtimer(0x0, tempString, xxe->count, xxe->count);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
            ESMF_ERR_PASSTHRU, &rc)) return rc;
          delete [] tempString;
#endif
          ++pSendWait;
        }
      }
    }
  
    // post all XXE::waitOnAllSendnb at the end
    //  localrc = xxe->appendWaitOnAllSendnb(0x0);
    //  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, 
    //    ESMF_ERR_PASSTHRU, &rc)) return rc;
      
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x0, "Wtimer End", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
      ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
      
#ifdef ASMMPROFILE
    localrc = xxe->appendWtimer(0x0, "Wtimer End2", xxe->count, xxe->count);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
    ESMF_ERR_PASSTHRU, &rc)) return rc;
#endif
    
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
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
  ESMC_RegionFlag zeroflag,             // in    - ESMF_REGION_TOTAL:
                                        //          -> zero out total region
                                        //         ESMF_REGION_SELECT:
                                        //          -> zero out target points
                                        //         ESMF_REGION_EMPTY:
                                        //          -> don't zero out any points
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
  
  try{

#define ASMMTIMING___disable

#ifdef ASMMTIMING
  double t0, t1, t2, t3, t4, t5, t6;            //gjt - profile
  VMK::wtime(&t0);      //gjt - profile
#endif

  // basic input checking
  bool srcArrayFlag = false;
  if (srcArray != ESMC_NULL_POINTER) srcArrayFlag = true;
  bool dstArrayFlag = false;
  if (dstArray != ESMC_NULL_POINTER) dstArrayFlag = true;
  
  // srcArray and dstArray may not point to the identical Array object
  if (srcArrayFlag && dstArrayFlag){
    if (srcArray == dstArray){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- srcArray and dstArray must not be identical", &rc);
      return rc;
    }
  }
  
#ifdef ASMMTIMING
  VMK::wtime(&t1);      //gjt - profile
#endif
  
  // get a handle on the XXE stored in routehandle
  XXE *xxe = (XXE *)(*routehandle)->ESMC_RouteHandleGetStorage();

  // conditionally perform full input checks
  if (checkflag==ESMF_TRUE){
    // check that srcArray's typekind matches
    if (srcArrayFlag && (xxe->typekind[1] != srcArray->getTypekind())){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between srcArray argument and precomputed XXE",
        &rc);
      return rc;
    }
    // check that dstArray's typekind matches
    if (dstArrayFlag && (xxe->typekind[2] != dstArray->getTypekind())){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- TypeKind mismatch between dstArray argument and precomputed XXE",
        &rc);
      return rc;
    }
    //TODO: need to check DistGrid-conformance and congruence of local tiles
    //      between argument Array pair and Array pair used during XXE precomp.
    // use Array::match() method to perform this check. This however, would
    // require that a reference is kept inside the routehandle to the Array
    // pair used during store(). not sure how good an idea that is. The 
    // alternative is to keep a "fingerprint" of the Arrays that's just enough
    // info to perform the congurence check here.
  }
  
#ifdef ASMMTIMING
  VMK::wtime(&t2);      //gjt - profile
#endif
  
  // prepare for relative run-time addressing (RRA)
  int rraCount = 0; // init
  if (srcArrayFlag)
    rraCount += srcArray->delayout->getLocalDeCount();
  if (dstArrayFlag)
    rraCount += dstArray->delayout->getLocalDeCount();
  char **rraList = new char*[rraCount];
  char **rraListPtr = rraList;
  if (srcArrayFlag){
    memcpy((char *)rraListPtr, srcArray->larrayBaseAddrList,
      srcArray->delayout->getLocalDeCount() * sizeof(char *));
    rraListPtr += srcArray->delayout->getLocalDeCount();
  }
  if (dstArrayFlag)
    memcpy((char *)rraListPtr, dstArray->larrayBaseAddrList,
      dstArray->delayout->getLocalDeCount() * sizeof(char *));

#ifdef ASMMTIMING
  VMK::wtime(&t3);      //gjt - profile
#endif
  
  // set filterBitField  
  int filterBitField = 0x0; // init. to execute _all_ operations in XXE stream
  
  //TODO: determine XXE filterBitField for XXE exec(),
  //TODO: considering src vs. dst, DE, phase of nb-call...
  
  if (zeroflag!=ESMF_REGION_TOTAL)
    filterBitField |= 1;  // filter the region_total zero operations
  if (zeroflag!=ESMF_REGION_SELECT)
    filterBitField |= 2;  // filter the region_select zero operations
  
#ifdef ASMMTIMING
  VMK::wtime(&t4);      //gjt - profile
#endif

  // execute XXE stream
  localrc = xxe->exec(rraCount, rraList, filterBitField);
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
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
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

  try{
  
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
      fflush(stdout);
    }
    vm->barrier();
  }
#endif
  
  // delete xxe
  delete xxe;

  // mark storage pointer in RouteHandle as invalid  
  routehandle->ESMC_RouteHandleSetStorage(NULL);
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
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


#ifdef FIRSTNEWARRAYPROTOTYPE


//-----------------------------------------------------------------------------
//
// This section includes all the newArray routines
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
  vm->broadcast(&laRank, sizeof(int), rootPET);
  int *laLength = new int[laRank];
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLengths(laRank, laLength);
  vm->broadcast(laLength, laRank * sizeof(int), rootPET);
  int *laLbound = new int[laRank];
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLbounds(laRank, laLbound);
  vm->broadcast(laLbound, laRank * sizeof(int), rootPET);
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
    vm->broadcast(&kind, sizeof(ESMC_TypeKind), rootPET);
  }else{
    vm->broadcast(&kind, sizeof(ESMC_TypeKind), rootPET);
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
        vm->send(&localArrays, sizeof(ESMC_LocalArray **), pet);
        vm->send(&commhArray, sizeof(ESMC_newArrayCommHandle *), pet);
        vm->send(&thargArray, sizeof(ESMC_newArrayThreadArg *), pet);
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
    vm->recv(&localArrays, sizeof(ESMC_LocalArray **), pet);
    vm->recv(&commhArray, sizeof(ESMC_newArrayCommHandle *), pet);
    vm->recv(&thargArray, sizeof(ESMC_newArrayThreadArg *), pet);
  }
  int *temp_counts = new int[rank];
  int de;
  for (int i=0; i<localDeCount; i++){
    de = localDeList[i];
    if (delayout->serviceOffer(de, NULL) == ESMCI::DELAYOUT_SERVICE_ACCEPT){
      for (int j=0; j<rank; j++)
        temp_counts[j] = localFullUBound[de][j] - localFullLBound[de][j] + 1;
      localArrays[i] =
        ESMC_LocalArray::ESMC_LocalArrayCreate(rank, kind, temp_counts);
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
  vm->broadcast(laLength, rank * sizeof(int), rootPET);
  int *laLbound = new int[rank];
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetLbounds(rank, laLbound);
  vm->broadcast(laLbound, rank * sizeof(int), rootPET);
  int laByteCount;
  if (localPet == rootPET)
    larray->ESMC_LocalArrayGetByteCount(&laByteCount);
  vm->broadcast(&laByteCount, sizeof(int), rootPET);
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
    vm->broadcast(buffer, blockSize, rootPET);
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
  commh->vmk_commh = new VMK::commhandle*[totalHandles];
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
    vm->send(laLength, rank * sizeof(int), deVASList[de],
      &(commh->vmk_commh[(*cc)++]), de+3000);
    commh->vmk_commh[*cc] = NULL; // mark as invalid element
    vm->send(&laByteCount, sizeof(int), deVASList[de],
      &(commh->vmk_commh[(*cc)++]), de+3000);
    commh->vmk_commh[*cc] = NULL; // mark as invalid element
    vm->send(laLbound, rank * sizeof(int), deVASList[de],
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
      vm->send(buffer, blockSize, deVASList[de],
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
  vm->reduce(localResult, result, 1, vmt, (vmOp)op, rootPET);
  
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
  commhArray[localDe].vmk_commh = new VMK::commhandle*[1];
  commhArray[localDe].vmk_commh[0] = NULL;  // mark as invalid
  vm->send(localResult, size, rootVAS,
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
    vm->broadcast(&laRank, sizeof(int), rootPET);
    laLength = new int[laRank];
    larray->ESMC_LocalArrayGetLengths(laRank, laLength);
    vm->broadcast(laLength, laRank * sizeof(int), rootPET);
  }else{
    vm->broadcast(&laRank, sizeof(int), rootPET);
    laLength = new int[laRank];
    vm->broadcast(laLength, laRank * sizeof(int), rootPET);
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
  vm->broadcast(&haloWidth, sizeof(int *), rootPET);
  // don't worry, it is o.k. to overwrite local variables!
  if (haloWidth != NULL){
    if (localPet != rootPET)
      haloWidth = new int[laRank];
    vm->broadcast(haloWidth, laRank * sizeof(int), rootPET);
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
  vm->broadcast(&deCount, sizeof(int), rootPET);
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
  VMK::commhandle *ch_laLength = NULL; // mark as invalid
  vm->recv(laLength, rank * sizeof(int), rootVAS, &ch_laLength, de+3000);
  int laByteCount;
  VMK::commhandle *ch_laByteCount = NULL; // mark as invalid
  vm->recv(&laByteCount, sizeof(int), rootVAS, &ch_laByteCount, de+3000);
  int *laLbound = new int[rank];
  VMK::commhandle *ch_laLbound = NULL; // mark as invalid
  vm->recv(laLbound, rank * sizeof(int), rootVAS, &ch_laLbound, de+3000);
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
  VMK::commhandle *ch_buffer = NULL; // mark as invalid
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
    vm->recv(buffer, blockSize, rootVAS, &ch_buffer, de+3000);
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
//gjt      vm->cancel(&ch_buffer);
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
  VMK::commhandle **vmk_commh = new VMK::commhandle*[deCount];
  int *deVASList = array->deVASList;
  for (int de=0; de<deCount; de++){
    vmk_commh[de] = NULL;    // mark as invalid
    vm->recv(localResultChar, size, deVASList[de], &(vmk_commh[de]),
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

#endif
