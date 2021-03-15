// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Array.C"
//==============================================================================
#define HALO_STORE_MEMLOG_off

#define ASMM_STORE_LOG_off
#define ASMM_STORE_TIMING_off
#define ASMM_STORE_MEMLOG_off
#define ASMM_STORE_TUNELOG_off
#define ASMM_STORE_COMMMATRIX_on
#define ASMM_STORE_DUMPSMM_off

#define ASMM_EXEC_INFO_off
#define ASMM_EXEC_TIMING_off
#define ASMM_EXEC_PROFILE_off

#define MALLOC_TRIM_REPORT_off

//==============================================================================
// Set OPTION!!!
#define SMMSLSQV_OPTION 2
// OPTION 1 - Use sparseMatMulStoreLinSeqVect() (i.e. old) for all cases
// OPTION 2 - Use sparseMatMulStoreLinSeqVect_new() for halo, old all other
// OPTION 3 - Use sparseMatMulStoreLinSeqVect_new() for all cases
//==============================================================================


//==============================================================================
//
// Array class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array methods declared
// in the companion file ESMCI_Array.h
//
// Macros that affect the code:
//   * WORKAROUND_NONBLOCKPROGRESSBUG - when defined limits the outstanding
//     non-blocking sends to one at a time, or in other places uses blocking
//     send calls instead. This work-around was introduced for the
//     discover/pgi-14.1.0/mvapich2-2.0b combination that started hanging some
//     of the RegridWeightGen tests in the --check part.
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_Array.h"

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>
#include <vector>
#include <list>
#include <map>
#include <algorithm>
#include <sstream>
#if (defined ESMF_OS_Linux || defined ESMF_OS_Unicos)
#include <malloc.h>
#endif

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_IO.h"
#include "ESMCI_TraceRegion.h"
#include "ESMCI_Info.h"

#ifdef ASMM_STORE_DUMPSMM_on
extern "C" {
  void FTN_X(f_esmf_outputsimpleweightfile)(char const *fileName, int *count,
    double const *factorList, int const *factorIndexList,
    int *rc, ESMCI_FortranStrLenArg len);
}
#endif

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#define VERBOSITY             (1)       // 0: off, 10: max
//-----------------------------------------------------------------------------


namespace ESMCI {

//-----------------------------------------------------------------------------
//
// Explicit template instantiation (do not confuse with specialization!!!)
// The reason for explicit instantiation here is that it will tell the compiler
// explicitly to instantiate the following special instantiations of the
// template. This way the definition of the templated methods do not have to
// sit with the declaration in the header file, but can be located in the
// source file.
//
//-----------------------------------------------------------------------------

template SparseMatrix<ESMC_I4,ESMC_I4>::SparseMatrix(
  ESMC_TypeKind_Flag const typekind_, void const *factorList_,
  int const factorListCount_, int const srcN_, int const dstN_,
  void const *factorIndexList_);

template SparseMatrix<ESMC_I8,ESMC_I8>::SparseMatrix(
  ESMC_TypeKind_Flag const typekind_, void const *factorList_,
  int const factorListCount_, int const srcN_, int const dstN_,
  void const *factorIndexList_);

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
  ESMC_TypeKind_Flag typekindArg,         // (in)
  int rankArg,                            // (in)
  LocalArray **larrayListArg,             // (in)
  VM::memhandle *mhArg,                   // (in)
  int vasLocalDeCountArg,                 // (in)
  int ssiLocalDeCountArg,                 // (in)
  int *localDeToDeMapArg,                 // (in)
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
  int *distgridToArrayMapArray,           // (in)
  int *arrayToDistGridMapArray,           // (in)
  int *distgridToPackedArrayMapArray,     // (in)
  ESMC_IndexFlag indexflagArg,            // (in)
  int *rc,                                // (out)
  VM *vm                                  // (in)
  ):ESMC_Base(vm){    // allow specific VM instead default
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMCI::Array object.
//    No error checking wrt consistency of input arguments is needed because
//    Array constructor is only to be called by Array::create() interfaces which
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
  mh = mhArg;
  mhCreator = false; //default
  if (mh) mhCreator = true;
  distgrid = distgridArg;
  distgridCreator = distgridCreatorArg;
  delayout = distgrid->getDELayout();
  // copy the PET-local LocalArray pointers
  int localDeCount = delayout->getLocalDeCount();
  vasLocalDeCount = vasLocalDeCountArg;
  ssiLocalDeCount = ssiLocalDeCountArg;
  if (vasLocalDeCount < localDeCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "vasLocalDeCount must not be less than localDeCount", ESMC_CONTEXT, rc);
    return;
  }
  if (ssiLocalDeCount < vasLocalDeCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "ssiLocalDeCount must not be less than vasLocalDeCount", ESMC_CONTEXT, 
      rc);
    return;
  }
  if (localDeToDeMapArg == NULL){
    // use DELayouts map as default
    localDeToDeMapArg = (int *)delayout->getLocalDeToDeMap();
    if (ssiLocalDeCount != localDeCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Default localDeToDeMapArg requires ssiLocalDeCount==localDeCount", 
        ESMC_CONTEXT, rc);
      return;
    }
  }
  localDeToDeMap = new int[ssiLocalDeCount];
  larrayList = new LocalArray*[ssiLocalDeCount];
  if (ssiLocalDeCount){
    memcpy(localDeToDeMap, localDeToDeMapArg, ssiLocalDeCount*sizeof(int));
    memcpy(larrayList, larrayListArg, ssiLocalDeCount*sizeof(LocalArray *));
  }
  // determine the base addresses of the local arrays:
  larrayBaseAddrList = new void*[ssiLocalDeCount];
  for (int i=0; i<ssiLocalDeCount; i++)
    larrayBaseAddrList[i] = larrayList[i]->getBaseAddr();
  // copy the PET-local bound arrays
  int redDimCount = rank - tensorCountArg; // reduced dimCount w/o repl. dims
  exclusiveLBound = new int[redDimCount*ssiLocalDeCount];
  exclusiveUBound = new int[redDimCount*ssiLocalDeCount];
  computationalLBound = new int[redDimCount*ssiLocalDeCount];
  computationalUBound = new int[redDimCount*ssiLocalDeCount];
  totalLBound = new int[redDimCount*ssiLocalDeCount];
  totalUBound = new int[redDimCount*ssiLocalDeCount];
  if (redDimCount*ssiLocalDeCount > 0){
    memcpy(exclusiveLBound, exclusiveLBoundArg,
      redDimCount*ssiLocalDeCount*sizeof(int));
    memcpy(exclusiveUBound, exclusiveUBoundArg,
      redDimCount*ssiLocalDeCount*sizeof(int));
    memcpy(computationalLBound, computationalLBoundArg,
      redDimCount*ssiLocalDeCount*sizeof(int));
    memcpy(computationalUBound, computationalUBoundArg,
      redDimCount*ssiLocalDeCount*sizeof(int));
    memcpy(totalLBound, totalLBoundArg,
      redDimCount*ssiLocalDeCount*sizeof(int));
    memcpy(totalUBound, totalUBoundArg,
      redDimCount*ssiLocalDeCount*sizeof(int));
  }
  // tensor dimensions
  tensorCount = tensorCountArg;
  tensorElementCount = tensorElementCountArg;
  undistLBound = new int[tensorCountArg];
  undistUBound = new int[tensorCountArg];
  if (tensorCountArg){
    memcpy(undistLBound, undistLBoundArray, tensorCountArg * sizeof(int));
    memcpy(undistUBound, undistUBoundArray, tensorCountArg * sizeof(int));
  }
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
  totalElementCountPLocalDe = new int[ssiLocalDeCount];
  for (int i=0; i<ssiLocalDeCount; i++){
    totalElementCountPLocalDe[i] = 1;   // prime totalElementCountPLocalDe elem
    for (int j=0; j<redDimCount; j++){
      totalElementCountPLocalDe[i] *=
        totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
    }
  }

  // contiguous flag
  contiguousFlag = new int[ssiLocalDeCount];
  for (int i=0; i<ssiLocalDeCount; i++)
    contiguousFlag[i] = -1;  // initialize as "not yet constructed"

  // Set up rim members and fill with canonical seqIndex values
  this->setRimMembers();

  // special variables for super-vectorization in XXE
  sizeSuperUndist = new int[redDimCount+1];
  sizeDist = new int[redDimCount*localDeCount];
  int k=0;
  int jj=0;
  for (int j=0; j<redDimCount+1; j++){
    // construct SizeSuperUndist
    sizeSuperUndist[j] = 1; // prime
    for (; k<rank; k++){
      if (arrayToDistGridMap[k]){
        // decomposed dimension found
        ++k;
        break;
      }else{
        // tensor dimension
        sizeSuperUndist[j] *= undistUBound[jj] - undistLBound[jj] + 1;
        ++jj;
      }
    }
    // construct sizeDist
    if (j<redDimCount){
      for (int i=0; i<localDeCount; i++){
        sizeDist[i*redDimCount+j] =
          totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
      }
    }
  }

  // Auxiliary
  localDeCountAux = localDeCount; // TODO: auxiliary for garb until ref. counting

  ioRH = NULL; // invalidate

  // invalidate the name for this Array object in the Base class
  ESMC_BaseSetName(NULL, "Array");

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, rc);
    return;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::destruct()"
//BOPI
// !IROUTINE:  ESMCI::Array::destruct
//
// !INTERFACE:
void Array::destruct(bool followCreator, bool noGarbage){
//
// TODO: The followCreator flag is only needed until we have reference counting
// TODO: For now followCreator, which by default is true, will be coming in as
// TODO: false when calling through the native destructor. This prevents
// TODO: sequence problems during automatic garbage collection until reference
// TODO: counting comes in to solve this problem in the final manner.
//
// !DESCRIPTION:
//    Destruct the internal information structure of an ESMCI::Array object.
//
//EOPI
//-----------------------------------------------------------------------------
  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    // garbage collection
    for (int i=0; i<ssiLocalDeCount; i++){
      // destroy this DEs LocalArray
      int localrc = LocalArray::destroy(larrayList[i]);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }
    // free shared memory handle if it is present and Array responsible for it
    if ((mh != NULL) && mhCreator){
      int localrc;
      VM *vm = delayout->getVM();      
      localrc = vm->ssishmFree(mh);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
      delete mh;
      mh = NULL;
    }
    if (localDeToDeMap != NULL)
      delete [] localDeToDeMap;
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
    if (sizeSuperUndist != NULL)
      delete [] sizeSuperUndist;
    if (sizeDist != NULL)
      delete [] sizeDist;
    if (distgridCreator && followCreator){
      int localrc = DistGrid::destroy(&distgrid, noGarbage);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::constructContiguousFlag()"
//BOPI
// !IROUTINE:  ESMCI::Array::constructContiguousFlag
//
// !INTERFACE:
int Array::constructContiguousFlag(int redDimCount){
//
// !DESCRIPTION:
//    Fill the contiguousFlag member in an ESMCI::Array object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code

  int const localDeCount = delayout->getLocalDeCount();
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  int dimCount = distgrid->getDimCount();

  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
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
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) return localrc;
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

  // return successfully
  return ESMF_SUCCESS;
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
  LocalArray      **larrayListArg,              // (in)
  int             larrayCount,                  // (in)
  DistGrid        *distgrid,                    // (in)
  DataCopyFlag    copyflag,                     // (in)
  InterArray<int> *distgridToArrayMap,          // (in)
  InterArray<int> *computationalEdgeLWidthArg,  // (in)
  InterArray<int> *computationalEdgeUWidthArg,  // (in)
  InterArray<int> *computationalLWidthArg,      // (in)
  InterArray<int> *computationalUWidthArg,      // (in)
  InterArray<int> *totalLWidthArg,              // (in)
  InterArray<int> *totalUWidthArg,              // (in)
  ESMC_IndexFlag  *indexflagArg,                // (in)
  InterArray<int> *undistLBoundArg,             // (in)
  InterArray<int> *undistUBoundArg,             // (in)
  int             *rc                           // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMCI::Array} object from list if LocalArrays and DistGrid.
//EOPI
//-----------------------------------------------------------------------------
#undef DEBUGLOG
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  Array *array;
  try{

  // check the input and get the information together to call construct()
  // larrayListArg -> typekind/rank
  if (larrayListArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to larrayList argument", ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  if (larrayCount < 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "The larrayList argument must provide at least one LocalArray object",
      ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind_Flag typekind = larrayListArg[0]->getTypeKind();
  int rank = larrayListArg[0]->getRank();
  for (int i=1; i<larrayCount; i++){
    if (larrayListArg[0]->getTypeKind() != typekind){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "TypeKind mismatch in the elements of larrayList argument",
        ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (larrayListArg[0]->getRank() != rank){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "Rank mismatch in the elements of larrayList argument", ESMC_CONTEXT,
        rc);
      return ESMC_NULL_POINTER;
    }
  }
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to distgrid", ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  const DELayout *delayout = distgrid->getDELayout();
  int dimCount = distgrid->getDimCount();
#ifdef DEBUGLOG
  {
    std::stringstream debugmsg;
    debugmsg << "rank=" << rank << " dimCount=" << dimCount;
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  // check if distgridToArrayMap was provided and matches rest of arguments
  vector<int> distgridToArrayMapArrayV(dimCount);
  int *distgridToArrayMapArray = &distgridToArrayMapArrayV[0];
  for (int i=0; i<dimCount; i++){
    if (i < rank)
      distgridToArrayMapArray[i] = i+1; // default (basis 1)
    else
      distgridToArrayMapArray[i] = 0;   // default (replicator dims beyond rank)
  }
  if (present(distgridToArrayMap)){
    if (distgridToArrayMap->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "distgridToArrayMap array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (distgridToArrayMap->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "distgridToArrayMap and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(distgridToArrayMapArray, distgridToArrayMap->array,
      dimCount*sizeof(int));
  }
  {
    // check distgridToArrayMapArray
    vector<bool> check(rank);
    for (int i=0; i<rank; i++)
      check[i] = false; // initialize
    for (int i=0; i<dimCount; i++){
      if (distgridToArrayMapArray[i] < 0 || distgridToArrayMapArray[i] > rank){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "invalid distgridToArrayMap element", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (distgridToArrayMapArray[i] > 0){
        if(check[distgridToArrayMapArray[i]-1]){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "invalid distgridToArrayMap element", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
        check[distgridToArrayMapArray[i]-1] = true;
      }
    }
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
#ifdef DEBUGLOG
  {
    std::stringstream debugmsg;
    debugmsg << "rank=" << rank << " dimCount=" << dimCount;
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  // generate arrayToDistGridMap
  vector<int> arrayToDistGridMapArrayV(rank);
  int *arrayToDistGridMapArray = &arrayToDistGridMapArrayV[0];
  for (int i=0; i<rank; i++)
    arrayToDistGridMapArray[i] = 0; // reset  (basis 1), 0 indicates tensor dim
  for (int i=0; i<dimCount; i++)
    if (int j=distgridToArrayMapArray[i])
      arrayToDistGridMapArray[j-1] = i+1;
  // generate distgridToPackedArrayMap - labels the distributed Array dims 1,2,.
  vector<int> distgridToPackedArrayMapV(dimCount);
  int *distgridToPackedArrayMap = &distgridToPackedArrayMapV[0];
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
  vector<int> undistLBoundArrayV(tensorCount);
  int *undistLBoundArray = NULL; // reset
  if (present(undistLBoundArg)){
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "undistLBound array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (undistLBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "undistLBound, arrayspec, distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    undistLBoundArray = undistLBoundArg->array;
  }else if (tensorCount > 0){
    // tensor dimensions are present, but no explicit bounds provided'
    // -> set to bounds of incoming array at DE 0 (should be same across DEs!)
    const int *undistLBound = larrayListArg[0]->getLbounds();
    undistLBoundArray = &undistLBoundArrayV[0];
    int tensorIndex = 0;  // reset
    for (int i=0; i<rank; i++)
      if (arrayToDistGridMapArray[i] == 0){
        undistLBoundArray[tensorIndex] = undistLBound[i];
        ++tensorIndex;
      }
  }
  vector<int> undistUBoundArrayV(tensorCount);
  int *undistUBoundArray = NULL; // reset
  if (present(undistUBoundArg)){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "undistUBound array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (undistUBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "undistUBound, arrayspec, distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    undistUBoundArray = undistUBoundArg->array;
  }else if (tensorCount > 0){
    // tensor dimensions are present, but no explicit bounds provided
    // -> set to bounds of incoming array at DE 0
    // -> set to bounds of incoming array at DE 0 (should be same across DEs!)
    const int *undistUBound = larrayListArg[0]->getUbounds();
    undistUBoundArray = &undistUBoundArrayV[0];
    int tensorIndex = 0;  // reset
    for (int i=0; i<rank; i++)
      if (arrayToDistGridMapArray[i] == 0){
        undistUBoundArray[tensorIndex] = undistUBound[i];
        ++tensorIndex;
      }
  }
  // tensorElementCount
  int tensorElementCount = 1;  // prime tensorElementCount
  for (int i=0; i<tensorCount; i++)
    tensorElementCount *= (undistUBoundArray[i] - undistLBoundArray[i] + 1);

  // delayout -> deCount, localDeCount, localDeToDeMap
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  if ((localDeCount > 0) && (localDeCount != larrayCount)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "Mismatch in localDeCount between larrayList argument and DELayout",
      ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  const int *localDeToDeMap = delayout->getLocalDeToDeMap();
  // distgrid -> indexCountPDimPDe[]
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  // check on indexflag
  ESMC_IndexFlag indexflag = ESMC_INDEX_DELOCAL;  // default
  if (indexflagArg != NULL)
    indexflag = *indexflagArg;
#ifdef DEBUGLOG
  {
    std::stringstream debugmsg;
    debugmsg << "indexflag=" << indexflag;
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  // figure exclusive region
  vector<int> exclusiveLBoundV(redDimCount*localDeCount);
  vector<int> exclusiveUBoundV(redDimCount*localDeCount);
  int *exclusiveLBound = NULL;  // default safe guard
  int *exclusiveUBound = NULL;  // default safe guard
  if (redDimCount*localDeCount > 0){
    exclusiveLBound = &exclusiveLBoundV[0];
    exclusiveUBound = &exclusiveUBoundV[0];
  }
  for (int i=0; i<redDimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    for (int j=0; j<dimCount; j++)
      if (int k=distgridToPackedArrayMap[j])
        exclusiveUBound[i*redDimCount+k-1] = indexCountPDimPDe[de*dimCount+j];
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMC_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeToDeMap[i];
      for (int j=0; j<dimCount; j++){
        // check that this DE/dim has a contiguous index list
        const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
        if (!contigFlagPDimPDe[de*dimCount+j]){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
            "Cannot use non-contiguous decomposition for pseudo global"
            " index space", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
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
  vector<int> computationalEdgeLWidthV(redDimCount);
  vector<int> computationalEdgeUWidthV(redDimCount);
  int *computationalEdgeLWidth = NULL;  // default safe guard
  int *computationalEdgeUWidth = NULL;  // default safe guard
  if (redDimCount){
    computationalEdgeLWidth = &computationalEdgeLWidthV[0];
    computationalEdgeUWidth = &computationalEdgeUWidthV[0];
  }
  if (present(computationalEdgeLWidthArg)){
    if (computationalEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalEdgeLWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalEdgeLWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(computationalEdgeLWidth, computationalEdgeLWidthArg->array,
      redDimCount*sizeof(int));
  }else{
    // set default
    for (int i=0; i<redDimCount; i++)
      computationalEdgeLWidth[i] = 0;
  }
  if (present(computationalEdgeUWidthArg)){
    if (computationalEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalEdgeUWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalEdgeUWidth and distgrid mismatch", ESMC_CONTEXT, rc);
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
  vector<int> computationalLBoundV(redDimCount*localDeCount);
  vector<int> computationalUBoundV(redDimCount*localDeCount);
  int *computationalLBound = NULL;  // default safe guard
  int *computationalUBound = NULL;  // default safe guard
  if (redDimCount*localDeCount > 0){
    computationalLBound = &computationalLBoundV[0];
    computationalUBound = &computationalUBoundV[0];
  }
  if (present(computationalLWidthArg)){
    if (computationalLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalLWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalLWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalLBound[j*redDimCount+i] = exclusiveLBound[j*redDimCount+i]
          - computationalLWidthArg->array[i];
  }else{
    // set default
    if (localDeCount*redDimCount > 0)
      memcpy(computationalLBound, exclusiveLBound,
        localDeCount*redDimCount*sizeof(int));
  }
  if (present(computationalUWidthArg)){
    if (computationalUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalUWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalUWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalUBound[j*redDimCount+i] = exclusiveUBound[j*redDimCount+i]
          + computationalUWidthArg->array[i];
  }else{
    // set default
    if (localDeCount*redDimCount > 0)
      memcpy(computationalUBound, exclusiveUBound,
        localDeCount*redDimCount*sizeof(int));
  }
  // modify computational bounds on tile edges
  for (int j=0; j<localDeCount; j++){
    for (int i=0; i<dimCount; i++){
      if (int k=distgridToPackedArrayMap[i]){
        if (computationalEdgeLWidth[k-1]){
          bool onEdgeL = distgrid->isLocalDeOnEdgeL(j, i+1, &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
          if (onEdgeL)
            computationalLBound[j*redDimCount+k-1]
              -= computationalEdgeLWidth[k-1];
        }
        if (computationalEdgeUWidth[k-1]){
          bool onEdgeU = distgrid->isLocalDeOnEdgeU(j, i+1, &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
          if (onEdgeU)
            computationalUBound[j*redDimCount+k-1]
              += computationalEdgeUWidth[k-1];
        }
      }
    }
  }
  // deal with total widths
  int totalLBoundFlag = 0;  // reset
  int totalUBoundFlag = 0;  // reset
  vector<int> totalLBoundV(redDimCount*localDeCount);
  vector<int> totalUBoundV(redDimCount*localDeCount);
  int *totalLBound = NULL;  // default safe guard
  int *totalUBound = NULL;  // default safe guard
  if (redDimCount*localDeCount > 0){
    totalLBound = &totalLBoundV[0];
    totalUBound = &totalUBoundV[0];
  }
  if (present(totalLWidthArg)){
    totalLBoundFlag = 1;  // set
    if (totalLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "totalLWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "totalLWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "totalLWidth may only contain positive values", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*redDimCount+i] = exclusiveLBound[j*redDimCount+i]
          - totalLWidthArg->array[i];
      }
    }
  }else{
    // set default
    for (int i=0; i<localDeCount*redDimCount; i++)
      totalLBound[i] = computationalLBound[i];
  }
  if (present(totalUWidthArg)){
    totalUBoundFlag = 1;  // set
    if (totalUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "totalUWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "totalUWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "totalUWidth may only contain positive values", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*redDimCount+i] = exclusiveUBound[j*redDimCount+i]
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaLBound / totalLBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalLBound[i] > totalUBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaLBound / totalUBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] < totalLBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationalUBound / totalLBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] > totalUBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationalUBound / totalUBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
    }
  }

  // allocate LocalArray list that holds all PET-local DEs and adjust elements
  vector<LocalArray *> larrayListV(localDeCount);
  LocalArray **larrayList = NULL; // default safe guard
  if (larrayListV.size())
    larrayList = &larrayListV[0];
  vector<int> temp_larrayLBound(rank);
  vector<int> temp_larrayUBound(rank);
  for (int i=0; i<localDeCount; i++){
    if (indexflag == ESMC_INDEX_USER){
      // don't adjust dope vector, use F90 pointers directly and their bounds
      const int *temp_counts = larrayListArg[i]->getCounts();
      int j=0;    // reset distributed index
      int jjj=0;  // reset undistributed index
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          if (temp_counts[jj] <
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
              "LocalArray does not accommodate requested element count",
              ESMC_CONTEXT, rc);
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
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
              "LocalArray does not match requested element count",
              ESMC_CONTEXT, rc);
            return ESMC_NULL_POINTER;
          }
          ++j;
        }else{
          // non-distributed dimension
          if (temp_counts[jj] !=
            undistUBoundArray[jjj] - undistLBoundArray[jjj] + 1){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
              "LocalArray does not match requested element count",
              ESMC_CONTEXT, rc);
            return ESMC_NULL_POINTER;
          }
          ++jjj;
        }
      }
      // adjust all of the bounds to match absolute bounds of F90 pointers
      const int *larrayLBound = larrayListArg[i]->getLbounds();
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
      // Use the incoming LocalArray objects directly. No need for a create
      // from copy here because the bounds and Fortran dope vector will stay
      // unchanged.
      larrayList[i] = larrayListArg[i];
    }else{
      // prepare to adjust dope vector
      const int *temp_counts = larrayListArg[i]->getCounts();
      int j=0;    // reset distributed index
      int jjj=0;  // reset undistributed index
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          if (temp_counts[jj] <
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1){
            std::stringstream debugmsg;
            debugmsg << "jj=" << jj << ": " << temp_counts[jj] << " < "
              << totalUBound[i*redDimCount+j] << " - "
              << totalLBound[i*redDimCount+j] << " + 1";
            ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_WARN);
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
              "LocalArray does not accommodate requested element count",
              ESMC_CONTEXT, rc);
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
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
              "LocalArray does not match requested element count",
              ESMC_CONTEXT, rc);
            return ESMC_NULL_POINTER;
          }
          temp_larrayLBound[jj] = undistLBoundArray[jjj];
          temp_larrayUBound[jj] = undistUBoundArray[jjj];
          ++jjj;
        }
      }
      // Make a copy of the LocalArray object with adjusted undistLBound and
      // undistUBound values. The returned LocalArray object will contain a
      // correctly adjusted Fortran dope vector.
      // Depending on copyflag the original memory used for data storage will
      // either be referenced, or a copy of the data will have been made.
      larrayList[i] = LocalArray::create(larrayListArg[i], copyflag,
        &temp_larrayLBound[0], &temp_larrayUBound[0], &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
    }
  }

  // call class constructor
  try{
    int vasLocalDeCountArg = localDeCount;
    int ssiLocalDeCountArg = localDeCount;
    array = new Array(typekind, rank, larrayList, NULL, vasLocalDeCountArg,
      ssiLocalDeCountArg, NULL, distgrid, false,
      exclusiveLBound, exclusiveUBound, computationalLBound,
      computationalUBound, totalLBound, totalUBound, tensorCount,
      tensorElementCount, undistLBoundArray, undistUBoundArray,
      distgridToArrayMapArray, arrayToDistGridMapArray,
      distgridToPackedArrayMap, indexflag, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      array->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return NULL;
    }
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    array->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::Array.", ESMC_CONTEXT, rc);
    array->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    return NULL;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return array;
#undef DEBUGLOG
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
  ArraySpec       *arrayspec,                   // (in)
  DistGrid        *distgrid,                    // (in)
  InterArray<int> *distgridToArrayMap,          // (in)
  InterArray<int> *computationalEdgeLWidthArg,  // (in)
  InterArray<int> *computationalEdgeUWidthArg,  // (in)
  InterArray<int> *computationalLWidthArg,      // (in)
  InterArray<int> *computationalUWidthArg,      // (in)
  InterArray<int> *totalLWidthArg,              // (in)
  InterArray<int> *totalUWidthArg,              // (in)
  ESMC_IndexFlag  *indexflagArg,                // (in)
  ESMC_Pin_Flag   *pinflagArg,                  // (in)
  InterArray<int> *distLBoundArg,               // (in)
  InterArray<int> *undistLBoundArg,             // (in)
  InterArray<int> *undistUBoundArg,             // (in)
  int             *rc,                          // (out) return code
  VM              *vm                           // (in, optional)
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMCI::Array} object from ArraySpec and DistGrid.
//EOPI
//-----------------------------------------------------------------------------
#undef DEBUGLOG
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  Array *array;
  try{

  // check the input and get the information together to call construct()
  // arrayspec -> typekind/rank
  if (arrayspec == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to arrayspec", ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  ESMC_TypeKind_Flag typekind = arrayspec->getTypeKind(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMC_NULL_POINTER;
  int rank = arrayspec->getRank(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return ESMC_NULL_POINTER;
  // distgrid -> delayout, dimCount
  if (distgrid == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to distgrid", ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  const DELayout *delayout = distgrid->getDELayout();
#if 0
  {
    std::stringstream debugmsg;
    debugmsg << "Array::create(): DELayout" << delayout << " localDeCount=" 
      << delayout->getLocalDeCount() << " localDeToDeMap()=" 
      << delayout->getLocalDeToDeMap() << " : " 
      << *delayout->getLocalDeToDeMap();
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  int dimCount = distgrid->getDimCount();
  // check if distgridToArrayMap was provided and matches rest of arguments
  vector<int> distgridToArrayMapArrayV(dimCount);
  int *distgridToArrayMapArray = &distgridToArrayMapArrayV[0];
  for (int i=0; i<dimCount; i++){
    if (i < rank)
      distgridToArrayMapArray[i] = i+1; // default (basis 1)
    else
      distgridToArrayMapArray[i] = 0;   // default (replicator dims beyond rank)
  }
  if (present(distgridToArrayMap)){
    if (distgridToArrayMap->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "distgridToArrayMap array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (distgridToArrayMap->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "distgridToArrayMap and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(distgridToArrayMapArray, distgridToArrayMap->array,
      dimCount*sizeof(int));
  }
  {
    // check distgridToArrayMapArray
    vector<bool> check(rank);
    for (int i=0; i<rank; i++)
      check[i] = false; // initialize
    for (int i=0; i<dimCount; i++){
      if (distgridToArrayMapArray[i] < 0 || distgridToArrayMapArray[i] > rank){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "invalid distgridToArrayMap element", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (distgridToArrayMapArray[i] > 0){
        if(check[distgridToArrayMapArray[i]-1]){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "invalid distgridToArrayMap element", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
        check[distgridToArrayMapArray[i]-1] = true;
      }
    }
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
#ifdef DEBUGLOG
  {
    std::stringstream debugmsg;
    debugmsg << "rank=" << rank << " dimCount=" << dimCount
      << " redDimCount=" << redDimCount << " tensorCount=" << tensorCount;
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  // generate arrayToDistGridMap
  vector<int> arrayToDistGridMapArrayV(rank);
  int *arrayToDistGridMapArray = &arrayToDistGridMapArrayV[0];
  for (int i=0; i<rank; i++)
    arrayToDistGridMapArray[i] = 0; // reset  (basis 1), 0 indicates tensor dim
  for (int i=0; i<dimCount; i++)
    if (int j=distgridToArrayMapArray[i])
      arrayToDistGridMapArray[j-1] = i+1;
  // generate distgridToPackedArrayMap - labels the distributed Array dims 1,2,.
  vector<int> distgridToPackedArrayMapV(dimCount);
  int *distgridToPackedArrayMap = &distgridToPackedArrayMapV[0];
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
  if (tensorCount > 0 && !present(undistLBoundArg)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Valid undistLBound argument required to create Array with tensor dims",
      ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  if (tensorCount > 0 && !present(undistUBoundArg)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Valid undistUBound argument required to create Array with tensor dims",
      ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }
  int *undistLBoundArray = NULL; // reset
  if (present(undistLBoundArg)){
    if (undistLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "undistLBound array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (undistLBoundArg->extent[0] != tensorCount){
      std::stringstream msg;
      msg << "undistLBound, arrayspec, distgrid mismatch, "
        "undistLBoundArg->extent[0]=" << undistLBoundArg->extent[0];
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE, msg, ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    undistLBoundArray = undistLBoundArg->array;
  }
  int *undistUBoundArray = NULL; // reset
  if (present(undistUBoundArg)){
    if (undistUBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "undistUBound array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (undistUBoundArg->extent[0] != tensorCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "undistUBound, arrayspec, distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    undistUBoundArray = undistUBoundArg->array;
  }
  // tensorElementCount
  int tensorElementCount = 1;  // prime tensorElementCount
  for (int i=0; i<tensorCount; i++)
    tensorElementCount *= (undistUBoundArray[i] - undistLBoundArray[i] + 1);

  // delayout -> deCount, localDeCount, localDeToDeMap
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();
  const int *localDeToDeMap = delayout->getLocalDeToDeMap();
  // distgrid -> indexCountPDimPDe[]
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  // check on indexflag
  ESMC_IndexFlag indexflag = ESMC_INDEX_DELOCAL;  // default
  if (indexflagArg != NULL)
    indexflag = *indexflagArg;
  // check that presence of distLBoundArg is consistent with indexflag
  if(indexflag == ESMC_INDEX_USER){
    if (!present(distLBoundArg)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "distLBoundArg required in ESMC_INDEX_USER mode", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
  }else{
    if (present(distLBoundArg)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "distLBoundArg must only be specified in ESMC_INDEX_USER mode",
        ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
  }
  // figure exclusive region
  vector<int> exclusiveLBoundV(redDimCount*localDeCount);
  vector<int> exclusiveUBoundV(redDimCount*localDeCount);
  int *exclusiveLBound = NULL;  // default safe guard
  int *exclusiveUBound = NULL;  // default safe guard
  if (redDimCount*localDeCount > 0){
    exclusiveLBound = &exclusiveLBoundV[0];
    exclusiveUBound = &exclusiveUBoundV[0];
  }
  for (int i=0; i<redDimCount*localDeCount; i++)
    exclusiveLBound[i] = 1; // excl. region starts at (1,1,1...) <- Fortran
  // exlc. region for each DE ends at indexCountPDimPDe of the associated
  // DistGrid
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    for (int j=0; j<dimCount; j++)
      if (int k=distgridToPackedArrayMap[j])
        exclusiveUBound[i*redDimCount+k-1] = indexCountPDimPDe[de*dimCount+j];
  }
  // optionally shift origin of exclusive region to pseudo global index space
  if (indexflag == ESMC_INDEX_GLOBAL){
    for (int i=0; i<localDeCount; i++){
      int de = localDeToDeMap[i];
      for (int j=0; j<dimCount; j++){
        // check that this DE/dim has a contiguous index list
        const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
        if (!contigFlagPDimPDe[de*dimCount+j]){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
            "Cannot use non-contiguous decomposition for pseudo global"
            " index space", ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
        // obtain indexList for this DE and dim
        const int *indexList =
          distgrid->getIndexListPDimPLocalDe(i, j+1, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
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
  vector<int> computationalEdgeLWidthV(redDimCount);
  vector<int> computationalEdgeUWidthV(redDimCount);
  int *computationalEdgeLWidth = NULL;  // default safe guard
  int *computationalEdgeUWidth = NULL;  // default safe guard
  if (redDimCount){
    computationalEdgeLWidth = &computationalEdgeLWidthV[0];
    computationalEdgeUWidth = &computationalEdgeUWidthV[0];
  }
  if (present(computationalEdgeLWidthArg)){
    if (computationalEdgeLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalEdgeLWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalEdgeLWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    memcpy(computationalEdgeLWidth, computationalEdgeLWidthArg->array,
      redDimCount*sizeof(int));
  }else{
    // set default
    for (int i=0; i<redDimCount; i++)
      computationalEdgeLWidth[i] = 0;
  }
  if (present(computationalEdgeUWidthArg)){
    if (computationalEdgeUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalEdgeUWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalEdgeUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalEdgeUWidth and distgrid mismatch", ESMC_CONTEXT, rc);
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
  vector<int> computationalLBoundV(redDimCount*localDeCount);
  vector<int> computationalUBoundV(redDimCount*localDeCount);
  int *computationalLBound = NULL;  // default safe guard
  int *computationalUBound = NULL;  // default safe guard
  if (redDimCount*localDeCount > 0){
    computationalLBound = &computationalLBoundV[0];
    computationalUBound = &computationalUBoundV[0];
  }
  if (present(computationalLWidthArg)){
    if (computationalLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalLWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalLWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalLBound[j*redDimCount+i] = exclusiveLBound[j*redDimCount+i]
          - computationalLWidthArg->array[i];
  }else{
    // set default
    if (localDeCount*redDimCount > 0)
      memcpy(computationalLBound, exclusiveLBound,
        localDeCount*redDimCount*sizeof(int));
  }
  if (present(computationalUWidthArg)){
    if (computationalUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalUWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (computationalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "computationalUWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int j=0; j<localDeCount; j++)
      for (int i=0; i<redDimCount; i++)
        computationalUBound[j*redDimCount+i] = exclusiveUBound[j*redDimCount+i]
          + computationalUWidthArg->array[i];
  }else{
    // set default
    if (localDeCount*redDimCount > 0)
      memcpy(computationalUBound, exclusiveUBound,
        localDeCount*redDimCount*sizeof(int));
  }
  // modify computational bounds on tile edges
  for (int j=0; j<localDeCount; j++){
    for (int i=0; i<dimCount; i++){
      if (int k=distgridToPackedArrayMap[i]){
        if (computationalEdgeLWidth[k-1]){
          bool onEdgeL = distgrid->isLocalDeOnEdgeL(j, i+1, &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
          if (onEdgeL)
            computationalLBound[j*redDimCount+k-1]
              -= computationalEdgeLWidth[k-1];
        }
        if (computationalEdgeUWidth[k-1]){
          bool onEdgeU = distgrid->isLocalDeOnEdgeU(j, i+1, &localrc);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
          if (onEdgeU)
            computationalUBound[j*redDimCount+k-1]
              += computationalEdgeUWidth[k-1];
        }
      }
    }
  }
  // deal with total widths
  vector<int> totalLBoundV(redDimCount*localDeCount);
  vector<int> totalUBoundV(redDimCount*localDeCount);
  int *totalLBound = NULL;  // default safe guard
  int *totalUBound = NULL;  // default safe guard
  if (redDimCount*localDeCount > 0){
    totalLBound = &totalLBoundV[0];
    totalUBound = &totalUBoundV[0];
  }
  if (present(totalLWidthArg)){
    if (totalLWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "totalLWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (totalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "totalLWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalLWidthArg->array[i] < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "totalLWidth may only contain positive values", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalLBound[j*redDimCount+i] = exclusiveLBound[j*redDimCount+i]
          - totalLWidthArg->array[i];
      }
    }
  }else{
    // set default
    for (int i=0; i<localDeCount*redDimCount; i++)
      totalLBound[i] = computationalLBound[i];
  }
  if (present(totalUWidthArg)){
    if (totalUWidthArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "totalUWidth array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (totalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "totalUWidth and distgrid mismatch", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    for (int i=0; i<redDimCount; i++){
      if (totalUWidthArg->array[i] < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "totalUWidth may only contain positive values", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      for (int j=0; j<localDeCount; j++){
        totalUBound[j*redDimCount+i] = exclusiveUBound[j*redDimCount+i]
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaLBound / totalLBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalLBound[i] > totalUBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaLBound / totalUBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] < totalLBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationalUBound / totalLBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (computationalUBound[i] > totalUBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationalUBound / totalUBound mismatch", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
    }
  }

  // optionally shift to supplied lower bounds
  if (present(distLBoundArg)){
    if (distLBoundArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "distLBoundArg array must be of rank 1", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
    if (distLBoundArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "distLBoundArg and distgrid mismatch", ESMC_CONTEXT, rc);
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
  
  // prepare for pinflag specific handling
  vector<LocalArray *> larrayListV;
  int ssiLocalDeCountArg = localDeCount;  // default
  vector<int> localDeToDeMapArgV;
  int *localDeToDeMapArg = NULL;          // default: use map from DELayout
  VM::memhandle *mh = NULL;               // default: no memory sharing
  
  // branch on pinflag
  ESMC_Pin_Flag pinflag = ESMF_PIN_DE_TO_PET; // default
  if (pinflagArg) pinflag = *pinflagArg;
  if (pinflag == ESMF_PIN_DE_TO_PET){
    // regular case where each DE is only accessible from the local PET
    vector<int> temp_counts(rank);
    vector<int> temp_larrayLBound(rank);
    vector<int> temp_larrayUBound(rank);
    larrayListV.resize(localDeCount);
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
      larrayListV[i] = LocalArray::create(typekind, rank, &temp_counts[0],
        &temp_larrayLBound[0], &temp_larrayUBound[0], NULL, DATACOPY_NONE,
        &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
    }
  }else if (pinflag == ESMF_PIN_DE_TO_SSI || 
    pinflag == ESMF_PIN_DE_TO_SSI_CONTIG){
    // make DEs accessible from all the PETs that are on the same SSI
    vector<int> temp_counts(rank);
    vector<int> temp_larrayLBound(rank);
    vector<int> temp_larrayUBound(rank);
    VM *cvm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
    // allocate temporary memhandle to share information across PETs
    vector<unsigned long> bytes(1); // only a single segment on each PET
    int intCount = 2 + localDeCount + 6*redDimCount*localDeCount;
    bytes[0] = intCount*sizeof(int); // size of shared info
    VM::memhandle mhTemp;
    localrc = cvm->ssishmAllocate(bytes, &mhTemp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
    int mhLocalPet = cvm->ssishmGetLocalPet(mhTemp);
    int mhLocalPetCount = cvm->ssishmGetLocalPetCount(mhTemp);
    // access all of the mhTemp allocations
    vector<int*> info(mhLocalPetCount);
    vector<void*> mems;
    for (int i=0; i<mhLocalPetCount; i++){
      localrc = cvm->ssishmGetMems(mhTemp, i, &mems);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      info[i] = (int *)mems[0];
    }
    // fill local information into info allocation
    info[mhLocalPet][0] = cvm->getLocalPet(); // localPet index in the full VM
    info[mhLocalPet][1] = localDeCount;       // number of DEs on localPet
    int *infoP = info[mhLocalPet] + 2;
    memcpy(infoP, localDeToDeMap, localDeCount*sizeof(int));
    infoP += localDeCount;
    memcpy(infoP, exclusiveLBound, redDimCount*localDeCount*sizeof(int));
    infoP += redDimCount*localDeCount;
    memcpy(infoP, exclusiveUBound, redDimCount*localDeCount*sizeof(int));
    infoP += redDimCount*localDeCount;
    memcpy(infoP, computationalLBound, redDimCount*localDeCount*sizeof(int));
    infoP += redDimCount*localDeCount;
    memcpy(infoP, computationalUBound, redDimCount*localDeCount*sizeof(int));
    infoP += redDimCount*localDeCount;
    memcpy(infoP, totalLBound, redDimCount*localDeCount*sizeof(int));
    infoP += redDimCount*localDeCount;
    memcpy(infoP, totalUBound, redDimCount*localDeCount*sizeof(int));
    // synchronize PETs across memhandle
    localrc = cvm->ssishmSync(mhTemp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
    // determine ssiLocalDeCountArg and prepare mhLocalPetList
    vector<int> mhLocalPetList(mhLocalPetCount);
    mhLocalPetList[0] = mhLocalPet; // localPet in first position
    int ii=1;
    for (int i=0; i<mhLocalPetCount; i++){
      if (i==mhLocalPet) continue;  // skip over localPet
      mhLocalPetList[ii++] = i; // store this pet in the list
      ssiLocalDeCountArg += info[i][1];   // add localDeCount of the other PET
    }
    // resize vectors to fit complete ssiLocalDeCount
    localDeToDeMapArgV.resize(ssiLocalDeCountArg);
    localDeToDeMapArg = &localDeToDeMapArgV[0];
    exclusiveLBoundV.resize(redDimCount*ssiLocalDeCountArg);
    exclusiveUBoundV.resize(redDimCount*ssiLocalDeCountArg);
    exclusiveLBound = &exclusiveLBoundV[0];
    exclusiveUBound = &exclusiveUBoundV[0];
    computationalLBoundV.resize(redDimCount*ssiLocalDeCountArg);
    computationalUBoundV.resize(redDimCount*ssiLocalDeCountArg);
    computationalLBound = &computationalLBoundV[0];
    computationalUBound = &computationalUBoundV[0];
    totalLBoundV.resize(redDimCount*ssiLocalDeCountArg);
    totalUBoundV.resize(redDimCount*ssiLocalDeCountArg);
    totalLBound = &totalLBoundV[0];
    totalUBound = &totalUBoundV[0];
    // fill resized vectors with information shared across mhTemp
    int *localDeToDeMapP = localDeToDeMapArg;
    int *exclusiveLBoundP = exclusiveLBound;
    int *exclusiveUBoundP = exclusiveUBound;
    int *computationalLBoundP = computationalLBound;
    int *computationalUBoundP = computationalUBound;
    int *totalLBoundP = totalLBound;
    int *totalUBoundP = totalUBound;
    for (int i=0; i<mhLocalPetCount; i++){
      ii = mhLocalPetList[i]; // this list has localPet first
      int thisLocalDeCount = info[ii][1];
      infoP = info[ii] + 2;
      memcpy(localDeToDeMapP, infoP, thisLocalDeCount*sizeof(int));
      localDeToDeMapP += thisLocalDeCount;
      infoP += thisLocalDeCount;
      memcpy(exclusiveLBoundP, infoP, redDimCount*thisLocalDeCount*sizeof(int));
      exclusiveLBoundP += redDimCount*thisLocalDeCount;
      infoP += redDimCount*thisLocalDeCount;
      memcpy(exclusiveUBoundP, infoP, redDimCount*thisLocalDeCount*sizeof(int));
      exclusiveUBoundP += redDimCount*thisLocalDeCount;
      infoP += redDimCount*thisLocalDeCount;
      memcpy(computationalLBoundP, infoP, redDimCount*thisLocalDeCount*sizeof(int));
      computationalLBoundP += redDimCount*thisLocalDeCount;
      infoP += redDimCount*thisLocalDeCount;
      memcpy(computationalUBoundP, infoP, redDimCount*thisLocalDeCount*sizeof(int));
      computationalUBoundP += redDimCount*thisLocalDeCount;
      infoP += redDimCount*thisLocalDeCount;
      memcpy(totalLBoundP, infoP, redDimCount*thisLocalDeCount*sizeof(int));
      totalLBoundP += redDimCount*thisLocalDeCount;
      infoP += redDimCount*thisLocalDeCount;
      memcpy(totalUBoundP, infoP, redDimCount*thisLocalDeCount*sizeof(int));
      totalUBoundP += redDimCount*thisLocalDeCount;
    }
    // determine the size of all localDE allocations
    bytes.resize(localDeCount);
    for (int i=0; i<localDeCount; i++){
      int j=0;    // reset distributed index
      int jjj=0;  // reset undistributed index
      unsigned long size = 1;
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMapArray[jj]){
          // distributed dimension
          size *=
            totalUBound[i*redDimCount+j] - totalLBound[i*redDimCount+j] + 1;
          ++j;
        }else{
          // non-distributed dimension
          size *= undistUBoundArray[jjj] - undistLBoundArray[jjj] + 1;
          ++jjj;
        }
      }
      bytes[i] = size * ESMC_TypeKind_FlagSize(typekind);
    }
    // use the VM ssishm interface to allocate memory for all localDEs
    mh = new VM::memhandle;
    bool contigFlag = false;  //default
    if (pinflag == ESMF_PIN_DE_TO_SSI_CONTIG){
      // contiguous memory allocation over each SSI requested
      contigFlag = true;
    }
    localrc = cvm->ssishmAllocate(bytes, mh, contigFlag);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
    // loop over all memhandle local PETs and construct LocalArrays for localDEs
    larrayListV.resize(ssiLocalDeCountArg); // room for all shared PETs localDEs
    int k=0;
    for (int i=0; i<mhLocalPetCount; i++){
      ii = mhLocalPetList[i]; // this list has localPet first
      // access the memory allocations of this PETs localDEs
      localrc = cvm->ssishmGetMems(*mh, ii, &mems);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      // construct the LocalArrays for all the localDEs
      int thisLocalDeCount = info[ii][1];
      for (int lde=0; lde<thisLocalDeCount; lde++){
        int j=0;    // reset distributed index
        int jjj=0;  // reset undistributed index
        for (int jj=0; jj<rank; jj++){
          if (arrayToDistGridMapArray[jj]){
            // distributed dimension
            temp_counts[jj] =
              totalUBound[k*redDimCount+j] - totalLBound[k*redDimCount+j] + 1;
            temp_larrayLBound[jj] = totalLBound[k*redDimCount+j];
            temp_larrayUBound[jj] = totalUBound[k*redDimCount+j];
            ++j;
          }else{
            // non-distributed dimension
            temp_counts[jj] = undistUBoundArray[jjj] - undistLBoundArray[jjj]
              + 1;
            temp_larrayLBound[jj] = undistLBoundArray[jjj];
            temp_larrayUBound[jj] = undistUBoundArray[jjj];
            ++jjj;
          }
        }
        // allocate LocalArray object with specific undist bounds
        larrayListV[k++] = LocalArray::create(typekind, rank, &temp_counts[0],
          &temp_larrayLBound[0], &temp_larrayUBound[0], mems[lde],
          DATACOPY_REFERENCE, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      }
    }
    // done sharing across mhTemp
    localrc = cvm->ssishmFree(&mhTemp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
  }else{
    // no other pinning option yet implemented
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
      "Requested pinflag not yet implemented.", ESMC_CONTEXT, rc);
    return ESMC_NULL_POINTER;
  }

  // call class constructor
  try{
    int vasLocalDeCountArg = localDeCount;
    LocalArray **larrayList = NULL; // default safe guard
    if (larrayListV.size())
      larrayList = &larrayListV[0];
    array = new Array(typekind, rank, larrayList, mh, vasLocalDeCountArg,
      ssiLocalDeCountArg, localDeToDeMapArg, distgrid, false,
      exclusiveLBound, exclusiveUBound, computationalLBound,
      computationalUBound, totalLBound, totalUBound, tensorCount,
      tensorElementCount, undistLBoundArray, undistUBoundArray,
      distgridToArrayMapArray, arrayToDistGridMapArray,
      distgridToPackedArrayMap, indexflag, &localrc, vm);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      array->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return NULL;
    }
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    array->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new ESMCI::Array.", ESMC_CONTEXT, rc);
    array->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,\
      rc);
    return NULL;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return array;
#undef DEBUGLOG
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
  Array         *arrayIn,                     // (in) Array to copy
  DataCopyFlag  copyflag,                     // (in)
  DELayout      *delayout,                    // (in)
  int           rmLeadingTensors,             // (in) leading tensors to remove
  int           *rc                           // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMCI::Array} object as copy of an existing
//    {\tt ESMCI::Array} object.
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
    }catch(int catchrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return NULL;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Array.", ESMC_CONTEXT, rc);
      return NULL;
    }
    // copy all scalar members and reference members
    ESMC_TypeKind_Flag typekind =
      arrayOut->typekind = arrayIn->typekind;
    int rank =
      arrayOut->rank = arrayIn->rank - rmLeadingTensors;
    arrayOut->indexflag = arrayIn->indexflag;
    int tensorCount =
      arrayOut->tensorCount = arrayIn->tensorCount - rmLeadingTensors;
    arrayOut->vasLocalDeCount = arrayIn->vasLocalDeCount;
    if (copyflag == DATACOPY_REFERENCE){
      // sharing reference means also sharing memhandle
      arrayOut->mh = arrayIn->mh;
      arrayOut->mhCreator = false;  // do not transfer ownership
      // shared DEs are supported
      arrayOut->ssiLocalDeCount = arrayIn->ssiLocalDeCount;
    }else{
      // shared DEs are not supported, only copy local DEs
      arrayOut->ssiLocalDeCount = arrayIn->localDeCountAux;
    }
    int ssiLocalDeCount = arrayOut->ssiLocalDeCount;
    // deal with DistGrid and DELayout
    if (delayout){
      if (copyflag!=DATACOPY_REFERENCE){
        // currently only support supply of delayout for DATACOPY_REFERENCE
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "Change in DELayout only supported under DATACOPY_REFERENCE option.",
          ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      arrayOut->distgrid = DistGrid::create(arrayIn->distgrid, NULL, NULL,
        NULL, NULL, false, delayout);
      arrayOut->distgridCreator = true;       // locally created object
      arrayOut->delayout = delayout;
      if ((delayout->getLocalDeCount() > arrayOut->ssiLocalDeCount) ||
        (delayout->getSsiLocalDeCount() > arrayOut->ssiLocalDeCount)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "Cannot move the number of desired DEs under this PET. "
          "Maybe wrong pinflag?", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      //TODO: need to use DELayout's localDeCount, because its localDeToDeMap
      //TODO: only covers localDeCount elements. Although ssiLocalDeCount is
      //TODO: correctly determined in the DELayout level, the localDeToDeMap
      //TODO: is NOT!!!
      ssiLocalDeCount = arrayOut->ssiLocalDeCount
//        = delayout->getSsiLocalDeCount(); //TODO: use this once working
        = delayout->getLocalDeCount();
      arrayOut->localDeToDeMap = new int[ssiLocalDeCount];
      memcpy(arrayOut->localDeToDeMap, delayout->getLocalDeToDeMap(),
        ssiLocalDeCount*sizeof(int));
    }else{
      arrayOut->distgrid = arrayIn->distgrid; // copy reference
      arrayOut->distgridCreator = false;      // not a locally created object
      arrayOut->delayout = arrayIn->delayout; // copy reference
      arrayOut->localDeToDeMap = new int[ssiLocalDeCount];
      memcpy(arrayOut->localDeToDeMap, arrayIn->localDeToDeMap,
        ssiLocalDeCount*sizeof(int));
    }
    // determine leading tensor elements
    int leadingTensorElementCount = 0;
    if (rmLeadingTensors){
      leadingTensorElementCount = 1;
      for (int i=0; i<rmLeadingTensors; i++)
        leadingTensorElementCount *=
          arrayIn->undistUBound[i] - arrayIn->undistLBound[i] + 1;
    }
    arrayOut->tensorElementCount =
      arrayIn->tensorElementCount - leadingTensorElementCount;
    if (arrayOut->tensorElementCount==0) arrayOut->tensorElementCount=1;
    // copy the PET-local LocalArray pointers
    arrayOut->larrayList = new LocalArray*[ssiLocalDeCount];
    int *i2jMap = NULL;
    if (rmLeadingTensors==0){
      // use the src larrayList as a template for the new larrayList
      i2jMap = new int[ssiLocalDeCount];
      for (int i=0; i<ssiLocalDeCount; i++){
        int j=i;
        if (delayout){
          int de = arrayOut->localDeToDeMap[i];
          // must assume reordering of localDE -> DE mapping
          for (j=0; j<arrayIn->ssiLocalDeCount; j++){
            if (arrayIn->localDeToDeMap[j]==de) break;
          }
        }
        i2jMap[i]=j;
        arrayOut->larrayList[i] =
          LocalArray::create(arrayIn->larrayList[j], copyflag, NULL, NULL,
            &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)){
          arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
      }
    }else{
      // create new LocalArray allocation with leading tensor dims removed
      if (copyflag != DATACOPY_ALLOC){
        // inconsistentcy detected
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "Only the DATACOPY_ALLOC option supported when removing "
          "leading tensor dims.", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      for (int i=0; i<ssiLocalDeCount; i++){
        const int *temp_counts = arrayIn->larrayList[i]->getCounts();
        arrayOut->larrayList[i] =
          LocalArray::create(typekind, rank, &(temp_counts[rmLeadingTensors]),
            NULL, NULL, NULL, DATACOPY_NONE, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)){
          arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
      }
    }
    // determine the base addresses of the local arrays:
    arrayOut->larrayBaseAddrList = new void*[ssiLocalDeCount];
    for (int i=0; i<ssiLocalDeCount; i++)
      arrayOut->larrayBaseAddrList[i] = arrayOut->larrayList[i]->getBaseAddr();
    // copy the PET-local bound arrays
    int redDimCount = rank - tensorCount;
    arrayOut->exclusiveLBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->exclusiveUBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->computationalLBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->computationalUBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->totalLBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->totalUBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->contiguousFlag = new int[ssiLocalDeCount];
    arrayOut->totalElementCountPLocalDe = new int[ssiLocalDeCount];
    if (delayout){
      // consider reshuffle of localDEs
      for (int i=0; i<ssiLocalDeCount; i++){
        int j = i2jMap[i];
        memcpy(arrayOut->exclusiveLBound+i*redDimCount,
          arrayIn->exclusiveLBound+j*redDimCount,
          redDimCount*sizeof(int));
        memcpy(arrayOut->exclusiveUBound+i*redDimCount,
          arrayIn->exclusiveUBound+j*redDimCount,
          redDimCount*sizeof(int));
        memcpy(arrayOut->computationalLBound+i*redDimCount,
          arrayIn->computationalLBound+j*redDimCount,
          redDimCount*sizeof(int));
        memcpy(arrayOut->computationalUBound+i*redDimCount,
          arrayIn->computationalUBound+j*redDimCount,
          redDimCount*sizeof(int));
        memcpy(arrayOut->totalLBound+i*redDimCount,
          arrayIn->totalLBound+j*redDimCount,
          redDimCount*sizeof(int));
        memcpy(arrayOut->totalUBound+i*redDimCount,
          arrayIn->totalUBound+j*redDimCount,
          redDimCount*sizeof(int));
        arrayOut->contiguousFlag[i] = arrayIn->contiguousFlag[j];
        arrayOut->totalElementCountPLocalDe[i]
          = arrayIn->totalElementCountPLocalDe[j];
      }
    }else{
      memcpy(arrayOut->exclusiveLBound, arrayIn->exclusiveLBound,
        redDimCount*ssiLocalDeCount*sizeof(int));
      memcpy(arrayOut->exclusiveUBound, arrayIn->exclusiveUBound,
        redDimCount*ssiLocalDeCount*sizeof(int));
      memcpy(arrayOut->computationalLBound, arrayIn->computationalLBound,
        redDimCount*ssiLocalDeCount*sizeof(int));
      memcpy(arrayOut->computationalUBound, arrayIn->computationalUBound,
        redDimCount*ssiLocalDeCount*sizeof(int));
      memcpy(arrayOut->totalLBound, arrayIn->totalLBound,
        redDimCount*ssiLocalDeCount*sizeof(int));
      memcpy(arrayOut->totalUBound, arrayIn->totalUBound,
        redDimCount*ssiLocalDeCount*sizeof(int));
      memcpy(arrayOut->contiguousFlag, arrayIn->contiguousFlag,
        ssiLocalDeCount * sizeof(int));
      memcpy(arrayOut->totalElementCountPLocalDe,
        arrayIn->totalElementCountPLocalDe, ssiLocalDeCount * sizeof(int));
    }
    // tensor dimensions
    arrayOut->undistLBound = new int[tensorCount];
    memcpy(arrayOut->undistLBound, arrayIn->undistLBound + rmLeadingTensors,
      tensorCount * sizeof(int));
    arrayOut->undistUBound = new int[tensorCount];
    memcpy(arrayOut->undistUBound, arrayIn->undistUBound + rmLeadingTensors,
      tensorCount * sizeof(int));
    // distgridToArrayMap, arrayToDistGridMap and distgridToPackedArrayMap
    int dimCount = arrayIn->distgrid->getDimCount();
    arrayOut->distgridToArrayMap = new int[dimCount];
    memcpy(arrayOut->distgridToArrayMap, arrayIn->distgridToArrayMap,
      dimCount * sizeof(int));
    if (rmLeadingTensors)
      for (int i=0; i<dimCount; i++)
        arrayOut->distgridToArrayMap[i] -= rmLeadingTensors;
    arrayOut->arrayToDistGridMap = new int[rank];
    memcpy(arrayOut->arrayToDistGridMap,
      arrayIn->arrayToDistGridMap + rmLeadingTensors,
      rank * sizeof(int));
    arrayOut->distgridToPackedArrayMap = new int[dimCount];
    memcpy(arrayOut->distgridToPackedArrayMap,
      arrayIn->distgridToPackedArrayMap, dimCount * sizeof(int));
    // exclusiveElementCountPDe
    int deCount = arrayIn->delayout->getDeCount();
    arrayOut->exclusiveElementCountPDe = new int[deCount];
    memcpy(arrayOut->exclusiveElementCountPDe,
      arrayIn->exclusiveElementCountPDe, deCount * sizeof(int));

    // Set up rim members and fill with canonical seqIndex values
    arrayOut->setRimMembers();

    // invalidate the name for this Array object in the Base class
    arrayOut->ESMC_BaseSetName(NULL, "Array");

    arrayOut->localDeCountAux =
      arrayOut->delayout->getLocalDeCount(); // TODO: auxilary for garb
                                             // TODO: until ref. counting
    arrayOut->ioRH = NULL; // invalidate

    if (i2jMap) delete [] i2jMap;

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, rc);
    arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  // reset the distgridCreator flag in the src Array, because the newly
  // created Array will now point to the same DistGrid by reference
  // -> leave it up to ESMF automatic garbage collection to clean up the
  // DistGrid when it is time
  arrayIn->distgridCreator = false; // drop ownership of the referenced DistGrid

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arrayOut;
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
  bool rmTensorFlag,                          // (in) if true remove all tensors
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ESMCI::Array} object as copy of an existing
//    {\tt ESMCI::Array} object.
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
    }catch(int catchrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return NULL;
    }catch(...){
      // allocation error
      ESMC_LogDefault.MsgAllocError("for new ESMCI::Array.", ESMC_CONTEXT, rc);
      return NULL;
    }
    //
    int rmTensors = 0;  // initialize
    if (rmTensorFlag)
      rmTensors = arrayIn->tensorCount;
    // copy all scalar members and reference members
    ESMC_TypeKind_Flag typekind =
      arrayOut->typekind = arrayIn->typekind;
    int rank = arrayOut->rank = arrayIn->rank - rmTensors;
    arrayOut->indexflag = arrayIn->indexflag;
    int tensorCount = arrayOut->tensorCount = arrayIn->tensorCount - rmTensors;
    if (rmTensorFlag)
      arrayOut->tensorElementCount = 1;
    else
      arrayOut->tensorElementCount = arrayIn->tensorElementCount;
    arrayOut->distgrid = arrayIn->distgrid; // copy reference
    arrayOut->distgridCreator = false;      // not a locally created object
    arrayOut->delayout = arrayIn->delayout; // copy reference
    arrayOut->vasLocalDeCount = arrayIn->vasLocalDeCount;
    // shared DEs are not supported under copy behavior, only copy local DEs
    int ssiLocalDeCount = arrayOut->ssiLocalDeCount = arrayIn->localDeCountAux;
    // deep copy of members with allocations
    // copy the PET-local bound arrays
    int redDimCount = rank - tensorCount;
    arrayOut->localDeToDeMap = new int[ssiLocalDeCount];
    arrayOut->exclusiveLBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->exclusiveUBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->computationalLBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->computationalUBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->totalLBound = new int[redDimCount*ssiLocalDeCount];
    arrayOut->totalUBound = new int[redDimCount*ssiLocalDeCount];
    if (ssiLocalDeCount){
      memcpy(arrayOut->localDeToDeMap, arrayIn->localDeToDeMap,
        ssiLocalDeCount*sizeof(int));
      if (redDimCount){
        memcpy(arrayOut->exclusiveLBound, arrayIn->exclusiveLBound,
          redDimCount*ssiLocalDeCount*sizeof(int));
        memcpy(arrayOut->exclusiveUBound, arrayIn->exclusiveUBound,
          redDimCount*ssiLocalDeCount*sizeof(int));
        memcpy(arrayOut->computationalLBound, arrayIn->computationalLBound,
          redDimCount*ssiLocalDeCount*sizeof(int));
        memcpy(arrayOut->computationalUBound, arrayIn->computationalUBound,
          redDimCount*ssiLocalDeCount*sizeof(int));
        memcpy(arrayOut->totalLBound, arrayIn->totalLBound,
          redDimCount*ssiLocalDeCount*sizeof(int));
        memcpy(arrayOut->totalUBound, arrayIn->totalUBound,
          redDimCount*ssiLocalDeCount*sizeof(int));
      }
    }
    // copy the PET-local LocalArray pointers
    arrayOut->larrayList = new LocalArray*[ssiLocalDeCount];
    if (rmTensorFlag){
      // remove the tensor dimensions from the allocation
      for (int i=0; i<ssiLocalDeCount; i++){
        vector<int> counts;
        for (int k=0; k<redDimCount; k++){
          int dimSize = arrayOut->totalUBound[i*redDimCount+k]
            - arrayOut->totalLBound[i*redDimCount+k] + 1;
          counts.push_back(dimSize);
        }
        arrayOut->larrayList[i] =
          LocalArray::create(typekind, rank, &(counts[0]),
            NULL, NULL, NULL, DATACOPY_NONE, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)){
          arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
        // initialize the data in the newly created array to zero
        memset(arrayOut->larrayList[i]->getBaseAddr(), 0,
          arrayOut->larrayList[i]->getByteCount());
      }
    }else{
      // use the src larrayList as a template for the new allocation
      for (int i=0; i<ssiLocalDeCount; i++){
        arrayOut->larrayList[i] =
          LocalArray::create(arrayIn->larrayList[i], DATACOPY_ALLOC, NULL, NULL,
            &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)){
          arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
        // initialize the data in the newly created array to zero
        memset(arrayOut->larrayList[i]->getBaseAddr(), 0,
          arrayOut->larrayList[i]->getByteCount());
      }
    }
    // determine the base addresses of the local arrays:
    arrayOut->larrayBaseAddrList = new void*[ssiLocalDeCount];
    for (int i=0; i<ssiLocalDeCount; i++)
      arrayOut->larrayBaseAddrList[i] = arrayOut->larrayList[i]->getBaseAddr();
    // tensor dimensions
    arrayOut->undistLBound = new int[tensorCount];
    memcpy(arrayOut->undistLBound, arrayIn->undistLBound,
      tensorCount * sizeof(int));
    arrayOut->undistUBound = new int[tensorCount];
    memcpy(arrayOut->undistUBound, arrayIn->undistUBound,
      tensorCount * sizeof(int));
    // distgridToArrayMap, arrayToDistGridMap and distgridToPackedArrayMap
    int dimCount = arrayIn->distgrid->getDimCount();
    arrayOut->distgridToArrayMap = new int[dimCount];
    for (int i=0; i<dimCount; i++)
      arrayOut->distgridToArrayMap[i] = 0;  // initialize
    arrayOut->arrayToDistGridMap = new int[rank];
    int j=0;
    for (int i=0; i<arrayIn->rank; i++){
      if (arrayIn->arrayToDistGridMap[i]){
        arrayOut->arrayToDistGridMap[j] = arrayIn->arrayToDistGridMap[i];
        arrayOut->distgridToArrayMap[arrayIn->arrayToDistGridMap[i]-1] = j+1;
        ++j;
      }
    }
    //TODO: check if the distgridToPackedArrayMap is set up correctly.
    arrayOut->distgridToPackedArrayMap = new int[dimCount];
    memcpy(arrayOut->distgridToPackedArrayMap,
      arrayOut->distgridToArrayMap, dimCount * sizeof(int));
    // contiguous flag
    arrayOut->contiguousFlag = new int[ssiLocalDeCount];
    if (ssiLocalDeCount)
      memcpy(arrayOut->contiguousFlag, arrayIn->contiguousFlag,
        ssiLocalDeCount * sizeof(int));
    // exclusiveElementCountPDe
    int deCount = arrayIn->delayout->getDeCount();
    arrayOut->exclusiveElementCountPDe = new int[deCount];
    if (deCount)
      memcpy(arrayOut->exclusiveElementCountPDe,
        arrayIn->exclusiveElementCountPDe, deCount * sizeof(int));
    // totalElementCountPLocalDe
    arrayOut->totalElementCountPLocalDe = new int[ssiLocalDeCount];
    if (ssiLocalDeCount)
      memcpy(arrayOut->totalElementCountPLocalDe,
        arrayIn->totalElementCountPLocalDe, ssiLocalDeCount * sizeof(int));

    // Set up rim members and fill with canonical seqIndex values
    arrayOut->setRimMembers();

    // invalidate the name for this Array object in the Base class
    arrayOut->ESMC_BaseSetName(NULL, "Array");

    arrayOut->localDeCountAux =
      arrayOut->delayout->getLocalDeCount(); // TODO: auxilary for garb
                                             // TODO: until ref. counting
                                        
    arrayOut->ioRH = NULL; // invalidate

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc);
    arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, rc);
    arrayOut->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return NULL;
  }

  // reset the distgridCreator flag in the src Array, because the newly
  // created Array will now point to the same DistGrid by reference
  // -> leave it up to ESMF automatic garbage collection to clean up the
  // DistGrid when it is time
  arrayIn->distgridCreator = false; // drop ownership of the referenced DistGrid

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
  Array **array,                // in - ESMC_Array to destroy
  bool noGarbage){              // in - remove from garbage collection
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (array == ESMC_NULL_POINTER || *array == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to Array", ESMC_CONTEXT, &rc);
    return rc;
  }

  try{
    // destruct Array object
    (*array)->destruct(true, noGarbage);
    // mark as invalid object
    (*array)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(*array); // remove object from garbage collection
    delete (*array);      // completely delete the object, free heap
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// data copy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::copy()"
//BOPI
// !IROUTINE:  ESMCI::Array::copy
//
// !INTERFACE:
//
int Array::copy(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array const *arrayIn                // (in) Array to copy data from
  ){
//
// !DESCRIPTION:
//    Copy data from one Array object to another
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{

    if (!matchBool(this, arrayIn)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "Arrays must match for data copy", ESMC_CONTEXT, &rc);
      return rc;
    }

    // do the actual data copy
    int const dataSize = ESMC_TypeKind_FlagSize(typekind);
    for (int i=0; i<ssiLocalDeCount; i++){
      int size =
        totalElementCountPLocalDe[i]*tensorElementCount*dataSize;  // bytes
      memcpy(larrayBaseAddrList[i], arrayIn->larrayBaseAddrList[i], size);
    }

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
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
  int const *index                  // in - DE-local index tuple in exclusive
                                    //      region basis 0
  )const{
//
// !DESCRIPTION:
//    Get linear index - assuming index input to be basis 0 in excl. region
//
//EOPI
//-----------------------------------------------------------------------------
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
template<typename IT> int Array::getSequenceIndexExclusive(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE
  int const *index,                 // in  - DE-local index tuple in exclusive
                                    //       region basis 0
  SeqIndex<IT> *seqIndex,           // out - sequence index
  bool recursive,                   // in  - recursive mode or not
  bool canonical                    // in  - return canonical seqIndex even if
                                    //       arbitrary seqIndices available
  )const{
//
// !DESCRIPTION:
//    Get sequential index - assuming index input to be basis 0 in excl. region,
//    but allowing the incoming index to be outside of the exclusive region
//    under special conditions. See the used DistGrid method for details.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check seqIndex argument
  if (seqIndex==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "The seqIndex argument must not be a NULL pointer", ESMC_CONTEXT, &rc);
    return rc;
  }

  // prepare decompIndex for decomposed dimensions in the DistGrid order
  int dimCount = distgrid->getDimCount();
  vector<int> decompIndex(dimCount);
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
  vector<IT> decompSeqIndex;
  localrc = distgrid->getSequenceIndexLocalDe(localDe, &(decompIndex[0]),
    decompSeqIndex, recursive, canonical);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  if (decompSeqIndex.size() > 0)
    seqIndex->decompSeqIndex = decompSeqIndex[0]; // use the first seqIndex
  else
    seqIndex->decompSeqIndex = -1;  // invalidate

  // determine sequentialized index for tensor dimensions
  seqIndex->setTensor(getTensorSequenceIndex(index));

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getSequenceIndexTile()"
//BOPI
// !IROUTINE:  ESMCI::Array::getSequenceIndexTile
//
// !INTERFACE:
template<typename IT> SeqIndex<IT> Array::getSequenceIndexTile(
//
// !RETURN VALUE:
//    SeqIndex sequence index
//
// !ARGUMENTS:
//
  int tile,                        // in - tile = {1,..., tileCount}
  const int *index,                 // in - index tuple within tile
                                    //    - basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index - assuming index input to be basis 0 in tile region
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // initialize seqIndex
  SeqIndex<IT> seqIndex;
  seqIndex.decompSeqIndex = -1;
  seqIndex.setTensor(-1);
  // prepare decompIndex for decomposed dimensions in the DistGrid order
  int dimCount = distgrid->getDimCount();
  int *decompIndex = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    if (distgridToArrayMap[i] > 0){
      // DistGrid dim associated with Array dim
      decompIndex[i] = index[distgridToArrayMap[i]-1];
    }else{
      // DistGrid dim _not_ associated with Array dim
      decompIndex[i] = 0;
      // use smallest seq index for replicated dims
    }
  }
  // determine the sequentialized index for decomposed dimensions
  IT decompSeqIndex;
  localrc = distgrid->getSequenceIndexTileRelative(tile, decompIndex,
    &decompSeqIndex);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return seqIndex;
  seqIndex.decompSeqIndex = decompSeqIndex;

  // garbage collection
  delete [] decompIndex;

  // determine sequentialized index for tensor dimensions
  seqIndex.setTensor(getTensorSequenceIndex(index));

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqIndex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getTensorSequenceIndex()"
//BOPI
// !IROUTINE:  ESMCI::Array::getTensorSequenceIndex
//
// !INTERFACE:
int Array::getTensorSequenceIndex(
//
// !RETURN VALUE:
//    Tensor sequence index
//
// !ARGUMENTS:
//
  const int *index,                 // in - index tuple basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get tensor sequential index - assuming index input to be basis 0
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

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

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return tensorSeqIndex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getArbSequenceIndexOffset()"
//BOPI
// !IROUTINE:  ESMCI::Array::getArbSequenceIndexOffset
//
// !INTERFACE:
int Array::getArbSequenceIndexOffset(
//
// !RETURN VALUE:
//    Arbitrary sequence index offset
//
// !ARGUMENTS:
//
  const int *index,                 // in - index tuple basis 0
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get offset into arb sequential index list - assuming index input to be
// basis 0
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // determine the sequentialized index for tensor dimensions
  int arbSeqIndexOffset = 0;             // reset
  int arbIndex = (rank - tensorCount) - 1;  // reset
  for (int jj=rank-1; jj>=0; jj--){
    if (arrayToDistGridMap[jj]==1){
      // decomposed dimension
      // first time multiply with zero intentionally:
      arbSeqIndexOffset *= totalUBound[arbIndex] - totalLBound[arbIndex] + 1;
      arbSeqIndexOffset += index[jj];
      --arbIndex;
    }
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arbSeqIndexOffset;
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
  InterArray<int> *computationalLWidthArg        // (in)
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
  if (present(computationalLWidthArg)){
    if (computationalLWidthArg->dimCount != 2){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalLWidth array must be of rank 2", ESMC_CONTEXT, &rc);
      return rc;
    }
    int redDimCount = rank - tensorCount;
    if (computationalLWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "1st dim of computationalLWidth argument must be of size redDimCount",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    int localDeCount = delayout->getLocalDeCount();
    if (computationalLWidthArg->extent[1] != localDeCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "2nd dim of computationalLWidth argument must be of size "
        "localDeCount", ESMC_CONTEXT, &rc);
      return rc;
    }
    int *computationalLBoundNew = new int[redDimCount*localDeCount];
    for (int i=0; i<localDeCount*redDimCount; i++){
      computationalLBoundNew[i] = exclusiveLBound[i]
        - computationalLWidthArg->array[i];
      if (computationalLBoundNew[i] < totalLBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaLWidth below totalLBound -> not updated", ESMC_CONTEXT,
          &rc);
        return rc;
      }
      if (computationalLBoundNew[i] > totalUBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaLWidth above totalUBound -> not updated", ESMC_CONTEXT,
          &rc);
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
  InterArray<int> *computationalUWidthArg        // (in)
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
  if (present(computationalUWidthArg)){
    if (computationalUWidthArg->dimCount != 2){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "computationalUWidth array must be of rank 2", ESMC_CONTEXT, &rc);
      return rc;
    }
    int redDimCount = rank - tensorCount;
    if (computationalUWidthArg->extent[0] != redDimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "1st dim of computationalUWidth argument must be of size redDimCount",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    int localDeCount = delayout->getLocalDeCount();
    if (computationalUWidthArg->extent[1] != localDeCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "2nd dim of computationalUWidth argument must be of size "
        "localDeCount", ESMC_CONTEXT, &rc);
      return rc;
    }
    int *computationalLBoundNew = new int[redDimCount*localDeCount];
    for (int i=0; i<localDeCount*redDimCount; i++){
      computationalLBoundNew[i] = exclusiveLBound[i]
        - computationalUWidthArg->array[i];
      if (computationalLBoundNew[i] < totalLBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaUWidth below totalLBound -> not updated", ESMC_CONTEXT,
          &rc);
        return rc;
      }
      if (computationalLBoundNew[i] > totalUBound[i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "computationaUWidth above totalUBound -> not updated", ESMC_CONTEXT,
          &rc);
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
#define ESMC_METHOD "ESMCI::Array::setRimMembers()"
//BOPI
// !IROUTINE:  ESMCI::Array::setRimMembers
//
// !INTERFACE:
void Array::setRimMembers(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//    Set setRimMembers
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{

  // Set up rim members and fill with canonical seqIndex values
  //TODO: rim setup has potentially sizable memory and performance impact???
  int localDeCount = delayout->getLocalDeCount();
  rimElementCount.resize(localDeCount);
  rimLinIndex.resize(localDeCount);
  ESMC_TypeKind_Flag indexTK = distgrid->getIndexTK();
  if (indexTK==ESMC_TYPEKIND_I4){
    rimSeqIndexI4.resize(localDeCount);
    for (int i=0; i<localDeCount; i++){
      // iterator through total region, skipping over exclusive region
      ArrayElement arrayElement(this, i, true, true, true, false);
      int element = 0;
      while(arrayElement.isWithin()){
        // obtain linear index for this element
        int linIndex = arrayElement.getLinearIndex();
        // obtain canonical seqIndex value according to DistGrid topology
        SeqIndex<ESMC_I4> seqIndex;  // invalidated by default constructor
        if (arrayElement.hasValidSeqIndex()){
          // seqIndex is well defined for this arrayElement
          seqIndex = arrayElement.getSequenceIndex<ESMC_I4>();
        }
        rimSeqIndexI4[i].push_back(seqIndex); // store seqIndex for this rim element
        rimLinIndex[i].push_back(linIndex); // store linIndex for this rim element
        arrayElement.next();  // next element
        ++element;
      } // multi dim index loop
      rimElementCount[i] = element; // store element count
    }
  }else if (indexTK==ESMC_TYPEKIND_I8){
    rimSeqIndexI8.resize(localDeCount);
    for (int i=0; i<localDeCount; i++){
      // iterator through total region, skipping over exclusive region
      ArrayElement arrayElement(this, i, true, true, true, false);
      int element = 0;
      while(arrayElement.isWithin()){
        // obtain linear index for this element
        int linIndex = arrayElement.getLinearIndex();
        // obtain canonical seqIndex value according to DistGrid topology
        SeqIndex<ESMC_I8> seqIndex;  // invalidated by default constructor
        if (arrayElement.hasValidSeqIndex()){
          // seqIndex is well defined for this arrayElement
          seqIndex = arrayElement.getSequenceIndex<ESMC_I8>();
        }
        rimSeqIndexI8[i].push_back(seqIndex); // store seqIndex for this rim element
        rimLinIndex[i].push_back(linIndex); // store linIndex for this rim element
        arrayElement.next();  // next element
        ++element;
      } // multi dim index loop
      rimElementCount[i] = element; // store element count
    }
  }

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    throw rc;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, &rc);
    throw rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    throw rc;
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::setRimSeqIndex<ESMC_I4>()"
//BOPI
// !IROUTINE:  ESMCI::Array::setRimSeqIndex
//
// !INTERFACE:
template<>
  int Array::setRimSeqIndex<ESMC_I4>(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int localDe,                          // (in)
  InterArray<ESMC_I4> *rimSeqIndexArg   // (in)
  ){
//
// !DESCRIPTION:
//    Set rimSeqIndex for localDe.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check for matching indexing typekind
  if (distgrid->getIndexTK() != ESMC_TYPEKIND_I4){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
      "mismatch between indexing typekind", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // ensure localDe is in range
  if (localDe < 0 || localDe >= delayout->getLocalDeCount()){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
      "localDe out of range", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // check rimSeqIndexArg input and process
  if (present(rimSeqIndexArg)){
    if (rimSeqIndexArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "rimSeqIndexArg array must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
    if (rimSeqIndexArg->extent[0]*tensorElementCount
      != (int)rimSeqIndexI4[localDe].size()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "rimSeqIndexArg argument must be of size rimSeqIndex[localDe].size()",
        ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }

    // fill Array rim elements with sequence indices provided in rimSeqIndexArg
    ArrayElement arrayElement(this, localDe, true, false, false, false);
    int i=0;
    int offStart = arrayElement.getArbSequenceIndexOffset();
    while(arrayElement.isWithin()){
      int off = arrayElement.getArbSequenceIndexOffset() - offStart;
      rimSeqIndexI4[localDe][i].decompSeqIndex =
        rimSeqIndexArg->array[off];
      rimSeqIndexI4[localDe][i].setTensor(arrayElement.getTensorSequenceIndex());
#if 0
printf("setRimSeqIndex(): %d, %d, %d, (%d, %d), %d\n", i, offStart, off,
  rimSeqIndexI4[localDe][i].decompSeqIndex,
  rimSeqIndexI4[localDe][i].getTensor(),
  rimLinIndex[localDe][i]);
#endif
      ++i;
      arrayElement.next();  // next element
    } // multi dim index loop
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::setRimSeqIndex<ESMC_I8>()"
//BOPI
// !IROUTINE:  ESMCI::Array::setRimSeqIndex
//
// !INTERFACE:
template<>
  int Array::setRimSeqIndex<ESMC_I8>(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int localDe,                          // (in)
  InterArray<ESMC_I8> *rimSeqIndexArg   // (in)
  ){
//
// !DESCRIPTION:
//    Set rimSeqIndex for localDe.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check for matching indexing typekind
  if (distgrid->getIndexTK() != ESMC_TYPEKIND_I8){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
      "mismatch between indexing typekind", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // ensure localDe is in range
  if (localDe < 0 || localDe >= delayout->getLocalDeCount()){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
      "localDe out of range", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // check rimSeqIndexArg input and process
  if (present(rimSeqIndexArg)){
    if (rimSeqIndexArg->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "rimSeqIndexArg array must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
    if (rimSeqIndexArg->extent[0]>0){
      // there are sequence indices passed in
      if (rimSeqIndexArg->extent[0]*tensorElementCount
        != (int)rimSeqIndexI8[localDe].size()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "rimSeqIndexArg argument must be of size rimSeqIndex[localDe].size()",
          ESMC_CONTEXT, &rc);
        return rc;  // bail out
      }

      // fill Array rim elements with seq indices provided in rimSeqIndexArg
      ArrayElement arrayElement(this, localDe, true, false, false, false);
      int i=0;
      int offStart = arrayElement.getArbSequenceIndexOffset();
      while(arrayElement.isWithin()){
        int off = arrayElement.getArbSequenceIndexOffset() - offStart;
        rimSeqIndexI8[localDe][i].decompSeqIndex =
          rimSeqIndexArg->array[off];
        rimSeqIndexI8[localDe][i].setTensor(arrayElement.getTensorSequenceIndex());
#if 0
printf("setRimSeqIndex(): %d, %d, %d, (%lld, %d), %d\n", i, offStart, off,
  rimSeqIndexI8[localDe][i].decompSeqIndex,
  rimSeqIndexI8[localDe][i].getTensor(),
  rimLinIndex[localDe][i]);
#endif
        ++i;
        arrayElement.next();  // next element
      } // multi dim index loop
    }
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getRimSeqIndex<ESMC_I4>()"
//BOPI
// !IROUTINE:  ESMCI::Array::getRimSeqIndex
//
// !INTERFACE:
template<>
  int Array::getRimSeqIndex<ESMC_I4>(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
    const std::vector<std::vector<SeqIndex<ESMC_I4> > > **rimSeqIndex_  // out -
  )const{
//
// !DESCRIPTION:
//    Get rimSeqIndex
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  *rimSeqIndex_ = &rimSeqIndexI4;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::getRimSeqIndex<ESMC_I8>()"
//BOPI
// !IROUTINE:  ESMCI::Array::getRimSeqIndex
//
// !INTERFACE:
template<>
  int Array::getRimSeqIndex<ESMC_I8>(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
    const std::vector<std::vector<SeqIndex<ESMC_I8> > > **rimSeqIndex_  // out -
  )const{
//
// !DESCRIPTION:
//    Get rimSeqIndex
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  *rimSeqIndex_ = &rimSeqIndexI8;

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
#define ESMC_METHOD "ESMCI::Array::isRHCompatible()"
//BOPI
// !IROUTINE:  ESMCI::Array::isRHCompatible
//
// !INTERFACE:
bool Array::isRHCompatible(
//
// !RETURN VALUE:
//    bool according to whether RHCompatible or not
//
// !ARGUMENTS:
//
  Array const *array,     // in - Array to compare to
  int *rc                 // (out) return code
  )const{
//
// !DESCRIPTION:
//    Determine whether a RouteHandle computed for this Array could also be
//    applied to the {\tt array} argument.
//
//EOPI
//-----------------------------------------------------------------------------
#undef DEBUGLOG
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // initialize return value
  bool result = false;  // not RHCompatible

  // return with errors for NULL pointer
  if (array == NULL){
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to Array", ESMC_CONTEXT, rc);
    return result;
  }

  // identical Array pointers are obviously RHCompatible
  if (this == array){
    result = true;
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return result;
  }

  // require typekind match
  if (typekind != array->typekind){
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return result;
  }

  // require DISTGRIDMATCH_INDEXSPACE or higher
  DistGridMatch_Flag dgMatch =
    DistGrid::match(distgrid, array->getDistGrid(), &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return rc;
  if (dgMatch < DISTGRIDMATCH_INDEXSPACE){
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return result;
  }

  // require match of order and memory layout of distributed dimensions
  int const *dgToPackedArrayMap = array->getDistGridToPackedArrayMap();
  int const *exLBound = array->getExclusiveLBound();
  int const *exUBound = array->getExclusiveUBound();
  int const *toLBound = array->getTotalLBound();
  int const *toUBound = array->getTotalUBound();
  for (int i=0; i<distgrid->getDimCount(); i++){
    int dim=distgridToPackedArrayMap[i];
    if (dim != dgToPackedArrayMap[i]){
      // found mismatch in order of distributed dimensions
#ifdef DEBUGLOG
      {
        std::stringstream msg;
        msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
#endif
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return result;
    }
    if (dim > 0){
      --dim;  // switch to base 0 for inside this block
      // not a replicated dimension -> check for memory layout match
      const int redDimCount = rank - tensorCount;
      for (int lde=0; lde<distgrid->getDELayout()->getLocalDeCount(); lde++){
        int diff1 = exclusiveLBound[lde*redDimCount+dim]
          -  totalLBound[lde*redDimCount+dim];
        int diff2 = exLBound[lde*redDimCount+dim]-toLBound[lde*redDimCount+dim];
        if (diff1 != diff2){
          // found mismatch in memory layout of distributed dimension
#ifdef DEBUGLOG
          {
            std::stringstream msg;
            msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
#endif
          if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
          return result;
        }
        diff1 = exclusiveUBound[lde*redDimCount+dim]
          -  totalUBound[lde*redDimCount+dim];
        diff2 = exUBound[lde*redDimCount+dim]-toUBound[lde*redDimCount+dim];
        if (diff1 != diff2){
          // found mismatch in memory layout of distributed dimension
#ifdef DEBUGLOG
          {
            std::stringstream msg;
            msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
#endif
          if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
          return result;
        }
      }
    }
  }

  // return successfully indicating RH compatibility
  result = true;
#ifdef DEBUGLOG
  {
    std::stringstream msg;
    msg << ESMC_METHOD": " << __LINE__ << " return:" << result;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
  return result;
#undef DEBUGLOG
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::matchBool()"
//BOPI
// !IROUTINE:  ESMCI::Array::matchBool
//
// !INTERFACE:
bool Array::matchBool(
//
// !RETURN VALUE:
//    bool according to match
//
// !ARGUMENTS:
//
  Array const *array1,                    // in
  Array const *array2,                    // in
  int *rc                                 // (out) return code
  ){
//
//TODO: Remove or rename according to below. See where it is used and how!
//
//TODO: 1) rename this method to compatible()
//TODO: 2) consider compatible: distributed dims match,
//TODO:    and strictly compatible: distributed and undistributed dims match.
//
// !DESCRIPTION:
//    Determine if array1 and array2 match.
//
//EOPI
//-----------------------------------------------------------------------------
#undef DEBUGLOG
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // initialize return value
  bool matchResult = false;

  // return with errors for NULL pointer
  if (array1 == NULL){
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to Array", ESMC_CONTEXT, rc);
    return matchResult;
  }
  if (array2 == NULL){
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to Array", ESMC_CONTEXT, rc);
    return matchResult;
  }

  // check if Array pointers are identical
  if (array1 == array2){
    // pointers are identical -> nothing more to check
    matchResult = true;
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }

  // check typekind, rank match
  if (array1->typekind != array2->typekind){
    matchResult = false;
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  if (array1->rank != array2->rank){
    matchResult = false;
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }

  // compare the distgrid members
  matchResult = DistGrid::match(array1->getDistGrid(), array2->getDistGrid(),
    &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return rc;
  if (matchResult==false){
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }

  // compare Array members to ensure DE-local tiles are congruent
  if (array1->tensorCount != array2->tensorCount){
    matchResult = false;
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  if (array1->tensorElementCount != array2->tensorElementCount){
    matchResult = false;
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  int localDeCount = array1->getDistGrid()->getDELayout()->getLocalDeCount();
  int *int1 = array1->totalElementCountPLocalDe;
  int *int2 = array2->totalElementCountPLocalDe;
  for (int i=0; i<localDeCount; i++){
    if (int1[i] != int2[i]){
      matchResult = false;
#ifdef DEBUGLOG
      {
        std::stringstream msg;
        msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
#endif
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }

  // return successfully indicating match
  matchResult = true;
#ifdef DEBUGLOG
  {
    std::stringstream msg;
    msg << ESMC_METHOD": " << __LINE__ << " return:" << matchResult;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
  return matchResult;
#undef DEBUGLOG
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::read()"
//BOPI
// !IROUTINE:  ESMCI::Array::read
//
// !INTERFACE:
int Array::read(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  const std::string &file,        // in    - name of file being read
  const std::string &variableName,// in    - optional variable name
  int   *timeslice,               // in    - timeslice option
  ESMC_IOFmt_Flag *iofmt          // in    - I/O format flag
  ){
//
// !DESCRIPTION:
//   Read Array data from file and put it into an ESMF_Array object.
//   For this API to be functional, the environment variable ESMF_PIO
//   should be set to "internal" when the ESMF library is built.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  ESMC_IOFmt_Flag localiofmt;

 // Set optional parameters which are not optional at next layer
  if ((ESMC_IOFmt_Flag *)NULL != iofmt) {
    localiofmt = *iofmt;
  } else {
    localiofmt = ESMF_IOFMT_NETCDF;
  }
  // It is an error to supply a variable name if not in NetCDF mode
  if (ESMF_IOFMT_NETCDF != localiofmt) {
    if (variableName.size() > 0) {
      ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD,
          "Array variable name not allowed in binary mode",
          ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  IO *newIO = IO::create(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  // For here on, we have to be sure to clean up before returning
  localrc = newIO->addArray(this, variableName, NULL, NULL, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
    IO::destroy(&newIO);
    newIO = (IO *) NULL;
    return rc;
  }

  // Call the IO read function
  localrc = newIO->read(file, localiofmt, timeslice);
  ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc);

  // cleanup
  IO::destroy(&newIO);
  newIO = (IO *)NULL;

  // return
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::write()"
//BOPI
// !IROUTINE:  ESMCI::Array::write
//
// !INTERFACE:
int Array::write(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  const std::string &file,        // in    - name of file being written
  const std::string &variableName,// in    - optional variable name
  const std::string &convention,  // in    - optional Attribute package
  const std::string &purpose,     // in    - optional Attribute package
  bool  *overwrite,               // in    - OK to overwrite file data
  ESMC_FileStatus_Flag *status,   // in    - file status flag
  int   *timeslice,               // in    - timeslice option
  ESMC_IOFmt_Flag *iofmt          // in    - I/O format flag
  ){
//
// !DESCRIPTION:
//   Write Array data into a file. For this API to be functional, the
//   environment variable {\tt ESMF\_PIO} should be set to "internal" when
//   the ESMF library is built.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  ESMC_IOFmt_Flag localiofmt;             // For default handling
  bool localoverwrite;                    // For default handling
  ESMC_FileStatus_Flag localstatus;       // For default handling
  int localrc;

  // Handle format default
  if ((ESMC_IOFmt_Flag *)NULL == iofmt) {
    localiofmt = ESMF_IOFMT_NETCDF;
  } else {
    localiofmt = *iofmt;
  }
  // Handle overwrite default
  if ((bool *)NULL == overwrite) {
    localoverwrite = false;
  } else {
    localoverwrite = *overwrite;
  }
  // Handle status default
  if ((ESMC_FileStatus_Flag *)NULL == status) {
    localstatus = ESMC_FILESTATUS_UNKNOWN;
  } else {
    localstatus = *status;
  }

  // It is an error to supply a variable name in binary mode
  if (ESMF_IOFMT_BIN == localiofmt) {
    if (variableName.size() > 0) {
      ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD,
          "NetCDF variable name not allowed in binary mode",
          ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // It is an error to supply Attribute convention in binary mode
  if (ESMF_IOFMT_BIN == localiofmt) {
    if (convention.size() > 0) {
      ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD,
          "NetCDF Attribute convention not allowed in binary mode",
          ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // It is an error to supply Attribute purpose in binary mode
  if (ESMF_IOFMT_BIN == localiofmt) {
    if (purpose.size() > 0) {
      ESMC_LogDefault.MsgFoundError(ESMF_RC_ARG_BAD,
          "NetCDF Attribute convention not allowed in binary mode",
          ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  DistGrid *dg = getDistGrid();
  ESMCI::Info *info_this = this->ESMC_BaseGetInfo();
  ESMCI::Info *info_dg = dg->ESMC_BaseGetInfo();
  if ((convention.length() > 0) && (purpose.length() > 0)) {
    if ((info_this->getCountPack() == 0) && (info_dg->getCountPack() == 0)) {
      localrc = ESMF_RC_ATTR_NOTSET;
      if (ESMC_LogDefault.MsgFoundError(localrc, "No Array or DistGrid AttPacks found", ESMC_CONTEXT,
          &rc)) return rc;
    }
  }

  // Key for the "attribute package"
  const std::string key = "/"+convention+"/"+purpose;
  bool has_convpurp = (convention.length() > 0) && (purpose.length() > 0);

  // If present, use Attributes at the DistGrid level for dimension names
  ESMCI::Info *dimAttPack = nullptr;
  if (has_convpurp) {
    try {
      if (info_dg->hasKey(key, true, false)) {
        dimAttPack = new ESMCI::Info();
        info_dg->get(*dimAttPack, key);
      }
    } catch (ESMCI::esmc_error &exc) {
      ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), exc.what(), ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // If present, use Attributes at the Array level for variable attributes
  ESMCI::Info *varAttPack = nullptr;
  if (has_convpurp) {
    try {
      if (info_this->hasKey(key, true, false)) {
        varAttPack = new ESMCI::Info();
        info_this->get(*varAttPack, key);
      }
    } catch (ESMCI::esmc_error &exc) {
      ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), exc.what(), ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  ESMCI::Info *gblAttPack = nullptr;

  IO *newIO = IO::create(&rc);
  if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)){
    return rc;
  }
  // From now on, we have to be sure to clean up before returning
  rc = newIO->addArray(this, variableName, dimAttPack, varAttPack, gblAttPack);
  if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) {
    IO::destroy(&newIO);
    newIO = (IO *)NULL;
    return rc;
  }

  // Call the IO write function
  rc = newIO->write(file, localiofmt,
                    localoverwrite, localstatus, timeslice);
  ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);

  // cleanup
  IO::destroy(&newIO);
  newIO = (IO *)NULL;

  // return
  return rc;
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

  // print info about the ESMCI::Array object
  printf("--- ESMCI::Array::print start ---\n");
  ESMC_Print(); // print the Base class info
  printf("Array typekind/rank: %s / %d \n", ESMC_TypeKind_FlagString(typekind),
    rank);
  printf("~ lower class' values ~\n");
  int dimCount = distgrid->getDimCount();
  printf("DistGrid dimCount = %d\n", dimCount);
  int deCount = delayout->getDeCount();
  printf("deCount = %d\n", deCount);
  int localDeCount = delayout->getLocalDeCount();
  printf("localDeCount = %d\n", localDeCount);
  const int redDimCount = rank - tensorCount;
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    printf("~ local data in LocalArray for DE %d ~\n", de);
    larrayList[i]->print();
    if (exclusiveElementCountPDe[de]){
      // associated DE
      int j=0;    // reset
      int jjj=0;  // reset
      for (int jj=0; jj<rank; jj++){
        if (arrayToDistGridMap[jj]){
          // distributed dimension
          printf("dim %d: [%d]: [%d [%d [%d, %d] %d] %d]\n",
            jj+1, j,
            totalLBound[i*redDimCount+j], computationalLBound[i*redDimCount+j],
            exclusiveLBound[i*redDimCount+j], exclusiveUBound[i*redDimCount+j],
            computationalUBound[i*redDimCount+j], totalUBound[i*redDimCount+j]);
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
#define ESMC_METHOD "ESMCI::Array::sync()"
//BOPI
// !IROUTINE:  ESMCI::Array::sync
//
// !INTERFACE:
int Array::sync(){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Sync DEs arcoss the Array object in case of sharing.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // see if the array holds a valid memhandle, optionally call sync
  if (mh != NULL){
    int localrc;
    VM *vm = delayout->getVM();      
    localrc = vm->ssishmSync(*mh);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

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

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::constructFileMap()"
//BOPI
// !IROUTINE:  ESMCI::Array::constructFileMap
//
// !INTERFACE:
//
int Array::constructFileMap(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int64_t *fileMapList,         // (in)  - Array of map elements to fill
  int mapListSize,              // (in)  - Number of elements in fileMapList
  int localDe,                  // (in)  - local DE = {0, ..., localDeCount-1}
  int64_t unmap_val             // (in)  - value to give to unmapped elements
  )const{
//
// !DESCRIPTION:
//    Construct the map between each local array element and its location
//    in a disk file for the total Array region on localDe.
//    Unmapped elements (those outside of exclusive region) are given a value
//    of of unmap_val (defaults to zero which is the PIO convention).
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{

    if (tensorElementCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "fileMapList is only defined for tensorElementCount == 1",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    // force the seqIndex lookup to use canonical mode because PIO expects
    // canonical sequence indices for its global ids---------------v
    ArrayElement arrayElement(this, localDe, false, true, false, true);
    int elementCount = totalElementCountPLocalDe[localDe];

    if (fileMapList != NULL){
      // fileMapList provided -> error checking
      if (mapListSize < elementCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "fileMapList array insufficiently sized", ESMC_CONTEXT, &rc);
        return rc;
      }
      // fill fileMapList
      //TODO: Make the following more efficient by obtaining the seqIndex list
      //TODO: from DistGrid for all the exclusive elements once, and then copy
      //TODO: from that list into fileMapList for each exclusive element.
      int element = 0;
      ESMC_TypeKind_Flag indexTK = distgrid->getIndexTK();
      if (indexTK==ESMC_TYPEKIND_I4){
        SeqIndex<ESMC_I4> seqIndex;
        while(arrayElement.isWithin()){
          if (arrayElement.isWithinWatch()){
            // within exclusive Array region -> obtain seqIndex value
            seqIndex = arrayElement.getSequenceIndex<ESMC_I4>();
            fileMapList[element] = seqIndex.decompSeqIndex;
          }else{
            // outside exclusive Array region -> mark this as unmapped element
            fileMapList[element] = unmap_val;
          }
          arrayElement.next();
          ++element;
        }
      }else if (indexTK==ESMC_TYPEKIND_I8){
        SeqIndex<ESMC_I8> seqIndex;
        while(arrayElement.isWithin()){
          if (arrayElement.isWithinWatch()){
            // within exclusive Array region -> obtain seqIndex value
            seqIndex = arrayElement.getSequenceIndex<ESMC_I8>();
            fileMapList[element] = seqIndex.decompSeqIndex;
          }else{
            // outside exclusive Array region -> mark this as unmapped element
            fileMapList[element] = unmap_val;
          }
          arrayElement.next();
          ++element;
        }
      }
    }

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
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
  int *offset,           // inout - original offset
  const ESMC_AttReconcileFlag &attreconflag,     // in - attreconcile flag
  const ESMC_InquireFlag &inquireflag) const {   // in - inquiry flag
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
  ESMC_TypeKind_Flag *dkp;
  ESMC_IndexFlag *ifp;
  int r;

  // Check if buffer has enough free memory to hold object
  if ((inquireflag != ESMF_INQUIREONLY) && (*length - *offset) <
    (int)sizeof(Array)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an Array object", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Serialize the Base class,
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Serialize(buffer, length, offset, attreconflag,
    inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Serialize the DistGrid
  localrc = distgrid->serialize(buffer, length, offset, inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Serialize Array meta data
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  dkp = (ESMC_TypeKind_Flag *)(buffer + *offset);
  if (inquireflag != ESMF_INQUIREONLY)
    *dkp++ = typekind;
  else
     dkp++;
  ip = (int *)dkp;
  if (inquireflag != ESMF_INQUIREONLY)
    *ip++ = rank;
  else
    ip++;
  ifp = (ESMC_IndexFlag *)ip;
  if (inquireflag != ESMF_INQUIREONLY)
    *ifp++ = indexflag;
  else
    ifp++;
  ip = (int *)ifp;
  if (inquireflag != ESMF_INQUIREONLY) {
    *ip++ = tensorCount;
    for (int i=0; i<tensorCount; i++){
      *ip++ = undistLBound[i];
      *ip++ = undistUBound[i];
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
  } else
    ip += 2 + 2*tensorCount + 2*distgrid->getDimCount () +
      rank + delayout->getDeCount ();

  // fix offset
  cp = (char *)ip;
  *offset = (cp - buffer);

  if (inquireflag == ESMF_INQUIREONLY)
    if (*offset < (int)sizeof (Array))
      *offset = sizeof (Array);

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
  int *offset,           // inout - original offset
  const ESMC_AttReconcileFlag &attreconflag) {  // in - attreconcile flag
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
  ESMC_TypeKind_Flag *dkp;
  ESMC_IndexFlag *ifp;
  int r;

  // Deserialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Deserialize(buffer, offset, attreconflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Deserialize the DistGrid
  distgrid = DistGrid::deserialize(buffer, offset);
  if (!distgrid)
     if (ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD,
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
         &rc)) return rc;
  distgridCreator = true;  // deserialize creates a local object
  // Pull DELayout out of DistGrid
  delayout = distgrid->getDELayout();
  // Deserialize Array meta data
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  dkp = (ESMC_TypeKind_Flag *)(buffer + *offset);
  typekind = *dkp++;
  ip = (int *)dkp;
  rank = *ip++;
  ifp = (ESMC_IndexFlag *)ip;
  indexflag = *ifp++;
  ip = (int *)ifp;
  tensorCount = *ip++;
  undistLBound = new int[tensorCount];
  undistUBound = new int[tensorCount];
  for (int i=0; i<tensorCount; i++){
    undistLBound[i] = *ip++;
    undistUBound[i] = *ip++;
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
  larrayList = new LocalArray*[0];     // no DE on proxy object
  larrayBaseAddrList = new void*[0];        // no DE on proxy object
  totalElementCountPLocalDe = NULL;         // no De on proxy object

  vasLocalDeCount = 0;
  ssiLocalDeCount = 0;
  localDeCountAux = delayout->getLocalDeCount(); // TODO: auxilary for garb
                                                 // TODO: until ref. counting

  ioRH = NULL;  // invalidate
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
  ESMC_TypeKind_Flag typekindArg,       // in -
  int rankArg,                          // in -
  int *counts,                          // in -
  int *tileArg,                         // in -
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

  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

  // query the VM for localPet and petCount
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  // deal with optional tile argument
  int tile = 1;  // default
  if (tileArg)
    tile = *tileArg;
  int tileCount = distgrid->getTileCount();
  if (tile < 1 || tile > tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
      "Specified tile out of bounds", ESMC_CONTEXT, &rc);
    return rc;
  }
  const int *tileListPDe = distgrid->getTileListPDe();

  // get minIndexPDim and maxIndexPDim for tile
  const int *minIndexPDim = distgrid->getMinIndexPDimPTile(tile, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  const int *maxIndexPDim = distgrid->getMaxIndexPDimPTile(tile, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // only rootPet checks for consistency of input, others listen
  if (localPet == rootPet){
    // check consistency of input information: shape = (typekind, rank, extents)
    if (typekindArg != typekind){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "TypeKind mismatch between array argument and Array object",
        ESMC_CONTEXT, &rc);
      vm->broadcast(&rc, sizeof(int), rootPet);
      return rc;
    }
    if (rankArg != rank){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "Type mismatch between array argument and Array object", ESMC_CONTEXT,
        &rc);
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
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "Extent mismatch between array argument and Array object",
            ESMC_CONTEXT, &rc);
          vm->broadcast(&rc, sizeof(int), rootPet);
          return rc;
        }
      }else{
        // tensor dimension
        if (counts[i] != undistUBound[tensorIndex] - undistLBound[tensorIndex]
          + 1){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "Extent mismatch between array argument and Array object",
            ESMC_CONTEXT, &rc);
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
    if (ESMC_LogDefault.MsgFoundError(localrc,
      "rootPet exited with error", ESMC_CONTEXT, &rc)) return rc;
  }

  // size in bytes of each piece of data
  int dataSize = ESMC_TypeKind_FlagSize(typekind);

  // distgrid and delayout values
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
  const int *minIndexPDimPDe = distgrid->getMinIndexPDimPDe();
  int dimCount = distgrid->getDimCount();
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();

  int redDimCount = rank - tensorCount;

  // prepare for comms
  VMK::commhandle **commh = new VMK::commhandle*; // used by all comm calls
  VMK::commhandle **commhList =
    new VMK::commhandle*[dimCount]; // used for indexList comm

  // the following code depends on the "contiguousFlag" -> may need to construct
  if (localDeCount && (contiguousFlag[0]==-1)){
    // has local DEs and contiguousFlag has no yet been constructed
    localrc = constructContiguousFlag(redDimCount);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc))
      return rc;
  }

  // all PETs may be senders of data, each PET issues a maximum of one
  // non-blocking send, so no problem with too many outstanding comms here
  char **sendBuffer = new char*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    if (tileListPDe[de] != tile) continue; // skip to next local DE
    sendBuffer[i] = (char *)larrayBaseAddrList[i]; // default: contiguous
    int sendSize =
      exclusiveElementCountPDe[de]*tensorElementCount*dataSize;  // bytes
    if (!contiguousFlag[i]){
      // only if this DE has a non-contiguous decomposition the contiguous
      // send buffer must be compiled from DE-local array segment piece by p.
      sendBuffer[i] = new char[sendSize];
      char *larrayBaseAddr = (char *)larrayBaseAddrList[i];
      int contigLength = exclusiveUBound[i*redDimCount]
        - exclusiveLBound[i*redDimCount] + 1;
      ArrayElement arrayElement(this, i, false, false, false);
      arrayElement.setSkipDim(0); // next() will skip ahead to next contig. line
      // loop over all elements in exclusive region for this DE and memcpy data
      long unsigned int sendBufferIndex = 0;  // reset
      while(arrayElement.isWithin()){
        // copy this element from excl. region into the contiguous sendBuffer
        long unsigned int linearIndex = arrayElement.getLinearIndex();
        // contiguous data copy in 1st dim
        memcpy(sendBuffer[i]+sendBufferIndex*dataSize,
          larrayBaseAddr+linearIndex*dataSize, contigLength*dataSize);
        sendBufferIndex += contigLength;
        arrayElement.next();  // skip ahead to next contiguous line
      } // multi dim index loop
    } // !contiguousFlag

    // ready to send the sendBuffer to rootPet
    *commh = NULL; // invalidate
    localrc = vm->send(sendBuffer[i], sendSize, rootPet, commh);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        message, ESMC_CONTEXT, &rc);
      delete [] message;
      return rc;
    }
  } // i -> de

  // rootPet is the only receiver for gather data
  char **recvBuffer;
  if (localPet == rootPet){
    int nbCount = 0;  // reset: count of outstanding non-blocking comms
    const int boostSize = 512;  // max number of posted non-blocking calls:
                                // stay below typical system limits
    VMK::commhandle **commhDataList =
      new VMK::commhandle*[boostSize]; // used for data comms

    // for each DE of the Array receive a single contiguous recvBuffer
    // from the associated PET non-blocking and memcpy it into the right
    // location in "array".
    recvBuffer = new char*[deCount]; // contiguous recvBuffer
    for (int i=0; i<deCount; i++){
      int de = i;
      if (tileListPDe[de] == tile){
        // this DE is located on sending tile
        // prepare contiguous recvBuffer for this DE and issue non-blocking recv
        int recvSize =
          exclusiveElementCountPDe[de]*tensorElementCount*dataSize;  // bytes
        recvBuffer[de] = new char[recvSize];
        int srcPet;
        delayout->getDEMatchPET(de, *vm, NULL, &srcPet, 1);
        commhDataList[nbCount] = NULL; // invalidate
        localrc = vm->recv(recvBuffer[de], recvSize, srcPet,
          &(commhDataList[nbCount]));
        ++nbCount;  // count this non-blocking recv
        if (localrc){
          char *message = new char[160];
          sprintf(message, "VMKernel/MPI error #%d\n", localrc);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            message, ESMC_CONTEXT, &rc);
          delete [] message;
          return rc;
        }

        // see if outstanding nb-recvs have reached boostSize limit
        if (nbCount >= boostSize){
//printf("clearing Gather() boost at nbCount = %d\n", nbCount);
          // wait for nb-recvs to finish before posting any more
          for (int j=0; j<nbCount; j++){
            vm->commwait(&(commhDataList[j]));
            delete commhDataList[j];
          }
          nbCount = 0;  // reset
        }

      } // DE on tile
    } // i -> de
//printf("clearing Gather() boost at nbCount = %d\n", nbCount);
    // wait for nb-recvs to finish before exiting
    for (int j=0; j<nbCount; j++){
      vm->commwait(&(commhDataList[j]));
      delete commhDataList[j];
    }
    nbCount = 0;  // reset
    delete [] commhDataList;
  }

  // wait until all the local sends are complete
  vm->commqueuewait();
  // - done waiting on sends -

  if (localPet == rootPet){
    char *array = (char *)arrayArg;
    // rootPet gathers information from _all_ DEs
    for (int i=0; i<deCount; i++){
      int de = i;
      if (tileListPDe[de] == tile){
        // this DE is located on sending tile
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
            if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          }
        } // j

        // initialize multi dim index loop
        vector<int> sizes;
        tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          int j = arrayToDistGridMap[jj];// j is dimIndex basis 1, or 0 f tensor
          if (j){
            // decomposed dimension
            --j;  // shift to basis 0
            sizes.push_back(indexCountPDimPDe[de*dimCount+j]);
          }else{
            // tensor dimension
            sizes.push_back(
              undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1);
            ++tensorIndex;
          }
        }

        // wait for all outstanding receives issued by fillIndexListPDimPDe()
        for (int j=0; j<commhListCount; j++){
          vm->commwait(&(commhList[j]));
          delete commhList[j];
        }

        MultiDimIndexLoop multiDimIndexLoop(sizes);
        if (contigFlagPDimPDe[de*dimCount])
          multiDimIndexLoop.setSkipDim(0); // contiguous data in first dimension
        // loop over all elements in exclusive region for this DE
        long unsigned int recvBufferIndex = 0;  // reset
        while(multiDimIndexLoop.isWithin()){
          // determine linear index for this element into array
          long unsigned int linearIndex = 0;  // reset
          for (int jj=rank-1; jj>=0; jj--){
            linearIndex *= counts[jj];  // first time zero o.k.
            int j = arrayToDistGridMap[jj];// j is dimIndex bas 1, or 0 f tensor
            if (j){
              // decomposed dimension
              --j;  // shift to basis 0
              if (contigFlagPDimPDe[de*dimCount+j]){
                linearIndex += minIndexPDimPDe[de*dimCount+j]
                  + multiDimIndexLoop.getIndexTuple()[jj];
              }else{
                linearIndex +=
                  indexList[j][multiDimIndexLoop.getIndexTuple()[jj]];
              }
              // shift basis 1 -> basis 0
              linearIndex -= minIndexPDim[j];
            }else{
              // tensor dimension
              linearIndex += multiDimIndexLoop.getIndexTuple()[jj];
            }
          }
          // copy this element from the contiguous recvBuffer for this DE
          if (contigFlagPDimPDe[de*dimCount]){
            // contiguous data in first dimension
            memcpy(array+linearIndex*dataSize,
              recvBuffer[de]+recvBufferIndex*dataSize,
              multiDimIndexLoop.getIndexTupleEnd()[0]*dataSize);
            multiDimIndexLoop.next(); // skip to next contiguous line
            recvBufferIndex += multiDimIndexLoop.getIndexTupleEnd()[0];
          }else{
            // non-contiguous data in first dimension
            memcpy(array+linearIndex*dataSize,
              recvBuffer[de]+recvBufferIndex*dataSize, dataSize);
            multiDimIndexLoop.next(); // next element
            ++recvBufferIndex;
          }
        } // multi dim index loop

        // clean-up
        for (int j=0; j<dimCount; j++)
          if(contigFlagPDimPDe[de*dimCount+j]==0)
            delete [] indexList[j];
        delete [] indexList;
      } // DE on tile
    } // i -> de
  }else{
    // localPet is _not_ rootPet -> provide localIndexList to rootPet if nec.
    int commhListCount = 0;  // reset
    for (int i=0; i<localDeCount; i++){
      int de = localDeToDeMap[i];
      if (tileListPDe[de] == tile){
        // this DE is located on receiving tile -> must send info to rootPet
        for (int j=0; j<dimCount; j++){
          if(distgridToArrayMap[j]!=0 && contigFlagPDimPDe[de*dimCount+j]==0){
            // associated and non-contiguous dimension
            // -> send local indexList for this DE and dim to rootPet
            commhList[commhListCount] = NULL; // prime for later test
            localrc = distgrid->fillIndexListPDimPDe(NULL, de, j+1,
              &(commhList[commhListCount]), rootPet, vm);
            if (commhList[commhListCount] != NULL)
              ++commhListCount;
            if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          }
        } // j
      } // DE on tile
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
      if (tileListPDe[de] == tile)
        delete [] recvBuffer[de];
    }
    delete [] recvBuffer;
  }
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    if (tileListPDe[de] != tile) continue; // skip to next local DE
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
  ESMC_TypeKind_Flag typekindArg,       // in -
  int rankArg,                          // in -
  int *counts,                          // in -
  int *tileArg,                         // in -
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

  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

  // query the VM
  int localPet = vm->getLocalPet();

  // deal with optional tile argument
  int tile = 1;  // default
  if (tileArg)
    tile = *tileArg;
  int tileCount = distgrid->getTileCount();
  if (tile < 1 || tile > tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
      "Specified tile out of bounds", ESMC_CONTEXT, &rc);
    return rc;
  }
  const int *tileListPDe = distgrid->getTileListPDe();

  // get minIndexPDim and maxIndexPDim for tile
  const int *minIndexPDim = distgrid->getMinIndexPDimPTile(tile, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  const int *maxIndexPDim = distgrid->getMaxIndexPDimPTile(tile, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // only rootPet checks for consistency of input, others listen
  if (localPet == rootPet){
    // check consistency of input information: shape = (typekind, rank, extents)
    if (typekindArg != typekind){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "TypeKind mismatch between array argument and Array object",
        ESMC_CONTEXT, &rc);
      vm->broadcast(&rc, sizeof(int), rootPet);
      return rc;
    }
    if (rankArg != rank){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "Type mismatch between array argument and Array object", ESMC_CONTEXT,
        &rc);
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
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "Extent mismatch between array argument and Array object",
            ESMC_CONTEXT, &rc);
          vm->broadcast(&rc, sizeof(int), rootPet);
          return rc;
        }
      }else{
        // tensor dimension
        if (counts[i] != undistUBound[tensorIndex] - undistLBound[tensorIndex]
          + 1){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "Extent mismatch between array argument and Array object",
            ESMC_CONTEXT, &rc);
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
    if (ESMC_LogDefault.MsgFoundError(localrc,
      "rootPet exited with error", ESMC_CONTEXT, &rc)) return rc;
  }

  // size in bytes of each piece of data
  int dataSize = ESMC_TypeKind_FlagSize(typekind);

  // distgrid and delayout values
  const int *indexCountPDimPDe = distgrid->getIndexCountPDimPDe();
  const int *contigFlagPDimPDe = distgrid->getContigFlagPDimPDe();
  const int *minIndexPDimPDe = distgrid->getMinIndexPDimPDe();
  int dimCount = distgrid->getDimCount();
  int deCount = delayout->getDeCount();
  int localDeCount = delayout->getLocalDeCount();

  int redDimCount = rank - tensorCount;

  // prepare for comms
  VMK::commhandle **commh = new VMK::commhandle*; // used by all comm calls
  VMK::commhandle **commhList =
    new VMK::commhandle*[dimCount]; // used for indexList comm

  // the following code depends on the "contiguousFlag" -> may need to construct
  if (localDeCount && (contiguousFlag[0]==-1)){
    // has local DEs and contiguousFlag has no yet been constructed
    localrc = constructContiguousFlag(redDimCount);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc))
      return rc;
  }

  // all PETs may be receivers of data, each PET issues a maximum of one
  // non-blocking receive, so no problem with too many outstanding comms here
  char **recvBuffer = new char*[localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    if (tileListPDe[de] != tile) continue; // skip to next local DE
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
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        message, ESMC_CONTEXT, &rc);
      delete [] message;
      return rc;
    }
  }
  // - done issuing nb receives (potentially all Pets) -

  // rootPet is the only sender of scatter data,
  // but may need info from other PETs to construct sendBuffer
  if (localPet == rootPet){
    int nbCount = 0; // reset: count of outstanding non-blocking comms
    const int boostSize = 512;  // max number of posted non-blocking calls:
                                // stay below typical system limits
    VMK::commhandle **commhDataList =
      new VMK::commhandle*[boostSize]; // used for data comms

    char *array = (char *)arrayArg;
    // rootPet scatters information to _all_ DEs
    // for each DE of the Array memcpy together a single contiguous sendBuffer
    // from "array" data and send it to the receiving PET non-blocking.
    char **sendBuffer = new char*[deCount]; // contiguous sendBuffer
    for (int i=0; i<deCount; i++){
      int de = i;
      if (tileListPDe[de] == tile){
        // this DE is located on receiving tile
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
            if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          }
        } // j

        // prepare contiguous sendBuffer for this DE
        int sendSize =
          exclusiveElementCountPDe[de]*tensorElementCount*dataSize;  // bytes
        sendBuffer[de] = new char[sendSize];

        // initialize multi dim index loop
        vector<int> sizes;
        tensorIndex=0;  // reset
        for (int jj=0; jj<rank; jj++){
          int j = arrayToDistGridMap[jj];// j is dimIndex basis 1, or 0 f tensor
          if (j){
            // decomposed dimension
            --j;  // shift to basis 0
            sizes.push_back(indexCountPDimPDe[de*dimCount+j]);
          }else{
            // tensor dimension
            sizes.push_back(
              undistUBound[tensorIndex] - undistLBound[tensorIndex] + 1);
            ++tensorIndex;
          }
        }

        // wait for all outstanding receives issued by fillIndexListPDimPDe()
        for (int j=0; j<commhListCount; j++){
          vm->commwait(&(commhList[j]));
          delete commhList[j];
        }

        MultiDimIndexLoop multiDimIndexLoop(sizes);
        if (contigFlagPDimPDe[de*dimCount])
          multiDimIndexLoop.setSkipDim(0); // contiguous data in first dimension
        // loop over all elements in exclusive region for this DE
        long unsigned int sendBufferIndex = 0;  // reset
        while(multiDimIndexLoop.isWithin()){
          // determine linear index for this element into array
          long unsigned int linearIndex = 0;  // reset
          for (int jj=rank-1; jj>=0; jj--){
            linearIndex *= counts[jj];  // first time zero o.k.
            int j = arrayToDistGridMap[jj];// j is dimIndex bas 1, or 0 f tensor
            if (j){
              // decomposed dimension
              --j;  // shift to basis 0
              if (contigFlagPDimPDe[de*dimCount+j]){
                linearIndex += minIndexPDimPDe[de*dimCount+j]
                  + multiDimIndexLoop.getIndexTuple()[jj];
              }else{
                linearIndex +=
                  indexList[j][multiDimIndexLoop.getIndexTuple()[jj]];
              }
              // shift basis 1 -> basis 0
              linearIndex -= minIndexPDim[j];
            }else{
              // tensor dimension
              linearIndex += multiDimIndexLoop.getIndexTuple()[jj];
            }
          }
          // copy this element into the contiguous sendBuffer for this DE
          if (contigFlagPDimPDe[de*dimCount]){
            // contiguous data in first dimension
            memcpy(sendBuffer[de]+sendBufferIndex*dataSize,
              array+linearIndex*dataSize,
              multiDimIndexLoop.getIndexTupleEnd()[0]*dataSize);
            multiDimIndexLoop.next(); // skip to next contiguous line
            sendBufferIndex += multiDimIndexLoop.getIndexTupleEnd()[0];
          }else{
            // non-contiguous data in first dimension
            memcpy(sendBuffer[de]+sendBufferIndex*dataSize,
              array+linearIndex*dataSize, dataSize);
            multiDimIndexLoop.next(); // next element
            ++sendBufferIndex;
          }
        } // multi dim index loop

        // ready to send the sendBuffer
        int dstPet;
        delayout->getDEMatchPET(de, *vm, NULL, &dstPet, 1);
        commhDataList[nbCount] = NULL;  // invalidate
        localrc = vm->send(sendBuffer[de], sendSize, dstPet,
          &(commhDataList[nbCount]));
        ++nbCount;  // count this non-blocking send
        if (localrc){
          char *message = new char[160];
          sprintf(message, "VMKernel/MPI error #%d\n", localrc);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            message, ESMC_CONTEXT, &rc);
          delete [] message;
          return rc;
        }

        // clean-up
        for (int j=0; j<dimCount; j++)
          if(contigFlagPDimPDe[de*dimCount+j]==0)
            delete [] indexList[j];
        delete [] indexList;

        // see if outstanding nb-sends have reached boostSize limit
        if (nbCount >= boostSize){
//printf("clearing Scatter() boost at nbCount = %d\n", nbCount);
          // wait for nb-sends to finish before posting any more
          for (int j=0; j<nbCount; j++){
            vm->commwait(&(commhDataList[j]));
            delete commhDataList[j];
          }
          nbCount = 0;  // reset
        }

      } // DE on tile
    } // i -> de
//printf("clearing Scatter() boost at nbCount = %d\n", nbCount);
    // wait for nb-sends to finish before exiting
    for (int j=0; j<nbCount; j++){
      vm->commwait(&(commhDataList[j]));
      delete commhDataList[j];
    }
    nbCount = 0;  // reset
    delete [] commhDataList;
    // TODO: move the delete [] sendBuffer[] into nb-wait loops to lower
    // TODO: memory foot print.
    for (int i=0; i<deCount; i++)
      if (tileListPDe[i] == tile)
        delete [] sendBuffer[i];
    delete [] sendBuffer;
  }
  // - done issuing nb sends (from rootPet) -

  // send localIndexList information to rootPet if necessary
  if (localPet != rootPet){
    // localPet is _not_ rootPet -> provide localIndexList to rootPet if nec.
    int commhListCount = 0;  // reset
    for (int i=0; i<localDeCount; i++){
      int de = localDeToDeMap[i];
      if (tileListPDe[de] == tile){
        // this DE is located on receiving tile -> must send info to rootPet
        for (int j=0; j<dimCount; j++){
          if(distgridToArrayMap[j]!=0 && contigFlagPDimPDe[de*dimCount+j]==0){
            // associated and non-contiguous dimension
            // -> send local indexList for this DE and dim to rootPet
            commhList[commhListCount] = NULL; // prime for later test
            localrc = distgrid->fillIndexListPDimPDe(NULL, de, j+1,
              &(commhList[commhListCount]), rootPet, vm);
            if (commhList[commhListCount] != NULL)
              ++commhListCount;
            if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          }
        } // j
      } // DE on tile
    } // i -> de
    // wait for all outstanding indexList sends issued by fillIndexListPDimPDe()
    for (int j=0; j<commhListCount; j++){
      vm->commwait(&(commhList[j]));
        delete commhList[j];
    }
  }

  // wait until all the local receives are complete
  vm->commqueuewait();
  // - done waiting on receives -

  // distribute received data into non-contiguous exclusive regions
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    if (tileListPDe[de] != tile) continue; // skip to next local DE
    if (!contiguousFlag[i]){
      // only if this DE has a non-contiguous decomposition the contiguous
      // receive buffer must be copied into DE-local array segment piece by p.
      char *larrayBaseAddr = (char *)larrayBaseAddrList[i];
      int contigLength = exclusiveUBound[i*redDimCount]
        - exclusiveLBound[i*redDimCount] + 1;
      ArrayElement arrayElement(this, i, false, false, false);
      arrayElement.setSkipDim(0); // next() will skip ahead to next contig. line
      // loop over all elements in exclusive region for this DE and memcpy data
      long unsigned int recvBufferIndex = 0;  // reset
      while(arrayElement.isWithin()){
        // copy this element from the contiguous recvBuffer into excl. region
        long unsigned int linearIndex = arrayElement.getLinearIndex();
        // since the data in the recvBuffer was constructed to be contiguous
        // wrt data layout on destination DE -> contiguous data copy in 1st dim
        memcpy(larrayBaseAddr+linearIndex*dataSize,
          recvBuffer[i]+recvBufferIndex*dataSize, contigLength*dataSize);
        recvBufferIndex += contigLength;
        arrayElement.next();  // skip ahead to next contiguous line
      } // multi dim index loop

      // clean-up
      delete [] recvBuffer[i];
    } // !contiguousFlag
  } // i -> de

  // garbage collection
  delete [] recvBuffer;
  delete commh;
  delete [] commhList;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::haloStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::haloStore
//
// !INTERFACE:
int Array::haloStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *array,                       // in    - Array
  RouteHandle **routehandle,          // inout - handle to precomputed comm
  ESMC_HaloStartRegionFlag halostartregionflag, // in - start of halo region
  InterArray<int> *haloLDepth,        // in    - lower corner halo depth
  InterArray<int> *haloUDepth,        // in    - upper corner halo depth
  int *pipelineDepthArg               // in (optional)
  ){
//
// !DESCRIPTION:
//  Precompute and store communication pattern for halo
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ESMC_TypeKind_Flag indexTK = array->getDistGrid()->getIndexTK();

  if (indexTK==ESMC_TYPEKIND_I4){
    localrc=array->tHaloStore<ESMC_I4>(array, routehandle, halostartregionflag,
      haloLDepth, haloUDepth, pipelineDepthArg);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }else if (indexTK==ESMC_TYPEKIND_I8){
    localrc=array->tHaloStore<ESMC_I8>(array, routehandle, halostartregionflag,
      haloLDepth, haloUDepth, pipelineDepthArg);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::tHaloStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::tHaloStore
//
// !INTERFACE:
template<typename IT>
  int Array::tHaloStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *array,                       // in    - Array
  RouteHandle **routehandle,          // inout - handle to precomputed comm
  ESMC_HaloStartRegionFlag halostartregionflag, // in - start of halo region
  InterArray<int> *haloLDepth,        // in    - lower corner halo depth
  InterArray<int> *haloUDepth,        // in    - upper corner halo depth
  int *pipelineDepthArg               // in (optional)
  ){
//
// !DESCRIPTION:
//  Precompute and store communication pattern for halo
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{

    // every Pet must provide array argument
    if (array == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to array", ESMC_CONTEXT, &rc);
      return rc;
    }

    // get the current VM and VM releated information
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    int localPet = vm->getLocalPet();
    int petCount = vm->getPetCount();

#ifdef HALO_STORE_MEMLOG_on
    VM::logMemInfo(std::string("HaloStore1"));
#endif

    // prepare haloOutsideLBound and haloInsideLBound arrays
    int localDeCount = array->getDELayout()->getLocalDeCount();
    int redDimCount = array->rank - array->tensorCount;
    vector<vector<int> > haloInsideLBound(localDeCount);
    vector<vector<int> > haloOutsideLBound(localDeCount);
    if (!present(haloLDepth)){
      // haloLDepth was not provided
      for (int i=0; i<localDeCount; i++){
        int kOff = i * (array->rank - array->tensorCount);
        int kPacked = 0;    // reset
        int kTensor = 0;    // reset
        haloInsideLBound[i].resize(array->rank);
        haloOutsideLBound[i].resize(array->rank);
        for (int k=0; k<array->rank; k++){
          if (array->getArrayToDistGridMap()[k]){
            // decomposed dimension
            if (halostartregionflag==ESMF_REGION_COMPUTATIONAL){
              haloInsideLBound[i][k] = array->computationalLBound[kOff+kPacked]
                - array->exclusiveLBound[kOff+kPacked];
            }else{
              haloInsideLBound[i][k] = 0;
            }
            haloOutsideLBound[i][k] = array->totalLBound[kOff+kPacked]
              - array->exclusiveLBound[kOff+kPacked];
            ++kPacked;
          }else{
            // tensor dimension
            haloOutsideLBound[i][k] = 0;
          }
        }
      }
    }else{
      // haloLDepth was provided -> check and use
      if (haloLDepth->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "haloLDepth array must be of rank 1", ESMC_CONTEXT, &rc);
        return rc;
      }
      if (haloLDepth->extent[0] != (array->rank - array->tensorCount)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "haloLDepth array has wrong size", ESMC_CONTEXT, &rc);
        return rc;
      }
      for (int i=0; i<localDeCount; i++){
        int kOff = i * (array->rank - array->tensorCount);
        int kPacked = 0;    // reset
        int kTensor = 0;    // reset
        haloInsideLBound[i].resize(array->rank);
        haloOutsideLBound[i].resize(array->rank);
        for (int k=0; k<array->rank; k++){
          if (array->getArrayToDistGridMap()[k]){
            // decomposed dimension
            if (halostartregionflag==ESMF_REGION_COMPUTATIONAL){
              haloInsideLBound[i][k] = array->computationalLBound[kOff+kPacked]
                - array->exclusiveLBound[kOff+kPacked];
              haloOutsideLBound[i][k] = haloInsideLBound[i][k]
                - haloLDepth->array[kPacked];
            }else{
              haloInsideLBound[i][k] = 0;
              haloOutsideLBound[i][k] = -haloLDepth->array[kPacked];
            }
            ++kPacked;
          }else{
            // tensor dimension
            haloOutsideLBound[i][k] = 0;
          }
        }
      }
    }
    // prepare haloOutsideUBound and haloInsideUBound arrays
    vector<vector<int> > haloInsideUBound(localDeCount);
    vector<vector<int> > haloOutsideUBound(localDeCount);
    if (!present(haloUDepth)){
      // haloUDepth was not provided
      for (int i=0; i<localDeCount; i++){
        int kOff = i * (array->rank - array->tensorCount);
        int kPacked = 0;    // reset
        int kTensor = 0;    // reset
        haloInsideUBound[i].resize(array->rank);
        haloOutsideUBound[i].resize(array->rank);
        for (int k=0; k<array->rank; k++){
          if (array->getArrayToDistGridMap()[k]){
            // decomposed dimension
            if (halostartregionflag==ESMF_REGION_COMPUTATIONAL){
              haloInsideUBound[i][k] = array->computationalUBound[kOff+kPacked]
                - array->exclusiveLBound[kOff+kPacked];
            }else{
              haloInsideUBound[i][k] = array->exclusiveUBound[kOff+kPacked]
              - array->exclusiveLBound[kOff+kPacked];
            }
            haloOutsideUBound[i][k] = array->totalUBound[kOff+kPacked]
              - array->exclusiveLBound[kOff+kPacked];
            ++kPacked;
          }else{
            // tensor dimension
            haloOutsideUBound[i][k] = array->undistUBound[kTensor]
              - array->undistLBound[kTensor] + 1;
            ++kTensor;
          }
        }
      }
    }else{
      // haloUDepth was provided -> check and use
      if (haloUDepth->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "haloUDepth array must be of rank 1", ESMC_CONTEXT, &rc);
        return rc;
      }
      if (haloUDepth->extent[0] != (array->rank - array->tensorCount)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "haloUDepth array has wrong size", ESMC_CONTEXT, &rc);
        return rc;
      }
      for (int i=0; i<localDeCount; i++){
        int kOff = i * (array->rank - array->tensorCount);
        int kPacked = 0;    // reset
        int kTensor = 0;    // reset
        haloInsideUBound[i].resize(array->rank);
        haloOutsideUBound[i].resize(array->rank);
        for (int k=0; k<array->rank; k++){
          if (array->getArrayToDistGridMap()[k]){
            // decomposed dimension
            if (halostartregionflag==ESMF_REGION_COMPUTATIONAL){
              haloInsideUBound[i][k] = array->computationalUBound[kOff+kPacked]
                - array->exclusiveLBound[kOff+kPacked];
              haloOutsideUBound[i][k] = haloInsideUBound[i][k]
                + haloUDepth->array[kPacked];
            }else{
              haloInsideUBound[i][k] = array->exclusiveUBound[kOff+kPacked]
              - array->exclusiveLBound[kOff+kPacked];
              haloOutsideUBound[i][k] = array->exclusiveUBound[kOff+kPacked]
                - array->exclusiveLBound[kOff+kPacked]
                + haloUDepth->array[kPacked];
            }
            ++kPacked;
          }else{
            // tensor dimension
            haloOutsideUBound[i][k] = array->undistUBound[kTensor]
              - array->undistLBound[kTensor] + 1;
            ++kTensor;
          }
        }
      }
    }

#ifdef HALO_STORE_MEMLOG_on
    VM::logMemInfo(std::string("HaloStore2"));
#endif

#define HALOTENSORMIX_off
    // construct identity sparse matrix from rim elements with valid seqIndex
    vector<IT> factorIndexList;
    int factorListCount = 0;  // init
    vector<vector<int> > rimMaskElement(localDeCount);
    vector<vector<SeqIndex<IT> > > rimMaskSeqIndex;
    rimMaskSeqIndex.resize(localDeCount);
    ESMC_TypeKind_Flag indexTK = array->getDistGrid()->getIndexTK();
    const std::vector<std::vector<SeqIndex<IT> > > *rimSeqIndex;
    array->getRimSeqIndex(&rimSeqIndex);
    for (int i=0; i<localDeCount; i++){
      ArrayElement arrayElement(array, i, true, false, false, false);
      int element = 0;
      while(arrayElement.isWithin()){
        SeqIndex<IT> seqIndex = (*rimSeqIndex)[i][element];
        if (seqIndex.valid()){
          // this rim element holds a valid seqIndex
          int const *indexTuple = arrayElement.getIndexTuple();
          // check whether indexTuple is within halo bounds
          bool withinHalo = true;
          bool insideFlag = true;
          for (int k=0; k<array->rank; k++){
            if (indexTuple[k] < haloOutsideLBound[i][k] ||
              indexTuple[k] > haloOutsideUBound[i][k]){
              withinHalo = false; // index outside halo region
              break;
            }
            if (indexTuple[k] < haloInsideLBound[i][k] ||
              indexTuple[k] > haloInsideUBound[i][k]){
              insideFlag = false; // below halo start
            }
          }
          if (insideFlag) withinHalo = false; // element below halo start
          if (withinHalo){
            // add element to identity matrix
            factorIndexList.push_back(seqIndex.decompSeqIndex); // src
#ifdef HALOTENSORMIX_on
            factorIndexList.push_back(seqIndex.getTensor()); // src
#endif
            factorIndexList.push_back(seqIndex.decompSeqIndex); // dst
#ifdef HALOTENSORMIX_on
            factorIndexList.push_back(seqIndex.getTensor()); // dst
#endif
            ++factorListCount;  // count this element
          }else{
            // need to mask this element in the Array rim region in order to
            // prevent haloing due to the fact that another DE may still
            // add an entry for this seqIndex to the identity matrix
            rimMaskElement[i].push_back(element);
            rimMaskSeqIndex[i].push_back(seqIndex);
            // mask entry
            if (indexTK==ESMC_TYPEKIND_I4)
              array->rimSeqIndexI4[i][element].decompSeqIndex = -1;  // mask
            else if (indexTK==ESMC_TYPEKIND_I8)
              array->rimSeqIndexI8[i][element].decompSeqIndex = -1;  // mask
          }
        }
        arrayElement.next();  // next element
        ++element;
      } // multi dim index loop
    }

    // load type specific factorList with "1" for the identity matrix
    ESMC_TypeKind_Flag typekindFactor = array->getTypekind();
    void *factorList;
    if (typekindFactor == ESMC_TYPEKIND_R4){
      ESMC_R4 *factorListT = new ESMC_R4[factorListCount];
      for (int i=0; i<factorListCount; i++)
        factorListT[i] = 1.;
      factorList = (void *)factorListT;
    }else if (typekindFactor == ESMC_TYPEKIND_R8){
      ESMC_R8 *factorListT = new ESMC_R8[factorListCount];
      for (int i=0; i<factorListCount; i++)
        factorListT[i] = 1.;
      factorList = (void *)factorListT;
    }else if (typekindFactor == ESMC_TYPEKIND_I4){
      ESMC_I4 *factorListT = new ESMC_I4[factorListCount];
      for (int i=0; i<factorListCount; i++)
        factorListT[i] = 1;
      factorList = (void *)factorListT;
    }else if (typekindFactor == ESMC_TYPEKIND_I8){
      ESMC_I8 *factorListT = new ESMC_I8[factorListCount];
      for (int i=0; i<factorListCount; i++)
        factorListT[i] = 1;
      factorList = (void *)factorListT;
    }

    // prepare SparseMatrix vector with the constructed sparse matrix
    vector<SparseMatrix<IT,IT> > sparseMatrix;
    void *factorIndexListPtr = NULL; // initialize
    if (factorListCount>0) factorIndexListPtr = (void *)&(factorIndexList[0]);
    sparseMatrix.push_back(SparseMatrix<IT,IT>(typekindFactor, factorList,
#ifdef HALOTENSORMIX_on
      factorListCount, 2, 2, factorIndexListPtr));
#else
      factorListCount, 1, 1, factorIndexListPtr));
#endif
#ifdef HALO_STORE_MEMLOG_on
    VM::logMemInfo(std::string("HaloStore3"));
#endif
    // precompute sparse matrix multiplication
    int srcTermProcessing = 0;  // no need to use auto-tuning to figure this out
    localrc = sparseMatMulStore(array, array, routehandle, sparseMatrix, true,
      false, &srcTermProcessing, pipelineDepthArg);

#ifdef HALO_STORE_MEMLOG_on
    VM::logMemInfo(std::string("HaloStore4"));
#endif

    // remove seqIndex masking in Array rim region before evaluating return code
    if (indexTK==ESMC_TYPEKIND_I4){
      for (int i=0; i<localDeCount; i++){
        for (unsigned k=0; k<rimMaskElement[i].size(); k++){
          int element = rimMaskElement[i][k];
          array->rimSeqIndexI4[i][element].decompSeqIndex =
            rimMaskSeqIndex[i][k].decompSeqIndex; // restore
        }
      }
    }else if (indexTK==ESMC_TYPEKIND_I8){
      for (int i=0; i<localDeCount; i++){
        for (unsigned k=0; k<rimMaskElement[i].size(); k++){
          int element = rimMaskElement[i][k];
          array->rimSeqIndexI8[i][element].decompSeqIndex =
            rimMaskSeqIndex[i][k].decompSeqIndex; // restore
        }
      }
    }

#ifdef HALO_STORE_MEMLOG_on
    VM::logMemInfo(std::string("HaloStore5"));
#endif

    // garbage collection before evaluating return code
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

#ifdef HALO_STORE_MEMLOG_on
    VM::logMemInfo(std::string("HaloStore6"));
#endif

    // error handling
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc)) return rc;

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::halo()"
//BOPI
// !IROUTINE:  ESMCI::Array::halo
//
// !INTERFACE:
int Array::halo(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *array,                         // in    - Array
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  ESMC_CommFlag commflag,               // in    - communication options
  bool *finishedflag,                   // out   - TEST ops finished or not
  bool *cancelledflag,                  // out   - any cancelled operations
  bool checkflag                        // in    - false: (def.) basic checks
                                        //         true:  full input check
  ){
//
// !DESCRIPTION:
//    Execute an Array halo
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // implemented via sparseMatMul
  localrc = sparseMatMul(array, array, routehandle,
    commflag, finishedflag, cancelledflag, ESMC_REGION_SELECT,
    ESMC_TERMORDER_FREE, checkflag, true);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::haloRelease()"
//BOPI
// !IROUTINE:  ESMCI::Array::haloRelease
//
// !INTERFACE:
int Array::haloRelease(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  RouteHandle *routehandle        // inout -
  ){
//
// !DESCRIPTION:
//    Release information for an Array halo
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // implemented via sparseMatMul
  localrc = sparseMatMulRelease(routehandle);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

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
  Array *srcArray,                        // in    - source Array
  Array *dstArray,                        // in    - destination Array
  RouteHandle **routehandle,              // inout - handle to precomputed comm
  InterArray<int> *srcToDstTransposeMap,  // in    - mapping src -> dst dims
  ESMC_TypeKind_Flag typekindFactor,      // in    - typekind of factor
  void *factor,                           // in    - redist factor
  bool ignoreUnmatched,                   // in    - support unmatched indices
  int *pipelineDepthArg                   // in (optional)
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to srcArray", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to dstArray", ESMC_CONTEXT, &rc);
    return rc;
  }

  // determine indexTK for src and dst
  ESMC_TypeKind_Flag srcIndexTK = srcArray->getDistGrid()->getIndexTK();
  ESMC_TypeKind_Flag dstIndexTK = dstArray->getDistGrid()->getIndexTK();

  //ESMCI_REGION_ENTER("ESMCI::Array::tRedistStore", localrc)
  if (srcIndexTK==ESMC_TYPEKIND_I4 && dstIndexTK==ESMC_TYPEKIND_I4){
    // call into the actual store method
    localrc = tRedistStore<ESMC_I4,ESMC_I4>(
      srcArray, dstArray, routehandle, srcToDstTransposeMap,
      typekindFactor, factor, ignoreUnmatched, pipelineDepthArg);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }else if (srcIndexTK==ESMC_TYPEKIND_I8 && dstIndexTK==ESMC_TYPEKIND_I8){
    // call into the actual store method
    localrc = tRedistStore<ESMC_I8,ESMC_I8>(
      srcArray, dstArray, routehandle, srcToDstTransposeMap,
      typekindFactor, factor, ignoreUnmatched, pipelineDepthArg);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }else{
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Type option not supported", ESMC_CONTEXT, &rc);
    return rc;
  }
  //ESMCI_REGION_EXIT("ESMCI::Array::tRedistStore", localrc)

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::tRedistStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::tRedistStore
//
// !INTERFACE:
template<typename SIT, typename DIT>
  int Array::tRedistStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                        // in    - source Array
  Array *dstArray,                        // in    - destination Array
  RouteHandle **routehandle,              // inout - handle to precomputed comm
  InterArray<int> *srcToDstTransposeMap,  // in    - mapping src -> dst dims
  ESMC_TypeKind_Flag typekindFactor,      // in    - typekind of factor
  void *factor,                           // in    - redist factor
  bool ignoreUnmatched,                   // in    - support unmatched indices
  int *pipelineDepthArg                   // in (optional)
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

  try{

  // every Pet must provide srcArray and dstArray
  if (srcArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to srcArray", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to dstArray", ESMC_CONTEXT, &rc);
    return rc;
  }
  // srcArray and dstArray must not point to the identical Array object
  if (srcArray == dstArray){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "srcArray and dstArray must not be identical", ESMC_CONTEXT, &rc);
    return rc;
  }

  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  // determine consistent typekindFactor and factor across all PETs
  if (factor != NULL){
    // must define a valid typekind
    if (typekindFactor == ESMF_NOKIND){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "must specify valid typekindFactor on PETs that provide factor",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    if (typekindFactor != ESMC_TYPEKIND_I4
      && typekindFactor != ESMC_TYPEKIND_I8
      && typekindFactor != ESMC_TYPEKIND_R4
      && typekindFactor != ESMC_TYPEKIND_R8){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "method not implemented for specified typekindFactor", ESMC_CONTEXT,
        &rc);
      return rc;
    }
  }else{
    // set typekindFactor to ESMF_NOKIND
    // -> this Pet will not provide factor
    typekindFactor = ESMF_NOKIND;
  }


  // communicate typekindFactor across all Pets
  ESMC_TypeKind_Flag *typekindList = new ESMC_TypeKind_Flag[petCount];
  vm->allgather(&typekindFactor, typekindList, sizeof(ESMC_TypeKind_Flag));
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
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "TypeKind mismatch between PETs", ESMC_CONTEXT, &rc);
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
  ESMC_R4 factorLocalTR4; // type specific factor, persisting during store call
  ESMC_R8 factorLocalTR8; // type specific factor, persisting during store call
  ESMC_I4 factorLocalTI4; // type specific factor, persisting during store call
  ESMC_I8 factorLocalTI8; // type specific factor, persisting during store call
  if (typekindFactor == ESMC_TYPEKIND_R4){
    ESMC_R4 *factorLocalT = &factorLocalTR4;
    if (factor)
      *factorLocalT = *(ESMC_R4 *)factor;
    else
      *factorLocalT = 1.;
    factorLocal = (void *)factorLocalT;
  }else if (typekindFactor == ESMC_TYPEKIND_R8){
    ESMC_R8 *factorLocalT = &factorLocalTR8;
    if (factor)
      *factorLocalT = *(ESMC_R8 *)factor;
    else
      *factorLocalT = 1.;
    factorLocal = (void *)factorLocalT;
  }else if (typekindFactor == ESMC_TYPEKIND_I4){
    ESMC_I4 *factorLocalT = &factorLocalTI4;
    if (factor)
      *factorLocalT = *(ESMC_I4 *)factor;
    else
      *factorLocalT = 1;
    factorLocal = (void *)factorLocalT;
  }else if (typekindFactor == ESMC_TYPEKIND_I8){
    ESMC_I8 *factorLocalT = &factorLocalTI8;
    if (factor)
      *factorLocalT = *(ESMC_I8 *)factor;
    else
      *factorLocalT = 1;
    factorLocal = (void *)factorLocalT;
  }

  if (factorPetCount > 0){
    // communicate factorLocal variables and check for consistency
    int factorSize = ESMC_TypeKind_FlagSize(typekindFactor);
    char *factorLocalList = new char[petCount*factorSize];
    vm->allgather(factorLocal, factorLocalList, factorSize);
    // prime factorLocal with value from first Pet with factor
    memcpy(factorLocal, factorLocalList + factorSize * factorPetList[0],
      factorSize);
    // check against all other factorLocal entries
    for (int i=1; i<factorPetCount; i++){
      char *factorPtr = factorLocalList + factorSize * factorPetList[i];
      if (memcmp(factorLocal, factorPtr, factorSize) != 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "Factor mismatch between PETs", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
    delete [] factorLocalList;
  }
  delete [] factorPetList;

  // set up local factorList and factorIndexList
  int factorListCount;
  int srcN;
  int dstN;
  SIT *factorIndexList;

  if (!present(srcToDstTransposeMap)){
    // srcToDstTransposeMap not specified -> default mode

    // src and dst Arrays must have identical number of exclusive elements
    ESMC_I8 srcElementCount = 0; // init
    const ESMC_I8 *srcElementCountPTile =
      srcArray->distgrid->getElementCountPTile();
    for (int i=0; i<srcArray->distgrid->getTileCount(); i++)
      srcElementCount += srcElementCountPTile[i];
    ESMC_I8 dstElementCount = 0; // init
    const ESMC_I8 *dstElementCountPTile =
      dstArray->distgrid->getElementCountPTile();
    for (int i=0; i<dstArray->distgrid->getTileCount(); i++)
      dstElementCount += dstElementCountPTile[i];
    if (!ignoreUnmatched && (srcElementCount != dstElementCount)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "srcArray and dstArray must provide identical number of exclusive"
        " elements", ESMC_CONTEXT, &rc);
      return rc;
    }

    // implemented via sparseMatMul using identity matrix
    const ESMC_I8 *srcElementCountPDe =
      srcArray->distgrid->getElementCountPDe();
    int *const*srcArbSeqIndexCountPCollPLocalDe =
      srcArray->distgrid->getElementCountPCollPLocalDe();
    const int *srcLocalDeToDeMap = srcArray->delayout->getLocalDeToDeMap();
    int srcLocalDeCount = srcArray->delayout->getLocalDeCount();
    factorListCount = 0;  // init
    for (int i=0; i<srcLocalDeCount; i++)
      factorListCount += srcElementCountPDe[srcLocalDeToDeMap[i]];
    // set up factorIndexList
    srcN = 1; // 1 component seqIndex
    dstN = 1; // 1 component seqIndex
    factorIndexList = new SIT[(srcN+dstN)*factorListCount];
    int jj = 0; // reset
    for (int i=0; i<srcLocalDeCount; i++){
      //TODO: this is hardcoded for first collocation only
      int arbSeqIndexCount = srcArbSeqIndexCountPCollPLocalDe[0][i];
      const SIT *srcArbSeqIndexListPLocalDe =
        (const SIT *)srcArray->distgrid->getArbSeqIndexList(i,1);
      if (srcArbSeqIndexListPLocalDe){
        for (int j=0; j<arbSeqIndexCount; j++){
          factorIndexList[2*jj] = factorIndexList[2*jj+1] =
            srcArbSeqIndexListPLocalDe[j];
          ++jj;
        }
      }else{
        // multi-dim loop object
        ArrayElement arrayElement(srcArray, i, true, false, false);
        // set up to skip over undistributed, i.e. tensor dimensions
        const int *srcArrayToDistGridMap = srcArray->getArrayToDistGridMap();
        for (int j=0; j<srcArray->getRank(); j++)
          if (srcArrayToDistGridMap[j]==0) arrayElement.setSkipDim(j);
        // fill in the factorIndexList
        while(arrayElement.isWithin()){
          SeqIndex<SIT> seqIndex = arrayElement.getSequenceIndex<SIT>();
          factorIndexList[2*jj] = factorIndexList[2*jj+1] =
            seqIndex.decompSeqIndex;
          ++jj; // increment counter
          arrayElement.next();
        } // end while over all exclusive elements
      }
    }

  }else{
    // srcToDstTransposeMap specified -> transpose mode

    // src and dst Arrays must be of same rank
    int rank = srcArray->getRank();
    if (rank != dstArray->getRank()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "in transpose mode srcArray and dstArray must be of same rank",
        ESMC_CONTEXT, &rc);
      return rc;
    }

    // src and dst Arrays must be have same number of tiles
    int tileCount = srcArray->distgrid->getTileCount();
    if (tileCount != dstArray->distgrid->getTileCount()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "in transpose mode srcArray and dstArray must have same number of"
        " tiles", ESMC_CONTEXT, &rc);
      return rc;
    }

    // check srcToDstTransposeMap input
    if (srcToDstTransposeMap->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "srcToDstTransposeMap must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (srcToDstTransposeMap->extent[0] != rank){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "srcToDstTransposeMap must provide rank values", ESMC_CONTEXT, &rc);
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
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
          "srcToDstTransposeMap values must be unique and within range:"
          " [1,..,rank].", ESMC_CONTEXT, &rc);
        return rc;
      }
    }

    // size of dims in src and dst Arrays must pairwise match in each tile
    const int *srcArrayToDistGridMap = srcArray->getArrayToDistGridMap();
    const int *dstArrayToDistGridMap = dstArray->getArrayToDistGridMap();
    const int *srcMinIndexPDimPTile =
      srcArray->distgrid->getMinIndexPDimPTile();
    const int *dstMinIndexPDimPTile =
      dstArray->distgrid->getMinIndexPDimPTile();
    const int *srcMaxIndexPDimPTile =
      srcArray->distgrid->getMaxIndexPDimPTile();
    const int *dstMaxIndexPDimPTile =
      dstArray->distgrid->getMaxIndexPDimPTile();
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
    int *localStart = new int[tileCount];  // localPet's start index
    int *localSize = new int[tileCount];   // localPet's number of elements
    for (int i=0; i<tileCount; i++){
      int tileFactorListCount = 1;
      // prepare decomposition along last distributed dim in srcArray
      //TODO: this assumes that srcArray has at least one distr. dim
      int lastDimSize = srcMaxIndexPDimPTile[i*srcDimCount+srcDimCount-1]
        - srcMinIndexPDimPTile[i*srcDimCount+srcDimCount-1] + 1;
      int intervalSize = lastDimSize/petCount;
      int extraElements = lastDimSize%petCount;
      localStart[i] = 0;  // initialize
      localStart[i] += localPet * intervalSize;
      localSize[i] = intervalSize;
      if (localPet < extraElements){
        localStart[i] += localPet;
        localSize[i] += 1;
      }
#if 0
fprintf(stderr, "%d start:%d, size:%d\n", localPet, localStart[i], localSize[i]);
#endif
      // check every tile
      int srcTensorIndex = 0;
      int dstTensorIndex = 0;
      for (int jj=0; jj<rank; jj++){
        int srcSize;
        int dstSize;
        int j = srcArrayToDistGridMap[jj];  // j is dimIndex bas 1, or 0 undist.
        if (j){
          // decomposed dimension
          --j;  // shift to basis 0
          srcSize = srcMaxIndexPDimPTile[i*srcDimCount+j]
            - srcMinIndexPDimPTile[i*srcDimCount+j] + 1;
          if (j == srcDimCount-1)
            tileFactorListCount *= localSize[i];
          else
            tileFactorListCount *= srcSize;
        }else{
          // tensor dimension
          srcSize = srcArray->undistUBound[srcTensorIndex]
            - srcArray->undistLBound[srcTensorIndex] + 1;
          ++srcTensorIndex;
          tileFactorListCount *= srcSize;
        }
        int jjj = srcToDstTMap[jj];         // src -> dst dimension mapping
        j = dstArrayToDistGridMap[jjj];     // j is dimIndex bas 1, or 0 undist.
        if (j){
          // decomposed dimension
          --j;  // shift to basis 0
          dstSize = dstMaxIndexPDimPTile[i*dstDimCount+j]
            - dstMinIndexPDimPTile[i*dstDimCount+j] + 1;
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
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "in transpose mode the size of srcArray and dstArray dimensions"
            " must pairwise match", ESMC_CONTEXT, &rc);
          return rc;
        }
      }
      factorListCount += tileFactorListCount;
    }
#if 0
fprintf(stderr, "factorListCount = %d\n", factorListCount);
#endif
    // set up factorIndexList
    srcN = 2; // 2 component seqIndex
    dstN = 2; // 2 component seqIndex
    factorIndexList = new SIT[(srcN+dstN)*factorListCount];
    // prepare to fill in factorIndexList elements
    int factorIndexListIndex = 0; // reset
    int *dstTuple = new int[rank];
    for (int i=0; i<tileCount; i++){
      // initialize multi dim index loop
      vector<int> offsets;
      vector<int> sizes;
      int tensorIndex = 0;  // reset
      for (int jj=0; jj<rank; jj++){
        int j = srcArrayToDistGridMap[jj];  // j is dimIndex bas 1, or 0 undist.
        if (j){
          // decomposed dimension
          --j;  // shift to basis 0
          if (j == srcDimCount-1){
            // share work across PETs for last decomposed dim
            offsets.push_back(localStart[i]);
            sizes.push_back(localSize[i]);
          }else{
            offsets.push_back(0);
            sizes.push_back(srcMaxIndexPDimPTile[i*srcDimCount+j]
              - srcMinIndexPDimPTile[i*srcDimCount+j] + 1);
          }
        }else{
          // tensor dimension
          offsets.push_back(0);
          sizes.push_back(srcArray->undistUBound[tensorIndex]
            - srcArray->undistLBound[tensorIndex] + 1);
          ++tensorIndex;
        }
      }

      MultiDimIndexLoop multiDimIndexLoop(offsets, sizes);
      while(multiDimIndexLoop.isWithin()){
        const int *srcTuple = multiDimIndexLoop.getIndexTuple();
        // redist is identity mapping, but must consider srcToDstTMap
        // srcTuple --(srcToDstTMap)--> dstTuple
        for (int j=0; j<rank; j++)
          dstTuple[srcToDstTMap[j]] = srcTuple[j];
        // determine seq indices
        SeqIndex<SIT> srcSeqIndex =
          srcArray->getSequenceIndexTile<SIT>(i+1, srcTuple);
        SeqIndex<DIT> dstSeqIndex =
          dstArray->getSequenceIndexTile<DIT>(i+1, dstTuple);
        // fill this info into factorIndexList
        int fili = 4*factorIndexListIndex;
        factorIndexList[fili]   = srcSeqIndex.decompSeqIndex;
        factorIndexList[fili+1] = srcSeqIndex.getTensor();
        factorIndexList[fili+2] = dstSeqIndex.decompSeqIndex;
        factorIndexList[fili+3] = dstSeqIndex.getTensor();

        ++factorIndexListIndex;
        multiDimIndexLoop.next();
      } // multi dim index loop
    }
    // garbage collection
    delete [] srcToDstTMap;
    delete [] dstArrayToTensorMap;
    delete [] localStart;
    delete [] localSize;
    delete [] dstTuple;
  }

#if 0
fprintf(stderr, "factorListCount = %d\n", factorListCount);
for (int i=0; i<factorListCount; i++)
  fprintf(stderr, "%d, %d, %d\n", i, factorIndexList[2*i],
    factorIndexList[2*i+1]);
#endif

  // load type specific factorList with "1"
  void *factorList;
  if (typekindFactor == ESMC_TYPEKIND_R4){
    ESMC_R4 *factorListT = new ESMC_R4[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_R4 *)factorLocal;
    factorList = (void *)factorListT;
  }else if (typekindFactor == ESMC_TYPEKIND_R8){
    ESMC_R8 *factorListT = new ESMC_R8[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_R8 *)factorLocal;
    factorList = (void *)factorListT;
  }else if (typekindFactor == ESMC_TYPEKIND_I4){
    ESMC_I4 *factorListT = new ESMC_I4[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_I4 *)factorLocal;
    factorList = (void *)factorListT;
  }else if (typekindFactor == ESMC_TYPEKIND_I8){
    ESMC_I8 *factorListT = new ESMC_I8[factorListCount];
    for (int i=0; i<factorListCount; i++)
      factorListT[i] = *(ESMC_I8 *)factorLocal;
    factorList = (void *)factorListT;
  }

  // prepare SparseMatrix vector
  vector<SparseMatrix<SIT,DIT> > sparseMatrix;
  sparseMatrix.push_back(SparseMatrix<SIT,DIT> (typekindFactor, factorList,
    factorListCount, srcN, dstN, factorIndexList));


  // precompute sparse matrix multiplication
  int srcTermProcessing = 0;  // no need to use auto-tuning to figure this out
  localrc = sparseMatMulStore(srcArray, dstArray, routehandle, sparseMatrix,
    false, ignoreUnmatched, &srcTermProcessing, pipelineDepthArg);
  // garbage collection here, to not cause memory leak when bail on failure
  delete [] factorIndexList;
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
  // error handling
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
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
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  ESMC_CommFlag commflag,               // in    - communication options
  bool *finishedflag,                   // out   - TEST ops finished or not
  bool *cancelledflag,                  // out   - any cancelled operations
  ESMC_Region_Flag zeroflag,            // in    - ESMC_REGION_TOTAL:
                                        //          -> zero out total region
                                        //         ESMC_REGION_SELECT:
                                        //          -> zero out target points
                                        //         ESMC_REGION_EMPTY:
                                        //          -> don't zero out any points
  bool checkflag                        // in    - false: (def.) basic checks
                                        //         true:  full input check
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
  localrc = sparseMatMul(srcArray, dstArray, routehandle,
    commflag, finishedflag, cancelledflag, zeroflag,
    ESMC_TERMORDER_FREE, checkflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

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
  RouteHandle *routehandle        // inout -
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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
template<typename T> bool operator==(SeqIndexTensor<T> a, SeqIndexTensor<T> b){
  if (a.decompSeqIndex != b.decompSeqIndex) return false;
  // decompSeqIndex was equal -> check tensorSeqIndex
  return (a.tensorSeqIndex == b.tensorSeqIndex);
}
template<typename T> bool operator!=(SeqIndexTensor<T> a, SeqIndexTensor<T> b){
  if (a.decompSeqIndex != b.decompSeqIndex) return true;
  // decompSeqIndex was equal -> check tensorSeqIndex
  return (a.tensorSeqIndex != b.tensorSeqIndex);
}
template<typename T> bool operator<(SeqIndexTensor<T> a, SeqIndexTensor<T> b){
  if (a.decompSeqIndex < b.decompSeqIndex) return true;
  if (a.decompSeqIndex > b.decompSeqIndex) return false;
  // decompSeqIndex must be equal
  return (a.tensorSeqIndex < b.tensorSeqIndex);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
template<typename T> bool operator==(SeqIndexLite<T> a, SeqIndexLite<T> b){
  return (a.decompSeqIndex == b.decompSeqIndex);
}
template<typename T> bool operator!=(SeqIndexLite<T> a, SeqIndexLite<T> b){
  return (a.decompSeqIndex != b.decompSeqIndex);
}
template<typename T> bool operator<(SeqIndexLite<T> a, SeqIndexLite<T> b){
  return (a.decompSeqIndex < b.decompSeqIndex);
}
//-----------------------------------------------------------------------------


namespace ArrayHelper{

  template<typename IT> struct Deflator{
    SeqIndex<IT> seqIndex;
    int index;
  };
  template<typename IT> bool operator==(Deflator<IT> a, Deflator<IT> b){
    return (a.seqIndex == b.seqIndex);
  }
  template<typename IT> bool operator!=(Deflator<IT> a, Deflator<IT> b){
    return (a.seqIndex != b.seqIndex);
  }
  template<typename IT> bool operator<(Deflator<IT> a, Deflator<IT> b){
    return (a.seqIndex < b.seqIndex);
  }

#define MSG_DST_CONTIG

  template<typename IT1, typename IT2> struct DstInfo{
    int linIndex;               // if vector element then this is start
    int vectorLength;           // ==1 single element, > 1 vector element
    IT1 seqIndex;               // if vector element then this is start
    IT2 partnerSeqIndex;        // if vector element then this is start
    void *factor;               // if vector element then this factor for all
    int bufferIndex;            // index into the receive buffer
  };
  template<typename IT1, typename IT2>
    bool scalarOrderDstInfo(DstInfo<IT1,IT2> a, DstInfo<IT1,IT2> b){
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
  template<typename IT1, typename IT2>
    bool vectorOrderDstInfo(DstInfo<IT1,IT2> a, DstInfo<IT1,IT2> b){
    if (a.seqIndex.decompSeqIndex == b.seqIndex.decompSeqIndex)
      if (a.partnerSeqIndex.decompSeqIndex == b.partnerSeqIndex.decompSeqIndex)
        return (a.seqIndex.getTensor() < b.seqIndex.getTensor());
      else
        return
          (a.partnerSeqIndex.decompSeqIndex < b.partnerSeqIndex.decompSeqIndex);
    else
      return (a.seqIndex.decompSeqIndex < b.seqIndex.decompSeqIndex);
  }

  template<typename IT1, typename IT2> struct DstInfoSrcSeqSort{
    typename vector<DstInfo<IT1,IT2> >::iterator pp;
    int recvnbVectorIndex;
    int rraIndexListIndex;
    DstInfoSrcSeqSort(typename vector<DstInfo<IT1,IT2> >::iterator pp_,
      int recvnbVectorIndex_, int rraIndexListIndex_){
      pp=pp_;
      recvnbVectorIndex=recvnbVectorIndex_;
      rraIndexListIndex=rraIndexListIndex_;
    }
  };
  template<typename IT1, typename IT2> bool operator<
    (DstInfoSrcSeqSort<IT1,IT2> a, DstInfoSrcSeqSort<IT1,IT2> b){
    // sorting hierarchy: 1) rraIndexListIndex, 2) linIndex, 3) partnerSeqIndex
    // 1) rraIndexListIndex
    if (a.rraIndexListIndex != b.rraIndexListIndex)
      return (a.rraIndexListIndex < b.rraIndexListIndex);
    // 2) linIndex
    if (a.pp->linIndex != b.pp->linIndex)
      return (a.pp->linIndex < b.pp->linIndex);
    // 3) partnerSeqIndex
    return (a.pp->partnerSeqIndex < b.pp->partnerSeqIndex);
  }

  template<typename IT1, typename IT2> struct SrcInfo{
    int linIndex;               // if vector element then this is start
    int vectorLength;           // ==1 single element, > 1 vector element
    IT1 seqIndex;               // if vector element then this is start
    IT2 partnerSeqIndex;        // if vector element then this is start
    void *factor;               // if vector element then this factor for all
  };
  template<typename IT1, typename IT2>
    bool scalarOrderSrcInfo(SrcInfo<IT1,IT2> a, SrcInfo<IT1,IT2> b){
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
  template<typename IT1, typename IT2>
    bool vectorOrderSrcInfo(SrcInfo<IT1,IT2> a, SrcInfo<IT1,IT2> b){
    if (a.partnerSeqIndex.decompSeqIndex == b.partnerSeqIndex.decompSeqIndex)
      if (a.seqIndex.decompSeqIndex == b.seqIndex.decompSeqIndex)
        return
          (a.partnerSeqIndex.getTensor() < b.partnerSeqIndex.getTensor());
      else
        return (a.seqIndex.decompSeqIndex < b.seqIndex.decompSeqIndex);
    else
      return
        (a.partnerSeqIndex.decompSeqIndex < b.partnerSeqIndex.decompSeqIndex);
  }

  template<typename IT1, typename IT2> struct RecvnbElement{
    int srcPet;
    int srcDe;          // global DE index of src DE in src DELayout
    int srcLocalDe;     // local enumeration of srcDe
    int dstDe;          // global DE index of dst DE in local DELayout
    int dstLocalDe;     // local enumeration of dstDe
    char **bufferInfo;  // indirect buffer pointer to support buffer resize
                        // during exec() via BufferInfo structure in XXE
    int partnerDeDataCount;
    int recvnbIndex;
    bool vectorFlag;  // control vectorization
    vector<DstInfo<IT1,IT2> > dstInfoTable;
    int localPet;
    int petCount;
    //
    int appendRecvnb(XXE *xxe, int predicateBitField, int srcTermProcessing,
      int dataSizeSrc, int k);
    int appendRecv(XXE *xxe, int predicateBitField, int srcTermProcessing,
      int dataSizeSrc, int k);
    int appendZeroSuperScalar(XXE *xxe, int predicateBitField,
      int srcLocalDeCount, XXE::TKId elementTK);
    int appendProductSum(XXE *xxe, int predicateBitField, int srcTermProcessing,
      int srcLocalDeCount, XXE::TKId elementTK, XXE::TKId valueTK,
      XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc, int dataSizeFactors,
      char **rraList, int rraCount);
    static int appendSingleProductSum(XXE *xxe,
      int predicateBitField, int srcTermProcessing,  int srcLocalDeCount,
      XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK,
      int dataSizeDst, int dataSizeSrc, int dataSizeFactors, char **rraList,
      int rraCount, vector<RecvnbElement> recvnbVector);
    int appendTestWaitProductSum(XXE *xxe, int predicateBitField,
      int srcTermProcessing, int srcLocalDeCount, XXE::TKId elementTK,
      XXE::TKId valueTK, XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc,
      int dataSizeFactors, char **rraList, int rraCount, int k);
  };
  template<typename IT1, typename IT2> bool operator<
    (RecvnbElement<IT1,IT2> a, RecvnbElement<IT1,IT2> b){
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
  template<typename IT1, typename IT2>
    int RecvnbElement<IT1,IT2>::appendRecvnb(XXE *xxe, int predicateBitField,
    int srcTermProcessing, int dataSizeSrc, int k){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::RecvnbElement::appendRecvnb()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " on localPet=" << localPet <<
        " from Pet=" << srcPet;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    recvnbIndex = xxe->count;  // store index for the associated wait
    int tag = 0;  // no need for special tags - messages are ordered to match
    // determine bufferItemCount according to srcTermProcessing
    int bufferItemCount = 0; // reset
    if (srcTermProcessing == 0)
      bufferItemCount = partnerDeDataCount;
    else{
      typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp =
        dstInfoTable.begin();
      while (pp != dstInfoTable.end()){
        IT1 seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
    }
    // append the recvnb operation
    localrc = xxe->appendRecvnb(predicateBitField, bufferInfo,
      bufferItemCount * dataSizeSrc, srcPet, tag, vectorFlag, true);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
    char *tempString = new char[160];
    sprintf(tempString, "Recvnb: vectorFlag=%d", vectorFlag);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "<(%04d/%04d)-Rnb(%d/%d)-(%04d/%04d)> ",
      srcDe, srcPet, k, recvnbIndex, dstDe, localPet);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "/Recvnb (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
      xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  template<typename IT1, typename IT2>
    int RecvnbElement<IT1,IT2>::appendRecv(XXE *xxe, int predicateBitField,
    int srcTermProcessing, int dataSizeSrc, int k){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::RecvnbElement::appendRecv()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " XXE::recv on localPet="
        << localPet << " from Pet=" << srcPet;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    recvnbIndex = xxe->count;  // store index for the associated wait
    int tag = 0;  // no need for special tags - messages are ordered to match
    // determine bufferItemCount according to srcTermProcessing
    int bufferItemCount = 0; // reset
    if (srcTermProcessing == 0)
      bufferItemCount = partnerDeDataCount;
    else{
      typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp =
        dstInfoTable.begin();
      while (pp != dstInfoTable.end()){
        IT1 seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
    }
    // append the recv operation
    localrc = xxe->appendRecv(predicateBitField, bufferInfo,
      bufferItemCount * dataSizeSrc, srcPet, tag, vectorFlag, true);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
    char *tempString = new char[160];
    sprintf(tempString, "Recv: vectorFlag=%d", vectorFlag);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "<(%04d/%04d)-Rcv(%d/%d)-(%04d/%04d)> ",
      srcDe, srcPet, k, recvnbIndex, dstDe, localPet);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "/Recv (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
      xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  template<typename IT1, typename IT2>
    int RecvnbElement<IT1,IT2>::appendZeroSuperScalar(
    XXE *xxe, int predicateBitField,
    int srcLocalDeCount, XXE::TKId elementTK){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::RecvnbElement::appendZeroSuperScalar()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int j = dstLocalDe;
    int rraIndex = srcLocalDeCount + j; // localDe index into dstArray shifted
                                        // by srcArray localDeCount
#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(predicateBitField, "slct zero",
      xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif
    int xxeIndex = xxe->count;  // need this beyond the increment
    localrc = xxe->appendZeroSuperScalarRRA(predicateBitField, elementTK,
      rraIndex, dstInfoTable.size(), vectorFlag);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    XXE::ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo =
      (XXE::ZeroSuperScalarRRAInfo *)&(xxe->opstream[xxeIndex]);
    // fill rraOffsetList[]
    int vectorLength = dstInfoTable.begin()->vectorLength;  // store time vLen
    int k=0;  // reset
    for (typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp =
      dstInfoTable.begin(); pp != dstInfoTable.end(); ++pp){
      int linIndex = pp->linIndex;
      xxeZeroSuperScalarRRAInfo->rraOffsetList[k] = linIndex/vectorLength;
      ++k;
    }
#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(predicateBitField, "/slct zero",
      xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  template<typename IT1, typename IT2>
    int RecvnbElement<IT1,IT2>::appendProductSum(
    XXE *xxe, int predicateBitField,
    int srcTermProcessing,  int srcLocalDeCount, XXE::TKId elementTK,
    XXE::TKId valueTK, XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc,
    int dataSizeFactors, char **rraList, int rraCount){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::RecvnbElement::appendProductSum()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int j = dstLocalDe;
    int vectorLength = dstInfoTable.begin()->vectorLength;  // store time vLen
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      // use super-scalar "+=*" operation containing all terms
      int rraIndex = srcLocalDeCount + j; // localDe index into dstArray
                                          // shifted by srcArray localDeCount
      int termCount = dstInfoTable.size();
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarDstRRA(predicateBitField,
        elementTK, valueTK, factorTK, rraIndex, termCount, bufferInfo,
        vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      XXE::ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo =
        (XXE::ProductSumSuperScalarDstRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList = xxeProductSumSuperScalarDstRRAInfo->rraOffsetList;
      void *factorList = xxeProductSumSuperScalarDstRRAInfo->factorList;
      int *valueOffsetList =
        xxeProductSumSuperScalarDstRRAInfo->valueOffsetList;
      // fill in rraOffsetList, valueOffsetList
      typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp =
        dstInfoTable.begin();
      for (int kk=0; kk<termCount; kk++){
        rraOffsetList[kk] = pp->linIndex/vectorLength;
        valueOffsetList[kk] = pp->bufferIndex;
        ++pp;
      } // for kk - termCount
      // fill in factorList according to factorTK
      pp = dstInfoTable.begin();
      switch (factorTK){
      case XXE::R4:
        {
          ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
          for (int kk=0; kk<termCount; kk++){
            factorListT[kk] = *(ESMC_R4 *)(pp->factor);
            ++pp;
          } // for kk - termCount
        }
        break;
      case XXE::R8:
        {
          ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
          for (int kk=0; kk<termCount; kk++){
            factorListT[kk] = *(ESMC_R8 *)(pp->factor);
            ++pp;
          } // for kk - termCount
        }
        break;
      case XXE::I4:
        {
          ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
          for (int kk=0; kk<termCount; kk++){
            factorListT[kk] = *(ESMC_I4 *)(pp->factor);
            ++pp;
          } // for kk - termCount
        }
        break;
      case XXE::I8:
        {
          ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
          for (int kk=0; kk<termCount; kk++){
            factorListT[kk] = *(ESMC_I8 *)(pp->factor);
            ++pp;
          } // for kk - termCount
        }
        break;
      default:
        break;
      }
      // need to fill in sensible elements and values or timing will be bogus
      switch (elementTK){ // elements in dstArray
      case XXE::R4:
        for (int kk=0; kk<termCount; kk++){
          ESMC_R4 *element =
            (ESMC_R4 *)rraList[rraIndex] + rraOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(element+k) = (ESMC_R4)0.;
        }
        break;
      case XXE::R8:
        for (int kk=0; kk<termCount; kk++){
          ESMC_R8 *element =
            (ESMC_R8 *)rraList[rraIndex] + rraOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(element+k) = (ESMC_R8)0.;
        }
        break;
      case XXE::I4:
        for (int kk=0; kk<termCount; kk++){
          ESMC_I4 *element =
            (ESMC_I4 *)rraList[rraIndex] + rraOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(element+k) = (ESMC_I4)0;
        }
        break;
      case XXE::I8:
        for (int kk=0; kk<termCount; kk++){
          ESMC_I8 *element =
            (ESMC_I8 *)rraList[rraIndex] + rraOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(element+k) = (ESMC_I8)0;
        }
        break;
      default:
        break;
      }
      char *buffer = *bufferInfo; // access buffer through layer of indirection
      switch (valueTK){ // values in buffer
      case XXE::R4:
        for (int kk=0; kk<termCount; kk++){
          ESMC_R4 *value =
            (ESMC_R4 *)buffer + valueOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(value+k) = (ESMC_R4)0.01;
        }
        break;
      case XXE::R8:
        for (int kk=0; kk<termCount; kk++){
          ESMC_R8 *value =
            (ESMC_R8 *)buffer + valueOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(value+k) = (ESMC_R8)0.01;
        }
        break;
      case XXE::I4:
        for (int kk=0; kk<termCount; kk++){
          ESMC_I4 *value =
            (ESMC_I4 *)buffer + valueOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(value+k) = (ESMC_I4)1;
        }
        break;
      case XXE::I8:
        for (int kk=0; kk<termCount; kk++){
          ESMC_I8 *value =
            (ESMC_I8 *)buffer + valueOffsetList[kk] * vectorLength;
          for (int k=0; k<vectorLength; k++)
            *(value+k) = (ESMC_I8)1;
        }
        break;
      default:
        break;
      }
      xxe->optimizeElement(xxeIndex);

#define MSG_DEFLATE___disable
#define MSG_DEFLATE

#ifdef MSG_DEFLATE
      // cannot use ProductSumSuperScalarContigRRAInfo operation unless the
      // values even in the deflated message are stored in continuous order
      // --> extremely unlikely for a regridding operation, but possible for
      // redist.
      double dt_sScalar = 1.;
      double dt_sScalarC = 2.;  // force dt_sScalar to be picked below
#else
      double dt_sScalar;
      localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
        &dt_sScalar, xxeIndex, xxeIndex);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
      // try super-scalar contig "+=*" operation in XXE opstream
      xxe->opstream[xxeIndex].opId = XXE::productSumSuperScalarContigRRA;
      XXE::ProductSumSuperScalarContigRRAInfo
        *xxeProductSumSuperScalarContigRRAInfo =
        (XXE::ProductSumSuperScalarContigRRAInfo *)&(xxe->opstream[xxeIndex]);
      // only change members that are different wrt super-scalar operation
      xxeProductSumSuperScalarContigRRAInfo->valueList = bufferInfo;
      xxe->optimizeElement(xxeIndex);
      double dt_sScalarC;
      localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
        &dt_sScalarC, xxeIndex, xxeIndex);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
#endif
#ifdef ASMM_EXEC_PROFILE_on
      char *tempString = new char[160];
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
#ifdef ASMM_EXEC_PROFILE_on
        sprintf(tempString, "use productSumSuperScalarDstRRA for %d terms"
          " (diffElements=%d), vectorFlag=%d", termCount, diffElements,
          vectorFlag);
#endif
        xxe->opstream[xxeIndex].opId = XXE::productSumSuperScalarDstRRA;
        xxeProductSumSuperScalarDstRRAInfo->valueBase = bufferInfo;
      }else{
        // use productSumSuperScalarContigRRA
#ifdef ASMM_EXEC_PROFILE_on
        sprintf(tempString, "use productSumSuperScalarContigRRA for %d terms"
          " (diffElements=%d), vectorFlag=%d", termCount, diffElements,
          vectorFlag);
#endif
        // nothing to be done -> already set from last trial
      }
#ifdef ASMM_EXEC_PROFILE_on
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
    }else{
      // do some processing on the src side
      // use super-scalar "+=" operation containing all terms
      int rraIndex = srcLocalDeCount + j; // localDe index into dstArray
                                          // shifted by srcArray localDeCount
      // determine bufferItemCount according to srcTermProcessing
      int bufferItemCount = 0; // reset
      typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp =
        dstInfoTable.begin();
      while (pp != dstInfoTable.end()){
        IT1 seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendSumSuperScalarDstRRA(predicateBitField, elementTK,
        valueTK, rraIndex, bufferItemCount, bufferInfo, vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      XXE::SumSuperScalarDstRRAInfo *xxeSumSuperScalarDstRRAInfo =
        (XXE::SumSuperScalarDstRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList = xxeSumSuperScalarDstRRAInfo->rraOffsetList;
      int *valueOffsetList = xxeSumSuperScalarDstRRAInfo->valueOffsetList;
      // fill in rraOffsetList, valueList
      int bufferItem = 0; // reset
      pp = dstInfoTable.begin();  // reset
      while (pp != dstInfoTable.end()){
        rraOffsetList[bufferItem] = pp->linIndex/vectorLength;
        valueOffsetList[bufferItem] = bufferItem;
        // skip dstInfoTable elements that were summed up on the src side
        IT1 seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == dstInfoTable.end()) || !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++bufferItem;
      }
#ifdef ASMM_EXEC_PROFILE_on
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
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  template<typename IT1, typename IT2>
    int RecvnbElement<IT1,IT2>::appendTestWaitProductSum(
    XXE *xxe, int predicateBitField,
    int srcTermProcessing, int srcLocalDeCount, XXE::TKId elementTK,
    XXE::TKId valueTK, XXE::TKId factorTK, int dataSizeDst, int dataSizeSrc,
    int dataSizeFactors, char **rraList, int rraCount, int k){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::RecvnbElement::appendTestWaitProductSum()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
#ifdef ASMM_EXEC_PROFILE_on
    char *tempString = new char[160];
    sprintf(tempString, "WaitProductSum (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField|XXE::filterBitNbWaitFinish,
      tempString, xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    delete [] tempString;
#endif
    // use subopstream for doing the productSum
    XXE *xxeSub;
    try{
      xxeSub = new XXE(xxe->vm, 10, 10, 10);
    }catch (...){
      ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
      return rc;
    }
    xxeSub->superVectorOkay = xxe->superVectorOkay; // inherit the same Okay
    localrc = xxe->storeXxeSub(xxeSub); // for XXE garbage collection
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = appendProductSum(xxeSub, predicateBitField, srcTermProcessing,
      srcLocalDeCount, elementTK, valueTK, factorTK, dataSizeDst, dataSizeSrc,
      dataSizeFactors, rraList, rraCount);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    // attach to testOnIndexSub element with filterBitNbTestFinish
    localrc = xxe->appendTestOnIndexSub(
      predicateBitField|XXE::filterBitNbTestFinish, xxeSub, 0, 0, recvnbIndex);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    // attach to waitOnIndexSub element with filterBitNbWaitFinish
    localrc = xxe->appendWaitOnIndexSub(
      predicateBitField|XXE::filterBitNbWaitFinish, xxeSub, 0, 0, recvnbIndex);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    // cancel for this receive operations
    localrc = xxe->appendCancelIndex(
      predicateBitField|XXE::filterBitCancel, recvnbIndex);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
    tempString = new char[160];
    sprintf(tempString, "/WaitProductSum (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField|XXE::filterBitNbWaitFinish,
      tempString, xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
  template<typename IT1, typename IT2>
    int RecvnbElement<IT1,IT2>::appendSingleProductSum(
    XXE *xxe,
    int predicateBitField, int srcTermProcessing,  int srcLocalDeCount,
    XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK,
    int dataSizeDst, int dataSizeSrc, int dataSizeFactors, char **rraList,
    int rraCount, vector<RecvnbElement> recvnbVector){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::RecvnbElement::appendSingleProductSum()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    if (recvnbVector.size() < 1){
      // bail out successfully
      rc = ESMF_SUCCESS;
      return rc;
    }
    // use first element in recvnbVector to access info that is the same for
    // all elements
    int vectorLength =
      recvnbVector[0].dstInfoTable.begin()->vectorLength;  // store time vLen
    bool vectorFlag = recvnbVector[0].vectorFlag;
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      // use super-scalar "+=*" operation containing all terms
      // this single productSum operation supports multiple receive buffers:
      // -> construct the necessary helper variables
      int termCount = 0;
      vector<void *>bufferInfoList;
      vector<int> rraIndexList;
      for (unsigned i=0; i<recvnbVector.size(); i++){
        termCount += recvnbVector[i].dstInfoTable.size();
        bufferInfoList.push_back(recvnbVector[i].bufferInfo);
        rraIndexList.push_back(srcLocalDeCount + recvnbVector[i].dstLocalDe);
      }
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarListDstRRA(predicateBitField,
        elementTK, valueTK, factorTK, rraIndexList, termCount, bufferInfoList,
        vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

      XXE::ProductSumSuperScalarListDstRRAInfo
        *xxeProductSumSuperScalarListDstRRAInfo =
        (XXE::ProductSumSuperScalarListDstRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList =
        xxeProductSumSuperScalarListDstRRAInfo->rraOffsetList;
      void *factorList = xxeProductSumSuperScalarListDstRRAInfo->factorList;
      int *valueOffsetList =
        xxeProductSumSuperScalarListDstRRAInfo->valueOffsetList;
      int *baseListIndexList =
        xxeProductSumSuperScalarListDstRRAInfo->baseListIndexList;
      // set up a temporary vector for sorting for TERMORDER_SRCSEQ
      vector<DstInfoSrcSeqSort<IT1,IT2> > dstInfoSort;
      typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp;
      for (unsigned i=0; i<recvnbVector.size(); i++){
        // append terms from buffer "i"
        for (pp=recvnbVector[i].dstInfoTable.begin();
          pp!=recvnbVector[i].dstInfoTable.end(); ++pp){
          dstInfoSort.push_back(DstInfoSrcSeqSort<IT1,IT2>(pp, i,
            rraIndexList[i]));
        }
      }
      // do the actual sort
      sort(dstInfoSort.begin(), dstInfoSort.end());
      // fill in rraOffsetList, valueOffsetList, baseListIndexList
      for (unsigned i=0; i<dstInfoSort.size(); i++){
        rraOffsetList[i] = dstInfoSort[i].pp->linIndex/vectorLength;
        valueOffsetList[i] = dstInfoSort[i].pp->bufferIndex;
        baseListIndexList[i] = dstInfoSort[i].recvnbVectorIndex;
      }
      // fill in factorList according to factorTK
      switch (factorTK){
      case XXE::R4:
        {
          ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
          for (unsigned i=0; i<dstInfoSort.size(); i++)
            factorListT[i] = *(ESMC_R4 *)(dstInfoSort[i].pp->factor);
        }
        break;
      case XXE::R8:
        {
          ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
          for (unsigned i=0; i<dstInfoSort.size(); i++)
            factorListT[i] = *(ESMC_R8 *)(dstInfoSort[i].pp->factor);
        }
        break;
      case XXE::I4:
        {
          ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
          for (unsigned i=0; i<dstInfoSort.size(); i++)
            factorListT[i] = *(ESMC_I4 *)(dstInfoSort[i].pp->factor);
        }
        break;
      case XXE::I8:
        {
          ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
          for (unsigned i=0; i<dstInfoSort.size(); i++)
            factorListT[i] = *(ESMC_I8 *)(dstInfoSort[i].pp->factor);
        }
        break;
      default:
        break;
      }
    }else{
      // do some processing on the src side
      // use super-scalar "+=" operation containing all terms
      // this single productSum operation supports multiple receive buffers:
      // -> construct the necessary helper variables

      //TODO: need to fix this implementation to work with srcTermProcessing > 1

      unsigned bufferItemCount = 0;
      vector<void *>bufferInfoList;
      vector<int> rraIndexList;
      typename vector<ArrayHelper::DstInfo<IT1,IT2> >::iterator pp;
      for (unsigned i=0; i<recvnbVector.size(); i++){
        pp = recvnbVector[i].dstInfoTable.begin();
        while (pp != recvnbVector[i].dstInfoTable.end()){
          IT1 seqIndex = pp->seqIndex;
          for (int term=0; term<srcTermProcessing; term++){
            ++pp;
            if ((pp == recvnbVector[i].dstInfoTable.end())
            || !(seqIndex == pp->seqIndex)) break;
          } // for srcTermProcessing
          ++bufferItemCount;
        }
        bufferInfoList.push_back(recvnbVector[i].bufferInfo);
        rraIndexList.push_back(srcLocalDeCount + recvnbVector[i].dstLocalDe);
      }
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendSumSuperScalarListDstRRA(predicateBitField,
        elementTK, valueTK, rraIndexList, bufferItemCount, bufferInfoList,
        vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

      XXE::SumSuperScalarListDstRRAInfo
        *xxeSumSuperScalarListDstRRAInfo =
        (XXE::SumSuperScalarListDstRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList =
        xxeSumSuperScalarListDstRRAInfo->rraOffsetList;
      int *valueOffsetList =
        xxeSumSuperScalarListDstRRAInfo->valueOffsetList;
      int *baseListIndexList =
        xxeSumSuperScalarListDstRRAInfo->baseListIndexList;
      // set up a temporary vector for sorting for TERMORDER_SRCSEQ
      vector<DstInfoSrcSeqSort<IT1,IT2> > dstInfoSort;
      for (unsigned i=0; i<recvnbVector.size(); i++){
        // append terms from buffer "i"
        int bufferItem = 0; // reset
        pp = recvnbVector[i].dstInfoTable.begin();
        while (pp != recvnbVector[i].dstInfoTable.end()){
          dstInfoSort.push_back(DstInfoSrcSeqSort<IT1,IT2>(pp, i,
            rraIndexList[i]));
          pp->bufferIndex = bufferItem; // adjust to modified buffer structure
          IT1 seqIndex = pp->seqIndex;
          for (int term=0; term<srcTermProcessing; term++){
            ++pp;
            if ((pp == recvnbVector[i].dstInfoTable.end())
            || !(seqIndex == pp->seqIndex)) break;
          } // for srcTermProcessing
          ++bufferItem;
        }
      }
      // sanity check to ensure srcTermProcessing was correctly considered
      if (bufferItemCount != dstInfoSort.size()) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
          "inconsistent number of buffer elements", ESMC_CONTEXT, &rc);
        return rc;  // bail out
      }
      // do the actual sort
      sort(dstInfoSort.begin(), dstInfoSort.end());
      // fill in rraOffsetList, valueOffsetList, baseListIndexList
      for (unsigned i=0; i<dstInfoSort.size(); i++){
        rraOffsetList[i] = dstInfoSort[i].pp->linIndex/vectorLength;
        valueOffsetList[i] = dstInfoSort[i].pp->bufferIndex;
        baseListIndexList[i] = dstInfoSort[i].recvnbVectorIndex;
      }
    }
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }


  struct LinIndexContigBlock{
    int linIndex;
    int linIndexCount;
  };
  template<typename IT1, typename IT2> struct SendnbElement{
    int dstPet;
    int dstDe;          // global DE index of dst DE in dst DELayout
    int dstLocalDe;     // local enumeration of dstDe
    int srcDe;          // global DE index of src DE in local DELayout
    int srcLocalDe;     // local enumeration of srcDe
    char **bufferInfo;  // indirect buffer pointer to support buffer resize
                        // during exec() via BufferInfo structure in XXE
    int partnerDeDataCount;
    int sendnbIndex;
    bool vectorFlag;  // control vectorization
    int vectorLength;
    vector<SrcInfo<IT1, IT2> > srcInfoTable;
    vector<LinIndexContigBlock> linIndexContigBlockList;
    int localPet;
    int petCount;
    //
    int appendSendnb(XXE *xxe, int predicateBitField, int srcTermProcessing,
      XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK,
      int dataSizeSrc, char **rraList, int rraCount, int k);
    int appendSend(XXE *xxe, int predicateBitField, int srcTermProcessing,
      XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK,
      int dataSizeSrc, char **rraList, int rraCount, int k);
    int appendSendRecv(XXE *xxe, int predicateBitField, int srcTermProcessing,
      XXE::TKId elementTK, XXE::TKId valueTK, XXE::TKId factorTK,
      int dataSizeSrc, char **rraList, int rraCount, int kSend,
      typename vector<RecvnbElement<IT2,IT1> >::iterator pRecv, int kRecv);
  };
  template<typename IT1, typename IT2>
    bool operator<(SendnbElement<IT1,IT2> a, SendnbElement<IT1,IT2> b){
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
  template<typename IT1, typename IT2>
    int SendnbElement<IT1,IT2>::appendSendnb(XXE *xxe, int predicateBitField,
    int srcTermProcessing, XXE::TKId elementTK, XXE::TKId valueTK,
    XXE::TKId factorTK, int dataSizeSrc, char **rraList, int rraCount, int k){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::SendnbElement::appendSendnb()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int tag = 0;  // no need for special tags - messages are ordered to match
    int j = srcLocalDe;
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      int count = linIndexContigBlockList.size();
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ << " from localPet=" << localPet <<
      " to Pet=" << dstPet << " count=" << count;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
      if (false){
//      if (count == 1){  // gjt: had to comment out in order to support
// super-vertorization. Even if under store-conditions there is a contiguous
// data block in the srcArray, this may not be the case for super-vectorized
// case at run-time. The safe way is to _always_ use a buffer: memGatherSrcRRA.
        // sendnbRRA out of single contiguous linIndex run
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ <<
      " single contiguous linIndex run on src side";
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
        sendnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendnbRRA(predicateBitField,
          linIndexContigBlockList[0].linIndex/vectorLength * dataSizeSrc,
          partnerDeDataCount * dataSizeSrc, dstPet, j, tag, vectorFlag);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
        char *tempString = new char[160];
        sprintf(tempString, "Contiguous send: vectorFlag=%d", vectorFlag);
        localrc = xxe->appendProfileMessage(predicateBitField, tempString);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        delete [] tempString;
#endif
      }else{
        // use intermediate buffer
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ <<
      " non-contiguous linIndex on src side -> need buffer";
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
        // use intermediate buffer
        // memGatherSrcRRA pieces into intermediate buffer
        int xxeIndex = xxe->count;  // need this beyond the increment
        localrc = xxe->appendMemGatherSrcRRA(predicateBitField, bufferInfo,
          valueTK, j, count, vectorFlag, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        XXE::MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo =
          (XXE::MemGatherSrcRRAInfo *) &(xxe->opstream[xxeIndex]);
        // try typekind specific memGatherSrcRRA
        for (int kk=0; kk<count; kk++){
          xxeMemGatherSrcRRAInfo->rraOffsetList[kk] =
            linIndexContigBlockList[kk].linIndex/vectorLength;
          xxeMemGatherSrcRRAInfo->countList[kk] =
            linIndexContigBlockList[kk].linIndexCount;
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ << " countList[]=" <<
      xxeMemGatherSrcRRAInfo->countList[kk];
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
        }
        double dt_tk;
        localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
          &dt_tk, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // try byte option for memGatherSrcRRA
        xxeMemGatherSrcRRAInfo->dstBaseTK = XXE::BYTE;
        for (int kk=0; kk<count; kk++){
          // scale to byte
          xxeMemGatherSrcRRAInfo->rraOffsetList[kk] *= dataSizeSrc;
          xxeMemGatherSrcRRAInfo->countList[kk] *= dataSizeSrc;
        }
        double dt_byte;
        localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
          &dt_byte, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " on localPet=" << localPet <<
        " memGatherSrcRRA took dt_tk=" << dt_tk << "s and dt_byte=" <<
        dt_byte << " for count=" << count;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        // decide for the fastest option
//gjt-hack: force typekind option for now        if (dt_byte < dt_tk){
        if (false){
          // use byte option for memGatherSrcRRA
          // -> nothing to do because this was the last mode tested
        }else{
          // use typekind specific memGatherSrcRRA
          xxeMemGatherSrcRRAInfo->dstBaseTK = valueTK;
          for (int k=0; k<count; k++){
            // return to element units
            xxeMemGatherSrcRRAInfo->rraOffsetList[k] =
              linIndexContigBlockList[k].linIndex/vectorLength;
            xxeMemGatherSrcRRAInfo->countList[k] =
              linIndexContigBlockList[k].linIndexCount;
          }
        }
#ifdef ASMM_EXEC_PROFILE_on
        char *tempString = new char[160];
        sprintf(tempString, "MemGatherSrcRRA: vectorFlag=%d", vectorFlag);
        localrc = xxe->appendProfileMessage(predicateBitField, tempString);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        sprintf(tempString, "/MemGatherSrcRRA (%d/)", k);
        localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
          xxe->count);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        delete [] tempString;
#endif
        // sendnb out of contiguous intermediate buffer
        sendnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendnb(predicateBitField, bufferInfo,
          partnerDeDataCount * dataSizeSrc, dstPet, tag, vectorFlag, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      }
    }else{
      // do some processing on the src side
      // determine bufferItemCount according to srcTermProcessing
      int bufferItemCount = 0; // reset
      typename vector<ArrayHelper::SrcInfo<IT1,IT2> >::iterator pp =
        srcInfoTable.begin();
      while (pp != srcInfoTable.end()){
        IT2 partnerSeqIndex = pp->partnerSeqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == srcInfoTable.end()) ||
            !(partnerSeqIndex == pp->partnerSeqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
      // zero out intermediate buffer
      localrc = xxe->appendZeroMemset(predicateBitField, bufferInfo,
        bufferItemCount * dataSizeSrc, vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
      char *tempString = new char[160];
      sprintf(tempString, "/ZeroVector (%d/)", k);
      localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
        xxe->count);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
      // use super-scalar "+=*" operation containing all terms
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarSrcRRA(predicateBitField,
        valueTK, valueTK, factorTK, j, srcInfoTable.size(), bufferInfo,
        vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      XXE::ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo =
        (XXE::ProductSumSuperScalarSrcRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList = xxeProductSumSuperScalarSrcRRAInfo->rraOffsetList;
      void *factorList = xxeProductSumSuperScalarSrcRRAInfo->factorList;
      int *elementOffsetList =
        xxeProductSumSuperScalarSrcRRAInfo->elementOffsetList;
      // fill in rraOffsetList, factorList, elementOffsetList
      int bufferItem = 0; // reset
      int kk = 0; // reset
      pp = srcInfoTable.begin();  // reset
      switch (factorTK){
      case XXE::R4:
        {
          ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_R4 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::R8:
        {
          ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_R8 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::I4:
        {
          ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_I4 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::I8:
        {
          ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_I8 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      default:
        break;
      }
#ifdef ASMM_EXEC_PROFILE_on
      tempString = new char[160];
      sprintf(tempString, "use productSumSuperScalarSrcRRA for termCount=%d"
        " -> reducing it to %d terms", srcInfoTable.size(), bufferItem);
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      sprintf(tempString, "productSumSuperScalarSrcRRA: vectorLength=%d",
        vectorLength);
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      sprintf(tempString, "/PSSSSrcRRA (%d/)", k);
      localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
        xxe->count);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
      // sendnb out of contiguous intermediate buffer
      sendnbIndex = xxe->count;  // store index for the associated wait
      localrc = xxe->appendSendnb(predicateBitField, bufferInfo,
        bufferItemCount * dataSizeSrc, dstPet, tag, vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    }
#ifdef ASMM_EXEC_PROFILE_on
    char *tempString = new char[160];
    sprintf(tempString, "<(%04d/%04d)-Snb(%d/%d)-(%04d/%04d)> ",
      srcDe, localPet, k, sendnbIndex, dstDe, dstPet);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "/Sendnb (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
      xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  template<typename IT1, typename IT2>
    int SendnbElement<IT1,IT2>::appendSend(XXE *xxe, int predicateBitField,
    int srcTermProcessing, XXE::TKId elementTK, XXE::TKId valueTK,
    XXE::TKId factorTK, int dataSizeSrc, char **rraList, int rraCount, int k){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::SendnbElement::appendSend()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int tag = 0;  // no need for special tags - messages are ordered to match
    int j = srcLocalDe;
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      int count = linIndexContigBlockList.size();
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " XXE::send from localPet="
        << localPet << " to Pet=" << dstPet;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
      if (false){
//      if (count == 1){  // gjt: had to comment out in order to support
// super-vertorization. Even if under store-conditions there is a contiguous
// data block in the srcArray, this may not be the case for super-vectorized
// case at run-time. The safe way is to _always_ use a buffer: memGatherSrcRRA.
        // sendRRA out of single contiguous linIndex run
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " single contiguous linIndex "
        "run on src side";
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        sendnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendRRA(predicateBitField,
          linIndexContigBlockList[0].linIndex/vectorLength * dataSizeSrc,
          partnerDeDataCount * dataSizeSrc, dstPet, j, tag, vectorFlag);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
        char *tempString = new char[160];
        sprintf(tempString, "Contiguous send: vectorFlag=%d", vectorFlag);
        localrc = xxe->appendProfileMessage(predicateBitField, tempString);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        delete [] tempString;
#endif
      }else{
        // use intermediate buffer
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " non-contiguous linIndex on "
        "src side -> need buffer";
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        // use intermediate buffer
        // memGatherSrcRRA pieces into intermediate buffer
        int xxeIndex = xxe->count;  // need this beyond the increment
        localrc = xxe->appendMemGatherSrcRRA(predicateBitField, bufferInfo,
          valueTK, j, count, vectorFlag, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        XXE::MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo =
          (XXE::MemGatherSrcRRAInfo *) &(xxe->opstream[xxeIndex]);
        // try typekind specific memGatherSrcRRA
        for (int kk=0; kk<count; kk++){
          xxeMemGatherSrcRRAInfo->rraOffsetList[kk] =
            linIndexContigBlockList[kk].linIndex/vectorLength;
          xxeMemGatherSrcRRAInfo->countList[kk] =
            linIndexContigBlockList[kk].linIndexCount;
        }
        double dt_tk;
        localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
          &dt_tk, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // try byte option for memGatherSrcRRA
        xxeMemGatherSrcRRAInfo->dstBaseTK = XXE::BYTE;
        for (int kk=0; kk<count; kk++){
          // scale to byte
          xxeMemGatherSrcRRAInfo->rraOffsetList[kk] *= dataSizeSrc;
          xxeMemGatherSrcRRAInfo->countList[kk] *= dataSizeSrc;
        }
        double dt_byte;
        localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
          &dt_byte, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " on localPet=" << localPet <<
          " memGatherSrcRRA took dt_tk=" << dt_tk << "s and dt_byte=" <<
          dt_byte << " for count=" << count;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        // decide for the fastest option
//gjt-hack: force typekind option for now        if (dt_byte < dt_tk){
        if (false){
          // use byte option for memGatherSrcRRA
          // -> nothing to do because this was the last mode tested
        }else{
          // use typekind specific memGatherSrcRRA
          xxeMemGatherSrcRRAInfo->dstBaseTK = valueTK;
          for (int k=0; k<count; k++){
            // return to element units
            xxeMemGatherSrcRRAInfo->rraOffsetList[k] =
              linIndexContigBlockList[k].linIndex/vectorLength;
            xxeMemGatherSrcRRAInfo->countList[k] =
              linIndexContigBlockList[k].linIndexCount;
          }
        }
#ifdef ASMM_EXEC_PROFILE_on
        char *tempString = new char[160];
        sprintf(tempString, "MemGatherSrcRRA: vectorFlag=%d", vectorFlag);
        localrc = xxe->appendProfileMessage(predicateBitField, tempString);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        sprintf(tempString, "/MemGatherSrcRRA (%d/)", k);
        localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
          xxe->count);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        delete [] tempString;
#endif
        // send out of contiguous intermediate buffer
        sendnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSend(predicateBitField, bufferInfo,
          partnerDeDataCount * dataSizeSrc, dstPet, tag, vectorFlag, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      }
    }else{
      // do some processing on the src side
      // determine bufferItemCount according to srcTermProcessing
      int bufferItemCount = 0; // reset
      typename vector<ArrayHelper::SrcInfo<IT1,IT2> >::iterator pp =
        srcInfoTable.begin();
      while (pp != srcInfoTable.end()){
        IT2 partnerSeqIndex = pp->partnerSeqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == srcInfoTable.end()) ||
            !(partnerSeqIndex == pp->partnerSeqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
      // zero out intermediate buffer
      localrc = xxe->appendZeroMemset(predicateBitField, bufferInfo,
        bufferItemCount * dataSizeSrc, vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
      char *tempString = new char[160];
      sprintf(tempString, "/ZeroVector (%d/)", k);
      localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
        xxe->count);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
      // use super-scalar "+=*" operation containing all terms
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarSrcRRA(predicateBitField,
        valueTK, valueTK, factorTK, j, srcInfoTable.size(), bufferInfo,
        vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      XXE::ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo =
        (XXE::ProductSumSuperScalarSrcRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList = xxeProductSumSuperScalarSrcRRAInfo->rraOffsetList;
      void *factorList = xxeProductSumSuperScalarSrcRRAInfo->factorList;
      int *elementOffsetList =
        xxeProductSumSuperScalarSrcRRAInfo->elementOffsetList;
      // fill in rraOffsetList, factorList, elementOffsetList
      int bufferItem = 0; // reset
      int kk = 0; // reset
      pp = srcInfoTable.begin();  // reset
      switch (factorTK){
      case XXE::R4:
        {
          ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_R4 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::R8:
        {
          ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_R8 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::I4:
        {
          ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_I4 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::I8:
        {
          ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_I8 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      default:
        break;
      }
#ifdef ASMM_EXEC_PROFILE_on
      tempString = new char[160];
      sprintf(tempString, "use productSumSuperScalarSrcRRA for termCount=%d"
        " -> reducing it to %d terms", srcInfoTable.size(), bufferItem);
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      sprintf(tempString, "productSumSuperScalarSrcRRA: vectorLength=%d",
        vectorLength);
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      sprintf(tempString, "/PSSSSrcRRA (%d/)", k);
      localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
        xxe->count);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
      // send out of contiguous intermediate buffer
      sendnbIndex = xxe->count;  // store index for the associated wait
      localrc = xxe->appendSend(predicateBitField, bufferInfo,
        bufferItemCount * dataSizeSrc, dstPet, tag, vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    }
#ifdef ASMM_EXEC_PROFILE_on
    char *tempString = new char[160];
    sprintf(tempString, "<(%04d/%04d)-Snb(%d/%d)-(%04d/%04d)> ",
      srcDe, localPet, k, sendnbIndex, dstDe, dstPet);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "/Send (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
      xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    delete [] tempString;
#endif
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  template<typename IT1, typename IT2>
    int SendnbElement<IT1,IT2>::appendSendRecv(
    XXE *xxe, int predicateBitField,
    int srcTermProcessing, XXE::TKId elementTK, XXE::TKId valueTK,
    XXE::TKId factorTK, int dataSizeSrc, char **rraList, int rraCount,
    int kSend, typename vector<RecvnbElement<IT2,IT1> >::iterator pRecv,
    int kRecv){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayHelper::SendnbElement::appendSendRecv()"
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code
    int tag = 0;  // no need for special tags - messages are ordered to match
    int j = srcLocalDe;
    // determine recv side bufferItemCount according to srcTermProcessing
    int dstBufferItemCount = 0; // reset
    if (srcTermProcessing == 0)
      dstBufferItemCount = pRecv->partnerDeDataCount;
    else{
      typename vector<ArrayHelper::DstInfo<IT2,IT1> >::iterator pp =
        pRecv->dstInfoTable.begin();
      while (pp != pRecv->dstInfoTable.end()){
        IT1 seqIndex = pp->seqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == pRecv->dstInfoTable.end()) ||
            !(seqIndex == pp->seqIndex)) break;
        } // for srcTermProcessing
        ++dstBufferItemCount;
      }
    }
    if (srcTermProcessing==0){
      // do all the processing on the dst side
      int count = linIndexContigBlockList.size();
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " XXE::sendrecv from localPet="
        << localPet << " to Pet=" << dstPet;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
      if (false){
//      if (count == 1){  // gjt: had to comment out in order to support
// super-vertorization. Even if under store-conditions there is a contiguous
// data block in the srcArray, this may not be the case for super-vectorized
// case at run-time. The safe way is to _always_ use a buffer: memGatherSrcRRA.
        // sendRRArecv out of single contiguous linIndex run
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " single contiguous linIndex "
        "run on src side";
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        sendnbIndex = xxe->count;  // store index for the associated wait
        pRecv->recvnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendRRARecv(predicateBitField,
          linIndexContigBlockList[0].linIndex/vectorLength * dataSizeSrc,
          pRecv->bufferInfo, partnerDeDataCount * dataSizeSrc,
          dstBufferItemCount * dataSizeSrc, pRecv->srcPet, dstPet,
          j, tag, tag, vectorFlag, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
        char *tempString = new char[160];
        sprintf(tempString, "Contiguous sendrecv: vectorFlag=%d", vectorFlag);
        localrc = xxe->appendProfileMessage(predicateBitField, tempString);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        delete [] tempString;
#endif
      }else{
        // use intermediate buffer
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " non-contiguous linIndex on "
          "src side -> need buffer";
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        // use intermediate buffer
        // memGatherSrcRRA pieces into intermediate buffer
        int xxeIndex = xxe->count;  // need this beyond the increment
        localrc = xxe->appendMemGatherSrcRRA(predicateBitField, bufferInfo,
          valueTK, j, count, vectorFlag, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        XXE::MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo =
          (XXE::MemGatherSrcRRAInfo *) &(xxe->opstream[xxeIndex]);
        // try typekind specific memGatherSrcRRA
        for (int kk=0; kk<count; kk++){
          xxeMemGatherSrcRRAInfo->rraOffsetList[kk] =
            linIndexContigBlockList[kk].linIndex/vectorLength;
          xxeMemGatherSrcRRAInfo->countList[kk] =
            linIndexContigBlockList[kk].linIndexCount;
        }
        double dt_tk;
        localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
          &dt_tk, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        // try byte option for memGatherSrcRRA
        xxeMemGatherSrcRRAInfo->dstBaseTK = XXE::BYTE;
        for (int kk=0; kk<count; kk++){
          // scale to byte
          xxeMemGatherSrcRRAInfo->rraOffsetList[kk] *= dataSizeSrc;
          xxeMemGatherSrcRRAInfo->countList[kk] *= dataSizeSrc;
        }
        double dt_byte;
        localrc = xxe->exec(rraCount, rraList, &vectorLength, 0x0, NULL, NULL,
          &dt_byte, xxeIndex, xxeIndex);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " on localPet " << localPet <<
        " memGatherSrcRRA took dt_tk=" << dt_tk << "s and dt_byte=" <<
        dt_byte << "byte for count=" << count;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
        // decide for the fastest option
//gjt-hack: force typekind option for now        if (dt_byte < dt_tk){
        if (false){
          // use byte option for memGatherSrcRRA
          // -> nothing to do because this was the last mode tested
        }else{
          // use typekind specific memGatherSrcRRA
          xxeMemGatherSrcRRAInfo->dstBaseTK = valueTK;
          for (int k=0; k<count; k++){
            // return to element units
            xxeMemGatherSrcRRAInfo->rraOffsetList[k] =
              linIndexContigBlockList[k].linIndex/vectorLength;
            xxeMemGatherSrcRRAInfo->countList[k] =
              linIndexContigBlockList[k].linIndexCount;
          }
        }
#ifdef ASMM_EXEC_PROFILE_on
        char *tempString = new char[160];
        sprintf(tempString, "MemGatherSrcRRA: vectorFlag=%d", vectorFlag);
        localrc = xxe->appendProfileMessage(predicateBitField, tempString);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        sprintf(tempString, "/MemGatherSrcRRA (%d/)", k);
        localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
          xxe->count);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        delete [] tempString;
#endif
        // sendrecv out of contiguous intermediate buffer
        sendnbIndex = xxe->count;  // store index for the associated wait
        pRecv->recvnbIndex = xxe->count;  // store index for the associated wait
        localrc = xxe->appendSendRecv(predicateBitField, bufferInfo,
          pRecv->bufferInfo, partnerDeDataCount * dataSizeSrc,
          dstBufferItemCount * dataSizeSrc, pRecv->srcPet, dstPet, tag, tag,
          vectorFlag, true, true);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      }
    }else{
      // do some processing on the src side
      // determine bufferItemCount according to srcTermProcessing
      int bufferItemCount = 0; // reset
      typename vector<ArrayHelper::SrcInfo<IT1,IT2> >::iterator pp =
        srcInfoTable.begin();
      while (pp != srcInfoTable.end()){
        IT2 partnerSeqIndex = pp->partnerSeqIndex;
        for (int term=0; term<srcTermProcessing; term++){
          ++pp;
          if ((pp == srcInfoTable.end()) ||
            !(partnerSeqIndex == pp->partnerSeqIndex)) break;
        } // for srcTermProcessing
        ++bufferItemCount;
      }
      // zero out intermediate buffer
      localrc = xxe->appendZeroMemset(predicateBitField, bufferInfo,
        bufferItemCount * dataSizeSrc, vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
      char *tempString = new char[160];
      sprintf(tempString, "/ZeroVector (%d/)", k);
      localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
        xxe->count);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
      // use super-scalar "+=*" operation containing all terms
      int xxeIndex = xxe->count;  // need this beyond the increment
      localrc = xxe->appendProductSumSuperScalarSrcRRA(predicateBitField,
        valueTK, valueTK, factorTK, j, srcInfoTable.size(), bufferInfo,
        vectorFlag, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      XXE::ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo =
        (XXE::ProductSumSuperScalarSrcRRAInfo *)&(xxe->opstream[xxeIndex]);
      int *rraOffsetList = xxeProductSumSuperScalarSrcRRAInfo->rraOffsetList;
      void *factorList = xxeProductSumSuperScalarSrcRRAInfo->factorList;
      int *elementOffsetList =
        xxeProductSumSuperScalarSrcRRAInfo->elementOffsetList;
      // fill in rraOffsetList, factorList, elementOffsetList
      int bufferItem = 0; // reset
      int kk = 0; // reset
      pp = srcInfoTable.begin();  // reset
      switch (factorTK){
      case XXE::R4:
        {
          ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_R4 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::R8:
        {
          ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_R8 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::I4:
        {
          ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_I4 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      case XXE::I8:
        {
          ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
          while (pp != srcInfoTable.end()){
            IT2 partnerSeqIndex = pp->partnerSeqIndex;
            for (int term=0; term<srcTermProcessing; term++){
              rraOffsetList[kk] = pp->linIndex/vectorLength;
              factorListT[kk] = *(ESMC_I8 *)(pp->factor);
              elementOffsetList[kk] = bufferItem;
              ++pp;
              ++kk;
              if ((pp == srcInfoTable.end()) ||
                !(partnerSeqIndex == pp->partnerSeqIndex)) break;
            } // for srcTermProcessing
            ++bufferItem;
          }
        }
        break;
      default:
        break;
      }
#ifdef ASMM_EXEC_PROFILE_on
      tempString = new char[160];
      sprintf(tempString, "use productSumSuperScalarSrcRRA for termCount=%d"
        " -> reducing it to %d terms", srcInfoTable.size(), bufferItem);
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      sprintf(tempString, "productSumSuperScalarSrcRRA: vectorLength=%d",
        vectorLength);
      localrc = xxe->appendProfileMessage(predicateBitField, tempString);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      sprintf(tempString, "/PSSSSrcRRA (%d/)", k);
      localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
        xxe->count);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      delete [] tempString;
#endif
      // sendrecv out of contiguous intermediate buffer
      sendnbIndex = xxe->count;  // store index for the associated wait
      pRecv->recvnbIndex = xxe->count;  // store index for the associated wait
      localrc = xxe->appendSendRecv(predicateBitField, bufferInfo,
        pRecv->bufferInfo, bufferItemCount * dataSizeSrc,
        dstBufferItemCount * dataSizeSrc, pRecv->srcPet, dstPet, tag, tag,
        vectorFlag, true, true);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    }
#ifdef ASMM_EXEC_PROFILE_on
    char *tempString = new char[160];
    sprintf(tempString, "<(%04d/%04d)-Snb(%d/%d)-(%04d/%04d)> ",
      srcDe, localPet, k, sendnbIndex, dstDe, dstPet);
    localrc = xxe->appendProfileMessage(predicateBitField, tempString);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    sprintf(tempString, "/Send (%d/)", k);
    localrc = xxe->appendWtimer(predicateBitField, tempString, xxe->count,
      xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
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
      if (bytes > limit){
        MemHelper *mh = this; // initialize to first element in list
        while (mh->next) mh = mh->next; // find last element in list
        mh->next = new MemHelper();
        mh = mh->next;
        mh->memAlloc = std::malloc(bytes);
        mh->memPtr = (char *)(mh->memAlloc);
        return mh->memAlloc;
      }
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


// FactorElement
template<typename IT> struct FactorElement{
  char factor[8]; // large enough for R8 and I8
  IT partnerSeqIndex;
#if (SMMSLSQV_OPTION==2 || SMMSLSQV_OPTION==3)
  int partnerDe;
#endif
#if (SMMSLSQV_OPTION==1 || SMMSLSQV_OPTION==2)
  vector<int> partnerDE;  //TODO: remove this once 
  // sparseMatMulStoreLinSeqVect.h has been reworked or removed!!!!
#endif
};
template<typename IT>
  bool operator==(FactorElement<IT> a, FactorElement<IT> b){
  return (a.partnerSeqIndex == b.partnerSeqIndex);
}
template<typename IT>
  bool operator!=(FactorElement<IT> a, FactorElement<IT> b){
  return (a.partnerSeqIndex != b.partnerSeqIndex);
}
template<typename IT>
  bool operator<(FactorElement<IT> a, FactorElement<IT> b){
  return (a.partnerSeqIndex < b.partnerSeqIndex);
}

// AssociationElement
template<typename IT1, typename IT2> struct AssociationElement{
  vector<FactorElement<IT2> > factorList;
  IT1 seqIndex;
  int linIndex;
};
template<typename IT1, typename IT2>
  bool operator<(AssociationElement<IT1,IT2> a,AssociationElement<IT1,IT2> b){
  return (a.seqIndex < b.seqIndex);
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::sparseMatMulStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::sparseMatMulStore
//
// !INTERFACE:
template<typename SIT, typename DIT>
  int Array::sparseMatMulStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                          // in    - source Array
  Array *dstArray,                          // in    - destination Array
  RouteHandle **routehandle,                // inout - handle to precomp. comm
  vector<SparseMatrix<SIT,DIT> > const &sparseMatrix,// in- sparse matrix vector
  bool haloFlag,                            // in    - support halo conditions
  bool ignoreUnmatched,                     // in    - support unmatched indices
  int *srcTermProcessingArg,                // inout - src term proc (optional)
                                // if (NULL) -> auto-tune, no pass back
                                // if (!NULL && -1) -> auto-tune, pass back
                                // if (!NULL && >=0) -> no auto-tune, use input
  int *pipelineDepthArg                     // inout - pipeline depth (optional)
                                // if (NULL) -> auto-tune, no pass back
                                // if (!NULL && -1) -> auto-tune, pass back
                                // if (!NULL && >=0) -> no auto-tune, use input
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

  //ESMCI_METHOD_ENTER(localrc)

  // every Pet must provide srcArray and dstArray
  if (srcArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to srcArray", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to dstArray", ESMC_CONTEXT, &rc);
    return rc;
  }

  // determine indexTK for src and dst
  ESMC_TypeKind_Flag srcIndexTK = srcArray->getDistGrid()->getIndexTK();
  ESMC_TypeKind_Flag dstIndexTK = dstArray->getDistGrid()->getIndexTK();

  if (srcIndexTK==ESMC_TYPEKIND_I4){
    if (sizeof(SIT)!=sizeof(ESMC_I4)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Type mismatch detected", ESMC_CONTEXT, &rc);
      return rc;
    }
  }else if (srcIndexTK==ESMC_TYPEKIND_I8){
    if (sizeof(SIT)!=sizeof(ESMC_I8)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Type mismatch detected", ESMC_CONTEXT, &rc);
      return rc;
    }
  }else{
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Type option not supported", ESMC_CONTEXT, &rc);
    return rc;
  }

  if (dstIndexTK==ESMC_TYPEKIND_I4){
    if (sizeof(DIT)!=sizeof(ESMC_I4)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Type mismatch detected", ESMC_CONTEXT, &rc);
      return rc;
    }
  }else if (dstIndexTK==ESMC_TYPEKIND_I8){
    if (sizeof(DIT)!=sizeof(ESMC_I8)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Type mismatch detected", ESMC_CONTEXT, &rc);
      return rc;
    }
  }else{
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Type option not supported", ESMC_CONTEXT, &rc);
    return rc;
  }

  //TODO: remove the SIT==DIT restriction in the future, but for now
  //TODO: must bail, because sparseMatrix object does not support mixing types
  if (srcIndexTK != dstIndexTK){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Mixed type option not supported", ESMC_CONTEXT, &rc);
    return rc;
  }

  // call into the actual store method
  localrc = tSparseMatMulStore<SIT,DIT>(
    srcArray, dstArray, routehandle, sparseMatrix,
    haloFlag, ignoreUnmatched, srcTermProcessingArg, pipelineDepthArg);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // fingerprint the src/dst Arrays in RH
  localrc = (*routehandle)->fingerprint(srcArray, dstArray);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  //ESMCI_METHOD_EXIT(localrc)

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


template<typename SIT, typename DIT> int sparseMatMulStoreNbVectors(
  VM *vm,                                 // in
  DELayout *srcDelayout,                  // in
  DELayout *dstDelayout,                  // in
#if (SMMSLSQV_OPTION==2)
  bool haloFlag,                          // in //TODO: remove when no longer needed
#endif
  bool tensorMixFlag,                     // in
  int srcTensorContigLength,              // in
  int dstTensorContigLength,              // in
  ESMC_TypeKind_Flag typekindFactors,     // in
  ESMC_TypeKind_Flag typekindSrc,         // in
  ESMC_TypeKind_Flag typekindDst,         // in
  const int *srcLocalDeElementCount,      // in
  const int *dstLocalDeElementCount,      // in
  vector<vector<AssociationElement<SeqIndex<SIT>,SeqIndex<DIT> > > >&srcLinSeqVect, // in - sparse mat "run dist."
  vector<vector<AssociationElement<SeqIndex<DIT>,SeqIndex<SIT> > > >&dstLinSeqVect, // in - sparse mat "run dist."
  RouteHandle **routehandle,              // inout
#ifdef ASMM_STORE_TIMING_on
  double *t8, double *t9, double *t10, double *t11,
#endif
  vector<ArrayHelper::SendnbElement<SeqIndex<SIT>,SeqIndex<DIT> > > &sendnbVector, // inout
  vector<ArrayHelper::RecvnbElement<SeqIndex<DIT>,SeqIndex<SIT> > > &recvnbVector, // inout
  bool srcTermProcessingExplicitZero,     // in
  bool srcTermProcessingExplicitPositive  // in
  );


template<typename SIT, typename DIT> int sparseMatMulStoreEncodeXXE(VM *vm,
  DELayout *srcDelayout, DELayout *dstDelayout, bool tensorMixFlag,
  int srcTensorContigLength, int dstTensorContigLength,
  ESMC_TypeKind_Flag typekindFactors, ESMC_TypeKind_Flag typekindSrc,
  ESMC_TypeKind_Flag typekindDst,
  vector<ArrayHelper::SendnbElement<SIT,DIT> > &sendnbVector,
  vector<ArrayHelper::RecvnbElement<DIT,SIT> > &recvnbVector,
  const int *dstLocalDeTotalElementCount,
  char **rraList, int rraCount, RouteHandle **routehandle,
  bool undistributedElementsPresent,
#ifdef ASMM_STORE_TIMING_on
  double *t12pre, double *t12, double *t13, double *t14,
#endif
  int *srcTermProcessingArg = NULL,       // in (optional)
  int *pipelineDepthArg = NULL            // in (optional)
  );


//-----------------------------------------------------------------------------
#if (SMMSLSQV_OPTION==1 || SMMSLSQV_OPTION==2)
#include "sparseMatMulStoreLinSeqVect.h"
#endif

#if (SMMSLSQV_OPTION==2 || SMMSLSQV_OPTION==3)
#include "sparseMatMulStoreLinSeqVect_new.h"
#endif
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::tSparseMatMulStore()"
//BOPI
// !IROUTINE:  ESMCI::Array::tSparseMatMulStore
//
// !INTERFACE:
template<typename SIT, typename DIT>
  int Array::tSparseMatMulStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  Array *srcArray,                          // in    - source Array
  Array *dstArray,                          // in    - destination Array
  RouteHandle **routehandle,                // inout - handle to precomp. comm
  vector<SparseMatrix<SIT,DIT> >const &sparseMatrix,// in - sparse matrix vector
  bool haloFlag,                            // in    - support halo conditions
  bool ignoreUnmatched,                     // in    - support unmatched indices
  int *srcTermProcessingArg,                // inout - src term proc (optional)
                                // if (NULL) -> auto-tune, no pass back
                                // if (!NULL && -1) -> auto-tune, pass back
                                // if (!NULL && >=0) -> no auto-tune, use input
  int *pipelineDepthArg                     // inout - pipeline depth (optional)
                                // if (NULL) -> auto-tune, no pass back
                                // if (!NULL && -1) -> auto-tune, pass back
                                // if (!NULL && >=0) -> no auto-tune, use input
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

  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#if 0
  vm->timerReset("tSparseMatMulStore");
  vm->timerStart("tSparseMatMulStore");
#endif

  try{

  //---------------------------------------------------------------------------
  // Phase I
  //---------------------------------------------------------------------------

  // get the current VM and VM releated information
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

#ifdef ASMM_STORE_TIMING_on
  double t0, t1, t2, t3, t4, t5, t6, t7;  //gjt - profile
  double t8, t9, t10, t11, t12pre, t12, t13, t14, t15; //gjt - profile
  double t4a, t4b;  //gjt - profile
  double t4a1, t4a2, t4a3;  //gjt - profile
  double t4b1, t4b2, t4b3;  //gjt - profile
  double t5c, t5f;  //gjt - profile
  vm->barrier();      //synchronize start time across all PETs
  VMK::wtime(&t0);    //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore1.0"));
#endif

  // every Pet must provide srcArray and dstArray
  if (srcArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to srcArray", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (dstArray == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to dstArray", ESMC_CONTEXT, &rc);
    return rc;
  }

  // srcArray and dstArray must not point to the identical Array object,
  // unless in halo mode
  if (!haloFlag && (srcArray == dstArray)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "srcArray and dstArray must not be identical", ESMC_CONTEXT, &rc);
    return rc;
  }

  // check that sparseMatrix vector does not contain more than one element
  // TODO: this is a limitation of the current sparseMatMulStore() implement.
  if (sparseMatrix.size() > 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "currently only a single sparseMatrix element is supported",
      ESMC_CONTEXT, &rc);
    return rc;
  }

  // check that only supported seqIndex component modes are present
  // TODO: this is a limitation of the current sparseMatMulStore() implement.
  if (sparseMatrix.size() == 1){
    if (sparseMatrix[0].getSrcN() != sparseMatrix[0].getDstN()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "src and dst sequence index tuples must have same number of"
        " components", ESMC_CONTEXT, &rc);
      return rc;
    }
    if ((sparseMatrix[0].getSrcN()!=1)&&(sparseMatrix[0].getSrcN()!=2)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "only 1- or 2-component sequence index tuples supported",
        ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // set factorListCount variable (may be different on each PET)
  int const factorListCount = (sparseMatrix.size()==0) ?
    0 : sparseMatrix[0].getFactorListCount();

  // determine local tensorMixFlag and typekindFactors settings
  bool tensorMixFlag = false;                   // default
  ESMC_TypeKind_Flag typekindFactors = ESMF_NOKIND;  // default
  if (factorListCount > 0){
    // set typekindFactors
    typekindFactors = sparseMatrix[0].getTypekind();
    // check if tensorMixFlag must be set
    if (sparseMatrix[0].getSrcN() == 2)
      tensorMixFlag = true;
  }

  // communicate typekindFactors across all Pets
  vector<ESMC_TypeKind_Flag> typekindList(petCount);
  vm->allgather(&typekindFactors, &(typekindList[0]),
    sizeof(ESMC_TypeKind_Flag));
  // communicate tensorMixFlag across all Pets
  bool *tensorMixFlagList = new bool[petCount]; // cannot use vector<bool> here
  vm->allgather(&tensorMixFlag, tensorMixFlagList, sizeof(bool));

  // Check that all non-ESMF_NOKIND typekindList elements match,
  // set local typekindFactors accordingly and keep track of Pets that have
  // factors. At the same time check that tensorMixFlag matches across Pets
  // that provide factors.
  int factorPetCount = 0;         // init: number of PETs that provide factors
  vector<bool> factorPetFlag(petCount); // flag whether factors on PET
  typekindFactors = ESMF_NOKIND;  // reset: typekind of factors in sparse matrix
  for (int i=0; i<petCount; i++){
    // simple counting section of the loop:
    if (typekindList[i] != ESMF_NOKIND){
      // factors on PET
      factorPetFlag[i] = true;
      ++factorPetCount;
    }else
      // no factors on PET
      factorPetFlag[i] = false;
    // consistency checking section of the loop:
    if (typekindFactors == ESMF_NOKIND){
      // still not found any factors -> use this element to initialize:
      typekindFactors = typekindList[i];    // typekindFactors
      tensorMixFlag = tensorMixFlagList[i]; // tensorMixFlag
    }else{
      // previously initialized consistency checking variables:
      // -> check following elements against the previously initialized ones
      if (typekindList[i] != ESMF_NOKIND){
        // only if i-th PET provides factors
        if (typekindFactors != typekindList[i]){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "TypeKind mismatch between PETs", ESMC_CONTEXT, &rc);
          return rc;
        }
        if (tensorMixFlag != tensorMixFlagList[i]){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "Mismatch between PETs with respect to tensorMixFlag",
            ESMC_CONTEXT, &rc);
          return rc;
        }
      }
    }
  }
  // garbage collection
  delete [] tensorMixFlagList;

  // check for the special case of no factors provided on any PET
  if (factorPetCount < 1){
    // it is consistent to treat the factorless case as a special situation
    // where no error is returned but the routehandle simply holds a nop
    // create and initialize the RouteHandle
    *routehandle = RouteHandle::create(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    localrc = (*routehandle)->setType(ESMC_ARRAYXXE);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    // mark storage pointer in RouteHandle as invalid/NOP
    localrc = (*routehandle)->setStorage(NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    // BREAK OUT EARLY: return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  // set dataSize for factors
  int dataSizeFactors = ESMC_TypeKind_FlagSize(typekindFactors);

  // check that if tensorMixFlag is not set that the tensorElementCount matches
  if (!tensorMixFlag){
    if (srcArray->getTensorElementCount() != dstArray->getTensorElementCount()){
      std::stringstream msg;
      msg << "w/o tensor mixing srcArray/dstArray tensorElementCount must match!"
        " srcArray.tensorElementCount=" << srcArray->getTensorElementCount() <<
        " dstArray.tensorElementCount=" << dstArray->getTensorElementCount();
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP, msg.str(), ESMC_CONTEXT,
        &rc);
      return rc;
    }
  }

  // prepare tensorContigLength arguments
  bool contigFlag = true;
  int srcTensorContigLength = 1;  // init
  int srcTensorLength = 1;        // init
  for (int j=0, jj=0; jj<srcArray->rank; jj++){
    if (srcArray->arrayToDistGridMap[jj]){
      // decomposed dimension
      contigFlag = false;
    }else{
      // tensor dimension
      srcTensorLength *= srcArray->undistUBound[j]
        - srcArray->undistLBound[j] + 1;
      if (contigFlag){
        srcTensorContigLength *= srcArray->undistUBound[j]
          - srcArray->undistLBound[j] + 1;
      }
      ++j;
    }
  }
  contigFlag = true;
  int dstTensorContigLength = 1;  // init
  int dstTensorLength = 1;        // init
  for (int j=0, jj=0; jj<dstArray->rank; jj++){
    if (dstArray->arrayToDistGridMap[jj]){
      // decomposed dimension
      contigFlag = false;
    }else{
      // tensor dimension
      dstTensorLength *= dstArray->undistUBound[j]
        - dstArray->undistLBound[j] + 1;
      if (contigFlag){
        dstTensorContigLength *= dstArray->undistUBound[j]
          - dstArray->undistLBound[j] + 1;
      }
      ++j;
    }
  }

#ifdef DEBUGLOG
  {
    std::stringstream debugmsg;
    debugmsg << "Array::tSparseMatMulStore(): workWithTempArrays check:"
      << " tensorMixFlag=" << tensorMixFlag
      << " srcTensorLength=" << srcTensorLength
      << " dstTensorLength=" << dstTensorLength
      << " srcDimCount=" << srcArray->getDistGrid()->getDimCount()
      << " dstDimCount=" << dstArray->getDistGrid()->getDimCount();
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif

#ifdef ASMM_STORE_DUMPSMM_on
  if (!tensorMixFlag && typekindFactors == ESMC_TYPEKIND_R8){
    // SCRIP weight file output only supported w/o tensor mixing and R8 factors
    char const *fileName="asmmDumpSMM.nc";
    double const *factorList = NULL;
    int const *factorIndexList = NULL;
    int count = 0;
    if (sparseMatrix.size()>0){
      factorList = (double const *)sparseMatrix[0].getFactorList();
      factorIndexList = (int const *)sparseMatrix[0].getFactorIndexList();
      count = sparseMatrix[0].getFactorListCount();
    }
    FTN_X(f_esmf_outputsimpleweightfile)(fileName, &count, factorList,
      factorIndexList, &localrc, strlen(fileName));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
  }
#endif

  bool workWithTempArrays=false;
  if (!haloFlag){
  //TODO: Fix this!
  // Currently this optimization does not completely work for halo case
  // because it is difficult to copy the masked rim into the temporary array.
  if (!tensorMixFlag && (srcTensorLength>1) &&
    (srcTensorLength==dstTensorLength) &&
    (srcArray->getDistGrid()->getDimCount() <= 2) &&
    (dstArray->getDistGrid()->getDimCount() <= 2) &&
    (srcArray->rank > 1 && dstArray->rank > 1)){
    // Optimization of undistributed dimensions if there is no tensor mixing.
    // This optimization requires that src and dst have the same number of
    // tensor elements (they can be across different number of dims).
    // Currently super-vectorization is only supported for dimCount<=2 on
    // the execution side, therefore this optimization must be limited here.
    // The strategy in this case is to create temporary arrays without the
    // undistributed dimensions and precompute the routehandle for those. This
    // is typically much faster, because of the general way in which
    // undistributed elements are currently treated.
    // The resulting routehandle can be used for arrays with or without
    // undistributed dimensions.
#ifdef DEBUGLOG
  {
    std::stringstream debugmsg;
    debugmsg << "Array::tSparseMatMulStore(): workWithTempArrays active:"
      << " srcTensorLength=" << srcTensorLength
      << " dstTensorLength=" << dstTensorLength;
    ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
    workWithTempArrays=true;
    // create the temporary arrays
    srcArray = Array::create(srcArray, true, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    if (haloFlag){
      // for halo dstArray is an alias to srcArray, and must stay that way
      dstArray = srcArray;
    }else{
      // if not halo, then dstArray must independently be adjusted
      dstArray = Array::create(dstArray, true, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }
    // adjust the precomputed values
    srcTensorLength = 1;
    srcTensorContigLength = 1;
    dstTensorLength = 1;
    dstTensorContigLength = 1;
  }
  }

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t1);   //gjt - profile
#endif

//---DEBUG-------------------
// return successfully
//rc = ESMF_SUCCESS;
//return rc;
//---DEBUG-------------------

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore1.1"));
#endif

  //---------------------------------------------------------------------------
  // Phase II + III
  //---------------------------------------------------------------------------

  // determine local srcElementCount
  const int srcLocalDeCount = srcArray->delayout->getLocalDeCount();
  const int *srcLocalDeToDeMap = srcArray->delayout->getLocalDeToDeMap();
  int *srcLocalDeElementCount = new int[srcLocalDeCount];
  int srcElementCount = 0;   // initialize
  for (int i=0; i<srcLocalDeCount; i++){
    int de = srcLocalDeToDeMap[i];  // global DE index
    srcLocalDeElementCount[i] = srcArray->exclusiveElementCountPDe[de]
      * srcArray->tensorElementCount;
    srcElementCount += srcLocalDeElementCount[i];
  }
  // determine local dstElementCount
  const int dstLocalDeCount = dstArray->delayout->getLocalDeCount();
  const int *dstLocalDeToDeMap = dstArray->delayout->getLocalDeToDeMap();
  int *dstLocalDeElementCount = new int[dstLocalDeCount];
  int dstElementCount = 0;   // initialize
  for (int i=0; i<dstLocalDeCount; i++){
    if (haloFlag){
      // for halo the dst elements are in the rim of dstArray
      dstLocalDeElementCount[i] = 0;  // init
      const std::vector<std::vector<SeqIndex<DIT> > > *rimSeqIndex;
      dstArray->getRimSeqIndex(&rimSeqIndex);
      for (int k=0; k<dstArray->rimElementCount[i]; k++){
        SeqIndex<DIT> seqIndex = (*rimSeqIndex)[i][k];
        if (seqIndex.valid()){
          // this rim element holds a valid seqIndex
          ++dstLocalDeElementCount[i];  // count this element
        }
      }
    }else{
      int de = dstLocalDeToDeMap[i];  // global DE index
      dstLocalDeElementCount[i] = dstArray->exclusiveElementCountPDe[de]
        * dstArray->tensorElementCount;
    }
    dstElementCount += dstLocalDeElementCount[i];
  }

  // tansform into "run distribution"
  vector<vector<AssociationElement<SeqIndex<SIT>,SeqIndex<DIT> > > >
    srcLinSeqVect(srcLocalDeCount);
  vector<vector<AssociationElement<SeqIndex<DIT>,SeqIndex<SIT> > > >
    dstLinSeqVect(dstLocalDeCount);

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore2.0a"));
#endif

#if (SMMSLSQV_OPTION==1)

//  localrc = sparseMatMulStoreLinSeqVect_new(vm,
  localrc = sparseMatMulStoreLinSeqVect(vm,
    srcArray, dstArray, sparseMatrix,
    haloFlag, ignoreUnmatched, tensorMixFlag,
    factorListCount, factorPetFlag, typekindFactors,
    srcLocalDeCount, dstLocalDeCount, srcElementCount, dstElementCount,
    srcLocalDeElementCount, dstLocalDeElementCount,
    srcLinSeqVect, dstLinSeqVect
  );
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#elif (SMMSLSQV_OPTION==2)

  if (haloFlag){
    localrc = sparseMatMulStoreLinSeqVect_new(vm,
      srcArray, dstArray, sparseMatrix,
      haloFlag, ignoreUnmatched, tensorMixFlag,
      factorListCount, factorPetFlag, typekindFactors,
      srcLocalDeCount, dstLocalDeCount, srcElementCount, dstElementCount,
      srcLocalDeElementCount, dstLocalDeElementCount,
      srcLinSeqVect, dstLinSeqVect
    );
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }else{
    localrc = sparseMatMulStoreLinSeqVect(vm,
      srcArray, dstArray, sparseMatrix,
      haloFlag, ignoreUnmatched, tensorMixFlag,
      factorListCount, factorPetFlag, typekindFactors,
      srcLocalDeCount, dstLocalDeCount, srcElementCount, dstElementCount,
      srcLocalDeElementCount, dstLocalDeElementCount,
      srcLinSeqVect, dstLinSeqVect
    );
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

#elif (SMMSLSQV_OPTION==3)

  localrc = sparseMatMulStoreLinSeqVect_new(vm,
    srcArray, dstArray, sparseMatrix,
    haloFlag, ignoreUnmatched, tensorMixFlag,
    factorListCount, factorPetFlag, typekindFactors,
    srcLocalDeCount, dstLocalDeCount, srcElementCount, dstElementCount,
    srcLocalDeElementCount, dstLocalDeElementCount,
    srcLinSeqVect, dstLinSeqVect
  );
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#endif  // SMMSLSQV_OPTION


  //---------------------------------------------------------------------------
  // Phase IV
  //---------------------------------------------------------------------------

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.0"));
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
  // obtain typekindSrc
  ESMC_TypeKind_Flag typekindSrc = srcArray->getTypekind();
  // obtain typekindDst
  ESMC_TypeKind_Flag typekindDst = dstArray->getTypekind();

  // prepare dstLocalDeTotalElementCount
  int *dstLocalDeTotalElementCount = new int[dstLocalDeCount];
  for (int i=0; i<dstLocalDeCount; i++)
    dstLocalDeTotalElementCount[i] =
      dstArray->totalElementCountPLocalDe[i] * dstArray->tensorElementCount;

  // determine if there are undistributed elements present in either src or dst
  bool undistributedElementsPresent = false;
  if (srcTensorLength>1) undistributedElementsPresent = true;
  if (dstTensorLength>1) undistributedElementsPresent = true;

#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.1"));
#endif

  // create and initialize the RouteHandle
  *routehandle = RouteHandle::create(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  localrc = (*routehandle)->setType(ESMC_ARRAYXXE);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // allocate XXE and attach to RouteHandle
  XXE *xxe;
  try{
    xxe = new XXE(vm, 1000, 10000, 1000);
  }catch (...){
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    return rc;
  }
  localrc = (*routehandle)->setStorage(xxe);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.2"));
#endif

  // prepare srcTermProcessingExplicitZero
  bool srcTermProcessingExplicitZero = false;
  if (srcTermProcessingArg && *srcTermProcessingArg==0)
    srcTermProcessingExplicitZero = true;
  
  // prepare srcTermProcessingExplicitPositive
  bool srcTermProcessingExplicitPositive = false;
  if (srcTermProcessingArg && *srcTermProcessingArg>0)
    srcTermProcessingExplicitPositive = true;
  
  // tansform "run distribution" into nb-vectors
  vector<ArrayHelper::SendnbElement<SeqIndex<SIT>,SeqIndex<DIT> > > sendnbVector;
  vector<ArrayHelper::RecvnbElement<SeqIndex<DIT>,SeqIndex<SIT> > > recvnbVector;
  localrc = sparseMatMulStoreNbVectors(vm,
    srcArray->delayout, dstArray->delayout,
#if (SMMSLSQV_OPTION==2)
    haloFlag,         //TODO: remove when no longer needed
#endif
    tensorMixFlag, srcTensorContigLength, dstTensorContigLength,
    typekindFactors, typekindSrc, typekindDst,
    srcLocalDeElementCount, dstLocalDeElementCount,
    srcLinSeqVect, dstLinSeqVect, routehandle,
#ifdef ASMM_STORE_TIMING_on
    &t8, &t9, &t10, &t11,
#endif
    sendnbVector, recvnbVector,
    srcTermProcessingExplicitZero, srcTermProcessingExplicitPositive
  );
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.3"));
#endif

  // force vectors out of scope by swapping with empty vector, to free memory
  vector<vector<AssociationElement<SeqIndex<SIT>,SeqIndex<DIT> > > >().swap(srcLinSeqVect);
  vector<vector<AssociationElement<SeqIndex<DIT>,SeqIndex<SIT> > > >().swap(dstLinSeqVect);

#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.4"));
#endif

  // encode sparseMatMul communication pattern into XXE stream
  localrc = sparseMatMulStoreEncodeXXE(vm,
    srcArray->delayout, dstArray->delayout,
    tensorMixFlag, srcTensorContigLength, dstTensorContigLength,
    typekindFactors, typekindSrc, typekindDst,
    sendnbVector, recvnbVector,
    dstLocalDeTotalElementCount,
    rraList, rraCount, routehandle,
    undistributedElementsPresent,
#ifdef ASMM_STORE_TIMING_on
    &t12pre, &t12, &t13, &t14,
#endif
    srcTermProcessingArg,
    pipelineDepthArg
  );
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.5"));
#endif

  // garbage collection
  delete [] rraList;
  delete [] dstLocalDeTotalElementCount;
  delete [] srcLocalDeElementCount;
  delete [] dstLocalDeElementCount;
  // force vectors out of scope by swapping with empty vector, to free memory
  vector<ArrayHelper::SendnbElement<SeqIndex<SIT>,SeqIndex<DIT> > >().swap(sendnbVector);
  vector<ArrayHelper::RecvnbElement<SeqIndex<DIT>,SeqIndex<SIT> > >().swap(recvnbVector);

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore4.6"));
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(&t15);   //gjt - profile
  {
    char msg[160];
    ESMC_LogDefault.Write("ASMM_STORE_TIMING: --- start ---", ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t1   = %g", t1-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t2   = %g", t2-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t3   = %g", t3-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t4   = %g", t4-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4a1   = %g", t4a1-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4a2   = %g", t4a2-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4a3   = %g", t4a3-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4a    = %g", t4a-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4b1   = %g", t4b1-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4b2   = %g", t4b2-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4b3   = %g", t4b3-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t4b    = %g", t4b-t3);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t5   = %g", t5-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t5c    = %g", t5c-t4);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING:    t5f    = %g", t5f-t4);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t6   = %g", t6-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t7   = %g", t7-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t8   = %g", t8-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t9   = %g", t9-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t10  = %g", t10-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t11  = %g", t11-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t12pre  = %g", t12pre-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t12  = %g", t12-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t13  = %g", t13-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t14  = %g", t14-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    sprintf(msg, "ASMM_STORE_TIMING: t15  = %g", t15-t0);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
    ESMC_LogDefault.Write("ASMM_STORE_TIMING: --- stop ----", ESMC_LOGMSG_DEBUG);
  }
#endif

  if (workWithTempArrays){
    // clean-up the temporary arrays
    Array::destroy(&srcArray, true);
    if (!haloFlag)
      Array::destroy(&dstArray, true);
  }

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStore5.0"));
#endif

#if 0
  vm->timerStop("tSparseMatMulStore");
  vm->timerLog("tSparseMatMulStore", ESMC_LOGMSG_DEBUG);
#endif

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::sparseMatMulStoreNbVectors()"
//BOPI
// !IROUTINE:  ESMCI::sparseMatMulStoreNbVectors
//
// !INTERFACE:
template<typename SIT, typename DIT> int sparseMatMulStoreNbVectors(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vm,                                 // in
  DELayout *srcDelayout,                  // in
  DELayout *dstDelayout,                  // in
#if (SMMSLSQV_OPTION==2)
  bool haloFlag,                          // in //TODO: remove when no longer needed
#endif
  bool tensorMixFlag,                     // in
  int srcTensorContigLength,              // in
  int dstTensorContigLength,              // in
  ESMC_TypeKind_Flag typekindFactors,     // in
  ESMC_TypeKind_Flag typekindSrc,         // in
  ESMC_TypeKind_Flag typekindDst,         // in
  const int *srcLocalDeElementCount,      // in
  const int *dstLocalDeElementCount,      // in
  vector<vector<AssociationElement<SeqIndex<SIT>,SeqIndex<DIT> > > >&srcLinSeqVect, // in - sparse mat "run dist."
  vector<vector<AssociationElement<SeqIndex<DIT>,SeqIndex<SIT> > > >&dstLinSeqVect, // in - sparse mat "run dist."
  RouteHandle **routehandle,              // inout
#ifdef ASMM_STORE_TIMING_on
  double *t8, double *t9, double *t10, double *t11,
#endif
  vector<ArrayHelper::SendnbElement<SeqIndex<SIT>,SeqIndex<DIT> > > &sendnbVector, // inout
  vector<ArrayHelper::RecvnbElement<SeqIndex<DIT>,SeqIndex<SIT> > > &recvnbVector, // inout
  bool srcTermProcessingExplicitZero,     // in
  bool srcTermProcessingExplicitPositive  // in
  ){
//
// !DESCRIPTION:
//    Take the incoming sparse matrix information from "run distribution" and
//    transform it into (srcDe, dstDe) pair specific SendnbElement and
//    RecvnbElement objects:
//
//      srcLinSeqVect -> sendnbVector
//      dstLinSeqVect -> recvnbVector
//
//    These two vectors contain as many objects as there are srcDe, dstDe on
//    the localPet, respectively.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    // srcLinSeqVect
    for (unsigned j=0; j<srcLinSeqVect.size(); j++){
      for (unsigned k=0; k<srcLinSeqVect[j].size(); k++){
        msg << "ASMM_STORE_LOG:" << __LINE__ <<
          " srcLinSeqVect["<< j <<"]["<< k <<"].linIndex = "
          << srcLinSeqVect[j][k].linIndex <<", "
          ".seqIndex = "<< srcLinSeqVect[j][k].seqIndex.decompSeqIndex
          <<"/"<< srcLinSeqVect[j][k].seqIndex.getTensor() <<
          ", .factorList.size() = "<< srcLinSeqVect[j][k].factorList.size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        msg.str("");  // clear
        for (unsigned kk=0; kk<srcLinSeqVect[j][k].factorList.size(); kk++){
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \tfactorList["<< kk <<"]";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerSeqIndex ="
            << srcLinSeqVect[j][k].factorList[kk].partnerSeqIndex.decompSeqIndex
            <<"/"
            << srcLinSeqVect[j][k].factorList[kk].partnerSeqIndex.getTensor();
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerDe =";
          for (unsigned jj=0;
            jj<srcLinSeqVect[j][k].factorList[kk].partnerDe.size(); jj++)
            msg << srcLinSeqVect[j][k].factorList[kk].partnerDe[jj] <<", ";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
          switch (typekindFactors){
          case ESMC_TYPEKIND_R4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R4 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_R8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R8 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I4 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I8 *)srcLinSeqVect[j][k].factorList[kk].factor);
            break;
          default:
            break;
          }
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
        }
      }
    }
    // dstLinSeqVect
    for (unsigned j=0; j<dstLinSeqVect.size(); j++){
      for (unsigned k=0; k<dstLinSeqVect[j].size(); k++){
        msg << "ASMM_STORE_LOG:" << __LINE__ <<
          " dstLinSeqVect["<< j <<"]["<< k <<"].linIndex = "
          << dstLinSeqVect[j][k].linIndex <<", "
          ".seqIndex = "<< dstLinSeqVect[j][k].seqIndex.decompSeqIndex
          <<"/"<< dstLinSeqVect[j][k].seqIndex.getTensor() <<
          ", .factorList.size() = "<< dstLinSeqVect[j][k].factorList.size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        msg.str("");  // clear
        for (unsigned kk=0; kk<dstLinSeqVect[j][k].factorList.size(); kk++){
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \tfactorList["<< kk <<"]";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerSeqIndex ="
            << dstLinSeqVect[j][k].factorList[kk].partnerSeqIndex.decompSeqIndex
            <<"/"
            << dstLinSeqVect[j][k].factorList[kk].partnerSeqIndex.getTensor();
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.partnerDe =";
          for (unsigned jj=0;
            jj<dstLinSeqVect[j][k].factorList[kk].partnerDe.size(); jj++)
            msg << dstLinSeqVect[j][k].factorList[kk].partnerDe[jj] <<", ";
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
          switch (typekindFactors){
          case ESMC_TYPEKIND_R4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R4 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_R8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_R8 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I4:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I4 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          case ESMC_TYPEKIND_I8:
            msg << "ASMM_STORE_LOG:" << __LINE__ << " \t\t.factor =" <<
              *((ESMC_I8 *)dstLinSeqVect[j][k].factorList[kk].factor);
            break;
          default:
            break;
          }
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          msg.str("");  // clear
        }
      }
    }
  }
#endif

  try{

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors1.0"));
#endif

  // get a handle on the XXE stored in routehandle
  XXE *xxe = (XXE *)(*routehandle)->getStorage();

  // prepare other local variables
  int dataSizeFactors = ESMC_TypeKind_FlagSize(typekindFactors);

  int dataSizeSrc = ESMC_TypeKind_FlagSize(typekindSrc);
  int srcLocalDeCount = srcDelayout->getLocalDeCount();
  const int *srcLocalDeToDeMap = srcDelayout->getLocalDeToDeMap();

  int dataSizeDst = ESMC_TypeKind_FlagSize(typekindDst);
  int dstLocalDeCount = dstDelayout->getLocalDeCount();
  const int *dstLocalDeToDeMap = dstDelayout->getLocalDeToDeMap();

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  bool vectorFlag = !(tensorMixFlag ||
    (srcTensorContigLength != dstTensorContigLength));

#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ << " tensorMixFlag=" << tensorMixFlag
      << " srcTensorContigLength=" << srcTensorContigLength
      << " dstTensorContigLength=" << dstTensorContigLength
      << " ==>> vectorFlag=" << vectorFlag;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif

#ifdef ASMM_STORE_TIMING_on
  double t9a, t9b, t9d, t9e; //gjt - profile
  double t9c1, t9c2; //gjt - profile
  VMK::wtime(t8);   //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors2.0"));
#endif

  // determine recv pattern for all localDEs on dst side
  for (int j=0; j<dstLocalDeCount; j++){
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " j=" << j <<
        " dstLocalDeElementCount[j]=" << dstLocalDeElementCount[j];
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    vector<int> index2Ref;
    index2Ref.reserve(dstLocalDeElementCount[j]); // good guess
    int localDeFactorCount = 0; // reset
    int iCount = 0; // reset
    for (unsigned k=0; k<dstLinSeqVect[j].size(); k++){
      unsigned factorCount = dstLinSeqVect[j][k].factorList.size();
      if (factorCount){
        index2Ref.push_back(k);   // store element index
        localDeFactorCount += factorCount;
        ++iCount; // increment counter
      }
    }

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors2.1"));
#endif

#ifdef ASMM_STORE_TIMING_on
    VMK::wtime(&t9a);   //gjt - profile
#endif

#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " j=" << j <<
        " iCount=" << iCount <<
        " localDeFactorCount=" << localDeFactorCount;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    int *index2Ref2 = new int[localDeFactorCount];            // large enough
    int *factorIndexRef = new int[localDeFactorCount];        // large enough
    int *partnerDeRef = new int[localDeFactorCount];          // large enough
    int *recvnbPartnerDeList = new int[localDeFactorCount];   // large enough
    int *recvnbPartnerDeCount = new  int[localDeFactorCount]; // large enough
    int recvnbDiffPartnerDeCount = 0; // reset
    int count = 0; // reset
    for (int i=0; i<iCount; i++){
      unsigned factorCount = dstLinSeqVect[j][index2Ref[i]].factorList.size();
      for (unsigned k=0; k<factorCount; k++){
        int partnerDe;
#if (SMMSLSQV_OPTION==1)
          partnerDe = dstLinSeqVect[j][index2Ref[i]].factorList[k].partnerDE[0];
#endif
#if (SMMSLSQV_OPTION==3)
          partnerDe = dstLinSeqVect[j][index2Ref[i]].factorList[k].partnerDe;
#endif
#if (SMMSLSQV_OPTION==2)
          if (haloFlag)
            partnerDe = dstLinSeqVect[j][index2Ref[i]].factorList[k].partnerDe;
          else
            partnerDe = dstLinSeqVect[j][index2Ref[i]].factorList[k].partnerDE[0];
#endif
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

#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " recvnbDiffPartnerDeCount="
        << recvnbDiffPartnerDeCount;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      for (int i=0; i<recvnbDiffPartnerDeCount; i++){
        msg.str("");  // clear
        msg << "ASMM_STORE_LOG:" << __LINE__ << " recvnbPartnerDeCount[" << i <<
          "]=" << recvnbPartnerDeCount[i];
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
      msg.str("");  // clear
      msg << "ASMM_STORE_LOG:" << __LINE__ << " sizeof(DstInfo)="
        << sizeof(ArrayHelper::DstInfo<SeqIndex<DIT>,SeqIndex<SIT> >);
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

#ifdef ASMM_STORE_TIMING_on
    VMK::wtime(&t9b);   //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors3.0"));
#endif

    // invert the look-up direction
    vector<vector<ArrayHelper::DstInfo<SeqIndex<DIT>,SeqIndex<SIT> > > >
      dstInfoTable(recvnbDiffPartnerDeCount);
    int *dstInfoTableInit = new int[recvnbDiffPartnerDeCount];
    for (int i=0; i<recvnbDiffPartnerDeCount; i++){
      dstInfoTable[i].resize(recvnbPartnerDeCount[i]);
      dstInfoTableInit[i] = 0;   // reset
    }
    char *localDeFactorBuffer;
    if (!srcTermProcessingExplicitPositive){
      // need factors on the RECV side
      // alignment char *localDeFactorBuffer = new char[localDeFactorCount * dataSizeFactors];
      int qwords = (localDeFactorCount * dataSizeFactors) / 8;
      if ((localDeFactorCount * dataSizeFactors) % 8) ++qwords;
#ifdef ASMM_STORE_MEMLOG_on
      VM::logMemInfo(std::string("ASMMStoreNbVectors3.1"));
#endif
      localDeFactorBuffer = (char *)(new double[qwords]);
      localrc = xxe->storeData(localDeFactorBuffer, qwords*8); // XXE garbage
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_STORE_TIMING_on
      VMK::wtime(&t9c1);   //gjt - profile
#endif
#ifdef ASMM_STORE_MEMLOG_on
      VM::logMemInfo(std::string("ASMMStoreNbVectors3.2"));
#endif
    }
    for (int i=0; i<localDeFactorCount; i++){
      int partnerDeListIndex = partnerDeRef[i];
      int index2 = dstInfoTableInit[partnerDeListIndex]++;
      dstInfoTable[partnerDeListIndex][index2].linIndex =
        dstLinSeqVect[j][index2Ref2[i]].linIndex;
      dstInfoTable[partnerDeListIndex][index2].vectorLength = 1;  // default
      dstInfoTable[partnerDeListIndex][index2].seqIndex =
        dstLinSeqVect[j][index2Ref2[i]].seqIndex;
      dstInfoTable[partnerDeListIndex][index2].partnerSeqIndex
        = dstLinSeqVect[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .partnerSeqIndex;
      if (!tensorMixFlag){
        // default into tensor for tensor src/dst mode
        dstInfoTable[partnerDeListIndex][index2].partnerSeqIndex.setTensor(
          dstInfoTable[partnerDeListIndex][index2].seqIndex.getTensor());
      }
      if (!srcTermProcessingExplicitPositive){
        // copy the factors to keep on RECV side
        char *localDeFactorBufferEntry = localDeFactorBuffer + i*dataSizeFactors;
        memcpy(localDeFactorBufferEntry,
          dstLinSeqVect[j][index2Ref2[i]].factorList[factorIndexRef[i]].factor,
          dataSizeFactors);
        dstInfoTable[partnerDeListIndex][index2].factor =
          (void *)(localDeFactorBufferEntry);
      }else{
        dstInfoTable[partnerDeListIndex][index2].factor = NULL;
      }
    }

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors3.3"));
#endif
    // garbage collection
    delete [] dstInfoTableInit;
    delete [] index2Ref2;
    delete [] factorIndexRef;
    delete [] partnerDeRef;

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors3.4"));
#endif
    // force vectors out of scope by swapping with empty vector, to free memory
    vector<AssociationElement<SeqIndex<DIT>,SeqIndex<SIT> > > ().swap(dstLinSeqVect[j]);
  
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors4.0"));
#endif
    
#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors4.1"));
#endif

#ifdef ASMM_STORE_TIMING_on
    VMK::wtime(&t9c2);   //gjt - profile
#endif

    // sort each "recvnbDiffPartnerDeCount group" (opposite of src)
    if (!vectorFlag){
      // no vectorization possible -> sort for scalar optimization
      for (int i=0; i<recvnbDiffPartnerDeCount; i++)
        sort(dstInfoTable[i].begin(), dstInfoTable[i].end(),
          ArrayHelper::scalarOrderDstInfo<SeqIndex<DIT>,SeqIndex<SIT> >);
    }else if (dstTensorContigLength == 1){
      // support vectorization during execution, but nothing to deflate here
      // sort for scalar optimization
      for (int i=0; i<recvnbDiffPartnerDeCount; i++)
        sort(dstInfoTable[i].begin(), dstInfoTable[i].end(),
          ArrayHelper::scalarOrderDstInfo<SeqIndex<DIT>,SeqIndex<SIT> >);
    }else{
      // vectorization
      // sort vector optimization
      for (int i=0; i<recvnbDiffPartnerDeCount; i++){
        sort(dstInfoTable[i].begin(), dstInfoTable[i].end(),
          ArrayHelper::vectorOrderDstInfo<SeqIndex<DIT>,SeqIndex<SIT> >);
#ifdef ASMM_STORE_LOG_on
        {
          std::stringstream msg;
          for (int k=0; k<dstInfoTable[i].size(); k++){
            msg.str("");  // clear
            msg << "ASMM_STORE_LOG:" << __LINE__ <<
              " dstInfoTable[" << i << "][" << k << "].seqIndex = "
              << dstInfoTable[i][k].seqIndex.decompSeqIndex << "/"
              << dstInfoTable[i][k].seqIndex.getTensor() << ", "
              << dstInfoTable[i][k].partnerSeqIndex.decompSeqIndex << "/"
              << dstInfoTable[i][k].partnerSeqIndex.getTensor()
              << " .factor = " << dstInfoTable[i][k].factor;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
        }
#endif
        // vectorize -> deflate dstInfoTable
        typename vector<ArrayHelper::DstInfo<SeqIndex<DIT>,SeqIndex<SIT> > >::
          iterator rangeStart = dstInfoTable[i].begin();
        typename vector<ArrayHelper::DstInfo<SeqIndex<DIT>,SeqIndex<SIT> > >::
          iterator rangeStop = rangeStart;
        typename vector<ArrayHelper::DstInfo<SeqIndex<DIT>,SeqIndex<SIT> > >::
          iterator rangeWrite = rangeStart;
        while (rangeStart != dstInfoTable[i].end()){
          int vectorLength = 1; // initialize
          DIT decompSeqIndex = rangeStart->seqIndex.decompSeqIndex;
          int linIndex = rangeStart->linIndex;
          rangeStop++;
          while((rangeStop != dstInfoTable[i].end())
            && (rangeStop->seqIndex.decompSeqIndex == decompSeqIndex)
            && (rangeStop->linIndex == (linIndex + 1))){
            linIndex = rangeStop->linIndex;
            ++vectorLength;
            rangeStop++;
          }
#ifdef ASMM_STORE_LOG_on
          {
            std::stringstream msg;
            msg << "ASMM_STORE_LOG:" << __LINE__ <<
              " dstTensorContigLength: " << dstTensorContigLength << 
              " vectorLength: " << vectorLength << " decompSeqIndex: " <<
              decompSeqIndex;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
#endif
          if ((rangeWrite != dstInfoTable[i].begin())
            && ((rangeWrite-1)->vectorLength != vectorLength)){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
            "vectorization failed", ESMC_CONTEXT, &rc);
            return rc;
          }
          if (vectorLength != dstTensorContigLength){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
            "vectorization failed", ESMC_CONTEXT, &rc);
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

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors5.0"));
#endif

#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      for (int i=0; i<recvnbDiffPartnerDeCount; i++){
        msg.str("");  // clear
        msg << "ASMM_STORE_LOG:" << __LINE__ << " dstInfoTable[" << i <<
          "].size()=" << dstInfoTable[i].size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
    }
#endif

#ifdef ASMM_STORE_TIMING_on
    VMK::wtime(&t9d);   //gjt - profile
#endif

    // construct recv elements
    int oldSize = recvnbVector.size();
    recvnbVector.resize(oldSize+recvnbDiffPartnerDeCount);
    for (int i=0; i<recvnbDiffPartnerDeCount; i++){
      int vectorLength = dstInfoTable[i].begin()->vectorLength;
      // initialize the bufferIndex member in the dstInfoTable
#ifdef MSG_DEFLATE
      // construct bufferIndex member in dstInfoTable according to deflation
      vector<ArrayHelper::Deflator<SIT> > deflator(dstInfoTable[i].size());
      for (unsigned k=0; k<dstInfoTable[i].size(); k++){
        // fill
        deflator[k].seqIndex = dstInfoTable[i][k].partnerSeqIndex;
        deflator[k].index = k;
      }
      // sort by partnerSeqIndex
      sort(deflator.begin(), deflator.end());
      // record the buffer index in dstInfoTable
      int kk = 0;
      dstInfoTable[i][deflator[0].index].bufferIndex = kk;  // spin up
      for (unsigned k=1; k<deflator.size(); k++){
        if (deflator[k].seqIndex != deflator[k-1].seqIndex)
          ++kk;
        dstInfoTable[i][deflator[k].index].bufferIndex = kk;
      }
      ++kk;

#define MSG_DEFLATE_DEBUG_off
#ifdef MSG_DEFLATE_DEBUG
      {
        std::stringstream msg;
        msg << "ASMM_STORE_LOG:" << __LINE__ <<
          " recv: deflated count: kk=" << kk;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#if 0
        for (int k=0; k<deflator.size(); k++){
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ <<
            " recv: deflator[" << k << "]: index=" << deflator[k].index <<
            ", bufferIndex=" << dstInfoTable[i][deflator[k].index].bufferIndex
            << ", seqIndex=" << deflator[k].seqIndex.decompSeqIndex;
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        }
#endif
      }
#endif

#else
      // do not consider deflation
      for (int k=0; k<dstInfoTable[i].size(); k++)
        dstInfoTable[i][k].bufferIndex = k;
      int kk = dstInfoTable[i].size();
#endif
      // determine buffer size needed
      int neededBufferSize = recvnbPartnerDeCount[i]; // default to largest
      if (srcTermProcessingExplicitZero){
        // only need a buffer large enough to hold the deflated size
        neededBufferSize = kk * vectorLength;
      }
      // large contiguous 1st level receive buffer
      int qwords = (neededBufferSize * dataSizeSrc) / 8;
      if ((neededBufferSize * dataSizeSrc) % 8) ++qwords;
      char *buffer = (char *)(new double[qwords]);
      // store buffer information in BufferInfo for XXE buffer control
      localrc = xxe->storeBufferInfo(buffer,
        neededBufferSize * dataSizeSrc,
        neededBufferSize * dataSizeSrc / vectorLength);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      // prepare DE/PET info
      int srcDe = recvnbPartnerDeList[i];
      int srcPet;   //TODO: DE-based comms
      srcDelayout->getDEMatchPET(srcDe, *vm, NULL, &srcPet, 1);
      int dstDe = dstLocalDeToDeMap[j];
      // fill values into recvnbVector
      int ii = oldSize + i;
      recvnbVector[ii].srcPet = srcPet;
      recvnbVector[ii].srcDe = srcDe;
      recvnbVector[ii].srcLocalDe = i;
      recvnbVector[ii].dstDe = dstDe;
      recvnbVector[ii].dstLocalDe = j;
      recvnbVector[ii].bufferInfo = (char **)xxe->getBufferInfoPtr();
      recvnbVector[ii].partnerDeDataCount = kk;
      recvnbVector[ii].vectorFlag = vectorFlag;
      recvnbVector[ii].dstInfoTable.swap(dstInfoTable[i]);
      recvnbVector[ii].localPet = localPet;
      recvnbVector[ii].petCount = petCount;
#ifdef ASMM_STORE_LOG_on
      {
        std::stringstream msg;
        msg << "ASMM_STORE_LOG:" << __LINE__ << " recvnbElement srcPet="
          << srcPet << " vectorLength=" << vectorLength;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
#endif
    } // for i - recvnbDiffPartnerDeCount

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors5.1"));
#endif

    // garbage collection
    delete [] recvnbPartnerDeList;
    delete [] recvnbPartnerDeCount;
      
#ifdef ASMM_STORE_TIMING_on
    VMK::wtime(&t9e);   //gjt - profile
//    printf("gjt - profile for PET %d, j-loop %d:\n"
//      " t9a=%g\n t9b=%g\n t9c1=%g\n t9c2=%g\n t9d=%g\n t9e=%g\n", localPet, j,
//      t9a-*t8, t9b-*t8, t9c1-*t8, t9c2-*t8, t9d-*t8, t9e-*t8);
#endif

  } // for j - dstLocalDeCount

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t9);   //gjt - profile
#endif

#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors6.0"));
#endif
  
  // determine send pattern for all localDEs on src side
  for (int j=0; j<srcLocalDeCount; j++){
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " j=" << j <<
        " srcLocalDeElementCount[j]=" << srcLocalDeElementCount[j];
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    vector<int> index2Ref;
    index2Ref.reserve(srcLocalDeElementCount[j]); // good guess
    int localDeFactorCount = 0; // reset
    int iCount = 0; // reset
    for (unsigned k=0; k<srcLinSeqVect[j].size(); k++){
      unsigned factorCount = srcLinSeqVect[j][k].factorList.size();
      if (factorCount){
        index2Ref.push_back(k);   // store element index
        localDeFactorCount += factorCount;
        ++iCount; // increment counter
      }
    }
    
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors6.1"));
#endif

#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " iCount=" << iCount <<
        " localDeFactorCount=" << localDeFactorCount;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
    
    int *index2Ref2 = new int[localDeFactorCount];            // large enough
    int *factorIndexRef = new int[localDeFactorCount];        // large enough
    int *partnerDeRef = new int[localDeFactorCount];          // large enough
    int *sendnbPartnerDeList = new int[localDeFactorCount];   // large enough
    int *sendnbPartnerDeCount = new int[localDeFactorCount];  // large enough
    int sendnbDiffPartnerDeCount = 0; // reset
    int count = 0; // reset
    for (int i=0; i<iCount; i++){
      unsigned factorCount = srcLinSeqVect[j][index2Ref[i]].factorList.size();
      for (unsigned k=0; k<factorCount; k++){
        int partnerDe;
#if (SMMSLSQV_OPTION==1)
          partnerDe = srcLinSeqVect[j][index2Ref[i]].factorList[k].partnerDE[0];
#endif
#if (SMMSLSQV_OPTION==3)
          partnerDe = srcLinSeqVect[j][index2Ref[i]].factorList[k].partnerDe;
#endif
#if (SMMSLSQV_OPTION==2)
          if (haloFlag)
            partnerDe = srcLinSeqVect[j][index2Ref[i]].factorList[k].partnerDe;
          else
            partnerDe = srcLinSeqVect[j][index2Ref[i]].factorList[k].partnerDE[0];
#endif
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
#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_LOG:" << __LINE__ << " sendnbDiffPartnerDeCount="
        << sendnbDiffPartnerDeCount;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      for (int i=0; i<sendnbDiffPartnerDeCount; i++){
        msg.str("");  // clear
        msg << "ASMM_STORE_LOG:" << __LINE__ << " sendnbPartnerDeCount[" << i <<
          "]=" << sendnbPartnerDeCount[i];
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
      msg.str("");  // clear
      msg << "ASMM_STORE_LOG:" << __LINE__ << " sizeof(SrcInfo)="
        << sizeof(ArrayHelper::SrcInfo<SeqIndex<DIT>,SeqIndex<SIT> >);
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors7.0"));
#endif
    // invert the look-up direction
    vector<vector<ArrayHelper::SrcInfo<SeqIndex<SIT>,SeqIndex<DIT> > > >
      srcInfoTable(sendnbDiffPartnerDeCount);
    int *srcInfoTableInit = new int[sendnbDiffPartnerDeCount];
    for (int i=0; i<sendnbDiffPartnerDeCount; i++){
      srcInfoTable[i].resize(sendnbPartnerDeCount[i]);
      srcInfoTableInit[i] = 0;   // reset
    }
    char *localDeFactorBuffer;
    if (!srcTermProcessingExplicitZero){
      // need factors on the SEND side
      // alignment char *localDeFactorBuffer = new char[localDeFactorCount * dataSizeFactors];
      int qwords = (localDeFactorCount * dataSizeFactors) / 8;
      if ((localDeFactorCount * dataSizeFactors) % 8) ++qwords;
#ifdef ASMM_STORE_MEMLOG_on
      VM::logMemInfo(std::string("ASMMStoreNbVectors7.1"));
#endif
      localDeFactorBuffer = (char *)(new double[qwords]);
      localrc = xxe->storeData(localDeFactorBuffer, qwords*8); // XXE garbage
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_STORE_MEMLOG_on
      VM::logMemInfo(std::string("ASMMStoreNbVectors7.2"));
#endif
    }
    for (int i=0; i<localDeFactorCount; i++){
      int partnerDeListIndex = partnerDeRef[i];
      int index2 = srcInfoTableInit[partnerDeListIndex]++;
      srcInfoTable[partnerDeListIndex][index2].linIndex =
        srcLinSeqVect[j][index2Ref2[i]].linIndex;
      srcInfoTable[partnerDeListIndex][index2].vectorLength = 1;
      srcInfoTable[partnerDeListIndex][index2].seqIndex =
        srcLinSeqVect[j][index2Ref2[i]].seqIndex;
      srcInfoTable[partnerDeListIndex][index2].partnerSeqIndex =
        srcLinSeqVect[j][index2Ref2[i]].factorList[factorIndexRef[i]]
        .partnerSeqIndex;
      if (!tensorMixFlag){
        // default into tensor for tensor src/dst mode
        srcInfoTable[partnerDeListIndex][index2].partnerSeqIndex.setTensor(
          srcInfoTable[partnerDeListIndex][index2].seqIndex.getTensor());
      }
      if (!srcTermProcessingExplicitZero){
        // copy the factors to keep on SEND side
        char *localDeFactorBufferEntry = localDeFactorBuffer + i*dataSizeFactors;
        memcpy(localDeFactorBufferEntry,
          srcLinSeqVect[j][index2Ref2[i]].factorList[factorIndexRef[i]].factor,
          dataSizeFactors);
        srcInfoTable[partnerDeListIndex][index2].factor =
          (void *)(localDeFactorBufferEntry);
      }else{
        srcInfoTable[partnerDeListIndex][index2].factor = NULL;
      }
    }

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors7.3"));
#endif

    // garbage collection
    delete [] srcInfoTableInit;
    delete [] index2Ref2;
    delete [] factorIndexRef;
    delete [] partnerDeRef;

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors7.4"));
#endif

    // force vectors out of scope by swapping with empty vector, to free memory
    vector<AssociationElement<SeqIndex<SIT>,SeqIndex<DIT> > > ().swap(srcLinSeqVect[j]);
  
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors8.0"));
#endif
    
#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors8.1"));
#endif

    // sort each "sendnbDiffPartnerDeCount group" (opposite of dst)
    if (!vectorFlag){
      // no vectorization possible -> sort for scalar optimization
      for (int i=0; i<sendnbDiffPartnerDeCount; i++)
        sort(srcInfoTable[i].begin(), srcInfoTable[i].end(),
          ArrayHelper::scalarOrderSrcInfo<SeqIndex<SIT>,SeqIndex<DIT> >);
    }else if (srcTensorContigLength == 1){
      // support vectorization during execution, but nothing to deflate here
      // sort for scalar optimization
      for (int i=0; i<sendnbDiffPartnerDeCount; i++)
        sort(srcInfoTable[i].begin(), srcInfoTable[i].end(),
          ArrayHelper::scalarOrderSrcInfo<SeqIndex<SIT>,SeqIndex<DIT> >);
    }else{
      // vectorization
      // sort vector optimization
      for (int i=0; i<sendnbDiffPartnerDeCount; i++){
        sort(srcInfoTable[i].begin(), srcInfoTable[i].end(),
          ArrayHelper::vectorOrderSrcInfo<SeqIndex<SIT>,SeqIndex<DIT> >);
#ifdef ASMM_STORE_LOG_on
        {
          std::stringstream msg;
          for (int k=0; k<srcInfoTable[i].size(); k++){
            msg.str("");  // clear
            msg << "ASMM_STORE_LOG:" << __LINE__ <<
              " srcInfoTable[" << i << "][" << k << "].seqIndex = "
              << srcInfoTable[i][k].seqIndex.decompSeqIndex << "/"
              << srcInfoTable[i][k].seqIndex.getTensor() << ", "
              << srcInfoTable[i][k].partnerSeqIndex.decompSeqIndex << "/"
              << srcInfoTable[i][k].partnerSeqIndex.getTensor()
              << " .factor = " << srcInfoTable[i][k].factor;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
        }
#endif
        // vectorize -> deflate srcInfoTable
        typename vector<ArrayHelper::SrcInfo<SeqIndex<SIT>,SeqIndex<DIT> > >::
          iterator rangeStart = srcInfoTable[i].begin();
        typename vector<ArrayHelper::SrcInfo<SeqIndex<SIT>,SeqIndex<DIT> > >::
          iterator rangeStop = rangeStart;
        typename vector<ArrayHelper::SrcInfo<SeqIndex<SIT>,SeqIndex<DIT> > >::
          iterator rangeWrite = rangeStart;
        while (rangeStart != srcInfoTable[i].end()){
          int vectorLength = 1; // initialize
          SIT decompSeqIndex = rangeStart->seqIndex.decompSeqIndex;
          int linIndex = rangeStart->linIndex;
          rangeStop++;
          while((rangeStop != srcInfoTable[i].end())
            && (rangeStop->seqIndex.decompSeqIndex == decompSeqIndex)
            && (rangeStop->linIndex == (linIndex + 1))){
            linIndex = rangeStop->linIndex;
            ++vectorLength;
            rangeStop++;
          }
#ifdef ASMM_STORE_LOG_on
          {
            std::stringstream msg;
            msg << "ASMM_STORE_LOG:" << __LINE__ <<
              " srcTensorContigLength: " << srcTensorContigLength << 
              " vectorLength: " << vectorLength << " decompSeqIndex: " <<
              decompSeqIndex;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
#endif
          if ((rangeWrite != srcInfoTable[i].begin())
            && ((rangeWrite-1)->vectorLength != vectorLength)){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
            "vectorization failed", ESMC_CONTEXT, &rc);
            return rc;
          }
          if (vectorLength != srcTensorContigLength){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
            "vectorization failed", ESMC_CONTEXT, &rc);
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

#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors9.0"));
#endif

#ifdef ASMM_STORE_LOG_on
    {
      std::stringstream msg;
      for (int i=0; i<sendnbDiffPartnerDeCount; i++){
        msg.str("");  // clear
        msg << "ASMM_STORE_LOG:" << __LINE__ << " srcInfoTable[" << i <<
          "].size()=" << srcInfoTable[i].size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
    }
#endif

    // construct send elements
    int oldSize = sendnbVector.size();
    sendnbVector.resize(oldSize+sendnbDiffPartnerDeCount);
    for (int i=0; i<sendnbDiffPartnerDeCount; i++){
      int vectorLength = srcInfoTable[i].begin()->vectorLength;
      // use a temporary vector to aide in deflating of the message to be sent
      vector<ArrayHelper::Deflator<SIT> > deflator(srcInfoTable[i].size());
      for (unsigned k=0; k<srcInfoTable[i].size(); k++){
        // fill
        deflator[k].seqIndex  = srcInfoTable[i][k].seqIndex;
        deflator[k].index     = srcInfoTable[i][k].linIndex;
      }
#ifdef MSG_DEFLATE
      // sort -> cut off duplic.
      sort(deflator.begin(), deflator.end());
      deflator.erase(unique(deflator.begin(),deflator.end()),deflator.end());

#ifdef MSG_DEFLATE_DEBUG
      {
        std::stringstream msg;
        msg << "ASMM_STORE_LOG:" << __LINE__ <<
          " send: after erase: deflator.size()=" << deflator.size();
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#if 0
        for (int k=0; k<deflator.size(); k++){
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ <<
            " send: deflator[" << k << "]: index=" << deflator[k].index <<
            ", seqIndex=" << deflator[k].seqIndex.decompSeqIndex;
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        }
#endif
      }
#endif

#endif
      // determine contiguous runs in linIndex to minimize memcpy overhead
      vector<ArrayHelper::LinIndexContigBlock> linIndexContigBlockList;
      if (!srcTermProcessingExplicitPositive){
        // linIndexContigBlockList will be used -> set it up here
        ArrayHelper::LinIndexContigBlock block;
        block.linIndex = deflator[0].index;
        block.linIndexCount = 1;
        linIndexContigBlockList.push_back(block);
        for (unsigned k=1; k<deflator.size(); k++){
          if (deflator[k-1].index + vectorLength == deflator[k].index){
            // contiguous step in linIndex
            ++(linIndexContigBlockList.back().linIndexCount);
          }else{
            // discontiguous jump in linIndex
            block.linIndex = deflator[k].index;
            block.linIndexCount = 1;
            linIndexContigBlockList.push_back(block);
          }
        }
#ifdef ASMM_STORE_LOG_on
        {
          std::stringstream msg;
          msg << "ASMM_STORE_LOG:" << __LINE__ << " sendnbDiffDe=" << i <<
            " linIndexContigBlockList.size()=" << linIndexContigBlockList.size();
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        }
#endif

#ifdef MSG_DEFLATE_DEBUG
        {
          std::stringstream msg;
          for (unsigned k=0; k<linIndexContigBlockList.size(); k++){
            msg.str("");  // clear
            msg << "ASMM_STORE_LOG:" << __LINE__ <<
              " linIndexContigBlockList[" << k << "]: linIndex=" << 
              linIndexContigBlockList[k].linIndex << " linIndexCount=" <<
              linIndexContigBlockList[k].linIndexCount;
            ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
          }
        }
#endif

      }
      
#ifdef ASMM_STORE_MEMLOG_on
      VM::logMemInfo(std::string("ASMMStoreNbVectors9.0.1"));
#endif

      // determine buffer size needed
      int neededBufferSize = sendnbPartnerDeCount[i]; // default to largest
      if (srcTermProcessingExplicitZero){
        // only need a buffer large enough to hold the deflated size
        neededBufferSize = deflator.size() * vectorLength;
      }
      // intermediate buffer (in case it is needed)
      int qwords = (neededBufferSize * dataSizeSrc) / 8;
      if ((neededBufferSize * dataSizeSrc) % 8) ++qwords;
      char *buffer = (char *)(new double[qwords]);
      // store buffer information in BufferInfo for XXE buffer control
      localrc = xxe->storeBufferInfo(buffer,
        neededBufferSize * dataSizeSrc,
        neededBufferSize * dataSizeSrc / vectorLength);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      // prepare DE/PET info
      int srcDe = srcLocalDeToDeMap[j];
      int dstDe = sendnbPartnerDeList[i];
      int dstPet;   //TODO: DE-based comms
      dstDelayout->getDEMatchPET(dstDe, *vm, NULL, &dstPet, 1);
      // fill values into sendnbVector
      int ii = oldSize + i;
      sendnbVector[ii].dstPet = dstPet;
      sendnbVector[ii].dstDe = dstDe;
      sendnbVector[ii].dstLocalDe = i;
      sendnbVector[ii].srcDe = srcDe;
      sendnbVector[ii].srcLocalDe = j;
      sendnbVector[ii].partnerDeDataCount = deflator.size();
      sendnbVector[ii].vectorFlag = vectorFlag;
      sendnbVector[ii].vectorLength = vectorLength;
      sendnbVector[ii].srcInfoTable.swap(srcInfoTable[i]);
      if (srcTermProcessingExplicitZero){
        // the srcInfoTable is no longer needed under this condition
        vector<ArrayHelper::SrcInfo<SeqIndex<SIT>,SeqIndex<DIT> > > 
          ().swap(sendnbVector[ii].srcInfoTable);
      }
      sendnbVector[ii].linIndexContigBlockList.swap(linIndexContigBlockList);
      if (srcTermProcessingExplicitPositive){
        // the linIndexContigBlockList is no longer needed under this condition
        vector<ArrayHelper::LinIndexContigBlock>
          ().swap(sendnbVector[ii].linIndexContigBlockList);
      }
      sendnbVector[ii].bufferInfo = (char **)xxe->getBufferInfoPtr();
      sendnbVector[ii].localPet = localPet;
      sendnbVector[ii].petCount = petCount;
#ifdef ASMM_STORE_LOG_on
      {
        std::stringstream msg;
        msg << "ASMM_STORE_LOG:" << __LINE__ << " sendnbElement dstPet="
          << dstPet << " vectorLength=" << vectorLength;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        for (int k=0; k<sendnbVector[ii].linIndexContigBlockList.size(); k++){
          msg.str("");  // clear
          msg << "ASMM_STORE_LOG:" << __LINE__ << " linIndexContigBlockList["
            << k << "]: linIndex=" << 
            sendnbVector[ii].linIndexContigBlockList[k].linIndex <<
            ", linIndexCount=" << 
            sendnbVector[ii].linIndexContigBlockList[k].linIndexCount;
          ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
        }
      }
#endif
#ifdef ASMM_STORE_LOG_on
      {
        std::stringstream msg;
        msg << "ASMM_STORE_LOG:" << __LINE__ << " sendnbElement dstPet="
          << dstPet << " vectorLength=" << vectorLength;
        ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
      }
#endif
    } // for i - sendnbDiffPartnerDeCount
#ifdef ASMM_STORE_MEMLOG_on
    VM::logMemInfo(std::string("ASMMStoreNbVectors9.1"));
#endif
    // garbage collection
    delete [] sendnbPartnerDeList;
    delete [] sendnbPartnerDeCount;
  } // for j - srcLocalDeCount

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors10.0"));
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t10);   //gjt - profile
#endif

  // --------------------------------------------------------------
  // recv and send patterns have been determined, ready to use them
  // --------------------------------------------------------------

  // sort recv and send vectors to lower communication contention
  // sorting also ensures correct ordering of sendnb and recvnb calls w/o tags
  sort(recvnbVector.begin(), recvnbVector.end());
  sort(sendnbVector.begin(), sendnbVector.end());

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors10.1"));
#endif

#define FORCE_SHRINK_AFTER_SORT_on
#ifdef FORCE_SHRINK_AFTER_SORT_on
  // shrink size of recvnbVector to where it was before sort
  // may come at a performance hit
  vector<ArrayHelper::RecvnbElement<SeqIndex<DIT>,SeqIndex<SIT> > >
    recvnbV = recvnbVector;
  recvnbV.swap(recvnbVector);
  vector<ArrayHelper::RecvnbElement<SeqIndex<DIT>,SeqIndex<SIT> > >
    ().swap(recvnbV);
  // shrink size of sendnbVector to where it was before sort
  // may come at a performance hit
  vector<ArrayHelper::SendnbElement<SeqIndex<SIT>,SeqIndex<DIT> > >
    sendnbV = sendnbVector;
  sendnbV.swap(sendnbVector);
  vector<ArrayHelper::SendnbElement<SeqIndex<SIT>,SeqIndex<DIT> > >
    ().swap(sendnbV);
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors10.2"));
#endif

#ifdef ASMM_STORE_COMMMATRIX_on
#if 0
  {
    std::stringstream msg;
    msg << "Storing CommMatrix: sendnbVector.size()=" << sendnbVector.size()
        << " recvnbVector.size()=" << recvnbVector.size();
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
  // store the communication matrix in compact (sparse) distributed fashion,
  vector<int> *commMatrixDstPet        = new vector<int>(sendnbVector.size());
  vector<int> *commMatrixDstDataCount  = new vector<int>(sendnbVector.size());
  vector<int> *commMatrixSrcPet        = new vector<int>(recvnbVector.size());
  vector<int> *commMatrixSrcDataCount  = new vector<int>(recvnbVector.size());
  for (unsigned i=0; i<sendnbVector.size(); i++){
    (*commMatrixDstPet)[i]       = sendnbVector[i].dstPet;
    (*commMatrixDstDataCount)[i] = sendnbVector[i].partnerDeDataCount;
  }
  for (unsigned i=0; i<recvnbVector.size(); i++){
    (*commMatrixSrcPet)[i]       = recvnbVector[i].srcPet;
    (*commMatrixSrcDataCount)[i] = recvnbVector[i].partnerDeDataCount;
  }
  // attach the communication matrix to the routehandle
  localrc = (*routehandle)->setStorage(commMatrixDstPet, 1);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  localrc = (*routehandle)->setStorage(commMatrixDstDataCount, 2);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  localrc = (*routehandle)->setStorage(commMatrixSrcPet, 3);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  localrc = (*routehandle)->setStorage(commMatrixSrcDataCount, 4);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t11);   //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreNbVectors11.0"));
#endif

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


template<typename SIT, typename DIT>
  int sparseMatMulStoreEncodeXXEStream(VM *vm,
  vector<ArrayHelper::SendnbElement<SIT,DIT> > &sendnbVector,
  vector<ArrayHelper::RecvnbElement<DIT,SIT> > &recvnbVector,
  int srcTermProcessing, int pipelineDepth, XXE::TKId elementTK,
  XXE::TKId valueTK, XXE::TKId factorTK,
  int dataSizeSrc, int dataSizeDst, int dataSizeFactors, int srcLocalDeCount,
  int dstLocalDeCount, const int *dstLocalDeTotalElementCount, char **rraList,
  int rraCount, int vectorLength, XXE *xxe);

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::sparseMatMulStoreEncodeXXE()"
//BOPI
// !IROUTINE:  ESMCI::sparseMatMulStoreEncodeXXE
//
// !INTERFACE:
template<typename SIT, typename DIT> int sparseMatMulStoreEncodeXXE(
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
  ESMC_TypeKind_Flag typekindFactors,     // in
  ESMC_TypeKind_Flag typekindSrc,         // in
  ESMC_TypeKind_Flag typekindDst,         // in
  vector<ArrayHelper::SendnbElement<SIT,DIT> > &sendnbVector, // in
  vector<ArrayHelper::RecvnbElement<DIT,SIT> > &recvnbVector, // in
  const int *dstLocalDeTotalElementCount, // in
  char **rraList,                         // in
  int rraCount,                           // in
  RouteHandle **routehandle,              // inout - handle to precomputed comm
  bool undistributedElementsPresent,      // in
#ifdef ASMM_STORE_TIMING_on
  double *t12pre, double *t12, double *t13, double *t14,
#endif
  int *srcTermProcessingArg,    // inout (optional)
                                // if (NULL) -> auto-tune, no pass back
                                // if (!NULL && -1) -> auto-tune, pass back
                                // if (!NULL && >=0) -> no auto-tune, use input
  int *pipelineDepthArg         // inout (optional)
                                // if (NULL) -> auto-tune, no pass back
                                // if (!NULL && -1) -> auto-tune, pass back
                                // if (!NULL && >=0) -> no auto-tune, use input
  ){
//
// !DESCRIPTION:
//    The incoming sparse matrix information is provided in (srcDe, dstDe) pair
//    specific SendnbElement and RecvnbElement objects. These elements are
//    stored in the two incoming vectors:
//
//      sendnbVector
//      recvnbVector
//
//    These two vectors contain as many objects as there are srcDe, dstDe on
//    the localPet, respectively.
//
//    In this method, sendnbVector and recvnbVector are used to encode the
//    actual XXE stream by calling sparseMatMulStoreEncodeXXEStream(). This
//    stream encode routine allows two stream parameters to be specified:
//    srcTermProcessing and pipelineDepth. sparseMatMulStoreEncodeXXE() calls
//    sparseMatMulStoreEncodeXXEStream() multiple times with different settings
//    for srcTermProcessing and pipelineDepth, scanning the parameter space to
//    find an optimum parameter setting for the current machine characteristic
//    and the communication pattern provided in sendnbVector and recvnbVector.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE1.0"));
#endif

  // get a handle on the XXE stored in routehandle
  XXE *xxe = (XXE *)(*routehandle)->getStorage();

  // set typekind in xxe which is used to check Arrays before ASMM execution
  xxe->typekind[0] = typekindFactors;
  xxe->typekind[1] = typekindSrc;
  xxe->typekind[2] = typekindDst;
  // set the superVectorOkay flag
  xxe->superVectorOkay = !undistributedElementsPresent; // if no undistr. elemts
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
  int dataSizeFactors = ESMC_TypeKind_FlagSize(typekindFactors);

  int dataSizeSrc = ESMC_TypeKind_FlagSize(typekindSrc);
  int srcLocalDeCount = srcDelayout->getLocalDeCount();

  int dataSizeDst = ESMC_TypeKind_FlagSize(typekindDst);
  int dstLocalDeCount = dstDelayout->getLocalDeCount();

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

  bool vectorFlag = !(tensorMixFlag ||
    (srcTensorContigLength != dstTensorContigLength));

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t12pre);   //gjt - profile
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE2.0"));
#endif

  // store current XXE parameter in order to efficiently rewrite multiple times
  const int startCount = xxe->count;
  const int startDataCount = xxe->dataCount;
  const int startCommhandleCount = xxe->commhandleCount;
  const int startXxeSubCount = xxe->xxeSubCount;
  const int startBufferInfoListSize = xxe->bufferInfoList.size();

  double dtMin;           // to find minimum time

  // Need correct setting of vectorLength for the XXE exec() calls.
  int vectorLength = 0; // initialize

  // For the case that vectorLength is relevant, i.e. vectorFlag is set to
  // true, and the XXE stream contains elements that are vectorized,
  // srcTensorContigLength and dstTensorContigLength must be equivalent, and
  // define the vectorLength.
  if (vectorFlag)
    vectorLength = srcTensorContigLength; // consistent vectorLength

  //TODO: Implement a smarter optimization algorithm to optimize both
  //TODO: srcTermProcessing and pipelineDepth in a concurrent manner, rather
  //TODO: than the one-after-the-other approach below.

  // Optimize srcTermProcessing, finding srcTermProcessingOpt:
  int srcTermProcessingOpt; // optimium src term processing ... to be determined

#define FORCE_SRCTERMPROCESSING_off
#ifdef FORCE_SRCTERMPROCESSING_on
  int dummyVar = 0; // force to do all processing on the dst side
  if (srcTermProcessingArg && *srcTermProcessingArg < 0)
    *srcTermProcessingArg = dummyVar; // replace incoming value
  else
    srcTermProcessingArg = &dummyVar; // ignore incoming value
#endif

  if (srcTermProcessingArg && *srcTermProcessingArg >= 0){
    // use the provided srcTermProcessing
#ifdef ASMM_STORE_TUNELOG_on
    char msg[160];
    sprintf(msg, "ASMM_STORE_TUNELOG:%d srcTermProcessingArg = %d"
      " was provided -> do not tune", __LINE__, *srcTermProcessingArg);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
#endif
    srcTermProcessingOpt = *srcTermProcessingArg;
  }else{
    // optimize srcTermProcessing
#ifdef ASMM_STORE_TUNELOG_on
    char msg[160];
    sprintf(msg, "ASMM_STORE_TUNELOG:%d srcTermProcessingArg was NOT"
      " provided -> tuning...", __LINE__);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
#endif
#ifdef WORKAROUND_NONBLOCKPROGRESSBUG
    int pipelineDepth = 1;// only allow one outstanding connection as workaround
#else
    int pipelineDepth = petCount/2; // tricky to pick a good value here for all
                                    // cases (different interconnects!!!)
                                    // therefore need a concurrent opt scheme!!
#endif
    const int srcTermProcMax = 8; // 8 different values in the srcTermProcList[]
    const int srcTermProcList[] = {0, 1, 2, 3, 4, 6, 8, 20};  // trial settings
    for (int srcTermProc=0; srcTermProc<srcTermProcMax; srcTermProc++){
      int srcTermProcessing=srcTermProcList[srcTermProc];
      // start writing a fresh XXE stream
      xxe->clearReset(startCount, startDataCount, startCommhandleCount,
        startXxeSubCount, startBufferInfoListSize);
      localrc = sparseMatMulStoreEncodeXXEStream(vm, sendnbVector, recvnbVector,
        srcTermProcessing, pipelineDepth, elementTK, valueTK, factorTK,
        dataSizeSrc, dataSizeDst, dataSizeFactors, srcLocalDeCount,
        dstLocalDeCount, dstLocalDeTotalElementCount, rraList, rraCount,
        vectorLength, xxe);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;

#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#if 0
      // optimize the XXE entire stream
      localrc = xxe->optimize();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
#endif
      // get XXE ready for execution
      localrc = xxe->execReady();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
      // obtain timing
      double dtAverage = 0.;
      double dtStart, dtEnd;
      const int dtCount = 10;
      for (int i=0; i<dtCount; i++){
        vm->barrier();
        vm->wtime(&dtStart);
        localrc = xxe->exec(rraCount, rraList, &vectorLength,
          0x0|XXE::filterBitRegionTotalZero|XXE::filterBitNbTestFinish
          |XXE::filterBitCancel|XXE::filterBitNbWaitFinishSingleSum);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc)) return rc;
        vm->barrier();
        vm->wtime(&dtEnd);
        dtAverage += dtEnd - dtStart;
      }
      dtAverage /= dtCount;
#ifdef ASMM_STORE_TUNELOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_TUNELOG:" << __LINE__
        << " srcTermProcessing=" << srcTermProcessing
        << " dtAverage=" << dtAverage;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
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
    } // srcTermProc

#ifdef ASMM_STORE_TUNELOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_TUNELOG:" << __LINE__
        << " srcTermProcessingOpt=" << srcTermProcessingOpt
        << " dtMin(local)=" << dtMin;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
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
#ifdef ASMM_STORE_TUNELOG_on
    sprintf(msg, "ASMM_STORE_TUNELOG:%d ... finished tuning, found"
      " srcTermProcessingOpt = %d", __LINE__, srcTermProcessingOpt);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
#endif
    if (srcTermProcessingArg) *srcTermProcessingArg = srcTermProcessingOpt;

  } // finished finding srcTermProcessingOpt

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.0"));
#endif

#ifdef ASMM_STORE_TUNELOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_TUNELOG:" << __LINE__
        << " srcTermProcessingOpt=" << srcTermProcessingOpt
        << " (majority vote)";
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t12);   //gjt - profile
#endif

  // Optimize pipelineDepth, finding pipelineDepthOpt:
  int pipelineDepthOpt;     // optimium pipeline depth ... to be determined

#define FORCE_PIPELINEDEPTH_off
#if (defined FORCE_PIPELINEDEPTH_on && !defined WORKAROUND_NONBLOCKPROGRESSBUG)
  int dummyVar2 = petCount; // force pipeline depth to be "petCount" deep
  if (pipelineDepthArg && *pipelineDepthArg < 0)
    *pipelineDepthArg = dummyVar2; // replace incoming value
  else
    pipelineDepthArg = &dummyVar2; // ignore incoming value
#endif

#ifdef WORKAROUND_NONBLOCKPROGRESSBUG
  int dummyVar2 = 1; // force pipeline depth to be only "1" deep as workaround
  if (pipelineDepthArg && *pipelineDepthArg < 0)
    *pipelineDepthArg = dummyVar2; // replace incoming value
  else
    pipelineDepthArg = &dummyVar2; // ignore incoming value
#endif

  if (pipelineDepthArg && *pipelineDepthArg >= 0){
    // use the provided pipelineDepthArg
#ifdef ASMM_STORE_TUNELOG_on
    char msg[160];
    sprintf(msg, "ASMM_STORE_TUNELOG:%d pipelineDepthArg = %d was provided"
      " -> do not tune", __LINE__, *pipelineDepthArg);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
#endif
    pipelineDepthOpt = *pipelineDepthArg;
  }else{
    // optimize pipeline depth
#ifdef ASMM_STORE_TUNELOG_on
    char msg[160];
    sprintf(msg, "ASMM_STORE_TUNELOG:%d pipelineDepthArg was NOT provided"
      " -> tuning...", __LINE__);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
#endif
    for (int pipelineDepth=1; pipelineDepth<=petCount; pipelineDepth*=2){
      // start writing a fresh XXE stream
      xxe->clearReset(startCount, startDataCount, startCommhandleCount,
        startXxeSubCount, startBufferInfoListSize);
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.1"));
#endif
      localrc = sparseMatMulStoreEncodeXXEStream(vm, sendnbVector, recvnbVector,
        srcTermProcessingOpt, pipelineDepth, elementTK, valueTK, factorTK,
        dataSizeSrc, dataSizeDst, dataSizeFactors, srcLocalDeCount,
        dstLocalDeCount, dstLocalDeTotalElementCount, rraList, rraCount,
        vectorLength, xxe);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;

#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.2"));
#endif
#if 0
      // optimize the XXE entire stream
      localrc = xxe->optimize();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
#endif
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.3"));
#endif
      // get XXE ready for execution
      localrc = xxe->execReady();
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.4"));
#endif
      // obtain timing
      double dtAverage = 0.;
      double dtStart, dtEnd;
      const int dtCount = 10;
      for (int i=0; i<dtCount; i++){
        vm->barrier();
        vm->wtime(&dtStart);
          localrc = xxe->exec(rraCount, rraList, &vectorLength,
            0x0|XXE::filterBitRegionTotalZero|XXE::filterBitNbTestFinish
            |XXE::filterBitCancel|XXE::filterBitNbWaitFinishSingleSum);
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        vm->barrier();
        vm->wtime(&dtEnd);
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.5"));
#endif
        dtAverage += dtEnd - dtStart;
      }
      dtAverage /= dtCount;
#ifdef ASMM_STORE_TUNELOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_TUNELOG:" << __LINE__
        << " pipelineDepth=" << pipelineDepth
        << " dtAverage=" << dtAverage;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.6"));
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

#ifdef ASMM_STORE_TUNELOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_TUNELOG:" << __LINE__
        << " pipelineDepthOpt=" << pipelineDepthOpt
        << " dtMin(local)=" << dtMin;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE9.7"));
#endif
    // all PETs vote on pipelineDepthOpt
    vector<int> pipelineDepthOptList(petCount);
    vm->allgather(&pipelineDepthOpt, &pipelineDepthOptList[0], sizeof(int));
    sort(pipelineDepthOptList.begin(), pipelineDepthOptList.end());
    int votes = 1; // initialize
    int votesMax = 0; // initialize
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
#ifdef ASMM_STORE_TUNELOG_on
    sprintf(msg, "... finished tuning, found pipelineDepthOpt = %d",
      pipelineDepthOpt);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_DEBUG);
#endif
    if (pipelineDepthArg) *pipelineDepthArg = pipelineDepthOpt;

  } // finished finding pipelineDepthOpt

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE10.0"));
#endif

#ifdef ASMM_STORE_TUNELOG_on
    {
      std::stringstream msg;
      msg << "ASMM_STORE_TUNELOG:" << __LINE__
        << " pipelineDepthOpt=" << pipelineDepthOpt
        << " (majority vote)";
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t13);   //gjt - profile
#endif

  // encode with the majority voted pipelineDepthOpt
  xxe->clearReset(startCount, startDataCount, startCommhandleCount,
    startXxeSubCount, startBufferInfoListSize);
  localrc = sparseMatMulStoreEncodeXXEStream(vm, sendnbVector, recvnbVector,
    srcTermProcessingOpt, pipelineDepthOpt, elementTK, valueTK, factorTK,
    dataSizeSrc, dataSizeDst, dataSizeFactors, srcLocalDeCount,
    dstLocalDeCount, dstLocalDeTotalElementCount, rraList, rraCount,
    vectorLength, xxe);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  
#ifdef USE_MALLOC_TRIM
  {
    int mtrim = malloc_trim(0);
#ifdef MALLOC_TRIM_REPORT_on
    std::stringstream msg;
    msg << "malloc_trim(0)#" << __LINE__ << ": " << mtrim;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
#endif
  }
#endif
  
#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE10.1"));
#endif

#if 0
  // optimize the XXE entire stream
  localrc = xxe->optimize();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE10.2"));
#endif

  // get XXE ready for execution
  localrc = xxe->execReady();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE10.3"));
#endif

  // execute final XXE stream for consistent profile data
  vm->barrier();  // ensure all PETs are present before profile run
  localrc = xxe->exec(rraCount, rraList, &vectorLength,
    0x0|XXE::filterBitRegionTotalZero|XXE::filterBitNbTestFinish
    |XXE::filterBitCancel|XXE::filterBitNbWaitFinishSingleSum);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXE11.0"));
#endif

#ifdef ASMM_STORE_TIMING_on
  VMK::wtime(t14);   //gjt - profile
#endif

//---DEBUG-------------------
//char file[160];
//sprintf(file, "xxe.%05d", localPet);
//FILE *fp = fopen(file, "a");
//xxe->print(fp);
//fclose(fp);
//---DEBUG-------------------

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
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
template<typename SIT, typename DIT> int sparseMatMulStoreEncodeXXEStream(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vm,                                 // in
  vector<ArrayHelper::SendnbElement<SIT,DIT> > &sendnbVector, // in - exchange pattern
  vector<ArrayHelper::RecvnbElement<DIT,SIT> > &recvnbVector, // in - exchange pattern
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
  int vectorLength,                       // in
  XXE *xxe                                // inout - XXE stream
  ){
//
// !DESCRIPTION:
//    Encode a pipelined XXE stream with the specified degree of
//    srcTermProcessing for the sparseMatMul exchange pattern defined by
//    recvnbVector and sendnbVector.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream1.0"));
#endif

  try{

#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(0x0, "Wtimer 0", xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif

    // use pipeline approach
    typename vector<ArrayHelper::RecvnbElement<DIT,SIT> >::iterator pRecv =
      recvnbVector.begin();
    typename vector<ArrayHelper::RecvnbElement<DIT,SIT> >::iterator pRecvWait =
      recvnbVector.begin();
    typename vector<ArrayHelper::SendnbElement<SIT,DIT> >::iterator pSend =
      sendnbVector.begin();
    typename vector<ArrayHelper::SendnbElement<SIT,DIT> >::iterator pSendWait =
      sendnbVector.begin();

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream2.0"));
#endif

#define SMM_NONBLOCKINGSTYLE_on

#ifdef SMM_NONBLOCKINGSTYLE_on
    // prepare pipeline
#ifdef OLDSTYLEPIPELINEPREPARE
    for (int i=0; i<pipelineDepth; i++){
      if (pRecv != recvnbVector.end()){
        int k = pRecv - recvnbVector.begin();
        localrc = pRecv->appendRecvnb(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, dataSizeSrc, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pRecv;
      }
      if (pSend != sendnbVector.end()){
        int k = pSend - sendnbVector.begin();
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ << " rraCount=" << rraCount;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
        localrc = pSend->appendSendnb(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, elementTK, valueTK, factorTK, dataSizeSrc, rraList,
          rraCount, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pSend;
      }
    }
#else
    // fill in recvnb's first
    for (int i=0; i<pipelineDepth; i++){
      if (pRecv != recvnbVector.end()){
        int k = pRecv - recvnbVector.begin();
        localrc = pRecv->appendRecvnb(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, dataSizeSrc, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pRecv;
      }
    }
    // fill in the sendnb's next
    for (int i=0; i<pipelineDepth; i++){
      if (pSend != sendnbVector.end()){
        int k = pSend - sendnbVector.begin();
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ << " rraCount=" << rraCount;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
        localrc = pSend->appendSendnb(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, elementTK, valueTK, factorTK, dataSizeSrc, rraList,
          rraCount, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pSend;
      }
    }
#endif
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream3.0"));
#endif

    // append predicated zero operations for the total region
#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(
      XXE::filterBitRegionTotalZero|XXE::filterBitNbStart, "total zero",
      xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif
    for (int i=0; i<dstLocalDeCount; i++){
      int byteCount = dstLocalDeTotalElementCount[i] * dataSizeDst; // init
      bool vectorFlag = false;  // init
      if (vectorLength){
        // vectorLength is consistent between src and dst Array -> can be used
        byteCount /= vectorLength;  // scale with store time vectorLength
        vectorFlag = true;
      }
      localrc = xxe->appendZeroMemsetRRA(
        XXE::filterBitRegionTotalZero|XXE::filterBitNbStart,
        byteCount, srcLocalDeCount + i, vectorFlag);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    }
#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(
      XXE::filterBitRegionTotalZero|XXE::filterBitNbStart,
      "/total zero", xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream4.0"));
#endif

    // append predicated zero operations for targeted dst elements
    for(typename vector<ArrayHelper::RecvnbElement<DIT,SIT> >::iterator pzero =
      recvnbVector.begin(); pzero != recvnbVector.end(); ++pzero){
      localrc = pzero->appendZeroSuperScalar(xxe,
        XXE::filterBitRegionSelectZero|XXE::filterBitNbStart, srcLocalDeCount,
        elementTK);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
    }

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream5.0"));
#endif

#ifdef SMM_NONBLOCKINGSTYLE_on
    // fill pipeline
    bool recvnbOK = true; // initialize
    bool sendnbOK = true; // initialize
    while ((pRecv != recvnbVector.end()) || (pSend != sendnbVector.end())){
      if ((pRecv != recvnbVector.end()) && recvnbOK){
        int k = pRecv - recvnbVector.begin();
        localrc = pRecv->appendRecvnb(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, dataSizeSrc, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pRecv;
      }
      if ((pSend != sendnbVector.end()) && sendnbOK){
        int k = pSend - sendnbVector.begin();
#ifdef ASMM_STORE_LOG_on
  {
    std::stringstream msg;
    msg << "ASMM_STORE_LOG:" << __LINE__ << " rraCount=" << rraCount;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
        localrc = pSend->appendSendnb(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, elementTK, valueTK, factorTK, dataSizeSrc, rraList,
          rraCount, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
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
          // append test and wait calls that trigger productSum
          localrc = pRecvWait->appendTestWaitProductSum(xxe,
            0x0,  // appendTestWaitProductSum() sets correct filter bits:
                  // XXE::filterBitNbTestFinish XXE::filterBitNbWaitFinish
            srcTermProcessing, srcLocalDeCount, elementTK, valueTK, factorTK,
            dataSizeDst, dataSizeSrc, dataSizeFactors, rraList, rraCount, k);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          // append simple wait if summing to be done in a single sum at the end
          localrc = xxe->appendWaitOnIndex(
            0x0|XXE::filterBitNbWaitFinishSingleSum, pRecvWait->recvnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
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
          localrc = xxe->appendTestOnIndex(0x0|XXE::filterBitNbTestFinish,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          localrc = xxe->appendWaitOnIndex(0x0|XXE::filterBitNbWaitFinish,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          localrc = xxe->appendWaitOnIndex(
            0x0|XXE::filterBitNbWaitFinishSingleSum,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          localrc = xxe->appendCancelIndex(0x0|XXE::filterBitCancel,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
          char *tempString = new char[160];
          sprintf(tempString, "done send WaitOnIndex: %d",
            pSendWait->sendnbIndex);
          localrc = xxe->appendWtimer(0x0|XXE::filterBitNbWaitFinish
            |XXE::filterBitNbWaitFinishSingleSum,
            tempString, xxe->count, xxe->count);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
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

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream6.0"));
#endif

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
          // append test and wait calls that trigger productSum
          localrc = pRecvWait->appendTestWaitProductSum(xxe,
            0x0,  // appendTestWaitProductSum() sets correct filter bits:
                  // XXE::filterBitNbTestFinish XXE::filterBitNbWaitFinish
            srcTermProcessing, srcLocalDeCount, elementTK, valueTK, factorTK,
            dataSizeDst, dataSizeSrc, dataSizeFactors, rraList, rraCount, k);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          // append simple wait if summing to be done in a single sum at the end
          localrc = xxe->appendWaitOnIndex(
            0x0|XXE::filterBitNbWaitFinishSingleSum, pRecvWait->recvnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
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
          localrc = xxe->appendTestOnIndex(0x0|XXE::filterBitNbTestFinish,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          localrc = xxe->appendWaitOnIndex(0x0|XXE::filterBitNbWaitFinish,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          localrc = xxe->appendWaitOnIndex(
            0x0|XXE::filterBitNbWaitFinishSingleSum,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          localrc = xxe->appendCancelIndex(0x0|XXE::filterBitCancel,
            pSendWait->sendnbIndex);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#ifdef ASMM_EXEC_PROFILE_on
          char *tempString = new char[160];
          sprintf(tempString, "done send WaitOnIndex: %d",
            pSendWait->sendnbIndex);
          localrc = xxe->appendWtimer(0x0|XXE::filterBitNbWaitFinish
            |XXE::filterBitNbWaitFinishSingleSum,
            tempString, xxe->count, xxe->count);
          if (ESMC_LogDefault.MsgFoundError(localrc,
            ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          delete [] tempString;
#endif
          ++pSendWait;
        }
      }
    }
    // alternatively could post all XXE::waitOnAllSendnb at the end
    // -> not sure what gives better performance.
    //  localrc = xxe->appendWaitOnAllSendnb(0x0);
    //  if (ESMC_LogDefault.MsgFoundError(localrc,
    //    ESMCI_ERR_PASSTHRU, &rc)) return rc;

    // append a single productSum operation that considers _all_ of the
    // incoming elements and orders them according the strict canonical
    // TERMORDER_SRCSEQ order
    localrc = ArrayHelper::RecvnbElement<DIT,SIT>::appendSingleProductSum(xxe,
      0x0|XXE::filterBitNbWaitFinishSingleSum, srcTermProcessing,
      srcLocalDeCount, elementTK, valueTK, factorTK, dataSizeDst,
      dataSizeSrc, dataSizeFactors, rraList, rraCount, recvnbVector);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

#else

    // use BLOCKING comms instead!!!!!!!!
    // note that this will produce a routehandle that cannot be executed
    // in non-blocking style.
    // TODO: the BLOCKING option should really be added with a special
    // TODO: pedication bit set so it can be executed for blocking user calls,
    // TODO: of course only if it has been determined as being faster by the
    // TODO: tuning phase over the non-blocking counter part implementation.

    while ((pRecv != recvnbVector.end()) || (pSend != sendnbVector.end())){
      int recvStage = -1; // invalidate
      if (pRecv != recvnbVector.end()){
        recvStage = (localPet - pRecv->srcPet + petCount) % petCount;
      }
      int sendStage = -1; // invalidate
      if (pSend != sendnbVector.end()){
        sendStage = (pSend->dstPet - localPet + petCount) % petCount;
      }
      // deal with invalid stages here (only one can be invalid!!!)
      if (recvStage == -1)
        recvStage = sendStage + 1;  // allow the send to post
      else if (sendStage == -1)
        sendStage = recvStage + 1;  // allow the recv to post
      // ensure to schedule sends and receives correctly
      if (sendStage == recvStage){
        int kSend = pSend - sendnbVector.begin();
        int kRecv = pRecv - recvnbVector.begin();
        localrc = pSend->appendSendRecv(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, elementTK, valueTK, factorTK, dataSizeSrc, rraList,
          rraCount, kSend, pRecv, kRecv);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pSend;
        ++pRecv;
      }
      if (sendStage < recvStage){
        int k = pSend - sendnbVector.begin();
        localrc = pSend->appendSend(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, elementTK, valueTK, factorTK, dataSizeSrc, rraList,
          rraCount, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pSend;
      }
      if (recvStage < sendStage){
        int k = pRecv - recvnbVector.begin();
        localrc = pRecv->appendRecv(xxe, 0x0|XXE::filterBitNbStart,
          srcTermProcessing, dataSizeSrc, k);
        if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
        ++pRecv;
      }
    }

    // here all blk. comms are done ... -> simple ProductSum() elements follow.

    while (pRecvWait!=recvnbVector.end()){
      localrc = pRecvWait->appendProductSum(xxe,
        0x0, srcTermProcessing, srcLocalDeCount,
        elementTK, valueTK, factorTK, dataSizeDst, dataSizeSrc,
        dataSizeFactors, rraList, rraCount);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      ++pRecvWait;
    }

#endif

#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(0x0, "Wtimer End", xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif

#ifdef ASMM_EXEC_PROFILE_on
    localrc = xxe->appendWtimer(0x0, "Wtimer End2", xxe->count, xxe->count);
    if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
#endif

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream7.0"));
#endif

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

#ifdef ASMM_STORE_MEMLOG_on
  VM::logMemInfo(std::string("ASMMStoreEncodeXXEStream8.0"));
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
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  ESMC_CommFlag commflag,               // in    - communication options
  bool *finishedflag,                   // out   - TEST ops finished or not
  bool *cancelledflag,                  // out   - any cancelled operations
  ESMC_Region_Flag zeroflag,            // in    - ESMC_REGION_TOTAL:
                                        //          -> zero out total region
                                        //         ESMC_REGION_SELECT:
                                        //          -> zero out target points
                                        //         ESMC_REGION_EMPTY:
                                        //          -> don't zero out any points
  ESMC_TermOrder_Flag termorderflag,    // in    - ESMC_TERMORDER_SRCSEQ:
                                        //          -> strict srcSeqInd order
                                        //         ESMC_TERMORDER_SRCPET:
                                        //          -> order by src PET & seqInd
                                        //         ESMC_TERMORDER_FREE:
                                        //          -> free order
  bool checkflag,                       // in    - false: (def.) basic checks
                                        //         true:  full input check
  bool haloFlag                         // in    - support halo conditions
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

#ifdef ASMM_EXEC_TIMING_on
    double t0, t1, t2, t3, t4, t5, t6, t7;            //gjt - profile
    VMK::wtime(&t0);      //gjt - profile
#endif

  // basic input checking
  bool srcArrayFlag = false;
  if (srcArray != ESMC_NULL_POINTER) srcArrayFlag = true;
  bool dstArrayFlag = false;
  if (dstArray != ESMC_NULL_POINTER) dstArrayFlag = true;

  // srcArray and dstArray must not point to the identical Array object
  if (!haloFlag && (srcArrayFlag && dstArrayFlag)){
    if (srcArray == dstArray){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "srcArray and dstArray must not be identical", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

#ifdef ASMM_EXEC_TIMING_on
  VMK::wtime(&t1);      //gjt - profile
#endif

  // get a handle on the XXE stored in routehandle
  XXE *xxe = (XXE *)(*routehandle)->getStorage();

  if (xxe == NULL){
    // NOP
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  // point back to the routehandle inside of xxe
  xxe->setRouteHandle(*routehandle);

  // conditionally perform full input checks
  if (checkflag){
    // Warning banner
    ESMC_LogDefault.Write("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      ESMC_LOGMSG_WARN);
    ESMC_LogDefault.Write("!!! Calling " ESMC_METHOD " with CHECKFLAG!",
      ESMC_LOGMSG_WARN);
    ESMC_LogDefault.Write("!!! Extra checking comes at the cost  !!!",
      ESMC_LOGMSG_WARN);
    ESMC_LogDefault.Write("!!! of performance. Only use for      !!!",
      ESMC_LOGMSG_WARN);
    ESMC_LogDefault.Write("!!! debugging, NOT for production!    !!!",
      ESMC_LOGMSG_WARN);
    ESMC_LogDefault.Write("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      ESMC_LOGMSG_WARN);
    
    // check that srcArray's typekind matches
    if (srcArrayFlag && (xxe->typekind[1] != srcArray->getTypekind())){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "TypeKind mismatch between srcArray argument and precomputed XXE",
        ESMC_CONTEXT, &rc);
      return rc;
    }
    // check that dstArray's typekind matches
    if (dstArrayFlag && (xxe->typekind[2] != dstArray->getTypekind())){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "TypeKind mismatch between dstArray argument and precomputed XXE",
        ESMC_CONTEXT, &rc);
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

  // deal with finishedflag argument which can be NULL, but is also needed
  // locally -> therefore ensure that finishedflag locally is always valid
  bool finishedflagLocal;
  if (!finishedflag) finishedflag = &finishedflagLocal;

#ifdef ASMM_EXEC_TIMING_on
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

#ifdef ASMM_EXEC_TIMING_on
  VMK::wtime(&t3);      //gjt - profile
#endif

  // set filterBitField
  int filterBitField = 0x0; // init. to execute _all_ operations in XXE stream

  // set filters according to commflag and termorderflag
  if (commflag==ESMF_COMM_BLOCKING){
    // blocking mode
    if (termorderflag == ESMC_TERMORDER_SRCSEQ){
      // strict order of terms in single sum, not partial sums
      filterBitField |= XXE::filterBitNbWaitFinish; // no NbWaitFinish ops
      filterBitField |= XXE::filterBitNbTestFinish; // no NbTestFinish ops
      filterBitField |= XXE::filterBitCancel;       // no Cancel ops
      // -> this leaves SingleSum ops enabled
#ifdef ASMM_EXEC_INFO_on
      ESMC_LogDefault.Write("SMM exec: COMM_BLOCKING, TERMORDER_SRCSEQ",
        ESMC_LOGMSG_DEBUG);
#endif
    }else if (termorderflag == ESMC_TERMORDER_SRCPET){
      // order of terms by src PET, allow partial sums of terms from same PET
      filterBitField |= XXE::filterBitNbTestFinish; // no NbTestFinish ops
      filterBitField |= XXE::filterBitCancel;       // no Cancel ops
      filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // no SingleSum ops
      // -> this leaves NbWaitFinish ops enabled
#ifdef ASMM_EXEC_INFO_on
      ESMC_LogDefault.Write("SMM exec: COMM_BLOCKING, TERMORDER_SRCPET",
        ESMC_LOGMSG_DEBUG);
#endif
    }else if (termorderflag == ESMC_TERMORDER_FREE){
      // completely free order of terms, allow any partial sums
#ifdef ENSURE_TO_LIMIT_OUTSTANDING_NBCOMMS
//TODO: This branch ensures that there are never more than pipelineDepth
//TODO: outstanding non-blocking comms held by this PET. This is basically
//TODO: makes this the same as ESMC_TERMORDER_SRCPET for now. We have had
//TODO: issues with the original ESMC_TERMORDER_FREE, for problems where
//TODO: some PETs send/recv a lot of messages, and there are enough delays
//TODO: so that first time the CommTest comes to check, the messages are not
//TODO: yet complete. CommWait will wait for this, and then clear out from
//TODO: MPI layer.
//TODO: In the long run want to implement a pipelineDepthProgressor operation
//TODO: into the XXE stream, as to limit outstanding non-blocking comms,
//TODO: in a dynamic fashion, without doing a blocking Wait. This is work in
//TODO: progress. Until then, if need be, turn on
//TODO:       ENSURE_TO_LIMIT_OUTSTANDING_NBCOMMS
//TODO: By default this is NOT turned on, but keep using the
//TODO: exsiting ESMC_TERMORDER_FREE implementations with risking too many
//TODO: outstanding nb-comms, under just the right conditions!
      filterBitField |= XXE::filterBitNbTestFinish; // no NbTestFinish ops
#else
      filterBitField |= XXE::filterBitNbWaitFinish; // no NbWaitFinish ops
#endif
      filterBitField |= XXE::filterBitCancel;       // no Cancel ops
      filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // no SingleSum ops
#ifdef ASMM_EXEC_INFO_on
      ESMC_LogDefault.Write("SMM exec: COMM_BLOCKING, TERMORDER_FREE",
        ESMC_LOGMSG_DEBUG);
#endif
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "termorderflag choice not supported under COMM_BLOCKING",
        ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
  }else if (commflag==ESMF_COMM_NBSTART){
    // non-blocking start -> independent of termorderflag
    filterBitField |= XXE::filterBitNbWaitFinish;     // set NbWaitFinish filter
    filterBitField |= XXE::filterBitNbTestFinish;     // set NbTestFinish filter
    filterBitField |= XXE::filterBitCancel;           // set Cancel filter
    filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef ASMM_EXEC_INFO_on
    ESMC_LogDefault.Write("SMM exec: COMM_NBSTART",
      ESMC_LOGMSG_DEBUG);
#endif
  }else if(commflag==ESMF_COMM_NBTESTFINISH){
    // non-blocking test and finish
    //TODO: implement the other TERMORDER options
    if (termorderflag == ESMC_TERMORDER_FREE){
      filterBitField |= XXE::filterBitNbStart;          // set NbStart filter
      filterBitField |= XXE::filterBitNbWaitFinish;     // set NbWaitFinish filter
      filterBitField |= XXE::filterBitCancel;           // set Cancel filter
      filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef ASMM_EXEC_INFO_on
      ESMC_LogDefault.Write("SMM exec: COMM_NBTESTFINISH, TERMORDER_FREE",
        ESMC_LOGMSG_DEBUG);
#endif
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "termorderflag choice not supported under COMM_NBTESTFINISH",
        ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
  }else if(commflag==ESMF_COMM_NBWAITFINISH){
    // non-blocking wait and finish
    //TODO: implement the other TERMORDER options
    if (termorderflag == ESMC_TERMORDER_FREE){
      filterBitField |= XXE::filterBitNbStart;          // set NbStart filter
      filterBitField |= XXE::filterBitNbTestFinish;     // set NbTestFinish filter
      filterBitField |= XXE::filterBitCancel;           // set Cancel filter
      filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef ASMM_EXEC_INFO_on
      ESMC_LogDefault.Write("SMM exec: COMM_NBWAITFINISH TERMORDER_FREE",
        ESMC_LOGMSG_DEBUG);
#endif
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "termorderflag choice not supported under COMM_NBWAITFINISH",
        ESMC_CONTEXT, &rc);
      return rc;  // bail out
    }
  }else if(commflag==ESMF_COMM_CANCEL){
    // cancel
    filterBitField |= XXE::filterBitNbStart;          // set NbStart filter
    filterBitField |= XXE::filterBitNbWaitFinish;     // set NbWaitFinish filter
    filterBitField |= XXE::filterBitNbTestFinish;     // set NbTestFinish filter
    filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
#ifdef ASMM_EXEC_INFO_on
    ESMC_LogDefault.Write("SMM exec: COMM_CANCEL",
      ESMC_LOGMSG_DEBUG);
#endif
  }

  // set filters according to zeroflag
  if (zeroflag!=ESMC_REGION_TOTAL)
    filterBitField |= XXE::filterBitRegionTotalZero;  // filter reg. total zero
  if (zeroflag!=ESMC_REGION_SELECT)
    filterBitField |= XXE::filterBitRegionSelectZero; // filter reg. select zero

#ifdef ASMM_EXEC_TIMING_on
  VMK::wtime(&t4);      //gjt - profile
#endif

  // set vectorLength
  int vectorLength = 0; // initialize

  // The vectorLength argument passed into xxe->exec() provides support
  // for flexible choice of vectorLength during execution time of the XXE
  // stream. Vectorization in the XXE stream is handled on a per operation
  // basis, i.e. it depends on each individual operation whether, and how the
  // vectorLength argument is used.
  // On those PETs that don't call in with srcArray nor dstArray (unusual case,
  // but possible and supported!), the vectorLength will be left at 0. In the
  // other cases (i.e. srcArray and/or dstArray are present) it is assumed that
  // the vectorLength can be determined from which ever Array is present (first
  // see about srcArray, and then dstArray. Last one present will set). This is
  // consistent because vectorization in the XXE stream is only meaningful under
  // the condition that the vectorLength determined from the srcArray equals
  // that determined from the dstArray. There is no check performed here on
  // whether srcArray and dstArray lead to the same vectorLength, because it
  // is not known here whether the XXE stream actually uses vectorization or
  // not. In the case that no vectorization is used there may well be
  // inconsistencies on this level, and the vectorLength passed into xxe->exec()
  // is bogus. However, the vectorLength is irrelevant under this condition, and
  // will be ignored by the XXE execution.


  // src-side super vectorization
  int srcLocalDeCount = 0;
  if (srcArrayFlag)
    srcLocalDeCount = srcArray->delayout->getLocalDeCount();
  int srcSuperVecSizeUnd[3];                    // undistributed: r, s, t
  int *srcSuperVecSizeDis[2];                   // distributed: i, j
  vector<int> srcdist_i(srcLocalDeCount);
  vector<int> srcdist_j(srcLocalDeCount);
  srcSuperVecSizeDis[0] = NULL; // initialze
  srcSuperVecSizeDis[1] = NULL; // initialze
  if (srcLocalDeCount){
    srcSuperVecSizeDis[0] = &srcdist_i[0];
    srcSuperVecSizeDis[1] = &srcdist_j[0];
  }
  superVecParam(srcArray, srcLocalDeCount, xxe->superVectorOkay,
    srcSuperVecSizeUnd, srcSuperVecSizeDis, vectorLength);

  // dst-side super vectorization
  int dstLocalDeCount = 0;
  if (dstArrayFlag)
    dstLocalDeCount = dstArray->delayout->getLocalDeCount();
  int dstSuperVecSizeUnd[3];                    // undistributed: r, s, t
  int *dstSuperVecSizeDis[2];                   // distributed: i, j
  vector<int> dstdist_i(dstLocalDeCount);
  vector<int> dstdist_j(dstLocalDeCount);
  dstSuperVecSizeDis[0] = NULL;
  dstSuperVecSizeDis[1] = NULL;
  if (dstLocalDeCount){
    dstSuperVecSizeDis[0] = &dstdist_i[0];
    dstSuperVecSizeDis[1] = &dstdist_j[0];
  }
  superVecParam(dstArray, dstLocalDeCount, xxe->superVectorOkay,
    dstSuperVecSizeUnd, dstSuperVecSizeDis, vectorLength);

  // load super vectorization parameters into SuperVectP data structure
  XXE::SuperVectP superVectP;
  superVectP.srcSuperVecSize_r = srcSuperVecSizeUnd[0];
  superVectP.srcSuperVecSize_s = srcSuperVecSizeUnd[1];
  superVectP.srcSuperVecSize_t = srcSuperVecSizeUnd[2];
  superVectP.srcSuperVecSize_i = srcSuperVecSizeDis[0];
  superVectP.srcSuperVecSize_j = srcSuperVecSizeDis[1];
  superVectP.dstSuperVecSize_r = dstSuperVecSizeUnd[0];
  superVectP.dstSuperVecSize_s = dstSuperVecSizeUnd[1];
  superVectP.dstSuperVecSize_t = dstSuperVecSizeUnd[2];
  superVectP.dstSuperVecSize_i = dstSuperVecSizeDis[0];
  superVectP.dstSuperVecSize_j = dstSuperVecSizeDis[1];

#ifdef ASMM_EXEC_TIMING_on
  VMK::wtime(&t5);      //gjt - profile
#endif

  // execute XXE stream
  localrc = xxe->exec(rraCount, rraList, &vectorLength, filterBitField,
    finishedflag, cancelledflag,
    NULL,     // dTime                  -> disabled
    -1, -1,   // indexStart, indexStop  -> full stream
    // super vector support:
    &srcLocalDeCount, &superVectP);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

#ifdef ASMMXXEPRINT
  // print XXE stream
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  int localPet = vm->getLocalPet();
  int petCount = vm->getPetCount();
  char file[160];
  sprintf(file, "asmmXXEprint.%05d", localPet);
  FILE *fp = fopen(file, "a");
  fprintf(fp, "\n=================================================="
    "==============================\n");
  for (int pet=0; pet<petCount; pet++){
    if (pet==localPet){
      localrc = xxe->print(fp, rraCount, rraList);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
    }
  }
  fprintf(fp, "\n=================================================="
    "==============================\n");
  fprintf(fp, "filterBitField = 0x%08x\n", filterBitField);
  fprintf(fp, "\n=================================================="
    "==============================\n");
  for (int pet=0; pet<petCount; pet++){
    if (pet==localPet){
      localrc = xxe->print(fp, rraCount, rraList, filterBitField);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
    }
  }
  fclose(fp);
#endif

#ifdef ASMM_EXEC_INFO_on
  {
    std::stringstream msg;
    msg << "SMM exec: finishedflag=" << *finishedflag;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif

  int finishLoopCount=0;
  while (commflag==ESMF_COMM_BLOCKING && !(*finishedflag)){
    // must be a blocking call with TERMORDER_FREE -> free-order while
#ifdef ASMM_EXEC_INFO_LOOP_on
    ESMC_LogDefault.Write("SMM exec: ...within free-order while",
      ESMC_LOGMSG_DEBUG);
#endif
    filterBitField = 0x0; // init. to execute _all_ operations in XXE stream
    // same as non-blocking test and finish
    filterBitField |= XXE::filterBitRegionTotalZero;  // filter reg. total zero
    filterBitField |= XXE::filterBitRegionSelectZero; // filter reg. select zero
    filterBitField |= XXE::filterBitNbStart;          // set NbStart filter
    filterBitField |= XXE::filterBitNbWaitFinish;     // set NbWaitFinish filter
    filterBitField |= XXE::filterBitCancel;           // set Cancel filter
    filterBitField |= XXE::filterBitNbWaitFinishSingleSum; // SingleSum filter
    localrc = xxe->exec(rraCount, rraList, &vectorLength, filterBitField,
      finishedflag, cancelledflag,
      NULL,     // dTime                  -> disabled
      -1, -1,   // indexStart, indexStop  -> full stream
      // super vector support:
      &srcLocalDeCount, &superVectP);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    ++finishLoopCount;
  }
#ifdef ASMM_EXEC_INFO_on
  {
    std::stringstream msg;
    msg << "SMM exec: finishLoopCount=" << finishLoopCount;
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif

#ifdef ASMM_EXEC_TIMING_on
  VMK::wtime(&t6);      //gjt - profile
#endif

  // garbage collection
  delete [] rraList;

#ifdef ASMM_EXEC_TIMING_on
  VMK::wtime(&t7);      //gjt - profile
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  int localPet = vm->getLocalPet();
  char file[160];
  sprintf(file, "ASMM_EXEC_TIMING_on.%05d", localPet);
  FILE *fp = fopen(file, "a");
  fprintf(fp, "\n=================================================="
    "==============================\n");
  fprintf(fp, "=================================================="
    "==============================\n\n");
  fprintf(fp, "gjt - exec() profile for PET %d:\n"
    "\tdt1 = %g\n"
    "\tdt2 = %g\n"
    "\tdt3 = %g\n"
    "\tdt4 = %g\n"
    "\tdt5 = %g\n"
    "\tdt6 = %g\n"
    "\tdt7 = %g\n",
    localPet, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0, t7-t0);
  fclose(fp);
#endif

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
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
  RouteHandle *routehandle        // inout -
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
    XXE *xxe = (XXE *)routehandle->getStorage();

    if (xxe == NULL){
      // NOP
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }

#define XXEPROFILEPRINT___disable

#ifdef XXEPROFILEPRINT
#ifdef ASMM_EXEC_PROFILE_on
    // print XXE stream profile
    VM *vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    int localPet = vm->getLocalPet();
    int petCount = vm->getPetCount();
    char file[160];
    sprintf(file, "ASMM_EXEC_PROFILE_on.%05d", localPet);
    FILE *fp = fopen(file, "a");
    fprintf(fp, "\n=================================================="
      "==============================\n");
    fprintf(fp, "=================================================="
      "==============================\n\n");
    for (int pet=0; pet<petCount; pet++){
      if (pet==localPet){
        localrc = xxe->printProfile(fp);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc))
        return rc;
      }
      vm->barrier();
    }
    fclose(fp);
#endif
#endif

#ifdef DEBUGLOG
    {
      std::stringstream debugmsg;
      debugmsg << ESMC_METHOD": delete xxe: " << xxe;
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

    // delete xxe
    delete xxe;

    // mark storage pointer in RouteHandle as invalid/NOP
    localrc = routehandle->setStorage(NULL);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

  }catch(int catchrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(catchrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::Array::superVecParam()"
//BOPI
// !IROUTINE:  ESMCI::Array::superVecParam
//
// !INTERFACE:
void Array::superVecParam(
//
// !RETURN VALUE:
//
// !ARGUMENTS:
//
      Array *array,             // in
      int localDeCount,         // in
      bool superVectorOkay,     // in
      int superVecSizeUnd[3],   // out
      int *superVecSizeDis[2],  // out
      int &vectorLength         // out
  ){
//
// !DESCRIPTION:
//    Determine super-vectorization parameter.
//
//EOPI
//-----------------------------------------------------------------------------
#undef DEBUGLOG
  bool arrayFlag = false;
  if (array != ESMC_NULL_POINTER) arrayFlag = true;

#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " superVectorOkay=" <<
        superVectorOkay;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif

  superVecSizeUnd[0]=-1;  // initialize with disabled super vector support
  superVecSizeUnd[1]=1;
  superVecSizeUnd[2]=1;

  for (int j=0; j<localDeCount; j++){
    superVecSizeDis[0][j]=1;
    superVecSizeDis[1][j]=1;
  }

  if (arrayFlag && array->sizeSuperUndist){
    // use array to determine vectorLength
    vectorLength = 1; // init
    int i;
    for (i=0; i<array->rank-array->tensorCount; i++)
      vectorLength *= array->sizeSuperUndist[i];
    if (superVectorOkay)
      vectorLength *= array->sizeSuperUndist[i];
    else
      vectorLength = array->sizeSuperUndist[0];
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " vectorLength=" << vectorLength;
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
  }

  if (arrayFlag && array->sizeSuperUndist &&
    (array->rank-array->tensorCount)<=2){
    // set superVecSize variables
    int i;
    for (i=0; i<array->rank-array->tensorCount; i++){
      superVecSizeUnd[i] = array->sizeSuperUndist[i];
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " superVecSizeUnd[i="<<i<<"]=" <<
        superVecSizeUnd[i];
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
      for (int j=0; j<localDeCount; j++)
        superVecSizeDis[i][j] =
          array->sizeDist[j*(array->rank - array->tensorCount)+i];
    }
    superVecSizeUnd[i] = array->sizeSuperUndist[i];
#ifdef DEBUGLOG
    {
      std::stringstream msg;
      msg << ESMC_METHOD": " << __LINE__ << " superVecSizeUnd[i="<<i<<"]=" <<
        superVecSizeUnd[i];
      ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
    }
#endif
  }
  if (superVecSizeUnd[1]==1 && superVecSizeUnd[2]==1)
    superVecSizeUnd[0]=-1;  // turn off super vectorization, simple vector ok
#ifdef DEBUGLOG
  {
    std::stringstream msg;
    msg << ESMC_METHOD": " << __LINE__ << " superVecSizeUnd[0]=" <<
      superVecSizeUnd[0];
    ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_DEBUG);
  }
#endif
#undef DEBUGLOG
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayElement::ArrayElement()"
//BOPI
// !IROUTINE:  ESMCI::ArrayElement::ArrayElement
//
// !INTERFACE:
ArrayElement::ArrayElement(
//
// !RETURN VALUE:
//    ArrayElement*
//
// !ARGUMENTS:
//
  Array const *arrayArg,      // in - the Array in which ArrayElement iterates
  int localDeArg,             // in - localDe index, starting with 0
  bool seqIndexEnabled,       // in - enable seqIndex lookup during iteration
  bool seqIndexRecursive,     // in - recursive or not seqIndex lookup
  bool seqIndexCanonical      // in - canonical or not seqIndex lookup
  ){
//
// !DESCRIPTION:
//    Constructor of ArrayElement iterator through Array elements in exclusive
//    region.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check input arguments
  if (arrayArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "arrayArg must not be NULL", ESMC_CONTEXT, &rc);
    throw rc;  // bail out with exception
  }
  if (localDeArg < 0 ||
    localDeArg >= arrayArg->getDELayout()->getLocalDeCount()){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
      "localDeArg out of range", ESMC_CONTEXT, &rc);
    throw rc;  // bail out with exception
  }

  // set members
  array = arrayArg;
  localDe = localDeArg;
  int rank = array->getRank();
  indexTupleStart.resize(rank);
  indexTupleEnd.resize(rank);
  indexTuple.resize(rank);
  skipDim.resize(rank);
  indexTupleBlockStart.resize(rank);
  indexTupleBlockEnd.resize(rank);
  indexTupleWatchStart.resize(rank);
  indexTupleWatchEnd.resize(rank);

  // initialize tuple variables for iteration through Array elements within
  // exclusive region, with origin of exclusive region at tuple (0,0,..)
  int redDimCount = array->getRank() - array->getTensorCount();
  int iOff = localDe * redDimCount;
  int iPacked = 0;    // reset
  int iTensor = 0;    // reset
  for (int i=0; i<rank; i++){
    indexTupleStart[i] = indexTuple[i] = 0;   // reset
    skipDim[i] = false;                       // reset
    indexTupleBlockStart[i] = indexTupleBlockEnd[i] = 0;  // reset
    indexTupleWatchStart[i] = indexTupleWatchEnd[i] = 0;  // reset
    if (array->getArrayToDistGridMap()[i]){
      // decomposed dimension
      indexTupleEnd[i] = array->getExclusiveUBound()[iOff+iPacked]
        - array->getExclusiveLBound()[iOff+iPacked] + 1;
      ++iPacked;
    }else{
      // tensor dimension
      indexTupleEnd[i] = array->getUndistUBound()[iTensor]
        - array->getUndistLBound()[iTensor] + 1;
      ++iTensor;
    }
  }

  // set some more member
  seqIndex = NULL;
  blockActiveFlag = false;  // no block active for exclusive only
  indexTK = array->getDistGrid()->getIndexTK();
  firstDimDecompFlag = false;  // init
  if (array->getArrayToDistGridMap()[0]) firstDimDecompFlag = true; // set
  firstDimFirstDecomp = false;  // default
  if (array->getArrayToDistGridMap()[0]==1) firstDimFirstDecomp = true; // set
  arbSeqIndexFlag = false;  // init
  if (array->getDistGrid()->getArbSeqIndexList(localDe,1))
    arbSeqIndexFlag = true; // set
  seqIndexRecursiveFlag = seqIndexRecursive;
  seqIndexCanonicalFlag = seqIndexCanonical;
  // flag condition that will prevent optimization of seqIndex lookup
  cannotOptimizeLookup = !firstDimFirstDecomp | arbSeqIndexFlag |
    seqIndexRecursiveFlag;
  lastSeqIndexInvalid = true;

  // early return if not within range
  if (!isWithin()) return;

  // set the linIndex member
  linIndex = array->getLinearIndexExclusive(localDe, &indexTuple[0]);

  // deal with seqIndex support
  if (seqIndexEnabled){
    // prepare seqIndex member for iteration
    if (indexTK==ESMC_TYPEKIND_I4){
      seqIndex = (void *) new SeqIndex<ESMC_I4>;
      localrc = array->getSequenceIndexExclusive(localDe, &indexTuple[0],
        (SeqIndex<ESMC_I4>*)seqIndex, seqIndexRecursiveFlag,
        seqIndexCanonicalFlag);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }else if (indexTK==ESMC_TYPEKIND_I8){
      seqIndex = (void *) new SeqIndex<ESMC_I8>;
      localrc = array->getSequenceIndexExclusive(localDe, &indexTuple[0],
        (SeqIndex<ESMC_I8>*)seqIndex, seqIndexRecursiveFlag,
        seqIndexCanonicalFlag);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
    }
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayElement::ArrayElement()"
//BOPI
// !IROUTINE:  ESMCI::ArrayElement::ArrayElement
//
// !INTERFACE:
ArrayElement::ArrayElement(
//
// !RETURN VALUE:
//    ArrayElement*
//
// !ARGUMENTS:
//
  Array const *arrayArg,      // in - the Array in which ArrayElement iterates
  int localDeArg,             // in - localDe index, starting with 0
  bool blockExclusiveFlag,    // in - block the exclusive region if set to true
  bool seqIndexEnabled,       // in - enable seqIndex lookup during iteration
  bool seqIndexRecursive,     // in - recursive or not seqIndex lookup
  bool seqIndexCanonical      // in - canonical or not seqIndex lookup
  ){
//
// !DESCRIPTION:
//    Constructor of ArrayElement iterator through Array elements in total
//    region, with the option to block the elements of the exclusive region.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // check input arguments
  if (arrayArg == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "arrayArg must not be NULL", ESMC_CONTEXT, &rc);
    throw rc;  // bail out with exception
  }
  if (localDeArg < 0 ||
    localDeArg >= arrayArg->getDELayout()->getLocalDeCount()){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
      "localDeArg out of range", ESMC_CONTEXT, &rc);
    throw rc;  // bail out with exception
  }

  // set members
  array = arrayArg;
  localDe = localDeArg;
  int rank = array->getRank();
  indexTupleStart.resize(rank);
  indexTupleEnd.resize(rank);
  indexTuple.resize(rank);
  skipDim.resize(rank);
  indexTupleBlockStart.resize(rank);
  vector<int> indexTupleBlockEndArg;
  indexTupleBlockEndArg.resize(rank);
  indexTupleWatchStart.resize(rank);
  indexTupleWatchEnd.resize(rank);

  // initialize tuple variables for iteration through Array elements within
  // total region, with origin of exclusive region at tuple (0,0,..)
  int redDimCount = array->getRank() - array->getTensorCount();
  int iOff = localDe * redDimCount;
  int iPacked = 0;    // reset
  int iTensor = 0;    // reset
  for (int i=0; i<rank; i++){
    skipDim[i] = false;                       // reset
    indexTupleBlockStart[i] = indexTupleBlockEndArg[i] = 0;  // reset
    indexTupleWatchStart[i] = indexTupleWatchEnd[i] = 0;  // reset
    if (array->getArrayToDistGridMap()[i]){
      // decomposed dimension
      indexTupleStart[i] = indexTuple[i] =
        array->getTotalLBound()[iOff+iPacked]
        - array->getExclusiveLBound()[iOff+iPacked];
      indexTupleEnd[i] = array->getTotalUBound()[iOff+iPacked]
        - array->getExclusiveLBound()[iOff+iPacked] + 1;
      indexTupleWatchEnd[i] = array->getExclusiveUBound()[iOff+iPacked]
        - array->getExclusiveLBound()[iOff+iPacked] + 1;
      ++iPacked;
    }else{
      // tensor dimension
      indexTupleStart[i] = indexTuple[i] = 0;
      indexTupleEnd[i] = array->getUndistUBound()[iTensor]
        - array->getUndistLBound()[iTensor] + 1;
      indexTupleWatchEnd[i] = array->getUndistUBound()[iTensor]
        - array->getUndistLBound()[iTensor] + 1;
      ++iTensor;
    }
    if (blockExclusiveFlag)
      indexTupleBlockEndArg[i] =  indexTupleWatchEnd[i];
    // for consistent internal setup, must call method to set BlockEnd
    setBlockEnd(indexTupleBlockEndArg);
  }
  first();  // point indexTuple to the first element that is not blocked

  // set some more members
  seqIndex = NULL;
  blockActiveFlag = blockExclusiveFlag;
  indexTK = array->getDistGrid()->getIndexTK();
  firstDimDecompFlag = false;  // default
  if (array->getArrayToDistGridMap()[0]) firstDimDecompFlag = true; // set
  firstDimFirstDecomp = false;  // default
  if (array->getArrayToDistGridMap()[0]==1) firstDimFirstDecomp = true; // set
  arbSeqIndexFlag = false;  // init
  if (array->getDistGrid()->getArbSeqIndexList(localDe,1))
    arbSeqIndexFlag = true; // set
  seqIndexRecursiveFlag = seqIndexRecursive;
  seqIndexCanonicalFlag = seqIndexCanonical;
  // flag condition that will prevent optimization of seqIndex lookup
  cannotOptimizeLookup = !firstDimFirstDecomp | arbSeqIndexFlag |
    seqIndexRecursiveFlag;
  lastSeqIndexInvalid = true;

  // early return if not within range
  if (!isWithin()) return;

  // set the linIndex member
  linIndex = array->getLinearIndexExclusive(localDe, &indexTuple[0]);

  // deal with seqIndex support
  if (seqIndexEnabled){
    // prepare seqIndex member for iteration
    if (indexTK==ESMC_TYPEKIND_I4){
      seqIndex = (void *) new SeqIndex<ESMC_I4>;
      if (!blockExclusiveFlag || hasValidSeqIndex()){
        localrc = array->getSequenceIndexExclusive(localDe, &indexTuple[0],
          (SeqIndex<ESMC_I4>*)seqIndex, seqIndexRecursiveFlag,
          seqIndexCanonicalFlag);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
      }
    }else if (indexTK==ESMC_TYPEKIND_I8){
      seqIndex = (void *) new SeqIndex<ESMC_I8>;
      if (!blockExclusiveFlag || hasValidSeqIndex()){
        localrc = array->getSequenceIndexExclusive(localDe, &indexTuple[0],
          (SeqIndex<ESMC_I8>*)seqIndex, seqIndexRecursiveFlag,
          seqIndexCanonicalFlag);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
      }
    }
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayElement::hasValidSeqIndex()"
//BOPI
// !IROUTINE:  ESMCI::ArrayElement::hasValidSeqIndex
//
// !INTERFACE:
bool ArrayElement::hasValidSeqIndex(
//
// !RETURN VALUE:
//    true or false
//
// !ARGUMENTS:
//
  )const{
//
// !DESCRIPTION:
//    Indicate whether the ArrayElement is valid. Invalid elements are
//    those that are outside the exclusive region for discontiguous dimensions,
//    or arbitrarily decomposed dimensions.
//
//EOPI
//-----------------------------------------------------------------------------
  int rank = array->getRank();
  int de = array->getLocalDeToDeMap()[localDe];
  int iOff = de * array->getDistGrid()->getDimCount();
  int iPacked = 0;    // reset
  for (int i=0; i<rank; i++){
    if (array->getArrayToDistGridMap()[i]){
      // decomposed dimension
      if (array->getDistGrid()->getContigFlagPDimPDe()[iOff+iPacked] == 0){
        // discontigous dimension -> check if within MultiDimIndexLoop block
        if (isWithinBlock(i) == false) return false;
      }
      int collocation = array->getDistGrid()->getCollocationPDim()[iPacked];
      if (array->getDistGrid()->getArbSeqIndexList(localDe, collocation)){
        // arbitrarily decomposed dimension
        // -> check if within MultiDimIndexLoop block
        if (isWithinBlock(i) == false) return false;
      }
      ++iPacked;
    }
  }
  return true;  // all dimensions are in valid range
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayElement::getTensorSequenceIndex()"
//BOPI
// !IROUTINE:  ESMCI::ArrayElement::getTensorSequenceIndex
//
// !INTERFACE:
int ArrayElement::getTensorSequenceIndex(
//
// !RETURN VALUE:
//    linear index
//
// !ARGUMENTS:
//
  )const{
//
// !DESCRIPTION:
//    Obtain linear index for ArrayElement
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code

  int tensorSeqIndex = array->getTensorSequenceIndex(&indexTuple[0], &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    NULL)) throw localrc;  // bail out with exception
  return tensorSeqIndex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayElement::getArbSequenceIndexOffset()"
//BOPI
// !IROUTINE:  ESMCI::ArrayElement::getArbSequenceIndexOffset
//
// !INTERFACE:
int ArrayElement::getArbSequenceIndexOffset(
//
// !RETURN VALUE:
//    linear index
//
// !ARGUMENTS:
//
  )const{
//
// !DESCRIPTION:
//    Obtain linear index for ArrayElement
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code

  int arbSeqIndexOffset = array->getArbSequenceIndexOffset(&indexTuple[0],
    &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    NULL)) throw localrc;  // bail out with exception
  return arbSeqIndexOffset;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayElement::log()"
//BOPI
// !IROUTINE:  ESMCI::ArrayElement::log
//
// !INTERFACE:
void ArrayElement::log(
//
// !ARGUMENTS:
//
  ESMC_LogMsgType_Flag msgType)const{
//
// !DESCRIPTION:
//    Print internal information..
//
//EOPI
//-----------------------------------------------------------------------------
  std::stringstream msg;
  msg << ESMC_METHOD << ": array=" << array << " localDe: " << localDe;
  ESMC_LogDefault.Write(msg.str(), msgType);
  MultiDimIndexLoop::log(msgType);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::SparseMatrix::SparseMatrix()"
//BOPI
// !IROUTINE:  ESMCI::SparseMatrix::SparseMatrix
//
// !INTERFACE:

template<typename SIT, typename DIT> SparseMatrix<SIT,DIT>::SparseMatrix(
//
// !RETURN VALUE:
//    SparseMatrix*
//
// !ARGUMENTS:
//
  ESMC_TypeKind_Flag const typekind_,
  void const *factorList_,
  int const factorListCount_,
  int const srcN_,
  int const dstN_,
  void const *factorIndexList_
  ){
//
// !DESCRIPTION:
//    Constructor
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  typekind = typekind_;
  factorList = factorList_;
  factorListCount = factorListCount_;
  srcN = srcN_;
  dstN = dstN_;
  factorIndexList = factorIndexList_;
  // check consistency
  if (factorListCount > 0){
    // must contain valid factorList and factorIndexList members
    if (factorList == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to factorList array", ESMC_CONTEXT, &rc);
      throw rc;  // bail out with exception
    }
    if (factorIndexList == NULL){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "Not a valid pointer to factorIndexList array", ESMC_CONTEXT, &rc);
      throw rc;  // bail out with exception
    }
    // must contain a valid typekind
    if (typekind == ESMF_NOKIND){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "must specify valid typekind", ESMC_CONTEXT, &rc);
      throw rc;  // bail out with exception
    }
    if (typekind != ESMC_TYPEKIND_I4
      && typekind != ESMC_TYPEKIND_I8
      && typekind != ESMC_TYPEKIND_R4
      && typekind != ESMC_TYPEKIND_R8){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
        "not a supported choice for typekind", ESMC_CONTEXT, &rc);
      throw rc;  // bail out with exception
    }
  }
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
