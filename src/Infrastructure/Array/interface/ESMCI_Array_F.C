// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Array_F.C"
//==============================================================================

#define ASMM_STORE_MEMLOG_off

//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <string>
#include <utility>

#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_DistGrid.h"
#include "ESMCI_ArraySpec.h"
#include "ESMCI_RHandle.h"
#include "ESMCI_Array.h"
#include "ESMCI_LogErr.h"

using namespace std;

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Array} class functions.
//
//EOP
//-------------------------------------------------------------------------

// the interface subroutine names MUST be in lower case by ESMF convention
extern "C" {

  // - ESMF-public methods:
        
  void FTN_X(c_esmc_arraycreatelocalarray)(ESMCI::Array **ptr, 
    ESMCI::LocalArray **larrayList, int *larrayCount,
    ESMCI::DistGrid **distgrid,
    ESMCI::CopyFlag *copyflag,
    ESMCI::InterArray<int> *distgridToArrayMap,
    ESMCI::InterArray<int> *computationalEdgeLWidthArg,
    ESMCI::InterArray<int> *computationalEdgeUWidthArg,
    ESMCI::InterArray<int> *computationalLWidthArg,
    ESMCI::InterArray<int> *computationalUWidthArg, 
    ESMCI::InterArray<int> *totalLWidthArg,
    ESMCI::InterArray<int> *totalUWidthArg,
    ESMC_IndexFlag *indexflag, ESMCI::InterArray<int> *undistLBoundArg,
    ESMCI::InterArray<int> *undistUBoundArg,
    char *name, int *len_name, int *rc,
    ESMCI_FortranStrLenArg name_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreatelocalarray()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *ptr = ESMCI::Array::create(larrayList, *larrayCount, *distgrid,
      *copyflag, distgridToArrayMap,
      computationalEdgeLWidthArg, computationalEdgeUWidthArg,
      computationalLWidthArg, computationalUWidthArg, totalLWidthArg,
      totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag),
      undistLBoundArg, undistUBoundArg, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // set the name in the Array object
    string cname = string(name, ESMC_F90lentrim (name, *len_name));
    if (cname.length() > 0) {
      localrc = (*ptr)->setName(cname);
      ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return;
    }
  }
  
  void FTN_X(c_esmc_arraycreateallocate)(ESMCI::Array **ptr, 
    ESMCI::ArraySpec *arrayspec, ESMCI::DistGrid **distgrid,
    ESMCI::InterArray<int> *distgridToArrayMap,
    ESMCI::InterArray<int> *computationalEdgeLWidthArg,
    ESMCI::InterArray<int> *computationalEdgeUWidthArg,
    ESMCI::InterArray<int> *computationalLWidthArg,
    ESMCI::InterArray<int> *computationalUWidthArg, 
    ESMCI::InterArray<int> *totalLWidthArg,
    ESMCI::InterArray<int> *totalUWidthArg,
    ESMC_IndexFlag *indexflag, ESMC_Pin_Flag *pinflag,
    ESMCI::InterArray<int> *undistLBoundArg,
    ESMCI::InterArray<int> *undistUBoundArg,
    char *name, int *len_name, ESMCI::VM **vm, int *rc,
    ESMCI_FortranStrLenArg name_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreateallocate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    bool actualFlag = true;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      opt_vm = NULL;
    else{
      opt_vm = *vm;
      if (opt_vm == NULL)
        actualFlag = false; // not an actual member because VM present but NULL
    }
#if 0
    printf("c_esmc_arraycreateallocate(): opt_vm=%p, actualFlag=%d\n", 
      opt_vm, actualFlag);
#endif
    if (actualFlag){
      // on PETs with actual members call into C++
      *ptr = ESMCI::Array::create(arrayspec, *distgrid, distgridToArrayMap,
        computationalEdgeLWidthArg, computationalEdgeUWidthArg,
        computationalLWidthArg, computationalUWidthArg, totalLWidthArg,
        totalUWidthArg, ESMC_NOT_PRESENT_FILTER(indexflag), 
        ESMC_NOT_PRESENT_FILTER(pinflag), NULL,
        undistLBoundArg, undistUBoundArg, &localrc, opt_vm);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
      // set the name in the Array object
      string cname = string(name, ESMC_F90lentrim (name, *len_name));
      if (cname.length() > 0) {
        localrc = (*ptr)->setName(cname);
        ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
        return;
      }
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_arraycopy)(ESMCI::Array **ptr, 
    ESMCI::Array **arrayIn, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycopy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    localrc = (*ptr)->copy(*arrayIn);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }
  
  void FTN_X(c_esmc_arraycreatecopy)(ESMCI::Array **ptr, 
    ESMCI::Array **arrayOut, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraycreatecopy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // call into C++
    *arrayOut = ESMCI::Array::create(*ptr, 0, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
  }
  
  void FTN_X(c_esmc_arraydestroy)(ESMCI::Array **ptr, ESMC_Logical *noGarbage, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool noGarbageOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(noGarbage) != ESMC_NULL_POINTER)
      if (*noGarbage == ESMF_TRUE) noGarbageOpt = true;
    // call into C++
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::destroy(ptr, noGarbageOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayget)(ESMCI::Array **ptr, ESMC_TypeKind_Flag *typekind,
    int *rank, int *ssiLocalDeCount, ESMCI::LocalArray **opt_localArrayList,
    int *len_localArrayList, ESMCI::InterArray<int> *localDeToDeMap,
    ESMCI::DistGrid **distgrid, ESMCI::DELayout **delayout,
    ESMC_IndexFlag *indexflag, 
    ESMCI::InterArray<int> *distgridToArrayMap,
    ESMCI::InterArray<int> *distgridToPackedArrayMap,
    ESMCI::InterArray<int> *arrayToDistGridMap,
    ESMCI::InterArray<int> *undistLBound,
    ESMCI::InterArray<int> *undistUBound,
    ESMCI::InterArray<int> *exclusiveLBound,
    ESMCI::InterArray<int> *exclusiveUBound,
    ESMCI::InterArray<int> *computationalLBound,
    ESMCI::InterArray<int> *computationalUBound,
    ESMCI::InterArray<int> *totalLBound,
    ESMCI::InterArray<int> *totalUBound,
    ESMCI::InterArray<int> *computationalLWidth,
    ESMCI::InterArray<int> *computationalUWidth,
    ESMCI::InterArray<int> *totalLWidth,
    ESMCI::InterArray<int> *totalUWidth,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // fill simple return values    
    if (ESMC_NOT_PRESENT_FILTER(distgrid) != ESMC_NULL_POINTER)
      *distgrid = (*ptr)->getDistGrid();
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDELayout();
    if (ESMC_NOT_PRESENT_FILTER(typekind) != ESMC_NULL_POINTER)
      *typekind = (*ptr)->getTypekind();
    if (ESMC_NOT_PRESENT_FILTER(rank) != ESMC_NULL_POINTER)
      *rank = (*ptr)->getRank();
    if (ESMC_NOT_PRESENT_FILTER(ssiLocalDeCount) != ESMC_NULL_POINTER)
      *ssiLocalDeCount = (*ptr)->getSsiLocalDeCount();
    if (ESMC_NOT_PRESENT_FILTER(indexflag) != ESMC_NULL_POINTER)
      *indexflag = (*ptr)->getIndexflag();
    // helper variables
    int dimCount = (*ptr)->getDistGrid()->getDimCount();
    int localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
    // fill localArrayList
    if (*len_localArrayList != 0){
      // opt_localArrayList was provided
      int ldeCount = -1; // initialize with guard value
      if (*len_localArrayList >= localDeCount)
        ldeCount=localDeCount;
      if (*len_localArrayList >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if (*len_localArrayList >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "opt_localArrayList must provide 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount' elements", ESMC_CONTEXT, rc);
        return;
      }
      // opt_localArrayList has supported number of elements
      for (int i=0; i<ldeCount; i++)
        opt_localArrayList[i] = ((*ptr)->getLocalarrayList())[i];
    }
    // fill localDeToDeMap
    if (present(localDeToDeMap)){
      // localDeToDeMap was provided
      if ((localDeToDeMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "localDeToDeMap array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((localDeToDeMap)->extent[0] >= localDeCount)
        ldeCount=localDeCount;
      if ((localDeToDeMap)->extent[0] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((localDeToDeMap)->extent[0] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of localDeToDeMap array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in distgridToArrayMap
      memcpy((localDeToDeMap)->array, (*ptr)->getLocalDeToDeMap(),
        sizeof(int) * ldeCount);
    }
    // fill distgridToArrayMap
    if (present(distgridToArrayMap)){
      // distgridToArrayMap was provided -> do some error checking
      if ((distgridToArrayMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "distgridToArrayMap array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((distgridToArrayMap)->extent[0] < dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of distgridToArrayMap array must be of size "
          "'dimCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in distgridToArrayMap
      memcpy((distgridToArrayMap)->array, (*ptr)->getDistGridToArrayMap(),
        sizeof(int) * dimCount);
    }
    // fill distgridToPackedArrayMap
    if (present(distgridToPackedArrayMap)){
      // distgridToPackedArrayMap was provided -> do some error checking
      if ((distgridToPackedArrayMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "distgridToPackedArrayMap array must be of rank 1", ESMC_CONTEXT,
          rc);
        return;
      }
      if ((distgridToPackedArrayMap)->extent[0] < dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of distgridToPackedArrayMap array must be of size "
          "'dimCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in distgridToPackedArrayMap
      memcpy((distgridToPackedArrayMap)->array,
        (*ptr)->getDistGridToPackedArrayMap(), sizeof(int) * dimCount);
    }
    // fill arrayToDistGridMap
    if (present(arrayToDistGridMap)){
      // arrayToDistGridMap was provided -> do some error checking
      if ((arrayToDistGridMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "arrayToDistGridMap array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((arrayToDistGridMap)->extent[0] < (*ptr)->getRank()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of arrayToDistGridMap array must be of size 'rank'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in arrayToDistGridMap
      memcpy((arrayToDistGridMap)->array, (*ptr)->getArrayToDistGridMap(),
        sizeof(int) * (*ptr)->getRank());
    }

    // fill undistLBound
    if (present(undistLBound)){
      //  undistLBound was provided -> do some error checking
      if ((undistLBound)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "undistLBound array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((undistLBound)->extent[0] < (*ptr)->getTensorCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "undistLBound array must at least be of size tensorCount", 
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in undistLBound
      if ((*ptr)->getTensorCount()){
        // 0 - sized undistLBound are legit, but memcpy may not behave
        memcpy((undistLBound)->array, (*ptr)->getUndistLBound(),
          sizeof(int) * (*ptr)->getTensorCount());
      }
    }

    // fill undistUBound
    if (present(undistUBound)){
      //  undistUBound was provided -> do some error checking
      if ((undistUBound)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "undistUBound array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((undistUBound)->extent[0] < (*ptr)->getTensorCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "undistUBound array must at least be of size tensorCount", 
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in undistUBound
      if ((*ptr)->getTensorCount()){
        // 0 - sized undistUBound are legit, but memcpy may not behave
        memcpy((undistUBound)->array, (*ptr)->getUndistUBound(),
          sizeof(int) * (*ptr)->getTensorCount());
      }
    }

    int redDimCount = (*ptr)->getRank() - (*ptr)->getTensorCount();
    // fill exclusiveLBound
    if (present(exclusiveLBound)){
      // exclusiveLBound was provided -> do some error checking
      if ((exclusiveLBound)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "exclusiveLBound array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((exclusiveLBound)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of exclusiveLBound must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((exclusiveLBound)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((exclusiveLBound)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((exclusiveLBound)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of exclusiveLBound array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveLBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveLBound array.
      for (int i=0; i<ldeCount; i++)
        memcpy(&((exclusiveLBound)->array[i*(exclusiveLBound)->extent[0]]),
          &(((*ptr)->getExclusiveLBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill exclusiveUBound
    if (present(exclusiveUBound)){
      // exclusiveUBound was provided -> do some error checking
      if ((exclusiveUBound)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "exclusiveUBound array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((exclusiveUBound)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of exclusiveUBound must be of size 'dimCount'", 
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((exclusiveUBound)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((exclusiveUBound)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((exclusiveUBound)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of exclusiveUBound array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in exclusiveUBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the exclusiveUBound array.
      for (int i=0; i<ldeCount; i++)
        memcpy(&((exclusiveUBound)->array[i*(exclusiveUBound)->extent[0]]),
          &(((*ptr)->getExclusiveUBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill computationalLBound
    if (present(computationalLBound)){
      // computationalLBound was provided -> do some error checking
      if ((computationalLBound)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "computationalLBound array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((computationalLBound)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of computationalLBound must be of size 'dimCount'", 
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((computationalLBound)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((computationalLBound)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((computationalLBound)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of computationalLBound array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalLBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalLBound array.
      for (int i=0; i<ldeCount; i++)
        memcpy(&((computationalLBound)->array[i*(computationalLBound)->extent[0]]),
          &(((*ptr)->getComputationalLBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill computationalUBound
    if (present(computationalUBound)){
      // computationalUBound was provided -> do some error checking
      if ((computationalUBound)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "computationalUBound array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((computationalUBound)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of computationalUBound must be of size 'dimCount'", 
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((computationalUBound)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((computationalUBound)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((computationalUBound)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of computationalUBound array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in computationalUBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the computationalUBound array.
      for (int i=0; i<ldeCount; i++)
        memcpy(&((computationalUBound)->array[i*(computationalUBound)->extent[0]]),
          &(((*ptr)->getComputationalUBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill totalLBound
    if (present(totalLBound)){
      // totalLBound was provided -> do some error checking
      if ((totalLBound)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "totalLBound array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((totalLBound)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of totalLBound must be of size 'dimCount'", 
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((totalLBound)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((totalLBound)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((totalLBound)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of totalLBound array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalLBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalLBound array.
      for (int i=0; i<ldeCount; i++)
        memcpy(&((totalLBound)->array[i*(totalLBound)->extent[0]]),
          &(((*ptr)->getTotalLBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill totalUBound
    if (present(totalUBound)){
      // totalUBound was provided -> do some error checking
      if ((totalUBound)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "totalUBound array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((totalUBound)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of totalUBound must be of size 'dimCount'", 
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((totalUBound)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((totalUBound)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((totalUBound)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of totalUBound array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in totalUBound
      // arrays which are larger than redDimCount x localDeCount. Consequently
      // it is necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the totalUBound array.
      for (int i=0; i<ldeCount; i++)
        memcpy(&((totalUBound)->array[i*(totalUBound)->extent[0]]),
          &(((*ptr)->getTotalUBound())[i*redDimCount]),
          sizeof(int)*redDimCount);
    }
    // fill computationalLWidth
    if (present(computationalLWidth)){
      // computationalLWidth was provided -> do some error checking
      if ((computationalLWidth)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "computationalLWidth array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((computationalLWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of computationalLWidth must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((computationalLWidth)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((computationalLWidth)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((computationalLWidth)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of computationalLWidth array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      for (int i=0; i<ldeCount; i++)
        for (int j=0; j<redDimCount; j++)
          (computationalLWidth)->array[i*(computationalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*redDimCount+j] -
            ((*ptr)->getComputationalLBound())[i*redDimCount+j];
    }
    // fill computationalUWidth
    if (present(computationalUWidth)){
      // computationalUWidth was provided -> do some error checking
      if ((computationalUWidth)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "computationalUWidth array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((computationalUWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of computationalUWidth must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((computationalUWidth)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((computationalUWidth)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((computationalUWidth)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of computationalUWidth array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      for (int i=0; i<ldeCount; i++)
        for (int j=0; j<redDimCount; j++)
          (computationalUWidth)->array[i*(computationalUWidth)->extent[0]+j] =
            ((*ptr)->getComputationalUBound())[i*redDimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*redDimCount+j];
    }
    // fill totalLWidth
    if (present(totalLWidth)){
      // totalLWidth was provided -> do some error checking
      if ((totalLWidth)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "totalLWidth array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((totalLWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of totalLWidth must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((totalLWidth)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((totalLWidth)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((totalLWidth)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of totalLWidth array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      for (int i=0; i<ldeCount; i++)
        for (int j=0; j<redDimCount; j++)
          (totalLWidth)->array[i*(totalLWidth)->extent[0]+j] =
            ((*ptr)->getExclusiveLBound())[i*redDimCount+j] -
            ((*ptr)->getTotalLBound())[i*redDimCount+j];
    }
    // fill totalUWidth
    if (present(totalUWidth)){
      // totalUWidth was provided -> do some error checking
      if ((totalUWidth)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "totalUWidth array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((totalUWidth)->extent[0] < redDimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of totalUWidth must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      int ldeCount = -1; // initialize with guard value
      if ((totalUWidth)->extent[1] >= localDeCount)
        ldeCount=localDeCount;
      if ((totalUWidth)->extent[1] >= (*ptr)->getVasLocalDeCount())
        ldeCount=(*ptr)->getVasLocalDeCount();
      if ((totalUWidth)->extent[1] >= (*ptr)->getSsiLocalDeCount())
        ldeCount=(*ptr)->getSsiLocalDeCount();
      if (ldeCount==-1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of totalUWidth array must of size 'localDeCount', "
          "'vasLocalDeCount' or 'ssiLocalDeCount'", ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      for (int i=0; i<ldeCount; i++)
        for (int j=0; j<redDimCount; j++)
          (totalUWidth)->array[i*(totalUWidth)->extent[0]+j] =
            ((*ptr)->getTotalUBound())[i*redDimCount+j] -
            ((*ptr)->getExclusiveUBound())[i*redDimCount+j];
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_arraygetlarray)(ESMCI::Array **ptr, int *localDe,
    ESMCI::LocalArray **localArray, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraygetlarray()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // helper variable
    int localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
    // check localDe
    if ((*localDe < 0) || (*localDe >= localDeCount)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "localDe is out of range.", ESMC_CONTEXT, rc);
      return;
    }
    // get the LocalArray for localDe
    *localArray = ((*ptr)->getLocalarrayList())[*localDe];
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_arraywrite)(ESMCI::Array **array,
                                char *file,
                                char *variableName, char *convention, char *purpose,
                                ESMC_Logical *opt_overwriteflag,
                                ESMC_FileStatus_Flag *status,
                                int *timeslice, ESMC_IOFmt_Flag *iofmt,
                                int *rc,
                                ESMCI_FortranStrLenArg file_l,
                                ESMCI_FortranStrLenArg varname_l,
                                ESMCI_FortranStrLenArg convention_l,
                                ESMCI_FortranStrLenArg purpose_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraywrite()"
// std::cout << ESMF_METHOD << ": file_l=" << file_l << ", varname_l=" << varname_l << std::endl;
    bool overwriteflag;
    // Initialize return code; assume routine not implemented
    if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) {
      *rc = ESMC_RC_NOT_IMPL;
    }
    int localrc = ESMC_RC_NOT_IMPL;
    // The Fortran interface always sets the flags and optional variables
    // except for timeslice. For character variables, create c++ string copies.

    string fileName (file, ESMC_F90lentrim (file, file_l));

    string varName;
    if (variableName && (varname_l > 0))
      varName = string (variableName,
        ESMC_F90lentrim (variableName, varname_l));

    string conv;
    if (convention && (convention_l > 0))
      conv = string (convention,
        ESMC_F90lentrim (convention, convention_l));

    string purp;
    if (purpose && (purpose_l > 0))
      purp = string (purpose,
        ESMC_F90lentrim (purpose, purpose_l));

    overwriteflag = (*opt_overwriteflag == ESMF_TRUE);
    // Call into the actual C++ method wrapped inside LogErr handling
    localrc = (*array)->write(fileName, varName,
                              conv, purp,
                              &overwriteflag, status, timeslice, iofmt);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayread)(ESMCI::Array **array,
                               char *file,
                               char *variableName, int *len_variableName,
                               int *timeslice,
                               ESMC_IOFmt_Flag *iofmt, int *rc,
                               ESMCI_FortranStrLenArg file_l,
                               ESMCI_FortranStrLenArg varname_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayread()"
    // Initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;
    if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) {
      *rc = ESMC_RC_NOT_IMPL;
    }

    //  For character variables, create c++ string copies.

    string fileName (file, ESMC_F90lentrim (file, file_l));

    string varName;
    if (*len_variableName > 0)
      varName = string (variableName,
        ESMC_F90lentrim (variableName, *len_variableName));

    // Call into the actual C++ method wrapped inside LogErr handling
    localrc = (*array)->read(fileName, varName, timeslice, iofmt);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                  ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayprint)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->print(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
  }

  void FTN_X(c_esmc_arraysync)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysync()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->sync(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayvalidate)(ESMCI::Array **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->validate(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayhalostore)(ESMCI::Array **array,
    ESMCI::RouteHandle **routehandle,
    ESMC_HaloStartRegionFlag *halostartregionflag,
    ESMCI::InterArray<int> *haloLDepth, ESMCI::InterArray<int> *haloUDepth,
    int *pipelineDepth, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayhalostore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::haloStore(
      *array, routehandle, *halostartregionflag, haloLDepth, haloUDepth,
      pipelineDepth),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayhalo)(ESMCI::Array **array,
    ESMCI::RouteHandle **routehandle, ESMC_CommFlag *commflag,
    ESMC_Logical *finishedflag, ESMC_Logical *cancelledflag,
    ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayhalo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool checkflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(checkflag) != ESMC_NULL_POINTER)
      if (*checkflag == ESMF_TRUE) checkflagOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    bool finished;
    bool cancelled;
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::halo(
      *array, routehandle, *commflag, &finished, &cancelled, checkflagOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // translate back finishedflag
    if (ESMC_NOT_PRESENT_FILTER(finishedflag) != ESMC_NULL_POINTER){
      if (finished)
        *finishedflag = ESMF_TRUE;
      else
        *finishedflag = ESMF_FALSE;
    }
    // translate back cancelledflag
    if (ESMC_NOT_PRESENT_FILTER(cancelledflag) != ESMC_NULL_POINTER){
      if (cancelled)
        *cancelledflag = ESMF_TRUE;
      else
        *cancelledflag = ESMF_FALSE;
    }
  }
  
  void FTN_X(c_esmc_arrayrediststore)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle, 
    ESMCI::InterArray<int> *srcToDstTransposeMap,
    ESMC_TypeKind_Flag *typekind,
    void *factor, ESMC_Logical *ignoreUnmatched, int *pipelineDepth, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayrediststore()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // ignoreUnmatched flag
    bool ignoreUnmatchedOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(ignoreUnmatched) != ESMC_NULL_POINTER)
      if (*ignoreUnmatched == ESMF_TRUE) ignoreUnmatchedOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::redistStore(
      *srcArray, *dstArray, routehandle, srcToDstTransposeMap, *typekind,
      factor, ignoreUnmatchedOpt, pipelineDepth),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayrediststorenf)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle, 
    ESMCI::InterArray<int> *srcToDstTransposeMap,
    ESMC_Logical *ignoreUnmatched,
    int *pipelineDepth, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayrediststorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // ignoreUnmatched flag
    bool ignoreUnmatchedOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(ignoreUnmatched) != ESMC_NULL_POINTER)
      if (*ignoreUnmatched == ESMF_TRUE) ignoreUnmatchedOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::redistStore(
      *srcArray, *dstArray, routehandle, srcToDstTransposeMap, ESMF_NOKIND,
      NULL, ignoreUnmatchedOpt, pipelineDepth),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arrayredist)(ESMCI::Array **srcArray, ESMCI::Array **dstArray,
    ESMCI::RouteHandle **routehandle, ESMC_CommFlag *commflag,
    ESMC_Logical *finishedflag, ESMC_Logical *cancelledflag,
    ESMC_Region_Flag *zeroflag, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayredist()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool checkflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(checkflag) != ESMC_NULL_POINTER)
      if (*checkflag == ESMF_TRUE) checkflagOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    bool finished;
    bool cancelled;
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::redist(
      *srcArray, *dstArray, routehandle, *commflag, &finished, &cancelled,
      *zeroflag, checkflagOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // translate back finishedflag
    if (ESMC_NOT_PRESENT_FILTER(finishedflag) != ESMC_NULL_POINTER){
      if (finished)
        *finishedflag = ESMF_TRUE;
      else
        *finishedflag = ESMF_FALSE;
    }
    // translate back cancelledflag
    if (ESMC_NOT_PRESENT_FILTER(cancelledflag) != ESMC_NULL_POINTER){
      if (cancelled)
        *cancelledflag = ESMF_TRUE;
      else
        *cancelledflag = ESMF_FALSE;
    }
  }
  
  void FTN_X(c_esmc_arraysmmstoreind4)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle, 
    ESMC_TypeKind_Flag *typekindFactors, void *factorList, int *factorListCount,
    ESMCI::InterArray<ESMC_I4> *factorIndexList, 
    ESMC_Logical *ignoreUnmatched,
    int *srcTermProcessing, int *pipelineDepth, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmmstoreind4()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 1.0"));
#endif

    try{
    
    // check argument consistency
    if (*factorListCount > 0){
      // must provide valid factorList and factorIndexList args
      if (!present(factorIndexList)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
          "Not a valid pointer to factorIndexList array", ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "factorIndexList array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->extent[0] != 2 && 
        (factorIndexList)->extent[0] != 4){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of factorIndexList array must be of size 2 or 4",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->extent[1] != *factorListCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of factorIndexList does not match factorListCount",
          ESMC_CONTEXT, rc);
        return;
      }
    }
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 2.0"));
#endif
    // ignoreUnmatched flag
    bool ignoreUnmatchedOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(ignoreUnmatched) != ESMC_NULL_POINTER)
      if (*ignoreUnmatched == ESMF_TRUE) ignoreUnmatchedOpt = true;
    // prepare SparseMatrix vector
    vector<ESMCI::SparseMatrix<ESMC_I4,ESMC_I4> > sparseMatrix;
    int srcN = (factorIndexList)->extent[0]/2;
    int dstN = (factorIndexList)->extent[0]/2;
    sparseMatrix.push_back(ESMCI::SparseMatrix<ESMC_I4,ESMC_I4>(
      *typekindFactors, factorList, *factorListCount, srcN, dstN, 
      (factorIndexList)->array));
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 3.0"));
#endif
    // Call into the actual C++ method wrapped inside LogErr handling
    if (ESMC_LogDefault.MsgFoundError(ESMCI::Array::sparseMatMulStore(
      *srcArray, *dstArray, routehandle, sparseMatrix, false, ignoreUnmatchedOpt,
      ESMC_NOT_PRESENT_FILTER(srcTermProcessing),
      ESMC_NOT_PRESENT_FILTER(pipelineDepth)),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 4.0"));
#endif
    
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Caught exception", ESMC_CONTEXT, rc);
      return;
    }
  
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 5.0"));
#endif

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_arraysmmstoreind8)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle, 
    ESMC_TypeKind_Flag *typekindFactors, void *factorList, int *factorListCount,
    ESMCI::InterArray<ESMC_I8> *factorIndexList, 
    ESMC_Logical *ignoreUnmatched,
    int *srcTermProcessing, int *pipelineDepth, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmmstoreind8()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 1.0"));
#endif

    try{
    
    // check argument consistency
    if (*factorListCount > 0){
      // must provide valid factorList and factorIndexList args
      if (!present(factorIndexList)){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
          "Not a valid pointer to factorIndexList array", ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "factorIndexList array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->extent[0] != 2 && 
        (factorIndexList)->extent[0] != 4){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of factorIndexList array must be of size 2 or 4",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((factorIndexList)->extent[1] != *factorListCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dimension of factorIndexList does not match factorListCount",
          ESMC_CONTEXT, rc);
        return;
      }
    }
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 2.0"));
#endif
    // ignoreUnmatched flag
    bool ignoreUnmatchedOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(ignoreUnmatched) != ESMC_NULL_POINTER)
      if (*ignoreUnmatched == ESMF_TRUE) ignoreUnmatchedOpt = true;
    // prepare SparseMatrix vector
    vector<ESMCI::SparseMatrix<ESMC_I8,ESMC_I8> > sparseMatrix;
    int srcN = (factorIndexList)->extent[0]/2;
    int dstN = (factorIndexList)->extent[0]/2;
    sparseMatrix.push_back(ESMCI::SparseMatrix<ESMC_I8,ESMC_I8>(
      *typekindFactors, factorList, *factorListCount, srcN, dstN, 
      (factorIndexList)->array));
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 3.0"));
#endif
    // Call into the actual C++ method wrapped inside LogErr handling
    if (ESMC_LogDefault.MsgFoundError(ESMCI::Array::sparseMatMulStore(
      *srcArray, *dstArray, routehandle, sparseMatrix, false, ignoreUnmatchedOpt,
      ESMC_NOT_PRESENT_FILTER(srcTermProcessing),
      ESMC_NOT_PRESENT_FILTER(pipelineDepth)),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 4.0"));
#endif
    
    }catch(int localrc){
      // catch standard ESMF return code
      ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        rc);
      return;
    }catch(exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return;
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Caught exception", ESMC_CONTEXT, rc);
      return;
    }
  
#ifdef ASMM_STORE_MEMLOG_on
    ESMCI::VM::logMemInfo(std::string(ESMC_METHOD": 5.0"));
#endif

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_arraysmmstorenf)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle, 
    ESMC_Logical *ignoreUnmatched,
    int *srcTermProcessing, int *pipelineDepth, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmmstorenf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // ignoreUnmatched flag
    bool ignoreUnmatchedOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(ignoreUnmatched) != ESMC_NULL_POINTER)
      if (*ignoreUnmatched == ESMF_TRUE) ignoreUnmatchedOpt = true;
    // prepare empty SparseMatrix vector
    ESMC_TypeKind_Flag srcIndexTK = (*srcArray)->getDistGrid()->getIndexTK();
    ESMC_TypeKind_Flag dstIndexTK = (*dstArray)->getDistGrid()->getIndexTK();
    if (srcIndexTK==ESMC_TYPEKIND_I4 && dstIndexTK==ESMC_TYPEKIND_I4){
      vector<ESMCI::SparseMatrix<ESMC_I4,ESMC_I4> > sparseMatrix;
      // Call into the actual C++ method wrapped inside LogErr handling
      if (ESMC_LogDefault.MsgFoundError(ESMCI::Array::sparseMatMulStore(
        *srcArray, *dstArray, routehandle, sparseMatrix, false, 
        ignoreUnmatchedOpt, ESMC_NOT_PRESENT_FILTER(srcTermProcessing),
        ESMC_NOT_PRESENT_FILTER(pipelineDepth)),
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }else if (srcIndexTK==ESMC_TYPEKIND_I8 && dstIndexTK==ESMC_TYPEKIND_I8){
      vector<ESMCI::SparseMatrix<ESMC_I8,ESMC_I8> > sparseMatrix;
      // Call into the actual C++ method wrapped inside LogErr handling
      if (ESMC_LogDefault.MsgFoundError(ESMCI::Array::sparseMatMulStore(
        *srcArray, *dstArray, routehandle, sparseMatrix, false, 
        ignoreUnmatchedOpt, ESMC_NOT_PRESENT_FILTER(srcTermProcessing),
        ESMC_NOT_PRESENT_FILTER(pipelineDepth)),
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc))) return;
    }else{
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
        "Type option not supported", ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_arraysmm)(ESMCI::Array **srcArray,
    ESMCI::Array **dstArray, ESMCI::RouteHandle **routehandle,
    ESMC_CommFlag *commflag, ESMC_Logical *finishedflag,
    ESMC_Logical *cancelledflag, ESMC_Region_Flag *zeroflag,
    ESMC_TermOrder_Flag *termorderflag, ESMC_Logical *checkflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysmm()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool checkflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(checkflag) != ESMC_NULL_POINTER)
      if (*checkflag == ESMF_TRUE) checkflagOpt = true;
    // Call into the actual C++ method wrapped inside LogErr handling
    bool finished;
    bool cancelled;
    ESMC_LogDefault.MsgFoundError(ESMCI::Array::sparseMatMul(
      *srcArray, *dstArray, routehandle, *commflag, &finished, &cancelled,
      *zeroflag, *termorderflag, checkflagOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // translate back finishedflag
    if (ESMC_NOT_PRESENT_FILTER(finishedflag) != ESMC_NULL_POINTER){
      if (finished)
        *finishedflag = ESMF_TRUE;
      else
        *finishedflag = ESMF_FALSE;
    }
    // translate back cancelledflag
    if (ESMC_NOT_PRESENT_FILTER(cancelledflag) != ESMC_NULL_POINTER){
      if (cancelled)
        *cancelledflag = ESMF_TRUE;
      else
        *cancelledflag = ESMF_FALSE;
    }
  }
  
  void FTN_X(c_esmc_arraygather)(ESMCI::Array **array, void *farray,
    ESMC_TypeKind_Flag *typekind, int *rank, int *counts,
    int *tile, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraygather()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->gather(
      farray, *typekind, *rank, counts, ESMC_NOT_PRESENT_FILTER(tile),
      *rootPet, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arraygathernotroot)(ESMCI::Array **array,
    int *tile, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraygathernotroot()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->gather(
      NULL, ESMF_NOKIND, 0, NULL, ESMC_NOT_PRESENT_FILTER(tile),
      *rootPet, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arrayscatter)(ESMCI::Array **array, void *farray,
    ESMC_TypeKind_Flag *typekind, int *rank, int *counts,
    int *tile, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayscatter()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->scatter(
      farray, *typekind, *rank, counts, ESMC_NOT_PRESENT_FILTER(tile),
      *rootPet, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arrayscatternotroot)(ESMCI::Array **array,
    int *tile, int *rootPet, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayscatternotroot()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->scatter(
      NULL, ESMF_NOKIND, 0, NULL, ESMC_NOT_PRESENT_FILTER(tile),
      *rootPet, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arrayset)(ESMCI::Array **array,
    ESMCI::InterArray<int> *computationalLWidthArg,
    ESMCI::InterArray<int> *computationalUWidthArg, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayset()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->setComputationalLWidth(
      computationalLWidthArg),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // Call into the C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->setComputationalUWidth(
      computationalUWidthArg),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arraysetplocalde)(ESMCI::Array **array,
    int *localDe, ESMCI::InterArray<ESMC_I4> *rimSeqIndexArg, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysetplocalde()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->setRimSeqIndex(*localDe,
      rimSeqIndexArg),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arraysetplocaldei8)(ESMCI::Array **array,
    int *localDe, ESMCI::InterArray<ESMC_I8> *rimSeqIndexArg, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraysetplocalde()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->setRimSeqIndex(*localDe,
      rimSeqIndexArg),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_arrayserialize)(ESMCI::Array **array, char *buf, int *length,
    int *offset, ESMC_AttReconcileFlag *attreconflag,
    ESMC_InquireFlag *inquireflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->serialize(
      buf, length, offset, *attreconflag, *inquireflag),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_arraydeserialize)(ESMCI::Array **array, char *buf,
    int *offset, ESMC_AttReconcileFlag *attreconflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydeserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    *array = new ESMCI::Array(-1);  // prevent baseID counter increment
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*array)->deserialize(
      buf, offset, *attreconflag),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
//-------------------------------------------------------------------------
// The following glue code is for a first newArray prototype which I used
// to check out some communication ideas: DE-nonblocking paradigm!
//-------------------------------------------------------------------------

#ifdef FIRSTNEWARRAYPROTOTYPE


// the interface subroutine names MUST be in lower case
extern "C" {

  // - ESMF-public methods:

  void FTN_X(c_esmc_newarraycreate)(ESMC_newArray **ptr, ESMC_LocalArray **larray,
    int *haloWidth, int *len_haloWidth, int *deCount, int *rootPET, int *rc){
    int localrc;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarraycreate()"
    // Deal with optional arguments
    ESMC_LocalArray *opt_larray;
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    int *opt_haloWidth = NULL;
    if (*len_haloWidth) opt_haloWidth = haloWidth;
    int opt_deCount;
    if (ESMC_NOT_PRESENT_FILTER(deCount) == ESMC_NULL_POINTER) opt_deCount = 0;
    else opt_deCount = *deCount;
    *ptr = ESMC_newArrayCreate(opt_larray, opt_haloWidth, opt_deCount, *rootPET,
      &localrc);
    // Use LogErr to handle return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_newarraydestroy)(ESMC_newArray **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarraydestroy()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(ESMC_newArrayDestroy(ptr),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_newarrayprint)(ESMC_newArray **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayprint()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayPrint(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_newarrayget)(ESMC_newArray **ptr, int *rank,
    ESMCI::DELayout **delayout, ESMC_LocalArray **localArrays, 
    int *len_localArrays, int *globalFullLBound, int *len_globalFullLBound,
    int *globalFullUBound, int *len_globalFullUBound,
    int *globalDataLBound, int *len_globalDataLBound,
    int *globalDataUBound, int *len_globalDataUBound,
    int *localDataLBound, int *len_localDataLBound,
    int *localDataUBound, int *len_localDataUBound,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayget()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayGet(
      ESMC_NOT_PRESENT_FILTER(rank), ESMC_NOT_PRESENT_FILTER(delayout),
      localArrays, *len_localArrays, globalFullLBound, len_globalFullLBound,
      globalFullUBound, len_globalFullUBound, 
      globalDataLBound, len_globalDataLBound,
      globalDataUBound, len_globalDataUBound,
      localDataLBound, len_localDataLBound, 
      localDataUBound, len_localDataUBound),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_newarrayscatterb)(ESMC_newArray **ptr, 
    ESMC_LocalArray **larray, int *rootPET, ESMCI::VM **vm, int *rc){
    // PET-based blocking scatter
    int localrc;
    ESMC_LocalArray *opt_larray;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatterb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayScatter(
      opt_larray, *rootPET, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_newarrayscatternbroot)(ESMC_newArray **ptr, 
    ESMC_LocalArray **larray, int *rootPET, ESMC_newArrayCommHandle **commh,
    ESMCI::VM **vm, int *rc){
    // DE-based non-blocking scatter (root call)
    int localrc;
    ESMC_LocalArray *opt_larray;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatternbroot()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) 
      opt_vm = ESMCI::VM::getCurrent(&localrc);
    else opt_vm = *vm;
    // check if this is rootPET
    int localPET = opt_vm->getLocalPet();
    // if this is not the rootPET then exit because this is root side of scatter
    if (localPET != *rootPET){
      if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return; // bail out
    }
    if (*commh != ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      " - a previously used commhandle has not been deleted",
      ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
    // Allocate a new commhandle
    *commh = new ESMC_newArrayCommHandle;
    (*commh)->commhandleCount = 0;  // reset
    (*commh)->pthidCount = 0;       // reset
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayScatter(
      opt_larray, *rootPET, *commh, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_newarrayscatternb)(ESMC_newArray **ptr, 
    ESMC_LocalArray **larray, int *rootPET, int *de, ESMCI::VM **vm, int *rc){
    // DE-based non-blocking scatter
    int localrc;
    ESMC_LocalArray *opt_larray;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatternb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(larray) == ESMC_NULL_POINTER) opt_larray = NULL;
    else opt_larray = *larray;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayScatter(
      opt_larray, *rootPET, *de, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_newarrayreducescalarb)(ESMC_newArray **ptr, void *result,
    ESMC_TypeKind_Flag *dtk, ESMC_Operation *op, int *rootPET, ESMCI::VM **vm,
    int *rc){
    // PET-based blocking scalar reduce
    int localrc;
    ESMCI::VM *opt_vm;    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayreducescalarb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayScalarReduce(
      result, *dtk, *op, *rootPET, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_newarrayreducescalarnbroot)(ESMC_newArray **ptr,
    void *result, ESMC_TypeKind_Flag *dtk, ESMC_Operation *op, int *rootPET,
    ESMC_newArrayCommHandle **commh, ESMCI::VM **vm, int *rc){
    // DE-based non-blocking reduce (root call)
    int localrc;
    ESMCI::VM *opt_vm;    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayreducescalarnbroot()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) 
      opt_vm = ESMCI::VM::getCurrent(&localrc);
    else opt_vm = *vm;
    // check if this is rootPET
    int localPET = opt_vm->getLocalPet();
    // if this is not the rootPET then exit because this is root side of scatter
    if (localPET != *rootPET){
      if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return; // bail out
    }
    if (*commh != ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      " - a previously used commhandle has not been deleted",
      ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
    // Allocate a new commhandle
    *commh = new ESMC_newArrayCommHandle;
    (*commh)->commhandleCount = 0;  // reset
    (*commh)->pthidCount = 0;       // reset
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayScalarReduce(
      result, *dtk, *op, *rootPET, *commh, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_newarrayreducescalarnb)(ESMC_newArray **ptr, void *result,
    ESMC_TypeKind_Flag *dtk, ESMC_Operation *op, int *rootPET, int *de,
    ESMCI::VM **vm, int *rc){
    // PET-based blocking scalar reduce
    int localrc;
    ESMCI::VM *opt_vm;    
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayreducescalarnb()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayScalarReduce(
      result, *dtk, *op, *rootPET, *de, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  
// ---- Wait methods ---  
  
  void FTN_X(c_esmc_newarraywaitroot)(ESMC_newArray **ptr, int *rootPET,
    ESMC_newArrayCommHandle **commh, ESMCI::VM **vm, int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatter()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) 
      opt_vm = ESMCI::VM::getCurrent(&localrc);
    else opt_vm = *vm;
    // check if this is rootPET
    int localPET = opt_vm->getLocalPet();
    // if this is not the rootPET then exit because this is root side of scatter
    if (localPET != *rootPET){
      if (ESMC_NOT_PRESENT_FILTER(rc) != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return; // bail out
    }
    // Check if a valid commhandle was provided
    if (*commh == ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      " - a valid commhandle must be provided",
      ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayWait(
      *rootPET, *commh, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // delete the commhandle and set to NULL
    delete *commh;
    *commh = ESMC_NULL_POINTER;
  }
  
  void FTN_X(c_esmc_newarraywaitde)(ESMC_newArray **ptr, int *de, ESMCI::VM **vm, 
    int *rc){
    int localrc;
    ESMCI::VM *opt_vm;
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_newarrayscatter()"
    // Deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError((*ptr)->ESMC_newArrayWait(
      *de, opt_vm),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

#undef  ESMC_METHOD
}

#endif
}
