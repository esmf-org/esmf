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
#define ESMC_FILENAME "ESMCI_DistGrid_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
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
    ESMCI::DistGrid **dg, ESMCI::InterArray<int> *firstExtra,
    ESMCI::InterArray<int> *lastExtra, ESMC_IndexFlag *indexflag,
    ESMCI::InterArray<int> *connectionList, ESMC_Logical *balanceflag,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedg()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    ESMCI::VM *opt_vm;
    bool actualFlag = true;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      opt_vm = NULL;
    else{
      opt_vm = *vm;
      if (opt_vm == NULL)
        actualFlag = false; // not an actual member because VM present but NULL
    }
#if 0
    printf("c_esmc_distgridcreatedg(): opt_vm=%p, actualFlag=%d\n", 
      opt_vm, actualFlag);
#endif
    ESMCI::DELayout *opt_delayout;
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER)
      opt_delayout = NULL;
    else{
      opt_delayout = *delayout;
    }
    // ensure 'dg' argument is valid to be dereferenced
    if (ESMC_NOT_PRESENT_FILTER(dg) == ESMC_NULL_POINTER){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "The 'dg' argument must not be a NULL pointer",
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
      return; // bail out
    }
    // convert to bool
    bool balanceflagOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(balanceflag) != ESMC_NULL_POINTER)
      if (*balanceflag == ESMF_TRUE) balanceflagOpt = true;
    // all PETs call into the C++ create(), but the actualFlag identifies PETs
    // that are expected to create actual DistGrid objects
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    *ptr = ESMCI::DistGrid::create(*dg, firstExtra, lastExtra,
      ESMC_NOT_PRESENT_FILTER(indexflag), connectionList, 
      balanceflagOpt, opt_delayout, opt_vm, actualFlag, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_distgridcreaterd)(ESMCI::DistGrid **ptr, 
    ESMCI::InterArray<int> *minIndex, ESMCI::InterArray<int> *maxIndex,
    ESMCI::InterArray<int> *regDecomp,
    ESMCI::Decomp_Flag *decompflag, int *decompflagCount, 
    ESMCI::InterArray<int> *regDecompFirstExtra,
    ESMCI::InterArray<int> *regDecompLastExtra,
    ESMCI::InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterArray<int> *connectionList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterd()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // deal with optional arguments
    ESMCI::DELayout *opt_delayout;
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else
      opt_delayout = *delayout;
    ESMCI::VM *opt_vm;
    bool actualFlag = true;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER)
      opt_vm = NULL;
    else{
      opt_vm = *vm;
      if (opt_vm == NULL)
        actualFlag = false; // not an actual member because VM present but NULL
    }
#if 0
    printf("c_esmc_distgridcreaterd(): opt_delayout=%p, opt_vm=%p, "
      "actualFlag=%d\n", opt_delayout, opt_vm, actualFlag);
#endif
    if (actualFlag){
      // on PETs with actual members call into C++
      // test for NULL pointer via macro before calling any class methods
      ESMCI_NULL_CHECK_PRC(ptr, rc)
      *ptr = ESMCI::DistGrid::create(minIndex, maxIndex, regDecomp,
        decompflag, *decompflagCount, regDecompFirstExtra, regDecompLastExtra,
        deLabelList, ESMC_NOT_PRESENT_FILTER(indexflag),
        connectionList, opt_delayout, opt_vm, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return; // bail out
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_distgridcreaterdt)(ESMCI::DistGrid **ptr, 
    ESMCI::InterArray<int> *minIndex, ESMCI::InterArray<int> *maxIndex,
    ESMCI::InterArray<int> *regDecomp,
    ESMCI::Decomp_Flag *decompflag,
    int *decompflagCount1, int *decompflagCount2,
    ESMCI::InterArray<int> *regDecompFirstExtra,
    ESMCI::InterArray<int> *regDecompLastExtra,
    ESMCI::InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterArray<int> *connectionList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdt()"
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
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    *ptr = ESMCI::DistGrid::create(minIndex, maxIndex, regDecomp,
      decompflag, *decompflagCount1, *decompflagCount2, 
      regDecompFirstExtra, regDecompLastExtra, deLabelList, 
      ESMC_NOT_PRESENT_FILTER(indexflag), connectionList,
      opt_delayout, opt_vm, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridcreaterdf)(ESMCI::DistGrid **ptr, 
    ESMCI::InterArray<int> *minIndex, ESMCI::InterArray<int> *maxIndex,
    ESMCI::InterArray<int> *regDecomp,
    ESMCI::Decomp_Flag *decompflag, int *decompflagCount,
    ESMCI::InterArray<int> *regDecompFirstExtra,
    ESMCI::InterArray<int> *regDecompLastExtra,
    ESMCI::InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterArray<int> *connectionList,
    int *fastAxis, ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreaterdf()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM *opt_vm;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    *ptr = ESMCI::DistGrid::create(minIndex, maxIndex, regDecomp,
      decompflag, *decompflagCount, regDecompFirstExtra, regDecompLastExtra,
      deLabelList, ESMC_NOT_PRESENT_FILTER(indexflag),
      connectionList, *fastAxis, opt_vm, &localrc);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridcreatedb)(ESMCI::DistGrid **ptr, 
    ESMCI::InterArray<int> *minIndex, ESMCI::InterArray<int> *maxIndex,
    ESMCI::InterArray<int> *deBlockList,
    ESMCI::InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterArray<int> *connectionList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, ESMC_TypeKind_Flag *indexTK, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
    ESMC_TypeKind_Flag opt_indexTK;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    if (ESMC_NOT_PRESENT_FILTER(indexTK) == ESMC_NULL_POINTER) 
      opt_indexTK = ESMF_NOKIND;
    else opt_indexTK = *indexTK;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    *ptr = ESMCI::DistGrid::create(minIndex, maxIndex, deBlockList,
      deLabelList, ESMC_NOT_PRESENT_FILTER(indexflag),
      connectionList, opt_delayout, opt_vm, &localrc, opt_indexTK);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridcreatedbt)(ESMCI::DistGrid **ptr, 
    ESMCI::InterArray<int> *minIndex, ESMCI::InterArray<int> *maxIndex,
    ESMCI::InterArray<int> *deBlockList, ESMCI::InterArray<int> *deToTileMap,
    ESMCI::InterArray<int> *deLabelList, ESMC_IndexFlag *indexflag, 
    ESMCI::InterArray<int> *connectionList,
    ESMCI::DELayout **delayout, ESMCI::VM **vm, ESMC_TypeKind_Flag *indexTK, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridcreatedbt()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::DELayout *opt_delayout;
    ESMCI::VM *opt_vm;
    ESMC_TypeKind_Flag opt_indexTK;
    // deal with optional arguments
    if (ESMC_NOT_PRESENT_FILTER(delayout) == ESMC_NULL_POINTER) 
      opt_delayout = NULL;
    else opt_delayout = *delayout;
    if (ESMC_NOT_PRESENT_FILTER(vm) == ESMC_NULL_POINTER) opt_vm = NULL;
    else opt_vm = *vm;
    if (ESMC_NOT_PRESENT_FILTER(indexTK) == ESMC_NULL_POINTER) 
      opt_indexTK = ESMF_NOKIND;
    else opt_indexTK = *indexTK;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    *ptr = ESMCI::DistGrid::create(minIndex, maxIndex, deBlockList,
      deToTileMap, deLabelList, ESMC_NOT_PRESENT_FILTER(indexflag),
      connectionList, opt_delayout, opt_vm, &localrc, opt_indexTK);
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgriddestroy)(ESMCI::DistGrid **ptr, 
    ESMC_Logical *noGarbage, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // convert to bool
    bool noGarbageOpt = false;  // default
    if (ESMC_NOT_PRESENT_FILTER(noGarbage) != ESMC_NULL_POINTER)
      if (*noGarbage == ESMF_TRUE) noGarbageOpt = true;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError(ESMCI::DistGrid::destroy(ptr, noGarbageOpt),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, 
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridget)(ESMCI::DistGrid **ptr,
    int *dimCount, int *tileCount, int *deCount, int *localDeCount,
    ESMCI::InterArray<int> *minIndexPDimPTile,
    ESMCI::InterArray<int> *maxIndexPDimPTile,
    ESMCI::InterArray<int> *elementCountPTile,
    ESMCI::InterArray<int> *minIndexPDimPDe,
    ESMCI::InterArray<int> *maxIndexPDimPDe,
    ESMCI::InterArray<int> *elementCountPDe,
    ESMCI::InterArray<int> *localDeToDeMap,
    ESMCI::InterArray<int> *tileListPDe,
    ESMCI::InterArray<int> *indexCountPDimPDe,
    ESMCI::InterArray<int> *collocationPDim,
    ESMC_Logical *regDecompFlag, 
    int *connectionCount,
    ESMCI::InterArray<int> *connectionList,
    ESMCI::DELayout **delayout, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // fill simple return values
    if (ESMC_NOT_PRESENT_FILTER(delayout) != ESMC_NULL_POINTER)
      *delayout = (*ptr)->getDELayout();
    if (ESMC_NOT_PRESENT_FILTER(tileCount) != ESMC_NULL_POINTER)
      *tileCount = (*ptr)->getTileCount();
    if (ESMC_NOT_PRESENT_FILTER(deCount) != ESMC_NULL_POINTER)
      *deCount = (*ptr)->getDELayout()->getDeCount();
    if (ESMC_NOT_PRESENT_FILTER(localDeCount) != ESMC_NULL_POINTER)
      *localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
    if (ESMC_NOT_PRESENT_FILTER(dimCount) != ESMC_NULL_POINTER)
      *dimCount = (*ptr)->getDimCount();
    if (ESMC_NOT_PRESENT_FILTER(connectionCount) != ESMC_NULL_POINTER)
      *connectionCount = (*ptr)->getConnectionCount();
    if (ESMC_NOT_PRESENT_FILTER(regDecompFlag) != ESMC_NULL_POINTER){
      if ((*ptr)->getRegDecomp())
        *regDecompFlag = ESMF_TRUE;
      else
        *regDecompFlag = ESMF_FALSE;
    }
    // fill minIndexPDimPTile
    if (present(minIndexPDimPTile)){
      // minIndexPDimPTile was provided -> do some error checking
      if ((minIndexPDimPTile)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "minIndexPDimPTile array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((minIndexPDimPTile)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of minIndexPDimPTile array must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((minIndexPDimPTile)->extent[1] < (*ptr)->getTileCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dim of minIndexPDimPTile array must be of size 'tileCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in minIndexPDimPTile
      // arrays which are larger than dimCount x tileCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the minIndexPDimPTile
      // array.
      for (int i=0; i<(*ptr)->getTileCount(); i++)
        memcpy(
          &((minIndexPDimPTile)->array[i*((minIndexPDimPTile)->extent[0])]),
          &(((*ptr)->getMinIndexPDimPTile())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill maxIndexPDimPTile
    if (present(maxIndexPDimPTile)){
      // maxIndexPDimPTile was provided -> do some error checking
      if ((maxIndexPDimPTile)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "maxIndexPDimPTile array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((maxIndexPDimPTile)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of maxIndexPDimPTile array must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((maxIndexPDimPTile)->extent[1] < (*ptr)->getTileCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dim of maxIndexPDimPTile array must be of size 'tileCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in maxIndexPDimPTile
      // arrays which are larger than dimCount x tileCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the maxIndexPDimPTile
      // array.
      for (int i=0; i<(*ptr)->getTileCount(); i++)
        memcpy(
          &((maxIndexPDimPTile)->array[i*((maxIndexPDimPTile)->extent[0])]),
          &(((*ptr)->getMaxIndexPDimPTile())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill elementCountPTile
    if (present(elementCountPTile)){
      // elementCountPTile was provided -> do some error checking
      if ((elementCountPTile)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "elementCountPTile array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((elementCountPTile)->extent[0] < (*ptr)->getTileCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of elementCountPTile array must be of size 'tileCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
#ifdef TYPESMATCH
      memcpy((elementCountPTile)->array, (*ptr)->getElementCountPTile(),
        sizeof(int)*(*ptr)->getTileCount());
#else
      int tileCount = (*ptr)->getTileCount();
      const ESMC_I8 *access = (*ptr)->getElementCountPTile();
      for (int i=0; i<tileCount; i++)
        (elementCountPTile)->array[i] = (int)(access[i]); // explicit type cast
#endif
    }
    // fill minIndexPDimPDe
    if (present(minIndexPDimPDe)){
      // minIndexPDimPDe was provided -> do some error checking
      if ((minIndexPDimPDe)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "minIndexPDimPDe array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((minIndexPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of minIndexPDimPDe array must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((minIndexPDimPDe)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dim of minIndexPDimPDe array must be of size 'deCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in minIndexPDimPDe
      // arrays which are larger than dimCount x deCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the minIndexPDimPDe
      // array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(
          &((minIndexPDimPDe)->array[i*((minIndexPDimPDe)->extent[0])]),
          &(((*ptr)->getMinIndexPDimPDe())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill maxIndexPDimPDe
    if (present(maxIndexPDimPDe)){
      // maxIndexPDimPDe was provided -> do some error checking
      if ((maxIndexPDimPDe)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "maxIndexPDimPDe array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((maxIndexPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of maxIndexPDimPDe array must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((maxIndexPDimPDe)->extent[1] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dim of maxIndexPDimPDe array must be of size 'deCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in maxIndexPDimPDe
      // arrays which are larger than dimCount x deCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the maxIndexPDimPDe
      // array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(
          &((maxIndexPDimPDe)->array[i*((maxIndexPDimPDe)->extent[0])]),
          &(((*ptr)->getMaxIndexPDimPDe())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill elementCountPDe
    if (present(elementCountPDe)){
      // elementCountPDe was provided -> do some error checking
      if ((elementCountPDe)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "elementCountPDe array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((elementCountPDe)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of elementCountPDe array must be of size 'deCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
#ifdef TYPESMATCH
      memcpy((elementCountPDe)->array, (*ptr)->getElementCountPDe(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
#else
      int deCount = (*ptr)->getDELayout()->getDeCount();
      const ESMC_I8 *access = (*ptr)->getElementCountPDe();
      for (int i=0; i<deCount; i++)
        (elementCountPDe)->array[i] = (int)(access[i]); // explicit type cast
#endif
    }
    // fill localDeToDeMap
    if (present(localDeToDeMap)){
      // localDeToDeMap was provided -> do some error checking
      if ((localDeToDeMap)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "localDeToDeMap array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((localDeToDeMap)->extent[0] < (*ptr)->getDELayout()->getLocalDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of localDeToDeMap array must be of size 'localDeCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      memcpy((localDeToDeMap)->array, (*ptr)->getDELayout()->getLocalDeToDeMap(),
        sizeof(int)*(*ptr)->getDELayout()->getLocalDeCount());
    }
    // fill tileListPDe
    if (present(tileListPDe)){
      // tileListPDe was provided -> do some error checking
      if ((tileListPDe)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "tileListPDe array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((tileListPDe)->extent[0] < (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of tileListPDe array must be of size 'deCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      memcpy((tileListPDe)->array, (*ptr)->getTileListPDe(),
        sizeof(int)*(*ptr)->getDELayout()->getDeCount());
    }
    // fill indexCountPDimPDe
    if (present(indexCountPDimPDe)){
      // indexCountPDimPDe was provided -> do some error checking
      if ((indexCountPDimPDe)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "indexCountPDimPDe array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((indexCountPDimPDe)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of indexCountPDimPDe array must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((indexCountPDimPDe)->extent[1] <
        (*ptr)->getDELayout()->getDeCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dim of indexCountPDimPDe array must be of size 'deCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values: The interface allows to pass in indexCountPDimPDe
      // arrays which are larger than dimCount x deCount. Consequently it is
      // necessary to memcpy strips of contiguous data since it cannot be
      // assumed that all data ends up contiguous in the indexCountPDimPDe
      // array.
      for (int i=0; i<(*ptr)->getDELayout()->getDeCount(); i++)
        memcpy(
          &((indexCountPDimPDe)->array[i*((indexCountPDimPDe)->extent[0])]),
          &(((*ptr)->getIndexCountPDimPDe())[i*(*ptr)->getDimCount()]),
          sizeof(int)*(*ptr)->getDimCount());
    }
    // fill collocationPDim
    if (present(collocationPDim)){
      // collocationPDim was provided -> do some error checking
      if ((collocationPDim)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "collocationPDim array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((collocationPDim)->extent[0] < (*ptr)->getDimCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of collocationPDim array must be of size 'dimCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in values
      memcpy((collocationPDim)->array, (*ptr)->getCollocationPDim(),
        sizeof(int)*((*ptr)->getDimCount()));
    }
    // fill connectionList
    if (present(connectionList)){
      // connectionList was provided -> do some error checking
      if ((connectionList)->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "connectionList array must be of rank 2", ESMC_CONTEXT, rc);
        return;
      }
      if ((connectionList)->extent[0] < 2*((*ptr)->getDimCount()) + 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dim of connectionList array must be of size '2*dimCount + 2'",
          ESMC_CONTEXT, rc);
        return;
      }
      if ((connectionList)->extent[1] < (*ptr)->getConnectionCount()){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "2nd dim of connectionList array must be of size 'connectionCount'",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values:
      for (int i=0; i<(*ptr)->getConnectionCount(); i++){
        memcpy(
          &((connectionList)->array[i*((connectionList)->extent[0])]),
          ((*ptr)->getConnectionList())[i], 
          sizeof(int)*(2*((*ptr)->getDimCount())+2));
      }
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_distgridgetplocalde)(ESMCI::DistGrid **ptr,
    int *localDeArg, int *collocationArg, ESMC_Logical *arbSeqIndexFlag,
    ESMCI::InterArray<int> *seqIndexList, int *elementCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetplocalde()"
    // Initialize return code; assume routine not implemented
    if (ESMC_NOT_PRESENT_FILTER(rc)) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // shift input indices
    int localDe = *localDeArg;  // already base 0
    // check input values
    int localDeCount = (*ptr)->getDELayout()->getLocalDeCount();
    if (localDe < 0 || localDe > localDeCount-1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "Specified local DE out of bounds", ESMC_CONTEXT,
        ESMC_NOT_PRESENT_FILTER(rc));
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
          "specified collocation not valid", ESMC_CONTEXT,
          ESMC_NOT_PRESENT_FILTER(rc));
        return;
      }
      collIndex = i;
    }else{
      collocation = collocationTable[0]; // default to first collocation 
      collIndex = 0;
    }
    const int *arbSeqIndexList =
      (const int *)(*ptr)->getArbSeqIndexList(localDe, collocation, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
    if (ESMC_NOT_PRESENT_FILTER(arbSeqIndexFlag) != ESMC_NULL_POINTER){  
      if (arbSeqIndexList)
        *arbSeqIndexFlag = ESMF_TRUE;
      else
        *arbSeqIndexFlag = ESMF_FALSE;
    }
    // fill seqIndexList
    localrc = (*ptr)->fillSeqIndexList(seqIndexList, localDe, collocation);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
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
    int *localDeArg, int *dimArg, ESMCI::InterArray<int> *indexList, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridgetplocaldepdim()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // shift input indices
    int localDe = *localDeArg;  // already base 0
    int dim = *dimArg - 1;      // shift to base 0
    // fill indexList
    if (present(indexList)){
      // indexList provided -> get indexListPtr & do some error checking
      // getIndexListPDimPLocalDe() checks localDe and dim for range!
      const int *indexListPtr =
        (*ptr)->getIndexListPDimPLocalDe(localDe, dim+1, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
      if ((indexList)->dimCount != 1){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "indexList array must be of rank 1", ESMC_CONTEXT, rc);
        return;
      }
      if ((indexList)->extent[0] <
        ((*ptr)->getIndexCountPDimPDe())[(*ptr)->getDELayout()->
        getLocalDeToDeMap()[localDe] * (*ptr)->getDimCount()+dim]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "1st dimension of indexList array size insufficiently",
          ESMC_CONTEXT, rc);
        return;
      }
      // fill in the values
      memcpy((indexList)->array, indexListPtr,
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
      ESMC_CONTEXT, ESMC_NOT_PRESENT_FILTER(rc))) return;
  }
  
  void FTN_X(c_esmc_distgridprint)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError((*ptr)->print(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
  }
  
  void FTN_X(c_esmc_distgridvalidate)(ESMCI::DistGrid **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError((*ptr)->validate(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridconnection)(
    ESMCI::InterArray<int> *connection, int *tileIndexA,
    int *tileIndexB, ESMCI::InterArray<int> *positionVector,
    ESMCI::InterArray<int> *orientationVector,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_connection()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(
      ESMCI::DistGrid::connection(connection, *tileIndexA,
      *tileIndexB, positionVector, orientationVector), 
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridrdsetcubic)(
    ESMCI::InterArray<int> *regDecomp, int *deCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridrdsetcubic()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.MsgFoundError(
      ESMCI::DistGrid::regDecompSetCubic(regDecomp, *deCount), 
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridset)(
    ESMCI::DistGrid **ptr, ESMCI::InterArray<int> *collocationPDim, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridset()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError(
      (*ptr)->setCollocationPDim(collocationPDim),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridsetarbseqindex)(
    ESMCI::DistGrid **ptr, ESMCI::InterArray<int> *arbSeqIndex, 
      int *localDe, int *collocation, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridsetarbseqindex()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError(
      (*ptr)->setArbSeqIndex(arbSeqIndex, *localDe, *collocation),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgridsetarbseqindexi8)(
    ESMCI::DistGrid **ptr, ESMCI::InterArray<ESMC_I8> *arbSeqIndex, 
      int *localDe, int *collocation, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridsetarbseqindex()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    ESMC_LogDefault.MsgFoundError(
      (*ptr)->setArbSeqIndex(arbSeqIndex, *localDe, *collocation),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN_X(c_esmc_distgridserialize)(ESMCI::DistGrid **distgrid, char *buf, int *length,
    int *offset, ESMC_InquireFlag *inquireflag, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgridserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(distgrid, rc)
    ESMCI_NULL_CHECK_PRC(*distgrid, rc)
    ESMC_LogDefault.MsgFoundError(
      (*distgrid)->serialize(buf, length, offset, *inquireflag),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc));
  }

  void FTN_X(c_esmc_distgriddeserialize)(ESMCI::DistGrid **distgrid, char *buf,
    int *offset, int *rc,
    ESMCI_FortranStrLenArg buf_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_distgriddeserialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(distgrid, rc)
    *distgrid=ESMCI::DistGrid::deserialize(buf, offset);
    // Return success
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
}

