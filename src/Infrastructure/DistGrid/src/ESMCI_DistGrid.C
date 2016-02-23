// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_DistGrid.C"
//==============================================================================
//
// DistGrid class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DistGrid methods declared
// in the companion file ESMCI_DistGrid.h
//
//-----------------------------------------------------------------------------
// include associated header file
#include "ESMCI_DistGrid.h"

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_Base.h" 
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"
#include "ESMCI_LogErr.h"

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::create
//
// !INTERFACE:
DistGrid *DistGrid::create(
//
// !RETURN VALUE:
//    ESMCI::DistGrid * to newly allocated DistGrid
//
// !ARGUMENTS:
//
  DistGrid *dg,                           // (in)
  InterfaceInt *firstExtra,               // (in)
  InterfaceInt *lastExtra,                // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
  VM *vm,                                 // (in)
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//    Create a new DistGrid from an existing DistGrid, keeping the decomposition
//    unchanged. The firstExtra and lastExtra arguments allow extra elements to
//    be added at the first/last edge DE in each dimension. The method also 
//    allows the indexflag to be set. Further, if the connectionList argument
//    is passed in it will be used to set connections in the newly created
//    DistGrid, otherwise the connections of the incoming DistGrid will be used.
//    If neither firstExtra, lastExtra, indexflag, nor connectionList arguments
//    are specified, the method reduces to a deep copy of the incoming DistGrid
//    object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  DistGrid *distgrid = NULL;  // initialize
  try{
    
  DELayout *delayout = NULL;
  if (!vm || (vm==VM::getCurrent())){
    // only use DELayout of incoming DistGrid if not creating for another VM
    delayout = dg->delayout;
  }
  
  if (present(firstExtra) || present(lastExtra) || indexflag ||
    present(connectionList) || vm){
    // creating a new DistGrid from the existing one considering additional info
    // prepare for internal InterfaceInt usage
    int dimInterfaceInt;
    int *dimCountInterfaceInt = new int[2];
    // prepare connectionList
    bool connectionListInternalFlag = false;
    int *connectionListAlloc = NULL; // default
    if (present(connectionList)){
      // connectionList was provided -> check for correct format
      int elementSize = 2*dg->dimCount+2;
      if (connectionList->dimCount != 2){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
          "- connectionList array must be of rank 2", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (connectionList->extent[0] != elementSize){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of connectionList array must be of size "
          "(2*dimCount+2)", ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      // check tileA & tileB entries
      for (int i=0; i<connectionList->extent[1]; i++){
        if (connectionList->array[elementSize*i] < 1 || 
          connectionList->array[elementSize*i] > dg->tileCount){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- tileA in connectionList element lies outside [1,tileCount]",
            ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
        if (connectionList->array[elementSize*i+1] < 1 ||
          connectionList->array[elementSize*i+1] > dg->tileCount){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- tileB in connectionList element lies outside [1,tileCount]",
            ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
      }
    }else{
      // connectionList was not provided on the interface -> set up internally
      connectionListInternalFlag = true;
      //TODO: connectionList may need to be modified according to
      //TODO: firstExtra and lastExtra arguments
      //TODO: on the other hand it seems like a mistake to add edge padding
      //TODO: across edges that are connected, since padding makes really only
      //TODO: sense for open edges.
      if (dg->connectionCount){
        dimInterfaceInt = 2;
        int elementSize = 2*dg->dimCount+2;
        dimCountInterfaceInt[0] = elementSize;
        dimCountInterfaceInt[1] = dg->connectionCount;
        connectionListAlloc = new int[elementSize * dg->connectionCount];
        for (int i=0; i<dg->connectionCount; i++){
          memcpy(&(connectionListAlloc[elementSize*i]), dg->connectionList[i],
            sizeof(int)*elementSize);
        }
        connectionList = new InterfaceInt(connectionListAlloc,
          dimInterfaceInt, dimCountInterfaceInt);
      }
    }
    // prepare for single- vs. multi-tile case
    dimInterfaceInt = 1;  // default single-tile
    if (dg->tileCount > 1)
      dimInterfaceInt = 2;  // multi-tile
    dimCountInterfaceInt[0] = dg->dimCount;
    if (dimInterfaceInt==2)
      dimCountInterfaceInt[1] = dg->tileCount;
    else
      dimCountInterfaceInt[1] = 1;
    int totalCountInterfaceInt = dimCountInterfaceInt[0]
      * dimCountInterfaceInt[1];
    // consistency check the input argument firstExtra
    if (present(firstExtra)){
      if (firstExtra->dimCount != dimInterfaceInt){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- distgrid and firstExtra arguments differ single/multi tile",
          ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (firstExtra->extent[0] != dg->dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- distgrid and firstExtra arguments assume different dimCount",
          ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (dimInterfaceInt==2){
        if (firstExtra->extent[1] != dg->tileCount){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- distgrid and firstExtra arguments assume different tileCount",
            ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
      }
    }
    // consistency check the input argument lastExtra
    if (present(lastExtra)){
      if (lastExtra->dimCount != dimInterfaceInt){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- distgrid and lastExtra arguments differ single/multi tile",
          ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (lastExtra->extent[0] != dg->dimCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- distgrid and lastExtra arguments assume different dimCount",
          ESMC_CONTEXT, rc);
        return ESMC_NULL_POINTER;
      }
      if (dimInterfaceInt==2){
        if (lastExtra->extent[1] != dg->tileCount){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- distgrid and lastExtra arguments assume different tileCount",
            ESMC_CONTEXT, rc);
          return ESMC_NULL_POINTER;
        }
      }
    }

     
#if 0
    // TURN OFF ERROR WITH CONNECTIONS ON Extra edge, BECAUSE GRID NEEDS TO DO IT
    // edges modified by firstExtra or lastExtra cannot also be connected
    if (present(connectionList)){
      // there are connections
      int elementSize = connectionList->extent[0];
      int connectionCount = connectionList->extent[1];
      for (int i=0; i<connectionCount; i++){
        int *element = connectionList->array + i*elementSize;
        int tileA = element[0];
        int tileB = element[1];
        int *positionVector = element + 2;
        if (present(firstExtra)){
          // there are possible modifications on the lower edge
          for (int j=0; j<dg->dimCount; j++){
            if (positionVector[j]==0) continue; // BOB 
            if (positionVector[j] < 0){
              if (firstExtra->array[dg->dimCount*(tileA-1)+j] != 0){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                  "- connected edges cannot be modified", ESMC_CONTEXT, rc);
                return ESMC_NULL_POINTER;
              }
            }else if (positionVector[j] > 0){
              if (firstExtra->array[dg->dimCount*(tileB-1)+j] != 0){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                  "- connected edges cannot be modified", ESMC_CONTEXT, rc);
                return ESMC_NULL_POINTER;
              }
            }
          }
        }
        if (present(lastExtra)){
          // there are possible modifications on the upper edge
          for (int j=0; j<dg->dimCount; j++){
            if (positionVector[j]==0) continue; // BOB 
            if (positionVector[j] < 0){
              if (lastExtra->array[dg->dimCount*(tileB-1)+j] != 0){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                  "- connected edges cannot be modified", ESMC_CONTEXT, rc);
                return ESMC_NULL_POINTER;
              }
            }else if (positionVector[j] > 0){
              if (lastExtra->array[dg->dimCount*(tileA-1)+j] != 0){
                printf(" posVec[%d]=%d\n",j,positionVector[j]);
                ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                  "- connected edges cannot be modified", ESMC_CONTEXT, rc);
                return ESMC_NULL_POINTER;
              }
            }
          }
        }
      }
    }
#endif
    // prepare minIndex and maxIndex
    int *minIndexAlloc = new int[totalCountInterfaceInt];
    if (present(firstExtra))
      for (int i=0; i<totalCountInterfaceInt; i++)
        minIndexAlloc[i] = dg->minIndexPDimPTile[i]
          - firstExtra->array[i];
    else
      memcpy(minIndexAlloc, dg->minIndexPDimPTile,
        sizeof(int)*totalCountInterfaceInt);
    InterfaceInt *minIndex = new InterfaceInt(minIndexAlloc,
      dimInterfaceInt, dimCountInterfaceInt);
    int *maxIndexAlloc = new int[totalCountInterfaceInt];
    if (present(lastExtra))
      for (int i=0; i<totalCountInterfaceInt; i++)
        maxIndexAlloc[i] = dg->maxIndexPDimPTile[i]
          + lastExtra->array[i];
    else
      memcpy(maxIndexAlloc, dg->maxIndexPDimPTile,
        sizeof(int)*totalCountInterfaceInt);
    InterfaceInt *maxIndex = new InterfaceInt(maxIndexAlloc,
      dimInterfaceInt, dimCountInterfaceInt);
    
    // use indexflag inside dg as the default if not supplied by caller
    ESMC_IndexFlag *indexflagOpt = dg->indexflag; // default
    if (indexflag)
      indexflagOpt = indexflag; // provided by caller
    
    //TODO: maybe also need to hold on to delabellist to preserve DE labeling?
    
    // create DistGrid according to collected information
    if (dg->regDecomp!=NULL){
      // this is a regDecomp
      // prepare regDecomp
      InterfaceInt *regDecomp = new InterfaceInt(dg->regDecomp,
        dimInterfaceInt, dimCountInterfaceInt);
    
      // use decompflag from inside dg to ensure identical decomposition
      Decomp_Flag *decompflag = dg->decompflag;
      
      if (dg->tileCount==1){
        // single tile
        int decompflagCount = 0;  // default
        if (decompflag)
          decompflagCount = dg->dimCount;
        distgrid = DistGrid::create(minIndex, maxIndex, regDecomp, decompflag,
          decompflagCount, firstExtra, lastExtra, NULL,
          indexflagOpt, connectionList, delayout, vm, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      }else{
        // multi tile
        int decompflagCount1 = 0;  // default
        int decompflagCount2 = 0;  // default
        if (decompflag){
          decompflagCount1 = dg->dimCount;
          decompflagCount2 = dg->tileCount;
        }
        distgrid = DistGrid::create(minIndex, maxIndex, regDecomp, decompflag,
          decompflagCount1, decompflagCount2, firstExtra, lastExtra, NULL,
          indexflagOpt, connectionList, delayout, vm, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      }
      delete regDecomp;
    }else{
      // this is a deBlockList
      if (dg->tileCount==1){
        // single tile
        // prepare deBlockList
        int deCount = dg->delayout->getDeCount();
        int dimCount = dg->dimCount;
        int *deBlockListAlloc = new int[dimCount*2*deCount];
        int *deBlockListDims = new int[3];
        deBlockListDims[0] = dimCount;
        deBlockListDims[1] = 2;
        deBlockListDims[2] = deCount;
        InterfaceInt *deBlockList = new InterfaceInt(deBlockListAlloc, 3,
          deBlockListDims);
        delete [] deBlockListDims;
        // fill deBlockListAlloc with correct info
        for (int i=0; i<deCount; i++){
          for (int k=0; k<dimCount; k++){
            deBlockListAlloc[i*2*dimCount+k] =
              dg->minIndexPDimPDe[i*dimCount+k];
            if (present(firstExtra)){
              if (deBlockListAlloc[i*2*dimCount+k]
                == dg->minIndexPDimPTile[k]){
                // found edge DE on single tile DistGrid
                deBlockListAlloc[i*2*dimCount+k] -=
                  firstExtra->array[k];
              }
            }
            deBlockListAlloc[i*2*dimCount+dimCount+k] =
              dg->maxIndexPDimPDe[i*dimCount+k];
            if (present(lastExtra)){
              if (deBlockListAlloc[i*2*dimCount+dimCount+k] ==
                dg->maxIndexPDimPTile[k]){
                // found edge DE on single tile DistGrid
                deBlockListAlloc[i*2*dimCount+dimCount+k] +=
                  lastExtra->array[k];
              }
            }
          }
        }
        // create DistGrid
        distgrid = DistGrid::create(minIndex, maxIndex, deBlockList,
          NULL, indexflagOpt, connectionList, delayout, vm, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
        delete deBlockList;
        delete [] deBlockListAlloc;
      }else{
        // multi tile
        //TODO: activate this branch once deBlockList multi-tile is implemented
      }
    }
    // garbage collection
    delete [] dimCountInterfaceInt;
    delete minIndex;
    delete [] minIndexAlloc;
    delete maxIndex;
    delete [] maxIndexAlloc;
    if (connectionListInternalFlag){
      if (present(connectionList))
        delete connectionList;
      if (connectionListAlloc)
        delete [] connectionListAlloc;
    }
  }else{
    // simple deep copy of the incoming DistGrid
    distgrid = new DistGrid(vm);  // specific VM, or default if vm==NULL
    int dimCount = distgrid->dimCount = dg->dimCount;
    int tileCount = distgrid->tileCount = dg->tileCount;
    int deCount = dg->delayout->getDeCount();
    int localDeCount = dg->delayout->getLocalDeCount();
    distgrid->minIndexPDimPTile = new int[dimCount*tileCount];
    memcpy(distgrid->minIndexPDimPTile, dg->minIndexPDimPTile,
      sizeof(int)*dimCount*tileCount);
    distgrid->maxIndexPDimPTile = new int[dimCount*tileCount];
    memcpy(distgrid->maxIndexPDimPTile, dg->maxIndexPDimPTile,
      sizeof(int)*dimCount*tileCount);
    distgrid->elementCountPTile = new int[tileCount];
    memcpy(distgrid->elementCountPTile, dg->elementCountPTile,
      sizeof(int)*tileCount);
    distgrid->minIndexPDimPDe = new int[dimCount*deCount];
    memcpy(distgrid->minIndexPDimPDe, dg->minIndexPDimPDe,
      sizeof(int)*dimCount*deCount);
    distgrid->maxIndexPDimPDe = new int[dimCount*deCount];
    memcpy(distgrid->maxIndexPDimPDe, dg->maxIndexPDimPDe,
      sizeof(int)*dimCount*deCount);
    distgrid->elementCountPDe = new int[deCount];
    memcpy(distgrid->elementCountPDe, dg->elementCountPDe,
      sizeof(int)*deCount);
    distgrid->tileListPDe = new int[deCount];
    memcpy(distgrid->tileListPDe, dg->tileListPDe,
      sizeof(int)*deCount);
    distgrid->contigFlagPDimPDe = new int[dimCount*deCount];
    memcpy(distgrid->contigFlagPDimPDe, dg->contigFlagPDimPDe,
      sizeof(int)*dimCount*deCount);
    distgrid->indexCountPDimPDe = new int[dimCount*deCount];
    memcpy(distgrid->indexCountPDimPDe, dg->indexCountPDimPDe,
      sizeof(int)*dimCount*deCount);
    distgrid->indexListPDimPLocalDe = new int*[dimCount*localDeCount];
    for (int i=0; i<localDeCount; i++){
      int de = dg->delayout->getLocalDeToDeMap()[i];
      for (int j=0; j<dimCount; j++){
        int size = distgrid->indexCountPDimPDe[de*dimCount+j];
        distgrid->indexListPDimPLocalDe[i*dimCount+j] = new int[size];
        memcpy(distgrid->indexListPDimPLocalDe[i*dimCount+j],
          dg->indexListPDimPLocalDe[i*dimCount+j], sizeof(int)*size);
      }
    }
    int connectionCount = distgrid->connectionCount = dg->connectionCount;
    if (connectionCount){
      int elementSize = 2*dimCount+2;
      distgrid->connectionList = new int*[connectionCount];
      for (int i=0; i<connectionCount; i++){
        distgrid->connectionList[i] = new int[elementSize];
        memcpy(distgrid->connectionList[i], dg->connectionList[i],
          sizeof(int)*elementSize);
      }
    }else
      distgrid->connectionList = NULL;

    distgrid->collocationPDim = new int[dimCount];
    memcpy(distgrid->collocationPDim, dg->collocationPDim,
      sizeof(int)*dimCount);
    distgrid->collocationTable = new int[dimCount];
    memcpy(distgrid->collocationTable, dg->collocationTable,
      sizeof(int)*dimCount);
    int diffCollocationCount =
      distgrid->diffCollocationCount = dg->diffCollocationCount;
    distgrid->arbSeqIndexListPCollPLocalDe = new int**[diffCollocationCount];
    distgrid->elementCountPCollPLocalDe = new int*[diffCollocationCount];
    for (int i=0; i<diffCollocationCount; i++){
      distgrid->arbSeqIndexListPCollPLocalDe[i] = new int*[localDeCount];
      distgrid->elementCountPCollPLocalDe[i] = new int[localDeCount];
      memcpy(distgrid->elementCountPCollPLocalDe[i],
        dg->elementCountPCollPLocalDe[i], sizeof(int)*localDeCount);
      for (int j=0; j<localDeCount; j++){
        if ((dg->arbSeqIndexListPCollPLocalDe[i][j]!=NULL)
          && (dg->elementCountPCollPLocalDe[i][j]>0)){
          distgrid->arbSeqIndexListPCollPLocalDe[i][j] =
            new int[dg->elementCountPCollPLocalDe[i][j]];
          memcpy(distgrid->arbSeqIndexListPCollPLocalDe[i][j],
            dg->arbSeqIndexListPCollPLocalDe[i][j],
            sizeof(int)*dg->elementCountPCollPLocalDe[i][j]);
        }else{
          distgrid->arbSeqIndexListPCollPLocalDe[i][j] = NULL;
        }
      }
    }
    if (dg->regDecomp){
      distgrid->regDecomp = new int[dimCount];
      memcpy(distgrid->regDecomp, dg->regDecomp, sizeof(int)*dimCount);
    }else
      distgrid->regDecomp = NULL;
    if (dg->decompflag){
      distgrid->decompflag = new Decomp_Flag[dimCount];
      memcpy(distgrid->decompflag, dg->decompflag,
        sizeof(Decomp_Flag)*dimCount);
    }else
      distgrid->regDecomp = NULL;
    if (dg->indexflag){
      distgrid->indexflag = new ESMC_IndexFlag;
      memcpy(distgrid->indexflag, dg->indexflag, sizeof(ESMC_IndexFlag));
    }else
      distgrid->indexflag = NULL;
    distgrid->delayout = dg->delayout;
    distgrid->delayoutCreator = false;
    distgrid->vm = dg->vm;
    distgrid->localDeCountAux = dg->localDeCountAux;
  }
  
  if (delayout){
    // reset the delayoutCreator flag in the src DistGrid, because the newly
    // created DistGrid will now point to the same DELayout by reference
    // -> leave it up to ESMF automatic garbage collection to clean up the
    // DELayout when it is time
    dg->delayoutCreator = false;  // drop ownership of the referenced DELayout
  }
  
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc);
    return NULL;
  }catch(exception &x){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      x.what(), ESMC_CONTEXT, rc);
    return NULL;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, rc);
    return NULL;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------
  
  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::create
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
  Decomp_Flag *decompflag,                // (in)
  int decompflagCount,                    // (in)
  InterfaceInt *regDecompFirstExtra,      // (in)
  InterfaceInt *regDecompLastExtra,       // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
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
    distgrid = new DistGrid(vm);  // specific VM, or default if vm==NULL
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMCI::DistGrid.", ESMC_CONTEXT,rc);
     return ESMC_NULL_POINTER;
  }
  
  // check the input and get the information together to call construct()
  if (!present(minIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minIndex array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (!present(maxIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxIndex array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (minIndex->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- minIndex array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (maxIndex->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- maxIndex array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  int dimCount = minIndex->extent[0];
  if (maxIndex->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = VM::getCurrent(&localrc);  // get current VM for default
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }
  int petCount = vm->getNpets();
  int deCount=1;  // reset
  if (present(regDecomp)){
    if (regDecomp->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- regDecomp array must be of rank 1", ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
    if (regDecomp->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of regDecomp array must be of size dimCount",
        ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
    // regDecomp was provided -> determine number of DEs according to regDecomp
    for (int i=0; i<regDecomp->extent[0]; i++)
      deCount *= regDecomp->array[i]; 
  }else{
    // regDecomp was not provided -> set deCount = petCount for default
    deCount = petCount;
  }
  bool delayoutCreator = true; // default assume delayout will be created here
  if (delayout == ESMC_NULL_POINTER){
    // delayout was not provided -> create default DELayout with deCount DEs
    delayout = DELayout::create(&deCount, NULL, NULL, NULL, vm, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> get deCount
    deCount = delayout->getDeCount();
    delayoutCreator = false;  // indicate that delayout was not created here
  }
  int *dummy;
  bool regDecompDeleteFlag = false;  // reset
  if (!present(regDecomp)){
    // regDecomp was not provided -> create a temporary default regDecomp
    regDecompDeleteFlag = true;  // set
    dummy = new int[dimCount];
    // set default decomposition
    dummy[0] = deCount;
    for (int i=1; i<dimCount; i++)
      dummy[i] = 1;
    regDecomp = new InterfaceInt(dummy, 1, &dimCount);
  }
  if (regDecomp->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecomp array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  bool decompflagDeleteFlag = false; // reset
  if (decompflagCount==0){
    // decompflag was not provided -> set up default decompflag
    decompflagDeleteFlag = true; // set
    decompflagCount = dimCount;
    decompflag = new Decomp_Flag[dimCount];
    for (int i=0; i<dimCount; i++)
      decompflag[i] = DECOMP_BALANCED;
  }
  if (decompflagCount != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minIndex and maxIndex arrays",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  bool deLabelListDeleteFlag = false;  // reset
  if (!present(deLabelList)){
    // deLabelList was not provided -> create a temporary default deLabelList
    deLabelListDeleteFlag = true;  // set
    dummy = new int[deCount];
    // set default sequence
    for (int i=0; i<deCount; i++)
      dummy[i] = i;
    deLabelList = new InterfaceInt(dummy, 1, &deCount);
  }
  if (deLabelList->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (deLabelList->extent[0] < deCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- deLabelList array must provide deCount DE labels", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  for (int i=0; i<deCount; i++){
    if (deLabelList->array[i] < 0 || deLabelList->array[i] >= deCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- deLabelList array contains invalid DE labels", ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }
  bool regDecompFirstExtraDeleteFlag = false;  // reset
  if (!present(regDecompFirstExtra)){
    // regDecompFirstExtra was not provided -> create a temporary default
    regDecompFirstExtraDeleteFlag = true;  // set
    dummy = new int[dimCount];
    // set default
    for (int i=0; i<dimCount; i++)
      dummy[i] = 0;
    regDecompFirstExtra = new InterfaceInt(dummy, 1, &dimCount);
  }
  if (regDecompFirstExtra->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecompFirstExtra array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (regDecompFirstExtra->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- regDecompFirstExtra array must be of size dimCount", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  bool regDecompLastExtraDeleteFlag = false;  // reset
  if (!present(regDecompLastExtra)){
    // regDecompLastExtra was not provided -> create a temporary default
    regDecompLastExtraDeleteFlag = true;  // set
    dummy = new int[dimCount];
    // set default
    for (int i=0; i<dimCount; i++)
      dummy[i] = 0;
    regDecompLastExtra = new InterfaceInt(dummy, 1, &dimCount);
  }
  if (regDecompLastExtra->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecompLastExtra array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (regDecompLastExtra->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- regDecompLastExtra array must be of size dimCount", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  
  // setup temporary indexCountPDimPDe, indexListPDimPLocalDe and dimCongtigFlag
  // arrays for construct()
  const int localDeCount = delayout->getLocalDeCount();
  const int *deList = delayout->getDeList();
  int *indexCountPDimPDe = new int[dimCount*deCount];
  int **indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  int *contigFlagPDimPDe = new int[dimCount*deCount];
  int *minIndexPDimPDe = new int[dimCount*deCount];
  int *maxIndexPDimPDe = new int[dimCount*deCount];
  int deDivider = 1;  // reset
  for (int i=0; i<dimCount; i++){
    const int firstExtra = regDecompFirstExtra->array[i];
    const int lastExtra = regDecompLastExtra->array[i];
    const int dimLength = maxIndex->array[i] - minIndex->array[i] + 1
      - firstExtra - lastExtra;
    if (dimLength < 0){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- more extra elements specified than are available", ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
    const int chunkLength = dimLength/regDecomp->array[i];  // basic chunk size
    const int chunkRest = dimLength%regDecomp->array[i];    // left over points
    int de, decompChunk, extentIndex;
    switch (decompflag[i]){
      case DECOMP_BALANCED:
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          indexCountPDimPDe[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest)
            ++indexCountPDimPDe[extentIndex]; // distribute rest
          if (decompChunk == 0)
            indexCountPDimPDe[extentIndex] += firstExtra;
          if (decompChunk == regDecomp->array[i]-1)
            indexCountPDimPDe[extentIndex] += lastExtra;
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk < chunkRest) indexStart += decompChunk;
          else indexStart += chunkRest;
          if (decompChunk > 0) indexStart += firstExtra;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + indexCountPDimPDe[extentIndex] - 1;
          // fill indexListPDimPLocalDe
          if (deList[de] > -1){
            // de is local
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // block structure
              indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
            }
          }
          // flag contiguous dimension
          contigFlagPDimPDe[extentIndex] = 1;
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
          if (decompChunk == 0)
            indexCountPDimPDe[extentIndex] += firstExtra;
          if (decompChunk == regDecomp->array[i]-1)
            indexCountPDimPDe[extentIndex] += lastExtra;
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk > 0) indexStart += firstExtra;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + indexCountPDimPDe[extentIndex] - 1;
          // fill indexListPDimPLocalDe
          if (deList[de] > -1){
            // de is local
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // block structure
              indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
            }
          }
          // flag contiguous dimension
          contigFlagPDimPDe[extentIndex] = 1;
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
          if (decompChunk == 0)
            indexCountPDimPDe[extentIndex] += firstExtra;
          if (decompChunk == regDecomp->array[i]-1)
            indexCountPDimPDe[extentIndex] += lastExtra;
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk * chunkLength;
          if (decompChunk > 0) indexStart += chunkRest;
          if (decompChunk > 0) indexStart += firstExtra;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + indexCountPDimPDe[extentIndex] - 1;
          // fill indexListPDimPLocalDe
          if (deList[de] > -1){
            // de is local
            int localExtentIndex = deList[de]*dimCount+i;
            indexListPDimPLocalDe[localExtentIndex] =
              new int[indexCountPDimPDe[extentIndex]];
            // fill the indexListPDimPLocalDe for this dimension and local DE
            for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
              // block structure
              indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
            }
          }
          // flag contiguous dimension
          contigFlagPDimPDe[extentIndex] = 1;
        }
        break;
      case DECOMP_CYCLIC:
        if (firstExtra > 0 || lastExtra > 0){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
            "- extra elements not supported for DECOMP_CYCLIC dims",
            ESMC_CONTEXT, rc);
          distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
        for (int j=0; j<deCount; j++){
          de = deLabelList->array[j];
          extentIndex = de*dimCount+i;  // index into temp. arrays
          indexCountPDimPDe[extentIndex] = chunkLength;
          decompChunk = (j/deDivider)%regDecomp->array[i];
          if (decompChunk < chunkRest)
            ++indexCountPDimPDe[extentIndex]; // distribute rest
          // determine min and max
          int indexStart = minIndex->array[i] + decompChunk;
          minIndexPDimPDe[extentIndex] = indexStart;
          maxIndexPDimPDe[extentIndex] = indexStart
            + (indexCountPDimPDe[extentIndex] - 1) * regDecomp->array[i];
          // fill indexListPDimPLocalDe
          if (deList[de] > -1){
            // de is local
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
          // flag non-contiguous dimension
          contigFlagPDimPDe[extentIndex] = 0;
        }
        break;
      default:
        ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
          "- this decompflag is currently not implemented", ESMC_CONTEXT, rc);
        distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
        return ESMC_NULL_POINTER;
        break;
    }
    deDivider *= regDecomp->array[i];
  }
  // set up tileListPDe
  int *tileListPDe = new int[deCount];
  for (int i=0; i<deCount; i++)
    tileListPDe[i] = 1;

  
  // call into construct()
  localrc = distgrid->construct(dimCount, 1, tileListPDe,
    minIndex->array, maxIndex->array, minIndexPDimPDe, maxIndexPDimPDe,
    contigFlagPDimPDe, indexCountPDimPDe, indexListPDimPLocalDe,
    regDecomp->array, connectionList, decompflag, indexflag,
    delayout, delayoutCreator, vm);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)){
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
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
  if (regDecompFirstExtraDeleteFlag){
    delete [] regDecompFirstExtra->array;
    delete regDecompFirstExtra;
  }
  if (regDecompLastExtraDeleteFlag){
    delete [] regDecompLastExtra->array;
    delete regDecompLastExtra;
  }
  delete [] tileListPDe;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::create
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
    distgrid = new DistGrid(vm);  // specific VM, or default if vm==NULL
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMCI::DistGrid.", ESMC_CONTEXT,rc);
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call construct()
  if (!present(minIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minIndex array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (!present(maxIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxIndex array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (minIndex->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- minIndex array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (maxIndex->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- maxIndex array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  int dimCount = minIndex->extent[0];
  if (maxIndex->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = VM::getCurrent(&localrc);  // get current VM for default
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }
  int petCount = vm->getNpets();
  if (!present(deBlockList)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to deBlockList array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (deBlockList->dimCount != 3){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- deBlockList array must be of rank 3", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (deBlockList->extent[0] < dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- deBlockList array must provide dimCount elements in first dimension",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (deBlockList->extent[1] < 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- deBlockList array must provide 2 elements in second dimension",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  int deCount = deBlockList->extent[2]; // the 3rd dimension runs through DEs
  bool delayoutCreator = true; // default assume delayout will be created here
  if (delayout == ESMC_NULL_POINTER){
    // delayout was not provided -> create default DELayout with deCount DEs
    delayout = DELayout::create(&deCount, NULL, NULL, NULL, vm, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> get deCount
    delayoutCreator = false;  // indicate that delayout was not created here
    if (deCount != delayout->getDeCount()){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- deBlockList must provide deCount elements in third dimension",
        ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }
  int *dummy;
  bool deLabelListDeleteFlag = false;  // reset
  if (!present(deLabelList)){
    // deLabelList was not provided -> create a temporary default deLabelList
    deLabelListDeleteFlag = true;  // set
    dummy = new int[deCount];
    // set default sequence
    for (int i=0; i<deCount; i++)
      dummy[i] = i;
    deLabelList = new InterfaceInt(dummy, 1, &deCount);
  }
  if (deLabelList->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (deLabelList->extent[0] < deCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- deLabelList array must provide deCount DE labels", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  for (int i=0; i<deCount; i++){
    if (deLabelList->array[i] < 0 || deLabelList->array[i] >= deCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- deLabelList array contains invalid DE labels", ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
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
    int min = minIndex->array[i];
    int max = maxIndex->array[i];
    int de, extentIndex, deBlockIndexMin, deBlockIndexMax;
    for (int j=0; j<deCount; j++){
      de = deLabelList->array[j];
      extentIndex = de*dimCount+i;  // index into temp. arrays
      deBlockIndexMin = j*deBlockList->extent[0]*deBlockList->extent[1] + i;
      deBlockIndexMax = deBlockIndexMin + deBlockList->extent[0];
      // determine min and max
      minIndexPDimPDe[extentIndex] = deBlockList->array[deBlockIndexMin];
      maxIndexPDimPDe[extentIndex] = deBlockList->array[deBlockIndexMax];
      // check min and max
      if (maxIndexPDimPDe[extentIndex] < minIndexPDimPDe[extentIndex]){
        // zero elements case -> skip bounds checks
        indexCountPDimPDe[extentIndex] = 0;
      }else{
        // normal case -> do normal bounds checks
        if (minIndexPDimPDe[extentIndex] < min ||
          minIndexPDimPDe[extentIndex] > max){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "- deBlockList contains out-of-bounds elements", ESMC_CONTEXT, rc);
          distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
        if (maxIndexPDimPDe[extentIndex] < min ||
          maxIndexPDimPDe[extentIndex] > max){
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
            "- deBlockList contains out-of-bounds elements", ESMC_CONTEXT, rc);
          distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
        }
        // determine count
        indexCountPDimPDe[extentIndex] = maxIndexPDimPDe[extentIndex]
          - minIndexPDimPDe[extentIndex] + 1;
      }
      // fill indexListPDimPLocalDe
      if (deList[j] > -1){
        // de is local
        int localExtentIndex = deList[j]*dimCount+i;
        indexListPDimPLocalDe[localExtentIndex] =
          new int[indexCountPDimPDe[extentIndex]];
        for (int k=0; k<indexCountPDimPDe[extentIndex]; k++)
          indexListPDimPLocalDe[localExtentIndex][k] =
            deBlockList->array[deBlockIndexMin] + k;
      }
      // flag contiguous dimension
      contigFlagPDimPDe[extentIndex] = 1;
    }
  }
  // set up tileListPDe
  int *tileListPDe = new int[deCount];
  for (int i=0; i<deCount; i++)
    tileListPDe[i] = 1;

  // todo: check for overlapping deBlocks!!
  // call into construct()
  localrc = distgrid->construct(dimCount, 1, tileListPDe, 
    minIndex->array, maxIndex->array, minIndexPDimPDe, maxIndexPDimPDe,
    contigFlagPDimPDe, indexCountPDimPDe, indexListPDimPLocalDe, NULL,
    connectionList, NULL, indexflag,
    delayout, delayoutCreator, vm);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, rc)){
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
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
  delete [] tileListPDe;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return distgrid;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::create
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
  Decomp_Flag *decompflag,                // (in)
  int decompflagCount,                    // (in)
  InterfaceInt *regDecompFirstExtra,      // (in)
  InterfaceInt *regDecompLastExtra,       // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
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
      decompflagCount, regDecompFirstExtra, regDecompLastExtra, deLabelList,
      indexflag, connectionList, delayout, vm, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return distgrid;
  
  // return successfully
  //if (rc!=NULL) *rc = ESMF_SUCCESS; TODO: override ESMC_RC_NOT_IMPL
  
  return distgrid;
}
//-----------------------------------------------------------------------------

  
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::create()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::create
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
  Decomp_Flag *decompflag,                // (in)
  int decompflagCount1,                   // (in)
  int decompflagCount2,                   // (in)
  InterfaceInt *regDecompFirstExtra,      // (in)
  InterfaceInt *regDecompLastExtra,       // (in)
  InterfaceInt *deLabelList,              // (in)
  ESMC_IndexFlag *indexflag,              // (in)
  InterfaceInt *connectionList,           // (in)
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
    distgrid = new DistGrid(vm);  // specific VM, or default if vm==NULL
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new ESMCI::DistGrid.", ESMC_CONTEXT,rc);
     return ESMC_NULL_POINTER;
  }

  // check the input and get the information together to call construct()
  if (!present(minIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to minIndex array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (!present(maxIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to maxIndex array", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (minIndex->dimCount != 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- minIndex array must be of rank 2", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (maxIndex->dimCount != 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- maxIndex array must be of rank 2", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  int dimCount = minIndex->extent[0];
  if (maxIndex->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch in dimCount", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  int tileCount = minIndex->extent[1];
  if (maxIndex->extent[1] != tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- minIndex and maxIndex array mismatch in tileCount", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (vm == ESMC_NULL_POINTER){
    // vm was not provided -> get the current VM
    vm = VM::getCurrent(&localrc);  // get current VM for default
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }
  int petCount = vm->getNpets();
  int deCount=0;  // reset
  int *deCountPTile;
  if (present(regDecomp)){
    if (regDecomp->dimCount != 2){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- regDecomp array must be of rank 2", ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
    if (regDecomp->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of regDecomp array must be of size dimCount",
        ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
    if (regDecomp->extent[1] != tileCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- 2nd dimension of regDecomp array must be of size tileCount",
        ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
    // regDecomp was provided -> determine number of DEs according to regDecomp
    deCountPTile = new int[tileCount];
    for (int i=0; i<tileCount; i++){
      int localProduct = 1; // reset
      for (int j=0; j<dimCount; j++)
        localProduct *= regDecomp->array[i*dimCount+j];
      deCountPTile[i] = localProduct;
      deCount += localProduct;
    }
  }else{
    // regDecomp was not provided -> set deCount = tileCount for default
    deCountPTile = new int[tileCount];
    for (int i=0; i<tileCount; i++)
      deCountPTile[i] = 1;
    deCount = tileCount;
  }
  bool delayoutCreator = true; // default assume delayout will be created here
  if (delayout == ESMC_NULL_POINTER){
    // delayout was not provided -> create default DELayout with deCount DEs
    delayout = DELayout::create(&deCount, NULL, NULL, NULL, vm, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)){
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }else{
    // delayout was provided -> get deCount
    deCount = delayout->getDeCount();
    delayoutCreator = false;  // indicate that delayout was not created here
  }
  int *dummy, dummyLen[2];
  bool regDecompDeleteFlag = false;  // reset
  if (!present(regDecomp)){
    // regDecomp was not provided -> create a temporary default regDecomp
    regDecompDeleteFlag = true;  // set
    dummy = new int[dimCount*tileCount];
    // set default decomposition
    for (int i=0; i<dimCount*tileCount; i++)
      dummy[i] = 1;
    dummyLen[0] = dimCount;
    dummyLen[1] = tileCount;
    regDecomp = new InterfaceInt(dummy, 2, dummyLen);
  }
  if (regDecomp->dimCount != 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecomp array must be of rank 2", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  bool decompflagDeleteFlag = false; // reset
  if (decompflagCount1==0 || decompflagCount2==0){
    // decompflag was not provided -> set up default decompflag
    decompflagDeleteFlag = true; // set
    decompflagCount1 = dimCount;
    decompflagCount2 = tileCount;
    decompflag = new Decomp_Flag[dimCount*tileCount];
    for (int i=0; i<dimCount*tileCount; i++)
      decompflag[i] = DECOMP_BALANCED;
  }
  if (decompflagCount1 != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minIndex and maxIndex arrays",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (decompflagCount2 != tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- decompflag array mismatches minIndex and maxIndex arrays",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  bool deLabelListDeleteFlag = false;  // reset
  if (!present(deLabelList)){
    // deLabelList was not provided -> create a temporary default deLabelList
    deLabelListDeleteFlag = true;  // set
    dummy = new int[deCount];
    // set default sequence
    for (int i=0; i<deCount; i++)
      dummy[i] = i;
    deLabelList = new InterfaceInt(dummy, 1, &deCount);
  }
  if (deLabelList->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must be of rank 1", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (deLabelList->extent[0] < deCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- deLabelList array must provide deCount DE labels", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  for (int i=0; i<deCount; i++){
    if (deLabelList->array[i] < 0 || deLabelList->array[i] >= deCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- deLabelList array contains invalid DE labels", ESMC_CONTEXT, rc);
      distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }
  bool regDecompFirstExtraDeleteFlag = false;  // reset
  if (!present(regDecompFirstExtra)){
    // regDecompFirstExtra was not provided -> create a temporary default
    regDecompFirstExtraDeleteFlag = true;  // set
    dummy = new int[dimCount*tileCount];
    // set default
    for (int i=0; i<dimCount*tileCount; i++)
      dummy[i] = 0;
    dummyLen[0] = dimCount;
    dummyLen[1] = tileCount;
    regDecompFirstExtra = new InterfaceInt(dummy, 2, dummyLen);
  }
  if (regDecompFirstExtra->dimCount != 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecompFirstExtra array must be of rank 2", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (regDecompFirstExtra->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dim of regDecompFirstExtra array must be of size dimCount",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (regDecompFirstExtra->extent[1] != tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- 2nd dim of regDecompFirstExtra array must be of size tileCount",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  bool regDecompLastExtraDeleteFlag = false;  // reset
  if (!present(regDecompLastExtra)){
    // regDecompLastExtra was not provided -> create a temporary default
    regDecompLastExtraDeleteFlag = true;  // set
    dummy = new int[dimCount*tileCount];
    // set default
    for (int i=0; i<dimCount*tileCount; i++)
      dummy[i] = 0;
    dummyLen[0] = dimCount;
    dummyLen[1] = tileCount;
    regDecompLastExtra = new InterfaceInt(dummy, 2, dummyLen);
  }
  if (regDecompLastExtra->dimCount != 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- regDecompLastExtra array must be of rank 2", ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (regDecompLastExtra->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dim of regDecompLastExtra array must be of size dimCount",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  if (regDecompLastExtra->extent[1] != tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- 2nd dim of regDecompLastExtra array must be of size tileCount",
      ESMC_CONTEXT, rc);
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
    
  // setup temporary indexCountPDimPDe, indexListPDimPLocalDe and dimCongtigFlag
  // arrays for construct()
  const int localDeCount = delayout->getLocalDeCount();
  const int *deList = delayout->getDeList();
  int *indexCountPDimPDe = new int[dimCount*deCount];
  int **indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  int *contigFlagPDimPDe = new int[dimCount*deCount];
  int *minIndexPDimPDe = new int[dimCount*deCount];
  int *maxIndexPDimPDe = new int[dimCount*deCount];
  
  // the following differs from the single tile case in that there is an
  // extra outer loop over tiles. The indexCountPDimPDe and
  // indexListPDimPLocalDe arrays are on DE basis, independent of tiles.

  int deTileStart = 0;  // reset  
  for (int tile=0; tile<tileCount; tile++){
    int deDivider = 1;  // reset
    for (int ii=0; ii<dimCount; ii++){
      const int i = tile*dimCount + ii;  // work in the current tile
      const int firstExtra = regDecompFirstExtra->array[i];
      const int lastExtra = regDecompLastExtra->array[i];
      const int dimLength = maxIndex->array[i] - minIndex->array[i] + 1
        - firstExtra - lastExtra;
      if (dimLength < 0){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- more extra elements specified than are available", ESMC_CONTEXT,
          rc);
        distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
        return ESMC_NULL_POINTER;
      }
      const int chunkLength = dimLength/regDecomp->array[i]; // basic chunk size
      const int chunkRest = dimLength%regDecomp->array[i];   // left over points
      int de, decompChunk, extentIndex;
      switch (decompflag[i]){
        case DECOMP_BALANCED:
          for (int jj=0; jj<deCountPTile[tile]; jj++){
            int j = deTileStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest)
              ++indexCountPDimPDe[extentIndex]; // distribute rest
            if (decompChunk == 0)
              indexCountPDimPDe[extentIndex] += firstExtra;
            if (decompChunk == regDecomp->array[i]-1)
              indexCountPDimPDe[extentIndex] += lastExtra;
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk < chunkRest) indexStart += decompChunk;
            else indexStart += chunkRest;
            if (decompChunk > 0) indexStart += firstExtra;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart
              + indexCountPDimPDe[extentIndex] - 1;
            // fill indexListPDimPLocalDe
            if (deList[de] > -1){
              // de is local
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
            // flag contiguous dimension
            contigFlagPDimPDe[extentIndex] = 1;
          }
          break;
        case DECOMP_RESTLAST:
          for (int jj=0; jj<deCountPTile[tile]; jj++){
            int j = deTileStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == regDecomp->array[i]-1) 
              indexCountPDimPDe[extentIndex] += chunkRest; // add rest
            if (decompChunk == 0)
              indexCountPDimPDe[extentIndex] += firstExtra;
            if (decompChunk == regDecomp->array[i]-1)
              indexCountPDimPDe[extentIndex] += lastExtra;
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk > 0) indexStart += firstExtra;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart
              + indexCountPDimPDe[extentIndex] - 1;
            // fill indexListPDimPLocalDe
            if (deList[de] > -1){
              // de is local
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
            // flag contiguous dimension
            contigFlagPDimPDe[extentIndex] = 1;
          }
          break;
        case DECOMP_RESTFIRST:
          for (int jj=0; jj<deCountPTile[tile]; jj++){
            int j = deTileStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk == 0) 
              indexCountPDimPDe[extentIndex] += chunkRest; // add rest
            if (decompChunk == 0)
              indexCountPDimPDe[extentIndex] += firstExtra;
            if (decompChunk == regDecomp->array[i]-1)
              indexCountPDimPDe[extentIndex] += lastExtra;
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            if (decompChunk > 0) indexStart += chunkRest;
            if (decompChunk > 0) indexStart += firstExtra;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart
              + indexCountPDimPDe[extentIndex] - 1;
            // fill indexListPDimPLocalDe
            if (deList[de] > -1){
              // de is local
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // block structure
                indexListPDimPLocalDe[localExtentIndex][k] = indexStart + k;
              }
            }
            // flag contiguous dimension
            contigFlagPDimPDe[extentIndex] = 1;
          }
          break;
        case DECOMP_CYCLIC:
          if (firstExtra > 0 || lastExtra > 0){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
              "- extra elements not supported for DECOMP_CYCLIC dims",
              ESMC_CONTEXT, rc);
            distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
            return ESMC_NULL_POINTER;
          }
          for (int jj=0; jj<deCountPTile[tile]; jj++){
            int j = deTileStart + jj;
            de = deLabelList->array[j];
            extentIndex = de*dimCount+ii;  // index into temp. arrays
            indexCountPDimPDe[extentIndex] = chunkLength;
            decompChunk = (jj/deDivider)%regDecomp->array[i];
            if (decompChunk < chunkRest)
              ++indexCountPDimPDe[extentIndex]; // distribute rest
            // determine min and max
            int indexStart = minIndex->array[i] + decompChunk * chunkLength;
            minIndexPDimPDe[extentIndex] = indexStart;
            maxIndexPDimPDe[extentIndex] = indexStart +
              + (indexCountPDimPDe[extentIndex] - 1) * regDecomp->array[i];
            // fill indexListPDimPLocalDe
            if (deList[de] > -1){
              // de is local
              int localExtentIndex = deList[de]*dimCount+ii;
              indexListPDimPLocalDe[localExtentIndex] =
                new int[indexCountPDimPDe[extentIndex]];
              // fill the indexListPDimPLocalDe for this dimension and local DE
              for (int k=0; k<indexCountPDimPDe[extentIndex]; k++){
                // cyclic
                indexListPDimPLocalDe[localExtentIndex][k] = 
                  indexStart + k * regDecomp->array[i];
              }
            }
            // flag non-contiguous dimension
            contigFlagPDimPDe[extentIndex] = 0;
          }
          break;
        default:
          ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
            "- this decompflag is currently not implemented",
            ESMC_CONTEXT, rc);
          distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
          return ESMC_NULL_POINTER;
          break;
      }
      deDivider *= regDecomp->array[i];
    } // i-loop
    deTileStart += deCountPTile[tile];
  } // tile-loop
  // set up tileListPDe
  deTileStart = 0;  // reset  
  int *tileListPDe = new int[deCount];
  for (int tile=0; tile<tileCount; tile++){
    for (int jj=0; jj<deCountPTile[tile]; jj++){
      int j = deTileStart + jj;
      int de = deLabelList->array[j];
      tileListPDe[de] = tile + 1;  // tile ids are basis 1
    }
    deTileStart += deCountPTile[tile];
  }

  // call into construct()
  localrc = distgrid->construct(dimCount, tileCount, tileListPDe,
    minIndex->array, maxIndex->array, minIndexPDimPDe, maxIndexPDimPDe,
    contigFlagPDimPDe, indexCountPDimPDe, indexListPDimPLocalDe,
    regDecomp->array, connectionList, decompflag, indexflag,
    delayout, delayoutCreator, vm);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)){
    distgrid->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }
  
  // garbage collection
  delete [] contigFlagPDimPDe;
  delete [] minIndexPDimPDe;
  delete [] maxIndexPDimPDe;
  delete [] deCountPTile;
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
  if (regDecompFirstExtraDeleteFlag){
    delete [] regDecompFirstExtra->array;
    delete regDecompFirstExtra;
  }
  if (regDecompLastExtraDeleteFlag){
    delete [] regDecompLastExtra->array;
    delete regDecompLastExtra;
  }
  delete [] tileListPDe;
  
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
  DistGrid **distgrid,          // in - DistGrid to destroy
  bool noGarbage){              // in - remove from garbage collection
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", ESMC_CONTEXT, &rc);
    return rc;
  }

  try{
    // destruct DistGrid object
    (*distgrid)->destruct(true, noGarbage);
    // mark as invalid object
    (*distgrid)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(*distgrid); // remove object from garbage collection
    delete (*distgrid);      // completely delete the object, free heap
  }

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
  int tileCountArg,                    // (in)
  int *tileListPDeArg,                 // (in)
  int *minIndexArg,                     // (in)
  int *maxIndexArg,                     // (in)
  int *minIndexPDimPDeArg,              // (in)
  int *maxIndexPDimPDeArg,              // (in)
  int *contigFlagPDimPDeArg,            // (in)
  int *indexCountPDimPDeArg,            // (in)
  int **indexListPDimPLocalDeArg,       // (in)
  int *regDecompArg,                    // (in)
  InterfaceInt *connectionListArg,      // (in)
  Decomp_Flag const *decompflagArg,     // (in)
  ESMC_IndexFlag *indexflagArg,         // (in)
  DELayout *delayoutArg,                // (in) DELayout
  bool delayoutCreatorArg,              // (in)
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
  
  // simple variables kept b/c possibly needed in DistGridCreate() from DG
  if (decompflagArg){
    decompflag = new Decomp_Flag[dimCountArg*tileCountArg];
    memcpy(decompflag, decompflagArg, sizeof(Decomp_Flag)
      *dimCountArg*tileCountArg);
  }else
    decompflag = NULL;
  if (indexflagArg){
    indexflag = new ESMC_IndexFlag; // must store in local storage
    *indexflag = *indexflagArg;     // copy the value
  }else
    indexflag = NULL;

  // fill in the DistGrid object
  dimCount = dimCountArg;
  tileCount = tileCountArg;
  if (present(connectionListArg)){
    // connectionList was provided
    int elementSize = 2*dimCount+2;
    if (connectionListArg->dimCount != 2){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- connectionListArg array must be of rank 2", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (connectionListArg->extent[0] != elementSize){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of connectionListArg array must be of size "
        "(2*dimCount+2)", ESMC_CONTEXT, &rc);
      return rc;
    }
    // fill in the connectionList member and check tileA & tileB entries
    connectionCount = connectionListArg->extent[1];
    connectionList = new int*[connectionCount];
    for (int i=0; i<connectionCount; i++){
      connectionList[i] = new int[elementSize];
      memcpy(connectionList[i],
        &(connectionListArg->array[elementSize*i]), sizeof(int)*elementSize);
      if (connectionList[i][0] < 1 || connectionList[i][0] > tileCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- tileA in connectionList element lies outside [1,tileCount]",
          ESMC_CONTEXT, &rc);
        return rc;
      }
      if (connectionList[i][1] < 1 || connectionList[i][1] > tileCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
          "- tileB in connectionList element lies outside [1,tileCount]",
          ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  }else{
    // connectionList was not provided -> nullify
    connectionCount = 0;
    connectionList = NULL;
  }
  delayout = delayoutArg;
  delayoutCreator = delayoutCreatorArg;
  vm = vmArg;
  // fill in the rest
  minIndexPDimPTile = new int[dimCount*tileCount];
  memcpy(minIndexPDimPTile, minIndexArg, sizeof(int)*dimCount*tileCount);
  maxIndexPDimPTile = new int[dimCount*tileCount];
  memcpy(maxIndexPDimPTile, maxIndexArg, sizeof(int)*dimCount*tileCount);
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
  const int *localDeToDeMap = delayout->getLocalDeToDeMap();
  indexListPDimPLocalDe = new int*[dimCount*localDeCount];
  for (int i=0; i<localDeCount; i++){
    int de = localDeToDeMap[i];
    for (int k=0; k<dimCount; k++){
      indexListPDimPLocalDe[i*dimCount+k] =
        new int[indexCountPDimPDe[de*dimCount+k]];
      memcpy(indexListPDimPLocalDe[i*dimCount+k],
        indexListPDimPLocalDeArg[i*dimCount+k],
        sizeof(int)*indexCountPDimPDe[de*dimCount+k]);
    }
  }
  // determine the elementCountPTile
  elementCountPTile = new int[tileCount];
  for (int i=0; i<tileCount; i++){
    elementCountPTile[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      elementCountPTile[i] *=
        maxIndexPDimPTile[i*dimCount+j] - minIndexPDimPTile[i*dimCount+j] + 1;
  }
  tileListPDe = new int[deCount];
  memcpy(tileListPDe, tileListPDeArg, sizeof(int)*deCount);
  elementCountPDe = new int[deCount];
  for (int i=0; i<deCount; i++){
    elementCountPDe[i] = 1;  // reset
    for (int j=0; j<dimCount; j++)
      elementCountPDe[i] *= indexCountPDimPDe[i*dimCount+j];
    // mark in tileListPDe DEs that have no elements as not being on any tile
    if (elementCountPDe[i]==0) tileListPDe[i]=0;
  }
  // complete sequence index collocation by default
  diffCollocationCount = 1; // collocate all dimensions
  collocationPDim = new int[dimCount];
  collocationTable = new int[dimCount];
  for (int i=0; i<dimCount; i++){
    collocationPDim[i]=1;
    collocationTable[i]=-1;
  }
  collocationTable[0]=1;
  // no arbitrary sequence indices by default
  arbSeqIndexListPCollPLocalDe = new int**[diffCollocationCount];
  elementCountPCollPLocalDe = new int*[diffCollocationCount];
  for (int i=0; i<diffCollocationCount; i++){
    arbSeqIndexListPCollPLocalDe[i] = new int*[localDeCount];
    elementCountPCollPLocalDe[i] = new int[localDeCount];
    for (int j=0; j<localDeCount; j++){
      arbSeqIndexListPCollPLocalDe[i][j] = NULL;
      elementCountPCollPLocalDe[i][j] = elementCountPDe[localDeToDeMap[i]];
    }
  }
  if (regDecompArg){
    regDecomp = new int[dimCount*tileCount];
    memcpy(regDecomp, regDecompArg, sizeof(int)*dimCount*tileCount);
  }else
    regDecomp = NULL;
  
  localDeCountAux = localDeCount; // TODO: auxilary for garb until ref. counting
  
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
int DistGrid::destruct(bool followCreator, bool noGarbage){
//
// TODO: The followCreator flag is only needed until we have reference counting
// TODO: For now followCreator, which by default is true, will be coming in as
// TODO: false when calling through the native destructor. This prevents
// TODO: sequence problems during automatic garbage collection unitl reference
// TODO: counting comes in to solve this problem in the final manner.
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

//fprintf(stderr, "DistGrid::destruct\n");

  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    // garbage collection
    delete [] indexCountPDimPDe;
    delete [] minIndexPDimPTile;
    delete [] maxIndexPDimPTile;
    delete [] minIndexPDimPDe;
    delete [] maxIndexPDimPDe;
    delete [] elementCountPTile;
    delete [] tileListPDe;
    delete [] elementCountPDe;
    delete [] contigFlagPDimPDe;
    
    if (decompflag)
      delete [] decompflag;
    if (indexflag)
      delete indexflag;
    
//    int localDeCount = delayout->getLocalDeCount();
    int localDeCount = localDeCountAux; // TODO: delayout may be gone already!
// TODO: replace the above line with the line before once ref. counting implem.
    
    for (int i=0; i<dimCount*localDeCount; i++)
      delete [] indexListPDimPLocalDe[i];
    delete [] indexListPDimPLocalDe;
    for (int i=0; i<connectionCount; i++)
      delete [] connectionList[i];
    if (connectionList)
      delete [] connectionList;
    for (int i=0; i<diffCollocationCount; i++){
      for (int j=0; j<localDeCount; j++)
        if (arbSeqIndexListPCollPLocalDe[i][j])
          delete [] arbSeqIndexListPCollPLocalDe[i][j];
      delete [] arbSeqIndexListPCollPLocalDe[i];
      delete [] elementCountPCollPLocalDe[i];
    }
    delete [] arbSeqIndexListPCollPLocalDe;
    delete [] elementCountPCollPLocalDe;
    delete [] collocationPDim;
    delete [] collocationTable;
    if (regDecomp)
      delete [] regDecomp;
    
    if (delayoutCreator && followCreator){
      localrc = DELayout::destroy(&delayout, noGarbage); 
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc)) return rc;
    }
  }

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
#define ESMC_METHOD "ESMCI::DistGrid::fillSeqIndexList()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::fillSeqIndexList
//
// !INTERFACE:
//
int DistGrid::fillSeqIndexList(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *seqIndexList,   // in
  int localDe,                  // in  - local DE = {0, ..., localDeCount-1}
  int collocation               // in  -
  )const{
//
// !DESCRIPTION:
//    Fill the seqIndexList argument. Providing this InterfaceInt based 
//    method is required for efficient filling of arrays that come through
//    the Fortran API, without having to do an extra copy. It can also leveraged
//    by the overloaded vector<int> based interface.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  if (present(seqIndexList)){
    // seqIndexList provided -> error checking
    if ((seqIndexList)->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- seqIndexList array must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    int i;
    for (i=0; i<diffCollocationCount; i++)
      if (collocationTable[i]==collocation) break;
    if (i==diffCollocationCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
        "- specified collocation not valid", ESMC_CONTEXT, &rc);
      return rc;
    }
    int collIndex = i;
    // check for arbitrary sequence indices
    const int *arbSeqIndexList =
      getArbSeqIndexList(localDe, collocation, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
    if (arbSeqIndexList){
      // arbitrary seq indices -> fill seqIndexList with arbSeqIndexList
      if ((seqIndexList)->extent[0] <
        elementCountPCollPLocalDe[collIndex][localDe]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of seqIndexList array insufficiently sized",
          ESMC_CONTEXT, &rc);
        return rc;
      }
      memcpy((seqIndexList)->array, arbSeqIndexList,
        sizeof(int) * elementCountPCollPLocalDe[collIndex][localDe]);
    }else{
      // default seq indices -> generate on the fly and fill in
      if ((seqIndexList)->extent[0] <
        (getElementCountPDe())[delayout->getLocalDeToDeMap()[localDe]]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
          "- 1st dimension of seqIndexList array insufficiently sized",
          ESMC_CONTEXT, &rc);
        return rc;
      }
      // TODO: must consider collocation subspace here!!!
      // TODO: use MultiDimIndexLoop class for the following multi-dim loop
      int *ii = new int[dimCount];     // index tuple basis 0
      const int *iiEnd = getIndexCountPDimPDe() + dimCount *
        delayout->getLocalDeToDeMap()[localDe];
      // reset counters
      int index = 0;
      for (int j=0; j<dimCount; j++)
        ii[j] = 0;  // reset
      // loop over all elements in exclusive region for localDe
      while(ii[dimCount-1] < iiEnd[dimCount-1]){
        (seqIndexList)->array[index] =
          getSequenceIndexLocalDe(localDe, ii);
        ++index;
        // multi-dim index increment
        ++ii[0];
        for (int j=0; j<dimCount-1; j++){
          if (ii[j] == iiEnd[j]){
            ii[j] = 0;  // reset
            ++ii[j+1];
          }
        }
      }
      delete [] ii;
    }
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::fillSeqIndexList()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::fillSeqIndexList
//
// !INTERFACE:
//
int DistGrid::fillSeqIndexList(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  vector<int> &seqIndexList,    // in
  int localDe,                  // in  - local DE = {0, ..., localDeCount-1}
  int collocation               // in  -
  )const{
//
// !DESCRIPTION:
//    Fill the seqIndexList argument. If the size of seqIndexList does not
//    match it will automatically resized by this method.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  int i;
  for (i=0; i<diffCollocationCount; i++)
    if (collocationTable[i]==collocation) break;
  if (i==diffCollocationCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
      "- specified collocation not valid", ESMC_CONTEXT, &rc);
    return rc;
  }
  int collIndex = i;

  int elementCount = elementCountPCollPLocalDe[collIndex][localDe];
  
  if (seqIndexList.size() != elementCount)
    seqIndexList.resize(elementCount);
  
  InterfaceInt *seqIndexListAux = new InterfaceInt(seqIndexList);
  localrc = fillSeqIndexList(seqIndexListAux, localDe, collocation);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  delete seqIndexListAux;
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
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
  VMK::commhandle **commh,          // inout -
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
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
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
      if (*commh == NULL) *commh = new VMK::commhandle;
      localrc = vm->recv(indexList,
        sizeof(int)*indexCountPDimPDe[de*dimCount+dim-1], srcPet, commh);
      if (localrc){
        char *message = new char[160];
        sprintf(message, "VMKernel/MPI error #%d\n", localrc);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, message, ESMC_CONTEXT,
          &rc);
        delete [] message;
        return rc;
      }
    }else{
      // this DE _is_ local -> look up indexList locally
      const int *localIndexList =
        getIndexListPDimPLocalDe(deList[de], dim, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      memcpy(indexList, localIndexList, sizeof(int)*
        indexCountPDimPDe[de*dimCount+dim-1]);
    }
  }else{
    if (deList[de] != -1){
      // this DE _is_ local -> send indexList to rootPet
      const int *localIndexList =
        getIndexListPDimPLocalDe(deList[de], dim, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      if (*commh == NULL) *commh = new VMK::commhandle;
      localrc = vm->send(localIndexList,
        sizeof(int)*indexCountPDimPDe[de*dimCount+dim-1], rootPet, commh);
      if (localrc){
        char *message = new char[160];
        sprintf(message, "VMKernel/MPI error #%d\n", localrc);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          message, ESMC_CONTEXT, &rc);
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
// match, print and validation class methods
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::match()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::match
//
// !INTERFACE:
DistGridMatch_Flag DistGrid::match(
//
// !RETURN VALUE:
//    bool according to match
//
// !ARGUMENTS:
//
  DistGrid *distgrid1,                    // in
  DistGrid *distgrid2,                    // in
  int *rc                                 // (out) return code
  ){
//
// !DESCRIPTION:
//    Determine if distgrid1 and distgrid2 match.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // initialize return value
  DistGridMatch_Flag matchResult = DISTGRIDMATCH_INVALID;
  
  // return with errors for NULL pointer
  if (distgrid1 == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", ESMC_CONTEXT, rc);
    return matchResult;
  }
  if (distgrid2 == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", ESMC_CONTEXT, rc);
    return matchResult;
  }
  
  // check if DistGrid pointers are identical
  if (distgrid1 == distgrid2){
    // pointers are identical -> nothing more to check
    matchResult = DISTGRIDMATCH_ALIAS;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  
  // go through all members of DistGrid and compare
  int dimCount1 = distgrid1->dimCount;
  int dimCount2 = distgrid2->dimCount;
  if (dimCount1 != dimCount2){
    matchResult = DISTGRIDMATCH_NONE;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  int tileCount1 = distgrid1->tileCount;
  int tileCount2 = distgrid2->tileCount;
  if (tileCount1 != tileCount2){
    matchResult = DISTGRIDMATCH_NONE;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  int deCount1 = distgrid1->delayout->getDeCount();
  int deCount2 = distgrid2->delayout->getDeCount();
  if (deCount1 != deCount2){
    matchResult = DISTGRIDMATCH_NONE;
    if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
    return matchResult;
  }
  int *int1 = distgrid1->minIndexPDimPTile;
  int *int2 = distgrid2->minIndexPDimPTile;
  for (int i=0; i<dimCount1*tileCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->maxIndexPDimPTile;
  int2 = distgrid2->maxIndexPDimPTile;
  for (int i=0; i<dimCount1*tileCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->elementCountPTile;
  int2 = distgrid2->elementCountPTile;
  for (int i=0; i<tileCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->minIndexPDimPDe;
  int2 = distgrid2->minIndexPDimPDe;
  for (int i=0; i<dimCount1*deCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->maxIndexPDimPDe;
  int2 = distgrid2->maxIndexPDimPDe;
  for (int i=0; i<dimCount1*deCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->elementCountPDe;
  int2 = distgrid2->elementCountPDe;
  for (int i=0; i<deCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->tileListPDe;
  int2 = distgrid2->tileListPDe;
  for (int i=0; i<deCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  int1 = distgrid1->contigFlagPDimPDe;
  int2 = distgrid2->contigFlagPDimPDe;
  for (int i=0; i<dimCount1*deCount1; i++){
    if (int1[i] != int2[i]){
      matchResult = DISTGRIDMATCH_NONE;
      if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
      return matchResult;
    }
  }
  
  // return successfully indicating match
  matchResult = DISTGRIDMATCH_EXACT;
  if (rc!=NULL) *rc = ESMF_SUCCESS; // bail out successfully
  return matchResult;
}
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DistGrid", ESMC_CONTEXT, &rc);
    return rc;
  }

  // print info about the DistGrid object
  printf("--- ESMCI::DistGrid::print start ---\n");
  printf("dimCount = %d\n", dimCount);
  printf("tileCount = %d\n", tileCount);
  printf("elementCountPTile: ");
  for (int i=0; i<tileCount; i++)
    printf("%d ", elementCountPTile[i]);
  printf("\n");
  printf("regDecomp = %s\n", (regDecomp)?"YES":"NO");
  printf("tileListPDe: ");
  int deCount = delayout->getDeCount();
  for (int i=0; i<deCount; i++)
    printf("%d ", tileListPDe[i]);
  printf("\n");
  printf("elementCountPDe: ");
  for (int i=0; i<deCount; i++)
    printf("%d ", elementCountPDe[i]);
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
  const int *localDeToDeMap = delayout->getLocalDeToDeMap();
  for (int i=0; i<localDeCount; i++){
    printf(" for localDE %d - DE %d: ", i, localDeToDeMap[i]);
    for (int j=0; j<dimCount; j++){
      printf(" (");
      for (int k=0; k<indexCountPDimPDe[localDeToDeMap[i]*dimCount+j]; k++){
        if (k!=0) printf(", ");
        printf("%d", indexListPDimPLocalDe[i*dimCount+j][k]);
      }
      printf(") /");
    }
    printf("\n");
  }
  printf("diffCollocationCount = %d\n", diffCollocationCount);
  printf("collocationPDim:\t");
  for (int i=0; i<dimCount-1; i++)
    printf("%d / ", collocationPDim[i]);
  printf("%d\n", collocationPDim[dimCount-1]);
  printf("arbSeqIndexListPCollPLocalDe:\n");
  for (int i=0; i<diffCollocationCount; i++){
    for (int j=0; j<localDeCount; j++){
      printf(" for collocation %d, localDE %d - DE %d - "
        " elementCountPCollPLocalDe %d: ", collocationTable[i], j,
        localDeToDeMap[j], elementCountPCollPLocalDe[i][j]);
      if (arbSeqIndexListPCollPLocalDe[i][j]){
        printf("(");
        for (int k=0; k<elementCountPCollPLocalDe[i][j]; k++){
          if (k!=0) printf(", ");
          printf("%d", arbSeqIndexListPCollPLocalDe[i][j][k]);
        }
        printf(")\n");
      }else
        printf(" default\n");
    }
  }
      
  printf("connectionCount = %d\n", connectionCount);
  printf("~ lower class' values ~\n");
  printf("deCount = %d\n", deCount);
  if (vm==NULL){
    printf("Member on this PET appears to be a proxy member.\n");
    printf("CurrentVM: localPet = %d\n", VM::getCurrent()->getLocalPet());
    printf("CurrentVM: petCount = %d\n", VM::getCurrent()->getPetCount());
  }else{
    printf("Member on this PET appears to be an actual member.\n");
    printf("DistGrid-VM: localPet = %d, CurrentVM: localPet = %d\n", 
      vm->getLocalPet(), VM::getCurrent()->getLocalPet());
    printf("DistGrid-VM: petCount = %d, CurrentVM: petCount = %d\n", 
      vm->getPetCount(), VM::getCurrent()->getPetCount());
  }
  printf("--- ESMCI::DistGrid::print end ---\n");
  
#if 0
  printf("--- ESMCI::DistGrid::print - Topology test start ---\n");
  int lrc;
  int indexTuple[2];
  int depth=4;
  int seqIndex;
  indexTuple[0] = 0; indexTuple[1] = 0;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 0; indexTuple[1] = 1;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 1; indexTuple[1] = 1;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 1; indexTuple[1] = 0;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 11; indexTuple[1] = 1;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 1; indexTuple[1] = 5;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 1; indexTuple[1] = 6;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 4; indexTuple[1] = 6;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 10; indexTuple[1] = 5;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 10; indexTuple[1] = 6;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  indexTuple[0] = 21; indexTuple[1] = 5;
  seqIndex = getSequenceIndexTile(1, indexTuple, depth, &lrc);
  printf("indexTuple = (%d, %d), sequenceIndex = %d, lrc = %d\n",
    indexTuple[0], indexTuple[1], seqIndex, lrc);
  printf("--- ESMCI::DistGrid::print - Topology test end ---\n");
#endif
  
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
       " - 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
    return rc;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// is()
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::isLocalDeOnEdgeL()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::isLocalDeOnEdgeL
//
// !INTERFACE:
bool DistGrid::isLocalDeOnEdgeL(
//
// !RETURN VALUE:
//    bool, true if local DE is on indicated edge, false otherwise
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  int dim,                          // in  - dim  = {1, ..., dimCount}
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Determine if local DE is on lower edge along dimension dim.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", ESMC_CONTEXT, rc);
    return false;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified dim out of bounds", ESMC_CONTEXT, rc);
    return false;
  }
  
  // determine which tile localDe is located on
  int de = delayout->getLocalDeToDeMap()[localDe];
  bool onEdge = true;            // assume local De is on edge
  if (elementCountPDe[de]){
    // local De is associated with elements
    // prepare localDe relative index tuple of neighbor index to check
    vector<int> localDeIndexTupleVec(dimCount);
    int *localDeIndexTuple = &(localDeIndexTupleVec[0]);
    vector<int> sizes;
    for (int i=0; i<dimCount; i++)
      sizes.push_back(indexCountPDimPDe[de*dimCount+i]);
    MultiDimIndexLoop multiDimIndexLoop(sizes);
    multiDimIndexLoop.setSkipDim(dim-1);  // next() to skip dim
    while(multiDimIndexLoop.isWithin()){
      // look at the entire interface spanned by all dimensions except dim
      int const *indexTuple = multiDimIndexLoop.getIndexTuple();
      for (int i=0; i<dimCount; i++)
        localDeIndexTuple[i] = indexTuple[i];
      // look just across interface along dim
      localDeIndexTuple[dim-1] = -1;
      // get sequence index providing localDe relative index tuple
      int seqindex =
        getSequenceIndexLocalDe(localDe, localDeIndexTuple, 3, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return false;
      // determine if seqindex indicates edge or not
      if (seqindex != -1){
        // valid seqindex indicates that there is a neighbor
        onEdge = false;
        break;
      }
      multiDimIndexLoop.next(); // increment tuple, but skip dim
    }
  }
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return onEdge;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::isLocalDeOnEdgeU()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::isLocalDeOnEdgeU
//
// !INTERFACE:
bool DistGrid::isLocalDeOnEdgeU(
//
// !RETURN VALUE:
//    bool, true if local DE is on indicated edge, false otherwise
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  int dim,                          // in  - dim  = {1, ..., dimCount}
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Determine if local DE is on upper edge along dimension dim.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", ESMC_CONTEXT, rc);
    return false;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified dim out of bounds", ESMC_CONTEXT, rc);
    return false;
  }
  
  // determine which tile localDe is located on
  int de = delayout->getLocalDeToDeMap()[localDe];
  bool onEdge = true;            // assume local De is on edge
  if (elementCountPDe[de]){
    // local De is associated with elements
    // prepare localDe relative index tuple of neighbor index to check
    vector<int> localDeIndexTupleVec(dimCount);
    int *localDeIndexTuple = &(localDeIndexTupleVec[0]);
    vector<int> sizes;
    for (int i=0; i<dimCount; i++)
      sizes.push_back(indexCountPDimPDe[de*dimCount+i]);
    MultiDimIndexLoop multiDimIndexLoop(sizes);
    multiDimIndexLoop.setSkipDim(dim-1);  // next() to skip dim
    while(multiDimIndexLoop.isWithin()){
      // look at the entire interface spanned by all dimensions except dim
      int const *indexTuple = multiDimIndexLoop.getIndexTuple();
      for (int i=0; i<dimCount; i++)
        localDeIndexTuple[i] = indexTuple[i];
      // look just across interface along dim
      localDeIndexTuple[dim-1] = indexCountPDimPDe[de*dimCount+(dim-1)];
      // get sequence index providing localDe relative index tuple
      int seqindex =
        getSequenceIndexLocalDe(localDe, localDeIndexTuple, 3, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return false;
      // determine if seqindex indicates edge or not
      if (seqindex != -1){
        // valid seqindex indicates that there is a neighbor
        onEdge = false;
        break;
      }
      multiDimIndexLoop.next(); // increment tuple, but skip dim
    }
  }
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return onEdge;
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified DE out of bounds", ESMC_CONTEXT, rc);
    return -1; // bail out
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified dim out of bounds", ESMC_CONTEXT, rc);
    return -1; // bail out
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return contigFlagPDimPDe[de*dimCount+(dim-1)];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getElementCountPDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getElementCountPDe
//
// !INTERFACE:
int DistGrid::getElementCountPDe(
//
// !RETURN VALUE:
//    int elementCount for DE
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified DE out of bounds", ESMC_CONTEXT, rc);
    return -1; // bail out
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return elementCountPDe[de];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getSequenceIndexLocalDe()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getSequenceIndexLocalDe
//
// !INTERFACE:
int DistGrid::getSequenceIndexLocalDe(
//
// !RETURN VALUE:
//    int sequence index
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  const int *index,                 // in  - DE-local index tuple in or 
                                    //       relative to exclusive region
                                    //       basis 0
  int depth,                        // in  - topology recursions depth
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index provided the index tuple in terms of the exclusive
//    region of a local DE starting at basis 0. DistGrid dimensions that have
//    contiguous indices support the specified index to lay outside of the
//    localDe exclusive region unless arbitrary sequence indices have been
//    specified during DistGrid creation. DistGrids with arbitrary sequence
//    indices and/or along dimensions with non-contiguous indices require
//    the specified index to be within [0..indexCountPDimPDe[dim,localDe]-1].
//
//    A value of -1 is returned by this function if the specified index tuple
//    cannot be mapped to a sequence index in DistGrid. If at the same time
//    the code returned in rc does not indicate an error a return value of -1
//    indicates that the index tuple lies outside of the DistGrid index space.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", ESMC_CONTEXT, rc);
    return -1;
  }
  int de = delayout->getLocalDeToDeMap()[localDe];
  for (int i=0; i<dimCount; i++){
    //TODO: this does _not_ support multiple collocations w/ arb seqIndices 
    //TODO: it assumes that arbSeqIndices may only exist on the first colloc.
    if (arbSeqIndexListPCollPLocalDe[0][localDe] ||
      !contigFlagPDimPDe[de*dimCount+i]){
      if (index[i] < 0 || index[i] >= indexCountPDimPDe[de*dimCount+i]){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "- Specified index out of bounds", ESMC_CONTEXT, rc);
        return -1;
      }
    }
  }
  
  // determine seqindex
  int seqindex;
  if (arbSeqIndexListPCollPLocalDe[0][localDe]){
    // determine the sequentialized index by arbSeqIndexListPLocalDe look-up
    //TODO: this does _not_ support multiple collocations w/ arb seqIndices 
    //TODO: it assumes that arbSeqIndices may only exist on the first colloc.
    int linExclusiveIndex = index[dimCount-1];  // initialize
    for (int i=dimCount-2; i>=0; i--){
      linExclusiveIndex *= indexCountPDimPDe[de*dimCount + i];
      linExclusiveIndex += index[i];
    }
    seqindex = arbSeqIndexListPCollPLocalDe[0][localDe][linExclusiveIndex];
  }else{
    // determine the sequentialized index by construction of default tile rule
    const int *localDeToDeMap = delayout->getLocalDeToDeMap();
    int tile = tileListPDe[localDeToDeMap[localDe]];  // tiles are basis 1 !!!!
    if (tile == 0){
      // means that the localDe does not have any elements thus not on tile
      seqindex = -1;  // indicate no seqIndex
    }else{
      // prepare tile relative index tuple
      int *tileIndexTuple = new int[dimCount];
      for (int i=0; i<dimCount; i++){
        if (contigFlagPDimPDe[de*dimCount+i])
          tileIndexTuple[i] = minIndexPDimPDe[de*dimCount+i] + index[i];
        else
          tileIndexTuple[i] =
            indexListPDimPLocalDe[localDe*dimCount+i][index[i]];
      }
      // get sequence index providing tile relative index tuple
      seqindex = getSequenceIndexTile(tile, tileIndexTuple, depth, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return -1;  //  bail out with invalid seqindex
      delete [] tileIndexTuple;
    }
  }
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqindex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getSequenceIndexTileRelative()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getSequenceIndexTileRelative
//
// !INTERFACE:
int DistGrid::getSequenceIndexTileRelative(
//
// !RETURN VALUE:
//    int sequence index
//
// !ARGUMENTS:
//
  int tile,                        // in  - tile = {1, ..., tileCount}
  const int *index,                 // in  - tile relative index tuple, base 0
  int depth,                        // in  - depth of recursive search
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index provided the tile relative index tuple.
//
//    A value of -1 is returned by this function if the specified index tuple
//    cannot be mapped to a sequence index in DistGrid. If at the same time
//    the code returned in rc does not indicate an error a return value of -1
//    indicates that the index tuple lies outside of the DistGrid index space.
//
//    Same as getSequenceIndexTile(), but allows index tuple to be passed in
//    tile relative, base 0, which is more conveninet in many cases.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // check input
  if (tile < 1 || tile > tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified tile out of bounds", ESMC_CONTEXT, rc);
    return -1;
  }

  int *indexTileSpecific = new int[dimCount];
  for (int i=0; i<dimCount; i++)
    indexTileSpecific[i] = index[i] + minIndexPDimPTile[(tile-1)*dimCount+i];
  
  int seqindex = getSequenceIndexTile(tile, indexTileSpecific, depth,
    &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
    ESMC_CONTEXT, rc)) return seqindex;  // bail out
  
  delete [] indexTileSpecific;
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqindex;
}
//-----------------------------------------------------------------------------
  
  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getSequenceIndexTile()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getSequenceIndexTile
//
// !INTERFACE:
int DistGrid::getSequenceIndexTile(
//
// !RETURN VALUE:
//    int sequence index
//
// !ARGUMENTS:
//
  int tile,                        // in  - tile = {1, ..., tileCount}
  const int *index,                 // in  - tile-specific absolute index tuple
  int depth,                        // in  - depth of recursive search
  int *rc                           // out - return code
  )const{
//
// !DESCRIPTION:
//    Get sequential index provided the tile relative index tuple.
//
//    A value of -1 is returned by this function if the specified index tuple
//    cannot be mapped to a sequence index in DistGrid. If at the same time
//    the code returned in rc does not indicate an error a return value of -1
//    indicates that the index tuple lies outside of the DistGrid index space.
//
//    The way this recursive algorithm is written, it requires that the 
//    provided index tuple be expressed in a "tile-specific absolute" sense.
//    It is "absolute" in that the (0,0,...) tuple is not 'defined' to
//    equal the origin of the tile. Instead the origin of the tile would
//    be indicated by an index tuple that is equal to the "tile-specific"
//    vector slice of minIndexPDimPTile[].
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

//printf("gjt - getSequenceIndexTile depth: %d\n", depth);

  // check input
  if (tile < 1 || tile > tileCount){
    char message[80];
    sprintf(message, "- Specified tile %d is out of bounds", tile);
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, message, ESMC_CONTEXT, rc);
    return -1;
  }
  
  // adjust recursion depth
  --depth;

  bool onTile = true;  // start assuming that index tuple can be found on tile
  // add up elements from tile
  int seqindex = 0; // initialize
  for (int i=dimCount-1; i>=0; i--){
    // first time multiply with zero intentionally:
    seqindex *= maxIndexPDimPTile[(tile-1)*dimCount+i] 
      - minIndexPDimPTile[(tile-1)*dimCount+i] + 1;
    if ((index[i] < minIndexPDimPTile[(tile-1)*dimCount+i])
      || (index[i] > maxIndexPDimPTile[(tile-1)*dimCount+i])){
      // index is outside of tile bounds -> break out of onTile code
      onTile = false;
      seqindex = -1;  // indicate not valid sequence index
      break;
    }
    seqindex += index[i]
      - minIndexPDimPTile[(tile-1)*dimCount+i];
  }
  if (onTile){
    // add all the elements of previous tiles
    for (int i=0; i<tile-1; i++)
      seqindex += elementCountPTile[i];
    ++seqindex;  // shift sequentialized index to basis 1 !!!!
  }else if (depth >= 0){
    for (int i=0; i<connectionCount; i++){
      int tileA = connectionList[i][0];
      int tileB = connectionList[i][1];
      if (tileA == tile){
        // found connection for this tile -> need to transform index
        int *indexB = new int[dimCount];
        int positionIndexOffset = 2;
        int orientationIndexOffset = 2+dimCount;
        for (int j=0; j<dimCount; j++){
          int positionOffset = connectionList[i][positionIndexOffset+j];
          int orientationIndex = connectionList[i][orientationIndexOffset+j];
          if (orientationIndex < 0){
            ++orientationIndex; // shift to basis 0
            indexB[j] = -(index[-orientationIndex] - positionOffset);
          }else{
            --orientationIndex; // shift to basis 0
            indexB[j] = index[orientationIndex] - positionOffset;
          }
        }
        seqindex = getSequenceIndexTile(tileB, indexB, depth, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return seqindex;  // bail out
        delete [] indexB;
        if (seqindex > -1)
          break;  // break out of loop over connections
      }
      if (tileB == tile){
        // found connection for this tile -> need to transform index
        int *indexA = new int[dimCount];
        int positionIndexOffset = 2;
        int orientationIndexOffset = 2+dimCount;
        for (int j=0; j<dimCount; j++){
          int positionOffset = connectionList[i][positionIndexOffset+j];
          int orientationIndex = connectionList[i][orientationIndexOffset+j];
          if (orientationIndex < 0){
            ++orientationIndex; // shift to basis 0
            indexA[-orientationIndex] = -index[j] + positionOffset;
          }else{
            --orientationIndex; // shift to basis 0
            indexA[orientationIndex] = index[j] + positionOffset;
          }
        }
        seqindex = getSequenceIndexTile(tileA, indexA, depth, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc)) return seqindex;  // bail out
        delete [] indexA;
        if (seqindex > -1)
          break;  // break out of loop over connections
      }
    }
  }
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return seqindex;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMinIndexPDimPTile()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMinIndexPDimPTile
//
// !INTERFACE:
const int *DistGrid::getMinIndexPDimPTile(
//
// !RETURN VALUE:
//    int *minIndex for tile
//
// !ARGUMENTS:
//
  int tile,                            // in  - tile   = {1, ..., tileCount}
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
  if (tile < 1 || tile > tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified tile out of bounds", ESMC_CONTEXT, rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &minIndexPDimPTile[(tile-1)*dimCount];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getMaxIndexPDimPTile()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getMaxIndexPDimPTile
//
// !INTERFACE:
const int *DistGrid::getMaxIndexPDimPTile(
//
// !RETURN VALUE:
//    int *maxIndex for tile
//
// !ARGUMENTS:
//
  int tile,                            // in  - tile   = {1, ..., tileCount}
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
  if (tile < 1 || tile > tileCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified tile out of bounds", ESMC_CONTEXT, rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return &maxIndexPDimPTile[(tile-1)*dimCount];
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified de out of bounds", ESMC_CONTEXT, rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified de out of bounds", ESMC_CONTEXT, rc);
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", ESMC_CONTEXT, rc);
    return NULL;
  }
  if (dim < 1 || dim > dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified dim out of bounds", ESMC_CONTEXT, rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return indexListPDimPLocalDe[localDe*dimCount+(dim-1)];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DistGrid::getArbSeqIndexList()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::getArbSeqIndexList
//
// !INTERFACE:
const int *DistGrid::getArbSeqIndexList(
//
// !RETURN VALUE:
//    int *arbSeqIndexList for localDe
//
// !ARGUMENTS:
//
  int localDe,                      // in  - local DE = {0, ..., localDeCount-1}
  int collocation,                  // in
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
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", ESMC_CONTEXT, rc);
    return NULL;
  }
  int collocationIndex;
  for (collocationIndex=0; collocationIndex<diffCollocationCount;
    collocationIndex++)
    if (collocationTable[collocationIndex] == collocation) break;
  if (collocationIndex==diffCollocationCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified collocation not found", ESMC_CONTEXT, rc);
    return NULL;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arbSeqIndexListPCollPLocalDe[collocationIndex][localDe];
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// serialize() and deserialize()
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
  int *offset,           // inout - original offset, updated to point 
                             //  to first free byte after current obj info
  ESMC_InquireFlag inquireflag // in - inquire flag
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

  int i;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  int r;

  // Check if buffer has enough free memory to hold object
  if ((inquireflag != ESMF_INQUIREONLY) && (*length - *offset)
    < sizeof(DistGrid)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
      "Buffer too short to add a DistGrid object", ESMC_CONTEXT, &rc);
    return rc;
  }

  // Serialize the Base class,
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ESMC_AttReconcileFlag attreconflag = ESMC_ATTRECONCILE_OFF;
  localrc = this->ESMC_Base::ESMC_Serialize(buffer,length,offset,attreconflag,
      inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Serialize the DELayout
  localrc = delayout->serialize(buffer, length, offset,inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;
  // Serialize DistGrid meta data
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ip = (int *)(buffer + *offset);
  if (inquireflag != ESMF_INQUIREONLY){
    *ip++ = dimCount;
    *ip++ = tileCount;
    for (int i=0; i<dimCount*tileCount; i++){
      *ip++ = minIndexPDimPTile[i];
      *ip++ = maxIndexPDimPTile[i];
    }
    for (int i=0; i<tileCount; i++)
      *ip++ = elementCountPTile[i];
  }else
    ip += 2 + 2*dimCount*tileCount + tileCount;

  int deCount = delayout->getDeCount();
  if (inquireflag != ESMF_INQUIREONLY){
    for (int i=0; i<dimCount*deCount; i++){
      *ip++ = minIndexPDimPDe[i];
      *ip++ = maxIndexPDimPDe[i];
      *ip++ = contigFlagPDimPDe[i];
      *ip++ = indexCountPDimPDe[i];
    }
    for (int i=0; i<deCount; i++){
      *ip++ = elementCountPDe[i];
      *ip++ = tileListPDe[i];
    }
    *ip++ = diffCollocationCount;
    for (int i=0; i<dimCount; i++){
      *ip++ = collocationPDim[i];
      *ip++ = collocationTable[i];
    }
  }else
    ip += 4*dimCount*deCount + 2*deCount + 1 + 2*dimCount;

  if (inquireflag != ESMF_INQUIREONLY){
    *ip++ = connectionCount;
    for (int i=0; i<connectionCount; i++)
      for (int j=0; j<2*dimCount+2; j++)
        *ip++ = connectionList[i][j];
  }else
    ip += 1 + connectionCount*(2*dimCount+2);
    
  if (inquireflag != ESMF_INQUIREONLY){
    if (regDecomp){
      *ip++ = dimCount;
      for (int i=0; i<dimCount; i++)
        *ip++ = regDecomp[i];
    }else
      *ip++ = 0;
  }else{
    ip++;
    if (regDecomp)
      ip += dimCount;
  }

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

  DistGrid *a = new DistGrid(-1); // prevent baseID counter increment
  int i;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  int r;
  
  // Deserialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ESMC_AttReconcileFlag attreconflag = ESMC_ATTRECONCILE_OFF;
  localrc = a->ESMC_Base::ESMC_Deserialize(buffer,offset,attreconflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return NULL;
  // Deserialize the DELayout
  a->delayout = DELayout::deserialize(buffer, offset);
  a->delayoutCreator = true;  // deserialize creates a local object
  // VM is a special case
  a->vm = NULL; // VM must be reset
  // Deserialize DistGrid meta data
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ip = (int *)(buffer + *offset);
  a->dimCount = *ip++;
  a->tileCount = *ip++;
  a->minIndexPDimPTile = new int[a->dimCount*a->tileCount];
  a->maxIndexPDimPTile = new int[a->dimCount*a->tileCount];
  for (int i=0; i<a->dimCount*a->tileCount; i++){
    a->minIndexPDimPTile[i] = *ip++;
    a->maxIndexPDimPTile[i] = *ip++;
  }
  a->elementCountPTile = new int[a->tileCount];
  for (int i=0; i<a->tileCount; i++)
    a->elementCountPTile[i] = *ip++;
  int deCount = a->delayout->getDeCount();
  a->minIndexPDimPDe = new int[a->dimCount*deCount];
  a->maxIndexPDimPDe = new int[a->dimCount*deCount];
  a->contigFlagPDimPDe = new int[a->dimCount*deCount];
  a->indexCountPDimPDe = new int[a->dimCount*deCount];
  for (int i=0; i<a->dimCount*deCount; i++){
    a->minIndexPDimPDe[i] = *ip++;
    a->maxIndexPDimPDe[i] = *ip++;
    a->contigFlagPDimPDe[i] = *ip++;
    a->indexCountPDimPDe[i] = *ip++;
  }
  a->elementCountPDe = new int[deCount];
  a->tileListPDe = new int[deCount];
  for (int i=0; i<deCount; i++){
    a->elementCountPDe[i] = *ip++;
    a->tileListPDe[i] = *ip++;
  }
  a->diffCollocationCount = *ip++;
  a->collocationPDim = new int[a->dimCount];
  a->collocationTable = new int[a->dimCount];
  for (int i=0; i<a->dimCount; i++){
    a->collocationPDim[i] = *ip++;
    a->collocationTable[i] = *ip++;
  }
  a->connectionCount = *ip++;
  a->connectionList = new int*[a->connectionCount];
  for (int i=0; i<a->connectionCount; i++){
    a->connectionList[i] = new int[2*a->dimCount+2];
    for (int j=0; j<2*a->dimCount+2; j++)
      a->connectionList[i][j] = *ip++;
  }
  // reset all xxPLocalDe variables on proxy object
  a->indexListPDimPLocalDe = new int*[0];
  a->arbSeqIndexListPCollPLocalDe = new int**[a->diffCollocationCount];
  a->elementCountPCollPLocalDe = new int*[a->diffCollocationCount];
  for (int i=0; i<a->diffCollocationCount; i++){
    a->arbSeqIndexListPCollPLocalDe[i] = new int*[1];
    a->elementCountPCollPLocalDe[i] = new int[1];
    a->arbSeqIndexListPCollPLocalDe[i][0] = NULL;
    a->elementCountPCollPLocalDe[i][0] = 0;
  }
  
  //regDecomp
  int regDecompDimCount = *ip++;
  if (regDecompDimCount == a->dimCount){
    a->regDecomp = new int[a->dimCount];
    for (int i=0; i<a->dimCount; i++)
      a->regDecomp[i] = *ip++;
  }else
    a->regDecomp = NULL;

  cp = (char *)ip;
  *offset = (cp - buffer);
  
  a->localDeCountAux = a->delayout->getLocalDeCount(); // TODO: auxilary f garb
                                                 // TODO: until ref. counting
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
  int tileIndexA,                  // in  -
  int tileIndexB,                  // in  -
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
  if (!present(connection)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to connection array", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (connection->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- connection array must be of rank 1", ESMC_CONTEXT, &rc);
    return rc;
  }
  int dimCount = (connection->extent[0]-2)/2;
  if (connection->extent[0] != dimCount*2 + 2){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dimension of connection array must be of size "
      "(2 * dimCount + 2)", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // fill in the tile indices
  connection->array[0] = tileIndexA;
  connection->array[1] = tileIndexB;
  
  // check positionVector argument
  if (!present(positionVector)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to positionVector array", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (positionVector->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- positionVector array must be of rank 1", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (positionVector->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- 1st dimension of positionVector array must be of size dimCount",
      ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // fill in the positionVector
  memcpy(&(connection->array[2]), positionVector->array,
    sizeof(int)*dimCount);
  
  // check on orientationVector
  if (present(orientationVector)){
    // orientationVector was provided
    if (orientationVector->dimCount != 1){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
        "- orientationVector array must be of rank 1", ESMC_CONTEXT, &rc);
      return rc;
    }
    if (orientationVector->extent[0] != dimCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "- 1st dimension of orientationVector array must be of size dimCount",
        ESMC_CONTEXT, &rc);
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
#define ESMC_METHOD "ESMCI::DistGrid::setCollocationPDim()"
//BOPI
// !IROUTINE:  ESMCI::DistGrid::setCollocationPDim
//
// !INTERFACE:
//
int DistGrid::setCollocationPDim(
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  InterfaceInt *collocationPDimArg // in
  ){
//
// !DESCRIPTION:
//    Set the collocation list
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  if (!present(collocationPDimArg)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to collocationPDimArg array", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (collocationPDimArg->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- collocationPDimArg array must be of rank 1", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (collocationPDimArg->extent[0] != dimCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- collocationPDimArg array must supply exactly dimCount entries",
      ESMC_CONTEXT, &rc);
    return rc;
  }

  // set collocationPDim[]
  memcpy(collocationPDim, collocationPDimArg->array, sizeof(int)*dimCount);
  
  // delete arbSeqIndex if previously set
  int localDeCount = delayout->getLocalDeCount();
  for (int i=0; i<diffCollocationCount; i++){
    for (int j=0; j<localDeCount; j++)
      if (arbSeqIndexListPCollPLocalDe[i][j])
        delete [] arbSeqIndexListPCollPLocalDe[i][j];
    delete [] arbSeqIndexListPCollPLocalDe[i];
    delete [] elementCountPCollPLocalDe[i];
  }
  delete [] arbSeqIndexListPCollPLocalDe;
  delete [] elementCountPCollPLocalDe;
  
  // determine diffCollocationCount and construct collocationTable
  diffCollocationCount = 0; // reset
  for (int i=0; i<dimCount; i++){
    int j;
    for (j=0; j<i; j++)
      if (collocationPDim[i] == collocationPDim[j]) break;
    if (j==i){
      collocationTable[diffCollocationCount] = collocationPDim[i];
      ++diffCollocationCount; // found a new collocation
    }
  }
  
  // no arbitrary sequence indices by default
  arbSeqIndexListPCollPLocalDe = new int**[diffCollocationCount];
  elementCountPCollPLocalDe = new int*[diffCollocationCount];
  const int *localDeToDeMap = delayout->getLocalDeToDeMap();
  for (int i=0; i<diffCollocationCount; i++){
    arbSeqIndexListPCollPLocalDe[i] = new int*[localDeCount];
    elementCountPCollPLocalDe[i] = new int[localDeCount];
    for (int j=0; j<localDeCount; j++){
      arbSeqIndexListPCollPLocalDe[i][j] = NULL;
      elementCountPCollPLocalDe[i][j] = 1;  // initialize
      for (int k=0; k<dimCount; k++){
        if (collocationPDim[k]==collocationTable[i]){
          elementCountPCollPLocalDe[i][j] *=
            indexCountPDimPDe[localDeToDeMap[j]*dimCount+k];
        }
      }
    }
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
  InterfaceInt *arbSeqIndex,    // in
  int localDe,                  // in  - local DE = {0, ..., localDeCount-1}
  int collocation               // in
  ){
//
// !DESCRIPTION:
//    Set the array of arbitrary indices
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // check input
  int localDeCount = delayout->getLocalDeCount();
  if (localDe < 0 || localDe > localDeCount-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified local DE out of bounds", ESMC_CONTEXT, &rc);
    return rc;
  }
  int collocationIndex;
  for (collocationIndex=0; collocationIndex<diffCollocationCount;
    collocationIndex++)
    if (collocationTable[collocationIndex] == collocation) break;
  if (collocationIndex==diffCollocationCount){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- Specified collocation not found", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (!present(arbSeqIndex)){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to arbSeqIndex array", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (arbSeqIndex->dimCount != 1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_RANK,
      "- arbSeqIndex array must be of rank 1", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (arbSeqIndex->extent[0] !=
    elementCountPCollPLocalDe[collocationIndex][localDe]){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "- arbSeqIndex array must supply one value for each element in local DE",
      ESMC_CONTEXT, &rc);
    return rc;
  }

  // set arbSeqIndexListPLocalDe[][]
  if (arbSeqIndexListPCollPLocalDe[collocationIndex][localDe])
    // delete previous index list
    delete [] arbSeqIndexListPCollPLocalDe[collocationIndex][localDe];
  arbSeqIndexListPCollPLocalDe[collocationIndex][localDe] =
    new int[arbSeqIndex->extent[0]];
  memcpy(arbSeqIndexListPCollPLocalDe[collocationIndex][localDe],
    arbSeqIndex->array, sizeof(int)*arbSeqIndex->extent[0]);
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


  //============================================================================
  // class MultiDimIndexLoop
  // Iterator type through regular multidimensional structures.
  MultiDimIndexLoop::MultiDimIndexLoop(){
    // default initialize
    indexTupleStart.resize(0);
    indexTupleEnd.resize(0);
    indexTuple.resize(0);
    skipDim.resize(0);
    indexTupleBlockStart.resize(0);
    indexTupleBlockEnd.resize(0);
    indexTupleWatchStart.resize(0);
    indexTupleWatchEnd.resize(0);
  }
  MultiDimIndexLoop::MultiDimIndexLoop(vector<int> const &sizes){
    indexTupleEnd = sizes;
    // default initialize rest
    int rank = sizes.size();
    indexTupleStart.resize(rank);
    indexTuple.resize(rank);
    skipDim.resize(rank);
    indexTupleBlockStart.resize(rank);
    indexTupleBlockEnd.resize(rank);
    indexTupleWatchStart.resize(rank);
    indexTupleWatchEnd.resize(rank);
    for (int i=0; i<rank; i++){
      indexTupleStart[i] = indexTuple[i] = 0; // reset
      skipDim[i] = false;                     // reset
      indexTupleBlockStart[i] = indexTupleBlockEnd[i] = 0;  // reset
      indexTupleWatchStart[i] = indexTupleWatchEnd[i] = 0;  // reset
    }
    adjust();
  }
  MultiDimIndexLoop::MultiDimIndexLoop(vector<int> const &offsets,
    vector<int> const &sizes){
    indexTupleStart = offsets;
    indexTupleEnd = sizes;
    // default initialize rest
    int rank = sizes.size();
    // todo: check that vector size matches, and throw exception if not
    indexTuple.resize(rank);
    skipDim.resize(rank);
    indexTupleBlockStart.resize(rank);
    indexTupleBlockEnd.resize(rank);
    indexTupleWatchStart.resize(rank);
    indexTupleWatchEnd.resize(rank);
    for (int i=0; i<rank; i++){
      indexTuple[i] = indexTupleStart[i];     // reset
      indexTupleEnd[i] += indexTupleStart[i]; // shift end by offsets
      skipDim[i] = false;                     // reset
      indexTupleBlockStart[i] = indexTupleBlockEnd[i] = 0;  // reset
      indexTupleWatchStart[i] = indexTupleWatchEnd[i] = 0;  // reset
    }
    adjust();
  }
  void MultiDimIndexLoop::setSkipDim(int dim){
    // todo: check that dim is between 0...,size-1
    skipDim[dim] = true;
  }
  void MultiDimIndexLoop::setBlockStart(vector<int> const &blockStart){
    // todo: check that size of incoming blockStart vector is equal to rank
    indexTupleBlockStart = blockStart;
    adjust();
  }
  void MultiDimIndexLoop::setBlockEnd(vector<int> const &blockEnd){
    // todo: check that size of incoming blockStart vector is equal to rank
    indexTupleBlockEnd = blockEnd;
    adjust();
  }
  void MultiDimIndexLoop::setWatchStart(vector<int> const &watchStart){
    // todo: check that size of incoming watchStart vector is equal to rank
    indexTupleWatchStart = watchStart;
    adjust();
  }
  void MultiDimIndexLoop::setWatchEnd(vector<int> const &watchEnd){
    // todo: check that size of incoming watchStart vector is equal to rank
    indexTupleWatchEnd = watchEnd;
    adjust();
  }
  void MultiDimIndexLoop::first(){
    // set indexTuple to the first valid index, considering blocked out region
    int i;
    for (i=0; i<indexTuple.size(); i++)
      indexTuple[i] = indexTupleStart[i];  // reset, to cover not fully blocked
    for (i=0; i<indexTuple.size(); i++){
      if ((indexTupleStart[i] < indexTupleBlockStart[i]) ||
        (indexTupleBlockEnd[i] < indexTupleEnd[i])){
        // found a dimension that does NOT have a fully blocked range
        break;
      }
    }
    if (i<indexTuple.size()){
      // dimension i is the first one that does NOT have fully blocked range
      if (indexTupleStart[i] < indexTupleBlockStart[i]){
        // there are unblocked elements before the blocked ranged
        indexTuple[i] = indexTupleStart[i];
      }else{
        // there must be unblocked elements after the blocked ranged
        indexTuple[i] = indexTupleBlockEnd[i];
      }
    }else{
      // all dimensions are fully blocked out -> set indexTuple to the End
      for (i=0; i<indexTuple.size(); i++)
        indexTuple[i] = indexTupleEnd[i];
    }
  }
  void MultiDimIndexLoop::last(){
    // set indexTuple to the last index, not considering whether it is blocked
    for (int i=0; i<indexTuple.size(); i++)
      indexTuple[i] = indexTupleEnd[i]-1;  // reset
  }
  void MultiDimIndexLoop::adjust(){
    // adjust the indexTuple after an increment to point to the next valid tuple
    // -> consider the blocked out region
    // to improve performance for the fully blocked case check for it first
    bool skipBlockedRegionFlag = true;
    for (int i=0; i<indexTuple.size(); i++){
      if ((indexTupleBlockStart[i] > indexTupleStart[i]) ||
        (indexTupleBlockEnd[i] < indexTupleEnd[i])){
        // found a dimension that does not have a fully blocked range
        skipBlockedRegionFlag = false;
        break;
      }
    }
    if (skipBlockedRegionFlag){
      // fully blocked range in all dimensions -> shift indexTuple to end
      for (int i=0; i<indexTuple.size(); i++)
        indexTuple[i] = indexTupleEnd[i];
    }else{
      // there are dimensions that do NOT have fully blocked ranges
      // -> must carefully adjust
      do{
        // adjust all tuples, if necessary skip blocked region
        skipBlockedRegionFlag = true;  // init
        int i;
        for (i=0; i<indexTuple.size()-1; i++){
          if (indexTuple[i] == indexTupleEnd[i]){
            indexTuple[i] = indexTupleStart[i];  // reset
            if (skipDim[i+1])
              indexTuple[i+1] = indexTupleEnd[i+1]; // skip
            else
              ++indexTuple[i+1];                    // increment
          }
          if ((indexTuple[i] < indexTupleBlockStart[i]) ||
            (indexTuple[i] >= indexTupleBlockEnd[i])){
            skipBlockedRegionFlag = false;  // not within blocked region
          }
        }
        if ((indexTuple[i] < indexTupleBlockStart[i]) ||
          (indexTuple[i] >= indexTupleBlockEnd[i])){
          skipBlockedRegionFlag = false;  // not within blocked region
        }
        if (skipBlockedRegionFlag){
          indexTuple[0] = indexTupleBlockEnd[0];
//          printf("gjt skip the blocked region\n");     
        }
      }while(skipBlockedRegionFlag && (indexTuple[0] >= indexTupleEnd[0]));
    }
  }
  void MultiDimIndexLoop::next(){
    if (skipDim[0])
      indexTuple[0] = indexTupleEnd[0]; // skip
    else
      ++indexTuple[0];                  // increment
    adjust();
  }
  bool MultiDimIndexLoop::isFirst()const{
    for (int i=0; i<indexTuple.size(); i++)
      if (indexTuple[i] != indexTupleStart[i]) return false;
    return true;
  }
  bool MultiDimIndexLoop::isLast()const{
    for (int i=0; i<indexTuple.size(); i++)
      if (indexTuple[i] != indexTupleEnd[i]-1) return false;
    return true;
  }
  bool MultiDimIndexLoop::isWithin()const{
    for (int i=0; i<indexTuple.size(); i++)
      if (indexTupleStart[i] >= indexTupleEnd[i])
        return false; // this means there are no elements to iterate over
    if (indexTuple[indexTuple.size()-1] < indexTupleEnd[indexTuple.size()-1])
      return true;
    return false;
  }
  bool MultiDimIndexLoop::isWithinBlock(int dim)const{
    if (indexTuple[dim] < indexTupleBlockStart[dim])
      return false;
    if (indexTuple[dim] >= indexTupleBlockEnd[dim])
      return false;
    return true;
  }
  bool MultiDimIndexLoop::isWithinWatch()const{
    bool withinWatchFlag = true;  // init
    int i;
    for (i=0; i<indexTuple.size()-1; i++){
      if ((indexTuple[i] < indexTupleWatchStart[i]) ||
        (indexTuple[i] >= indexTupleWatchEnd[i])){
        withinWatchFlag = false;  // not within watched region
        break;
      }
    }
    if ((indexTuple[i] < indexTupleWatchStart[i]) ||
      (indexTuple[i] >= indexTupleWatchEnd[i])){
      withinWatchFlag = false;  // not within watched region
    }
    return withinWatchFlag;
  }
  int const *MultiDimIndexLoop::getIndexTuple()const{
    return &indexTuple[0];
  }
  int const *MultiDimIndexLoop::getIndexTupleEnd()const{
    return &indexTupleEnd[0];
  }
  int const *MultiDimIndexLoop::getIndexTupleStart()const{
    return &indexTupleStart[0];
  }
  void MultiDimIndexLoop::print()const{
    int i;
    printf("MultiDimIndexLoop: indexTupleStart = (");
    for (i=0; i<indexTupleStart.size()-1; i++)
      printf(" %d,", indexTupleStart[i]);
    printf(" %d)\n", indexTupleStart[i]);
    printf("MultiDimIndexLoop: indexTupleEnd = (");
    for (i=0; i<indexTupleEnd.size()-1; i++)
      printf(" %d,", indexTupleEnd[i]);
    printf(" %d)\n", indexTupleEnd[i]);
    printf("MultiDimIndexLoop: indexTuple = (");
    for (i=0; i<indexTuple.size()-1; i++)
      printf(" %d,", indexTuple[i]);
    printf(" %d)\n", indexTuple[i]);
  }
  //============================================================================



} // namespace ESMCI
