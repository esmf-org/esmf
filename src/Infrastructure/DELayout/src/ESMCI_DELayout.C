// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_DELayout.C"
//==============================================================================
#define XXE_CONSTRUCTOR_LOG_off
#define XXE_STORAGEDELETE_LOG_off
#define XXE_EXEC_LOG_off
#define XXE_EXEC_MEMLOG_off
#define XXE_EXEC_BUFFLOG_off
#define XXE_EXEC_OPSLOG_off
#define XXE_EXEC_RECURSLOG_off
//==============================================================================
//
// DELayout class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DELayout methods declared
// in the companion file ESMCI_DELayout.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_DELayout.h"

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>
#include <typeinfo>
#include <vector>
#include <map>
#include <sstream>

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_RHandle.h"

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// prototypes for Fortran interface routines called by C++ code below
extern "C" {
  void FTN_X(f_esmf_dynmaskcallbackr8r8r8)(ESMCI::RouteHandle **ptr,
    int *count, void *elementVector, int *countVector, int *totalCount,
    void *factorsVector, void *valuesVector, int *vectorL, int *rc);
#ifndef ESMF_NO_DYNMASKOVERLOAD
  void FTN_X(f_esmf_dynmaskcallbackr4r8r4)(ESMCI::RouteHandle **ptr,
    int *count, void *elementVector, int *countVector, int *totalCount,
    void *factorsVector, void *valuesVector, int *vectorL, int *rc);
  void FTN_X(f_esmf_dynmaskcallbackr4r4r4)(ESMCI::RouteHandle **ptr,
    int *count, void *elementVector, int *countVector, int *totalCount,
    void *factorsVector, void *valuesVector, int *vectorL, int *rc);
#endif
}
//-------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::create()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::create
//
// !INTERFACE:
DELayout *DELayout::create(
//
// !RETURN VALUE:
//    DELayout * to newly allocated DELayout object.
//
// !ARGUMENTS:
//
  int *petMap,              // (in) pointer to petMap list
  int petMapCount,          // (in) number of element in petMap
  ESMC_Pin_Flag *pinFlag,   // (in) type of resources DEs are pinned to
  VM *vm,                   // (in) VM context
  int *rc){                 // (out) return code
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // allocate the new DELayout object and construct the inside
  DELayout *delayout;
  try{
    delayout = new DELayout(vm);  // specific VM, or default if vm==NULL
    localrc = delayout->construct(vm, pinFlag, petMap, petMapCount);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      delayout->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }catch(...){
     // allocation error
     ESMC_LogDefault.MsgAllocError("for new DELayout.", ESMC_CONTEXT, rc);
     return ESMC_NULL_POINTER;
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return delayout;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::create()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::create
//
// !INTERFACE:
DELayout *DELayout::create(
//
// !RETURN VALUE:
//    DELayout * to newly allocated DELayout object.
//
// !ARGUMENTS:
//
  int *deCountArg,              // (in) number of DEs
  InterArray<int> *deGrouping,  // (in) deGrouping vector
  ESMC_Pin_Flag *pinFlag,       // (in) type of resources DEs are pinned to
  InterArray<int> *petListArg,  // (in) list of PETs to be used in delayout
  VM *vm,                       // (in) VM context
  int *rc){                     // (out) return code
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // There is only one DELayoutConstruct() method - it requires a petMap
  // in order to construct the inside of a DELayout object. The task of
  // this DELayoutCreate() function is to build a petMap according to the
  // provided input and then call DELayoutConstruct() with this petMap.
  int *petMap;
  int petMapCount;
  bool petMapDeleteFlag = false; // reset

  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return ESMC_NULL_POINTER;
  }

  // query the VM for localPet and petCount
  int localPet = vm->getMypet();
  int petCount = vm->getNpets();

  // check deCount input
  int deCount = petCount; // number of DEs, default
  int deCountFlag = 0;    // reset
  if (deCountArg != ESMC_NULL_POINTER){
    deCountFlag = 1;      // set
    deCount = *deCountArg;
  }

  // check deGrouping input
  int deGroupingFlag = 0; // reset
  int deGroupingCount = 0;
  if (present(deGrouping) && deGrouping->extent[0] > 0){
    deGroupingCount = deGrouping->extent[0];
    deGroupingFlag = 1;   // set
    if (deGroupingCount != deCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
        "Size of deGrouping does not match deCount", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
  }

  // check petList input
  int petListDeleteFlag = 0;  // reset
  int petListFlag = 0;        // reset
  int petListCount = 0;
  int *petList;
  if (present(petListArg ) && petListArg->extent[0] > 0){
    petListCount=petListArg->extent[0];
    petListFlag = 1;        // set
    petList = (int *)petListArg->array;
  }else{
    petListDeleteFlag = 1;  // set
    petListCount = petCount;
    petList = new int[petListCount];
    for (int i=0; i<petListCount; i++)
      petList[i] = i;
  }

  // construct petMap according to input
  if (!(deCountFlag | deGroupingFlag | petListFlag)){
    // the trivial case: default DELayout
    petMap = ESMC_NULL_POINTER;
    petMapCount = 0;
  }else{
    // need a real petMap
    petMapDeleteFlag = true; // set
    petMapCount = deCount;
    petMap = new int[petMapCount];
    if (!deGroupingFlag){
      // by default run cyclic through petList until all DEs are mapped
      for (int i=0, j=0; i<deCount; i++, j++){
        if (j==petListCount) j=0; // enforce cyclic j index
        petMap[i] = petList[j];
      }
    }else{
      // first reset petMap in a way that will allow to detect errors in Stride
      for (int i=0; i<deCount; i++)
        petMap[i] = -1; // initialize
      // run cyclic through petList and fill petMap according to deGrouping
      int j=0;  // reset
      for (int i=0; i<deCount; i++){
        if (j==petListCount) j=0; // enforce cyclic j index
        if (petMap[i] == -1){
          // this petMap entry has not been filled yet
          petMap[i] = petList[j];
          // find all DEs in this deGroup and fill with same PET
          int deGroup = deGrouping->array[i];
          if (deGroup != -1)
            for (int k=i+1; k<deCount; k++)
              if (deGrouping->array[k] == deGroup)
               petMap[k] = petList[j];
          ++j;
        }
      }
    }

    // start cleanup
#if 0
    if (deStrideBlockDeleteFlag){
      for (int i=0; i<deStrideBlockCount; i++)
        delete [] deStrideBlock[i];
      delete [] deStrideBlock;
    }
#endif
  }

  // allocate the new DELayout object and construct the inside
  DELayout *delayout;
  try{
    delayout = new DELayout(vm);  // specific VM, or default if vm==NULL
    localrc = delayout->construct(vm, pinFlag, petMap, petMapCount);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      if (petMapDeleteFlag) delete [] petMap;
      delayout->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
      return ESMC_NULL_POINTER;
    }
  }catch(...){
    // allocation error
    ESMC_LogDefault.MsgAllocError("for new DELayout.", ESMC_CONTEXT, rc);
    if (petMapDeleteFlag) delete [] petMap;
    delayout->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);  // mark invalid
    return ESMC_NULL_POINTER;
  }

  // final cleanup
  if (petMapDeleteFlag) delete [] petMap;
  if (petListDeleteFlag) delete [] petList;

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return delayout;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::create() - deprecated"
//BOPI
// !IROUTINE:  ESMCI::DELayout::create - deprecated
//
// !INTERFACE:
DELayout *DELayout::create(
//
// !RETURN VALUE:
//    DELayout * to newly allocated DELayout object. - deprecated
//
// !ARGUMENTS:
//
  VM &vm,                   // reference to ESMCI::VM object
  int *deCountArg,          // number of DEs
  int ndim,                 // number of dimensions
  int *DEtoPET,             // DEtoPET list
  int len,                  // number of DEs in DEtoPET list
  ESMC_Logical *cyclic_opt, // cyclic boundary option
  int *rc){                 // return code
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  DELayout *layout;
  // deal with optional variables
  ESMC_Logical cyclic = ESMF_FALSE;
  if (cyclic_opt != ESMC_NULL_POINTER)
    cyclic = *cyclic_opt;

  // CAUTION: todo: THIS IS A _NASTY_ HACK to make things happy on higher levels
  // that rely on DELayout to be _always 2D! Here I promote a 1D layout request
  // to 2D: N x 1. I write a message to LogErr to make people aware of this!!!
  vector<int> deCountArgHelper(2);
  if (ndim==0){
    // ESMC_LogDefault.Write("Promoting 1D DELayout to 2D",
    //   ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    ndim = 2;
    deCountArg = &(deCountArgHelper[0]);
    deCountArg[0] = vm.getNpets();
    deCountArg[1] = 1;
  }
  if (ndim==1){
    // ESMC_LogDefault.Write("Promoting 1D DELayout to 2D",
    //  ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    ndim = 2;
    int firstDEdim = deCountArg[0];
    deCountArg = &(deCountArgHelper[0]);
    deCountArg[0] = firstDEdim;
    deCountArg[1] = 1;
  }


  // decide whether this is a 1D or an ND layout
  if (ndim==0){
    // special case of a 1D layout where deCount will equal petCount
    try {
      layout = new DELayout;
      localrc = layout->construct1D(vm, 0, DEtoPET, len, cyclic);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      // return successfully
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return layout;
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.MsgAllocError("for new DELayout.", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
  }else if(ndim==1){
    try {
      layout = new DELayout;
      localrc = layout->construct1D(vm, *deCountArg, DEtoPET, len, cyclic);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      // return successfully
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return layout;
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.MsgAllocError("for new DELayout.", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
  }else{
    try {
      layout = new DELayout;
      localrc = layout->constructND(vm, deCountArg, ndim, DEtoPET, len, cyclic);
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc)) return ESMC_NULL_POINTER;
      // return successfully
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return layout;
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.MsgAllocError("for new DELayout.", ESMC_CONTEXT, rc);
      return ESMC_NULL_POINTER;
    }
  }
  return ESMC_NULL_POINTER; // fall through
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::destroy()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::destroy
//
// !INTERFACE:
int DELayout::destroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  DELayout **delayout,          // in - DELayout to destroy
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
  if (delayout == ESMC_NULL_POINTER || *delayout == ESMC_NULL_POINTER){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid pointer to DELayout", ESMC_CONTEXT, &rc);
    return rc;
  }

  try{
    // destruct DELayout object
    (*delayout)->destruct();
    // mark as invalid object
    (*delayout)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  }catch(int localrc){
    // catch standard ESMF return code
    ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc);
    return rc;
  }catch(...){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "Caught exception", ESMC_CONTEXT, &rc);
    return rc;
  }

  // optionally delete the complete object and remove from garbage collection
  if (noGarbage){
    VM::rmObject(*delayout); // remove object from garbage collection
    delete (*delayout);      // completely delete the object, free heap
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
#define ESMC_METHOD "ESMCI::DELayout::construct()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::construct
//
// !INTERFACE:
int DELayout::construct(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VM *vmArg,                    // (in) VM context
  ESMC_Pin_Flag *pinFlagArg,    // (in) type of resources DEs are pinned to
  int *petMap,                  // (in) pointer to petMap list
  int petMapCount               // (in) number of element in petMap
  ){
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_DELayout object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // by default pin DEs to PETs
  if (pinFlagArg == ESMC_NULL_POINTER)
    pinFlag = ESMF_PIN_DE_TO_PET;
  else
    pinFlag = *pinFlagArg;

  // by default use the currentVM for vm
  if (vmArg == ESMC_NULL_POINTER){
    vmArg = VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;
  }

  // query the VM
  int petCount = vmArg->getPetCount();
  int localPet = vmArg->getLocalPet();
  int localVas = vmArg->getVas(localPet);

  // by default use a sequential 1-to-1 petMap
  bool petMapDeleteFlag = false; // reset
  if (petMap == ESMC_NULL_POINTER || petMapCount == 0){
    petMapDeleteFlag = true; // set
    petMapCount = petCount;
    petMap = new int[petMapCount];
    for (int i=0; i<petMapCount; i++)
      petMap[i] = i;
  }

  // set the members of the DELayout
  oldstyle = 0;           // while the old style delayout is still supported
  vm = vmArg;             // pointer to the VM this delayout is constructed for
  deCount = petMapCount;  // number of DEs in the delayout
  deInfoList = new de_type[deCount];// alloc as many elements as there are DEs
  // set the de specific information
  for (int i=0; i<deCount; i++){
    deInfoList[i].de = i;                // by default start at 0
    deInfoList[i].pet = petMap[i];
    deInfoList[i].vas = vmArg->getVas(petMap[i]);
  }

  // clean up petMap if necessary
  if (petMapDeleteFlag) delete [] petMap;

  // determine if PETs in this layout are valid and if it is 1-to-1 or not
  int *petFlag = new int[petCount];
  for (int i=0; i<petCount; i++)
    petFlag[i] = 0; // reset
  for (int i=0; i<deCount; i++){
    int pet = deInfoList[i].pet;
    // the following works because PETs in VM must be contiguous & start at zero
    if (pet < 0 || pet >= petCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
        "DE to PET mapping is invalid", ESMC_CONTEXT, &rc);
      delete [] deInfoList;
      delete [] petFlag;
      return rc;
    }
    ++petFlag[pet];
  }
  oneToOneFlag = ESMF_TRUE; // set
  for (int i=0; i<petCount; i++)
    if (petFlag[i]!=1) oneToOneFlag = ESMF_FALSE; // reset
  delete [] petFlag;

  // fill PET-local part of layout object
  deList = new int[deCount];
  localDeCount = 0;               // reset local de count
  for (int i=0; i<deCount; i++){
    if (deInfoList[i].pet == localPet){
      deList[i] = localDeCount; // set to local DE
      ++localDeCount;
    }else
      deList[i] = -1;           // indicate not a local DE
  }
  localDeToDeMap = new int[localDeCount];  // allocate space to hold local DEs
  int j=0;
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].pet == localPet){
      localDeToDeMap[j]=i;
      ++j;
    }

  // fill VAS-local part of layout object
  vasLocalDeCount = 0;               // reset vas-local de count
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].vas == localVas) ++vasLocalDeCount;
  vasLocalDeToDeMap = new int[vasLocalDeCount];  // vas-local de id list
  j=0;
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].vas == localVas){
      vasLocalDeToDeMap[j]=i;
      ++j;
    }

  // setup work queue
  localServiceOfferCount = new int[vasLocalDeCount];
  serviceMutexFlag = new int[vasLocalDeCount];
  for (int i=0; i<vasLocalDeCount; i++){
    localServiceOfferCount[i] = 0;  // reset
    serviceMutexFlag[i] = 0;        // reset
  }
  serviceMutex = new VMK::ipmutex*[vasLocalDeCount];
  serviceOfferMutex = new VMK::ipmutex*[vasLocalDeCount];
  for (int i=0; i<vasLocalDeCount; i++)
    serviceOfferMutex[i] = vm->ipmutexallocate();  // obtain shared mutex
  if (vasLocalDeCount)  // don't use mutex if it's not there
    vm->ipmutexlock(serviceOfferMutex[0]);   // lock mutex
  int firstFlag;
  maxServiceOfferCount = (int *)
    vm->ipshmallocate(4*vasLocalDeCount*sizeof(int), &firstFlag);
  if (firstFlag)
    for (int i=0; i<vasLocalDeCount; i++)
      maxServiceOfferCount[4*i] = 0; // reset:  step 4, better shared mem perf
  for (int i=0; i<vasLocalDeCount; i++)
    serviceMutex[i] = vm->ipmutexallocate();  // obtain shared mutex
  if (vasLocalDeCount)  // don't use mutex if it's not there
    vm->ipmutexunlock(serviceOfferMutex[0]); // unlock mutex

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::construct1D() - deprecated"
//BOPI
// !IROUTINE:  ESMCI::DELayout::construct1D - deprecated
//
// !INTERFACE:
int DELayout::construct1D(VM &vmArg, int deCountArg,
  int *DEtoPET, int len, ESMC_Logical cyclic){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new DELayout
//     - deprecated
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  oldstyle = 1;           // while the old style delayout is still supported
  vm = &vmArg;                       // set the pointer onto this VM instance
  int npets =  vm->getNpets(); // get number of PETs
  if (deCountArg==0){
    // this will be a 1:1 Layout
    deCount = npets;                   // number of DEs to be the same as PETs
  }else{
    // number of DEs has been supplied
    deCount = deCountArg;
  }
  deInfoList = new de_type[deCount];   // allocate as many DEs as there are PETs
  // uniquely label the DEs in the layout
  for (int i=0; i<deCount; i++){
    deInfoList[i].de = i;                // default is to use basis zero
  }
  // now define connectivity between the DEs
  if (deCount>1){
    for (int i=0; i<deCount; i++){
      if (i==0){
        if (cyclic==ESMF_TRUE){
          deInfoList[i].nconnect = 2;
          deInfoList[i].connect_de = new int[2];
          deInfoList[i].connect_w  = new int[2];
          deInfoList[i].connect_de[0] = deCount-1;
          deInfoList[i].connect_w[0] = DELAYOUT_CWGHT_NORMAL;
          deInfoList[i].connect_de[1] = 1;
          deInfoList[i].connect_w[1] = DELAYOUT_CWGHT_NORMAL;
        }else{
          deInfoList[i].nconnect = 1;
          deInfoList[i].connect_de = new int[1];
          deInfoList[i].connect_w  = new int[1];
          deInfoList[i].connect_de[0] = 1;
          deInfoList[i].connect_w[0] = DELAYOUT_CWGHT_NORMAL;
        }
      }else if (i==deCount-1){
        if (cyclic==ESMF_TRUE){
          deInfoList[i].nconnect = 2;
          deInfoList[i].connect_de = new int[2];
          deInfoList[i].connect_w  = new int[2];
          deInfoList[i].connect_de[0] = i-1;
          deInfoList[i].connect_w[0] = DELAYOUT_CWGHT_NORMAL;
          deInfoList[i].connect_de[1] = 0;
          deInfoList[i].connect_w[1] = DELAYOUT_CWGHT_NORMAL;
        }else{
          deInfoList[i].nconnect = 1;
          deInfoList[i].connect_de = new int[1];
          deInfoList[i].connect_w  = new int[1];
          deInfoList[i].connect_de[0] = 0;
          deInfoList[i].connect_w[0] = DELAYOUT_CWGHT_NORMAL;
        }
      }else{
        deInfoList[i].nconnect = 2;
        deInfoList[i].connect_de = new int[2];
        deInfoList[i].connect_w  = new int[2];
        deInfoList[i].connect_de[0] = i-1;
        deInfoList[i].connect_w[0] = DELAYOUT_CWGHT_NORMAL;
        deInfoList[i].connect_de[1] = i+1;
        deInfoList[i].connect_w[1] = DELAYOUT_CWGHT_NORMAL;
      }
    }
  } else  {
     deInfoList[0].nconnect = 1;
     deInfoList[0].connect_de = new int[1];
     deInfoList[0].connect_w  = new int[1];
     deInfoList[0].connect_de[0] = 0;
     deInfoList[0].connect_w[0] = DELAYOUT_CWGHT_NORMAL;
  }
        
  // Setup the dimensionality and coordinates of this layout. This information
  // is only kept for external use!
  ndim = 1; // this is a 1D logical rectangular routine
  logRectFlag = ESMF_TRUE;
  dims = new int[ndim];
  dims[0] = deCount;
  for (int i=0; i<deCount; i++){
    deInfoList[i].coord = new int[ndim];
    deInfoList[i].coord[0] = i;
  }
  // DE-to-PET mapping
  if (len==deCount){
    // DEtoPET mapping has been provided externally
    for (int i=0; i<deCount; i++)
      deInfoList[i].pet = DEtoPET[i];   // copy the mapping
  }else{
    // Use the mapper algorithm to find good DE-to-PET mapping
    ESMC_DELayoutFindDEtoPET(npets);
  }
  // Issue warning if this is not a 1:1 layout. Do this because higher levels
  // of ESMF are written with 1:1 in mind.
  // TODO: remove this warning once all of ESMF accepts the more general case
  // of multiple DEs per PET.
  if (oneToOneFlag == ESMF_FALSE){
    ESMC_LogDefault.Write("A layout without 1:1 DE:PET mapping was"
      " created! This may cause problems in higher layers of ESMF!",
      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
  }
  // Issue warning if this is not logically rectangular
  // TODO: remove this warning when non logRect layouts o.k.
  if (logRectFlag == ESMF_FALSE){
    ESMC_LogDefault.Write("A non logRect layout was"
      " created! This may cause problems in higher layers of ESMF!",
      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
  }
  // Fill local part of layout object
  int mypet = vm->getMypet();    // get my PET id
  ESMC_DELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct VAS.
  for (int i=0; i<deCount; i++)
    deInfoList[i].vas = vm->getVas(deInfoList[i].pet);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::constructND() - deprecated"
//BOPI
// !IROUTINE:  ESMCI::DELayout::constructND - deprecated
//
// !INTERFACE:
int DELayout::constructND(VM &vmArg, int *deCountArg, int nndim,
  int *DEtoPET, int len, ESMC_Logical cyclic){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new DELayout
//     - deprecated
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  oldstyle = 1;           // while the old style delayout is still supported
  vm = &vmArg;                       // set the pointer onto this VM instance
  int npets =  vm->getNpets(); // get number of PETs
  ndim = nndim; // set the number of dimensions
  // this is a N-dim logical rectangular routine
  logRectFlag = ESMF_TRUE;
  dims = new int[ndim];
  for (int i=0; i<ndim; i++)
    dims[i] = deCountArg[i];
  // determine how many DEs there are in this layout
  deCount = 1;
  for (int i=0; i<nndim; i++)
    deCount *= deCountArg[i];
  deInfoList = new de_type[deCount]; // allocate as many DEs as there are PETs
  // uniquely label the DEs in the layout
  for (int i=0; i<deCount; i++){
    deInfoList[i].de = i;                // default is to use basis zero
  }
  // Setup the dimensionality and coordinates of this layout. This information
  // is only kept for external use!
  for (int i=0; i<deCount; i++){
    deInfoList[i].coord = new int[ndim];
  }
  for (int j=0; j<ndim; j++)
    deInfoList[0].coord[j] = 0;
  for (int i=1; i<deCount; i++){
    int carryover = 1;
    for (int j=0; j<ndim; j++){
      deInfoList[i].coord[j] = deInfoList[i-1].coord[j] + carryover;
      if (deInfoList[i].coord[j]==deCountArg[j]){
        deInfoList[i].coord[j] = 0;
        carryover = 1;
      }else{
        carryover=0;
      }
    }
  }
  // TODO: define connectivity between the DEs
  // For now don't connect any of the DEs
  // however even without real connections the connect_de and connect_w arrays
  // must be valid in order for the delete method to function correctly!
  for (int i=0; i<deCount; i++){
    deInfoList[i].nconnect = 0;
    deInfoList[i].connect_de = new int[1];
    deInfoList[i].connect_w  = new int[1];
  }
  // DE-to-PET mapping
  if (len==deCount){
    // DEtoPET mapping has been provided externally
    for (int i=0; i<deCount; i++)
      deInfoList[i].pet = DEtoPET[i];   // copy the mapping
    // nsc - if deCount is = npets, go ahead and set the 1:1 flag
    // even if the user supplied the mapping.   6dec04
    // TODO: gjt - this needs a bit more consideration than this, just
    // deCount == npets alone does not indicate 1:1!
      if (deCount==npets) // 1:1 layout
        oneToOneFlag = ESMF_TRUE;
      else
        oneToOneFlag = ESMF_FALSE;  // if there are more or less DEs than PETs
  }else{
    // Use the mapper algorithm to find good DE-to-PET mapping
    ESMC_DELayoutFindDEtoPET(npets);
  }
  // Issue warning if this is not a 1:1 layout. Do this because higher levels
  // of ESMF are written with 1:1 in mind.
  // TODO: remove this warning once all of ESMF accepts the more general case
  // of multiple DEs per PET.
  if (oneToOneFlag == ESMF_FALSE){
    ESMC_LogDefault.Write("A layout without 1:1 DE:PET mapping was"
      " created! This may cause problems in higher layers of ESMF!",
      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
  }
  // Issue warning if this is not logically rectangular
  // TODO: remove this warning when non logRect layouts o.k.
  if (logRectFlag == ESMF_FALSE){
    ESMC_LogDefault.Write("A non logRect layout was"
      " created! This may cause problems in higher layers of ESMF!",
      ESMC_LOGMSG_WARN, ESMC_CONTEXT);
  }
  // Fill local part of layout object
  int mypet = vm->getMypet();    // get my PET id
  ESMC_DELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct VAS.
  for (int i=0; i<deCount; i++)
    deInfoList[i].vas = vm->getVas(deInfoList[i].pet);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::destruct()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::destruct
//
// !INTERFACE:
int DELayout::destruct(){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure of an DELayout object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

//fprintf(stderr, "DELayout::destruct\n");

  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    if (oldstyle){
      // oldstyle DELayout has several more allocations that need to be deleted
      for (int i=0; i<deCount; i++){
        delete [] deInfoList[i].connect_de;
        delete [] deInfoList[i].connect_w;
        delete [] deInfoList[i].coord;
      }
      if (logRectFlag == ESMF_TRUE)
        delete [] dims;
    }

    // oldstyle and newstyle DELayout alike must delete the following members
    delete [] deInfoList;
    delete [] localDeToDeMap;
    delete [] deList;

    if (!oldstyle){
      // this is only for newstyle DELayouts
      delete [] vasLocalDeToDeMap;
      delete [] localServiceOfferCount;
      delete [] serviceMutexFlag;
      if (vm!=NULL){
        vm->ipshmdeallocate(maxServiceOfferCount);
        for (int i=0; i<vasLocalDeCount; i++)
          vm->ipmutexdeallocate(serviceOfferMutex[i]);
        for (int i=0; i<vasLocalDeCount; i++)
          vm->ipmutexdeallocate(serviceMutex[i]);
      }
      delete [] serviceOfferMutex;
      delete [] serviceMutex;
    }
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

   //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutFillLocal() - deprecated"
//BOPI
// !IROUTINE:  ESMC_DELayoutFillLocal
//
// !INTERFACE:
int DELayout::ESMC_DELayoutFillLocal(int mypet){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Fill local part of layout object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  deList = new int[deCount];
  localDeCount = 0;               // reset local de count
  for (int i=0; i<deCount; i++){
    if (deInfoList[i].pet == mypet){
      deList[i] = localDeCount; // set to local DE
      ++localDeCount;
    }else
      deList[i] = -1;           // indicate not a local DE
  }
  localDeToDeMap = new int[localDeCount];  // allocate space to hold local DEs
  int j=0;
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].pet == mypet){
      localDeToDeMap[j]=i;
      ++j;
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
#define ESMC_METHOD "ESMCI::DELayout::getDEMatchDE()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::getDEMatchDE
//
// !INTERFACE:
int DELayout::getDEMatchDE(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int de,                       // in  - DE id of DE to be queried
  DELayout &layoutMatch,        // in  - layout to match against
  int *deMatchCount,            // out - number of matching DEs in layoutMatch
  int *deMatchList,             // out - list of matching DEs in layoutMatch
  int len_deMatchList           // in  - size of deMatchList
  )const{
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int *tempMatchList = new int[layoutMatch.deCount]; // maximum number of DEs
  int tempMatchCount = 0;
  int vasCompare = deInfoList[de].vas;
  int j=0;
  for (int i=0; i<layoutMatch.deCount; i++)
    if (layoutMatch.deInfoList[i].vas == vasCompare){
      tempMatchList[j] = i;
      ++j;
    }
  // now j is equal to the number of DEs in layoutMatch which operate in the
  // same virtual memory space as "de" does in this layout.
  if (deMatchCount != ESMC_NULL_POINTER)
    *deMatchCount = j;
  if (len_deMatchList >= j)
    for (int i=0; i<j; i++)
      deMatchList[i] = tempMatchList[i];
  else if (len_deMatchList != -1){
    // deMatchList argument was specified but its size is insufficient
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "deMatchList must be of size 'deMatchCount'", ESMC_CONTEXT, &rc);
    return rc;
  }

  // garbage collection
  delete [] tempMatchList;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::getDEMatchPET()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::getDEMatchPET
//
// !INTERFACE:
int DELayout::getDEMatchPET(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int de,                       // in  - DE id of DE to be matched
  VM &vmMatch,                  // in  - vm to match against
  int *petMatchCount,           // out - number of matching PETs in vmMatch
  int *petMatchList,            // out - list of matching PETs in vmMatch
  int len_petMatchList          // in  - size of petMatchList
  )const{
//
// !DESCRIPTION:
//    Match de in the current DELayout object against the PETs in the
//    provided vmMatch VM. Return number of matched PETs and a list of the
//    matching pet id's that operate in the same virtual address space in which
//    de lies.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int npets = vmMatch.getNpets();  // maximum number of PETs in vmMatch
  int *tempMatchList = new int[npets];
  int tempMatchCount = 0;
  int vasCompare = deInfoList[de].vas; // this is the virtual address space id
  int j=0;
  for (int i=0; i<npets; i++)
    if (vmMatch.getVas(i) == vasCompare){
      tempMatchList[j] = i;
      ++j;
    }
  // now j is equal to the number of PETs in vmMatch which operate in the
  // same virtual address space as "de" does in this layout.
  if (petMatchCount != ESMC_NULL_POINTER)
    *petMatchCount = j;
  if (len_petMatchList >= j)
    for (int i=0; i<j; i++)
      petMatchList[i] = tempMatchList[i];
  else if (len_petMatchList != -1){
    // petMatchList argument was specified but its size is insufficient
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE,
      "petMatchList must be of size 'petMatchCount'", ESMC_CONTEXT, &rc);
    return rc;
  }

  // garbage collection
  delete [] tempMatchList;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::getDeprecated()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::getDeprecated
//
// !INTERFACE:
int DELayout::getDeprecated(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int  *deCountArg,           // out - Total number of DEs
  int  *ndim,                 // out - Number of dimensions in coordinate tuple
  int  *localDeCountArg,      // out - number of DEs for my PET instance
  int  *localDeToDeMapArg,    // out - list DEs for my PET instance
  int  len_localDeToDeMap,    // in  - number of elements in localDeToDeMapArg
  int *localDe,               // out - local DE id for 1-to-1 layouts
  ESMC_Logical *oneToOneFlag, // out - 1-to-1 layout flag
  ESMC_Logical *logRectFlag,  // out - logical rectangular layout flag
  int  *deCountPerDim,        // out - list of dimension sizes
  int  len_deCountPerDim      // in  - number of elements in deCountPerDim list
  )const{
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (deCountArg != ESMC_NULL_POINTER)
    *deCountArg = deCount;

  if (ndim != ESMC_NULL_POINTER){
    if (!oldstyle){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
      return rc;
    }else
      *ndim = this->ndim;
  }

  if (localDeCountArg != ESMC_NULL_POINTER)
    *localDeCountArg = localDeCount;

  if (len_localDeToDeMap >= localDeCount)
    for (int i=0; i<localDeCount; i++)
      localDeToDeMapArg[i] = localDeToDeMap[i];

  if (localDe != ESMC_NULL_POINTER){
    if (!oldstyle){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
      return rc;
    }else{
      if (localDeCount >= 1)  // at least 1 DE on this PET -> return 1st
        *localDe = localDeToDeMap[0];
      else{
        *localDe = -1;    // mark invalid
        return ESMC_RC_CANNOT_GET;
      }
    }
  }

  if (oneToOneFlag != ESMC_NULL_POINTER)
    *oneToOneFlag = this->oneToOneFlag;

  if (logRectFlag != ESMC_NULL_POINTER){
    if (!oldstyle){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
      return rc;
    }else
      *logRectFlag = this->logRectFlag;
  }

  if (len_deCountPerDim >= this->ndim){
    if (!oldstyle){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
      return rc;
    }else{
      if (this->logRectFlag == ESMF_TRUE){
        for (int i=0; i<this->ndim; i++)
          deCountPerDim[i] = dims[i];
        for (int i=this->ndim; i<len_deCountPerDim; i++)
          deCountPerDim[i] = 1;
      }else{
        for (int i=0; i<len_deCountPerDim; i++)
          deCountPerDim[i] = -1;    // mark invalid
        return ESMC_RC_CANNOT_GET;
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
#define ESMC_METHOD "ESMCI::DELayout::getDELocalInfo() - deprecated"
//BOPI
// !IROUTINE:  ESMCI::DELayout::getDELocalInfo
//
// !INTERFACE:
int DELayout::getDELocalInfo(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int  de,                // in  - DE id of DE to be queried
  int  *DEcoord,          // out - DE's coordinate tuple
  int  len_coord,         // in  - dimensions in DEcoord
  int  *DEcde,            // out - DE's connection table
  int  len_cde,           // in  - dimensions in DEcde
  int  *DEcw,             // out - DE's connection weight table
  int  len_cw,            // in  - dimensions in DEcw
  int  *nDEc,             // out - DE's number of connections
  int  *vas               // out - vas for this DE
  )const{
//
// !DESCRIPTION:
//    Get information about a DELayout object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int i;
  if (de < 0 || de >= deCount){
    // de out of range
    return ESMC_RC_ARG_OUTOFRANGE;
  }
  if (len_coord >= ndim) {
    for (i=0; i<ndim; i++)
      DEcoord[i] = deInfoList[de].coord[i];
    for (i=ndim; i<len_coord; i++)
      DEcoord[i] = 0;
  }
  if (len_cde >= deInfoList[de].nconnect) {
    for (i=0; i<deInfoList[de].nconnect; i++)
      DEcde[i] = deInfoList[de].connect_de[i];
    for (i=deInfoList[de].nconnect; i<len_cde; i++)
      DEcde[i] = 0;
  }
  if (len_cw >= deInfoList[de].nconnect) {
    for (i=0; i<deInfoList[de].nconnect; i++)
      DEcw[i] = deInfoList[de].connect_w[i];
    for (i=deInfoList[de].nconnect; i<len_cw; i++)
      DEcw[i] = 0;
  }
  if (nDEc != ESMC_NULL_POINTER)
    *nDEc = deInfoList[de].nconnect;
  if (vas != ESMC_NULL_POINTER)
    *vas = deInfoList[de].vas;

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
#define ESMC_METHOD "ESMCI::DELayout::print()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::print
//
// !INTERFACE:
int DELayout::print()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of DELayout object
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      " - 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
    return rc;
  }
  // print info about the DELayout object
  printf("--- ESMCI::DELayout::print() start ---\n");
  if (oldstyle){
    printf("This is an OLDSTYLE DELayout!\n");
    printf("--- global DELayout section ---\n");
    printf("vm = %p\n", vm);
    printf("oneToOneFlag = ");
    if (oneToOneFlag == ESMF_TRUE)
      printf("ESMF_TRUE\n");
    else
      printf("ESMF_FALSE\n");
    printf("deCount = %d\n", deCount);
    for (int i=0; i<deCount; i++){
      printf("  deInfoList[%d]: de=%d, pet=%d, vas=%d, nconnect=%d\n", i,
        deInfoList[i].de, deInfoList[i].pet, deInfoList[i].vas,
        deInfoList[i].nconnect);
      for (int j=0; j<deInfoList[i].nconnect; j++)
        printf("      connect_de[%d]=%d, weight=%d\n", j,
          deInfoList[i].connect_de[j], deInfoList[i].connect_w[j]);
    }
    printf("--- local DELayout section ---\n");
    printf("localDeCount=%d\n", localDeCount);
    for (int i=0; i<localDeCount; i++)
      printf("  localDeToDeMap[%d]=%d\n", i, localDeToDeMap[i]);
    printf("ndim = %d\n", ndim);
    for (int i=0; i<deCount; i++){
      printf("[%d]: ", i);
      int j;
      for (j=0; j<ndim-1; j++)
        printf("%d, ", deInfoList[i].coord[j]);
      printf("%d\n", deInfoList[i].coord[j]);
    }
  }else{
    printf("This is a NEWSTYLE DELayout!\n");
    printf("--- global DELayout section ---\n");
    printf("vm = %p\n", vm);
    printf("oneToOneFlag = ");
    if (oneToOneFlag == ESMF_TRUE)
      printf("ESMF_TRUE\n");
    else
      printf("ESMF_FALSE\n");
    printf("pinFlag = ");
    if (pinFlag == ESMF_PIN_DE_TO_PET)
      printf("ESMF_PIN_DE_TO_PET\n");
    else if (pinFlag == ESMF_PIN_DE_TO_VAS)
      printf("ESMF_PIN_DE_TO_VAS\n");
    else
      printf(" ...unknown... \n");
    printf("deCount = %d\n", deCount);
    for (int i=0; i<deCount; i++){
      printf("  deInfoList[%d]: de=%d, pet=%d, vas=%d\n", i,
        deInfoList[i].de, deInfoList[i].pet, deInfoList[i].vas);
    }
    printf("--- PET-local DELayout section ---\n");
    printf("localDeCount=%d\n", localDeCount);
    for (int i=0; i<localDeCount; i++)
      printf("  localDeToDeMap[%d]=%d\n", i, localDeToDeMap[i]);
    printf("--- VAS-local DELayout section ---\n");
    printf("vasLocalDeCount=%d\n", vasLocalDeCount);
    for (int i=0; i<vasLocalDeCount; i++)
      printf("  vasLocalDeToDeMap[%d]=%d\n", i, vasLocalDeToDeMap[i]);
  }
  printf("--- ESMCI::DELayout::print() end ---\n");

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::validate()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::validate
//
// !INTERFACE:
int DELayout::validate()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Validate details of DELayout object
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
// serialize() and deserialize()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::serialize()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::serialize - Turn delayout into a byte stream
//
// !INTERFACE:
int DELayout::serialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length; realloc'd here if needed
  int *offset,           // inout - original offset, updated to point
                         //         to first free byte after current obj info
  ESMC_InquireFlag inquireflag) const { // in - inquiry flag
//
// !DESCRIPTION:
//    Turn info in delayout class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int i, j;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  ESMC_Pin_Flag *dp;
  de_type *dep;
  int r;

  // Check if buffer has enough free memory to hold object
  if (inquireflag != ESMF_INQUIREONLY){
    if ((*length - *offset) < (int)sizeof(DELayout)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add a DELayout object", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // Serialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ESMC_AttReconcileFlag attreconflag = ESMC_ATTRECONCILE_OFF;
  localrc = this->ESMC_Base::ESMC_Serialize(buffer, length, offset,
    attreconflag, inquireflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // Serialize the DELayout internal data members
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  cp = (char *)(buffer + *offset);
  ip = (int *)cp;
  if (inquireflag != ESMF_INQUIREONLY){
    *ip++ = deCount;
    *ip++ = oldstyle;
    if (oldstyle){
      // ndim must be available before decoding the next loop, so it has
      // to be sent now.
      *ip++ = ndim;
    }
  }else{
    ip += oldstyle ? 3 : 2;
  }

  if (!oldstyle){
    dp = (ESMC_Pin_Flag *)ip;
    if (inquireflag != ESMF_INQUIREONLY)
      *dp++ = pinFlag;
    else
      dp++;
    ip = (int *)dp;
  }

  if (inquireflag != ESMF_INQUIREONLY){
    for (i=0, dep=deInfoList; i<deCount; i++, dep++){
      *ip++ = dep->de;
      *ip++ = dep->pet;
      *ip++ = dep->vas;
      if (oldstyle){
        *ip++ = dep->nconnect;
        for (j=0; j<dep->nconnect; j++) {
          *ip++ = dep->connect_de[j];
          *ip++ = dep->connect_w[j];
        }
        for (j=0; j<ndim; j++)
          *ip++ = dep->coord[j];
      }
    }
  }else{
    ip += 3*deCount;
    if (oldstyle){
      for (i=0, dep=deInfoList; i<deCount; i++, dep++){
        ip += 1 + 2*dep->nconnect + ndim;
      }
    }
  }

  // this has to come before dims, since they are not allocated unless
  // logRectFlag is true.
  lp = (ESMC_Logical *)ip;
  if (inquireflag != ESMF_INQUIREONLY){
    *lp++ = oneToOneFlag;
    if (oldstyle)
      *lp++ = logRectFlag;
  }else{
    lp += oldstyle ? 1 : 2;
  }

  ip = (int *)lp;
  if (oldstyle){
    if (logRectFlag == ESMF_TRUE){
      if (inquireflag != ESMF_INQUIREONLY){
        for (i=0; i<ndim; i++)
          *ip++ = dims[i];
      }else{
        ip += ndim;
      }
    }
  }

  cp = (char *)ip;

  *offset = (cp - buffer);

  if (inquireflag == ESMF_INQUIREONLY){
    if (*offset < (int)sizeof(DELayout))
      *offset = sizeof(DELayout);
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::deserialize - Turn a byte stream into delayout
//
// !INTERFACE:
DELayout *DELayout::deserialize(
//
// !RETURN VALUE:
//    int return code
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

  DELayout *a = new DELayout(-1); // prevent baseID counter increment
  int i, j;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  ESMC_Pin_Flag *dp;
  de_type *dep;
  int r;

  // Deserialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ESMC_AttReconcileFlag attreconflag = ESMC_ATTRECONCILE_OFF;
  localrc = a->ESMC_Base::ESMC_Deserialize(buffer,offset,attreconflag);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return NULL;

  // Deserialize the DELayout internal data members
  a->vm = NULL;  // this must be NULL
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  cp = (char *)(buffer + *offset);
  ip = (int *)cp;
  a->deCount = *ip++;
  a->oldstyle = *ip++;

  if (a->oldstyle){
    // ndim must be known before the loop below
    a->ndim = *ip++;
  }
  if (!a->oldstyle){
    dp = (ESMC_Pin_Flag *)ip;
    a->pinFlag = *dp++;
    ip = (int *)dp;
  }

  a->deInfoList = new de_type[a->deCount];
  for (i=0, dep=a->deInfoList; i<a->deCount; i++, dep++) {
    dep->de = *ip++;
    dep->pet = *ip++;
    dep->vas = *ip++;
    if (a->oldstyle){
      dep->nconnect = *ip++;

      dep->connect_de = new int[dep->nconnect];
      dep->connect_w = new int[dep->nconnect];
      dep->coord = new int[a->ndim];
      for (j=0; j<dep->nconnect; j++) {
        dep->connect_de[j] = *ip++;
        dep->connect_w[j] = *ip++;
      }
      for (j=0; j<a->ndim; j++)
        dep->coord[j] = *ip++;
    }
  }

  a->localDeCount = 0;  // proxy objects don't have local DEs
  a->localDeToDeMap = new int[a->localDeCount];
  a->deList = new int[a->deCount];
  for (int i=0; i<a->deCount; i++)
    a->deList[i] = -1;                         // indicate not a local DE
  a->vasLocalDeCount = 0;  // proxy objects don't have local DEs
  a->vasLocalDeToDeMap = new int[a->vasLocalDeCount];
  a->localServiceOfferCount = new int[a->vasLocalDeCount];
  a->serviceMutexFlag = new int[a->vasLocalDeCount];
  a->serviceMutex = new VMK::ipmutex*[a->vasLocalDeCount];
  a->serviceOfferMutex = new VMK::ipmutex*[a->vasLocalDeCount];

  // decode flags first, because dims is not sent unless logRectFlag is true.
  lp = (ESMC_Logical *)ip;
  a->oneToOneFlag = *lp++;
  if (a->oldstyle)
    a->logRectFlag = *lp++;

  ip = (int *)lp;
  if (a->oldstyle){
    if (a->logRectFlag == ESMF_TRUE) {
      a->dims = new int[a->ndim];
      for (i=0; i<a->ndim; i++)
          a->dims[i] = *ip++;
    }else
      a->dims = NULL;
  }

  cp = (char *)ip;

  *offset = (cp - buffer);

  // return successfully
  rc = ESMF_SUCCESS;
  return a;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::serviceOffer()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::serviceOffer
//
// !INTERFACE:
ServiceReply DELayout::serviceOffer(
//
// !RETURN VALUE:
//    ServiceReply reply to serviceOffer.
//
// !ARGUMENTS:
//
  int de,                   // in  - de for which service is offered
  int *rc){                 // out - optional return code
//
// !DESCRIPTION:
//    Calling PET offers service for {\tt de} in DELayout. The
//    offer is either accepted or denied.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  // initialize the reply
  ServiceReply reply = SERVICEREPLY_DENY; // reset

  int localPet = vm->getMypet();
  int localVas = vm->getVas(localPet);

  // DE to PET pinning is more restrictive -> check first
  if (pinFlag == ESMF_PIN_DE_TO_PET){
    // search for de in localDeToDeMap
    int i;
    for (i=0; i<localDeCount; i++)
      if (localDeToDeMap[i] == de) break;
    if (i==localDeCount){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_ARG_WRONG;
//      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
//        "Specified DE is not in localDeToDeMap", ESMC_CONTEXT, rc);
      return reply;
    }
  }
  int ii;
  // search for de in vasLocalDeToDeMap
  for (ii=0; ii<vasLocalDeCount; ii++)
    if (vasLocalDeToDeMap[ii] == de) break;
  if (ii==vasLocalDeCount){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_ARG_WRONG;
//    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
//      "Specified DE is not in vasLocalDeToDeMap", ESMC_CONTEXT, rc);
    return reply;
  }

  // ...de was found in local list
  ++localServiceOfferCount[ii];
  vm->ipmutexlock(serviceOfferMutex[ii]);   // lock mutex
  if (localServiceOfferCount[ii] > maxServiceOfferCount[4*ii]){
    reply = SERVICEREPLY_ACCEPT; // accept this PET's service offer
    ++maxServiceOfferCount[4*ii];
  }
  vm->ipmutexunlock(serviceOfferMutex[ii]); // unlock mutex

  if (reply==SERVICEREPLY_ACCEPT){
    vm->ipmutexlock(serviceMutex[ii]);  // lock service mutex
    serviceMutexFlag[ii] = 1;               // set
  }

//  if (reply==SERVICEREPLY_ACCEPT)
//    printf("PET %d localServiceOfferCount for DE %d is %d: ACCEPT\n",
//      localPet, de, localServiceOfferCount[ii]);
//  else
//    printf("PET %d localServiceOfferCount for DE %d is %d: DENY\n",
//      localPet, de, localServiceOfferCount[ii]);


  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return reply;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::serviceComplete()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::serviceComplete
//
// !INTERFACE:
int DELayout::serviceComplete(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int de                    // in  - de for which service is complete
  ){
//
// !DESCRIPTION:
//    The PET whose serviceOffer() was accepted must call serviceComplete() in
//    order to close the service window for this DE.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int localPet = vm->getMypet();
  int localVas = vm->getVas(localPet);

  // DE to PET pinning is more restrictive -> check first
  if (pinFlag == ESMF_PIN_DE_TO_PET){
    // search for de in localDeToDeMap
    int i;
    for (i=0; i<localDeCount; i++)
      if (localDeToDeMap[i] == de) break;
    if (i==localDeCount){
//TODO: enable LogErr once it is thread-safe
rc=ESMC_RC_ARG_WRONG;
//      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
//        "Specified DE is not in localDeToDeMap", ESMC_CONTEXT, &rc);
      return rc;
    }
  }
  int ii;
  // search for de in vasLocalDeToDeMap
  for (ii=0; ii<vasLocalDeCount; ii++)
    if (vasLocalDeToDeMap[ii] == de) break;
  if (ii==vasLocalDeCount){
//TODO: enable LogErr once it is thread-safe
rc=ESMC_RC_ARG_WRONG;
//    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
//      "Specified DE is not in vasLocalDeToDeMap", ESMC_CONTEXT, &rc);
    return rc;
  }

  // ...de was found in local list
  if (!serviceMutexFlag[ii]){
//TODO: enable LogErr once it is thread-safe
rc=ESMC_RC_NOT_VALID;
//    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
//      "PET does not hold service mutex for specified", ESMC_CONTEXT, &rc);
    return rc;
  }

  // ...pet holds service mutex for de
  vm->ipmutexunlock(serviceMutex[ii]);  // unlock service mutex
  serviceMutexFlag[ii] = 0;                 // reset

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//----  DELayout communication calls are deprecated
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutCopy()"
//BOPI
// !IROUTINE:  ESMC_DELayoutCopy
//
// !INTERFACE:
int DELayout::ESMC_DELayoutCopy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,  // input array
  void *destdata, // output array
  int blen,       // size in bytes that need to be copied from src to dest
  int srcDE,      // input DE
  int destDE      // output DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // ensure this is a 1-to-1 delayout, if not bail out
  if (oneToOneFlag != ESMF_TRUE){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_IMPL,
      "Can only handle 1-to-1 DELayouts", ESMC_CONTEXT, &rc);
    return rc; // bail out
  }
  int mypet = vm->getMypet();
  int srcpet = deInfoList[srcDE].pet;      // PETid where srcDE lives
  int destpet = deInfoList[destDE].pet;    // PETid where destDE lives
  if (srcpet==mypet && destpet==mypet){
    // srcDE and destDE are on my PET
    memcpy(destdata, srcdata, blen);
  }else if (srcpet==mypet){
    // srcDE is on my PET, but destDE is on another PET
    vm->send(srcdata, blen, destpet);
  }else if (destpet==mypet){
    // destDE is on my PET, but srcDE is on another PET
    vm->recv(destdata, blen, srcpet);
  }
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutCopy()"
//BOPI
// !IROUTINE:  ESMC_DELayoutCopy
//
// !INTERFACE:
int DELayout::ESMC_DELayoutCopy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,    // input array
  void *destdata,   // output array
  int len,          // size in elements that need to be copied from src to dest
  ESMC_TypeKind_Flag dtk,// data type kind
  int srcDE,        // input DE
  int destDE        // output DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  int blen = len * ESMC_TypeKind_FlagSize(dtk);
  localrc = ESMC_DELayoutCopy(srcdata, destdata, blen, srcDE, destDE);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutExchange()"
//BOPI
// !IROUTINE:  ESMC_DELayoutExchange
//
// !INTERFACE:
int DELayout::ESMC_DELayoutExchange(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcData1,     // input array
  void *srcData2,     // input array
  void *dstData1,     // output array
  void *dstData2,     // output array
  int blen1,          // size in bytes to copy from srcData1 to dstData2
  int blen2,          // size in bytes to copy from srcData2 to dstData1
  int de1,            // de for data1
  int de2             // de for data2
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  if (de1<=de2){
    ESMC_DELayoutCopy(srcData1, dstData2, blen1, de1, de2);
    ESMC_DELayoutCopy(srcData2, dstData1, blen2, de2, de1);
  }else{
    ESMC_DELayoutCopy(srcData2, dstData1, blen2, de2, de1);
    ESMC_DELayoutCopy(srcData1, dstData2, blen1, de1, de2);
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutExchange()"
//BOPI
// !IROUTINE:  ESMC_DELayoutExchange
//
// !INTERFACE:
int DELayout::ESMC_DELayoutExchange(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcData1,     // input array
  void *srcData2,     // input array
  void *dstData1,     // output array
  void *dstData2,     // output array
  int len1,           // size in elements to copy from srcData1 to dstData2
  int len2,           // size in elements to copy from srcData2 to dstData1
  ESMC_TypeKind_Flag dtk1, // data type kind
  ESMC_TypeKind_Flag dtk2, // data type kind
  int de1,            // de for data1
  int de2             // de for data2
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  int blen1 = len1 * ESMC_TypeKind_FlagSize(dtk1);
  int blen2 = len2 * ESMC_TypeKind_FlagSize(dtk2);
  return ESMC_DELayoutExchange(srcData1, srcData2, dstData1, dstData2,
    blen1, blen2, de1, de2);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutBcast()"
//BOPI
// !IROUTINE:  ESMC_DELayoutBcast
//
// !INTERFACE:
int DELayout::ESMC_DELayoutBcast(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *data,     // data
  int blen,       // message size in bytes
  int rootDE      // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // very crude implementation of a layout wide bcast
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  for (int i=0; i<deCount; i++)
    ESMC_DELayoutCopy(data, data, blen, rootDE, i);
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutBcast()"
//BOPI
// !IROUTINE:  ESMC_DELayoutBcast
//
// !INTERFACE:
int DELayout::ESMC_DELayoutBcast(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *data,    // data
  int len,       // message size in elements
  ESMC_TypeKind_Flag dtk,// data type kind
  int rootDE      // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  int blen = len * ESMC_TypeKind_FlagSize(dtk);
  // very crude implementation of a layout wide bcast
  for (int i=0; i<deCount; i++)
    ESMC_DELayoutCopy(data, data, blen, rootDE, i);
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutScatter()"
//BOPI
// !IROUTINE:  ESMC_DELayoutScatter
//
// !INTERFACE:
int DELayout::ESMC_DELayoutScatter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,  // input array
  void *destdata, // output array
  int blen,       // message size in bytes
  int rootDE      // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // very crude implementation of a layout wide scatter
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  int mypet = vm->getMypet();
  if (deInfoList[rootDE].pet==mypet){
    // my PET holds the rootDE
    char *tempdata = (char *)srcdata;
    for (int i=0; i<deCount; i++){
      ESMC_DELayoutCopy((void *)tempdata, destdata, blen, rootDE, i);
      tempdata += blen;
    }
  }else{
    for (int i=0; i<deCount; i++)
      ESMC_DELayoutCopy(srcdata, destdata, blen, rootDE, i);
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutScatter()"
//BOPI
// !IROUTINE:  ESMC_DELayoutScatter
//
// !INTERFACE:
int DELayout::ESMC_DELayoutScatter(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,    // input array
  void *destdata,   // output array
  int len,          // message size in elements
  ESMC_TypeKind_Flag dtk,// data type kind
  int rootDE        // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  int blen = len * ESMC_TypeKind_FlagSize(dtk);
  return ESMC_DELayoutScatter(srcdata, destdata, blen, rootDE);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGather()"
//BOPI
// !IROUTINE:  ESMC_DELayoutGather
//
// !INTERFACE:
int DELayout::ESMC_DELayoutGather(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,  // input array
  void *destdata, // output array
  int blen,       // message size in bytes
  int rootDE      // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // very crude implementation of a layout wide gather
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  int mypet = vm->getMypet();
  if (deInfoList[rootDE].pet==mypet){
    // my PET holds the rootDE -> receive chunks from all other DEs
    char *tempdata = (char *)destdata;
    for (int i=0; i<deCount; i++){
      ESMC_DELayoutCopy(srcdata, (void *)tempdata, blen, i, rootDE);
      tempdata += blen;
    }
  }else{
    for (int i=0; i<deCount; i++)
      ESMC_DELayoutCopy(srcdata, destdata, blen, i, rootDE);
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGather()"
//BOPI
// !IROUTINE:  ESMC_DELayoutGather
//
// !INTERFACE:
int DELayout::ESMC_DELayoutGather(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,    // input array
  void *destdata,   // output array
  int len,          // message size in bytes
  ESMC_TypeKind_Flag dtk,// data type kind
  int rootDE        // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  int blen = len * ESMC_TypeKind_FlagSize(dtk);
  localrc = ESMC_DELayoutGather(srcdata, destdata, blen, rootDE);
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGatherV()"
//BOPI
// !IROUTINE:  ESMC_DELayoutGatherV
//
// !INTERFACE:
int DELayout::ESMC_DELayoutGatherV(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,  // input array
  void *destdata, // output array
  int *blen,      // array of message sizes in bytes for each DE
                  // - the PET that holds rootDE must provide all blen elementes
                  // - all other PETs only need to fill elements for their DEs
  int *bdestdispl,// displacement vector for destdata for each DE mes. in bytes
  int rootDE      // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // very crude implementation of a layout wide gather
  int localrc;
  int mypet = vm->getMypet();

  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  if (deInfoList[rootDE].pet==mypet){
    // my PET holds the rootDE -> receive chunks from all other DEs
    char *tempdata;
    for (int i=0; i<deCount; i++){
      tempdata = (char *)destdata + bdestdispl[i];
      ESMC_DELayoutCopy(srcdata, (void *)tempdata, blen[i], i, rootDE);
    }
  }else{
    for (int i=0; i<deCount; i++)
      ESMC_DELayoutCopy(srcdata, destdata, blen[i], i, rootDE);
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutGatherV()"
//BOPI
// !IROUTINE:  ESMC_DELayoutGatherV
//
// !INTERFACE:
int DELayout::ESMC_DELayoutGatherV(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  void *srcdata,  // input array
  void *destdata, // output array
  int *len,       // array of message sizes in elements for each DE
                  // - the PET that holds rootDE must provide all blen elementes
                  // - all other PETs only need to fill elements for their DEs
  int *destdispl, // displacement vector for destdata for each DE mes. in elem.
  ESMC_TypeKind_Flag dtk,// data type kind
  int rootDE      // root DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int rc;
  rc = ESMC_RC_NOT_IMPL;
  int *blen = new int[deCount];
  int *bdestdispl = new int[deCount];
  int dtk_size = ESMC_TypeKind_FlagSize(dtk);

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  for (int i=0; i<deCount; i++){
    blen[i] = len[i] * dtk_size;
    bdestdispl[i] = destdispl[i] * dtk_size;
  }
  rc =  ESMC_DELayoutGatherV(srcdata, destdata, blen, bdestdispl, rootDE);
  delete [] blen;
  delete [] bdestdispl;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutFindDEtoPET()"
//BOPI
// !IROUTINE:  ESMC_DELayoutFindDEtoPET
//
// !INTERFACE:
int DELayout::ESMC_DELayoutFindDEtoPET(int npets){
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Find best DE-to-PET mapping for the layout
//
//EOPI
//-----------------------------------------------------------------------------
// TODO: Use the connectivity info to find best DE-to-PET mapping!
// For now I simply connect in sequence...
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  if (deCount==npets){
    // 1:1 layout
    oneToOneFlag = ESMF_TRUE;
    for (int i=0; i<deCount; i++)
      deInfoList[i].pet = i;   // default 1:1 DE-to-PET mapping
  }else{
    // not a 1:1 layout
    oneToOneFlag = ESMF_FALSE;
    // first find how many DEs will be placed onto a certain PET
    int *ndepet = new int[npets];
    for (int i=0; i<npets; i++)
      ndepet[i] = 0;
    int i = 0;
    for (int j=0; j<deCount; j++){
      ++ndepet[i];
      ++i;
      if (i>=npets) i=0;
    }
    // now associate the DEs with their PETs
    i=0;
    int k=0;
    for (int j=0; j<deCount; j++){
      deInfoList[j].pet = i;
      ++k;
      if (k>=ndepet[i]){
        k=0;
        ++i;
      }
    }
    delete [] ndepet;
  }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-- eXtreme eXchange Engine (XXE) --------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// utility function used by XXE() constructor from streami
template<typename T> void readin(stringstream &streami, T *value){
  streami.read((char*)value, sizeof(T));
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::XXE()"
// constructor
XXE::XXE(stringstream &streami, vector<int> *originToTargetMap,
  map<void *, void *> *bufferOldNewMap,
  map<void *, void *> *dataOldNewMap){
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) throw rc;
  rh = NULL;  // guard

  // HEADER
  readin(streami, &count);                // number of elements in op-stream
  map<void *, unsigned long>::size_type dataMapSize;    // aux. variable
  readin(streami, &dataMapSize);          // number of data elements
  vector<BufferInfo *>::size_type bufferInfoListSize;   // aux. variable
  readin(streami, &bufferInfoListSize);   // number of buffer elements
  readin(streami, &commhandleCount);      // number of commhandles
  readin(streami, &xxeSubCount);          // number of subs
  for (int i=0; i<10; i++)
    readin(streami, &typekind[i]);        // typekinds
  readin(streami, &lastFilterBitField);   //
  readin(streami, &superVectorOkay);      //
  readin(streami, &max);                  //
  readin(streami, &dataMaxCount);         //
  readin(streami, &commhandleMaxCount);   //
  readin(streami, &xxeSubMaxCount);       //

  streampos positionOpstream;
  readin(streami, &positionOpstream);
  streampos positionDataMap;
  readin(streami, &positionDataMap);
  streampos positionBufferMap;
  readin(streami, &positionBufferMap);
  streampos positionCommhMap;
  readin(streami, &positionCommhMap);
  streampos positionSubsMap;
  readin(streami, &positionSubsMap);

  dataCount = 0;              // reset, because building up here
  dataMaxCount = dataMapSize; // appropriate size from incoming streami
  dataList = new char*[dataMapSize];

  // need a map object in order to track association between old->new data
  bool dataOldNewMapCreatorFlag = false;
  if (dataOldNewMap==NULL){
    // incoming dataOldNewMap is not created, created it here,
    // i.e. top level of recursion
    dataOldNewMapCreatorFlag = true;
    dataOldNewMap = new map<void *, void *>;
  }

  // associate new data allocation address with position of data in streami
  map<void *, streampos> dataPos;
  // position to read in dataMap
  streami.seekg(positionDataMap);
  // replicate the dataMap, allocating own local data
  for (unsigned i=0; i<dataMapSize; i++){
    void *oldAddr;
    unsigned long size;
    streampos pos;
    readin(streami, &oldAddr);          // old address
    readin(streami, &size);             // size
    readin(streami, &pos);              // pos
    // ensure alignment
    int qwords = size / 8;
    if (size % 8) ++qwords;
    void *data = (void *)(new double[qwords]);
    // duplicate the dataMap, but with local data reference
    dataMap[data] = size;
    dataList[dataCount] = (char *)data; // also set old dataList member for now
    ++dataCount;
    dataPos[data] = pos;
    // track association between old->new dataMap element
    (*dataOldNewMap)[oldAddr] = data;
  }
  // now read the actual data from streami and fill into new allocations
  map<void *, streampos>::iterator it;
  for (it=dataPos.begin(); it!=dataPos.end(); ++it){
    void *data = it->first;
    streampos pos = it->second;
    unsigned long size = dataMap[data];
    streami.seekg(pos);
    streami.read((char *)data, size);
  }

  // need a map object in order to track association between old->new buffer
  bool bufferOldNewMapCreatorFlag = false;
  if (bufferOldNewMap==NULL){
    // incoming bufferOldNewMap is not created, created it here,
    // i.e. top level of recursion
    bufferOldNewMapCreatorFlag = true;
    bufferOldNewMap = new map<void *, void *>;
  }

  // position to read in bufferMap
  streami.seekg(positionBufferMap);
  // replicate the bufferInfoList, allocating own local buffers
  bufferInfoList.reserve(bufferInfoListSize);  // initial preparation
  for (unsigned i=0; i<bufferInfoListSize; i++){
    void *oldAddr;
    unsigned long size;
    int multiplier;
    readin(streami, &oldAddr);          // old address
    readin(streami, &size);             // size
    readin(streami, &multiplier);       // multiplier
    // ensure alignment
    unsigned long qwords = size / 8;
    if (size % 8) ++qwords;
    char *buffer = (char *)(new double[qwords]);
    // duplicate the bufferInfo, but with local buffer reference
    BufferInfo *bufferInfo = new BufferInfo(buffer, size, multiplier);
    bufferInfoList.push_back(bufferInfo);
    // track association between old->new bufferInfoList element
    (*bufferOldNewMap)[oldAddr] = bufferInfoList[i];
  }

  // position to read in commhMap
  streami.seekg(positionCommhMap);
  // replicate the commhandle member
  map<void *, void *> commhOldNewMap;  // keep track of old->new association
  // know how many commhandles are needed
  commhandleMaxCount = commhandleCount + 1; // see note below for +1
  // +1 prevents storeCommhandle() below from incrementing commhandleMaxCount
  commhandleCount = 0;  // go through the regular process to increment this
  commhandle = new VMK::commhandle**[commhandleMaxCount-1];
  for (int i=0; i<commhandleMaxCount-1; i++){
    void *oldAddr;
    readin(streami, &oldAddr);          // old address
    // duplicate the commhandle, but locally
    VMK::commhandle** commh = new VMK::commhandle*;
    *commh = new VMK::commhandle;
    // track newly commh in XXE
    if (ESMC_LogDefault.MsgFoundError(storeCommhandle(commh),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) throw rc;
    // track association between old->new commhandle
    commhOldNewMap[oldAddr] = commhandle[i];
  }

  // position to read in subsMap
  streami.seekg(positionSubsMap);
  // replicate the xxeSubList
  // _after_ dataOldNewMap, and bufferOldNewMap  have been filled on this level
  map<void *, void *> xxeSubOldNewMap;  // keep track of association
  xxeSubMaxCount = xxeSubCount; // know the exact number, don't need extra
  xxeSubList = new XXE*[xxeSubMaxCount];
  for (int i=0; i<xxeSubCount; i++){
    void *oldAddr;
    streampos pos;
    readin(streami, &oldAddr);          // old address
    readin(streami, &pos);              // pos
    // hang on to current position in streami for next iteration
    streampos currPos = streami.tellg();
    // recursive constructor call into XXE()
    streami.seekg(pos); // position streami for this sub
    xxeSubList[i] = new XXE(streami, originToTargetMap,
      bufferOldNewMap, dataOldNewMap);
    // track association between old->new xxeSubList element
    xxeSubOldNewMap[oldAddr] = xxeSubList[i];
    // reposition streami for next iteration
    streami.seekg(currPos);
  }

  // position to read in opstream
  streami.seekg(positionOpstream);
  max = count;        // know exactly how many elements there are
  opstream = new StreamElement[count];
  // read the full opstream
  streami.read((char*)opstream, count*sizeof(StreamElement));

  // translate old->new addresses in the entire opstream
  for (int index=0; index<count; index++){
    StreamElement *xxeElement = &(opstream[index]);

    switch(opstream[index].opId){
    case send:
    case recv:
      {
        BuffInfo *element = (BuffInfo *)xxeElement;
        void *oldAddr = element->buffer;
        void *newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->buffer = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "send/recv:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
      // no break on purpose .... need to also shuffle PETs
      /* FALLTHRU */
    case sendRRA:
    case recvRRA:
      {
        BuffInfo *element = (BuffInfo *)xxeElement;
        if (originToTargetMap){
          // shuffle PETs
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "originToTargetMap:"
            << " old PET: " << element->pet
            << " new PET: " << (*originToTargetMap)[element->pet] << "\n";
#endif
          element->pet = (*originToTargetMap)[element->pet];
        }
      }
      break;
    case sendrecv:
      {
        SendRecvInfo *element = (SendRecvInfo *)xxeElement;
        void *oldAddr = element->srcBuffer;
        void *newAddr = NULL;
        if (element->srcIndirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->srcBuffer = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "sendrecv:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->dstBuffer;
        newAddr = NULL;
        if (element->dstIndirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->dstBuffer = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "sendrecv:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        if (originToTargetMap){
          // shuffle PETs
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "originToTargetMap:"
            << " old PET: " << element->srcPet
            << " new PET: " << (*originToTargetMap)[element->srcPet] << "\n";
#endif
          element->srcPet = (*originToTargetMap)[element->srcPet];
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "originToTargetMap:"
            << " old PET: " << element->dstPet
            << " new PET: " << (*originToTargetMap)[element->dstPet] << "\n";
#endif
          element->dstPet = (*originToTargetMap)[element->dstPet];
        }
      }
      break;
    case sendRRArecv:
      {
        SendRRARecvInfo *element = (SendRRARecvInfo *)xxeElement;
        void *oldAddr = element->dstBuffer;
        void *newAddr = NULL;
        if (element->dstIndirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->dstBuffer = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "sendRRArecv:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        if (originToTargetMap){
          // shuffle PETs
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "originToTargetMap:"
            << " old PET: " << element->srcPet
            << " new PET: " << (*originToTargetMap)[element->srcPet] << "\n";
          element->srcPet = (*originToTargetMap)[element->srcPet];
          cout << "originToTargetMap:"
            << " old PET: " << element->dstPet
            << " new PET: " << (*originToTargetMap)[element->dstPet] << "\n";
#endif
          element->dstPet = (*originToTargetMap)[element->dstPet];
        }
      }
      break;
    case sendnb:
    case recvnb:
      {
        BuffnbInfo *element = (BuffnbInfo *)xxeElement;
        void *oldAddr = element->buffer;
        void *newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->buffer = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "sendnb/recvnb:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      // no break on purpose .... need to also swap commhandle as below
      /* FALLTHRU */
    case sendnbRRA:
    case recvnbRRA:
      {
        CommhandleInfo *commhandleInfo = (CommhandleInfo *)xxeElement;
        void *oldAddr = commhandleInfo->commhandle;
        void *newAddr = commhOldNewMap[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "commhandle:"
          << " old Commhandle: " << oldAddr
          << " new Commhandle: " << newAddr << "\n";
#endif
        commhandleInfo->commhandle = (VMK::commhandle **)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        if (originToTargetMap){
          // shuffle PETs
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "originToTargetMap:"
            << " old PET: " << commhandleInfo->pet
            << " new PET: " << (*originToTargetMap)[commhandleInfo->pet] << "\n";
#endif
          commhandleInfo->pet = (*originToTargetMap)[commhandleInfo->pet];
        }
      }
      break;
    case productSumVector:
      {
        ProductSumVectorInfo *element
          = (ProductSumVectorInfo *)xxeElement;
        void *oldAddr = element->element;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumVector:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->element = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->factorList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumVector:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factorList = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumVector:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueList = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case productSumScalar:
      {
        ProductSumScalarInfo *element
          = (ProductSumScalarInfo *)xxeElement;
        void *oldAddr = element->element;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "productSumScalar:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->element = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->factor;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "productSumScalar:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factor = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->value;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "productSumScalar:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->value = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case productSumScalarRRA:
      {
        ProductSumScalarRRAInfo *element
          = (ProductSumScalarRRAInfo *)xxeElement;
        void *oldAddr = element->factor;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "productSumScalarRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factor = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->value;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "productSumScalarRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->value = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case sumSuperScalarDstRRA:
      {
        SumSuperScalarDstRRAInfo *element
          = (SumSuperScalarDstRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueOffsetList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueBase;
        newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->valueBase = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case sumSuperScalarListDstRRA:
      {
        SumSuperScalarListDstRRAInfo *element
          = (SumSuperScalarListDstRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueBaseList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueBaseList = (void **)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueBaseListResolve;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueBaseListResolve = (void **)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->rraIndexList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraIndexList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueOffsetList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->baseListIndexList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "SumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->baseListIndexList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        // replace the pointers old->new one level deep
        for (int i=0; i<element->valueBaseListSize; i++){
          oldAddr = element->valueBaseList[i];
          newAddr = NULL;
          if (element->indirectionFlag)
            newAddr = (*bufferOldNewMap)[oldAddr];
          else
            newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "SumSuperScalarListDstRRA:"
            << " oldAddr: " << oldAddr
            << " newAddr: " << newAddr << "\n";
#endif
          element->valueBaseList[i] = (void *)newAddr;
          if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        }
      }
      break;
    case productSumSuperScalarDstRRA:
      {
        ProductSumSuperScalarDstRRAInfo *element
          = (ProductSumSuperScalarDstRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->factorList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factorList = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueOffsetList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueBase;
        newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->valueBase = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case productSumSuperScalarListDstRRA:
      {
        ProductSumSuperScalarListDstRRAInfo *element
          = (ProductSumSuperScalarListDstRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->factorList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factorList = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueBaseList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueBaseList = (void **)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueBaseListResolve;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueBaseListResolve = (void **)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->rraIndexList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraIndexList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueOffsetList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->valueOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->baseListIndexList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarListDstRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->baseListIndexList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        // replace the pointers old->new one level deep
        for (int i=0; i<element->valueBaseListSize; i++){
          oldAddr = element->valueBaseList[i];
          newAddr = NULL;
          if (element->indirectionFlag)
            newAddr = (*bufferOldNewMap)[oldAddr];
          else
            newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "ProductSumSuperScalarListDstRRA:"
            << " oldAddr: " << oldAddr
            << " newAddr: " << newAddr << "\n";
#endif
          element->valueBaseList[i] = (void *)newAddr;
          if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        }
      }
      break;
    case productSumSuperScalarSrcRRA:
      {
        ProductSumSuperScalarSrcRRAInfo *element
          = (ProductSumSuperScalarSrcRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->factorList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factorList = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->elementOffsetList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->elementOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->elementBase;
        newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->elementBase = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case productSumSuperScalarContigRRA:
      {
        ProductSumSuperScalarContigRRAInfo *element
          = (ProductSumSuperScalarContigRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarContigRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->factorList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarContigRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->factorList = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->valueList;
        newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->valueList = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ProductSumSuperScalarContigRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case zeroSuperScalarRRA:
      {
        ZeroSuperScalarRRAInfo *element
          = (ZeroSuperScalarRRAInfo *)xxeElement;
        void *oldAddr = element->rraOffsetList;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ZeroSuperScalarRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case zeroMemset:
      {
        ZeroMemsetInfo *element = (ZeroMemsetInfo *)xxeElement;
        void *oldAddr = element->buffer;
        void *newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
        element->buffer = newAddr;
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "ZeroMemset:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case memCpy:
      {
        MemCpyInfo *element
          = (MemCpyInfo *)xxeElement;
        void *oldAddr = element->dstMem;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "MemCpy:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->dstMem = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->srcMem;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "MemCpy:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->srcMem = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case memCpySrcRRA:
      {
        MemCpySrcRRAInfo *element
          = (MemCpySrcRRAInfo *)xxeElement;
        void *oldAddr = element->dstMem;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "MemCpySrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->dstMem = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case memGatherSrcRRA:
      {
        MemGatherSrcRRAInfo *element
          = (MemGatherSrcRRAInfo *)xxeElement;
        void *oldAddr = element->dstBase;
        void *newAddr = NULL;
        if (element->indirectionFlag)
          newAddr = (*bufferOldNewMap)[oldAddr];
        else
          newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "MemGatherSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->dstBase = (void *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->rraOffsetList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "MemGatherSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->rraOffsetList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->countList;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "MemGatherSrcRRA:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->countList = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case waitOnIndexSub:
    case testOnIndexSub:
    case xxeSub:
      {
        SingleSubInfo *singleSubInfo = (SingleSubInfo *)xxeElement;
        void *oldAddr = singleSubInfo->xxe;
        void *newAddr = xxeSubOldNewMap[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "singleSub:"
          << " old Sub: " << oldAddr
          << " new Sub: " << newAddr << "\n";
#endif
        singleSubInfo->xxe = (XXE *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case waitOnAnyIndexSub:
      {
        WaitOnAnyIndexSubInfo *element = (WaitOnAnyIndexSubInfo *)xxeElement;
        void *oldAddr = element->index;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "waitOnAnyIndexSub:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->index = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->completeFlag;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "waitOnAnyIndexSub:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->completeFlag = (int *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      // no break on purpose .... need to also swap sub as below
      /* FALLTHRU */
    case xxeSubMulti:
      {
        MultiSubInfo *multiSubInfo = (MultiSubInfo *)xxeElement;
        void *oldAddr = multiSubInfo->xxe;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "multiSub:"
          << " old Sub: " << oldAddr
          << " new Sub: " << newAddr << "\n";
#endif
        multiSubInfo->xxe = (XXE **)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        for (int i=0; i<multiSubInfo->count; i++){
          oldAddr = multiSubInfo->xxe[i];
          newAddr = xxeSubOldNewMap[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
          cout << "multiSub:"
            << " count: " << multiSubInfo->count
            << " old Sub[i]: " << oldAddr
            << " new Sub[i]: " << newAddr << "\n";
#endif
          multiSubInfo->xxe[i] = (XXE *)newAddr;
          if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        }
      }
      break;
    case wtimer:
      {
        WtimerInfo *element = (WtimerInfo *)xxeElement;
        void *oldAddr = element->timerString;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "wtimer:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->timerString = (char *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->relativeWtime;
        newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "wtimer:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->relativeWtime = (double *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
        oldAddr = element->relativeWtimerXXE;
        newAddr = xxeSubOldNewMap[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "wtimer:"
          << " old Sub: " << oldAddr
          << " new Sub: " << newAddr << "\n";
#endif
        element->relativeWtimerXXE = (XXE *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    case message:
    case profileMessage:
      {
        MessageInfo *element = (MessageInfo *)xxeElement;
        void *oldAddr = element->messageString;
        void *newAddr = (*dataOldNewMap)[oldAddr];
#ifdef XXE_CONSTRUCTOR_LOG_on
        cout << "Message:"
          << " oldAddr: " << oldAddr
          << " newAddr: " << newAddr << "\n";
#endif
        element->messageString = (char *)newAddr;
        if (newAddr==NULL) cout << "ERROR in old->new translation!!\n";
      }
      break;
    default:
      break;
    }

  } // index

  // local garbage collection
  if (dataOldNewMapCreatorFlag){
    delete dataOldNewMap;
  }
  if (bufferOldNewMapCreatorFlag){
    delete bufferOldNewMap;
  }

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::~XXE()"
// destructor
XXE::~XXE(){
  // -> clean-up all allocations for which this XXE object is responsible:
  // opstream of XXE elements
  delete [] opstream;
  // memory allocations held in data
  std::map<void *, unsigned long>::iterator it;
  for (it=dataMap.begin(); it!=dataMap.end(); ++it){
#ifdef XXE_STORAGEDELETE_LOG_on
    {
      std::stringstream debugmsg;
      debugmsg << ESMC_METHOD": delete from dataMap: "
        << it->first << " size (byte): " << it->second;
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
    delete [] (char *)it->first;  // free the associated memory
  }
  delete [] dataList;
  // CommHandles held in commhandle
  for (int i=0; i<commhandleCount; i++){
    delete *commhandle[i];
    delete commhandle[i];
  }
  delete [] commhandle;
  // XXE sub objects held in xxeSubList
  for (int i=0; i<xxeSubCount; i++)
    delete xxeSubList[i];
  delete [] xxeSubList;
  // BufferInfo objects held in bufferInfoList
  for (unsigned int i=0; i<bufferInfoList.size(); i++){
#ifdef XXE_STORAGEDELETE_LOG_on
    {
      std::stringstream debugmsg;
      debugmsg << ESMC_METHOD": delete from bufferInfoList: "
        << (void *)bufferInfoList[i]->buffer << " size (byte): "
        << bufferInfoList[i]->size;
      ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
    delete [] (char *)(bufferInfoList[i]->buffer);  // free associated memory
    delete bufferInfoList[i];
  }
  bufferInfoList.clear();
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// utility function used by streamify
template<typename T> void append(stringstream &streami, T value){
  streami.write((char*)&value, sizeof(T));
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::streamify()"
void XXE::streamify(stringstream &streami){

  // HEADER
  append(streami, count);                 // number of elements in op-stream
  append(streami, dataMap.size());        // number of data elements
  append(streami, bufferInfoList.size()); // number of buffer elements
  append(streami, commhandleCount);       // number of commhandles
  append(streami, xxeSubCount);           // number of subs
  for (int i=0; i<10; i++)
    append(streami, typekind[i]);         // typekinds
  append(streami, lastFilterBitField);    //
  append(streami, superVectorOkay);       //
  append(streami, max);                   //
  append(streami, dataMaxCount);          //
  append(streami, commhandleMaxCount);    //
  append(streami, xxeSubMaxCount);        //

  streampos posZero = 0;  // dummy position variable

  streampos posPositionOpstream = streami.tellp(); // store this position
  append(streami, posZero);               // placeholder for actual position

  streampos posPositionDataMap = streami.tellp(); // store this position
  append(streami, posZero);               // placeholder for actual position

  streampos posPositionBufferMap = streami.tellp(); // store this position
  append(streami, posZero);               // placeholder for actual position

  streampos posPositionCommhMap = streami.tellp(); // store this position
  append(streami, posZero);               // placeholder for actual position

  streampos posPositionSubsMap = streami.tellp(); // store this position
  append(streami, posZero);               // placeholder for actual position

  // SUBS
  vector<streampos> subPos;
  for (int i=0; i<xxeSubCount; ++i){
    subPos.push_back(streami.tellp()); // hang on to position in stream
    xxeSubList[i]->streamify(streami); // recursively streamify
  }

  // needed below for iterations
  map<void *, unsigned long>::iterator it;
  int ii;

  // DATA
  vector<streampos> dataPos;
  for (it=dataMap.begin(); it!=dataMap.end(); ++it){
    dataPos.push_back(streami.tellp()); // hang on to position in stream
    streami.write((char*)it->first, it->second);  // data block
  }

  // MAPS
  // dataMap
  streampos positionDataMap = streami.tellp();
  for (it=dataMap.begin(), ii=0; it!=dataMap.end(); ++it, ++ii){
    append(streami, it->first);   // old address
    append(streami, it->second);  // size
    append(streami, dataPos[ii]); // position in stream
  }
  // bufferMap
  streampos positionBufferMap = streami.tellp();
  for (unsigned i=0; i<bufferInfoList.size(); ++i){
    append(streami, (void *)bufferInfoList[i]);                 // old address
    append(streami, bufferInfoList[i]->size);                   // size
    append(streami, bufferInfoList[i]->vectorLengthMultiplier); // multiplier
  }
  // commhMap
  streampos positionCommhMap = streami.tellp();
  for (int i=0; i<commhandleCount; ++i){
    append(streami, (void *)(commhandle[i]));   // old address
  }
  // subsMap
  streampos positionSubsMap = streami.tellp();
  for (int i=0; i<xxeSubCount; ++i){
    append(streami, (void *)(xxeSubList[i]));   // old address
    append(streami, subPos[i]);                 // position in stream
  }

  // OPSTREAM
  streampos positionOpstream = streami.tellp();
  streami.write((char*)opstream, count*sizeof(StreamElement));// write opstream

  // finish up by jumping through the streami and write some positions
  streami.seekp(posPositionOpstream);
  append(streami, positionOpstream);
  append(streami, positionDataMap);
  append(streami, positionBufferMap);
  append(streami, positionCommhMap);
  append(streami, positionSubsMap);

  // reset to end of stream again -> very important for recusion!
  streami.seekp(0, ios::end);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::clearReset()"
void XXE::clearReset(int countArg, int dataCountArg, int commhandleCountArg,
  int xxeSubCountArg, int bufferInfoListArg){
  // reset the stream back to a specified position, and clear all
  // bookkeeping elements above specified positions
  count = countArg; // reset
  // cannot use dataMap to reset, because need something linear
  if (dataCountArg>-1){
    for (int i=dataCountArg; i<dataCount; i++){
#ifdef XXE_STORAGEDELETE_LOG_on
      {
        std::stringstream debugmsg;
        debugmsg << ESMC_METHOD": delete from dataMap: "
          << (void *)dataList[i] << " size (byte): " << dataMap[dataList[i]];
        ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
      dataMap.erase(dataList[i]); // remove entry from dataMap
      delete [] dataList[i];       // free the referenced memory
    }
    dataCount = dataCountArg; // reset
  }
  if (commhandleCountArg>-1){
    for (int i=commhandleCountArg; i<commhandleCount; i++){
      delete *commhandle[i];
      delete commhandle[i];
    }
    commhandleCount = commhandleCountArg; // reset
  }
  if (xxeSubCountArg>-1){
    for (int i=xxeSubCountArg; i<xxeSubCount; i++)
      delete xxeSubList[i];
    xxeSubCount = xxeSubCountArg; // reset
  }
  if (bufferInfoListArg>-1){
    std::vector<BufferInfo *>::iterator first =
      bufferInfoList.begin() + bufferInfoListArg;
    std::vector<BufferInfo *>::iterator last = bufferInfoList.end();
    for (std::vector<BufferInfo *>::iterator bi=first; bi!=last; ++bi){
#ifdef XXE_STORAGEDELETE_LOG_on
      {
        std::stringstream debugmsg;
        debugmsg << ESMC_METHOD": delete from bufferInfoList: "
          << (void *)((*bi)->buffer) << " size (byte): "
          << (*bi)->size;
        ESMC_LogDefault.Write(debugmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
      delete [] (char *)((*bi)->buffer);  // free associated memory
      delete *bi;
    }
    bufferInfoList.erase(first, last);
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::exec()"
//BOPI
// !IROUTINE:  ESMCI::XXE::exec
//
// !INTERFACE:
int XXE::exec(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int rraCount,       // in  - number of relative run-time address in rraList
  char **rraList,     // in  - relative run-time addresses
  int *vectorLength,  // in  - run-time vectorLength
  int filterBitField, // in  - filter operations according to predicateBitField
  bool *finished,     // out - TEST ops executed as per filterBitField are
                      //       finished, or need additional FINISH calls
  bool *cancelled,    // out - indicates whether there are any cancelled ops
  double *dTime,      // out - execution time, NULL to disable
  int indexStart,     // in  - start index, < 0 for default (full stream)
  int indexStop,      // in  - stop index, < 0 for default (full stream)
  int *srcLocalDeCount,   // in  - in order to determine dst index from rraIndex
  SuperVectP *superVectP  // in  - super vector support
  ){
//
// !DESCRIPTION:
//  Execute the XXE stream. For performance reasons there is _no_ checking
//  during execution to ensure relative run-time addressing (RRA) references
//  in the XXE stream are within the rraList bounds, i.e. [0...rraCount-1].
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#ifdef XXE_EXEC_LOG_on
  char msg[1024];
  sprintf(msg, "ESMCI::XXE::exec(): START: stream=%p, count=%d, "
    "indexStart=%d, indexStop=%d",
    stream, count, indexStart, indexStop);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  sprintf(msg, "ESMCI::XXE::exec(): START: sizeof(StreamElement)=%lu, "
    "rraCount=%d, vectorLength=%d",
    sizeof(StreamElement), rraCount, *vectorLength);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
  sprintf(msg, "ESMCI::XXE::exec(): START'ed: filterBitField=0x%08x, "
    "finished=%p, cancelled=%p", filterBitField, finished, cancelled);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif

#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():1.0"));
#endif

  // set index range
  int indexRangeStart = 0;        // default
  if (indexStart > 0) indexRangeStart = indexStart;
  int indexRangeStop = count-1;   // default
  if (indexStop > 0) indexRangeStop = indexStop;

  // check index range
  if (count > 0 && indexRangeStart > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "indexStart out of range", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (indexRangeStop > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "indexStop out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  // store filterBitField in XXE
  lastFilterBitField = filterBitField;

  // initialize finished and cancelled flags
  if (finished) *finished = true; // assume all ops finished unless find otherw.
  if (cancelled) *cancelled = false; // assume no ops cancelled unless find ow.
  // XXE element variables used below
  StreamElement *xxeElement, *xxeIndexElement;
  SendInfo *xxeSendInfo;
  RecvInfo *xxeRecvInfo;
  SendRRAInfo *xxeSendRRAInfo;
  RecvRRAInfo *xxeRecvRRAInfo;
  SendRecvInfo *xxeSendRecvInfo;
  SendRRARecvInfo *xxeSendRRARecvInfo;
  SendnbInfo *xxeSendnbInfo;
  RecvnbInfo *xxeRecvnbInfo;
  SendnbRRAInfo *xxeSendnbRRAInfo;
  RecvnbRRAInfo *xxeRecvnbRRAInfo;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  TestOnIndexInfo *xxeTestOnIndexInfo;
  WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  WaitOnIndexSubInfo *waitOnIndexSubInfo;
  TestOnIndexSubInfo *testOnIndexSubInfo;
  CancelIndexInfo *xxeCancelIndexInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumVectorInfo *xxeProductSumVectorInfo;
  ProductSumScalarInfo *xxeProductSumScalarInfo;
  ProductSumScalarRRAInfo *xxeProductSumScalarRRAInfo;
  SumSuperScalarDstRRAInfo *xxeSumSuperScalarDstRRAInfo;
  SumSuperScalarListDstRRAInfo *xxeSumSuperScalarListDstRRAInfo;
  ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo;
  ProductSumSuperScalarListDstRRAInfo *xxeProductSumSuperScalarListDstRRAInfo;
  ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo;
  ProductSumSuperScalarContigRRAInfo *xxeProductSumSuperScalarContigRRAInfo;
  ZeroScalarRRAInfo *xxeZeroScalarRRAInfo;
  ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo;
  ZeroMemsetInfo *xxeZeroMemsetInfo;
  ZeroMemsetRRAInfo *xxeZeroMemsetRRAInfo;
  MemCpyInfo *xxeMemCpyInfo;
  MemCpySrcRRAInfo *xxeMemCpySrcRRAInfo;
  MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo;
  XxeSubInfo *xxeSubInfo;
  XxeSubMultiInfo *xxeSubMultiInfo;
  WtimerInfo *xxeWtimerInfo, *xxeWtimerInfoActual, *xxeWtimerInfoRelative;
  MessageInfo *xxeMessageInfo;

  double t0, t1;

  if (dTime != NULL)
    VMK::wtime(&t0);

#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():2.0"));
#endif

#ifdef XXE_EXEC_LOG_on
  sprintf(msg, "ESMCI::XXE::exec(): bufferInfoList.size()=%d",
    bufferInfoList.size());
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
  for (unsigned i=0; i<bufferInfoList.size(); i++){
    unsigned long currentSize = bufferInfoList[i]->vectorLengthMultiplier
      * *vectorLength;
#ifdef XXE_EXEC_BUFFLOG_on
    sprintf(msg, "ESMCI::XXE::exec(): buffer #%d, (needed)currentSize=%d, "
      " existing buffer size=%d", i, currentSize, bufferInfoList[i]->size);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    if (bufferInfoList[i]->size < currentSize){
      // deallocate the old buffer
      delete [] (char *)(bufferInfoList[i]->buffer);  // free associated memory
      // allocate a new, larger buffer to accommodate currentSize
      char *buffer = new char[currentSize];
#ifdef XXE_EXEC_BUFFLOG_on
      sprintf(msg, "ESMCI::XXE::exec(): buffer #%d, new buffer allocated: %p",
        i, buffer);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
      // update the bufferInfoList entry with the newly allocated buffer
      bufferInfoList[i]->buffer = buffer;
      bufferInfoList[i]->size = currentSize;
    }
#define EXECBUFFERSET____disable
#ifdef EXECBUFFERSET
    // Turn this ON only for DEBUG to help identify buffer access issues!!!
    memset(bufferInfoList[i]->buffer, 0xff, bufferInfoList[i]->size);
#endif
  }

#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():3.0"));
#endif

  for (int i=indexRangeStart; i<=indexRangeStop; i++){
    xxeElement = &(opstream[i]);

    if (xxeElement->predicateBitField & filterBitField)
      continue; // filter out this operation

#ifdef XXE_EXEC_LOG_on
    sprintf(msg, "ESMCI::XXE::exec(): %d, opId=%d", i, opstream[i].opId);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif

    switch(opstream[i].opId){
    case send:
      {
        xxeSendInfo = (SendInfo *)xxeElement;
        char *buffer = (char *)xxeSendInfo->buffer;
        if (xxeSendInfo->indirectionFlag)
          buffer = *(char **)xxeSendInfo->buffer;
        int size = xxeSendInfo->size;
        if (xxeSendInfo->vectorFlag)
          size *= *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::send: buffer=%p, size=%d, dst=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d",
          xxeSendInfo->buffer, xxeSendInfo->size, xxeSendInfo->dstPet,
          xxeSendInfo->tag, xxeSendInfo->vectorFlag,
          xxeSendInfo->indirectionFlag);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->send(buffer, size, xxeSendInfo->dstPet, xxeSendInfo->tag);
        xxeSendInfo->activeFlag = true;     // set
        xxeSendInfo->cancelledFlag = false; // set
      }
      break;
    case recv:
      {
        xxeRecvInfo = (RecvInfo *)xxeElement;
        char *buffer = (char *)xxeRecvInfo->buffer;
        if (xxeRecvInfo->indirectionFlag)
          buffer = *(char **)xxeRecvInfo->buffer;
        int size = xxeRecvInfo->size;
        if (xxeRecvInfo->vectorFlag)
          size *= *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::recv: buffer=%p, size=%d, src=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d",
          xxeRecvInfo->buffer, xxeRecvInfo->size, xxeRecvInfo->srcPet,
          xxeRecvInfo->tag, xxeRecvInfo->vectorFlag,
          xxeRecvInfo->indirectionFlag);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->recv(buffer, size, xxeRecvInfo->srcPet, xxeRecvInfo->tag);
        xxeRecvInfo->activeFlag = true;     // set
        xxeRecvInfo->cancelledFlag = false; // set
      }
      break;
    case sendRRA:
      {
        xxeSendRRAInfo = (SendRRAInfo *)xxeElement;
        int size = xxeSendRRAInfo->size;
        int rraOffset = xxeSendRRAInfo->rraOffset;
        if (xxeSendRRAInfo->vectorFlag){
          size *= *vectorLength;
          rraOffset *= *vectorLength;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sendRRA: size=%d", xxeSendRRAInfo->size);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->send(rraList[xxeSendRRAInfo->rraIndex]
          + rraOffset, size, xxeSendRRAInfo->dstPet, xxeSendRRAInfo->tag);
        xxeSendRRAInfo->activeFlag = true;      // set
        xxeSendRRAInfo->cancelledFlag = false;  // set
      }
      break;
    case recvRRA:
      {
        xxeRecvRRAInfo = (RecvRRAInfo *)xxeElement;
        int size = xxeRecvRRAInfo->size;
        int rraOffset = xxeRecvRRAInfo->rraOffset;
        if (xxeRecvRRAInfo->vectorFlag){
          size *= *vectorLength;
          rraOffset *= *vectorLength;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::recvRRA: size=%d", xxeRecvRRAInfo->size);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->recv(rraList[xxeRecvRRAInfo->rraIndex]
          + rraOffset, size, xxeRecvRRAInfo->srcPet, xxeRecvRRAInfo->tag);
        xxeRecvRRAInfo->activeFlag = true;      // set
        xxeRecvRRAInfo->cancelledFlag = false;  // set
      }
      break;
    case sendrecv:
      {
        xxeSendRecvInfo = (SendRecvInfo *)xxeElement;
        char *srcBuffer = (char *)xxeSendRecvInfo->srcBuffer;
        char *dstBuffer = (char *)xxeSendRecvInfo->dstBuffer;
        if (xxeSendRecvInfo->srcIndirectionFlag)
          srcBuffer = *(char **)xxeSendRecvInfo->srcBuffer;
        if (xxeSendRecvInfo->dstIndirectionFlag)
          dstBuffer = *(char **)xxeSendRecvInfo->dstBuffer;
        int srcSize = xxeSendRecvInfo->srcSize;
        int dstSize = xxeSendRecvInfo->dstSize;
        if (xxeSendRecvInfo->vectorFlag){
          srcSize *= *vectorLength;
          dstSize *= *vectorLength;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sendrecv: dst=%d, sendSize=%d, src=%d, recvSize=%d",
          xxeSendRecvInfo->dstPet, srcSize, xxeSendRecvInfo->srcPet, dstSize);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->sendrecv(srcBuffer, srcSize, xxeSendRecvInfo->dstPet,
          dstBuffer, dstSize, xxeSendRecvInfo->srcPet, xxeSendRecvInfo->dstTag,
          xxeSendRecvInfo->srcTag);
        xxeSendRecvInfo->activeFlag = true;     // set
        xxeSendRecvInfo->cancelledFlag = false; // set
      }
      break;
    case sendRRArecv:
      {
        xxeSendRRARecvInfo = (SendRRARecvInfo *)xxeElement;
        int srcSize = xxeSendRRARecvInfo->srcSize;
        int dstSize = xxeSendRRARecvInfo->dstSize;
        char *dstBuffer = (char *)xxeSendRRARecvInfo->dstBuffer;
        if (xxeSendRRARecvInfo->dstIndirectionFlag)
          dstBuffer = *(char **)xxeSendRRARecvInfo->dstBuffer;
        int rraOffset = xxeSendRRARecvInfo->rraOffset;
        if (xxeSendRRARecvInfo->vectorFlag){
          srcSize *= *vectorLength;
          dstSize *= *vectorLength;
          rraOffset *= *vectorLength;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sendRRArecv: dst=%d, src=%d",
          xxeSendRRARecvInfo->dstPet, xxeSendRRARecvInfo->srcPet);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->sendrecv(rraList[xxeSendRRARecvInfo->rraIndex] + rraOffset,
          srcSize, xxeSendRRARecvInfo->dstPet, dstBuffer, dstSize,
          xxeSendRRARecvInfo->srcPet, xxeSendRRARecvInfo->dstTag,
          xxeSendRRARecvInfo->srcTag);
        xxeSendRRARecvInfo->activeFlag = true;      // set
        xxeSendRRARecvInfo->cancelledFlag = false;  // set
      }
      break;
    case sendnb:
      {
#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():sendnb1.0"));
#endif
        xxeSendnbInfo = (SendnbInfo *)xxeElement;
        char *buffer = (char *)xxeSendnbInfo->buffer;
        if (xxeSendnbInfo->indirectionFlag)
          buffer = *(char **)xxeSendnbInfo->buffer;
        int size = xxeSendnbInfo->size;
        if (xxeSendnbInfo->vectorFlag)
          size *= *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sendnb: dst=%d, size=%d",
          xxeSendnbInfo->dstPet, size);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():sendnb2.0"));
#endif
        vm->send(buffer, size, xxeSendnbInfo->dstPet, xxeSendnbInfo->commhandle,
          xxeSendnbInfo->tag);
#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():sendnb3.0"));
#endif
        xxeSendnbInfo->activeFlag = true;     // set
        xxeSendnbInfo->cancelledFlag = false; // set
      }
      break;
    case recvnb:
      {
        xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
        char *buffer = (char *)xxeRecvnbInfo->buffer;
        if (xxeRecvnbInfo->indirectionFlag)
          buffer = *(char **)xxeRecvnbInfo->buffer;
        int size = xxeRecvnbInfo->size;
        if (xxeRecvnbInfo->vectorFlag)
          size *= *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::recvnb: src=%d, size=%d",
          xxeRecvnbInfo->srcPet, size);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->recv(buffer, size, xxeRecvnbInfo->srcPet, xxeRecvnbInfo->commhandle,
          xxeRecvnbInfo->tag);
        xxeRecvnbInfo->activeFlag = true;     // set
        xxeRecvnbInfo->cancelledFlag = false; // set
      }
      break;
    case sendnbRRA:
      {
        xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeElement;
        int size = xxeSendnbRRAInfo->size;
        int rraOffset = xxeSendnbRRAInfo->rraOffset;
        if (xxeSendnbRRAInfo->vectorFlag){
          size *= *vectorLength;
          rraOffset *= *vectorLength;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sendnbRRA: dst=%d, size=%d",
          xxeSendnbRRAInfo->dstPet, size);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->send(rraList[xxeSendnbRRAInfo->rraIndex]
          + rraOffset, size, xxeSendnbRRAInfo->dstPet,
          xxeSendnbRRAInfo->commhandle, xxeSendnbRRAInfo->tag);
        xxeSendnbRRAInfo->activeFlag = true;      // set
        xxeSendnbRRAInfo->cancelledFlag = false;  // set
      }
      break;
    case recvnbRRA:
      {
        xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeElement;
        int size = xxeRecvnbRRAInfo->size;
        int rraOffset = xxeRecvnbRRAInfo->rraOffset;
        if (xxeRecvnbRRAInfo->vectorFlag){
          size *= *vectorLength;
          rraOffset *= *vectorLength;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::recvnbRRA: src=%d, size=%d",
          xxeRecvnbRRAInfo->srcPet, size);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        vm->recv(rraList[xxeRecvnbRRAInfo->rraIndex]
          + rraOffset, size, xxeRecvnbRRAInfo->srcPet,
          xxeRecvnbRRAInfo->commhandle, xxeRecvnbRRAInfo->tag);
        xxeRecvnbRRAInfo->activeFlag = true;      // set
        xxeRecvnbRRAInfo->cancelledFlag = false;  // set
      }
      break;
    case waitOnIndex:
      {
        xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
        xxeIndexElement = &(opstream[xxeWaitOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::waitOnIndex: index=%d, activeFlag=%d",
          xxeWaitOnIndexInfo->index, xxeCommhandleInfo->activeFlag);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
        if (xxeCommhandleInfo->activeFlag){
          // this test is still active -> log some info about the actual comm
          if (xxeIndexElement->opId == sendnb){
            xxeSendnbInfo = (SendnbInfo *)xxeIndexElement;
            int size = xxeSendnbInfo->size;
            if (xxeSendnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndex:sendnb: dst=%d, size=%d",
              xxeSendnbInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnb){
            xxeRecvnbInfo = (RecvnbInfo *)xxeIndexElement;
            int size = xxeRecvnbInfo->size;
            if (xxeRecvnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndex:recvnb: src=%d, size=%d",
              xxeRecvnbInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == sendnbRRA){
            xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeIndexElement;
            int size = xxeSendnbRRAInfo->size;
            if (xxeSendnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndex:sendnbRRA: dst=%d, size=%d",
              xxeSendnbRRAInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnbRRA){
            xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeIndexElement;
            int size = xxeRecvnbRRAInfo->size;
            if (xxeRecvnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndex:recvnbRRA: src=%d, size=%d",
              xxeRecvnbRRAInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }
        }
#endif
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          VMK::status status;
          vm->commwait(xxeCommhandleInfo->commhandle, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
          xxeCommhandleInfo->activeFlag = false;  // reset
        }
        if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
      }
      break;
    case testOnIndex:
      {
        xxeTestOnIndexInfo = (TestOnIndexInfo *)xxeElement;
        xxeIndexElement = &(opstream[xxeTestOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::testOnIndex: index=%d, activeFlag=%d",
          xxeTestOnIndexInfo->index, xxeCommhandleInfo->activeFlag);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
        if (xxeCommhandleInfo->activeFlag){
          // this test is still active -> log some info about the actual comm
          if (xxeIndexElement->opId == sendnb){
            xxeSendnbInfo = (SendnbInfo *)xxeIndexElement;
            int size = xxeSendnbInfo->size;
            if (xxeSendnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndex:sendnb: dst=%d, size=%d",
              xxeSendnbInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnb){
            xxeRecvnbInfo = (RecvnbInfo *)xxeIndexElement;
            int size = xxeRecvnbInfo->size;
            if (xxeRecvnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndex:recvnb: src=%d, size=%d",
              xxeRecvnbInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == sendnbRRA){
            xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeIndexElement;
            int size = xxeSendnbRRAInfo->size;
            if (xxeSendnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndex:sendnbRRA: dst=%d, size=%d",
              xxeSendnbRRAInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnbRRA){
            xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeIndexElement;
            int size = xxeRecvnbRRAInfo->size;
            if (xxeRecvnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndex:recvnbRRA: src=%d, size=%d",
              xxeRecvnbRRAInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }
        }
#endif
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          int completeFlag;
          VMK::status status;
          vm->commtest(xxeCommhandleInfo->commhandle, &completeFlag, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::testOnIndex: completeFlag=%d, cancelledFlag=%d",
            completeFlag, xxeCommhandleInfo->cancelledFlag);
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          if (completeFlag){
            // comm finished
            xxeCommhandleInfo->activeFlag = false;  // reset
          }else{
            if (finished) *finished = false;  // comm not finished
          }
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::testOnIndex: returning with finished=%d",
            *finished);
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        }
        if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
      }
      break;
    case waitOnAnyIndexSub:
      {
        xxeWaitOnAnyIndexSubInfo = (WaitOnAnyIndexSubInfo *)xxeElement;
        int *completeFlag = xxeWaitOnAnyIndexSubInfo->completeFlag;
        int count = xxeWaitOnAnyIndexSubInfo->count;
        int completeTotal = 0;  // reset
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::waitOnAnyIndexSub: count=%d",
          xxeWaitOnAnyIndexSubInfo->count);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        for (int k=0; k<count; k++)
          completeFlag[k] = 0;  // reset
        while (completeTotal < count){
          for (int k=0; k<count; k++){
            if (!completeFlag[k]){
              xxeIndexElement = &(opstream[xxeWaitOnAnyIndexSubInfo->index[k]]);
              xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
              if (xxeCommhandleInfo->activeFlag){
                // there is an outstanding active communication
                VMK::status status;
                vm->commtest(xxeCommhandleInfo->commhandle, &(completeFlag[k]),
                  &status);
                xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
                if (completeFlag[k]){
                  // comm finished -> recursive call into xxe execution
                  xxeCommhandleInfo->activeFlag = false;  // reset
                  bool localFinished;
                  bool localCancelled;
                  xxeWaitOnAnyIndexSubInfo->xxe[k]->
                    exec(rraCount, rraList, vectorLength, filterBitField,
                    &localFinished, &localCancelled, NULL, -1, -1,
                    srcLocalDeCount, superVectP);
                  if (!localFinished)
                    if (finished) *finished = false;  // unfinished ops in sub
                 if (localCancelled)
                   if (cancelled) *cancelled = true;  // cancelled ops in sub
                  ++completeTotal;
                }else
                  if (finished) *finished = false;  // comm not finished
              }else{
                // this communication is not active
                completeFlag[k] = 1;
                ++completeTotal;
              }
              if (cancelled && xxeCommhandleInfo->cancelledFlag)
                *cancelled = true;
            }
          }
          if (completeTotal == count) break;
        }
      }
      break;
    case waitOnIndexRange:
      {
        xxeWaitOnIndexRangeInfo = (WaitOnIndexRangeInfo *)xxeElement;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::waitOnIndexRange");
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        for (int j=xxeWaitOnIndexRangeInfo->indexStart;
          j<xxeWaitOnIndexRangeInfo->indexEnd; j++){
          xxeIndexElement = &(opstream[j]);
          xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
          if (xxeCommhandleInfo->activeFlag){
            // there is an outstanding active communication
            VMK::status status;
            vm->commwait(xxeCommhandleInfo->commhandle, &status);
            xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
            xxeCommhandleInfo->activeFlag = false;  // reset
          }
          if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
        }
      }
      break;
    case waitOnIndexSub:
      {
        waitOnIndexSubInfo = (WaitOnIndexSubInfo *)xxeElement;
        xxeIndexElement = &(opstream[waitOnIndexSubInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::waitOnIndexSub: index=%d, activeFlag=%d",
          waitOnIndexSubInfo->index, xxeCommhandleInfo->activeFlag);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
        if (xxeCommhandleInfo->activeFlag){
          // this test is still active -> log some info about the actual comm
          if (xxeIndexElement->opId == sendnb){
            xxeSendnbInfo = (SendnbInfo *)xxeIndexElement;
            int size = xxeSendnbInfo->size;
            if (xxeSendnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndexSub:sendnb: dst=%d, size=%d",
              xxeSendnbInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnb){
            xxeRecvnbInfo = (RecvnbInfo *)xxeIndexElement;
            int size = xxeRecvnbInfo->size;
            if (xxeRecvnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndexSub:recvnb: src=%d, size=%d",
              xxeRecvnbInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == sendnbRRA){
            xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeIndexElement;
            int size = xxeSendnbRRAInfo->size;
            if (xxeSendnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndexSub:sendnbRRA: dst=%d, size=%d",
              xxeSendnbRRAInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnbRRA){
            xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeIndexElement;
            int size = xxeRecvnbRRAInfo->size;
            if (xxeRecvnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::waitOnIndexSub:recvnbRRA: src=%d, size=%d",
              xxeRecvnbRRAInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }
        }
#endif
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          VMK::status status;
          vm->commwait(xxeCommhandleInfo->commhandle, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
          xxeCommhandleInfo->activeFlag = false;  // reset
          if (waitOnIndexSubInfo->xxe){
            // recursive call into xxe execution
            bool localFinished;
            bool localCancelled;
            waitOnIndexSubInfo->xxe->exec(rraCount,
              rraList + waitOnIndexSubInfo->rraShift,
              vectorLength + waitOnIndexSubInfo->vectorLengthShift,
              filterBitField, &localFinished, &localCancelled, NULL, -1, -1,
              srcLocalDeCount + waitOnIndexSubInfo->vectorLengthShift,
              superVectP + waitOnIndexSubInfo->vectorLengthShift);
            if (!localFinished)
              if (finished) *finished = false;  // unfinished ops in sub
            if (localCancelled)
              if (cancelled) *cancelled = true;  // cancelled ops in sub
          }
        }
        if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
      }
      break;
    case testOnIndexSub:
      {
        testOnIndexSubInfo = (TestOnIndexSubInfo *)xxeElement;
        xxeIndexElement = &(opstream[testOnIndexSubInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::testOnIndexSub: index=%d, activeFlag=%d",
          testOnIndexSubInfo->index, xxeCommhandleInfo->activeFlag);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
        if (xxeCommhandleInfo->activeFlag){
          // this test is still active -> log some info about the actual comm
          if (xxeIndexElement->opId == sendnb){
            xxeSendnbInfo = (SendnbInfo *)xxeIndexElement;
            int size = xxeSendnbInfo->size;
            if (xxeSendnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndexSub:sendnb: dst=%d, size=%d",
              xxeSendnbInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnb){
            xxeRecvnbInfo = (RecvnbInfo *)xxeIndexElement;
            int size = xxeRecvnbInfo->size;
            if (xxeRecvnbInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndexSub:recvnb: src=%d, size=%d",
              xxeRecvnbInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == sendnbRRA){
            xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeIndexElement;
            int size = xxeSendnbRRAInfo->size;
            if (xxeSendnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndexSub:sendnbRRA: dst=%d, size=%d",
              xxeSendnbRRAInfo->dstPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }else if (xxeIndexElement->opId == recvnbRRA){
            xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeIndexElement;
            int size = xxeRecvnbRRAInfo->size;
            if (xxeRecvnbRRAInfo->vectorFlag)
              size *= *vectorLength;
            sprintf(msg, "XXE::testOnIndexSub:recvnbRRA: src=%d, size=%d",
              xxeRecvnbRRAInfo->srcPet, size);
            ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
          }
        }
#endif
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          int completeFlag;
          VMK::status status;
          vm->commtest(xxeCommhandleInfo->commhandle, &completeFlag, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::testOnIndexSub: completeFlag=%d, cancelledFlag=%d",
            completeFlag, xxeCommhandleInfo->cancelledFlag);
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          if (completeFlag){
            // comm finished -> recursive call into xxe execution
            xxeCommhandleInfo->activeFlag = false;  // reset
            if (testOnIndexSubInfo->xxe){
              // recursive call into xxe execution
              bool localFinished;
              bool localCancelled;
#ifdef XXE_EXEC_LOG_on
              sprintf(msg, "XXE::testOnIndexSub: calling into sub xxe.");
              ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
              testOnIndexSubInfo->xxe->exec(rraCount,
                rraList + testOnIndexSubInfo->rraShift,
                vectorLength + testOnIndexSubInfo->vectorLengthShift,
                filterBitField, &localFinished, &localCancelled, NULL, -1, -1,
                srcLocalDeCount + testOnIndexSubInfo->vectorLengthShift,
                superVectP + testOnIndexSubInfo->vectorLengthShift);
#ifdef XXE_EXEC_LOG_on
              sprintf(msg, "XXE::testOnIndexSub: sub xxe returned with "
                "finished=%d", localFinished);
              ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
              if (!localFinished)
                if (finished) *finished = false;  // unfinished ops in sub
              if (localCancelled)
                if (cancelled) *cancelled = true;  // cancelled ops in sub
            }
          }else{
            if (finished) *finished = false;  // comm not finished
          }
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::testOnIndexSub: returning with finished=%d",
            *finished);
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        }
        if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
      }
      break;
    case cancelIndex:
      {
        xxeCancelIndexInfo = (CancelIndexInfo *)xxeElement;
        xxeIndexElement = &(opstream[xxeCancelIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          vm->commcancel(xxeCommhandleInfo->commhandle);  // try to cancel
          // test the outstanding request
          int completeFlag;
          VMK::status status;
          vm->commtest(xxeCommhandleInfo->commhandle, &completeFlag, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
          if (completeFlag){
            // comm finished
            xxeCommhandleInfo->activeFlag = false;  // reset
          }else
            if (finished) *finished = false;  // comm not finished


if (xxeCommhandleInfo->cancelledFlag)
printf("gjt - CANCELLED commhandle\n");
else
printf("gjt - DID NOT CANCEL commhandle\n");

        }
        if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
      }
      break;
    case productSumVector:
      {
        xxeProductSumVectorInfo = (ProductSumVectorInfo *)xxeElement;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *element = (char *)xxeProductSumVectorInfo->element;
        char *factorList = (char *)xxeProductSumVectorInfo->factorList;
        char *valueList = (char *)xxeProductSumVectorInfo->valueList;
#else
        int *element = (int *)xxeProductSumVectorInfo->element;
        int *factorList = (int *)xxeProductSumVectorInfo->factorList;
        int *valueList = (int *)xxeProductSumVectorInfo->valueList;
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumVector: factorCount=%d",
          xxeProductSumVectorInfo->factorCount);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        psv(element, xxeProductSumVectorInfo->elementTK,
          factorList, xxeProductSumVectorInfo->factorTK,
          valueList, xxeProductSumVectorInfo->valueTK,
          xxeProductSumVectorInfo->factorCount, 0);
      }
      break;
    case productSumScalar:
      {
        xxeProductSumScalarInfo = (ProductSumScalarInfo *)xxeElement;
        // the following typecasts are necessary to  provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *element = (char *)xxeProductSumScalarInfo->element;
        char *factor = (char *)xxeProductSumScalarInfo->factor;
        char *value = (char *)xxeProductSumScalarInfo->value;
#else
        int *element = (int *)xxeProductSumScalarInfo->element;
        int *factor = (int *)xxeProductSumScalarInfo->factor;
        int *value = (int *)xxeProductSumScalarInfo->value;
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumScalar");
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        pss(element, xxeProductSumScalarInfo->elementTK,
          factor, xxeProductSumScalarInfo->factorTK,
          value, xxeProductSumScalarInfo->valueTK, 0);
      }
      break;
    case productSumScalarRRA:
      {
        xxeProductSumScalarRRAInfo = (ProductSumScalarRRAInfo *)xxeElement;
        char *rraBase = rraList[xxeProductSumScalarRRAInfo->rraIndex];
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *element =
          (char *)(rraBase + xxeProductSumScalarRRAInfo->rraOffset);
        char *factor = (char *)xxeProductSumScalarRRAInfo->factor;
        char *value = (char *)xxeProductSumScalarRRAInfo->value;
#else
        int *element =
          (int *)(rraBase + xxeProductSumScalarRRAInfo->rraOffset);
        int *factor = (int *)xxeProductSumScalarRRAInfo->factor;
        int *value = (int *)xxeProductSumScalarRRAInfo->value;
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumScalarRRA");
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        pss(element, xxeProductSumScalarRRAInfo->elementTK,
          factor, xxeProductSumScalarRRAInfo->factorTK,
          value, xxeProductSumScalarRRAInfo->valueTK, 0);
      }
      break;
    case sumSuperScalarDstRRA:
      {
        xxeSumSuperScalarDstRRAInfo =
          (SumSuperScalarDstRRAInfo *)xxeElement;
        int *rraOffsetList = xxeSumSuperScalarDstRRAInfo->rraOffsetList;
        int *valueOffsetList = xxeSumSuperScalarDstRRAInfo->valueOffsetList;
        int termCount = xxeSumSuperScalarDstRRAInfo->termCount;
        int vectorL = 1; // initialize
        if (xxeSumSuperScalarDstRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *rraBase = (char *)rraList[xxeSumSuperScalarDstRRAInfo->rraIndex];
        char *valueBase = (char *)xxeSumSuperScalarDstRRAInfo->valueBase;
        if (xxeSumSuperScalarDstRRAInfo->indirectionFlag)
          valueBase = *(char **)xxeSumSuperScalarDstRRAInfo->valueBase;
#else
        int *rraBase = (int *)rraList[xxeSumSuperScalarDstRRAInfo->rraIndex];
        int *valueBase = (int *)xxeSumSuperScalarDstRRAInfo->valueBase;
        if (xxeSumSuperScalarDstRRAInfo->indirectionFlag)
          valueBase = *(int **)xxeSumSuperScalarDstRRAInfo->valueBase;
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sumSuperScalarDstRRA: termCount=%d, vectorL=%d",
          termCount, vectorL);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        bool superVector = (xxeSumSuperScalarDstRRAInfo->vectorFlag
          && (superVectP && superVectP->dstSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int dstSuperVecSize_r =-1;
        int dstSuperVecSize_s = 1;
        int dstSuperVecSize_t = 1;
        int *dstSuperVecSize_i = NULL;
        int *dstSuperVecSize_j = NULL;
        if (superVectP){
          dstSuperVecSize_r = superVectP->dstSuperVecSize_r;
          dstSuperVecSize_s = superVectP->dstSuperVecSize_s;
          dstSuperVecSize_t = superVectP->dstSuperVecSize_t;
          dstSuperVecSize_i = superVectP->dstSuperVecSize_i;
          dstSuperVecSize_j = superVectP->dstSuperVecSize_j;
        }
        int srcLocalDeC = 0;  // init
        if (srcLocalDeCount) srcLocalDeC = *srcLocalDeCount;
        sssDstRra(rraBase, xxeSumSuperScalarDstRRAInfo->elementTK,
          rraOffsetList, valueBase, valueOffsetList,
          xxeSumSuperScalarDstRRAInfo->valueTK, termCount, vectorL, 0,
          xxeSumSuperScalarDstRRAInfo->rraIndex - srcLocalDeC,
          dstSuperVecSize_r,
          dstSuperVecSize_s,
          dstSuperVecSize_t,
          dstSuperVecSize_i,
          dstSuperVecSize_j,
          superVector);
      }
      break;
    case sumSuperScalarListDstRRA:
      {
        xxeSumSuperScalarListDstRRAInfo =
          (SumSuperScalarListDstRRAInfo *)xxeElement;
        int *rraOffsetList =
          xxeSumSuperScalarListDstRRAInfo->rraOffsetList;
        int *valueOffsetList =
          xxeSumSuperScalarListDstRRAInfo->valueOffsetList;
        int *baseListIndexList =
          xxeSumSuperScalarListDstRRAInfo->baseListIndexList;
        int termCount = xxeSumSuperScalarListDstRRAInfo->termCount;
        int vectorL = 1; // initialize
        if (xxeSumSuperScalarListDstRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char **rraBaseList = (char **)rraList;
        int *rraIndexList =
          xxeSumSuperScalarListDstRRAInfo->rraIndexList;
        char **valueBaseListResolve =
          (char **)xxeSumSuperScalarListDstRRAInfo->valueBaseListResolve;
        int valueBaseListSize =
          xxeSumSuperScalarListDstRRAInfo->valueBaseListSize;
        if (xxeSumSuperScalarListDstRRAInfo->indirectionFlag){
          // consider extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              *(char **)xxeSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }else{
          // no extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              (char *)xxeSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }
#else
        int **rraBaseList = (int **)rraList;
        int *rraIndexList =
          xxeSumSuperScalarListDstRRAInfo->rraIndexList;
        int **valueBaseListResolve =
          (int **)xxeSumSuperScalarListDstRRAInfo->valueBaseListResolve;
        int valueBaseListSize =
          xxeSumSuperScalarListDstRRAInfo->valueBaseListSize;
        if (xxeSumSuperScalarListDstRRAInfo->indirectionFlag){
          // consider extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              *(int **)xxeSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }else{
          // no extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              (int *)xxeSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::sumSuperScalarListDstRRA: termCount=%d, vectorL=%d",
          termCount, vectorL);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        bool superVector = (xxeSumSuperScalarListDstRRAInfo->vectorFlag
          && (superVectP && superVectP->dstSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int dstSuperVecSize_r =-1;
        int dstSuperVecSize_s = 1;
        int dstSuperVecSize_t = 1;
        int *dstSuperVecSize_i = NULL;
        int *dstSuperVecSize_j = NULL;
        if (superVectP){
          dstSuperVecSize_r = superVectP->dstSuperVecSize_r;
          dstSuperVecSize_s = superVectP->dstSuperVecSize_s;
          dstSuperVecSize_t = superVectP->dstSuperVecSize_t;
          dstSuperVecSize_i = superVectP->dstSuperVecSize_i;
          dstSuperVecSize_j = superVectP->dstSuperVecSize_j;
        }
        int srcLocalDeC = 0;  // init
        if (srcLocalDeCount) srcLocalDeC = *srcLocalDeCount;
        ssslDstRra(rraBaseList, rraIndexList,
          xxeSumSuperScalarListDstRRAInfo->elementTK,
          rraOffsetList,
          valueBaseListResolve, valueOffsetList, baseListIndexList,
          xxeSumSuperScalarListDstRRAInfo->valueTK, termCount, vectorL, 0,
          srcLocalDeC,
          dstSuperVecSize_r,
          dstSuperVecSize_s,
          dstSuperVecSize_t,
          dstSuperVecSize_i,
          dstSuperVecSize_j,
          superVector);
      }
      break;
    case productSumSuperScalarDstRRA:
      {
        xxeProductSumSuperScalarDstRRAInfo =
          (ProductSumSuperScalarDstRRAInfo *)xxeElement;
        int *rraOffsetList = xxeProductSumSuperScalarDstRRAInfo->rraOffsetList;
        int *valueOffsetList =
          xxeProductSumSuperScalarDstRRAInfo->valueOffsetList;
        int termCount = xxeProductSumSuperScalarDstRRAInfo->termCount;
        int vectorL = 1; // initialize
        if (xxeProductSumSuperScalarDstRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *rraBase =
          (char *)rraList[xxeProductSumSuperScalarDstRRAInfo->rraIndex];
        char *factorList =
          (char *)xxeProductSumSuperScalarDstRRAInfo->factorList;
        char *valueBase =
          (char *)xxeProductSumSuperScalarDstRRAInfo->valueBase;
        if (xxeProductSumSuperScalarDstRRAInfo->indirectionFlag)
          valueBase = *(char **)xxeProductSumSuperScalarDstRRAInfo->valueBase;
#else
        int *rraBase =
          (int *)rraList[xxeProductSumSuperScalarDstRRAInfo->rraIndex];
        int *factorList =
          (int *)xxeProductSumSuperScalarDstRRAInfo->factorList;
        int *valueBase =
          (int *)xxeProductSumSuperScalarDstRRAInfo->valueBase;
        if (xxeProductSumSuperScalarDstRRAInfo->indirectionFlag)
          valueBase = *(int **)xxeProductSumSuperScalarDstRRAInfo->valueBase;
#endif
        // recursively resolve the TKs of the arguments and execute operation
        bool superVector = (xxeProductSumSuperScalarDstRRAInfo->vectorFlag
          && (superVectP && superVectP->dstSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int dstSuperVecSize_r =-1;
        int dstSuperVecSize_s = 1;
        int dstSuperVecSize_t = 1;
        int *dstSuperVecSize_i = NULL;
        int *dstSuperVecSize_j = NULL;
        if (superVectP){
          dstSuperVecSize_r = superVectP->dstSuperVecSize_r;
          dstSuperVecSize_s = superVectP->dstSuperVecSize_s;
          dstSuperVecSize_t = superVectP->dstSuperVecSize_t;
          dstSuperVecSize_i = superVectP->dstSuperVecSize_i;
          dstSuperVecSize_j = superVectP->dstSuperVecSize_j;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumSuperScalarDstRRA: "
          "termCount=%d, vectorL=%d, vectorFlag=%d, dstSuperVecSize_r=%d, "
          "superVectorOkay=%d", termCount, vectorL,
          xxeProductSumSuperScalarDstRRAInfo->vectorFlag,
          dstSuperVecSize_r, superVectorOkay);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        int srcLocalDeC = 0;  // init
        if (srcLocalDeCount) srcLocalDeC = *srcLocalDeCount;
        psssDstRra(rraBase, xxeProductSumSuperScalarDstRRAInfo->elementTK,
          rraOffsetList, factorList,
          xxeProductSumSuperScalarDstRRAInfo->factorTK,
          valueBase, valueOffsetList,
          xxeProductSumSuperScalarDstRRAInfo->valueTK, termCount, vectorL, 0,
          xxeProductSumSuperScalarDstRRAInfo->rraIndex - srcLocalDeC,
          dstSuperVecSize_r,
          dstSuperVecSize_s,
          dstSuperVecSize_t,
          dstSuperVecSize_i,
          dstSuperVecSize_j,
          superVector);
      }
      break;
    case productSumSuperScalarListDstRRA:
      {
        xxeProductSumSuperScalarListDstRRAInfo =
          (ProductSumSuperScalarListDstRRAInfo *)xxeElement;
        int *rraOffsetList =
          xxeProductSumSuperScalarListDstRRAInfo->rraOffsetList;
        int *valueOffsetList =
          xxeProductSumSuperScalarListDstRRAInfo->valueOffsetList;
        int *baseListIndexList =
          xxeProductSumSuperScalarListDstRRAInfo->baseListIndexList;
        int termCount = xxeProductSumSuperScalarListDstRRAInfo->termCount;
        int vectorL = 1; // initialize
        if (xxeProductSumSuperScalarListDstRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char **rraBaseList = (char **)rraList;
        int *rraIndexList =
          xxeProductSumSuperScalarListDstRRAInfo->rraIndexList;
        char *factorList =
          (char *)xxeProductSumSuperScalarListDstRRAInfo->factorList;
        char **valueBaseListResolve =
          (char **)xxeProductSumSuperScalarListDstRRAInfo->valueBaseListResolve;
        int valueBaseListSize =
          xxeProductSumSuperScalarListDstRRAInfo->valueBaseListSize;
        if (xxeProductSumSuperScalarListDstRRAInfo->indirectionFlag){
          // consider extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              *(char **)xxeProductSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }else{
          // no extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              (char *)xxeProductSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }
#else
        int **rraBaseList = (int **)rraList;
        int *rraIndexList =
          xxeProductSumSuperScalarListDstRRAInfo->rraIndexList;
        int *factorList =
          (int *)xxeProductSumSuperScalarListDstRRAInfo->factorList;
        int **valueBaseListResolve =
          (int **)xxeProductSumSuperScalarListDstRRAInfo->valueBaseListResolve;
        int valueBaseListSize =
          xxeProductSumSuperScalarListDstRRAInfo->valueBaseListSize;
        if (xxeProductSumSuperScalarListDstRRAInfo->indirectionFlag){
          // consider extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              *(int **)xxeProductSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }else{
          // no extra indirection
          for (int i=0; i<valueBaseListSize; i++){
            valueBaseListResolve[i] =
              (int *)xxeProductSumSuperScalarListDstRRAInfo->valueBaseList[i];
          }
        }
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumSuperScalarListDstRRA: "
          "termCount=%d, vectorL=%d", termCount, vectorL);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        bool superVector = (xxeProductSumSuperScalarListDstRRAInfo->vectorFlag
          && (superVectP && superVectP->dstSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int dstSuperVecSize_r =-1;
        int dstSuperVecSize_s = 1;
        int dstSuperVecSize_t = 1;
        int *dstSuperVecSize_i = NULL;
        int *dstSuperVecSize_j = NULL;
        if (superVectP){
          dstSuperVecSize_r = superVectP->dstSuperVecSize_r;
          dstSuperVecSize_s = superVectP->dstSuperVecSize_s;
          dstSuperVecSize_t = superVectP->dstSuperVecSize_t;
          dstSuperVecSize_i = superVectP->dstSuperVecSize_i;
          dstSuperVecSize_j = superVectP->dstSuperVecSize_j;
        }
        int srcLocalDeC = 0;  // init
        if (srcLocalDeCount) srcLocalDeC = *srcLocalDeCount;
        pssslDstRra(rraBaseList, rraIndexList,
          xxeProductSumSuperScalarListDstRRAInfo->elementTK,
          rraOffsetList, factorList,
          xxeProductSumSuperScalarListDstRRAInfo->factorTK,
          valueBaseListResolve, valueOffsetList, baseListIndexList,
          xxeProductSumSuperScalarListDstRRAInfo->valueTK, termCount, vectorL,0,
          srcLocalDeC,
          dstSuperVecSize_r,
          dstSuperVecSize_s,
          dstSuperVecSize_t,
          dstSuperVecSize_i,
          dstSuperVecSize_j,
          superVector, rh);
      }
      break;
    case productSumSuperScalarSrcRRA:
      {
        xxeProductSumSuperScalarSrcRRAInfo =
          (ProductSumSuperScalarSrcRRAInfo *)xxeElement;
        int *rraOffsetList = xxeProductSumSuperScalarSrcRRAInfo->rraOffsetList;
        int *elementOffsetList =
          xxeProductSumSuperScalarSrcRRAInfo->elementOffsetList;
        int termCount = xxeProductSumSuperScalarSrcRRAInfo->termCount;
        int vectorL = 1; // initialize
        if (xxeProductSumSuperScalarSrcRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *rraBase =
          (char *)rraList[xxeProductSumSuperScalarSrcRRAInfo->rraIndex];
        char *factorList =
          (char *)xxeProductSumSuperScalarSrcRRAInfo->factorList;
        char *elementBase =
          (char *)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
        if (xxeProductSumSuperScalarSrcRRAInfo->indirectionFlag)
          elementBase =
            *(char **)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
#else
        int *rraBase =
          (int *)rraList[xxeProductSumSuperScalarSrcRRAInfo->rraIndex];
        int *factorList =
          (int *)xxeProductSumSuperScalarSrcRRAInfo->factorList;
        int *elementBase =
          (int *)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
        if (xxeProductSumSuperScalarSrcRRAInfo->indirectionFlag)
          elementBase =
            *(int **)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumSuperScalarSrcRRA: "
          "termCount=%d, vectorL=%d", termCount, vectorL);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        bool superVector = (xxeProductSumSuperScalarSrcRRAInfo->vectorFlag
          && (superVectP && superVectP->srcSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int srcSuperVecSize_r =-1;
        int srcSuperVecSize_s = 1;
        int srcSuperVecSize_t = 1;
        int *srcSuperVecSize_i = NULL;
        int *srcSuperVecSize_j = NULL;
        if (superVectP){
          srcSuperVecSize_r = superVectP->srcSuperVecSize_r;
          srcSuperVecSize_s = superVectP->srcSuperVecSize_s;
          srcSuperVecSize_t = superVectP->srcSuperVecSize_t;
          srcSuperVecSize_i = superVectP->srcSuperVecSize_i;
          srcSuperVecSize_j = superVectP->srcSuperVecSize_j;
        }
        psssSrcRra(rraBase, xxeProductSumSuperScalarSrcRRAInfo->valueTK,
          rraOffsetList, factorList,
          xxeProductSumSuperScalarSrcRRAInfo->factorTK,
          elementBase, elementOffsetList,
          xxeProductSumSuperScalarSrcRRAInfo->elementTK, termCount, vectorL, 0,
          xxeProductSumSuperScalarSrcRRAInfo->rraIndex,
          srcSuperVecSize_r,
          srcSuperVecSize_s,
          srcSuperVecSize_t,
          srcSuperVecSize_i,
          srcSuperVecSize_j,
          superVector);
      }
      break;
    case productSumSuperScalarContigRRA:
      {
        xxeProductSumSuperScalarContigRRAInfo =
          (ProductSumSuperScalarContigRRAInfo *)xxeElement;
        int *rraOffsetList =
          xxeProductSumSuperScalarContigRRAInfo->rraOffsetList;
        int termCount = xxeProductSumSuperScalarContigRRAInfo->termCount;
        int vectorL = 1; // initialize
        if (xxeProductSumSuperScalarContigRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        // the following typecasts are necessary to provide a valid TK
        // combination to call into the recursive function
#ifdef BGLWORKAROUND
        char *rraBase =
          (char *)rraList[xxeProductSumSuperScalarContigRRAInfo->rraIndex];
        char *factorList =
          (char *)xxeProductSumSuperScalarContigRRAInfo->factorList;
        char *valueList =
          (char *)xxeProductSumSuperScalarContigRRAInfo->valueList;
        if (xxeProductSumSuperScalarContigRRAInfo->indirectionFlag)
          valueList =
            *(char **)xxeProductSumSuperScalarContigRRAInfo->valueList;
#else
        int *rraBase =
          (int *)rraList[xxeProductSumSuperScalarContigRRAInfo->rraIndex];
        int *factorList =
          (int *)xxeProductSumSuperScalarContigRRAInfo->factorList;
        int *valueList =
          (int *)xxeProductSumSuperScalarContigRRAInfo->valueList;
        if (xxeProductSumSuperScalarContigRRAInfo->indirectionFlag)
          valueList =
            *(int **)xxeProductSumSuperScalarContigRRAInfo->valueList;
#endif
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::productSumSuperScalarContigRRA: "
          "termCount=%d, vectorL=%d", termCount, vectorL);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        // recursively resolve the TKs of the arguments and execute operation
        pssscRra(rraBase, xxeProductSumSuperScalarContigRRAInfo->elementTK,
          rraOffsetList, factorList,
          xxeProductSumSuperScalarContigRRAInfo->factorTK,
          valueList, xxeProductSumSuperScalarContigRRAInfo->valueTK, termCount,
          vectorL, 0);
      }
      break;
    case zeroScalarRRA:
      {
        xxeZeroScalarRRAInfo = (ZeroScalarRRAInfo *)xxeElement;
        int rraOffset = xxeZeroScalarRRAInfo->rraOffset;
        switch (xxeZeroScalarRRAInfo->elementTK){
        case I4:
          {
            ESMC_I4 *rraBase =
              (ESMC_I4 *)rraList[xxeZeroScalarRRAInfo->rraIndex];
            *(rraBase+rraOffset) = 0;
          }
          break;
        case I8:
          {
            ESMC_I8 *rraBase =
              (ESMC_I8 *)rraList[xxeZeroScalarRRAInfo->rraIndex];
            *(rraBase+rraOffset) = 0;
          }
          break;
        case R4:
          {
            ESMC_R4 *rraBase =
              (ESMC_R4 *)rraList[xxeZeroScalarRRAInfo->rraIndex];
            *(rraBase+rraOffset) = 0.;
          }
          break;
        case R8:
          {
            ESMC_R8 *rraBase =
              (ESMC_R8 *)rraList[xxeZeroScalarRRAInfo->rraIndex];
            *(rraBase+rraOffset) = 0.;
          }
          break;
        case BYTE:
          rc = ESMF_FAILURE;
          return rc;  // bail out
        }
      }
      break;
    case zeroSuperScalarRRA:
      {
        xxeZeroSuperScalarRRAInfo =
          (ZeroSuperScalarRRAInfo *)xxeElement;
        int vectorL = 1; // initialize
        if (xxeZeroSuperScalarRRAInfo->vectorFlag)
          vectorL = *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::zeroSuperScalarRRA: elementTK=%d, "
          "vectorFlag=%d, vectorL=%d", xxeZeroSuperScalarRRAInfo->elementTK,
          xxeZeroSuperScalarRRAInfo->vectorFlag, vectorL);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        bool superVector = (xxeZeroSuperScalarRRAInfo->vectorFlag
          && (superVectP && superVectP->dstSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int dstSuperVecSize_r =-1;
        int dstSuperVecSize_s = 1;
        int dstSuperVecSize_t = 1;
        int *dstSuperVecSize_i = NULL;
        int *dstSuperVecSize_j = NULL;
        if (superVectP){
          dstSuperVecSize_r = superVectP->dstSuperVecSize_r;
          dstSuperVecSize_s = superVectP->dstSuperVecSize_s;
          dstSuperVecSize_t = superVectP->dstSuperVecSize_t;
          dstSuperVecSize_i = superVectP->dstSuperVecSize_i;
          dstSuperVecSize_j = superVectP->dstSuperVecSize_j;
        }
        int srcLocalDeC = 0;  // init
        if (srcLocalDeCount) srcLocalDeC = *srcLocalDeCount;
        if(superVector){
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::zeroSuperScalarRRA: "
            "taking super-vector branch...");
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          switch (xxeZeroSuperScalarRRAInfo->elementTK){
          case I4:
            exec_zeroSuperScalarRRASuper<ESMC_I4>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList,
              xxeZeroSuperScalarRRAInfo->rraIndex - srcLocalDeC,
              dstSuperVecSize_r,
              dstSuperVecSize_s,
              dstSuperVecSize_t,
              dstSuperVecSize_i,
              dstSuperVecSize_j);
            break;
          case I8:
            exec_zeroSuperScalarRRASuper<ESMC_I8>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList,
              xxeZeroSuperScalarRRAInfo->rraIndex - srcLocalDeC,
              dstSuperVecSize_r,
              dstSuperVecSize_s,
              dstSuperVecSize_t,
              dstSuperVecSize_i,
              dstSuperVecSize_j);
            break;
          case R4:
            exec_zeroSuperScalarRRASuper<ESMC_R4>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList,
              xxeZeroSuperScalarRRAInfo->rraIndex - srcLocalDeC,
              dstSuperVecSize_r,
              dstSuperVecSize_s,
              dstSuperVecSize_t,
              dstSuperVecSize_i,
              dstSuperVecSize_j);
            break;
          case R8:
            exec_zeroSuperScalarRRASuper<ESMC_R8>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList,
              xxeZeroSuperScalarRRAInfo->rraIndex - srcLocalDeC,
              dstSuperVecSize_r,
              dstSuperVecSize_s,
              dstSuperVecSize_t,
              dstSuperVecSize_i,
              dstSuperVecSize_j);
            break;
          case BYTE:
            rc = ESMF_FAILURE;
            return rc;  // bail out
          }
        }else{
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::zeroSuperScalarRRA: "
            "taking vector branch...");
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          switch (xxeZeroSuperScalarRRAInfo->elementTK){
          case I4:
            exec_zeroSuperScalarRRA<ESMC_I4>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList);
            break;
          case I8:
            exec_zeroSuperScalarRRA<ESMC_I8>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList);
            break;
          case R4:
            exec_zeroSuperScalarRRA<ESMC_R4>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList);
            break;
          case R8:
            exec_zeroSuperScalarRRA<ESMC_R8>(xxeZeroSuperScalarRRAInfo,
              vectorL, rraList);
            break;
          case BYTE:
            rc = ESMF_FAILURE;
            return rc;  // bail out
          }
        }
      }
      break;
    case zeroMemset:
      {
        xxeZeroMemsetInfo =
          (ZeroMemsetInfo *)xxeElement;
        char *buffer = (char *)xxeZeroMemsetInfo->buffer;
        if (xxeZeroMemsetInfo->indirectionFlag)
          buffer = *(char **)xxeZeroMemsetInfo->buffer;
        int byteCount = xxeZeroMemsetInfo->byteCount;
        if(xxeZeroMemsetInfo->vectorFlag)
          byteCount *= *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::zeroMemset: indirectionFlag=%d, buffer=%p, "
          "vectorFlag=%d, byteCount=%d", xxeZeroMemsetInfo->indirectionFlag,
          buffer, xxeZeroMemsetInfo->vectorFlag, byteCount);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        memset(buffer, 0, byteCount);
      }
      break;
    case zeroMemsetRRA:
      {
        xxeZeroMemsetRRAInfo =
          (ZeroMemsetRRAInfo *)xxeElement;
        char *rraBase = rraList[xxeZeroMemsetRRAInfo->rraIndex];
        int byteCount = xxeZeroMemsetRRAInfo->byteCount;
        if(xxeZeroMemsetRRAInfo->vectorFlag)
          byteCount *= *vectorLength;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::zeroMemsetRRA: rraBase=%p, "
          "vectorFlag=%d, byteCount=%d", rraBase,
          xxeZeroMemsetRRAInfo->vectorFlag, byteCount);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        memset(rraBase, 0, byteCount);
      }
      break;
    case memCpy:
      {
        xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
        memcpy(xxeMemCpyInfo->dstMem, xxeMemCpyInfo->srcMem,
          xxeMemCpyInfo->size);
      }
      break;
    case memCpySrcRRA:
      {
        xxeMemCpySrcRRAInfo = (MemCpySrcRRAInfo *)xxeElement;
        memcpy(xxeMemCpySrcRRAInfo->dstMem,
          rraList[xxeMemCpySrcRRAInfo->rraIndex]
          + xxeMemCpySrcRRAInfo->rraOffset,
          xxeMemCpySrcRRAInfo->size);
      }
      break;
    case memGatherSrcRRA:
      {
        xxeMemGatherSrcRRAInfo = (MemGatherSrcRRAInfo *)xxeElement;
        char *dstBase = (char *)xxeMemGatherSrcRRAInfo->dstBase;
        if (xxeMemGatherSrcRRAInfo->indirectionFlag)
          dstBase = *(char **)xxeMemGatherSrcRRAInfo->dstBase;
        char *rraBase = rraList[xxeMemGatherSrcRRAInfo->rraIndex];
        int *rraOffsetList = xxeMemGatherSrcRRAInfo->rraOffsetList;
        int *countList = xxeMemGatherSrcRRAInfo->countList;
        int vectorL = 1; // initialize
        if (xxeMemGatherSrcRRAInfo->vectorFlag)
          vectorL = *vectorLength;
        bool superVector = (xxeMemGatherSrcRRAInfo->vectorFlag
          && (superVectP && superVectP->srcSuperVecSize_r>=1)
          && superVectorOkay);
        // initialize
        int srcSuperVecSize_r =-1;
        int srcSuperVecSize_s = 1;
        int srcSuperVecSize_t = 1;
        int *srcSuperVecSize_i = NULL;
        int *srcSuperVecSize_j = NULL;
        if (superVectP){
          srcSuperVecSize_r = superVectP->srcSuperVecSize_r;
          srcSuperVecSize_s = superVectP->srcSuperVecSize_s;
          srcSuperVecSize_t = superVectP->srcSuperVecSize_t;
          srcSuperVecSize_i = superVectP->srcSuperVecSize_i;
          srcSuperVecSize_j = superVectP->srcSuperVecSize_j;
        }
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::memGatherSrcRRA: dstBaseTK=%d, vectorFlag=%d, "
          "vectorL=%d, srcSuperVecSize_r=%d, superVectorOkay=%d",
          xxeMemGatherSrcRRAInfo->dstBaseTK,
          xxeMemGatherSrcRRAInfo->vectorFlag, vectorL,
          srcSuperVecSize_r,
          superVectorOkay);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        if(superVector){
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::memGatherSrcRRA: taking super-vector branch...");
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          switch (xxeMemGatherSrcRRAInfo->dstBaseTK){
          case I4:
            exec_memGatherSrcRRASuper<ESMC_I4>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList,
              srcSuperVecSize_r,
              srcSuperVecSize_s,
              srcSuperVecSize_t,
              srcSuperVecSize_i,
              srcSuperVecSize_j);
            break;
          case I8:
            exec_memGatherSrcRRASuper<ESMC_I8>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList,
              srcSuperVecSize_r,
              srcSuperVecSize_s,
              srcSuperVecSize_t,
              srcSuperVecSize_i,
              srcSuperVecSize_j);
            break;
          case R4:
            exec_memGatherSrcRRASuper<ESMC_R4>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList,
              srcSuperVecSize_r,
              srcSuperVecSize_s,
              srcSuperVecSize_t,
              srcSuperVecSize_i,
              srcSuperVecSize_j);
            break;
          case R8:
            exec_memGatherSrcRRASuper<ESMC_R8>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList,
              srcSuperVecSize_r,
              srcSuperVecSize_s,
              srcSuperVecSize_t,
              srcSuperVecSize_i,
              srcSuperVecSize_j);
            break;
          default:
            break;
          }
        }else{
#ifdef XXE_EXEC_LOG_on
          sprintf(msg, "XXE::memGatherSrcRRA: taking vector branch...");
          ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          switch (xxeMemGatherSrcRRAInfo->dstBaseTK){
          case BYTE:
            {
              char *dstPointer = dstBase;
              for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
                memcpy(dstPointer, rraBase + rraOffsetList[k] * vectorL,
                  countList[k] * vectorL);
                dstPointer += countList[k] * vectorL;
              }
            }
            break;
          case I4:
            exec_memGatherSrcRRA<ESMC_I4>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList);
            break;
          case I8:
            exec_memGatherSrcRRA<ESMC_I8>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList);
            break;
          case R4:
            exec_memGatherSrcRRA<ESMC_R4>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList);
            break;
          case R8:
            exec_memGatherSrcRRA<ESMC_R8>(xxeMemGatherSrcRRAInfo, vectorL,
              rraList);
            break;
          }
        }
      }
      break;
    case xxeSub:
      {
        xxeSubInfo = (XxeSubInfo *)xxeElement;
        if (xxeSubInfo->xxe){
          // recursive call:
          bool localFinished;
          bool localCancelled;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::xxeSub: rraCount=%d, rraList=%p, "
          "rraShift=%d, vectorLength=%p, vectorLengthShift=%d",
          rraCount, rraList, xxeSubInfo->rraShift, vectorLength,
          xxeSubInfo->vectorLengthShift);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
          xxeSubInfo->xxe->exec(rraCount, rraList + xxeSubInfo->rraShift,
            vectorLength + xxeSubInfo->vectorLengthShift, filterBitField,
            &localFinished, &localCancelled, NULL, -1, -1,
            srcLocalDeCount + xxeSubInfo->vectorLengthShift,
            superVectP + xxeSubInfo->vectorLengthShift);
          if (!localFinished)
            if (finished) *finished = false;  // unfinished ops in sub
          if (localCancelled)
            if (cancelled) *cancelled = true;  // cancelled ops in sub
        }
      }
      break;
    case xxeSubMulti:
      {
        xxeSubMultiInfo = (XxeSubMultiInfo *)xxeElement;
#ifdef XXE_EXEC_LOG_on
        sprintf(msg, "XXE::xxeSubMulti: count=%d", xxeSubMultiInfo->count);
        ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
        for (int k=0; k<xxeSubMultiInfo->count; k++){
          if (xxeSubMultiInfo->xxe[k]){
            // recursive call:
            bool localFinished;
            bool localCancelled;
            xxeSubMultiInfo->xxe[k]->exec(rraCount, rraList, vectorLength,
              filterBitField, &localFinished, &localCancelled, NULL, -1, -1,
              srcLocalDeCount, superVectP);
            if (!localFinished)
              if (finished) *finished = false;  // unfinished ops in sub
            if (localCancelled)
              if (cancelled) *cancelled = true;  // cancelled ops in sub
          }
        }
      }
      break;
    case wtimer:
      {
        xxeWtimerInfo = (WtimerInfo *)xxeElement;
        int index = xxeWtimerInfo->actualWtimerIndex;
        double *wtime = &(xxeWtimerInfo->wtime);
        *wtime = 0.;                      // initialize
        xxeWtimerInfo->wtimeSum = 0.;     // initialize
        xxeWtimerInfo->sumTermCount = -1;  // initialize
        xxeWtimerInfoActual = (WtimerInfo *)(&(opstream[index]));
        double *wtimeActual = &(xxeWtimerInfoActual->wtime);
        double *wtimeSumActual = &(xxeWtimerInfoActual->wtimeSum);
        int *sumTermCountActual = &(xxeWtimerInfoActual->sumTermCount);
        double wtimeRelative = *(xxeWtimerInfo->relativeWtime);
        // this xxe wtimer opstream element
        VMK::wtime(wtime);
        *wtime -= wtimeRelative;
        // actual xxe wtimer opstream element
        *wtimeSumActual += *wtime - *wtimeActual; // add time interval
        ++(*sumTermCountActual);  // count this sum term
        *wtimeActual = *wtime;
      }
      break;
    case message:
      {
        xxeMessageInfo = (MessageInfo *)xxeElement;
        printf("%s\n", xxeMessageInfo->messageString);
        fflush(stdout);
      }
      break;
    default:
      break;
    }
#ifdef XXE_EXEC_MEMLOG_on
    VM::logMemInfo(std::string("XXE::exec(): op-loop"));
#endif
  }

  if (dTime != NULL){
    VMK::wtime(&t1);
    *dTime = t1 - t0;
  }

#ifdef XXE_EXEC_MEMLOG_on
  VM::logMemInfo(std::string("XXE::exec():4.0"));
#endif

#ifdef XXE_EXEC_LOG_on
  sprintf(msg, "ESMCI::XXE::exec(): STOP");
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// templated XXE operations used in XXE::exec()
//-----------------------------------------------------------------------------

template<typename T>
inline void XXE::exec_memGatherSrcRRA(
  MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo, int vectorL, char **rraList){
  char *dstBase = (char *)xxeMemGatherSrcRRAInfo->dstBase;
  if (xxeMemGatherSrcRRAInfo->indirectionFlag)
    dstBase = *(char **)xxeMemGatherSrcRRAInfo->dstBase;
  char *rraBase = rraList[xxeMemGatherSrcRRAInfo->rraIndex];
  int *rraOffsetList = xxeMemGatherSrcRRAInfo->rraOffsetList;
  int *countList = xxeMemGatherSrcRRAInfo->countList;
  T *dstPointer = (T*)dstBase;
  T *srcPointer;
#ifdef XXE_EXEC_OPSLOG_on
  char msg[1024];
  sprintf(msg, "chunkCount=%d", xxeMemGatherSrcRRAInfo->chunkCount);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
  for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
    srcPointer = ((T*)rraBase) + rraOffsetList[k] * vectorL;
    for (int kk=0; kk<countList[k]*vectorL; kk++){
      dstPointer[kk] = srcPointer[kk];
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "srcPointer=" << &(srcPointer[kk]) << " *=" << srcPointer[kk]
          << " (" << k << "," << kk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
    }
    dstPointer += countList[k] * vectorL;
  }
}

//-----------------------------------------------------------------------------

template<typename T>
inline void XXE::exec_memGatherSrcRRASuper(
  MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo, int vectorL, char **rraList,
  int size_r, int size_s, int size_t, int *size_i, int *size_j){
  char *dstBase = (char *)xxeMemGatherSrcRRAInfo->dstBase;
  if (xxeMemGatherSrcRRAInfo->indirectionFlag)
    dstBase = *(char **)xxeMemGatherSrcRRAInfo->dstBase;
  char *rraBase = rraList[xxeMemGatherSrcRRAInfo->rraIndex];
  int *rraOffsetList = xxeMemGatherSrcRRAInfo->rraOffsetList;
  int *countList = xxeMemGatherSrcRRAInfo->countList;
  T *dstPointer = (T*)dstBase;
  T *srcPointer;
  int sz_i = size_i[xxeMemGatherSrcRRAInfo->rraIndex];
  int sz_j = size_j[xxeMemGatherSrcRRAInfo->rraIndex];
#ifdef XXE_EXEC_OPSLOG_on
  char msg[1024];
  sprintf(msg, "sz_i=%d, sz_j=%d, chunkCount=%d", sz_i, sz_j,
    xxeMemGatherSrcRRAInfo->chunkCount);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
  for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
    for (int kk=0; kk<countList[k]; kk++){
      int i = (rraOffsetList[k] + kk) % sz_i;
      int j = (rraOffsetList[k] + kk) / sz_i;
      srcPointer = ((T*)rraBase)
        + (j*size_s*sz_i + i) * size_r;
      int t=0;
      int s=0;
      for (int kkk=0; kkk<vectorL/size_r; kkk++){
        for (int kkkk=0; kkkk<size_r; kkkk++){
          dstPointer[kkkk] = srcPointer[kkkk];
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "srcPointer=" << &(srcPointer[kkkk]) << " *=" <<
          srcPointer[kkkk]
          << " (" << k << "," << kk << "," << kkk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        }
        dstPointer += size_r; // dst step in contiguous buffer
        // determine next src step
        ++s;
        if (s<size_s){
          srcPointer += sz_i*size_r;
        }else{
          s=0;
          ++t;
          srcPointer += (size_s*(sz_j-1)+1)*sz_i*size_r;
        }
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T>
inline void XXE::exec_zeroSuperScalarRRA(
  ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo, int vectorL,
  char **rraList){
  int *rraOffsetList = xxeZeroSuperScalarRRAInfo->rraOffsetList;
  int rraIndex = xxeZeroSuperScalarRRAInfo->rraIndex;
  int termCount = xxeZeroSuperScalarRRAInfo->termCount;
  bool vectorFlag = xxeZeroSuperScalarRRAInfo->vectorFlag;
  T *rraBase = (T*)rraList[rraIndex];
  if (!vectorFlag)
    for (int k=0; k<termCount; k++)
      *(rraBase+rraOffsetList[k]) = (T)0;
  else
    for (int k=0; k<termCount; k++)
      for (int kk=0; kk<vectorL; kk++)
        *(rraBase+(rraOffsetList[k]*vectorL)+kk) = (T)0;
}

//-----------------------------------------------------------------------------

template<typename T>
inline void XXE::exec_zeroSuperScalarRRASuper(
  ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo, int vectorL,
  char **rraList, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j){
  int *rraOffsetList = xxeZeroSuperScalarRRAInfo->rraOffsetList;
  int rraIndex = xxeZeroSuperScalarRRAInfo->rraIndex;
  int termCount = xxeZeroSuperScalarRRAInfo->termCount;
  T *rraBase = (T*)rraList[rraIndex];
  T *dstPointer;
  int sz_i = size_i[localDeIndexOff];
  int sz_j = size_j[localDeIndexOff];
  for (int k=0; k<termCount; k++){
    int i = rraOffsetList[k] % sz_i;
    int j = rraOffsetList[k] / sz_i;
    dstPointer = rraBase + (j*size_s*sz_i + i) * size_r;
    int t=0;
    int s=0;
    for (int kkk=0; kkk<vectorL/size_r; kkk++){
      for (int kkkk=0; kkkk<size_r; kkkk++){
        dstPointer[kkkk] = (T)0;
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "dstPointer=" <<  &(dstPointer[kkkk]) << " *=" <<
          dstPointer[kkkk]
          << " (" << k << "," << kkk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
      }
      // determine next dst step
      ++s;
      if (s<size_s){
        dstPointer += sz_i*size_r;
      }else{
        s=0;
        ++t;
        dstPointer += (size_s*(sz_j-1)+1)*sz_i*size_r;
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::psv(T *element, TKId elementTK, U *factorList, TKId factorTK,
  V *valueList, TKId valueTK, int factorCount, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psv operation on the data.
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *elementT = (ESMC_I4 *)element;
        psv(elementT, elementTK, factorList, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *elementT = (ESMC_I8 *)element;
        psv(elementT, elementTK, factorList, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *elementT = (ESMC_R4 *)element;
        psv(elementT, elementTK, factorList, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *elementT = (ESMC_R8 *)element;
        psv(elementT, elementTK, factorList, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (factorTK){
    case I4:
      {
        ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
        psv(element, elementTK, factorListT, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
        psv(element, elementTK, factorListT, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
        psv(element, elementTK, factorListT, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
        psv(element, elementTK, factorListT, factorTK, valueList, valueTK,
          factorCount, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==2){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *valueListT = (ESMC_I4 *)valueList;
        psv(element, elementTK, factorList, factorTK, valueListT, valueTK,
          factorCount, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueListT = (ESMC_I8 *)valueList;
        psv(element, elementTK, factorList, factorTK, valueListT, valueTK,
          factorCount, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueListT = (ESMC_R4 *)valueList;
        psv(element, elementTK, factorList, factorTK, valueListT, valueTK,
          factorCount, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueListT = (ESMC_R8 *)valueList;
        psv(element, elementTK, factorList, factorTK, valueListT, valueTK,
          factorCount, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in psv kernel with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  for (int i=0; i<factorCount; i++){
#ifdef XXE_EXEC_OPSLOG_on
    {
      std::stringstream logmsg;
      logmsg << "psv: element=" << element << ":" << *element
        << " factorList[i]=" << &(factorList[i]) << ":" << factorList[i]
        << " valueList[i]=" << &(valueList[i]) << ":" << valueList[i];
      ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
    *element += factorList[i] * valueList[i];
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::pss(T *element, TKId elementTK, U *factor, TKId factorTK,
  V *value, TKId valueTK, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing pss operation on the data.
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *elementT = (ESMC_I4 *)element;
        pss(elementT, elementTK, factor, factorTK, value, valueTK, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *elementT = (ESMC_I8 *)element;
        pss(elementT, elementTK, factor, factorTK, value, valueTK, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *elementT = (ESMC_R4 *)element;
        pss(elementT, elementTK, factor, factorTK, value, valueTK, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *elementT = (ESMC_R8 *)element;
        pss(elementT, elementTK, factor, factorTK, value, valueTK, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (factorTK){
    case I4:
      {
        ESMC_I4 *factorT = (ESMC_I4 *)factor;
        pss(element, elementTK, factorT, factorTK, value, valueTK, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *factorT = (ESMC_I8 *)factor;
        pss(element, elementTK, factorT, factorTK, value, valueTK, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *factorT = (ESMC_R4 *)factor;
        pss(element, elementTK, factorT, factorTK, value, valueTK, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *factorT = (ESMC_R8 *)factor;
        pss(element, elementTK, factorT, factorTK, value, valueTK, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==2){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *valueT = (ESMC_I4 *)value;
        pss(element, elementTK, factor, factorTK, valueT, valueTK, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueT = (ESMC_I8 *)value;
        pss(element, elementTK, factor, factorTK, valueT, valueTK, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueT = (ESMC_R4 *)value;
        pss(element, elementTK, factor, factorTK, valueT, valueTK, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueT = (ESMC_R8 *)value;
        pss(element, elementTK, factor, factorTK, valueT, valueTK, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in pss kernel with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
#ifdef XXE_EXEC_OPSLOG_on
    {
      std::stringstream logmsg;
      logmsg << "psv: element=" << element << ":" << *element
        << " factor=" << factor << ":" << *factor
        << " value=" << value << ":" << *value;
      ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
  *element += *factor * *value;
}

//-----------------------------------------------------------------------------

template<typename T, typename V>
void XXE::sssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
  V *valueBase, int *valueOffsetList, TKId valueTK, int termCount,
  int vectorL, int resolved, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j,
  bool superVector){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing sssDstRra operation on the data.
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Entering sssDstRra with T=" << typeid(T).name()
      << " V=" << typeid(V).name()
      << " resolved=" << resolved;
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *valueBaseT = (ESMC_I4 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueBaseT = (ESMC_I8 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueBaseT = (ESMC_R4 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueBaseT = (ESMC_R8 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in sssDstRra kernel with T=" << typeid(T).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if(superVector){
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::sumSuperScalarDstRRA: "
      "taking super-vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_sssDstRraSuper(rraBase, rraOffsetList, valueBase, valueOffsetList,
      termCount, vectorL, localDeIndexOff, size_r, size_s, size_t, size_i,
      size_j);
  }else{
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::sumSuperScalarDstRRA: "
      "taking vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_sssDstRra(rraBase, rraOffsetList, valueBase, valueOffsetList,
      termCount, vectorL);
  }
}

//---

template<typename T, typename V>
void XXE::exec_sssDstRra(T *rraBase, int *rraOffsetList, V *valueBase,
  int *valueOffsetList, int termCount, int vectorL){
  T *element;
  V *value;
  if (vectorL==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i];
      value = valueBase + valueOffsetList[i];
      *element += *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i] * vectorL;
      value = valueBase + valueOffsetList[i] * vectorL;
      for (int k=0; k<vectorL; k++)  // vector loop
        *(element+k) += *(value+k);
    }
  }
}

//---

template<typename T, typename V>
void XXE::exec_sssDstRraSuper(T *rraBase, int *rraOffsetList, V *valueBase,
  int *valueOffsetList, int termCount, int vectorL, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j){
  T *element;
  V *value;
  int sz_i = size_i[localDeIndexOff];
  int sz_j = size_j[localDeIndexOff];
#ifdef XXE_EXEC_OPSLOG_on
  char msg[1024];
  sprintf(msg, "sz_i=%d, sz_j=%d, termCount=%d", sz_i, sz_j, termCount);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
  for (int k=0; k<termCount; k++){  // super scalar loop
    int i = rraOffsetList[k] % sz_i;
    int j = rraOffsetList[k] / sz_i;
    element = rraBase + (j*size_s*sz_i + i) * size_r;
    value = valueBase + valueOffsetList[k] * vectorL;
    int t=0;
    int s=0;
    int kk=0;
    for (int kkk=0; kkk<vectorL/size_r; kkk++){
      for (int kkkk=0; kkkk<size_r; kkkk++){
        element[kkkk] += *(value+kk);
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "element=" <<  &(element[kkkk]) << " *=" << element[kkkk]
          << " (" << k << "," << kk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        ++kk;
      }
      // determine next dst step
      ++s;
      if (s<size_s){
        element += sz_i*size_r;
      }else{
        s=0;
        ++t;
        element += (size_s*(sz_j-1)+1)*sz_i*size_r;
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename V>
void XXE::ssslDstRra(T **rraBaseList, int *rraIndexList, TKId elementTK,
  int *rraOffsetList, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList,
  TKId valueTK, int termCount, int vectorL, int resolved, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j,
  bool superVector){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing ssslDstRra operation on the data.
  T *element;
  V *value;
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 **rraBaseTList = (ESMC_I4 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 **rraBaseTList = (ESMC_I8 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 **rraBaseTList = (ESMC_R4 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 **rraBaseTList = (ESMC_R8 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 **valueBaseTList = (ESMC_I4 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 **valueBaseTList = (ESMC_I8 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 **valueBaseTList = (ESMC_R4 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 **valueBaseTList = (ESMC_R8 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in ssslDstRra kernel with T=" << typeid(T).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if(superVector){
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::sumSuperScalarListDstRRA: "
      "taking super-vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_ssslDstRraSuper(rraBaseList, rraIndexList, rraOffsetList,
      valueBaseList, valueOffsetList, baseListIndexList, termCount, vectorL,
      localDeIndexOff, size_r, size_s, size_t, size_i, size_j);
  }else{
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::sumSuperScalarListDstRRA: "
      "taking vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_ssslDstRra(rraBaseList, rraIndexList, rraOffsetList,
      valueBaseList, valueOffsetList, baseListIndexList, termCount, vectorL);
  }
}

//---

template<typename T, typename V>
void XXE::exec_ssslDstRra(T **rraBaseList, int *rraIndexList,
  int *rraOffsetList, V **valueBaseList, int *valueOffsetList,
  int *baseListIndexList, int termCount, int vectorL){
  T *element;
  V *value;
  if (vectorL==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBaseList[rraIndexList[baseListIndexList[i]]]
        + rraOffsetList[i];
      value = valueBaseList[baseListIndexList[i]] + valueOffsetList[i];
      *element += *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBaseList[rraIndexList[baseListIndexList[i]]]
        + rraOffsetList[i] * vectorL;
      value = valueBaseList[baseListIndexList[i]]
        + valueOffsetList[i] * vectorL;
      for (int k=0; k<vectorL; k++)  // vector loop
        *(element+k) += *(value+k);
    }
  }
}

//---

template<typename T, typename V>
void XXE::exec_ssslDstRraSuper(T **rraBaseList, int *rraIndexList,
  int *rraOffsetList, V **valueBaseList, int *valueOffsetList,
  int *baseListIndexList, int termCount, int vectorL, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j){
  T *element;
  V *value;
  for (int k=0; k<termCount; k++){  // super scalar loop
    int sz_i = size_i[rraIndexList[baseListIndexList[k]]-localDeIndexOff];
    int sz_j = size_j[rraIndexList[baseListIndexList[k]]-localDeIndexOff];
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "sz_i=%d, sz_j=%d, termCount=%d", sz_i, sz_j, termCount);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    int i = rraOffsetList[k] % sz_i;
    int j = rraOffsetList[k] / sz_i;
    element = rraBaseList[rraIndexList[baseListIndexList[k]]]
      + (j*size_s*sz_i + i) * size_r;
    value = valueBaseList[baseListIndexList[k]]
      + valueOffsetList[k] * vectorL;
    int t=0;
    int s=0;
    int kk=0;
    for (int kkk=0; kkk<vectorL/size_r; kkk++){
      for (int kkkk=0; kkkk<size_r; kkkk++){
        element[kkkk] += *(value+kk);
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "element=" <<  &(element[kkkk]) << " *=" << element[kkkk]
          << " (" << k << "," << kk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        ++kk;
      }
      // determine next dst step
      ++s;
      if (s<size_s){
        element += sz_i*size_r;
      }else{
        s=0;
        ++t;
        element += (size_s*(sz_j-1)+1)*sz_i*size_r;
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::psssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
  U *factorList, TKId factorTK, V *valueBase, int *valueOffsetList,
  TKId valueTK, int termCount, int vectorL, int resolved, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j,
  bool superVector){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssDstRra operation on the data.
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Entering psssDstRra with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name()
      << " resolved=" << resolved;
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (factorTK){
    case I4:
      {
        ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==2){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *valueBaseT = (ESMC_I4 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueBaseT = (ESMC_I8 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueBaseT = (ESMC_R4 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueBaseT = (ESMC_R8 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in psssDstRra kernel with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if(superVector){
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::productSumSuperScalarDstRRA: "
      "taking super-vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_psssDstRraSuper(rraBase, rraOffsetList, factorList, valueBase,
      valueOffsetList, termCount, vectorL, localDeIndexOff,
      size_r, size_s, size_t, size_i, size_j);
  }else{
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::productSumSuperScalarDstRRA: "
      "taking vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_psssDstRra(rraBase, rraOffsetList, factorList, valueBase,
      valueOffsetList, termCount, vectorL);
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_psssDstRra(T *rraBase, int *rraOffsetList, U *factorList,
  V *valueBase, int *valueOffsetList, int termCount, int vectorL){
  T *element;
  U factor;
  V *value;
  if (vectorL==1){
    // scalar elements
    for (int k=0; k<termCount; k++){  // super scalar loop
      element = rraBase + rraOffsetList[k];
      factor = factorList[k];
      value = valueBase + valueOffsetList[k];
#ifdef XXE_EXEC_OPSLOG_on
    {
      std::stringstream logmsg;
      logmsg << "exec_psssDstRra: element=" << element << ":" << *element
        << " factor=" << factor
        << " value=" << value << ":" << *value;
      ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
      *element += factor * *value;
    }
  }else{
    // vector elements
    for (int k=0; k<termCount; k++){  // super scalar loop
      element = rraBase + rraOffsetList[k] * vectorL;
      factor = factorList[k];
      value = valueBase + valueOffsetList[k] * vectorL;
      for (int kk=0; kk<vectorL; kk++){  // vector loop
#ifdef XXE_EXEC_OPSLOG_on
    {
      std::stringstream logmsg;
      logmsg << "exec_psssDstRra: element+kk=" << element+kk << ":"
        << *(element+kk)
        << " factor=" << factor
        << " value+kk=" << value+kk << ":" << *(value+kk);
      ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
    }
#endif
        *(element+kk) += factor * *(value+kk);
      }
    }
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_psssDstRraSuper(T *rraBase, int *rraOffsetList, U *factorList,
  V *valueBase, int *valueOffsetList, int termCount, int vectorL,
  int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j){
  T *element;
  U factor;
  V *value;
  int sz_i = size_i[localDeIndexOff];
  int sz_j = size_j[localDeIndexOff];
#ifdef XXE_EXEC_OPSLOG_on
  char msg[1024];
  sprintf(msg, "sz_i=%d, sz_j=%d, termCount=%d", sz_i, sz_j, termCount);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
  for (int k=0; k<termCount; k++){  // super scalar loop
    int i = rraOffsetList[k] % sz_i;
    int j = rraOffsetList[k] / sz_i;
    element = rraBase + (j*size_s*sz_i + i) * size_r;
    factor = factorList[k];
    value = valueBase + valueOffsetList[k] * vectorL;
    int t=0;
    int s=0;
    int kk=0;
    for (int kkk=0; kkk<vectorL/size_r; kkk++){
      for (int kkkk=0; kkkk<size_r; kkkk++){
        element[kkkk] += factor * *(value+kk);
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "element=" <<  &(element[kkkk]) << " *=" << element[kkkk]
          << " (" << k << "," << kk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        ++kk;
      }
      // determine next dst step
      ++s;
      if (s<size_s){
        element += sz_i*size_r;
      }else{
        s=0;
        ++t;
        element += (size_s*(sz_j-1)+1)*sz_i*size_r;
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::pssslDstRra(T **rraBaseList, int *rraIndexList, TKId elementTK,
  int *rraOffsetList, U *factorList, TKId factorTK, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList,
  TKId valueTK, int termCount, int vectorL, int resolved, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j,
  bool superVector, RouteHandle *rh){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssDstRra operation on the data.
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 **rraBaseTList = (ESMC_I4 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case I8:
      {
        ESMC_I8 **rraBaseTList = (ESMC_I8 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case R4:
      {
        ESMC_R4 **rraBaseTList = (ESMC_R4 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case R8:
      {
        ESMC_R8 **rraBaseTList = (ESMC_R8 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (factorTK){
    case I4:
      {
        ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case I8:
      {
        ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case R4:
      {
        ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case R8:
      {
        ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==2){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 **valueBaseTList = (ESMC_I4 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case I8:
      {
        ESMC_I8 **valueBaseTList = (ESMC_I8 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case R4:
      {
        ESMC_R4 **valueBaseTList = (ESMC_R4 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    case R8:
      {
        ESMC_R8 **valueBaseTList = (ESMC_R8 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorL, resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector,
          rh);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in pssslDstRra kernel with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  int localrc;
#if 0
//TODO: strictly check for RH here once it is expected valid from all comms
  if (rh==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid RouteHandle pointer!", ESMC_CONTEXT, &localrc);
    throw localrc;  // bail out with exception
  }
  bool dynMask = rh->validAsPtr();
#else
  bool dynMask = false; // default
  if (rh) dynMask = rh->validAsPtr();
#endif
  if(superVector){
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::productSumSuperScalarListDstRRA: "
      "taking super-vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    if (dynMask){
      // dynamic masking is undefined for superVector condition -> error out
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
        "Dynamic masking is not defined for interleaved "
        "distributed and undistributed dims", ESMC_CONTEXT, &localrc);
      throw localrc;  // bail out with exception
    }
    exec_pssslDstRraSuper(rraBaseList, rraIndexList, rraOffsetList, factorList,
      valueBaseList, valueOffsetList, baseListIndexList, termCount, vectorL,
      localDeIndexOff, size_r, size_s, size_t, size_i, size_j);
  }else{
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::productSumSuperScalarListDstRRA: "
      "taking vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    if (dynMask){
      // with dynamic masking
      exec_pssslDstRraDynMask(rraBaseList, rraIndexList, rraOffsetList,
        factorList, valueBaseList, valueOffsetList, baseListIndexList,
        termCount, vectorL, rh);
    }else{
      // without dynamic masking
      exec_pssslDstRra(rraBaseList, rraIndexList, rraOffsetList, factorList,
        valueBaseList, valueOffsetList, baseListIndexList, termCount, vectorL);
    }
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_pssslDstRra(T **rraBaseList, int *rraIndexList,
  int *rraOffsetList, U *factorList, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList, int termCount, int vectorL){
  T *element;
  U factor;
  V *value;
  if (vectorL==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBaseList[rraIndexList[baseListIndexList[i]]]
        + rraOffsetList[i];
      factor = factorList[i];
      value = valueBaseList[baseListIndexList[i]] + valueOffsetList[i];
      *element += factor * *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBaseList[rraIndexList[baseListIndexList[i]]]
        + rraOffsetList[i] * vectorL;
      factor = factorList[i];
      value = valueBaseList[baseListIndexList[i]]
        + valueOffsetList[i] * vectorL;
      for (int k=0; k<vectorL; k++)  // vector loop
        *(element+k) += factor * *(value+k);
    }
  }
}

//---

template<typename T, typename U, typename V>
  void XXE::dynMaskHandler(vector<XXE::DynMaskElement<T,U,V> > &dynMaskList,
  RouteHandle *rh, int vectorL){
#if 0
  {
    std::stringstream logmsg;
    logmsg << "dynMaskHandler(): with dynMaskList.size()="
      << dynMaskList.size();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif

  int localrc;
  if (rh==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid RouteHandle pointer!", ESMC_CONTEXT, &localrc);
    throw localrc;  // bail out with exception
  }
  // prepare vectors to be passed to the fortran call back
  int count=dynMaskList.size();
  vector<T *> elementVector(count);
  vector<int> countVector(count);
  int totalCount=0;
  for (int i=0; i<count; i++){
    elementVector[i] = dynMaskList[i].element;
    countVector[i] = dynMaskList[i].factors.size();
    totalCount += countVector[i];
#if 0
  {
    std::stringstream logmsg;
    logmsg << "dynMaskHandler(): i=" << i << " element=" << elementVector[i];
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif

  }
  vector<U> factorsVector(totalCount);
  vector<V *> valuesVector(totalCount);
  int k=0;
  for (int i=0; i<count; i++){
    for (int j=0; j<countVector[i]; j++){
      factorsVector[k] = *(dynMaskList[i].factors[j]);
      valuesVector[k] = dynMaskList[i].values[j];
      ++k;
    }
  }
  // hand control back to Fortran layer - but must be type specific
  if (typeid(T)==typeid(ESMC_R8) && typeid(U)==typeid(ESMC_R8) &&
    typeid(V)==typeid(ESMC_R8)){
    FTN_X(f_esmf_dynmaskcallbackr8r8r8)(&rh, &count, &(elementVector[0]),
      &(countVector[0]), &totalCount, &(factorsVector[0]), &(valuesVector[0]),
      &vectorL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
#ifndef ESMF_NO_DYNMASKOVERLOAD
  }else if (typeid(T)==typeid(ESMC_R4) && typeid(U)==typeid(ESMC_R8) &&
    typeid(V)==typeid(ESMC_R4)){
    FTN_X(f_esmf_dynmaskcallbackr4r8r4)(&rh, &count, &(elementVector[0]),
      &(countVector[0]), &totalCount, &(factorsVector[0]), &(valuesVector[0]),
      &vectorL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
  }else if (typeid(T)==typeid(ESMC_R4) && typeid(U)==typeid(ESMC_R4) &&
    typeid(V)==typeid(ESMC_R4)){
    FTN_X(f_esmf_dynmaskcallbackr4r4r4)(&rh, &count, &(elementVector[0]),
      &(countVector[0]), &totalCount, &(factorsVector[0]), &(valuesVector[0]),
      &vectorL, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, NULL)) throw localrc;  // bail out with exception
#endif
  }else{
    // not a supported type combination
    char msg[1024];
    sprintf(msg, "Dynamic masking is not currently available for the "
      "requested combination of types: %s, %s, %s", typeid(T).name(),
      typeid(U).name(), typeid(V).name());
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, msg,
      ESMC_CONTEXT, &localrc);
    throw localrc;  // bail out with exception
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_pssslDstRraDynMask(T **rraBaseList, int *rraIndexList,
  int *rraOffsetList, U *factorList, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList, int termCount, int vectorL,
  RouteHandle *rh){
  T *element;
  T *prevElement=NULL;
  T tmpElement;
  T *dstMaskValue;
  U factor;
  V *value;
  V *srcMaskValue;
#if 0
  {
    std::stringstream logmsg;
    logmsg << "exec_pssslDstRraDynMask(): termCount=" << termCount;
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  int localrc;
  if (rh==NULL){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
      "Not a valid RouteHandle pointer!", ESMC_CONTEXT, &localrc);
    throw localrc;  // bail out with exception
  }
  rh->getSrcMaskValue(srcMaskValue);
  rh->getDstMaskValue(dstMaskValue);
  bool handleAllElements = rh->getHandleAllElements();
  vector<DynMaskElement<T,U,V> > dynMaskList;
  DynMaskElement<T,U,V> dynMaskElement;
  if (vectorL==1){
    // scalar elements
    if (handleAllElements){
      // dynMaskList to hold all local elements
      for (int i=0; i<termCount; i++){  // super scalar loop
        element = rraBaseList[rraIndexList[baseListIndexList[i]]]
          + rraOffsetList[i];
        if (prevElement != element){
          if (prevElement != NULL){
            // store dynMaskElement in dynMaskList
            dynMaskElement.element = prevElement;   // finish the entry
            dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
          }
          dynMaskElement.factors.clear(); // reset
          dynMaskElement.values.clear();  // reset
          prevElement = element;
        }
        value = valueBaseList[baseListIndexList[i]] + valueOffsetList[i];
        dynMaskElement.factors.push_back(&(factorList[i])); // dynMaskElement
        dynMaskElement.values.push_back(value); // dynMaskElement
      }
      // process the last element of the loop
      if (prevElement != NULL){
        // store dynMaskElement in dynMaskList
        dynMaskElement.element = prevElement;   // finish the entry
        dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
      }
    }else{
      // dynMaskList to hold only elements affected by dynamic mask
      // handle interpolation of all other elements here
      bool dstMask = false;
      bool srcMask = false;
      for (int i=0; i<termCount; i++){  // super scalar loop
        element = rraBaseList[rraIndexList[baseListIndexList[i]]]
          + rraOffsetList[i];
        if (prevElement != element){
          // this is a new element
          if (prevElement != NULL && !(dstMask || srcMask)){
            // finally write the previous sum into the actual element b/c unmasked
            *prevElement = tmpElement;
          }else if (dstMask || srcMask){
            // finally store dynMaskElement in dynMaskList b/c masking detected
            dynMaskElement.element = prevElement;   // finish the entry
            dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
          }
          dynMaskElement.factors.clear(); // reset
          dynMaskElement.values.clear();  // reset
          prevElement = element;
          srcMask = false;  // reset
          dstMask = false;  // reset
          if (dstMaskValue && (*element == *dstMaskValue))
            dstMask = true;
          else
            tmpElement = *element;  // load tmpElement with current element
        }
        factor = factorList[i];
        value = valueBaseList[baseListIndexList[i]] + valueOffsetList[i];
        dynMaskElement.factors.push_back(&(factorList[i])); // dynMaskElement
        dynMaskElement.values.push_back(value); // dynMaskElement
        if (srcMaskValue && (*value == *srcMaskValue)) srcMask = true;
        if (!(dstMask || srcMask)){
          tmpElement += factor * *value;  // perform calculation
        }
      }
      // process the last element of the loop
      if (prevElement != NULL && !(dstMask || srcMask)){
        // finally write the previous sum into the actual element
        *prevElement = tmpElement;
      }else if (dstMask || srcMask){
        // finally store dynMaskElement in dynMaskList b/c masking detected
        dynMaskElement.element = prevElement;   // finish the entry
        dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
      }
    }
  }else{
    // vector elements
    if (handleAllElements){
      // dynMaskList to hold all local elements
      for (int i=0; i<termCount; i++){  // super scalar loop
        element = rraBaseList[rraIndexList[baseListIndexList[i]]]
          + rraOffsetList[i] * vectorL;
        if (prevElement != element){
          if (prevElement != NULL){
            // store dynMaskElement in dynMaskList
            dynMaskElement.element = prevElement;   // finish the entry
            dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
          }
          dynMaskElement.factors.clear(); // reset
          dynMaskElement.values.clear();  // reset
          prevElement = element;
        }
        value = valueBaseList[baseListIndexList[i]]
          + valueOffsetList[i] * vectorL;
        dynMaskElement.factors.push_back(&(factorList[i])); // dynMaskElement
        dynMaskElement.values.push_back(value); // dynMaskElement
      }
      // process the last element of the loop
      if (prevElement != NULL){
        // store dynMaskElement in dynMaskList
        dynMaskElement.element = prevElement;   // finish the entry
        dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
      }
    }else{
      // vector handling with dynamic mask -> unroll over vectorL
      // dynMaskList to hold only elements affected by dynamic mask
      // handle interpolation of all other elements here
      bool dstMask = false;
      bool srcMask = false;
      for (int v=0; v<vectorL; v++){  // vector loop
        for (int i=0; i<termCount; i++){  // super scalar loop
          element = rraBaseList[rraIndexList[baseListIndexList[i]]]
            + rraOffsetList[i] * vectorL + v;
          if (prevElement != element){
            // this is a new element
            if (prevElement != NULL && !(dstMask || srcMask)){
              // finally write the previous sum into the actual element b/c unmasked
              *prevElement = tmpElement;
            }else if (dstMask || srcMask){
              // finally store dynMaskElement in dynMaskList b/c masking detected
              dynMaskElement.element = prevElement;   // finish the entry
              dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
            }
            dynMaskElement.factors.clear(); // reset
            dynMaskElement.values.clear();  // reset
            prevElement = element;
            srcMask = false;  // reset
            dstMask = false;  // reset
            if (dstMaskValue && (*element == *dstMaskValue))
              dstMask = true;
            else
              tmpElement = *element;  // load tmpElement with current element
          }
          factor = factorList[i];
          value = valueBaseList[baseListIndexList[i]]
            + valueOffsetList[i] * vectorL + v;
          dynMaskElement.factors.push_back(&(factorList[i])); // dynMaskElement
          dynMaskElement.values.push_back(value); // dynMaskElement
          if (srcMaskValue && (*value == *srcMaskValue)) srcMask = true;
          if (!(dstMask || srcMask)){
            tmpElement += factor * *value;  // perform calculation
          }
        }
      }
      // process the last element of the loop
      if (prevElement != NULL && !(dstMask || srcMask)){
        // finally write the previous sum into the actual element
        *prevElement = tmpElement;
      }else if (dstMask || srcMask){
        // finally store dynMaskElement in dynMaskList b/c masking detected
        dynMaskElement.element = prevElement;   // finish the entry
        dynMaskList.push_back(dynMaskElement);  // push into dynMaskList
      }
      // fully unrolled across vectorL -> set to 1 before passing down
      vectorL = 1;
    }
  }
  // call into handler method if there are any elements to handle
  if (dynMaskList.size() > 0){
    // call into dynMaskHandler to handle the masked elements
    dynMaskHandler(dynMaskList, rh, vectorL);
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_pssslDstRraSuper(T **rraBaseList, int *rraIndexList,
  int *rraOffsetList, U *factorList, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList, int termCount, int vectorL,
  int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j){
  T *element;
  U factor;
  V *value;
  for (int k=0; k<termCount; k++){  // super scalar loop
    int sz_i = size_i[rraIndexList[baseListIndexList[k]]-localDeIndexOff];
    int sz_j = size_j[rraIndexList[baseListIndexList[k]]-localDeIndexOff];
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "sz_i=%d, sz_j=%d, termCount=%d", sz_i, sz_j, termCount);
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    int i = rraOffsetList[k] % sz_i;
    int j = rraOffsetList[k] / sz_i;
    element = rraBaseList[rraIndexList[baseListIndexList[k]]]
      + (j*size_s*sz_i + i) * size_r;
    factor = factorList[k];
    value = valueBaseList[baseListIndexList[k]]
      + valueOffsetList[k] * vectorL;
    int t=0;
    int s=0;
    int kk=0;
    for (int kkk=0; kkk<vectorL/size_r; kkk++){
      for (int kkkk=0; kkkk<size_r; kkkk++){
        element[kkkk] += factor * *(value+kk);
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "element=" <<  &(element[kkkk]) << " *=" << element[kkkk]
          << " (" << k << "," << kk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        ++kk;
      }
      // determine next dst step
      ++s;
      if (s<size_s){
        element += sz_i*size_r;
      }else{
        s=0;
        ++t;
        element += (size_s*(sz_j-1)+1)*sz_i*size_r;
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::psssSrcRra(T *rraBase, TKId valueTK, int *rraOffsetList,
  U *factorList, TKId factorTK, V *elementBase, int *elementOffsetList,
  TKId elementTK, int termCount, int vectorL, int resolved, int localDeIndexOff,
  int size_r, int size_s, int size_t, int *size_i, int *size_j,
  bool superVector){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssSrcRra operation on the data.
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Entering psssSrcRra with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name()
      << " resolved=" << resolved;
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if (resolved==0){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (factorTK){
    case I4:
      {
        ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==2){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *elementBaseT = (ESMC_I4 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case I8:
      {
        ESMC_I8 *elementBaseT = (ESMC_I8 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R4:
      {
        ESMC_R4 *elementBaseT = (ESMC_R4 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    case R8:
      {
        ESMC_R8 *elementBaseT = (ESMC_R8 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorL,
          resolved,
          localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in psssSrcRra kernel with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if(superVector){
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::productSumSuperScalarSrcRRA: "
      "taking super-vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_psssSrcRraSuper(rraBase, rraOffsetList, factorList,
      elementBase, elementOffsetList, termCount, vectorL,
      localDeIndexOff, size_r, size_s, size_t, size_i, size_j, superVector);
  }else{
#ifdef XXE_EXEC_OPSLOG_on
    char msg[1024];
    sprintf(msg, "XXE::productSumSuperScalarSrcRRA: "
      "taking vector branch...");
    ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
    exec_psssSrcRra(rraBase, rraOffsetList, factorList,
      elementBase, elementOffsetList, termCount, vectorL);
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_psssSrcRra(T *rraBase, int *rraOffsetList, U *factorList,
  V *elementBase, int *elementOffsetList, int termCount, int vectorL){
  T *value;
  U factor;
  V *element;
  if (vectorL==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      value = rraBase + rraOffsetList[i];
      factor = factorList[i];
      element = elementBase + elementOffsetList[i];
      *element += factor * *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      value = rraBase + rraOffsetList[i] * vectorL;
      factor = factorList[i];
      element = elementBase + elementOffsetList[i] * vectorL;
      for (int k=0; k<vectorL; k++)  // vector loop
        *(element+k) += factor * *(value+k);
    }
  }
}

//---

template<typename T, typename U, typename V>
void XXE::exec_psssSrcRraSuper(T *rraBase, int *rraOffsetList, U *factorList,
  V *elementBase, int *elementOffsetList, int termCount, int vectorL,
  int localDeIndexOff, int size_r, int size_s, int size_t,
  int *size_i, int *size_j, bool superVector){
  T *value;
  U factor;
  V *element;
  int sz_i = size_i[localDeIndexOff];
  int sz_j = size_j[localDeIndexOff];
#ifdef XXE_EXEC_OPSLOG_on
  char msg[1024];
  sprintf(msg, "sz_i=%d, sz_j=%d, termCount=%d", sz_i, sz_j, termCount);
  ESMC_LogDefault.Write(msg, ESMC_LOGMSG_INFO);
#endif
  for (int k=0; k<termCount; k++){  // super scalar loop
    int i = rraOffsetList[k] % sz_i;
    int j = rraOffsetList[k] / sz_i;
    value = rraBase + (j*size_s*sz_i + i) * size_r;
    factor = factorList[k];
    element = elementBase + elementOffsetList[k] * vectorL;
    int t=0;
    int s=0;
    int kk=0;
    for (int kkk=0; kkk<vectorL/size_r; kkk++){
      for (int kkkk=0; kkkk<size_r; kkkk++){
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "element=" << &(element[kk]) << " *=" << element[kk]
          << " factor=" << factor
          << " value=" << &(value[kkkk]) << ":" << value[kkkk];
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        element[kk] += factor * value[kkkk];
#ifdef XXE_EXEC_OPSLOG_on
      {
        std::stringstream logmsg;
        logmsg << "element=" << &(element[kk]) << " *=" << element[kk]
          << " (" << k << "," << kk << "," << kkkk << ")";
        ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
      }
#endif
        ++kk;
      }
      // determine next src step
      ++s;
      if (s<size_s){
        value += sz_i*size_r;
      }else{
        s=0;
        ++t;
        value += (size_s*(sz_j-1)+1)*sz_i*size_r;
      }
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::pssscRra(T *rraBase, TKId elementTK, int *rraOffsetList,
  U *factorList, TKId factorTK, V *valueList, TKId valueTK,
  int termCount, int vectorL, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing pssscRra operation on the data.
  T *element;
  U factor;
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==1){
    ++resolved;
    switch (factorTK){
    case I4:
      {
        ESMC_I4 *factorListT = (ESMC_I4 *)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *factorListT = (ESMC_I8 *)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *factorListT = (ESMC_R4 *)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *factorListT = (ESMC_R8 *)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorL, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
  if (resolved==2){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *valueListT = (ESMC_I4 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorL, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueListT = (ESMC_I8 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorL, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueListT = (ESMC_R4 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorL, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueListT = (ESMC_R8 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorL, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_EXEC_RECURSLOG_on
  {
    std::stringstream logmsg;
    logmsg << "Arrived in pssscRra kernel with T=" << typeid(T).name()
      << " U=" << typeid(U).name()
      << " V=" << typeid(V).name();
    ESMC_LogDefault.Write(logmsg.str(), ESMC_LOGMSG_INFO);
  }
#endif
  if (vectorL==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i];
      factor = factorList[i];
      *element += factor * valueList[i];
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i] * vectorL;
      factor = factorList[i];
      for (int k=0; k<vectorL; k++)  // vector loop
        *(element+k) += factor * valueList[i*vectorL+k];
    }
  }
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::print()"
//BOPI
// !IROUTINE:  ESMCI::XXE::print
//
// !INTERFACE:
int XXE::print(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  FILE *fp,           // in  - file pointer
  int rraCount,       // in  - number of relative run-time address in rraList
  char **rraList,     // in  - relative run-time addresses
  int filterBitField, // in  - filter operations according to predicateBitField
  int indexStart,     // in  - start index, < 0 for default (full stream)
  int indexStop       // in  - stop index, < 0 for default (full stream)
  ){
//
// !DESCRIPTION:
//  Print XXE stream content.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // set index range
  int indexRangeStart = 0;        // default
  if (indexStart > 0) indexRangeStart = indexStart;
  int indexRangeStop = count-1;   // default
  if (indexStop > 0) indexRangeStop = indexStop;

  // check index range
  if (count > 0 && indexRangeStart > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "indexStart out of range", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (indexRangeStop > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "indexStop out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  StreamElement *xxeElement, *xxeIndexElement;
  SendInfo *xxeSendInfo;
  RecvInfo *xxeRecvInfo;
  SendRRAInfo *xxeSendRRAInfo;
  RecvRRAInfo *xxeRecvRRAInfo;
  SendnbInfo *xxeSendnbInfo;
  RecvnbInfo *xxeRecvnbInfo;
  SendnbRRAInfo *xxeSendnbRRAInfo;
  RecvnbRRAInfo *xxeRecvnbRRAInfo;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  TestOnIndexInfo *xxeTestOnIndexInfo;
  WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  WaitOnIndexSubInfo *waitOnIndexSubInfo;
  TestOnIndexSubInfo *testOnIndexSubInfo;
  CancelIndexInfo *xxeCancelIndexInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumVectorInfo *xxeProductSumVectorInfo;
  ProductSumScalarInfo *xxeProductSumScalarInfo;
  ProductSumScalarRRAInfo *xxeProductSumScalarRRAInfo;
  SumSuperScalarDstRRAInfo *xxeSumSuperScalarDstRRAInfo;
  ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo;
  ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo;
  ProductSumSuperScalarContigRRAInfo *xxeProductSumSuperScalarContigRRAInfo;
  ZeroScalarRRAInfo *xxeZeroScalarRRAInfo;
  ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo;
  ZeroMemsetInfo *xxeZeroMemsetInfo;
  ZeroMemsetRRAInfo *xxeZeroMemsetRRAInfo;
  MemCpyInfo *xxeMemCpyInfo;
  MemCpySrcRRAInfo *xxeMemCpySrcRRAInfo;
  MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo;
  XxeSubInfo *xxeSubInfo;
  XxeSubMultiInfo *xxeSubMultiInfo;
  WtimerInfo *xxeWtimerInfo, *xxeWtimerInfoActual, *xxeWtimerInfoRelative;
  MessageInfo *xxeMessageInfo;
  ProfileMessageInfo *xxeProfileMessageInfo;

  for (int i=indexRangeStart; i<=indexRangeStop; i++){
    xxeElement = &(opstream[i]);

    if (xxeElement->predicateBitField & filterBitField)
      continue; // filter out this operation

    fprintf(fp, "XXE::print(): <xxe=%p> <localPet=%d> i=%d, xxeElement=%p, "
      "opId=%d, predicateBitField=0x%08x\n", this,
      vm->getLocalPet(), i, xxeElement, opstream[i].opId,
      opstream[i].predicateBitField);
    switch(opstream[i].opId){
    case send:
      {
        xxeSendInfo = (SendInfo *)xxeElement;
        fprintf(fp, "  XXE::send: buffer=%p, size=%d, dst=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d\n",
          xxeSendInfo->buffer, xxeSendInfo->size, xxeSendInfo->dstPet,
          xxeSendInfo->tag, xxeSendInfo->vectorFlag,
          xxeSendInfo->indirectionFlag);
      }
      break;
    case recv:
      {
        xxeRecvInfo = (RecvInfo *)xxeElement;
        fprintf(fp, "  XXE::recv: buffer=%p, size=%d, src=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d\n",
          xxeRecvInfo->buffer, xxeRecvInfo->size, xxeRecvInfo->srcPet,
          xxeRecvInfo->tag, xxeRecvInfo->vectorFlag,
          xxeRecvInfo->indirectionFlag);
      }
      break;
    case sendRRA:
      {
        xxeSendRRAInfo = (SendRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::sendRRA: rraOffset=%d, size=%d, "
          "dst=%d, rraIndex=%d, tag=%d, vectorFlag=%d\n",
          xxeSendRRAInfo->rraOffset, xxeSendRRAInfo->size,
          xxeSendRRAInfo->dstPet, xxeSendRRAInfo->rraIndex,
          xxeSendRRAInfo->tag, xxeSendRRAInfo->vectorFlag);
      }
      break;
    case recvRRA:
      {
        xxeRecvRRAInfo = (RecvRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::recvRRA: rraOffset=%d, size=%d, "
          "src=%d, rraIndex=%d, tag=%d, vectorFlag=%d\n",
          xxeRecvRRAInfo->rraOffset, xxeRecvRRAInfo->size,
          xxeRecvRRAInfo->srcPet, xxeRecvRRAInfo->rraIndex,
          xxeRecvRRAInfo->tag, xxeRecvRRAInfo->vectorFlag);
      }
      break;
    case sendnb:
      {
        xxeSendnbInfo = (SendnbInfo *)xxeElement;
        fprintf(fp, "  XXE::sendnb: buffer=%p, size=%d, dst=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d, commhandle=%p\n",
          xxeSendnbInfo->buffer, xxeSendnbInfo->size, xxeSendnbInfo->dstPet,
          xxeSendnbInfo->tag, xxeSendnbInfo->vectorFlag,
          xxeSendnbInfo->indirectionFlag, xxeSendnbInfo->commhandle);
      }
      break;
    case recvnb:
      {
        xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
        fprintf(fp, "  XXE::recvnb: buffer=%p, size=%d, src=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d, commhandle=%p\n",
          xxeRecvnbInfo->buffer, xxeRecvnbInfo->size, xxeRecvnbInfo->srcPet,
          xxeRecvnbInfo->tag, xxeRecvnbInfo->vectorFlag,
          xxeRecvnbInfo->indirectionFlag, xxeRecvnbInfo->commhandle);
      }
      break;
    case sendnbRRA:
      {
        xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::sendnbRRA: rraOffset=%d, size=%d, "
          "dst=%d, rraIndex=%d, tag=%d, vectorFlag=%d, commhandle=%p\n",
          xxeSendnbRRAInfo->rraOffset, xxeSendnbRRAInfo->size,
          xxeSendnbRRAInfo->dstPet, xxeSendnbRRAInfo->rraIndex,
          xxeSendnbRRAInfo->tag, xxeSendnbRRAInfo->vectorFlag,
          xxeSendnbRRAInfo->commhandle);
      }
      break;
    case recvnbRRA:
      {
        xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::recvnbRRA: rraOffset=%d, size=%d, "
          "src=%d, rraIndex=%d, tag=%d, vectorFlag=%d, commhandle=%p\n",
          xxeRecvnbRRAInfo->rraOffset, xxeRecvnbRRAInfo->size,
          xxeRecvnbRRAInfo->srcPet, xxeRecvnbRRAInfo->rraIndex,
          xxeRecvnbRRAInfo->tag, xxeRecvnbRRAInfo->vectorFlag,
          xxeRecvnbRRAInfo->commhandle);
      }
      break;
    case waitOnIndex:
      {
        xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
        xxeIndexElement = &(opstream[xxeWaitOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::waitOnIndex: index=%d, "
          " commhandle=%p\n", xxeWaitOnIndexInfo->index,
          xxeCommhandleInfo->commhandle);
      }
      break;
    case testOnIndex:
      {
        xxeTestOnIndexInfo = (TestOnIndexInfo *)xxeElement;
        xxeIndexElement = &(opstream[xxeTestOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::testOnIndex: index=%d, "
          " commhandle=%p\n", xxeTestOnIndexInfo->index,
          xxeCommhandleInfo->commhandle);
      }
      break;
    case waitOnAnyIndexSub:
      {
        xxeWaitOnAnyIndexSubInfo = (WaitOnAnyIndexSubInfo *)xxeElement;
        fprintf(fp, "  XXE::waitOnAnyIndexSub <localPet=%d>\n",
          vm->getLocalPet());
        int *completeFlag = xxeWaitOnAnyIndexSubInfo->completeFlag;
        int count = xxeWaitOnAnyIndexSubInfo->count;
        int completeTotal = 0;  // reset
        for (int k=0; k<count; k++)
          xxeWaitOnAnyIndexSubInfo->xxe[k]->print(fp, rraCount, rraList);
          // recursive call
      }
      break;
    case waitOnIndexRange:
      {
        xxeWaitOnIndexRangeInfo = (WaitOnIndexRangeInfo *)xxeElement;
        fprintf(fp, "  XXE::waitOnIndexRange <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case waitOnIndexSub:
      {
        waitOnIndexSubInfo = (WaitOnIndexSubInfo *)xxeElement;
        xxeIndexElement = &(opstream[waitOnIndexSubInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::waitOnIndexSub index=%d, "
          " commhandle=%p\n", waitOnIndexSubInfo->index,
          xxeCommhandleInfo->commhandle);
        if (waitOnIndexSubInfo->xxe)
          waitOnIndexSubInfo->xxe->print(fp, rraCount,
            rraList+waitOnIndexSubInfo->rraShift);        // recursive call
      }
      break;
    case testOnIndexSub:
      {
        testOnIndexSubInfo = (TestOnIndexSubInfo *)xxeElement;
        xxeIndexElement = &(opstream[testOnIndexSubInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::testOnIndexSub index=%d, "
          " commhandle=%p\n", testOnIndexSubInfo->index,
          xxeCommhandleInfo->commhandle);
        if (testOnIndexSubInfo->xxe)
          testOnIndexSubInfo->xxe->print(fp, rraCount,
            rraList+testOnIndexSubInfo->rraShift);        // recursive call
      }
      break;
    case cancelIndex:
      {
        xxeCancelIndexInfo = (CancelIndexInfo *)xxeElement;
        xxeIndexElement = &(opstream[xxeCancelIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::CancelIndex: index=%d, "
          " commhandle=%p\n", xxeCancelIndexInfo->index,
          xxeCommhandleInfo->commhandle);
      }
      break;
    case productSumVector:
      {
        xxeProductSumVectorInfo = (ProductSumVectorInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumVector <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case productSumScalar:
      {
        xxeProductSumScalarInfo = (ProductSumScalarInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumScalar <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case productSumScalarRRA:
      {
        xxeProductSumScalarRRAInfo = (ProductSumScalarRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumScalarRRA <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case sumSuperScalarDstRRA:
      {
        xxeSumSuperScalarDstRRAInfo =
          (SumSuperScalarDstRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::sumSuperScalarDstRRA rraIndex=%d, "
          "termCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          xxeSumSuperScalarDstRRAInfo->rraIndex,
          xxeSumSuperScalarDstRRAInfo->termCount,
          xxeSumSuperScalarDstRRAInfo->vectorFlag,
          xxeSumSuperScalarDstRRAInfo->indirectionFlag);
      }
      break;
    case productSumSuperScalarDstRRA:
      {
        xxeProductSumSuperScalarDstRRAInfo =
          (ProductSumSuperScalarDstRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumSuperScalarDstRRA "
          "rraIndex=%d, termCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          xxeProductSumSuperScalarDstRRAInfo->rraIndex,
          xxeProductSumSuperScalarDstRRAInfo->termCount,
          xxeProductSumSuperScalarDstRRAInfo->vectorFlag,
          xxeProductSumSuperScalarDstRRAInfo->indirectionFlag);
      }
      break;
    case productSumSuperScalarSrcRRA:
      {
        xxeProductSumSuperScalarSrcRRAInfo =
          (ProductSumSuperScalarSrcRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumSuperScalarSrcRRA "
          "rraIndex=%d, termCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          xxeProductSumSuperScalarSrcRRAInfo->rraIndex,
          xxeProductSumSuperScalarSrcRRAInfo->termCount,
          xxeProductSumSuperScalarSrcRRAInfo->vectorFlag,
          xxeProductSumSuperScalarSrcRRAInfo->indirectionFlag);
      }
      break;
    case productSumSuperScalarContigRRA:
      {
        xxeProductSumSuperScalarContigRRAInfo =
          (ProductSumSuperScalarContigRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumSuperScalarContigRRA <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case zeroScalarRRA:
      {
        xxeZeroScalarRRAInfo = (ZeroScalarRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroScalarRRA rraOffset=%d, "
          "rraIndex=%d\n", xxeZeroScalarRRAInfo->rraOffset,
          xxeZeroScalarRRAInfo->rraIndex);
      }
      break;
    case zeroSuperScalarRRA:
      {
        xxeZeroSuperScalarRRAInfo = (ZeroSuperScalarRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroSuperScalarRRA rraOffsetList=%p, "
          "rraIndex=%d, termCount=%d, vectorFlag=%d\n",
          xxeZeroSuperScalarRRAInfo->rraOffsetList,
          xxeZeroSuperScalarRRAInfo->rraIndex,
          xxeZeroSuperScalarRRAInfo->termCount,
          xxeZeroSuperScalarRRAInfo->vectorFlag);
      }
      break;
    case zeroMemset:
      {
        xxeZeroMemsetInfo = (ZeroMemsetInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroMemset buffer=%p, byteCount=%d, "
          "vectorFlag=%d, indirectionFlag=%d\n",
            xxeZeroMemsetInfo->buffer,
            xxeZeroMemsetInfo->byteCount, xxeZeroMemsetInfo->vectorFlag,
            xxeZeroMemsetInfo->indirectionFlag);
      }
      break;
    case zeroMemsetRRA:
      {
        xxeZeroMemsetRRAInfo = (ZeroMemsetRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroMemsetRRA byteCount=%d, "
          "rraIndex=%d, vectorFlag=%d\n",
          xxeZeroMemsetRRAInfo->byteCount,
          xxeZeroMemsetRRAInfo->rraIndex, xxeZeroMemsetRRAInfo->vectorFlag);
      }
      break;
    case memCpy:
      {
        xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
        fprintf(fp, "  XXE::memCpy <localPet=%d>\n", vm->getLocalPet());
      }
      break;
    case memCpySrcRRA:
      {
        xxeMemCpySrcRRAInfo = (MemCpySrcRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::memCpySrcRRA <localPet=%d>\n", vm->getLocalPet());
      }
      break;
    case memGatherSrcRRA:
      {
        xxeMemGatherSrcRRAInfo = (MemGatherSrcRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::memGatherSrcRRA: dstBase=%p, dstBaseTK=%d, "
          "chunkCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          xxeMemGatherSrcRRAInfo->dstBase,
          xxeMemGatherSrcRRAInfo->dstBaseTK,
          xxeMemGatherSrcRRAInfo->chunkCount,
          xxeMemGatherSrcRRAInfo->vectorFlag,
          xxeMemGatherSrcRRAInfo->indirectionFlag);
      }
      break;
    case xxeSub:
      {
        xxeSubInfo = (XxeSubInfo *)xxeElement;
        fprintf(fp, "  XXE::xxeSub <localPet=%d>\n", vm->getLocalPet());
        if (xxeSubInfo->xxe)
          xxeSubInfo->xxe->print(fp, rraCount, rraList); // recursive call
      }
      break;
    case xxeSubMulti:
      {
        xxeSubMultiInfo = (XxeSubMultiInfo *)xxeElement;
        fprintf(fp, "  XXE::xxeSubMulti count=%d\n", xxeSubMultiInfo->count);
        for (int k=0; k<xxeSubMultiInfo->count; k++)
          xxeSubMultiInfo->xxe[k]->print(fp, rraCount, rraList); // recurs. call
      }
      break;
    case wtimer:
      {
        xxeWtimerInfo = (WtimerInfo *)xxeElement;
        int index = xxeWtimerInfo->actualWtimerIndex;
        fprintf(fp, "  XXE::wtimer index=%d\n", index);
        double *wtime = &(xxeWtimerInfo->wtime);
        *wtime = 0.;                      // initialize
        xxeWtimerInfo->wtimeSum = 0.;     // initialize
        xxeWtimerInfo->sumTermCount = -1;  // initialize
        xxeWtimerInfoActual = (WtimerInfo *)(&(opstream[index]));
        double *wtimeActual = &(xxeWtimerInfoActual->wtime);
        double *wtimeSumActual = &(xxeWtimerInfoActual->wtimeSum);
        int *sumTermCountActual = &(xxeWtimerInfoActual->sumTermCount);
        double wtimeRelative = *(xxeWtimerInfo->relativeWtime);
      }
      break;
    case message:
      {
        xxeMessageInfo = (MessageInfo *)xxeElement;
        fprintf(fp, "  XXE::message string=%s\n", xxeMessageInfo->messageString);
      }
      break;
    case profileMessage:
      {
        xxeProfileMessageInfo = (ProfileMessageInfo *)xxeElement;
        fprintf(fp, "  XXE::profileMessage string=%s\n",
          xxeProfileMessageInfo->messageString);
      }
      break;
    case nop:
      {
        fprintf(fp, "  XXE::nop\n");
      }
      break;
    default:
      break;
    }
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::printProfile()"
//BOPI
// !IROUTINE:  ESMCI::XXE::printProfile
//
// !INTERFACE:
int XXE::printProfile(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  FILE *fp){
//
// !DESCRIPTION:
//  Print profile data collected during the XXE opstream execution.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#if 0
  printf("gjt in ESMCI::XXE::printProfile(), opstream=%p, %d, %d\n", opstream,
    count, sizeof(StreamElement));
#endif

  int localPet = vm->getLocalPet();

  StreamElement *xxeElement;
  WtimerInfo *xxeWtimerInfo;
  XxeSubInfo *xxeSubInfo;
  XxeSubMultiInfo *xxeSubMultiInfo;
  WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo;
  WaitOnIndexSubInfo *waitOnIndexSubInfo;
  TestOnIndexSubInfo *testOnIndexSubInfo;
  ProfileMessageInfo *xxeProfileMessageInfo;

  for (int i=0; i<count; i++){
    xxeElement = &(opstream[i]);

    if (xxeElement->predicateBitField & lastFilterBitField)
      continue; // filter out this operation

//    printf("gjt: %d, opId=%d\n", i, opstream[i].opId);
    switch(opstream[i].opId){
    case xxeSub:
      xxeSubInfo = (XxeSubInfo *)xxeElement;
      if (xxeSubInfo->xxe)
        xxeSubInfo->xxe->printProfile(fp); // recursive call
      break;
    case xxeSubMulti:
      xxeSubMultiInfo = (XxeSubMultiInfo *)xxeElement;
      for (int k=0; k<xxeSubMultiInfo->count; k++)
        xxeSubMultiInfo->xxe[k]->printProfile(fp); // recursive call
      break;
    case waitOnAnyIndexSub:
      xxeWaitOnAnyIndexSubInfo = (WaitOnAnyIndexSubInfo *)xxeElement;
      for (int k=0; k<xxeWaitOnAnyIndexSubInfo->count; k++)
        xxeWaitOnAnyIndexSubInfo->xxe[k]->printProfile(fp); // recursive call
      break;
    case waitOnIndexSub:
      waitOnIndexSubInfo = (WaitOnIndexSubInfo *)xxeElement;
      if (waitOnIndexSubInfo->xxe)
        waitOnIndexSubInfo->xxe->printProfile(fp); // recursive call
      break;
    case testOnIndexSub:
      testOnIndexSubInfo = (TestOnIndexSubInfo *)xxeElement;
      if (testOnIndexSubInfo->xxe)
        testOnIndexSubInfo->xxe->printProfile(fp); // recursive call
      break;
    case wtimer:
      {
        xxeWtimerInfo = (WtimerInfo *)xxeElement;
        int index = xxeWtimerInfo->actualWtimerIndex;
        if (index == i){
          // this is an actual wtimer element -> print
          fprintf(fp, "localPet %d - XXE profile wtimer - id: %04d  "
            "element: %04d\t%s\t wtime = %gs\t wtimeSum = %gs\t"
            "sumTermCount: %d\n",
            localPet, xxeWtimerInfo->timerId, i, xxeWtimerInfo->timerString,
            xxeWtimerInfo->wtime, xxeWtimerInfo->wtimeSum,
            xxeWtimerInfo->sumTermCount);
        }
      }
      break;
    case profileMessage:
      {
        xxeProfileMessageInfo = (ProfileMessageInfo *)xxeElement;
        fprintf(fp, "localPet %d - XXE profileMessage: %s\n",
          localPet, xxeProfileMessageInfo->messageString);
      }
      break;
    default:
      break;
    }
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

      //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::optimizeElement()"
//BOPI
// !IROUTINE:  ESMCI::XXE::optimizeElement
//
// !INTERFACE:
int XXE::optimizeElement(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int index){
//
// !DESCRIPTION:
//  Optimize element indexed by "index".
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#if 0
  printf("gjt in ESMCI::XXE::optimizeElement(), opstream=%p, %d, %d\n", opstream,
    count, sizeof(StreamElement));
#endif

  if (index < 0 || index >= count){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "index out of range", ESMC_CONTEXT, &rc);
    return rc;
  }

  StreamElement *xxeElement = &(opstream[index]);
  switch(opstream[index].opId){
  case productSumSuperScalarDstRRA:
    ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo;
    struct ListSort{
      int sortIndex;
      int elementIndex;
      static int cmp(const void *a, const void *b){
        ListSort *aObj = (ListSort *)a;
        ListSort *bObj = (ListSort *)b;
        if (aObj->sortIndex < bObj->sortIndex) return -1;
        if (aObj->sortIndex > bObj->sortIndex) return +1;
        // sortIndex must be equal
        return 0;
      }
    };
    {
      xxeProductSumSuperScalarDstRRAInfo =
        (ProductSumSuperScalarDstRRAInfo *)xxeElement;
      int termCount = xxeProductSumSuperScalarDstRRAInfo->termCount;
      int *rraOffsetList = xxeProductSumSuperScalarDstRRAInfo->rraOffsetList;
      void *factorList = xxeProductSumSuperScalarDstRRAInfo->factorList;
//      void **valueList = xxeProductSumSuperScalarDstRRAInfo->valueList;
#if 0
      // The following code rearranges the sequence of element, factor, value.
      // I introduced this code in hopes of optimizing the
      // productSumSuperScalarDstRRA execution time. On columbia I see
      // performance
      // variations on the order of one magnitude between different
      // productSumSuperScalarDstRRA's (all of the same length 69490 elements),
      // so I thought that the difference might be the order in which the
      // elements are being processed. Interestingly the following
      // rearrangement did not change performance on any of the
      // productSumSuperScalarDstRRA's! The slow ones stayed slow and the fast
      // ones stayed fast. This finding leads me to believe that the differnce
      // must be not in the inter-element interaction but must be an
      // intra-element issue, so that rearranging elements does not change
      // anything. The most likely intra-element interaction I can think of
      // are cache effects. *gjt*
      ListSort *list = new ListSort[termCount];
      for (int k=0; k<termCount; k++){
        list[k].sortIndex = rraOffsetList[k];
        list[k].elementIndex = k;
      }
      qsort(list, termCount, sizeof(ListSort), ListSort::cmp);
      void **factorSortList = new void*[termCount];
      void **valueSortList = new void*[termCount];
      memcpy(factorSortList, factorList, termCount*sizeof(void *));
      memcpy(valueSortList, valueList, termCount*sizeof(void *));
      int altFlag = 0;  // reset
      int kk = 0; // reset
      int halfShift = (termCount+1) / 2;
      for (int k=0; k<termCount; k++){
        int kkk = kk;
        if (altFlag){
          kkk += halfShift;
          ++kk;
          altFlag = 0;
        }else
          altFlag = 1;
        rraOffsetList[kkk] = list[k].sortIndex;
        factorList[kkk] = factorSortList[list[k].elementIndex];
        valueList[kkk] = valueSortList[list[k].elementIndex];
      }
#endif
    }
  default:
    break;
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

      //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::execReady()"
//BOPI
// !IROUTINE:  ESMCI::XXE::execReady
//
// !INTERFACE:
int XXE::execReady(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#if 0
  printf("gjt in ESMCI::XXE::execReady(), opstream=%p, %d, %d\n", opstream,
    count, sizeof(StreamElement));
#endif

  const int sendnbMax = 20000;
  int *sendnbIndexList = new int[sendnbMax];
  int sendnbCount = 0;
  int sendnbLowerIndex = -1;  // prime lower index indicator blow 0

  const int recvnbMax = 20000;
  int *recvnbIndexList = new int[recvnbMax];
  int recvnbCount = 0;
  int recvnbLowerIndex = -1;  // prime lower index indicator blow 0

  StreamElement *xxeElement, *xxeIndexElement, *xxeElement2;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  TestOnIndexSubInfo *testOnIndexSubInfo;
  WaitOnIndexSubInfo *waitOnIndexSubInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumVectorInfo *xxeProductSumVectorInfo;
  WtimerInfo *xxeWtimerInfo, *xxeWtimerInfo2;
  XxeSubInfo *xxeSubInfo;
  XxeSubMultiInfo *xxeSubMultiInfo;

  int i = 0;  // prime index counter
  while(i!=count){
    // repeat going through the entire opstream until no more StreamElements
    // need to be replaced, i.e. the i-loop will finally make it all the way
    // through.
    sendnbCount = 0;
    recvnbCount = 0;

    for (i=0; i<count; i++){
      xxeElement = &(opstream[i]);
//    printf("gjt: %d, opId=%d\n", i, opstream[i].opId);
      int breakFlag = 0;  // reset
      switch(opstream[i].opId){
      case xxeSub:
        xxeSubInfo = (XxeSubInfo *)xxeElement;
        if (xxeSubInfo->xxe){
          localrc = xxeSubInfo->xxe->execReady(); // recursive call
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
        break;
      case xxeSubMulti:
        xxeSubMultiInfo = (XxeSubMultiInfo *)xxeElement;
        for (int k=0; k<xxeSubMultiInfo->count; k++){
          localrc = xxeSubMultiInfo->xxe[k]->execReady(); // recursive call
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
        break;
      case waitOnAnyIndexSub:
        xxeWaitOnAnyIndexSubInfo = (WaitOnAnyIndexSubInfo *)xxeElement;
        for (int k=0; k<xxeWaitOnAnyIndexSubInfo->count; k++){
          localrc = xxeWaitOnAnyIndexSubInfo->xxe[k]->execReady(); // recu. call
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
        break;
      case waitOnIndexSub:
        waitOnIndexSubInfo = (WaitOnIndexSubInfo *)xxeElement;
        if (waitOnIndexSubInfo->xxe){
          localrc = waitOnIndexSubInfo->xxe->execReady(); // recursive call
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
        break;
      case testOnIndexSub:
        testOnIndexSubInfo = (TestOnIndexSubInfo *)xxeElement;
        if (testOnIndexSubInfo->xxe){
          localrc = testOnIndexSubInfo->xxe->execReady(); // recursive call
          if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc)) return rc;
        }
        break;
      case send:
        break;
      case recv:
        break;
      case sendnb:
        if (i>sendnbLowerIndex){
          sendnbIndexList[sendnbCount] = i;
          ++sendnbCount;
          if (sendnbCount >= sendnbMax){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
              "sendnbCount out of range", ESMC_CONTEXT, &rc);
            return rc;
          }
        }
        break;
      case recvnb:
        if (i>recvnbLowerIndex){
          recvnbIndexList[recvnbCount] = i;
          ++recvnbCount;
          if (recvnbCount >= recvnbMax){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
              "recvnbCount out of range", ESMC_CONTEXT, &rc);
            return rc;
          }
        }
        break;
      case sendnbRRA:
        if (i>sendnbLowerIndex){
          sendnbIndexList[sendnbCount] = i;
          ++sendnbCount;
          if (sendnbCount >= sendnbMax){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
              "sendnbCount out of range", ESMC_CONTEXT, &rc);
            return rc;
          }
        }
        break;
      case recvnbRRA:
        if (i>recvnbLowerIndex){
          recvnbIndexList[recvnbCount] = i;
          ++recvnbCount;
          if (recvnbCount >= recvnbMax){
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
              "recvnbCount out of range", ESMC_CONTEXT, &rc);
            return rc;
          }
        }
        break;
        // --- cases below this line cannot be used in execution -> must replace
      case waitOnAllSendnb:
        sendnbLowerIndex = i; // all sendnb prior this index are considered
        {
#if 0
          printf("case: waitOnAllSendnb: %d outstanding sendnb\n",
            sendnbCount);
#endif
          // replace opstream
          int oldCount = count;               // hold on to old count
          StreamElement *oldStream = opstream;  // hold on to old opstream
          opstream = new StreamElement[max];    // prepare new opstream
          // fill in StreamElements from before this StreamElement
          memcpy(opstream, oldStream, i*sizeof(StreamElement));
          // insert explicit waitOnIndex StreamElements
          count = i;
          for (int j=0; j<sendnbCount; j++){
            int index = sendnbIndexList[j];
            opstream[count].opId = waitOnIndex;
            opstream[count].predicateBitField =
              opstream[index].predicateBitField;
            xxeElement = &(opstream[count]);
            xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
            xxeWaitOnIndexInfo->index = index;
            ++count;
            if (count >= max){
              localrc = growStream(1000);
              if (ESMC_LogDefault.MsgFoundError(localrc,
                ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
            }
          }
          // fill in StreamElements from after this StreamElement
          // (excluding this StreamElement)
          if (sendnbCount+oldCount-1 >= max){
            localrc = growStream(sendnbCount+oldCount-max);
            if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          }
          memcpy(opstream+count, oldStream+i+1, (oldCount-i-1)
            * sizeof(StreamElement));
          count = sendnbCount + oldCount -1;  // account for modification
          delete [] oldStream;                // delete original opstream
        }
        breakFlag = 1;  // set
        break;
      case waitOnAllRecvnb:
        recvnbLowerIndex = i; // all recvnb prior this index are considered
        {
#if 0
          printf("case: waitOnAllRecvnb: %d outstanding recvnb\n",
            recvnbCount);
#endif
          // replace opstream
          int oldCount = count;               // hold on to old count
          StreamElement *oldStream = opstream;  // hold on to old opstream
          opstream = new StreamElement[max];    // prepare new opstream
          // fill in StreamElements from before this StreamElement
          memcpy(opstream, oldStream, i*sizeof(StreamElement));
          // insert explicit waitOnIndex StreamElements
          count = i;
          for (int j=0; j<recvnbCount; j++){
            int index = recvnbIndexList[j];
            opstream[count].opId = waitOnIndex;
            opstream[count].predicateBitField =
              opstream[index].predicateBitField;
            xxeElement = &(opstream[count]);
            xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
            xxeWaitOnIndexInfo->index = index;
            ++count;
            if (count >= max){
              localrc = growStream(1000);
              if (ESMC_LogDefault.MsgFoundError(localrc,
                ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
            }
          }
          // fill in StreamElements from after this StreamElement
          // (excluding this StreamElement)
          if (recvnbCount+count-1 >= max){
            localrc = growStream(recvnbCount+oldCount-max);
            if (ESMC_LogDefault.MsgFoundError(localrc,
              ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
          }
          memcpy(opstream+count, oldStream+i+1, (oldCount-i-1)
            * sizeof(StreamElement));
          count = recvnbCount + oldCount -1;  // account for modification
          delete [] oldStream;                // delete original opstream
        }
        breakFlag = 1;  // set
        break;
      default:
        break;
      }
      if (breakFlag) break;
    } // for i
  } // while

  // garbage collection
  delete [] sendnbIndexList;
  delete [] recvnbIndexList;

  // translate profiling Wtimer Ids into XXE opstream indices
  int *idList = new int[count];
  int *indexList = new int[count];
  int iCount = 0; // reset
  for (i=0; i<count; i++){
    // set up id-to-index look-up
    if (opstream[i].opId == wtimer){
      xxeElement = &(opstream[i]);
      xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
      idList[iCount] = xxeWtimerInfo->timerId;
      indexList[iCount] = i;
      ++iCount;
    }
  }
  for (i=0; i<count; i++){
    // use id-to-index look-up to translate Wtimer Ids
    if (opstream[i].opId == wtimer){
      xxeElement = &(opstream[i]);
      xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
      xxeWtimerInfo->wtime = 0.;  // reset
      int actualWtimerId = xxeWtimerInfo->actualWtimerId;
      int relativeWtimerId = xxeWtimerInfo->relativeWtimerId;
      XXE *xxe = xxeWtimerInfo->relativeWtimerXXE;
      int resolveCounter = 0; // reset
      int j;
      if (xxe != NULL){
        // look through the referenced XXE
        for (int ii=0; ii<xxe->count; ii++){
          if (xxe->opstream[ii].opId == wtimer){
            xxeElement2 = &(xxe->opstream[ii]);
            xxeWtimerInfo2 = (XXE::WtimerInfo *)xxeElement2;
            if (xxeWtimerInfo2->timerId == relativeWtimerId){
              xxeWtimerInfo->relativeWtime = &(xxeWtimerInfo2->wtime);
              ++resolveCounter;
                break;
            }
          }
        }
      }
      for (j=0; j<iCount; j++){
        if (idList[j] == actualWtimerId){
          xxeWtimerInfo->actualWtimerIndex = indexList[j];
          ++resolveCounter;
        }
        if (xxe == NULL){
          if (idList[j] == relativeWtimerId){
            xxeWtimerInfo->relativeWtime =
              &(((WtimerInfo *)(&(opstream[indexList[j]])))->wtime);
            ++resolveCounter;
          }
        }
        if (resolveCounter==2) break; // resolved both Ids
      }
      if (j==iCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "unable to resolve XXE WTimer Id", ESMC_CONTEXT, &rc);
        return rc;
      }
    }
  }

  // garbage collection
  delete [] idList;
  delete [] indexList;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::optimize()"
//BOPI
// !IROUTINE:  ESMCI::XXE::optimize
//
// !INTERFACE:
int XXE::optimize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

#if 0
  printf("gjt in ESMCI::XXE::optimize(), opstream=%p, %d, %d\n", opstream,
    count, sizeof(StreamElement));
#endif

  StreamElement *xxeElement, *xxeIndexElement;
  SendnbInfo *xxeSendnbInfo;
  RecvnbInfo *xxeRecvnbInfo;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumVectorInfo *xxeProductSumVectorInfo;
  MemCpyInfo *xxeMemCpyInfo;

  class AnalyzeElement{
    public:
      OpId opId;
      int predicateBitField;
      int partnerPet;
      char *bufferStart;
      char *bufferEnd;
      int bufferSize;
      int indexCount;
      int indexList[10000];
      AnalyzeElement *next;
    public:
      AnalyzeElement(int index, OpId opIdArg, int predicateBitFieldArg,
        int petArg, void *buffer, int size){
        opId=opIdArg;
        predicateBitField=predicateBitFieldArg;
        partnerPet=petArg;
        bufferStart=(char *)buffer;
        bufferEnd=bufferStart+size;
        bufferSize=size;
        indexCount=1;
        indexList[0]=index;
        next = NULL;
      }
      ~AnalyzeElement(){
        if (next)
          delete next;
      }
      void print(){
        if (this){
          printf("opId: %d, predicateBitField: %d, partnerPet: %d, "
            "indexCount: %d, buffer: %p - %p\n",
            opId, predicateBitField, partnerPet, indexCount, bufferStart,
            bufferEnd);
          next->print();  // recursive call
        }else
          printf("ae: END\n");
      }
      void add(int index, OpId opIdArg, int predicateBitFieldArg,
        int petArg, void *buffer, int size){
        if (opIdArg!=opId || predicateBitFieldArg!=predicateBitField
          || petArg!=partnerPet)
          if (next)
            next->add(index, opIdArg, predicateBitFieldArg, petArg, buffer,
            size);
          else
            next = new
              AnalyzeElement(index, opIdArg, predicateBitFieldArg, petArg,
              buffer, size);
        else{
          if ((char *)buffer == bufferEnd){
            // in this simple analyzer elements must be in order
            bufferEnd += size;
            bufferSize += size;
            indexList[indexCount]=index;
            ++indexCount;
          }else
            if (next)
              next->add(index, opIdArg, predicateBitFieldArg, petArg, buffer,
              size);
            else
              next = new
                AnalyzeElement(index, opIdArg, predicateBitFieldArg, petArg,
                buffer, size);
        }
      }
      int elementCount(){
        // count the number of AnalyzeElements
        int elementCount = 0;
        AnalyzeElement *ae = this;
        while (ae!=NULL){
          ++elementCount;
          ae = ae->next;
        }
        return elementCount;
      }
      int elementMatchCount(OpId opIdArg){
        // count the number of AnalyzeElements that match opIdArg
        int elementCount = 0;
        AnalyzeElement *ae = this;
        while (ae!=NULL){
          if (ae->opId == opIdArg)
            ++elementCount;
          ae = ae->next;
        }
        return elementCount;
      }
      int elementPetList(OpId opIdArg, int *petList, int *aeCountList){
        // find petList of all the queued AnalyzeElements
        int petListCount = 0;
        AnalyzeElement *ae = this;
        // walk through the queue and find all matching elements
        while (ae!=NULL){
          if (ae->opId == opIdArg){
            // check if this Pet has been entered the petList
            int i;
            for (i=0; i<petListCount; i++)
              if (petList[i] == ae->partnerPet) break;
            if (i==petListCount){
              // did not find Pet in petList yet -> new entry
              petList[petListCount] = ae->partnerPet;
              aeCountList[petListCount] = 1;
              ++petListCount;
            }else{
              ++aeCountList[i]; // increment the aeCount for this Pet
            }
          }
          ae = ae->next;
        }
        return petListCount;
      }
      int aeList(OpId opIdArg, int partnerPetArg, AnalyzeElement **aeList){
        // enter all matching AnalyzeElements into aeList
        int aeListCount = 0;
        AnalyzeElement *ae = this;
        // walk through the queue and find all matching elements
        while (ae!=NULL){
          if (ae->opId == opIdArg && ae->partnerPet == partnerPetArg){
            // enter ae into aeList
            aeList[aeListCount] = ae;
            ++aeListCount;
          }
          ae = ae->next;
        }
        return aeListCount;
      }
  };

  // prime the analyzeQueue
  AnalyzeElement *aq = NULL;

  int i = 0;  // prime index counter
  while(i!=count){
    // repeat going through the entire opstream until no more StreamElements
    // need to be replaced, i.e. the i-loop will finally make it all the way
    // through.

    if (aq != NULL) delete aq;  // delete from previous analysis loop
    aq = NULL;  // indicate empty queue

    int breakFlag = 0;  // reset
    for (i=0; i<count; i++){
      xxeElement = &(opstream[i]);
//    printf("gjt: %d, opId=%d\n", i, opstream[i].opId);
      switch(opstream[i].opId){
      case send:
        break;
      case recv:
        break;
      case sendnb:
        {
          xxeSendnbInfo = (SendnbInfo *)xxeElement;
          if (aq) aq->add(i, xxeSendnbInfo->opId,
            xxeSendnbInfo->predicateBitField, xxeSendnbInfo->dstPet,
            xxeSendnbInfo->buffer, xxeSendnbInfo->size);
          else
            aq = new AnalyzeElement(i, xxeSendnbInfo->opId,
              xxeSendnbInfo->predicateBitField, xxeSendnbInfo->dstPet,
              xxeSendnbInfo->buffer, xxeSendnbInfo->size);
        }
        break;
      case recvnb:
        {
          xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
          if (aq) aq->add(i, xxeRecvnbInfo->opId,
            xxeRecvnbInfo->predicateBitField, xxeRecvnbInfo->srcPet,
            xxeRecvnbInfo->buffer, xxeRecvnbInfo->size);
          else
            aq = new AnalyzeElement(i, xxeRecvnbInfo->opId,
              xxeRecvnbInfo->predicateBitField, xxeRecvnbInfo->srcPet,
              xxeRecvnbInfo->buffer, xxeRecvnbInfo->size);
        }
        break;
      case waitOnAllSendnb:
        {
          // use single buffer for all previous sendnb with identical partnerPet
#if 0
          printf("case: waitOnAllSendnb: elementMatchCount() = %d\n",
            aq->elementMatchCount(sendnb));
#endif
          // can't have more different Pets than there are sendnb entries
          int *petList = new int[aq->elementMatchCount(sendnb)];
          int *aeCountList = new int[aq->elementMatchCount(sendnb)];
          int petListCount = aq->elementPetList(sendnb, petList, aeCountList);
          for (int j=0; j<petListCount; j++){
#if 0
            printf("%d, petList[j]=%d, aeCountList[j]=%d\n", j, petList[j],
              aeCountList[j]);
#endif
            AnalyzeElement **aeList = new AnalyzeElement*[aeCountList[j]];
            aq->aeList(sendnb, petList[j], aeList);
            if (aeCountList[j]==1){
              // the sendnb StreamElements form a _single_ contiguous block
              // -> don't need extra buffer
              char *buffer = aeList[0]->bufferStart;
              int bufferSize = aeList[0]->bufferSize;
              // replace the very first sendnb StreamElement using the buffer
              xxeElement = &(opstream[aeList[0]->indexList[0]]);
              xxeSendnbInfo = (SendnbInfo *)xxeElement;
              xxeSendnbInfo->buffer = buffer;
              xxeSendnbInfo->size = bufferSize;
              // invalidate all other sendnb StreamElements to nop
              for (int k=1; k<aeList[0]->indexCount; k++){
                opstream[aeList[0]->indexList[k]].opId = nop;
                opstream[aeList[0]->indexList[k]].predicateBitField = 0x0;
              }
            }else{
              // need to introduce a contiguous intermediate buffer
              int bufferSize = 0; // reset
              for (int kk=0; kk<aeCountList[j]; kk++)
                bufferSize += aeList[kk]->bufferSize;
      //printf("allocate itermediate buffer of size: %d bytes, filling in"
      //  " for opstream index: %d\n", bufferSize, aeList[0]->indexList[0]);
              char *buffer = new char[bufferSize]; //TODO: leave leak
              // prepare extra opstream element with memCpy StreamElements
              StreamElement *extrastream = new StreamElement[aeCountList[j]];
              int xxeCount = 0;
              int bufferOffset = 0;
              for (int kk=0; kk<aeCountList[j]; kk++){
                // data of StreamElements ref. by aeList[kk] are congtig. memory
                // -> only need a single memCpy for all of them
                extrastream[xxeCount].opId = memCpy;
                extrastream[xxeCount].predicateBitField = 0x0;  //TODO: match!
                xxeElement = &(extrastream[xxeCount]);
                xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
                xxeMemCpyInfo->dstMem = buffer + bufferOffset;
                xxeMemCpyInfo->srcMem = aeList[kk]->bufferStart;
                xxeMemCpyInfo->size = aeList[kk]->bufferSize;
                ++xxeCount;
                bufferOffset += aeList[kk]->bufferSize;
                // invalidate all StreamElements in opstream assoc. w. aeList[kk]
                for (int k=0; k<aeList[kk]->indexCount; k++){
                  opstream[aeList[kk]->indexList[k]].opId = nop;
                  opstream[aeList[kk]->indexList[k]].predicateBitField = 0x0;
                }
              }
              // determine first Sendnb index
              int firstIndex = aeList[0]->indexList[0];
              // replace the first sendnb StreamElement using the interm. buffer
              opstream[firstIndex].opId = sendnb;
              opstream[firstIndex].predicateBitField = 0x0;  //TODO: match!
              xxeElement = &(opstream[firstIndex]);
              xxeSendnbInfo = (SendnbInfo *)xxeElement;
              xxeSendnbInfo->buffer = buffer;
              xxeSendnbInfo->size = bufferSize;
              // slip in the associated memcpy()s _before_ the first Sendnb
              if (xxeCount+count > max){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                  "count out of range", ESMC_CONTEXT, &rc);
                return rc;
              }
              // start a new opstream
              StreamElement *newstream = new StreamElement[max]; // prep. opstream
              // fill in StreamElements from before firstIndex
              // (excluding firstIndex)
              memcpy(newstream, opstream, firstIndex*sizeof(StreamElement));
              // insert extrastream
              memcpy(newstream+firstIndex, extrastream, xxeCount
                * sizeof(StreamElement));
              delete [] extrastream;  // done using
              // fill in StreamElements from after firstIndex
              // (including firstIndex)
              memcpy(newstream+firstIndex+xxeCount, opstream+firstIndex,
                (count-firstIndex)*sizeof(StreamElement));
              // replace opstream
              delete [] opstream; // delete original opstream
              opstream = newstream; // replace by new opstream
              count = count + xxeCount;        // account for modification
              // need to indicate that opstream was modified _before_ current
              // StreamElement
              breakFlag = 1;  // set
            }
            delete [] aeList;
            if (breakFlag) break;
          }
          delete [] aeCountList;
          delete [] petList;
        }
        break;
      case waitOnAllRecvnb:
        {
          // use single buffer for all previous recvnb with identical partnerPet
#if 0
          printf("case: waitOnAllRecvnb: elementMatchCount() = %d\n",
            aq->elementMatchCount(recvnb));
#endif
          // can't have more different Pets than there are recvnb entries
          int *petList = new int[aq->elementMatchCount(recvnb)];
          int *aeCountList = new int[aq->elementMatchCount(recvnb)];
          int petListCount = aq->elementPetList(recvnb, petList, aeCountList);
          for (int j=0; j<petListCount; j++){
#if 0
            printf("%d, petList[j]=%d, aeCountList[j]=%d\n", j, petList[j],
              aeCountList[j]);
#endif
            AnalyzeElement **aeList = new AnalyzeElement*[aeCountList[j]];
            aq->aeList(recvnb, petList[j], aeList);
            if (aeCountList[j]==1){
              // the recvnb StreamElements form a _single_ contiguous block
              // -> don't need extra buffer
              char *buffer = aeList[0]->bufferStart;
              int bufferSize = aeList[0]->bufferSize;
              // replace the very first recvnb StreamElement using the buffer
              xxeElement = &(opstream[aeList[0]->indexList[0]]);
              xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
              xxeRecvnbInfo->buffer = buffer;
              xxeRecvnbInfo->size = bufferSize;
              // invalidate all other recvnb StreamElements to nop
              for (int k=1; k<aeList[0]->indexCount; k++){
                opstream[aeList[0]->indexList[k]].opId = nop;
                opstream[aeList[0]->indexList[k]].predicateBitField = 0x0;
              }
            }else{
              // need to introduce a contiguous intermediate buffer
              int bufferSize = 0; // reset
              for (int kk=0; kk<aeCountList[j]; kk++)
                bufferSize += aeList[kk]->bufferSize;
           //printf("allocate itermediate buffer of size: %d bytes, filling in"
           //  " for opstream index: %d\n", bufferSize, aeList[0]->indexList[0]);
              char *buffer = new char[bufferSize]; //TODO: leave leak
              // prepare extra opstream segment with memCpy StreamElements
              StreamElement *extrastream = new StreamElement[aeCountList[j]];
              int xxeCount = 0;
              int bufferOffset = 0;
              for (int kk=0; kk<aeCountList[j]; kk++){
                // data of StreamElements referenced by aeList[kk] are congtig.
                // -> only need a single memCpy for all of them
                extrastream[xxeCount].opId = memCpy;
                extrastream[xxeCount].predicateBitField = 0x0;  //TODO: match!
                xxeElement = &(extrastream[xxeCount]);
                xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
                xxeMemCpyInfo->dstMem = aeList[kk]->bufferStart;
                xxeMemCpyInfo->srcMem = buffer + bufferOffset;
                xxeMemCpyInfo->size = aeList[kk]->bufferSize;
                ++xxeCount;
                bufferOffset += aeList[kk]->bufferSize;
                // invalidate all StreamElements in opstream asso. with aeList[kk]
                for (int k=0; k<aeList[kk]->indexCount; k++){
                  opstream[aeList[kk]->indexList[k]].opId = nop;
                  opstream[aeList[kk]->indexList[k]].predicateBitField = 0x0;
                }
              }
              // replace the very first recvnb StreamElement using the
              // intermediate buffer
              opstream[aeList[0]->indexList[0]].opId = recvnb;
              opstream[aeList[0]->indexList[0]].predicateBitField = 0x0;//TODO:mat
              xxeElement = &(opstream[aeList[0]->indexList[0]]);
              xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
              xxeRecvnbInfo->buffer = buffer;
              xxeRecvnbInfo->size = bufferSize;
              // slip in the associated memcpy()s _after_ the waitOnAllRecvnb
              if (xxeCount+count > max){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                  "count out of range", ESMC_CONTEXT, &rc);
                return rc;
              }
              // start a new opstream
              StreamElement *newstream = new StreamElement[max];  // prep opstream
              // fill in StreamElements from before this StreamElement
              // (including this StreamElement)
              memcpy(newstream, opstream, (i+1)*sizeof(StreamElement));
              // insert extrastream
              memcpy(newstream+i+1, extrastream, xxeCount
                * sizeof(StreamElement));
              delete [] extrastream;  // done using
              // fill in StreamElements from after this StreamElement
              // (excluding this StreamElement)
              memcpy(newstream+i+1+xxeCount, opstream+i+1, (count-i-1)
                * sizeof(StreamElement));
              // replace opstream
              delete [] opstream; // delete original opstream
              opstream = newstream; // replace by new opstream
              count = count + xxeCount;        // account for modification
            }
            delete [] aeList;
          }
          delete [] aeCountList;
          delete [] petList;
        }
        break;
      default:
        break;
      }
      if (breakFlag) break;
    } // for i
  } // while

  // remove all the nop StreamElements from opstream
  // start a new opstream
  StreamElement *newstream = new StreamElement[max];  // prepare a new opstream
  int xxeCount = 0;
  for (int i=0; i<count; i++){
    xxeElement = &(opstream[i]);
    if (opstream[i].opId != nop){
      memcpy(newstream+xxeCount, opstream+i, sizeof(StreamElement));
      ++xxeCount;
    }
  }
  // replace opstream
  delete [] opstream; // delete original opstream
  opstream = newstream; // replace by new opstream
  count = xxeCount;
  // done replacing the nop StreamElements from opstream

  // garbage collection
  if (aq != NULL) delete aq;  // delete from previous analysis loop

  // return successfully
  //rc = ESMF_SUCCESS       todo: activate once done implementing
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::growStream()"
//BOPI
// !IROUTINE:  ESMCI::XXE::growStream
//
// !INTERFACE:
int XXE::growStream(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int increase){    // in - number of additional elements
//
// !DESCRIPTION:
//  Increase the length of the XXE opstream.
//  CAUTION: This method changes the location (in memory) of the entire opstream!
//    Previously written opstream elements will be moved to a new location.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (increase == 0){
    // nothing to do -> return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  if (increase < 0){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "increase must be positive", ESMC_CONTEXT, &rc);
    return rc;
  }

  int maxNew = max + increase;
  StreamElement *streamNew;
  try{
    streamNew = new StreamElement[maxNew];
  }catch (...){
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    return rc;
  }
  memcpy(streamNew, opstream, count*sizeof(StreamElement)); // copy prev. elements
  delete [] opstream;   // delete previous opstream
  opstream = streamNew; // plug in newly allocated opstream
  max = maxNew;         // adjust max value

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::growDataList()"
//BOPI
// !IROUTINE:  ESMCI::XXE::growDataList
//
// !INTERFACE:
int XXE::growDataList(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int increase){    // in - number of additional elements
//
// !DESCRIPTION:
//  Increase the length of the XXE data.
//  CAUTION: This method changes the location (in memory) of the entire dataList!
//    Previously written dataList elements will be moved to a new location.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (increase == 0){
    // nothing to do -> return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  if (increase < 0){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "increase must be positive", ESMC_CONTEXT, &rc);
    return rc;
  }

  int dataMaxCountNew = dataMaxCount + increase;
  char **dataListNew;
  try{
    dataListNew = new char*[dataMaxCountNew];
  }catch (...){
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    return rc;
  }
  memcpy(dataListNew, dataList, dataCount*sizeof(char *)); //copy prev elements
  delete [] dataList;      // delete previous dataList
  dataList = dataListNew;   // plug in newly allocated dataList
  dataMaxCount = dataMaxCountNew;     // adjust max value

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::growCommhandle()"
//BOPI
// !IROUTINE:  ESMCI::XXE::growCommhandle
//
// !INTERFACE:
int XXE::growCommhandle(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int increase){    // in - number of additional elements
//
// !DESCRIPTION:
//  Increase the length of the XXE commhandle manager.
//  CAUTION: This method changes the location (in memory) of the entire
//           commhandle manager!
//    Previously written commhandle manager elements will be moved to a new
//    location.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (increase == 0){
    // nothing to do -> return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  if (increase < 0){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "increase must be positive", ESMC_CONTEXT, &rc);
    return rc;
  }

  int commhandleMaxCountNew = commhandleMaxCount + increase;
  VMK::commhandle ***commhandleNew;
  try{
    commhandleNew = new VMK::commhandle**[commhandleMaxCountNew];
  }catch (...){
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    return rc;
  }
  memcpy(commhandleNew, commhandle, commhandleCount*sizeof(VMK::commhandle **));
  delete [] commhandle;                         // delete previous commhandle
  commhandle = commhandleNew;           // plug in newly allocated commhandle
  commhandleMaxCount = commhandleMaxCountNew;   // adjust max value

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::growXxeSub()"
//BOPI
// !IROUTINE:  ESMCI::XXE::growXxeSub
//
// !INTERFACE:
int XXE::growXxeSub(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int increase){    // in - number of additional elements
//
// !DESCRIPTION:
//  Increase the length of the XXE Sub manager.
//  CAUTION: This method changes the location (in memory) of the entire
//           XXE Sub manager!
//    Previously written XXE Sub manager elements will be moved to a new
//    location.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  if (increase == 0){
    // nothing to do -> return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }

  if (increase < 0){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "increase must be positive", ESMC_CONTEXT, &rc);
    return rc;
  }

  int xxeSubMaxCountNew = xxeSubMaxCount + increase;
  XXE **xxeSubListNew;
  try{
    xxeSubListNew = new XXE*[xxeSubMaxCountNew];
  }catch (...){
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    return rc;
  }
  memcpy(xxeSubListNew, xxeSubList, xxeSubCount*sizeof(XXE *));
  delete [] xxeSubList;               // delete previous xxeSubList
  xxeSubList = xxeSubListNew;       // plug in newly allocated xxeSubList
  xxeSubMaxCount = xxeSubMaxCountNew;   // adjust max value

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::incCount()"
//BOPI
// !IROUTINE:  ESMCI::XXE::incCount
//
// !INTERFACE:
int XXE::incCount(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//  Increment the count by one.
//  CAUTION: The location (in memory) of the entire opstream may be changed by
//           this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ++count;
  if (count >= max)
    if (ESMC_LogDefault.MsgFoundError(growStream(1000),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::incDataCount()"
//BOPI
// !IROUTINE:  ESMCI::XXE::incDataCount
//
// !INTERFACE:
int XXE::incDataCount(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//  Increment the dataCount by one.
//  CAUTION: The location (in memory) of the entire dataList may be changed by
//           this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ++dataCount;
  if (dataCount >= dataMaxCount)
    if (ESMC_LogDefault.MsgFoundError(growDataList(10000),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::incCommhandleCount()"
//BOPI
// !IROUTINE:  ESMCI::XXE::incCommhandleCount
//
// !INTERFACE:
int XXE::incCommhandleCount(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//  Increment the commhandleCount by one.
//  CAUTION: The location (in memory) of the entire commhandle manager may be
//           changed by this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ++commhandleCount;
  if (commhandleCount >= commhandleMaxCount)
    if (ESMC_LogDefault.MsgFoundError(growCommhandle(1000),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::incXxeSubCount()"
//BOPI
// !IROUTINE:  ESMCI::XXE::incXxeSubCount
//
// !INTERFACE:
int XXE::incXxeSubCount(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//  Increment the xxeSubCount by one.
//  CAUTION: The location (in memory) of the entire SSE Sub manager may be
//           changed by this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ++xxeSubCount;
  if (xxeSubCount >= xxeSubMaxCount)
    if (ESMC_LogDefault.MsgFoundError(growXxeSub(1000),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::storeData()"
//BOPI
// !IROUTINE:  ESMCI::XXE::storeData
//
// !INTERFACE:
int XXE::storeData(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  char *dataArg,
  unsigned long size
  ){
//
// !DESCRIPTION:
//  Append an element at the end of the dataList.
//  CAUTION: The location (in memory) of the entire dataList may be changed by
//           this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  dataMap[dataArg] = size;  // store the size of the allocation

  dataList[dataCount] = dataArg;
  localrc = incDataCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::storeCommhandle()"
//BOPI
// !IROUTINE:  ESMCI::XXE::storeCommhandle
//
// !INTERFACE:
int XXE::storeCommhandle(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  VMK::commhandle **commhandleArg
  ){
//
// !DESCRIPTION:
//  Append an element at the end of the commhandle manager.
//  CAUTION: The location (in memory) of the entire commhandle manager may be
//           changed by this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  commhandle[commhandleCount] = commhandleArg;
  localrc = incCommhandleCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::storeXxeSub()"
//BOPI
// !IROUTINE:  ESMCI::XXE::storeXxeSub
//
// !INTERFACE:
int XXE::storeXxeSub(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  XXE *xxe
  ){
//
// !DESCRIPTION:
//  Append an element at the end of the XXE Sub manager.
//  CAUTION: The location (in memory) of the entire XXE Sub manager may be
//           changed by this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  xxeSubList[xxeSubCount] = xxe;
  localrc = incXxeSubCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::storeBufferInfo()"
//BOPI
// !IROUTINE:  ESMCI::XXE::storeBufferInfo
//
// !INTERFACE:
int XXE::storeBufferInfo(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  char *buffer,
  unsigned long size,
  int vectorLengthMultiplier
  ){
//
// !DESCRIPTION:
//  Append an element at the end of the buffer info vector.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  BufferInfo *bufferInfo = new BufferInfo(buffer, size, vectorLengthMultiplier);

  unsigned capacity = bufferInfoList.capacity();  // old capacity

  bufferInfoList.push_back(bufferInfo);

  if (bufferInfoList.capacity() != capacity){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      "bufferInfoList overflow!!!", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendXxeSub()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendXxeSub
//
// !INTERFACE:
int XXE::appendXxeSub(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  XXE *xxe,
  int rraShift,
  int vectorLengthShift
  ){
//
// !DESCRIPTION:
//  Append an xxeSub at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = xxeSub;
  opstream[count].predicateBitField = predicateBitField;
  XxeSubInfo *xxeSubInfo = (XxeSubInfo *)&(opstream[count]);
  xxeSubInfo->xxe = xxe;
  xxeSubInfo->rraShift = rraShift;
  xxeSubInfo->vectorLengthShift = vectorLengthShift;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendWtimer()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendWtimer
//
// !INTERFACE:
int XXE::appendWtimer(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  char *string,
  int id,
  int actualId,
  int relativeId,
  XXE *relativeXXE
  ){
//
// !DESCRIPTION:
//  Append a wtimer element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = wtimer;
  opstream[count].predicateBitField = predicateBitField;
  WtimerInfo *xxeWtimerInfo = (WtimerInfo *)&(opstream[count]);
  xxeWtimerInfo->timerId = id;
  int stringLen = strlen(string);
  xxeWtimerInfo->timerString = new char[stringLen+1];
  strcpy(xxeWtimerInfo->timerString, string);
  xxeWtimerInfo->actualWtimerId = actualId;
  xxeWtimerInfo->relativeWtimerId = relativeId;
  xxeWtimerInfo->relativeWtimerXXE = relativeXXE;

  // keep track of strings for xxe garbage collection
  localrc = storeData(xxeWtimerInfo->timerString, stringLen+1);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendRecv()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendRecv
//
// !INTERFACE:
int XXE::appendRecv(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *buffer,
  int size,
  int srcPet,
  int tag,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a recv element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = recv;
  opstream[count].predicateBitField = predicateBitField;
  RecvInfo *xxeRecvInfo = (RecvInfo *)&(opstream[count]);
  xxeRecvInfo->buffer = buffer;
  xxeRecvInfo->size = size;
  xxeRecvInfo->srcPet = srcPet;
  xxeRecvInfo->tag = tag;
  xxeRecvInfo->vectorFlag = vectorFlag;
  xxeRecvInfo->indirectionFlag = indirectionFlag;
  xxeRecvInfo->activeFlag = false;
  xxeRecvInfo->cancelledFlag = false;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSend()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSend
//
// !INTERFACE:
int XXE::appendSend(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *buffer,
  int size,
  int dstPet,
  int tag,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a send element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = send;
  opstream[count].predicateBitField = predicateBitField;
  SendInfo *xxeSendInfo = (SendInfo *)&(opstream[count]);
  xxeSendInfo->buffer = buffer;
  xxeSendInfo->size = size;
  xxeSendInfo->dstPet = dstPet;
  xxeSendInfo->tag = tag;
  xxeSendInfo->vectorFlag = vectorFlag;
  xxeSendInfo->indirectionFlag = indirectionFlag;
  xxeSendInfo->activeFlag = false;
  xxeSendInfo->cancelledFlag = false;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSendRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSendRRA
//
// !INTERFACE:
int XXE::appendSendRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int rraOffset,
  int size,
  int dstPet,
  int rraIndex,
  int tag,
  bool vectorFlag
  ){
//
// !DESCRIPTION:
//  Append a sendRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sendRRA;
  opstream[count].predicateBitField = predicateBitField;
  SendRRAInfo *xxeSendRRAInfo = (SendRRAInfo *)&(opstream[count]);
  xxeSendRRAInfo->rraOffset = rraOffset;
  xxeSendRRAInfo->size = size;
  xxeSendRRAInfo->dstPet = dstPet;
  xxeSendRRAInfo->rraIndex = rraIndex;
  xxeSendRRAInfo->tag = tag;
  xxeSendRRAInfo->vectorFlag = vectorFlag;
  xxeSendRRAInfo->activeFlag = false;
  xxeSendRRAInfo->cancelledFlag = false;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSendRecv()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSendRecv
//
// !INTERFACE:
int XXE::appendSendRecv(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *srcBuffer,
  void *dstBuffer,
  int srcSize,
  int dstSize,
  int srcPet,
  int dstPet,
  int srcTag,
  int dstTag,
  bool vectorFlag,
  bool srcIndirectionFlag,
  bool dstIndirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a sendrecv element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sendrecv;
  opstream[count].predicateBitField = predicateBitField;
  SendRecvInfo *xxeSendRecvInfo = (SendRecvInfo *)&(opstream[count]);
  xxeSendRecvInfo->srcBuffer = srcBuffer;
  xxeSendRecvInfo->dstBuffer = dstBuffer;
  xxeSendRecvInfo->srcSize = srcSize;
  xxeSendRecvInfo->dstSize = dstSize;
  xxeSendRecvInfo->srcPet = srcPet;
  xxeSendRecvInfo->dstPet = dstPet;
  xxeSendRecvInfo->srcTag = srcTag;
  xxeSendRecvInfo->dstTag = dstTag;
  xxeSendRecvInfo->vectorFlag = vectorFlag;
  xxeSendRecvInfo->srcIndirectionFlag = srcIndirectionFlag;
  xxeSendRecvInfo->dstIndirectionFlag = dstIndirectionFlag;
  xxeSendRecvInfo->activeFlag = false;
  xxeSendRecvInfo->cancelledFlag = false;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSendRRARecv()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSendRRARecv
//
// !INTERFACE:
int XXE::appendSendRRARecv(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int rraOffset,
  void *dstBuffer,
  int srcSize,
  int dstSize,
  int srcPet,
  int dstPet,
  int rraIndex,
  int srcTag,
  int dstTag,
  bool vectorFlag,
  bool dstIndirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a sendRRArecv element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sendRRArecv;
  opstream[count].predicateBitField = predicateBitField;
  SendRRARecvInfo *xxeSendRRARecvInfo = (SendRRARecvInfo *)&(opstream[count]);
  xxeSendRRARecvInfo->rraOffset = rraOffset;
  xxeSendRRARecvInfo->dstBuffer = dstBuffer;
  xxeSendRRARecvInfo->srcSize = srcSize;
  xxeSendRRARecvInfo->dstSize = dstSize;
  xxeSendRRARecvInfo->srcPet = srcPet;
  xxeSendRRARecvInfo->dstPet = dstPet;
  xxeSendRRARecvInfo->rraIndex = rraIndex;
  xxeSendRRARecvInfo->srcTag = srcTag;
  xxeSendRRARecvInfo->dstTag = dstTag;
  xxeSendRRARecvInfo->vectorFlag = vectorFlag;
  xxeSendRRARecvInfo->dstIndirectionFlag = dstIndirectionFlag;
  xxeSendRRARecvInfo->activeFlag = false;
  xxeSendRRARecvInfo->cancelledFlag = false;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendRecvnb()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendRecvnb
//
// !INTERFACE:
int XXE::appendRecvnb(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *buffer,
  int size,
  int srcPet,
  int tag,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a recvnb element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = recvnb;
  opstream[count].predicateBitField = predicateBitField;
  RecvnbInfo *xxeRecvnbInfo = (RecvnbInfo *)&(opstream[count]);
  xxeRecvnbInfo->buffer = buffer;
  xxeRecvnbInfo->size = size;
  xxeRecvnbInfo->srcPet = srcPet;
  xxeRecvnbInfo->tag = tag;
  xxeRecvnbInfo->vectorFlag = vectorFlag;
  xxeRecvnbInfo->indirectionFlag = indirectionFlag;
  xxeRecvnbInfo->activeFlag = false;
  xxeRecvnbInfo->cancelledFlag = false;
  xxeRecvnbInfo->commhandle = new VMK::commhandle*;
  *(xxeRecvnbInfo->commhandle) = new VMK::commhandle;

  // keep track of commhandles for xxe garbage collection
  localrc = storeCommhandle(xxeRecvnbInfo->commhandle);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSendnb()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSendnb
//
// !INTERFACE:
int XXE::appendSendnb(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *buffer,
  int size,
  int dstPet,
  int tag,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a sendnb element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sendnb;
  opstream[count].predicateBitField = predicateBitField;
  SendnbInfo *xxeSendnbInfo = (SendnbInfo *)&(opstream[count]);
  xxeSendnbInfo->buffer = buffer;
  xxeSendnbInfo->size = size;
  xxeSendnbInfo->dstPet = dstPet;
  xxeSendnbInfo->tag = tag;
  xxeSendnbInfo->vectorFlag = vectorFlag;
  xxeSendnbInfo->indirectionFlag = indirectionFlag;
  xxeSendnbInfo->activeFlag = false;
  xxeSendnbInfo->cancelledFlag = false;
  xxeSendnbInfo->commhandle = new VMK::commhandle*;
  *(xxeSendnbInfo->commhandle) = new VMK::commhandle;

  // keep track of commhandles for xxe garbage collection
  localrc = storeCommhandle(xxeSendnbInfo->commhandle);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSendnbRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSendnbRRA
//
// !INTERFACE:
int XXE::appendSendnbRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int rraOffset,
  int size,
  int dstPet,
  int rraIndex,
  int tag,
  bool vectorFlag
  ){
//
// !DESCRIPTION:
//  Append a sendnbRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sendnbRRA;
  opstream[count].predicateBitField = predicateBitField;
  SendnbRRAInfo *xxeSendnbRRAInfo = (SendnbRRAInfo *)&(opstream[count]);
  xxeSendnbRRAInfo->rraOffset = rraOffset;
  xxeSendnbRRAInfo->size = size;
  xxeSendnbRRAInfo->dstPet = dstPet;
  xxeSendnbRRAInfo->rraIndex = rraIndex;
  xxeSendnbRRAInfo->tag = tag;
  xxeSendnbRRAInfo->vectorFlag = vectorFlag;
  xxeSendnbRRAInfo->activeFlag = false;
  xxeSendnbRRAInfo->cancelledFlag = false;
  xxeSendnbRRAInfo->commhandle = new VMK::commhandle*;
  *(xxeSendnbRRAInfo->commhandle) = new VMK::commhandle;

  // keep track of commhandles for xxe garbage collection
  localrc = storeCommhandle(xxeSendnbRRAInfo->commhandle);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendMemCpySrcRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendMemCpySrcRRA
//
// !INTERFACE:
int XXE::appendMemCpySrcRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int rraOffset,
  int size,
  void *dstMem,
  int rraIndex
  ){
//
// !DESCRIPTION:
//  Append a memCpySrcRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = memCpySrcRRA;
  opstream[count].predicateBitField = predicateBitField;
  MemCpySrcRRAInfo *xxeMemCpySrcRRAInfo = (MemCpySrcRRAInfo *)&(opstream[count]);
  xxeMemCpySrcRRAInfo->rraOffset = rraOffset;
  xxeMemCpySrcRRAInfo->size = size;
  xxeMemCpySrcRRAInfo->dstMem = dstMem;
  xxeMemCpySrcRRAInfo->rraIndex = rraIndex;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendMemGatherSrcRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendMemGatherSrcRRA
//
// !INTERFACE:
int XXE::appendMemGatherSrcRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *dstBase,
  TKId dstBaseTK,
  int rraIndex,
  int chunkCount,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a memGatherSrcRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = memGatherSrcRRA;
  opstream[count].predicateBitField = predicateBitField;
  MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo =
    (MemGatherSrcRRAInfo *)&(opstream[count]);
  xxeMemGatherSrcRRAInfo->dstBase = dstBase;
  xxeMemGatherSrcRRAInfo->dstBaseTK = dstBaseTK;
  xxeMemGatherSrcRRAInfo->rraIndex = rraIndex;
  xxeMemGatherSrcRRAInfo->chunkCount = chunkCount;
  xxeMemGatherSrcRRAInfo->vectorFlag = vectorFlag;
  xxeMemGatherSrcRRAInfo->indirectionFlag = indirectionFlag;
  char *rraOffsetListChar = new char[chunkCount*sizeof(int)];
  xxeMemGatherSrcRRAInfo->rraOffsetList = (int *)rraOffsetListChar;
  char *countListChar = new char[chunkCount*sizeof(int)];
  xxeMemGatherSrcRRAInfo->countList = (int *)countListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraOffsetListChar, chunkCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(countListChar, chunkCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendZeroScalarRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendZeroScalarRRA
//
// !INTERFACE:
int XXE::appendZeroScalarRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  int rraOffset,
  int rraIndex
  ){
//
// !DESCRIPTION:
//  Append a zeroScalarRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = zeroScalarRRA;
  opstream[count].predicateBitField = predicateBitField;
  ZeroScalarRRAInfo *xxeZeroScalarRRAInfo =
    (ZeroScalarRRAInfo *)&(opstream[count]);
  xxeZeroScalarRRAInfo->elementTK = elementTK;
  xxeZeroScalarRRAInfo->rraOffset = rraOffset;
  xxeZeroScalarRRAInfo->rraIndex = rraIndex;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendZeroSuperScalarRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendZeroSuperScalarRRA
//
// !INTERFACE:
int XXE::appendZeroSuperScalarRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  int rraIndex,
  int termCount,
  bool vectorFlag
  ){
//
// !DESCRIPTION:
//  Append a zeroSuperScalarRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = zeroSuperScalarRRA;
  opstream[count].predicateBitField = predicateBitField;
  ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo =
    (ZeroSuperScalarRRAInfo *)&(opstream[count]);
  xxeZeroSuperScalarRRAInfo->elementTK = elementTK;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeZeroSuperScalarRRAInfo->rraOffsetList = (int *)rraOffsetListChar;
  xxeZeroSuperScalarRRAInfo->rraIndex = rraIndex;
  xxeZeroSuperScalarRRAInfo->termCount = termCount;
  xxeZeroSuperScalarRRAInfo->vectorFlag = vectorFlag;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendZeroMemset()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendZeroMemset
//
// !INTERFACE:
int XXE::appendZeroMemset(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  void *buffer,
  int byteCount,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a zeroMemset element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = zeroMemset;
  opstream[count].predicateBitField = predicateBitField;
  ZeroMemsetInfo *xxeZeroMemsetInfo =
    (ZeroMemsetInfo *)&(opstream[count]);
  xxeZeroMemsetInfo->buffer = buffer;
  xxeZeroMemsetInfo->byteCount = byteCount;
  xxeZeroMemsetInfo->vectorFlag = vectorFlag;
  xxeZeroMemsetInfo->indirectionFlag = indirectionFlag;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendZeroMemsetRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendZeroMemsetRRA
//
// !INTERFACE:
int XXE::appendZeroMemsetRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int byteCount,
  int rraIndex,
  bool vectorFlag
  ){
//
// !DESCRIPTION:
//  Append a zeroMemsetRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = zeroMemsetRRA;
  opstream[count].predicateBitField = predicateBitField;
  ZeroMemsetRRAInfo *xxeZeroMemsetRRAInfo =
    (ZeroMemsetRRAInfo *)&(opstream[count]);
  xxeZeroMemsetRRAInfo->byteCount = byteCount;
  xxeZeroMemsetRRAInfo->rraIndex = rraIndex;
  xxeZeroMemsetRRAInfo->vectorFlag = vectorFlag;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendProductSumScalarRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendProductSumScalarRRA
//
// !INTERFACE:
int XXE::appendProductSumScalarRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  TKId valueTK,
  TKId factorTK,
  int rraOffset,
  void *factor,
  void *value,
  int rraIndex
  ){
//
// !DESCRIPTION:
//  Append a productSumScalarRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = productSumScalarRRA;
  opstream[count].predicateBitField = predicateBitField;
  ProductSumScalarRRAInfo *xxeProductSumScalarRRAInfo =
    (ProductSumScalarRRAInfo *)&(opstream[count]);
  xxeProductSumScalarRRAInfo->elementTK = elementTK;
  xxeProductSumScalarRRAInfo->valueTK = valueTK;
  xxeProductSumScalarRRAInfo->factorTK = factorTK;
  xxeProductSumScalarRRAInfo->rraOffset = rraOffset;
  xxeProductSumScalarRRAInfo->factor = factor;
  xxeProductSumScalarRRAInfo->value = value;
  xxeProductSumScalarRRAInfo->rraIndex = rraIndex;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSumSuperScalarDstRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSumSuperScalarDstRRA
//
// !INTERFACE:
int XXE::appendSumSuperScalarDstRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  TKId valueTK,
  int rraIndex,
  int termCount,
  void *valueBase,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a SumSuperScalarDstRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sumSuperScalarDstRRA;
  opstream[count].predicateBitField = predicateBitField;
  SumSuperScalarDstRRAInfo *xxeSumSuperScalarDstRRAInfo =
    (SumSuperScalarDstRRAInfo *)&(opstream[count]);
  xxeSumSuperScalarDstRRAInfo->elementTK = elementTK;
  xxeSumSuperScalarDstRRAInfo->valueTK = valueTK;
  xxeSumSuperScalarDstRRAInfo->rraIndex = rraIndex;
  xxeSumSuperScalarDstRRAInfo->termCount = termCount;
  xxeSumSuperScalarDstRRAInfo->valueBase = valueBase;
  xxeSumSuperScalarDstRRAInfo->vectorFlag = vectorFlag;
  xxeSumSuperScalarDstRRAInfo->indirectionFlag = indirectionFlag;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeSumSuperScalarDstRRAInfo->rraOffsetList = (int *)rraOffsetListChar;
  char *valueOffsetListChar = new char[termCount*sizeof(int)];
  xxeSumSuperScalarDstRRAInfo->valueOffsetList = (int *)valueOffsetListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendSumSuperScalarListDstRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendSumSuperScalarListDstRRA
//
// !INTERFACE:
int XXE::appendSumSuperScalarListDstRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  TKId valueTK,
  std::vector<int> rraIndexList,
  int termCount,
  vector<void *>valueBaseList,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a sumSuperScalarListDstRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = sumSuperScalarListDstRRA;
  opstream[count].predicateBitField = predicateBitField;
  SumSuperScalarListDstRRAInfo *xxeSumSuperScalarListDstRRAInfo =
    (SumSuperScalarListDstRRAInfo *)&(opstream[count]);
  xxeSumSuperScalarListDstRRAInfo->elementTK = elementTK;
  xxeSumSuperScalarListDstRRAInfo->valueTK = valueTK;
  char *rraIndexListChar = new char[rraIndexList.size()*sizeof(int)];
  xxeSumSuperScalarListDstRRAInfo->rraIndexList =
    (int *)rraIndexListChar;
  for (unsigned i=0; i<rraIndexList.size(); i++){
    xxeSumSuperScalarListDstRRAInfo->rraIndexList[i] =
      rraIndexList[i];
  }
  xxeSumSuperScalarListDstRRAInfo->termCount = termCount;
  char *valueBaseListChar = new char[valueBaseList.size()*sizeof(void *)];
  xxeSumSuperScalarListDstRRAInfo->valueBaseList =
    (void **)valueBaseListChar;
  for (unsigned i=0; i<valueBaseList.size(); i++){
    xxeSumSuperScalarListDstRRAInfo->valueBaseList[i] =
      valueBaseList[i];
  }
  char *valueBaseListResolveChar =
    new char[valueBaseList.size()*sizeof(void *)];
  xxeSumSuperScalarListDstRRAInfo->valueBaseListResolve =
    (void **)valueBaseListResolveChar;
  xxeSumSuperScalarListDstRRAInfo->valueBaseListSize =
    valueBaseList.size();
  xxeSumSuperScalarListDstRRAInfo->vectorFlag = vectorFlag;
  xxeSumSuperScalarListDstRRAInfo->indirectionFlag = indirectionFlag;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeSumSuperScalarListDstRRAInfo->rraOffsetList =
    (int *)rraOffsetListChar;
  char *valueOffsetListChar = new char[termCount*sizeof(int)];
  xxeSumSuperScalarListDstRRAInfo->valueOffsetList =
    (int *)valueOffsetListChar;
  char *baseListIndexListChar = new char[termCount*sizeof(int)];
  xxeSumSuperScalarListDstRRAInfo->baseListIndexList =
    (int *)baseListIndexListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraIndexListChar, rraIndexList.size()*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueBaseListChar, valueBaseList.size()*sizeof(void *));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueBaseListResolveChar,
    valueBaseList.size()*sizeof(void *));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(rraOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(baseListIndexListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendProductSumSuperScalarDstRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendProductSumSuperScalarDstRRA
//
// !INTERFACE:
int XXE::appendProductSumSuperScalarDstRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  TKId valueTK,
  TKId factorTK,
  int rraIndex,
  int termCount,
  void *valueBase,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a productSumSuperScalarDstRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = productSumSuperScalarDstRRA;
  opstream[count].predicateBitField = predicateBitField;
  ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo =
    (ProductSumSuperScalarDstRRAInfo *)&(opstream[count]);
  xxeProductSumSuperScalarDstRRAInfo->elementTK = elementTK;
  xxeProductSumSuperScalarDstRRAInfo->valueTK = valueTK;
  xxeProductSumSuperScalarDstRRAInfo->factorTK = factorTK;
  xxeProductSumSuperScalarDstRRAInfo->rraIndex = rraIndex;
  xxeProductSumSuperScalarDstRRAInfo->termCount = termCount;
  xxeProductSumSuperScalarDstRRAInfo->valueBase = valueBase;
  xxeProductSumSuperScalarDstRRAInfo->vectorFlag = vectorFlag;
  xxeProductSumSuperScalarDstRRAInfo->indirectionFlag = indirectionFlag;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarDstRRAInfo->rraOffsetList = (int *)rraOffsetListChar;
  unsigned factorTKSize;
  if (factorTK==I4)
    factorTKSize = sizeof(ESMC_I4);
  else if (factorTK==I8)
    factorTKSize = sizeof(ESMC_I8);
  else if (factorTK==R4)
    factorTKSize = sizeof(ESMC_R4);
  else if (factorTK==R8)
    factorTKSize = sizeof(ESMC_R8);
  char *factorListChar = new char[termCount*factorTKSize];
  xxeProductSumSuperScalarDstRRAInfo->factorList = (void *)factorListChar;
  char *valueOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarDstRRAInfo->valueOffsetList =
    (int *)valueOffsetListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(factorListChar, termCount*factorTKSize);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendProductSumSuperScalarListDstRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendProductSumSuperScalarListDstRRA
//
// !INTERFACE:
int XXE::appendProductSumSuperScalarListDstRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  TKId valueTK,
  TKId factorTK,
  std::vector<int> rraIndexList,
  int termCount,
  vector<void *>valueBaseList,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a productSumSuperScalarListDstRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = productSumSuperScalarListDstRRA;
  opstream[count].predicateBitField = predicateBitField;
  ProductSumSuperScalarListDstRRAInfo *xxeProductSumSuperScalarListDstRRAInfo =
    (ProductSumSuperScalarListDstRRAInfo *)&(opstream[count]);
  xxeProductSumSuperScalarListDstRRAInfo->elementTK = elementTK;
  xxeProductSumSuperScalarListDstRRAInfo->valueTK = valueTK;
  xxeProductSumSuperScalarListDstRRAInfo->factorTK = factorTK;
  char *rraIndexListChar = new char[rraIndexList.size()*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->rraIndexList =
    (int *)rraIndexListChar;
  for (unsigned i=0; i<rraIndexList.size(); i++){
    xxeProductSumSuperScalarListDstRRAInfo->rraIndexList[i] =
      rraIndexList[i];
  }
  xxeProductSumSuperScalarListDstRRAInfo->termCount = termCount;
  char *valueBaseListChar = new char[valueBaseList.size()*sizeof(void *)];
  xxeProductSumSuperScalarListDstRRAInfo->valueBaseList =
    (void **)valueBaseListChar;
  for (unsigned i=0; i<valueBaseList.size(); i++){
    xxeProductSumSuperScalarListDstRRAInfo->valueBaseList[i] =
      valueBaseList[i];
  }
  char *valueBaseListResolveChar =
    new char[valueBaseList.size()*sizeof(void *)];
  xxeProductSumSuperScalarListDstRRAInfo->valueBaseListResolve =
    (void **)valueBaseListResolveChar;
  xxeProductSumSuperScalarListDstRRAInfo->valueBaseListSize =
    valueBaseList.size();
  xxeProductSumSuperScalarListDstRRAInfo->vectorFlag = vectorFlag;
  xxeProductSumSuperScalarListDstRRAInfo->indirectionFlag = indirectionFlag;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->rraOffsetList =
    (int *)rraOffsetListChar;
  unsigned factorTKSize;
  if (factorTK==I4)
    factorTKSize = sizeof(ESMC_I4);
  else if (factorTK==I8)
    factorTKSize = sizeof(ESMC_I8);
  else if (factorTK==R4)
    factorTKSize = sizeof(ESMC_R4);
  else if (factorTK==R8)
    factorTKSize = sizeof(ESMC_R8);
  char *factorListChar = new char[termCount*factorTKSize];
  xxeProductSumSuperScalarListDstRRAInfo->factorList = (void *)factorListChar;
  char *valueOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->valueOffsetList =
    (int *)valueOffsetListChar;
  char *baseListIndexListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->baseListIndexList =
    (int *)baseListIndexListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraIndexListChar, rraIndexList.size()*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueBaseListChar, valueBaseList.size()*sizeof(void *));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueBaseListResolveChar,
    valueBaseList.size()*sizeof(void *));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(rraOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(factorListChar, termCount*factorTKSize);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(valueOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(baseListIndexListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendProductSumSuperScalarSrcRRA()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendProductSumSuperScalarSrcRRA
//
// !INTERFACE:
int XXE::appendProductSumSuperScalarSrcRRA(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  TKId elementTK,
  TKId valueTK,
  TKId factorTK,
  int rraIndex,
  int termCount,
  void *elementBase,
  bool vectorFlag,
  bool indirectionFlag
  ){
//
// !DESCRIPTION:
//  Append a productSumSuperScalarSrcRRA element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = productSumSuperScalarSrcRRA;
  opstream[count].predicateBitField = predicateBitField;
  ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo =
    (ProductSumSuperScalarSrcRRAInfo *)&(opstream[count]);
  xxeProductSumSuperScalarSrcRRAInfo->elementTK = elementTK;
  xxeProductSumSuperScalarSrcRRAInfo->valueTK = valueTK;
  xxeProductSumSuperScalarSrcRRAInfo->factorTK = factorTK;
  xxeProductSumSuperScalarSrcRRAInfo->rraIndex = rraIndex;
  xxeProductSumSuperScalarSrcRRAInfo->termCount = termCount;
  xxeProductSumSuperScalarSrcRRAInfo->elementBase = elementBase;
  xxeProductSumSuperScalarSrcRRAInfo->vectorFlag = vectorFlag;
  xxeProductSumSuperScalarSrcRRAInfo->indirectionFlag = indirectionFlag;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarSrcRRAInfo->rraOffsetList = (int *)rraOffsetListChar;
  char *factorListChar = new char[termCount*sizeof(void *)];
  xxeProductSumSuperScalarSrcRRAInfo->factorList = (void **)factorListChar;
  char *elementOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarSrcRRAInfo->elementOffsetList =
    (int *)elementOffsetListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(rraOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(factorListChar, termCount*sizeof(void *));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(elementOffsetListChar, termCount*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendWaitOnIndex()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendWaitOnIndex
//
// !INTERFACE:
int XXE::appendWaitOnIndex(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int index
  ){
//
// !DESCRIPTION:
//  Append a waitOnIndex element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = waitOnIndex;
  opstream[count].predicateBitField = predicateBitField;
  WaitOnIndexInfo *xxeWaitOnIndexInfo =
    (WaitOnIndexInfo *)&(opstream[count]);
  xxeWaitOnIndexInfo->index = index;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendTestOnIndex()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendTestOnIndex
//
// !INTERFACE:
int XXE::appendTestOnIndex(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int index
  ){
//
// !DESCRIPTION:
//  Append a testOnIndex element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = testOnIndex;
  opstream[count].predicateBitField = predicateBitField;
  TestOnIndexInfo *xxeTestOnIndexInfo =
    (TestOnIndexInfo *)&(opstream[count]);
  xxeTestOnIndexInfo->index = index;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendWaitOnAnyIndexSub()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendWaitOnAnyIndexSub
//
// !INTERFACE:
int XXE::appendWaitOnAnyIndexSub(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int countArg
  ){
//
// !DESCRIPTION:
//  Append a waitOnAnyIndexSub element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = waitOnAnyIndexSub;
  opstream[count].predicateBitField = predicateBitField;
  WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo =
    (WaitOnAnyIndexSubInfo *)&(opstream[count]);
  xxeWaitOnAnyIndexSubInfo->count = countArg;
  char *xxeChar = new char[countArg*sizeof(XXE *)];
  xxeWaitOnAnyIndexSubInfo->xxe = (XXE **)xxeChar;
  char *indexChar = new char[countArg*sizeof(int)];
  xxeWaitOnAnyIndexSubInfo->index = (int *)indexChar;
  char *completeFlagChar = new char[countArg*sizeof(int)];
  xxeWaitOnAnyIndexSubInfo->completeFlag = (int *)completeFlagChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeData(xxeChar, countArg*sizeof(XXE *));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(indexChar, countArg*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeData(completeFlagChar, countArg*sizeof(int));
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendWaitOnAllSendnb()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendWaitOnAllSendnb
//
// !INTERFACE:
int XXE::appendWaitOnAllSendnb(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField
  ){
//
// !DESCRIPTION:
//  Append a waitOnAllSendnb element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = waitOnAllSendnb;
  opstream[count].predicateBitField = predicateBitField;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendWaitOnIndexSub()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendWaitOnIndexSub
//
// !INTERFACE:
int XXE::appendWaitOnIndexSub(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  XXE *xxe,
  int rraShift,
  int vectorLengthShift,
  int index
  ){
//
// !DESCRIPTION:
//  Append an xxeSub at the end of the XXE opstream that is executed depending
//  on test conditional
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = waitOnIndexSub;
  opstream[count].predicateBitField = predicateBitField;
  WaitOnIndexSubInfo *waitOnIndexSubInfo =
    (WaitOnIndexSubInfo *)&(opstream[count]);
  waitOnIndexSubInfo->xxe = xxe;
  waitOnIndexSubInfo->rraShift = rraShift;
  waitOnIndexSubInfo->vectorLengthShift = vectorLengthShift;
  waitOnIndexSubInfo->index = index;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendTestOnIndexSub()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendTestOnIndexSub
//
// !INTERFACE:
int XXE::appendTestOnIndexSub(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  XXE *xxe,
  int rraShift,
  int vectorLengthShift,
  int index
  ){
//
// !DESCRIPTION:
//  Append an xxeSub at the end of the XXE opstream that is executed depending
//  on test conditional
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = testOnIndexSub;
  opstream[count].predicateBitField = predicateBitField;
  TestOnIndexSubInfo *testOnIndexSubInfo =
    (TestOnIndexSubInfo *)&(opstream[count]);
  testOnIndexSubInfo->xxe = xxe;
  testOnIndexSubInfo->rraShift = rraShift;
  testOnIndexSubInfo->vectorLengthShift = vectorLengthShift;
  testOnIndexSubInfo->index = index;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendCancelIndex()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendCancelIndex
//
// !INTERFACE:
int XXE::appendCancelIndex(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  int index
  ){
//
// !DESCRIPTION:
//  Append a cancelIndex element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = cancelIndex;
  opstream[count].predicateBitField = predicateBitField;
  CancelIndexInfo *xxeCancelIndexInfo =
    (CancelIndexInfo *)&(opstream[count]);
  xxeCancelIndexInfo->index = index;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendProfileMessage()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendProfileMessage
//
// !INTERFACE:
int XXE::appendProfileMessage(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  char *messageString
  ){
//
// !DESCRIPTION:
//  Append a profileMessage element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = profileMessage;
  opstream[count].predicateBitField = predicateBitField;
  ProfileMessageInfo *xxeProfileMessageInfo =
    (ProfileMessageInfo *)&(opstream[count]);
  int stringLen = strlen(messageString);
  xxeProfileMessageInfo->messageString = new char[stringLen+1];
  strcpy(xxeProfileMessageInfo->messageString, messageString);

  // keep track of strings for xxe garbage collection
  localrc = storeData(xxeProfileMessageInfo->messageString, stringLen+1);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::appendMessage()"
//BOPI
// !IROUTINE:  ESMCI::XXE::appendMessage
//
// !INTERFACE:
int XXE::appendMessage(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int predicateBitField,
  char *messageString
  ){
//
// !DESCRIPTION:
//  Append a Message element at the end of the XXE opstream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  opstream[count].opId = message;
  opstream[count].predicateBitField = predicateBitField;
  MessageInfo *xxeMessageInfo =
    (MessageInfo *)&(opstream[count]);
  int stringLen = strlen(messageString);
  xxeMessageInfo->messageString = new char[stringLen+1];
  strcpy(xxeMessageInfo->messageString, messageString);

  // keep track of strings for xxe garbage collection
  localrc = storeData(xxeMessageInfo->messageString, stringLen+1);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // bump up element count, this may move entire opstream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
