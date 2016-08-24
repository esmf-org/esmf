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
#define ESMC_FILENAME "ESMCI_DELayout.C"
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

// include ESMF headers
#include "ESMCI_Macros.h"
#include "ESMCI_VM.h"
#include "ESMCI_F90Interface.h"
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
  int *deCountArg,          // (in) number of DEs
  InterfaceInt *deGrouping, // (in) deGrouping vector
  ESMC_Pin_Flag *pinFlag,   // (in) type of resources DEs are pinned to
  InterfaceInt *petListArg, // (in) list of PETs to be used in delayout
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
        "- Size of deGrouping does not match deCount", ESMC_CONTEXT, rc);
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
    petList = petListArg->array;
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
      "- Not a valid pointer to DELayout", ESMC_CONTEXT, &rc);
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
      "- Caught exception", ESMC_CONTEXT, &rc);
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
        "- DE to PET mapping is invalid", ESMC_CONTEXT, &rc);
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
      "- deMatchList must be of size 'deMatchCount'", ESMC_CONTEXT, &rc);
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
      "- petMatchList must be of size 'petMatchCount'", ESMC_CONTEXT, &rc);
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
        "- only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
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
        "- only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
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
        "- only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
      return rc;
    }else
      *logRectFlag = this->logRectFlag;
  }
  
  if (len_deCountPerDim >= this->ndim){
    if (!oldstyle){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
        "- only OLDSTYLE DELayouts support this query", ESMC_CONTEXT, &rc);
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
    if ((*length - *offset) < sizeof(DELayout)){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD, 
      "- Buffer too short to add a DELayout object", ESMC_CONTEXT, &rc);
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
    if (*offset < sizeof (DELayout))
      *offset = sizeof (DELayout);
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
//        "- Specified DE is not in localDeToDeMap", ESMC_CONTEXT, rc);
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
//      "- Specified DE is not in vasLocalDeToDeMap", ESMC_CONTEXT, rc);
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
//        "- Specified DE is not in localDeToDeMap", ESMC_CONTEXT, &rc);
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
//      "- Specified DE is not in vasLocalDeToDeMap", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  // ...de was found in local list
  if (!serviceMutexFlag[ii]){
//TODO: enable LogErr once it is thread-safe
rc=ESMC_RC_NOT_VALID;
//    ESMC_LogDefault.MsgFoundError(ESMC_RC_NOT_VALID,
//      "- PET does not hold service mutex for specified", ESMC_CONTEXT, &rc);
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
      "- Can only handle 1-to-1 DELayouts", ESMC_CONTEXT, &rc);
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
  int indexStop       // in  - stop index, < 0 for default (full stream)
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
  
#if 0
  printf("gjt in ESMCI::XXE::exec(), stream=%p, %d, %d\n", stream, count, 
    sizeof(StreamElement));
#endif
  
  // set index range
  int indexRangeStart = 0;        // default
  if (indexStart > 0) indexRangeStart = indexStart;
  int indexRangeStop = count-1;   // default
  if (indexStop > 0) indexRangeStop = indexStop;
  
  // check index range
  if (count > 0 && indexRangeStart > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- indexStart out of range", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (indexRangeStop > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- indexStop out of range", ESMC_CONTEXT, &rc);
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
  
  for (int i=0; i<bufferInfoList.size(); i++){
    int currentSize = bufferInfoList[i]->vectorLengthMultiplier * *vectorLength;
    if (bufferInfoList[i]->size < currentSize){
      // allocate a new, larger buffer to accommodate currentSize
      char *buffer = new char[currentSize];
      localrc = storeStorage(buffer); // XXE garbage collec.
      if (ESMC_LogDefault.MsgFoundError(localrc,
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
      //TODO: It may make sense here to do a linear search for the old buffer
      //TODO: entry in "storage", to deallocate the buffer that is now found
      //TODO: too small, and to replace the entry with the newly allocated,
      //TODO: larger buffer. Not doing this means that the old, too small buffer
      //TODO: remains allocated until the XXE object is destroyed, so there is
      //TODO: a certain amount of memory "wasted". However, for now I decided
      //TODO: not to implement the above suggested deallocation and replacement
      //TODO: of buffer in "storage", because I am worried about the potential
      //TODO: performance hit of such a linear search, especially for large
      //TODO: storage cases, right during the very performance critical exec()
      //TODO: phase!
      // 
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
  
#define EXECWITHPRINT____disable

  for (int i=indexRangeStart; i<=indexRangeStop; i++){
    xxeElement = &(stream[i]);
    
    if (xxeElement->predicateBitField & filterBitField)
      continue; // filter out this operation
    
//    printf("gjt: %d, opId=%d\n", i, stream[i].opId);
    switch(stream[i].opId){
    case send:
      {
        xxeSendInfo = (SendInfo *)xxeElement;
        char *buffer = (char *)xxeSendInfo->buffer;
        if (xxeSendInfo->indirectionFlag)
          buffer = *(char **)xxeSendInfo->buffer;
        int size = xxeSendInfo->size;
        if (xxeSendInfo->vectorFlag)
          size *= *vectorLength;
#ifdef EXECWITHPRINT
        fprintf(stderr, "XXE::send: <localPet=%d> buffer=%p, size=%d, dst=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(),
          xxeSendInfo->buffer, xxeSendInfo->size, xxeSendInfo->dstPet,
          xxeSendInfo->tag, xxeSendInfo->vectorFlag,
          xxeSendInfo->indirectionFlag);
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
#ifdef EXECWITHPRINT
        fprintf(stderr, "XXE::recv: <localPet=%d> buffer=%p, size=%d, src=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(),
          xxeRecvInfo->buffer, xxeRecvInfo->size, xxeRecvInfo->srcPet,
          xxeRecvInfo->tag, xxeRecvInfo->vectorFlag,
          xxeRecvInfo->indirectionFlag);
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
#ifdef EXECWITHPRINT
        fprintf(stderr, "XXE::sendRRA: <localPet=%d> \n", vm->getLocalPet());
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
#ifdef EXECWITHPRINT
        fprintf(stderr, "XXE::recvRRA: <localPet=%d> \n", vm->getLocalPet());
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
#ifdef EXECWITHPRINT
        fprintf(stderr, "XXE::sendrecv: <localPet=%d> dst=%d, sendSize=%d, "
          " src=%d, recvSize=%d\n", vm->getLocalPet(), xxeSendRecvInfo->dstPet,
          srcSize, xxeSendRecvInfo->srcPet, dstSize);
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
#ifdef EXECWITHPRINT
        fprintf(stderr, "XXE::sendRRArecv: <localPet=%d> \n", vm->getLocalPet());
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
        xxeSendnbInfo = (SendnbInfo *)xxeElement;
        char *buffer = (char *)xxeSendnbInfo->buffer;
        if (xxeSendnbInfo->indirectionFlag)
          buffer = *(char **)xxeSendnbInfo->buffer;
        int size = xxeSendnbInfo->size;
        if (xxeSendnbInfo->vectorFlag)
          size *= *vectorLength;
        vm->send(buffer, size, xxeSendnbInfo->dstPet, xxeSendnbInfo->commhandle,
          xxeSendnbInfo->tag);
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
        xxeIndexElement = &(stream[xxeWaitOnIndexInfo->index]);
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
      break;
    case testOnIndex:
      {
        xxeTestOnIndexInfo = (TestOnIndexInfo *)xxeElement;
        xxeIndexElement = &(stream[xxeTestOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          int completeFlag;
          VMK::status status;
          vm->commtest(xxeCommhandleInfo->commhandle, &completeFlag, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
          if (completeFlag){
            // comm finished
            xxeCommhandleInfo->activeFlag = false;  // reset
          }else
            if (finished) *finished = false;  // comm not finished
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
        for (int k=0; k<count; k++)
          completeFlag[k] = 0;  // reset
        while (completeTotal < count){
          for (int k=0; k<count; k++){
            if (!completeFlag[k]){
              xxeIndexElement = &(stream[xxeWaitOnAnyIndexSubInfo->index[k]]);
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
                    &localFinished, &localCancelled);
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
        for (int j=xxeWaitOnIndexRangeInfo->indexStart;
          j<xxeWaitOnIndexRangeInfo->indexEnd; j++){
          xxeIndexElement = &(stream[j]);
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
        xxeIndexElement = &(stream[waitOnIndexSubInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
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
              filterBitField, &localFinished, &localCancelled);
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
        xxeIndexElement = &(stream[testOnIndexSubInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        if (xxeCommhandleInfo->activeFlag){
          // there is an outstanding active communication
          int completeFlag;
          VMK::status status;
          vm->commtest(xxeCommhandleInfo->commhandle, &completeFlag, &status);
          xxeCommhandleInfo->cancelledFlag = vm->cancelled(&status);
          if (completeFlag){
            // comm finished -> recursive call into xxe execution
            xxeCommhandleInfo->activeFlag = false;  // reset
            if (testOnIndexSubInfo->xxe){
              // recursive call into xxe execution
              bool localFinished;
              bool localCancelled;
              testOnIndexSubInfo->xxe->exec(rraCount,
                rraList + testOnIndexSubInfo->rraShift,
                vectorLength + testOnIndexSubInfo->vectorLengthShift,
                filterBitField, &localFinished, &localCancelled);
              if (!localFinished)
                if (finished) *finished = false;  // unfinished ops in sub
              if (localCancelled)
                if (cancelled) *cancelled = true;  // cancelled ops in sub
            }
          }else
            if (finished) *finished = false;  // comm not finished
        }
        if (cancelled && xxeCommhandleInfo->cancelledFlag) *cancelled = true;
      }
      break;
    case cancelIndex:
      {
        xxeCancelIndexInfo = (CancelIndexInfo *)xxeElement;
        xxeIndexElement = &(stream[xxeCancelIndexInfo->index]);
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
        // recursively resolve the TKs of the arguments and execute operation
        sssDstRra(rraBase, xxeSumSuperScalarDstRRAInfo->elementTK,
          rraOffsetList, valueBase, valueOffsetList, 
          xxeSumSuperScalarDstRRAInfo->valueTK, termCount, vectorL, 0);
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
        // recursively resolve the TKs of the arguments and execute operation
        ssslDstRra(rraBaseList, rraIndexList,
          xxeSumSuperScalarListDstRRAInfo->elementTK,
          rraOffsetList,
          valueBaseListResolve, valueOffsetList, baseListIndexList,
          xxeSumSuperScalarListDstRRAInfo->valueTK, termCount, vectorL,
          0);
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
        char **factorList =
          (char **)xxeProductSumSuperScalarDstRRAInfo->factorList;
        char *valueBase =
          (char *)xxeProductSumSuperScalarDstRRAInfo->valueBase;
        if (xxeProductSumSuperScalarDstRRAInfo->indirectionFlag)
          valueBase = *(char **)xxeProductSumSuperScalarDstRRAInfo->valueBase;
#else
        int *rraBase =
          (int *)rraList[xxeProductSumSuperScalarDstRRAInfo->rraIndex];
        int **factorList =
          (int **)xxeProductSumSuperScalarDstRRAInfo->factorList;
        int *valueBase =
          (int *)xxeProductSumSuperScalarDstRRAInfo->valueBase;
        if (xxeProductSumSuperScalarDstRRAInfo->indirectionFlag)
          valueBase = *(int **)xxeProductSumSuperScalarDstRRAInfo->valueBase;
#endif
        // recursively resolve the TKs of the arguments and execute operation
        psssDstRra(rraBase, xxeProductSumSuperScalarDstRRAInfo->elementTK,
          rraOffsetList, factorList, 
          xxeProductSumSuperScalarDstRRAInfo->factorTK,
          valueBase, valueOffsetList,
          xxeProductSumSuperScalarDstRRAInfo->valueTK, termCount, vectorL, 0);
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
        char **factorList =
          (char **)xxeProductSumSuperScalarListDstRRAInfo->factorList;
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
        int **factorList =
          (int **)xxeProductSumSuperScalarListDstRRAInfo->factorList;
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
        // recursively resolve the TKs of the arguments and execute operation
        pssslDstRra(rraBaseList, rraIndexList,
          xxeProductSumSuperScalarListDstRRAInfo->elementTK,
          rraOffsetList, factorList, 
          xxeProductSumSuperScalarListDstRRAInfo->factorTK,
          valueBaseListResolve, valueOffsetList, baseListIndexList,
          xxeProductSumSuperScalarListDstRRAInfo->valueTK, termCount, vectorL,
          0);
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
        char **factorList =
          (char **)xxeProductSumSuperScalarSrcRRAInfo->factorList;
        char *elementBase =
          (char *)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
        if (xxeProductSumSuperScalarSrcRRAInfo->indirectionFlag)
          elementBase =
            *(char **)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
#else
        int *rraBase =
          (int *)rraList[xxeProductSumSuperScalarSrcRRAInfo->rraIndex];
        int **factorList =
          (int **)xxeProductSumSuperScalarSrcRRAInfo->factorList;
        int *elementBase =
          (int *)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
        if (xxeProductSumSuperScalarSrcRRAInfo->indirectionFlag)
          elementBase =
            *(int **)xxeProductSumSuperScalarSrcRRAInfo->elementBase;
#endif
        // recursively resolve the TKs of the arguments and execute operation
        psssSrcRra(rraBase, xxeProductSumSuperScalarSrcRRAInfo->valueTK,
          rraOffsetList, factorList,
          xxeProductSumSuperScalarSrcRRAInfo->factorTK,
          elementBase, elementOffsetList,
          xxeProductSumSuperScalarSrcRRAInfo->elementTK, termCount,
          vectorL, 0);
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
        char **factorList =
          (char **)xxeProductSumSuperScalarContigRRAInfo->factorList;
        char *valueList =
          (char *)xxeProductSumSuperScalarContigRRAInfo->valueList;
        if (xxeProductSumSuperScalarContigRRAInfo->indirectionFlag)
          valueList =
            *(char **)xxeProductSumSuperScalarContigRRAInfo->valueList;
#else
        int *rraBase =
          (int *)rraList[xxeProductSumSuperScalarContigRRAInfo->rraIndex];
        int **factorList =
          (int **)xxeProductSumSuperScalarContigRRAInfo->factorList;
        int *valueList =
          (int *)xxeProductSumSuperScalarContigRRAInfo->valueList;
        if (xxeProductSumSuperScalarContigRRAInfo->indirectionFlag)
          valueList =
            *(int **)xxeProductSumSuperScalarContigRRAInfo->valueList;
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
        }
      }
      break;
    case zeroSuperScalarRRA:
      {
        xxeZeroSuperScalarRRAInfo =
          (ZeroSuperScalarRRAInfo *)xxeElement;
        int *rraOffsetList = xxeZeroSuperScalarRRAInfo->rraOffsetList;
        int rraIndex = xxeZeroSuperScalarRRAInfo->rraIndex;
        int termCount = xxeZeroSuperScalarRRAInfo->termCount;
        bool vectorFlag = xxeZeroSuperScalarRRAInfo->vectorFlag;
        switch (xxeZeroSuperScalarRRAInfo->elementTK){
        case I4:
          {
            ESMC_I4 *rraBase = (ESMC_I4 *)rraList[rraIndex];
            if (!vectorFlag)
              for (int i=0; i<termCount; i++)
                *(rraBase+rraOffsetList[i]) = 0;
            else
              for (int i=0; i<termCount; i++)
                for (int k=0; k<*vectorLength; k++)
                  *(rraBase+(rraOffsetList[i]* *vectorLength)+k) = 0;
          }
          break;
        case I8:
          {
            ESMC_I8 *rraBase = (ESMC_I8 *)rraList[rraIndex];
            if (!vectorFlag)
              for (int i=0; i<termCount; i++)
                *(rraBase+rraOffsetList[i]) = 0;
            else
              for (int i=0; i<termCount; i++)
                for (int k=0; k<*vectorLength; k++)
                  *(rraBase+(rraOffsetList[i]* *vectorLength)+k) = 0;
          }
          break;
        case R4:
          {
            ESMC_R4 *rraBase = (ESMC_R4 *)rraList[rraIndex];
            if (!vectorFlag)
              for (int i=0; i<termCount; i++)
                *(rraBase+rraOffsetList[i]) = 0.;
            else
              for (int i=0; i<termCount; i++)
                for (int k=0; k<*vectorLength; k++)
                  *(rraBase+(rraOffsetList[i]* *vectorLength)+k) = 0.;
          }
          break;
        case R8:
          {
            ESMC_R8 *rraBase = (ESMC_R8 *)rraList[rraIndex];
            if (!vectorFlag)
              for (int i=0; i<termCount; i++)
                *(rraBase+rraOffsetList[i]) = 0.;
            else
              for (int i=0; i<termCount; i++)
                for (int k=0; k<*vectorLength; k++)
                  *(rraBase+(rraOffsetList[i]* *vectorLength)+k) = 0.;
          }
          break;
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
          {
            ESMC_I4 *dstPointer = (ESMC_I4*)dstBase;
            ESMC_I4 *srcPointer;
            for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
              srcPointer = ((ESMC_I4*)rraBase) + rraOffsetList[k] * vectorL;
              for (int kk=0; kk<countList[k]*vectorL; kk++)
                dstPointer[kk] = srcPointer[kk]; 
              dstPointer += countList[k] * vectorL;
            }
          }
          break;
        case I8:
          {
            ESMC_I8 *dstPointer = (ESMC_I8*)dstBase;
            ESMC_I8 *srcPointer;
            for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
              srcPointer = ((ESMC_I8*)rraBase) + rraOffsetList[k] * vectorL;
              for (int kk=0; kk<countList[k]*vectorL; kk++)
                dstPointer[kk] = srcPointer[kk]; 
              dstPointer += countList[k] * vectorL;
            }
          }
          break;
        case R4:
          {
            ESMC_R4 *dstPointer = (ESMC_R4*)dstBase;
            ESMC_R4 *srcPointer;
            for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
              srcPointer = ((ESMC_R4*)rraBase) + rraOffsetList[k] * vectorL;
              for (int kk=0; kk<countList[k]*vectorL; kk++)
                dstPointer[kk] = srcPointer[kk]; 
              dstPointer += countList[k] * vectorL;
            }
          }
          break;
        case R8:
          {
            ESMC_R8 *dstPointer = (ESMC_R8*)dstBase;
            ESMC_R8 *srcPointer;
            for (int k=0; k<xxeMemGatherSrcRRAInfo->chunkCount; k++){
              srcPointer = ((ESMC_R8*)rraBase) + rraOffsetList[k] * vectorL;
              for (int kk=0; kk<countList[k]*vectorL; kk++)
                dstPointer[kk] = srcPointer[kk]; 
              dstPointer += countList[k] * vectorL;
            }
          }
          break;
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
          xxeSubInfo->xxe->exec(rraCount, rraList + xxeSubInfo->rraShift,
            vectorLength + xxeSubInfo->vectorLengthShift, filterBitField,
            &localFinished, &localCancelled);
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
        for (int k=0; k<xxeSubMultiInfo->count; k++){
          if (xxeSubMultiInfo->xxe[k]){
            // recursive call:
            bool localFinished;
            bool localCancelled;
            xxeSubMultiInfo->xxe[k]->exec(rraCount, rraList, vectorLength,
              filterBitField, &localFinished, &localCancelled);
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
        xxeWtimerInfoActual = (WtimerInfo *)(&(stream[index]));
        double *wtimeActual = &(xxeWtimerInfoActual->wtime);
        double *wtimeSumActual = &(xxeWtimerInfoActual->wtimeSum);
        int *sumTermCountActual = &(xxeWtimerInfoActual->sumTermCount);
        double wtimeRelative = *(xxeWtimerInfo->relativeWtime);
        // this xxe wtimer stream element
        VMK::wtime(wtime);
        *wtime -= wtimeRelative;
        // actual xxe wtimer stream element
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
  }
  
  if (dTime != NULL){
    VMK::wtime(&t1);
    *dTime = t1 - t0;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// templated XXE operations used in XXE::exec()
//-----------------------------------------------------------------------------
#define XXE_RECURSIVE_DEBUG___disable

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
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in psv kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  for (int i=0; i<factorCount; i++)
    *element += factorList[i] * valueList[i];
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
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in pss kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  *element += *factor * *value;
}

//-----------------------------------------------------------------------------

template<typename T, typename V>
void XXE::sssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
  V *valueBase, int *valueOffsetList, TKId valueTK, int termCount,
  int vectorLength, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing sssDstRra operation on the data.
  T *element;
  V *value;
#ifdef XXE_RECURSIVE_DEBUG
  printf("Entering sssDstRra with %s, %s, resolved=%d\n", typeid(T).name(), 
    typeid(V).name(), resolved);
#endif
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        sssDstRra(rraBaseT, elementTK, rraOffsetList, valueBase,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
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
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueBaseT = (ESMC_I8 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueBaseT = (ESMC_R4 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueBaseT = (ESMC_R8 *)valueBase;
        sssDstRra(rraBase, elementTK, rraOffsetList, valueBaseT,
          valueOffsetList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in sssDstRra kernel with %s, %s\n", typeid(T).name(), 
    typeid(V).name());
#endif
  if (vectorLength==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i];
      value = valueBase + valueOffsetList[i];
      *element += *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i] * vectorLength;
      value = valueBase + valueOffsetList[i] * vectorLength;
      for (int k=0; k<vectorLength; k++)  // vector loop
        *(element+k) += *(value+k);
    }    
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename V>
void XXE::ssslDstRra(T **rraBaseList, int *rraIndexList, TKId elementTK,
  int *rraOffsetList, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList,
  TKId valueTK, int termCount, int vectorLength, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssDstRra operation on the data.
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
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **rraBaseTList = (ESMC_I8 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **rraBaseTList = (ESMC_R4 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **rraBaseTList = (ESMC_R8 **)rraBaseList;
        ssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
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
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **valueBaseTList = (ESMC_I8 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **valueBaseTList = (ESMC_R4 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **valueBaseTList = (ESMC_R8 **)valueBaseList;
        ssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in psssDstRra kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  if (vectorLength==1){
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
        + rraOffsetList[i] * vectorLength;
      value = valueBaseList[baseListIndexList[i]]
        + valueOffsetList[i] * vectorLength;
      for (int k=0; k<vectorLength; k++)  // vector loop
        *(element+k) += *(value+k);
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::psssDstRra(T *rraBase, TKId elementTK, int *rraOffsetList,
  U **factorList, TKId factorTK, V *valueBase, int *valueOffsetList,
  TKId valueTK, int termCount, int vectorLength, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssDstRra operation on the data.
  T *element;
  U *factor;
  V *value;
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        psssDstRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
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
        ESMC_I4 **factorListT = (ESMC_I4 **)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **factorListT = (ESMC_I8 **)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **factorListT = (ESMC_R4 **)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **factorListT = (ESMC_R8 **)factorList;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueBase, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
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
          valueBaseT, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueBaseT = (ESMC_I8 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueBaseT = (ESMC_R4 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueBaseT = (ESMC_R8 *)valueBase;
        psssDstRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueBaseT, valueOffsetList, valueTK, termCount, vectorLength,
          resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in psssDstRra kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  if (vectorLength==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i];
      factor = factorList[i];
      value = valueBase + valueOffsetList[i];
      *element += *factor * *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i] * vectorLength;
      factor = factorList[i];
      value = valueBase + valueOffsetList[i] * vectorLength;
      for (int k=0; k<vectorLength; k++)  // vector loop
        *(element+k) += *factor * *(value+k);
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::pssslDstRra(T **rraBaseList, int *rraIndexList, TKId elementTK,
  int *rraOffsetList, U **factorList, TKId factorTK, V **valueBaseList,
  int *valueOffsetList, int *baseListIndexList,
  TKId valueTK, int termCount, int vectorLength, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssDstRra operation on the data.
  T *element;
  U *factor;
  V *value;
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 **rraBaseTList = (ESMC_I4 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **rraBaseTList = (ESMC_I8 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **rraBaseTList = (ESMC_R4 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **rraBaseTList = (ESMC_R8 **)rraBaseList;
        pssslDstRra(rraBaseTList, rraIndexList, elementTK, rraOffsetList, 
          factorList, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
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
        ESMC_I4 **factorListT = (ESMC_I4 **)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **factorListT = (ESMC_I8 **)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **factorListT = (ESMC_R4 **)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **factorListT = (ESMC_R8 **)factorList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorListT, factorTK, valueBaseList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
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
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **valueBaseTList = (ESMC_I8 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **valueBaseTList = (ESMC_R4 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **valueBaseTList = (ESMC_R8 **)valueBaseList;
        pssslDstRra(rraBaseList, rraIndexList, elementTK, rraOffsetList,
          factorList, factorTK, valueBaseTList, valueOffsetList,
          baseListIndexList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in psssDstRra kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  if (vectorLength==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBaseList[rraIndexList[baseListIndexList[i]]]
        + rraOffsetList[i];
      factor = factorList[i];
      value = valueBaseList[baseListIndexList[i]] + valueOffsetList[i];
      *element += *factor * *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBaseList[rraIndexList[baseListIndexList[i]]]
        + rraOffsetList[i] * vectorLength;
      factor = factorList[i];
      value = valueBaseList[baseListIndexList[i]]
        + valueOffsetList[i] * vectorLength;
      for (int k=0; k<vectorLength; k++)  // vector loop
        *(element+k) += *factor * *(value+k);
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::psssSrcRra(T *rraBase, TKId valueTK, int *rraOffsetList,
  U **factorList, TKId factorTK, V *elementBase, int *elementOffsetList,
  TKId elementTK, int termCount, int vectorLength, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing psssSrcRra operation on the data.
  T *value;
  U *factor;
  V *element;
#ifdef XXE_RECURSIVE_DEBUG
  printf("Entering psssSrcRra kernel with %s, %s, %s, resolved=%d\n",
    typeid(T).name(), typeid(U).name(), typeid(V).name(), resolved);
#endif
  if (resolved==0){
    ++resolved;
    switch (valueTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        psssSrcRra(rraBaseT, valueTK, rraOffsetList, factorList, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
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
        ESMC_I4 **factorListT = (ESMC_I4 **)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **factorListT = (ESMC_I8 **)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **factorListT = (ESMC_R4 **)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **factorListT = (ESMC_R8 **)factorList;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorListT, factorTK,
          elementBase, elementOffsetList, elementTK, termCount, vectorLength,
          resolved);
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
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorLength,
          resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *elementBaseT = (ESMC_I8 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorLength,
          resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *elementBaseT = (ESMC_R4 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorLength,
          resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *elementBaseT = (ESMC_R8 *)elementBase;
        psssSrcRra(rraBase, valueTK, rraOffsetList, factorList, factorTK,
          elementBaseT, elementOffsetList, elementTK,  termCount, vectorLength,
          resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in psssSrcRra kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  if (vectorLength==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      value = rraBase + rraOffsetList[i];
      factor = factorList[i];
      element = elementBase + elementOffsetList[i];
      *element += *factor * *value;
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      value = rraBase + rraOffsetList[i] * vectorLength;
      factor = factorList[i];
      element = elementBase + elementOffsetList[i] * vectorLength;
      for (int k=0; k<vectorLength; k++)  // vector loop
        *(element+k) += *factor * *(value+k);
    }
  }
}

//-----------------------------------------------------------------------------

template<typename T, typename U, typename V>
void XXE::pssscRra(T *rraBase, TKId elementTK, int *rraOffsetList,
  U **factorList, TKId factorTK, V *valueList, TKId valueTK,
  int termCount, int vectorLength, int resolved){
  // Recursively resolve the TKs and typecast the arguments appropriately
  // before executing pssscRra operation on the data.
  T *element;
  U *factor;
  if (resolved==0){
    ++resolved;
    switch (elementTK){
    case I4:
      {
        ESMC_I4 *rraBaseT = (ESMC_I4 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *rraBaseT = (ESMC_I8 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *rraBaseT = (ESMC_R4 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *rraBaseT = (ESMC_R8 *)rraBase;
        pssscRra(rraBaseT, elementTK, rraOffsetList, factorList, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
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
        ESMC_I4 **factorListT = (ESMC_I4 **)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 **factorListT = (ESMC_I8 **)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 **factorListT = (ESMC_R4 **)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 **factorListT = (ESMC_R8 **)factorList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorListT, factorTK,
          valueList, valueTK, termCount, vectorLength, resolved);
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
          valueListT, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case I8:
      {
        ESMC_I8 *valueListT = (ESMC_I8 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R4:
      {
        ESMC_R4 *valueListT = (ESMC_R4 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorLength, resolved);
      }
      break;
    case R8:
      {
        ESMC_R8 *valueListT = (ESMC_R8 *)valueList;
        pssscRra(rraBase, elementTK, rraOffsetList, factorList, factorTK,
          valueListT, valueTK, termCount, vectorLength, resolved);
      }
      break;
    default:
      break;
    }
    return;
  }
#ifdef XXE_RECURSIVE_DEBUG
  printf("Arrived in pssscRra kernel with %s, %s, %s\n", typeid(T).name(), 
    typeid(U).name(), typeid(V).name());
#endif
  if (vectorLength==1){
    // scalar elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i];
      factor = factorList[i];
      *element += *factor * valueList[i];
    }
  }else{
    // vector elements
    for (int i=0; i<termCount; i++){  // super scalar loop
      element = rraBase + rraOffsetList[i] * vectorLength;
      factor = factorList[i];
      for (int k=0; k<vectorLength; k++)  // vector loop
        *(element+k) += *factor * valueList[i*vectorLength+k];
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
      "- indexStart out of range", ESMC_CONTEXT, &rc);
    return rc;
  }
  if (indexRangeStop > count-1){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- indexStop out of range", ESMC_CONTEXT, &rc);
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
    xxeElement = &(stream[i]);
    
    if (xxeElement->predicateBitField & filterBitField)
      continue; // filter out this operation
    
    fprintf(fp, "XXE::print(): <localPet=%d> i=%d, xxeElement=%p, opId=%d, "
      "predicateBitField=0x%08x\n", 
      vm->getLocalPet(), i, xxeElement, stream[i].opId,
      stream[i].predicateBitField);
    switch(stream[i].opId){
    case send:
      {
        xxeSendInfo = (SendInfo *)xxeElement;
        fprintf(fp, "  XXE::send: <localPet=%d> buffer=%p, size=%d, dst=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(),
          xxeSendInfo->buffer, xxeSendInfo->size, xxeSendInfo->dstPet,
          xxeSendInfo->tag, xxeSendInfo->vectorFlag,
          xxeSendInfo->indirectionFlag);
      }
      break;
    case recv:
      {
        xxeRecvInfo = (RecvInfo *)xxeElement;
        fprintf(fp, "  XXE::recv: <localPet=%d> buffer=%p, size=%d, src=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(),
          xxeRecvInfo->buffer, xxeRecvInfo->size, xxeRecvInfo->srcPet,
          xxeRecvInfo->tag, xxeRecvInfo->vectorFlag,
          xxeRecvInfo->indirectionFlag);
      }
      break;
    case sendRRA:
      {
        xxeSendRRAInfo = (SendRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::sendRRA: <localPet=%d> rraOffset=%d, size=%d, "
          "dst=%d, rraIndex=%d, tag=%d, vectorFlag=%d\n",
          vm->getLocalPet(),
          xxeSendRRAInfo->rraOffset, xxeSendRRAInfo->size,
          xxeSendRRAInfo->dstPet, xxeSendRRAInfo->rraIndex,
          xxeSendRRAInfo->tag, xxeSendRRAInfo->vectorFlag);
      }
      break;
    case recvRRA:
      {
        xxeRecvRRAInfo = (RecvRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::recvRRA: <localPet=%d> rraOffset=%d, size=%d, "
          "src=%d, rraIndex=%d, tag=%d, vectorFlag=%d\n",
          vm->getLocalPet(),
          xxeRecvRRAInfo->rraOffset, xxeRecvRRAInfo->size,
          xxeRecvRRAInfo->srcPet, xxeRecvRRAInfo->rraIndex,
          xxeRecvRRAInfo->tag, xxeRecvRRAInfo->vectorFlag);
      }
      break;
    case sendnb:
      {
        xxeSendnbInfo = (SendnbInfo *)xxeElement;
        fprintf(fp, "  XXE::sendnb: <localPet=%d> buffer=%p, size=%d, dst=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d, commhandle=%p\n",
          vm->getLocalPet(),
          xxeSendnbInfo->buffer, xxeSendnbInfo->size, xxeSendnbInfo->dstPet,
          xxeSendnbInfo->tag, xxeSendnbInfo->vectorFlag,
          xxeSendnbInfo->indirectionFlag, xxeSendnbInfo->commhandle);
      }
      break;
    case recvnb:
      {
        xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
        fprintf(fp, "  XXE::recvnb: <localPet=%d> buffer=%p, size=%d, src=%d, "
          "tag=%d, vectorFlag=%d, indirectionFlag=%d, commhandle=%p\n",
          vm->getLocalPet(),
          xxeRecvnbInfo->buffer, xxeRecvnbInfo->size, xxeRecvnbInfo->srcPet,
          xxeRecvnbInfo->tag, xxeRecvnbInfo->vectorFlag,
          xxeRecvnbInfo->indirectionFlag, xxeRecvnbInfo->commhandle);
      }
      break;
    case sendnbRRA:
      {
        xxeSendnbRRAInfo = (SendnbRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::sendnbRRA: <localPet=%d> rraOffset=%d, size=%d, "
          "dst=%d, rraIndex=%d, tag=%d, vectorFlag=%d, commhandle=%p\n",
          vm->getLocalPet(),
          xxeSendnbRRAInfo->rraOffset, xxeSendnbRRAInfo->size,
          xxeSendnbRRAInfo->dstPet, xxeSendnbRRAInfo->rraIndex,
          xxeSendnbRRAInfo->tag, xxeSendnbRRAInfo->vectorFlag,
          xxeSendnbRRAInfo->commhandle);
      }
      break;
    case recvnbRRA:
      {
        xxeRecvnbRRAInfo = (RecvnbRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::recvnbRRA: <localPet=%d> rraOffset=%d, size=%d, "
          "src=%d, rraIndex=%d, tag=%d, vectorFlag=%d, commhandle=%p\n",
          vm->getLocalPet(),
          xxeRecvnbRRAInfo->rraOffset, xxeRecvnbRRAInfo->size,
          xxeRecvnbRRAInfo->srcPet, xxeRecvnbRRAInfo->rraIndex,
          xxeRecvnbRRAInfo->tag, xxeRecvnbRRAInfo->vectorFlag,
          xxeRecvnbRRAInfo->commhandle);
      }
      break;
    case waitOnIndex:
      {
        xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
        xxeIndexElement = &(stream[xxeWaitOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::waitOnIndex: <localPet=%d> index=%d, "
          " commhandle=%p\n", vm->getLocalPet(), xxeWaitOnIndexInfo->index,
          xxeCommhandleInfo->commhandle);
      }
      break;
    case testOnIndex:
      {
        xxeTestOnIndexInfo = (TestOnIndexInfo *)xxeElement;
        xxeIndexElement = &(stream[xxeTestOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::TestOnIndex: <localPet=%d> index=%d, "
          " commhandle=%p\n", vm->getLocalPet(), xxeTestOnIndexInfo->index,
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
        fprintf(fp, "  XXE::waitOnIndexSubInfo <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case testOnIndexSub:
      {
        testOnIndexSubInfo = (TestOnIndexSubInfo *)xxeElement;
        fprintf(fp, "  XXE::testOnIndexSubInfo <localPet=%d>\n",
          vm->getLocalPet());
      }
      break;
    case cancelIndex:
      {
        xxeCancelIndexInfo = (CancelIndexInfo *)xxeElement;
        xxeIndexElement = &(stream[xxeCancelIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
        fprintf(fp, "  XXE::CancelIndex: <localPet=%d> index=%d, "
          " commhandle=%p\n", vm->getLocalPet(), xxeCancelIndexInfo->index,
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
        fprintf(fp, "  XXE::sumSuperScalarDstRRA <localPet=%d> rraIndex=%d, "
          "termCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(), xxeSumSuperScalarDstRRAInfo->rraIndex,
          xxeSumSuperScalarDstRRAInfo->termCount,
          xxeSumSuperScalarDstRRAInfo->vectorFlag,
          xxeSumSuperScalarDstRRAInfo->indirectionFlag);
      }
      break;
    case productSumSuperScalarDstRRA:
      {
        xxeProductSumSuperScalarDstRRAInfo =
          (ProductSumSuperScalarDstRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumSuperScalarDstRRA <localPet=%d> "
          "rraIndex=%d, termCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(), xxeProductSumSuperScalarDstRRAInfo->rraIndex,
          xxeProductSumSuperScalarDstRRAInfo->termCount,
          xxeProductSumSuperScalarDstRRAInfo->vectorFlag,
          xxeProductSumSuperScalarDstRRAInfo->indirectionFlag);
      }
      break;
    case productSumSuperScalarSrcRRA:
      {
        xxeProductSumSuperScalarSrcRRAInfo =
          (ProductSumSuperScalarSrcRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::productSumSuperScalarSrcRRA <localPet=%d> "
          "rraIndex=%d, termCount=%d, vectorFlag=%d, indirectionFlag=%d\n",
          vm->getLocalPet(), xxeProductSumSuperScalarSrcRRAInfo->rraIndex,
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
        fprintf(fp, "  XXE::zeroScalarRRA <localPet=%d> rraOffset=%d, "
          "rraIndex=%d\n", vm->getLocalPet(), xxeZeroScalarRRAInfo->rraOffset,
          xxeZeroScalarRRAInfo->rraIndex);
      }
      break;
    case zeroSuperScalarRRA:
      {
        xxeZeroSuperScalarRRAInfo = (ZeroSuperScalarRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroSuperScalarRRA <localPet=%d> rraOffsetList=%p, "
          "rraIndex=%d, termCount=%d, vectorFlag=%d\n", vm->getLocalPet(),
          xxeZeroSuperScalarRRAInfo->rraOffsetList,
          xxeZeroSuperScalarRRAInfo->rraIndex,
          xxeZeroSuperScalarRRAInfo->termCount,
          xxeZeroSuperScalarRRAInfo->vectorFlag);
      }
      break;
    case zeroMemset:
      {
        xxeZeroMemsetInfo = (ZeroMemsetInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroMemset <localPet=%d> buffer=%p, byteCount=%d, "
          "vectorFlag=%d, indirectionFlag=%d\n", 
            vm->getLocalPet(), xxeZeroMemsetInfo->buffer,
            xxeZeroMemsetInfo->byteCount, xxeZeroMemsetInfo->vectorFlag,
            xxeZeroMemsetInfo->indirectionFlag);
      }
      break;
    case zeroMemsetRRA:
      {
        xxeZeroMemsetRRAInfo = (ZeroMemsetRRAInfo *)xxeElement;
        fprintf(fp, "  XXE::zeroMemsetRRA <localPet=%d> byteCount=%d, "   
          "rraIndex=%d, vectorFlag=%d\n", 
          vm->getLocalPet(), xxeZeroMemsetRRAInfo->byteCount,
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
        fprintf(fp, "  XXE::memGatherSrcRRA <localPet=%d>\n",
          vm->getLocalPet());
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
        fprintf(fp, "  XXE::xxeSubMulti <localPet=%d>\n", vm->getLocalPet());
        for (int k=0; k<xxeSubMultiInfo->count; k++)
          xxeSubMultiInfo->xxe[k]->print(fp, rraCount, rraList); // recurs. call
      }
      break;
    case wtimer:
      {
        xxeWtimerInfo = (WtimerInfo *)xxeElement;
        fprintf(fp, "  XXE::wtimer <localPet=%d>\n", vm->getLocalPet());
        int index = xxeWtimerInfo->actualWtimerIndex;
        double *wtime = &(xxeWtimerInfo->wtime);
        *wtime = 0.;                      // initialize
        xxeWtimerInfo->wtimeSum = 0.;     // initialize
        xxeWtimerInfo->sumTermCount = -1;  // initialize
        xxeWtimerInfoActual = (WtimerInfo *)(&(stream[index]));
        double *wtimeActual = &(xxeWtimerInfoActual->wtime);
        double *wtimeSumActual = &(xxeWtimerInfoActual->wtimeSum);
        int *sumTermCountActual = &(xxeWtimerInfoActual->sumTermCount);
        double wtimeRelative = *(xxeWtimerInfo->relativeWtime);
      }
      break;
    case message:
      {
        xxeMessageInfo = (MessageInfo *)xxeElement;
        fprintf(fp, "  XXE::message <localPet=%d>\n", vm->getLocalPet());
      }
      break;
    case profileMessage:
      {
        xxeProfileMessageInfo = (ProfileMessageInfo *)xxeElement;
        fprintf(fp, "  XXE::profileMessage <localPet=%d>\n", vm->getLocalPet());
      }
      break;
    case nop:
      {
        fprintf(fp, "  XXE::nop <localPet=%d>\n", vm->getLocalPet());
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
//  Print profile data collected during the XXE stream execution.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
#if 0
  printf("gjt in ESMCI::XXE::printProfile(), stream=%p, %d, %d\n", stream,
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
    xxeElement = &(stream[i]);
    
    if (xxeElement->predicateBitField & lastFilterBitField)
      continue; // filter out this operation
    
//    printf("gjt: %d, opId=%d\n", i, stream[i].opId);
    switch(stream[i].opId){
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
  printf("gjt in ESMCI::XXE::optimizeElement(), stream=%p, %d, %d\n", stream,
    count, sizeof(StreamElement));
#endif
  
  if (index < 0 || index >= count){
    ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
      "- index out of range", ESMC_CONTEXT, &rc);
    return rc;
  }
    
  StreamElement *xxeElement = &(stream[index]);
  switch(stream[index].opId){
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
      void **factorList = xxeProductSumSuperScalarDstRRAInfo->factorList;
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
  printf("gjt in ESMCI::XXE::execReady(), stream=%p, %d, %d\n", stream, count, 
    sizeof(StreamElement));
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
    // repeat going through the entire stream until no more StreamElements need 
    // to be replaced, i.e. the i-loop will finally make it all the way through.
    sendnbCount = 0;
    recvnbCount = 0;
    
    for (i=0; i<count; i++){
      xxeElement = &(stream[i]);
//    printf("gjt: %d, opId=%d\n", i, stream[i].opId);
      int breakFlag = 0;  // reset
      switch(stream[i].opId){
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
              "- sendnbCount out of range", ESMC_CONTEXT, &rc);
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
              "- recvnbCount out of range", ESMC_CONTEXT, &rc);
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
              "- sendnbCount out of range", ESMC_CONTEXT, &rc);
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
              "- recvnbCount out of range", ESMC_CONTEXT, &rc);
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
          // replace stream
          int oldCount = count;               // hold on to old count
          StreamElement *oldStream = stream;  // hold on to old stream
          stream = new StreamElement[max];    // prepare new stream
          // fill in StreamElements from before this StreamElement
          memcpy(stream, oldStream, i*sizeof(StreamElement));
          // insert explicit waitOnIndex StreamElements
          count = i;
          for (int j=0; j<sendnbCount; j++){
            int index = sendnbIndexList[j];
            stream[count].opId = waitOnIndex;
            stream[count].predicateBitField = stream[index].predicateBitField;
            xxeElement = &(stream[count]);
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
          memcpy(stream+count, oldStream+i+1, (oldCount-i-1)
            * sizeof(StreamElement));
          count = sendnbCount + oldCount -1;  // account for modification
          delete [] oldStream;                // delete original stream
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
          // replace stream
          int oldCount = count;               // hold on to old count
          StreamElement *oldStream = stream;  // hold on to old stream
          stream = new StreamElement[max];    // prepare new stream
          // fill in StreamElements from before this StreamElement
          memcpy(stream, oldStream, i*sizeof(StreamElement));
          // insert explicit waitOnIndex StreamElements
          count = i;
          for (int j=0; j<recvnbCount; j++){
            int index = recvnbIndexList[j];
            stream[count].opId = waitOnIndex;
            stream[count].predicateBitField = stream[index].predicateBitField;
            xxeElement = &(stream[count]);
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
          memcpy(stream+count, oldStream+i+1, (oldCount-i-1)
            * sizeof(StreamElement));
          count = recvnbCount + oldCount -1;  // account for modification
          delete [] oldStream;                // delete original stream
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

  // translate profiling Wtimer Ids into XXE stream indices
  int *idList = new int[count];
  int *indexList = new int[count];
  int iCount = 0; // reset
  for (i=0; i<count; i++){
    // set up id-to-index look-up
    if (stream[i].opId == wtimer){
      xxeElement = &(stream[i]);
      xxeWtimerInfo = (XXE::WtimerInfo *)xxeElement;
      idList[iCount] = xxeWtimerInfo->timerId;
      indexList[iCount] = i;
      ++iCount;
    }
  }
  for (i=0; i<count; i++){
    // use id-to-index look-up to translate Wtimer Ids
    if (stream[i].opId == wtimer){
      xxeElement = &(stream[i]);
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
          if (xxe->stream[ii].opId == wtimer){
            xxeElement2 = &(xxe->stream[ii]);
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
              &(((WtimerInfo *)(&(stream[indexList[j]])))->wtime);
            ++resolveCounter;
          }
        }
        if (resolveCounter==2) break; // resolved both Ids
      }
      if (j==iCount){
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          "- unable to resolve XXE WTimer Id", ESMC_CONTEXT, &rc);
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
  printf("gjt in ESMCI::XXE::optimize(), stream=%p, %d, %d\n", stream, count, 
    sizeof(StreamElement));
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
    // repeat going through the entire stream until no more StreamElements need 
    // to be replaced, i.e. the i-loop will finally make it all the way through.

    if (aq != NULL) delete aq;  // delete from previous analysis loop
    aq = NULL;  // indicate empty queue
    
    int breakFlag = 0;  // reset
    for (i=0; i<count; i++){
      xxeElement = &(stream[i]);
//    printf("gjt: %d, opId=%d\n", i, stream[i].opId);
      switch(stream[i].opId){
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
              xxeElement = &(stream[aeList[0]->indexList[0]]);
              xxeSendnbInfo = (SendnbInfo *)xxeElement;
              xxeSendnbInfo->buffer = buffer;
              xxeSendnbInfo->size = bufferSize;
              // invalidate all other sendnb StreamElements to nop
              for (int k=1; k<aeList[0]->indexCount; k++){
                stream[aeList[0]->indexList[k]].opId = nop;
                stream[aeList[0]->indexList[k]].predicateBitField = 0x0;
              }
            }else{
              // need to introduce a contiguous intermediate buffer
              int bufferSize = 0; // reset
              for (int kk=0; kk<aeCountList[j]; kk++)
                bufferSize += aeList[kk]->bufferSize;
      //printf("allocate itermediate buffer of size: %d bytes, filling in"
      //  " for stream index: %d\n", bufferSize, aeList[0]->indexList[0]);
              char *buffer = new char[bufferSize]; //TODO: leave leak
              // prepare extra stream element with memCpy StreamElements
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
                // invalidate all StreamElements in stream assoc. w. aeList[kk]
                for (int k=0; k<aeList[kk]->indexCount; k++){
                  stream[aeList[kk]->indexList[k]].opId = nop;
                  stream[aeList[kk]->indexList[k]].predicateBitField = 0x0;
                }
              }
              // determine first Sendnb index
              int firstIndex = aeList[0]->indexList[0];
              // replace the first sendnb StreamElement using the interm. buffer
              stream[firstIndex].opId = sendnb;
              stream[firstIndex].predicateBitField = 0x0;  //TODO: match!
              xxeElement = &(stream[firstIndex]);
              xxeSendnbInfo = (SendnbInfo *)xxeElement;
              xxeSendnbInfo->buffer = buffer;
              xxeSendnbInfo->size = bufferSize;
              // slip in the associated memcpy()s _before_ the first Sendnb
              if (xxeCount+count > max){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                  "- count out of range", ESMC_CONTEXT, &rc);
                return rc;
              }
              // start a new stream
              StreamElement *newstream = new StreamElement[max]; // prep. stream
              // fill in StreamElements from before firstIndex 
              // (excluding firstIndex)
              memcpy(newstream, stream, firstIndex*sizeof(StreamElement));
              // insert extrastream
              memcpy(newstream+firstIndex, extrastream, xxeCount 
                * sizeof(StreamElement));
              delete [] extrastream;  // done using
              // fill in StreamElements from after firstIndex
              // (including firstIndex)
              memcpy(newstream+firstIndex+xxeCount, stream+firstIndex,
                (count-firstIndex)*sizeof(StreamElement));
              // replace stream
              delete [] stream; // delete original stream
              stream = newstream; // replace by new stream
              count = count + xxeCount;        // account for modification
              // need to indicate that stream was modified _before_ current
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
              xxeElement = &(stream[aeList[0]->indexList[0]]);
              xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
              xxeRecvnbInfo->buffer = buffer;
              xxeRecvnbInfo->size = bufferSize;
              // invalidate all other recvnb StreamElements to nop
              for (int k=1; k<aeList[0]->indexCount; k++){
                stream[aeList[0]->indexList[k]].opId = nop;
                stream[aeList[0]->indexList[k]].predicateBitField = 0x0;
              }
            }else{
              // need to introduce a contiguous intermediate buffer
              int bufferSize = 0; // reset
              for (int kk=0; kk<aeCountList[j]; kk++)
                bufferSize += aeList[kk]->bufferSize;
           //printf("allocate itermediate buffer of size: %d bytes, filling in"
           //  " for stream index: %d\n", bufferSize, aeList[0]->indexList[0]);
              char *buffer = new char[bufferSize]; //TODO: leave leak
              // prepare extra stream segment with memCpy StreamElements
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
                // invalidate all StreamElements in stream asso. with aeList[kk]
                for (int k=0; k<aeList[kk]->indexCount; k++){
                  stream[aeList[kk]->indexList[k]].opId = nop;
                  stream[aeList[kk]->indexList[k]].predicateBitField = 0x0;
                }
              }
              // replace the very first recvnb StreamElement using the
              // intermediate buffer
              stream[aeList[0]->indexList[0]].opId = recvnb;
              stream[aeList[0]->indexList[0]].predicateBitField = 0x0;//TODO:mat
              xxeElement = &(stream[aeList[0]->indexList[0]]);
              xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
              xxeRecvnbInfo->buffer = buffer;
              xxeRecvnbInfo->size = bufferSize;
              // slip in the associated memcpy()s _after_ the waitOnAllRecvnb
              if (xxeCount+count > max){
                ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                  "- count out of range", ESMC_CONTEXT, &rc);
                return rc;
              }
              // start a new stream
              StreamElement *newstream = new StreamElement[max];  // prep stream
              // fill in StreamElements from before this StreamElement
              // (including this StreamElement)
              memcpy(newstream, stream, (i+1)*sizeof(StreamElement));
              // insert extrastream
              memcpy(newstream+i+1, extrastream, xxeCount
                * sizeof(StreamElement));
              delete [] extrastream;  // done using
              // fill in StreamElements from after this StreamElement
              // (excluding this StreamElement)
              memcpy(newstream+i+1+xxeCount, stream+i+1, (count-i-1)
                * sizeof(StreamElement));
              // replace stream
              delete [] stream; // delete original stream
              stream = newstream; // replace by new stream
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

  // remove all the nop StreamElements from stream
  // start a new stream
  StreamElement *newstream = new StreamElement[max];  // prepare a new stream
  int xxeCount = 0;
  for (int i=0; i<count; i++){
    xxeElement = &(stream[i]);
    if (stream[i].opId != nop){
      memcpy(newstream+xxeCount, stream+i, sizeof(StreamElement));
      ++xxeCount;
    }
  }
  // replace stream
  delete [] stream; // delete original stream
  stream = newstream; // replace by new stream
  count = xxeCount;
  // done replacing the nop StreamElements from stream
  
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
//  Increase the length of the XXE stream.
//  CAUTION: This method changes the location (in memory) of the entire stream!
//    Previously written stream elements will be moved to a new location.
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
      "- increase must be positive", ESMC_CONTEXT, &rc);
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
  memcpy(streamNew, stream, count*sizeof(StreamElement)); // copy prev. elements
  delete [] stream;     // delete previous stream
  stream = streamNew;   // plug in newly allocated stream
  max = maxNew;         // adjust max value

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::XXE::growStorage()"
//BOPI
// !IROUTINE:  ESMCI::XXE::growStorage
//
// !INTERFACE:
int XXE::growStorage(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  int increase){    // in - number of additional elements
//
// !DESCRIPTION:
//  Increase the length of the XXE storage.
//  CAUTION: This method changes the location (in memory) of the entire storage!
//    Previously written storage elements will be moved to a new location.
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
      "- increase must be positive", ESMC_CONTEXT, &rc);
    return rc;
  }
  
  int storageMaxCountNew = storageMaxCount + increase;
  char **storageNew;
  try{
    storageNew = new char*[storageMaxCountNew];
  }catch (...){
    ESMC_LogDefault.AllocError(ESMC_CONTEXT, &rc);
    return rc;
  }
  memcpy(storageNew, storage, storageCount*sizeof(char *)); //copy prev elements
  delete [] storage;      // delete previous storage
  storage = storageNew;   // plug in newly allocated storage
  storageMaxCount = storageMaxCountNew;     // adjust max value

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
//  Increase the length of the XXE commhandle storage.
//  CAUTION: This method changes the location (in memory) of the entire 
//           commhandle storage!
//    Previously written commhandle storage elements will be moved to a new
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
      "- increase must be positive", ESMC_CONTEXT, &rc);
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
//  Increase the length of the XXE Sub storage.
//  CAUTION: This method changes the location (in memory) of the entire 
//           XXE Sub storage!
//    Previously written XXE Sub storage elements will be moved to a new
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
      "- increase must be positive", ESMC_CONTEXT, &rc);
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
//  CAUTION: The location (in memory) of the entire stream may be changed by
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
#define ESMC_METHOD "ESMCI::XXE::incStorageCount()"
//BOPI
// !IROUTINE:  ESMCI::XXE::incStorageCount
//
// !INTERFACE:
int XXE::incStorageCount(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ){
//
// !DESCRIPTION:
//  Increment the storageCount by one.
//  CAUTION: The location (in memory) of the entire storage may be changed by
//           this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  ++storageCount;
  if (storageCount >= storageMaxCount)
    if (ESMC_LogDefault.MsgFoundError(growStorage(10000), 
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
//  CAUTION: The location (in memory) of the entire commhandle storage may be
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
//  CAUTION: The location (in memory) of the entire SSE Sub storage may be
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
#define ESMC_METHOD "ESMCI::XXE::storeStorage()"
//BOPI
// !IROUTINE:  ESMCI::XXE::storeStorage
//
// !INTERFACE:
int XXE::storeStorage(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  char *storageArg
  ){
//
// !DESCRIPTION:
//  Append an element at the end of the storage.
//  CAUTION: The location (in memory) of the entire storage may be changed by
//           this call!
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  storage[storageCount] = storageArg;
  localrc = incStorageCount();
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
//  Append an element at the end of the commhandle storage.
//  CAUTION: The location (in memory) of the entire commhandle storage may be
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
//  Append an element at the end of the XXE Sub storage.
//  CAUTION: The location (in memory) of the entire XXE Sub storage may be
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
  int size, 
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
  bufferInfoList.push_back(bufferInfo);
  
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
//  Append an xxeSub at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = xxeSub;
  stream[count].predicateBitField = predicateBitField;
  XxeSubInfo *xxeSubInfo = (XxeSubInfo *)&(stream[count]);
  xxeSubInfo->xxe = xxe;
  xxeSubInfo->rraShift = rraShift;
  xxeSubInfo->vectorLengthShift = vectorLengthShift;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a wtimer element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = wtimer;
  stream[count].predicateBitField = predicateBitField;
  WtimerInfo *xxeWtimerInfo = (WtimerInfo *)&(stream[count]);
  xxeWtimerInfo->timerId = id;
  int stringLen = strlen(string);
  xxeWtimerInfo->timerString = new char[stringLen+1];
  strcpy(xxeWtimerInfo->timerString, string);
  xxeWtimerInfo->actualWtimerId = actualId;
  xxeWtimerInfo->relativeWtimerId = relativeId;
  xxeWtimerInfo->relativeWtimerXXE = relativeXXE;

  // keep track of strings for xxe garbage collection
  localrc = storeStorage(xxeWtimerInfo->timerString);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a recv element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = recv;
  stream[count].predicateBitField = predicateBitField;
  RecvInfo *xxeRecvInfo = (RecvInfo *)&(stream[count]);
  xxeRecvInfo->buffer = buffer;
  xxeRecvInfo->size = size;
  xxeRecvInfo->srcPet = srcPet;
  xxeRecvInfo->tag = tag;
  xxeRecvInfo->vectorFlag = vectorFlag;
  xxeRecvInfo->indirectionFlag = indirectionFlag;
  xxeRecvInfo->activeFlag = false;
  xxeRecvInfo->cancelledFlag = false;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a send element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = send;
  stream[count].predicateBitField = predicateBitField;
  SendInfo *xxeSendInfo = (SendInfo *)&(stream[count]);
  xxeSendInfo->buffer = buffer;
  xxeSendInfo->size = size;
  xxeSendInfo->dstPet = dstPet;
  xxeSendInfo->tag = tag;
  xxeSendInfo->vectorFlag = vectorFlag;
  xxeSendInfo->indirectionFlag = indirectionFlag;
  xxeSendInfo->activeFlag = false;
  xxeSendInfo->cancelledFlag = false;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a sendRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = sendRRA;
  stream[count].predicateBitField = predicateBitField;
  SendRRAInfo *xxeSendRRAInfo = (SendRRAInfo *)&(stream[count]);
  xxeSendRRAInfo->rraOffset = rraOffset;
  xxeSendRRAInfo->size = size;
  xxeSendRRAInfo->dstPet = dstPet;
  xxeSendRRAInfo->rraIndex = rraIndex;
  xxeSendRRAInfo->tag = tag;
  xxeSendRRAInfo->vectorFlag = vectorFlag;
  xxeSendRRAInfo->activeFlag = false;
  xxeSendRRAInfo->cancelledFlag = false;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a sendrecv element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = sendrecv;
  stream[count].predicateBitField = predicateBitField;
  SendRecvInfo *xxeSendRecvInfo = (SendRecvInfo *)&(stream[count]);
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
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a sendRRArecv element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = sendRRArecv;
  stream[count].predicateBitField = predicateBitField;
  SendRRARecvInfo *xxeSendRRARecvInfo = (SendRRARecvInfo *)&(stream[count]);
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
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a recvnb element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = recvnb;
  stream[count].predicateBitField = predicateBitField;
  RecvnbInfo *xxeRecvnbInfo = (RecvnbInfo *)&(stream[count]);
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
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a sendnb element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = sendnb;
  stream[count].predicateBitField = predicateBitField;
  SendnbInfo *xxeSendnbInfo = (SendnbInfo *)&(stream[count]);
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
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a sendnbRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = sendnbRRA;
  stream[count].predicateBitField = predicateBitField;
  SendnbRRAInfo *xxeSendnbRRAInfo = (SendnbRRAInfo *)&(stream[count]);
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
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a memCpySrcRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = memCpySrcRRA;
  stream[count].predicateBitField = predicateBitField;
  MemCpySrcRRAInfo *xxeMemCpySrcRRAInfo = (MemCpySrcRRAInfo *)&(stream[count]);
  xxeMemCpySrcRRAInfo->rraOffset = rraOffset;
  xxeMemCpySrcRRAInfo->size = size;
  xxeMemCpySrcRRAInfo->dstMem = dstMem;
  xxeMemCpySrcRRAInfo->rraIndex = rraIndex;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a memGatherSrcRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = memGatherSrcRRA;
  stream[count].predicateBitField = predicateBitField;
  MemGatherSrcRRAInfo *xxeMemGatherSrcRRAInfo =
    (MemGatherSrcRRAInfo *)&(stream[count]);
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
  localrc = storeStorage(rraOffsetListChar);  // for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(countListChar);  // for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a zeroScalarRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = zeroScalarRRA;
  stream[count].predicateBitField = predicateBitField;
  ZeroScalarRRAInfo *xxeZeroScalarRRAInfo =
    (ZeroScalarRRAInfo *)&(stream[count]);
  xxeZeroScalarRRAInfo->elementTK = elementTK;
  xxeZeroScalarRRAInfo->rraOffset = rraOffset;
  xxeZeroScalarRRAInfo->rraIndex = rraIndex;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a zeroSuperScalarRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = zeroSuperScalarRRA;
  stream[count].predicateBitField = predicateBitField;
  ZeroSuperScalarRRAInfo *xxeZeroSuperScalarRRAInfo =
    (ZeroSuperScalarRRAInfo *)&(stream[count]);
  xxeZeroSuperScalarRRAInfo->elementTK = elementTK;
  char *rraOffsetListChar = new char[termCount*sizeof(int)];
  xxeZeroSuperScalarRRAInfo->rraOffsetList = (int *)rraOffsetListChar;
  xxeZeroSuperScalarRRAInfo->rraIndex = rraIndex;
  xxeZeroSuperScalarRRAInfo->termCount = termCount;
  xxeZeroSuperScalarRRAInfo->vectorFlag = vectorFlag;

  // keep track of allocations for xxe garbage collection
  localrc = storeStorage(rraOffsetListChar);  // for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a zeroMemset element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = zeroMemset;
  stream[count].predicateBitField = predicateBitField;
  ZeroMemsetInfo *xxeZeroMemsetInfo =
    (ZeroMemsetInfo *)&(stream[count]);
  xxeZeroMemsetInfo->buffer = buffer;
  xxeZeroMemsetInfo->byteCount = byteCount;
  xxeZeroMemsetInfo->vectorFlag = vectorFlag;
  xxeZeroMemsetInfo->indirectionFlag = indirectionFlag;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a zeroMemsetRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = zeroMemsetRRA;
  stream[count].predicateBitField = predicateBitField;
  ZeroMemsetRRAInfo *xxeZeroMemsetRRAInfo =
    (ZeroMemsetRRAInfo *)&(stream[count]);
  xxeZeroMemsetRRAInfo->byteCount = byteCount;
  xxeZeroMemsetRRAInfo->rraIndex = rraIndex;
  xxeZeroMemsetRRAInfo->vectorFlag = vectorFlag;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a productSumScalarRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = productSumScalarRRA;
  stream[count].predicateBitField = predicateBitField;
  ProductSumScalarRRAInfo *xxeProductSumScalarRRAInfo =
    (ProductSumScalarRRAInfo *)&(stream[count]);
  xxeProductSumScalarRRAInfo->elementTK = elementTK;
  xxeProductSumScalarRRAInfo->valueTK = valueTK;
  xxeProductSumScalarRRAInfo->factorTK = factorTK;
  xxeProductSumScalarRRAInfo->rraOffset = rraOffset;
  xxeProductSumScalarRRAInfo->factor = factor;
  xxeProductSumScalarRRAInfo->value = value;
  xxeProductSumScalarRRAInfo->rraIndex = rraIndex;

  // bump up element count, this may move entire stream to new memory location
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
//  Append a SumSuperScalarDstRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = sumSuperScalarDstRRA;
  stream[count].predicateBitField = predicateBitField;
  SumSuperScalarDstRRAInfo *xxeSumSuperScalarDstRRAInfo =
    (SumSuperScalarDstRRAInfo *)&(stream[count]);
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
  localrc = storeStorage(rraOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a sumSuperScalarListDstRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = sumSuperScalarListDstRRA;
  stream[count].predicateBitField = predicateBitField;
  SumSuperScalarListDstRRAInfo *xxeSumSuperScalarListDstRRAInfo =
    (SumSuperScalarListDstRRAInfo *)&(stream[count]);
  xxeSumSuperScalarListDstRRAInfo->elementTK = elementTK;
  xxeSumSuperScalarListDstRRAInfo->valueTK = valueTK;
  char *rraIndexListChar = new char[rraIndexList.size()*sizeof(int)];
  xxeSumSuperScalarListDstRRAInfo->rraIndexList =
    (int *)rraIndexListChar;
  for (int i=0; i<rraIndexList.size(); i++){
    xxeSumSuperScalarListDstRRAInfo->rraIndexList[i] =
      rraIndexList[i];
  }
  xxeSumSuperScalarListDstRRAInfo->termCount = termCount;
  char *valueBaseListChar = new char[valueBaseList.size()*sizeof(void *)];
  xxeSumSuperScalarListDstRRAInfo->valueBaseList =
    (void **)valueBaseListChar;
  for (int i=0; i<valueBaseList.size(); i++){
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
  localrc = storeStorage(rraIndexListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueBaseListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueBaseListResolveChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(rraOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(baseListIndexListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a productSumSuperScalarDstRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = productSumSuperScalarDstRRA;
  stream[count].predicateBitField = predicateBitField;
  ProductSumSuperScalarDstRRAInfo *xxeProductSumSuperScalarDstRRAInfo =
    (ProductSumSuperScalarDstRRAInfo *)&(stream[count]);
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
  char *factorListChar = new char[termCount*sizeof(void *)];
  xxeProductSumSuperScalarDstRRAInfo->factorList = (void **)factorListChar;
  char *valueOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarDstRRAInfo->valueOffsetList =
    (int *)valueOffsetListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeStorage(rraOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(factorListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a productSumSuperScalarListDstRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = productSumSuperScalarListDstRRA;
  stream[count].predicateBitField = predicateBitField;
  ProductSumSuperScalarListDstRRAInfo *xxeProductSumSuperScalarListDstRRAInfo =
    (ProductSumSuperScalarListDstRRAInfo *)&(stream[count]);
  xxeProductSumSuperScalarListDstRRAInfo->elementTK = elementTK;
  xxeProductSumSuperScalarListDstRRAInfo->valueTK = valueTK;
  xxeProductSumSuperScalarListDstRRAInfo->factorTK = factorTK;
  char *rraIndexListChar = new char[rraIndexList.size()*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->rraIndexList =
    (int *)rraIndexListChar;
  for (int i=0; i<rraIndexList.size(); i++){
    xxeProductSumSuperScalarListDstRRAInfo->rraIndexList[i] =
      rraIndexList[i];
  }
  xxeProductSumSuperScalarListDstRRAInfo->termCount = termCount;
  char *valueBaseListChar = new char[valueBaseList.size()*sizeof(void *)];
  xxeProductSumSuperScalarListDstRRAInfo->valueBaseList =
    (void **)valueBaseListChar;
  for (int i=0; i<valueBaseList.size(); i++){
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
  char *factorListChar = new char[termCount*sizeof(void *)];
  xxeProductSumSuperScalarListDstRRAInfo->factorList = (void **)factorListChar;
  char *valueOffsetListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->valueOffsetList =
    (int *)valueOffsetListChar;
  char *baseListIndexListChar = new char[termCount*sizeof(int)];
  xxeProductSumSuperScalarListDstRRAInfo->baseListIndexList =
    (int *)baseListIndexListChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeStorage(rraIndexListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueBaseListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueBaseListResolveChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(rraOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(factorListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(valueOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(baseListIndexListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a productSumSuperScalarSrcRRA element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = productSumSuperScalarSrcRRA;
  stream[count].predicateBitField = predicateBitField;
  ProductSumSuperScalarSrcRRAInfo *xxeProductSumSuperScalarSrcRRAInfo =
    (ProductSumSuperScalarSrcRRAInfo *)&(stream[count]);
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
  localrc = storeStorage(rraOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(factorListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(elementOffsetListChar);// for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a waitOnIndex element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = waitOnIndex;
  stream[count].predicateBitField = predicateBitField;
  WaitOnIndexInfo *xxeWaitOnIndexInfo =
    (WaitOnIndexInfo *)&(stream[count]);
  xxeWaitOnIndexInfo->index = index;

  // bump up element count, this may move entire stream to new memory location
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
//  Append a testOnIndex element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = testOnIndex;
  stream[count].predicateBitField = predicateBitField;
  TestOnIndexInfo *xxeTestOnIndexInfo =
    (TestOnIndexInfo *)&(stream[count]);
  xxeTestOnIndexInfo->index = index;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a waitOnAnyIndexSub element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = waitOnAnyIndexSub;
  stream[count].predicateBitField = predicateBitField;
  WaitOnAnyIndexSubInfo *xxeWaitOnAnyIndexSubInfo =
    (WaitOnAnyIndexSubInfo *)&(stream[count]);
  xxeWaitOnAnyIndexSubInfo->count = countArg;
  char *xxeChar = new char[countArg*sizeof(XXE *)];
  xxeWaitOnAnyIndexSubInfo->xxe = (XXE **)xxeChar;
  char *indexChar = new char[countArg*sizeof(int)];
  xxeWaitOnAnyIndexSubInfo->index = (int *)indexChar;
  char *completeFlagChar = new char[countArg*sizeof(int)];
  xxeWaitOnAnyIndexSubInfo->completeFlag = (int *)completeFlagChar;

  // keep track of allocations for xxe garbage collection
  localrc = storeStorage(xxeChar);  // for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(indexChar);  // for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  localrc = storeStorage(completeFlagChar);  // for xxe garb. coll.
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a waitOnAllSendnb element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = waitOnAllSendnb;
  stream[count].predicateBitField = predicateBitField;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append an xxeSub at the end of the XXE stream that is executed depending
//  on test conditional
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = waitOnIndexSub;
  stream[count].predicateBitField = predicateBitField;
  WaitOnIndexSubInfo *waitOnIndexSubInfo =
    (WaitOnIndexSubInfo *)&(stream[count]);
  waitOnIndexSubInfo->xxe = xxe;
  waitOnIndexSubInfo->rraShift = rraShift;
  waitOnIndexSubInfo->vectorLengthShift = vectorLengthShift;
  waitOnIndexSubInfo->index = index;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append an xxeSub at the end of the XXE stream that is executed depending
//  on test conditional
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  stream[count].opId = testOnIndexSub;
  stream[count].predicateBitField = predicateBitField;
  TestOnIndexSubInfo *testOnIndexSubInfo =
    (TestOnIndexSubInfo *)&(stream[count]);
  testOnIndexSubInfo->xxe = xxe;
  testOnIndexSubInfo->rraShift = rraShift;
  testOnIndexSubInfo->vectorLengthShift = vectorLengthShift;
  testOnIndexSubInfo->index = index;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a cancelIndex element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = cancelIndex;
  stream[count].predicateBitField = predicateBitField;
  CancelIndexInfo *xxeCancelIndexInfo =
    (CancelIndexInfo *)&(stream[count]);
  xxeCancelIndexInfo->index = index;
  
  // bump up element count, this may move entire stream to new memory location 
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
//  Append a profileMessage element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = profileMessage;
  stream[count].predicateBitField = predicateBitField;
  ProfileMessageInfo *xxeProfileMessageInfo =
    (ProfileMessageInfo *)&(stream[count]);
  int stringLen = strlen(messageString);
  xxeProfileMessageInfo->messageString = new char[stringLen+1];
  strcpy(xxeProfileMessageInfo->messageString, messageString);
  
  // keep track of strings for xxe garbage collection
  localrc = storeStorage(xxeProfileMessageInfo->messageString);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
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
//  Append a Message element at the end of the XXE stream.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  stream[count].opId = message;
  stream[count].predicateBitField = predicateBitField;
  MessageInfo *xxeMessageInfo =
    (MessageInfo *)&(stream[count]);
  int stringLen = strlen(messageString);
  xxeMessageInfo->messageString = new char[stringLen+1];
  strcpy(xxeMessageInfo->messageString, messageString);

  // keep track of strings for xxe garbage collection
  localrc = storeStorage(xxeMessageInfo->messageString);
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;
  
  // bump up element count, this may move entire stream to new memory location
  localrc = incCount();
  if (ESMC_LogDefault.MsgFoundError(localrc,
    ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return rc;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


} // namespace ESMCI
