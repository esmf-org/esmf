// $Id: ESMC_DELayout.C,v 1.61 2007/07/18 20:43:40 theurich Exp $
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
#define ESMC_FILENAME "ESMC_DELayout.C"
//==============================================================================
//
// ESMC DELayout method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DELayout methods declared
// in the companion file ESMC_DELayout.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_DELayout.h"

// include higher level, 3rd party or system headers
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMC_Start.h"
#include "ESMC_Base.h" 
#include "ESMC_VM.h"

// LogErr headers
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_DELayout.C,v 1.61 2007/07/18 20:43:40 theurich Exp $";
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
  ESMC_DePinFlag *dePinFlag,// (in) type of resources DEs are pinned to
  VM *vm,                   // (in) VM context
  int *rc){                 // (out) return code
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
   
  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // allocate the new DELayout object and construct the inside
  DELayout *delayout;
  try{
    delayout = new DELayout;
    status = delayout->construct(vm, dePinFlag, petMap, petMapCount);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      delete delayout;
      delayout = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new DELayout.", rc);  
     return ESMC_NULL_POINTER;
  }
  
  // return successfully
  *rc = ESMF_SUCCESS;
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
  InterfaceInt *deGrouping,     // (in) deGrouping vector
  ESMC_DePinFlag *dePinFlag,    // (in) type of resources DEs are pinned to
  InterfaceInt *petListArg,     // (in) list of PETs to be used in delayout
  VM *vm,                       // (in) VM context
  int *rc){                     // (out) return code
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int status;                 // local error status
  
  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // There is only one DELayoutConstruct() method - it requires a petMap
  // in order to construct the inside of a DELayout object. The task of 
  // this DELayoutCreate() function is to build a petMap according to the
  // provided input and then call DELayoutConstruct() with this petMap.
  int *petMap;
  int petMapCount;
  
  // by default use the currentVM for vm
  if (vm == ESMC_NULL_POINTER){
    vm = VM::getCurrent(&status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return ESMC_NULL_POINTER;
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
  if (deGrouping != ESMC_NULL_POINTER && deGrouping->extent[0] > 0){
    deGroupingCount = deGrouping->extent[0];
    deGroupingFlag = 1;   // set
    if (deGroupingCount != deCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                "- Size of deGrouping does not match deCount", rc);
      return ESMC_NULL_POINTER;
    }
  }
  
  // check petList input
  int petListDeleteFlag = 0;  // reset
  int petListFlag = 0;        // reset
  int petListCount = 0;
  int *petList;
  if (petListArg != ESMC_NULL_POINTER && petListArg->extent[0] > 0){
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
  int petMapDeleteFlag = 0; // reset
  if (!(deCountFlag | deGroupingFlag | petListFlag)){
    // the trivial case: default DELayout
    petMap = ESMC_NULL_POINTER;
    petMapCount = 0;
  }else{
    // need a real petMap
    petMapDeleteFlag = 1; // set
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
  }
  
  // start cleanup
  if (petListDeleteFlag) delete [] petList;
#if 0  
  if (deStrideBlockDeleteFlag){
    for (int i=0; i<deStrideBlockCount; i++)
      delete [] deStrideBlock[i];
    delete [] deStrideBlock;
  }
#endif
  
  // allocate the new DELayout object and construct the inside
  DELayout *delayout;
  try{
    delayout = new DELayout;
    status = delayout->construct(vm, dePinFlag, petMap, petMapCount);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc)){
      if (petMapDeleteFlag) delete [] petMap;
      delete delayout;
      delayout = ESMC_NULL_POINTER;
      return ESMC_NULL_POINTER;
    }
  }catch(...){
     // allocation error
     ESMC_LogDefault.ESMC_LogMsgAllocError("for new DELayout.", rc);
     if (petMapDeleteFlag) delete [] petMap;
     return ESMC_NULL_POINTER;
  }
  
  // final cleanup
  if (petMapDeleteFlag) delete [] petMap;
  
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
  DELayout *layout;
  // deal with optional variables
  ESMC_Logical cyclic = ESMF_FALSE;
  if (cyclic_opt != ESMC_NULL_POINTER)
    cyclic = *cyclic_opt;
  
  // CAUTION: todo: THIS IS A _NASTY_ HACK to make things happy on higher levels
  // that rely on DELayout to be _always 2D! Here I promote a 1D layout request
  // to 2D: N x 1. I write a message to LogErr to make people aware of this!!!
  if (ndim==0){
    // ESMC_LogDefault.ESMC_LogWrite("Promoting 1D DELayout to 2D",
    //   ESMC_LOG_WARN);
    ndim = 2;
    deCountArg = new int[2];  // TODO: this will leave a memory leak
    deCountArg[0] = vm.getNpets();
    deCountArg[1] = 1;
  }
  if (ndim==1){
    // ESMC_LogDefault.ESMC_LogWrite("Promoting 1D DELayout to 2D",
    //  ESMC_LOG_WARN);
    ndim = 2;
    int firstDEdim = deCountArg[0];
    deCountArg = new int[2];  // TODO: this will leave a memory leak
    deCountArg[0] = firstDEdim;
    deCountArg[1] = 1;
  }
  

  // decide whether this is a 1D or an ND layout
  if (ndim==0){
    // special case of a 1D layout where deCount will equal petCount
    try {
      layout = new DELayout;
      *rc = layout->construct1D(vm, 0, DEtoPET, len, cyclic);
      return(layout);
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new DELayout.", rc);  
      return(ESMC_NULL_POINTER);
    }
  }else if(ndim==1){
    try {
      layout = new DELayout;
      *rc = layout->construct1D(vm, *deCountArg, DEtoPET, len,
        cyclic);
      return(layout);
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new DELayout.", rc);  
      return(ESMC_NULL_POINTER);
    }
  }else{
    try {
      layout = new DELayout;
      *rc = layout->constructND(vm, deCountArg, ndim, DEtoPET, len,
        cyclic);
      return(layout);
    }
    catch (...) {
      // LogErr catches the allocation error
      ESMC_LogDefault.ESMC_LogMsgAllocError("for new DELayout.", rc);  
      return(ESMC_NULL_POINTER);
    }
  }
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
//    int error return code
//
// !ARGUMENTS:
//
  DELayout **delayout){  // in - DELayout to destroy
//
// !DESCRIPTION:
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

  // return with errors for NULL pointer
  if (delayout == ESMC_NULL_POINTER || *delayout == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to DELayout", rc);
    return localrc;
  }

  // destruct and delete DELayout object
  status = (*delayout)->destruct();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  delete *delayout;
  *delayout = ESMC_NULL_POINTER;
  
  // return successfully
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
//
// !ARGUMENTS:
//
  VM *vmArg,                   // (in) VM context
  ESMC_DePinFlag *dePinFlagArg,// (in) type of resources DEs are pinned to
  int *petMap,                 // (in) pointer to petMap list
  int petMapCount){            // (in) number of element in petMap
//
// !DESCRIPTION:
//    Construct the internal information structure of an ESMC\_DELayout object.
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

  // by default use the currentVM for vm
  if (vmArg == ESMC_NULL_POINTER){
    vmArg = VM::getCurrent(&status);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, rc))
    return localrc;
  }

  // query the VM for localPet and petCount
  int localPet, petCount, localVas;
  vmArg->get(&localPet, &petCount, NULL, NULL, NULL);
  localVas = vmArg->getVas(localPet);
  
  // by default pin DEs to PETs
  if (dePinFlagArg == ESMC_NULL_POINTER)
    dePinFlag = ESMF_DE_PIN_PET;
  else
    dePinFlag = *dePinFlagArg;
  
  // by default use a sequential 1-to-1 petMap
  int petMapDeleteFlag = 0; // reset
  if (petMap == ESMC_NULL_POINTER || petMapCount == 0){
    petMapDeleteFlag = 1; // set
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
        "- DE to PET mapping is invalid", rc);
      delete [] deInfoList;
      delete [] petFlag;
      return localrc;
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
  localDeList = new int[localDeCount];  // allocate space to hold local de ids
  int j=0;
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].pet == localPet){
      localDeList[j]=i;
      ++j;
    }
    
  // fill VAS-local part of layout object
  vasLocalDeCount = 0;               // reset vas-local de count
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].vas == localVas) ++vasLocalDeCount;
  vasLocalDeList = new int[vasLocalDeCount];  // vas-local de id list
  j=0;
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].vas == localVas){
      vasLocalDeList[j]=i;
      ++j;
    }
    
  // setup work queue
  localServiceOfferCount = new int[vasLocalDeCount];
  serviceMutexFlag = new int[vasLocalDeCount];
  for (int i=0; i<vasLocalDeCount; i++){
    localServiceOfferCount[i] = 0;  // reset
    serviceMutexFlag[i] = 0;        // reset
  }
  serviceMutex = new vmk_ipmutex*[vasLocalDeCount];
  serviceOfferMutex = new vmk_ipmutex*[vasLocalDeCount];
  for (int i=0; i<vasLocalDeCount; i++)
    serviceOfferMutex[i] = vm->vmk_ipmutexallocate();  // obtain shared mutex
  if (vasLocalDeCount)  // don't use mutex if it's not there
    vm->vmk_ipmutexlock(serviceOfferMutex[0]);   // lock mutex
  int firstFlag;
  maxServiceOfferCount = (int *)
    vm->vmk_ipshmallocate(4*vasLocalDeCount*sizeof(int), &firstFlag);
  if (firstFlag)
    for (int i=0; i<vasLocalDeCount; i++)
      maxServiceOfferCount[4*i] = 0; // reset:  step 4, better shared mem perf
  for (int i=0; i<vasLocalDeCount; i++)
    serviceMutex[i] = vm->vmk_ipmutexallocate();  // obtain shared mutex
  if (vasLocalDeCount)  // don't use mutex if it's not there
    vm->vmk_ipmutexunlock(serviceOfferMutex[0]); // unlock mutex
  
  // return successfully
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new DELayout
//     - deprecated
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
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
    ESMC_LogDefault.ESMC_LogWrite("A layout without 1:1 DE:PET mapping was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Issue warning if this is not logically rectangular
  // TODO: remove this warning when non logRect layouts o.k.
  if (logRectFlag == ESMF_FALSE){
    ESMC_LogDefault.ESMC_LogWrite("A non logRect layout was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Fill local part of layout object
  int mypet = vm->getMypet();    // get my PET id
  ESMC_DELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct VAS.
  for (int i=0; i<deCount; i++)
    deInfoList[i].vas = vm->getVas(deInfoList[i].pet);
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
//
//
// !DESCRIPTION:
//    Construct the internal information structure in a new DELayout 
//     - deprecated
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
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
    ESMC_LogDefault.ESMC_LogWrite("A layout without 1:1 DE:PET mapping was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Issue warning if this is not logically rectangular
  // TODO: remove this warning when non logRect layouts o.k.
  if (logRectFlag == ESMF_FALSE){
    ESMC_LogDefault.ESMC_LogWrite("A non logRect layout was"
      " created! This may cause problems in higher layers of ESMF!", 
      ESMC_LOG_WARN);
  }
  // Fill local part of layout object
  int mypet = vm->getMypet();    // get my PET id
  ESMC_DELayoutFillLocal(mypet);
  // Now that the layout is pretty much set up it is time to go through once
  // more to set the correct VAS.
  for (int i=0; i<deCount; i++)
    deInfoList[i].vas = vm->getVas(deInfoList[i].pet);
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::destruct()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::destruct
//
// !INTERFACE:
int DELayout::destruct(void){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Destruct the internal information structure of an DELayout object.
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
  delete [] localDeList;
  delete [] deList;

  if (!oldstyle){
    // this is only for newstyle DELayouts
    delete [] vasLocalDeList;
    delete [] localServiceOfferCount;
    delete [] serviceMutexFlag;
    vm->vmk_ipshmdeallocate(maxServiceOfferCount);
    for (int i=0; i<vasLocalDeCount; i++)
      vm->vmk_ipmutexdeallocate(serviceOfferMutex[i]);
    delete [] serviceOfferMutex;
    for (int i=0; i<vasLocalDeCount; i++)
      vm->vmk_ipmutexdeallocate(serviceMutex[i]);
    delete [] serviceMutex;
  }

  // return successfully
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------

   //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::get()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::get
//
// !INTERFACE:
int DELayout::get(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  VM **vmArg,                 // out - VM context
  int  *deCountArg,           // out - Total number of DEs
  int  *petMap,               // out - list that maps each DE against a PET
  int  petMapCount,           // in  - number of elements in petMap
  int  *vasMap,               // out - list that maps each DE against a VAS
  int  vasMapCount,           // in  - number of elements in vasMap
  ESMC_Logical *oneToOneFlagArg, // out - 1-to-1 layout flag
  ESMC_DePinFlag *dePinFlagArg,  // out - resources DEs are pinned to
  int  *localDeCountArg,      // out - number of local DEs
  int  *localDeListArg,       // out - list of local DEs
  int  localDeListCount,      // in  - number of elements in localDeListArg
  int  *vasLocalDeCountArg,   // out - number of vas-local DEs
  int  *vasLocalDeListArg,    // out - list of vas-local DEs
  int  vasLocalDeListCount    // in  - number of elements in vasLocalDeListArg
  )const{    
//
// !DESCRIPTION:
//    Get information about a DELayout object
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

  if (vmArg != NULL)
    *vmArg = vm;

  if (deCountArg != ESMC_NULL_POINTER)
    *deCountArg = deCount;

  if (petMapCount == deCount){
    for (int i=0; i<deCount; i++)
      petMap[i] = deInfoList[i].pet;
  }else if (petMapCount != 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- Size of petMap does not match deCount", rc);
    return localrc;
  }  

  if (vasMapCount == deCount){
    for (int i=0; i<deCount; i++)
      vasMap[i] = deInfoList[i].vas;
  }else if (vasMapCount != 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- Size of vasMap does not match deCount", rc);
    return localrc;
  }  

  if (oneToOneFlagArg != ESMC_NULL_POINTER)
    *oneToOneFlagArg = oneToOneFlag;

  if (dePinFlagArg != ESMC_NULL_POINTER){
    // TODO: once OLDSTYLE DELayout goes remove this check!
    if (oldstyle){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- OLDSTYLE DELayout does not support this query", rc);
      return localrc;
    }else
      *dePinFlagArg = dePinFlag;
  }
  
  if (localDeCountArg != ESMC_NULL_POINTER)
    *localDeCountArg = localDeCount;

  if (localDeListCount == localDeCount){
    for (int i=0; i<localDeCount; i++)
      localDeListArg[i] = localDeList[i];
  }else if (localDeListCount != 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- Size of localDeListArg does not match localDeCount", rc);
    return localrc;
  }  

  if (vasLocalDeCountArg != ESMC_NULL_POINTER)
    // TODO: once OLDSTYLE DELayout goes remove this check!
    if (oldstyle){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- OLDSTYLE DELayout does not support this query", rc);
      return localrc;
    }else
      *vasLocalDeCountArg = vasLocalDeCount;

  if (vasLocalDeListCount == vasLocalDeCount){
    // TODO: once OLDSTYLE DELayout goes remove this check!
    if (oldstyle && vasLocalDeCount!=0){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- OLDSTYLE DELayout does not support this query", rc);
      return localrc;
    }else
      for (int i=0; i<vasLocalDeCount; i++)
        vasLocalDeListArg[i] = vasLocalDeList[i];
  }else if (vasLocalDeListCount != 0){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
      "- Size of vasLocalDeListArg does not match vasLocalDeCount", rc);
    return localrc;
  }  

  // return successfully
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
//
// !ARGUMENTS:
//
  int  *deCountArg,           // out - Total number of DEs
  int  *ndim,                 // out - Number of dimensions in coordinate tuple
  int  *localDeCountArg,      // out - number of DEs for my PET instance
  int  *localDeListArg,       // out - list DEs for my PET instance
  int  len_localDeList,       // in  - number of elements in localDeListArg
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
  // local vars
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status

  // initialize return code; assume routine not implemented
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;

  if (deCountArg != ESMC_NULL_POINTER)
    *deCountArg = deCount;
  
  if (ndim != ESMC_NULL_POINTER){
    if (!oldstyle){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- only OLDSTYLE DELayouts support this query", rc);
      return localrc;
    }else
      *ndim = this->ndim;
  }
  
  if (localDeCountArg != ESMC_NULL_POINTER)
    *localDeCountArg = localDeCount;
  
  if (len_localDeList >= localDeCount)
    for (int i=0; i<localDeCount; i++)
      localDeListArg[i] = localDeList[i];
  
  if (localDe != ESMC_NULL_POINTER){
    if (!oldstyle){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- only OLDSTYLE DELayouts support this query", rc);
      return localrc;
    }else{
      if (localDeCount >= 1)  // at least 1 DE on this PET -> return 1st
        *localDe = localDeList[0];
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
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- only OLDSTYLE DELayouts support this query", rc);
      return localrc;
    }else
      *logRectFlag = this->logRectFlag;
  }
  
  if (len_deCountPerDim >= this->ndim){
    if (!oldstyle){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- only OLDSTYLE DELayouts support this query", rc);
      return localrc;
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
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::getVM()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::getVM
//
// !INTERFACE:
int DELayout::getVM(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//
  VM **vmArg                // out - VM this layout is defined on
  )const{ 
//
// !DESCRIPTION:
//    Get VM of this DELayout object
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  *vmArg = vm;
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::getDELocalInfo()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::getDELocalInfo
//
// !INTERFACE:
int DELayout::getDELocalInfo(
//
// !RETURN VALUE:
//    int error return code
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
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
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
  localrc = ESMF_SUCCESS;
  return localrc;
}
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
//    int error return code
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
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
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
  delete [] tempMatchList;
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
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
  int localrc;

  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

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
  delete [] tempMatchList;
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::print()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::rrint
//
// !INTERFACE:
int DELayout::print(){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Print details of DELayout object 
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  // print info about the ESMC_DELayout object
  printf("--- ESMC_DELayoutPrint start ---\n");
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
      printf("  localDeList[%d]=%d\n", i, localDeList[i]);
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
    printf("dePinFlag = ");
    if (dePinFlag == ESMF_DE_PIN_PET)
      printf("ESMF_DE_PIN_PET\n");
    else if (dePinFlag == ESMF_DE_PIN_VAS)
      printf("ESMF_DE_PIN_VAS\n");
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
      printf("  localDeList[%d]=%d\n", i, localDeList[i]);
    printf("--- VAS-local DELayout section ---\n");
    printf("vasLocalDeCount=%d\n", vasLocalDeCount);
    for (int i=0; i<vasLocalDeCount; i++)
      printf("  vasLocalDeList[%d]=%d\n", i, vasLocalDeList[i]);
  }
  printf("--- ESMC_DELayoutPrint end ---\n");

  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::DELayout::validate()"
//BOPI
// !IROUTINE:  ESMCI::DELayout::validate
//
// !INTERFACE:
int DELayout::validate(){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Validate details of DELayout object 
//
//EOPI
//-----------------------------------------------------------------------------
  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // validate info about the ESMC_DELayout object
  //printf("--- ESMC_DELayoutValidate start ---\n");
  //printf("myvm = %p\n", myvm);
  //printf("ndes = %d\n", ndes);
  //for (int i=0; i<ndes; i++){
  //  printf("  des[%d]: deid=%d, petid=%d, pid=%d, nconnect=%d\n", i, 
  //    des[i].deid, des[i].petid, des[i].pid, des[i].nconnect);
  //  for (int j=0; j<des[i].nconnect; j++)
  //    printf("      connect_de[%d]=%d, weight=%d\n", j, des[i].connect_de[j],
  //      des[i].connect_w[j]);
  //}
  //printf("nmydes=%d\n", nmydes);
  //for (int i=0; i<nmydes; i++)
  //  printf("  mydes[%d]=%d\n", i, mydes[i]);
  //printf("ndim = %d\n", ndim);
  //for (int i=0; i<ndes; i++){
  //  printf("[%d]: ", i);
  //  int j;
  //  for (j=0; j<ndim-1; j++)
  //    printf("%d, ", des[i].coord[j]);
  //  printf("%d\n", des[i].coord[j]);
  // }
  // printf("--- ESMC_DELayoutValidate end ---\n");
 
  // for now, validate at least the base object
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }
  // rc = this->ESMC_Validate();

  // This is an incomplete success.
     rc = ESMF_SUCCESS;
  return rc;
}
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
//    {\tt ESMF\_SUCCESS} or error code on failure.
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length; realloc'd here if needed
  int *offset) {         // inout - original offset, updated to point 
                             //  to first free byte after current obj info
//
// !DESCRIPTION:
//    Turn info in delayout class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  int fixedpart, nbytes, rc;
  int i, j;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  VM **vp;
  ESMC_DePinFlag *dp;
  de_type *dep;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // TODO: we cannot reallocate from C++ if the original buffer is
  //  allocated on the f90 side.  change the code to make the allocate
  //  happen in C++; then this will be fine.  (for now make sure buffer
  //  is always big enough so realloc is not needed.)
  fixedpart = sizeof(DELayout);
  if ((*length - *offset) < fixedpart) {
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
    "Buffer too short to add a DELayout object", &rc);
    return ESMF_FAILURE; 
  }

  // first set the base part of the object
  rc = this->ESMC_Base::ESMC_Serialize(buffer, length, offset);

  cp = (char *)(buffer + *offset);
  
  // TODO: for now, send NULL as the vm, because i do not know how to
  // serialize a VM.   probably sending an integer VM ID number would be
  // what we want in the long run.
  vp = (VM **)cp;   
  *vp++ = NULL;     

  ip = (int *)vp;
  *ip++ = deCount;
  *ip++ = oldstyle;
  if (oldstyle){
    // ndim must be available before decoding the next loop, so it has
    // to be sent now.
    *ip++ = ndim;
  }
  if (!oldstyle){
    dp = (ESMC_DePinFlag *)ip;
    *dp++ = dePinFlag;
    ip = (int *)dp;
  }

  for (i=0, dep=deInfoList; i<deCount; i++, dep++) {
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
  
  // this has to come before dims, since they are not allocated unless
  // logRectFlag is true.
  lp = (ESMC_Logical *)ip;
  *lp++ = oneToOneFlag;
  if (oldstyle)
    *lp++ = logRectFlag;
  
  ip = (int *)lp;
  if (oldstyle){
    if (logRectFlag == ESMF_TRUE)
      for (i=0; i<ndim; i++) 
        *ip++ = dims[i];
  }

  cp = (char *)ip;

  *offset = (cp - buffer);
  
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
  DELayout *a = new DELayout;
  int fixedpart, nbytes, rc;
  int i, j;
  char *cp;
  int *ip;
  ESMC_Logical *lp;
  VM **vp;
  ESMC_DePinFlag *dp;
  de_type *dep;

  // Initialize return code; assume routine not implemented
  rc = ESMC_RC_NOT_IMPL;

  // first get the base part of the object
  rc = a->ESMC_Base::ESMC_Deserialize(buffer, offset);

  // now the rest
  cp = (char *)(buffer + *offset);
  
  vp = (VM **)cp;
  a->vm = *vp++; 

  ip = (int *)vp;
  a->deCount = *ip++;
  a->oldstyle = *ip++;

  if (a->oldstyle){
    // ndim must be known before this loop.
    a->ndim = *ip++;
  }
  if (!a->oldstyle){
    dp = (ESMC_DePinFlag *)ip;
    a->dePinFlag = *dp++;
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
  a->localDeList = new int[a->localDeCount];
  a->deList = new int[a->deCount];
  for (int i=0; i<a->deCount; i++)
    a->deList[i] = -1;                         // indicate not a local DE
  a->vasLocalDeCount = 0;  // proxy objects don't have local DEs
  a->vasLocalDeList = new int[a->vasLocalDeCount];

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
DELayoutServiceReply DELayout::serviceOffer(
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
//    Calling PET offers service for {\tt de} in {\tt ESMC_DELayout}. The 
//    offer is either accepted or denied.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int status;                 // local error status
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;
  
  // initialize the reply
  DELayoutServiceReply reply = DELAYOUT_SERVICE_DENY; // reset

  int localPet = vm->getMypet();
  int localVas = vm->getVas(localPet);
  
  // DE to PET pinning is more restrictive -> check first
  if (dePinFlag == ESMF_DE_PIN_PET){
    // search for de in localDeList
    int i;
    for (i=0; i<localDeCount; i++)
      if (localDeList[i] == de) break;
    if (i==localDeCount){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_ARG_WRONG;
//      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
//        "- Specified DE is not in localDeList", rc);
      return reply;
    }
  }
  int ii;
  // search for de in vasLocalDeList
  for (ii=0; ii<vasLocalDeCount; ii++)
    if (vasLocalDeList[ii] == de) break;
  if (ii==vasLocalDeCount){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_ARG_WRONG;
//    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
//      "- Specified DE is not in vasLocalDeList", rc);
    return reply;
  }
  
  // ...de was found in local list
  ++localServiceOfferCount[ii];
  vm->vmk_ipmutexlock(serviceOfferMutex[ii]);   // lock mutex
  if (localServiceOfferCount[ii] > maxServiceOfferCount[4*ii]){
    reply = DELAYOUT_SERVICE_ACCEPT; // accept this PET's service offer
    ++maxServiceOfferCount[4*ii];
  }
  vm->vmk_ipmutexunlock(serviceOfferMutex[ii]); // unlock mutex
  
  if (reply==DELAYOUT_SERVICE_ACCEPT){
    vm->vmk_ipmutexlock(serviceMutex[ii]);  // lock service mutex
    serviceMutexFlag[ii] = 1;               // set
  }
    
//  if (reply==ESMC_DELAYOUT_SERVICE_ACCEPT)
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
//    int error return code
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
  int localrc;                // automatic variable for local return code
  int *rc = &localrc;         // pointer to localrc
  int status;                 // local error status
  status = ESMC_RC_NOT_IMPL;
  if (rc!=NULL)
    *rc = ESMC_RC_NOT_IMPL;

  int localPet = vm->getMypet();
  int localVas = vm->getVas(localPet);
  
  // DE to PET pinning is more restrictive -> check first
  if (dePinFlag == ESMF_DE_PIN_PET){
    // search for de in localDeList
    int i;
    for (i=0; i<localDeCount; i++)
      if (localDeList[i] == de) break;
    if (i==localDeCount){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_ARG_WRONG;
//      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
//        "- Specified DE is not in localDeList", rc);
      return localrc;
    }
  }
  int ii;
  // search for de in vasLocalDeList
  for (ii=0; ii<vasLocalDeCount; ii++)
    if (vasLocalDeList[ii] == de) break;
  if (ii==vasLocalDeCount){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_ARG_WRONG;
//    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
//      "- Specified DE is not in vasLocalDeList", rc);
    return localrc;
  }
  
  // ...de was found in local list
  if (!serviceMutexFlag[ii]){
//TODO: enable LogErr once it is thread-safe
*rc=ESMC_RC_NOT_VALID;
//    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID,
//      "- PET does not hold service mutex for specified", rc);
    return localrc;
  }
  
  // ...pet holds service mutex for de   
  vm->vmk_ipmutexunlock(serviceMutex[ii]);  // unlock service mutex
  serviceMutexFlag[ii] = 0;                 // reset
  
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
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
  // local vars
  int localrc;                // automatic variable for local return code

  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;

  int *rc = &localrc;         // pointer to localrc
  // ensure this is a 1-to-1 delayout, if not bail out
  if (oneToOneFlag != ESMF_TRUE){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
      "- Can only handle 1-to-1 DELayouts", rc);
    return localrc; // bail out
  }
  int mypet = vm->getMypet();
  int srcpet = deInfoList[srcDE].pet;      // PETid where srcDE lives
  int destpet = deInfoList[destDE].pet;    // PETid where destDE lives
  if (srcpet==mypet && destpet==mypet){
    // srcDE and destDE are on my PET
    memcpy(destdata, srcdata, blen);
  }else if (srcpet==mypet){
    // srcDE is on my PET, but destDE is on another PET
    vm->vmk_send(srcdata, blen, destpet);
  }else if (destpet==mypet){
    // destDE is on my PET, but srcDE is on another PET
    vm->vmk_recv(destdata, blen, srcpet);
  }
  // return successfully
  localrc = ESMF_SUCCESS;
  return localrc;
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
//    int error return code
//
// !ARGUMENTS:
//
  void *srcdata,    // input array
  void *destdata,   // output array
  int len,          // size in elements that need to be copied from src to dest
  ESMC_TypeKind dtk,// data type kind
  int srcDE,        // input DE
  int destDE        // output DE
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;

  // Initialize return code; assume routine not initialized
  localrc = ESMC_RC_NOT_IMPL;

  int blen = len * ESMC_TypeKindSize(dtk);
  return ESMC_DELayoutCopy(srcdata, destdata, blen, srcDE, destDE);
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
//    int error return code
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
//    int error return code
//
// !ARGUMENTS:
//
  void *srcData1,     // input array
  void *srcData2,     // input array
  void *dstData1,     // output array
  void *dstData2,     // output array
  int len1,           // size in elements to copy from srcData1 to dstData2
  int len2,           // size in elements to copy from srcData2 to dstData1
  ESMC_TypeKind dtk1, // data type kind
  ESMC_TypeKind dtk2, // data type kind
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

  int blen1 = len1 * ESMC_TypeKindSize(dtk1);
  int blen2 = len2 * ESMC_TypeKindSize(dtk2);
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
//    int error return code
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
//    int error return code
//
// !ARGUMENTS:
//
  void *data,    // data 
  int len,       // message size in elements
  ESMC_TypeKind dtk,// data type kind
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

  int blen = len * ESMC_TypeKindSize(dtk);
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
//    int error return code
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
//    int error return code
//
// !ARGUMENTS:
//
  void *srcdata,    // input array
  void *destdata,   // output array
  int len,          // message size in elements
  ESMC_TypeKind dtk,// data type kind
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

  int blen = len * ESMC_TypeKindSize(dtk);
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
//    int error return code
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
//    int error return code
//
// !ARGUMENTS:
//
  void *srcdata,    // input array
  void *destdata,   // output array
  int len,          // message size in bytes
  ESMC_TypeKind dtk,// data type kind
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

  int blen = len * ESMC_TypeKindSize(dtk);
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
//    int error return code
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
//    int error return code
//
// !ARGUMENTS:
//
  void *srcdata,  // input array
  void *destdata, // output array
  int *len,       // array of message sizes in elements for each DE
                  // - the PET that holds rootDE must provide all blen elementes
                  // - all other PETs only need to fill elements for their DEs
  int *destdispl, // displacement vector for destdata for each DE mes. in elem.
  ESMC_TypeKind dtk,// data type kind
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
  int dtk_size = ESMC_TypeKindSize(dtk);

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
//    int error return code
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_DELayoutFillLocal()"
//BOPI
// !IROUTINE:  ESMC_DELayoutFillLocal
//
// !INTERFACE:
int DELayout::ESMC_DELayoutFillLocal(int mypet){
//
// !RETURN VALUE:
//    int error return code
//
//
// !DESCRIPTION:
//    Fill local part of layout object
//
//EOPI
//-----------------------------------------------------------------------------
  int localrc;
  // Initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  localDeCount = 0;               // reset local de count
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].pet == mypet) ++localDeCount;
  localDeList = new int[localDeCount];  // allocate space to hold local de ids
  int j=0;
  for (int i=0; i<deCount; i++)
    if (deInfoList[i].pet == mypet){
      localDeList[j]=i;
      ++j;
    }
  localrc = ESMF_SUCCESS;
  return localrc;
}
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
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
  ){
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
  // get the current VM
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
#if 0
  printf("gjt in ESMCI::XXE::exec(), stream=%p, %d, %d\n", stream, count, 
    sizeof(StreamElement));
#endif
  
  void *xxeElement, *xxeIndexElement;
  SendnbInfo *xxeSendnbInfo;
  RecvnbInfo *xxeRecvnbInfo;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumInfo *xxeProductSumInfo;
  MemCpyInfo *xxeMemCpyInfo;
  
  for (int i=0; i<count; i++){
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
//        printf("case: sendnb: %p, %d, %d, %p\n", xxeSendnbInfo->buffer,
//          xxeSendnbInfo->size, xxeSendnbInfo->dstPet,
//          xxeSendnbInfo->commhandle);
        vm->vmk_send(xxeSendnbInfo->buffer, xxeSendnbInfo->size,
          xxeSendnbInfo->dstPet, xxeSendnbInfo->commhandle);
      }
      break;
    case recvnb:
      {
        xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
//        printf("case: recvnb: %p, %d, %d, %p\n", xxeRecvnbInfo->buffer,
//          xxeRecvnbInfo->size, xxeRecvnbInfo->srcPet,
//          xxeRecvnbInfo->commhandle);
        vm->vmk_recv(xxeRecvnbInfo->buffer, xxeRecvnbInfo->size,
          xxeRecvnbInfo->srcPet, xxeRecvnbInfo->commhandle);
      }
      break;
    case waitOnIndex:
      {
        xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
        xxeIndexElement = &(stream[xxeWaitOnIndexInfo->index]);
        xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
//        printf("case: waitOnIndex: %d, %p\n", xxeWaitOnIndexInfo->index,
//          xxeCommhandleInfo->commhandle);
        vm->commwait(xxeCommhandleInfo->commhandle);
      }
      break;
    case waitOnIndexRange:
      {
        xxeWaitOnIndexRangeInfo = (WaitOnIndexRangeInfo *)xxeElement;
        for (int j=xxeWaitOnIndexRangeInfo->indexStart;
          j<xxeWaitOnIndexRangeInfo->indexEnd; j++){
          xxeIndexElement = &(stream[j]);
          xxeCommhandleInfo = (CommhandleInfo *)xxeIndexElement;
          vm->commwait(xxeCommhandleInfo->commhandle);
        }
      }
      break;
    case productSum:
      {
        xxeProductSumInfo = (ProductSumInfo *)xxeElement;
        switch (xxeProductSumInfo->opSubId){
        case I4:
          {
            ESMC_I4 *element = (ESMC_I4 *)xxeProductSumInfo->element;
            ESMC_I4 *factorList = (ESMC_I4 *)xxeProductSumInfo->factorList;
            ESMC_I4 *valueList = (ESMC_I4 *)xxeProductSumInfo->valueList;
            for (int j=0; j<xxeProductSumInfo->factorCount; j++)
              *element += factorList[j] * valueList[j];
          }
          break;
        case I8:
          {
            ESMC_I8 *element = (ESMC_I8 *)xxeProductSumInfo->element;
            ESMC_I8 *factorList = (ESMC_I8 *)xxeProductSumInfo->factorList;
            ESMC_I8 *valueList = (ESMC_I8 *)xxeProductSumInfo->valueList;
            for (int j=0; j<xxeProductSumInfo->factorCount; j++)
              *element += factorList[j] * valueList[j];
          }
          break;
        case R4:
          {
            ESMC_R4 *element = (ESMC_R4 *)xxeProductSumInfo->element;
            ESMC_R4 *factorList = (ESMC_R4 *)xxeProductSumInfo->factorList;
            ESMC_R4 *valueList = (ESMC_R4 *)xxeProductSumInfo->valueList;
            for (int j=0; j<xxeProductSumInfo->factorCount; j++)
              *element += factorList[j] * valueList[j];
          }
          break;
        case R8:
          {
            ESMC_R8 *element = (ESMC_R8 *)xxeProductSumInfo->element;
            ESMC_R8 *factorList = (ESMC_R8 *)xxeProductSumInfo->factorList;
            ESMC_R8 *valueList = (ESMC_R8 *)xxeProductSumInfo->valueList;
            for (int j=0; j<xxeProductSumInfo->factorCount; j++)
              *element += factorList[j] * valueList[j];
          }
          break;
        default:
          break;
        }
      }
      break;
    case memCpy:
      {
        xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
        memcpy(xxeMemCpyInfo->dstMem, xxeMemCpyInfo->srcMem,
          xxeMemCpyInfo->size);
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;

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
  
  void *xxeElement, *xxeIndexElement;
  SendnbInfo *xxeSendnbInfo;
  RecvnbInfo *xxeRecvnbInfo;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumInfo *xxeProductSumInfo;
  
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
      case send:
        break;
      case recv:
        break;
      case sendnb:
        if (i>sendnbLowerIndex){
          xxeSendnbInfo = (SendnbInfo *)xxeElement;
          sendnbIndexList[sendnbCount] = i;
          ++sendnbCount;
          if (sendnbCount >= sendnbMax){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
              "- sendnbCount out of range", &rc);
            return rc;
          }
        }
        break;
      case recvnb:
        if (i>recvnbLowerIndex){
          xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
          recvnbIndexList[recvnbCount] = i;
          ++recvnbCount;
          if (recvnbCount >= recvnbMax){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
              "- recvnbCount out of range", &rc);
            return rc;
          }
        }
        break;
        // --- cases below this line must cannot be used in execution ---  
      case waitOnAllSendnb:
        sendnbLowerIndex = i; // all sendnb below this index are considered
        {
#if 0
          printf("case: waitOnAllSendnb: %d outstanding sendnb\n",
            sendnbCount);
#endif
          // start a new stream
          StreamElement *newstream = new StreamElement[max];  // prepare stream
          // fill in StreamElements from before this StreamElement
          memcpy(newstream, stream, i*sizeof(StreamElement));
          // insert explicit waitOnIndex StreamElements
          int xxeCount = i;
          for (int j=0; j<sendnbCount; j++){
            newstream[xxeCount].opId = waitOnIndex;
            xxeElement = &(newstream[xxeCount]);
            xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
            xxeWaitOnIndexInfo->index = sendnbIndexList[j];
            ++xxeCount;
            if (xxeCount >= max){
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                "- xxeCount out of range", &rc);
              return rc;
            }
          }
          // fill in StreamElements from after this StreamElement
          if (sendnbCount+count-1 >= max){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
              "- count out of range", &rc);
            return rc;
          }
          memcpy(newstream+xxeCount, stream+i+1, (count-i-1)
            * sizeof(StreamElement));
          // replace stream
          delete [] stream; // delete original stream
          stream = newstream; // replace by new stream
          count = sendnbCount + count -1;        // account for modification
        }
        breakFlag = 1;  // set
        break;
      case waitOnAllRecvnb:
        recvnbLowerIndex = i; // all recvnb below this index are considered
        {
#if 0
          printf("case: waitOnAllRecvnb: %d outstanding recvnb\n",
            recvnbCount);
#endif
          // start a new stream
          StreamElement *newstream = new StreamElement[max];  // prepare stream
          // fill in StreamElements from before this StreamElement
          // (excluding this StreamElement)
          memcpy(newstream, stream, i*sizeof(StreamElement));
          // insert explicit waitOnIndex StreamElements
          int xxeCount = i;
          for (int j=0; j<recvnbCount; j++){
            //printf("inserting waitOnIndex for: %d\n", recvnbIndexList[j]);
            newstream[xxeCount].opId = waitOnIndex;
            xxeElement = &(newstream[xxeCount]);
            xxeWaitOnIndexInfo = (WaitOnIndexInfo *)xxeElement;
            xxeWaitOnIndexInfo->index = recvnbIndexList[j];
            ++xxeCount;
            if (xxeCount >= max){
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                "- xxeCount out of range", &rc);
              return rc;
            }
          }
          // fill in StreamElements from after this StreamElement
          // (excluding this StreamElement)
          if (recvnbCount+count-1 >= max){
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
              "- count out of range", &rc);
            return rc;
          }
          memcpy(newstream+xxeCount, stream+i+1, (count-i-1)
            * sizeof(StreamElement));
          // replace stream
          delete [] stream; // delete original stream
          stream = newstream; // replace by new stream
          count = recvnbCount + count -1;        // account for modification
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
  // local vars
  int localrc;                // local return code
  int rc;                     // final return code

  // initialize return code; assume routine not implemented
  localrc = ESMC_RC_NOT_IMPL;
  rc = ESMC_RC_NOT_IMPL;
  
#if 0
  printf("gjt in ESMCI::XXE::optimize(), stream=%p, %d, %d\n", stream, count, 
    sizeof(StreamElement));
#endif

  void *xxeElement, *xxeIndexElement;
  SendnbInfo *xxeSendnbInfo;
  RecvnbInfo *xxeRecvnbInfo;
  WaitOnIndexInfo *xxeWaitOnIndexInfo;
  WaitOnIndexRangeInfo *xxeWaitOnIndexRangeInfo;
  CommhandleInfo *xxeCommhandleInfo;
  ProductSumInfo *xxeProductSumInfo;
  MemCpyInfo *xxeMemCpyInfo;
  
  class AnalyzeElement{
    public:
      OpId opId;
      OpSubId opSubId;
      int partnerPet;
      char *bufferStart;
      char *bufferEnd;
      int bufferSize;
      int indexCount;
      int indexList[10000];
      AnalyzeElement *next;
    public:
      AnalyzeElement(int index, OpId opIdArg, OpSubId opSubIdArg, 
        int petArg, void *buffer, int size){
        opId=opIdArg;
        opSubId=opSubIdArg;
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
          printf("opId: %d, opSubId: %d, partnerPet: %d, indexCount: %d, "
            "buffer: %p - %p\n",
            opId, opSubId, partnerPet, indexCount, bufferStart, bufferEnd);
          next->print();  // recursive call
        }else
          printf("ae: END\n");
      }
      void add(int index, OpId opIdArg, OpSubId opSubIdArg, 
        int petArg, void *buffer, int size){
        if (opIdArg!=opId || opSubId!=opSubId || petArg!=partnerPet)
          if (next)
            next->add(index, opIdArg, opSubIdArg, petArg, buffer, size);
          else
            next = new 
              AnalyzeElement(index, opIdArg, opSubIdArg, petArg, buffer, size);
        else{
          if ((char *)buffer == bufferEnd){
            // in this simple analyzer elements must be in order
            bufferEnd += size;
            bufferSize += size;
            indexList[indexCount]=index;
            ++indexCount;
          }else
            if (next)
              next->add(index, opIdArg, opSubIdArg, petArg, buffer, size);
            else
              next = new 
                AnalyzeElement(index, opIdArg, opSubIdArg, petArg, buffer,
                  size);
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
          if (aq) aq->add(i, xxeSendnbInfo->opId, xxeSendnbInfo->opSubId,
            xxeSendnbInfo->dstPet, xxeSendnbInfo->buffer, xxeSendnbInfo->size);
          else
            aq = new AnalyzeElement(i, xxeSendnbInfo->opId,
              xxeSendnbInfo->opSubId, xxeSendnbInfo->dstPet,
              xxeSendnbInfo->buffer, xxeSendnbInfo->size);
        }
        break;
      case recvnb:
        {
          xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
          if (aq) aq->add(i, xxeRecvnbInfo->opId, xxeRecvnbInfo->opSubId,
            xxeRecvnbInfo->srcPet, xxeRecvnbInfo->buffer, xxeRecvnbInfo->size);
          else
            aq = new AnalyzeElement(i, xxeRecvnbInfo->opId,
              xxeRecvnbInfo->opSubId, xxeRecvnbInfo->srcPet,
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
              for (int k=1; k<aeList[0]->indexCount; k++)
                stream[aeList[0]->indexList[k]].opId = nop;
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
                xxeElement = &(extrastream[xxeCount]);
                xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
                xxeMemCpyInfo->dstMem = buffer + bufferOffset;
                xxeMemCpyInfo->srcMem = aeList[kk]->bufferStart;
                xxeMemCpyInfo->size = aeList[kk]->bufferSize;
                ++xxeCount;
                bufferOffset += aeList[kk]->bufferSize;
                // invalidate all StreamElements in stream assoc. w. aeList[kk]
                for (int k=0; k<aeList[kk]->indexCount; k++)
                  stream[aeList[kk]->indexList[k]].opId = nop;  // invalidate
              }
              // determine first Sendnb index
              int firstIndex = aeList[0]->indexList[0];
              // replace the first sendnb StreamElement using the interm. buffer
              stream[firstIndex].opId = sendnb;
              xxeElement = &(stream[firstIndex]);
              xxeSendnbInfo = (SendnbInfo *)xxeElement;
              xxeSendnbInfo->buffer = buffer;
              xxeSendnbInfo->size = bufferSize;
              // slip in the associated memcpy()s _before_ the first Sendnb
              if (xxeCount+count > max){
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                  "- count out of range", &rc);
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
              for (int k=1; k<aeList[0]->indexCount; k++)
                stream[aeList[0]->indexList[k]].opId = nop;
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
                xxeElement = &(extrastream[xxeCount]);
                xxeMemCpyInfo = (MemCpyInfo *)xxeElement;
                xxeMemCpyInfo->dstMem = aeList[kk]->bufferStart;
                xxeMemCpyInfo->srcMem = buffer + bufferOffset;
                xxeMemCpyInfo->size = aeList[kk]->bufferSize;
                ++xxeCount;
                bufferOffset += aeList[kk]->bufferSize;
                // invalidate all StreamElements in stream asso. with aeList[kk]
                for (int k=0; k<aeList[kk]->indexCount; k++)
                  stream[aeList[kk]->indexList[k]].opId = nop;  // invalidate
              }
              // replace the very first recvnb StreamElement using the
              // intermediate buffer
              stream[aeList[0]->indexList[0]].opId = recvnb;
              xxeElement = &(stream[aeList[0]->indexList[0]]);
              xxeRecvnbInfo = (RecvnbInfo *)xxeElement;
              xxeRecvnbInfo->buffer = buffer;
              xxeRecvnbInfo->size = bufferSize;
              // slip in the associated memcpy()s _after_ the waitOnAllRecvnb
              if (xxeCount+count > max){
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
                  "- count out of range", &rc);
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

} // namespace ESMCI
