// $Id: ESMCI_ArrayBundle.C,v 1.19.2.1 2010/02/05 19:53:04 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_ArrayBundle.C"
//==============================================================================
//
// ESMCI ArrayBundle method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ ArrayBundle methods declared
// in the companion file ESMCI_ArrayBundle.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_ArrayBundle.h"

// include higher level, 3rd party or system headers
#include <cstdio>
#include <cstring>
#include <vector>
#include <algorithm>

// include ESMF headers
#include "ESMC_Start.h"

// LogErr headers
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_ArrayBundle.C,v 1.19.2.1 2010/02/05 19:53:04 svasquez Exp $";
//-----------------------------------------------------------------------------


namespace ESMCI {

  //-----------------------------------------------------------------------------
//
// constructor and destructor
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::ArrayBundle()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::ArrayBundle    - constructor
//
// !INTERFACE:
ArrayBundle::ArrayBundle(
//
// !RETURN VALUE:
//    
//
// !ARGUMENTS:
//
  Array **arrayListArg,                   // (in)
  int arrayCountArg,                      // (in)
  int *rc                                 // (out)
  ){
//
// !DESCRIPTION:
//    Construct the internal structure of an ESMCI::ArrayBundle object.
//    No error checking wrt consistency of input arguments is needed because
//    this ArrayBundle constructor is only to be called by ArrayCreate()
//    interfaces which are responsible for providing consistent arguments to
//    this layer.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  try{  

  // fill in the ArrayBundle object
  arrayCount = arrayCountArg;
  arrayList = new Array*[arrayCount];
  memcpy(arrayList, arrayListArg, arrayCount*sizeof(Array *));
  arrayCreator = false; // Array objects were provided externally
  
  // invalidate the name for this ArrayBundle object in the Base class
  ESMC_BaseSetName(NULL, "ArrayBundle");
   
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
#define ESMC_METHOD "ESMCI::ArrayBundle::destrict()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::destruct
//
// !INTERFACE:
int ArrayBundle::destruct(bool followCreator){
//
// TODO: The followCreator flag is only needed until we have reference counting // TODO: For now followCreator, which by default is true, will be coming in as
// TODO: false when calling through the native destructor. This prevents
// TODO: sequence problems during automatic garbage collection unitl reference
// TODO: counting comes in to solve this problem in the final manner.
//
// !DESCRIPTION:
//    Destruct the internal structure of an ESMCI::ArrayBundle object.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  if (ESMC_BaseGetStatus()==ESMF_STATUS_READY){
    // garbage collection
    if (arrayList != NULL){
      if (arrayCreator && followCreator)
        for (int i=0; i<arrayCount; i++)
          Array::destroy(&arrayList[i]);
      delete [] arrayList;
    }
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// create() and destroy()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::create()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::create
//
// !INTERFACE:
ArrayBundle *ArrayBundle::create(
//
// !RETURN VALUE:
//    ArrayBundle * to newly allocated ArrayBundle
//
// !ARGUMENTS:
//
  Array **arrayListArg,                       // (in)
  int arrayCount,                             // (in)
  int *rc                                     // (out) return code
  ){
//
// !DESCRIPTION:
//    Create an {\tt ArrayBundle} object from list of Arrays.
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code
  
  ArrayBundle *arraybundle;
  try{
  
  // check the input and get the information together to call creator

  // call class constructor
  try{
    arraybundle = new ArrayBundle(arrayListArg, arrayCount, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return ESMC_NULL_POINTER;
  }catch(...){
    // allocation error
    ESMC_LogDefault.ESMC_LogMsgAllocError("for new ESMCI::ArrayBundle.", rc);  
    return ESMC_NULL_POINTER;
  }
  
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", rc);
    return ESMC_NULL_POINTER;
  }
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return arraybundle;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::destroy()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::destroy
//
// !INTERFACE:
int ArrayBundle::destroy(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle **arraybundle){  // in - ArrayBundle to destroy
//
// !DESCRIPTION:
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // return with errors for NULL pointer
  if (arraybundle == ESMC_NULL_POINTER || *arraybundle == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ArrayBundle", &rc);
    return rc;
  }

  // destruct ArrayBundle object
  localrc = (*arraybundle)->destruct();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // mark as invalid object
  (*arraybundle)->ESMC_BaseSetStatus(ESMF_STATUS_INVALID);
  
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
#define ESMC_METHOD "ESMCI::ArrayBundle::print()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::print
//
// !INTERFACE:
int ArrayBundle::print()const{
//
// !RETURN VALUE:
//    int return code
//
//
// !DESCRIPTION:
//    Print details of ArrayBundle object 
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // return with errors for NULL pointer
  if (this == NULL){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ArrayBundle", &rc);
    return rc;
  }

  // print info about the ESMCI::ArrayBundle object
  printf("--- ESMCI::ArrayBundle::print start ---\n");
  printf("ArrayBundle: %s\n", getName());
  printf("arrayCount = %d\n", arrayCount);
  for (int i=0; i<arrayCount; i++)
    printf("arrayList[%d]: %s\n", i, arrayList[i]->getName());
  printf("--- ESMCI::ArrayBundle::print end ---\n");
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//
// comms
//
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::redistStore()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::redistStore
//
// !INTERFACE:
int ArrayBundle::redistStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,          // in    - source ArrayBundle
  ArrayBundle *dstArraybundle,          // in    - destination ArrayBundle
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  InterfaceInt *srcToDstTransposeMap,   // in    - mapping src -> dst dims
  ESMC_TypeKind typekindFactor,         // in    - typekind of factor
  void *factor                          // in    - redist factor
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for redistribution
//  from srcArrayBundle to dstArrayBundle.
//
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  int petCount = vm->getPetCount();
  int localPet = vm->getLocalPet();

  try{
    // every Pet must provide srcArraybundle and dstArraybundle
    if (srcArraybundle == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to srcArraybundle", &rc);
      return rc;
    }
    if (dstArraybundle == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to dstArraybundle", &rc);
      return rc;
    }
    int arrayCount = arrayCount = srcArraybundle->getArrayCount();
    if (arrayCount != dstArraybundle->getArrayCount()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle contain different number"
        " of Arrays", &rc);
      return rc;
    }
    vector<int> arrayCountList(petCount);
    vm->allgather(&arrayCount, &(arrayCountList[0]), sizeof(int));
    arrayCount = *min_element(arrayCountList.begin(), arrayCountList.end());
    if (arrayCount != 
      *max_element(arrayCountList.begin(), arrayCountList.end())){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle arguments contain different number"
        " of Arrays on different PETs", &rc);
      return rc;
    }
    if (arrayCount == 0){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle arguments contain no Arrays", &rc);
      return rc;
    }
    // construct local matchList
    vector<int> matchList(arrayCount);
    for (int i=0; i<arrayCount; i++){
      matchList[i] = i; // initialize
      Array *srcArray = srcArraybundle->getArrayList()[i];
      Array *dstArray = dstArraybundle->getArrayList()[i];
      // search if there was an earlier entry that matches
      for (int j=i-1; j>=0; j--){
        bool srcMatch = Array::match(srcArray,
          srcArraybundle->getArrayList()[j], &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        bool dstMatch = Array::match(dstArray,
          dstArraybundle->getArrayList()[j], &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        if (srcMatch && dstMatch){
          // found match
          matchList[i] = matchList[j];
          break;
        }
      }
    }
    // communicate to construct global matchList
    vector<int> matchPetList(petCount);
    for (int i=0; i<arrayCount; i++){
      vm->allgather(&(matchList[i]), &(matchPetList[0]), sizeof(int));
      int match = *min_element(matchPetList.begin(), matchPetList.end());
      if (match == *max_element(matchPetList.begin(), matchPetList.end()))
        matchList[i] = match;        
      else
        matchList[i] = i;
    }
    // create and initialize the RouteHandle
    *routehandle = RouteHandle::create(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      &rc)) return rc;
    localrc = (*routehandle)->setType(ESMC_ARRAYBUNDLEXXE);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      &rc)) return rc;
    // allocate XXE and attach to RouteHandle
    XXE *xxe;
    try{
      xxe = new XXE(vm, 100, 10, 1000);
    }catch (...){
      ESMC_LogDefault.ESMC_LogAllocError(&rc);
      return rc;
    }
    localrc = (*routehandle)->setStorage(xxe);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      &rc)) return rc;
    // use Array::redistStore() to determine the required XXE streams
    int rraShift = 0; // reset
    vector<XXE *> xxeSub(arrayCount);
    for (int i=0; i<arrayCount; i++){
      Array *srcArray = srcArraybundle->getArrayList()[i];
      Array *dstArray = dstArraybundle->getArrayList()[i];
      if (matchList[i] < i){
        // src/dst Array pair matches previous pair in ArrayBundle
//        printf("localPet=%d, src/dst pair #%d does not require precompute\n",
//          localPet, i);
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[matchList[i]], rraShift);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
      }else{
        // src/dst Array pair does _not_ match any previous pair in ArrayBundle
//        printf("localPet=%d, src/dst pair #%d requires precompute\n",
//          localPet, i);
        RouteHandle *rh;
        localrc = Array::redistStore(srcArray, dstArray, &rh,
          srcToDstTransposeMap, typekindFactor, factor);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // get a handle on the XXE stored in rh
        xxeSub[i] = (XXE *)rh->getStorage();
        // delete the temporary routehandle w/o deleting the xxeSub
        localrc = rh->setType(ESMC_UNINITIALIZEDHANDLE);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        localrc = RouteHandle::destroy(rh);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[i], rraShift);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // keep track of xxeSub for xxe garbage collection
        localrc = xxe->storeXxeSub(xxeSub[i]);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }
      rraShift += srcArray->getDELayout()->getLocalDeCount()
        + dstArray->getDELayout()->getLocalDeCount();
    }
    //TODO: consider calling an XXE optimization method here that could
    //TODO: re-arrange what is in all of the sub XXE streams for performance opt
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }

  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------
    
    //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::redist()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::redist
//
// !INTERFACE:
int ArrayBundle::redist(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,          // in    - source Array
  ArrayBundle *dstArraybundle,          // inout - destination Array
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  ESMC_Logical checkflag                // in    - ESMF_FALSE: (def.) bas. chcks
                                        //         ESMF_TRUE: full input check
  ){    
//
// !DESCRIPTION:
//    Execute an ArrayBundle redistribution operation
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // implemented via sparseMatMul
  localrc = sparseMatMul(srcArraybundle, dstArraybundle, routehandle,
    ESMF_REGION_TOTAL, checkflag);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::redistRelease()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::redistRelease
//
// !INTERFACE:
int ArrayBundle::redistRelease(
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
//    Release information for an ArrayBundle redistribution
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
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::sparseMatMulStore()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::sparseMatMulStore
//
// !INTERFACE:
int ArrayBundle::sparseMatMulStore(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,          // in    - source ArrayBundle
  ArrayBundle *dstArraybundle,          // in    - destination ArrayBundle
  RouteHandle **routehandle,            // inout - handle to precomputed comm
  vector<SparseMatrix> &sparseMatrix    // in    - sparse matrix
  ){    
//
// !DESCRIPTION:
//  Precompute and store communication pattern for sparse matrix multiplication
//  from srcArrayBundle to dstArrayBundle.
//
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // get the current VM and VM releated information
  VM *vm = VM::getCurrent(&localrc);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  int petCount = vm->getPetCount();
  int localPet = vm->getLocalPet();

  try{
    // every Pet must provide srcArraybundle and dstArraybundle
    if (srcArraybundle == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to srcArraybundle", &rc);
      return rc;
    }
    if (dstArraybundle == NULL){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
        "- Not a valid pointer to dstArraybundle", &rc);
      return rc;
    }
    int arrayCount = arrayCount = srcArraybundle->getArrayCount();
    if (arrayCount != dstArraybundle->getArrayCount()){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle contain different number"
        " of Arrays", &rc);
      return rc;
    }
    vector<int> arrayCountList(petCount);
    vm->allgather(&arrayCount, &(arrayCountList[0]), sizeof(int));
    arrayCount = *min_element(arrayCountList.begin(), arrayCountList.end());
    if (arrayCount != 
      *max_element(arrayCountList.begin(), arrayCountList.end())){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle arguments contain different number"
        " of Arrays on different PETs", &rc);
      return rc;
    }
    if (arrayCount == 0){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle arguments contain no Arrays", &rc);
      return rc;
    }
    // construct local matchList
    vector<int> matchList(arrayCount);
    for (int i=0; i<arrayCount; i++){
      matchList[i] = i; // initialize
      Array *srcArray = srcArraybundle->getArrayList()[i];
      Array *dstArray = dstArraybundle->getArrayList()[i];
      // search if there was an earlier entry that matches
      for (int j=i-1; j>=0; j--){
        bool srcMatch = Array::match(srcArray,
          srcArraybundle->getArrayList()[j], &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        bool dstMatch = Array::match(dstArray,
          dstArraybundle->getArrayList()[j], &localrc);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        if (srcMatch && dstMatch){
          // found match
          matchList[i] = matchList[j];
          break;
        }
      }
    }
    // communicate to construct global matchList
    vector<int> matchPetList(petCount);
    for (int i=0; i<arrayCount; i++){
      vm->allgather(&(matchList[i]), &(matchPetList[0]), sizeof(int));
      int match = *min_element(matchPetList.begin(), matchPetList.end());
      if (match == *max_element(matchPetList.begin(), matchPetList.end()))
        matchList[i] = match;        
      else
        matchList[i] = i;
    }
    // create and initialize the RouteHandle
    *routehandle = RouteHandle::create(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      &rc)) return rc;
    localrc = (*routehandle)->setType(ESMC_ARRAYBUNDLEXXE);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      &rc)) return rc;
    // allocate XXE and attach to RouteHandle
    XXE *xxe;
    try{
      xxe = new XXE(vm, 100, 10, 1000);
    }catch (...){
      ESMC_LogDefault.ESMC_LogAllocError(&rc);
      return rc;
    }
    localrc = (*routehandle)->setStorage(xxe);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
      &rc)) return rc;
    // use Array::sparseMatMulStore() to determine the required XXE streams
    int rraShift = 0; // reset
    vector<XXE *> xxeSub(arrayCount);
    for (int i=0; i<arrayCount; i++){
      Array *srcArray = srcArraybundle->getArrayList()[i];
      Array *dstArray = dstArraybundle->getArrayList()[i];
      if (matchList[i] < i){
        // src/dst Array pair matches previous pair in ArrayBundle
//        printf("localPet=%d, src/dst pair #%d does not require precompute\n",
//          localPet, i);
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[matchList[i]], rraShift);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
      }else{
        // src/dst Array pair does _not_ match any previous pair in ArrayBundle
//        printf("localPet=%d, src/dst pair #%d requires precompute\n",
//          localPet, i);
        RouteHandle *rh;
        localrc = Array::sparseMatMulStore(srcArray, dstArray, &rh,
          sparseMatrix);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // get a handle on the XXE stored in rh
        xxeSub[i] = (XXE *)rh->getStorage();
        // delete the temporary routehandle w/o deleting the xxeSub
        localrc = rh->setType(ESMC_UNINITIALIZEDHANDLE);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        localrc = RouteHandle::destroy(rh);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // append the xxeSub to the xxe object with RRA offset info
        localrc = xxe->appendXxeSub(0x0, xxeSub[i], rraShift);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
          &rc)) return rc;
        // keep track of xxeSub for xxe garbage collection
        localrc = xxe->storeXxeSub(xxeSub[i]);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,
          ESMF_ERR_PASSTHRU, &rc)) return rc;
      }
      rraShift += srcArray->getDELayout()->getLocalDeCount()
        + dstArray->getDELayout()->getLocalDeCount();
    }
    //TODO: consider calling an XXE optimization method here that could
    //TODO: re-arrange what is in all of the sub XXE streams for performance opt
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------
    
    //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::sparseMatMul()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::sparseMatMul
//
// !INTERFACE:
int ArrayBundle::sparseMatMul(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
//
  ArrayBundle *srcArraybundle,          // in    - source Array
  ArrayBundle *dstArraybundle,          // inout - destination Array
  RouteHandle **routehandle,            // inout - handle to precomputed comm
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
//    Execute an ArrayBundle sparse matrix multiplication operation
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{
    // in the most trivial implementation call Array::sparseMatMul() for each
    // Array pair
    
    RouteHandleType rhType = (*routehandle)->getType();
    
    Array *srcArray = NULL;
    Array *dstArray = NULL;
    if (rhType == ESMC_ARRAYXXE){
      // apply same routehandle to each src/dst Array pair
      if (srcArraybundle != NULL && dstArraybundle != NULL){
        if (srcArraybundle->getArrayCount() != dstArraybundle->getArrayCount()){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- srcArraybundle and dstArraybundle contain different number"
            " of Arrays", &rc);
          return rc;
        }
        for (int i=0; i<srcArraybundle->getArrayCount(); i++){
          srcArray = srcArraybundle->getArrayList()[i];
          dstArray = dstArraybundle->getArrayList()[i];
          localrc = Array::sparseMatMul(srcArray, dstArray, routehandle,
            zeroflag, checkflag);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &rc)) return rc;
        }
      }else if (srcArraybundle != NULL){
        for (int i=0; i<srcArraybundle->getArrayCount(); i++){
          srcArray = srcArraybundle->getArrayList()[i];
          localrc = Array::sparseMatMul(srcArray, dstArray, routehandle,
            zeroflag, checkflag);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &rc)) return rc;
        }
      }else if (dstArraybundle != NULL){
        for (int i=0; i<dstArraybundle->getArrayCount(); i++){
          dstArray = dstArraybundle->getArrayList()[i];
          localrc = Array::sparseMatMul(srcArray, dstArray, routehandle,
            zeroflag, checkflag);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &rc)) return rc;
        }
      }
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else if(rhType == ESMC_ARRAYBUNDLEXXE){
      // prepare for relative run-time addressing (RRA)
      vector<char *> rraList;
      rraList.reserve(100); // optimize performance
      if (srcArraybundle != NULL && dstArraybundle != NULL){
        if (srcArraybundle->getArrayCount() != dstArraybundle->getArrayCount()){
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
            "- srcArraybundle and dstArraybundle contain different number"
            " of Arrays", &rc);
          return rc;
        }
        for (int i=0; i<srcArraybundle->getArrayCount(); i++){
          srcArray = srcArraybundle->getArrayList()[i];
          void **larrayBaseAddrList = srcArray->getLarrayBaseAddrList();
          for (int j=0; j<srcArray->getDELayout()->getLocalDeCount(); j++){
            char *rraElement = (char *)larrayBaseAddrList[j];
            rraList.push_back(rraElement);
          }
          dstArray = dstArraybundle->getArrayList()[i];
          larrayBaseAddrList = dstArray->getLarrayBaseAddrList();
          for (int j=0; j<dstArray->getDELayout()->getLocalDeCount(); j++){
            char *rraElement = (char *)larrayBaseAddrList[j];
            rraList.push_back(rraElement);
          }
        }
      }else if (srcArraybundle != NULL){
        for (int i=0; i<srcArraybundle->getArrayCount(); i++){
          srcArray = srcArraybundle->getArrayList()[i];
          void **larrayBaseAddrList = srcArray->getLarrayBaseAddrList();
          for (int j=0; j<srcArray->getDELayout()->getLocalDeCount(); j++){
            char *rraElement = (char *)larrayBaseAddrList[j];
            rraList.push_back(rraElement);
          }
        }
      }else if (dstArraybundle != NULL){
        for (int i=0; i<dstArraybundle->getArrayCount(); i++){
          dstArray = dstArraybundle->getArrayList()[i];
          void **larrayBaseAddrList = dstArray->getLarrayBaseAddrList();
          for (int j=0; j<dstArray->getDELayout()->getLocalDeCount(); j++){
            char *rraElement = (char *)larrayBaseAddrList[j];
            rraList.push_back(rraElement);
          }
        }
      }
      int rraCount = rraList.size();
      // set filterBitField  
      int filterBitField = 0x0; // init. to execute _all_ operations in XXE
  
      //TODO: determine XXE filterBitField for XXE exec(),
      //TODO: considering src vs. dst, DE, phase of nb-call...
  
      if (zeroflag!=ESMF_REGION_TOTAL)
        filterBitField |= 1;  // filter the region_total zero operations
      if (zeroflag!=ESMF_REGION_SELECT)
        filterBitField |= 2;  // filter the region_select zero operations
      // get a handle on the XXE stored in routehandle
      XXE *xxe = (XXE *)(*routehandle)->getStorage();
      // execute XXE stream
      localrc = xxe->exec(rraCount, &(rraList[0]), filterBitField);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else{
      // unimplemented branch
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- only ESMC_ARRAYXXE and ESMC_ARRAYBUNDLEXXE are supported", &rc);
      return rc;
    }
  }catch(...){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_INTNRL_BAD,
      "- Caught exception", &rc);
    return rc;
  }
  
  // return, don't set success, this is multi exit method
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::sparseMatMulRelease()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::sparseMatMulRelease
//
// !INTERFACE:
int ArrayBundle::sparseMatMulRelease(
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
//    Release information for an ArrayBundle sparse matrix multiplication
//    operation.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  try{
  
  // get XXE from routehandle
  XXE *xxe = (XXE *)routehandle->getStorage();
  
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
    vm->barrier();
  }
#endif
  
  // delete xxe
  delete xxe;

  // mark storage pointer in RouteHandle as invalid  
  routehandle->setStorage(NULL);
  
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
// serialize() and deserialize()
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::serialize()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::serialize - Turn ArrayBundle into byte stream
//
// !INTERFACE:
int ArrayBundle::serialize(
//
// !RETURN VALUE:
//    int return code
//
// !ARGUMENTS:
  char *buffer,          // inout - byte stream to fill
  int *length,           // inout - buf length
  int *offset,           // inout - original offset
  const ESMC_AttReconcileFlag &attreconflag,   // in - attreconcile flag
  const ESMC_InquireFlag &inquireflag) const { // in - inquireflag
//
// !DESCRIPTION:
//    Turn info in ArrayBundle class into a stream of bytes.
//
//EOPI
//-----------------------------------------------------------------------------
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Prepare pointer variables of different types
  char *cp;
  int *ip;
  int r;

  // Check if buffer has enough free memory to hold object
  if ((inquireflag != ESMF_INQUIREONLY) && (*length - *offset) < sizeof(ArrayBundle)){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an ArrayBundle object", &rc);
    return rc;
  }

  // Serialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Serialize(buffer,length,offset,attreconflag,inquireflag);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Serialize the ArrayBundle with all its Arrays
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ip = (int *)(buffer + *offset);
  if (inquireflag != ESMF_INQUIREONLY)
    *ip++ = arrayCount;
  else
    ip++;

  cp = (char *)ip;
  *offset = (cp - buffer);
  for (int i=0; i<arrayCount; i++){
    localrc = arrayList[i]->serialize(buffer,length,offset,attreconflag,inquireflag);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
      return rc;
  }
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::ArrayBundle::deserialize()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::deserialize - Turn byte strm into ArrayBundle
//
// !INTERFACE:
int ArrayBundle::deserialize(
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
  int r;
  
  // Deserialize the Base class
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  localrc = ESMC_Base::ESMC_Deserialize(buffer,offset,attreconflag);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Deserialize the ArrayBundle with all its Arrays
  r=*offset%8;
  if (r!=0) *offset += 8-r;  // alignment
  ip = (int *)(buffer + *offset);
  arrayCount = *ip++;
  cp = (char *)ip;
  *offset = (cp - buffer);
  arrayList = new Array*[arrayCount];
  for (int i=0; i<arrayCount; i++){
    arrayList[i] = new Array(-1); // prevent baseID counter increment
    arrayList[i]->deserialize(buffer,offset,attreconflag);
  }
  arrayCreator = true;  // deserialize creates local Array objects
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


} // namespace ESMCI
