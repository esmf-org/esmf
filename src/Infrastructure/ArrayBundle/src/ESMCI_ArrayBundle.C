// $Id: ESMCI_ArrayBundle.C,v 1.1.2.8 2008/05/09 04:52:33 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
#include <stdio.h>
#include <string.h>

// include ESMF headers
#include "ESMC_Start.h"

// LogErr headers
#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_ArrayBundle.C,v 1.1.2.8 2008/05/09 04:52:33 theurich Exp $";
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
//    Construct the internal structure of an ESMC\_ArrayBundle object.
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
#define ESMC_METHOD "ESMCI::ArrayBundle::~ArrayBundle()"
//BOPI
// !IROUTINE:  ESMCI::ArrayBundle::~ArrayBundle   - destructor
//
// !INTERFACE:
ArrayBundle::~ArrayBundle(){
//
// !DESCRIPTION:
//    Destruct the internal structure of an ESMC\_ArrayBundle object.
//
//EOPI
//-----------------------------------------------------------------------------
  // garbage collection
  if (arrayList != NULL){
    if (arrayCreator)
      for (int i=0; i<arrayCount; i++)
        Array::destroy(&arrayList[i]);
    delete [] arrayList;
  }
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
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  try{

  // return with errors for NULL pointer
  if (arraybundle == ESMC_NULL_POINTER || *arraybundle == ESMC_NULL_POINTER){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
      "- Not a valid pointer to ArrayBundle", &rc);
    return rc;
  }

  // delete ArrayBundle object
  delete *arraybundle;
  *arraybundle = ESMC_NULL_POINTER;
  
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
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
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
  
  try{
    // initial check of input
    int srcArrayCount = 0; // reset
    int dstArrayCount = 0; // reset
    if (srcArraybundle != NULL)
      srcArrayCount = srcArraybundle->getArrayCount();
    if (dstArraybundle != NULL)
      dstArrayCount = dstArraybundle->getArrayCount();
    if (srcArrayCount != dstArrayCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle contain different number"
        " of Arrays", &rc);
      return rc;
    }
    
    if (srcArrayCount == 0){
      // nothing to do for empty ArrayBundles
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }
    Array *srcArray = srcArraybundle->getArrayList()[0];
    Array *dstArray = dstArraybundle->getArrayList()[0];

    // check if all src/dst Array pairs are congruent
    bool allPairsCongruent = true;  // reset: assume pairs are congruent
    for (int i=1; i<srcArrayCount; i++){
      allPairsCongruent = Array::match(srcArray,
        srcArraybundle->getArrayList()[i], &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      if (!allPairsCongruent) break;  // found mismatch
      allPairsCongruent = Array::match(dstArray,
        dstArraybundle->getArrayList()[i], &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      if (!allPairsCongruent) break;  // found mismatch
    }
    
    if (allPairsCongruent){
      // trivial implementation only call Array::redistStore() for first
      // src/dst Array pair
      localrc = Array::redistStore(srcArray, dstArray, routehandle,
        srcToDstTransposeMap, typekindFactor, factor);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else{
      // unimplemented branch
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- non-congruent src/dst Array pairs are not yet supported", &rc);
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
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
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
  
  try{
    // in the most trivial implementation call Array::redist() for each
    // Array pair
    
    ESMC_HandleType rhType = (*routehandle)->ESMC_RouteHandleGetType();
    
    if (rhType == ESMC_ARRAYSPARSEMATMULHANDLE){
      // apply same routehandle to each src/dst Array pair
    
      Array *srcArray = NULL;
      Array *dstArray = NULL;

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
          localrc = Array::redist(srcArray, dstArray, routehandle, checkflag);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &rc)) return rc;
        }
      }else if (srcArraybundle != NULL){
        for (int i=0; i<srcArraybundle->getArrayCount(); i++){
          srcArray = srcArraybundle->getArrayList()[i];
          localrc = Array::redist(srcArray, dstArray, routehandle, checkflag);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &rc)) return rc;
        }
      }else if (dstArraybundle != NULL){
        for (int i=0; i<dstArraybundle->getArrayCount(); i++){
          dstArray = dstArraybundle->getArrayList()[i];
          localrc = Array::redist(srcArray, dstArray, routehandle, checkflag);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
            &rc)) return rc;
        }
      }
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else{
      // unimplemented branch
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- only simple ESMC_ARRAYSPARSEMATMULHANDLE are supported", &rc);
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
  ESMC_RouteHandle *routehandle        // inout -
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
  ESMC_RouteHandle **routehandle,       // inout - handle to precomputed comm
  ESMC_TypeKind typekindFactors,        // in    - typekind of factors
  void *factorList,                     // in    - sparse matrix factors
  int factorListCount,                  // in    - number of sparse mat. indices
  InterfaceInt *factorIndexList         // in    - sparse matrix indices
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
  
  try{
    // initial check of input
    int srcArrayCount = 0; // reset
    int dstArrayCount = 0; // reset
    if (srcArraybundle != NULL)
      srcArrayCount = srcArraybundle->getArrayCount();
    if (dstArraybundle != NULL)
      dstArrayCount = dstArraybundle->getArrayCount();
    if (srcArrayCount != dstArrayCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_INCOMP,
        "- srcArraybundle and dstArraybundle contain different number"
        " of Arrays", &rc);
      return rc;
    }
    
    if (srcArrayCount == 0){
      // nothing to do for empty ArrayBundles
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }
    Array *srcArray = srcArraybundle->getArrayList()[0];
    Array *dstArray = dstArraybundle->getArrayList()[0];

    // check if all src/dst Array pairs are congruent
    bool allPairsCongruent = true;  // reset: assume pairs are congruent
    for (int i=1; i<srcArrayCount; i++){
      allPairsCongruent = Array::match(srcArray,
        srcArraybundle->getArrayList()[i], &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      if (!allPairsCongruent) break;  // found mismatch
      allPairsCongruent = Array::match(dstArray,
        dstArraybundle->getArrayList()[i], &localrc);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      if (!allPairsCongruent) break;  // found mismatch
    }
    
    if (allPairsCongruent){
      // trivial implementation only call Array::sparseMatMulStore() for first
      // src/dst Array pair
      localrc = Array::sparseMatMulStore(srcArray, dstArray, routehandle,
        typekindFactors, factorList, factorListCount, factorIndexList);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU,
        &rc)) return rc;
      // return successfully
      rc = ESMF_SUCCESS;
      return rc;
    }else{
      // unimplemented branch
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- non-congruent src/dst Array pairs are not yet supported", &rc);
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
    
    ESMC_HandleType rhType = (*routehandle)->ESMC_RouteHandleGetType();
    
    if (rhType == ESMC_ARRAYSPARSEMATMULHANDLE){
      // apply same routehandle to each src/dst Array pair
    
      Array *srcArray = NULL;
      Array *dstArray = NULL;

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
    }else{
      // unimplemented branch
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
        "- only simple ESMC_ARRAYSPARSEMATMULHANDLE are supported", &rc);
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
  ESMC_RouteHandle *routehandle        // inout -
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
  int *offset)const{     // inout - original offset, updated to point 
                         //         to first free byte after current obj info
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

  // Check if buffer has enough free memory to hold object
  if ((*length - *offset) < sizeof(ArrayBundle)){
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
      "Buffer too short to add an ArrayBundle object", &rc);
    return rc;
  }

  // Serialize the Base class,
  localrc = ESMC_Base::ESMC_Serialize(buffer, length, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Serialize the ArrayBundle with all its Arrays
  ip = (int *)(buffer + *offset);
  *ip++ = arrayCount;
  cp = (char *)ip;
  *offset = (cp - buffer);
  for (int i=0; i<arrayCount; i++){
    localrc = arrayList[i]->serialize(buffer, length, offset);
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

  // Deserialize the Base class
  localrc = ESMC_Base::ESMC_Deserialize(buffer, offset);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;
  // Deserialize the ArrayBundle with all its Arrays
  ip = (int *)(buffer + *offset);
  arrayCount = *ip++;
  cp = (char *)ip;
  *offset = (cp - buffer);
  arrayList = new Array*[arrayCount];
  for (int i=0; i<arrayCount; i++){
    arrayList[i] = new Array;
    arrayList[i]->deserialize(buffer, offset);
  }
  arrayCreator = true;  // deserialize creates local Array objects
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


} // namespace ESMCI
