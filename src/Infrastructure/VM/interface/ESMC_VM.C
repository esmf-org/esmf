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
#define ESMC_FILENAME "ESMC_VM.C"
//==============================================================================
//
// ESMC VM method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C VM methods declared
// in the companion file ESMC_VM.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_VM.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

int ESMC_VMBarrier(ESMC_VM vm){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMPrint()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::VM *vmp = (ESMCI::VM *)(vm.ptr);

  // call into ESMCI method  
  localrc = vmp->barrier();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

int ESMC_VMBroadcast(ESMC_VM vm, void *bcstData, int count,
                     enum ESMC_TypeKind_Flag *typekind, int rootPet){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMReduce()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::VM *vmp = (ESMCI::VM *)(vm.ptr);

  int len = count;
  // have to manually reset because the enum values don't line up
  if (typekind != NULL) {
    if (*typekind == ESMC_TYPEKIND_I4)
      len *= 4;
    else if (*typekind == ESMC_TYPEKIND_I8)
      len *= 8;
    else if (*typekind == ESMC_TYPEKIND_R4)
      len *= 4;
    else if (*typekind == ESMC_TYPEKIND_R8)
      len *= 8;
    else if (*typekind == ESMC_TYPEKIND_LOGICAL)
      len *= 4;
    // else if (*typekind == ESMC_TYPEKIND_CHARACTER)
    //   len *= 1;
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                    "bad typekind value", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  // call into ESMCI method  
  localrc = vmp->broadcast(bcstData, len, rootPet);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}

int ESMC_VMGet(ESMC_VM vm, int *localPet, int *petCount, int *peCount,
  MPI_Comm *mpiCommunicator, int *pthreadsEnabledFlag, int *openMPEnabledFlag){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGet()"

  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ESMCI::VM *vmp = (ESMCI::VM*)(vm.ptr);

  if (localPet) *localPet = vmp->getLocalPet();
  if (petCount) *petCount = vmp->getPetCount();

  if (peCount){
    //compute peCount
    int npets = vmp->getNpets();
    *peCount = 0; // reset
    for (int i=0; i<npets; i++)
      *peCount += vmp->getNcpet(i);
  }

  if (mpiCommunicator) *mpiCommunicator = vmp->getMpi_c();
  if (pthreadsEnabledFlag) *pthreadsEnabledFlag = vmp->isPthreadsEnabled();
  if (openMPEnabledFlag) *openMPEnabledFlag = vmp->isOpenMPEnabled();

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


ESMC_VM ESMC_VMGetCurrent(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetCurrent()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc) *rc = ESMC_RC_NOT_IMPL;         // final return code

  ESMC_VM vm;
  vm.ptr = (void *)NULL; // initialize

  ESMCI::VM *vmp = ESMCI::VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc))
    return vm;  // bail out

  vm.ptr = (void*)vmp;

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return vm;
}


ESMC_VM ESMC_VMGetGlobal(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetGlobal()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  if (rc) *rc = ESMC_RC_NOT_IMPL;         // final return code

  ESMC_VM vm;
  vm.ptr = (void *)NULL; // initialize

  ESMCI::VM *vmp = ESMCI::VM::getGlobal(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc))
    return vm;  // bail out

  vm.ptr = (void*)vmp;

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return vm;
}


int ESMC_VMPrint(ESMC_VM vm){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMPrint()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::VM *vmp = (ESMCI::VM *)(vm.ptr);

  // call into ESMCI method  
  localrc = vmp->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

int ESMC_VMReduce(ESMC_VM vm, void *in, void *out, int len, 
                  enum ESMC_TypeKind_Flag *typekind, enum ESMC_Reduce_Flag *reduceflag, 
                  int root){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMReduce()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code
  
  // typecast into ESMCI type
  ESMCI::VM *vmp = (ESMCI::VM *)(vm.ptr);

  vmType vmt;
  vmOp vmo;

  // have to manually reset because the enum values don't line up
  if (typekind != NULL) {
    if (*typekind == ESMC_TYPEKIND_I4)
      vmt = vmI4;
    else if (*typekind == ESMC_TYPEKIND_I8)
      vmt = vmI8;
    else if (*typekind == ESMC_TYPEKIND_R4)
      vmt = vmR4;
    else if (*typekind == ESMC_TYPEKIND_R8)
      vmt = vmR8;
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                    "bad typekind value", ESMC_CONTEXT, &rc);
      return rc;
    }
  }

  if (reduceflag != NULL)
    vmo = (vmOp) (*reduceflag);

  // printf("vmt = %d, vmo = %d\n", vmt, vmo);

  // call into ESMCI method  
  localrc = vmp->reduce(in, out, len, vmt, vmo, root);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  

}; // extern "C"
