// $Id: ESMC_VM.C,v 1.7.2.1 2010/02/05 20:01:36 svasquez Exp $
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
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_VM.C,v 1.7.2.1 2010/02/05 20:01:36 svasquez Exp $";
//-----------------------------------------------------------------------------

extern "C" {

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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc))
    return rc;  // bail out
    
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}  


ESMC_VM ESMC_VMGetGlobal(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetGlobal()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  *rc = ESMC_RC_NOT_IMPL;                 // final return code

  ESMC_VM vm;
  vm.ptr = (void *)NULL; // initialize

  ESMCI::VM *vmp = ESMCI::VM::getGlobal(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return vm;  // bail out

  vm.ptr = (void*)vmp;

  // return successfully
  *rc = ESMF_SUCCESS;
  return vm;
}


ESMC_VM ESMC_VMGetCurrent(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGetCurrent()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  *rc = ESMC_RC_NOT_IMPL;                 // final return code

  ESMC_VM vm;
  vm.ptr = (void *)NULL; // initialize

  ESMCI::VM *vmp = ESMCI::VM::getCurrent(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
    return vm;  // bail out

  vm.ptr = (void*)vmp;

  // return successfully
  *rc = ESMF_SUCCESS;
  return vm;
}


int ESMC_VMGet(ESMC_VM vm, int *localPet, int *petCount, int *peCount,
  MPI_Comm *mpiCommunicator, int *pthreadsEnabledFlag, int *openMPEnabledFlag){
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_VMGet()"

  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ESMCI::VM *vmp = (ESMCI::VM*)(vm.ptr);

  *localPet = vmp->getLocalPet();
  *petCount = vmp->getPetCount();

  //compute peCount
  int npets = vmp->getNpets();
  *peCount = 0; // reset
  for (int i=0; i<npets; i++)
    *peCount += vmp->getNcpet(i);

  *mpiCommunicator = vmp->getMpi_c();
  *pthreadsEnabledFlag = vmp->isPthreadsEnabled();
  *openMPEnabledFlag = vmp->isOpenMPEnabled();

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}


}; // extern "C"
