// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_VM_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_VM.h"

#include "ESMCI_F90Interface.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

#include <string>

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_VM} and {\tt ESMC\_VMPlan} 
//  class functions.
//
//EOP
//------------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case

extern "C" {

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMCI::VM interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN_X(c_esmc_vmallfullreduce)(ESMCI::VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind_Flag *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallfullreduce()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc,"Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->allfullreduce(input, output, *count, vmt, (vmOp)(*op));
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmallgather)(ESMCI::VM **vm, void *input, void *output, 
    int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgather()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->allgather(input, output, *size);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmallgathernb)(ESMCI::VM **vm, void *input, void *output, 
    int *size, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgathernb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->allgather(input, output, *size,
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmallgatherv)(ESMCI::VM **vm,
    void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets,
    ESMC_TypeKind_Flag *dtk, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgatherv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    case ESMC_TYPEKIND_CHARACTER:
      vmt = vmBYTE;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, "Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->allgatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmallreduce)(ESMCI::VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind_Flag *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallreduce()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, "Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->allreduce(input, output, *count, vmt, (vmOp)(*op));
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmalltoall)(ESMCI::VM **vm,
    void *sendData, int *sendCount,
    void *recvData, int *recvCount, 
    ESMC_TypeKind_Flag *dtk, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmalltoall()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, "Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->alltoall(sendData, *sendCount,
      recvData, *recvCount, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmalltoallv)(ESMCI::VM **vm,
    void *sendData, int *sendCounts, int *sendOffsets,
    void *recvData, int *recvCounts, int *recvOffsets, 
    ESMC_TypeKind_Flag *dtk, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmalltoallv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    case ESMC_TYPEKIND_LOGICAL:
      vmt = vmL4;
      break;
    case ESMC_TYPEKIND_CHARACTER:
      vmt = vmBYTE;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, "Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->alltoallv(sendData, sendCounts, sendOffsets, recvData,
      recvCounts, recvOffsets, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmbarrier)(ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbarrier()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->barrier();
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmbroadcast)(ESMCI::VM **vm, void *data, int *size, int *root,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->broadcast(data, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmbroadcastnb)(ESMCI::VM **vm, void *data, int *size,
    int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->broadcast(data, *size, *root,
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmepochenter)(ESMCI::VM **vm, vmEpoch *epoch, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmepochenter()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    try{
      (*vm)->epochEnter(*epoch);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmepochexit)(ESMCI::VM **vm, ESMC_Logical *keepAlloc, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmepochexit()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    // convert to bool
    bool keepAllocOpt = true; // default
    if (ESMC_NOT_PRESENT_FILTER(keepAlloc) != ESMC_NULL_POINTER)
      if (*keepAlloc == ESMF_FALSE) keepAllocOpt = false;
    try{
      (*vm)->epochExit(keepAllocOpt);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmgather)(ESMCI::VM **vm, void *input, void *output,
    int *size, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgather()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->gather(input, output, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmgathernb)(ESMCI::VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgathernb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->gather(input, output, *size, *root, 
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmgatherv)(ESMCI::VM **vm, void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets, ESMC_TypeKind_Flag *dtk, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgatherv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, "Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->gatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmget)(ESMCI::VM **vm, int *localPet, int *currentSsiPe, 
    int *petCount, int *peCount, int *ssiCount, int *ssiMinPetCount,
    int *ssiMaxPetCount, int *ssiLocalPetCount, int *mpiCommunicator,
    ESMC_Logical *pthreadsEnabledFlag, ESMC_Logical *openMPEnabledFlag,
    ESMC_Logical *ssiSharedMemoryEnabledFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    MPI_Comm mpiCommTemp;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(localPet) != ESMC_NULL_POINTER)
      *localPet = (*vm)->getLocalPet();
    if (ESMC_NOT_PRESENT_FILTER(currentSsiPe) != ESMC_NULL_POINTER)
      *currentSsiPe = (*vm)->getCurrentSsiPe();
    if (ESMC_NOT_PRESENT_FILTER(petCount) != ESMC_NULL_POINTER)
      *petCount = (*vm)->getPetCount();
    if (ESMC_NOT_PRESENT_FILTER(peCount) != ESMC_NULL_POINTER){
      int npets = (*vm)->getNpets();
      *peCount = 0; // reset
      for (int i=0; i<npets; i++)
        *peCount += (*vm)->getNcpet(i);
    }
    if (ESMC_NOT_PRESENT_FILTER(ssiCount) != ESMC_NULL_POINTER)
      *ssiCount = (*vm)->getSsiCount();
    if (ESMC_NOT_PRESENT_FILTER(ssiMinPetCount) != ESMC_NULL_POINTER)
      *ssiMinPetCount = (*vm)->getSsiMinPetCount();
    if (ESMC_NOT_PRESENT_FILTER(ssiMaxPetCount) != ESMC_NULL_POINTER)
      *ssiMaxPetCount = (*vm)->getSsiMaxPetCount();
    if (ESMC_NOT_PRESENT_FILTER(ssiLocalPetCount) != ESMC_NULL_POINTER)
      *ssiLocalPetCount = (*vm)->getSsiLocalPetCount();
    if (ESMC_NOT_PRESENT_FILTER(mpiCommunicator) != ESMC_NULL_POINTER){
      mpiCommTemp = (*vm)->getMpi_c();
#ifdef ESMF_DONT_HAVE_MPI_COMM_C2F
      *mpiCommunicator = (int)(mpiCommTemp);
#else
      *mpiCommunicator = (int)MPI_Comm_c2f(mpiCommTemp);
#endif
    }
    if (ESMC_NOT_PRESENT_FILTER(pthreadsEnabledFlag) != ESMC_NULL_POINTER){
      if ((*vm)->isPthreadsEnabled())
        *pthreadsEnabledFlag = ESMF_TRUE;
      else
        *pthreadsEnabledFlag = ESMF_FALSE;
    }
    if (ESMC_NOT_PRESENT_FILTER(openMPEnabledFlag) != ESMC_NULL_POINTER){
      if ((*vm)->isOpenMPEnabled())
        *openMPEnabledFlag = ESMF_TRUE;
      else
        *openMPEnabledFlag = ESMF_FALSE;
    }
    if (ESMC_NOT_PRESENT_FILTER(ssiSharedMemoryEnabledFlag)
      != ESMC_NULL_POINTER){
      if ((*vm)->isSsiSharedMemoryEnabled())
        *ssiSharedMemoryEnabledFlag = ESMF_TRUE;
      else
        *ssiSharedMemoryEnabledFlag = ESMF_FALSE;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmgetmpicommnull)(int *mpiCommunicator, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetmpicommnull()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    MPI_Comm mpiCommTemp;
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(mpiCommunicator) != ESMC_NULL_POINTER){
      mpiCommTemp = MPI_COMM_NULL;
#ifdef ESMF_DONT_HAVE_MPI_COMM_C2F
      *mpiCommunicator = (int)(mpiCommTemp);
#else
      *mpiCommunicator = (int)MPI_Comm_c2f(mpiCommTemp);
#endif
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmgetpetlocalinfo)(ESMCI::VM **vm, int *pet, int *peCount, 
    int *accDeviceCount, int *ssiId, int *threadCount, int *threadId,
    int *vas, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetpetlocalinfo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    // check that pet is within bounds
    int petCount = (*vm)->getPetCount();
    if (*pet < 0 || *pet >= petCount){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        "- provided pet id is out of range", ESMC_CONTEXT, rc);
      return;
    }
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(peCount) != ESMC_NULL_POINTER)
      *peCount = (*vm)->getNcpet(*pet);
    if (ESMC_NOT_PRESENT_FILTER(accDeviceCount) != ESMC_NULL_POINTER)
      *accDeviceCount = (*vm)->getNadevs(*pet);
    if (ESMC_NOT_PRESENT_FILTER(ssiId) != ESMC_NULL_POINTER)
      *ssiId = (*vm)->getSsi(*pet);
    if (ESMC_NOT_PRESENT_FILTER(threadCount) != ESMC_NULL_POINTER)
      *threadCount = (*vm)->getNthreads(*pet);
    if (ESMC_NOT_PRESENT_FILTER(threadId) != ESMC_NULL_POINTER)
      *threadId = (*vm)->getTid(*pet);
    if (ESMC_NOT_PRESENT_FILTER(vas) != ESMC_NULL_POINTER)
      *vas = (*vm)->getVas(*pet);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmgetvmid)(ESMCI::VM **vm, ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    *vmid = (*vm)->getVMId(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmprint)(ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    if (ESMC_LogDefault.MsgFoundError((*vm)->print(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmprintmatchtable)(ESMCI::VM **vm){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmprintmatchtable()"
    int localrc = ESMC_RC_NOT_IMPL;
    int *rc = &localrc;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    (*vm)->printMatchTable();
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
  }

  void FTN_X(c_esmc_vmvalidate)(ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    if (ESMC_LogDefault.MsgFoundError((*vm)->validate(),
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmrecv)(ESMCI::VM **vm, void *message, int *size,
    int *source, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->recv(message, *size, *source);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmrecvnb)(ESMCI::VM **vm, void *message, int *size, 
    int *source, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvnb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(commhandle, rc)
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->recv(message, *size, *source,
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmreduce)(ESMCI::VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind_Flag *dtk, ESMC_Operation *op, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmreduce()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc,"Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->reduce(input, output, *count, vmt, (vmOp)(*op), *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmscatter)(ESMCI::VM **vm, void *input, void *output,
    int *size, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatter()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->scatter(input, output, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmscatternb)(ESMCI::VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatternb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(commhandle, rc)
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->scatter(input, output, *size, *root, 
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmscatterv)(ESMCI::VM **vm, void *sendData, int *sendCounts,
    int *sendOffsets, void *recvData, int *recvCount, ESMC_TypeKind_Flag *dtk, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatterv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMCI::VMK types
    vmType vmt;
    switch (*dtk){
    case ESMC_TYPEKIND_I4:
      vmt = vmI4;
      break;
    case ESMC_TYPEKIND_I8:
      vmt = vmI8;
      break;
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.MsgFoundError(localrc, "Unsupported data type.",
      ESMC_CONTEXT, rc)) return;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->scatterv(sendData, sendCounts, sendOffsets, recvData,
      *recvCount, vmt, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmsend)(ESMCI::VM **vm, void *message, int *size, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsend()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->send(message, *size, *dest);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmsendnb)(ESMCI::VM **vm, void *message, int *size,
    int *dest, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendnb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(commhandle, rc)
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->send(message, *size, *dest, 
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmsendrecv)(ESMCI::VM **vm, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->sendrecv(sendData, *sendSize, *dst, recvData,
      *recvSize, *src);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmsendrecvnb)(ESMCI::VM **vm, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, void **commhandle, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecvnb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(commhandle, rc)
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->sendrecv(sendData, *sendSize, *dst, recvData,
      *recvSize, *src, (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmcommwait)(ESMCI::VM **vm, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmcommwait()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(commhandle, rc)
    localrc = (*vm)->commwait((ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmcommqueuewait)(ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmcommqueuewait()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    (*vm)->commqueuewait();
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN_X(c_esmc_vmwtime)(ESMC_R8 *time, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VMK::wtime(time);
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN_X(c_esmc_vmwtimedelay)(ESMC_R8 *delay, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VMK::wtimedelay(*delay);
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN_X(c_esmc_vmthreadbarrier)(ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmthreadbarrier()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    localrc = (*vm)->threadbarrier();
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.MsgFoundError(ESMF_RC_INTNRL_BAD, message, ESMC_CONTEXT,
        rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmgetcurrent)(ESMCI::VM **vm, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrent()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    *vm = ESMCI::VM::getCurrent(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vminitializeprempi)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vminitializeprempi()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VMK::InitPreMPI();
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vminitialize)(ESMCI::VM **vm, int *mpiCommunicator, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vminitialize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    MPI_Comm localMpiComm;
    if (ESMC_NOT_PRESENT_FILTER(mpiCommunicator)){
      int localMpiCommFortran = *mpiCommunicator;
#ifdef ESMF_DONT_HAVE_MPI_COMM_F2C
      localMpiComm = (MPI_Comm)localMpiCommFortran; // best guess in this case
#else
      localMpiComm = MPI_Comm_f2c(localMpiCommFortran);
#endif
    }else
      localMpiComm = MPI_COMM_WORLD;  // this is the default
    if (vm==NULL){
      *rc = ESMC_RC_INTNRL_BAD;
      return;
    }
    *vm = ESMCI::VM::initialize(localMpiComm, &localrc);
    // Cannot use LogErr here because LogErr initializes _after_ VM
    if (rc!=NULL) *rc = localrc;
  }

  void FTN_X(c_esmc_vmfinalize)(ESMC_Logical *keepMpiFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmfinalize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM::finalize(ESMC_NOT_PRESENT_FILTER(keepMpiFlag), &localrc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    if (rc!=NULL) *rc = localrc;
  }

  void FTN_X(c_esmc_vmabort)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmabort()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM::abort(&localrc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    if (rc!=NULL) *rc = localrc;
  }

  void FTN_X(c_esmc_isinitialized)(ESMC_Logical *isInitialized, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_isinitialized()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    bool flag = ESMCI::VM::isInitialized(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    *isInitialized = flag ? ESMF_TRUE : ESMF_FALSE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_isfinalized)(ESMC_Logical *isFinalized, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_isfinalized()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    bool flag = ESMCI::VM::isFinalized(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    *isFinalized = flag ? ESMF_TRUE : ESMF_FALSE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmshutdown)(ESMCI::VM **ptr_vmparent,
    ESMCI::VMPlan **ptr_vmplan,
    void **vm_info, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmshutdown()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr_vmparent, rc)
    ESMCI_NULL_CHECK_PRC(*ptr_vmparent, rc)
    ESMCI_NULL_CHECK_PRC(ptr_vmplan, rc)
    ESMCI_NULL_CHECK_PRC(vm_info, rc)
    (*ptr_vmparent)->shutdown(*ptr_vmplan, *vm_info, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMCI::VMPlan interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN_X(c_esmc_vmplanconstruct)(ESMCI::VMPlan **ptr, ESMCI::VM **vm,
    int *npetlist, int *petlist, ESMC_ContextFlag *contextflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanconstruct()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    (*ptr) = new ESMCI::VMPlan;
    if (*contextflag==ESMF_CHILD_IN_PARENT_VM)
      (*ptr)->vmkplan_useparentvm(**vm);
    else if (*npetlist > 0){
      (*ptr)->vmkplan_minthreads(**vm, 1, (int*)petlist, *npetlist);
      (*ptr)->vmkplan_mpi_c_part(**vm);
    }else{
      (*ptr)->vmkplan_minthreads(**vm, 1);
      (*ptr)->vmkplan_mpi_c_part(**vm);
    }
    // set the nothreadflag because this is the default for new VMs
    (*ptr)->openmphandling = 0; // override what vmkplan_minthreads() above set
    (*ptr)->nothreadflag = 1; // override what vmkplan_minthreads() above set
    //debug: (*ptr)->vmkplan_print();
    // Allocate as many ESMCI::VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMCI::VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMCI::VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMCI::VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMCI::VM;
      (*ptr)->myvmachs[i] = static_cast<ESMCI::VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmplandestruct)(ESMCI::VMPlan **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplandestruct()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(*ptr, rc)
    // Do garbage collection on this PET's VM instances that were allocated
    for (int i=0; i<(*ptr)->nspawn; i++)
      delete (*ptr)->myvms[i];
    delete [] (*ptr)->myvms;
    delete [] (*ptr)->myvmachs;
    // Now delete the actual ESMCI::VMPlan object
    delete (*ptr);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmplansetminstacksize)(ESMCI::VMPlan **ptr,
    int *minStackSize, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplansetminstacksize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // Sort out the non-present F90 optional arguments.
    minStackSize = ESMC_NOT_PRESENT_FILTER(minStackSize);
    int loc_minStackSize = VM_PTHREAD_STACKSIZE_USER; 
    if ((void*)minStackSize != ESMC_NULL_POINTER)
      loc_minStackSize = *minStackSize;
    // set the minStackSize
    (*ptr)->minStackSize = (size_t)loc_minStackSize;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
   
  void FTN_X(c_esmc_vmplansetopenmp)(ESMCI::VMPlan **ptr,
    int *openMpHandling, int *openMpNumThreads, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplansetopenmp()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    // Sort out the non-present F90 optional arguments.
    openMpHandling = ESMC_NOT_PRESENT_FILTER(openMpHandling);
    int loc_openMpHandling = 3; // default: pin OpenMP threads
    if ((void*)openMpHandling != ESMC_NULL_POINTER)
      loc_openMpHandling = *openMpHandling;
    openMpNumThreads = ESMC_NOT_PRESENT_FILTER(openMpNumThreads);
    int loc_openMpNumThreads = -1; // default: local peCount
    if ((void*)openMpNumThreads != ESMC_NULL_POINTER)
      loc_openMpNumThreads = *openMpNumThreads;
    // validate argument consistency
    if (loc_openMpHandling==0 && loc_openMpNumThreads >= 0){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP, "Incompatible "
        "openMpNumThreads setting for requested openMpHandling.",
        ESMC_CONTEXT, rc);
      return; // bail out
    }
    // set the openMpHandling
    (*ptr)->openmphandling = loc_openMpHandling;
    // set the openMpNumThreads
    (*ptr)->openmpnumthreads = loc_openMpNumThreads;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
   
  void FTN_X(c_esmc_vmplanmaxpes)(ESMCI::VMPlan **ptr, ESMCI::VM **vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanmaxpes()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    // Sort out the non-present F90 optional arguments. 
    max = ESMC_NOT_PRESENT_FILTER(max);
    pref_intra_process = ESMC_NOT_PRESENT_FILTER(pref_intra_process);
    pref_intra_ssi = ESMC_NOT_PRESENT_FILTER(pref_intra_ssi);
    pref_inter_ssi = ESMC_NOT_PRESENT_FILTER(pref_inter_ssi);
    petlist = ESMC_NOT_PRESENT_FILTER(petlist);
    // Done sorting out non-present F90 optional arguments.
    int maxx = 0; 
    if ((void*)max != ESMC_NULL_POINTER)
      maxx = *max;
    int ppref_intra_process = -1;
    if ((void*)pref_intra_process != ESMC_NULL_POINTER)
      ppref_intra_process = *pref_intra_process;
    int ppref_intra_ssi = -1;
    if ((void*)pref_intra_ssi != ESMC_NULL_POINTER)
      ppref_intra_ssi = *pref_intra_ssi;
    int ppref_inter_ssi = -1;
    if ((void*)pref_inter_ssi != ESMC_NULL_POINTER)
      ppref_inter_ssi = *pref_inter_ssi;
    // Do garbage collection on this PET's VM instances that were allocated
    for (int i=0; i<(*ptr)->nspawn; i++)
      delete (*ptr)->myvms[i];
    delete [] (*ptr)->myvms;
    delete [] (*ptr)->myvmachs;
    // Now define a new vmplan
    localrc = (*ptr)->vmkplan_maxcores(**vm, maxx, (int*)petlist,
      *npetlist, ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    if (localrc) localrc = ESMF_FAILURE;
    else localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.MsgFoundError(localrc,"- this ESMF library"
      " was compiled with ESMF_PTHREADS=OFF and thus does not support"
      " ESMF-threading!", ESMC_CONTEXT, rc)) return;
    //debug: (*ptr)->vmkplan_print();
    // Allocate as many ESMCI::VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMCI::VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMCI::VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMCI::VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMCI::VM;
      (*ptr)->myvmachs[i] = static_cast<ESMCI::VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
       
  void FTN_X(c_esmc_vmplanmaxthreads)(ESMCI::VMPlan **ptr, ESMCI::VM **vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanmaxthreads()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    // Sort out the non-present F90 optional arguments. 
    max = ESMC_NOT_PRESENT_FILTER(max);
    pref_intra_process = ESMC_NOT_PRESENT_FILTER(pref_intra_process);
    pref_intra_ssi = ESMC_NOT_PRESENT_FILTER(pref_intra_ssi);
    pref_inter_ssi = ESMC_NOT_PRESENT_FILTER(pref_inter_ssi);
    petlist = ESMC_NOT_PRESENT_FILTER(petlist);
    // Done sorting out non-present F90 optional arguments.
    int maxx = 0; 
    if ((void*)max != ESMC_NULL_POINTER)
      maxx = *max;
    int ppref_intra_process = -1;
    if ((void*)pref_intra_process != ESMC_NULL_POINTER)
      ppref_intra_process = *pref_intra_process;
    int ppref_intra_ssi = -1;
    if ((void*)pref_intra_ssi != ESMC_NULL_POINTER)
      ppref_intra_ssi = *pref_intra_ssi;
    int ppref_inter_ssi = -1;
    if ((void*)pref_inter_ssi != ESMC_NULL_POINTER)
      ppref_inter_ssi = *pref_inter_ssi;
    // Do garbage collection on this PET's VM instances that were allocated
    for (int i=0; i<(*ptr)->nspawn; i++)
      delete (*ptr)->myvms[i];
    delete [] (*ptr)->myvms;
    delete [] (*ptr)->myvmachs;
    // Now define a new vmplan
    localrc = (*ptr)->vmkplan_maxthreads(**vm, maxx, (int*)petlist,
      *npetlist, ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    if (localrc) localrc = ESMF_FAILURE;
    else localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.MsgFoundError(localrc,"- this ESMF library"
      " was compiled with ESMF_PTHREADS=OFF and thus does not support"
      " ESMF-threading!", ESMC_CONTEXT, rc)) return;
    //debug: (*ptr)->vmkplan_print();
    // Allocate as many ESMCI::VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMCI::VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMCI::VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMCI::VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMCI::VM;
      (*ptr)->myvmachs[i] = static_cast<ESMCI::VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmc_vmplanminthreads)(ESMCI::VMPlan **ptr, ESMCI::VM **vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanminthreads()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(ptr, rc)
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    // Sort out the non-present F90 optional arguments. 
    max = ESMC_NOT_PRESENT_FILTER(max);
    pref_intra_process = ESMC_NOT_PRESENT_FILTER(pref_intra_process);
    pref_intra_ssi = ESMC_NOT_PRESENT_FILTER(pref_intra_ssi);
    pref_inter_ssi = ESMC_NOT_PRESENT_FILTER(pref_inter_ssi);
    petlist = ESMC_NOT_PRESENT_FILTER(petlist);
    // Done sorting out non-present F90 optional arguments.
    int maxx = 0; 
    if ((void*)max != ESMC_NULL_POINTER)
      maxx = *max;
    int ppref_intra_process = -1;
    if ((void*)pref_intra_process != ESMC_NULL_POINTER)
      ppref_intra_process = *pref_intra_process;
    int ppref_intra_ssi = -1;
    if ((void*)pref_intra_ssi != ESMC_NULL_POINTER)
      ppref_intra_ssi = *pref_intra_ssi;
    int ppref_inter_ssi = -1;
    if ((void*)pref_inter_ssi != ESMC_NULL_POINTER)
      ppref_inter_ssi = *pref_inter_ssi;
    // Do garbage collection on this PET's VM instances that were allocated
    for (int i=0; i<(*ptr)->nspawn; i++)
      delete (*ptr)->myvms[i];
    delete [] (*ptr)->myvms;
    delete [] (*ptr)->myvmachs;
    // Now define a new vmplan
    localrc = (*ptr)->vmkplan_minthreads(**vm, maxx, (int*)petlist,
      *npetlist, ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    if (localrc) localrc = ESMF_FAILURE;
    else localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.MsgFoundError(localrc,"- this ESMF library"
      " was compiled with ESMF_PTHREADS=OFF and thus does not support"
      " ESMF-threading!", ESMC_CONTEXT, rc)) return;
    //debug: (*ptr)->vmkplan_print();
    // Allocate as many ESMCI::VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMCI::VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMCI::VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMCI::VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMCI::VM;
      (*ptr)->myvmachs[i] = static_cast<ESMCI::VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMCI::VMId interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmgetcurrentid)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrentid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    *vmid = ESMCI::VM::getCurrentID(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmidcompare)(ESMCI::VMId **vmid1, ESMCI::VMId **vmid2,
    ESMC_Logical *result, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcompare()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid1, rc)
    ESMCI_NULL_CHECK_PRC(vmid2, rc)
    ESMCI_NULL_CHECK_PRC(result, rc)
    bool resultBool = ESMCI::VMIdCompare(*vmid1, *vmid2);
    *result = resultBool ? ESMF_TRUE : ESMF_FALSE;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmidcopy)(ESMCI::VMId **dest, ESMCI::VMId **source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcopy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(dest, rc)
    ESMCI_NULL_CHECK_PRC(source, rc)
    int localrc = ESMCI::VMIdCopy(*dest, *source);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmidcreate)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    *vmid = new ESMCI::VMId;              // allocate memory off the heap
    localrc = (*vmid)->create ();         // allocate VMId internal members
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmiddestroy)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmiddestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    localrc = (*vmid)->destroy ();      // free memory for internal members
    delete *vmid;                       // free memory for this VMId
    *vmid=NULL;
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmci_vmidget)(ESMCI::VMId **vmid, int *localID,
      char *key, int *rc, ESMCI_FortranStrLenArg key_len) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_vmidget()"
    // This method is primarily intended for use by VM unit tests.
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    localrc = (*vmid)->get(localID, key, key_len);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN_X(c_esmc_vmidlog)(ESMCI::VMId **vmid, char *prefix, 
    ESMC_LogMsgType_Flag *logMsgFlag, int *rc, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidlog()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    try{
      std::string prefixStr(prefix, prefix_l);
      (*vmid)->log(prefixStr, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmidprint)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    int localrc = (*vmid)->print();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // Flush before crossing language interface to ensure correct output order
    fflush(stdout);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmci_vmidset)(ESMCI::VMId **vmid, int *localID,
      char *key, int *rc, ESMCI_FortranStrLenArg key_len) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_vmidset()"
    // This method is primarily intended for use by VM unit tests.
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    localrc = (*vmid)->set(*localID, key, key_len);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmsendvmid)(ESMCI::VM **vm, ESMCI::VMId **vmid, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    localrc = (*vm)->sendVMId(*vmid, *dest);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmrecvvmid)(ESMCI::VM **vm, ESMCI::VMId **vmid, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    ESMCI_NULL_CHECK_PRC(*vmid, rc)
    localrc = (*vm)->recvVMId(*vmid, *source);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmalltoallvvmid)(ESMCI::VM **vm,
      ESMCI::VMId **sendData, int *sendCounts, int *sendOffsets,
      ESMCI::VMId **recvData, int *recvCounts, int *recvOffsets, 
      int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmalltoallvvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(sendData, rc)
    ESMCI_NULL_CHECK_PRC(recvData, rc)
    localrc = (*vm)->alltoallvVMId(sendData, sendCounts, sendOffsets,
      recvData, recvCounts, recvOffsets);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmallgathervvmid)(ESMCI::VM **vm,
      ESMCI::VMId **sendData, int *sendCount,
      ESMCI::VMId **recvData, int *recvCounts, int *recvOffsets,
      int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgathervvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(sendData, rc)
    ESMCI_NULL_CHECK_PRC(recvData, rc)
    localrc = (*vm)->allgathervVMId(sendData, *sendCount, recvData,  
      recvCounts, recvOffsets);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmbcastvmid)(ESMCI::VM **vm, ESMCI::VMId **vmid, int *count,
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbcastvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(vmid, rc)
    localrc = (*vm)->bcastVMId(vmid, *count, *root);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmtranslatevmid)(ESMCI::VM **vm, ESMCI::VMId **vmids,
    ESMCI::InterArray<int> *ids, ESMCI::InterArray<int> *rootVmIds, 
    int *rootVmIdCount, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmtranslatevmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    // test for NULL pointer via macro before calling any class methods
    ESMCI_NULL_CHECK_PRC(vm, rc)
    ESMCI_NULL_CHECK_PRC(*vm, rc)
    ESMCI_NULL_CHECK_PRC(vmids, rc)
    localrc = (*vm)->translateVMId(vmids, ids, rootVmIds, rootVmIdCount);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // VM utilities
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmpointerprint)(void *ptr){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmpointerprint()"
    // print the address to which this pointer points
    printf("c_esmc_vmpointerprint: %p\n", ptr);
    fflush (stdout);
  }
  
  void FTN_X(c_esmc_vmlogpointer)(void *ptr, char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmlogpointer()"
    std::string prefixStr(prefix, prefix_l);
    std::stringstream msg;
    msg << prefixStr << ptr;
    if (ptr) msg << " => " << *(void **)ptr;
    ESMC_LogDefault.Write(msg.str(), *logMsgFlag);
  }
  
  void FTN_X(c_pointerlog)(void **ptr, char *prefix, 
    ESMC_LogMsgType_Flag *logMsgFlag, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_pointerlog()"
    std::string prefixStr(prefix, prefix_l);
    std::stringstream msg;
    msg << prefixStr << *ptr;
    ESMC_LogDefault.Write(msg.str(), *logMsgFlag);
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Fortran entry point to automatic garbage collection on Component scope
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmaddfobject)(void **fobject, int *objectID){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmaddfobject()"
    int rc;
    ESMCI::VMId *vmID = ESMCI::VM::getCurrentID(&rc);  // get current vmID
    ESMCI::VM::addFObject(fobject, *objectID, vmID);
  }

  void FTN_X(c_esmc_vmrmfobject)(void **fobject){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrmfobject()"
    int rc;
    ESMCI::VM::rmFObject(fobject);
  }
 
  void FTN_X(c_esmc_vmrmobject)(ESMC_Base **base){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrmobject()"
    int rc;
    ESMCI::VM::rmObject(*base);
  }
    
  void FTN_X(c_esmc_vmvalidobject)(ESMC_Base **base, ESMC_Logical *validFlag,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmvalidobject()"
    if (ESMCI::VM::validObject(*base))
      *validFlag = ESMF_TRUE;
    else
      *validFlag = ESMF_FALSE;
    // return successfully
    *rc = ESMF_SUCCESS;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Fortran entry point to info about automatic garbage collection
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmgetcurrentgarbageinfo)(int *fobjCount, int *objCount,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrentgarbageinfo()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      // query the C++ layer
      ESMCI::VM::getCurrentGarbageInfo(fobjCount, objCount);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmlogcurrentgarbageinfo)(char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, int *rc, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmlogcurrentgarbageinfo()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
      ESMCI::VM::logGarbageInfo(prefixStr, true, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmloggarbageinfo)(char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, int *rc, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmloggarbageinfo()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
      ESMCI::VM::logGarbageInfo(prefixStr, false, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Fortran entry point to backtrace
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmlogbacktrace)(char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, int *rc, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmlogbacktrace()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
      ESMCI::VM::logBacktrace(prefixStr, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Fortran entry point to log and logSystem
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmlog)(ESMCI::VM **vm, char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, int *rc, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmlog()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
      (*vm)->log(prefixStr, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmc_vmlogsystem)(char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, int *rc, ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmlogsystem()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
      ESMCI::VM::logSystem(prefixStr, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Fortran entry point to PET specific memory info
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmc_vmgetmeminfo)(int *virtMemPet, int *physMemPet,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetmeminfo()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      // query the C++ layer
      ESMCI::VM::getMemInfo(virtMemPet, physMemPet);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
    
  void FTN_X(c_esmc_vmlogmeminfo)(char *prefix,
    ESMC_LogMsgType_Flag *logMsgFlag, ESMCI::LogErr **log, int *rc,
    ESMCI_FortranStrLenArg prefix_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmlogmeminfo()"
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    try{
      std::string prefixStr(prefix, prefix_l);
#if 0
// Currently the ESMCI side does not support custom Logs. Do not send this down!
      if (ESMC_NOT_PRESENT_FILTER(log) != ESMC_NULL_POINTER)
        ESMCI::VM::logMemInfo(prefixStr, *logMsgFlag, *log);
      else
#endif
        ESMCI::VM::logMemInfo(prefixStr, *logMsgFlag);
    }catch(int localrc){
      if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc))
        return; // bail out
    }catch(std::exception &x){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, x.what(), ESMC_CONTEXT,
        rc);
      return; // bail out
    }catch(...){
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD, "- Caught exception", 
        ESMC_CONTEXT, rc);
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Socket based VM entry point
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN_X(c_esmci_vmksocketserver)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_vmksocketserver()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = ESMCI::socketServer();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmci_vmksocketclient)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmci_vmksocketclient()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = ESMCI::socketClient();
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return; // bail out
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
#undef  ESMC_METHOD
}
