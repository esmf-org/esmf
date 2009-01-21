// $Id: ESMC_VM_F.C,v 1.76.2.5 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_VM_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMCI_F90Interface.h"
#include "ESMC_Start.h"
#include "ESMC_Base.h"

#include "ESMC_VM.h"

#include "ESMC_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
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
  // ESMC_VM interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_vmallfullreduce)(ESMCI::VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *rc){
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"Unsupported data type.",
      rc)) return;
    localrc = (*vm)->allfullreduce(input, output, *count, vmt, (vmOp)(*op));
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmallgather)(ESMCI::VM **vm, void *input, void *output, 
    int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgather()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->allgather(input, output, *size);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmallgathernb)(ESMCI::VM **vm, void *input, void *output, 
    int *size, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgathernb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->allgather(input, output, *size,
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmallgatherv)(ESMCI::VM **vm, void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets, ESMC_TypeKind *dtk, 
    int *rc){
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    localrc = (*vm)->allgatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmallreduce)(ESMCI::VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *rc){
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    localrc = (*vm)->allreduce(input, output, *count, vmt, (vmOp)(*op));
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmalltoallv)(ESMCI::VM **vm, void *sendData, int *sendCounts,
    int *sendOffsets, void *recvData, int *recvCounts, int *recvOffsets, 
    ESMC_TypeKind *dtk, int *rc){
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    localrc = (*vm)->alltoallv(sendData, sendCounts, sendOffsets, recvData,
      recvCounts, recvOffsets, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmbarrier)(ESMCI::VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbarrier()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->barrier();
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmbroadcast)(ESMCI::VM **vm, void *data, int *size, int *root,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->broadcast(data, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmbroadcastnb)(ESMCI::VM **vm, void *data, int *size,
    int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->broadcast(data, *size, *root,
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmgather)(ESMCI::VM **vm, void *input, void *output,
    int *size, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgather()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->gather(input, output, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgathernb)(ESMCI::VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgathernb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->gather(input, output, *size, *root, 
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgatherv)(ESMCI::VM **vm, void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets, ESMC_TypeKind *dtk, 
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    localrc = (*vm)->gatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmget)(ESMCI::VM **ptr, int *localPet, int *petCount, 
    int *peCount, int *mpiCommunicator, ESMC_Logical *supportPthreadsFlag,
    ESMC_Logical *supportOpenMPFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmget()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    MPI_Comm mpiCommTemp;
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(localPet) != ESMC_NULL_POINTER)
      *localPet = (*ptr)->getLocalPet();
    if (ESMC_NOT_PRESENT_FILTER(petCount) != ESMC_NULL_POINTER)
      *petCount = (*ptr)->getPetCount();
    if (ESMC_NOT_PRESENT_FILTER(peCount) != ESMC_NULL_POINTER){
      int npets = (*ptr)->getNpets();
      *peCount = 0; // reset
      for (int i=0; i<npets; i++)
        *peCount += (*ptr)->getNcpet(i);
    }
    if (ESMC_NOT_PRESENT_FILTER(mpiCommunicator) != ESMC_NULL_POINTER){
      mpiCommTemp = (*ptr)->getMpi_c();
#ifdef ESMF_DONT_HAVE_MPI_COMM_C2F
      *mpiCommunicator = (int)(mpiCommTemp);
#else
      *mpiCommunicator = (int)MPI_Comm_c2f(mpiCommTemp);
#endif
    }        
    if (ESMC_NOT_PRESENT_FILTER(supportPthreadsFlag) != ESMC_NULL_POINTER){
      if ((*ptr)->getSupportPthreads())
        *supportPthreadsFlag = ESMF_TRUE;
      else
        *supportPthreadsFlag = ESMF_FALSE;
    }
    if (ESMC_NOT_PRESENT_FILTER(supportOpenMPFlag) != ESMC_NULL_POINTER){
      if ((*ptr)->getSupportOpenMP())
        *supportOpenMPFlag = ESMF_TRUE;
      else
        *supportOpenMPFlag = ESMF_FALSE;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgetpetlocalinfo)(ESMCI::VM **ptr, int *pet, int *peCount, 
    int *ssiId, int *threadCount, int *threadId, int *vas, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetpetlocalinfo()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // check that pet is within bounds
    int petCount = (*ptr)->getPetCount();
    if (*pet < 0 || *pet >= petCount){
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
        "- provided pet id is out of range", rc);
      return;
    }
    // fill return values
    if (ESMC_NOT_PRESENT_FILTER(peCount) != ESMC_NULL_POINTER)
      *peCount = (*ptr)->getNcpet(*pet);
    if (ESMC_NOT_PRESENT_FILTER(ssiId) != ESMC_NULL_POINTER)
      *ssiId = (*ptr)->getSsiid(*pet);
    if (ESMC_NOT_PRESENT_FILTER(threadCount) != ESMC_NULL_POINTER)
      *threadCount = (*ptr)->getNthreads(*pet);
    if (ESMC_NOT_PRESENT_FILTER(threadId) != ESMC_NULL_POINTER)
      *threadId = (*ptr)->getTid(*pet);
    if (ESMC_NOT_PRESENT_FILTER(vas) != ESMC_NULL_POINTER)
      *vas = (*ptr)->getVas(*pet);
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmgetvmid)(ESMCI::VM **ptr, ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *vmid = (*ptr)->getVMId(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmprint)(ESMCI::VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    if (ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->print(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmvalidate)(ESMCI::VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmvalidate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    // Call into the actual C++ method wrapped inside LogErr handling
    if (ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->validate(),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc))) return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmrecv)(ESMCI::VM **ptr, void *message, int *size,
    int *source, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->recv(message, *size, *source);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmrecvnb)(ESMCI::VM **ptr, void *message, int *size, 
    int *source, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvnb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*ptr)->recv(message, *size, *source,
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmreduce)(ESMCI::VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *root, int *rc){
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"Unsupported data type.",
      rc)) return;
    localrc = (*vm)->reduce(input, output, *count, vmt, (vmOp)(*op), *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmscatter)(ESMCI::VM **vm, void *input, void *output,
    int *size, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatter()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->scatter(input, output, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmscatternb)(ESMCI::VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatternb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->scatter(input, output, *size, *root, 
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmscatterv)(ESMCI::VM **vm, void *sendData, int *sendCounts,
    int *sendOffsets, void *recvData, int *recvCount, ESMC_TypeKind *dtk, 
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
    case ESMC_TYPEKIND_R4:
      vmt = vmR4;
      break;
    case ESMC_TYPEKIND_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMC_RC_ARG_BAD;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    localrc = (*vm)->scatterv(sendData, sendCounts, sendOffsets, recvData,
      *recvCount, vmt, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmsend)(ESMCI::VM **ptr, void *message, int *size, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsend()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->send(message, *size, *dest);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendnb)(ESMCI::VM **ptr, void *message, int *size,
    int *dest, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendnb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*ptr)->send(message, *size, *dest, 
      (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendrecv)(ESMCI::VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecv()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->sendrecv(sendData, *sendSize, *dst, recvData,
      *recvSize, *src);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendrecvnb)(ESMCI::VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, void **commhandle, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecvnb()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*ptr)->sendrecv(sendData, *sendSize, *dst, recvData,
      *recvSize, *src, (ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmcommwait)(ESMCI::VM **ptr, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmcommwait()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->commwait((ESMCI::VMK::commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmcommqueuewait)(ESMCI::VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmcommqueuewait()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    (*ptr)->commqueuewait();
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN(c_esmc_vmwtime)(ESMC_R8 *time, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VMK::wtime(time);
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN(c_esmc_vmwtimedelay)(ESMC_R8 *delay, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VMK::wtimedelay(*delay);
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN(c_esmc_vmthreadbarrier)(ESMCI::VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmthreadbarrier()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->threadbarrier();
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
      return;
    }
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgetcurrent)(ESMCI::VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrent()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *ptr = ESMCI::VM::getCurrent(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vminitialize)(ESMCI::VM **ptr, int *mpiCommunicator, int *rc){
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
    *ptr = ESMCI::VM::initialize(localMpiComm, &localrc);
    // Cannot use LogErr here because LogErr initializes _after_ VM
    if (rc!=NULL) *rc = localrc;
  }

  void FTN(c_esmc_vmfinalize)(ESMC_Logical *keepMpiFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmfinalize()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM::finalize(ESMC_NOT_PRESENT_FILTER(keepMpiFlag), &localrc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    if (rc!=NULL) *rc = localrc;
  }

  void FTN(c_esmc_vmabort)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmabort()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VM::abort(&localrc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    if (rc!=NULL) *rc = localrc;
  }

  void FTN(c_esmc_vmshutdown)(ESMCI::VM **ptr_vmparent,
    ESMCI::VMPlan **ptr_vmplan,
    void **vm_info, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmshutdown()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    (*ptr_vmparent)->shutdown(*ptr_vmplan, *vm_info, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMCI::VMPlan interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_vmplanconstruct)(ESMCI::VMPlan **ptr, ESMCI::VM **ptr_vm,
    int *npetlist, int *petlist, ESMC_ContextFlag *contextflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanconstruct()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    (*ptr) = new ESMCI::VMPlan;
    if (*contextflag==ESMF_CHILD_IN_PARENT_VM)
      (*ptr)->vmkplan_useparentvm(**ptr_vm);
    else if (*npetlist > 0){
      (*ptr)->vmkplan_minthreads(**ptr_vm, 1, (int*)petlist, *npetlist);
      (*ptr)->vmkplan_mpi_c_part(**ptr_vm);
    }else{
      (*ptr)->vmkplan_minthreads(**ptr_vm, 1);
      (*ptr)->vmkplan_mpi_c_part(**ptr_vm);
    }
    // set the nothreadflag because this is the default for new VMs
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

  void FTN(c_esmc_vmplandestruct)(ESMCI::VMPlan **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplandestruct()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
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
  
  void FTN(c_esmc_vmplanmaxpes)(ESMCI::VMPlan **ptr, ESMCI::VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanmaxpes()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
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
    localrc = (*ptr)->vmkplan_maxcores(**ptr_vm, maxx, (int*)petlist,
      *npetlist, ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    if (localrc) localrc = ESMF_FAILURE;
    else localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"- this ESMF library"
      " was compiled with ESMF_PTHREADS=OFF and thus does not support"
      " ESMF-threading!", rc)) return;
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
       
  void FTN(c_esmc_vmplanmaxthreads)(ESMCI::VMPlan **ptr, ESMCI::VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanmaxthreads()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
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
    localrc = (*ptr)->vmkplan_maxthreads(**ptr_vm, maxx, (int*)petlist,
      *npetlist, ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    if (localrc) localrc = ESMF_FAILURE;
    else localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"- this ESMF library"
      " was compiled with ESMF_PTHREADS=OFF and thus does not support"
      " ESMF-threading!", rc)) return;
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
  
  void FTN(c_esmc_vmplanminthreads)(ESMCI::VMPlan **ptr, ESMCI::VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanminthreads()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
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
    localrc = (*ptr)->vmkplan_minthreads(**ptr_vm, maxx, (int*)petlist,
      *npetlist, ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    if (localrc) localrc = ESMF_FAILURE;
    else localrc = ESMF_SUCCESS;
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"- this ESMF library"
      " was compiled with ESMF_PTHREADS=OFF and thus does not support"
      " ESMF-threading!", rc)) return;
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

  void FTN(c_esmc_vmgetcurrentid)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrentid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *vmid = ESMCI::VM::getCurrentID(&localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmidcompare)(ESMCI::VMId **vmid1, ESMCI::VMId **vmid2,
    ESMC_Logical *result, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcompare()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    *result = ESMCI::VMIdCompare(*vmid1, *vmid2);
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN(c_esmc_vmidprint)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidprint()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    ESMCI::VMIdPrint(*vmid);
    if (rc!=NULL) *rc = ESMF_SUCCESS; // TODO: finish error handling
  }

  void FTN(c_esmc_vmidcreate)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcreate()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    *vmid = new ESMCI::VMId;              // allocate memory off the heap
    **vmid = ESMCI::VMIdCreate(&localrc); // allocate VMId internal members
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmiddestroy)(ESMCI::VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmiddestroy()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    ESMCI::VMIdDestroy(*vmid, &localrc);  // free memory for internal members
    delete *vmid;                       // free memory for this VMId
    *vmid=NULL;
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendvmid)(ESMCI::VM **ptr, ESMCI::VMId **vmid, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->sendVMId(*vmid, *dest);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmrecvvmid)(ESMCI::VM **ptr, ESMCI::VMId **vmid, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvvmid()"
    // Initialize return code; assume routine not implemented
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->recvVMId(*vmid, *source);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return;
    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }
    
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // VM utilities
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  void FTN(c_esmc_vmpointerprint)(void *ptr){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmpointerprint()"
    // print the address to which this pointer points
    printf("c_esmc_vmpointerprint: %p\n", ptr);
  }
  
#undef  ESMC_METHOD
}
