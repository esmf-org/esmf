// $Id: ESMC_VM_F.C,v 1.70 2007/05/11 02:41:59 rosalind Exp $
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
  
  void FTN(c_esmc_vmallfullreduce)(ESMC_VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallfullreduce()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_allfullreduce(input, output, *count, vmt, (vmOp)(*op));
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmallgather)(ESMC_VM **vm, void *input, void *output, 
    int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgather()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->vmk_allgather(input, output, *size);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmallgathernb)(ESMC_VM **vm, void *input, void *output, 
    int *size, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgathernb()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->vmk_allgather(input, output, *size,
      (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmallgatherv)(ESMC_VM **vm, void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets, ESMC_TypeKind *dtk, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgatherv()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_allgatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmallreduce)(ESMC_VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallreduce()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_allreduce(input, output, *count, vmt, (vmOp)(*op));
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmalltoallv)(ESMC_VM **vm, void *sendData, int *sendCounts,
    int *sendOffsets, void *recvData, int *recvCounts, int *recvOffsets, 
    ESMC_TypeKind *dtk, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmalltoallv()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_alltoallv(sendData, sendCounts, sendOffsets, recvData,
      recvCounts, recvOffsets, vmt);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmbarrier)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbarrier()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*ptr)->vmk_barrier();
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmbroadcast)(ESMC_VM **vm, void *data, int *size, int *root,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->vmk_broadcast(data, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmbroadcastnb)(ESMC_VM **vm, void *data, int *size, int *root,
    void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->vmk_broadcast(data, *size, *root,
      (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmgather)(ESMC_VM **vm, void *input, void *output, int *size, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgather()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    localrc = (*vm)->vmk_gather(input, output, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgathernb)(ESMC_VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgathernb()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->vmk_gather(input, output, *size, *root, 
      (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgatherv)(ESMC_VM **vm, void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets, ESMC_TypeKind *dtk, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgatherv()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_gatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmget)(ESMC_VM **ptr, int *localPet, int *petCount, 
    int *peCount, int *mpiCommunicator, ESMC_Logical *okOpenMpFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmget()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    MPI_Comm mpiCommTemp;
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_VMGet(
      ESMC_NOT_PRESENT_FILTER(localPet), 
      ESMC_NOT_PRESENT_FILTER(petCount), 
      ESMC_NOT_PRESENT_FILTER(peCount),
      &mpiCommTemp, 
      ESMC_NOT_PRESENT_FILTER(okOpenMpFlag)),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
    // deal with the MPI communicator  
    if (ESMC_NOT_PRESENT_FILTER(mpiCommunicator) != ESMC_NULL_POINTER){
#ifdef ESMF_DONT_HAVE_MPI_COMM_C2F
      *mpiCommunicator = (int)(mpiCommTemp);
#else
      *mpiCommunicator = (int)MPI_Comm_c2f(mpiCommTemp);
#endif
    }
  }

  void FTN(c_esmc_vmgetpetlocalinfo)(ESMC_VM **ptr, int *pet, int *peCount, 
    int *ssiId, int *threadCount, int *threadId, int *vas, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetpetlocalinfo()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_VMGetPETLocalInfo(
      *pet, 
      ESMC_NOT_PRESENT_FILTER(peCount), 
      ESMC_NOT_PRESENT_FILTER(ssiId), 
      ESMC_NOT_PRESENT_FILTER(threadCount), 
      ESMC_NOT_PRESENT_FILTER(threadId), 
      ESMC_NOT_PRESENT_FILTER(vas)),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_vmgetvmid)(ESMC_VM **ptr, ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetvmid()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    int localrc = ESMC_RC_NOT_IMPL;
    *vmid = (*ptr)->ESMC_VMGetVMId(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmprint)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmprint()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    int localrc = ESMC_RC_NOT_IMPL;
    (*ptr)->ESMC_VMPrint(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmrecv)(ESMC_VM **ptr, void *message, int *size, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecv()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    localrc = (*ptr)->vmk_recv(message, *size, *source);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmrecvnb)(ESMC_VM **ptr, void *message, int *size, 
    int *source, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvnb()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;
    *commhandle = NULL; // reset the commhandle
    localrc = (*ptr)->vmk_recv(message, *size, *source, 
      (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmreduce)(ESMC_VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmreduce()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_reduce(input, output, *count, vmt, (vmOp)(*op), *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmscatter)(ESMC_VM **vm, void *input, void *output, int *size,
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatter()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    localrc = (*vm)->vmk_scatter(input, output, *size, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmscatternb)(ESMC_VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatternb()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    *commhandle = NULL; // reset the commhandle
    localrc = (*vm)->vmk_scatter(input, output, *size, *root, 
      (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmscatterv)(ESMC_VM **vm, void *sendData, int *sendCounts,
    int *sendOffsets, void *recvData, int *recvCount, ESMC_TypeKind *dtk, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatterv()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
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
    localrc = (*vm)->vmk_scatterv(sendData, sendCounts, sendOffsets, recvData,
      *recvCount, vmt, *root);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmsend)(ESMC_VM **ptr, void *message, int *size, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsend()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    localrc = (*ptr)->vmk_send(message, *size, *dest);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendnb)(ESMC_VM **ptr, void *message, int *size, int *dest,
    void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendnb()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    *commhandle = NULL; // reset the commhandle
    localrc = (*ptr)->vmk_send(message, *size, *dest, 
      (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendrecv)(ESMC_VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecv()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    localrc = (*ptr)->vmk_sendrecv(sendData, *sendSize, *dst, recvData,
      *recvSize, *src);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsendrecvnb)(ESMC_VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, void **commhandle, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecvnb()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    *commhandle = NULL; // reset the commhandle
    localrc = (*ptr)->vmk_sendrecv(sendData, *sendSize, *dst, recvData,
      *recvSize, *src, (vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmcommwait)(ESMC_VM **ptr, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmcommwait()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    localrc = (*ptr)->vmk_commwait((vmk_commhandle **)commhandle);
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmcommqueuewait)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmcommqueuewait()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    (*ptr)->vmk_commqueuewait();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmwtime)(ESMC_R8 *time, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    vmk_wtime(time);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmwtimedelay)(ESMC_R8 *delay, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    vmk_wtimedelay(*delay);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmthreadbarrier)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmthreadbarrier()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     int localrc = ESMC_RC_NOT_IMPL;

    localrc = (*ptr)->vmk_threadbarrier();
    if (localrc){
      char *message = new char[160];
      sprintf(message, "VMKernel/MPI error #%d\n", localrc);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMF_RC_INTNRL_BAD, message, rc);
      delete [] message;
    }else
      *rc = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmgetcurrent)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrent()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

    *ptr = ESMC_VMGetCurrent(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vminitialize)(ESMC_VM **ptr, int *mpiCommunicator, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vminitialize()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

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
    *ptr = ESMC_VMInitialize(localMpiComm, &localrc);
    //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    // Cannot use LogErr here because LogErr initializes _after_ VM
    *rc = localrc;
  }

  void FTN(c_esmc_vmfinalize)(ESMC_Logical *keepMpiFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmfinalize()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
     localrc = ESMC_RC_NOT_IMPL;

    ESMC_VMFinalize(ESMC_NOT_PRESENT_FILTER(keepMpiFlag), &localrc);
    //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    *rc = localrc;
  }

  void FTN(c_esmc_vmabort)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmabort()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    ESMC_VMAbort(&localrc);
    //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    *rc = localrc;
  }

  void FTN(c_esmc_vmshutdown)(ESMC_VM **ptr_vmparent, ESMC_VMPlan **ptr_vmplan,
    void **vm_info, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmshutdown()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    (*ptr_vmparent)->ESMC_VMShutdown(*ptr_vmplan, *vm_info, &localrc);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc))
      return; // bail out on error
    
    if (rc) *rc = ESMF_SUCCESS; // Return successfully
  }
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_VMPlan interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  void FTN(c_esmc_vmplanconstruct)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *npetlist, int *petlist, ESMC_ContextFlag *contextflag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanconstruct()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    (*ptr) = new ESMC_VMPlan;
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
    (*ptr)->nothreadflag = 1;
    //debug: (*ptr)->vmkplan_print();
    // Allocate as many ESMC_VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMC_VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMC_VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMC_VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMC_VM;
      (*ptr)->myvmachs[i] = static_cast<ESMC_VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
    *rc = ESMF_SUCCESS;   // TODO: error handling, catching allocation failure
  }

  void FTN(c_esmc_vmplandestruct)(ESMC_VMPlan **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplandestruct()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    // Do garbage collection on this PET's VM instances that were allocated
    for (int i=0; i<(*ptr)->nspawn; i++)
      delete (*ptr)->myvms[i];
    delete [] (*ptr)->myvms;
    delete [] (*ptr)->myvmachs;
    // Now delete the actual ESMC_VMPlan object
    delete (*ptr);
    *rc = ESMF_SUCCESS;   // TODO: error handling, catching allocation failure
  }
  
  void FTN(c_esmc_vmplanmaxpes)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanmaxpes()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
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
    // Allocate as many ESMC_VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMC_VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMC_VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMC_VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMC_VM;
      (*ptr)->myvmachs[i] = static_cast<ESMC_VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
  }
       
  void FTN(c_esmc_vmplanmaxthreads)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanmaxthreads()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
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
    // Allocate as many ESMC_VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMC_VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMC_VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMC_VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMC_VM;
      (*ptr)->myvmachs[i] = static_cast<ESMC_VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
  }
  
  void FTN(c_esmc_vmplanminthreads)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanminthreads()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
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
    // Allocate as many ESMC_VM instances as this PET will spawn 
    // and hold the information in the public portion of ESMC_VMPlan
    (*ptr)->nspawn = (*ptr)->vmkplan_nspawn(); // determine spawned PETs
    (*ptr)->myvms = new ESMC_VM*[(*ptr)->nspawn];
    (*ptr)->myvmachs = new ESMC_VMK*[(*ptr)->nspawn];
    for (int i=0; i<(*ptr)->nspawn; i++){
      (*ptr)->myvms[i] = new ESMC_VM;
      (*ptr)->myvmachs[i] = static_cast<ESMC_VMK *>((*ptr)->myvms[i]);
    }
    (*ptr)->vmkplan_myvms((*ptr)->myvmachs); // use pointer array inside
  }
  
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_VMId interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  void FTN(c_esmc_vmgetcurrentid)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrentid()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    *vmid = ESMC_VMGetCurrentID(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmidcompare)(ESMC_VMId **vmid1, ESMC_VMId **vmid2,
    ESMC_Logical *result, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcompare()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    *result = ESMC_VMIdCompare(*vmid1, *vmid2);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmidprint)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidprint()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    ESMC_VMIdPrint(*vmid);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmidcreate)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcreate()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    *vmid = new ESMC_VMId;              // allocate memory off the heap
    **vmid = ESMC_VMIdCreate(&localrc); // allocate memory for internal members
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmiddestroy)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmiddestroy()"
    int localrc;
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;
    localrc = ESMC_RC_NOT_IMPL;

    ESMC_VMIdDestroy(*vmid, &localrc);  // free memory for internal members
    delete *vmid;                       // free memory for this VMId
    *vmid=NULL;
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }


  void FTN(c_esmc_vmsendvmid)(ESMC_VM **ptr, ESMC_VMId **vmid, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendvmid()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    *rc = (*ptr)->ESMC_VMSendVMId(*vmid, *dest);
  }

  void FTN(c_esmc_vmrecvvmid)(ESMC_VM **ptr, ESMC_VMId **vmid, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvvmid()"
    // Initialize return code; assume routine not implemented
    *rc = ESMC_RC_NOT_IMPL;

    *rc = (*ptr)->ESMC_VMRecvVMId(*vmid, *source);
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
  
};
