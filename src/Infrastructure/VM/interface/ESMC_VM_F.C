// $Id: ESMC_VM_F.C,v 1.64 2007/02/23 23:28:23 theurich Exp $
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
#define ESMC_FILENAME "ESMC_VM_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_F90Interface.h"
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
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"Unknown data type.", rc))
      return;
    (*vm)->vmk_allfullreduce(input, output, *count, vmt, (vmOp)(*op));
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmallgather)(ESMC_VM **vm, void *input, void *output, 
    int *size, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgather()"
    (*vm)->vmk_allgather(input, output, *size);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmallgathernb)(ESMC_VM **vm, void *input, void *output, 
    int *size, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgathernb()"
    *commhandle = NULL; // reset the commhandle
    (*vm)->vmk_allgather(input, output, *size, (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmallgatherv)(ESMC_VM **vm, void *sendData, int *sendCount,
    void *recvData, int *recvCounts, int *recvOffsets, ESMC_TypeKind *dtk, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallgatherv()"
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
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    (*vm)->vmk_allgatherv(sendData, *sendCount, recvData, recvCounts,
      recvOffsets, vmt);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmallreduce)(ESMC_VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallreduce()"
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
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    (*vm)->vmk_allreduce(input, output, *count, vmt, (vmOp)(*op));
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmalltoallv)(ESMC_VM **vm, void *sendData, int *sendCounts,
    int *sendOffsets, void *recvData, int *recvCounts, int *recvOffsets, 
    ESMC_TypeKind *dtk, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmalltoallv()"
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
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, "Unsupported data type.",
      rc)) return;
    (*vm)->vmk_alltoallv(sendData, sendCounts, sendOffsets, recvData,
      recvCounts, recvOffsets, vmt);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmbarrier)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbarrier()"
    (*ptr)->vmk_barrier();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmbroadcast)(ESMC_VM **vm, void *data, int *size, int *root,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    (*vm)->vmk_broadcast(data, *size, *root);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmbroadcastnb)(ESMC_VM **vm, void *data, int *size, int *root,
    void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbroadcast()"
    *commhandle = NULL; // reset the commhandle
    (*vm)->vmk_broadcast(data, *size, *root, (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmgather)(ESMC_VM **vm, void *input, void *output, int *size, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgather()"
    (*vm)->vmk_gather(input, output, *size, *root);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmgathernb)(ESMC_VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgathernb()"
    *commhandle = NULL; // reset the commhandle
    (*vm)->vmk_gather(input, output, *size, *root, 
      (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmget)(ESMC_VM **ptr, int *localPet, int *petCount, 
    int *peCount, int *mpiCommunicator, ESMC_Logical *okOpenMpFlag, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmget()"
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
    int localrc;
    *vmid = (*ptr)->ESMC_VMGetVMId(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmprint)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmprint()"
    int localrc;
    (*ptr)->ESMC_VMPrint(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmrecv)(ESMC_VM **ptr, void *message, int *size, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecv()"
    (*ptr)->vmk_recv(message, *size, *source);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmrecvnb)(ESMC_VM **ptr, void *message, int *size, 
    int *source, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvnb()"
    *commhandle = NULL; // reset the commhandle
    (*ptr)->vmk_recv(message, *size, *source, (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmreduce)(ESMC_VM **vm, void *input, void *output, 
    int *count, ESMC_TypeKind *dtk, ESMC_Operation *op, int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmreduce()"
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
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"Unknown data type.", rc))
      return;
    (*vm)->vmk_reduce(input, output, *count, vmt, (vmOp)(*op), *root);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmscatter)(ESMC_VM **vm, void *input, void *output, int *size,
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatter()"
    (*vm)->vmk_scatter(input, output, *size, *root);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmscatternb)(ESMC_VM **vm, void *input, void *output, 
    int *size, int *root, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatternb()"
    *commhandle = NULL; // reset the commhandle
    (*vm)->vmk_scatter(input, output, *size, *root, 
      (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmsend)(ESMC_VM **ptr, void *message, int *size, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsend()"
    (*ptr)->vmk_send(message, *size, *dest);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmsendnb)(ESMC_VM **ptr, void *message, int *size, int *dest,
    void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendnb()"
    *commhandle = NULL; // reset the commhandle
    (*ptr)->vmk_send(message, *size, *dest, (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmsendrecv)(ESMC_VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecv()"
    (*ptr)->vmk_sendrecv(sendData, *sendSize, *dst, recvData, *recvSize, *src);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmsendrecvnb)(ESMC_VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, void **commhandle, 
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecvnb()"
    *commhandle = NULL; // reset the commhandle
    (*ptr)->vmk_sendrecv(sendData, *sendSize, *dst, recvData, *recvSize, *src,
      (vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmwait)(ESMC_VM **ptr, void **commhandle, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwait()"
    (*ptr)->vmk_commwait((vmk_commhandle **)commhandle);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmwaitqueue)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwaitqueue()"
    (*ptr)->vmk_commqueuewait();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmwtime)(ESMC_R8 *time, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    vmk_wtime(time);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmwtimedelay)(ESMC_R8 *delay, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmwtime()"
    vmk_wtimedelay(*delay);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmthreadbarrier)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmthreadbarrier()"
    (*ptr)->vmk_threadbarrier();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmgetcurrent)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetcurrent()"
    int localrc;
    *ptr = ESMC_VMGetCurrent(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vminitialize)(ESMC_VM **ptr, int *mpiCommunicator, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vminitialize()"
    int localrc;
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
    ESMC_VMFinalize(ESMC_NOT_PRESENT_FILTER(keepMpiFlag), &localrc);
    //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    *rc = localrc;
  }

  void FTN(c_esmc_vmabort)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmabort()"
    int localrc;
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
    int localrc = (*ptr)->vmkplan_maxcores(**ptr_vm, maxx, (int*)petlist,
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
    int localrc = (*ptr)->vmkplan_maxthreads(**ptr_vm, maxx, (int*)petlist,
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
    int localrc = (*ptr)->vmkplan_minthreads(**ptr_vm, maxx, (int*)petlist,
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
    *vmid = ESMC_VMGetCurrentID(&localrc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmidcompare)(ESMC_VMId **vmid1, ESMC_VMId **vmid2,
    ESMC_Logical *result, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcompare()"
    *result = ESMC_VMIdCompare(*vmid1, *vmid2);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmidprint)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidprint()"
    ESMC_VMIdPrint(*vmid);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmidcreate)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmidcreate()"
    int localrc;
    *vmid = new ESMC_VMId;              // allocate memory off the heap
    **vmid = ESMC_VMIdCreate(&localrc); // allocate memory for internal members
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }

  void FTN(c_esmc_vmiddestroy)(ESMC_VMId **vmid, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmiddestroy()"
    int localrc;
    ESMC_VMIdDestroy(*vmid, &localrc);  // free memory for internal members
    delete *vmid;                       // free memory for this VMId
    *vmid=NULL;
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }


  void FTN(c_esmc_vmsendvmid)(ESMC_VM **ptr, ESMC_VMId **vmid, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendvmid()"
    *rc = (*ptr)->ESMC_VMSendVMId(*vmid, *dest);
  }

  void FTN(c_esmc_vmrecvvmid)(ESMC_VM **ptr, ESMC_VMId **vmid, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecvvmid()"
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
