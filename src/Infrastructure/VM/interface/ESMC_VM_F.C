// $Id: ESMC_VM_F.C,v 1.22 2004/11/16 16:31:02 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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
    int *count, ESMC_DataKind *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallfullreduce()"
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
    vmType vmt;
    switch (*dtk){
    case ESMF_I4:
      vmt = vmI4;
      break;
    case ESMF_R4:
      vmt = vmR4;
      break;
    case ESMF_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"Unknown data type.", rc))
      return;
    (*vm)->vmk_allglobalreduce(input, output, *count, vmt, (vmOp)(*op));
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmallreduce)(ESMC_VM **vm, void *input, void *output, 
    int *count, ESMC_DataKind *dtk, ESMC_Operation *op, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmallreduce()"
    // start assuming local success
    int localrc = ESMF_SUCCESS;
    // need to type cast or transform dtk and op into ESMC_VMK types
    vmType vmt;
    switch (*dtk){
    case ESMF_I4:
      vmt = vmI4;
      break;
    case ESMF_R4:
      vmt = vmR4;
      break;
    case ESMF_R8:
      vmt = vmR8;
      break;
    default:
      localrc = ESMF_FAILURE;
    }
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc,"Unknown data type.", rc))
      return;
    (*vm)->vmk_allreduce(input, output, *count, vmt, (vmOp)(*op));
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmbarrier)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmbarrier()"
    (*ptr)->vmk_barrier();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmgather)(ESMC_VM **vm, void *input, void *output, int *size, 
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgather()"
    (*vm)->vmk_gather(input, output, *size, *root);
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
#ifdef VM_DONT_HAVE_MPI_COMM_C2F
      *mpiCommunicator = (int)(mpiCommTemp);
#else
      *mpiCommunicator = (int)MPI_Comm_c2f(mpiCommTemp);
#endif
    }
  }

  void FTN(c_esmc_vmgetpetlocalinfo)(ESMC_VM **ptr, int *pet, int *peCount, 
    int *ssiId, int *threadCount, int *threadId, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmgetpetlocalinfo()"
    // Call into the actual C++ method wrapped inside LogErr handling
    ESMC_LogDefault.ESMC_LogMsgFoundError((*ptr)->ESMC_VMGetPETLocalInfo(
      *pet, 
      ESMC_NOT_PRESENT_FILTER(peCount), 
      ESMC_NOT_PRESENT_FILTER(ssiId), 
      ESMC_NOT_PRESENT_FILTER(threadCount), 
      ESMC_NOT_PRESENT_FILTER(threadId)),
      ESMF_ERR_PASSTHRU,
      ESMC_NOT_PRESENT_FILTER(rc));
  }
  
  void FTN(c_esmc_vmprint)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmprint()"
    (*ptr)->vmk_print();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmrecv)(ESMC_VM **ptr, void *message, int *size, int *source,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmrecv()"
    (*ptr)->vmk_recv(message, *size, *source);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmscatter)(ESMC_VM **vm, void *input, void *output, int *size,
    int *root, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmscatter()"
    (*vm)->vmk_scatter(input, output, *size, *root);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }
  
  void FTN(c_esmc_vmsend)(ESMC_VM **ptr, void *message, int *size, int *dest,
    int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsend()"
    (*ptr)->vmk_send(message, *size, *dest);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmsendrecv)(ESMC_VM **ptr, void *sendData, int *sendSize, 
    int *dst, void *recvData, int *recvSize, int *src, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmsendrecv()"
    (*ptr)->vmk_sendrecv(sendData, *sendSize, *dst, 
      recvData, *recvSize, *src);
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vmthreadbarrier)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmthreadbarrier()"
    (*ptr)->vmk_threadbarrier();
    *rc = ESMF_SUCCESS;       // TODO: finish error handling when ESMC_VMK done
  }

  void FTN(c_esmc_vminitialize)(ESMC_VM **ptr, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vminitialize()"
    int localrc;
    *ptr = ESMC_VMInitialize(&localrc);
    //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    // Cannot use LogErr here because LogErr initializes _after_ VM
    *rc = localrc;
  }

  void FTN(c_esmc_vmfinalize)(int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmfinalize()"
    int localrc;
    ESMC_VMFinalize(&localrc);
    //ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
    // Cannot use LogErr here because LogErr finalizes _before_ VM
    *rc = localrc;
  }

  void FTN(c_esmc_vmshutdown)(ESMC_VM **ptr_vmparent, ESMC_VMPlan **ptr_vmplan,
    void **vm_info, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmshutdown()"
    int localrc;
    (*ptr_vmparent)->vmk_shutdown(static_cast<ESMC_VMKPlan *>(*ptr_vmplan),
         *vm_info);
    
    localrc=ESMF_SUCCESS;
    ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc);
  }
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_VMPlan interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  void FTN(c_esmc_vmplanconstruct)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *npetlist, int *petlist, int *rc){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_vmplanconstruct()"
    (*ptr) = new ESMC_VMPlan;
    if (npetlist > 0)
      (*ptr)->vmkplan_minthreads(**ptr_vm, 1, (int*)petlist, *npetlist);
    else
      (*ptr)->vmkplan_minthreads(**ptr_vm, 1);
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
    (*ptr)->vmkplan_maxcores(**ptr_vm, maxx, (int*)petlist, *npetlist,
      ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
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
    (*ptr)->vmkplan_maxthreads(**ptr_vm, maxx, (int*)petlist, *npetlist,
      ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
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
    (*ptr)->vmkplan_minthreads(**ptr_vm, maxx, (int*)petlist, *npetlist,
      ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
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
  
};
