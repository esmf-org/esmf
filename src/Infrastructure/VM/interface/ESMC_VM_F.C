// $Id: ESMC_VM_F.C,v 1.1 2004/02/24 22:07:27 theurich Exp $
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
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_VM.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC_VM} and {\tt ESMC_VMPlan} 
//  class functions.
//
//EOP
//------------------------------------------------------------------------------


// the interface subroutine names MUST be in lower case

extern "C" {

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_VM interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  void FTN(c_esmc_vminitialize)(ESMC_VM **ptr, int *status){
    *ptr = ESMC_VMInitialize(status);
  }

  void FTN(c_esmc_vmfinalize)(int *status){
    ESMC_VMFinalize(status);
  }

  void FTN(c_esmc_vmget)(ESMC_VM **ptr, int *mypet, int *npets, int *npes, 
    int *mpic, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)mypet    == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : mypet;
    (void*)npets    == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : npets;
    (void*)npes     == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : npes;
    (void*)mpic     == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : mpic;
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
    // Done sorting out non-present F90 optional arguments.
    int rc = (*ptr)->ESMC_VMGet(mypet, npets, npes, mpic);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_vmgetpet)(ESMC_VM **ptr, int *petid, int *npes, int *ssiid,
    int *mthpet, int *nthpet, int *tidpet, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)npes     == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : npes;
    (void*)ssiid    == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : ssiid;
    (void*)mthpet    == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : mthpet;
    (void*)nthpet    == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : nthpet;
    (void*)tidpet    == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : tidpet;
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
    // Done sorting out non-present F90 optional arguments.
    int rc = (*ptr)->ESMC_VMGetPET(petid, npes, ssiid, mthpet, nthpet, tidpet);
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }
  
  void FTN(c_esmc_vmprint)(ESMC_VM **ptr, int *status){
    (*ptr)->vmachine_print();
    *status = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmbarrier)(ESMC_VM **ptr, int *status){
    (*ptr)->vmachine_barrier();
    *status = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmthreadbarrier)(ESMC_VM **ptr, int *status){
    (*ptr)->vmachine_threadbarrier();
    *status = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmsend)(ESMC_VM **ptr, void *message, int *size, int *dest,
    int *status){
    (*ptr)->vmachine_send(message, *size, *dest);
    *status = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmrecv)(ESMC_VM **ptr, void *message, int *size, int *source,
    int *status){
    (*ptr)->vmachine_recv(message, *size, *source);
    *status = ESMF_SUCCESS;
  }

  void FTN(c_esmc_vmscatter)(ESMC_VM **vm, int *input, int *output, int *len,
    int *root, int *status){
    // len must be converted into bytes:
    int blen = (*len) * 4; // this is hard-coded for 32 bit integers
    (*vm)->vmachine_scatter(input, output, blen, *root);
    *status = ESMF_SUCCESS;
  }
  
  void FTN(c_esmc_vmgather)(ESMC_VM **vm, int *input, int *output, int *len, 
    int *root, int *status){
    // len must be converted into bytes:
    int blen = (*len) * 4; // this is hard-coded for 32 bit integers
    (*vm)->vmachine_gather(input, output, blen, *root);
    *status = ESMF_SUCCESS;
  }

  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ESMC_VMPlan interfaces
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  void FTN(c_esmc_vmplanconstruct)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *npetlist, int *petlist, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
    // Done sorting out non-present F90 optional arguments.
    (*ptr) = new ESMC_VMPlan;
    if (npetlist > 0)
      (*ptr)->vmplan_minthreads(**ptr_vm, 1, (int*)petlist, *npetlist);
    else
      (*ptr)->vmplan_minthreads(**ptr_vm, 1);
    //debug: (*ptr)->vmplan_print();
    int rc = ESMF_SUCCESS;
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }

  void FTN(c_esmc_vmplandestruct)(ESMC_VMPlan **ptr, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
    // Done sorting out non-present F90 optional arguments.
    delete (*ptr);
    int rc = ESMF_SUCCESS;
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }
  
  void FTN(c_esmc_vmplanmaxthreads)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)max      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : max;
    (void*)pref_intra_process == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_intra_process;
    (void*)pref_intra_ssi == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_intra_ssi;
    (void*)pref_inter_ssi == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_inter_ssi;
    (void*)petlist  == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : petlist;
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
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
    (*ptr)->vmplan_maxthreads(**ptr_vm, maxx, (int*)petlist, *npetlist,
      ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    //debug: (*ptr)->vmplan_print();
    int rc = ESMF_SUCCESS;
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }
  
  void FTN(c_esmc_vmplanminthreads)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)max      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : max;
    (void*)pref_intra_process == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_intra_process;
    (void*)pref_intra_ssi == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_intra_ssi;
    (void*)pref_inter_ssi == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_inter_ssi;
    (void*)petlist  == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : petlist;
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
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
    (*ptr)->vmplan_minthreads(**ptr_vm, maxx, (int*)petlist, *npetlist,
      ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    //debug: (*ptr)->vmplan_print();
    int rc = ESMF_SUCCESS;
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }
  
  void FTN(c_esmc_vmplanmaxpes)(ESMC_VMPlan **ptr, ESMC_VM **ptr_vm,
    int *max, int *pref_intra_process, int *pref_intra_ssi, int *pref_inter_ssi,
    int *npetlist, int *petlist, int *status){
    // Sort out the non-present F90 optional arguments. 
    // The detection of non-present F90 optional arguemtns is compiler/platform
    // dependent. Currently we expect either a pointer to NULL or (NULL - 1).
    // Since the actual C++ methods expect non-present arguments to be
    // indicated by a pointer to NULL all we need to do here is set those 
    // that point to (NULL - 1) [which is available as macro ESMC_BAD_POINTER]
    // to NULL as well before passing them down further.
    (void*)max      == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : max;
    (void*)pref_intra_process == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_intra_process;
    (void*)pref_intra_ssi == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_intra_ssi;
    (void*)pref_inter_ssi == 
      (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : pref_inter_ssi;
    (void*)petlist  == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : petlist;
    (void*)status   == (void*)ESMC_BAD_POINTER ? ESMC_NULL_POINTER : status;
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
    (*ptr)->vmplan_maxcores(**ptr_vm, maxx, (int*)petlist, *npetlist,
      ppref_intra_process, ppref_intra_ssi, ppref_inter_ssi);
    //debug: (*ptr)->vmplan_print();
    int rc = ESMF_SUCCESS;
    if (status != ESMC_NULL_POINTER) 
      *status = rc;
  }
       
};
