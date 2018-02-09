// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_VMUTest - Check ESMC_VM functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ESMC_VM vm;
  ESMC_VM vmCurrent;
  int localPet, petCount, peCount;
  int pthreadsEnabledFlag, openMPEnabledFlag;
  MPI_Comm mpiCommunicator;

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get the global VM
  strcpy(name, "VMGetGlobal");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  vm = ESMC_VMGetGlobal(&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get the current VM
  strcpy(name, "VMGetCurrent");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  vmCurrent = ESMC_VMGetCurrent(&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get parameters from a VM
  strcpy(name, "VMGet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_VMGet(vm, &localPet, &petCount, &peCount, &mpiCommunicator,
    &pthreadsEnabledFlag, &openMPEnabledFlag);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  printf("localPet= %d , petCount= %d, peCount= %d, pthreadsEnabledFlag= %d, "
    "openMPEnabledFlag= %d \n", localPet, petCount, peCount,
    pthreadsEnabledFlag, openMPEnabledFlag);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // call Barrier
  strcpy(name, "VMBarrier");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_VMBarrier(vm);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Print a VM
  strcpy(name, "VMPrint");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_VMPrint(vm);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Reduce
  {
  strcpy(name, "VMReduceI4");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  int in[4];
  int out[4];
  in[0] = 1; in[1] = 1; in[2] = 1; in[3] = 1;
  out[0] = 0; out[1] = 0; out[2] = 0; out[3] = 0;
  rc = ESMC_VMReduce(vm, in, out, len, ESMC_TYPEKIND_I4,
                     ESMC_REDUCE_SUM, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != petCount) correct = false;
      // printf("PET%d: out[%d] = %d\n", localPet, i, out[i]);
    }
  } else {
    for (int i = 0; i < len; ++i) {
      if (out[i] != 0) correct = false;
      // printf("PET%d: out[%d] = %d\n", localPet, i, out[i]);
    }
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // VM Reduce
  {
  strcpy(name, "VMReduceI8");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  long long in[4];
  long long out[4];
  in[0] = 1; in[1] = 1; in[2] = 1; in[3] = 1;
  out[0] = 0; out[1] = 0; out[2] = 0; out[3] = 0;
  rc = ESMC_VMReduce(vm, in, out, len, ESMC_TYPEKIND_I8,
                     ESMC_REDUCE_SUM, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (long long)petCount) correct = false;
      // printf("PET%d: out[%d] = %lld\n", localPet, i, out[i]);
    }
  } else {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (long long)0) correct = false;
      // printf("PET%d: out[%d] = %lld\n", localPet, i, out[i]);
    }
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Reduce
  {
  strcpy(name, "VMReduceR4");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  float in[4];
  float out[4];
  in[0] = 1; in[1] = 1; in[2] = 1; in[3] = 1;
  out[0] = 0; out[1] = 0; out[2] = 0; out[3] = 0;
  rc = ESMC_VMReduce(vm, in, out, len, ESMC_TYPEKIND_R4,
                     ESMC_REDUCE_SUM, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (float)petCount) correct = false;
      // printf("PET%d: out[%d] = %f\n", localPet, i, out[i]);
    }
  } else {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (float)0) correct = false;
      // printf("PET%d: out[%d] = %f\n", localPet, i, out[i]);
    }
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // VM Reduce
  {
  strcpy(name, "VMReduceR8");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  double in[4];
  double out[4];
  in[0] = 1; in[1] = 1; in[2] = 1; in[3] = 1;
  out[0] = 0; out[1] = 0; out[2] = 0; out[3] = 0;
  rc = ESMC_VMReduce(vm, in, out, len, ESMC_TYPEKIND_R8,
                     ESMC_REDUCE_SUM, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (double)petCount) correct = false;
      // printf("PET%d: out[%d] = %f\n", localPet, i, out[i]);
    }
  } else {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (double)0) correct = false;
      // printf("PET%d: out[%d] = %f\n", localPet, i, out[i]);
    }
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}




