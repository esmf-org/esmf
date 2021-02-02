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
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_I4;
  ESMC_Reduce_Flag rd = ESMC_REDUCE_SUM;
  rc = ESMC_VMReduce(vm, in, out, len, &tk, &rd, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != petCount) correct = false;
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
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_I8;
  ESMC_Reduce_Flag rd = ESMC_REDUCE_SUM;
  rc = ESMC_VMReduce(vm, in, out, len, &tk, &rd, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (long long)petCount) correct = false;
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
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_R4;
  ESMC_Reduce_Flag rd = ESMC_REDUCE_SUM;
  rc = ESMC_VMReduce(vm, in, out, len, &tk, &rd, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (float)petCount) correct = false;
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
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_R8;
  ESMC_Reduce_Flag rd = ESMC_REDUCE_SUM;
  rc = ESMC_VMReduce(vm, in, out, len, &tk, &rd, root);
  bool correct = true;
  if (localPet == 0) {
    for (int i = 0; i < len; ++i) {
      if (out[i] != (double)petCount) correct = false;
      // printf("PET%d: out[%d] = %f\n", localPet, i, out[i]);
    }
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Broadcast
  {
  strcpy(name, "VMBroadcastI4");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  int bcast[4];
  if (localPet == 0) {
    bcast[0] = 4; bcast[1] = 4; bcast[2] = 4; bcast[3] = 4;
  } else {
    bcast[0] = 2; bcast[1] = 2; bcast[2] = 2; bcast[3] = 2;
  }
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_I4;
  rc = ESMC_VMBroadcast(vm, bcast, len, &tk, root);
  bool correct = true;
  for (int i = 0; i < len; ++i) {
    if (bcast[i] != 4) correct = false;
    // printf("PET%d: out[%d] = %d\n", localPet, i, bcast[i]);
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Broadcast
  {
  strcpy(name, "VMBroadcastI8");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  long long bcast[4];
  if (localPet == 0) {
    bcast[0] = 4; bcast[1] = 4; bcast[2] = 4; bcast[3] = 4;
  } else {
    bcast[0] = 2; bcast[1] = 2; bcast[2] = 2; bcast[3] = 2;
  }
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_I8;
  rc = ESMC_VMBroadcast(vm, bcast, len, &tk, root);
  bool correct = true;
  for (int i = 0; i < len; ++i) {
    if (bcast[i] != (long long) 4) correct = false;
    // printf("PET%d: out[%d] = %lld\n", localPet, i, bcast[i]);
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Broadcast
  {
  strcpy(name, "VMBroadcastR4");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  float bcast[4];
  if (localPet == 0) {
    bcast[0] = 4; bcast[1] = 4; bcast[2] = 4; bcast[3] = 4;
  } else {
    bcast[0] = 2; bcast[1] = 2; bcast[2] = 2; bcast[3] = 2;
  }
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_R4;
  rc = ESMC_VMBroadcast(vm, bcast, len, &tk, root);
  bool correct = true;
  for (int i = 0; i < len; ++i) {
    if (bcast[i] != (float) 4) correct = false;
    // printf("PET%d: out[%d] = %f\n", localPet, i, bcast[i]);
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Broadcast
  {
  strcpy(name, "VMBroadcastR8");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  double bcast[4];
  if (localPet == 0) {
    bcast[0] = 4; bcast[1] = 4; bcast[2] = 4; bcast[3] = 4;
  } else {
    bcast[0] = 2; bcast[1] = 2; bcast[2] = 2; bcast[3] = 2;
  }
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_R8;
  rc = ESMC_VMBroadcast(vm, bcast, len, &tk, root);
  bool correct = true;
  for (int i = 0; i < len; ++i) {
    if (bcast[i] != (double) 4) correct = false;
    // printf("PET%d: out[%d] = %f\n", localPet, i, bcast[i]);
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //VM Broadcast
  {
  strcpy(name, "VMBroadcastLogical");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int root = 0;
  int len = 4;
  bool bcast[4];
  if (localPet == 0) {
    bcast[0] = true; bcast[1] = true; bcast[2] = true; bcast[3] = true;
  } else {
    bcast[0] = false; bcast[1] = false; bcast[2] = false; bcast[3] = false;
  }
  ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_LOGICAL;
  rc = ESMC_VMBroadcast(vm, bcast, len, &tk, root);
  bool correct = true;
  for (int i = 0; i < len; ++i) {
    if (bcast[i] != true) correct = false;
    // printf("PET%d: out[%d] = %d\n", localPet, i, bcast[i]);
  }
  ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  }
  //----------------------------------------------------------------------------

  // //----------------------------------------------------------------------------
  // //NEX_disable_UTest
  // //VM Broadcast
  // {
  // strcpy(name, "VMBroadcastCharacter");
  // strcpy(failMsg, "Did not return ESMF_SUCCESS");
  // int root = 0;
  // int len = 4;
  // char bcast[4];
  // if (localPet == 0) {
  //   bcast[0] = 'x'; bcast[1] = 'x'; bcast[2] = 'x'; bcast[3] = 'x';
  // } else {
  //   bcast[0] = 'y'; bcast[1] = 'y'; bcast[2] = 'y'; bcast[3] = 'y';
  // }
  // ESMC_TypeKind_Flag tk = ESMC_TYPEKIND_CHARACTER;
  // rc = ESMC_VMBroadcast(vm, bcast, len, &tk, root);
  // bool correct = true;
  // for (int i = 0; i < len; ++i) {
  //   if (bcast[i] != 'y') correct = false;
  //   printf("PET%d: out[%d] = %s\n", localPet, i, bcast[i]);
  // }
  // ESMC_Test((rc==ESMF_SUCCESS && correct), name, failMsg, &result, __FILE__, __LINE__, 0);
  // }
  // //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}




