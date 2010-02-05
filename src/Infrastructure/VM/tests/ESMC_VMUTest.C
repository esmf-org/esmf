// $Id: ESMC_VMUTest.C,v 1.7.2.1 2010/02/05 20:02:03 svasquez Exp $
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
  // Print a VM
  strcpy(name, "VMPrint");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_VMPrint(vm);
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
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}




