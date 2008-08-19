// $Id: ESMC_VMUTest.C,v 1.1 2008/08/19 22:52:43 rosalind Exp $
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

#include <stdlib.h>
#include <string.h>

// ESMF header
#include "ESMC.h"
#include "ESMC_VM.h"

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
  int localPet, petCount, peCount, mpiCommunicator;
  int supportPthreadsFlag, supportOpenMPFlag;

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get a pointer to the global VM
  strcpy(name, "VMGetGlobal\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  vm = ESMC_VMGetGlobal(&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Print a VM
  strcpy(name, "VMPrint\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_VMPrint(vm);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get a pointer to the current VM
  strcpy(name, "VMGetCurrent\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  vmCurrent = ESMC_VMGetCurrent(&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get parameters from a VM
  strcpy(name, "VMGet\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_VMGet( &vm, &localPet, &petCount, &peCount, &mpiCommunicator,
                   &supportPthreadsFlag, &supportOpenMPFlag);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

//printf("localPet= %d , petCount= %d, peCount= %d, mpiCommunicator=%d,"
//       " supportPthreadsFlag= %d, supportOpenMPFlag= %d \n", 
//        localPet, petCount, peCount, mpiCommunicator, supportPthreadsFlag,
//        supportOpenMPFlag);
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}




