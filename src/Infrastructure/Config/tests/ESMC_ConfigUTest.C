// $Id: ESMC_ConfigUTest.C,v 1.4 2008/04/05 03:38:11 cdeluca Exp $
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

#include <string.h>
#include "ESMC.h"
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TestUTest - Check ESMC_Test functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  ESMC_Config* cf;              // ESMC_Config object
  char* fileName = "ESMF_Resource_File_Sample.rc";                   // file name
  char* fileName2= "ESMF_Resource_File_Sample2.rc";                  // file name
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = ESMF_RC_NOT_IMPL;
  int unique = 0;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  rc = ESMF_RC_NOT_IMPL;
  strcpy(name, "ESMC_Initialize Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_Initialize(NULL, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a config object -- cf
  strcpy(name, "ConfigCreate Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  cf = ESMC_ConfigCreate(&rc);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
 //----------------------------------------------------------------------------
  //NEX_UTest
  //Load resource file into memory
  rc = ESMF_RC_NOT_IMPL;
  strcpy(name, "ConfigLoadFile Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_ConfigLoadFile(cf, fileName2,  ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

 //----------------------------------------------------------------------------
  //NEX_UTest
  //Load resource file into memory - set optional argument unique to .true.
  rc = ESMF_RC_NOT_IMPL;
  unique = 1;
  strcpy(name, "ConfigLoadFile Unit test - optional arg. \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_ConfigLoadFile(cf, fileName2, ESMCI_ConfigArgUniqueID,
                            unique, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
#ifdef ESMF_EXHAUSTIVE
 //----------------------------------------------------------------------------
  //EX_UTest
  //Load resource file into memory
  //This UTest tests whether the code will recognize that the input file 
  //repeats the definition of an attribute
  rc = ESMF_RC_NOT_IMPL;
  strcpy(name, "ConfigLoadFile Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_ConfigLoadFile(cf, fileName, ESMCI_ConfigArgUniqueID,
                            unique, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_RC_DUP_NAME), name, failMsg, &result, __FILE__, __LINE__,
 0);
  //----------------------------------------------------------------------------
#endif


  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
