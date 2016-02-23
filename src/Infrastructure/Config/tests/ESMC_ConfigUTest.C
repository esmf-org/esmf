// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <string.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
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

  ESMC_Config cf;              // ESMC_Config object
  const char* fileName = "ESMF_Resource_File_Sample.rc";   // file name
  const char* fileName2= "ESMF_Resource_File_Sample2.rc";  // file name
  char name[80];
  char failMsg[80];
  int linecount, colcount;
  int result = 0;
  int rc = ESMF_RC_NOT_IMPL;
  int unique = 0;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_Initialize Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_Initialize(NULL, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a config object -- cf
  strcpy(name, "ConfigCreate Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  cf = ESMC_ConfigCreate(&rc);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  //Load resource file into memory
  strcpy(name, "ConfigLoadFile Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigLoadFile(cf, fileName2,  ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Load resource file into memory - set optional argument unique to .true.
  unique = 1;
  strcpy(name, "ConfigLoadFile Unit test - optional arg");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigLoadFile(cf, fileName2, ESMCI_ConfigArgUniqueID, unique,
    ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Validate the Config object
  strcpy(name, "ConfigValidate Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigValidate (cf, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Get Config object array dimensions
  strcpy(name, "ConfigGetDim Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigGetDim (cf, &linecount, &colcount, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------
 
#ifdef ESMF_TESTEXHAUSTIVE
 //----------------------------------------------------------------------------
  //EX_UTest
  //Load resource file into memory
  //This UTest tests whether the code will recognize that the input file 
  //repeats the definition of an attribute
  strcpy(name, "ConfigLoadFile Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigLoadFile(cf, fileName, ESMCI_ConfigArgUniqueID, unique,
    ESMC_ArgLast);
  ESMC_Test((rc == ESMF_RC_DUP_NAME), name, failMsg, &result, __FILE__, 
    __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Find a label in the loaded resource file
  const char* label="Number_of_Members:";
  int present;
  strcpy(name, "ConfigFindLabel Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigFindLabel(cf, label, &present);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Presence of a label in the loaded resource file
  strcpy(name, "ConfigFindLabel presence Unit test");
  strcpy(failMsg, "Did not return present");
  ESMC_Test(present, name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Find the next line
  int tableEnd = 1;
  strcpy(name, "ConfigNextLine Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigNextLine(cf, &tableEnd);
  ESMC_Test((rc == ESMF_SUCCESS) && (tableEnd == 0), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Get the length of the line in words (w/o label optional argument)
  strcpy(name, "ConfigGetLen Unit test - no optional arguments");
  int  wordCount=0;
  int* wordCountp=NULL;
  wordCountp = &wordCount;
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigGetLen(cf, wordCountp, ESMC_ArgLast);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Find a non-present label in the loaded resource file
  const char* labelx="xyzzy";
  int presentx;
  strcpy(name, "ConfigFindLabel of non-present label Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigFindLabel(cf, labelx, &presentx);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  //Presence of a non-present label in the loaded resource file
  strcpy(name, "ConfigFindLabel presence Unit test");
  strcpy(failMsg, "Did not return present");
  ESMC_Test(!presentx, name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  //Destroy Config object
  strcpy(name, "ConfigDestroy Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ConfigDestroy(&cf);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
