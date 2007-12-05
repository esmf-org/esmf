// $Id: ESMC_ConfigUTest.C,v 1.1 2007/12/05 06:03:33 theurich Exp $
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
  char* fileName;                   // file name
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = 0;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a config object -- cf
  strcpy(name, "ConfigCreate Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  cf = ESMC_ConfigCreate(&rc);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
