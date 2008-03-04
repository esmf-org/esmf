// $Id: ESMC_StateUTest.C,v 1.3 2008/03/04 18:07:52 rokuingh Exp $
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
#include "ESMCI.h"
#include "ESMC_Test.h"
#include "ESMC_Start.h"
//#include "ESMC_DistGrid.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_State.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TestUTest - Check ESMC_Test functionality
//
// !DESCRIPTION:
//
//  The code in this file drives C State unit tests.
 // The companion file ESMC\_State.F90 contains the definitions for the
 // State methods.

//EOP
//-----------------------------------------------------------------------------

int main(void){

  ESMC_State* st;              // ESMC_State object
  ESMC_Array testArray;
  char* StateName = "stateName";                   // state name
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = 0;
  int localrc;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a state object -- cf
  strcpy(name, "StateCreate Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  st = ESMC_StateCreate(StateName,&rc);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Destroy a state object -- cf
  strcpy(name, "StateDestroy Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_StateDestroy(st);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
