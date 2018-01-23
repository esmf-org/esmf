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
#include <math.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TimeIntervalUTest - Check ESMC_TimeInterval functionality
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
  
  ESMC_TimeInterval timeInterval1;
  ESMC_I4 h_I4;
  ESMC_I4 h1_I4=1;
  ESMC_I8 s_I8;
  ESMC_I8 s1_I8=0;
  ESMC_R8 h_R8;
  ESMC_R8 hl_R8=0;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set a TimeInterval");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeIntervalSet(&timeInterval1, h1_I4);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_TimeInterval object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeIntervalPrint(timeInterval1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a TimeInterval");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeIntervalGet(timeInterval1, &s_I8, &h_R8);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that hour parameter is returned correctly");
  strcpy(failMsg, "h_R8 is not 1.");
  ESMC_Test((fabs(h_R8-1.) < 1.e-10), name, failMsg, &result, __FILE__,
    __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that the integer sec. parameter is returned correctly");
  strcpy(failMsg, "s_I8 is not 3600");
  ESMC_Test((int(s_I8)==3600), name, failMsg, &result, __FILE__, __LINE__, 0); 
  //----------------------------------------------------------------------------

#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
