// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2015, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include "ESMCI_Test.h"
#include "ESMCI.h"

//==============================================================================
//BOP
// !PROGRAM: ESMCI_FeatureUTest - Check for support of various compiler features.
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

extern "C" {
  int FTN_X(esmf_optional_arg_pos)(int *, int *);
}

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;

  int arg1=42, arg2=43;
  int returnVal;

  //----------------------------------------------------------------------------
  ESMCI::TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // C->Fortran optional argument tests

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with no optional args Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(&arg1, &arg2);
  ESMCI::Test(returnVal==3, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #1 not present Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(NULL, &arg2);
  ESMCI::Test(returnVal==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #2 not present Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(&arg1, NULL);
  ESMCI::Test(returnVal==1, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with no args present Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(NULL, NULL);
  ESMCI::Test(returnVal==0, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMCI::TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
