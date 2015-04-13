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

extern "C" {
  int FTN_X(esmf_optional_arg_pos_a1d)(double a1[], double a2[]);
}

extern "C" {
  int FTN_X(esmf_optional_arg_pos_a2d)(double a1[], int *, double a2[], int *);
}

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;

  int arg1=42, arg2=43;
  double arg1_1d[10], arg2_1d[20];
  double arg1_2d[10*10], arg2_2d[20*20];  // dense 2D arrays
  int arg1_idim, arg2_idim;               // leading dimension for dense 2D arrays
  int returnVal;

  //----------------------------------------------------------------------------
  ESMCI::TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // C->Fortran optional argument tests

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with both args present scalar Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(&arg1, &arg2);
  ESMCI::Test(returnVal==3, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #1 not present scalar Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(NULL, &arg2);
  ESMCI::Test(returnVal==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #2 not present scalar Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(&arg1, NULL);
  ESMCI::Test(returnVal==1, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with no args present scalar Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos)(NULL, NULL);
  ESMCI::Test(returnVal==0, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with both args present 1D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a1d)(arg1_1d, arg2_1d);
  ESMCI::Test(returnVal==3, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #1 not present 1D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a1d)(NULL, arg2_1d);
  ESMCI::Test(returnVal==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #2 not present 1D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a1d)(arg1_1d, NULL);
  ESMCI::Test(returnVal==1, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with no args present 1D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a1d)(NULL, NULL);
  ESMCI::Test(returnVal==0, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with both args present 2D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  arg1_idim = 10;
  arg2_idim = 20;
  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a2d)(arg1_1d, &arg1_idim, arg2_2d, &arg2_idim);
  ESMCI::Test(returnVal==3, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #1 not present 2D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  arg1_idim = 0;
  arg2_idim = 20;
  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a2d)(NULL, &arg1_idim, arg2_2d, &arg2_idim);
  ESMCI::Test(returnVal==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with arg #2 not present 1D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  arg1_idim = 10;
  arg2_idim = 0;
  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a2d)(arg1_2d, &arg1_idim, NULL, &arg2_idim);
  ESMCI::Test(returnVal==1, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Call Fortran with no args present 2D array Test");
  strcpy(failMsg, "Did not report correct arguments");

  arg1_idim = 0;
  arg2_idim = 0;
  // Fortran function returns a 1 bit for each argument that is present.
  returnVal = FTN_X(esmf_optional_arg_pos_a2d)(NULL, &arg1_idim, NULL, &arg2_idim);
  ESMCI::Test(returnVal==0, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMCI::TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
