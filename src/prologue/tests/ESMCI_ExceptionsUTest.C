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

#include <string.h>
#include "ESMCI_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TestUTest - Check ESMCI::Test functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

void bareThrow(){
  throw 1;
}

class classNoDestructor{
  int dummyMember;
};

void classNoDestructorThrow(){
  throw 2;
}

class classWithDestructor{
  int dummyMember;
 public:
  ~classWithDestructor(){} 
};

void classWithDestructorThrow(){
  throw 3;
}

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int caught;

  //----------------------------------------------------------------------------
  ESMCI::TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Bare throw Test");
  strcpy(failMsg, "Did not catch correct exception value");
  caught = 0;
  try{
    bareThrow();
  }catch(int localCaught){
    caught=localCaught;
  }
  ESMCI::Test(caught==1, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ClassNoDestructor throw Test");
  strcpy(failMsg, "Did not catch correct exception value");
  caught = 0;
  try{
    classNoDestructorThrow();
  }catch(int localCaught){
    caught=localCaught;
  }
  ESMCI::Test(caught==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ClassWithDestructor throw Test");
  strcpy(failMsg, "Did not catch correct exception value");
  caught = 0;
  try{
    classWithDestructorThrow();
  }catch(int localCaught){
    caught=localCaught;
  }
  ESMCI::Test(caught==3, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMCI::TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
