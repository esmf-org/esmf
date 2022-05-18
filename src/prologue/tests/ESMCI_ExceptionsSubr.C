// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// C++ exceptions tests
//
//-------------------------------------------------------------------------

#include <string.h>
#include "ESMCI.h"
#include "ESMCI_Test.h"

void bareThrow(){
  throw 1;
}

void deepThrow(){
  try{
    throw -1;
  }catch (int localCaught) {
    throw localCaught;
  }
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

extern "C" {

  void FTN_X(esmc_bare_throw_test)(int result){
    char name[80];
    char failMsg[80];
    int caught;
    strcpy(name, "Bare throw Test");
    strcpy(failMsg, "Did not catch correct exception value");
    caught = 0;
    try{
      bareThrow();
    }catch(int localCaught){
      caught=localCaught;
    }
    ESMCI::Test(caught==1, name, failMsg, &result, __FILE__, __LINE__, 0);
  }

  void FTN_X(esmc_deep_throw_test)(int result){
    char name[80];
    char failMsg[80];
    int caught;
    strcpy(name, "Deep throw Test");
    strcpy(failMsg, "Did not catch correct exception value");
    caught = 0;
    try{
      deepThrow();
    }catch(int localCaught){
      caught=localCaught;
    }
    ESMCI::Test(caught==-1, name, failMsg, &result, __FILE__, __LINE__, 0);
  }

  void FTN_X(esmc_class_no_destructor_throw_test)(int result){
    char name[80];
    char failMsg[80];
    int caught;
    strcpy(name, "ClassNoDestructor throw Test");
    strcpy(failMsg, "Did not catch correct exception value");
    caught = 0;
    try{
      classNoDestructorThrow();
    }catch(int localCaught){
      caught=localCaught;
    }
    ESMCI::Test(caught==2, name, failMsg, &result, __FILE__, __LINE__, 0);
  }

  void FTN_X(esmc_class_with_destructor_throw_test)(int result){
    char name[80];
    char failMsg[80];
    int caught;
    strcpy(name, "ClassWithDestructor throw Test");
    strcpy(failMsg, "Did not catch correct exception value");
    caught = 0;
    try{
      classWithDestructorThrow();
    }catch(int localCaught){
      caught=localCaught;
    }
    ESMCI::Test(caught==3, name, failMsg, &result, __FILE__, __LINE__, 0);
  }

}
