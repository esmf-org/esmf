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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_ComponentUTest - Check ESMC_Component functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

void myInitInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myInitInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // test internal state -> set
  int *data = (int *)malloc(10*sizeof(int));
  printf("setting GridComp internal state to data: %p\n", data);
  ESMC_GridCompSetInternalState(gcomp, data);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myRunInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myRunInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // test internal state -> get
  int *data = (int *)ESMC_GridCompGetInternalState(gcomp, NULL);
  printf("getting GridComp internal state to data: %p\n", data);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myFinalInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myFinalizeInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);

  // test internal state -> get and free
  int *data = (int *)ESMC_GridCompGetInternalState(gcomp, NULL);
  printf("getting GridComp internal state to data: %p\n", data);
  free(data);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void myRegistrationInC(ESMC_GridComp gcomp, int *rc){
  // register Init(), Run(), Finalize()
  printf("I am in myRegistrationInC()\n");
  ESMC_GridCompPrint(gcomp);
  
  ESMC_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, myInitInC, 1);
  ESMC_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, myRunInC, 1);
  ESMC_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, myFinalInC, 1);
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void myCplInitInC(ESMC_CplComp cplcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myInitInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // test internal state -> set
  int *data = (int *)malloc(10*sizeof(int));
  printf("setting CplComp internal state to data: %p\n", data);
  ESMC_CplCompSetInternalState(cplcomp, data);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myCplRunInC(ESMC_CplComp cplcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myRunInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // test internal state -> get
  int *data = (int *)ESMC_CplCompGetInternalState(cplcomp, NULL);
  printf("getting CplComp internal state to data: %p\n", data);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myCplFinalInC(ESMC_CplComp cplcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myFinalizeInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // test internal state -> get and free
  int *data = (int *)ESMC_CplCompGetInternalState(cplcomp, NULL);
  printf("getting CplComp internal state to data: %p\n", data);
  free(data);

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void myCplRegistrationInC(ESMC_CplComp cplcomp, int *rc){
  // register Init(), Run(), Finalize()
  printf("I am in myCplRegistrationInC()\n");
  ESMC_CplCompPrint(cplcomp);
  
  ESMC_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_INITIALIZE, myCplInitInC, 1);
  ESMC_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_RUN, myCplRunInC, 1);
  ESMC_CplCompSetEntryPoint(cplcomp, ESMF_METHOD_FINALIZE, myCplFinalInC, 1);
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

extern "C"{
  void FTN_X(my_registrationinfortran)(ESMC_GridComp comp, int *rc);
  void FTN_X(my_cplregistrationinfortran)(ESMC_CplComp comp, int *rc);
}


int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int userRc;
  ESMC_Calendar calendar;
  ESMC_Time startTime;
  ESMC_I4 yy1;
  ESMC_I4 h1;
  ESMC_CalKind_Flag calKind1;
  int tZ1;
  ESMC_Time stopTime;
  ESMC_I4 h2;
  ESMC_TimeInterval timeStep;
  const ESMC_I4 one=1;
  ESMC_Clock clock;
  ESMC_State importState;
  ESMC_State exportState;
  ESMC_GridComp gcomp;
  ESMC_CplComp cplcomp;
  ESMC_SciComp scicomp;
  
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  
  // Preparation
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Calendar object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  calendar = ESMC_CalendarCreate("Gregorian", ESMC_CALKIND_GREGORIAN, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Start Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  yy1=2006;
  h1=0;
  calKind1=ESMC_CALKIND_GREGORIAN;
  tZ1=-6;
  rc = ESMC_TimeSet(&startTime, yy1, h1, calendar, calKind1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Stop Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  h2=1;
  rc = ESMC_TimeSet(&stopTime, yy1, h2, calendar, calKind1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set a TimeInterval");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeIntervalSet(&timeStep, one);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Clock object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  clock = ESMC_ClockCreate("TEST_CLOCK",timeStep,startTime, stopTime,
          &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  importState = ESMC_StateCreate("my import state", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  exportState = ESMC_StateCreate("my export state", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

      
  // ESMC_GridComp tests

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  gcomp = ESMC_GridCompCreate("gridded component in C", "grid.rc", clock, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompPrint(gcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompInitialize() before calling"     
    "ESMC_GridCompSetServices()");
  strcpy(failMsg, "Did return ESMF_SUCCESS");
  rc = ESMC_GridCompInitialize(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc!=ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompSetServices() using myRegistrationInC()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompSetServices(gcomp, myRegistrationInC, &userRc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompInitialize() w/ myRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompInitialize(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompRun() w/ myRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompRun(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompFinalize() w/ myRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompFinalize(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompDestroy(&gcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  gcomp = ESMC_GridCompCreate("gridded Component in C w/ Fortran registration",
    "grid.rc", clock, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompSetServices() using my_RegistrationInFortran()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompSetServices(gcomp, FTN_X(my_registrationinfortran), &userRc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompInitialize() w/ my_RegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompInitialize(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompRun() w/ my_RegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompRun(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompFinalize() w/ my_RegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompFinalize(gcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompDestroy(&gcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  
  // ESMC_CplComp tests
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_CplComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  cplcomp = ESMC_CplCompCreate("coupler component in C", "grid.rc", clock, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_CplComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompPrint(cplcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompSetServices() using myCplRegistrationInC()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompSetServices(cplcomp, myCplRegistrationInC, &userRc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompInitialize() w/ myCplRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompInitialize(cplcomp, importState, exportState, clock, 1, 
    NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompRun() w/ myCplRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompRun(cplcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompFinalize() w/ myCplRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompFinalize(cplcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_CplComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompDestroy(&cplcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_CplComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  cplcomp = ESMC_CplCompCreate("coupler component in C w/ Fortran registration",
    "grid.rc", clock, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompSetServices() using my_RegistrationInFortran()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompSetServices(cplcomp, FTN_X(my_cplregistrationinfortran),
    &userRc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompInitialize() w/ my_RegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompInitialize(cplcomp, importState, exportState, clock, 1,
    NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompRun() w/ my_RegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompRun(cplcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_CplCompFinalize() w/ my_RegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompFinalize(cplcomp, importState, exportState, clock, 1, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_CplComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CplCompDestroy(&cplcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  
  // ESMC_SciComp tests
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_SciComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  scicomp = ESMC_SciCompCreate("science component in C", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_SciComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_SciCompPrint(scicomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_SciComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_SciCompDestroy(&scicomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  
  // Garbage collection
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Clock object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ClockDestroy(&clock);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Calendar object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CalendarDestroy(&calendar);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_StateDestroy(&importState);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_StateDestroy(&exportState);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
