// $Id: ESMC_ComponentUTest.C,v 1.3 2008/08/26 23:47:51 theurich Exp $
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
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myRunInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myRunInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

void myFinalInC(ESMC_GridComp gcomp, ESMC_State importState,
  ESMC_State exportState, ESMC_Clock *clock, int *rc){
  // do something here
  printf("I am in myFinalizeInC()\n");
  
  ESMC_StatePrint(importState);
  ESMC_ClockPrint(*clock);
  
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}


void myRegistrationInC(ESMC_GridComp gcomp, int *rc){
  // register Init(), Run(), Finalize()
  printf("I am in myRegistrationInC()\n");
  ESMC_GridCompPrint(gcomp, "");
  
  ESMC_GridCompSetEntryPoint(gcomp, ESMC_SetInit, myInitInC, 0);
  ESMC_GridCompSetEntryPoint(gcomp, ESMC_SetRun, myRunInC, 0);
  ESMC_GridCompSetEntryPoint(gcomp, ESMC_SetFinal, myFinalInC, 0);
    
  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
}

extern "C"{
  void FTN(myregistrationinfortran)(ESMC_GridComp comp, int *rc);
}


int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Calendar object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Calendar calendar =
    ESMC_CalendarCreate(9, "Gregorian", ESMC_CAL_GREGORIAN, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Start Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Time startTime;
  ESMC_I4 yy1=2006;
  ESMC_I4 h1=0;
  ESMC_CalendarType calType1=ESMC_CAL_GREGORIAN;
  int tZ1=-6;
  rc = ESMC_TimeSet(&startTime, yy1, h1, calendar, calType1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Stop Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Time stopTime;
  ESMC_I4 h2=1;
  rc = ESMC_TimeSet(&stopTime, yy1, h2, calendar, calType1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set a TimeInterval");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_TimeInterval timeStep;
  const ESMC_I4 one=1;
  rc = ESMC_TimeIntervalSet(&timeStep, one);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Clock object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Clock clock =
    ESMC_ClockCreate(10,"TEST_CLOCK",timeStep,startTime, stopTime,
  //      0, 0, 0, 
          &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_State importState = ESMC_StateCreate("my import state", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_State exportState = ESMC_StateCreate("my export state", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_GridComp gcomp =
    ESMC_GridCompCreate("my gridded component in C", ESMF_ATM,"grid.rc", clock,
    &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompPrint(gcomp, "");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompSetServices() using myRegistrationInC()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompSetServices(gcomp, myRegistrationInC);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompInitialize() w/ myRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompInitialize(gcomp, importState, exportState, clock, 0);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompRun() w/ myRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompRun(gcomp, importState, exportState, clock, 0);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompFinalize() w/ myRegistrationInC() methods");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompFinalize(gcomp, importState, exportState, clock, 0);
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
  gcomp =
    ESMC_GridCompCreate("my gridded component in C with Fortran registration",
      ESMF_ATM,"grid.rc", clock, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompSetServices() using myRegistrationInFortran()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompSetServices(gcomp, FTN(myregistrationinfortran));
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompInitialize() w/ myRegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompInitialize(gcomp, importState, exportState, clock, 0);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompRun() w/ myRegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompRun(gcomp, importState, exportState, clock, 0);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_GridCompFinalize() w/ myRegistrationInFortran() meths");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompFinalize(gcomp, importState, exportState, clock, 0);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_GridComp object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridCompDestroy(&gcomp);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  
// add CPL tests here
  
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Clock object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ClockDestroy(&clock);
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
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
