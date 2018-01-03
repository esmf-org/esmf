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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//ESMF header
#include "ESMC.h"

// ESMC_Test function
#include "ESMC_Test.h"

//-----------------------------------------------------------------------------
//BOP
// !PROGRAM:  ESMC_ClockUTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C Clock unit tests.
// The companion files ESMC\_Clock.h and ESMC\_Clock.C contain
// the declarations and definitions for the Clock methods.
//
//EOP
//-----------------------------------------------------------------------------


int main(void){
 
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  //  the ESMC_Clock object
  ESMC_Clock clock;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ESMC_TimeInterval timeStep,runDuration;
  ESMC_Time startTime, stopTime, refTime; 
  ESMC_Calendar calendar;
  ESMC_I4 yy1=2006;
  ESMC_I4 h1=0;
  ESMC_I4 h2=1;
  ESMC_CalKind_Flag calKind1=ESMC_CALKIND_GREGORIAN;
  int tZ1=-6;
  const ESMC_I4 one=1;
  ESMC_TimeInterval currSimTime;
  ESMC_I8 advanceCount, advanceCount1;
  ESMC_I8 initSec, currSec1;
  ESMC_R8 initHour, currHour1;


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
  rc = ESMC_TimeSet(&startTime, yy1, h1, calendar, calKind1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Stop Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
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
  clock = ESMC_ClockCreate("TEST_CLOCK", timeStep, startTime, stopTime, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Clock object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ClockPrint(clock);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get ESMC_Clock object current time and advance count");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ClockGet(clock, &currSimTime, &advanceCount);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Advance the Clock object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ClockAdvance(clock);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get ESMC_Clock object current time and advance count");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ClockGet(clock, &currSimTime, &advanceCount1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get currSimTime in seconds and in hours");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeIntervalGet(currSimTime, &currSec1, &currHour1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Verify that currSimTime in second units");
  strcpy(failMsg, "Did not return 3600");
  ESMC_Test((int(currSec1)==3600), name, failMsg, &result, __FILE__, __LINE__,
    0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Verify that currSimTime in hour units");
  strcpy(failMsg, "Did not return 1");
  ESMC_Test((int(currHour1)==1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

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

/*
#ifdef ESMF_TESTEXHAUSTIVE
#if 0
   // perform exhaustive tests here;
   //   see #else below for non-exhaustive tests
   // future release will use run-time switching mechanism

   // test dynamic allocation of ESMC_Clock
   //   also tests default constructor
   clock_ptr = ESMC_ClockCreate(args, &rc);
   sprintf(name, "ESMC_ClockCreate"); 
   sprintf(failMsg, "rc = %d, clock_ptr = %p, args = %f",
           rc, clock_ptr, args);
   ESMC_Test((clock_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_Clock
   rc = clock_ptr->ESMC_ClockConstruct(args);
   sprintf(name, "ESMC_ClockConstruct"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test initialization of members of statically allocated ESMC_Clock
   //   may want to read back values via Get methods for comparison
   rc = clock_ptr->ESMC_ClockSet(args);
   sprintf(name, "ESMC_ClockSet"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_ClockConfig config_set;
   rc = clock_ptr->ESMC_ClockSetConfig(config_set);
   sprintf(name, "ESMC_ClockSetConfig"); 
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_ClockConfig config_get;
   rc = clock_ptr->ESMC_ClockGetConfig(&config_get);
   sprintf(name, "ESMC_ClockGetConfig"); 
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_Clock members values
   <value type> value_set;
   rc = clock_ptr->ESMC_ClockSet<Value>(value_set);
   sprintf(name, "ESMC_ClockSet<Value>"); 
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_Clock members values,
   //   compare to values set previously
   <value type> value_get;
   rc = clock_ptr->ESMC_ClockGet<Value>(&value_get);
   sprintf(name, "ESMC_ClockGet<Value>"); 
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = clock_ptr->ESMC_ClockValidate(validate_options);
   sprintf(name, "ESMC_ClockValidate"); 
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = clock_ptr->ESMC_ClockPrint(print_options);
   sprintf(name, "ESMC_ClockPrint"); 
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_Clock
   rc = clock_ptr->ESMC_ClockDestruct();
   sprintf(name, "ESMC_ClockDestruct"); 
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_Clock
   //   also tests destructor
   rc = ESMC_ClockDestroy(clock_ptr);
   sprintf(name, "ESMC_ClockDestroy"); 
   sprintf(failMsg, "rc = %d, clock_ptr = %p", rc, clock_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

#endif
#else

   // perform non-exhaustive tests here;
   //   use same templates as above

#endif
*/

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // return number of failures to environment; 0 = success (all pass)
  printf("result = %d\n",result);
  return(result);
  
}
