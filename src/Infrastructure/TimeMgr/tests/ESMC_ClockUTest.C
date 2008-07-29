// $Id: ESMC_ClockUTest.C,v 1.13 2008/07/29 01:34:55 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------
//BOP
// !PROGRAM:  ESMF_ClockTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C Clock unit tests.
// The companion files ESMC\_Clock.h and ESMC\_Clock.C contain
// the declarations and definitions for the Clock methods.
//
// 
//
//EOP
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//ESMF header
#include "ESMC.h"

// associated class definition file
#include "ESMC_Clock.h"

// ESMC_Test function
#include "ESMC_Test.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_ClockUTest.C,v 1.13 2008/07/29 01:34:55 rosalind Exp $";
//-----------------------------------------------------------------------------


 int main(void)
 {
   // cumulative result: count failures; no failures equals "all pass"
   int result = 0;

   // individual test result code
   int rc;

   // individual test name
   char name[ESMF_MAXSTR];

   // individual test failure message
   char failMsg[ESMF_MAXSTR];

   //  the C clock object that points to the C++ allocated one
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
  ESMC_CalendarType calType1=ESMC_CAL_GREGORIAN;
  int tZ1=-6;
  const ESMC_I4 one=1;
  ESMC_TimeInterval currSimTime;
  ESMC_I8 advanceCount, advanceCount1;
  ESMC_I8 initSec, currSec1;
  ESMC_R8 initHour, currHour1;


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Calendar object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  calendar = ESMC_CalendarCreate(9, "Gregorian", ESMC_CAL_GREGORIAN, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Start Time\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimeSet(&startTime, yy1, h1, calendar, calType1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set Stop Time\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimeSet(&stopTime, yy1, h2, calendar, calType1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set a TimeInterval\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimeIntervalSet(&timeStep, one);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Clock object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  clock = ESMC_ClockCreate(10,"TEST_CLOCK",timeStep,startTime, stopTime,
  //      0, 0, 0, 
          &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Clock object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ClockPrint(clock);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get ESMC_Clock object current time and advance count\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ClockGet(clock, &currSimTime, &advanceCount);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Advance the Clock object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ClockAdvance(clock);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get ESMC_Clock object current time and advance count\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ClockGet(clock, &currSimTime, &advanceCount1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get currSimTime in seconds and in hours\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimeIntervalGet(currSimTime, &currSec1, &currHour1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Verify that currSimTime in second units\0");
  strcpy(failMsg, "Did not return 3600\0");
  ESMC_Test((int(currSec1)==3600), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Verify that currSimTime in hour units\0");
  strcpy(failMsg, "Did not return 1\0");
  ESMC_Test((int(currHour1)==1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Clock object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ClockDestroy(&clock);
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
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

   // return number of failures to environment; 0 = success (all pass)
   printf("result = %d\n",result);
   return(result);
 } // end unit test main()
