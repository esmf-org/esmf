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
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TimeUTest - Check ESMC_Time functionality
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
  
  ESMC_Calendar calendar;
  ESMC_Calendar calendarOut;
  ESMC_Time time1;
  ESMC_I4 yy;
  ESMC_I4 yy1=2006;
  ESMC_I4 h;
  ESMC_I4 h1=0;
  ESMC_CalKind_Flag calKind;
  ESMC_CalKind_Flag calKind1=ESMC_CALKIND_GREGORIAN;
  int tZ;
  int tZ1=-6;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Calendar object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  calendar = ESMC_CalendarCreate("Gregorian", ESMC_CALKIND_GREGORIAN, &rc);
  printf("After CalendarCreate rc = %d \n",rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Calendar object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CalendarPrint(calendar);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set a Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeSet(&time1, yy1, h1, calendar, calKind1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Time object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimePrint(time1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a Time");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_TimeGet(time1, &yy, &h, &calendarOut, &calKind, &tZ);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that TimeZone parameter is set and get correctly");
  strcpy(failMsg, "tZ is different from tZ1");
  ESMC_Test((tZ==tZ1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that ESMC_Calendar parameter is set and get correctly");
  strcpy(failMsg, "calendar.ptr is different from calendarOut.ptr");
  ESMC_Test((calendar.ptr==calendarOut.ptr), name, failMsg, &result, __FILE__,
    __LINE__, 0);
  //----------------------------------------------------------------------------

#endif
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Calendar object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_CalendarDestroy(&calendar);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
