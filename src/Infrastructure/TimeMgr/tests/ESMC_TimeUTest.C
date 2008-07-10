// $Id: ESMC_TimeUTest.C,v 1.1 2008/07/10 15:43:51 rosalind Exp $
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

#include <stdlib.h>
#include <string.h>

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

// Time header
#include "ESMC_Time.h"

// Calendar header
#include "ESMC_Calendar.h"

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
  ESMC_CalendarType calType;
  ESMC_CalendarType calType1=ESMC_CAL_GREGORIAN;
  int tZ;
  int tZ1=-6;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Calendar object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  calendar = ESMC_CalendarCreate(9, "Gregorian", ESMC_CAL_GREGORIAN, &rc);
  printf("After CalendarCreate rc = %d \n",rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Calendar object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_CalendarPrint(calendar);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set a Time\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimeSet(&time1, yy1, h1, calendar, calType1, tZ1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Time object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimePrint(time1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a Time\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_TimeGet(time1, &yy, &h, &calendarOut, &calType, &tZ);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that year parameter is set and get correctly\0");
  strcpy(failMsg, "yy is different from yy1\0");
  ESMC_Test((yy==yy1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that hour parameter is set and get correctly\0");
  strcpy(failMsg, "h is different from h1\0");
  ESMC_Test((h==h1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that CalendarType parameter is set and get correctly\0");
  strcpy(failMsg, "calType is different from calType1\0");
  ESMC_Test((calType==calType1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that TimeZone parameter is set and get correctly\0");
  strcpy(failMsg, "tZ is different from tZ1\0");
  ESMC_Test((tZ==tZ1), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Check that ESMC_Calendar parameter is set and get correctly\0");
  strcpy(failMsg, "calendar.ptr is different from calendarOut.ptr\0");
  ESMC_Test((calendar.ptr==calendarOut.ptr), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Calendar object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_CalendarDestroy(&calendar);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
