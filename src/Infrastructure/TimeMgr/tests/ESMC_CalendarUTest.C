// $Id: ESMC_CalendarUTest.C,v 1.2 2008/07/09 14:20:17 rosalind Exp $
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
// !PROGRAM: ESMC_CalendarUTest - Check ESMC_Calendar functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

// Calendar header
#include "ESMC_Calendar.h"

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  
  ESMC_Calendar calendar;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Calendar object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  calendar = ESMC_CalendarCreate(9, "Gregorian", ESMC_CAL_GREGORIAN, &rc);
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
