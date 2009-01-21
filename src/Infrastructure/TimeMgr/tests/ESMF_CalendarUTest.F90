! $Id: ESMF_CalendarUTest.F90,v 1.42.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_CalendarUTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_CalendarUTest - Test Calendar.
!
! !DESCRIPTION:
!
! The code in this file drives F90 Calendar unit tests.
! The companion file ESMF\_Calendar.F90 contains the definitions for the
! Clock methods.
!
! A good test reference tool is a day & date calculator at
!   http://www.numerical-recipes.com/julian.html
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CalendarUTest.F90,v 1.42.2.4 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, MM, DD, YY, days, totalDays, dayOfWeek
      integer(ESMF_KIND_I8) :: year
      integer(ESMF_KIND_I8) :: julianDay, advanceCounts

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: calendarsEqual, calendarsNotEqual, bool

      ! instantiate a clock 
      type(ESMF_Clock) :: clock_gregorian, &
                          clock_no_leap, clock_360day
      type(ESMF_Time) :: startTime, stopTime

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, gregorianCalendar1, &
                             gregorianCalendar2, julianCalendar, &
                             julianDayCalendar, &
                             no_leapCalendar, esmf_360dayCalendar
      type(ESMF_Calendar) :: customCalendar
      type(ESMF_CalendarType) :: cal_type, cal_type1, cal_type2
      integer, dimension(MONTHS_PER_YEAR) :: &
             days_per_month =(/30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30/)
      integer, dimension(MONTHS_PER_YEAR) :: &
         dayspermonth =(/1000, 0, 8900, -120, 930, 70, 80, 90, 0, -70, 90, 60/)

      ! instantiate Time Intervals
      type(ESMF_TimeInterval) :: timeStep

!-------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests
!     are always run.  When the environment variable, EXHAUSTIVE, is set to ON
!     then the EXHAUSTIVE and sanity tests both run.  If the EXHAUSTIVE
!     variable is set to OFF, then only the sanity unit tests run.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! initialize one calendar to be Gregorian type
      write(name, *) "Create Gregorian Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Destroy  Calendar
      ! Test Calendar Destroy
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Destroy Gregorian Type Calendar Test"
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a destroyed Calendar
      ! Test Calendar Destroy
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed Gregorian Type Calendar Test"
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a uncreated Calendar
      ! Test Calendar Destroy
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a uncreated Gregorian Type Calendar Test"
      call ESMF_CalendarDestroy(gregorianCalendar1, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_RC_OBJ_DELETED
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Deleted Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_RC_OBJ_NOT_CREATED
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Uncreated Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate deleted Calendar
      ! Test that validate subroutine returns ESMF_RC_OBJ_DELETED
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Validate desttroyed Calendar Test"
      call ESMF_CalendarValidate(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate uncreated Calendar
      ! Test that validate subroutine returns ESMF_RC_OBJ_NOT_CREATED
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Validate uncreated Calendar Test"
      call ESMF_CalendarValidate(gregorianCalendar1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar type of deleted Calendar
      write(name, *) "Get deleted Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_CalendarGet(gregorianCalendar, calendarType=cal_type, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar type of uncreated Calendar
      write(name, *) "Get deleted Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_CalendarGet(gregorianCalendar1, calendarType=cal_type, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_____UTest
      ! Commented out until bug 1648057 is closed
      ! Testing for calendar equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar1,calendar2)
      !write(failMsg, *) "Returned not equal"
      !write(name, *) "Calendar Equal of deleted and uncreated Calendars Test"
      !calendarsEqual = (gregorianCalendar == gregorianCalendar1)
      !call ESMF_Test((calendarsEqual), &
                      !name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Set calendar type of deleted Calendar
      write(name, *) "Set Calendar Type of deleted Calendar Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_CalendarSet(gregorianCalendar, calendarType=ESMF_CAL_NOLEAP, &
                            rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Set calendar type of uncreated Calendar
      write(name, *) "Set Calendar Type of uncreated Calendar Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_CalendarSet(gregorianCalendar1, calendarType=ESMF_CAL_NOLEAP, &
                            rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I8 test 4
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants
      year = year + 100 !    are 32-bit
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "IsLeapYear of deleted Calendar Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I8 test 4
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants
      year = year + 100 !    are 32-bit
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar1, year, rc=rc)
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "IsLeapYear of uncreated Calendar Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize one calendar to be Gregorian type
      write(name, *) "Initialize Gregorian Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_____UTest
      ! Commented out until bug 1648057 is closed
      ! Testing for calendar equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar1,calendar2)
      !write(failMsg, *) "Returned equal"
      !write(name, *) "Calendar Equal of created  and uncreated Calendars Test"
      !calendarsEqual = (gregorianCalendar == gregorianCalendar1)
      !call ESMF_Test((.not.calendarsEqual), &
                      !name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar  type
      write(name, *) "Get Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarGet(gregorianCalendar, calendarType=cal_type, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized Gregorian Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Gregorian Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate Gregorian Calendar Test"
      call ESMF_CalendarValidate(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Make a copy of Gregorian type calendar
      write(name, *) "Make copy of  Gregorian Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar1 = ESMF_CalendarCreate(gregorianCalendar, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar  type
      write(name, *) "Get Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarGet(gregorianCalendar1, calendarType=cal_type1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Test" 
      calendarsEqual = (gregorianCalendar == gregorianCalendar1)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      
                          
      !EX_UTest
      ! initialize another calendar to be Gregorian type
      write(name, *) "Initialize Gregorian Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar2 = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Test" 
      calendarsEqual = (gregorianCalendar2 == gregorianCalendar1)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar,calendartype)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal CalendarType Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CAL_GREGORIAN)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendartype,calendar)
      write(failMsg, *) "Returned not equal"
      write(name, *) "CalendarType Equal Calendar Test" 
      calendarsEqual = (ESMF_CAL_GREGORIAN == gregorianCalendar)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar,calendartype)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar Equal CalendarType Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CAL_NOLEAP)
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendartype,calendar)
      write(failMsg, *) "Returned equal"
      write(name, *) "CalendarType Equal Calendar Test" 
      calendarsEqual = (ESMF_CAL_NOLEAP == gregorianCalendar)
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set calendar type
      write(name, *) "Set Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(gregorianCalendar, calendarType=ESMF_CAL_NOLEAP, &
                            rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar,calendartype)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal CalendarType Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CAL_NOLEAP)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set calendar type
      write(name, *) "Set Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(gregorianCalendar, &
                            calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
  
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar,calendartype)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal CalendarType Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CAL_GREGORIAN)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize secand calendar to be Julian Day type
      write(name, *) "Initialize Julian Day Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      julianDayCalendar = ESMF_CalendarCreate("JulianDay", &
                                              ESMF_CAL_JULIANDAY, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar  type
      write(name, *) "Get Calendar Type Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarGet(julianDayCalendar, calendarType=cal_type2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized Julian Day Calendar Print Test"
      call ESMF_CalendarPrint(julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar inequality
      ! calendarsEqual = ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendars Equal Test" 
      calendarsEqual = (julianDayCalendar == gregorianCalendar1)
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar inequality
      ! calendarsNotEqual = ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendars Not Equal Test" 
      calendarsNotEqual = (julianDayCalendar /= gregorianCalendar1)
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar inequality
      ! calendarsNotEqual = ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendars Equal Test" 
      calendarsNotEqual = (gregorianCalendar2 /= gregorianCalendar1)
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type inequality
      ! calendarsNotEqual = ESMF_CalendarOperator(/=)(calendar,calendartype)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar and Calendar Type Equal Test" 
      calendarsNotEqual = (gregorianCalendar /= ESMF_CAL_360DAY)
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type inequality
      ! calendarsNotEqual = ESMF_CalendarOperator(/=)(calendartype,calendar)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar Type and Calendar Equal Test" 
      calendarsNotEqual = (ESMF_CAL_360DAY /= gregorianCalendar)
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type inequality
      ! calendarsNotEqual = ESMF_CalendarOperator(/=)(calendar,calendartype)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar and Calendar Type Equal Test" 
      calendarsNotEqual = (gregorianCalendar /= ESMF_CAL_GREGORIAN)
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type inequality
      ! calendarsNotEqual = ESMF_CalendarOperator(/=)(calendartype,calendar)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Type and Calendar Equal Test" 
      calendarsNotEqual = (ESMF_CAL_GREGORIAN /= gregorianCalendar)
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarTypeOperator(==)(calendarType1,calendarType2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Type Equal Test"
      calendarsEqual = (cal_type == cal_type1)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type inequality
      ! calendarsEqual = ESMF_CalendarTypeOperator(==)(calendarType1,calendarType2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar Type Not Equal Test"
      calendarsEqual = (cal_type1 == cal_type2)
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type inequality
      ! calendarsNotEqual = ESMF_CalendarTypeOperator(/=)(calendarType1,calendarType2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar Type Not Equal Test"
      calendarsNotEqual = (cal_type /= cal_type2)
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsNotEqual = ESMF_CalendarTypeOperator(/=)(calendarType1,calendarType2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Type Equal Test"
      calendarsNotEqual = (cal_type /= cal_type1)
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for calendar type equality
      ! calendarsEqual = ESMF_CalendarTypeOperator(==)(calendarType1,calendarType2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Type Equal Test"
      calendarsEqual = (cal_type == cal_type1)
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Validate Julian Day Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate Julian Day Calendar Test"
      call ESMF_CalendarValidate(julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! initialize third calendar to be No Leap type
      write(name, *) "Initialize No Leap Year Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      no_leapCalendar = ESMF_CalendarCreate("No_Leap", ESMF_CAL_NOLEAP, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized No Leap Calendar Print Test"
      call ESMF_CalendarPrint(no_leapCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate No Leap Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate No Leap Calendar Test"
      call ESMF_CalendarValidate(no_leapCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize third calendar to be 360 day type
      write(name, *) "Initialize 360 Day Year Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      esmf_360dayCalendar = ESMF_CalendarCreate("360Day", ESMF_CAL_360DAY, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized 360 Day  Calendar Print Test"
      call ESMF_CalendarPrint(esmf_360dayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Validate 360 Day Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate 360 Day Calendar Test"
      call ESMF_CalendarValidate(esmf_360dayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate("CustomCalendar", &
                                        daysPerMonth=days_per_month, &
					secondsPerDay=86400, &
					daysPerYear=360, &
					daysPerYearDn=1, &
					daysPerYearDd=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized Custom Type Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Type Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate Custom Type Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type with overflow Calendar Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate("CustomCalendar", &
                                        daysPerMonth=dayspermonth, &
					secondsPerDay=86400, &
					daysPerYear=100000000, &
					daysPerYearDn=1, &
					daysPerYearDd=1, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine doesn't return ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Un-initialized Custom Type Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Type Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Validate Bad Custom Type Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type with negative number seconds Calendar Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate("CustomCalendar", &
                                        daysPerMonth=dayspermonth, &
					secondsPerDay=-400, &
					daysPerYear=1, &
					daysPerYearDn=1, &
					daysPerYearDd=0, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine doesn't return ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Un-initialized Custom Type Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Type Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Validate Custom Type Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type with nonsense numbers Calendar Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate("CustomCalendar", &
                                        daysPerMonth=dayspermonth, &
					secondsPerDay=86400, &
					daysPerYear=1, &
					daysPerYearDn=1, &
					daysPerYearDd=0, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine doesn't return ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Un-initialized Custom Type Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Type Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Validate Custom Type Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Start Time for the Gregorian Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit of Gregorian Calendar Test"
      call ESMF_TimeSet(startTime, yy=-4800, mm=3, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Start Time for the Gregorian Calencar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit minus 1 second of Gregorian Calendar Test"
      call ESMF_TimeSet(startTime, yy=-4800, mm=3, dd=1, s=-1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time for the Gregorian Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit of Gregorian Calendar Test"
      year=292277019914_ESMF_KIND_I8
      call ESMF_TimeSet(stopTime, yy_i8=year, mm=10, dd=29, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time for the Gregorian Calencar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit plus 86400 second of Gregorian Calendar Test"
      year=292277019914_ESMF_KIND_I8
      call ESMF_TimeSet(stopTime, yy_i8=year, mm=10, dd=29, s=86400, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Start Time for the Julian Day Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit of Julian Day Calendar Test"
      call ESMF_TimeSet(startTime, d=-32044, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! TODO: For the following 2 tests, determine JD equivalent to 3/1/-4900
      ! (not -4800) Gregorian, which is lower limit of JD->Gregorian date
      ! conversion algorithm.
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Start Time for the Julian Day Calendar
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit (of Gregorian) minus 1 day of Julian Day Calendar Test"
      call ESMF_TimeSet(startTime, d=-32045, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting the Start Time to Gregorian Calendar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Convert Start Time to Gregorian Calendar Test"
      call ESMF_TimeSet(startTime, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, rc=rc)
      !call ESMF_Test((rc.ne.ESMF_SUCCESS), &  ! TODO: replace when above test
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &   !       fixed.
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time for the Julian Day Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit of Julian Day Calendar Test"
      julianDay=100000000000000_ESMF_KIND_I8
      call ESMF_TimeSet(stopTime, d_i8=julianDay, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time for the Julian Day Calencar
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit plus 1 day of Julian Day Calendar Test"
      julianDay=100000000000001_ESMF_KIND_I8
      call ESMF_TimeSet(stopTime, d_i8=julianDay,  &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
       

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Gregorian Cal. Start Time init with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Gregorian Cal. Stop Time init with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      days=1
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      write(name, *) "Set Time Interval Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization with Gregorian CalendarTest"
      clock_gregorian = ESMF_ClockCreate("Gregorian Clock", timeStep, &
                                         startTime, stopTime, rc=rc)

      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! uncomment to test bug #959580
      !call ESMF_ClockDestroy(clock_gregorian, rc)

      totalDays=0
      ! time step from start time to stop time
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_TimePrint(stopTime, rc=rc)
      do while (.not.ESMF_ClockIsStopTime(clock_gregorian, rc) .and. &
                rc == ESMF_SUCCESS)
        call ESMF_ClockAdvance(clock_gregorian, timeStep=timeStep, rc=rc)
        totalDays=totalDays+days
      end do

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 73049 from -100 to +100 years in Gregorian Cal. Test"
      call ESMF_Test((totalDays.eq.73049), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=no_leapCalendar, rc=rc)
      write(name, *) "No Leap Cal. Start Time init with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=no_leapCalendar, rc=rc)
      write(name, *) "No Leap Cal. Stop Time init with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      clock_no_leap = ESMF_ClockCreate("No Leap Clock", timeStep, &    
                                        startTime, stopTime, rc=rc) 
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      totalDays=0
      days=1
      call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_TimePrint(stopTime, rc=rc)
      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock_no_leap, rc))
        call ESMF_ClockAdvance(clock_no_leap, timeStep=timeStep, rc=rc)
        totalDays=totalDays+days
      end do

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 73000 from -100 to +100 years in No Leap Cal. Test"
      call ESMF_Test((totalDays.eq.73000), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      write(name, *) "360 Day Cal. Start Time init with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=esmf_360dayCalendar, rc=rc)
      write(name, *) "360 Day Cal. Stop Time init with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      clock_360day = ESMF_ClockCreate("360 Day Clock", timeStep, startTime, &
                                         stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      totalDays=0
      days=1
      call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_TimePrint(stopTime, rc=rc)
      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock_360day, rc))
        call ESMF_ClockAdvance(clock_360day, timeStep=timeStep, rc=rc)
        totalDays=totalDays+days
      end do

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 72000 from -100 to +100 years in 360 Day Cal. Test"
      call ESMF_Test((totalDays.eq.72000), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Total Counts 72000 from -100 to +100 years in 360 Day Cal. Test"
      call ESMF_ClockGet(clock_360day, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "Results Total Counts = ", advanceCounts
      call ESMF_Test((advanceCounts.eq.72000), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Gregorian date to Julian Day
      !   from http://www.friesian.com/numbers.htm
      write(failMsg, *) " Did not return days = 2450713 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1997, mm=9, dd=21, & 
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Gregorian to Julian Day Test 1"
      call ESMF_Test((days.eq.2450713).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------  
      !EX_UTest
      ! Test Converting Gregorian date to Julian Day
      !   from Henry F. Fliegel and Thomas C. Van Flandern, in
      !   Communications of the ACM, CACM, volume 11, number 10,
      !   October 1968, p. 657.
      write(failMsg, *) " Did not return days = 2440588 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1970, mm=1, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Gregorian to Julian Day Test 2"
      call ESMF_Test((days.eq.2440588).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Create Julian Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      julianCalendar = ESMF_CalendarCreate("Julian", ESMF_CAL_JULIAN, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian date to Julian Day
      !   Last "official" day of the Julian calendar
      write(failMsg, *) " Did not return days = 2299160 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1582, mm=10, dd=4, & 
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Julian to Julian Day Test 1"
      call ESMF_Test((days.eq.2299160).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "10/4/1582 Julian = ", days, " Julian days."

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test day of the week
      ! Last day of Julian calendar 10/4/1582 was a Thursday
      ! http://www.hermetic.ch/cal_stud/cal_art.html
      write(failMsg, *) " Did not return dayOfWeek = 4 and ESMF_SUCCESS"
      call ESMF_TimeGet(stopTime, dayOfWeek=dayOfWeek, rc=rc)
      write(name, *) "Julian Time Get day of the week test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfWeek.eq.4), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian Day to Julian date
      !   Last "official" day of the Julian calendar
      write(failMsg, *) " Did not return 10/4/1582 and ESMF_SUCCESS"
      call ESMF_TimeGet(stopTime, mm=MM, dd=DD, yy=YY, rc=rc)
      write(name, *) "Convert Julian Day to Julian Date Test 1"
      call ESMF_Test(MM.eq.10 .and. DD.eq.4 .and. YY.eq.1582 .and. &
                     rc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
      print *, "Julian Date = ", MM, "/", DD, "/", YY
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian Date to Gregorian date
      !   Last "official" day of the Julian calendar is 10/14/1582 in
      !   the proleptic Gregorian calendar.
      write(failMsg, *) " Did not return 10/14/1582 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, mm=MM, dd=DD, yy=YY, rc=rc)
      write(name, *) "Convert Julian Date to Gregorian Date Test 1"
      call ESMF_Test(MM.eq.10 .and. DD.eq.14 .and. YY.eq.1582 .and. &
                     rc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
      print *, "10/4/1582 Julian = Proleptic Gregorian Date ", &
                MM, "/", DD, "/", YY
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Gregorian date to Proleptic Julian Date
      !   First "official" day of the Gregorian calendar 10/15/1582 is
      !   10/5/1582 in the Proleptic Julian calendar.
      write(failMsg, *) " Did not return 10/5/1582 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1582, mm=10, dd=15, & 
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, mm=MM, dd=DD, yy=YY, rc=rc)
      write(name, *) "Convert Gregorian Date to Proleptic Julian Date Test 1"
      call ESMF_Test(MM.eq.10 .and. DD.eq.5 .and. YY.eq.1582 .and. &
                     rc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
      print *, "10/15/1582 Gregorian = Proleptic Julian Date ", &
                MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test day of the week
      ! First day of Gregorian calendar 10/15/1582 was a Friday
      ! http://www.hermetic.ch/cal_stud/cal_art.html
      write(failMsg, *) " Did not return dayOfWeek = 5 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1582, mm=10, dd=15, & 
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, dayOfWeek=dayOfWeek, rc=rc)
      write(name, *) "Gregorian Time Get day of the week test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfWeek.eq.5), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian date to Julian Day
      !   Oldest Julian date, 3/1/-4712, convertible to Julian Day 60
      !   D.A. Hatcher algorithm limit
      write(failMsg, *) " Did not return days = 60 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=-4712, mm=3, dd=1, & 
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Julian to Julian Day Test 2"
      call ESMF_Test((days.eq.60).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "3/1/-4712 Julian = ", days, " Julian days."

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian Day to Julian date
      !   Smallest Julian Day, 60 convertible to Julian Date 3/1/-4712
      !   D.A. Hatcher algorithm limit
      write(failMsg, *) " Did not return 3/1/-4712 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, d=60, calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, mm=MM, dd=DD, yy=YY, rc=rc)
      write(name, *) "Convert Julian Day to Julian Date Test 2"
      call ESMF_Test(MM.eq.3 .and. DD.eq.1 .and. YY.eq.-4712 .and. &
                     rc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
      print *, "Julian Day 60 is Julian Date ", MM, "/", DD, "/", YY
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Julian Leap Year in a negative year.
      write(failMsg, *) " Did not return 2/29/-4 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, mm=2, dd=29, yy=-4, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, mm=MM, dd=DD, yy=YY, rc=rc)
      write(name, *) "Negative Leap Year Test"
      call ESMF_Test(MM.eq.2 .and. DD.eq.29 .and. YY.eq.-4 .and. &
                     rc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
      print *, "Leap Year Test returned ", MM, "/", DD, "/", YY
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian date to Julian Day
      !   Solar eclipse at Nineveh on 6/15/-762, JD 1442903
      !   http://www.hermetic.ch/cal_stud/jdn.htm#julian_day_number
      write(failMsg, *) " Did not return days = 1442903 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=-762, mm=6, dd=15, & 
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Julian to Julian Day Test 3"
      call ESMF_Test((days.eq.1442903).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "6/15/-762 Julian = ", days, " Julian days."

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Julian date to Julian Day
      !   Lunar eclipse at Nineveh on 4/15/-424, JD 1566297
      !   http://www.hermetic.ch/cal_stud/jdn.htm#julian_day_number
      write(failMsg, *) " Did not return days = 1566297 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=-424, mm=4, dd=15, & 
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Julian to Julian Day Test 4"
      call ESMF_Test((days.eq.1566297).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "4/15/-424 Julian = ", days, " Julian days."

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I4 test 1
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, 2000, rc=rc)
      write(name, *) "IsLeapYear test 1"
      call ESMF_Test(bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I4 test 2
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, 2100, rc=rc)
      write(name, *) "IsLeapYear test 2"
      call ESMF_Test(.not.bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I8 test 3
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants are 32-bit
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 3"
      call ESMF_Test(bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I8 test 4
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants
      year = year + 100 !    are 32-bit
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 4"
      call ESMF_Test(.not.bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I4 test 5
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      bool = ESMF_CalendarIsLeapYear(julianCalendar, 2100, rc=rc)
      write(name, *) "IsLeapYear test 5"
      call ESMF_Test(bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I4 test 6
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      bool = ESMF_CalendarIsLeapYear(julianCalendar, 2005, rc=rc)
      write(name, *) "IsLeapYear test 6"
      call ESMF_Test(.not.bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I8 test 7
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants
      year = year + 100 !    are 32-bit
      bool = ESMF_CalendarIsLeapYear(julianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 7"
      call ESMF_Test(bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year method I8 test 8
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants
      year = year + 10  !   are 32-bit
      bool = ESMF_CalendarIsLeapYear(julianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 8"
      call ESMF_Test(.not.bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      call ESMF_CalendarDestroy(julianCalendar, rc)
      call ESMF_CalendarDestroy(esmf_360dayCalendar, rc)
      call ESMF_CalendarDestroy(no_leapCalendar, rc)
      call ESMF_CalendarDestroy(julianDayCalendar, rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
#endif 

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_CalendarUTest
