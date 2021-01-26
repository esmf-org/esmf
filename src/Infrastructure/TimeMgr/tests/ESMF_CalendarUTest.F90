! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
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
      use ESMF
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

     ! instantiate a calendar
     type(ESMF_Calendar) :: gregorianCalendar

     logical :: isCreated

#ifdef ESMF_TESTEXHAUSTIVE
      integer :: DD, MM, YY, totalDays, days, sols, H, M, S
      real(ESMF_KIND_R8) :: sols_r8

      ! instantiate a calendar
      type(ESMF_Calendar) :: no_leapCalendar, modifiedJulianDayCalendar, &
          julianDayCalendar, gregorianCalendar1, julianCalendar
      type(ESMF_Calendar) :: customCalendar, customCalendar2, marsCalendar, &
                             esmf_360dayCalendar, gregorianCalendar2
      type(ESMF_CalKind_Flag) :: cal_kind1, cal_kind2, cal_kind

      ! instantiate a clock 
      type(ESMF_Clock) :: clock_360day, clock_no_leap, clock_gregorian

      ! set up days per month arrays, 12 months per year
      integer, dimension(12) :: &
             days_per_month =(/30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30/)
      integer, dimension(12) :: &
         dayspermonth =(/1000, 0, 8900, -120, 930, 70, 80, 90, 0, -70, 90, 60/)
      integer, dimension(12) :: &
       customDaysPerMonth =(/10, 20, 30, 40, 50, 60, 55, 45, 35, 25, 15, 5/)

      integer :: dayOfWeek
      integer(ESMF_KIND_I8) :: advanceCounts, julianDay, year, days_i8
      type(ESMF_Time) :: startTime, stopTime
      logical :: bool, calendarsNotEqual, calendarsEqual
      character(ESMF_MAXSTR) :: cal_name

      ! instantiate Time Intervals
      type(ESMF_TimeInterval) :: timeStep
#endif

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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! initialize one calendar to be Gregorian type
      write(name, *) "Create Gregorian Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                              name="Gregorian", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Destroy  Calendar
      ! Test Calendar Destroy
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Destroy Gregorian Kind Calendar Test"
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Calendar IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_CalendarIsCreated(gregorianCalendar)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Calendar IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_CalendarIsCreated(gregorianCalendar, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test Calendar for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                          name="Gregorian", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Calendar IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_CalendarIsCreated(gregorianCalendar)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Calendar IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_CalendarIsCreated(gregorianCalendar, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test Calendar for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Calendar IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_CalendarIsCreated(gregorianCalendar)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Calendar IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_CalendarIsCreated(gregorianCalendar, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------


#ifdef ESMF_TESTEXHAUSTIVE

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Destroy a destroyed Calendar
      ! Test Calendar Destroy
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed Gregorian Kind Calendar Test"
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Destroy a uncreated Calendar
      ! Test Calendar Destroy
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a uncreated Gregorian Kind Calendar Test"
      call ESMF_CalendarDestroy(gregorianCalendar1, rc=rc)
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
      ! Get calendar kind of deleted Calendar
      write(name, *) "Get deleted Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_CalendarGet(gregorianCalendar, calkindflag=cal_kind, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar kind of uncreated Calendar
      write(name, *) "Get deleted Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_CalendarGet(gregorianCalendar1, calkindflag=cal_kind, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test fix for bug 1648057
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar equality of deleted and uncreated Calendars Test"
      calendarsEqual = (gregorianCalendar == gregorianCalendar1)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testig ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar Non Equality of deleted and uncreated Calendars Test"
      calendarsNotEqual = (gregorianCalendar /= gregorianCalendar1)
      !                                      ^-- exercise Calendar == operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar equality of two deleted Calendars Test"
      calendarsEqual = (gregorianCalendar == gregorianCalendar)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testig ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equality of two uncreated Calendars Test"
      calendarsNotEqual = (gregorianCalendar1 /= gregorianCalendar1)
      !                                       ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testig ESMF_CalendarOperator(==)(calendar1,calkindflag)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted Calendar Equal Calendar kind Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CALKIND_GREGORIAN)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testig ESMF_CalendarOperator(==)(calkindflag,calendar1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar kind Equal uncreated Calendar Test" 
      calendarsEqual = (ESMF_CALKIND_GREGORIAN == gregorianCalendar1)
      !                                        ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testig ESMF_CalendarOperator(/=)(calendar1,calkindflag)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated Calendar Not Equal Calendar kind Test" 
      calendarsNotEqual = (gregorianCalendar1 /= ESMF_CALKIND_GREGORIAN)
      !                                       ^-- exercise Calendar /= operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testig ESMF_CalendarOperator(/=)(calkindflag,calendar1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar kind Not Equal Deleted Calendar Test" 
      calendarsNotEqual = (ESMF_CALKIND_GREGORIAN /= gregorianCalendar)
      !                                           ^-- exercise Calendar /= operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set calendar kind of deleted Calendar
      write(name, *) "Set Calendar kind of deleted Calendar Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_CalendarSet(gregorianCalendar, &
        calkindflag=ESMF_CALKIND_NOLEAP, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set calendar kind of uncreated Calendar
      write(name, *) "Set Calendar kind of uncreated Calendar Test"
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_CalendarSet(gregorianCalendar1, &
        calkindflag=ESMF_CALKIND_NOLEAP, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Leap Year method I8 test 4
      year = 5000000100_ESMF_KIND_I8
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "IsLeapYear of deleted Calendar Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Leap Year method I8 test 4
      year = 5000000100_ESMF_KIND_I8
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar1, yy=year, rc=rc)
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "IsLeapYear of uncreated Calendar Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize one calendar to be Gregorian type
      write(name, *) "Initialize Gregorian Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                              name="Gregorian", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarAssignment(=)(calendar,calendar)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Assignment Test"
      gregorianCalendar2 = gregorianCalendar  ! exercise default F90 Calendar =
                                              ! assignment
      call ESMF_CalendarGet(gregorianCalendar2, calkindflag=cal_kind, &
                            name=cal_name, rc=rc)
      call ESMF_Test((gregorianCalendar2.eq.gregorianCalendar .and. &
                      cal_kind.eq.ESMF_CALKIND_GREGORIAN .and. &
                      cal_name.eq."Gregorian"), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test fix for bug 1648057
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar equality of created and uncreated Calendars Test"
      calendarsEqual = (gregorianCalendar == gregorianCalendar1)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar  type
      write(name, *) "Get Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarGet(gregorianCalendar, calkindflag=cal_kind, rc=rc)
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
      ! Make a copy of Gregorian Kind Calendar
      write(name, *) "Make copy of  Gregorian Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar1 = ESMF_CalendarCreate(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar  type
      write(name, *) "Get Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarGet(gregorianCalendar1, calkindflag=cal_kind1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Test" 
      calendarsEqual = (gregorianCalendar == gregorianCalendar1)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      
      !EX_UTest
      ! initialize another calendar to be Gregorian type
      write(name, *) "Initialize Gregorian Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      gregorianCalendar2 = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                               name="Gregorian", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Test" 
      calendarsEqual = (gregorianCalendar2 == gregorianCalendar1)
      !                                    ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar,calkindflag)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Calendar kind Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CALKIND_GREGORIAN)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calkindflag,calendar)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar kind Equal Calendar Test" 
      calendarsEqual = (ESMF_CALKIND_GREGORIAN == gregorianCalendar)
      !                                        ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar,calkindflag)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar Equal Calendar kind Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CALKIND_NOLEAP)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calkindflag,calendar)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar kind Equal Calendar Test" 
      calendarsEqual = (ESMF_CALKIND_NOLEAP == gregorianCalendar)
      !                                     ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set calendar kind
      write(name, *) "Set Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(gregorianCalendar, &
        calkindflag=ESMF_CALKIND_NOLEAP, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar,calkindflag)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Calendar kind Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CALKIND_NOLEAP)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set calendar kind
      write(name, *) "Set Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(gregorianCalendar, &
                            calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
  
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar,calkindflag)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar Equal Calendar kind Test" 
      calendarsEqual = (gregorianCalendar == ESMF_CALKIND_GREGORIAN)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize secand calendar to be Julian Day type
      write(name, *) "Initialize Julian Day Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      julianDayCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIANDAY, &
                                              name="JulianDay", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Get calendar  type
      write(name, *) "Get Calendar kind Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarGet(julianDayCalendar, calkindflag=cal_kind2, rc=rc)
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
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendars Equal Test" 
      calendarsEqual = (julianDayCalendar == gregorianCalendar1)
      !                                   ^-- exercise Calendar == operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendars Not Equal Test" 
      calendarsNotEqual = (julianDayCalendar /= gregorianCalendar1)
      !                                      ^-- exercise Calendar /= operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendars Equal Test" 
      calendarsNotEqual = (gregorianCalendar2 /= gregorianCalendar1)
      !                                       ^-- exercise Calendar /= operator
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(gregorianCalendar1, rc=rc)
      call ESMF_CalendarDestroy(gregorianCalendar2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar and Calendar kind Equal Test" 
      calendarsNotEqual = (gregorianCalendar /= ESMF_CALKIND_360DAY)
      !                                      ^-- exercise Calendar /= operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calkindflag,calendar)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar kind and Calendar Equal Test" 
      calendarsNotEqual = (ESMF_CALKIND_360DAY /= gregorianCalendar)
      !                                        ^-- exercise Calendar /= operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calendar,calkindflag)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar and Calendar kind Equal Test" 
      calendarsNotEqual = (gregorianCalendar /= ESMF_CALKIND_GREGORIAN)
      !                                      ^-- exercise Calendar /= operator
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calkindflag,calendar)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar kind and Calendar Equal Test" 
      calendarsNotEqual = (ESMF_CALKIND_GREGORIAN /= gregorianCalendar)
      !                                           ^-- exercise Calendar /= operator
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarKind(==)(calkindflag1,calkindflag2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar kind Equal Test"
      calendarsEqual = (cal_kind == cal_kind1)
      !                          ^-- exercise Calendar kind /= operator
      call ESMF_Test((calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarKind(==)(calkindflag1,calkindflag2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar kind Not Equal Test"
      calendarsEqual = (cal_kind1 == cal_kind2)
      !                           ^-- exercise Calendar kind /= operator
      call ESMF_Test((.not.calendarsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarKind(/=)(calkindflag1,calkindflag2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Calendar kind Not Equal Test"
      calendarsNotEqual = (cal_kind /= cal_kind2)
      !                             ^-- exercise Calendar kind /= operator
      call ESMF_Test((calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarKind(/=)(calkindflag1,calkindflag2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar kind Equal Test"
      calendarsNotEqual = (cal_kind /= cal_kind1)
      !                             ^-- exercise Calendar kind /= operator
      call ESMF_Test((.not.calendarsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarKind(==)(calkindflag1,calkindflag2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Calendar kind Equal Test"
      calendarsEqual = (cal_kind == cal_kind1)
      !                          ^-- exercise Calendar kind /= operator
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
      write(name, *) "Initialize No Leap Year Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      no_leapCalendar = ESMF_CalendarCreate(ESMF_CALKIND_NOLEAP, &
                                            name="No_Leap", rc=rc)
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
      write(name, *) "Initialize 360 Day Year Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      esmf_360dayCalendar = ESMF_CalendarCreate(ESMF_CALKIND_360DAY, &
                                                name="360Day", rc=rc)
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
      write(name, *) "Initialize Custom Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate(daysPerMonth=days_per_month, &
          secondsPerDay=86400, daysPerYear=360, daysPerYearDn=0, &
          daysPerYearDd=1, name="CustomCalendar", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized Custom Kind Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Kind Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate Custom Kind Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(name, *) "Calendar equality of custom Calendars Test"
      write(failMsg, *) "Returned not equal"
      customCalendar2 = ESMF_CalendarCreate(daysPerMonth=days_per_month, &
          secondsPerDay=86400, daysPerYear=360, daysPerYearDn=0, &
          daysPerYearDd=1, name="CustomCalendar2", rc=rc)
      call ESMF_Test((customCalendar.eq.customCalendar2 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(name, *) "Calendar inequality of custom Calendars Test"
      write(failMsg, *) "Returned not equal"
      call ESMF_Test((.not.(customCalendar.ne.customCalendar2) .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_CalendarDestroy(customCalendar2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calendar1,calendar2)
      write(name, *) "Calendar inequality of custom Calendars Test"
      write(failMsg, *) "Returned equal"
      customCalendar2 = ESMF_CalendarCreate(daysPerMonth=days_per_month, &
          secondsPerDay=20000, daysPerYear=360, daysPerYearDn=0, &
          daysPerYearDd=1, name="CustomCalendar2", rc=rc)
      call ESMF_Test((customCalendar.ne.customCalendar2 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(name, *) "Calendar equality of custom Calendars Test"
      write(failMsg, *) "Returned equal"
      call ESMF_Test((.not.(customCalendar.eq.customCalendar2) .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_CalendarDestroy(customCalendar2, rc=rc)
      call ESMF_CalendarDestroy(customCalendar, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type with mismatched daysPerYear Calendar Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate(daysPerMonth=days_per_month, &
                 secondsPerDay=86400, daysPerYear=100000000, daysPerYearDn=1, &
                 daysPerYearDd=1, name="CustomCalendar", rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine doesn't return ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Un-initialized Custom Kind Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Kind Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Validate Bad Custom Kind Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type with negative number seconds Calendar Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate(daysPerMonth=dayspermonth, &
          secondsPerDay=-400, daysPerYear=1, daysPerYearDn=1, &
          daysPerYearDd=1, name="CustomCalendar", rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine doesn't return ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Un-initialized Custom Kind Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Kind Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Validate Custom Kind Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize fourth calendar to be custom type
      write(name, *) "Initialize Custom Type with nonsense numbers Calendar Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate(daysPerMonth=dayspermonth, &
          secondsPerDay=86400, daysPerYear=1, daysPerYearDn=1, &
          daysPerYearDd=0, name="CustomCalendar", rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarDestroy(customCalendar, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine doesn't return ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Un-initialized Custom Kind Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Kind Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Validate Custom Kind Calendar Test"
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
      clock_gregorian = ESMF_ClockCreate(timeStep, startTime, &
                                         stopTime=stopTime, &
                                         name="Gregorian Clock", rc=rc)

      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! TODO: uncomment to test bug #959580
      !call ESMF_ClockDestroy(clock_gregorian, rc)

      totalDays=0
      ! time step from start time to stop time
      !call ESMF_TimePrint(startTime, rc=rc)
      !call ESMF_TimePrint(stopTime, rc=rc)
      do while (.not.ESMF_ClockIsStopTime(clock_gregorian, rc=rc) .and. &
                rc == ESMF_SUCCESS)
        call ESMF_ClockAdvance(clock_gregorian, timeStep=timeStep, rc=rc)
        totalDays=totalDays+days
      end do

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 73049 from -100 to +100 years in Gregorian Cal. Test"
      call ESMF_Test((totalDays.eq.73049), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock_gregorian, rc=rc)

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
      clock_no_leap = ESMF_ClockCreate(timeStep, startTime, &
                                       stopTime=stopTime, &    
                                       name="No Leap Clock", rc=rc) 
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      totalDays=0
      days=1
      call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      !call ESMF_TimePrint(startTime, rc=rc)
      !call ESMF_TimePrint(stopTime, rc=rc)
      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock_no_leap, rc=rc))
        call ESMF_ClockAdvance(clock_no_leap, timeStep=timeStep, rc=rc)
        totalDays=totalDays+days
      end do

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 73000 from -100 to +100 years in No Leap Cal. Test"
      call ESMF_Test((totalDays.eq.73000), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock_no_leap, rc=rc)

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
      clock_360day = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                      name="360 Day Clock", rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      totalDays=0
      days=1
      call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      !call ESMF_TimePrint(startTime, rc=rc)
      !call ESMF_TimePrint(stopTime, rc=rc)
      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock_360day, rc=rc))
        call ESMF_ClockAdvance(clock_360day, timeStep=timeStep, rc=rc)
        totalDays=totalDays+days
      end do

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !print *, " Total days = ", totalDays
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
      call ESMF_ClockDestroy(clock_360day, rc=rc)

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
      write(name, *) "Create Julian Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      julianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIAN, &
                                           name="Julian", rc=rc)
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
      !print *, "10/4/1582 Julian = ", days, " Julian days."

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
      !print *, "Julian Date = ", MM, "/", DD, "/", YY
      
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
      !print *, "10/4/1582 Julian = Proleptic Gregorian Date ", &
      !          MM, "/", DD, "/", YY
      
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
      !print *, "10/15/1582 Gregorian = Proleptic Julian Date ", &
      !          MM, "/", DD, "/", YY

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
      !print *, "3/1/-4712 Julian = ", days, " Julian days."

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
      !print *, "Julian Day 60 is Julian Date ", MM, "/", DD, "/", YY
      
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
      !print *, "Leap Year Test returned ", MM, "/", DD, "/", YY
      
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
      !print *, "6/15/-762 Julian = ", days, " Julian days."

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
      !print *, "4/15/-424 Julian = ", days, " Julian days."

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! initialize calendar to be Modified Julian Day type
      write(name, *) "Initialize Modified Julian Day Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      modifiedJulianDayCalendar = &
        ESMF_CalendarCreate(ESMF_CALKIND_MODJULIANDAY, &
        name="ModifiedJulianDay", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Converting Gregorian date to Modified Julian Day
      !   from http://tycho.usno.navy.mil/mjd.html
      write(failMsg, *) " Did not return days = 49987 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1995, mm=9, dd=27, & 
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, calendar=modifiedJulianDayCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=days, rc=rc)
      write(name, *) "Convert Gregorian to Modified Julian Day Test 1"
      call ESMF_Test((days.eq.49987).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "9/27/1995 Gregorian = ", days, " Modified Julian days."
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test day of the week from Modified Julian Day to Gregorian date
      !   from http://tycho.usno.navy.mil/mjd.html
      !  MJD 49987 = Wed., 27 Sept 1995
      write(failMsg, *) " Did not return dayOfWeek = 3 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, d=49987, &
                        calendar=modifiedJulianDayCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, dayOfWeek=dayOfWeek, rc=rc)
      write(name, *) "Convert Modified Julian Day to Gregorian day-of-week Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfWeek.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Converting Gregorian date to Modified Julian Day
      !   from http://tycho.usno.navy.mil/mjd.html
      write(failMsg, *) " Did not return days = 49352 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1993, mm=12, dd=31, & 
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, calendar=modifiedJulianDayCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d_i8=days_i8, rc=rc)
      write(name, *) "Convert Gregorian to Modified Julian Day Test 2 (i8)"
      call ESMF_Test((days_i8.eq.49352).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "12/31/1993 Gregorian = ", days_i8, " Modified Julian days."
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Converting Modified Julian Day to Gregorian date
      !   from http://tycho.usno.navy.mil/mjd.html
      write(failMsg, *) " Did not return 12/31/1992 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, d=48987, &
                        calendar=modifiedJulianDayCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, mm=MM, dd=DD, yy=YY, rc=rc)
      write(name, *) "Convert Modified Julian Day to Gregorian Test 1"
      call ESMF_Test((MM.eq.12).and.(DD.eq.31).and.(YY.eq.1992).and. &
                     (rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "48987 MJD = ", MM, "/", DD, "/", YY, " Gregorian"
      
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
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, yy=2100, rc=rc)
      write(name, *) "IsLeapYear test 2"
      call ESMF_Test(.not.bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Leap Year method I8 test 3
      year = 5000000000_ESMF_KIND_I8
      bool = ESMF_CalendarIsLeapYear(gregorianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 3"
      call ESMF_Test(bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Leap Year method I8 test 4
      year = 5000000100_ESMF_KIND_I8
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
      year = 5000000100_ESMF_KIND_I8
      bool = ESMF_CalendarIsLeapYear(julianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 7"
      call ESMF_Test(bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Leap Year method I8 test 8
      year = 5000000010_ESMF_KIND_I8
      bool = ESMF_CalendarIsLeapYear(julianCalendar, year, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "IsLeapYear test 8"
      call ESMF_Test(.not.bool .and. (rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Create custom calendar for Mars
      ! http://www.giss.nasa.gov/tools/mars24/help/notes.html
      ! http://www.giss.nasa.gov/research/briefs/allison_02/
      write(name, *) "Initialize Custom Kind Calendar for Mars Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      marsCalendar = ESMF_CalendarCreate(secondsPerDay=88775, &  ! 1 Sol
                               ! TODO: fractional secondsPerDay = 88775.244
                                         daysPerYear=668, & ! 668.5921 Sols/year
                                         daysPerYearDn=5921, &
                                         daysPerYearDd=10000, &
                                         name="MarsCalendar", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized Mars Custom Kind Calendar Print Test"
      call ESMF_CalendarPrint(marsCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Mars Custom Kind Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate Mars Custom Kind Calendar Test"
      call ESMF_CalendarValidate(marsCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(name, *) "Calendar equality of Mars vs. Earth Gregorian Calendars Test"
      write(failMsg, *) "Returned equal"
      call ESMF_Test((.not.(marsCalendar == gregorianCalendar) .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calkindflag,calendar)
      write(name, *) "Calendar inequality of Mars vs. Earth Julian Day Calendars Test"
      write(failMsg, *) "Returned equal"
      call ESMF_Test(((ESMF_CALKIND_JULIANDAY.ne.marsCalendar) .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting a Time for the Mars Calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time at arbitrary Mars solar year 0"
      call ESMF_TimeSet(startTime, yy=0, calendar=marsCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting a TimeInterval (sols) for the Mars Calendar
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=40, rc=rc)  ! 40 sols
      write(name, *) "Set Time Interval (sols) for Mars Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Mars Calendar Interval increment yy=0 by 40 sols Test"
      write(failMsg, *) " Did not return yy=0, s=3,551,000 or ESMF_SUCCESS"
      startTime = startTime + timeStep  ! exercise Time + operator
      call ESMF_TimeGet(startTime, yy=YY, s=S, rc=rc) ! S bounded by YY; seconds
                                                      !  within a year, i.e.
                                                      !  S modulo YY
      call ESMF_Test((YY==0 .and. S==3551000 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting a TimeInterval (solar year) for the Mars Calendar
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)  ! 1 Mars solar year
      write(name, *) "Set Time Interval (solar year) for Mars Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Mars Calendar Interval increment 1 solar year Test"
      write(failMsg, *) " Did not return yy=1, s=3,551,000 or ESMF_SUCCESS"
      startTime = startTime + timeStep  ! exercise Time + operator
      call ESMF_TimeGet(startTime, yy=YY, s=S, rc=rc) ! S bounded by YY; seconds
                                                      !  within a year, i.e.
                                                      ! S modulo YY
      call ESMF_Test((YY==1 .and. S==3551000 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeGet() seconds for Mars calendar
      write(name, *) "Mars Calendar Time Get seconds"
      write(failMsg, *) " Did not return s=62,905,263 or ESMF_SUCCESS"
      call ESMF_TimeGet(startTime, s=S, rc=rc) ! S unbounded by YY;total seconds
      call ESMF_Test((S==62905263 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeGet() sols (solar days) for Mars calendar
      write(name, *) "Mars Calendar Time Get sols (solar days) and seconds"
      write(failMsg, *) " Did not return sols=708, sols_r8=708.592092368347d0, S=52563 or ESMF_SUCCESS"
      call ESMF_TimeGet(startTime, d=sols, d_r8=sols_r8, s=S, rc=rc)
                                                         ! S bounded by sols;
                                                         !  seconds within a sol
      call ESMF_Test((sols==708.and.abs(sols_r8 - 708.592092368347d0) < 1d-12 &
                      .and. S==52563 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "sols = ", sols, "sols_r8 = ", sols_r8, "S = ", s

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeGet() sols, h,m,s for Mars calendar
      write(name, *) "Mars Calendar Time Get sols,h,m,s"
      write(failMsg, *) " Did not return sols=708, H,M,S = (14,36,3) or ESMF_SUCCESS"
      call ESMF_TimeGet(startTime, d=sols, h=H, m=M, s=S, rc=rc)
                        ! H bounded by sols; M bounded by H, S bounded by M
      call ESMF_Test((sols==708.and.H==14.and.M==36.and.S==3 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "sols = ", sols, "H,M,S = ", H, ",", M, ",", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeGet() unbounded h,m for Mars calendar
      write(name, *) "Mars Calendar Time Get unbounded h,m"
      write(failMsg, *) " Did not return H = 17473, M = 1048421 or ESMF_SUCCESS"
      call ESMF_TimeGet(startTime, h=H, rc=rc) ! total, unbounded H
      call ESMF_TimeGet(startTime, m=M, rc=rc) ! total, unbounded M
      call ESMF_Test((H==17473 .and. M==1048421 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "H,M = ", H, ",", M

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting a TimeInterval (solar year) for the Mars Calendar
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=1, d=40, rc=rc)
      ! 1 Mars solar year and 40 sols
      write(name, *) "Set Time Interval (1 solar year + 40 sols) for Mars Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Mars Calendar Interval decrement 1 solar year, 40 sols Test"
      write(failMsg, *) " Did not return yy=0, sols=0, s=0 or ESMF_SUCCESS"
      startTime = startTime - timeStep  ! exercise Time - operator
      call ESMF_TimeGet(startTime, yy=YY, rc=rc)
      call ESMF_TimeGet(startTime, d=sols, rc=rc) ! total sols unbounded by YY
      call ESMF_TimeGet(startTime, s=S, rc=rc) ! total S unbounded by YY or sols
      call ESMF_Test((YY==0 .and. S==0 .and. sols==0 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting a Time for the Mars Calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time at arbitrary Mars solar year 4 + 668.5921 sols"
      call ESMF_TimeSet(startTime, yy=4, d_r8=668.5921d0, &
                        calendar=marsCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime, rc=rc)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeGet() YY (solar years) for Mars calendar
      write(name, *) "Mars Calendar Time Get solar years"
      write(failMsg, *) " Did not return YY = 5, S = 0 or ESMF_SUCCESS"
      call ESMF_TimeGet(startTime, yy=YY, s=S, rc=rc)
      call ESMF_Test((YY==5 .and. S==0 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "YY = ", YY, "S =", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_CalendarSet() for Mars calendar
      write(name, *) "Mars CalendarSet/Get for Mars calendar"
      write(failMsg, *) " Did not return S = 1000 or ESMF_SUCCESS"
      call ESMF_CalendarSet(marsCalendar, secondsPerDay=1000, rc=rc)
      call ESMF_CalendarGet(marsCalendar, secondsPerDay=S, rc=rc)
      call ESMF_Test((S==1000 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Destroy Mars Calendar
      ! Test Mars Calendar Destroy
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Destroy Mars Custom Kind Calendar Test"
      call ESMF_CalendarDestroy(marsCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Create custom calendar: 12 months with different days each month
      write(name, *) "Initialize Custom Kind Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      customCalendar = ESMF_CalendarCreate(daysPerMonth=customDaysPerMonth, &
                                           secondsPerDay=86400, &
                                           name="CustomCalendar", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Initialized Custom Kind Calendar Print Test"
      call ESMF_CalendarPrint(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate Custom Kind Calendar
      ! Test that validate subroutine returns ESMF_SUCESS
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Validate Custom Kind Calendar Test"
      call ESMF_CalendarValidate(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(==)(calendar1,calendar2)
      write(name, *) "Calendar equality of Custom vs. Earth Gregorian Calendars Test"
      write(failMsg, *) "Returned equal"
      call ESMF_Test((.not.(customCalendar == gregorianCalendar) .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_CalendarOperator(/=)(calkindflag,calendar)
      write(name, *) "Calendar inequality of Custom vs. Earth Julian Day Calendars Test"
      write(failMsg, *) "Returned equal"
      call ESMF_Test(((ESMF_CALKIND_JULIANDAY.ne.customCalendar) .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting a Time for the Custom Calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time in Custom Calendar at 2/15/100"
      call ESMF_TimeSet(startTime, mm=2, dd=15, yy=100, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Getting a Time for the Custom Calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Get Time in Custom Calendar, should return 2/15/100"
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==2 .and. DD==15 .and. YY==100 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Custom Calendar Interval increment 5 days to end-of-month"
      write(failMsg, *) " Did not return 2/20/100 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=5, rc=rc)
      startTime = startTime + timeStep  ! exercise Time + operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==2 .and. DD==20 .and. YY==100 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Custom Calendar Interval increment 1 days to 1st-of-next-month"
      write(failMsg, *) " Did not return 3/1/100 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      startTime = startTime + timeStep  ! exercise Time + operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==3 .and. DD==1 .and. YY==100 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Custom Calendar Interval decrement 1 month to 1st-of-previous-month"
      write(failMsg, *) " Did not return 2/1/100 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime - timeStep  ! exercise Time - operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==2 .and. DD==1 .and. YY==100 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Custom Calendar Interval increment 2 months, 40 days to 1st-of-5th-month"
      write(failMsg, *) " Did not return 5/1/100 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=2, d=40, rc=rc)
      startTime = startTime + timeStep  ! exercise Time + operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==5 .and. DD==1 .and. YY==100 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Custom Calendar Interval decrement 1 year, 1 month, 31 days to end-of-2nd-month"
      write(failMsg, *) " Did not return 2/20/99 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=1, mm=1, d=31, rc=rc)
      startTime = startTime - timeStep  ! exercise Time - operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==2 .and. DD==20 .and. YY==99 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Custom Calendar Interval decrement 1 month, to end-of-1st-month"
      write(failMsg, *) " Did not return 1/10/99 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime - timeStep  ! exercise Time - operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==1 .and. DD==10 .and. YY==99 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Custom Calendar Interval increment 1 month" 
      write(failMsg, *) " Did not return 2/10/99 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep  ! exercise Time + operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==2 .and. DD==10 .and. YY==99 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Custom Calendar Interval increment 3 months, 40 days"
      write(failMsg, *) " Did not return 5/50/99 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=3, d=40, rc=rc)
      startTime = startTime + timeStep  ! exercise Time - operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==5 .and. DD==50 .and. YY==99 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Custom Calendar Interval increment 3 months"
      write(failMsg, *) " Did not return 8/45/99 or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep  ! exercise Time - operator
      call ESMF_TimeGet(startTime, mm=MM, dd=DD, yy=YY, & 
                        calendar=customCalendar, rc=rc)
      call ESMF_Test((MM==8 .and. DD==45 .and. YY==99 .and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "MM/DD/YY = ", MM, "/", DD, "/", YY

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_CalendarSet() for Custom calendar
      write(name, *) "Test CalendarSet() daysPerMonth array for Custom calendar"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(customCalendar, daysPerMonth=days_per_month, rc=rc)
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarPrint(customCalendar, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Destroy Custom Calendar
      ! Test Custom Calendar Destroy
      write(failMsg, *) " Should return ESMF_SUCCESS"
      write(name, *) "Destroy Custom Kind Calendar Test"
      call ESMF_CalendarDestroy(customCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      call ESMF_CalendarDestroy(julianCalendar, rc=rc)
      call ESMF_CalendarDestroy(esmf_360dayCalendar, rc=rc)
      call ESMF_CalendarDestroy(no_leapCalendar, rc=rc)
      call ESMF_CalendarDestroy(julianDayCalendar, rc=rc)
      call ESMF_CalendarDestroy(modifiedJulianDayCalendar, rc=rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)
#endif 

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_TestEnd(ESMF_SRCLINE)

      end program ESMF_CalendarUTest
