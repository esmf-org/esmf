! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_TimeUTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_TimeTest - Test Time initialization and manipulation.
!
! !DESCRIPTION:
!
! The code in this file drives F90 Time unit tests.
! The companion file ESMF\_Time.F90 contains the definitions for the
! Time methods.
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
      integer :: rc, H, M, S, MM, DD, YY

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! to retrieve time in string format
      character(ESMF_MAXSTR) :: timeString

      ! instantiate start time
      type(ESMF_Time) :: startTime
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             julianDayCalendar, modifiedJulianDayCalendar, &
                             noLeapCalendar, day360Calendar


#ifdef ESMF_TESTEXHAUSTIVE
      ! individual test result code
      logical :: bool
      integer :: dayOfYear, dayOfWeek, D, sD, sN, MS, NS, &
                 US
      real(ESMF_KIND_R8) :: NS_r8, S_r8, US_r8, MS_r8
      real(ESMF_KIND_R8) :: dayOfYear_r8, M_r8, D_r8, H_r8
      integer(ESMF_KIND_I8) :: year, SN_I8, SD_i8
 
      ! instantitate some general times and timeintervals
      type(ESMF_Time) :: time1, time2, time3, time4, time5, time6, time7, &
                         midMonth, startTime2
      type(ESMF_TimeInterval) :: timeInterval2, timeInterval3, timeInterval4, &
                                 timeInterval5, timeInterval6, timeInterval7

      ! instantiate timestep, start and stop times
      type(ESMF_Time) :: stopTime

      type(ESMF_CalKind_Flag) :: calkindflag
#endif

!-------------------------------------------------------------------------------
!    The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!    always run. When the environment variable, EXHAUSTIVE, is set to ON then
!    the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!    to OFF, then only the sanity unit tests.
!    Special strings (Non-exhaustive and exhaustive) have been
!    added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

     ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! ----------------------------------------------------------------------------
      ! Calendar Interval tests
      ! ----------------------------------------------------------------------------
      ! initialize calendars
      gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
        name="Gregorian", rc=rc)
      julianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIAN, &
        name="Julian", rc=rc)
      noLeapCalendar = ESMF_CalendarCreate(ESMF_CALKIND_NOLEAP, &
        name="No Leap", rc=rc)
      day360Calendar = ESMF_CalendarCreate(ESMF_CALKIND_360DAY, &
        name="360 Day", rc=rc)
      julianDayCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIANDAY, &
        name="Julian Day", rc=rc)
      modifiedJulianDayCalendar = &
        ESMF_CalendarCreate(ESMF_CALKIND_MODJULIANDAY, &
        name="ModifiedJulianDay", rc=rc)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting Start Time 
      write(name, *) "Set Time Initialization Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest

      write(name, *) "Get Time Test 1"
      write(failMsg, *) " Did not return 2004-01-29T12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, &
                        timeString=timeString, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==1 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      timeString=="2004-01-29T12:17:58" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !print *, "startTime = ", timeString


#ifdef ESMF_TESTEXHAUSTIVE

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Attempt to compare an initialized time to an un-initialized time.
      ! In support of bug #2826626 "Error in ESMC_Fraction.C w/ ESMF 3.1.0rp2",
      ! which in all likelihood was due to user-code using an uninitialized 
      ! time in a comparison.  If run against ESMF 3.1.0rp2, this test will
      ! produce the same log error message the user reported:
      !  "ESMC_Fraction.C 352 ESMC_FractionSimplify() Cannot divide by zero".
      ! In ESMF >= 4.0.0r, the Init macros catch this condition and report it
      ! in the log file accordingly:
      !  "Object Set or SetDefault method not called - Object not Initialized".
      ! However, since this test uses an overloaded operator, there is no
      ! return code to check.  Also, the == operation does not change its input 
      ! arguments, and always returns .false. in case of error.
      ! TODO:  Hence the only way to verify that the test is truly successful
      ! is to check that the appropriate error message was written to the log
      ! file at the appropriate place.  Currently, this can only be done via
      ! manual inspection, so we need a way to automate it, probably with a new
      ! ESMF_Test*() method.
      write(name, *) "Compare initialized and uninitialized Times Test 1"
      bool = (startTime == time2)
      write(failMsg, *) " Returned .true."
      call ESMF_Test((bool .eqv. .false.), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! TODO:  Automate check for real success; see "Compare ... Times Test 1"
      ! above.
      write(name, *) "Compare initialized and uninitialized Times Test 2"
      bool = (time1 .ne. time2)
      write(failMsg, *) " Returned .false."
      call ESMF_Test((bool .eqv. .true.), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! TODO:  Automate check for real success; see "Compare ... Times Test 1"
      ! above.
      write(name, *) "Compare initialized and uninitialized Times Test 3"
      bool = (time1 < startTime)
      write(failMsg, *) " Returned .true."
      call ESMF_Test((bool .eqv. .false.), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! TODO:  Automate check for real success; see "Compare ... Times Test 1"
      ! above.
      write(name, *) "Compare initialized and uninitialized Times Test 4"
      bool = (time1 .le. time2)
      write(failMsg, *) " Returned .true."
      call ESMF_Test((bool .eqv. .false.), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! TODO:  Automate check for real success; see "Compare ... Times Test 1"
      ! above.
      write(name, *) "Compare initialized and uninitialized Times Test 5"
      bool = (time1 > time2)
      write(failMsg, *) " Returned .true."
      call ESMF_Test((bool .eqv. .false.), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! TODO:  Automate check for real success; see "Compare ... Times Test 1"
      ! above.
      write(name, *) "Compare initialized and uninitialized Times Test 6"
      bool = (startTime .ge. time2)
      write(failMsg, *) " Returned .true."
      call ESMF_Test((bool .eqv. .false.), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Tests fix to support #1115836, bug #1118178, "ESMF_TimeGet returns
      !   string with %lld not %04lld" reported by Paul Schopf/GMU
      write(name, *) "Get String Time Test 2"
      write(failMsg, *) " Did not return 0009-02-07T00:00:00 or ESMF_SUCCESS"
      call ESMF_TimeSet(time1, yy=9, mm=2, dd=7, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(time1, yy=YY, mm=MM, dd=DD, &
                        timeString=timeString, rc=rc)
      call ESMF_Test((YY==9 .and. MM==2 .and. DD==7 .and. &
                      timeString=="0009-02-07T00:00:00" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !print *, "time1 = ", timeString

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Tests fix to support #1115836, bug #1118178, "ESMF_TimeGet returns
      !   string with %lld not %04lld" reported by Paul Schopf/GMU
      write(name, *) "Get String Time Test 3"
      write(failMsg, *) " Did not return 10000-02-07T00:00:00 or ESMF_SUCCESS"
      call ESMF_TimeSet(time1, yy=10000, mm=2, dd=7, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(time1, yy=YY, mm=MM, dd=DD, &
                        timeString=timeString, rc=rc)
      call ESMF_Test((YY==10000 .and. MM==2 .and. DD==7 .and. &
                      timeString=="10000-02-07T00:00:00" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !print *, "time1 = ", timeString

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting Stop Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set End Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Time have the same calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      bool =  ESMF_TimeIsSameCalendar(stopTime, stopTime, rc=rc)
      write(name, *) "Time Is Same CalendarTest"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Time day of the year
      write(failMsg, *) " Did not return dayOfYear = 29 and ESMF_SUCCESS"
      call ESMF_TimeGet(stopTime, dayOfYear=dayOfYear, rc=rc)
      write(name, *) "Time Get day of the year test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfYear.eq.29), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Time floating point day of the year
      write(failMsg, *) " Did not return dayOfYear = 29.5124768518518 and ESMF_SUCCESS"
      call ESMF_TimeGet(stopTime, dayOfYear_r8=dayOfYear_r8, rc=rc)
      write(name, *) "Time Get floating point day of the year test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and. &
                     (abs(dayOfYear_r8 - 29.5124768518518d0) < 1d-13), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "dayOfYear_r8 = ", dayOfyear_r8

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test day of the week
      write(failMsg, *) " Did not return dayOfWeek = 4 and ESMF_SUCCESS"
      call ESMF_TimeGet(stopTime, dayOfWeek=dayOfWeek, rc=rc)
      write(name, *) "Time Get day of the week test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfWeek.eq.4), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test middle of the month
      write(failMsg, *) " Did not return midMonth = 1/16/2004 12:00:00 and ESMF_SUCCESS"
      call ESMF_TimeGet(stopTime, midMonth=midMonth, rc=rc)
      call ESMF_TimeGet(midMonth, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      write(name, *) "Time Get middle of the month test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2004).and.(MM.eq.1) &
                    .and.(DD.eq.16).and.(H.eq.12).and.(M.eq.0).and.(S.eq.0), &
                    name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting Time with Julian calendar
      ! Use a leap year in the Julian, but not Gregorian calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(time1, yy=2100, mm=2, dd=29, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      write(name, *) "Set Julian Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Time have the same Julian calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      bool =  ESMF_TimeIsSameCalendar(time1, time1, rc=rc)
      write(name, *) "Time Is Same Julian CalendarTest"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Julian Time day of the year
      write(failMsg, *) " Did not return dayOfYear = 60 and ESMF_SUCCESS"
      call ESMF_TimeGet(time1, dayOfYear=dayOfYear, rc=rc)
      write(name, *) "Julian Time Get day of the year test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfYear.eq.60), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test day of the week
      write(failMsg, *) " Did not return dayOfWeek = 7 and ESMF_SUCCESS"
      call ESMF_TimeGet(time1, dayOfWeek=dayOfWeek, rc=rc)
      write(name, *) "Time Get day of the week test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(dayOfWeek.eq.7), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test middle of the month
      write(failMsg, *) " Did not return midMonth = 2/15/2100 12:00:00 and ESMF_SUCCESS"
      call ESMF_TimeGet(time1, midMonth=midMonth, rc=rc)
      call ESMF_TimeGet(midMonth, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      write(name, *) "Julian Time Get middle of the month test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2100).and.(MM.eq.2) &
                    .and.(DD.eq.15).and.(H.eq.12).and.(M.eq.0).and.(S.eq.0), &
                    name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Start Time 2
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(startTime2, yy=2004, mm=1, dd=29, h=12, m=17, s=59, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing ESMF_TimeAssignment(=)(time, time)
      write(name, *) "Assign one time to another test"
      write(failMsg, *) " Did not return time1=startTime2, 1/29/2004 12:17:59 or ESMF_SUCCESS"
      time1 = startTime2  ! exercise default F90 Time = assignment
      call ESMF_TimeGet(time1, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, &
                        calkindflag=calkindflag, rc=rc)
      call ESMF_Test((time1==startTime2 .and. YY==2004 .and. MM==1 .and. &
                      DD==29 .and. H==12 .and. M==17 .and. S==59 .and. &
                      calkindflag==ESMF_CALKIND_GREGORIAN .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !print *, " yy/mm/d h:m:s = ", YY, "/", MM, "/", DD, " ", H, ":", M, ":", S
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == stopTime  ! exercise Time == operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == startTime2  ! exercise Time == operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= stopTime  ! exercise Time /= operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= startTime2  ! exercise Time /= operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > stopTime  ! exercise Time > operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > startTime2  ! exercise Time > operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 > startTime  ! exercise Time > operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= stopTime  ! exercise Time <= operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= startTime2  ! exercise Time <= operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime2 <= startTime  ! exercise Time <= operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > stopTime  ! exercise Time > operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime < startTime2  ! exercise Time < operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime2 < startTime  ! exercise Time < operator    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= stopTime  ! exercise Time >= operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= startTime2  ! exercise Time >= operator
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime2 >= startTime  ! exercise Time >= operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=julianDayCalendar, rc=rc)
      write(name, *) "Set End Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! The following 5 tests are from Shep Smithline/Giang/GFDL on 
      !   support #1087160

      !EX_UTest
      ! Test Setting Stop Time, relying on defaults 1
      write(failMsg, *) " Did not return 2/3/2004 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=2004, mm=2, dd=3, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 1"
      call ESMF_Test((YY.eq.2004.and.mm.eq.2.and.dd.eq.3.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time, relying on defaults 2
      write(failMsg, *) " Did not return 1/4/3 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=3, dd=4, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 2"
      call ESMF_Test((YY.eq.3.and.mm.eq.1.and.dd.eq.4.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time, relying on defaults 3
      write(failMsg, *) " Did not return 2/3/0 ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, mm=2, dd=5, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 3"
      call ESMF_Test((YY.eq.0.and.mm.eq.2.and.dd.eq.5.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time, relying on defaults 4
      write(failMsg, *) " Did not return 1/6/0 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, dd=6, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 4"
      call ESMF_Test((YY.eq.0.and.mm.eq.1.and.dd.eq.6.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time, relying on defaults 5
      write(failMsg, *) " Default NOLEAP Did not return 1/1/0 and ESMF_SUCCESS"
      call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP)
      call ESMF_TimeSet(stopTime, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 5"
      call ESMF_Test((YY.eq.0.and.mm.eq.1.and.dd.eq.1.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_NOCALENDAR)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! This test verifies the fix to bug #1306230, support #1305193, reported
      !   by Giang Nong/GFDL
      ! Test Setting Stop Time, relying on defaults 6
      write(failMsg, *) " Default JULIAN Did not return 1/1/0 and ESMF_SUCCESS"
      call ESMF_CalendarSetDefault(ESMF_CALKIND_JULIAN)
      call ESMF_TimeSet(stopTime, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 6"
      call ESMF_Test((YY.eq.0.and.mm.eq.1.and.dd.eq.1.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_NOCALENDAR)

      ! ----------------------------------------------------------------------------
      
      !EX_UTest
      ! This test verifies the fix to support #1415439, reported
      !   by Tim Campbell/NRL
      ! Test Setting Time with No Calendar, just s, ms, ns
      write(failMsg, *) " Did not set/get s=1, ms=2, ns=3 with ESMF_CALKIND_NOCALENDAR, and return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, s=1, ms=2, ns=3, &
                        calkindflag=ESMF_CALKIND_NOCALENDAR, rc=rc)
      call ESMF_TimeGet(stopTime, s=S, ms=MS, ns=NS)
      write(name, *) "Set Time Initialization Test w/ESMF_CALKIND_NOCALENDAR"
      call ESMF_Test(S.eq.1.and.MS.eq.2.and.NS.eq.3.and. &
                     rc.eq.ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Time have the same calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      bool =  ESMF_TimeIsSameCalendar(stopTime, stopTime, rc=rc)
      write(name, *) "Time Is Same CalendarTest"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time with mm=0
      write(failMsg, *) " Did not return ESMC_RC_ARG_OUTOFRANGE"
      call ESMF_TimeSet(stopTime, mm=0, dd=1, calendar=noLeapCalendar, rc=rc)
      write(name, *) "Set End Time with mm=0 Initialization Test"
      call ESMF_Test((rc.eq.ESMC_RC_ARG_OUTOFRANGE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time with dd=0
      write(failMsg, *) " Did return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, dd=0, s=58, calendar=noLeapCalendar, rc=rc)
      write(name, *) "Set End Time with dd=0 Initialization Test"
      call ESMF_Test((rc.eq.ESMC_RC_ARG_OUTOFRANGE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test driving No-Leap calendar with seconds only
      !   From bug #1050260
      write(failMsg, *) " Did not return S=0, D=8, and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, s=691200, calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, s=S, d=D, rc=rc)
      write(name, *) "Test driving No-Leap calendar with seconds only"
      call ESMF_Test((S.eq.0.and.D.eq.8.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test driving 360-Day calendar with seconds only
      !   From bug #1050260
      write(failMsg, *) " Did not return S=0, D=8, and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, s=691200, calendar=day360Calendar, rc=rc)
      call ESMF_TimeGet(stopTime, s=S, d=D, rc=rc)
      write(name, *) "Test driving 360-Day calendar with seconds only"
      call ESMF_Test((S.eq.0.and.D.eq.8.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Fractional times tests
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting a Millisecond Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(time1, yy=2004, mm=10, dd=14, h=15, m=52, s=58, &
                        ms=10, calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Millisecond Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting a Microsecond Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(time2, yy=2004, mm=10, dd=14, h=15, m=52, s=58, &
                        us=20, calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Microsecond Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting a Rational Fraction Time 1
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(time4, yy=2004, mm=10, dd=22, h=13, m=45, &
                        sN=5, sD=9, calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Rational Fraction Time Initialization Test 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting a Rational Fraction Time 2
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(time5, yy=2004, mm=10, dd=21, h=13, m=45, &
                        sN=17, sD=11, calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Rational Fraction Time Initialization Test 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting a Millisecond Time Interval
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval2, s=1, ms=40, rc=rc)
      write(name, *) "Set Millisecond Time Interval Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting a Rational Fraction Time Interval
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval4, sN=3, sD=4, rc=rc)
      write(name, *) "Set Rational Fraction Time Interval Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test differencing two fractional times 1
      ! Testing ESMF_TimeOperator(-)(time, time)
      write(failMsg, *) " Did not return 9980 us and ESMF_SUCCESS"
      timeInterval3 = time1 - time2  ! exercise Time - operator
      call ESMF_TimeIntervalGet(timeInterval3, us=US, rc=rc)
      !call ESMF_TimeIntervalPrint(timeInterval3, rc=rc)
      write(name, *) "Difference between two fractional Times Test 2"
      call ESMF_Test((US.eq.9980.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test differencing two fractional times 2
      ! Testing ESMF_TimeOperator(-)(time, time)
      write(failMsg, *) " Did not return D=0, H=-23, M=-59, S=-59, -1/99 seconds and ESMF_SUCCESS"
      timeInterval5 = time5 - time4  ! exercise Time - operator
      call ESMF_TimeIntervalGet(timeInterval5, d=D, h=H, m=M, s=S, &
                                sN=sN, sD=sD, rc=rc)
      !call ESMF_TimeIntervalPrint(timeInterval5, rc=rc)
      write(name, *) "Difference between two fractional Times Test 2"
      call ESMF_Test((D.eq.0.and.H.eq.-23.and.M.eq.-59.and.S.eq.-59 &
                      .and.sN.eq.-1.and.sD.eq.99.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a fractional time and time interval
      ! Testing ESMF_TimeOperator(+)(time, timeinterval)
      write(failMsg, *) " Did not return 10/14/2004 15:52:59.040020 and ESMF_SUCCESS"
      time3 = time2 + timeInterval2  ! exercise Time + operator
      call ESMF_TimeGet(time3, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, us=US, rc=rc)
      !call ESMF_TimePrint(time3, rc=rc)
      write(name, *) "Adding fractional Time and Timeinterval Test"
      call ESMF_Test((YY.eq.2004.and.MM.eq.10.and.DD.eq.14.and.H.eq.15.and. &
                      M.eq.52.and.S.eq.59.and.US.eq.40020 &
                      .and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test subtracting a fractional time and time interval
      ! Testing ESMF_TimeOperator(-)(time, timeinterval)
      write(failMsg, *) " Did not return 10/22/2004 13:44:59 29/36 second and ESMF_SUCCESS"
      time5 = time4 - timeInterval4  ! exercise Time - operator
      call ESMF_TimeGet(time5, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, sN=sN, &
                        sD=sD, rc=rc)
      !call ESMF_TimePrint(time5, rc=rc)
      write(name, *) "Subtracting fractional Time and Timeinterval Test"
      call ESMF_Test((YY.eq.2004.and.MM.eq.10.and.DD.eq.22.and.H.eq.13.and. &
                      M.eq.44.and.S.eq.59.and.sN.eq.29.and.sD.eq.36 &
                      .and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (>) two fractional times 1
      ! Testing ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) " Did not return (time1 > time2)"
      bool = time1 > time2  ! exercise Time > operator
      write(name, *) "Fractional time1 > time2 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (>) two fractional times 2
      ! Testing ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) " Did not return (time4 > time5)"
      bool = time4 > time5  ! exercise Time > operator
      write(name, *) "Fractional time4 > time5 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (<) two fractional times 1
      ! Testing ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) " Did not return (time2 < time3)"
      bool = time2 < time3  ! exercise Time < operator
      write(name, *) "Fractional time2 < time3 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (<) two fractional times 2
      ! Testing ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) " Did not return (time2 < time5)"
      bool = time2 < time5  ! exercise Time < operator
      write(name, *) "Fractional time2 < time5 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (==) two fractional times
      ! Testing ESMF_TimeOperator(==)(time, time)
      write(failMsg, *) " Did not return (time3 == time3)"
      bool = time3 == time3  ! exercise Time == operator
      write(name, *) "Fractional time3 == time3 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (/=) two fractional times
      ! Testing ESMF_TimeOperator(/=)(time, time)
      write(failMsg, *) " Did not return (time1 /= time2)"
      bool = time1 /= time2  ! exercise Time /= operator
      write(name, *) "Fractional time1 /= time2 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (>=) two fractional times
      ! Testing ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) " Did not return (time1 >= time2)"
      bool = time1 >= time2  ! exercise Time >= operator
      write(name, *) "Fractional time1 >= time2 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (<=) two fractional times
      ! Testing ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) " Did not return (time1 <= time3)"
      bool = time1 <= time3  ! exercise Time <= operator
      write(name, *) "Fractional time1 <= time3 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Real fractional times tests
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting/Getting with s_r8 on Julian Day calendar
      write(failMsg, *) " Did not return Set values and ESMF_SUCCESS"
      call ESMF_TimeSet(time1, s_r8=-37.25d0, &
                        calendar=julianDayCalendar, rc=rc)
      !call ESMF_TimePrint(time1, rc=rc)
      call ESMF_TimeGet(time1, s_r8=S_r8, s=S, sN=SN, sD=SD, rc=rc)
      write(name, *) "Set/Get Real (s_r8) Time on Julian Day calendar Test"
      call ESMF_Test((abs(S_r8 + 37.25d0) < 1d-17) .and. &
                      S .eq.-37 .and. &
                      SN.eq.-1.and. &
                      SD.eq.4.and. &
                      rc.eq.ESMF_SUCCESS, &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, S_r8, S, SN, SD

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting/Getting with m_r8 on Julian Day calendar
      ! Test to verify fix for Brian Eaton/CCSM in #2992547
      write(failMsg, *) " Did not return Set values and ESMF_SUCCESS"
      call ESMF_TimeSet(time1, m_r8=-59.0d0, &
                        calendar=julianDayCalendar, rc=rc)
      !call ESMF_TimePrint(time1, rc=rc)
      call ESMF_TimeGet(time1, m_r8=M_r8, m=M, sN=SN, sD=SD, rc=rc)
      write(name, *) "Set/Get Real (m_r8) Time on Julian Day calendar Test"
      call ESMF_Test((abs(M_r8 + 59.0d0) < 1d-17) .and. &
                      M .eq.-59 .and. &
                      SN.eq.0.and. &
                      SD.eq.1.and. &
                      rc.eq.ESMF_SUCCESS, &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, M_r8, M, SN, SD

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting/Getting with _r8's on Julian Day calendar
      write(failMsg, *) " Did not return Set values and ESMF_SUCCESS"
      call ESMF_TimeSet(time1, d_r8=9.1d0, &
                        h_r8=17.4d0, m_r8=47.75d0, &
                        s_r8=52.25d0, &
                        ms_r8=22.5d0, us_r8=159.0d0, ns_r8=592.125d0, &
                        sN_i8=1234567890_ESMF_KIND_I8, &
                        sD_i8=3000000000_ESMF_KIND_I8, & 
                        calendar=julianDayCalendar, rc=rc)
      !call ESMF_TimePrint(time1, rc=rc)
      call ESMF_TimeGet(time1, d_r8=D_r8, h_r8=H_r8, m_r8=M_r8, s_r8=S_r8, &
                        ms_r8=MS_r8, us_r8=US_r8, ns_r8=NS_r8, &
                        sN_i8=SN_i8, sD_i8=SD_i8, rc=rc)
      write(name, *) "Set/Get Real (R8) Time on Julian Day calendar Test"
      call ESMF_Test((abs(D_r8 -  9.85876949284979d0) < 1d-14) .and. &
                     (abs(H_r8 -  20.6104678283950d0) < 1d-13) .and. &
                     (abs(M_r8 -  1236.62806970370d0) < 1d-11) .and. &
                     (abs(S_r8 -  74197.6841822221d0) < 1d-10) .and. &
                     (abs(MS_r8 - 74197684.1822221d0) < 1d-7)  .and. &
                     (abs(US_r8 - 74197684182.2221d0) < 1d-4)  .and. &
                     (abs(NS_r8 - 74197684182222.1d0) < 1d-1)  .and. &
                      SN_i8.eq.5473457777_ESMF_KIND_I8 .and. &
                      SD_i8.eq.8000000000_ESMF_KIND_I8 .and. &
                      rc.eq.ESMF_SUCCESS, &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, D_r8, H_r8, M_r8, S_r8, MS_r8, US_r8, NS_r8, SN_i8, SD_i8
      !print *, "D_r8 - 9.1 = ", D_r8 - 9.1d0

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting/Getting with _r8's on Modified Julian Day calendar
      write(failMsg, *) " Did not return Set values and ESMF_SUCCESS"
      call ESMF_TimeSet(time1, d_r8=9.1d0, &
                        h_r8=17.4d0, m_r8=47.75d0, &
                        s_r8=52.25d0, &
                        ms_r8=22.5d0, us_r8=159.0d0, ns_r8=592.125d0, &
                        sN_i8=1234567890_ESMF_KIND_I8, &
                        sD_i8=3000000000_ESMF_KIND_I8, & 
                        calendar=modifiedJulianDayCalendar, rc=rc)
      !call ESMF_TimePrint(time1, rc=rc)
      call ESMF_TimeGet(time1, d_r8=D_r8, h_r8=H_r8, m_r8=M_r8, s_r8=S_r8, &
                        ms_r8=MS_r8, us_r8=US_r8, ns_r8=NS_r8, &
                        sN_i8=SN_i8, sD_i8=SD_i8, rc=rc)
      write(name, *) "Set/Get Real (R8) Time on Modified Julian Day calendar Test"
      call ESMF_Test((abs(D_r8 -  9.85876949284979d0) < 1d-14) .and. &
                     (abs(H_r8 -  20.6104678283950d0) < 1d-13) .and. &
                     (abs(M_r8 -  1236.62806970370d0) < 1d-11) .and. &
                     (abs(S_r8 -  74197.6841822221d0) < 1d-10) .and. &
                     (abs(MS_r8 - 74197684.1822221d0) < 1d-7)  .and. &
                     (abs(US_r8 - 74197684182.2221d0) < 1d-4)  .and. &
                     (abs(NS_r8 - 74197684182222.1d0) < 1d-1)  .and. &
                      SN_i8.eq.5473457777_ESMF_KIND_I8 .and. &
                      SD_i8.eq.8000000000_ESMF_KIND_I8 .and. &
                      rc.eq.ESMF_SUCCESS, &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, D_r8, H_r8, M_r8, S_r8, MS_r8, US_r8, NS_r8, SN_i8, SD_i8
      !print *, "D_r8 - 9.1 = ", D_r8 - 9.1d0

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test adding a time and real fractional days time interval
      ! Testing ESMF_TimeOperator(+)(time, timeinterval)
      write(failMsg, *) " Did not return 11/27/2008 03:45:40 and ESMF_SUCCESS"
      call ESMF_TimeSet(time6, yy=2008, mm=11, dd=25, h=17, m=15, s=40, &
                       calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval6, d_r8=1.4375d0, rc=rc)
      time7 = time6 + timeInterval6  ! exercise Time + operator
      call ESMF_TimeGet(time7, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      !call ESMF_TimePrint(time7, rc=rc)
      write(name, *) "Adding Time and real fractional days Timeinterval Test"
      call ESMF_Test((YY.eq.2008.and.MM.eq.11.and.DD.eq.27.and.H.eq.3.and. &
                      M.eq.45.and.S.eq.40 &
                      .and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, YY, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test taking the difference of two times to produce a real fractional
      ! days result.
      ! Testing ESMF_TimeOperator(-)(time, time)
      write(failMsg, *) " Did not return D_r8=1.4375 and ESMF_SUCCESS"
      timeInterval7 = time7 - time6  ! exercise Time - operator
      call ESMF_TimeIntervalGet(timeInterval7, d_r8=D_r8, rc=rc)
      !call ESMF_TimeIntervalPrint(timeInterval7, rc=rc)
      write(name, *) "Test subtracting two times, resulting in a real fractional days Timeinterval"
      call ESMF_Test((D_r8.eq.1.4375d0.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, D_r8

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test subracting a real fractional time and real fractional days time interval
      ! Testing ESMF_TimeOperator(-)(time, timeinterval)
      write(failMsg, *) " Did not return 11/27/2008 03:45:40 and ESMF_SUCCESS"
      call ESMF_TimeSet(time6, yy=2008, mm=12, dd=1, h=2, m=3, s=4, &
                       ms_r8=0.125d0, calendar=gregorianCalendar, rc=rc)
      !call ESMF_TimePrint(time6, rc=rc)
      ! interval of 1+5/7 days, to 16 digits of precison
      call ESMF_TimeIntervalSet(timeInterval6, d_r8=1.714285714285714d0, rc=rc)
      !call ESMF_TimeIntervalPrint(timeInterval6, rc=rc)
      time7 = time6 - timeInterval6  ! exercise Time - operator
      !call ESMF_TimePrint(time7, rc=rc)
      call ESMF_TimeGet(time7, yy=YY, mm=MM, dd=DD, s_r8=S_r8, rc=rc)
      write(name, *) "Subtracting real fractional Time and real fractional days Timeinterval Test"
      call ESMF_Test((YY.eq.2008.and.MM.eq.11.and.DD.eq.29.and. &
                      abs(S_r8-32069.7144107143d0) < 1d-10 &
                      .and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, YY, "/", MM, "/", DD, " ", S_r8

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test taking the difference of two times to produce time interval
      ! equal to previous subtraction
      ! Testing ESMF_TimeOperator(-)(time, time)
      write(failMsg, *) " Did not return equivalent timeInterval and ESMF_SUCCESS"
      timeInterval7 = time6 - time7  ! exercise Time - operator
      !call ESMF_TimeIntervalPrint(timeInterval7, rc=rc)
      ! calendars must match for equality test, so reset timeInterval6 with cal
      call ESMF_TimeIntervalSet(timeInterval6, d_r8=1.714285714285714d0, &
                                calendar=gregorianCalendar, rc=rc)
      !call ESMF_TimeIntervalPrint(timeInterval6, rc=rc)
      write(name, *) "Test subtracting two times, producing equal Timeinterval used in previous test"
      call ESMF_Test((timeInterval7.eq.timeInterval6.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Leap Year tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year Test 1
      call ESMF_TimeSet(time1, yy=2000, mm=2, dd=29, &
                                   calendar=gregorianCalendar, rc=rc)
      bool =  ESMF_TimeIsLeapYear(time1, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "Time Is Leap Year Test 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year Test 2
      year = 5000000100_ESMF_KIND_I8
      call ESMF_TimeSet(time1, yy_i8=year, mm=2, dd=28, &
                                   calendar=gregorianCalendar, rc=rc)
      bool =  ESMF_TimeIsLeapYear(time1, rc=rc)
      write(failMsg, *) " Did not return false and ESMF_SUCCESS"
      write(name, *) "Time Is Leap Year Test 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year Test 3
      call ESMF_TimeSet(time1, yy=2100, mm=2, dd=29, &
                                   calendar=julianCalendar, rc=rc)
      bool =  ESMF_TimeIsLeapYear(time1, rc=rc)
      write(failMsg, *) " Did not return true and ESMF_SUCCESS"
      write(name, *) "Time Is Leap Year Test 3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Leap Year Test 4
      year = year - 90  !   reuse the year variable set above
      call ESMF_TimeSet(time1, yy_i8=year, mm=2, dd=28, &
                                   calendar=julianCalendar, rc=rc)
      bool =  ESMF_TimeIsLeapYear(time1, rc=rc)
      write(failMsg, *) " Did not return false and ESMF_SUCCESS"
      write(name, *) "Time Is Leap Year Test 4"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

#endif

      ! free calendar memory
      call ESMF_CalendarDestroy(julianDayCalendar)
      call ESMF_CalendarDestroy(modifiedJulianDayCalendar)
      call ESMF_CalendarDestroy(day360Calendar)
      call ESMF_CalendarDestroy(noLeapCalendar)
      call ESMF_CalendarDestroy(julianCalendar)
      call ESMF_CalendarDestroy(gregorianCalendar)

  
      ! finalize ESMF framework
      call ESMF_TestEnd(ESMF_SRCLINE)

      end program ESMF_TimeUTest
