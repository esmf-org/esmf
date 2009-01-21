! $Id: ESMF_TimeUTest.F90,v 1.25.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_TimeUTest.F90,v 1.25.2.4 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, M, S, MS, US, NS, MM, DD, YY, D, dayOfYear, dayOfWeek
      integer :: sN, sD
      integer(ESMF_KIND_I8) :: year
      real(ESMF_KIND_R8) :: dayOfYear_r8
      logical :: bool

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! to retrieve time in string format
      character(ESMF_MAXSTR) :: timeString

      ! instantiate timestep, start and stop times
      type(ESMF_Time) :: startTime, stopTime, startTime2, midMonth
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             julianDayCalendar, &
                             noLeapCalendar, day360Calendar

      ! instantitate some general times and timeintervals
      type(ESMF_Time) :: time1, time2, time3, time4, time5
      type(ESMF_TimeInterval) :: timeInterval2, timeInterval3, timeInterval4, &
                                 timeInterval5

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

      ! ----------------------------------------------------------------------------
      ! Calendar Interval tests
      ! ----------------------------------------------------------------------------
      ! initialize calendars
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      julianCalendar = ESMF_CalendarCreate("Julian", &
                                              ESMF_CAL_JULIAN, rc)
      noLeapCalendar = ESMF_CalendarCreate("No Leap", &
                                              ESMF_CAL_NOLEAP, rc)
      day360Calendar = ESMF_CalendarCreate("360 Day", &
                                              ESMF_CAL_360DAY, rc)
      julianDayCalendar = ESMF_CalendarCreate("Julian Day", &
                                              ESMF_CAL_JULIANDAY, rc)


      
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

      print *, "startTime = ", timeString


#ifdef ESMF_TESTEXHAUSTIVE

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

      print *, "time1 = ", timeString

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

      print *, "time1 = ", timeString

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
                     (abs(dayOfYear_r8 - 29.5124768518518d0) < 1d-6), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "dayOfYear_r8 = ", dayOfyear_r8

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
      ! Testing the ==  operator 
      ! resultTime = ESMF_Time(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the ==  operator 
      ! resultTime = ESMF_TimeOperator(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /=  operator 
      ! resultTime = ESMF_TimeOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /=  operator 
      ! resultTime = ESMF_TimeIntervalOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 > startTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime2 <= startTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime > stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime < startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 < startTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime2 >= startTime    
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
      call ESMF_CalendarSetDefault(ESMF_CAL_NOLEAP)
      call ESMF_TimeSet(stopTime, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 5"
      call ESMF_Test((YY.eq.0.and.mm.eq.1.and.dd.eq.1.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarSetDefault(ESMF_CAL_NOCALENDAR)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! This test verifies the fix to bug #1306230, support #1305193, reported
      !   by Giang Nong/GFDL
      ! Test Setting Stop Time, relying on defaults 6
      write(failMsg, *) " Default JULIAN Did not return 1/1/0 and ESMF_SUCCESS"
      call ESMF_CalendarSetDefault(ESMF_CAL_JULIAN)
      call ESMF_TimeSet(stopTime, rc=rc)
      call ESMF_TimeGet(stopTime, yy=YY, mm=MM, dd=DD)
      write(name, *) "Set Time Initialization Test w/Defaults 6"
      call ESMF_Test((YY.eq.0.and.mm.eq.1.and.dd.eq.1.and. &
                      rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_CalendarSetDefault(ESMF_CAL_NOCALENDAR)

  ! ----------------------------------------------------------------------------
      
      !EX_UTest
      ! This test verifies the fix to support #1415439, reported
      !   by Tim Campbell/NRL
      ! Test Setting Time with No Calendar, just s, ms, ns
      write(failMsg, *) " Did not set/get s=1, ms=2, ns=3 with ESMF_CAL_NOCALENDAR, and return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, s=1, ms=2, ns=3, &
                        calendarType=ESMF_CAL_NOCALENDAR, rc=rc)
      call ESMF_TimeGet(stopTime, s=S, ms=MS, ns=NS)
      write(name, *) "Set Time Initialization Test w/ESMF_CAL_NOCALENDAR"
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
      write(failMsg, *) " Did return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, mm=0, dd=1, calendar=noLeapCalendar, rc=rc)
      write(name, *) "Set End Time with mm=0 Initialization Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time with dd=0
      write(failMsg, *) " Did return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, dd=0, s=58, calendar=noLeapCalendar, rc=rc)
      write(name, *) "Set End Time with dd=0 Initialization Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
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
      write(failMsg, *) " Did not return 9980 us and ESMF_SUCCESS"
      timeInterval3 = time1 - time2
      call ESMF_TimeIntervalGet(timeInterval3, us=US, rc=rc)
      call ESMF_TimeIntervalPrint(timeInterval3, rc=rc)
      write(name, *) "Difference between two fractional Times Test 2"
      call ESMF_Test((US.eq.9980.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test differencing two fractional times 2
      write(failMsg, *) " Did not return D=0, H=-23, M=-59, S=-59, -1/99 seconds and ESMF_SUCCESS"
      timeInterval5 = time5 - time4
      call ESMF_TimeIntervalGet(timeInterval5, d=D, h=H, m=M, s=S, &
                                sN=sN, sD=sD, rc=rc)
      call ESMF_TimeIntervalPrint(timeInterval5, rc=rc)
      write(name, *) "Difference between two fractional Times Test 2"
      call ESMF_Test((D.eq.0.and.H.eq.-23.and.M.eq.-59.and.S.eq.-59 &
                      .and.sN.eq.-1.and.sD.eq.99.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a fractional time and time interval
      write(failMsg, *) " Did not return 10/14/2004 15:52:59.040020 and ESMF_SUCCESS"
      time3 = time2 + timeInterval2
      call ESMF_TimeGet(time3, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, us=US, rc=rc)
      call ESMF_TimePrint(time3, rc=rc)
      write(name, *) "Adding fractional Time and Timeinterval Test"
      call ESMF_Test((YY.eq.2004.and.MM.eq.10.and.DD.eq.14.and.H.eq.15.and. &
                      M.eq.52.and.S.eq.59.and.US.eq.40020 &
                      .and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test subtracting a fractional time and time interval
      write(failMsg, *) " Did not return 10/22/2004 13:44:59 29/36 second and ESMF_SUCCESS"
      time5 = time4 - timeInterval4
      call ESMF_TimeGet(time5, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, sN=sN, &
                        sD=sD, rc=rc)
      call ESMF_TimePrint(time5, rc=rc)
      write(name, *) "Subtracting fractional Time and Timeinterval Test"
      call ESMF_Test((YY.eq.2004.and.MM.eq.10.and.DD.eq.22.and.H.eq.13.and. &
                      M.eq.44.and.S.eq.59.and.sN.eq.29.and.sD.eq.36 &
                      .and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (>) two fractional times 1
      write(failMsg, *) " Did not return (time1 > time2)"
      bool = time1 > time2
      write(name, *) "Fractional time1 > time2 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (>) two fractional times 2
      write(failMsg, *) " Did not return (time4 > time5)"
      bool = time4 > time5
      write(name, *) "Fractional time4 > time5 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (<) two fractional times 1
      write(failMsg, *) " Did not return (time2 < time3)"
      bool = time2 < time3
      write(name, *) "Fractional time2 < time3 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (<) two fractional times 2
      write(failMsg, *) " Did not return (time2 < time5)"
      bool = time2 < time5
      write(name, *) "Fractional time2 < time5 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (==) two fractional times
      write(failMsg, *) " Did not return (time3 == time3)"
      bool = time3 == time3
      write(name, *) "Fractional time3 == time3 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (/=) two fractional times
      write(failMsg, *) " Did not return (time1 /= time2)"
      bool = time1 /= time2
      write(name, *) "Fractional time1 /= time2 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (>=) two fractional times
      write(failMsg, *) " Did not return (time1 >= time2)"
      bool = time1 >= time2
      write(name, *) "Fractional time1 >= time2 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test comparing (<=) two fractional times
      write(failMsg, *) " Did not return (time1 <= time3)"
      bool = time1 <= time3
      write(name, *) "Fractional time1 <= time3 Test"
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

 
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
      year = 500000000  ! break up initialization,
      year = year * 10  !  since F90 constants
      year = year + 100 !    are 32-bit
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
      call ESMF_CalendarDestroy(day360Calendar)
      call ESMF_CalendarDestroy(noLeapCalendar)
      call ESMF_CalendarDestroy(julianCalendar)
      call ESMF_CalendarDestroy(gregorianCalendar)

  
      ! finalize ESMF framework
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_TimeUTest
