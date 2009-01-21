! $Id: ESMF_ClockUTest.F90,v 1.103.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
      program ESMF_ClockTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_ClockTest - Test Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! The code in this file drives F90 Clock unit tests.
! The companion file ESMF\_Clock.F90 contains the definitions for the
! Clock methods.
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
      '$Id: ESMF_ClockUTest.F90,v 1.103.2.4 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, M, MM, DD, YY, days, totalDays, secs, testResults
      integer :: checkSec
      integer :: hr, min, sec, ms, julday, julyr

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool, clocksEqual, clocksNotEqual, testPass, clockStopped
      logical :: stopTimeEnabled, runTheClock, stepOnePass, stepTwoPass

      type(ESMF_Direction) :: direction

      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1, clock2, clock_gregorian, &
                          clock_no_leap, clock_360day, topClock

      ! Random number
      real :: ranNum
      integer :: seed(32)
      integer :: timevals(8)

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             no_leapCalendar, esmf_360dayCalendar
      type(ESMF_CalendarType) :: cal_type

      ! to retrieve time in string format
      character(ESMF_MAXSTR) :: timeString

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, stopTime, startTime2, stopTime2, stopTime3
      type(ESMF_Time) :: currentTime, current_time, &
                         previousTime, syncTime, stopTime4
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      integer(ESMF_KIND_I8) :: advanceCounts, year, day2, minute, second
      integer(ESMF_KIND_I4) :: day, hour
      integer :: datetime(8)
      integer :: timeStepCount

      ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! set default calendar to Gregorian
      call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, rc)

      ! initialize one calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)

      ! initialize secand calendar to be Julian type
      julianCalendar = ESMF_CalendarCreate("Julian", ESMF_CAL_JULIANDAY, rc)

      ! initialize third calendar to be No Leap type
      no_leapCalendar = ESMF_CalendarCreate("NoLeap", ESMF_CAL_NOLEAP, rc)

      ! initialize third calendar to be 360 day type
      esmf_360dayCalendar = ESMF_CalendarCreate("360Day", ESMF_CAL_360DAY, rc)

!-------------------------------------------------------------------------------
!    The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!    always run. When the environment variable, EXHAUSTIVE, is set to ON then
!    the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!    to OFF, then only the sanity unit tests.
!    Special strings (Non-exhaustive and exhaustive) have been
!    added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------


      ! initialize clock time intervals and instants

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting Time Step
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=13, &
                             	   h=18, m=45, s=27, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Destroy Test"
      call ESMF_ClockDestroy(clock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


#ifdef ESMF_TESTEXHAUSTIVE 
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroyed Clock Destroy Test"
      call ESMF_ClockDestroy(clock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroyed Clock Destroy Test"
      call ESMF_ClockDestroy(clock1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Set destroyed Clock Name Test"
      call ESMF_ClockSet(clock, name="Clock 2", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Set uncreated  Clock Name Test"
      call ESMF_ClockSet(clock1, name="Clock 2", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

       !EX_UTest
       write(name, *) "Get previous time from deleted Clock Test"
       write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
       call ESMF_ClockGet(clock, prevTime=previousTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

       !EX_UTest
       write(name, *) "Get previous time from uncreated Clock Test"
       write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
       call ESMF_ClockGet(clock1, prevTime=previousTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

       !EX_UTest
       write(name, *) "Advance deleted CLock Test"
       write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
       call ESMF_ClockAdvance(clock, rc=rc)
       call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

       !EX_UTest
       write(name, *) "Advance uncreated CLock Test"
       write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
       call ESMF_ClockAdvance(clock1, rc=rc)
       call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "ClockIsStopTime on destroyed Clock Test"
      bool = ESMF_ClockIsStopTime(clock, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "ClockIsStopTime on uncreated Clock Test"
      bool = ESMF_ClockIsStopTime(clock1, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the year is set correctly
      write(name, *) "Get Start Time Year Test"
      call ESMF_TimeGet(startTime, yy=YY, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StartTime Month Test"
      call ESMF_TimeGet(startTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the day is set correctly
      write(name, *) "Get StartTime Day Test"
      call ESMF_TimeGet(startTime, dd=DD, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! hours, minutes, seconds within the day tests
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the hour is set correctly within the day
      write(name, *) "Get StartTime Hours Within the Day Test"
      call ESMF_TimeGet(startTime, dd=DD, h=H, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or day or hour not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13).and.(H.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "hours = ", H

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the minutes is set correctly within the day
      write(name, *) "Get StartTime Minutes Within the Day Test"
      call ESMF_TimeGet(startTime, dd=DD, m=M, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or day or minutes not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13).and.(M.eq.1125), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = 18 X 60 + 45 = 1125
      print *, "minutes = ", M

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the seconds are set correctly within the day
      write(name, *) "Get StartTime seconds Within the Day Test"
      call ESMF_TimeGet(startTime, dd=DD, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or day or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13).and.(secs.eq.67527), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! seconds = 1125 X 60 + 27 = 67527

      print *, "seconds = ", secs
      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the hours, minutes and seconds are set correctly within the day
      write(name, *) "Get StartTime in hours, minutes & seconds Within the Day Test"
      call ESMF_TimeGet(startTime, dd=DD, h=H, m=M, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or day, hours, minutes or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13).and.(secs.eq.27) &
                      .and.(H.eq.18).and.(M.eq.45), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the hours and seconds are set correctly within the day
      write(name, *) "Get StartTime in hours, & seconds Within the Day Test"
      call ESMF_TimeGet(startTime, dd=DD, h=H, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or day, hour or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13).and.(secs.eq.2727) &
                      .and.(H.eq.18), name, failMsg, result, ESMF_SRCLINE)
      ! Seconds = 60 X 45 + 27 = 2727

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the minutes and seconds are set correctly within the day
      write(name, *) "Get StartTime in minutes, & seconds Within the Day Test"
      call ESMF_TimeGet(startTime, s=secs, m=M, dd=DD, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or day, minutes, or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.27) &
                      .and.(M.eq.1125).and.(DD.eq.13), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = 60 X 18 + 45 = 1125

      ! ----------------------------------------------------------------------------
      ! hours, minutes, seconds within the month tests
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the hour is set correctly within the month
      write(name, *) "Get StartTime Hours Within the Month Test"
      call ESMF_TimeGet(startTime, mm=MM, h=H, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or month or hour not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3).and.(H.eq.306), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Hours = 12 X 24 + 18 = 306
      print *, "hours = ", H

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the minutes is set correctly within the month
      write(name, *) "Get StartTime Minutes Within the Month Test"
      call ESMF_TimeGet(startTime, mm=MM, m=M, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or month or minutes not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3).and.(M.eq.18405), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = 12 X 1440 + 18 X 60 + 45 = 18405
      print *, "minutes = ", M

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the seconds are set correctly within the month
      write(name, *) "Get StartTime seconds Within the Month Test"
      call ESMF_TimeGet(startTime, mm=MM, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or month or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3).and.(secs.eq.1104327), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! seconds = 18405 X 60 + 27 = 1104327

      print *, "seconds = ", secs

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the hours, minutes and seconds are set correctly within the month
      write(name, *) "Get StartTime in hours, minutes & seconds Within the Month Test"
      call ESMF_TimeGet(startTime, mm=MM, h=H, m=M, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or month, hours, minutes or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3).and.(secs.eq.27) &
                      .and.(H.eq.306).and.(M.eq.45), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the hours and seconds are set correctly within the month
      write(name, *) "Get StartTime in hours, & seconds Within the Month Test"
      call ESMF_TimeGet(startTime, mm=MM, h=H, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or month, hour or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3).and.(secs.eq.2727) &
                      .and.(H.eq.306), name, failMsg, result, ESMF_SRCLINE)
      ! Seconds = 60 X 45 + 27 = 2727

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Verify the minutes and seconds are set correctly within the month
      write(name, *) "Get StartTime in minutes, & seconds Within the Month Test"
      call ESMF_TimeGet(startTime, s=secs, m=M, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or month, minutes, or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.27) &
                      .and.(M.eq.18405).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = 12 X 1440 + 18 X 60 + 45 = 18405

      ! ----------------------------------------------------------------------------
      ! hours, minutes, seconds within the year tests
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the hour is set correctly within the year
      write(name, *) "Get StartTime Hours Within the Year Test"
      call ESMF_TimeGet(startTime, yy=YY, h=H, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year or hour not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003).and.(H.eq.1722), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Hours = (59+12) X 24 + 18 = 1722
      print *, "hours = ", H

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the minutes is set correctly within the year
      write(name, *) "Get StartTime Minutes Within the Year Test"
      call ESMF_TimeGet(startTime, yy=YY, m=M, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year or minutes not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003).and.(M.eq.103365), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = (59+12) X 1440 + 18 X 60 + 45 = 103365
      print *, "minutes = ", M

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the seconds are set correctly within the year
      write(name, *) "Get StartTime seconds Within the Year Test"
      call ESMF_TimeGet(startTime, yy=YY, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003).and.(secs.eq.6201927), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! seconds = 103365 X 60 + 27 = 6201927

      print *, "seconds = ", secs

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the hours, minutes and seconds are set correctly within the year
      write(name, *) "Get StartTime in hours, minutes & seconds Within the Year Test"
      call ESMF_TimeGet(startTime, yy=YY, h=H, m=M, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year, hours, minutes or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003).and.(secs.eq.27) &
                      .and.(H.eq.1722).and.(M.eq.45), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the hours and seconds are set correctly within the year
      write(name, *) "Get StartTime in hours, & seconds Within the Year Test"
      call ESMF_TimeGet(startTime, yy=YY, h=H, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year, hour or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003).and.(secs.eq.2727) &
                      .and.(H.eq.1722), name, failMsg, result, ESMF_SRCLINE)
      ! Seconds = 60 X 45 + 27 = 2727

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the minutes and seconds are set correctly within the year
      write(name, *) "Get StartTime in minutes, & seconds Within the Year Test"
      call ESMF_TimeGet(startTime, s=secs, m=M, yy=YY, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year, minutes, or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.27) &
                      .and.(M.eq.103365).and.(YY.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = (59+12) X 1440 + 18 X 60 + 45 = 103365

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test fix for bug #1113438 "getting dayOfYear should impose day
      !   boundary for getting hrs"  From Tom Henderson/WRF
      write(name, *) "Get current_time in dayOfYear, hours, minutes, seconds, & milliseconds Within the Year Test"
      call ESMF_TimeSet(current_time, YY=2000, MM=1, DD=24, &
                        H=12, M=0, S=0, MS=0, rc=rc)
      call ESMF_TimeGet(current_time, YY=julyr, dayOfYear=julday, &
                        H=hr, M=min, S=sec, MS=ms, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or year, dayOfYear, hours, minutes, seconds, milliseconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(ms.eq.0).and.(sec.eq.0) &
                      .and.(min.eq.0).and.(hr.eq.12).and.(julday.eq.24) &
                      .and.(julyr.eq.2000), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "julyr,hr,min,sec,ms,julday = ", julyr,hr,min,sec,ms,julday

      ! ----------------------------------------------------------------------------
      ! end of hours, minutes, seconds within the year tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Start Time
      day = 25
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy=yy, mm=11, dd=day, &
                             	   h=11, m=45, s=18, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the day is set correctly
      write(name, *) "Get StartTime Day Test"
      call ESMF_TimeGet(startTime, dd=day, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(day.eq.25), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Start Time
      year = 30067
      day2 = 25
      hour = 7
      minute = 33
      second = 51
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy_i8=year, mm=6, dd=25, &
                                   h=hour, m=56, s_i8=second, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the year is set correctly
      !write(name, *) "Get Start Time Year Test"
      !call ESMF_TimeGet(startTime, yy_i8=YY, rc=rc)
      !write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.30067), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StartTime Month Test"
      call ESMF_TimeGet(startTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.6), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the day is set correctly
      !write(name, *) "Get StartTime Day Test"
      !call ESMF_TimeGet(startTime, d_i8=DD, rc=rc)
      !write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.31), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the calendar is set correctly
      write(name, *) "Get Calendar Type Test"
      call ESMF_CalendarGet(gregorianCalendar, calendarType=cal_type, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or calendar not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(cal_type.eq.ESMF_CAL_GREGORIAN), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
! run this test only on platforms that support F95 initializers, otherwise
!   may crash or produce FAIL commented out until after release.
! see bug #755424
! TODO:  test count will be "off-by-one" on platforms where this test
!        doesn't run
      ! This code crashes, bug 79753 has been opened.
      ! Attempt to get un-initialized year from stop time
      write(name, *) "Get Uninitialized StopTime Year Test"
      call ESMF_TimeGet(stopTime, yy=YY, rc=rc)
      write(failMsg, *) " Returned ESMF_SUCCESS"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (0) Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=2003, mm=0, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (13) Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=2003, mm=13, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite day
      write(name, *) "Stop Time Initialization to  Feb. 31st. Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yy=2003, mm=2, dd=31, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Set time to lower bound of Fliegel algoritm
      write(name, *) "Test lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=-4800, mm=3, dd=1, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time beyond lower bound of Fliegel algoritm
      write(name, *) "Test beyond lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yy=-4800, mm=2, dd=28, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the year is set correctly
      write(name, *) "Get StopTime Year Test"
      call ESMF_TimeGet(stopTime, yy=YY, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StopTime Month Test"
      call ESMF_TimeGet(stopTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
! run these tests only on platforms that support F95 initializers, otherwise
!   may crash or produce FAIL
! see bug #755445
! TODO:  test count will be "off-by-one" on platforms where this test
!        doesn't run
      ! ClockPrint with an unallocated clock
       write(name, *) "Clock Print Test with unallocated clock"
       write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
       call ESMF_ClockPrint(clock, rc=rc)
       call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX____UTest
      ! This test is commented out bug 1649164 has been opened.
      ! Initialize clock with uninitialized Start Time.
      ! write(name, *) "Clock Initialization Test with uninitialized startTime"
       !write(failMsg, *) " Returned ESMF_SUCCESS"
       !clock = ESMF_ClockCreate("Clock 1", timeStep, startTime2, &
                                          !stopTime, rc=rc)
       !call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                       !name, failMsg, result, ESMF_SRCLINE)
       !call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Start Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=13, &
                             	   h=18, m=45, s=27, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Time Interval Print Test"
      call ESMF_TimeIntervalPrint(timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                               stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      clock2 = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                               stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Clock Name Test"
      call ESMF_ClockSet(clock2, name="Clock 2", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Create Clock copy Test"
      clock1 = ESMF_ClockCreate(clock,  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for clock equality
      ! clocksEqual = ESMF_ClockOperator(==)(clock1,clock2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Clocks Equal Test"
      clocksEqual = (clock == clock1)
      call ESMF_Test((clocksEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for clock inequality
      ! clocksEqual = ESMF_ClockOperator(==)(clock1,clock2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Clocks Not Equal Test"
      clocksEqual = (clock == clock2)
      call ESMF_Test((.not.clocksEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for clock inequality
      ! clocksEqual = ESMF_ClockOperator(/=)(clock1,clock2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Clocks Not Equal Test"
      clocksNotEqual = (clock /= clock2)
      call ESMF_Test((clocksNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for clock equality
      ! clocksEqual = ESMF_ClockOperator(/=)(clock1,clock2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Clocks Equal Test"
      clocksNotEqual = (clock /= clock1)
      call ESMF_Test((.not.clocksNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Print Test"
      call ESMF_ClockPrint(clock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or bool not False"
      write(name, *) "ClockIsStopTime Test"
      bool = ESMF_ClockIsStopTime(clock, rc)
      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and.(.not.bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                               stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	!EX_UTest
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
        	call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      	end do

      	bool = ESMF_ClockIsStopTime(clock, rc)
      	print *, "bool = ", bool
      	write(failMsg, *) " Did not return ESMF_SUCCESS and/or bool not True"
      	write(name, *) "Clock Advance Test"
      	call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
       end if


      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      ! Sync to real time test
      ! TODO: THis test only will test for rc=ESMF_SUCCESS
      ! A test must be written that verifies that it works
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Sync to Real Time Test"
      call ESMF_TimeSet(stopTime4, yy=100000, mm=1, dd=1, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_ClockSet(clock2, stopTime=stopTime4, rc=rc)
      call ESMF_ClockSyncToRealTime(clock2,rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !EX_UTest
      ! Clock Validate
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Validate Test"
      call ESMF_ClockValidate(clock2,rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      print *, "Print Clock after advancing 4 times"
      write(failMsg, *) " Returned ESMF_FAILURE and/or bool not True"
      write(name, *) "Clock Advanced beyond stopTime Test"
      call ESMF_ClockPrint(clock, rc=rc)
      bool = ESMF_ClockIsStopTime(clock, rc)
      call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool

      ! ----------------------------------------------------------------------------

       !EX_UTest
       write(name, *) "Get previous time and verify Test"
       call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
       call ESMF_ClockAdvance(clock, rc=rc)
       write(failMsg, *) " Returned ESMF_FAILURE and/or currTime != prevTime"
       call ESMF_ClockGet(clock, prevTime=previousTime, rc=rc)
       call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(currentTime.eq.previousTime)), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
 

       !EX_UTest
       write(name, *) "Get previous SimTime and verify Test"
       call ESMF_ClockGet(clock, currSimTime=currentSimTime, rc=rc)
       call ESMF_ClockAdvance(clock, rc=rc)
       write(failMsg, *) " Returned ESMF_FAILURE and/or currSimTime != prevTime"
       call ESMF_ClockGet(clock, prevSimTime=previousSimTime, rc=rc)
       call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(currentSimTime.eq.previousSimTime)), &
                       name, failMsg, result, ESMF_SRCLINE)

      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Clock Initialization with stop time set before start time Test"
      call ESMF_TimeSet(stopTime, yy=2002, mm=3, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS because timestep is positive."
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                               stopTime, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(name, *) "Clock Initialization with stop time & start time with different calendars Test" 
      call ESMF_TimeSet(startTime, yy=2000, mm=3, dd=13, &
                                   calendar=julianCalendar, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                               stopTime, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with Years = -1000 Test" 
      call ESMF_TimeSet(startTime, yy=-1000, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with hour = zero Test" 
      call ESMF_TimeSet(startTime, yy=-1000, h=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
     write(failMsg, *) "Should return ESMF_SUCCESS."
     write(name, *) "Time Initialization with hour = negative 3 Test" 
     call ESMF_TimeSet(startTime, h=-3, calendar=gregorianCalendar, rc=rc)
     call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)
     call ESMF_TimePrint(startTime, rc=rc)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with hour = 3 Test" 
      call ESMF_TimeSet(startTime, h=3, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with hour = 25 Test" 
      call ESMF_TimeSet(startTime, h=25, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = zero Test" 
      call ESMF_TimeSet(startTime, m=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = negative 5  Test" 
      call ESMF_TimeSet(startTime, m=-5, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = 5  Test" 
      call ESMF_TimeSet(startTime, m=5, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = 65 Test" 
      call ESMF_TimeSet(startTime, m=65, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds = 0 Test" 
      call ESMF_TimeSet(startTime, s=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds =  negative 8 Test" 
      call ESMF_TimeSet(startTime, s=-8,calendar=gregorianCalendar,  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds =  8 with no calendar type specified Test" 
      call ESMF_TimeSet(startTime, s=8, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds =  8 with calendar type specified Test" 
      call ESMF_TimeSet(startTime, s=8, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds = 65 Test" 
      call ESMF_TimeSet(startTime, s=65, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with milli-seconds = 0 Test" 
      call ESMF_TimeSet(startTime, ms=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with milli-seconds = 84 Test" 
      call ESMF_TimeSet(startTime, ms=84, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with milli-seconds = 101 Test" 
      call ESMF_TimeSet(startTime, ms=101, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with micro-seconds = 0 Test" 
      call ESMF_TimeSet(startTime, us=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with micro-seconds = 55 Test" 
      call ESMF_TimeSet(startTime, us=55, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with micro-seconds = 101 Test" 
      call ESMF_TimeSet(startTime, us=101, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=-100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, h=-1, rc=rc)
      clock_gregorian = ESMF_ClockCreate("Gregorian Clock", timeStep, &
                                         startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	totalDays=0
      	days=-1
      	call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      	call ESMF_TimePrint(startTime, rc=rc)
      	call ESMF_TimePrint(stopTime, rc=rc)
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock_gregorian, rc))
        	call ESMF_ClockAdvance(clock_gregorian, timeStep=timeStep, rc=rc)
        	totalDays=totalDays+days
      	end do

       end if

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days -73049 from +100 to -100 years in Gregorian Cal. Test"
      call ESMF_Test((totalDays.eq.-73049), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Total Counts 73049 from -100 to +100 years in Gregorian Cal. Test"
      call ESMF_ClockGet(clock_gregorian, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "Results Total Counts = ", advanceCounts
      call ESMF_Test((advanceCounts.eq.73049), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock_gregorian, rc)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=10, rc=rc)
      write(name, *) "Time Step initialization with years = 10 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      clock_gregorian = ESMF_ClockCreate("Gregorian Clock", timeStep, &
                                         startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	call date_and_time(values=timevals)
      	seed=timevals(8)
      	call random_seed(put=seed)
      	testResults = 0
      	do while (.not.ESMF_ClockIsStopTime(clock_gregorian, rc))
        	call random_number(ranNum)
        	days=ranNum*25
        	call random_number(ranNum)
        	H=ranNum*100
        	call random_number(ranNum)
        	MM=ranNum*100
        	call random_number(ranNum)
        	secs=ranNum*100
        	call ESMF_TimeIntervalSet(timeStep, d=days, h=H, m=MM, s=secs, rc=rc)
        	call ESMF_ClockAdvance(clock_gregorian, timeStep=timeStep, rc=rc)
        	call ESMF_ClockGet(clock_gregorian, currTime=currentTime, rc=rc)
        	call ESMF_ClockGet(clock_gregorian, prevTime=previousTime, rc=rc)
        	timeDiff =  currentTime - previousTime 

                ! Note: this timeInterval comparison depends on
                ! ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN) being set
                ! so the timeIntervals' (timeStep) magnitude can be determined.
        	if((timeDiff.ne.timeStep).and.(testResults.eq.0)) then	
	     		testResults=1
             		call ESMF_TimeIntervalPrint(timeStep, rc=rc)
             		call ESMF_TimeIntervalPrint(timeDiff, rc=rc)
             		call ESMF_TimePrint(currentTime, rc=rc)
             		call ESMF_TimePrint(previousTime, rc=rc)
	                ! Exit loop on first failure
			goto 10
        	end if
      	end do
10    continue

      end if
		
        !EX_UTest
        write(failMsg, *) "Time comparison failed."
        write(name, *) "Current Time minus PreviousTime = timeStep for Gregorian Cal. Test"
        call ESMF_Test((testResults.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_ClockDestroy(clock_gregorian, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=no_leapCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test" 
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=no_leapCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
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

      if (rc.eq.ESMF_SUCCESS) then
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

      end if

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 73000 from -100 to +100 years in No Leap Cal. Test"
      call ESMF_Test((totalDays.eq.73000), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(name, *) "Total Counts 73000 from -100 to +100 years in No Leap Cal. Test"
      call ESMF_ClockGet(clock_no_leap, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "Results Total Counts = ",  advanceCounts
      call ESMF_Test((advanceCounts.eq.73000), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock_no_leap, rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      call ESMF_TimeSet(startTime, yy=100, mm=1, dd=1, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=esmf_360dayCalendar, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      clock_360day = ESMF_ClockCreate("360 Day Clock", timeStep, startTime, &
                                         stopTime, rc=rc)
      write(name, *) "Clock initialization with Start Time equal to Stop Time Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock_360day, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, rc=rc)
      write(name, *) "Time Step initialization with nothing set Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=31, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      write(name, *) "Start Time set to illegite day in 360 day Calendar Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=380, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      write(name, *) "Start Time set to illegite day in 360 day Calendar Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------


      !EX_UTest

      write(failMsg, *) "Did not return ESMC_RC_VAL_WRONG."
      call ESMF_TimeSet(startTime, yy=100, mm=1, dd=1, h=0,  m=-9, s=-5, &
                                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=99, mm=12, dd=31, h=23, m=50, s=55, &
                                        calendar=gregorianCalendar, rc=rc)

      write(name, *) "Time setting equivalency Test"
      clock_gregorian = ESMF_ClockCreate("Gregorian Clock", timeStep, &
                                         startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMC_RC_VAL_WRONG), &
                       name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------

      ! uncomment when fixed
      !write(failMsg, *) "Should not return ESMF_SUCCESS."
      !call ESMF_TimeSyncToRealTime(syncTime, rc)
      !write(name, *) "Time Sync to Real Time Test"
      !call ESMF_Test((rc.ne.ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------

      !EX_UTest
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       write(name, *) "Sync Time Set Calendar Test"
       call ESMF_TimeSet(syncTime, calendar=gregorianCalendar, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      
      !EX_UTest
       write(failMsg, *) "Should return ESMF_SUCCESS."
       ! It is possible, but not probable, that a year boundary will be crossed
       ! between the next two calls, causing the test for year match below to
       ! fail.  Keep these two calls adjacent to one another to ensure the
       ! lowest probability of failure.
       call ESMF_TimeSyncToRealTime(syncTime, rc)
       call date_and_time(values=datetime)

       write(name, *) "Time Sync to Real Time Test"
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      
      !EX_UTest
      ! Verify the year is set correctly
       write(name, *) "Get Sync Time Year Test"
       call ESMF_TimeGet(syncTime, yy=YY, rc=rc)
       write(failMsg, *) " Did not return ESMF_SUCCESS and/or Year not correct value"
       call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YY.eq.datetime(1)), &
                       name, failMsg, result, ESMF_SRCLINE)
      
       print *, " Sync Time year = ", YY
       print *, " Get Sync Time rc  = ", rc

     ! ----------------------------------------------------------------------------

      !EX_UTest
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       write(name, *) "Sync Time Print Test"
       call ESMF_TimePrint(syncTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(stopTime, rc=rc)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Test Setting Time Interval
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval to 73049 days Test"
      call ESMF_TimeIntervalSet(timeStep, d=73049, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimeIntervalPrint(timeStep, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Incrementing Time by Time Interval
      write(failMsg, *) " startTime not equal to stopTime"
      write(name, *) "Incrementing starttime by 73049 days Test"
      startTime=startTime+timeStep
      call ESMF_Test((startTime.eq.stopTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimePrint(startTime, rc=rc)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime, rc=rc)

      ! ----------------------------------------------------------------------------

      ! Test Decrementing Time by Time Interval
      !EX_UTest
      write(failMsg, *) " startTime not equal to stopTime"
      write(name, *) "Decrementing stoptime by 73049 days Test"
      stopTime=stopTime-timeStep
      call ESMF_Test((startTime.eq.stopTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimePrint(stopTime, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yy=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be equal to startTime"
      write(name, *) "Verifying the EQ operator Test"
      call ESMF_Test((startTime2.eq.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be equal to startTime"
      write(name, *) "Verifying the NE operator Test"
      call ESMF_Test(.not.(startTime2.ne.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yy=-100, mm=1, dd=1, s=-1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should not be equal to startTime"
      write(name, *) "Verifying the EQ operator Test"
      call ESMF_Test(.not.(startTime2.eq.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "startTime2 should not be equal to startTime"
      write(name, *) "Verifying the NE operator Test"
      call ESMF_Test((startTime2.ne.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be less than startTime"
      write(name, *) "Verifying the LT operator Test"
      call ESMF_Test((startTime2.lt.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should less than startTime"
      write(name, *) "Verifying the LT operator Test"
      call ESMF_Test(.not.(startTime.lt.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should be greater than startTime2"
      write(name, *) "Verifying the GT operator Test"
      call ESMF_Test((startTime.gt.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should less than startTime2"
      write(name, *) "Verifying the GT operator Test"
      call ESMF_Test(.not.(startTime2.gt.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be less than or equal to startTime"
      write(name, *) "Verifying the LE operator Test"
      call ESMF_Test((startTime2.le.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be less than or equal to startTime"
      write(name, *) "Verifying the LE operator Test"
      call ESMF_Test(.not.(startTime.le.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yy=-100, mm=1, dd=1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should equal startTime2"
      write(name, *) "Verifying the LE operator Test"
      call ESMF_Test((startTime2.le.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should be greater than or equal to startTime2"
      write(name, *) "Verifying the GE operator Test"
      call ESMF_Test((startTime.ge.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be greater than or equal to startTime"
      write(name, *) "Verifying the GE operator Test"
      call ESMF_Test((startTime2.ge.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yy=-100, mm=1, dd=1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should equal startTime2"
      write(name, *) "Verifying the GE operator Test"
      call ESMF_Test((startTime2.ge.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate an uninitialized time
      write(failMsg, *) "Should not return ESMF_SUCCESS "
      write(name, *) "Validate an uninitialzed time Test"
      call ESMF_TimeValidate(stopTime3, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime3, yy=-100, mm=1, dd=1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate an initialized time
      write(failMsg, *) "Should return ESMF_SUCCESS "
      write(name, *) "Validate an initialzed time Test"
      call ESMF_TimeValidate(stopTime3, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime3, yy=-4900, mm=2, dd=28, &
                                  calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = -4900 Test"
      print *, "  "
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate an initialized time
      write(failMsg, *) "Should return ESMF_SUCCESS "
      write(name, *) "Validate an initialzed time Test"
      call ESMF_TimeValidate(stopTime3, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=10, rc=rc)
      write(name, *) "Time Step initialization with years = 10 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yy=-100, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "startTime = "
      call ESMF_TimePrint(startTime, "string", rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yy=100, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "stopTime = "
      call ESMF_TimePrint(startTime, "string", rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      clock_gregorian = ESMF_ClockCreate("Gregorian Clock", timeStep, &
                                         startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_ClockIsStopTime() false."
      bool = ESMF_ClockIsStopTime(clock_gregorian, rc)
      write(name, *) "Check mm/dd defaults Test"
      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and.(.not.bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool
      call ESMF_ClockDestroy(clock_gregorian, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Millisecond clock test"
      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=28, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2004, mm=9, dd=28, &
                                h=0, m=1, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, ms=1, rc=rc)
      clock = ESMF_ClockCreate("Millisecond Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      if (rc.eq.ESMF_SUCCESS) then
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
      	end do
      end if

      call ESMF_ClockPrint(clock, rc=rc)
      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      call ESMF_ClockPrint(clock, "timestep", rc=rc)
      call ESMF_ClockPrint(clock, "timestep string", rc=rc)
      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "Millisecond clock advanced ", advanceCounts, &
                        ", not 60,000 or not ESMF_SUCCESS."
      call ESMF_Test((advanceCounts.eq.60000.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)
      print *, "Millisecond clock advanced ", advanceCounts, " times."

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Microsecond clock test"
      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=28, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2004, mm=9, dd=28, &
                                s=1, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, us=100, rc=rc)
      clock = ESMF_ClockCreate("Microsecond Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      if (rc.eq.ESMF_SUCCESS) then
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
      	end do
      end if

      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      call ESMF_ClockPrint(clock, "timestep", rc=rc)
      call ESMF_ClockPrint(clock, "timestep string", rc=rc)
      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "Microsecond clock advanced ", advanceCounts, &
                        ", not 10,000 or not ESMF_SUCCESS."
      call ESMF_Test((advanceCounts.eq.10000.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)
      print *, "Microsecond clock advanced ", advanceCounts, " times."

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Nanosecond clock test"
      call ESMF_TimeSet(startTime,  yy=2004, mm=9, dd=28, ns=10000, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=28, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, ns=-1, rc=rc)
      clock = ESMF_ClockCreate("Nanosecond Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      call ESMF_ClockPrint(clock, "currtime string", rc=rc)

      if (rc.eq.ESMF_SUCCESS) then
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
      	end do
      end if

      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      call ESMF_ClockPrint(clock, "timestep", rc=rc)
      call ESMF_ClockPrint(clock, "timestep string", rc=rc)
      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "Nanosecond clock advanced ", advanceCounts, &
                        ", not 10,000 or not ESMF_SUCCESS."
      call ESMF_Test((advanceCounts.eq.10000.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Nanosecond clock advanced ", advanceCounts, " times."

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Nanosecond time string test"
      call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
      call ESMF_TimeGet(startTime, timeStringISOFrac=timeString, rc=rc)
      write(failMsg, *) "Nanosecond time string not 2004-09-28T00:00:00.000010000 or not ESMF_SUCCESS."
      call ESMF_Test((timeString=="2004-09-28T00:00:00.000010000" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "timeString = ", timeString
      call ESMF_TimePrint(startTime)
      call ESMF_TimePrint(startTime, "string isofrac")

      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Nanosecond time string test 2"
      call ESMF_TimeGet(startTime, timeString=timeString, rc=rc)
      write(failMsg, *) "Nanosecond time string not 2004-09-28T00:00:00:1/100000 or not ESMF_SUCCESS."
      print *, "  "
      call ESMF_Test((timeString=="2004-09-28T00:00:00:1/100000" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime)
      call ESMF_TimePrint(startTime, "string")

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Nanosecond time interval string test"
      call ESMF_TimeIntervalGet(timeStep, timeStringISOFrac=timeString, rc=rc)
      write(failMsg, *) "Nanosecond time interval string not P0Y0M0DT0H0M-0.000000001S or not ESMF_SUCCESS."
      call ESMF_Test((timeString=="P0Y0M0DT0H0M-0.000000001S" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "timeString = ", timeString
      call ESMF_TimeIntervalPrint(timeStep)
      call ESMF_TimeIntervalPrint(timeStep, "string isofrac")

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Nanosecond time interval string test 2"
      call ESMF_TimeIntervalGet(timeStep, timeString=timeString, rc=rc)
      write(failMsg, *) "Nanosecond time interval string not P0Y0M0DT0H0M0:-1/1000000000S or not ESMF_SUCCESS."
      call ESMF_Test((timeString=="P0Y0M0DT0H0M0:-1/1000000000S" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimeIntervalPrint(timeStep)
      call ESMF_TimeIntervalPrint(timeStep, "string")

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Rational fraction time step clock test"
      call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=28, sN=1, sD=3, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2004, mm=9, dd=28, s=1, sN=2, sD=3, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, sN=1, sD=3, rc=rc)
      clock = ESMF_ClockCreate("1/3 second Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      if (rc.eq.ESMF_SUCCESS) then
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
      	end do
      end if

      call ESMF_ClockPrint(clock, "currtime", rc=rc)
      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      call ESMF_ClockPrint(clock, "timestep", rc=rc)
      call ESMF_ClockPrint(clock, "timestep string", rc=rc)
      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      write(failMsg, *) "1/3 second clock advanced ", advanceCounts, &
                        ", not 4 times, or not ESMF_SUCCESS."
      call ESMF_Test((advanceCounts.eq.4.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)
      print *, "1/3 second clock advanced ", advanceCounts, " times."

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! From Shujia Zhou in Support #1091846, Bug #1099731
      write(name, *) "ESMF_TimeGet() Seconds Beyond a Day Test"
      write(failMsg, *) " currentTime seconds incorrect or ESMF_FAILURE"

      call ESMF_TimeIntervalSet(timeStep, s=600, calendar=no_leapCalendar,rc=rc)
      call ESMF_TimeSet(startTime, s=0, calendar=no_leapCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, s=180000, calendar=no_leapCalendar, rc=rc)

      topClock = ESMF_ClockCreate("Top Level Clock", timeStep, startTime, &
                                  stopTime=stopTime, rc=rc)

      checkSec = 0
      testPass = .true.
      do while (.not. ESMF_ClockIsStopTime(topClock, rc ))
        call ESMF_ClockGet(topClock, currTime=currentTime)
        call ESMF_TimeGet(currentTime, s=secs)
        if (secs .ne. checkSec) then
          testPass = .false. 
        end if
        checkSec = checkSec + 600
        call ESMF_ClockAdvance(topClock, rc=rc)
      end do

      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! This checks Tom Henderson's WRF request that a clock be valid at
      ! the end of a timestep loop when currTime == stopTime.
      ! Bug #1101839, Support #1099854
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Clock Valid at end of Advance Loop Test"
      call ESMF_ClockValidate(topClock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_ClockDestroy(topClock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test fix for Support #1099584, Bug #1101904 "Disable stopTime to make
      ! clock run forever" requested by Tom Henderson/WRF
      write(name, *) "Clock stopTime disabled test"

      call ESMF_TimeSet(startTime, yy=2005, mm=2, dd=3, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2005, mm=2, dd=4, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      clock = ESMF_ClockCreate("Hourly Clock", &
                               timeStep, startTime, stopTime, rc=rc)

      testPass = .false.
      clockStopped = .false.

      if (rc.eq.ESMF_SUCCESS) then
        ! set clock to run "forever"
        call ESMF_ClockStopTimeDisable(clock, rc)
        stopTimeEnabled = ESMF_ClockIsStopTimeEnabled(clock, rc)

        ! time step from start time to one step beyond stop time
        do while (.not.clockStopped)
          call ESMF_ClockAdvance(clock, rc=rc)
          clockStopped = ESMF_ClockIsStopTime(clock, rc)
          call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
          if (currentTime == (stopTime + timeStep)) then
            testPass = .true.
            clockStopped = .true.
          end if
        end do
      end if

      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      print *, "Hourly clock advanced ", advanceCounts, " times."
      write(failMsg, *) "Disabled hourly clock advanced ", advanceCounts, &
                        ", not 25 times, or not disabled, or not ESMF_SUCCESS."
      call ESMF_Test((testPass .and. .not.stopTimeEnabled .and. &
                      advanceCounts.eq.25 .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test fix for Support #1099584, Bug #1101904 "Disable stopTime to make
      ! clock run forever" requested by Tom Henderson/WRF
      write(name, *) "Clock stopTime re-enabled test"

      testPass = .true.
      clockStopped = .false.

      ! re-enable and reset clock stopTime to the next day
      call ESMF_TimeSet(stopTime, yy=2005, mm=2, dd=5, rc=rc)
      call ESMF_ClockStopTimeEnable(clock, stopTime=stopTime, rc=rc)
      stopTimeEnabled = ESMF_ClockIsStopTimeEnabled(clock, rc)

      ! time step from current time to stop time
      do while (.not.clockStopped)
        call ESMF_ClockAdvance(clock, rc=rc)
        clockStopped = ESMF_ClockIsStopTime(clock, rc)
        call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
        if (currentTime > stopTime) then
          testPass = .false.
          clockStopped = .true.
        end if
      end do

      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      print *, "Hourly clock advanced ", advanceCounts, " times."
      write(failMsg, *) "Re-enabled hourly clock advanced ", advanceCounts, &
                        ", not 48 times, or not enabled, or not ESMF_SUCCESS."
      call ESMF_Test((testPass .and. stopTimeEnabled .and. &
                      advanceCounts.eq.48 .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Reverse day clock test 1"
      call ESMF_TimeSet(startTime, yy=2005, mm=5, dd=24, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2005, mm=6, dd=24, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      clock = ESMF_ClockCreate("Day Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      stepOnePass = .false.
      stepTwoPass = .false.

      runTheClock = .true.
      do while (runTheClock)

      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsDone(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
      	end do

        call ESMF_ClockGet(clock, advanceCount=advanceCounts, &
                           direction=direction, rc=rc)
        if (direction .eq. ESMF_MODE_FORWARD) then
          print *, "Reverse clock advanced ", advanceCounts, " times forward."
          if (advanceCounts .eq. 31 .and. rc .eq. ESMF_SUCCESS) then
            stepOnePass = .true.
          end if
          call ESMF_ClockSet(clock, direction=ESMF_MODE_REVERSE, rc=rc)
        else
          print *, "Reverse clock count is ", advanceCounts, "."
          if (advanceCounts .eq. 0 .and. rc .eq. ESMF_SUCCESS) then
            stepTwoPass = .true.
          end if
          runTheClock = .false.
        end if

      end do

      write(failMsg, *) "Reverse clock 1 failed."
      call ESMF_Test((stepOnePass .and. stepTwoPass), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Reverse day clock test 2"
      call ESMF_TimeSet(startTime, yy=2005, mm=5, dd=24, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2005, mm=6, dd=24, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      clock = ESMF_ClockCreate("Day Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      stepOnePass = .false.
      stepTwoPass = .false.

      ! time step 2/3 of the way from start time to stop time and back again
      do while (.not.ESMF_ClockIsDone(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)

        call ESMF_ClockGet(clock, advanceCount=advanceCounts, &
                           direction=direction, rc=rc)
        ! print *, "Reverse clock advanceCount = ", advanceCounts

        if (direction .eq. ESMF_MODE_FORWARD) then
          if (advanceCounts .eq. 20 .and. rc .eq. ESMF_SUCCESS) then
            print *, "Reverse clock advanced ", advanceCounts, " times forward."
            stepOnePass = .true.
            call ESMF_ClockSet(clock, direction=ESMF_MODE_REVERSE, rc=rc)
          end if
        else
          if (advanceCounts .eq. 0 .and. rc .eq. ESMF_SUCCESS) then
            print *, "Reverse clock count is ", advanceCounts, "."
            stepTwoPass = .true.
          end if
        end if

      end do

      write(failMsg, *) "Reverse clock 2 failed."
      call ESMF_Test((stepOnePass .and. stepTwoPass), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Reverse day clock test 3"
      call ESMF_TimeSet(startTime, yy=2005, mm=5, dd=24, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2005, mm=6, dd=24, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      clock = ESMF_ClockCreate("Day Clock", &
                                         timeStep, startTime, stopTime, rc=rc)

      stepOnePass = .false.
      stepTwoPass = .false.

      ! time step 2/3 of the way from start time to stop time, then back 1/3,
      !   and then forward to stopTime
      do while (.not.ESMF_ClockIsDone(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)

        call ESMF_ClockGet(clock, advanceCount=advanceCounts, &
                           direction=direction, rc=rc)
        ! print *, "Reverse clock advanceCount = ", advanceCounts

        if (direction .eq. ESMF_MODE_FORWARD) then
          if (.not.stepOnePass) then
            if (advanceCounts .eq. 20 .and. rc .eq. ESMF_SUCCESS) then
              print *, "Reverse clock advanced ", advanceCounts, " times forward."
              stepOnePass = .true.
              call ESMF_ClockSet(clock, direction=ESMF_MODE_REVERSE, rc=rc)
            end if
          end if
        else
          if (advanceCounts .eq. 10 .and. rc .eq. ESMF_SUCCESS) then
            print *, "Reverse clock reversed to ", advanceCounts, "."
            call ESMF_ClockSet(clock, direction=ESMF_MODE_FORWARD, rc=rc)
            stepTwoPass = .true.
          end if
        end if

      end do

      print *, "Reverse clock ended at advanceCount ", advanceCounts
      write(failMsg, *) "Reverse clock 3 failed."
      call ESMF_Test((stepOnePass .and. stepTwoPass .and. &
                      advanceCounts .eq. 31), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Set runTimeStepCount test 1"
      call ESMF_TimeSet(startTime, yy=2006, mm=9, dd=8, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime,  yy=2006, mm=9, dd=12, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, h=24, rc=rc)
      clock = ESMF_ClockCreate("Day Clock", &
                                timeStep, startTime, stopTime, rc=rc)
      timeStepCount = 2
      call ESMF_ClockSet(clock, runTimeStepCount=timeStepCount, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
      end do

      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      print *, "advanceCounts = ", advanceCounts

      write(failMsg, *) "runTimeStepCount test 1 failed."
      call ESMF_Test(advanceCounts.eq.2 .and. rc.eq.ESMF_SUCCESS, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Set runTimeStepCount test 2"
      timeStepCount = 3
      clock = ESMF_ClockCreate("Day Clock", timeStep, startTime, &
                               runTimeStepCount=timeStepCount, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
      end do

      call ESMF_ClockGet(clock, advanceCount=advanceCounts, rc=rc)
      print *, "advanceCounts = ", advanceCounts

      write(failMsg, *) "runTimeStepCount test 2 failed."
      call ESMF_Test(advanceCounts.eq.3 .and. rc.eq.ESMF_SUCCESS, &
                     name, failMsg, result, ESMF_SRCLINE)
      call ESMF_ClockDestroy(clock, rc)

      ! ----------------------------------------------------------------------------
#endif
      ! destroy calendars
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      call ESMF_CalendarDestroy(julianCalendar, rc)
      call ESMF_CalendarDestroy(no_leapCalendar, rc)
      call ESMF_CalendarDestroy(esmf_360dayCalendar, rc)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_ClockTest
