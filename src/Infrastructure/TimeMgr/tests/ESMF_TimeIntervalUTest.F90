! $Id: ESMF_TimeIntervalUTest.F90,v 1.46.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
      program ESMF_TimeIntervalUTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_TimeIntervalTest - Test Time Interval initialization and time-stepping
!
! !DESCRIPTION:
!
! The code in this file drives F90 Time Interval unit tests.
! The companion file ESMF\_TimeInterval.F90 contains the definitions for the
! TimeInterval methods.
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
      '$Id: ESMF_TimeIntervalUTest.F90,v 1.46.2.4 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, M, S, MM, DD, D, YY, days, months, years, &
                 hours, secs, ans, ns, sN, sD
      logical :: bool

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! to retrieve time in string format
      character(ESMF_MAXSTR) :: timeString

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2, timeStep3
      type(ESMF_Time) :: startTime, time1, time2
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             julianDayCalendar, &
                             noLeapCalendar, day360Calendar
      type(ESMF_TimeInterval) :: absoluteTime, diffTime
      type(ESMF_TimeInterval) :: timeInterval1, timeInterval2, timeInterval3, &
                                 timeInterval4, timeInterval5, timeInterval6
      integer(ESMF_KIND_I8) :: days2
      real(ESMF_KIND_R8) :: ratio, days_r8


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
      ! Gregorian Leap year 2004 tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/29/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing TimeInterval Validation
      write(name, *) "Time Interval Validate Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalValidate(timeStep, rc=rc)
      call ESMF_Test(( rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/29/2004 by mm= -1 Test"
      write(failMsg, *) " Did not return 12/29/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==12 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Gregorian Calendar Interval decrement 3/31/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Gregorian Calendar Interval decrement 1/31/2004 by mm= -1 Test"
      write(failMsg, *) " Did not return 2/29/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/30/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2004 by (mm=1, d=1) Test"
      write(failMsg, *) " Did not return 3/1/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2004 by mm=2 Test"
      write(failMsg, *) " Did not return 3/31/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2004 by mm=3 Test"
      write(failMsg, *) " Did not return 4/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==4 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2004 by mm=49 Test"
      write(failMsg, *) " Did not return 2/29/2008 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2008 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time1, timestep)
      write(name, *) "Gregorian Calendar Interval decrement 1/31/2004 by mm=49 Test"
      write(failMsg, *) " Did not return 12/31/1999 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1999 .and. MM==12 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 2/28/2004 by d=2 Test"
      write(failMsg, *) " Did not return 3/1/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 2/29/2004 by yy=1 Test"
      write(failMsg, *) " Did not return 2/28/2005 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2005 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 12/1/2004 by (yy=2, mm=24, d=90) Test"
      write(failMsg, *) " Did not return 3/1/2009 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2009 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should be yy=-2, mm=-24, d=-90 and ESMF_SUCCESS"
      write(name, *) "Negate Time Step Test 1"
      timeStep = -timeStep
      call ESMF_TimeIntervalGet(timeStep, yy=years, mm=months, d=days, rc=rc)
      call ESMF_Test((years.eq.-2.and.months.eq.-24.and.days.eq.-90.and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, " yy,mm,d = ", years, ",", months, ",", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 12/1/2004 by (yy= -2, mm=24, d= -90) Test"
      write(failMsg, *) " Did not return 9/2/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=-2, mm=24, d=-90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==9 .and. DD==2 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should be yy=2, mm=-24, d=90 and ESMF_SUCCESS"
      write(name, *) "Negate Time Step Test 2"
      timeStep = -timeStep
      call ESMF_TimeIntervalGet(timeStep, yy=years, mm=months, d=days, rc=rc)
      call ESMF_Test((years.eq.2.and.months.eq.-24.and.days.eq.90.and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, " yy,mm,d = ", years, ",", months, ",", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Gregorian Calendar Interval decrement 3/1/2004 by (yy=2, mm=24, d=90, h=13, m=62, s=68) Test"
      write(failMsg, *) " Did not return 12/1/1999 22:14:50 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=1, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, h=13, m=62, s=68, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1999 .and. MM==12 .and. DD==1 .and. &
                      H==22 .and. M==14 .and. S==50 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should be yy=-2, mm=-24, d=-90, h=-14, m=-3, s=-8 and ESMF_SUCCESS"
      write(name, *) "Negate Time Step Test 3"
      timeStep = -timeStep
      call ESMF_TimeIntervalGet(timeStep, yy=YY, mm=MM, d=D, &
                                h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY.eq.-2.and.MM.eq.-24.and.D.eq.-90.and. &
                      H.eq.-14.and.M.eq.-3.and.S.eq.-8.and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, " yy,mm,d,h,m,s = ", YY, ",", MM, ",", D, ",", H, ",", M, ",", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Gregorian Calendar Interval decrement 3/1/2004 by (yy=2, mm=-24, d=90, h=-13, m=62, s=-68) Test"
      write(failMsg, *) " Did not return 12/3/2003 00:17:06 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=1, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=-24, d=90, h=-13, m=62, s=-68, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==12 .and. DD==3 .and. &
                      H==0 .and. M==17 .and. S==6 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should be yy=-2, mm=24, d=-90, h=11, m=59, s=8 and ESMF_SUCCESS"
      write(name, *) "Negate Time Step Test 4"
      timeStep = -timeStep
      call ESMF_TimeIntervalGet(timeStep, yy=YY, mm=MM, d=D, &
                                h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY.eq.-2.and.MM.eq.24.and.D.eq.-90.and. &
                      H.eq.11.and.M.eq.59.and.S.eq.8.and. &
                      rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, " yy,mm,d,h,m,s = ", YY, ",", MM, ",", D, ",", H, ",", M, ",", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (1 year to days) Test"
      write(failMsg, *) " Did not return 366 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==366.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (2 years to months) Test"
      write(failMsg, *) " Did not return 24 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=2, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, &
                                calendarIn=gregorianCalendar, rc=rc)
      call ESMF_Test((months==24.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (months to years) Test"
      write(failMsg, *) " Did not return 3 years and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=36, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, &
                                calendarIn=gregorianCalendar, rc=rc)
      call ESMF_Test((years==3.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years = ", years

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (2 months to days) Test"
      write(failMsg, *) " Did not return 60 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==60.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (days to months) Test"
      write(failMsg, *) " Did not return 2 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=60, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, startTimeIn=startTime, &
                                timeString=timeString, rc=rc)
      call ESMF_Test((months==2 .and. timeString=="P0Y0M60DT0H0M0S" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months
      print *, "timeStep = ", timeString

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (730 days to years and days) Test"
      write(failMsg, *) " Did not return years=1, days=364 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=31, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=730, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==1 .and. days==364 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (731 days to years and days) Test"
      write(failMsg, *) " Did not return years=2, days=0 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2002, mm=3, dd=31, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=731, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==2 .and. days==0 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      ! Gregorian Non-leap year 2003 tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/29/2003 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/30/2003 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2003 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2003 by (mm=1, d=1) Test"
      write(failMsg, *) " Did not return 3/1/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeIntervalOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2003 by mm=2 Test"
      write(failMsg, *) " Did not return 3/31/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==3 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2003 by mm=3 Test"
      write(failMsg, *) " Did not return 4/30/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==4 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 1/31/2003 by mm=49 Test"
      write(failMsg, *) " Did not return 2/28/2007 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2007 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 2/28/2003 by d=2 Test"
      write(failMsg, *) " Did not return 3/2/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==3 .and. DD==2 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 2/28/2003 by yy=1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 12/1/2003 by (yy=2, mm=24, d=90) Test"
      write(failMsg, *) " Did not return 2/29/2008 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2008 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (1 year to days) Test"
      write(failMsg, *) " Did not return 365 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==365.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (2 years to months) Test"
      write(failMsg, *) " Did not return 24 months or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=2, calendar=gregorianCalendar, &
                                rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, rc=rc)
      call ESMF_Test((months==24 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (months to years) Test"
      write(failMsg, *) " Did not return 4 years or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=48, calendar=gregorianCalendar, &
                                rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, rc=rc)
      call ESMF_Test((years==4 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years = ", years

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (2 months to days) Test"
      write(failMsg, *) " Did not return 59 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==59.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (days to months) Test"
      write(failMsg, *) " Did not return 2 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=59, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, startTimeIn=startTime, &
                                rc=rc)
      call ESMF_Test((months==2.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (730 days to years and days) Test"
      write(failMsg, *) " Did not return years=2, days=0 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2005, mm=3, dd=31, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=730, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==2 .and. days==0 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval conversion (731 days to years and days) Test"
      write(failMsg, *) " Did not return years=2, days=1 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2001, mm=1, dd=31, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=731, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==2 .and. days==1 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      ! General Gregorian Calendar tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Gregorian Calendar Interval increment 12/31/2004 by d=1 Test"
      write(failMsg, *) " Did not return 1/1/2005 12:17:58 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=12, dd=31, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2005 .and. MM==1 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the == operator
      ! resultTime = ESMF_TimeOperator(==)(timestep, timestep)
      write(name, *) "Gregorian Calendar Interval == test"
      write(failMsg, *) " Did not return true"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=4, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=2, mm=16, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((timeStep == timeStep2), name, failMsg, result, &
                      ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval Abs test"
      write(failMsg, *) " Did not return mm=32, d=10 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=-3, mm=4, d=-10, &
                                calendar=gregorianCalendar, rc=rc)
      timeStep2 = ESMF_TimeIntervalAbsValue(timeStep)
      call ESMF_TimeIntervalGet(timeStep2, mm=months, d=days, rc=rc)
      call ESMF_Test((months==32 .and. days==10 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months, days, rc = ", months, days, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval Negative Abs test"
      write(failMsg, *) " Did not return mm=-32, d=-10 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=-4, d=10, &
                                calendar=gregorianCalendar, rc=rc)
      timeStep2 = ESMF_TimeIntervalNegAbsValue(timeStep)
      call ESMF_TimeIntervalGet(timeStep2, mm=months, d=days, rc=rc)
      call ESMF_Test((months==-32 .and. days==-10 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months, days, rc = ", months, days, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the MOD operator
      ! timestep = ESMF_TimeIntervalFunction(MOD)(timestep, timestep)
      write(name, *) "Gregorian Calendar Interval Modulus test 1"
      write(failMsg, *) " Did not return mm=10 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=4, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=2, mm=6, &
                                calendar=gregorianCalendar, rc=rc)
      timeStep3 = MOD(timeStep, timeStep2)
      call ESMF_TimeIntervalGet(timeStep3, mm=months, rc=rc)
      call ESMF_Test((months==10 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months, rc = ", months, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the MOD operator
      ! timestep = ESMF_TimeIntervalFunction(MOD)(timestep, timestep)
      write(name, *) "Gregorian Calendar Interval Modulus test 2"
      write(failMsg, *) " Did not return yy=2 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=5, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=3, &
                                calendar=gregorianCalendar, rc=rc)
      timeStep3 = MOD(timeStep, timeStep2)
      call ESMF_TimeIntervalGet(timeStep3, yy=years, rc=rc)
      call ESMF_Test((years==2 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, rc = ", years, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the MOD operator
      ! timestep = ESMF_TimeIntervalFunction(MOD)(timestep, timestep)
      write(name, *) "Gregorian Calendar Interval Modulus test 3"
      write(failMsg, *) " Did not return d=7 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=15, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, d=8, &
                                calendar=gregorianCalendar, rc=rc)
      timeStep3 = MOD(timeStep, timeStep2)
      call ESMF_TimeIntervalGet(timeStep3, d=days, rc=rc)
      call ESMF_Test((days==7 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days, rc = ", days, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval ratio test 1"
      write(failMsg, *) " Did not return 0.5 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=5, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, d=10, &
                                calendar=gregorianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-.5) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval ratio test 2"
      write(failMsg, *) " Did not return 0.25 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=12, &
                                calendar=gregorianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-.25) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval ratio test 3"
      write(failMsg, *) " Did not return 0.75 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=6, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, mm=8, &
                                calendar=gregorianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-.75) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Gregorian Calendar Interval ratio test 4"
      write(failMsg, *) " Did not return 1.5 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, &
                                calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, mm=24, &
                                calendar=gregorianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-1.5) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      ! TimeintervalGet(*_r8) floating point tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Suggested by Erik Kluzek/CCSM in Support Request #1518335
      write(name, *) "Get floating point days"
      write(failMsg, *) " Did not return 722502.253518553"

      ! 1978 2/22 time 06:05:04.003002001 Z
      call ESMF_TimeSet(time1, yy=1978, mm=2, dd=22, h=6, m=5, s=4, &
                               ms=3, us=2, ns=1, &
                               calendar=gregorianCalendar, rc=rc)
      ! 0000 1/01 time 00:00:00 Z
      call ESMF_TimeSet(time2, yy=0, mm=1, dd=1, &
                               calendar=gregorianCalendar, rc=rc)
      diffTime = time1 - time2
      call ESMF_TimeIntervalGet(diffTime, d_r8=days_r8, rc=rc)
      call ESMF_Test((abs(days_r8 - 722502.253518553d0) < 1d-6 .and. &
                     rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "Diff time in floating point days = ", days_r8, " rc = ", rc

      ! ----------------------------------------------------------------------------
      ! Julian Leap year 1900 tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/29/1900 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing TimeInterval Validation
      write(name, *) "Time Interval Validate Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalValidate(timeStep, rc=rc)
      call ESMF_Test(( rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/29/1900 by mm= -1 Test"
      write(failMsg, *) " Did not return 12/29/1899 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1899 .and. MM==12 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Julian Calendar Interval decrement 3/31/1900 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=3, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Julian Calendar Interval decrement 1/31/1900 by mm= -1 Test"
      write(failMsg, *) " Did not return 2/29/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/30/1900 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/1900 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/1900 by (mm=1, d=1) Test"
      write(failMsg, *) " Did not return 3/1/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/1900 by mm=2 Test"
      write(failMsg, *) " Did not return 3/31/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==3 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/1900 by mm=3 Test"
      write(failMsg, *) " Did not return 4/30/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==4 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/1900 by mm=49 Test"
      write(failMsg, *) " Did not return 2/29/1904 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1904 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time1, timestep)
      write(name, *) "Julian Calendar Interval decrement 1/31/1900 by mm=49 Test"
      write(failMsg, *) " Did not return 12/31/1895 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1895 .and. MM==12 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 2/28/1900 by d=2 Test"
      write(failMsg, *) " Did not return 3/1/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 2/29/1900 by yy=1 Test"
      write(failMsg, *) " Did not return 2/28/1901 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=2, dd=29, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1901 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 12/1/1900 by (yy=2, mm=24, d=90) Test"
      write(failMsg, *) " Did not return 3/1/1905 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1905 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 12/1/1900 by (yy= -2, mm=24, d= -90) Test"
      write(failMsg, *) " Did not return 9/2/1900 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=-2, mm=24, d=-90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1900 .and. MM==9 .and. DD==2 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Julian Calendar Interval decrement 3/1/1900 by (yy=2, mm=24, d=90, h=13, m=62, s=68) Test"
      write(failMsg, *) " Did not return 12/1/1895 22:14:50 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=3, dd=1, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, h=13, m=62, s=68, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1895 .and. MM==12 .and. DD==1 .and. &
                      H==22 .and. M==14 .and. S==50 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Julian Calendar Interval decrement 3/1/1900 by (yy=2, mm=-24, d=90, h=-13, m=62, s=-68) Test"
      write(failMsg, *) " Did not return 12/3/1899 00:17:06 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=3, dd=1, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=-24, d=90, h=-13, m=62, s=-68, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1899 .and. MM==12 .and. DD==3 .and. &
                      H==0 .and. M==17 .and. S==6 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (1 year to days) Test"
      write(failMsg, *) " Did not return 366 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==366.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (2 years to months) Test"
      write(failMsg, *) " Did not return 24 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=2, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, &
                                calendarIn=julianCalendar, rc=rc)
      call ESMF_Test((months==24.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (months to years) Test"
      write(failMsg, *) " Did not return 3 years and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=36, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, &
                                calendarIn=julianCalendar, rc=rc)
      call ESMF_Test((years==3.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years = ", years

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (2 months to days) Test"
      write(failMsg, *) " Did not return 60 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=2, dd=1, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==60.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (days to months) Test"
      write(failMsg, *) " Did not return 2 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=1, dd=1, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=60, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, startTimeIn=startTime, &
                                timeString=timeString, rc=rc)
      call ESMF_Test((months==2 .and. timeString=="P0Y0M60DT0H0M0S" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months
      print *, "timeStep = ", timeString

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (730 days to years and days) Test"
      write(failMsg, *) " Did not return years=1, days=364 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1899, mm=3, dd=31, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=730, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==1 .and. days==364 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (731 days to years and days) Test"
      write(failMsg, *) " Did not return years=2, days=0 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2002, mm=3, dd=31, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=731, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==2 .and. days==0 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      ! Julian Non-leap year 2003 tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/29/2003 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/30/2003 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/2003 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/2003 by (mm=1, d=1) Test"
      write(failMsg, *) " Did not return 3/1/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeIntervalOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/2003 by mm=2 Test"
      write(failMsg, *) " Did not return 3/31/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==3 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/2003 by mm=3 Test"
      write(failMsg, *) " Did not return 4/30/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==4 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 1/31/2003 by mm=49 Test"
      write(failMsg, *) " Did not return 2/28/2007 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2007 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 2/28/2003 by d=2 Test"
      write(failMsg, *) " Did not return 3/2/2003 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2003 .and. MM==3 .and. DD==2 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 2/28/2003 by yy=1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 12/1/2003 by (yy=2, mm=24, d=90) Test"
      write(failMsg, *) " Did not return 2/29/2008 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2008 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (1 year to days) Test"
      write(failMsg, *) " Did not return 365 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==365.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (2 years to months) Test"
      write(failMsg, *) " Did not return 24 months or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=2, calendar=julianCalendar, &
                                rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, rc=rc)
      call ESMF_Test((months==24 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (months to years) Test"
      write(failMsg, *) " Did not return 4 years or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=48, calendar=julianCalendar, &
                                rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, rc=rc)
      call ESMF_Test((years==4 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years = ", years

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (2 months to days) Test"
      write(failMsg, *) " Did not return 59 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=1, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, startTimeIn=startTime, rc=rc)
      call ESMF_Test((days==59.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (days to months) Test"
      write(failMsg, *) " Did not return 2 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2003, mm=2, dd=1, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=59, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, mm=months, startTimeIn=startTime, &
                                rc=rc)
      call ESMF_Test((months==2.and.rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months = ", months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (730 days to years and days) Test"
      write(failMsg, *) " Did not return years=2, days=0 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2005, mm=3, dd=31, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=730, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==2 .and. days==0 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval conversion (731 days to years and days) Test"
      write(failMsg, *) " Did not return years=2, days=1 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2001, mm=1, dd=31, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=731, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, &
                                startTimeIn=startTime, rc=rc)
      call ESMF_Test((years==2 .and. days==1 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, days = ", years, days

      ! ----------------------------------------------------------------------------
      ! General Julian Calendar tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Calendar Interval increment 12/31/1900 by d=1 Test"
      write(failMsg, *) " Did not return 1/1/1901 12:17:58 and ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=1900, mm=12, dd=31, h=12, m=17, s=58, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1901 .and. MM==1 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the == operator
      ! resultTime = ESMF_TimeOperator(==)(timestep, timestep)
      write(name, *) "Julian Calendar Interval == test"
      write(failMsg, *) " Did not return true"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=4, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=2, mm=16, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_Test((timeStep == timeStep2), name, failMsg, result, &
                      ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval Abs test"
      write(failMsg, *) " Did not return mm=32, d=10 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=-3, mm=4, d=-10, &
                                calendar=julianCalendar, rc=rc)
      timeStep2 = ESMF_TimeIntervalAbsValue(timeStep)
      call ESMF_TimeIntervalGet(timeStep2, mm=months, d=days, rc=rc)
      call ESMF_Test((months==32 .and. days==10 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months, days, rc = ", months, days, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval Negative Abs test"
      write(failMsg, *) " Did not return mm=-32, d=-10 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=-4, d=10, &
                                calendar=julianCalendar, rc=rc)
      timeStep2 = ESMF_TimeIntervalNegAbsValue(timeStep)
      call ESMF_TimeIntervalGet(timeStep2, mm=months, d=days, rc=rc)
      call ESMF_Test((months==-32 .and. days==-10 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months, days, rc = ", months, days, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the MOD operator
      ! timestep = ESMF_TimeIntervalFunction(MOD)(timestep, timestep)
      write(name, *) "Julian Calendar Interval Modulus test 1"
      write(failMsg, *) " Did not return mm=10 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=4, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=2, mm=6, &
                                calendar=julianCalendar, rc=rc)
      timeStep3 = MOD(timeStep, timeStep2)
      call ESMF_TimeIntervalGet(timeStep3, mm=months, rc=rc)
      call ESMF_Test((months==10 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "months, rc = ", months, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the MOD operator
      ! timestep = ESMF_TimeIntervalFunction(MOD)(timestep, timestep)
      write(name, *) "Julian Calendar Interval Modulus test 2"
      write(failMsg, *) " Did not return yy=2 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=5, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=3, &
                                calendar=julianCalendar, rc=rc)
      timeStep3 = MOD(timeStep, timeStep2)
      call ESMF_TimeIntervalGet(timeStep3, yy=years, rc=rc)
      call ESMF_Test((years==2 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "years, rc = ", years, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the MOD operator
      ! timestep = ESMF_TimeIntervalFunction(MOD)(timestep, timestep)
      write(name, *) "Julian Calendar Interval Modulus test 3"
      write(failMsg, *) " Did not return d=7 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=15, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, d=8, &
                                calendar=julianCalendar, rc=rc)
      timeStep3 = MOD(timeStep, timeStep2)
      call ESMF_TimeIntervalGet(timeStep3, d=days, rc=rc)
      call ESMF_Test((days==7 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days, rc = ", days, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval ratio test 1"
      write(failMsg, *) " Did not return 0.5 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=5, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, d=10, &
                                calendar=julianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-.5) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval ratio test 2"
      write(failMsg, *) " Did not return 0.25 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=12, &
                                calendar=julianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-.25) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval ratio test 3"
      write(failMsg, *) " Did not return 0.75 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, mm=6, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, mm=8, &
                                calendar=julianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-.75) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Calendar Interval ratio test 4"
      write(failMsg, *) " Did not return 1.5 and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, &
                                calendar=julianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, mm=24, &
                                calendar=julianCalendar, rc=rc)
      ratio = timeStep / timeStep2
      call ESMF_Test((abs(ratio-1.5) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------
      ! No Leap calendar 2004 tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/29/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 3/31/2004 by mm= -1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "No Leap Calendar Interval decrement 3/31/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "No Leap Calendar Interval decrement 1/31/2004 by mm= -1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/30/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/31/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/28/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/31/2004 by (mm=1, d=1) Test"
      write(failMsg, *) " Did not return 3/1/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/31/2004 by mm=2 Test"
      write(failMsg, *) " Did not return 3/31/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/31/2004 by mm=3 Test"
      write(failMsg, *) " Did not return 4/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==4 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 1/31/2004 by mm=49 Test"
      write(failMsg, *) " Did not return 2/28/2008 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2008 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "No Leap Calendar Interval decrement 1/31/2004 by mm=49 Test"
      write(failMsg, *) " Did not return 12/31/1999 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=31, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1999 .and. MM==12 .and. DD==31 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 2/28/2004 by d=2 Test"
      write(failMsg, *) " Did not return 3/2/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==2 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 2/28/2004 by yy=1 Test"
      write(failMsg, *) " Did not return 2/28/2005 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2005 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "No Leap Calendar Interval increment 12/1/2004 by (yy=2, mm=24, d=90) Test"
      write(failMsg, *) " Did not return 3/1/2009 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2009 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "No Leap Calendar Interval decrement 3/1/2004 by (yy=2, mm=24, d=90, h=13, m=62, s=68) Test"
      write(failMsg, *) " Did not return 11/30/1999 22:14:50 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=1, h=12, m=17, s=58, &
                                   calendar=noLeapCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, h=13, m=62, s=68, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1999 .and. MM==11 .and. DD==30 .and. &
                      H==22 .and. M==14 .and. S==50 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "No Leap Calendar Interval conversion (1 year to days) Test"
      write(failMsg, *) " Did not return 365 days or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, &
                                calendarIn=noLeapCalendar, rc=rc)
      call ESMF_Test((days==365 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      ! 360 Day calendar 2004 tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 1/29/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/29/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 3/30/2004 by mm= -1 Test"
      write(failMsg, *) " Did not return 2/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "360 Day Calendar Interval decrement 3/30/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "360 Day Calendar Interval decrement 1/30/2004 by mm= -1 Test"
      write(failMsg, *) " Did not return 2/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=-1, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 1/30/2004 by mm=1 Test"
      write(failMsg, *) " Did not return 2/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 1/30/2004 by (mm=1, d=1) Test"
      write(failMsg, *) " Did not return 3/1/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=1, d=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 1/30/2004 by mm=2 Test"
      write(failMsg, *) " Did not return 3/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==3 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 1/30/2004 by mm=3 Test"
      write(failMsg, *) " Did not return 4/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=3, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==4 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 1/30/2004 by mm=49 Test"
      write(failMsg, *) " Did not return 2/30/2008 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2008 .and. MM==2 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "360 Day Calendar Interval decrement 1/30/2004 by mm=49 Test"
      write(failMsg, *) " Did not return 12/30/1999 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=30, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, mm=49, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1999 .and. MM==12 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 2/28/2004 by d=2 Test"
      write(failMsg, *) " Did not return 2/30/2004 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==2 .and. DD==30 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 2/28/2004 by yy=1 Test"
      write(failMsg, *) " Did not return 2/28/2005 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=2, dd=28, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2005 .and. MM==2 .and. DD==28 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "360 Day Calendar Interval increment 12/1/2004 by (yy=2, mm=24, d=90) Test"
      write(failMsg, *) " Did not return 3/1/2009 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=12, dd=1, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==2009 .and. MM==3 .and. DD==1 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "360 Day Calendar Interval decrement 3/1/2004 by (yy=2, mm=24, d=90, h=13, m=62, s=68) Test"
      write(failMsg, *) " Did not return 11/30/1999 22:14:50 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=3, dd=1, h=12, m=17, s=58, &
                                   calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=24, d=90, h=13, m=62, s=68, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1999 .and. MM==11 .and. DD==30 .and. &
                      H==22 .and. M==14 .and. S==50 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, MM, "/", DD, "/", YY, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "360 Day Calendar Interval conversion (1 year to days) Test"
      write(failMsg, *) " Did not return 360 days or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, &
                                calendarIn=day360Calendar, rc=rc)
      call ESMF_Test((days==360 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the == operator
      ! resultTime = ESMF_TimeOperator(==)(timestep, timestep)
      write(name, *) "360-Day Calendar Interval == test"
      write(failMsg, *) " Did not return true"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=4, d=730, &
                                calendar=day360Calendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=4, mm=16, d=10, &
                                calendar=day360Calendar, rc=rc)
      call ESMF_Test((timeStep == timeStep2), name, failMsg, result, &
                      ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Julian Day calendar interval tests
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Day Calendar Interval increment d=1200 by d=34 Test"
      write(failMsg, *) " Did not return d=1234 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, d=1200, h=12, m=17, s=58, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=34, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, d=days, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((days==1234 .and. H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Julian Day Calendar Interval decrement d=4350 by d=29 Test"
      write(failMsg, *) " Did not return d=4321 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, d=4350, h=12, m=17, s=58, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=29, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, d=days, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((days==4321 .and. H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeOperator(+)(time, timestep)
      write(name, *) "Julian Day Calendar Interval increment d=4350 by d=-29 Test"
      write(failMsg, *) " Did not return d=4350 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, d=4350, h=12, m=17, s=58, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=-29, rc=rc)
      startTime = startTime + timeStep
      call ESMF_TimeGet(startTime, d=days, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((days==4321 .and. H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeOperator(-)(time, timestep)
      write(name, *) "Julian Day Calendar Interval decrement d=1200 by d=-34 Test"
      write(failMsg, *) " Did not return d=1234 12:17:58 or ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, d=1200, h=12, m=17, s=58, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeIntervalSet(timeStep, d=-34, rc=rc)
      startTime = startTime - timeStep
      call ESMF_TimeGet(startTime, d=days, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((days==1234 .and. H==12 .and. M==17 .and. S==58 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days, " ", H, ":", M, ":", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Day Calendar Interval conversion (days to hours) Test"
      write(failMsg, *) " Did not return 96 hours or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, d=4, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, h=hours, &
                                calendarIn=julianDayCalendar, rc=rc)
      call ESMF_Test((hours==96 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "hours = ", hours

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Julian Day Calendar Interval conversion (minutes to days) Test"
      write(failMsg, *) " Did not return 2 days or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, m=2880, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, &
                                calendarIn=julianDayCalendar, rc=rc)
      call ESMF_Test((days==2 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "days = ", days

      ! ----------------------------------------------------------------------------
      ! Calendar Interval tests where days=86400 seconds
      ! ----------------------------------------------------------------------------

      ! ----------------------------------------------------------------------------

      ! can test with any of these types
      call ESMF_CalendarSetDefault(ESMF_CAL_JULIANDAY, rc)
      !call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, rc)
      !call ESMF_CalendarSetDefault(ESMF_CAL_NOLEAP, rc)
      !call ESMF_CalendarSetDefault(ESMF_CAL_360DAY, rc)

      !EX_UTest
      write(name, *) "Day Calendar Time Interval conversion with s=172800 (2 days) Test"
      write(failMsg, *) "Days should = 2 and return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, s=172800, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, s=secs, rc=rc)
      call ESMF_Test((days==2 .and. secs==0 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Day Calendar Time Interval conversion with d=1, s=172860 (3 days, 1 minute) Test"
      write(failMsg, *) "Days should = 3, secs = 60 and return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=1, s=172860, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, s=secs, rc=rc)
      call ESMF_Test((days==3 .and. secs==60 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Day Calendar Time Interval conversion with d=1, h=48, m=2880, s=86401 (6 days, 1 second) Test"
      write(failMsg, *) "Days should = 6, secs = 1 and return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=1, h=48, m=2880, s=86401, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, d=days, s=secs, rc=rc)
      call ESMF_Test((days==6 .and. secs==1 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Day Calendar Time Interval conversion with d=6, m=1, s=1 Test"
      write(failMsg, *) "Hours should = 144, secs = 61 and return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=6, m=1, s=1, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, h=H, s=secs, rc=rc)
      call ESMF_Test((H==144 .and. secs==61 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " hours = ", H
      print *, " secs = ", secs

      ! ----------------------------------------------------------------------------
      call ESMF_CalendarSetDefault(ESMF_CAL_NOCALENDAR, rc)

      !EX_UTest
      write(name, *) "No Calendar Time Interval conversion with yy=2, mm=30, d=720 Test"
      write(failMsg, *) "yy, mm, d should = 2,30,720 and return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=30, d=720, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, mm=months, d=days, rc=rc)
      call ESMF_Test((years==2 .and. months==30 .and. days==720 &
                      .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " years, months, days = ", years, months, days

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "No Calendar Time Interval conversion with yy=2, mm=30, d=720 Test"
      write(failMsg, *) "should return ESMF_FAILURE."
      call ESMF_TimeIntervalSet(timeStep, yy=2, mm=30, d=720, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, d=days, rc=rc)
      call ESMF_Test((rc/=ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " rc = ", rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "No Calendar Time Interval conversion with yy=1, mm=48 Test"
      write(failMsg, *) "yy, mm should = 1,48 and return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=1, mm=48, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, mm=months, rc=rc)
      call ESMF_Test((years==1 .and. months==48 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " years, months = ", years, months

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "No Calendar Time Interval conversion with yy=1, mm=48 Test"
      write(failMsg, *) "should return ESMF_FAILURE."
      call ESMF_TimeIntervalSet(timeStep, yy=1, mm=48, rc=rc)
      call ESMF_TimeIntervalGet(timeStep, yy=years, rc=rc)
      call ESMF_Test((rc/=ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "rc = ", rc

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the + operator
      ! resultTime = ESMF_TimeIntervalOperator(+)(timestep, timestep)
      write(name, *) "No Calendar sum of 2 Time intervals Test 1"
      write(failMsg, *) " Did not return (yy=5, mm=-19, d=120, h=-4, m=-50, s=-50) or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=5, d=30, h=7, m=8, s=18, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=2, mm=-24, d=90, h=-13, m=62, s=-68, rc=rc)
      timeStep3 = timeStep + timeStep2
      call ESMF_TimeIntervalGet(timeStep3, yy=YY, mm=MM, d=D, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==5 .and. MM==-19 .and. D==120 .and. &
                      H==-4 .and. M==-50 .and. S==-50 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "yy=", YY, "mm=", MM, "d=", D, "h=", H, "m=", M, "s=", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the - operator
      ! resultTime = ESMF_TimeIntervalOperator(-)(timestep, timestep)
      write(name, *) "No Calendar difference of 2 Time intervals Test 1"
      write(failMsg, *) " Did not return (yy=1, mm=29, d=-60, h=-4, m=-50, s=-50) or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=5, d=30, h=7, m=8, s=18, rc=rc)
      call ESMF_TimeIntervalSet(timeStep2, yy=2, mm=-24, d=90, h=-13, m=62, s=-68, rc=rc)
      timeStep3 = timeStep - timeStep2
      call ESMF_TimeIntervalGet(timeStep3, yy=YY, mm=MM, d=D, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==1 .and. MM==29 .and. D==-60 .and. &
                      H==19 .and. M==7 .and. S==26 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "yy=", YY, "mm=", MM, "d=", D, "h=", H, "m=", M, "s=", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the x operator
      ! resultTime = ESMF_TimeIntervalOperator(x)(timestep, integer)
      write(name, *) "No Calendar multiplication of a Time interval Test 1"
      write(failMsg, *) " Did not return (yy=9, mm=-15, d=90, h=20, m=37, s=15) or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=-5, d=30, h=7, m=-8, s=25, rc=rc)
      timeStep2 = timeStep * 3
      call ESMF_TimeIntervalGet(timeStep2, yy=YY, mm=MM, d=D, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==9 .and. MM==-15 .and. D==90 .and. &
                      H==20 .and. M==37 .and. S==15 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "yy=", YY, "mm=", MM, "d=", D, "h=", H, "m=", M, "s=", S

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the x operator
      ! resultTime = ESMF_TimeIntervalOperator(x)(integer, timestep)
      write(name, *) "No Calendar multiplication of a Time interval Test 2"
      write(failMsg, *) " Did not return (yy=9, mm=-15, d=90, h=20, m=37, s=15) or ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, yy=3, mm=-5, d=30, h=7, m=-8, s=25, rc=rc)
      timeStep2 = 3 * timeStep
      call ESMF_TimeIntervalGet(timeStep2, yy=YY, mm=MM, d=D, h=H, m=M, s=S, rc=rc)
      call ESMF_Test((YY==9 .and. MM==-15 .and. D==90 .and. &
                      H==20 .and. M==37 .and. S==15 .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "yy=", YY, "mm=", MM, "d=", D, "h=", H, "m=", M, "s=", S

      call ESMF_CalendarDestroy(julianDayCalendar)
      call ESMF_CalendarDestroy(day360Calendar)
      call ESMF_CalendarDestroy(noLeapCalendar)
      call ESMF_CalendarDestroy(julianCalendar)
      call ESMF_CalendarDestroy(gregorianCalendar)

      ! ----------------------------------------------------------------------------
      ! End Calendar Interval tests
      ! ----------------------------------------------------------------------------

      ! initialize clock time intervals and instants

      !EX_UTest
      ! Test Setting Time Step
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test getting the timestep
      write(name, *) "Get Time Interval Test"
      call ESMF_TimeIntervalGet(timeStep, h=H, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or timeStep not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(H.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Test getting the timestep
      write(name, *) "Get Time Interval Test"
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or timeStep not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.3600), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=70000, rc=rc)
      write(name, *) "Time Step initialization with days = 70000  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Days should = 70000."
      write(name, *) "Get Time Step in days Test"
      call ESMF_TimeIntervalGet(timeStep, d=days, rc=rc)
      call ESMF_Test((days.eq.70000), &
                      name, failMsg, result, ESMF_SRCLINE)


      print *, " Days = ", days

      ! ----------------------------------------------------------------------------
      call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, rc)
      !call ESMF_CalendarSetDefault(ESMF_CAL_JULIANDAY, rc)
      !call ESMF_CalendarSetDefault(ESMF_CAL_NOLEAP, rc)
      !call ESMF_CalendarSetDefault(ESMF_CAL_360DAY, rc)

      !EX_UTest

      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
      write(name, *) "Time Step initialization with d = 1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Seconds should = 86400"
      write(name, *) "Get Time Step in seconds Test 1"
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      call ESMF_Test((secs.eq.86400), name, failMsg, result, ESMF_SRCLINE)

      print *, " Seconds = ", secs

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Seconds should = -86400 and ESMF_SUCCESS"
      write(name, *) "Negate Time Step Test 5"
      timeStep = -timeStep
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      call ESMF_Test((secs.eq.-86400.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " Seconds = ", secs

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Hours should = 24 and ESMF_SUCCESS"
      write(name, *) "Negate Time Step Test 6"
      timeStep = -timeStep
      call ESMF_TimeIntervalGet(timeStep, h=hours, rc=rc)
      call ESMF_Test((hours.eq.24.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " Hours = ", hours

      ! ----------------------------------------------------------------------------
      call ESMF_CalendarSetDefault(ESMF_CAL_NOCALENDAR, rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, h=65, rc=rc)
      write(name, *) "Time Step initialization with hr = 65  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Seconds should = 234000."
      write(name, *) "Get Time Step in seconds Test 2"
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      call ESMF_Test((secs.eq.234000), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " Seconds = ", secs

      ! ----------------------------------------------------------------------------



      !EX_UTest
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_TimeIntervalGet(timeStep2, s=secs, rc=rc)
      write(name, *) "Time Step Get of no calendar Time Interval Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, " Seconds = ", secs

     ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=100000000, rc=rc)
      write(name, *) "Time Step initialization with days = 100000000  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Days should = 100000000."
      write(name, *) "Get Time Step in days Test"
      call ESMF_TimeIntervalGet(timeStep, d=days, rc=rc)
      call ESMF_Test((days.eq.100000000), &
                      name, failMsg, result, ESMF_SRCLINE)


      print *, " Days = ", days

     ! ----------------------------------------------------------------------------

      !EX_UTest
      days2=70000
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d_i8=days2, rc=rc)
      write(name, *) "Time Step initialization with days = 70000  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Days should = 70000."
      write(name, *) "Get Time Step in days Test"
      call ESMF_TimeIntervalGet(timeStep, d_i8=days2, rc=rc)
      call ESMF_Test((days2.eq.70000), &
                      name, failMsg, result, ESMF_SRCLINE)


      print *, " Days = ", days2

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalNegAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Neg. Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=100, rc=rc)
      write(name, *) "Time Step initialization with days = 100  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, d=50, rc=rc)
      write(name, *) "Time Step initialization with days = 50  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the / operator
      ! resultTime = ESMF_TimeIntervalOperator(/)(timestep, timestep)
      write(failMsg, *) "The answer should be 2."
      ans=timeStep/timeStep2
      write(name, *) "Time Step division Test"
      call ESMF_Test((ans.eq.2), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, " 100 / 50 = ", ans

      ! ----------------------------------------------------------------------------

      !write(failMsg, *) "The answer should be 5000."
      !timeStep3=timeStep*timeStep2
      !write(name, *) "Time Step multiplication Test"
      !call ESMF_TimeIntervalGet(timeStep3, d=days, rc=rc)
      !call ESMF_Test((days.eq.5000), &
                      !name, failMsg, result, ESMF_SRCLINE)


      !print *, " Days = ", days2

      ! ----------------------------------------------------------------------------

      !write(failMsg, *) "The answer should be 150."
      !timeStep3=timeStep*timeStep2
      !write(name, *) "Time Step addition Test"
      !call ESMF_TimeIntervalGet(timeStep3, d=days, rc=rc)
      !call ESMF_Test((days.eq.150), &
                      !name, failMsg, result, ESMF_SRCLINE)


      !print *, " Days = ", days

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /= operator
      ! resultTime = ESMF_TimeIntervalOperator(/=)(timestep, timestep)
      write(failMsg, *) "The time steps are not equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, s=1, rc=rc)
      write(name, *) "Time Step initialization with seconds = 1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, s=0, rc=rc)
      write(name, *) "Time Step initialization with seconds = 0  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /= operator
      ! resultTime = ESMF_TimeIntervalOperator(/=)(timestep, timestep)
      write(failMsg, *) "The time steps are not equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep2)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, s=-1, rc=rc)
      write(name, *) "Time Step initialization with seconds = -1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalNegAbsValue(timeStep)
      !call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Neg. Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /= operator
      ! resultTime = ESMF_TimeIntervalOperator(/=)(timestep, Timestep)
      write(failMsg, *) "The time steps are not equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, s=-1, rc=rc)
      write(name, *) "Time Step initialization with seconds = -1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the == operator
      ! resultTime = ESMF_TimeIntervalOperator(==)(timestep, timestep)
      write(failMsg, *) "The time steps are equal."
      write(name, *) "TimeInterval EQ operator Test"
      call ESMF_Test((timeStep.eq.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      !call ESMF_TimeIntervalPrint(timeStep2, rc=rc)
      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, rc=rc)
      write(name, *) "Time Step initialization with nothing set Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !call ESMF_TimeIntervalPrint(timeStep2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /= operator
      ! resultTime = ESMF_TimeIntervalOperator(/=)(timestep, timestep)
      write(failMsg, *) "The time steps are equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, s=-1, rc=rc)
      write(name, *) "Time Step initialization with seconds = -1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the * operator
      ! resultTime = ESMF_TimeIntervalOperator(x)(timestep, integer)
      write(failMsg, *) "The time steps is not correct."
      write(name, *) "TimeInterval * operator Test"
      timeStep = timeStep2 * 86400    
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      call ESMF_Test((secs.eq.-86400), &
                      name, failMsg, result, ESMF_SRCLINE)

	print *, "secs =", secs

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, s=-1, rc=rc)
      write(name, *) "Time Step initialization with seconds = -1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, s=1, rc=rc)
      write(name, *) "Time Step initialization with seconds = 1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, s=1, rc=rc)
      write(name, *) "Time Step initialization with seconds = 1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep3, s=1, rc=rc)
      write(name, *) "Time Step initialization with seconds = 1  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the <=  operator
      ! resultTime = ESMF_TimeIntervalOperator(<=)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval <= operator Test"
      bool = timeStep2 <= timeStep    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the <=  operator
      ! resultTime = ESMF_TimeIntervalOperator(<=)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval <= operator Test"
      bool = timeStep <= timeStep3    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the <=  operator
      ! resultTime = ESMF_TimeIntervalOperator(<=)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval <= operator Test"
      bool = timeStep <= timeStep2    
      print *
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the <  operator
      ! resultTime = ESMF_TimeIntervalOperator(<)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval < operator Test"
      bool = timeStep2 < timeStep    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the <  operator
      ! resultTime = ESMF_TimeIntervalOperator(<)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval < operator Test"
      bool = timeStep3 < timeStep    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the <  operator
      ! resultTime = ESMF_TimeIntervalOperator(<)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval < operator Test"
      bool = timeStep < timeStep2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the >=  operator
      ! resultTime = ESMF_TimeIntervalOperator(>=)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval >= operator Test"
      bool = timeStep2 >= timeStep    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the >=  operator
      ! resultTime = ESMF_TimeIntervalOperator(>=)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval >= operator Test"
      bool = timeStep3 >= timeStep    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the >=  operator
      ! resultTime = ESMF_TimeIntervalOperator(>=)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval >= operator Test"
      bool = timeStep >= timeStep2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the >  operator
      ! resultTime = ESMF_TimeIntervalOperator(>)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval > operator Test"
      bool = timeStep2 > timeStep    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the >  operator
      ! resultTime = ESMF_TimeIntervalOperator(>)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval > operator Test"
      bool = timeStep3 > timeStep    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing the >  operator
      ! resultTime = ESMF_TimeIntervalOperator(>)(timestep, timestep)
      write(failMsg, *) "The result is not correct."
      write(name, *) "TimeInterval > operator Test"
      bool = timeStep > timeStep2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Fractional time interval tests
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Set millisecond Time Interval Initialization Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, ms=5, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Set nanosecond Time Interval Initialization Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval2, ns=70, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Set rational fraction Time Interval Initialization Test 1"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval4, sN=1, sD=2, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Set rational fraction Time Interval Initialization Test 2"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval5, sN=-3, sD=4, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Sum of two fractional Time Intervals Test 1"
      write(failMsg, *) " Did not return 5,000,070 nanoseconds and ESMF_SUCCESS"
      timeInterval3 = timeInterval1 + timeInterval2
      call ESMF_TimeIntervalGet(timeInterval3, ns=ns, rc=rc)
      call ESMF_Test((ns.eq.5000070.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Difference of two fractional Time Intervals Test 1"
      write(failMsg, *) " Did not return -4,999,930 nanoseconds and ESMF_SUCCESS"
      timeInterval3 = timeInterval2 - timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, ns=ns, rc=rc)
      call ESMF_Test((ns.eq.-4999930.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Incrementing a fractional Time Interval with another Test"
      write(failMsg, *) " Did not return 70 nanoseconds and ESMF_SUCCESS"
      timeInterval3 = timeInterval3 + timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, ns=ns, rc=rc)
      call ESMF_Test((ns.eq.70.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Sum of two fractional Time Intervals Test 2"
      write(failMsg, *) " Did not return -1/4 seconds and ESMF_SUCCESS"
      timeInterval6 = timeInterval4 + timeInterval5
      call ESMF_TimeIntervalGet(timeInterval6, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((sN.eq.-1.and.sD.eq.4.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Difference of two fractional Time Intervals Test 2"
      write(failMsg, *) " Did not return 1 1/4 seconds and ESMF_SUCCESS"
      timeInterval6 = timeInterval4 - timeInterval5
      call ESMF_TimeIntervalGet(timeInterval6, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.1.and.sN.eq.1.and.sD.eq.4.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval3 == timeInterval2 Test"
      write(failMsg, *) " Did not return (timeInterval3 == timeInterval2)"
      bool = timeInterval3 == timeInterval2
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval1 /= timeInterval2 Test"
      write(failMsg, *) " Did not return (timeInterval1 /= timeInterval2)"
      bool = timeInterval1 /= timeInterval2
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval1 > timeInterval2 Test"
      write(failMsg, *) " Did not return (timeInterval1 > timeInterval2)"
      bool = timeInterval1 > timeInterval2
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval4 > timeInterval5 Test"
      write(failMsg, *) " Did not return (timeInterval4 > timeInterval5)"
      bool = timeInterval4 > timeInterval5
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval2 < timeInterval1 Test"
      write(failMsg, *) " Did not return (timeInterval2 < timeInterval1)"
      bool = timeInterval2 < timeInterval1
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval1 < timeInterval4 Test"
      write(failMsg, *) " Did not return (timeInterval1 < timeInterval4)"
      bool = timeInterval1 < timeInterval4
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval1 >= timeInterval2 Test"
      write(failMsg, *) " Did not return (timeInterval1 >= timeInterval2)"
      bool = timeInterval1 >= timeInterval2
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Fractional timeInterval3 <= timeInterval2 Test"
      write(failMsg, *) " Did not return (timeInterval3 <= timeInterval2)"
      bool = timeInterval3 <= timeInterval2
      call ESMF_Test(bool, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Divide rational fraction Time Interval by an integer Test 1"
      write(failMsg, *) " Did not return 5 11/14 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=11, sN=4, sD=7, rc=rc)
      timeInterval2 = timeInterval1 / 2
      call ESMF_TimeIntervalGet(timeInterval2, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.5.and.sN.eq.11.and.sD.eq.14 &
           .and.rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Divide rational fraction Time Interval by an integer Test 2"
      write(failMsg, *) " Did not return 2 62/99 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=23, sN=7, sD=11, rc=rc)
      timeInterval2 = timeInterval1 / 9
      call ESMF_TimeIntervalGet(timeInterval2, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.2.and.sN.eq.62.and.sD.eq.99 &
           .and.rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Real ratio of two fractional Time Intervals Test 1"
      write(failMsg, *) " Did not return 2.933333333 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=10, sN=2, sD=3, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval2, s=3, sN=7, sD=11, rc=rc)
      ratio = timeInterval1 / timeInterval2
      call ESMF_Test((abs(ratio-2.93333333) < 1d-6 .and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "ratio, rc = ", ratio, rc

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Suggested by John Michalakes/WRF
      write(name, *) "Divide whole number Time Interval by an integer Test 1"
      write(failMsg, *) " Did not return 3 1/3 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=10, rc=rc)
      timeInterval2 = timeInterval1 / 3
      call ESMF_TimeIntervalGet(timeInterval2, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.3.and.sN.eq.1.and.sD.eq.3 &
           .and.rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Modulus of two fractional Time Intervals Test 1"
      write(failMsg, *) " Did not return 4/5 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=3, sN=1, sD=7, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval2, s=0, sN=5, sD=14, rc=rc)
      timeInterval3 = MOD(timeInterval1, timeInterval2)
      call ESMF_TimeIntervalGet(timeInterval3, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.4.and.sD.eq.5.and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "modulus result 1 = ", S, sN, "/", sD

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Modulus of two fractional Time Intervals Test 2"
      write(failMsg, *) " Did not return 5/14 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, sN=5, sD=7, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval2, s=2, rc=rc)
      timeInterval3 = MOD(timeInterval1, timeInterval2)
      call ESMF_TimeIntervalGet(timeInterval3, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.5.and.sD.eq.14.and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "modulus result 2 = ", S, sN, "/", sD

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Modulus of two fractional Time Intervals Test 3"
      write(failMsg, *) " Did not return 1/2 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=3, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval2, sN=2, sD=5, rc=rc)
      timeInterval3 = MOD(timeInterval1, timeInterval2)
      call ESMF_TimeIntervalGet(timeInterval3, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.1.and.sD.eq.2.and. rc==ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "modulus result 3 = ", S, sN, "/", sD

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Multiply rational fraction Time Interval by an integer Test 1"
      write(failMsg, *) " Did not return 30 2/5 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=7, sN=3, sD=5, rc=rc)
      timeInterval2 = timeInterval1 * 4
      call ESMF_TimeIntervalGet(timeInterval2, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.30.and.sN.eq.2.and.sD.eq.5.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Multiply rational fraction Time Interval by an integer Test 2"
      write(failMsg, *) " Did not return 3/4 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, sN=1, sD=8, rc=rc)
      timeInterval2 = timeInterval1 * 6
      call ESMF_TimeIntervalGet(timeInterval2, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.3.and.sD.eq.4.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Suggested by John Michalakes/WRF
      write(name, *) "Sum of two fractional Time Intervals Test 3"
      write(failMsg, *) " Did not return 6 1/21 seconds and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=3, sN=1, sD=3, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval2, s=2, sN=5, sD=7, rc=rc)
      timeInterval3 = timeInterval1 + timeInterval2
      call ESMF_TimeIntervalGet(timeInterval3, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.6.and.sN.eq.1.and.sD.eq.21.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Negate Time Interval Test 7"
      write(failMsg, *) " Did not return -6 -1/21 seconds and ESMF_SUCCESS"
      timeInterval3 = -timeInterval3
      call ESMF_TimeIntervalGet(timeInterval3, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.-6.and.sN.eq.-1.and.sD.eq.21.and. &
                     rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "S sN/sD = ", S, sN, "/", sD

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Suggested by John Michalakes/WRF
      write(name, *) "Sum of two fractional Time Intervals Test 4"
      write(failMsg, *) " Did not return 1/3 second and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, sN=1, sD=6, rc=rc)
      call ESMF_TimeIntervalSet(timeInterval2, sN=1, sD=6, rc=rc)
      timeInterval3 = timeInterval1 + timeInterval2
      call ESMF_TimeIntervalGet(timeInterval3, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.1.and.sD.eq.3.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Simplification of fractional Time Intervals Test 1"
      write(failMsg, *) " Did not return -1/3 second and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=2, sN=7, sD=-3, rc=rc)
      call ESMF_TimeIntervalGet(timeInterval1, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.-1.and.sD.eq.3.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Simplification of fractional Time Intervals Test 2"
      write(failMsg, *) " Did not return -1/7 second and ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeInterval1, s=-3, sN=20, sD=7, rc=rc)
      call ESMF_TimeIntervalGet(timeInterval1, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.-1.and.sD.eq.7.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(name, *) "Negate Time Interval Test 8"
      write(failMsg, *) " Did not return 1/7 seconds and ESMF_SUCCESS"
      timeInterval1 = -timeInterval1
      call ESMF_TimeIntervalGet(timeInterval1, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.1.and.sD.eq.7.and. &
                     rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "sN/sD = ", sN, "/", sD

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Should produce error message in ESMF_LogFile
      write(name, *) "Upper limit of denominator Test"
      write(failMsg, *) " Did not return 0 0/1 seconds"
      call ESMF_TimeIntervalSet(timeInterval1, sN=1, sD=1000000000, rc=rc)
      timeInterval1 = timeInterval1 / 5
      call ESMF_TimeIntervalGet(timeInterval1, s=S, sN=sN, sD=sD, rc=rc)
      call ESMF_Test((S.eq.0.and.sN.eq.0.and.sD.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
#endif
      ! finalize ESMF framework
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_TimeIntervalUTest
