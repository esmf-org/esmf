! $Id: ESMF_AlarmUTest.F90,v 1.1 2004/04/07 22:10:07 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_ClockTest

!------------------------------------------------------------------------------
!

#include <ESMF.h>
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_AlarmTest - Test Alarm functionalities
!
! !DESCRIPTION:
!
! The code in this file drives F90 Alarm unit tests.
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
      '$Id: ESMF_AlarmUTest.F90,v 1.1 2004/04/07 22:10:07 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YY, days, totalDays, secs, testResults

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1, clock_gregorian, clock_julian, &
                          clock_no_leap, clock_360day

      ! Random number
      real :: ranNum
      integer :: seed(32)
      integer :: timevals(8)

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             no_leapCalendar, esmf_360dayCalendar
      type(ESMF_CalendarType) :: cal_type

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2
      type(ESMF_Time) :: startTime, stopTime, startTime2, stopTime2
      type(ESMF_Time) :: currentTime, previousTime, syncTime, stopTime3 
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      integer(ESMF_KIND_I8) :: advanceCounts, year, day2, month, minute, second
      integer(ESMF_KIND_I4) :: day, hour


      ! initialize ESMF framework
      call ESMF_Initialize(rc)

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
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
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
  
      ! finalize ESMF framework
      call ESMF_Finalize(rc)

      end program ESMF_AlarmTest
