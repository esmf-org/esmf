! $Id: ESMF_TimeIntervalUTest.F90,v 1.4 2004/01/26 21:29:38 eschwab Exp $
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
      program ESMF_TimeIntervalUTest

!------------------------------------------------------------------------------
!

#include <ESMF_Macros.inc>
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_TimeIntervalTest - Test Time Interval initialization and time-stepping
!
! !DESCRIPTION:
!
! The code in this file drives F90 Time Interval unit tests.
! The companion file ESMF\_TimeInterval.F90 contains the definitions for the
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
      '$Id: ESMF_TimeIntervalUTest.F90,v 1.4 2004/01/26 21:29:38 eschwab Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YY, days, totalDays, secs, testResults, ans

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1, clock_gregorian, clock_julian, clock_no_leap, clock_360day

      ! Random number
      real :: ranNum
      integer :: seed(32)
      integer :: timevals(8)

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, no_leapCalendar, esmf_360dayCalendar
      type(ESMF_CalendarType) :: cal_type

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2, timeStep3
      type(ESMF_Time) :: startTime, stopTime, startTime2, stopTime2
      type(ESMF_Time) :: currentTime, previousTime, syncTime, stopTime3 
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      type(ESMF_TimeInterval) :: absoluteTime
      integer(ESMF_KIND_I8) :: advanceCounts, year, days2, month, minute, second
      integer(ESMF_KIND_I4) :: day, hour


!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------

     ! initialize ESMF framework
      call ESMF_Initialize(rc)


      ! initialize clock time intervals and instants

      !NEX_UTest
      ! Test Setting Time Step
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test getting the timestep
      write(name, *) "Get Time Interval Test"
      call ESMF_TimeIntervalGet(timeStep, h=H, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or timeStep not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(H.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! Test getting the timestep
      write(name, *) "Get Time Interval Test"
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or timeStep not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.3600), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalGet(timeStep2, s=secs, rc=rc)
      write(name, *) "Time Step Get of uninitalized Time Interval Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, " Seconds = ", secs

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
      write(name, *) "Get Time Step in seconds Test"
      call ESMF_TimeIntervalGet(timeStep, s=secs, rc=rc)
      call ESMF_Test((secs.eq.234000), &
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
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
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
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, d=70000, rc=rc)
      write(name, *) "Time Step initialization with days = 70000  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
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
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalNegAbsValue(timeStep)
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
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
      write(failMsg, *) "The time steps are not equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep2)
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, s=-1, rc=rc)
      write(name, *) "Time Step initialization with seconds = 0  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalAbsValue(timeStep)
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      absoluteTime=ESMF_TimeIntervalNegAbsValue(timeStep)
      call ESMF_TimeIntervalPrint(absoluteTime, rc=rc)
      write(name, *) "Print Neg. Absolute Time Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "The time steps are not equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, s=-1, rc=rc)
      write(name, *) "Time Step initialization with seconds = 0  Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "The time steps are equal."
      write(name, *) "TimeInterval EQ operator Test"
      call ESMF_Test((timeStep.eq.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimeIntervalPrint(timeStep2, rc=rc)
      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep2, rc=rc)
      write(name, *) "Time Step initialization with nothing set Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimeIntervalPrint(timeStep2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "The time steps are equal."
      write(name, *) "TimeInterval NE operator Test"
      call ESMF_Test((timeStep.ne.timeStep2), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
#endif


      ! ----------------------------------------------------------------------------
      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_Finalize(rc)

      end program ESMF_TimeIntervalUTest
