! $Id: ESMF_TimeIntervalUTest.F90,v 1.1 2003/10/06 23:04:01 svasquez Exp $
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
      '$Id: ESMF_TimeIntervalUTest.F90,v 1.1 2003/10/06 23:04:01 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YR, days, totalDays, secs, testResults

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
      type(ESMF_TimeInterval) :: timeStep, timeStep2
      type(ESMF_Time) :: startTime, stopTime, startTime2, stopTime2
      type(ESMF_Time) :: currentTime, previousTime, syncTime, stopTime3 
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      integer(ESMF_KIND_I8) :: advanceCounts, year, day2, month, minute, second
      integer(ESMF_KIND_I4) :: day, hour


!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------


      ! ----------------------------------------------------------------------------
      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_FrameworkFinalize(rc)

      end program ESMF_TimeIntervalUTest
