! $Id: ESMF_ClockUTest.F90,v 1.38 2003/09/08 17:00:25 svasquez Exp $
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

#include <ESMF_Macros.inc>
 
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
      '$Id: ESMF_ClockUTest.F90,v 1.38 2003/09/08 17:00:25 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YR

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar
      type(ESMF_CalendarType) :: cal_type

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2
      type(ESMF_Time) :: startTime, stopTime, startTime2
      type(ESMF_Time) :: currentTime, previousTime


      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism


      ! initialize ESMF framework
      call ESMF_FrameworkInitialize(rc)

      ! initialize one calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize secand calendar to be Julian type
      call ESMF_CalendarSet(julianCalendar, ESMF_CAL_JULIANDAY, rc)

!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------



      ! initialize clock time intervals and instants
      !call ESMF_TimeIntervalSet(timeStep, s=1, rc=rc)

      !NEX_UTest
      ! Test Setting Time Step
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test getting the timestep
      write(name, *) "Get Time Interval Test"
      call ESMF_TimeIntervalGet(timeStep, h=H, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or timeStep not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(H.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, " H = ", H

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yr=2003, mm=3, dd=13, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the year is set correctly
      write(name, *) "Get Start Time Year Test"
      call ESMF_TimeGet(startTime, yr=YR, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StartTime Month Test"
      call ESMF_TimeGet(startTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the day is set correctly
      write(name, *) "Get StartTime Day Test"
      call ESMF_TimeGet(startTime, dd=DD, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the calendar is set correctly
      !write(name, *) "Get Calendar Type Test"
      !call ESMF_CalendarGet(gregorianCalendar, type=cal_type, rc=rc)
      !write(failMsg, *) " Returned ESMF_FAILURE and/or calendar not correct value"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(cal_type%calendarType.eq.ESMF_CAL_GREGORIAN), &
      !               name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE
      ! This code crashes, bug 79753 has been opened.
      ! Attempt to get un-initialized year from stop time
      ! write(name, *) "Get Uninitialized StopTime Year Test"
      ! call ESMF_TimeGet(stopTime, yr=YR, rc=rc)
      ! write(failMsg, *) " Returned ESMF_SUCCESS"
      ! call ESMF_Test((rc.eq.ESMF_FAILURE), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (0) Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yr=2003, mm=0, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (13) Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yr=2003, mm=13, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite day
      write(name, *) "Stop Time Initialization to  Feb. 31st. Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yr=2003, mm=2, dd=31, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Set time to lower bound of Fliegel algoritm
      write(name, *) "Test lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=-4800, mm=3, dd=1, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time beyond lower bound of Fliegel algoritm
      write(name, *) "Test beyond lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yr=-4800, mm=2, dd=28, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
#endif

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yr=2003, mm=3, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the year is set correctly
      write(name, *) "Get StopTime Year Test"
      call ESMF_TimeGet(stopTime, yr=YR, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StopTime Month Test"
      call ESMF_TimeGet(stopTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! This code crashes, bug report 801311 has been opened.
      ! write(name, *) "Clock Initialization Test with uninitialized startTime"
      ! write(failMsg, *) " Returned ESMF_SUCCESS"
      ! call ESMF_ClockSetup(clock, timeStep, startTime2, stopTime, rc=rc)
      ! call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      ! name, failMsg, result, ESMF_SRCLINE)
#endif

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Time Interval Print Test"
      call ESMF_TimeIntervalPrint(timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Print Test"
      call ESMF_ClockPrint(clock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE and/or bool not False"
      write(name, *) "ClockIsStopTime Test"
      bool = ESMF_ClockIsStopTime(clock, rc)
      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and.(.not.bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
        call ESMF_ClockPrint(clock, rc=rc)
      end do

      bool = ESMF_ClockIsStopTime(clock, rc)
      print *, "bool = ", bool
      write(failMsg, *) " Returned ESMF_FAILURE and/or bool not True"
      write(name, *) "Clock Advance Test"
      call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! print out ending clock state
      call ESMF_ClockPrint(clock, rc=rc)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

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
      write(name, *) "Clock Initialization with stop time set before start time Test"
      call ESMF_TimeSet(stopTime, yr=2002, mm=3, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      write(failMsg, *) "Should return ESMF_FAILURE because timestep is positive."
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 

     !EX_UTest
      write(name, *) "Clock Initialization with stop time & start time with different calendars Test" 
      call ESMF_TimeSet(startTime, yr=2000, mm=3, dd=13, &
                                   calendar=julianCalendar, rc=rc)
      write(failMsg, *) "Should return ESMF_FAILURE."
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif


      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_FrameworkFinalize(rc)

      end program ESMF_ClockTest
