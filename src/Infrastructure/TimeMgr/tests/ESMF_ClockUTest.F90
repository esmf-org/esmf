! $Id: ESMF_ClockUTest.F90,v 1.18 2003/04/30 15:56:59 svasquez Exp $
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
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockUTest.F90,v 1.18 2003/04/30 15:56:59 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD
      integer(ESMF_IKIND_I8) :: YR

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2
      type(ESMF_Time) :: startTime, stopTime, startTime2


      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism


      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!--------------------------------------------------------------------------------



      ! initialize clock time intervals and instants
      !call ESMF_TimeIntervalInit(timeStep, S=1, rc=rc)

      ! Test Setting Time Step
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalInit(timeStep, H=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Test getting the timestep
      write(name, *) "Get Time Interval Test"
      call ESMF_TimeIntervalGet(timeStep, H=H, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or timeStep not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(H.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, " H = ", H

      ! ----------------------------------------------------------------------------

      ! Test Setting the Start Time
      write(name, *) "Set Start Time Initialization Test"
      YR=2003
      call ESMF_TimeInit(startTime, YR=YR, MM=3, DD=13, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the year is set correctly
      write(name, *) "Get Start Time Year Test"
      call ESMF_TimeGet(startTime, YR=YR, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the month is set correctly
      write(name, *) "Get StartTime Month Test"
      call ESMF_TimeGet(startTime, MM=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the day is set correctly
      write(name, *) "Get StartTime Day Test"
      call ESMF_TimeGet(startTime, DD=DD, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the calendar is set correctly
      ! write(name, *) "Get StartTime calendar Test"
      ! call ESMF_TimeGet(startTime, cal=cal, rc=rc)
      ! write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(cal.eq.gregorianCalendar), &
       !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Attempt to get un-initialized year from stop time
       write(name, *) "Get Uninitialized StopTime Year Test"
       call ESMF_TimeGet(stopTime, YR=YR, rc=rc)
       write(failMsg, *) " Returned ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (0) Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      YR=2003
      call ESMF_TimeInit(stopTime, YR=YR, MM=0, DD=14, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (13) Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      YR=2003
      call ESMF_TimeInit(stopTime, YR=YR, MM=13, DD=14, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      ! Set time to illegite day
      write(name, *) "Stop Time Initialization to  Feb. 31st. Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      YR=2003
      call ESMF_TimeInit(stopTime, YR=YR, MM=2, DD=31, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------


      ! Set time to lower bound of Fliegel algoritm
      write(name, *) "Test lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_SUCCESS."
      YR=-4900
      call ESMF_TimeInit(stopTime, YR=YR, MM=3, DD=1, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Set time beyond lower bound of Fliegel algoritm
      write(name, *) "Test beyond lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      YR=-4900
      call ESMF_TimeInit(stopTime, YR=YR, MM=2, DD=28, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Set time to upper bound of Fliegel algoritm
      write(name, *) "Test upper bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_SUCCESS."
      YR=1465002
      call ESMF_TimeInit(stopTime, YR=YR, MM=10, DD=17, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Set time beyond upper bound of Fliegel algoritm
      write(name, *) "Test beyond upper bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      YR=1465002
      call ESMF_TimeInit(stopTime, YR=YR, MM=10, DD=18, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif

      ! ----------------------------------------------------------------------------
      write(name, *) "Stop Time Initialization Test"
      YR=2003
      call ESMF_TimeInit(stopTime, YR=YR, MM=3, DD=14, &
                                   cal=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the year is set correctly
      write(name, *) "Get StopTime Year Test"
      call ESMF_TimeGet(stopTime, YR=YR, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the month is set correctly
      write(name, *) "Get StopTime Month Test"
      call ESMF_TimeGet(stopTime, MM=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      write(name, *) "Clock Initialization Test with uninitialized startTime"
      write(failMsg, *) " Returned ESMF_SUCCESS"
      call ESMF_ClockInit(clock, timeStep, startTime2, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      ! ----------------------------------------------------------------------------
      write(name, *) "Clock Initialization Test"
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(name, *) "Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      write(name, *) "Time Interval Print Test"
      call ESMF_TimeIntervalPrint(timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      write(name, *) "Clock Print Test"
      call ESMF_ClockPrint(clock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      write(name, *) "ClockIsStopTime Test"
      bool = ESMF_ClockIsStopTime(clock, rc)
      !call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and.(bool.eq."F")), &
      !                name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool


      ! ----------------------------------------------------------------------------
      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        write(name, *) "Clock Advance Test"
        call ESMF_ClockAdvance(clock, rc=rc)
        call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
        call ESMF_ClockPrint(clock, rc=rc)
      end do

      bool = ESMF_ClockIsStopTime(clock1, rc)
      print *, "bool = ", bool

      ! print out ending clock state
      call ESMF_ClockPrint(clock, rc=rc)

#ifdef ESMF_EXHAUSTIVE

      ! ----------------------------------------------------------------------------
      write(name, *) "Clock Initialization with stop time set before start time Test"
      YR=2002
      call ESMF_TimeInit(stopTime, YR=YR, MM=3, DD=14, &
                                   cal=gregorianCalendar, rc=rc)
      write(failMsg, *) "Should return ESMF_SUCCESS"
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif


#if 0
      ! test initialization of members of statically allocated ESMF_Clock
      !   may want to read back values via Get methods for comparison
      call ESMF_ClockInit(clock, args, rc)
      write(name, *) "ESMF_ClockInit"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_Clock members values
      <value type> :: value_set
      call ESMF_ClockSet<Value>(clock, value_set, rc)
      write(name, *) "ESMF_ClockSet<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_Clock members values,
      !   compare to values set previously
      <value type> :: value_get
      call ESMF_ClockGet<Value>(clock, value_get, rc)
      write(name, *) "ESMF_ClockGet<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      character(ESMF_MAXSTR) :: validate_options
      call ESMF_ClockValidate(clock, validate_options, rc)
      write(name, *) "ESMF_ClockValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      character(ESMF_MAXSTR) :: print_options
      call ESMF_ClockPrint(clock, print_options, rc)
      write(name, *) "ESMF_ClockPrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_ClockTest
