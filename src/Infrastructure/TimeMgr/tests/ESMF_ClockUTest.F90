! $Id: ESMF_ClockUTest.F90,v 1.2 2003/03/18 04:38:57 eschwab Exp $
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
! INCLUDES
#include <ESMF.h>
!
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
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod
      use ESMF_CalendarMod
      use ESMF_ClockMod     ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockUTest.F90,v 1.2 2003/03/18 04:38:57 eschwab Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, stopTime


      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism


      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

#ifdef ESMF_EXHAUSTIVE
      ! initialize clock time intervals and instants
      call ESMF_TimeIntervalInit(timeStep, S=1, rc=rc)
      call ESMF_TimeInit(startTime, YY=2003, MM=3, DD=13,
                         cal=gregorianCalendar, rc=rc)
      call ESMF_TimeInit(stopTime, YY=2003, MM=3, DD=14,
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)

      ! print out initialized variables
      call ESMF_BasePrint(gregorianCalendar, rc)
      call ESMF_BasePrint(timeStep, rc)
      call ESMF_BasePrint(startTime, rc)
      call ESMF_BasePrint(stopTime, rc)
      call ESMF_BasePrint(clock, rc)

      ! time step from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
        call ESMF_BasePrint(clock, rc)
      end do

      ! print out ending clock state
      call ESMF_BasePrint(clock, rc)


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

#else

      ! perform non-exhaustive tests here;
      !   use same templates as above

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_ClockTest
