! $Id: ESMF_ClockEx.F90,v 1.23 2003/09/04 18:57:56 cdeluca Exp $
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
      program ESMF_ClockEx

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_ClockEx - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows an example of how to set-up, run, and examine a basic clock
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockEx.F90,v 1.23 2003/09/04 18:57:56 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime

      ! temp variables for Get functions
      integer(ESMF_KIND_I4) :: days, sec, yD
      type(ESMF_TimeInterval) :: time_step
      type(ESMF_TimeInterval) :: time_diff
      integer(ESMF_KIND_I8) :: advanceCount
      real(ESMF_KIND_R8) :: days_r8

      ! result code
      integer :: rc

      !
      ! initialization
      !

      ! initialize ESMF framework
      call ESMF_FrameworkInitialize(rc)

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 2 days, 4 hours (6 timesteps in 13 days)
      call ESMF_TimeIntervalSet(timeStep, d=2, h=4, rc=rc)

      ! initialize start time to 4/1/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(startTime, yr=2003, mm=4, dd=1, h=2, m=24, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize stop time to 4/14/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(stopTime, yr=2003, mm=4, dd=14, h=2, m=24, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)

      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currtime string", rc)

      !
      ! time step clock from start time to stop time
      !

      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
        call ESMF_ClockPrint(clock, "currtime string", rc)
      end do

      !
      ! examine clock
      !

      ! get and print clock's time_step
      call ESMF_ClockGet(clock, timeStep=time_step, rc=rc)
      call ESMF_ClockPrint(clock, "timestep string", rc)

      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, d=days, s=sec, rc=rc)
      print *, "Clock's timestep = ", days, " integer days, ", &
                sec, " integer seconds."

      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_r8=days_r8, rc=rc)
      print *, "Clock's timestep = ", days_r8, " floating point days."

      ! get start time's floating point day of the year
      call ESMF_TimeGet(startTime, dayOfYear_r8=days_r8, rc=rc)
      print *, "Start time's floating point day of the year = ", days_r8

      ! get stop time's integer day of the year
      call ESMF_TimeGet(stopTime, dayOfYear=yD, rc=rc)
      print *, "Stop time's integer day of the year = ", yD

      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
      print *, "The clock was advanced ", advanceCount, " times."
  
      ! calculate the difference between the start and stop times
      time_diff = stopTime - startTime
      call ESMF_TimeIntervalGet(time_diff, d=days, s=sec, rc=rc)
      print *, "Difference between start and stop times = ", days, " days, ", &
                sec, " seconds."

      ! finalize ESMF framework
      call ESMF_FrameworkFinalize(rc)

      end program ESMF_ClockEx
