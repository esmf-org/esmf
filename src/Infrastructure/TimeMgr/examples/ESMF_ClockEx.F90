! $Id: ESMF_ClockEx.F90,v 1.15 2003/05/02 22:15:04 eschwab Exp $
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
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockEx.F90,v 1.15 2003/05/02 22:15:04 eschwab Exp $'
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
      double precision :: d_
      integer :: MM, DD, H, M, yD
      type(ESMF_TimeInterval) :: time_step
      type(ESMF_TimeInterval) :: time_diff
      integer(ESMF_IKIND_I8) :: advanceCount, D, S

      ! result code
      integer :: rc

      !
      ! initialization
      !

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 2 days, 4 hours (6 timesteps in 13 days)
      call ESMF_TimeIntervalInit(timeStep, D=int(2,kind=ESMF_IKIND_I8), &
                                 H=4, rc=rc)

      ! initialize start time to 4/1/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeInit(startTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=4, DD=1, H=2, M=24, cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 4/14/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeInit(stopTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=4, DD=14, H=2, M=24, cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)

      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currtime", rc)        ! default format
      call ESMF_ClockPrint(clock, "currtime string", rc) ! string format

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

      ! print entire clock state
      call ESMF_ClockPrint(clock, rc=rc)

      ! get clock's time_step
      call ESMF_ClockGetTimeStep(clock, time_step, rc)

      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, D=D, S=S, rc=rc)
      print *, "Clock's timestep = ", D, " integer days, ", &
                S, " integer seconds."

      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_=d_, rc=rc)
      print *, "Clock's timestep = ", d_, " floating point days."

      ! get start time's floating point day of the year
      call ESMF_TimeGetDayOfYear(startTime, d_, rc=rc)
      print *, "Start time's floating point day of the year = ", d_

      ! get stop time's integer day of the year
      call ESMF_TimeGetDayOfYear(stopTime, yD, rc=rc)
      print *, "Stop time's integer day of the year = ", yD

      ! get the number of times the clock was advanced
      call ESMF_ClockGetAdvanceCount(clock, advanceCount, rc)
      print *, "The clock was advanced ", advanceCount, " times."
  
      ! calculate the difference between the start and stop times
      time_diff = stopTime - startTime
      call ESMF_TimeIntervalGet(time_diff, D=D, S=S, rc=rc)
      print *, "Difference between start and stop times = ", D, " days, ", &
                S, " seconds."

      ! sync clock to wall clock
      call ESMF_ClockSyncToWallClock(clock, rc)
      print *
      print *, "Clock sync to wall clock = "
      call ESMF_ClockPrint(clock, "currtime string", rc)

      end program ESMF_ClockEx
