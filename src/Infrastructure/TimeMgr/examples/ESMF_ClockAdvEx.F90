! $Id: ESMF_ClockAdvEx.F90,v 1.1 2003/05/07 16:46:26 eschwab Exp $
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
! This program shows some advanced examples of clock operation, including a
! simple user-defined single-shot alarm
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockAdvEx.F90,v 1.1 2003/05/07 16:46:26 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start and stop times, alarm time
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, stopTime, refTime, alarmTime
      type(ESMF_TimeInterval) :: currSimTime, prevSimTime

      ! temp variables for Get functions
      integer :: MM, DD, H, M, yD
      type(ESMF_TimeInterval) :: time_step
      type(ESMF_TimeInterval) :: time_diff
      type(ESMF_Time) :: curr_time
      integer(ESMF_IKIND_I8) :: advanceCount, YR, D, S
      double precision :: d_
      logical alarmRinging

      ! result code
      integer :: rc

      !
      ! initialization
      !

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to -1 day
      call ESMF_TimeIntervalInit(timeStep, D=int(-1,kind=ESMF_IKIND_I8), rc=rc)

      ! initialize start time to 5/15/2003 12:00:00 noon
      call ESMF_TimeInit(startTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=5, DD=15, H=12, cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 5/15/2002 12:00:00 noon
      call ESMF_TimeInit(stopTime, YR=int(2002,kind=ESMF_IKIND_I8), &
                         MM=5, DD=15, H=12, cal=gregorianCalendar, rc=rc)

      ! initialize reference time to 1/1/2000 00:00:00 midnight
      call ESMF_TimeInit(refTime, YR=int(2000,kind=ESMF_IKIND_I8), &
                         MM=1, DD=1, cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, refTime, rc)

      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currtime", rc)        ! default format
      call ESMF_ClockPrint(clock, "currtime string", rc) ! string format

      ! initialize alarm time to July 4, 2002, noon
      call ESMF_TimeInit(alarmTime, YR=int(2002,kind=ESMF_IKIND_I8), &
                         MM=7, DD=4, H=12, cal=gregorianCalendar, rc=rc)
      alarmRinging = .false.

      !
      ! time step clock (backward -- negative time step!) from start time
      ! to stop time, checking for alarm
      !

      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
        call ESMF_ClockPrint(clock, "currtime string", rc)
        call ESMF_ClockGetCurrTime(clock, curr_time, rc)
        if (curr_time .le. alarmTime .and. .not.alarmRinging) then
          alarmRinging = .true.
          print *, "************************************"
          print *, "Alarm time (7/4/2002, noon) reached!"
          print *, "************************************"
        endif
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

      ! get clock's reference time
      call ESMF_TimeGet(refTime, YR=YR, MM=MM, DD=DD, rc=rc)
      print *, "Clock's reference time = ", YR, "/", MM, "/", DD, " midnight."

      ! get clock's current simulation time
      call ESMF_ClockGetCurrSimTime(clock, currSimTime, rc)
      call ESMF_TimeIntervalGet(currSimTime, D=D, S=S, rc=rc)
      print *, "Clock's current simulation time = ", D, " days", S, " seconds."

      ! get clock's previous simulation time
      call ESMF_ClockGetPrevSimTime(clock, prevSimTime, rc)
      call ESMF_TimeIntervalGet(prevSimTime, D=D, S=S, rc=rc)
      print *, "Clock's previous simulation time = ", D, " days", S, " seconds."

      ! sync clock to wall clock
      call ESMF_ClockSyncToWallClock(clock, rc)
      print *
      print *, "Clock sync to wall clock = "
      call ESMF_ClockPrint(clock, "currtime string", rc)

      end program ESMF_ClockEx
