! $Id: ESMF_ClockAdvEx.F90,v 1.13 2003/09/10 03:30:55 eschwab Exp $
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
      program ESMF_ClockAdvEx

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_ClockAdvEx - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows some advanced examples of clock operation, including a
! simple user-defined single-shot alarm
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockAdvEx.F90,v 1.13 2003/09/10 03:30:55 eschwab Exp $'
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
      integer :: yr, mm, dd, d, h, m, s, yD
      type(ESMF_TimeInterval) :: time_step, time_step_copy
      type(ESMF_TimeInterval) :: time_diff
      type(ESMF_Time) :: curr_time, curr_time_copy, prev_time
      integer(ESMF_KIND_I8) :: advanceCount, yr_i8, d_i8, s_i8
      double precision :: d_r8
      logical alarmRinging

      ! result code
      integer :: rc

      !
      ! initialization
      !

      ! initialize ESMF framework
      call ESMF_FrameworkInitialize(rc)

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to -1 day
      call ESMF_TimeIntervalSet(timeStep, d=-1, rc=rc)

      ! initialize start time to 5/15/2003 12:00:00 noon
      call ESMF_TimeSet(startTime, yr=2003, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize stop time to 5/15/2002 12:00:00 noon
      call ESMF_TimeSet(stopTime, yr=2002, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize reference time to 1/1/2000 00:00:00 midnight
      call ESMF_TimeSet(refTime, yr=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, refTime, rc)

      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currtime", rc)        ! default format
      call ESMF_ClockPrint(clock, "currtime string", rc) ! string format

      ! initialize alarm time to July 4, 2002, noon
      call ESMF_TimeSet(alarmTime, yr=2002, mm=7, dd=4, h=12, &
                        calendar=gregorianCalendar, rc=rc)
      alarmRinging = .false.

      !
      ! time step clock (backward -- negative time step!) from start time
      ! to stop time, checking for alarm
      !

      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
        call ESMF_ClockPrint(clock, "currtime string", rc)
        call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)
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

      ! get and print clock's time_step
      call ESMF_ClockGet(clock, timeStep=time_step, rc=rc)
      call ESMF_ClockPrint(clock, "timestep string", rc)

      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, d=d, s=s, rc=rc)
      print *, "Clock's timestep = ", D, " integer days, ", &
                S, " integer seconds."

      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_r8=d_r8, rc=rc)
      print *, "Clock's timestep = ", d_r8, " floating point days."

      ! get start time's floating point day of the year
      call ESMF_TimeGet(startTime, dayOfYear_r8=d_r8, rc=rc)
      print *, "Start time's floating point day of the year = ", d_r8

      ! get stop time's integer day of the year
      call ESMF_TimeGet(stopTime, dayOfYear=yD, rc=rc)
      print *, "Stop time's integer day of the year = ", yD

      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
      print *, "The clock was advanced ", advanceCount, " times."
  
      ! calculate the difference between the start and stop times
      time_diff = stopTime - startTime
      call ESMF_TimeIntervalGet(time_diff, d=d, s=s, rc=rc)
      print *, "Difference between start and stop times = ", d, " days, ", &
                s, " seconds."

      ! get clock's start time
      call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
      call ESMF_TimeGet(startTime, yr=yr, mm=mm, dd=dd, h=h, rc=rc)
      print *, "Clock's start time = ", yr, "/", mm, "/", dd, " ", h, " hours."

      ! get clock's stop time
      call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
      call ESMF_TimeGet(stopTime, yr=yr, mm=mm, dd=dd, h=h, rc=rc)
      print *, "Clock's stop time = ", yr, "/", mm, "/", dd, " ", h, " hours."

      ! get clock's reference time
      call ESMF_ClockGet(clock, refTime=refTime, rc=rc)
      call ESMF_TimeGet(refTime, yr=yr, mm=mm, dd=dd, rc=rc)
      print *, "Clock's reference time = ", yr, "/", mm, "/", dd, " midnight."

      ! get clock's current time
      call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)
      print *, "Clock's current time = "
      call ESMF_TimePrint(curr_time, "string", rc)

      ! get clock's previous time
      call ESMF_ClockGet(clock, prevTime=prev_time, rc=rc)
      print *, "Clock's previous time = "
      call ESMF_TimePrint(prev_time, "string", rc)

      ! get clock's current simulation time
      call ESMF_ClockGet(clock, currSimTime=currSimTime, rc=rc)
      call ESMF_TimeIntervalGet(currSimTime, d=d, s=s, rc=rc)
      print *, "Clock's current simulation time = ", d, " days", s, " seconds."

      ! get clock's previous simulation time
      call ESMF_ClockGet(clock, prevSimTime=prevSimTime, rc=rc)
      call ESMF_TimeIntervalGet(prevSimTime, d=d, s=s, rc=rc)
      print *, "Clock's previous simulation time = ", d, " days", s, " seconds."
      print *

      !
      ! copy time step and current time
      !

      time_step_copy = time_step
      print *, "Time step copy = "
      call ESMF_TimeIntervalPrint(time_step_copy, "string", rc)

      curr_time_copy = curr_time
      print *, "Current time copy = "
      call ESMF_TimePrint(curr_time_copy, "string", rc)

      !
      ! change time step and current time
      !

      print *, "Current Time Step = "
      call ESMF_ClockPrint(clock, "timestep string", rc)

      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
      call ESMF_ClockSet(clock, timeStep=timeStep, rc=rc)
      print *, "Time Step reset to = "
      call ESMF_ClockPrint(clock, "timestep string", rc)

      print *, "Current time = "
      call ESMF_ClockPrint(clock, "currtime string", rc)
      print *, "Previous time = "
      call ESMF_ClockPrint(clock, "prevtime string", rc)

      call ESMF_TimeSet(curr_time, yr=1776, &
                        mm=7, dd=4, calendar=gregorianCalendar, rc=rc)
      call ESMF_ClockSet(clock, currTime=curr_time, rc=rc)
      print *, "Current Time changed to = "
      call ESMF_ClockPrint(clock, "currtime string", rc)
      print *, "Previous Time changed to = "
      call ESMF_ClockPrint(clock, "prevtime string", rc)

      !
      ! sync clock to wall clock
      !

      call ESMF_ClockSyncToRealTime(clock, rc)
      print *
      print *, "Clock sync to wall clock = "
      call ESMF_ClockPrint(clock, "currtime string", rc)

      ! finalize ESMF framework
      call ESMF_FrameworkFinalize(rc)

      end program ESMF_ClockAdvEx
