! $Id: ESMF_ClockAdvEx.F90,v 1.17 2003/12/02 23:35:15 svasquez Exp $
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
      '$Id: ESMF_ClockAdvEx.F90,v 1.17 2003/12/02 23:35:15 svasquez Exp $'
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
      integer :: rc, finalrc

      !
      ! initialization
      !
      finalrc = ESMF_SUCCESS

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize time interval to -1 day
      call ESMF_TimeIntervalSet(timeStep, d=-1, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize start time to 5/15/2003 12:00:00 noon
      call ESMF_TimeSet(startTime, yr=2003, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize stop time to 5/15/2002 12:00:00 noon
      call ESMF_TimeSet(stopTime, yr=2002, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize reference time to 1/1/2000 00:00:00 midnight
      call ESMF_TimeSet(refTime, yr=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock A", timeStep, startTime, stopTime, &
                               refTime, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currtime", rc)        ! default format

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockPrint(clock, "currtime string", rc) ! string format

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize alarm time to July 4, 2002, noon
      call ESMF_TimeSet(alarmTime, yr=2002, mm=7, dd=4, h=12, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      alarmRinging = .false.

      !
      ! time step clock (backward -- negative time step!) from start time
      ! to stop time, checking for alarm
      !

      do while (.not.ESMF_ClockIsStopTime(clock, rc))

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        call ESMF_ClockAdvance(clock, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        call ESMF_ClockPrint(clock, "currtime string", rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

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

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! get and print clock's time_step
      call ESMF_ClockGet(clock, timeStep=time_step, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockPrint(clock, "timestep string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, d=d, s=s, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's timestep = ", D, " integer days, ", &
                S, " integer seconds."

      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_r8=d_r8, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's timestep = ", d_r8, " floating point days."

      ! get start time's floating point day of the year
      call ESMF_TimeGet(startTime, dayOfYear_r8=d_r8, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Start time's floating point day of the year = ", d_r8

      ! get stop time's integer day of the year
      call ESMF_TimeGet(stopTime, dayOfYear=yD, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Stop time's integer day of the year = ", yD

      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "The clock was advanced ", advanceCount, " times."
  
      ! calculate the difference between the start and stop times
      time_diff = stopTime - startTime
      call ESMF_TimeIntervalGet(time_diff, d=d, s=s, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Difference between start and stop times = ", d, " days, ", &
                s, " seconds."

      ! get clock's start time
      call ESMF_ClockGet(clock, startTime=startTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeGet(startTime, yr=yr, mm=mm, dd=dd, h=h, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's start time = ", yr, "/", mm, "/", dd, " ", h, " hours."

      ! get clock's stop time
      call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeGet(stopTime, yr=yr, mm=mm, dd=dd, h=h, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's stop time = ", yr, "/", mm, "/", dd, " ", h, " hours."

      ! get clock's reference time
      call ESMF_ClockGet(clock, refTime=refTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeGet(refTime, yr=yr, mm=mm, dd=dd, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's reference time = ", yr, "/", mm, "/", dd, " midnight."

      ! get clock's current time
      call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's current time = "
      call ESMF_TimePrint(curr_time, "string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! get clock's previous time
      call ESMF_ClockGet(clock, prevTime=prev_time, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's previous time = "
      call ESMF_TimePrint(prev_time, "string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if


      ! get clock's current simulation time
      call ESMF_ClockGet(clock, currSimTime=currSimTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeIntervalGet(currSimTime, d=d, s=s, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's current simulation time = ", d, " days", s, " seconds."

      ! get clock's previous simulation time
      call ESMF_ClockGet(clock, prevSimTime=prevSimTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeIntervalGet(prevSimTime, d=d, s=s, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Clock's previous simulation time = ", d, " days", s, " seconds."
      print *

      !
      ! copy time step and current time
      !

      time_step_copy = time_step
      print *, "Time step copy = "
      call ESMF_TimeIntervalPrint(time_step_copy, "string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      curr_time_copy = curr_time
      print *, "Current time copy = "
      call ESMF_TimePrint(curr_time_copy, "string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if


      !
      ! change time step and current time
      !

      print *, "Current Time Step = "
      call ESMF_ClockPrint(clock, "timestep string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockSet(clock, timeStep=timeStep, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Time Step reset to = "
      call ESMF_ClockPrint(clock, "timestep string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Current time = "
      call ESMF_ClockPrint(clock, "currtime string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Previous time = "
      call ESMF_ClockPrint(clock, "prevtime string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeSet(curr_time, yr=1776, &
                        mm=7, dd=4, calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockSet(clock, currTime=curr_time, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Current Time changed to = "
      call ESMF_ClockPrint(clock, "currtime string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "Previous Time changed to = "
      call ESMF_ClockPrint(clock, "prevtime string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if


      !
      ! sync clock to wall clock
      !

      call ESMF_ClockSyncToRealTime(clock, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *
      print *, "Clock sync to wall clock = "
      call ESMF_ClockPrint(clock, "currtime string", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if
   
      ! destroy clock
      call ESMF_ClockDestroy(clock, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ClockAdvEx.F90"
     else
        print *, "FAIL: ESMF_ClockAdvEx.F90"
     end if


      end program ESMF_ClockAdvEx
