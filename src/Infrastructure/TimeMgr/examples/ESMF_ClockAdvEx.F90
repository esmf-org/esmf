! $Id: ESMF_ClockAdvEx.F90,v 1.23 2004/01/30 01:31:13 eschwab Exp $
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
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOP
!\begin{verbatim}
! !PROGRAM: ESMF_ClockAdvEx - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows some advanced examples of clock operation, including a
! simple user-defined single-shot alarm
!
!-----------------------------------------------------------------------------
      use ESMF_Mod
      implicit none

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
      integer :: yy, mm, dd, d, h, m, s, yD, rc
      type(ESMF_TimeInterval) :: time_step, time_step_copy
      type(ESMF_TimeInterval) :: time_diff
      type(ESMF_Time) :: curr_time, curr_time_copy, prev_time
      integer(ESMF_KIND_I8) :: advanceCount, yy_i8, d_i8, s_i8
      double precision :: d_r8
      logical alarmRinging
!\end{verbatim}
!EOP

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
      !
      ! initialization
      !

      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize time interval to -1 day
      call ESMF_TimeIntervalSet(timeStep, d=-1, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize start time to 5/15/2003 12:00:00 noon
      call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize stop time to 5/15/2002 12:00:00 noon
      call ESMF_TimeSet(stopTime, yy=2002, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize reference time to 1/1/2000 00:00:00 midnight
      call ESMF_TimeSet(refTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock A", timeStep, startTime, stopTime, &
                               refTime=refTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currtime", rc)        ! default format
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockPrint(clock, "currtime string", rc) ! string format
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize alarm time to July 4, 2002, noon
      call ESMF_TimeSet(alarmTime, yy=2002, mm=7, dd=4, h=12, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      alarmRinging = .false.

      !
      ! time step clock (backward -- negative time step!) from start time
      ! to stop time, checking for alarm
      !

      do while (.not.ESMF_ClockIsStopTime(clock, rc))
!\end{verbatim}
!EOP

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

!BOP
!\begin{verbatim}
        call ESMF_ClockAdvance(clock, rc=rc)
!\end{verbatim}
!EOP

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

!BOP
!\begin{verbatim}
        call ESMF_ClockPrint(clock, "currtime string", rc)
!\end{verbatim}
!EOP

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

!BOP
!\begin{verbatim}
        call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)
!\end{verbatim}
!EOP

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

!BOP
!\begin{verbatim}
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
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! get and print clock's time_step
      call ESMF_ClockGet(clock, timeStep=time_step, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockPrint(clock, "timestep string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, d=d, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's timestep = ", D, " integer days, ", &
                S, " integer seconds."

      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_r8=d_r8, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's timestep = ", d_r8, " floating point days."

      ! get start time's floating point day of the year
      call ESMF_TimeGet(startTime, dayOfYear_r8=d_r8, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Start time's floating point day of the year = ", d_r8

      ! get stop time's integer day of the year
      call ESMF_TimeGet(stopTime, dayOfYear=yD, rc=rc)

!\end{verbatim}
!EOP
      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Stop time's integer day of the year = ", yD

      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "The clock was advanced ", advanceCount, " times."
  
      ! calculate the difference between the start and stop times
      time_diff = stopTime - startTime
      call ESMF_TimeIntervalGet(time_diff, d=d, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Difference between start and stop times = ", d, " days, ", &
                s, " seconds."

      ! get clock's start time
      call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(startTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's start time = ", yy, "/", mm, "/", dd, " ", h, " hours."

      ! get clock's stop time
      call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(stopTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's stop time = ", yy, "/", mm, "/", dd, " ", h, " hours."

      ! get clock's reference time
      call ESMF_ClockGet(clock, refTime=refTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(refTime, yy=yy, mm=mm, dd=dd, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's reference time = ", yy, "/", mm, "/", dd, " midnight."

      ! get clock's current time
      call ESMF_ClockGet(clock, currTime=curr_time, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's current time = "
      call ESMF_TimePrint(curr_time, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! get clock's previous time
      call ESMF_ClockGet(clock, prevTime=prev_time, rc=rc)

!\end{verbatim}
!EOP
      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's previous time = "
      call ESMF_TimePrint(prev_time, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if


!BOP
!\begin{verbatim}
      ! get clock's current simulation time
      call ESMF_ClockGet(clock, currSimTime=currSimTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeIntervalGet(currSimTime, d=d, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's current simulation time = ", d, " days", s, " seconds."

      ! get clock's previous simulation time
      call ESMF_ClockGet(clock, prevSimTime=prevSimTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeIntervalGet(prevSimTime, d=d, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's previous simulation time = ", d, " days", s, " seconds."
      print *

      !
      ! copy time step and current time
      !

      time_step_copy = time_step
      print *, "Time step copy = "
      call ESMF_TimeIntervalPrint(time_step_copy, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      curr_time_copy = curr_time
      print *, "Current time copy = "
      call ESMF_TimePrint(curr_time_copy, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}

      !
      ! change time step and current time
      !

      print *, "Current Time Step = "
      call ESMF_ClockPrint(clock, "timestep string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeIntervalSet(timeStep, d=2, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockSet(clock, timeStep=timeStep, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Step reset to = "
      call ESMF_ClockPrint(clock, "timestep string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Current time = "
      call ESMF_ClockPrint(clock, "currtime string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Previous time = "
      call ESMF_ClockPrint(clock, "prevtime string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeSet(curr_time, yy=1776, &
                        mm=7, dd=4, calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockSet(clock, currTime=curr_time, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Current Time changed to = "
      call ESMF_ClockPrint(clock, "currtime string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Previous Time changed to = "
      call ESMF_ClockPrint(clock, "prevtime string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}

      !
      ! sync clock to wall clock
      !

      call ESMF_ClockSyncToRealTime(clock, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *
      print *, "Clock sync to wall clock = "
      call ESMF_ClockPrint(clock, "currtime string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if
   
!BOP
!\begin{verbatim}
      ! destroy clock
      call ESMF_ClockDestroy(clock, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
     call ESMF_CalendarDestroy(gregorianCalendar, rc)
!\end{verbatim}    
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_ClockAdvEx.F90"
     else
        print *, "FAIL: ESMF_ClockAdvEx.F90"
     end if

!BOP
!\begin{verbatim}
      end program ESMF_ClockAdvEx
!\end{verbatim}
!EOP
