! $Id: ESMF_ClockAdv2Ex.F90,v 1.1 2004/01/30 01:29:49 eschwab Exp $
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
      program ESMF_ClockAdv2Ex

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOP
!\begin{verbatim}
! !PROGRAM: ESMF_ClockAdv2Ex - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows some advanced examples of setting a clock's stop time.
! See ESMF_ClockCreate() below.
!
!-----------------------------------------------------------------------------
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start and stop times
      type(ESMF_TimeInterval) :: timeStep, runDuration
      type(ESMF_Time) :: startTime, stopTime

      ! temp variables for Get functions
      type(ESMF_TimeInterval) :: run_duration
      integer(ESMF_KIND_I8) :: advanceCount
      real(ESMF_KIND_R8) :: time_step_count
      integer :: rc

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
      ! initialize time step to 1 hour
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
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
      ! initialize stop time to 5/15/2003 08:00:00 PM
      call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=15, h=20, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize stop time to 6 hours later
      call ESMF_TimeIntervalSet(runDuration, h=6, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock A", timeStep, startTime, &
                               runDuration=runDuration, rc=rc)
!                               runTimeStepCount=4, rc=rc)
!                               stopTime=stopTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockPrint(clock, "starttime string", rc) ! string format
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockPrint(clock, "stoptime string", rc) ! string format
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockPrint(clock, "timestep string", rc) ! string format
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockGet(clock, runDuration=run_duration, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Clock's run duration = " 
      call ESMF_TimeIntervalPrint(run_duration, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_ClockGet(clock, runTimeStepCount=time_step_count, rc=rc)
      print *, "Clock's run duration as a number of timesteps = ", &
               time_step_count 
      print *
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}

      !
      ! time step clock from start time to stop time
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

      end do

      !
      ! examine clock
      !

      ! print clock current time
      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
      print *, "The clock was advanced ", advanceCount, " times."
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
        print *, "PASS: ESMF_ClockAdv2Ex.F90"
     else
        print *, "FAIL: ESMF_ClockAdv2Ex.F90"
     end if

!BOP
!\begin{verbatim}
      end program ESMF_ClockAdv2Ex
!\end{verbatim}
!EOP
