! $Id: ESMF_ClockAdv2Ex.F90,v 1.3 2004/06/05 00:17:33 eschwab Exp $
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
!BOC
! !PROGRAM: ESMF_ClockAdv2Ex - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows some advanced examples of setting a clock's stop time.
! See ESMF_ClockCreate() below.
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start and stop times
      type(ESMF_TimeInterval) :: timeStep, runDuration
      type(ESMF_Time) :: startTime, stopTime

      ! local variables for Get functions
      type(ESMF_TimeInterval) :: run_duration
      integer(ESMF_KIND_I8) :: advanceCount
      real(ESMF_KIND_R8) :: time_step_count

      ! return code
      integer :: rc
!EOC

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      !
      ! initialization
      !

      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize time step to 1 hour
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize start time to 5/15/2003 12:00:00 noon
      call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=15, h=12, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize stop time to 5/15/2003 08:00:00 PM
      call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=15, h=20, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize stop time to 6 hours later
      call ESMF_TimeIntervalSet(runDuration, h=6, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock A", timeStep, startTime, &
                               runDuration=runDuration, rc=rc)
!                               runTimeStepCount=4, rc=rc)
!                               stopTime=stopTime, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockPrint(clock, "starttime string", rc) ! string format
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockPrint(clock, "stoptime string", rc) ! string format
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockPrint(clock, "timestep string", rc) ! string format
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockGet(clock, runDuration=run_duration, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Clock's run duration = " 
      call ESMF_TimeIntervalPrint(run_duration, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockGet(clock, runTimeStepCount=time_step_count, rc=rc)
      print *, "Clock's run duration as a number of timesteps = ", &
               time_step_count 
      print *
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC

      !
      ! time step clock from start time to stop time
      !

      do while (.not.ESMF_ClockIsStopTime(clock, rc))
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        call ESMF_ClockAdvance(clock, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      end do

      !
      ! examine clock
      !

      ! print clock current time
      call ESMF_ClockPrint(clock, "currtime string", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
      print *, "The clock was advanced ", advanceCount, " times."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! destroy clock
      call ESMF_ClockDestroy(clock, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
         print *, "PASS: ESMF_ClockAdv2Ex.F90"
      else
         print *, "FAIL: ESMF_ClockAdv2Ex.F90"
      end if

!BOC
      end program ESMF_ClockAdv2Ex
!EOC
