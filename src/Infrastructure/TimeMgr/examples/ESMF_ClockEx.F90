! $Id: ESMF_ClockEx.F90,v 1.36 2004/06/05 00:17:33 eschwab Exp $
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
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_ClockEx - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows an example of how to create, initialize, run, and
! examine a basic clock
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime

      ! local variables for Get functions
      integer(ESMF_KIND_I4) :: days, sec, yD
      type(ESMF_TimeInterval) :: time_step
      type(ESMF_TimeInterval) :: time_diff
      integer(ESMF_KIND_I8) :: advanceCount
      real(ESMF_KIND_R8) :: days_r8
      type(ESMF_Calendar) :: cal
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
      ! initialize time interval to 2 days, 4 hours (6 timesteps in 13 days)
      call ESMF_TimeIntervalSet(timeStep, d=2, h=4, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize start time to 4/1/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(startTime, yy=2003, mm=4, dd=1, h=2, m=24, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize stop time to 4/14/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(stopTime, yy=2003, mm=4, dd=14, h=2, m=24, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! print starting time (initial current time)
      call ESMF_ClockPrint(clock, "currTime string", rc)
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
        call ESMF_ClockPrint(clock, "currTime string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      end do
      !
      ! examine clock
      !

      ! get and print clock's time_step
      call ESMF_ClockGet(clock, timeStep=time_step, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockPrint(clock, "timeStep string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, d=days, s=sec, rc=rc)

      print *, "Clock's timestep = ", days, " integer days, ", &
                sec, " integer seconds."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_r8=days_r8, rc=rc)

      print *, "Clock's timestep = ", days_r8, " floating point days."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get start time's floating point day of the year
      call ESMF_TimeGet(startTime, dayOfYear_r8=days_r8, rc=rc)

      print *, "Start time's floating point day of the year = ", days_r8

!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get stop time's integer day of the year
      call ESMF_TimeGet(stopTime, dayOfYear=yD, rc=rc)

      print *, "Stop time's integer day of the year = ", yD
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get clock's calendar
      call ESMF_ClockGet(clock, calendar=cal, rc=rc)
      print *, "Clock's calendar type = "
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_CalendarPrint(cal, "calendarType", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)

      print *, "The clock was advanced ", advanceCount, " times."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BOC
      ! calculate the difference between the start and stop times
      time_diff = stopTime - startTime
      call ESMF_TimeIntervalGet(time_diff, d=days, s=sec, rc=rc)

      print *, "Difference between start and stop times = ", days, " days, ", &
                sec, " seconds."
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
         print *, "PASS: ESMF_ClockEx.F90"
      else
         print *, "FAIL: ESMF_ClockEx.F90"
      end if

!BOC
      end program ESMF_ClockEx
!EOC
