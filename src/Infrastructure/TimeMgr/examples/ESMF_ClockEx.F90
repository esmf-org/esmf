! $Id: ESMF_ClockEx.F90,v 1.41.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_ClockEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_ClockEx - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows an example of how to create, initialize, advance, and
! examine a basic clock
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate time_step, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime

      ! local variables for Get methods
      type(ESMF_Time) :: currTime
      integer(ESMF_KIND_I8) :: advanceCount
      integer :: YY, MM, DD, H, M, S

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

!BOE
!\subsubsection{Clock Creation}

! This example shows how to create and initialize an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! initialize time interval to 2 days, 4 hours (6 timesteps in 13 days)
      call ESMF_TimeIntervalSet(timeStep, d=2, h=4, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize start time to 4/1/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(startTime, yy=2003, mm=4, dd=1, h=2, m=24, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize stop time to 4/14/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(stopTime, yy=2003, mm=4, dd=14, h=2, m=24, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, stopTime, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Clock Advance}

! This example shows how to time-step an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        call ESMF_ClockPrint(clock, "currTime string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        call ESMF_ClockAdvance(clock, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      end do
!EOC

!BOE
!\subsubsection{Clock Examination}

! This example shows how to examine an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! get the clock's final current time
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BOC
      call ESMF_TimeGet(currTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc) 
      print *, "The clock's final current time is ", YY, "/", MM, "/", DD, &
               " ", H, ":", M, ":", S
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BOC
      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
      print *, "The clock was advanced ", advanceCount, " times."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE
  
!BOE
!\subsubsection{Clock Reversal}

! This example shows how to time-step an {\tt ESMF\_Clock} in reverse mode.
!EOE

!BOC
      call ESMF_ClockSet(clock, direction=ESMF_MODE_REVERSE, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! time step clock in reverse from stop time back to start time;
      !  note use of ESMF_ClockIsDone() rather than ESMF_ClockIsStopTime()
      do while (.not.ESMF_ClockIsDone(clock, rc))
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        call ESMF_ClockPrint(clock, "currTime string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        call ESMF_ClockAdvance(clock, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      end do
!EOC

!BOE
!\subsubsection{Clock Destruction}

! This example shows how to destroy an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! destroy clock
      call ESMF_ClockDestroy(clock, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
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
