! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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
#include "ESMF.h"

      ! ESMF Framework module
      use ESMF
      use ESMF_TestMod
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
      integer :: finalrc, result
      character(ESMF_MAXSTR) :: testname
      character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_ClockEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
        defaultlogfilename="ClockEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Clock creation}

! This example shows how to create and initialize an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! initialize time interval to 2 days, 4 hours (6 timesteps in 13 days)
      call ESMF_TimeIntervalSet(timeStep, d=2, h=4, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! initialize start time to 4/1/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(startTime, yy=2003, mm=4, dd=1, h=2, m=24, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! initialize stop time to 4/14/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeSet(stopTime, yy=2003, mm=4, dd=14, h=2, m=24, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! initialize the clock with the above values
      clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                               name="Clock 1", rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Clock advance}

! This example shows how to time-step an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc=rc))
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        call ESMF_ClockPrint(clock, options="currTime string", rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        call ESMF_ClockAdvance(clock, rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      end do
!EOC

!BOE
!\subsubsection{Clock examination}

! This example shows how to examine an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! get the clock's final current time
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
      call ESMF_TimeGet(currTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc) 
      print *, "The clock's final current time is ", YY, "/", MM, "/", DD, &
               " ", H, ":", M, ":", S
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
      ! get the number of times the clock was advanced
      call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
      print *, "The clock was advanced ", advanceCount, " times."
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
!\subsubsection{Clock reversal}

! This example shows how to time-step an {\tt ESMF\_Clock} in reverse mode.
!EOE

!BOC
      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! time step clock in reverse from stop time back to start time;
      !  note use of ESMF_ClockIsDone() rather than ESMF_ClockIsStopTime()
      do while (.not.ESMF_ClockIsDone(clock, rc=rc))
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        call ESMF_ClockPrint(clock, options="currTime string", rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        call ESMF_ClockAdvance(clock, rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      end do
!EOC

!BOE
!\subsubsection{Clock destruction}

! This example shows how to destroy an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! destroy clock
      call ESMF_ClockDestroy(clock, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
      ! file that the scripts grep for.
      call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)


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
