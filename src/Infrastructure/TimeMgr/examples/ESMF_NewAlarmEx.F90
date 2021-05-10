! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_NewAlarmEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_NewAlarmEx - NewAlarm examples
!
! !DESCRIPTION:
!
! This program shows an example of how to create, initialize, and process
! newalarms associated with a clock.
!-----------------------------------------------------------------------------
#include "ESMF.h"

      ! ESMF Framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! instantiate time_step, start, stop, and newalarm times
      type(ESMF_TimeInterval) :: timeStep, newalarmInterval
      type(ESMF_Time) :: newalarmTime, startTime, stopTime

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate NewAlarm lists
      integer, parameter :: NUMNEWALARMS = 2
      type(ESMF_NewAlarm) :: newalarm(NUMNEWALARMS)

      ! local variables for Get methods
      integer :: ringingNewAlarmCount  ! at any time step (0 to NUMNEWALARMS)

      ! name, loop counter, result code
      character (len=ESMF_MAXSTR) :: name
      integer :: i, rc, result
!EOC

      ! result code
      integer :: finalrc

      character(ESMF_MAXSTR) :: testname
      character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      write(failMsg, *) "Example failure"
      write(testname, *) "Example ESMF_NewAlarmEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
        defaultlogfilename="NewAlarmEx.Log", &
        logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Clock initialization}

! This example shows how to create and initialize an {\tt ESMF\_Clock}.
!EOE

!BOC
      ! initialize time interval to 1 day
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! initialize start time to 9/1/2003
      call ESMF_TimeSet(startTime, yy=2003, mm=9, dd=1, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! initialize stop time to 9/30/2003
      call ESMF_TimeSet(stopTime, yy=2003, mm=9, dd=30, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! create & initialize the clock with the above values
      clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                               name="The Clock", rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{NewAlarm initialization}

! This example shows how to create and initialize two {\tt ESMF\_NewAlarms} and
! associate them with the clock.
!EOE

!BOC
      ! Initialize first newalarm to be a one-shot on 9/15/2003 and associate
      ! it with the clock
      call ESMF_TimeSet(newalarmTime, yy=2003, mm=9, dd=15, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      newalarm(1) = ESMF_NewAlarmCreate(clock, &
         ringTime=newalarmTime, name="Example newalarm 1", rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! Initialize second newalarm to ring on a 1 week interval starting 9/1/2003
      ! and associate it with the clock
      call ESMF_TimeSet(newalarmTime, yy=2003, mm=9, dd=1, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      call ESMF_TimeIntervalSet(newalarmInterval, d=7, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! NewAlarm gets default name "NewAlarm002"
      newalarm(2) = ESMF_NewAlarmCreate(clock=clock, ringTime=newalarmTime, &
                                  ringInterval=newalarmInterval, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Clock advance and NewAlarm processing}

! This example shows how to advance an {\tt ESMF\_Clock} and process any 
! resulting ringing newalarms.
!EOE

!BOC
      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc=rc))
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        ! perform time step and get the number of any ringing newalarms
        call ESMF_ClockAdvance(clock, ringingNewAlarmCount=ringingNewAlarmCount, &
                               rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        call ESMF_ClockPrint(clock, options="currTime string", rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
        ! check if newalarms are ringing
        if (ringingNewAlarmCount > 0) then
          print *, "number of ringing newalarms = ", ringingNewAlarmCount

          do i = 1, NUMNEWALARMS
            if (ESMF_NewAlarmIsRinging(newalarm(i), rc=rc)) then
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
              call ESMF_NewAlarmGet(newalarm(i), name=name, rc=rc)
              print *, trim(name), " is ringing!"
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
              ! after processing newalarm, turn it off
              call ESMF_NewAlarmRingerOff(newalarm(i), rc=rc)
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
            end if ! this newalarm is ringing
          end do ! each ringing newalarm
        endif ! ringing newalarms
      end do ! timestep clock
!EOC

!BOE
!\subsubsection{NewAlarm and Clock destruction}

! This example shows how to destroy {\tt ESMF\_NewAlarms} and {\tt ESMF\_Clocks}.
!EOE

!BOC 
      call ESMF_NewAlarmDestroy(newalarm(1), rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
      call ESMF_NewAlarmDestroy(newalarm(2), rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
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
         print *, "PASS: ESMF_NewAlarmEx.F90"
      else
         print *, "FAIL: ESMF_NewAlarmEx.F90"
      end if

!BOC 
      end program ESMF_NewAlarmEx
!EOC
