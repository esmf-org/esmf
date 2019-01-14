! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_AlarmEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_AlarmEx - Alarm examples
!
! !DESCRIPTION:
!
! This program shows an example of how to create, initialize, and process
! alarms associated with a clock.
!-----------------------------------------------------------------------------
#include "ESMF.h"

      ! ESMF Framework module
      use ESMF
      use ESMF_TestMod
      implicit none

      ! instantiate time_step, start, stop, and alarm times
      type(ESMF_TimeInterval) :: timeStep, alarmInterval
      type(ESMF_Time) :: alarmTime, startTime, stopTime

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate Alarm lists
      integer, parameter :: NUMALARMS = 2
      type(ESMF_Alarm) :: alarm(NUMALARMS)

      ! local variables for Get methods
      integer :: ringingAlarmCount  ! at any time step (0 to NUMALARMS)

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
      write(testname, *) "Example ESMF_AlarmEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
        defaultlogfilename="AlarmEx.Log", &
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
!\subsubsection{Alarm initialization}

! This example shows how to create and initialize two {\tt ESMF\_Alarms} and
! associate them with the clock.
!EOE

!BOC
      ! Initialize first alarm to be a one-shot on 9/15/2003 and associate
      ! it with the clock
      call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=15, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      alarm(1) = ESMF_AlarmCreate(clock, &
         ringTime=alarmTime, name="Example alarm 1", rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! Initialize second alarm to ring on a 1 week interval starting 9/1/2003
      ! and associate it with the clock
      call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=1, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      call ESMF_TimeIntervalSet(alarmInterval, d=7, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
      ! Alarm gets default name "Alarm002"
      alarm(2) = ESMF_AlarmCreate(clock=clock, ringTime=alarmTime, &
                                  ringInterval=alarmInterval, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!\subsubsection{Clock advance and Alarm processing}

! This example shows how to advance an {\tt ESMF\_Clock} and process any 
! resulting ringing alarms.
!EOE

!BOC
      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc=rc))
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        ! perform time step and get the number of any ringing alarms
        call ESMF_ClockAdvance(clock, ringingAlarmCount=ringingAlarmCount, &
                               rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
        call ESMF_ClockPrint(clock, options="currTime string", rc=rc)
!EOC

        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
        ! check if alarms are ringing
        if (ringingAlarmCount > 0) then
          print *, "number of ringing alarms = ", ringingAlarmCount

          do i = 1, NUMALARMS
            if (ESMF_AlarmIsRinging(alarm(i), rc=rc)) then
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
              call ESMF_AlarmGet(alarm(i), name=name, rc=rc)
              print *, trim(name), " is ringing!"
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
              ! after processing alarm, turn it off
              call ESMF_AlarmRingerOff(alarm(i), rc=rc)
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
            end if ! this alarm is ringing
          end do ! each ringing alarm
        endif ! ringing alarms
      end do ! timestep clock
!EOC

!BOE
!\subsubsection{Alarm and Clock destruction}

! This example shows how to destroy {\tt ESMF\_Alarms} and {\tt ESMF\_Clocks}.
!EOE

!BOC 
      call ESMF_AlarmDestroy(alarm(1), rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
      call ESMF_AlarmDestroy(alarm(2), rc=rc)
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
         print *, "PASS: ESMF_AlarmEx.F90"
      else
         print *, "FAIL: ESMF_AlarmEx.F90"
      end if

!BOC 
      end program ESMF_AlarmEx
!EOC
