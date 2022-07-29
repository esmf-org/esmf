! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
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
      type(ESMF_TimeInterval) :: timeStep, alarmInterval, ringTimeInterval
      type(ESMF_Time) :: alarmTime, startTime, stopTime, ringTime
      logical           :: ringing, enabled, sticky

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate Alarm lists
      integer, parameter :: NUMALARMS = 2
      type(ESMF_Alarm) :: alarms(NUMALARMS), alarm

      ! local variables for Get methods
      integer :: ringingAlarmCount  ! at any time step (0 to NUMALARMS)

      ! name, loop counter, result code
      character (len=ESMF_MAXSTR) :: name
      integer :: i, n, nrings, rc, result, status
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
      alarms(1) = ESMF_AlarmCreate(clock, &
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
      alarms(2) = ESMF_AlarmCreate(clock=clock, ringTime=alarmTime, &
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
            if (ESMF_AlarmIsRinging(alarms(i), rc=rc)) then
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
              call ESMF_AlarmGet(alarms(i), name=name, rc=rc)
              print *, trim(name), " is ringing!"
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
              ! after processing alarm, turn it off
              call ESMF_AlarmRingerOff(alarms(i), rc=rc)
!EOC

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
            end if ! this alarm is ringing
          end do ! each ringing alarm
        endif ! ringing alarms
      end do ! timestep clock
!EOC

!------------------------------------------------------------------------------
!BOE
!\subsubsection{Alarm and Clock destruction}

! This example shows how to destroy {\tt ESMF\_Alarms} and {\tt ESMF\_Clocks}.
!EOE

!BOC 
      call ESMF_AlarmDestroy(alarms(1), rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
      call ESMF_AlarmDestroy(alarms(2), rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC 
      call ESMF_ClockDestroy(clock, rc=rc)
!EOC

      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOE
!\subsubsection{One shot Alarm}

! This example shows how to set up an one shot alarm that will ring exactly at
! the ringtime specified during alarmCreate but not at any other time.
! 
! To specify such an alarm, do not use the ringInterval argument during AlarmCreate.
!EOE
!BOE

! A clock is set up to run from 0h0m0s to 1h0m0s; One shot ring time is set to 0h30m0s.

!EOE
!BOC
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=30,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! The clock advances by 10 minutes.

!EOE
!BOC
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    nrings = 0
!BOE

! Create the clock from the specified start time, stop time and time step. 

!EOE
!BOC
    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Create the alarm from the specified ring time (0h0m0s) only. This is how one shot alarm
! is created.
! This alarm is enabled upon creation and non sticky.

!EOE
!BOC
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, enabled=.true., sticky=.false., name='alarm', rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm to return ringing=true, enabled=true, sticky=false.

!EOE
!BOC
    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    !print *, 'Ex: ringing = ', ringing
!BOE

! Advance the clock 6 times to move from 0h0m0s to 1h0m0s.

!EOE
!BOC
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm only return ringing=true at 0h 30m 0s.

!EOE
!BOC
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      !print *, '  Ex: ringing = ', ringing
    enddo

!BOE

! Destroy the alarm and clock created for this example.

!EOE
!BOC
    call ESMF_AlarmDestroy(alarm, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_ClockDestroy(clock, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!\subsubsection{Using Alarm in clock running forward}

! This example shows how to set up an alarm in conjuction with a reverse
! running clock.
!EOE
!BOE

! A clock is set up to run from 0h0m0s to 1h0m0s; Initial ring time is set to 0h0m0s.

!EOE
!BOC
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! The clock advances by 10 minutes and the alarm rings every 10 minutes as well.

!EOE
!BOC
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    nrings = 0
!BOE

! Create the clock from the specified start time, stop time and time step. 

!EOE
!BOC
    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Create the alarm from the specified ring time (0h0m0s), ring interval (10 minutes). 
! This alarm is enabled upon creation and non sticky.

!EOE
!BOC
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm to return ringing=true, enabled=true, sticky=false.

!EOE
!BOC
    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    !print *, 'Ex: ringing = ', ringing
!BOE

! Advance the clock 6 times to move from 0h0m0s to 1h0m0s.

!EOE
!BOC
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm should return ringing=true, enabled=true, sticky=false every clock time step
! because the alarm and the clock have identical time step (or interval).

!EOE
!BOC
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      !print *, '  Ex: ringing = ', ringing
      if(ringing) nrings = nrings + 1
    enddo

!BOE

! Destroy the alarm and clock created for this example.

!EOE
!BOC
    call ESMF_AlarmDestroy(alarm, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_ClockDestroy(clock, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!\subsubsection{Using Alarm in clock running reverse}

! This example shows how to set up an alarm in conjuction with a reverse
! running clock.
!EOE
!BOE

! A clock is set up to run from 1h0m0s to 0h0m0s; Initial ring time is set to 1h0m0s.
! For clock running in reverse direction, the start time should still be in the past
! compared with the stoptime. This example shows the start time is 0h0m0s, the stop
! time is 1h0m0s. We will set the current time to 1h0m0s and set the clock to run
! in reverse direction.

!EOE
!BOC
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! The clock advances by 10 minutes and the alarm rings every 10 minutes as well.

!EOE
!BOC
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    nrings = 0
!BOE

! Create the clock from the specified start time, stop time and time step. 

!EOE
!BOC
    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Here we set the current time on the clock to stop time 1h0m0s and set the clock to run in
! reverse direction.

!EOE

!BOC
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Create the alarm from the specified ring time (0h0m0s), ring interval (10 minutes). 
! This alarm is enabled upon creation and non sticky.

!EOE
!BOC
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm to return ringing=true, enabled=true, sticky=false.

!EOE
!BOC
    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    !print *, 'Ex: ringing = ', ringing
!BOE

! Advance the clock 6 times to move from 0h0m0s to 1h0m0s.

!EOE
!BOC
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm should return ringing=true, enabled=true, sticky=false every clock time step
! because the alarm and the clock have identical time step (or interval).

!EOE
!BOC
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      !print *, '  Ex: ringing = ', ringing
      if(ringing) nrings = nrings + 1
    enddo

!BOE

! Destroy the alarm and clock created for this example.

!EOE
!BOC
    call ESMF_AlarmDestroy(alarm, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_ClockDestroy(clock, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!\subsubsection{Using Alarm in clock runs forward then reverse}

! This example shows how to set up an alarm in conjuction with a reverse
! running clock.
!EOE

!BOE

! A clock is set up to run from 0h0m0s to 1h0m0s; Initial ring time is set to 0h0m0s.

!EOE
!BOC
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! The clock advances by 10 minutes and the alarm rings every 10 minutes as well.

!EOE
!BOC
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    nrings = 0
!BOE

! Create the clock from the specified start time, stop time and time step. 

!EOE
!BOC
    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Create the alarm from the specified ring time (0h0m0s), ring interval (10 minutes). 
! This alarm is enabled upon creation and non sticky.

!EOE
!BOC
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm to return ringing=true, enabled=true, sticky=false.

!EOE
!BOC
    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    !print *, 'Ex: ringing = ', ringing
!BOE

! Advance the clock 6 times to move from 0h0m0s to 1h0m0s.

!EOE
!BOC
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm should return ringing=true, enabled=true, sticky=false every clock time step
! because the alarm and the clock have identical time step (or interval).

!EOE
!BOC
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      !print *, '  Ex: ringing = ', ringing
      if(ringing) nrings = nrings + 1
    enddo
!BOE

! Reverse the direction of the clock, since the clock's current time is 1h0m0s, the clock
! now runs backward to 0h50m0s, 0h40m0s, etc.

!EOE
!BOC
    call ESMF_ClockSet(clock, direction = ESMF_DIRECTION_REVERSE, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Advance the clock 6 times to move from 1h0m0s to 0h0m0s.

!EOE
!BOC
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOE

! Query the alarm to check if it's ringing, enabled and sticky. In this case, we expect
! the alarm should return ringing=true, enabled=true, sticky=false every clock time step
! because the alarm and the clock have identical time step (or interval).

!EOE
!BOC
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
!EOC
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      !print *, '  Ex: ringing = ', ringing
    enddo
!BOE

! Destroy the alarm and clock created for this example.

!EOE
!BOC
    call ESMF_AlarmDestroy(alarm, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
!BOC
    call ESMF_ClockDestroy(clock, rc=status)
!EOC
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()


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
