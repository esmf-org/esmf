! $Id: ESMF_AlarmEx.F90,v 1.14 2004/06/05 00:17:33 eschwab Exp $
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
      program ESMF_AlarmEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_AlarmEx - Alarm examples
!
! !DESCRIPTION:
!
! This program shows an example of how to create, initialize, run, and examine
! a basic clock with alarms
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clockCopy

      ! instantiate Alarm lists (of pointers)
      integer, parameter :: NUMALARMS = 2
      type(ESMF_Alarm) :: alarm(NUMALARMS)
      integer :: ringingAlarmCount  ! at any time step (0 to NUMALARMS)
      type(ESMF_Alarm) :: ringingAlarm(NUMALARMS)
      type(ESMF_Alarm) :: tempAlarm
      type(ESMF_Alarm) :: alarmList(NUMALARMS)
      integer :: nAlarms

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start, stop, and alarm times
      type(ESMF_TimeInterval) :: timeStep, alarmInterval
      type(ESMF_Time) :: alarmTime, startTime, stopTime, nextTime, ringTime

      ! name, loop counters, result code
      character (len=ESMF_MAXSTR) :: name
      integer :: i, j, rc
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
!  Setup clock
!
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize time interval to 1 day
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize start time to 9/1/2003
      call ESMF_TimeSet(startTime, yy=2003, mm=9, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize stop time to 9/30/2003
      call ESMF_TimeSet(stopTime, yy=2003, mm=9, dd=30, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      !
      ! initialize the clock with the above values
      !
      clock = ESMF_ClockCreate("The Clock", timeStep, startTime, stopTime, &
                               rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockPrint(clock, "name", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
!
!  Setup alarms
!
      !
      ! Initialize first alarm to be a one-shot on 9/15/2003 and associate
      ! it with the clock
      !
      call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=15, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      name = "Example alarm 1"
      alarm(1) = ESMF_AlarmCreate(name, clock, ringTime=alarmTime, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "alarm(1) = "
      call ESMF_AlarmPrint(alarm(1), rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      !
      ! Initialize second alarm to ring on a 1 week interval from 9/1/2003 and
      ! associate it with the clock
      !
      call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeIntervalSet(alarmInterval, d=7, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      alarm(2) = ESMF_AlarmCreate(clock=clock, ringTime=alarmTime, &
                                  ringInterval=alarmInterval, rc=rc)
print *, "ESMF_AlarmCreate() alarm2 rc = ", rc

!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "alarm(2) = "
      call ESMF_AlarmPrint(alarm(2), rc=rc)

!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      clockCopy = ESMF_ClockCreate(clock, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Original clock = "
      call ESMF_ClockPrint(clock, "name", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Clock copy = "
      call ESMF_ClockPrint(clockCopy, "name", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      !
      ! See if alarms are associated with clock
      !
      call ESMF_ClockGetAlarm(clock, "Alarm002", tempAlarm, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "alarm fetched from clock = "

      call ESMF_AlarmPrint(tempAlarm, "name", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockGetAlarm(clock, "Example alarm 1", tempAlarm, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "alarm fetched from clock = "

      call ESMF_AlarmPrint(tempAlarm, "name", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_ALL, &
                                  alarmList, nAlarms, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "alarm list fetched from clock = "

      do i = 1, nAlarms

        call ESMF_AlarmPrint(alarmList(i), "name", rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      end do

#if 1
!
!  Example clock run 1:  Upon each clock advance (timestep), return a list
!                        of any ringing alarms.
!

      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))

        ! perform time step and get a list of any ringing alarms
        !call ESMF_ClockAdvance(clock, rc=rc)
        !call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_RINGING, &
        !                            ringingAlarm, ringingAlarmCount, rc=rc)
        call ESMF_ClockAdvance(clock, ringingAlarmList=ringingAlarm, &
                               ringingAlarmCount=ringingAlarmCount, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        call ESMF_ClockPrint(clock, "currTime string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        ! get what the clock's next time would be after one timestep
        call ESMF_ClockGetNextTime(clock, nextTime, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        print *, "clock next time after one timestep = "
        call ESMF_TimePrint(nextTime, "string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        ! get what the clock's next time would be after a timestep equal to 10
        ! clock timesteps
        call ESMF_ClockGetNextTime(clock, nextTime, 10*timeStep, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        print *, &
         "clock next time after one big timestep equal to 10 clock timesteps = "
        call ESMF_TimePrint(nextTime, "string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
        if (ringingAlarmCount > 0) then
          print *, "number of ringing alarms = ", ringingAlarmCount
        endif

        ! Process any ringing alarms in list returned by clock
        do i = 1, ringingAlarmCount

          call ESMF_AlarmPrint(ringingAlarm(i), "name", rc=rc)
!EOC

          if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC

          ! identify ringing alarm relative to clock's list
          do j = 1, NUMALARMS
            if (ringingAlarm(i) == alarm(j)) then
!EOC

            if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
              call ESMF_AlarmGet(alarm(j), name=name, rc=rc)
!EOC

              if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
              print *, trim(name), ", Alarm #", j, &
                                   " in clock's list is ringing!"
              if (j == 2) then
                ! get what alarm(2)'s next ring time will be
                call ESMF_AlarmGet(alarm(2), ringTime=ringTime, rc=rc)
!EOC

                 if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
                print *, "alarm(2)'s next ring time = "
                call ESMF_TimePrint(ringTime, "string", rc)
!EOC

                if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 

              endif

            end if
          end do

          ! after processing alarm, turn off interval alarm to
          ! prepare for next ring time
          call ESMF_AlarmRingerOff(ringingAlarm(i), rc)
!EOC

          if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 

        end do

        ! get previously ringing alarms
        call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_PREVRINGING, &
                                    ringingAlarm, ringingAlarmCount, rc=rc)

!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        do i = 1, ringingAlarmCount
          print *, "Previously ringing alarm = "
          call ESMF_AlarmPrint(ringingAlarm(i), "name", rc=rc)
!EOC

          if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        end do

        ! get next ringing alarms
        call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_NEXTRINGING, &
                                    ringingAlarm, ringingAlarmCount, rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        do i = 1, ringingAlarmCount
          print *, "Next ringing alarm = "
          call ESMF_AlarmPrint(ringingAlarm(i), "name", rc=rc)
!EOC

          if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        end do

      end do
#endif

#if 0
! 
!  Example clock run 2:  Upon each clock advance (timestep), return the
!                        number of ringing alarms, if any.  Then query
!                        each original alarm directly for ringing state.
!
      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
!EOC

          if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        ! perform time step 
        call ESMF_ClockAdvance(clock, ringingAlarmCount=ringingAlarmCount, &
                               rc=rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        call ESMF_ClockPrint(clock, "currTime string", rc)
!EOC

        if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
        ! check if alarms are ringing
        if (ringingAlarmCount > 0) then
          print *, "number of ringing alarms = ", ringingAlarmCount
          if (ESMF_AlarmIsRinging(alarm(1), rc)) then
!EOC

            if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
            print *, "Alarm(1) is ringing!"
            call ESMF_AlarmRingerOff(Alarm(1), rc)
!EOC

            if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
          end if

          if (ESMF_AlarmIsRinging(alarm(2), rc)) then
!EOC

            if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 

            print *, "Alarm(2) is ringing!"
            call ESMF_AlarmRingerOff(Alarm(2), rc)

!EOC

            if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
          end if
        end if

      end do
#endif

      ! destroy clock and alarms
      call ESMF_AlarmDestroy(alarm(1), rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
      call ESMF_AlarmDestroy(alarm(2), rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
      call ESMF_ClockDestroy(clock, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC 
      call ESMF_ClockDestroy(clock, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc)
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
