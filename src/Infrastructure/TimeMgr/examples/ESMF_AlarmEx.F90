! $Id: ESMF_AlarmEx.F90,v 1.3 2003/12/02 21:17:19 svasquez Exp $
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
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AlarmEx - Alarm example
!
! !DESCRIPTION:
!
! This program shows an example of how to set-up, run, and examine a basic clock
! with alarms
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AlarmEx.F90,v 1.3 2003/12/02 21:17:19 svasquez Exp $'
!------------------------------------------------------------------------------

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate Alarm lists (of pointers)
      integer, parameter :: NUMALARMS = 2
      type(ESMF_Alarm) :: alarm(NUMALARMS)
      integer :: numRingingAlarms  ! at any time step (0 to NUMALARMS)
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
      integer :: i, j, rc, finalrc
      finalrc = ESMF_SUCCESS

!
!  Setup clock
!
   
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize time interval to 1 day
      call ESMF_TimeIntervalSet(timeStep, d=1, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize start time to 9/1/2003
      call ESMF_TimeSet(startTime, yr=2003, mm=9, dd=1, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      ! initialize stop time to 9/30/2003
      call ESMF_TimeSet(stopTime, yr=2003, mm=9, dd=30, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      !
      ! initialize the clock with the above values
      !
      clock = ESMF_ClockCreate("The Clock", timeStep, startTime, stopTime, &
                               rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockPrint(clock, "name", rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!
!  Setup alarms
!

      !
      ! Initialize first alarm to be a one-shot on 9/15/2003 and associate
      ! it with the clock
      !
      call ESMF_TimeSet(alarmTime, yr=2003, mm=9, dd=15, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      name = "Example alarm 1"
      alarm(1) = ESMF_AlarmCreate(name, clock, ringTime=alarmTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "alarm(1) = "
      call ESMF_AlarmPrint(alarm(1), rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      !
      ! Initialize second alarm to ring on a 1 week interval from 9/1/2003 and
      ! associate it with the clock
      !
      call ESMF_TimeSet(alarmTime, yr=2003, mm=9, dd=1, &
                        calendar=gregorianCalendar, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_TimeIntervalSet(alarmInterval, d=7, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      alarm(2) = ESMF_AlarmCreate(clock=clock, ringTime=alarmTime, &
                                  ringInterval=alarmInterval, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "alarm(2) = "
      call ESMF_AlarmPrint(alarm(2), rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      !
      ! See if alarms are associated with clock
      !
      call ESMF_ClockGetAlarm(clock, "Alarm002", tempAlarm, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "alarm fetched from clock = "
      call ESMF_AlarmPrint(tempAlarm, "name", rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockGetAlarm(clock, "Example alarm 1", tempAlarm, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "alarm fetched from clock = "
      call ESMF_AlarmPrint(tempAlarm, "name", rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_ALL, &
                                  alarmList, nAlarms, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

      print *, "alarm list fetched from clock = "
      do i = 1, nAlarms
        call ESMF_AlarmPrint(alarmList(i), "name", rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

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
        !                            ringingAlarm, numRingingAlarms, rc=rc)
        call ESMF_ClockAdvance(clock, ringingAlarmList=ringingAlarm, &
                               numRingingAlarms=numRingingAlarms, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        call ESMF_ClockPrint(clock, "currTime string", rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        ! get what the clock's next time would be after one timestep
        call ESMF_ClockGetNextTime(clock, nextTime, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        print *, "clock next time after one timestep = "
        call ESMF_TimePrint(nextTime, "string", rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        ! get what the clock's next time would be after a timestep equal to 10
        ! clock timesteps
        call ESMF_ClockGetNextTime(clock, nextTime, 10*timeStep, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        print *, &
         "clock next time after one big timestep equal to 10 clock timesteps = "
        call ESMF_TimePrint(nextTime, "string", rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        if (numRingingAlarms > 0) then
          print *, "number of ringing alarms = ", numRingingAlarms
        endif

        ! Process any ringing alarms in list returned by clock
        do i = 1, numRingingAlarms

          call ESMF_AlarmPrint(ringingAlarm(i), "name", rc=rc)

          if (rc.NE.ESMF_SUCCESS) then
              finalrc = ESMF_FAILURE
          end if


          ! identify ringing alarm relative to clock's list
          do j = 1, NUMALARMS
            if (ringingAlarm(i) == alarm(j)) then

            if (rc.NE.ESMF_SUCCESS) then
                finalrc = ESMF_FAILURE
            end if

              call ESMF_AlarmGet(alarm(j), name=name, rc=rc)

              if (rc.NE.ESMF_SUCCESS) then
                  finalrc = ESMF_FAILURE
              end if

              print *, trim(name), ", Alarm #", j, &
                                   " in clock's list is ringing!"
              if (j == 2) then
                ! get what alarm(2)'s next ring time will be
                call ESMF_AlarmGet(alarm(2), ringTime=ringTime, rc=rc)

                 if (rc.NE.ESMF_SUCCESS) then
                     finalrc = ESMF_FAILURE
                 end if

                print *, "alarm(2)'s next ring time = "
                call ESMF_TimePrint(ringTime, "string", rc)

                if (rc.NE.ESMF_SUCCESS) then
                    finalrc = ESMF_FAILURE
                end if

              endif

            end if
          end do

          ! after processing alarm, turn off interval alarm to
          ! prepare for next ring time
          call ESMF_AlarmRingerOff(ringingAlarm(i), rc)

          if (rc.NE.ESMF_SUCCESS) then
              finalrc = ESMF_FAILURE
           end if


        end do

        ! get previously ringing alarms
        call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_PREVRINGING, &
                                    ringingAlarm, numRingingAlarms, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        do i = 1, numRingingAlarms
          print *, "Previously ringing alarm = "
          call ESMF_AlarmPrint(ringingAlarm(i), "name", rc=rc)

          if (rc.NE.ESMF_SUCCESS) then
              finalrc = ESMF_FAILURE
          end if

        end do

        ! get next ringing alarms
        call ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_NEXTRINGING, &
                                    ringingAlarm, numRingingAlarms, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        do i = 1, numRingingAlarms
          print *, "Next ringing alarm = "
          call ESMF_AlarmPrint(ringingAlarm(i), "name", rc=rc)

          if (rc.NE.ESMF_SUCCESS) then
              finalrc = ESMF_FAILURE
          end if

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

          if (rc.NE.ESMF_SUCCESS) then
              finalrc = ESMF_FAILURE
          end if

        ! perform time step 
        call ESMF_ClockAdvance(clock, numRingingAlarms=numRingingAlarms, rc=rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        call ESMF_ClockPrint(clock, "currTime string", rc)

        if (rc.NE.ESMF_SUCCESS) then
            finalrc = ESMF_FAILURE
        end if

        ! check if alarms are ringing
        if (numRingingAlarms > 0) then
          print *, "number of ringing alarms = ", numRingingAlarms
          if (ESMF_AlarmIsRinging(alarm(1), rc)) then

            if (rc.NE.ESMF_SUCCESS) then
                finalrc = ESMF_FAILURE
             end if

            print *, "Alarm(1) is ringing!"
            call ESMF_AlarmRingerOff(Alarm(1), rc)

            if (rc.NE.ESMF_SUCCESS) then
                finalrc = ESMF_FAILURE
             end if

          end if

          if (ESMF_AlarmIsRinging(alarm(2), rc)) then

            if (rc.NE.ESMF_SUCCESS) then
                finalrc = ESMF_FAILURE
            end if

            print *, "Alarm(2) is ringing!"
            call ESMF_AlarmRingerOff(Alarm(2), rc)

            if (rc.NE.ESMF_SUCCESS) then
                finalrc = ESMF_FAILURE
            end if

          end if
        end if

      end do
#endif

      ! destroy clock and alarms
      call ESMF_AlarmDestroy(alarm(1), rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
           finalrc = ESMF_FAILURE
       end if

      call ESMF_AlarmDestroy(alarm(2), rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
           finalrc = ESMF_FAILURE
       end if

      call ESMF_ClockDestroy(clock, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
           finalrc = ESMF_FAILURE
       end if

      if (finalrc.EQ.ESMF_SUCCESS) then
         print *, "PASS: ESMF_AlarmEx.F90"
      else
         print *, "FAIL: ESMF_AlarmEx.F90"
      end if

      end program ESMF_AlarmEx
