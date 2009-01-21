! $Id: ESMF_AlarmUTest.F90,v 1.32.2.9 2009/01/21 21:25:24 cdeluca Exp $
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
      program ESMF_AlarmTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_AlarmTest - Test Alarm functionalities
!
! !DESCRIPTION:
!
! The code in this file drives F90 Alarm unit tests.
!
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AlarmUTest.F90,v 1.32.2.9 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1, clock2, domainClock
      type(ESMF_Alarm) :: alarm, alarm1, alarm2, alarm3, alarm4, alarm6
      type(ESMF_Alarm) :: beforeAlarm, afterAlarm
      type(ESMF_Alarm) :: alarm5(200)
      type(ESMF_Alarm) :: alarmList(201)
      type(ESMF_Direction) :: forwardDirection, reverseDirection
      logical :: enabled, isringing, sticky, alarmsEqual, alarmsNotEqual
      logical :: willRingNext, testPass, alarmCountPass
      integer(ESMF_KIND_I8) :: forwardCount, reverseCount
      integer :: alarmCount, ringCount, nring, nclock, nstep, sstep
      integer :: expectedCount, i, iteration
      integer :: yy, mm, dd, h, m

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             no_leapCalendar, esmf_360dayCalendar

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, alarmStep, alarmStep2, ringDuration
      type(ESMF_TimeInterval) :: runDuration
      type(ESMF_Time) :: startTime, stopTime, nextTime, prevTime
      type(ESMF_Time) :: alarmTime, alarmStopTime
      type(ESMF_Time) :: beforeAlarmTime, afterAlarmTime
      type(ESMF_Time) :: currentTime, currentTime2
      character(ESMF_MAXSTR) :: aName


      ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! initialize one calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)

      ! initialize secand calendar to be Julian type
      julianCalendar = ESMF_CalendarCreate("Julian", ESMF_CAL_JULIANDAY, rc)

      ! initialize third calendar to be No Leap type
      no_leapCalendar = ESMF_CalendarCreate("NoLeap", ESMF_CAL_NOLEAP, rc)

      ! initialize third calendar to be 360 day type
      esmf_360dayCalendar = ESMF_CalendarCreate("360Day", ESMF_CAL_360DAY, rc)

!-------------------------------------------------------------------------------
!    The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!    always run. When the environment variable, EXHAUSTIVE, is set to ON then
!    the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!    to OFF, then only the sanity unit tests.
!    Special strings (Non-exhaustive and exhaustive) have been
!    added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------


      ! initialize clock time intervals and instants

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting Time Step
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=13, &
                             	   h=18, m=45, s=27, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=24, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Alarm Time Initialization Test"
      call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=15, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Clock Initialization Test"
      clock1 = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                               stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Test"
      alarm1 = ESMF_AlarmCreate(name="WAKEUP", clock=clock1, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroy Alarm Test"
      call ESMF_AlarmDestroy(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      

#ifdef ESMF_TESTEXHAUSTIVE
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed Alarm Test"
      call ESMF_AlarmDestroy(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a non-created Alarm Test"
      call ESMF_AlarmDestroy(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed Alarm is sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created Alarm is sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Validate destroyed Alarm Test"
      call ESMF_AlarmValidate(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Validate non-created Alarm Test"
      call ESMF_AlarmValidate(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Get name of destroyed Alarm Test"
      call  ESMF_AlarmGet(alarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Get name of destroyed Alarm Test"
      call  ESMF_AlarmGet(alarm6, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Set  name of destroyed Alarm Test"
      call  ESMF_AlarmSet(alarm1, name="ALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Set  name of non-created Alarm Test"
      call  ESMF_AlarmSet(alarm6, name="ALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Turn on Ringing on destroyed Alarm "
      call ESMF_AlarmRingerOn(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Turn on Ringing on non-created Alarm "
      call ESMF_AlarmRingerOn(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Turn off Ringing on destroyed Alarm "
      call ESMF_AlarmRingerOff(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Turn off Ringing on non-created Alarm "
      call ESMF_AlarmRingerOff(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Enable destroyed Alarm Test"
      call ESMF_AlarmEnable(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Enable non-create Alarm Test"
      call ESMF_AlarmEnable(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Disable destroyed Alarm Test"
      call ESMF_AlarmDisable(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Disable non-created Alarm Test"
      call ESMF_AlarmDisable(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test if destroyed Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroyed Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm1, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test if non-created Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Non-created Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm6, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Turn off Ringing on destroyed Alarm "
      call ESMF_AlarmRingerOff(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Turn off Ringing on non-created Alarm "
      call ESMF_AlarmRingerOff(alarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test destroyed Alarm will ring next
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroyed Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm1, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test non-created Alarm will ring next
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Non-created Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm6, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)



      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Test"
      alarm1 = ESMF_AlarmCreate(name="WAKEUP", clock=clock1, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Test"
      alarm3 = ESMF_AlarmCreate(name="WAKEUP", clock=clock1, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Copy Test"
      alarm2 = ESMF_AlarmCreate(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and alarmCount = 3"
      write(name, *) "Clock Get Alarm List Test"
      call ESMF_ClockGetAlarmList(clock1, ESMF_ALARMLIST_ALL, alarmList, &
                                  alarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.alarmCount.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "alarmCount = ", alarmCount

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for alarm equality
      ! alarmsEqual = ESMF_AlarmOperator(==)(alarm1,alarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarms Equal Test"
      alarmsEqual = (alarm1 == alarm2)
      call ESMF_Test((alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing for alarm inequality:
      ! alarmsEqual = ESMF_AlarmOperator(==)(alarm1,alarm3)
      write(failMsg, *) "Returned equal"
      write(name, *) "Alarms Not Equal Test"
      alarmsEqual = (alarm1 == alarm3)
      call ESMF_Test((.not.alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing alarms equality using:
      ! alarmsNotEqual = ESMF_AlarmOperator(/=)(alarm1,alarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarms Equal Test"
      alarmsNotEqual = (alarm1 /= alarm2)
      call ESMF_Test((.not.alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Testing alarms inequality using:
      ! alarmsNotEqual = ESMF_AlarmOperator(/=)(alarm1,alarm3)
      write(failMsg, *) "Returned equal"
      write(name, *) "Alarms Not Equal Test"
      alarmsNotEqual = (alarm1 /= alarm3)
      call ESMF_Test((alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Validate Alarm Test"
      call ESMF_AlarmValidate(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not sticky"
      write(name, *) "Check if Alarm is sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(sticky), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not enabled"
      write(name, *) "Check if Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Alarm not sticky Test"
      call  ESMF_AlarmNotSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Get Alarm name Test"
      call  ESMF_AlarmGet(alarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."WAKEUP"), &
                      name, failMsg, result, ESMF_SRCLINE)
	print *, "Alarm name is ", aName


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Set Alarm name Test"
      call  ESMF_AlarmSet(alarm1, name="ALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Get Alarm name Test"
      call  ESMF_AlarmGet(alarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."ALARM1"), &
                      name, failMsg, result, ESMF_SRCLINE)
        print *, "Alarm name is ", aName


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned sticky"
      write(name, *) "Check if Alarm is not sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.sticky), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Alarm sticky Test"
      call  ESMF_AlarmSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not sticky"
      write(name, *) "Check if Alarm is sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(sticky), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or is ringing"
      write(name, *) "Check if Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn on Ringing Alarm "
      call ESMF_AlarmRingerOn(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its not ringing"
      write(name, *) "Check if Alarm is ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn off Ringing Alarm "
      call ESMF_AlarmRingerOff(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its ringing"
      write(name, *) "Check if Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Enable Alarm Test"
      call ESMF_AlarmEnable(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned enabled"
      write(name, *) "Check if Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Alarm Print Test"
      call  ESMF_AlarmPrint(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Disable Alarm Test"
      call ESMF_AlarmDisable(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned enabled"
      write(name, *) "Check if Alarm is disabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroy Alarm Test"
      call ESMF_AlarmDestroy(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Time Step
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval Initialization Test"
      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Start Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=13, &
                                   h=1, m=45, s=27, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=13, &
                                   h=18, m=45, s=27, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Alarm Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Alarm Time Initialization Test"
      call ESMF_TimeSet(alarmTime, yy=2003, mm=3, dd=13, h=5, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      ! Initialize clock 
      !EX_UTest
       write(name, *) "Clock Initialization Test"
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                                          stopTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Sticky Alarm Time Initialization Test"
      alarm =  ESMF_AlarmCreate(name="alarm1", clock=clock, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Get Alarm Test"
      call ESMF_ClockGetAlarm(clock, name="alarm1", alarm=alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Get Clock Next Time
      write(failMsg, *) " Did not return ESMF_SUCCESS)"
      write(name, *) "Get Clock Next Time Test"
      call ESMF_ClockGetNextTime(clock, nextTime, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Get Time from clock
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Get Clock Current Time Test"
      call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test that Clock Get Next Time Passed
      write(failMsg, *) " Next Time not equal to current Time"
      write(name, *) "Get Clock Next Time Test"
      bool = (nextTime == currentTime) 
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is still ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
    ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

	print *, "bool is ", bool


      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is still ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn off Ringing Alarm "
      call ESMF_AlarmRingerOff(alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its ringing"
      write(name, *) "Check if Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Initialize clock 
      !EX_UTest
       write(name, *) "Clock Initialization Test"
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                                          stopTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Time Initialization Test"
      alarm =  ESMF_AlarmCreate(name="alarm1", clock=clock, &
					ringTime=alarmTime, sticky=.FALSE., rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is still ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
    ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

	print *, "bool is ", bool


      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Non-Sticky Alarms 1
      !  from Chris Hill via support issue 988241, bug 996229
      write(failMsg, *) " Did not return nstep=48, sstep=73, i=144, and ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Test 1"
      call ESMF_TimeIntervalSet(timeStep, s=100, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, s=150, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=3, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, sticky=.false., rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! starting time step number for first alarm ring
      sstep = 0

      ! total clock time steps
      i = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        i = i + 1
        call ESMF_ClockGetAlarmList(clock2, ESMF_ALARMLIST_RINGING, &
                                    alarmList, alarmCount, rc=rc)
        if (alarmCount .gt. 0) then
          if (sstep .eq. 0) sstep = i
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS) &
                      .and.(nstep.eq.48).and.(sstep.eq.73).and.(i.eq.144), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Non-Sticky Alarms 2
      !  Test ringDuration and stopTime
      write(failMsg, *) " Did not return nstep=12 and ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Test 2"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, h=2, rc=rc)
      call ESMF_TimeIntervalSet(ringDuration, m=30, rc=rc)
      call ESMF_TimeSet(alarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, &
                                ringDuration=ringDuration, &
                                stopTime=alarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_AlarmIsRinging(alarm4)) then
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.12), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky Alarms 3
      !  Test ringTimeStepCount and stopTime
      write(failMsg, *) " Did not return nstep=18 and ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Test 3"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, h=2, rc=rc)
      call ESMF_TimeSet(alarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, &
                                ringTimeStepCount=3, &
                                stopTime=alarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_AlarmIsRinging(alarm4)) then
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky Alarms 4
      !  Test ringTimeStepCount precedence over ringDuration
      write(failMsg, *) " Did not return nstep=18 and ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Test 4"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, h=2, rc=rc)

      call ESMF_TimeIntervalSet(ringDuration, m=30, rc=rc)

      call ESMF_TimeSet(alarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, &
                                ringTimeStepCount=3, &
                                ringDuration=ringDuration, &
                                stopTime=alarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_AlarmIsRinging(alarm4)) then
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Non-Sticky Alarms 5
      !  Test reverse non-sticky alarm 
      write(failMsg, *) "Did not return nstep=24, forwardCount=96, reverseCount=0, etc., and ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Test 5"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, h=2, rc=rc)
      call ESMF_TimeIntervalSet(ringDuration, m=30, rc=rc)
      call ESMF_TimeSet(alarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, &
                                ringDuration=ringDuration, &
                                stopTime=alarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          if (ESMF_AlarmIsRinging(alarm4)) then
            nstep = nstep + 1
            !print *, "on"
          else
            !print *, "off"
          endif
          call ESMF_ClockAdvance(clock2, rc=rc)
        enddo

        if (nclock.eq.0) then
          call ESMF_ClockGet(clock2, direction=forwardDirection, &
                                     advanceCount=forwardCount, rc=rc)
          !print *, "forwardCount = ", forwardCount
        else
          call ESMF_ClockGet(clock2, direction=reverseDirection, &
                             advanceCount=reverseCount, rc=rc)
          !print *, "reverseCount = ", reverseCount
        endif

        call ESMF_ClockSet(clock2, direction=ESMF_MODE_REVERSE, rc=rc)
        nclock = nclock + 1


      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.24).and. &
                     (forwardCount.eq.96).and.(reverseCount.eq.0).and. &
                     (forwardDirection.eq.ESMF_MODE_FORWARD).and. &
                     (reverseDirection.eq.ESMF_MODE_REVERSE).and. &
                     ESMF_ClockIsReverse(clock2), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nstep = ", nstep, " nclock = ", nclock

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Sticky Alarms 1
      !  Test reverse sticky alarm 
      write(failMsg, *) " Did not return nstep=2 and ESMF_SUCCESS"
      write(name, *) "Sticky Alarm Test 1"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          if (ESMF_AlarmIsRinging(alarm4)) then
            nstep = nstep + 1
            !print *, "on"
            call ESMF_AlarmRingerOff(alarm4, rc=rc)
          else
            !print *, "off"
          endif
          call ESMF_ClockAdvance(clock2, rc=rc)
        enddo

        call ESMF_ClockSet(clock2, direction=ESMF_MODE_REVERSE, rc=rc)
        nclock = nclock + 1
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.2), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "nstep = ", nstep, " nclock = ", nclock

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Sticky Alarms 2
      !  Test reverse interval sticky alarm 
      write(failMsg, *) " Did not return nstep=23 and ESMF_SUCCESS"
      write(name, *) "Sticky Alarm Test 2"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, h=2, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, rc=rc)
      ! number of clock time steps alarm rings for
      nstep = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          if (ESMF_AlarmIsRinging(alarm4)) then
            nstep = nstep + 1
            !print *, "on"
            call ESMF_AlarmRingerOff(alarm4, rc=rc)
          else
            !print *, "off"
          endif
          call ESMF_ClockAdvance(clock2, rc=rc)
        enddo

        call ESMF_ClockSet(clock2, direction=ESMF_MODE_REVERSE, rc=rc)
        nclock = nclock + 1
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.23), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nstep = ", nstep, " nclock = ", nclock

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Sticky Alarms 3
      !  Test reverse one-shot sticky alarms per WRF use case
      !  From Tom Henderson/WRF

      write(name, *) "Sticky Alarm Test 3"
      write(failMsg, *) " Alarms did not turn on/off at the correct time/iteration or did not return ESMF_SUCCESS"

      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_TimeSet(startTime, yy=2005, mm=6, dd=15, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2005, mm=6, dd=15, h=5, &
                        calendar=gregorianCalendar, rc=rc)
      domainClock = ESMF_ClockCreate("WRF Clock", &
                                     timeStep, startTime, stopTime, rc=rc)

      call ESMF_TimeSet(beforeAlarmTime, yy=2005, mm=6, dd=15, h=2, &
                        calendar=gregorianCalendar, rc=rc)
      beforeAlarm = ESMF_AlarmCreate(clock=domainClock, &
                                     ringTime=beforeAlarmTime, rc=rc)

      call ESMF_TimeSet(afterAlarmTime, yy=2005, mm=6, dd=15, h=3, &
                        calendar=gregorianCalendar, rc=rc)
      afterAlarm  = ESMF_AlarmCreate(clock=domainClock, &
                                     ringTime=afterAlarmTime, rc=rc)

      ! any single failure will cause the whole test to fail
      testPass = .true.

      ! track loop iterations
      iteration = 0

      call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
      call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
      print *, "Begin"
      print *, mm, "/", dd, "/", yy, " ", h, ": 0"

      ! run the clock forward
      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        iteration = iteration + 1
        print *, "Iteration = ", iteration

        if (ESMF_AlarmIsRinging(beforeAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 2) then
            testPass = .false.
          endif
          print *, "  beforeAlarm on"
          call ESMF_AlarmRingerOff(beforeAlarm, rc=rc)
        else
          print *, "  beforeAlarm off"
        endif

        ! would call WRF solver here, before clock advance
        print *, "  Solve"

        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        print *, " ClockAdvance()"
        print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        if (ESMF_AlarmIsRinging(afterAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 3) then
            testPass = .false.
          endif
          print *, "  afterAlarm on"
          call ESMF_AlarmRingerOff(afterAlarm, rc=rc)
        else
          print *, "  afterAlarm off"
        endif
      enddo

      ! run the clock backwards
      call ESMF_ClockSet(domainClock, direction=ESMF_MODE_REVERSE, rc=rc)
      print *
      print *, "domainClock set in reverse"
      print *

      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        print *, "Iteration = ", iteration

        if (ESMF_AlarmIsRinging(afterAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 3) then
            testPass = .false.
          endif
          print *, "  afterAlarm on"
          call ESMF_AlarmRingerOff(afterAlarm, rc=rc)
        else
          print *, "  afterAlarm off"
        endif

        ! advance clock
        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        print *, " ClockAdvance()"
        print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        ! would call WRF solver here, after clock advance
        print *, "  Solve"

        if (ESMF_AlarmIsRinging(beforeAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 2) then
            testPass = .false.
          endif
          print *, "  beforeAlarm on"
          call ESMF_AlarmRingerOff(beforeAlarm, rc=rc)
        else
          print *, "  beforeAlarm off"
        endif

        iteration = iteration - 1
      enddo

      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(afterAlarm, rc=rc)
      call ESMF_AlarmDestroy(beforeAlarm, rc=rc)
      call ESMF_ClockDestroy(domainClock, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Sticky Alarms 4
      !  Test reverse *interval* sticky alarms; variation on WRF use case above

      write(name, *) "Sticky Alarm Test 4"
      write(failMsg, *) " Alarms did not turn on/off at the correct time/iteration or did not return ESMF_SUCCESS"

      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_TimeSet(startTime, yy=2005, mm=6, dd=15, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2005, mm=6, dd=15, h=10, &
                        calendar=gregorianCalendar, rc=rc)
      domainClock = ESMF_ClockCreate("WRF Clock", &
                                     timeStep, startTime, stopTime, rc=rc)

      call ESMF_TimeSet(beforeAlarmTime, yy=2005, mm=6, dd=15, h=2, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, h=3, rc=rc)
      beforeAlarm = ESMF_AlarmCreate(clock=domainClock, &
                                     ringTime=beforeAlarmTime, &
                                     ringInterval=alarmStep, rc=rc)

      call ESMF_TimeSet(afterAlarmTime, yy=2005, mm=6, dd=15, h=3, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep2, h=2, rc=rc)
      afterAlarm  = ESMF_AlarmCreate(clock=domainClock, &
                                     ringTime=afterAlarmTime, &
                                     ringInterval=alarmStep2, rc=rc)

      ! any single failure will cause the whole test to fail
      testPass = .true.

      ! track loop iterations
      iteration = 0

      call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
      call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
      print *, "Begin"
      print *, mm, "/", dd, "/", yy, " ", h, ": 0"

      ! run the clock forward
      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        iteration = iteration + 1
        print *, "Iteration = ", iteration

        if (ESMF_AlarmIsRinging(beforeAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 2).and. &
              (iteration .ne. 5 .or. h .ne. 5).and. &
              (iteration .ne. 8 .or. h .ne. 8)) then
            testPass = .false.
          endif
          print *, "  beforeAlarm on"
          call ESMF_AlarmRingerOff(beforeAlarm, rc=rc)
        else
          print *, "  beforeAlarm off"
        endif

        ! would call WRF solver here, before clock advance
        print *, "  Solve"

        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        print *, " ClockAdvance()"
        print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        if (ESMF_AlarmIsRinging(afterAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 3).and. &
              (iteration .ne. 4 .or. h .ne. 5).and. &
              (iteration .ne. 6 .or. h .ne. 7).and. &
              (iteration .ne. 8 .or. h .ne. 9)) then
            testPass = .false.
          endif
          print *, "  afterAlarm on"
          call ESMF_AlarmRingerOff(afterAlarm, rc=rc)
        else
          print *, "  afterAlarm off"
        endif
      enddo

      ! run the clock backwards
      call ESMF_ClockSet(domainClock, direction=ESMF_MODE_REVERSE, rc=rc)
      print *
      print *, "domainClock set in reverse"
      print *

      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        print *, "Iteration = ", iteration

        if (ESMF_AlarmIsRinging(afterAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 3).and. &
              (iteration .ne. 4 .or. h .ne. 5).and. &
              (iteration .ne. 6 .or. h .ne. 7).and. &
              (iteration .ne. 8 .or. h .ne. 9)) then
            testPass = .false.
          endif
          print *, "  afterAlarm on"
          call ESMF_AlarmRingerOff(afterAlarm, rc=rc)
        else
          print *, "  afterAlarm off"
        endif

        ! advance clock
        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        print *, " ClockAdvance()"
        print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        ! would call WRF solver here, after clock advance
        print *, "  Solve"

        if (ESMF_AlarmIsRinging(beforeAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 2).and. &
              (iteration .ne. 5 .or. h .ne. 5).and. &
              (iteration .ne. 8 .or. h .ne. 8)) then
            testPass = .false.
          endif
          print *, "  beforeAlarm on"
          call ESMF_AlarmRingerOff(beforeAlarm, rc=rc)
        else
          print *, "  beforeAlarm off"
        endif

        iteration = iteration - 1
      enddo

      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(afterAlarm, rc=rc)
      call ESMF_AlarmDestroy(beforeAlarm, rc=rc)
      call ESMF_ClockDestroy(domainClock, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Fractional Time Alarms 1
      write(failMsg, *) " Did not return nstep=8, and ESMF_SUCCESS"
      write(name, *) "Fractional Time Alarm Test 1"
      call ESMF_TimeIntervalSet(timeStep, ms=10, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, ms=100, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        ms=200, calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, rc=rc)

      ! number of clock time steps alarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_AlarmIsRinging(alarm4)) then
          nstep = nstep + 1
          call ESMF_AlarmRingerOff(alarm4)
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.8), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Fractional Time Alarms 2
      write(failMsg, *) " Did not return nstep=4, and ESMF_SUCCESS"
      write(name, *) "Fractional Time Alarm Test 2"
      call ESMF_TimeIntervalSet(timeStep, sN=-22, sD=7, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, sN=-44, sD=7, rc=rc)
      call ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=1999, mm=12, dd=31, h=23, m=59, s=28, &
                        sN=4, sD=7, calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=53, &
                        sN=2, sD=7, calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                ringInterval=alarmStep, rc=rc)

      print *, "stopTime = "
      call ESMF_TimePrint(stopTime)
      print *, "alarmTime = "
      call ESMF_TimePrint(alarmTime)
      print *, "timeStep = "
      call ESMF_TimeIntervalPrint(timeStep)
      print *, "alarmStep = "
      call ESMF_TimeIntervalPrint(alarmStep)

      ! number of clock time steps alarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_AlarmIsRinging(alarm4)) then
          nstep = nstep + 1
          call ESMF_AlarmRingerOff(alarm4)
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.4), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime = clock startTime => should ring immediately
      !  upon alarm creation.
      write(failMsg, *) " Did not return alarm ringing and ESMF_SUCCESS"
      write(name, *) "Test Alarm ringTime = Clock startTime"
      call ESMF_TimeIntervalSet(timeStep, ms=10, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=startTime, rc=rc)

      bool = ESMF_AlarmIsRinging(alarm4)

      call ESMF_Test((bool.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime = clock startTime *and* ringInterval specified
      !  upon alarm creation => should ring immediately.  Test 1
      !  From Tom Black in Support ticket 1989990.
      write(failMsg, *) " Did not return alarm ringing and ESMF_SUCCESS"
      write(name, *) "Alarm ringTime = Clock startTime with ringInterval, test 1"
      call ESMF_TimeIntervalSet(timeStep, m=2, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, s=60, rc=rc)
      call ESMF_TimeSet(startTime, yy=2007, mm=9, dd=18, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2007, mm=9, dd=19, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      alarm4 = ESMF_AlarmCreate(clock=clock2, ringTime=startTime, &
                                ringInterval=alarmStep, rc=rc)

      call ESMF_AlarmGet(alarm4, ringTime=alarmTime, rc=rc)
      bool = ESMF_AlarmIsRinging(alarm4)

      call ESMF_Test(((alarmTime==startTime+alarmStep).and.bool.and. &
                       rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime = clock startTime *and* ringInterval specified
      !  upon alarm creation => should ring immediately.  Test 2
      !  From Tom Black in Support ticket 1989990.
      write(failMsg, *) " Did not return alarm ringing 21 times and ESMF_SUCCESS"
      write(name, *) "Alarm ringTime = Clock startTime with ringInterval, test 2"

      ! any single failure will cause the whole test to fail
      testPass = .true.

      call ESMF_TimeIntervalSet(timeStep, s=60, rc=rc)
      call ESMF_TimeIntervalSet(runDuration, s=3600, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, s=180, rc=rc)
      call ESMF_TimeSet(startTime, yy=2007, mm=9, dd=18, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, &
                              runDuration=runDuration, rc=rc)

      ! Tom Black's AlarmCreate() ...
      alarm4=ESMF_AlarmCreate(       &
         name             ='ALARM Recv from Parent'  &  !<-- Name of Alarm
        ,clock            =clock2    &  !<-- Each domain's ATM Driver Clock
        ,ringTime         =startTime &  !<-- First time the Alarm rings (ESMF)
        ,ringInterval     =alarmStep &  !<-- Recv from my parent at this
                                        !    frequency (ESMF)
        ,ringTimeStepCount=1       &  !<-- The Alarm rings for this many
                                      !    timesteps
        ,sticky           =.false. &  !<-- Alarm does not ring until turned off
        ,rc               =rc)

      ! run the clock
      ringCount = 0
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        call ESMF_ClockGet(clock2, advanceCount=forwardCount, rc=rc)        
        !print *, "At clock timestep #", forwardCount

        call ESMF_AlarmGet(alarm4, clock=clock1, rc=rc)
        call ESMF_ClockGet(clock1, currTime=currentTime2, rc=rc)
        !print *, "Alarm's clock's currTime ..."
        !call ESMF_TimePrint(currentTime2, "string", rc=rc)
        if (clock2 /= clock1) then
           testPass = .false.
           print *, "Alarm's clock and domain clock are *not* the same"
        endif

        call ESMF_ClockGet(clock2, currTime=currentTime, prevTime=prevTime, &
                           rc=rc)
        !print *, "Clock's currTime and prevTime ..."
        !call ESMF_TimePrint(currentTime, "string", rc=rc)
        !call ESMF_TimePrint(prevTime, "string", rc=rc)

        if (currentTime /= currentTime2) then
           testPass = .false.
           print *, "Alarm's clock's currTime is *not* the same as the domain clock's currTime"
        endif

        call ESMF_AlarmGet(alarm4, ringTime=alarmTime, rc=rc)
        !print *, "Alarm's ringTime ..."
        !call ESMF_TimePrint(alarmTime, "string", rc=rc)

        if (ESMF_AlarmIsRinging(alarm=alarm4, rc=rc)) then
            ringCount = ringCount + 1
            !print *, " Alarm is ringing"
            if (alarmTime /= currentTime+alarmStep) then
               testPass = .false.
               print *, "  Alarm ringTime *not* equal to clock currTime + alarmStep"
            endif
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_ClockGet(clock2, advanceCount=forwardCount, rc=rc)        
      print *, "End of clock run: At clock timestep #", forwardCount
      if (ESMF_AlarmIsRinging(alarm=alarm4, rc=rc)) then
          !print *, " Alarm is ringing"
          ringCount = ringCount + 1
      endif

      ! Alarm should have rung 21 times, including from timestep 0 (upon alarm
      ! creation) through timestep 60 (end of clock run) inclusive.
      ! Final ringTime should be one alarmStep past the clock end time.
      call ESMF_AlarmGet(alarm4, ringTime=alarmTime, rc=rc)
      call ESMF_Test(((alarmTime==startTime+runDuration+alarmStep).and. &
                       ringCount==21.and.testPass.and. &
                       rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, "Alarm rang ", ringCount, " times."
      !print *, "Alarm's final ringTime, after final clockAdvance() ..."
      !call ESMF_TimePrint(alarmTime, "string", rc=rc)

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime = clock startTime *and* ringInterval specified
      !  upon alarm creation => should ring immediately.  Test 3, test REVERSE
      !  back to clock starttime.  Robustness test for solution to
      !  Tom Black problem in Support ticket 1989990.
      write(failMsg, *) " Did not return alarm ringing 41 times and ESMF_SUCCESS"
      write(name, *) "Alarm ringTime = Clock startTime with ringInterval, test 3 (reverse)"

      call ESMF_TimeIntervalSet(timeStep, s=60, rc=rc)
      call ESMF_TimeIntervalSet(runDuration, s=3600, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, s=180, rc=rc)
      call ESMF_TimeSet(startTime, yy=2007, mm=9, dd=18, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, &
                              runDuration=runDuration, rc=rc)

      ! like Tom Black's AlarmCreate(), but sticky
      alarm4=ESMF_AlarmCreate(       &
         name             ='ALARM Recv from Parent'  &  !<-- Name of Alarm
        ,clock            =clock2    &  !<-- Each domain's ATM Driver Clock
        ,ringTime         =startTime &  !<-- First time the Alarm rings (ESMF)
        ,ringInterval     =alarmStep &  !<-- Recv from my parent at this
                                        !    frequency (ESMF)
        ,rc               =rc)

      ! number of clock time steps alarm rings for
      nring = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          if (ESMF_AlarmIsRinging(alarm4)) then
            nring = nring + 1
            !print *, "on"
            call ESMF_AlarmRingerOff(alarm4, rc=rc)
          else
            !print *, "off"
          endif
          call ESMF_ClockAdvance(clock2, rc=rc)
        enddo

        if (nclock.eq.0) then
          call ESMF_ClockGet(clock2, direction=forwardDirection, &
                                     advanceCount=forwardCount, rc=rc)
          print *, "forwardCount = ", forwardCount
        else
          call ESMF_ClockGet(clock2, direction=reverseDirection, &
                             advanceCount=reverseCount, rc=rc)
          print *, "reverseCount = ", reverseCount
        endif

        call ESMF_ClockSet(clock2, direction=ESMF_MODE_REVERSE, rc=rc)
        nclock = nclock + 1
      enddo

      ! count ring at the end point
      if (ESMF_AlarmIsRinging(alarm4)) then
        nring = nring + 1
        !print *, "on"
        call ESMF_AlarmRingerOff(alarm4, rc=rc)
      else
        !print *, "off"
      endif

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nring.eq.41).and. &
                     (forwardCount.eq.60).and.(reverseCount.eq.0).and. &
                     (forwardDirection.eq.ESMF_MODE_FORWARD).and. &
                     (reverseDirection.eq.ESMF_MODE_REVERSE).and. &
                     ESMF_ClockIsReverse(clock2), &
                      name, failMsg, result, ESMF_SRCLINE)

      print *, "nring = ", nring, " nclock = ", nclock

      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm list re-allocation within clock, part 1
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Alarm list reallocation Test 1"
      call ESMF_TimeIntervalSet(timeStep, ms=10, rc=rc)
      call ESMF_TimeIntervalSet(alarmStep, ms=100, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate("Clock 2", timeStep, startTime, stopTime, rc=rc)
      call ESMF_TimeSet(alarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        ms=200, calendar=gregorianCalendar, rc=rc)

      ! fill up clock's alarmList
      do i=1,200
        alarm5(i) = ESMF_AlarmCreate(clock=clock2, ringTime=alarmTime, &
                                     ringInterval=alarmStep, rc=rc)
      enddo

      ! add one more alarm than there is space for (200), forcing a
      !   reallocation to 400 alarms
      ! also, set 201st alarm to be the first to ring upon the 1st timestep
      call ESMF_TimeSet(alarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        ms=10, calendar=gregorianCalendar, rc=rc)
      alarm4 = ESMF_AlarmCreate(name="201st Alarm", clock=clock2, &
                                ringTime=alarmTime, ringInterval=alarmStep, &
                                rc=rc)

      ! see if the 201st alarm was successfully added to the clock
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm list re-allocation within clock, part 2
      write(failMsg, *) " Did not return 201 alarms and name '201st Alarm'"
      write(name, *) "Alarm list reallocation Test 2"
      call ESMF_ClockGetAlarmList(clock2, ESMF_ALARMLIST_ALL, alarmList, &
                                  alarmCount, rc=rc)
      write(*,*) "rc=",rc
      call ESMF_AlarmGet(alarmList(alarmCount), name=aName, rc=rc)

      print *, "alarmCount = ", alarmCount
      print *, "201st alarm name = ", aName
      write(*,*) "rc=",rc

      ! see if we have 201 alarms and if the 201st alarm has the right name!
      call ESMF_Test((alarmCount.eq.201).and.(aName.eq."201st Alarm") &
                      .and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm list re-allocation within clock, part 3
      write(failMsg, *) " Did not return 201st alarm ringing"
      write(name, *) "Alarm list reallocation Test 3"
      call ESMF_ClockAdvance(clock2, rc=rc)
      call ESMF_ClockGetAlarmList(clock2, ESMF_ALARMLIST_RINGING, alarmList, &
                                  alarmCount, rc=rc)
      ! double check ringing with Alarm API call
      isringing = ESMF_AlarmIsRinging(alarm4, rc=rc)

      print *, "alarmCount = ", alarmCount
      call ESMF_AlarmGet(alarmList(alarmCount), name=aName, rc=rc)
      print *, "Ringing alarm name = ", trim(aName), ", is ringing = ", &
            isringing

      ! see if the 201st alarm is the only one ringing
      call ESMF_Test(isringing.and.(alarmCount.eq.1) &
                     .and.(aName.eq."201st Alarm") &
                     .and.(rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! cleanup
      do i=1,200
        call ESMF_AlarmDestroy(alarm5(i), rc=rc)
      enddo
      call ESMF_AlarmDestroy(alarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime increment, first forwards a fixed number of
      !timesteps, stopping at an alarm ringing time step.
      !Using ESMF_MODE_REVERSE, step backwards to some time prior to the
      !clock's startTime.  Then go ESMF_MODE_FORWARD to one step past an
      !alarm ringing time step, and then ESMF_MODE_REVERSE once more. 
      ! Count number of rings.  See bug #1531948.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test ESMF_MODE_FORWARD to an alarm point, ESMF_MODE_REVERSE, ESMF_MODE_FORWARD, ESMF_MODE_REVERSE"

      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      !call ESMF_TimePrint (startTime, "string isofrac", rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      clock = ESMF_ClockCreate (  &
          name = "clock 1",  &
          startTime = startTime, timeStep=timeStep, rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (alarmTime, yy=2008, mm=1, dd=23, h=6,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (alarmStep, h=6, rc=rc)
      alarm1 = ESMF_AlarmCreate (  &
          name="Alarm 1", clock=clock,  &
          ringTime=alarmTime, ringInterval=alarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      alarmCount = 0
      expectedCount = 11
      do, i=1,6
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at forwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at forwards timestep', i
        end if
      end do

      print *, 'SETTING CLOCK BACKWARDS'
      call ESMF_ClockSet (clock, direction=ESMF_MODE_REVERSE, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=5, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at backwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
          !call ESMF_AlarmPrint(alarm1, "ringTime string")
        else
          !print *, 'alarm not ringing at backwards timestep', i
          ! call ESMF_ClockPrint (clock, options="currTime string")
        end if
      end do

      print *, 'SETTING CLOCK FORWARDS'
      call ESMF_ClockSet (clock, direction=ESMF_MODE_FORWARD, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=-4,7
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at forwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at forwards timestep', i
        end if
      end do

      print *, 'SETTING CLOCK BACKWARDS'
      call ESMF_ClockSet (clock, direction=ESMF_MODE_REVERSE, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=6, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at backwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
          !call ESMF_AlarmPrint(alarm1, "ringTime string")
        else
          !print *, 'alarm not ringing at backwards timestep', i
          ! call ESMF_ClockPrint (clock, options="currTime string")
        end if
      end do

      if (.not. testPass .or. alarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', alarmCount = ', alarmCount, ', expected = ', expectedCount
          print *, 'The alarm ringTime may be stuck at:'
          call ESMF_AlarmPrint (alarm1, "ringTime string")
      end if

      call ESMF_Test (testPass .and. alarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy (alarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime increment, first forwards a fixed number of
      !timesteps, stopping at an alarm ringing time step.  Using a negative
      !timestemp, step backwards to some time prior to the clock's startTime.
      !Count number of rings.  See bugs #1531948, #1457135.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test forward to an alarm point, then step backward using a negative timeStep"

      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      clock = ESMF_ClockCreate (  &
          name = "clock 1",  &
          startTime = startTime, timeStep=timeStep, rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (alarmTime, yy=2008, mm=1, dd=23, h=6,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (alarmStep, h=6, rc=rc)
      alarm1 = ESMF_AlarmCreate (  &
          name="Alarm 1", clock=clock,  &
          ringTime=alarmTime, ringInterval=alarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      alarmCount = 0
      expectedCount = 8
      do, i=1,6
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at forwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at forwards timestep', i
        end if
      end do

      print *, 'SETTING CLOCK BACKWARDS WITH NEGATIVE TIMESTEP'
      timeStep = -timeStep
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=5, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at backwards timestep', i, ', time:'
          ! call ESMF_ClockPrint (clock, options="currTime string")
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at backwards timestep', i
          ! call ESMF_ClockPrint (clock, options="currTime string")
        end if
      end do

      if (.not. testPass .or. alarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', alarmCount = ', alarmCount, ', expected = ', expectedCount
          print *, 'The alarm ringTime may be stuck at:'
          call ESMF_AlarmPrint (alarm1, "ringTime string")
      end if

      call ESMF_Test (testPass .and. alarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy (alarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime increment, first forwards a fixed number of
      !timesteps, stopping at a non-alarm ringing time step.  Using a negative
      !timestemp, step backwards to some time prior to the clock's startTime.
      !Count number of rings.  See bugs #1531948, #1457135.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test forward to a non-ringing step, then step backward using a negative timeStep"

      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      clock = ESMF_ClockCreate (  &
          name = "clock 1",  &
          startTime = startTime, timeStep=timeStep, rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (alarmTime, yy=2008, mm=1, dd=23, h=6,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (alarmStep, h=6, rc=rc)
      alarm1 = ESMF_AlarmCreate (  &
          name="Alarm 1", clock=clock,  &
          ringTime=alarmTime, ringInterval=alarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      alarmCount = 0
      expectedCount = 9
      do, i=1,7
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at forwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at forwards timestep', i
        end if
      end do

      print *, 'SETTING CLOCK BACKWARDS WITH NEGATIVE TIMESTEP'
      timeStep = -timeStep
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=6, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at backwards timestep', i, ', time:'
          ! call ESMF_ClockPrint (clock, options="currTime string")
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at backwards timestep', i
          ! call ESMF_ClockPrint (clock, options="currTime string")
        end if
      end do

      if (.not. testPass .or. alarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', alarmCount = ', alarmCount, ', expected = ', expectedCount
          print *, 'The alarm ringTime may be stuck at:'
          call ESMF_AlarmPrint (alarm1, "ringTime string")
      end if

      call ESMF_Test (testPass .and. alarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy (alarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Alarm ringTime increment, arbitrarily forwards and backwards using
      ! positive and negative timesteps.  Alarm ringTime and ringInterval are
      ! changed with each change in timestep direction.  The extent of the run
      ! of each timestep value is different with each change in direction.
      write(failMsg, *) " Did not ring enough times during arbitrary forward/backward march"
      write(name, *) "Test arbitrary positive/negative timeStep runs with differing alarms"

      testPass = .true.
      alarmCountPass = .true.

      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=1, rc=rc)
      clock = ESMF_ClockCreate (  &
          name = "clock 1",  &
          startTime = startTime, timeStep=timeStep, rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (alarmTime, yy=2008, mm=1, dd=23, h=2,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (alarmStep, h=2, rc=rc)
      alarm1 = ESMF_AlarmCreate (  &
          name="Alarm 1", clock=clock,  &
          ringTime=alarmTime, ringInterval=alarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      alarmCount = 0
      expectedCount = 15

      do, i=1,7
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at forwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at forwards timestep', i
        end if
      end do
      if (alarmCountPass .and. alarmCount /= 3) then
        alarmCountPass = .false.
        print *, 'alarmCount = ', alarmCount, ' not 3 at 1st turnaround point'
      end if

      print *, 'SETTING CLOCK BACKWARDS WITH NEW NEGATIVE TIMESTEP AND RINGINTERVAL'
      timeStep = -2 * timeStep
      alarmStep = -2 * alarmStep
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      call ESMF_AlarmSet (alarm1, ringInterval=alarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=6, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at backwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at backwards timestep', i
        end if
      end do
      if (alarmCountPass .and. alarmCount /= 9) then
        alarmCountPass = .false.
        print *, 'alarmCount = ', alarmCount, ' not 9 at 2nd turnaround point'
      end if

      print *, 'SETTING CLOCK FORWARDS WITH NEW POSITIVE TIMESTEP, RINGINTERVAL, and RINGTIME'
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      call ESMF_TimeSet (alarmTime, yy=2008, mm=1, dd=22, h=11,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (alarmStep, h=6, rc=rc)
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      call ESMF_AlarmSet (alarm1, ringTime=alarmTime, ringInterval=alarmStep, &
                          rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=1,5
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at forwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at forwards timestep', i
        end if
      end do
      if (alarmCountPass .and. alarmCount /= 11) then
        alarmCountPass = .false.
        print *, 'alarmCount = ', alarmCount, ' not 11 at 3rd turnaround point'
      end if

      print *, 'SETTING CLOCK BACKWARDS WITH NEW NEGATIVE TIMESTEP AND RINGTIME'
      call ESMF_TimeIntervalSet (timeStep, h=-1, rc=rc)
      call ESMF_TimeSet (alarmTime, yy=2008, mm=1, dd=22, h=21,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      call ESMF_AlarmSet (alarm1, ringTime=alarmTime, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=4,2,-1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing at backwards timestep', i
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing at backwards timestep', i
        end if
      end do
      if (alarmCountPass .and. alarmCount /= 12) then
        alarmCountPass = .false.
        print *, 'alarmCount = ', alarmCount, ' not 12 at 4th turnaround point'
      end if

      print *, 'SETTING CLOCK FORWARDS WITH NEW POSITIVE TIMESTEP'
      timeStep = -timeStep
      call ESMF_ClockGet(clock, currTime=startTime, rc=rc)
      call ESMF_ClockSet(clock, timeStep=timeStep, startTime=startTime, &
                         runTimeStepCount=15, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
   
      do while (.not. ESMF_ClockIsDone(clock, rc=rc))
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string")
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_alarmIsRinging (alarm1)) then
          alarmCount = alarmCount + 1
          !print *, 'alarm IS ringing'
          call ESMF_AlarmRingerOff (alarm1, rc=rc)
        else
          !print *, 'alarm not ringing'
        end if
      end do
      if (alarmCountPass .and. alarmCount /= expectedCount) then
        alarmCountPass = .false.
        print *, 'alarmCount = ', alarmCount, ', not ', expectedCount, ' at the end'
      end if

      if (.not. alarmCountPass) then
          write(failMsg,*) trim(failMsg), ', alarmCount incorrect at one or more turnaround points'
          print *, 'Final alarmCount = ', alarmCount, ', expected = ', expectedCount
          print *, 'Final alarm ringTime:'
          call ESMF_AlarmPrint (alarm1, "ringTime string")
      end if

      if (.not. testPass) print *, 'bad return codes discovered'

      call ESMF_Test (testPass .and. alarmCountPass, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy (alarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------
#endif

      ! destroy calendars
      call ESMF_CalendarDestroy(esmf_360dayCalendar, rc)
      call ESMF_CalendarDestroy(no_leapCalendar, rc)
      call ESMF_CalendarDestroy(julianCalendar, rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc)

      ! finalize ESMF framework
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_AlarmTest
