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
      use ESMF
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a clock 
      type(ESMF_Clock) :: clock1
      type(ESMF_Alarm) :: alarm1

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             no_leapCalendar, esmf_360dayCalendar

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, stopTime
      type(ESMF_Time) :: alarmTime

      logical :: isCreated

#ifdef ESMF_TESTEXHAUSTIVE
      logical :: bool

      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock2, clock3, domainClock, CLOCK_ATM
      logical :: alarmCountPass, isringing, sticky, enabled
      integer(ESMF_KIND_I8) :: forwardCount
      logical :: willRingNext, testPass, alarmsNotEqual, alarmsEqual
      type(ESMF_Direction_Flag) :: reverseDirection, forwardDirection
      type(ESMF_Alarm) :: alarm, alarm2, afterAlarm, beforeAlarm
      type(ESMF_Alarm) :: alarmList(201), alarmListOne(1)
      type(ESMF_Alarm) :: alarm6, alarm7, alarm8
      type(ESMF_Alarm) :: alarm5(200), alarm3, alarm4
      type(ESMF_Alarm) :: ALARM_HISTORY

      integer(ESMF_KIND_I8) :: reverseCount, iteration
      integer :: dd, nclock, ringCount, expectedCount, i, yy
      integer :: mm, m, h, sstep, nstep, nring, alarmCount

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: TIMEINTERVAL_HISTORY, alarmStep, alarmStep2
      type(ESMF_TimeInterval) :: runDuration, ringDuration
      type(ESMF_Time) :: currentTime, currTime, afterAlarmTime, beforeAlarmTime
      type(ESMF_Time) :: alarmStopTime, nextTime, prevTime, currentTime2, time1

      character(ESMF_MAXSTR) :: aName
#endif


      ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! initialize one calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
        name="Gregorian", rc=rc)

      ! initialize secand calendar to be Julian type
      julianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIANDAY, &
        name="Julian", rc=rc)

      ! initialize third calendar to be No Leap type
      no_leapCalendar = ESMF_CalendarCreate(ESMF_CALKIND_NOLEAP, &
        name="NoLeap", rc=rc)

      ! initialize third calendar to be 360 day type
      esmf_360dayCalendar = ESMF_CalendarCreate(ESMF_CALKIND_360DAY, &
        name="360Day", rc=rc)

      call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=rc )
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize()

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
      clock1 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                               name="Clock 1", rc=rc)
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


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Alarm IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_AlarmIsCreated(alarm1)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Alarm IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_AlarmIsCreated(alarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test Alarm for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  alarm1 = ESMF_AlarmCreate(name="WAKEUP", clock=clock1, ringTime=alarmTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Alarm IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_AlarmIsCreated(alarm1)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Alarm IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_AlarmIsCreated(alarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test Alarm for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AlarmDestroy(alarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Alarm IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_AlarmIsCreated(alarm1)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Alarm IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_AlarmIsCreated(alarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------




#ifdef ESMF_TESTEXHAUSTIVE
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and alarmCount = 0"
      write(name, *) "Get number of alarms after destroyed Alarm Test"
      call ESMF_ClockGet(clock1, alarmCount=alarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.alarmCount.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "alarmCount = ", alarmCount

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_FAILURE"
      write(name, *) "Get a destroyed Alarm Test"
      call ESMF_ClockGetAlarm(clock1, "WAKEUP", alarm=alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Advance clock after destroyed alarm test"
      call ESMF_ClockAdvance(clock1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

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
      write(name, *) "Set name of destroyed Alarm Test"
      call  ESMF_AlarmSet(alarm1, name="ALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Set name of non-created Alarm Test"
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
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Test"
      alarm1 = ESMF_AlarmCreate(name="WAKEUP1", clock=clock1, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Test"
      alarm3 = ESMF_AlarmCreate(name="WAKEUP3", clock=clock1, ringTime=alarmTime, rc=rc)
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
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Clock Get Alarm List Test 1 - optional args missing"
      call ESMF_ClockGetAlarmList(clock1, ESMF_ALARMLIST_ALL, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Clock Get Alarm List Test 2 - only alarmList specified"
      call ESMF_ClockGetAlarmList(clock1, ESMF_ALARMLIST_ALL, &
                                  alarmList=alarmList, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and alarmCount=3"
      write(name, *) "Clock Get Alarm List Test 3 - only alarmCount specified"
      call ESMF_ClockGetAlarmList(clock1, ESMF_ALARMLIST_ALL, &
                                  alarmCount=alarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.alarmCount.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and alarmCount=1"
      write(name, *) "Clock Get Alarm List Test 4 - single-element length alarmList specified"
      call ESMF_AlarmRingerOn(alarm1, rc=rc)
      call ESMF_ClockGetAlarmList(clock1, ESMF_ALARMLIST_RINGING, &
                                  alarmList=alarmListOne, &
                                  alarmCount=alarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.alarmCount.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_AlarmRingerOff(alarm1, rc=rc)
      !call ESMF_AlarmPrint(alarmListOne(1), options="name", rc=rc)
      !print *, "alarmCount = ", alarmCount

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and alarmCount = 3"
      write(name, *) "Clock Get Alarm List Test 5"
      call ESMF_ClockGetAlarmList(clock1, ESMF_ALARMLIST_ALL, &
                                  alarmList=alarmList, alarmCount=alarmCount, &
                                  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.alarmCount.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)
      !call ESMF_AlarmPrint(alarmList(1), options="name", rc=rc)
      !call ESMF_AlarmPrint(alarmList(2), options="name", rc=rc)
      !call ESMF_AlarmPrint(alarmList(3), options="name", rc=rc)
      !print *, "alarmCount = ", alarmCount

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmAssignment(=)(alarm,alarm)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarm Assignment Test"
      alarm8 = alarm1  ! exercise default F90 Alarm = assignment
      call ESMF_AlarmGet(alarm8, name=aName, clock=clock3, &
                         ringTime=time1, rc=rc)
      call ESMF_Test((alarm8==alarm1 .and. aName=="WAKEUP1" .and. &
                      clock3==clock1 .and. time1==alarmTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm1,alarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarms Equal Test"
      alarmsEqual = (alarm1 == alarm2)  ! exercise Alarm == operator
      call ESMF_Test((alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm1,alarm2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Alarms Not Equal Test"
      alarmsEqual = (alarm1 == alarm3)  ! exercise Alarm == operator
      call ESMF_Test((.not.alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest 
      ! Testing ESMF_AlarmOperator(/=)(alarm1,alarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarms Equal Test"
      alarmsNotEqual = (alarm1 /= alarm2)  ! exercise Alarm /= operator
      call ESMF_Test((.not.alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_AlarmDestroy(alarm2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(/=)(alarm1,alarm2)
      write(failMsg, *) "Returned equal"
      write(name, *) "Alarms Not Equal Test"
      alarmsNotEqual = (alarm1 /= alarm3)  ! exercise Alarm /= operator
      call ESMF_Test((alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_AlarmDestroy(alarm3, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted Alarm Not Equal Created Alarm Test 1"
      alarmsEqual = (alarm2 == alarm1)  ! exercise Alarm == operator
      call ESMF_Test((.not.alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted Alarm Not Equal Uncreated Alarm Test 1"
      alarmsEqual = (alarm3 == alarm7)  ! exercise Alarm == operator
      call ESMF_Test((.not.alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated Alarm Not Equal Created Alarm Test 1"
      alarmsEqual = (alarm7 == alarm1)  ! exercise Alarm == operator
      call ESMF_Test((.not.alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm,alarm1)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Deleted Alarm Equal Deleted Alarm Test 1"
      alarmsEqual = (alarm2 == alarm3)  ! exercise Alarm == operator
      call ESMF_Test((alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(==)(alarm,alarm1)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Uncreated Alarm Equal Uncreated Alarm Test 1"
      alarmsEqual = (alarm7 == alarm7)  ! exercise Alarm == operator
      call ESMF_Test((alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(/=)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted Alarm Not Equal Created Alarm Test 2"
      alarmsNotEqual = (alarm2 /= alarm1)  ! exercise Alarm /= operator
      call ESMF_Test((alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(/=)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted Alarm Not Equal Uncreated Alarm Test 2"
      alarmsNotEqual = (alarm3 /= alarm7)  ! exercise Alarm /= operator
      call ESMF_Test((alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(/=)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated Alarm Not Equal Created Alarm Test 2"
      alarmsNotEqual = (alarm7 /= alarm1)  ! exercise Alarm /= operator
      call ESMF_Test((alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(/=)(alarm,alarm1)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Deleted Alarm Equal Deleted Alarm Test 2"
      alarmsNotEqual = (alarm2 /= alarm3)  ! exercise Alarm /= operator
      call ESMF_Test((.not.alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_AlarmOperator(/=)(alarm,alarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated Alarm Equal Uncreated Alarm Test 2"
      alarmsNotEqual = (alarm7 /= alarm7)  ! exercise Alarm /= operator
      call ESMF_Test((.not.alarmsNotEqual), &
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
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."WAKEUP1"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "Alarm name is ", aName

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
      !print *, "Alarm name is ", aName

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
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroy Clock Test"
      call ESMF_ClockDestroy(clock1, rc=rc)
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
                                   calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=13, &
                                   h=18, m=45, s=27, &
                                   calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Alarm Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Alarm Time Initialization Test"
      call ESMF_TimeSet(alarmTime, yy=2003, mm=3, dd=13, h=5, m=45, s=27, &
                                   calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Initialize clock 
      !EX_UTest
       write(name, *) "Clock Initialization Test"
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       clock = ESMF_ClockCreate(timeStep, startTime, &
                                stopTime=stopTime, name="Clock 1", rc=rc)
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
      call ESMF_ClockGetAlarm(clock, alarmname="alarm1", alarm=alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_AlarmDestroy (alarm, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 1"
      call ForwardAlarm_Test1(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 2"
      call ForwardAlarm_Test2(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 3"
      call ForwardAlarm_Test3(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 4"
      call ForwardAlarm_Test4(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 5"
      call ForwardAlarm_Test5(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 6"
      call ForwardAlarm_Test6(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward Alarm Test Case 7"
      call ForwardAlarm_Test7(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Reverse Alarm Test Case 1"
      call ReverseAlarm_Test1(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Reverse Alarm Test Case 2"
      call ReverseAlarm_Test2(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Reverse Alarm Test Case 3"
      call ReverseAlarm_Test3(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Reverse Alarm Test Case 4"
      call ReverseAlarm_Test4(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Reverse Alarm Test Case 5"
      call ReverseAlarm_Test5(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Reverse Alarm Test Case 6"
      call ReverseAlarm_Test6(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward then Reverse Alarm Test Case 1"
      call ForwardReverseAlarm_Test1(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward then Reverse Alarm Test Case 2"
      call ForwardReverseAlarm_Test2(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Forward then Reverse Alarm Test Case 3"
      call ForwardReverseAlarm_Test3(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " Alarms with ClockSet... "
      write(name, *) "Test ClockSet after alarm attached to clock "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_ClockSet(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      ! The following tests are from Ben@NASA's support ticket 3614994
      write(failMsg, *) " Alarms did not rewind correct number of times "
      write(name, *) "Test multiple alarms rewind correct number of times "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_ReverseAlarms(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " Alarms hang... "
      write(name, *) "Test multiple alarms replay without hanging "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_AlarmHang(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " Alarms with ring intervals equal to clock interval, incorrect behavior "
      write(name, *) "Test running an alarms forward-reverse-forward with ring interval equal to clock interval "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_AlarmAdvRewind(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " Alarms reverse with sticky set... "
      write(name, *) "Test running an alarm reverse with sticky bit set "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_RevAlarmSticky(60._ESMF_KIND_R8, testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#if 1
      write(failMsg, *) " Alarms with getPrevRingTime... "
      write(name, *) "Test getPrevRingTime... after alarm attached to clock "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_GetPrevRingTime(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)
#endif

      ! ----------------------------------------------------------------------------
      ! ----------------------------------------------------------------------------
#endif

      ! destroy calendars
      call ESMF_CalendarDestroy(esmf_360dayCalendar, rc=rc)
      call ESMF_CalendarDestroy(no_leapCalendar, rc=rc)
      call ESMF_CalendarDestroy(julianCalendar, rc=rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)

      ! finalize ESMF framework
      call ESMF_TestEnd(ESMF_SRCLINE)

#if 1
  contains

    subroutine test_reverseAlarms(testPass, rc)
      implicit none

      logical, intent(out) :: testPass
      integer :: status,rc

      type(ESMF_TimeInterval) :: dt
      type(ESMF_Time) :: start_time, clock_start, clock_end
      type(ESMF_Clock) :: clock
      character(len=5) :: add_2nd
      integer :: nargs

      type(ESMF_TimeInterval) :: tint
      type(ESMF_Alarm) :: esmfalarm, firstalarm

      integer :: i,nstep, nrings1, nrings2

      type(ESMF_Time) :: time
      character(len=10) :: iam='test_clock'
      logical :: esmf_ring
      integer :: nalarms, n_rings=0

      testPass = .false.
      call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
      call verify_(status)
      call ESMF_TimeSet(clock_start,yy=2000,mm=1,dd=1,h=21,m=0,s=0,rc=status)
      call verify_(status)
      call ESMF_TimeSet(clock_end,yy=2000,mm=12,dd=1,h=21,m=0,s=0,rc=status)
      call verify_(status)
      call ESMF_TimeSet(start_time,yy=2000,mm=10,dd=1,h=21,m=0,s=0,rc=status)
      call verify_(status)
      call ESMF_TimeIntervalSet(dt,S=900, sN=0, sD=1,rc=status)
      call verify_(status)
      clock= ESMF_ClockCreate(timeStep=dt,startTime=clock_start,stopTime=clock_end,rc=status)
      call verify_(status)
      call ESMF_ClockSet(clock,currTime=start_time,rc=status)
      call verify_(status)

      call ESMF_ClockGet(clock,currtime=start_time,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_TimeIntervalSet(tint,h=2,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      firstalarm = ESMF_AlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name="alarm1",rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()

      nstep=47
      call ESMF_ClockGet(clock,currtime=start_time,alarmCount=nalarms,rc=status)
      !write(*,*) "alarms already in clock: ",nalarms
      call verify_(status)
      call ESMF_TimeIntervalSet(tint,h=1,rc=status)
      call verify_(status)
      esmfalarm = ESMF_AlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name="alarm2",rc=status)
      nrings1 = 0
      nrings2 = 0
      do i=1,nstep
         !call ESMF_ClockDebug(clock, 'forward clock', rc=status)
         !call verify_(status)
         !call ESMF_AlarmDebug(esmfalarm, 'forward esmfalarm', rc=status)
         !call verify_(status)
         call ESMF_ClockGet(clock,currTime=time)
         esmf_ring = ESMF_AlarmIsRinging(esmfalarm,rc=status)
         call verify_(status)
         if ( esmf_ring) then
            write(*,*)'ringing'
            call ESMF_TimePrint(time,options='string')
            nrings2=nrings2 + 1
         end if
         call ESMF_ClockAdvance(clock)
      enddo
      if(nrings2 == 12) then
        testPass = .true. ! ringerIV = 1h, timeST=15min, 48/4=12
      else 
        return
      endif
      testPass = .false.
      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=status)
      call verify_(status)
      !write(*,*)"*************** start rewind *********************"
      i = 0
      do
         i = i + 1
         write(*,*) 'Rewind step: ', i
         call ESMF_ClockAdvance(clock,rc=status)
         call verify_(status)
         !call ESMF_ClockDebug(clock, 'reverse clock', rc=status)
         !call verify_(status)
         !call ESMF_AlarmDebug(esmfalarm, 'reverse esmfalarm', rc=status)
         !call verify_(status)
         call ESMF_ClockGet(clock,currTime=time)
         if (ESMF_AlarmIsRinging(esmfalarm)) then
            write(*,*)'rewinding one step ',ESMF_AlarmIsRinging(esmfalarm)
            n_rings = n_rings + 1
            call ESMF_TimePrint(time,options='string')
         end if
         
         if (time == start_time) exit
      enddo
      if(n_rings == 12) then
        testPass = .true. ! ringerIV = 1h, timeST=15min, 48/4=12
      else 
        return
      endif
      testPass = .false.
      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=status)
      call verify_(status)
      write(*,*)"*************** end rewind *********************"
      do i=1,nstep*2
         !call ESMF_ClockDebug(clock, 'forward clock again', rc=status)
         !call verify_(status)
         !call ESMF_AlarmDebug(esmfalarm, 'forward esmfalarm again', rc=status)
         !call verify_(status)
         call ESMF_ClockGet(clock,currTime=time)
         esmf_ring = ESMF_AlarmIsRinging(esmfalarm,rc=status)
         call verify_(status)
         if ( esmf_ring ) then
            write(*,*)'ringing'
            call ESMF_TimePrint(time,options='string')
         end if
         call ESMF_ClockAdvance(clock)
      enddo
      if(n_rings == 12) testPass = .true.

       call ESMF_AlarmDestroy(esmfalarm, rc=status)
       call verify_(status)
       call ESMF_AlarmDestroy(firstalarm, rc=status)
       call verify_(status)
       call ESMF_ClockDestroy(clock, rc=status)
       call verify_(status)
           
    end subroutine Test_ReverseAlarms

    subroutine Test_AlarmHang(testPass, rc)
      logical, intent(out) :: testPass
      integer :: status,rc

      type(ESMF_TimeInterval) :: dt
      type(ESMF_Time) :: start_time, clock_start, clock_end
      type(ESMF_Clock) :: clock
      character(len=5) :: add_2nd
      integer :: nargs
           
      integer :: i,nstep

      type(ESMF_TimeInterval) :: tint
      type(ESMF_Time) :: time
      type(ESMF_Alarm) :: esmfalarm
      type(ESMF_Alarm) :: testalarm
      character(len=10) :: iam='test_clock'
      logical :: esmf_ring
      integer :: nalarms

      testPass = .false.

      call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
      call verify_(status)
      call ESMF_TimeSet(clock_start,yy=2000,mm=1,dd=1,h=21,m=0,s=0,rc=status)
      call verify_(status)
      call ESMF_TimeSet(clock_end,yy=2000,mm=12,dd=1,h=21,m=0,s=0,rc=status)
      call verify_(status)
      call ESMF_TimeSet(start_time,yy=2000,mm=10,dd=1,h=21,m=0,s=0,rc=status)
      call verify_(status)
      call ESMF_TimeIntervalSet(dt,S=900, sN=0, sD=1,rc=status)
      call verify_(status)
      clock= ESMF_ClockCreate(timeStep=dt,startTime=clock_start,stopTime=clock_end,rc=status)
      call verify_(status)
      call ESMF_ClockSet(clock,currTime=start_time,rc=status)
      call verify_(status)

      nstep = 12
      call ESMF_ClockGet(clock,currtime=start_time,alarmCount=nalarms,rc=status)
      !write(*,*) "alarms already in clock: ",nalarms
      call verify_(status)
      call ESMF_TimeIntervalSet(tint,h=1,rc=status)
      call verify_(status)
      esmfalarm = ESMF_AlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name="alarm2",rc=status)
      !call ESMF_TimeIntervalSet(tint,h=3,rc=status)
      !testalarm = ESMF_AlarmCreate(clock=clock,name="alarm3",ringInterval=tint, ringtime=start_time,sticky=.true.,rc=status)
      testalarm = ESMF_AlarmCreate(clock=clock,name="alarm3",ringtime=start_time,sticky=.true.,rc=status)
      call verify_(status)
      call ESMF_AlarmRingerOff(testalarm,rc=status)
      call verify_(status)
      do i=1,nstep
         call ESMF_AlarmRingerOn(testalarm,rc=status)
         call verify_(status)
         call ESMF_ClockGet(clock,currTime=time)
         esmf_ring = ESMF_AlarmIsRinging(esmfalarm,rc=status)
         call verify_(status)
         if ( esmf_ring) then
            !write(*,*)'ringing: esmfalarm'
            call ESMF_TimePrint(time,options='string')
         end if
         call ESMF_ClockAdvance(clock)
      enddo
      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=status)
      call verify_(status)
      call ESMF_AlarmRingerOff(testalarm,rc=status)
      call verify_(status)
      !write(*,*)"*************** start rewind *********************"
      do
         call ESMF_ClockAdvance(clock,rc=status)
         call verify_(status)
         call ESMF_ClockGet(clock,currTime=time)
         if (ESMF_AlarmIsRinging(esmfalarm)) then
            !write(*,*)'rewinding one step: esmfalarm ',ESMF_AlarmIsRinging(esmfalarm)
            call ESMF_TimePrint(time,options='string')
         end if

         if (ESMF_AlarmIsRinging(esmfalarm)) then
            write(*,*)'rewinding one step: testalarm ',ESMF_AlarmIsRinging(testalarm)
            call ESMF_TimePrint(time,options='string')
         end if
         
         if (time == start_time) exit
      enddo
      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=status)
      call verify_(status)
      !write(*,*)"*************** end rewind *********************"
      do i=1,nstep*2
         call ESMF_ClockGet(clock,currTime=time)
         esmf_ring = ESMF_AlarmIsRinging(esmfalarm,rc=status)
         call verify_(status)
         if ( esmf_ring ) then
            !write(*,*)'ringing: esmfalarm'
            call ESMF_TimePrint(time,options='string')
         end if
         call ESMF_ClockAdvance(clock)
      enddo

       call ESMF_AlarmDestroy(esmfalarm, rc=status)
       call verify_(status)
       call ESMF_AlarmDestroy(testalarm, rc=status)
       call verify_(status)
       call ESMF_ClockDestroy(clock, rc=status)
       call verify_(status)

       testPass = .true.
      
    end subroutine Test_AlarmHang

subroutine Test_AlarmAdvRewind(testPass, rc)
  logical, intent(out) :: testPass
  integer :: rc

   type(ESMF_TimeInterval) :: dt
   type(ESMF_Time) :: start_time, clock_start, clock_end
   type(ESMF_Clock) :: clock

   type(ESMF_TimeInterval) :: tint

   integer :: status,i,j,nstep, nrings(2)

   logical :: esmf_ring
   character(len=ESMF_MAXSTR) :: alarm_name
   integer :: nalarms
   type(ESMF_Alarm), allocatable :: alarm_list(:)
   type(ESMF_Time) :: time
   type(ESMF_Alarm) :: esmfalarm
   type(ESMF_CalKind_Flag) :: calkindflag

   rc = ESMF_SUCCESS

   !call ESMF_CalendarGet(calkindflag=calkindflag, rc=status)
   !call verify_(status)
   call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=status )
   call verify_(status)
   call ESMF_TimeSet(clock_start,yy=2000,mm=1,dd=1,h=21,m=0,s=0,rc=status)
   call verify_(status)
   call ESMF_TimeSet(clock_end,yy=2000,mm=12,dd=1,h=21,m=0,s=0,rc=status)
   call verify_(status)
   call ESMF_TimeSet(start_time,yy=2000,mm=10,dd=1,h=21,m=0,s=0,rc=status)
   call verify_(status)
   call ESMF_TimeIntervalSet(dt,S=900, sN=0, sD=1,rc=status)
   call verify_(status)
   clock= ESMF_ClockCreate(timeStep=dt,startTime=clock_start,stopTime=clock_end,rc=status)
   call verify_(status)
   call ESMF_ClockSet(clock,currTime=start_time,rc=status)
   call verify_(status)

   ! fails if s=900
   call ESMF_TimeIntervalSet(tint,s=900)
   esmfalarm = ESMF_AlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name='alarm_900',rc=status)
   call verify_(status)
   ! works if s=1800
   call ESMF_TimeIntervalSet(tint,s=1800)
   esmfalarm = ESMF_AlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name='alarm_1800',rc=status)
   call verify_(status)


   nstep=12
   nrings=0
   call ESMF_ClockGet(clock,currtime=start_time,alarmCount=nalarms,rc=status)
   allocate(alarm_list(nalarms))
   call ESMF_ClockGetAlarmList(clock, alarmListFlag=ESMF_ALARMLIST_ALL, alarmList=alarm_list)
   
   do i=1,nstep
      call ESMF_ClockGet(clock,currTime=time)
      do j = 1,nalarms
         call ESMF_AlarmGet(alarm_list(j),name=alarm_name)
         esmf_ring = ESMF_AlarmIsRinging(alarm_list(j),rc=status)
         call verify_(status)
         if ( esmf_ring) then
            nrings(j) = nrings(j) + 1
            write(*,*)trim(alarm_name)//' is ringing'
            call ESMF_TimePrint(time,options='string')
         end if
      end do
      call ESMF_ClockAdvance(clock)
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=status)
   call verify_(status)
   write(*,*)"*************** start rewind *********************"
   nrings=0
   do
      call ESMF_ClockAdvance(clock,rc=status)
      call verify_(status)
      call ESMF_ClockGet(clock,currTime=time)
      do j=1,nalarms
         call ESMF_AlarmGet(alarm_list(j),name=alarm_name)
         if (ESMF_AlarmIsRinging(alarm_list(j))) then
            nrings(j) = nrings(j) + 1
            write(*,*)trim(alarm_name)//' is ringing'
            call ESMF_TimePrint(time,options='string')
         end if
      end do
      
      if (time == start_time) exit
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=status)
   call verify_(status)
   write(*,*)"*************** end rewind *********************"
   nrings=0
   do i=1,nstep*2
      call ESMF_ClockGet(clock,currTime=time)
      do j = 1,nalarms
         call ESMF_AlarmGet(alarm_list(j),name=alarm_name)
         esmf_ring = ESMF_AlarmIsRinging(alarm_list(j),rc=status)
         call verify_(status)
         if ( esmf_ring) then
            nrings(j) = nrings(j) + 1
            write(*,*)trim(alarm_name)//' is ringing'
            call ESMF_TimePrint(time,options='string')
         end if
      end do
      call ESMF_ClockAdvance(clock)
   enddo
     
end subroutine Test_AlarmAdvRewind

subroutine Test_RevAlarmSticky(dt, testPass, rc)
#define CONTEXT line=__LINE__,file=__FILE__
#define CHECKRC if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,CONTEXT,rcToReturn=rc))then;write(0,*)'app abort: ',__FILE__,__LINE__;return;endif
  logical, intent(out) :: testPass
  integer, parameter :: r8 = SELECTED_REAL_KIND(12)   ! real r8
  real(kind=r8), intent(in) :: dt
  integer :: rc
  
  type(ESMF_Clock)         :: clock
  type(ESMF_Alarm)         :: alarm
  type(ESMF_TimeInterval)  :: esmf_ival
  type(ESMF_Time)          :: time, initial, finish, ring_time
  real(kind=r8)            :: secs
  logical                  :: reverse_clock, sticky_alarm

  rc = ESMF_SUCCESS
  
  call ESMF_TimeSet(time, yy=2021, mm=4, dd=6, rc=rc)                          ; CHECKRC
  
  secs = 0
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)                         ; CHECKRC
  initial = time + esmf_ival
  
  secs = dt*100
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)                         ; CHECKRC
  finish = time + esmf_ival
  
  secs = dt
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)                         ; CHECKRC
  
  clock = ESMF_Clockcreate(timeStep=esmf_ival   &
          ,startTime=initial,stopTime=finish    &
          ,refTime=time, rc=rc                  )      ; CHECKRC
  
  call ESMF_ClockSet(clock,direction=ESMF_DIRECTION_REVERSE, rc=rc)    ; CHECKRC

  reverse_clock = ESMF_ClockIsReverse(clock, rc=rc)                      ; CHECKRC
  sticky_alarm  = .not.reverse_clock
  
  write(0,'("reverse =",x,l)') reverse_clock
  write(0,'("sticky  =",x,l)') sticky_alarm
  
  secs = dt*50
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)  ; CHECKRC
  ring_time = initial + esmf_ival

#if 1
  alarm = ESMF_AlarmCreate(clock, ringTime=ring_time, sticky=sticky_alarm,  &
                           rc=rc) ; CHECKRC
#else
  alarm = ESMF_AlarmCreate(clock, ringTime=ring_time, sticky=.false.,  &
                           rc=rc) ; CHECKRC
#endif
  
  call ESMF_alarmPrint(alarm,options='sticky')

  testPass = .true. ! Because the C++ runtime failure cannot be caught reliably, set this to false.

#undef CONTEXT
#undef CHECKRC
end subroutine Test_RevAlarmSticky

    subroutine verify_(rc)
      integer, intent(in)  :: rc

      integer :: error_code,status
      if (rc /=0) then
         call ESMF_Finalize()
      end if
    end subroutine verify_

subroutine Test_ClockSet(testPass, rc)
#define CONTEXT line=__LINE__,file=__FILE__
#define CHECKRC if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,CONTEXT,rcToReturn=rc))then;write(0,*)'app abort: ',__FILE__,__LINE__;return;endif
  logical, intent(out) :: testPass
  integer, parameter :: r8 = SELECTED_REAL_KIND(12)   ! real r8
  integer :: rc
  
  type(ESMF_Clock)         :: clock
  type(ESMF_Alarm)         :: alarm
  type(ESMF_TimeInterval)  :: esmf_ival
  type(ESMF_Time)          :: time, initial, finish, ring_time
  real(kind=r8)            :: secs
  logical                  :: reverse_clock, sticky_alarm

  rc = ESMF_SUCCESS

  call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, rc=rc)
  CHECKRC
  
  call ESMF_TimeSet(time, yy=2021, mm=4, dd=6, rc=rc)
  CHECKRC
  secs = 0
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)
  CHECKRC
  
  initial = time + esmf_ival
  call ESMF_TimePrint(initial, options="string", rc=rc)
  CHECKRC
  
  secs = 6000
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)
  CHECKRC
  finish = time + 2*esmf_ival
  call ESMF_TimePrint(finish, options="string", rc=rc)
  CHECKRC
  
  secs = 60
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)
  CHECKRC
  
  clock = ESMF_Clockcreate(timeStep=esmf_ival   &
          ,startTime=initial,stopTime=finish    &
          ,refTime=time, rc=rc                  )
  CHECKRC
  
  call ESMF_ClockSet(clock,direction=ESMF_DIRECTION_REVERSE, rc=rc)
  CHECKRC

  reverse_clock = ESMF_ClockIsReverse(clock, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  sticky_alarm  = .not.reverse_clock
  
  write(0,'("reverse =",x,l)') reverse_clock
  write(0,'("sticky  =",x,l)') sticky_alarm
  
  secs = 3000
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)
  CHECKRC
  ring_time = initial + 2*esmf_ival
  print *, 'Before Alarm Create ringTime: '
  call ESMF_TimePrint(ring_time, options="string", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#if 1
  alarm = ESMF_AlarmCreate(clock, ringTime=ring_time, sticky=sticky_alarm,  &
                           rc=rc)
  CHECKRC
#else
  alarm = ESMF_AlarmCreate(clock, ringTime=ring_time, sticky=.false.,  &
                           rc=rc)
  CHECKRC
#endif

  call ESMF_TimePrint(ring_time, options="string", rc=rc)
  CHECKRC
  print *, 'After Alarm Create ringTime: '
  call ESMF_TimePrint(ring_time, options="string", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ClockSet(clock, stopTime=ring_time, rc=rc)
  CHECKRC
  
  call ESMF_alarmPrint(alarm,options='sticky')

  testPass = .true. ! Because the C++ runtime failure cannot be caught reliably, set this to false.

#undef CONTEXT
#undef CHECKRC
end subroutine Test_ClockSet

subroutine Test_GetPrevRingTime(testPass, rc)
#define CONTEXT line=__LINE__,file=__FILE__
#define CHECKRC if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,CONTEXT,rcToReturn=rc))then;write(0,*)'app abort: ',__FILE__,__LINE__;return;endif
  logical, intent(out) :: testPass
  integer, parameter :: r8 = SELECTED_REAL_KIND(12)   ! real r8
  integer :: rc
  
  type(ESMF_Clock)         :: clock
  type(ESMF_Alarm)         :: alarm
  type(ESMF_TimeInterval)  :: esmf_ival, diffTime
  type(ESMF_Time)          :: time, initial, finish, ring_time, ringTime, prevTime
  real(kind=r8)            :: secs
  logical                  :: reverse_clock, sticky_alarm, esmf_ring
  integer                  :: i, nstep = 6, nrings

  rc = ESMF_SUCCESS
  testPass = .true.
  
  call ESMF_TimeSet(time, yy=2021, mm=4, dd=6, rc=rc)                          ; CHECKRC
  
  secs = 0
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)                         ; CHECKRC
  initial = time + esmf_ival
  call ESMF_TimePrint(initial, options="string", rc=rc)  ; CHECKRC
  
  secs = 6000    ! 100 minutes
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)                         ; CHECKRC
  finish = time + esmf_ival
  call ESMF_TimePrint(finish, options="string", rc=rc)  ; CHECKRC
  
  secs = 60      ! 1 minute
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)                         ; CHECKRC
  
  clock = ESMF_Clockcreate(timeStep=esmf_ival   &
          ,startTime=initial,stopTime=finish    &
          ,refTime=time, rc=rc                  )      ; CHECKRC

  secs = 120 ! alarm step is clock step x 2, 2 minutes
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)  ; CHECKRC
  ring_time = initial
  alarm = ESMF_AlarmCreate(clock, ringTime=ring_time, ringInterval=esmf_ival,  &
                           sticky=.false., rc=rc) ; CHECKRC
  nrings = 0
  prevTime = initial
  do i=1,nstep
     call ESMF_ClockAdvance(clock)
     call ESMF_ClockGet(clock,currTime=time)
     esmf_ring = ESMF_AlarmIsRinging(alarm, rc=rc)
     call verify_(rc)
     if ( esmf_ring) then
       nrings = nrings + 1
       write(*,*) 'alarm is ringing', nrings
       call ESMF_TimePrint(time,options='string')
       call ESMF_AlarmGet(alarm, ringTime=ringTime, rc=rc)
       call verify_(rc)
       write(*,*) 'Ring Time'
       call ESMF_TimePrint(ringTime,options='string')

       diffTime = ringTime - prevTime
       if(diffTime /= esmf_ival) testPass = .false. ! both should be 20 minutes or 120 seconds
       prevTime = ringTime
      end if
  enddo
  
!  call ESMF_ClockSet(clock,direction=ESMF_DIRECTION_REVERSE, rc=rc)    ; CHECKRC
!
!  reverse_clock = ESMF_ClockIsReverse(clock, rc=rc)                      ; CHECKRC
!  sticky_alarm  = .not.reverse_clock
!  
!  write(0,'("reverse =",x,l)') reverse_clock
!  write(0,'("sticky  =",x,l)') sticky_alarm
!  
!  secs = 3000
!  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)  ; CHECKRC
!  ring_time = initial + esmf_ival
!  print *, 'Before Alarm Create ringTime: '
!  call ESMF_TimePrint(ring_time, options="string", rc=rc)  ; CHECKRC
!
!  call ESMF_TimePrint(ring_time, options="string", rc=rc)  ; CHECKRC
!  call ESMF_AlarmGet(alarm, ringTime=ring_time, rc=rc)                         ; CHECKRC
!  print *, 'After Alarm Create ringTime: '
!  call ESMF_TimePrint(ring_time, options="string", rc=rc)  ; CHECKRC
!
!  call ESMF_ClockSet(clock, stopTime=ring_time, rc=rc) ; CHECKRC
!  
!  call ESMF_alarmPrint(alarm,options='sticky')


#undef CONTEXT
#undef CHECKRC
end subroutine Test_GetPrevRingTime


#endif

  character*64 function clockCurrTime(clock)
    type(ESMF_Clock) :: clock
    type(ESMF_Time ) :: time
    integer                :: yy, mm, dd, d, h, m, s, rc
    type (ESMF_VM)   :: vm
    integer          :: lpet

    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize()
  
    call ESMF_TimeGet(Time, yy=yy, mm=mm, dd=dd, d=d, h=h, m=m, s=s, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize()
 
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_VMGet(vm, localpet = lpet, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize()

    if(lpet == 0) then
      !write(clockCurrTime, '(3I4, 3I4)') yy, mm, dd, h, m, s
      write(clockCurrTime, '(I2.2,A1,I2.2,A1,I2.2)') h, ':', m, ':', s
    endif
  end function

!------------------------------------------------------------------------
! Forward Tests
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test1(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 1: c.t0 = a.t0, c.dt = a.dt = 10 min
    print *, 'Test 1: c.t0 = a.t0, c.dt = a.dt = 10 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    !call ESMF_AlarmDebug(alarm,'test1',rc=status)
    !if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
    if(ringing) nrings = nrings + 1
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 7) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test2(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    print *, 'Test 2: c.t0 = a.t0, c.dt = 10 min a.dt = 20 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=1200, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    !call ESMF_AlarmDebug(alarm,'test2',rc=status)
    !if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
    if(ringing) nrings = nrings + 1
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
      if(ringing) nrings = nrings + 1
    enddo

    rc = ESMF_FAILURE
    if(nrings == 4) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test3(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS
  !print *, 'Test 3: c.t0 = a.t0, c.dt = 20 min a.dt = 10 min'
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_TimeIntervalSet(timeStep,S=1200, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    ! It's an error to create an alarm with alarm timeInterval less than clock timeStep
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) return
    call ESMF_AlarmDebug(alarm,'test3',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
  end subroutine
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test4(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 4: c.t0 = a.t0, c.dt = 10 min a.dt = 12 min
    print *, 'Test 4: c.t0 = a.t0, c.dt = 10 min a.dt = 12 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=720, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringDuration,S=1, sN=0, sD=1,rc=status)

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test4',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    write(*, '(A20,Z16)') 'LOC(sticky) = ', LOC(sticky)

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 4: ringing = ', ringing, trim(clockCurrTime(clock)), ' sticky = ', sticky
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 4: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 2) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test5(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    print *, 'Test 5: c.t0 = 0:0:0 a.t0=0:4:0, c.dt = a.dt = 10 min sticky (an alarm wont ring)'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=4,s=0,rc=status)
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.true., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) return
    call ESMF_AlarmDebug(alarm,'test5',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 5: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 5: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

  end subroutine
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test6(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=30,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 6: c.t0 = a.t0, c.dt = 10 min a.dt = 12 min
    print *, 'Test 6: c.t0 = a.t0, c.dt = 10 min a.dt = unspecified (one shot)'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=720, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringDuration,S=1, sN=0, sD=1,rc=status)

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test6',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 6: ringing = ', ringing, trim(clockCurrTime(clock)), ' sticky = ', sticky
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 6: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 1) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ForwardAlarm_Test7(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=30,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    print *, 'Test 7: c.t0, a.t0 unspecified default to c.t0, c.dt = 10 min a.dt = 10 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringDuration,S=1, sN=0, sD=1,rc=status)

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test7',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 7: ringing = ', ringing, trim(clockCurrTime(clock)), ' sticky = ', sticky
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 7: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 7) rc = ESMF_SUCCESS
  end subroutine


!------------------------------------------------------------------------
! Reverse Tests
!------------------------------------------------------------------------
  subroutine ReverseAlarm_Test1(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime, currTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 1: c.t0 = a.t0, c.dt = a.dt = -10 min
    print *, 'Test 1: c.t0 = a.t0, c.dt = a.dt = 10 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    !call ESMF_AlarmDebug(alarm,'test1',rc=status)
    !if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
    if(ringing) nrings = nrings + 1
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 7) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ReverseAlarm_Test2(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    print *, 'Test 2: c.t0 = a.t0, c.dt = 10 min a.dt = 20 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=1200, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    !call ESMF_AlarmDebug(alarm,'test2',rc=status)
    !if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
    if(ringing) nrings = nrings + 1
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
      if(ringing) nrings = nrings + 1
    enddo

    rc = ESMF_FAILURE
    if(nrings == 4) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ReverseAlarm_Test3(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS
  !print *, 'Test 3: c.t0 = a.t0, c.dt = 20 min a.dt = 10 min'
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_TimeIntervalSet(timeStep,S=1200, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    ! It's an error to create an alarm with alarm timeInterval less than clock timeStep
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) return
    call ESMF_AlarmDebug(alarm,'test3',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
  end subroutine
!------------------------------------------------------------------------
  subroutine ReverseAlarm_Test4(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 4: c.t0 = a.t0, c.dt = 10 min a.dt = 12 min
    print *, 'Test 4: c.t0 = a.t0, c.dt = 10 min a.dt = 12 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=720, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringDuration,S=1, sN=0, sD=1,rc=status)

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test4',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    write(*, '(A20,Z16)') 'LOC(sticky) = ', LOC(sticky)

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 4: ringing = ', ringing, trim(clockCurrTime(clock)), ' sticky = ', sticky
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 4: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 2) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------
  subroutine ReverseAlarm_Test5(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    print *, 'Test 5: c.t0 = 0:0:0 a.t0=0:4:0, c.dt = a.dt = 10 min sticky (an alarm wont ring)'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=4,s=0,rc=status)
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.true., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) return
    call ESMF_AlarmDebug(alarm,'test5',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    print *, 'Test 5: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 5: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

  end subroutine
!------------------------------------------------------------------------
  subroutine ReverseAlarm_Test6(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS

    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=30,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 6: c.t0 = a.t0, c.dt = 10 min a.dt = 12 min
    print *, 'Test 6: c.t0 = a.t0, c.dt = 10 min a.dt = unspecified (one shot)'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=720, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringDuration,S=1, sN=0, sD=1,rc=status)

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_ClockSet(clock, currTime = stopTime, direction=ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test6',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 6: ringing = ', ringing, trim(clockCurrTime(clock)), ' sticky = ', sticky
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 6: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo

    rc = ESMF_FAILURE
    if(nrings == 1) rc = ESMF_SUCCESS
  end subroutine

!------------------------------------------------------------------------
! Forward and Reverse
!------------------------------------------------------------------------
  subroutine ForwardReverseAlarm_Test1(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS
    print *, 'Forward and Reverse Alarms Tests: '
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 1: c.t0 = a.t0, c.dt = a.dt = 10 min
    print *, 'Test 1: c.t0 = a.t0, c.dt = a.dt = 10 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test1',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
      if(ringing) nrings = nrings + 1
    enddo
    call ESMF_ClockSet(clock, direction = ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 1: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo
    rc = ESMF_FAILURE
    print *, nrings
    if(nrings == 13) rc = ESMF_SUCCESS
  end subroutine

  subroutine ForwardReverseAlarm_Test2(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS
    print *, 'Forward and Reverse Alarms Tests: '
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 2: c.t0 = a.t0, c.dt = a.dt = 10 min
    print *, 'Test 2: c.t0 = a.t0, c.dt = a.dt = 20 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=1200, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test1',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
      if(ringing) nrings = nrings + 1
    enddo
    call ESMF_ClockSet(clock, direction = ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 2: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo
    rc = ESMF_FAILURE
    print *, nrings
    if(nrings == 7) rc = ESMF_SUCCESS
  end subroutine

  subroutine ForwardReverseAlarm_Test3(rc)

    integer, intent(out) :: rc

    type (ESMF_Clock) :: clock
    type (ESMF_Alarm) :: alarm
    type (ESMF_Time)  :: startTime, stopTime, ringTime
    type (ESMF_TimeInterval)  :: timeStep, ringTimeInterval, ringDuration

    integer           :: status, n, nrings=0
    logical           :: ringing, enabled, sticky

    rc = ESMF_SUCCESS
    print *, 'Forward and Reverse Alarms Tests: '
    call ESMF_TimeSet(startTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(stopTime,yy=1,mm=1,dd=1,h=1,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeSet(ringTime,yy=1,mm=1,dd=1,h=0,m=0,s=0,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

  ! Test 3: c.t0 = a.t0, c.dt = a.dt = 10 min
    print *, 'Test 3: c.t0 = a.t0, c.dt = a.dt = 12 min'
    call ESMF_TimeIntervalSet(timeStep,S=600, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_TimeIntervalSet(ringTimeInterval,S=720, sN=0, sD=1,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    clock = ESMF_ClockCreate(timeStep=timeStep,startTime=startTime,stopTime=stopTime,rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    alarm = ESMF_AlarmCreate(clock, ringTime=ringTime, ringInterval=ringTimeInterval, enabled=.true., sticky=.false., name='alarm', rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    call ESMF_AlarmDebug(alarm,'test1',rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()

    call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    if(ringing) nrings = nrings + 1
    print *, 'Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      print *, '  Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
      if(ringing) nrings = nrings + 1
    enddo
    call ESMF_ClockSet(clock, direction = ESMF_DIRECTION_REVERSE, rc=status)
    if(status /= ESMF_SUCCESS) call ESMF_Finalize()
    do n = 1, 6
      call ESMF_ClockAdvance(clock,rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      call ESMF_AlarmGet(alarm, ringing=ringing, enabled=enabled, sticky=sticky, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize()
      if(ringing) nrings = nrings + 1
      print *, '  Test 3: ringing = ', ringing, trim(clockCurrTime(clock))
    enddo
    rc = ESMF_FAILURE
    print *, nrings
    if(nrings == 3) rc = ESMF_SUCCESS
  end subroutine

end program ESMF_AlarmTest
