! $Id: ESMF_AlarmUTest.F90,v 1.11 2004/04/27 21:32:21 svasquez Exp $
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
      program ESMF_AlarmTest

!------------------------------------------------------------------------------
!

#include <ESMF.h>
 
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
      '$Id: ESMF_AlarmUTest.F90,v 1.11 2004/04/27 21:32:21 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YY, days, totalDays, secs, testResults

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1
      type(ESMF_Alarm) :: alarm, alarm1, alarm2, alarm3
      logical :: enabled, isringing, sticky, alarmsEqual, alarmsNotEqual
      logical :: willRingNext

      ! Random number
      real :: ranNum
      integer :: seed(32)
      integer :: timevals(8)

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             no_leapCalendar, esmf_360dayCalendar
      type(ESMF_CalendarType) :: cal_type

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2
      type(ESMF_Time) :: startTime, stopTime, alarmTime
      type(ESMF_Time) :: currentTime, previousTime, syncTime, stopTime3 
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      integer(ESMF_KIND_I8) :: advanceCounts, year, day2, month, minute, second
      integer(ESMF_KIND_I4) :: day, hour
      character(ESMF_MAXSTR) :: aName


      ! initialize ESMF framework
      call ESMF_Initialize(rc=rc)

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
      write(name, *) "Create Alarm Test"
      alarm3 = ESMF_AlarmCreate(name="WAKEUP", clock=clock1, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create Alarm Copy Test"
      alarm2 = ESMF_AlarmCreate(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      ! Testing for alarm equality
      ! alarmsEqual = ESMF_AlarmOperator(==)(alarm1,alarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarms Equal Test"
      alarmsEqual = (alarm1 == alarm2)
      call ESMF_Test((alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      ! Testing for alarm inequality:
      ! alarmsEqual = ESMF_AlarmOperator(==)(alarm1,alarm3)
      write(failMsg, *) "Returned equal"
      write(name, *) "Alarms Not Equal Test"
      alarmsEqual = (alarm1 == alarm3)
      call ESMF_Test((.not.alarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      ! Testing alarms equality using:
      ! alarmsNotEqual = ESMF_AlarmOperator(/=)(alarm1,alarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Alarms Equal Test"
      alarmsNotEqual = (alarm1 /= alarm2)
      call ESMF_Test((.not.alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      ! Testing alarms inequality using:
      ! alarmsNotEqual = ESMF_AlarmOperator(/=)(alarm1,alarm3)
      write(failMsg, *) "Returned equal"
      write(name, *) "Alarms Not Equal Test"
      alarmsNotEqual = (alarm1 /= alarm3)
      call ESMF_Test((alarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Validate Alarm Test"
      call ESMF_AlarmValidate(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not sticky"
      write(name, *) "Check if Alarm is sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(sticky), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not enabled"
      write(name, *) "Check if Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Alarm not sticky Test"
      call  ESMF_AlarmNotSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Get Alarm name Test"
      call  ESMF_AlarmGet(alarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."WAKEUP"), &
                      name, failMsg, result, ESMF_SRCLINE)
	print *, "Alarm name is ", aName


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Set Alarm name Test"
      call  ESMF_AlarmSet(alarm1, name="ALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Get Alarm name Test"
      call  ESMF_AlarmGet(alarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."ALARM1"), &
                      name, failMsg, result, ESMF_SRCLINE)
        print *, "Alarm name is ", aName


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned sticky"
      write(name, *) "Check if Alarm is not sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.sticky), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Alarm sticky Test"
      call  ESMF_AlarmSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not sticky"
      write(name, *) "Check if Alarm is sticky Test"
      sticky =  ESMF_AlarmIsSticky(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(sticky), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or is ringing"
      write(name, *) "Check if Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn on Ringing Alarm "
      call ESMF_AlarmRingerOn(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its not ringing"
      write(name, *) "Check if Alarm is ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn off Ringing Alarm "
      call ESMF_AlarmRingerOff(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its ringing"
      write(name, *) "Check if Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Enable Alarm Test"
      call ESMF_AlarmEnable(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned enabled"
      write(name, *) "Check if Alarm is enabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Alarm Print Test"
      call  ESMF_AlarmPrint(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Disable Alarm Test"
      call ESMF_AlarmDisable(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned enabled"
      write(name, *) "Check if Alarm is disabled Test"
      enabled =  ESMF_AlarmIsEnabled(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroy Alarm Test"
      call ESMF_AlarmDestroy(alarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


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
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=13, &
                                   h=1, m=45, s=27, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Stop Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=13, &
                                   h=18, m=45, s=27, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      ! Test Setting the Alarm Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Alarm Time Initialization Test"
      call ESMF_TimeSet(alarmTime, yy=2003, mm=3, dd=13, h=5, &
                                   calendarType=ESMF_CAL_GREGORIAN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      ! Initialize clock 
      !NEX_UTest
       write(name, *) "Clock Initialization Test"
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                                          stopTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Sticky Alarm Time Initialization Test"
      alarm =  ESMF_AlarmCreate(name="alarm1", clock=clock, ringTime=alarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is still ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
    ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

	print *, "bool is ", bool


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
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

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn off Ringing Alarm "
      call ESMF_AlarmRingerOff(alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its ringing"
      write(name, *) "Check if Alarm is not ringing Test"
      isringing = ESMF_AlarmIsRinging(alarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      ! Initialize clock 
      !NEX_UTest
       write(name, *) "Clock Initialization Test"
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       clock = ESMF_ClockCreate("Clock 1", timeStep, startTime, &
                                          stopTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Non-Sticky Alarm Time Initialization Test"
      alarm =  ESMF_AlarmCreate(name="alarm1", clock=clock, &
					ringTime=alarmTime, sticky=.FALSE., rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm will ring next Test"
      willRingNext = ESMF_AlarmWillRingNext(alarm, timeStep, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !NEX_UTest
      !Test Alarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm is still ringing Test"
      bool =  ESMF_AlarmIsRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
    ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

	print *, "bool is ", bool


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      !Test Alarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "Alarm Was Previously ringing Test"
      bool =  ESMF_AlarmWasPrevRinging(alarm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! finalize ESMF framework
      call ESMF_Finalize(rc)

      end program ESMF_AlarmTest
