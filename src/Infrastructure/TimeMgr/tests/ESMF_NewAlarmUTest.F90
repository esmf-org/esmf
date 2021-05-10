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
program ESMF_NewAlarmTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_NewAlarmTest - Test NewAlarm functionalities
!
! !DESCRIPTION:
!
! The code in this file drives F90 NewAlarm unit tests.
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
      type(ESMF_NewAlarm) :: newalarm1

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, &
                             no_leapCalendar, esmf_360dayCalendar

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, stopTime
      type(ESMF_Time) :: newalarmTime

      logical :: isCreated

#ifdef ESMF_TESTEXHAUSTIVE
      logical :: bool

      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock2, clock3, domainClock, CLOCK_ATM
      logical :: newalarmCountPass, isringing, sticky, enabled
      integer(ESMF_KIND_I8) :: forwardCount
      logical :: willRingNext, testPass, newalarmsNotEqual, newalarmsEqual
      type(ESMF_Direction_Flag) :: reverseDirection, forwardDirection
      type(ESMF_NewAlarm) :: newalarm, newalarm2, afterNewAlarm, beforeNewAlarm
      type(ESMF_NewAlarm) :: newalarmList(201), newalarmListOne(1)
      type(ESMF_NewAlarm) :: newalarm6, newalarm7, newalarm8
      type(ESMF_NewAlarm) :: newalarm5(200), newalarm3, newalarm4
      type(ESMF_NewAlarm) :: NEWALARM_HISTORY

      integer(ESMF_KIND_I8) :: reverseCount, iteration
      integer :: dd, nclock, ringCount, expectedCount, i, yy
      integer :: mm, m, h, sstep, nstep, nring, newalarmCount

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: TIMEINTERVAL_HISTORY, newalarmStep, newalarmStep2
      type(ESMF_TimeInterval) :: runDuration, ringDuration
      type(ESMF_Time) :: currentTime, currTime, afterNewAlarmTime, beforeNewAlarmTime
      type(ESMF_Time) :: newalarmStopTime, nextTime, prevTime, currentTime2, time1

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
      write(name, *) "NewAlarm Time Initialization Test"
      call ESMF_TimeSet(newalarmTime, yy=2003, mm=9, dd=15, &
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
      write(name, *) "Create NewAlarm Test"
      newalarm1 = ESMF_NewAlarmCreate(name="WAKEUP", clock=clock1, ringTime=newalarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroy NewAlarm Test"
      call ESMF_NewAlarmDestroy(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing NewAlarm IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_NewAlarmIsCreated(newalarm1)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing NewAlarm IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_NewAlarmIsCreated(newalarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test NewAlarm for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  newalarm1 = ESMF_NewAlarmCreate(name="WAKEUP", clock=clock1, ringTime=newalarmTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing NewAlarm IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_NewAlarmIsCreated(newalarm1)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing NewAlarm IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_NewAlarmIsCreated(newalarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test NewAlarm for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_NewAlarmDestroy(newalarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing NewAlarm IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_NewAlarmIsCreated(newalarm1)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing NewAlarm IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_NewAlarmIsCreated(newalarm1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------




#ifdef ESMF_TESTEXHAUSTIVE
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and newalarmCount = 0"
      write(name, *) "Get number of newalarms after destroyed NewAlarm Test"
      call ESMF_ClockGet(clock1, newalarmCount=newalarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.newalarmCount.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "newalarmCount = ", newalarmCount

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_FAILURE"
      write(name, *) "Get a destroyed NewAlarm Test"
      call ESMF_ClockGetNewAlarm(clock1, "WAKEUP", newalarm=newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Advance clock after destroyed newalarm test"
      call ESMF_ClockAdvance(clock1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed NewAlarm Test"
      call ESMF_NewAlarmDestroy(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Destroy a non-created NewAlarm Test"
      call ESMF_NewAlarmDestroy(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed NewAlarm is sticky Test"
      sticky =  ESMF_NewAlarmIsSticky(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created NewAlarm is sticky Test"
      sticky =  ESMF_NewAlarmIsSticky(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Validate destroyed NewAlarm Test"
      call ESMF_NewAlarmValidate(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Validate non-created NewAlarm Test"
      call ESMF_NewAlarmValidate(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed NewAlarm is enabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created NewAlarm is enabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed NewAlarm is not ringing Test"
      isringing = ESMF_NewAlarmIsRinging(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created NewAlarm is not ringing Test"
      isringing = ESMF_NewAlarmIsRinging(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Get name of destroyed NewAlarm Test"
      call  ESMF_NewAlarmGet(newalarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Get name of destroyed NewAlarm Test"
      call  ESMF_NewAlarmGet(newalarm6, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Set name of destroyed NewAlarm Test"
      call  ESMF_NewAlarmSet(newalarm1, name="NEWALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Set name of non-created NewAlarm Test"
      call  ESMF_NewAlarmSet(newalarm6, name="NEWALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Turn on Ringing on destroyed NewAlarm "
      call ESMF_NewAlarmRingerOn(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Turn on Ringing on non-created NewAlarm "
      call ESMF_NewAlarmRingerOn(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Turn off Ringing on destroyed NewAlarm "
      call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Turn off Ringing on non-created NewAlarm "
      call ESMF_NewAlarmRingerOff(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Enable destroyed NewAlarm Test"
      call ESMF_NewAlarmEnable(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Enable non-create NewAlarm Test"
      call ESMF_NewAlarmEnable(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Check if destroyed NewAlarm is enabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Check if non-created NewAlarm is enabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Disable destroyed NewAlarm Test"
      call ESMF_NewAlarmDisable(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Disable non-created NewAlarm Test"
      call ESMF_NewAlarmDisable(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test if destroyed NewAlarm Previously ringing
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroyed NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test if non-created NewAlarm Previously ringing
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Non-created NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Turn off Ringing on destroyed NewAlarm "
      call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Turn off Ringing on non-created NewAlarm "
      call ESMF_NewAlarmRingerOff(newalarm6, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test destroyed NewAlarm will ring next
      write(failMsg, *) " Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroyed NewAlarm will ring next Test"
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm1, timeStep=timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test non-created NewAlarm will ring next
      write(failMsg, *) " Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Non-created NewAlarm will ring next Test"
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm6, timeStep=timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create NewAlarm Test"
      newalarm1 = ESMF_NewAlarmCreate(name="WAKEUP1", clock=clock1, ringTime=newalarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create NewAlarm Test"
      newalarm3 = ESMF_NewAlarmCreate(name="WAKEUP3", clock=clock1, ringTime=newalarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Create NewAlarm Copy Test"
      newalarm2 = ESMF_NewAlarmCreate(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Clock Get NewAlarm List Test 1 - optional args missing"
      call ESMF_ClockGetNewAlarmList(clock1, ESMF_NEWALARMLIST_ALL, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Clock Get NewAlarm List Test 2 - only newalarmList specified"
      call ESMF_ClockGetNewAlarmList(clock1, ESMF_NEWALARMLIST_ALL, &
                                  newalarmList=newalarmList, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and newalarmCount=3"
      write(name, *) "Clock Get NewAlarm List Test 3 - only newalarmCount specified"
      call ESMF_ClockGetNewAlarmList(clock1, ESMF_NEWALARMLIST_ALL, &
                                  newalarmCount=newalarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.newalarmCount.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and newalarmCount=1"
      write(name, *) "Clock Get NewAlarm List Test 4 - single-element length newalarmList specified"
      call ESMF_NewAlarmRingerOn(newalarm1, rc=rc)
      call ESMF_ClockGetNewAlarmList(clock1, ESMF_NEWALARMLIST_RINGING, &
                                  newalarmList=newalarmListOne, &
                                  newalarmCount=newalarmCount, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.newalarmCount.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
      !call ESMF_NewAlarmPrint(newalarmListOne(1), options="name", rc=rc)
      !print *, "newalarmCount = ", newalarmCount

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and newalarmCount = 3"
      write(name, *) "Clock Get NewAlarm List Test 5"
      call ESMF_ClockGetNewAlarmList(clock1, ESMF_NEWALARMLIST_ALL, &
                                  newalarmList=newalarmList, newalarmCount=newalarmCount, &
                                  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS.and.newalarmCount.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)
      !call ESMF_NewAlarmPrint(newalarmList(1), options="name", rc=rc)
      !call ESMF_NewAlarmPrint(newalarmList(2), options="name", rc=rc)
      !call ESMF_NewAlarmPrint(newalarmList(3), options="name", rc=rc)
      !print *, "newalarmCount = ", newalarmCount

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmAssignment(=)(newalarm,newalarm)
      write(failMsg, *) "Returned not equal"
      write(name, *) "NewAlarm Assignment Test"
      newalarm8 = newalarm1  ! exercise default F90 NewAlarm = assignment
      call ESMF_NewAlarmGet(newalarm8, name=aName, clock=clock3, &
                         ringTime=time1, rc=rc)
      call ESMF_Test((newalarm8==newalarm1 .and. aName=="WAKEUP1" .and. &
                      clock3==clock1 .and. time1==newalarmTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm1,newalarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "NewAlarms Equal Test"
      newalarmsEqual = (newalarm1 == newalarm2)  ! exercise NewAlarm == operator
      call ESMF_Test((newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm1,newalarm2)
      write(failMsg, *) "Returned equal"
      write(name, *) "NewAlarms Not Equal Test"
      newalarmsEqual = (newalarm1 == newalarm3)  ! exercise NewAlarm == operator
      call ESMF_Test((.not.newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest 
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm1,newalarm2)
      write(failMsg, *) "Returned not equal"
      write(name, *) "NewAlarms Equal Test"
      newalarmsNotEqual = (newalarm1 /= newalarm2)  ! exercise NewAlarm /= operator
      call ESMF_Test((.not.newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_NewAlarmDestroy(newalarm2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm1,newalarm2)
      write(failMsg, *) "Returned equal"
      write(name, *) "NewAlarms Not Equal Test"
      newalarmsNotEqual = (newalarm1 /= newalarm3)  ! exercise NewAlarm /= operator
      call ESMF_Test((newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_NewAlarmDestroy(newalarm3, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted NewAlarm Not Equal Created NewAlarm Test 1"
      newalarmsEqual = (newalarm2 == newalarm1)  ! exercise NewAlarm == operator
      call ESMF_Test((.not.newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted NewAlarm Not Equal Uncreated NewAlarm Test 1"
      newalarmsEqual = (newalarm3 == newalarm7)  ! exercise NewAlarm == operator
      call ESMF_Test((.not.newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated NewAlarm Not Equal Created NewAlarm Test 1"
      newalarmsEqual = (newalarm7 == newalarm1)  ! exercise NewAlarm == operator
      call ESMF_Test((.not.newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm,newalarm1)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Deleted NewAlarm Equal Deleted NewAlarm Test 1"
      newalarmsEqual = (newalarm2 == newalarm3)  ! exercise NewAlarm == operator
      call ESMF_Test((newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(==)(newalarm,newalarm1)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Uncreated NewAlarm Equal Uncreated NewAlarm Test 1"
      newalarmsEqual = (newalarm7 == newalarm7)  ! exercise NewAlarm == operator
      call ESMF_Test((newalarmsEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted NewAlarm Not Equal Created NewAlarm Test 2"
      newalarmsNotEqual = (newalarm2 /= newalarm1)  ! exercise NewAlarm /= operator
      call ESMF_Test((newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Deleted NewAlarm Not Equal Uncreated NewAlarm Test 2"
      newalarmsNotEqual = (newalarm3 /= newalarm7)  ! exercise NewAlarm /= operator
      call ESMF_Test((newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated NewAlarm Not Equal Created NewAlarm Test 2"
      newalarmsNotEqual = (newalarm7 /= newalarm1)  ! exercise NewAlarm /= operator
      call ESMF_Test((newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm,newalarm1)
      write(failMsg, *) "Returned not equal"
      write(name, *) "Deleted NewAlarm Equal Deleted NewAlarm Test 2"
      newalarmsNotEqual = (newalarm2 /= newalarm3)  ! exercise NewAlarm /= operator
      call ESMF_Test((.not.newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing ESMF_NewAlarmOperator(/=)(newalarm,newalarm1)
      write(failMsg, *) "Returned equal"
      write(name, *) "Uncreated NewAlarm Equal Uncreated NewAlarm Test 2"
      newalarmsNotEqual = (newalarm7 /= newalarm7)  ! exercise NewAlarm /= operator
      call ESMF_Test((.not.newalarmsNotEqual), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Validate NewAlarm Test"
      call ESMF_NewAlarmValidate(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not sticky"
      write(name, *) "Check if NewAlarm is sticky Test"
      sticky =  ESMF_NewAlarmIsSticky(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(sticky), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not enabled"
      write(name, *) "Check if NewAlarm is enabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set NewAlarm not sticky Test"
      call  ESMF_NewAlarmNotSticky(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Get NewAlarm name Test"
      call  ESMF_NewAlarmGet(newalarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."WAKEUP1"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "NewAlarm name is ", aName

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Set NewAlarm name Test"
      call  ESMF_NewAlarmSet(newalarm1, name="NEWALARM1", rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or name is not correct"
      write(name, *) "Get NewAlarm name Test"
      call  ESMF_NewAlarmGet(newalarm1, name=aName, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aName.eq."NEWALARM1"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *, "NewAlarm name is ", aName

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned sticky"
      write(name, *) "Check if NewAlarm is not sticky Test"
      sticky =  ESMF_NewAlarmIsSticky(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.sticky), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set NewAlarm sticky Test"
      call  ESMF_NewAlarmSticky(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned not sticky"
      write(name, *) "Check if NewAlarm is sticky Test"
      sticky =  ESMF_NewAlarmIsSticky(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(sticky), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or is ringing"
      write(name, *) "Check if NewAlarm is not ringing Test"
      isringing = ESMF_NewAlarmIsRinging(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn on Ringing NewAlarm "
      call ESMF_NewAlarmRingerOn(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its not ringing"
      write(name, *) "Check if NewAlarm is ringing Test"
      isringing = ESMF_NewAlarmIsRinging(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn off Ringing NewAlarm "
      call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its ringing"
      write(name, *) "Check if NewAlarm is not ringing Test"
      isringing = ESMF_NewAlarmIsRinging(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Enable NewAlarm Test"
      call ESMF_NewAlarmEnable(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned enabled"
      write(name, *) "Check if NewAlarm is enabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "NewAlarm Print Test"
      call  ESMF_NewAlarmPrint(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Disable NewAlarm Test"
      call ESMF_NewAlarmDisable(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned enabled"
      write(name, *) "Check if NewAlarm is disabled Test"
      enabled =  ESMF_NewAlarmIsEnabled(newalarm1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.enabled), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroy NewAlarm Test"
      call ESMF_NewAlarmDestroy(newalarm1, rc=rc)
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
      ! Test Setting the NewAlarm Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set NewAlarm Time Initialization Test"
      call ESMF_TimeSet(newalarmTime, yy=2003, mm=3, dd=13, h=5, &
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
      write(name, *) "Sticky NewAlarm Time Initialization Test"
      newalarm =  ESMF_NewAlarmCreate(name="newalarm1", clock=clock, ringTime=newalarmTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Get NewAlarm Test"
      call ESMF_ClockGetNewAlarm(clock, newalarmname="newalarm1", newalarm=newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm will ring next Test"
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm, timeStep=timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Get Clock Next Time
      write(failMsg, *) " Did not return ESMF_SUCCESS)"
      write(name, *) "Get Clock Next Time Test"
      call ESMF_ClockGetNextTime(clock, nextTime, timeStep=timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm will ring next Test"
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm, timeStep=timeStep, rc=rc)
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
      !Testing ESMF_TimeOperator(==)(time1,time2)
      write(failMsg, *) " Next Time not equal to current Time"
      write(name, *) "Get Clock Next Time Test"
      bool = (nextTime == currentTime)  ! exercising Time == operator
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm is ringing Test"
      bool =  ESMF_NewAlarmIsRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm is still ringing Test"
      bool =  ESMF_NewAlarmIsRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "bool is ", bool

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm is still ringing Test"
      bool =  ESMF_NewAlarmIsRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Turn off Ringing NewAlarm "
      call ESMF_NewAlarmRingerOff(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS or its ringing"
      write(name, *) "Check if NewAlarm is not ringing Test"
      isringing = ESMF_NewAlarmIsRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.isringing), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_NewAlarmDestroy (newalarm, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------

      ! Initialize clock 
      !EX_UTest
       write(name, *) "Clock Initialization Test"
       write(failMsg, *) " Did not return ESMF_SUCCESS"
       clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock 1", rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Time Initialization Test"
      newalarm =  ESMF_NewAlarmCreate(name="newalarm1", clock=clock, &
          ringTime=newalarmTime, sticky=.FALSE., rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test NewAlarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm will ring next Test"
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm, timeStep=timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm will ring next
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm will ring next Test"
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm, timeStep=timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(willRingNext), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm is ringing Test"
      bool =  ESMF_NewAlarmIsRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm still ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm is still ringing Test"
      bool =  ESMF_NewAlarmIsRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "bool is ", bool

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm Previously ringing
      call ESMF_ClockAdvance(clock, rc=rc)
      write(failMsg, *) " Did not return ESMF_SUCCESS or returned wrong state"
      write(name, *) "NewAlarm Was Previously ringing Test"
      bool =  ESMF_NewAlarmWasPrevRinging(newalarm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test destroying newalarm
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroying newalarm test"
      call ESMF_NewAlarmDestroy (newalarm, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test destroying clock after newalarm
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Destroying clock after newalarm test"
      call ESMF_ClockDestroy (clock, rc=rc)
      call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky NewAlarms 1
      !  from Chris Hill via support issue 988241, bug 996229
      write(failMsg, *) " Did not return nstep=48, sstep=73, i=144, and ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Test 1"
      call ESMF_TimeIntervalSet(timeStep, s=100, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, s=150, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=3, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, sticky=.false., rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! starting time step number for first newalarm ring
      sstep = 0

      ! total clock time steps
      i = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        i = i + 1
        call ESMF_ClockGetNewAlarmList(clock2, ESMF_NEWALARMLIST_RINGING, &
                                    newalarmList=newalarmList, &
                                    newalarmCount=newalarmCount, rc=rc)
        if (newalarmCount .gt. 0) then
          if (sstep .eq. 0) sstep = i
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS) &
                      .and.(nstep.eq.48).and.(sstep.eq.73).and.(i.eq.144), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test Non-Sticky NewAlarms 2
      !  Test ringDuration and stopTime
      write(failMsg, *) " Did not return nstep=12 and ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Test 2"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=2, rc=rc)
      call ESMF_TimeIntervalSet(ringDuration, m=30, rc=rc)
      call ESMF_TimeSet(newalarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, &
                                ringDuration=ringDuration, &
                                stopTime=newalarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_NewAlarmIsRinging(newalarm4)) then
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.12), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky NewAlarms 3
      !  Test ringTimeStepCount and stopTime
      write(failMsg, *) " Did not return nstep=18 and ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Test 3"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=2, rc=rc)
      call ESMF_TimeSet(newalarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, &
                                ringTimeStepCount=3, &
                                stopTime=newalarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_NewAlarmIsRinging(newalarm4)) then
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky NewAlarms 4
      !  Test ringTimeStepCount precedence over ringDuration
      write(failMsg, *) " Did not return nstep=18 and ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Test 4"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=2, rc=rc)

      call ESMF_TimeIntervalSet(ringDuration, m=30, rc=rc)

      call ESMF_TimeSet(newalarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, &
                                ringTimeStepCount=3, &
                                ringDuration=ringDuration, &
                                stopTime=newalarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_NewAlarmIsRinging(newalarm4)) then
          nstep = nstep + 1
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky NewAlarms 5
      !  Test reverse non-sticky newalarm 
      write(failMsg, *) "Did not return nstep=24, forwardCount=96, reverseCount=0, etc., and ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Test 5"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=2, rc=rc)
      call ESMF_TimeIntervalSet(ringDuration, m=30, rc=rc)
      call ESMF_TimeSet(newalarmStopTime, yy=2000, mm=1, dd=1, h=13, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, &
                                ringDuration=ringDuration, &
                                stopTime=newalarmStopTime, sticky=.false., rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          !call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          !call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          if (ESMF_NewAlarmIsRinging(newalarm4)) then
            nstep = nstep + 1
            !print *, "on"
            !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          else
            !print *, "off"
          endif
          !call ESMF_NewAlarmPrint(newalarm4, options="ringtime string", rc=rc)
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
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

        call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)
        nclock = nclock + 1

      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.24).and. &
                     (forwardCount.eq.96).and.(reverseCount.eq.0).and. &
                     (forwardDirection.eq.ESMF_DIRECTION_FORWARD).and. &
                     (reverseDirection.eq.ESMF_DIRECTION_REVERSE).and. &
                     ESMF_ClockIsReverse(clock2), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nstep = ", nstep, " nclock = ", nclock
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Non-Sticky NewAlarms 6
      !  Test reverse non-sticky newalarm with negative timestep
      write(failMsg, *) "Did not return nring=48, forwardCount=96, reverseCount=0, etc., and ESMF_SUCCESS"
      write(name, *) "Non-Sticky NewAlarm Test 6"
      call ESMF_TimeIntervalSet(timeStep, m=-15, rc=rc)
      call ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=-2, rc=rc)
      call ESMF_TimeIntervalSet(ringDuration, m=-30, rc=rc)
      call ESMF_TimeSet(newalarmStopTime, yy=1999, mm=12, dd=31, h=21, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, &
                                ringDuration=ringDuration, &
                                stopTime=newalarmStopTime, &
                                sticky=.false., rc=rc)
      ! number of clock time steps newalarm rings for
      nring = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          if (ESMF_NewAlarmIsRinging(newalarm4)) then
            nring = nring + 1
            !print *, "ringing!"
            !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          endif
          !call ESMF_NewAlarmPrint(newalarm4, options="ringbegin string", rc=rc)
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          !call ESMF_NewAlarmPrint(newalarm4, options="ringtime string", rc=rc)
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
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

        !print *, "Going in REVERSE ..."
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)
        nclock = nclock + 1

      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nring.eq.48).and. &
                     (forwardCount.eq.96).and.(reverseCount.eq.0).and. &
                     (forwardDirection.eq.ESMF_DIRECTION_FORWARD).and. &
                     (reverseDirection.eq.ESMF_DIRECTION_REVERSE).and. &
                     ESMF_ClockIsReverse(clock2), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nring = ", nring, " nclock = ", nclock
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 1
      !  Test reverse sticky newalarm 
      write(failMsg, *) " Did not return nstep=2 and ESMF_SUCCESS"
      write(name, *) "Sticky NewAlarm Test 1"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          if (ESMF_NewAlarmIsRinging(newalarm4)) then
            nstep = nstep + 1
            !print *, "on"
            call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
          else
            !print *, "off"
          endif
          call ESMF_ClockAdvance(clock2, rc=rc)
        enddo

        call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)
        nclock = nclock + 1
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.2), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nstep = ", nstep, " nclock = ", nclock

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 2
      !  Test reverse interval sticky newalarm 
      write(failMsg, *) " Did not return nstep=23 and ESMF_SUCCESS"
      write(name, *) "Sticky NewAlarm Test 2"
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, h=23, &
                        calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=2, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, rc=rc)
      ! number of clock time steps newalarm rings for
      nstep = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          call ESMF_ClockGet(clock2, currTime=currentTime, rc=rc)
          call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
          !print *, mm, "/", dd, "/", yy, " ", h, ":", m
          if (ESMF_NewAlarmIsRinging(newalarm4)) then
            nstep = nstep + 1
            !print *, "on"
            call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
          else
            !print *, "off"
          endif
          call ESMF_ClockAdvance(clock2, rc=rc)
        enddo

        call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)
        nclock = nclock + 1
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.23), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nstep = ", nstep, " nclock = ", nclock

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 3
      !  Test reverse one-shot sticky newalarms per WRF use case
      !  From Tom Henderson/WRF

      write(name, *) "Sticky NewAlarm Test 3"
      write(failMsg, *) " NewAlarms did not turn on/off at the correct time/iteration or did not return ESMF_SUCCESS"

      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_TimeSet(startTime, yy=2005, mm=6, dd=15, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2005, mm=6, dd=15, h=5, &
                        calendar=gregorianCalendar, rc=rc)
      domainClock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                     name="WRF Clock", rc=rc)

      call ESMF_TimeSet(beforeNewAlarmTime, yy=2005, mm=6, dd=15, h=2, &
                        calendar=gregorianCalendar, rc=rc)
      beforeNewAlarm = ESMF_NewAlarmCreate(clock=domainClock, &
                                     ringTime=beforeNewAlarmTime, rc=rc)

      call ESMF_TimeSet(afterNewAlarmTime, yy=2005, mm=6, dd=15, h=3, &
                        calendar=gregorianCalendar, rc=rc)
      afterNewAlarm  = ESMF_NewAlarmCreate(clock=domainClock, &
                                     ringTime=afterNewAlarmTime, rc=rc)

      ! any single failure will cause the whole test to fail
      testPass = .true.

      ! track loop iterations
      iteration = 0

      call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
      call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
      !print *, "Begin"
      !print *, mm, "/", dd, "/", yy, " ", h, ": 0"

      ! run the clock forward
      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        iteration = iteration + 1
        !print *, "Iteration = ", iteration

        if (ESMF_NewAlarmIsRinging(beforeNewAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 2) then
            testPass = .false.
          endif
          !print *, "  beforeNewAlarm on"
          call ESMF_NewAlarmRingerOff(beforeNewAlarm, rc=rc)
        else
          !print *, "  beforeNewAlarm off"
        endif

        ! would call WRF solver here, before clock advance
        !print *, "  Solve"

        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        !print *, " ClockAdvance()"
        !print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        if (ESMF_NewAlarmIsRinging(afterNewAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 3) then
            testPass = .false.
          endif
          !print *, "  afterNewAlarm on"
          call ESMF_NewAlarmRingerOff(afterNewAlarm, rc=rc)
        else
          !print *, "  afterNewAlarm off"
        endif
      enddo

      ! run the clock backwards
      call ESMF_ClockSet(domainClock, direction=ESMF_DIRECTION_REVERSE, rc=rc)
      !print *
      !print *, "domainClock set in reverse"
      !print *

      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        !print *, "Iteration = ", iteration

        if (ESMF_NewAlarmIsRinging(afterNewAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 3) then
            testPass = .false.
          endif
          !print *, "  afterNewAlarm on"
          call ESMF_NewAlarmRingerOff(afterNewAlarm, rc=rc)
        else
          !print *, "  afterNewAlarm off"
        endif

        ! advance clock
        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        !print *, " ClockAdvance()"
        !print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        ! would call WRF solver here, after clock advance
        !print *, "  Solve"

        if (ESMF_NewAlarmIsRinging(beforeNewAlarm, rc=rc)) then
          if (iteration .ne. 2 .or. h .ne. 2) then
            testPass = .false.
          endif
          !print *, "  beforeNewAlarm on"
          call ESMF_NewAlarmRingerOff(beforeNewAlarm, rc=rc)
        else
          !print *, "  beforeNewAlarm off"
        endif

        iteration = iteration - 1
      enddo

      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(afterNewAlarm, rc=rc)
      call ESMF_NewAlarmDestroy(beforeNewAlarm, rc=rc)
      call ESMF_ClockDestroy(domainClock, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 4
      !  Test reverse *interval* sticky newalarms; variation on WRF use case above

      write(name, *) "Sticky NewAlarm Test 4"
      write(failMsg, *) " NewAlarms did not turn on/off at the correct time/iteration or did not return ESMF_SUCCESS"

      call ESMF_TimeIntervalSet(timeStep, h=1, rc=rc)
      call ESMF_TimeSet(startTime, yy=2005, mm=6, dd=15, h=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2005, mm=6, dd=15, h=10, &
                        calendar=gregorianCalendar, rc=rc)
      domainClock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                     name="WRF Clock", rc=rc)

      call ESMF_TimeSet(beforeNewAlarmTime, yy=2005, mm=6, dd=15, h=2, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=3, rc=rc)
      beforeNewAlarm = ESMF_NewAlarmCreate(clock=domainClock, &
                                     ringTime=beforeNewAlarmTime, &
                                     ringInterval=newalarmStep, rc=rc)

      call ESMF_TimeSet(afterNewAlarmTime, yy=2005, mm=6, dd=15, h=3, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep2, h=2, rc=rc)
      afterNewAlarm  = ESMF_NewAlarmCreate(clock=domainClock, &
                                     ringTime=afterNewAlarmTime, &
                                     ringInterval=newalarmStep2, rc=rc)

      ! any single failure will cause the whole test to fail
      testPass = .true.

      ! track loop iterations
      iteration = 0

      call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
      call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
      !print *, "Begin"
      !print *, mm, "/", dd, "/", yy, " ", h, ": 0"

      ! run the clock forward
      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        iteration = iteration + 1
        !print *, "Iteration = ", iteration

        if (ESMF_NewAlarmIsRinging(beforeNewAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 2).and. &
              (iteration .ne. 5 .or. h .ne. 5).and. &
              (iteration .ne. 8 .or. h .ne. 8)) then
            testPass = .false.
          endif
          !print *, "  beforeNewAlarm on"
          call ESMF_NewAlarmRingerOff(beforeNewAlarm, rc=rc)
        else
          !print *, "  beforeNewAlarm off"
        endif

        ! would call WRF solver here, before clock advance
        !print *, "  Solve"

        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        !print *, " ClockAdvance()"
        !print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        if (ESMF_NewAlarmIsRinging(afterNewAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 3).and. &
              (iteration .ne. 4 .or. h .ne. 5).and. &
              (iteration .ne. 6 .or. h .ne. 7).and. &
              (iteration .ne. 8 .or. h .ne. 9)) then
            testPass = .false.
          endif
          !print *, "  afterNewAlarm on"
          call ESMF_NewAlarmRingerOff(afterNewAlarm, rc=rc)
        else
          !print *, "  afterNewAlarm off"
        endif
      enddo

      ! run the clock backwards
      call ESMF_ClockSet(domainClock, direction=ESMF_DIRECTION_REVERSE, rc=rc)
      !print *
      !print *, "domainClock set in reverse"
      !print *

      do while (.not. ESMF_ClockIsDone(domainClock, rc=rc))
        !print *, "Iteration = ", iteration

        if (ESMF_NewAlarmIsRinging(afterNewAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 3).and. &
              (iteration .ne. 4 .or. h .ne. 5).and. &
              (iteration .ne. 6 .or. h .ne. 7).and. &
              (iteration .ne. 8 .or. h .ne. 9)) then
            testPass = .false.
          endif
          !print *, "  afterNewAlarm on"
          call ESMF_NewAlarmRingerOff(afterNewAlarm, rc=rc)
        else
          !print *, "  afterNewAlarm off"
        endif

        ! advance clock
        call ESMF_ClockAdvance(domainClock, rc=rc)
        call ESMF_ClockGet(domainClock, currTime=currentTime, rc=rc)
        call ESMF_TimeGet(currentTime, yy=yy, mm=mm, dd=dd, h=h, rc=rc)
        !print *, " ClockAdvance()"
        !print *, mm, "/", dd, "/", yy, " ", h, ": 0"

        ! would call WRF solver here, after clock advance
        !print *, "  Solve"

        if (ESMF_NewAlarmIsRinging(beforeNewAlarm, rc=rc)) then
          if ((iteration .ne. 2 .or. h .ne. 2).and. &
              (iteration .ne. 5 .or. h .ne. 5).and. &
              (iteration .ne. 8 .or. h .ne. 8)) then
            testPass = .false.
          endif
          !print *, "  beforeNewAlarm on"
          call ESMF_NewAlarmRingerOff(beforeNewAlarm, rc=rc)
        else
          !print *, "  beforeNewAlarm off"
        endif

        iteration = iteration - 1
      enddo

      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(afterNewAlarm, rc=rc)
      call ESMF_NewAlarmDestroy(beforeNewAlarm, rc=rc)
      call ESMF_ClockDestroy(domainClock, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 5
      !  Test delay in checking for ringing sticky newalarms,
      !  from James Geiger in #2825456, case #2 in his newalarm_test.f90 reproducer
      write(name, *) "Sticky NewAlarm Test 5"
      write(failMsg, *) " NewAlarm did not ring at the correct times or did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, s=3600, rc=rc)
      call ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=3, &
                                   calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="Clock", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, h=3, rc=rc)
      newalarmTime = newalarmTime + newalarmStep
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, rc=rc)
      testPass = .false.
      iteration = 0
      nring = 0
      do while (.not.ESMF_ClockIsStopTime(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (iteration .eq. 10) then
          ! waiting period is over, clear any newalarm which may have occurred
          if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
            call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
          endif
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)

        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (iteration .gt. 10) then
          ! process any newalarm occuring after initial 10 hour waiting period
          if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
            nring = nring + 1
            !print *, "newalarm4 is ringing!"
            !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
            call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
          endif
        endif
      end do

      ! test in REVERSE mode (not from James)
      call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
      end do

      ! test double back in FORWARD mode (not from James)
      call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_FORWARD, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
      end do

      !print *, "newalarm4 rang ", nring, " times, clock stepped ", iteration, " times."
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      if (nring.eq.44 .and. iteration.eq.144) testPass = .true.
      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 6
      !  Test timeStep == ringInterval, from James Geiger in #2910203
      write(name, *) "Sticky NewAlarm Test 6"
      write(failMsg, *) " NewAlarm did not ring at the correct times or did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, s=900, rc=rc)
      call ESMF_TimeSet(startTime, yy=2003, mm=9, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      ! dd=3, instead of original 30 in Jims report, to shorten test run time
      call ESMF_TimeSet(stopTime, yy=2003, mm=9, dd=3, &
                                   calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="The Clock", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2003, mm=9, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, s=900, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, rc=rc)
      testPass = .false.
      iteration = 0
      nring = 0
      do while (.not.ESMF_ClockIsStopTime(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
      end do

      ! test in REVERSE mode (not from James)
      call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
      end do

      ! test double back in FORWARD mode (not from James),
      call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_FORWARD, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
      end do

      !print *, "newalarm4 rang ", nring, " times, clock stepped ", iteration, " times."
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      if (nring.eq.576 .and. iteration.eq.576) testPass = .true.
      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Sticky NewAlarms 7
      !  Test timeStep > ringInterval, inspired by James Geiger in #2910203
      write(name, *) "Sticky NewAlarm Test 7"
      write(failMsg, *) " NewAlarm did not ring at the correct times or did not return ESMF_SUCCESS"
      call ESMF_TimeIntervalSet(timeStep, s=1800, rc=rc)
      call ESMF_TimeSet(startTime, yy=2003, mm=9, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      ! dd=3, instead of original 30 in Jims report, to save test run time
      call ESMF_TimeSet(stopTime, yy=2003, mm=9, dd=3, &
                                   calendar=gregorianCalendar, rc=rc)
      clock2 = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                                name="The Clock", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=2003, mm=9, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, s=900, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, rc=rc)
      testPass = .false.
      iteration = 0
      nring = 0
      do while (.not.ESMF_ClockIsStopTime(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
        !call ESMF_NewAlarmPrint(newalarm4, options="ringtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      end do
      !print *, "At end of 1st forward run, nring = ", nring
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      ! TODO: test in REVERSE mode (not from James)
      !       currently doesn't ring in REVERSE
      call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
        !call ESMF_NewAlarmPrint(newalarm4, options="ringtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      end do
      !print *, "At end of reverse run, nring = ", nring
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      ! TODO: test double back in FORWARD mode (not from James)
      !       currently doesn't work until back into day 2  
      call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_FORWARD, rc=rc)

      do while (.not.ESMF_ClockIsDone(clock2, rc=rc))
        iteration = iteration + 1
        !print *, "clock2 timestep ", iteration
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock2, rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock2, options="currTime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm4, rc=rc)) then
          nring = nring + 1
          !print *, "newalarm4 is ringing!"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
        endif
        !call ESMF_NewAlarmPrint(newalarm4, options="ringtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      end do
      !print *, "At end of 2nd forward run, nring = ", nring
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      !print *, "newalarm4 rang ", nring, " times, clock stepped ", iteration, " times."
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      if (nring.eq.288 .and. iteration.eq.288) testPass = .true.
      call ESMF_Test(testPass.and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Fractional Time NewAlarms 1
      write(failMsg, *) " Did not return nstep=8, and ESMF_SUCCESS"
      write(name, *) "Fractional Time NewAlarm Test 1"
      call ESMF_TimeIntervalSet(timeStep, ms=10, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, ms=100, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        ms=200, calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, rc=rc)

      ! number of clock time steps newalarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_NewAlarmIsRinging(newalarm4)) then
          nstep = nstep + 1
          call ESMF_NewAlarmRingerOff(newalarm4)
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.8), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test Fractional Time NewAlarms 2
      write(failMsg, *) " Did not return nstep=4, and ESMF_SUCCESS"
      write(name, *) "Fractional Time NewAlarm Test 2"
      call ESMF_TimeIntervalSet(timeStep, sN=-22, sD=7, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, sN=-44, sD=7, rc=rc)
      call ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=1999, mm=12, dd=31, h=23, m=59, s=28, &
                        sN=4, sD=7, calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=53, &
                        sN=2, sD=7, calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                ringInterval=newalarmStep, rc=rc)

      !print *, "stopTime = "
      !call ESMF_TimePrint(stopTime)
      !print *, "newalarmTime = "
      !call ESMF_TimePrint(newalarmTime)
      !print *, "timeStep = "
      !call ESMF_TimeIntervalPrint(timeStep)
      !print *, "newalarmStep = "
      !call ESMF_TimeIntervalPrint(newalarmStep)

      ! number of clock time steps newalarm rings for
      nstep = 0

      ! run the clock
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        if (ESMF_NewAlarmIsRinging(newalarm4)) then
          nstep = nstep + 1
          call ESMF_NewAlarmRingerOff(newalarm4)
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nstep.eq.4), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime = clock startTime => should ring immediately
      !  upon newalarm creation.
      write(failMsg, *) " Did not return newalarm ringing and ESMF_SUCCESS"
      write(name, *) "Test NewAlarm ringTime = Clock startTime"
      call ESMF_TimeIntervalSet(timeStep, ms=10, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=startTime, rc=rc)

      bool = ESMF_NewAlarmIsRinging(newalarm4)

      call ESMF_Test((bool.and.rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime = clock startTime *and* ringInterval specified
      !  upon newalarm creation => should ring immediately.  Test 1
      !  From Tom Black in Support ticket 1989990.
      write(failMsg, *) " Did not return newalarm ringing and ESMF_SUCCESS"
      write(name, *) "NewAlarm ringTime = Clock startTime with ringInterval, test 1"
      call ESMF_TimeIntervalSet(timeStep, m=2, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, s=60, rc=rc)
      call ESMF_TimeSet(startTime, yy=2007, mm=9, dd=18, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2007, mm=9, dd=19, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(clock=clock2, ringTime=startTime, &
                                ringInterval=newalarmStep, rc=rc)

      call ESMF_NewAlarmGet(newalarm4, ringTime=newalarmTime, rc=rc)
      bool = ESMF_NewAlarmIsRinging(newalarm4)

      call ESMF_Test(((newalarmTime==startTime+newalarmStep).and.bool.and. &
                       rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime = clock startTime *and* ringInterval specified
      !  upon newalarm creation => should ring immediately.  Test 2
      !  From Tom Black in Support ticket 1989990.
      write(failMsg, *) " Did not return newalarm ringing 21 times and ESMF_SUCCESS"
      write(name, *) "NewAlarm ringTime = Clock startTime with ringInterval, test 2"

      ! any single failure will cause the whole test to fail
      testPass = .true.

      call ESMF_TimeIntervalSet(timeStep, s=60, rc=rc)
      call ESMF_TimeIntervalSet(runDuration, s=3600, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, s=180, rc=rc)
      call ESMF_TimeSet(startTime, yy=2007, mm=9, dd=18, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, &
                              runDuration=runDuration, name="Clock 2", rc=rc)

      ! Tom Black's NewAlarmCreate() ...
      newalarm4=ESMF_NewAlarmCreate(       &
         name             ='NEWALARM Recv from Parent'  &  !<-- Name of NewAlarm
        ,clock            =clock2    &  !<-- Each domain's ATM Driver Clock
        ,ringTime         =startTime &  !<-- First time the NewAlarm rings (ESMF)
        ,ringInterval     =newalarmStep &  !<-- Recv from my parent at this
                                        !    frequency (ESMF)
        ,ringTimeStepCount=1       &  !<-- The NewAlarm rings for this many
                                      !    timesteps
        ,sticky           =.false. &  !<-- NewAlarm does not ring until turned off
        ,rc               =rc)

      ! run the clock
      ringCount = 0
      do while (.not. ESMF_ClockIsStopTime(clock2, rc=rc))
        call ESMF_ClockGet(clock2, advanceCount=forwardCount, rc=rc)        
        !print *, "At clock timestep #", forwardCount

        call ESMF_NewAlarmGet(newalarm4, clock=clock1, rc=rc)
        call ESMF_ClockGet(clock1, currTime=currentTime2, rc=rc)
        !print *, "NewAlarm's clock's currTime ..."
        !call ESMF_TimePrint(currentTime2, options="string", rc=rc)
        if (clock2 /= clock1) then
           testPass = .false.
           !print *, "NewAlarm's clock and domain clock are *not* the same"
        endif

        call ESMF_ClockGet(clock2, currTime=currentTime, prevTime=prevTime, &
                           rc=rc)
        !print *, "Clock's currTime and prevTime ..."
        !call ESMF_TimePrint(currentTime, options="string", rc=rc)
        !call ESMF_TimePrint(prevTime, options="string", rc=rc)

        if (currentTime /= currentTime2) then
           testPass = .false.
           !print *, "NewAlarm's clock's currTime is *not* the same as the domain clock's currTime"
        endif

        call ESMF_NewAlarmGet(newalarm4, ringTime=newalarmTime, rc=rc)
        !print *, "NewAlarm's ringTime ..."
        !call ESMF_TimePrint(newalarmTime, options="string", rc=rc)

        if (ESMF_NewAlarmIsRinging(newalarm=newalarm4, rc=rc)) then
            ringCount = ringCount + 1
            !print *, " NewAlarm is ringing"
            if (newalarmTime /= currentTime+newalarmStep) then
               testPass = .false.
               !print *, "  NewAlarm ringTime *not* equal to clock currTime + newalarmStep"
            endif
        endif
        call ESMF_ClockAdvance(clock2, rc=rc)
      enddo

      call ESMF_ClockGet(clock2, advanceCount=forwardCount, rc=rc)        
      !print *, "End of clock run: At clock timestep #", forwardCount
      if (ESMF_NewAlarmIsRinging(newalarm=newalarm4, rc=rc)) then
          !print *, " NewAlarm is ringing"
          ringCount = ringCount + 1
      endif

      ! NewAlarm should have rung 21 times, including from timestep 0 (upon newalarm
      ! creation) through timestep 60 (end of clock run) inclusive.
      ! Final ringTime should be one newalarmStep past the clock end time.
      call ESMF_NewAlarmGet(newalarm4, ringTime=newalarmTime, rc=rc)
      call ESMF_Test(((newalarmTime==startTime+runDuration+newalarmStep).and. &
                       ringCount==21.and.testPass.and. &
                       rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !print *, "NewAlarm rang ", ringCount, " times."
      !print *, "NewAlarm's final ringTime, after final clockAdvance() ..."
      !call ESMF_TimePrint(newalarmTime, options="string", rc=rc)

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime = clock startTime *and* ringInterval specified
      !  upon newalarm creation => should ring immediately.  Test 3, test REVERSE
      !  back to clock starttime.  Robustness test for solution to
      !  Tom Black problem in Support ticket 1989990.
      write(failMsg, *) " Did not return newalarm ringing 41 times and ESMF_SUCCESS"
      write(name, *) "NewAlarm ringTime = Clock startTime with ringInterval, test 3 (reverse)"

      call ESMF_TimeIntervalSet(timeStep, s=60, rc=rc)
      call ESMF_TimeIntervalSet(runDuration, s=3600, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, s=180, rc=rc)
      call ESMF_TimeSet(startTime, yy=2007, mm=9, dd=18, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, &
                              runDuration=runDuration, name="Clock 2", rc=rc)

      ! like Tom Black's NewAlarmCreate(), but sticky
      newalarm4=ESMF_NewAlarmCreate(       &
         name             ='NEWALARM Recv from Parent'  &  !<-- Name of NewAlarm
        ,clock            =clock2    &  !<-- Each domain's ATM Driver Clock
        ,ringTime         =startTime &  !<-- First time the NewAlarm rings (ESMF)
        ,ringInterval     =newalarmStep &  !<-- Recv from my parent at this
                                        !    frequency (ESMF)
        ,rc               =rc)

      ! number of clock time steps newalarm rings for
      nring = 0

      ! number of times the clock has been run
      nclock = 0

      do while (nclock < 2)
        ! run the clock
        do while (.not. ESMF_ClockIsDone(clock2, rc=rc))
          if (ESMF_NewAlarmIsRinging(newalarm4)) then
            nring = nring + 1
            !print *, "on"
            call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
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

        call ESMF_ClockSet(clock2, direction=ESMF_DIRECTION_REVERSE, rc=rc)
        nclock = nclock + 1
      enddo

      ! count ring at the end point
      if (ESMF_NewAlarmIsRinging(newalarm4)) then
        nring = nring + 1
        !print *, "on"
        call ESMF_NewAlarmRingerOff(newalarm4, rc=rc)
      else
        !print *, "off"
      endif

      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(nring.eq.41).and. &
                     (forwardCount.eq.60).and.(reverseCount.eq.0).and. &
                     (forwardDirection.eq.ESMF_DIRECTION_FORWARD).and. &
                     (reverseDirection.eq.ESMF_DIRECTION_REVERSE).and. &
                     ESMF_ClockIsReverse(clock2), &
                      name, failMsg, result, ESMF_SRCLINE)

      !print *, "nring = ", nring, " nclock = ", nclock

      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime = clock startTime *and* ringInterval specified
      !  upon newalarm creation *and* after clock restored from restart state --
      !  with 2 calls to ESMF_ClockSet() -- should ring immediately.  
      !  This approach to restart will become obsolete with the implementation
      !  of WriteRestart()/ReadStart(), but it should remain technically valid,
      !  along with this unit test.  From Ratko Vasic in ticket #2685243.

      write(failMsg, *) " Did not return newalarm ringing and ESMF_SUCCESS"
      write(name, *) "NewAlarm ringTime = Clock startTime with ringInterval, after clock restore"

      call ESMF_TimeIntervalSet(timeStep, m=3, rc=rc)
      call ESMF_TimeIntervalSet(TIMEINTERVAL_HISTORY, h=3, rc=rc)
      call ESMF_TimeSet(startTime, mm=9, dd=18, yy=2007, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, mm=9, dd=19, yy=2007, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(currTime, mm=9, dd=18, yy=2007, h=12, &
                        calendar=gregorianCalendar, rc=rc)

      CLOCK_ATM=ESMF_ClockCreate( &
        name       ='CLOCK_ATM'   &  !<-- The ATM Clock's name
       ,timeStep   =timeStep      &  !<-- The fundamental timestep in
                                     !     this component
       ,startTime  =startTime     &  !<-- Start time of simulation
       ,stopTime   =stopTime      &  !<-- Stop time of simulation
       ,rc         =rc)

      ! 1st ClockSet() to reset currTime and advanceCount for restart
      call ESMF_ClockSet(             &
        clock       =CLOCK_ATM        &  !<-- The ATM Component's Clock
       ,currtime    =currTime         &  !<-- Current time of simulation
       ,advanceCount=240_ESMF_KIND_I8 &  !<-- Timestep at this current time
       ,rc          =rc)

      ! 2nd ClockSet() with same values that CLOCK_ATM already has; should
      ! have no affect
      call ESMF_ClockSet(clock    =CLOCK_ATM &
                        ,currtime =currTime  &
                        ,starttime=startTime &
                        ,rc       =rc)

      !call ESMF_ClockPrint(CLOCK_ATM, rc=rc)

      NEWALARM_HISTORY=ESMF_NewAlarmCreate(          &
        name             ='NEWALARM_HISTORY'      &
       ,clock            =CLOCK_ATM            &  ! <-- ATM Clock
       ,ringTime         =currTime             &  ! <-- Forecast/Restart start
                                                  !      time (ESMF)
       ,ringInterval     =TIMEINTERVAL_HISTORY &  ! <-- Time interval between
       ,ringTimeStepCount=1                    &  ! <-- The NewAlarm rings for
                                                  !      this many timesteps
       ,sticky           =.false.              &  ! <-- NewAlarm does not ring
                                                  !      until turned off
       ,rc               =rc) 

      !call ESMF_NewAlarmPrint(NEWALARM_HISTORY, rc=rc)

      call ESMF_NewAlarmGet(NEWALARM_HISTORY, ringTime=newalarmTime, rc=rc)
      bool = ESMF_NewAlarmIsRinging(NEWALARM_HISTORY)

      call ESMF_Test(((newalarmTime==currTime+TIMEINTERVAL_HISTORY).and.bool &
                       .and. rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy(NEWALARM_HISTORY, rc=rc)
      call ESMF_ClockDestroy(CLOCK_ATM, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm list re-allocation within clock, part 1
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "NewAlarm list reallocation Test 1"
      call ESMF_TimeIntervalSet(timeStep, ms=10, rc=rc)
      call ESMF_TimeIntervalSet(newalarmStep, ms=100, rc=rc)
      call ESMF_TimeSet(startTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
      clock2=ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                              name="Clock 2", rc=rc)
      call ESMF_TimeSet(newalarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        ms=200, calendar=gregorianCalendar, rc=rc)

      ! fill up clock's newalarmList
      do i=1,200
        newalarm5(i) = ESMF_NewAlarmCreate(clock=clock2, ringTime=newalarmTime, &
                                     ringInterval=newalarmStep, rc=rc)
      enddo

      ! add one more newalarm than there is space for (200), forcing a
      !   reallocation to 400 newalarms
      ! also, set 201st newalarm to be the first to ring upon the 1st timestep
      call ESMF_TimeSet(newalarmTime, yy=1999, mm=12, dd=31, h=23, m=59, s=59, &
                        ms=10, calendar=gregorianCalendar, rc=rc)
      newalarm4 = ESMF_NewAlarmCreate(name="201st NewAlarm", clock=clock2, &
                                ringTime=newalarmTime, ringInterval=newalarmStep, &
                                rc=rc)

      ! see if the 201st newalarm was successfully added to the clock
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm list re-allocation within clock, part 2
      write(failMsg, *) " Did not return 201 newalarms and name '201st NewAlarm'"
      write(name, *) "NewAlarm list reallocation Test 2"
      call ESMF_ClockGetNewAlarmList(clock2, ESMF_NEWALARMLIST_ALL, &
                                  newalarmList=newalarmList, &
                                  newalarmCount=newalarmCount, rc=rc)
      write(*,*) "rc=",rc
      call ESMF_NewAlarmGet(newalarmList(newalarmCount), name=aName, rc=rc)

      !print *, "newalarmCount = ", newalarmCount
      !print *, "201st newalarm name = ", aName
      write(*,*) "rc=",rc

      ! see if we have 201 newalarms and if the 201st newalarm has the right name!
      call ESMF_Test((newalarmCount.eq.201).and.(aName.eq."201st NewAlarm") &
                      .and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm list re-allocation within clock, part 3
      write(failMsg, *) " Did not return 201st newalarm ringing"
      write(name, *) "NewAlarm list reallocation Test 3"
      call ESMF_ClockAdvance(clock2, rc=rc)
      call ESMF_ClockGetNewAlarmList(clock2, ESMF_NEWALARMLIST_RINGING, &
                                  newalarmList=newalarmList, &
                                  newalarmCount=newalarmCount, rc=rc)
      ! double check ringing with NewAlarm API call
      isringing = ESMF_NewAlarmIsRinging(newalarm4, rc=rc)

      !print *, "newalarmCount = ", newalarmCount
      call ESMF_NewAlarmGet(newalarmList(newalarmCount), name=aName, rc=rc)
      !print *, "Ringing newalarm name = ", trim(aName), ", is ringing = ", &
            !isringing

      ! see if the 201st newalarm is the only one ringing
      call ESMF_Test(isringing.and.(newalarmCount.eq.1) &
                     .and.(aName.eq."201st NewAlarm") &
                     .and.(rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

      ! cleanup
      do i=1,200
        call ESMF_NewAlarmDestroy(newalarm5(i), rc=rc)
      enddo
      call ESMF_NewAlarmDestroy(newalarm4, rc=rc)
      call ESMF_ClockDestroy(clock2, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Based on reproducer clocktester.F90 from Atanas. See bug #1531948.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test ESMF_DIRECTION_FORWARD to a non-sticky newalarm " // &
                     "point, ESMF_DIRECTION_REVERSE, ESMF_DIRECTION_FORWARD"
      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2009, mm=1, dd=1, &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet (stopTime, yy=2009, mm=1, dd=2, h=0, &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, s=3600, rc=rc)
      clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                               name="ApplClock", rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeIntervalSet (newalarmStep, s=7200, rc=rc)
      !call ESMF_TimeIntervalSet(ringDuration, h=1, rc=rc)
      !call ESMF_TimeSet(newalarmStopTime, yy=2009, mm=1, dd=1, h=21, &
      !                  calendar=gregorianCalendar, rc=rc)
      newalarm1 = ESMF_NewAlarmCreate (clock=clock,  &
          ringTime=startTime, ringInterval=newalarmStep, &
      !    ringDuration=ringDuration, &
      !    stopTime=newalarmStopTime, &
          sticky=.false., name="testNewAlarm", rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      !call ESMF_NewAlarmPrint(newalarm1, options="ringduration string", rc=rc)
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      !call ESMF_NewAlarmPrint(newalarm1, options="ringtimestepcount string", rc=rc)
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      newalarmCount = 0
      expectedCount = 22 
      do
        !print *, "***********************Top of loop 1 **********************"
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock, options="currtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm1, rc=rc)) then
          newalarmCount = newalarmCount + 1
          !print *, "newalarm1 is ringing"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          !call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
        endif
        !call ESMF_NewAlarmPrint(newalarm1, options="ringbegin string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_NewAlarmPrint(newalarm1, options="ringtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_NewAlarmPrint(newalarm1, options="ringend string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock, rc=rc)
        !call ESMF_NewAlarmPrint(newalarm1, options="ringend string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_ClockIsStopTime(clock, rc=rc)) exit
      enddo
      !print *, "At end of 1st forward run, newalarmCount = ", newalarmCount
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=rc)

      i=0
      do
        !print *, "***********************Top of loop 2 **********************"
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock, options="currtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm1, rc=rc)) then
          newalarmCount = newalarmCount + 1
          !print *, "newalarm1 is ringing"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          !call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
        endif
        !call ESMF_NewAlarmPrint(newalarm1, options="ringbegin string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_NewAlarmPrint(newalarm1, options="ringtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock, rc=rc)
        i = i+1
        if (i == 5) exit
      enddo
      !print *, "At end of reverse run, newalarmCount = ", newalarmCount
      !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=rc)

      i=0
      do
        !print *, "***********************Top of loop 3 **********************"
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_ClockPrint(clock, options="currtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        if (ESMF_NewAlarmIsRinging(newalarm1, rc=rc)) then
          newalarmCount = newalarmCount + 1
          !print *, "newalarm1 is ringing"
          !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          !call ESMF_NewAlarmRingerOff(newalarm1, rc=rc)
        endif
        !call ESMF_NewAlarmPrint(newalarm1, options="ringbegin string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        !call ESMF_NewAlarmPrint(newalarm1, options="ringtime string", rc=rc)
        !call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
        call ESMF_ClockAdvance(clock, rc=rc)
        i = i+1
        if (i == 15) exit
      enddo

      if (.not. testPass .or. newalarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', newalarmCount = ', newalarmCount, ', expected = ', expectedCount
          print *, 'The newalarm ringTime may be stuck at:'
          call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
          call ESMF_NewAlarmPrint (newalarm1, options="ringTime string", rc=rc)
          call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)
      end if

      call ESMF_Test (testPass .and. newalarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)
      call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout)

      call ESMF_NewAlarmDestroy (newalarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      !Test NewAlarm ringTime increment, first forwards a fixed number of
      !timesteps, stopping at an newalarm ringing time step.
      !Using ESMF_DIRECTION_REVERSE, step backwards to some time prior to the
      !clock's startTime.  Then go ESMF_DIRECTION_FORWARD to one step past an
      !newalarm ringing time step, and then ESMF_DIRECTION_REVERSE once more. 
      ! Count number of rings.  See bug #1531948.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test ESMF_DIRECTION_FORWARD to an newalarm point, " // &
                     "ESMF_DIRECTION_REVERSE, ESMF_DIRECTION_FORWARD, " // &
                     "ESMF_DIRECTION_REVERSE"

      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      !call ESMF_TimePrint (startTime, options="string isofrac", rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      clock = ESMF_ClockCreate(startTime = startTime, timeStep=timeStep, &
                              name="clock 1", rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (newalarmTime, yy=2008, mm=1, dd=23, h=6,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (newalarmStep, h=6, rc=rc)
      newalarm1 = ESMF_NewAlarmCreate (  &
          name="NewAlarm 1", clock=clock,  &
          ringTime=newalarmTime, ringInterval=newalarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      newalarmCount = 0
      expectedCount = 11
      do, i=1,6
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at forwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at forwards timestep', i
        end if
      end do

      !print *, 'SETTING CLOCK BACKWARDS'
      call ESMF_ClockSet (clock, direction=ESMF_DIRECTION_REVERSE, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=5, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at backwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
          !call ESMF_NewAlarmPrint(newalarm1, options="ringTime string", rc=rc)
        else
          !print *, 'newalarm not ringing at backwards timestep', i
          !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        end if
      end do

      !print *, 'SETTING CLOCK FORWARDS'
      call ESMF_ClockSet (clock, direction=ESMF_DIRECTION_FORWARD, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=-4,7
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at forwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at forwards timestep', i
        end if
      end do

      !print *, 'SETTING CLOCK BACKWARDS'
      call ESMF_ClockSet (clock, direction=ESMF_DIRECTION_REVERSE, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=6, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at backwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
          !call ESMF_NewAlarmPrint(newalarm1, options="ringTime string", rc=rc)
        else
          !print *, 'newalarm not ringing at backwards timestep', i
          !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        end if
      end do

      if (.not. testPass .or. newalarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', newalarmCount = ', newalarmCount, ', expected = ', expectedCount
          print *, 'The newalarm ringTime may be stuck at:'
          call ESMF_NewAlarmPrint (newalarm1, options="ringTime string", rc=rc)
      end if

      call ESMF_Test (testPass .and. newalarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy (newalarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime increment, first forwards a fixed number of
      !timesteps, stopping at an newalarm ringing time step.  Using a negative
      !timestemp, step backwards to some time prior to the clock's startTime.
      !Count number of rings.  See bugs #1531948, #1457135.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test forward to an newalarm point, then step backward using a negative timeStep"

      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      clock = ESMF_ClockCreate(startTime = startTime, timeStep=timeStep, &
                               name="clock 1", rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (newalarmTime, yy=2008, mm=1, dd=23, h=6,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (newalarmStep, h=6, rc=rc)
      newalarm1 = ESMF_NewAlarmCreate (  &
          name="NewAlarm 1", clock=clock,  &
          ringTime=newalarmTime, ringInterval=newalarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      newalarmCount = 0
      expectedCount = 8
      do, i=1,6
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at forwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at forwards timestep', i
        end if
      end do

      !print *, 'SETTING CLOCK BACKWARDS WITH NEGATIVE TIMESTEP'
      timeStep = -timeStep
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=5, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at backwards timestep', i, ', time:'
          !call ESMF_ClockPrint (clock, options="currTime string")
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at backwards timestep', i
          !call ESMF_ClockPrint (clock, options="currTime string")
        end if
      end do

      if (.not. testPass .or. newalarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', newalarmCount = ', newalarmCount, ', expected = ', expectedCount
          print *, 'The newalarm ringTime may be stuck at:'
          call ESMF_NewAlarmPrint (newalarm1, options="ringTime string", rc=rc)
      end if

      call ESMF_Test (testPass .and. newalarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy (newalarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime increment, first forwards a fixed number of
      !timesteps, stopping at a non-newalarm ringing time step.  Using a negative
      !timestemp, step backwards to some time prior to the clock's startTime.
      !Count number of rings.  See bugs #1531948, #1457135.
      write(failMsg, *) " Did not ring enough times during forward/backward march"
      write(name, *) "Test forward to a non-ringing step, then step backward using a negative timeStep"

      testPass = .true.
      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      clock = ESMF_ClockCreate(startTime = startTime, timeStep=timeStep, &
                               name="clock 1", rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (newalarmTime, yy=2008, mm=1, dd=23, h=6,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (newalarmStep, h=6, rc=rc)
      newalarm1 = ESMF_NewAlarmCreate (  &
          name="NewAlarm 1", clock=clock,  &
          ringTime=newalarmTime, ringInterval=newalarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      newalarmCount = 0
      expectedCount = 9
      do, i=1,7
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at forwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at forwards timestep', i
        end if
      end do

      !print *, 'SETTING CLOCK BACKWARDS WITH NEGATIVE TIMESTEP'
      timeStep = -timeStep
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=6, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at backwards timestep', i, ', time:'
          !call ESMF_ClockPrint (clock, options="currTime string")
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at backwards timestep', i
          !call ESMF_ClockPrint (clock, options="currTime string")
        end if
      end do

      if (.not. testPass .or. newalarmCount /= expectedCount) then
          if (.not. testPass) print *, 'bad return codes discovered'
          write (failMsg,*) trim (failMsg), ', newalarmCount = ', newalarmCount, ', expected = ', expectedCount
          print *, 'The newalarm ringTime may be stuck at:'
          call ESMF_NewAlarmPrint (newalarm1, options="ringTime string", rc=rc)
      end if

      call ESMF_Test (testPass .and. newalarmCount == expectedCount, &
                     name, failMsg, result, ESMF_SRCLINE)

      ! 
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not show null clock in newalarm"
      write(name, *) "Test newalarm after destroying its associated clock"
      call ESMF_ClockDestroy (clock, rc=rc)
      willRingNext = ESMF_NewAlarmWillRingNext(newalarm1, rc=rc)
      call ESMF_Test (rc==ESMC_RC_PTR_NULL .and. .not.willRingNext, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy (newalarm1, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      !Test NewAlarm ringTime increment, arbitrarily forwards and backwards using
      ! positive and negative timesteps.  NewAlarm ringTime and ringInterval are
      ! changed with each change in timestep direction.  The extent of the run
      ! of each timestep value is different with each change in direction.
      write(failMsg, *) " Did not ring enough times during arbitrary forward/backward march"
      write(name, *) "Test arbitrary positive/negative timeStep runs with differing newalarms"

      testPass = .true.
      newalarmCountPass = .true.

      call ESMF_TimeSet (startTime, yy=2008, mm=1, dd=23, h=0,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (timeStep, h=1, rc=rc)
      clock = ESMF_ClockCreate(startTime=startTime, timeStep=timeStep, &
                               name="clock 1", rc=rc)
      if (rc /= ESMF_SUCCESS) testPass = .false.

      call ESMF_TimeSet (newalarmTime, yy=2008, mm=1, dd=23, h=2,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (newalarmStep, h=2, rc=rc)
      newalarm1 = ESMF_NewAlarmCreate (  &
          name="NewAlarm 1", clock=clock,  &
          ringTime=newalarmTime, ringInterval=newalarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      newalarmCount = 0
      expectedCount = 15

      do, i=1,7
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at forwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at forwards timestep', i
        end if
      end do
      if (newalarmCountPass .and. newalarmCount /= 3) then
        newalarmCountPass = .false.
        !print *, 'newalarmCount = ', newalarmCount, ' not 3 at 1st turnaround point'
      end if

      !print *, 'SETTING CLOCK BACKWARDS WITH NEW NEGATIVE TIMESTEP AND RINGINTERVAL'
      timeStep = -2 * timeStep
      newalarmStep = -2 * newalarmStep
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      call ESMF_NewAlarmSet (newalarm1, ringInterval=newalarmStep, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=6, -5, -1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at backwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at backwards timestep', i
        end if
      end do
      if (newalarmCountPass .and. newalarmCount /= 9) then
        newalarmCountPass = .false.
        !print *, 'newalarmCount = ', newalarmCount, ' not 9 at 2nd turnaround point'
      end if

      !print *, 'SETTING CLOCK FORWARDS WITH NEW POSITIVE TIMESTEP, RINGINTERVAL, and RINGTIME'
      call ESMF_TimeIntervalSet (timeStep, h=3, rc=rc)
      call ESMF_TimeSet (newalarmTime, yy=2008, mm=1, dd=22, h=11,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeIntervalSet (newalarmStep, h=6, rc=rc)
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      call ESMF_NewAlarmSet (newalarm1, ringTime=newalarmTime, ringInterval=newalarmStep, &
                          rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=1,5
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at forwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at forwards timestep', i
        end if
      end do
      if (newalarmCountPass .and. newalarmCount /= 11) then
        newalarmCountPass = .false.
        !print *, 'newalarmCount = ', newalarmCount, ' not 11 at 3rd turnaround point'
      end if

      !print *, 'SETTING CLOCK BACKWARDS WITH NEW NEGATIVE TIMESTEP AND RINGTIME'
      call ESMF_TimeIntervalSet (timeStep, h=-1, rc=rc)
      call ESMF_TimeSet (newalarmTime, yy=2008, mm=1, dd=22, h=21,  &
          calendar=gregorianCalendar, rc=rc)
      call ESMF_ClockSet (clock, timeStep=timeStep, rc=rc)
      call ESMF_NewAlarmSet (newalarm1, ringTime=newalarmTime, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.

      do, i=4,2,-1
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing at backwards timestep', i
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing at backwards timestep', i
        end if
      end do
      if (newalarmCountPass .and. newalarmCount /= 12) then
        newalarmCountPass = .false.
        !print *, 'newalarmCount = ', newalarmCount, ' not 12 at 4th turnaround point'
      end if

      !print *, 'SETTING CLOCK FORWARDS WITH NEW POSITIVE TIMESTEP'
      timeStep = -timeStep
      call ESMF_ClockGet(clock, currTime=startTime, rc=rc)
      call ESMF_ClockSet(clock, timeStep=timeStep, startTime=startTime, &
                         runTimeStepCount=15, rc=rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
   
      do while (.not. ESMF_ClockIsDone(clock, rc=rc))
        call ESMF_ClockAdvance (clock, rc=rc)
        !call ESMF_ClockPrint (clock, options="currTime string", rc=rc)
        if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
        if (ESMF_newalarmIsRinging (newalarm1)) then
          newalarmCount = newalarmCount + 1
          !print *, 'newalarm IS ringing'
          call ESMF_NewAlarmRingerOff (newalarm1, rc=rc)
        else
          !print *, 'newalarm not ringing'
        end if
      end do
      if (newalarmCountPass .and. newalarmCount /= expectedCount) then
        newalarmCountPass = .false.
        !print *, 'newalarmCount = ', newalarmCount, ', not ', expectedCount, ' at the end'
      end if

      if (.not. newalarmCountPass) then
          write(failMsg,*) trim(failMsg), ', newalarmCount incorrect at one or more turnaround points'
          !print *, 'Final newalarmCount = ', newalarmCount, ', expected = ', expectedCount
          !print *, 'Final newalarm ringTime:'
          !call ESMF_NewAlarmPrint (newalarm1, "ringTime string", rc=rc)
      end if

      if (.not. testPass) print *, 'bad return codes discovered'

      call ESMF_Test (testPass .and. newalarmCountPass, &
                     name, failMsg, result, ESMF_SRCLINE)

      call ESMF_NewAlarmDestroy (newalarm1, rc=rc)
      call ESMF_ClockDestroy (clock, rc=rc)

      ! ----------------------------------------------------------------------------
      ! The following tests are from Ben@NASA's support ticket 3614994
      write(failMsg, *) " NewAlarms did not rewind correct number of times "
      write(name, *) "Test multiple newalarms rewind correct number of times "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_ReverseNewAlarms(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " NewAlarms hang... "
      write(name, *) "Test multiple newalarms replay without hanging "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_NewAlarmHang(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " NewAlarms with ring intervals equal to clock interval, incorrect behavior "
      write(name, *) "Test running an newalarms forward-reverse-forward with ring interval equal to clock interval "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_NewAlarmAdvRewind(testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      write(failMsg, *) " NewAlarms reverse with sticky set... "
      write(name, *) "Test running an newalarm reverse with sticky bit set "
      rc = ESMF_SUCCESS
      testPass = .true.
      call Test_RevNewAlarmSticky(60._ESMF_KIND_R8, testPass, rc)
      if (testPass .and. rc /= ESMF_SUCCESS) testPass = .false.
      if (.not. testPass) print *, 'bad return codes discovered'
      call ESMF_Test (testPass, name, failMsg, result, ESMF_SRCLINE)

#endif

      ! destroy calendars
      call ESMF_CalendarDestroy(esmf_360dayCalendar, rc=rc)
      call ESMF_CalendarDestroy(no_leapCalendar, rc=rc)
      call ESMF_CalendarDestroy(julianCalendar, rc=rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)

      ! finalize ESMF framework
      call ESMF_TestEnd(ESMF_SRCLINE)

contains
  
 subroutine verify_(rc)
   integer, intent(in)  :: rc

   integer :: error_code,status
   if (rc /=0) then
      call ESMF_Finalize()
   end if
 end subroutine verify_

subroutine test_reverseNewAlarms(testPass, rc)
   implicit none

   logical, intent(out) :: testPass
   integer :: status,rc

   type(ESMF_TimeInterval) :: dt
   type(ESMF_Time) :: start_time, clock_start, clock_end
   type(ESMF_Clock) :: clock
   character(len=5) :: add_2nd
   integer :: nargs

   type(ESMF_TimeInterval) :: tint
   type(ESMF_NewAlarm) :: esmfnewalarm, firstnewalarm

   integer :: i,nstep

   type(ESMF_Time) :: time
   character(len=10) :: iam='test_clock'
   logical :: esmf_ring
   integer :: nnewalarms, n_rings=0

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
   firstnewalarm = ESMF_NewAlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name="newalarm1",rc=status)
   if(status /= ESMF_SUCCESS) call ESMF_Finalize()

   nstep=47
   call ESMF_ClockGet(clock,currtime=start_time,newalarmCount=nnewalarms,rc=status)
   !write(*,*) "newalarms already in clock: ",nnewalarms
   call verify_(status)
   call ESMF_TimeIntervalSet(tint,h=1,rc=status)
   call verify_(status)
   esmfnewalarm = ESMF_NewAlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name="newalarm2",rc=status)
   do i=1,nstep
      call ESMF_ClockGet(clock,currTime=time)
      esmf_ring = ESMF_NewAlarmIsRinging(esmfnewalarm,rc=status)
      call verify_(status)
      if ( esmf_ring) then
          !write(*,*)'ringing'
         !call ESMF_TimePrint(time,options='string')
      end if
      call ESMF_ClockAdvance(clock)
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=status)
   call verify_(status)
   !write(*,*)"*************** start rewind *********************"
   i = 0
   do
      i = i + 1
      !write(*,*) 'Rewind step: ', i
      call ESMF_ClockAdvance(clock,rc=status)
      call verify_(status)
      call ESMF_ClockGet(clock,currTime=time)
      if (ESMF_NewAlarmIsRinging(esmfnewalarm)) then
         !write(*,*)'rewinding one step ',ESMF_NewAlarmIsRinging(esmfnewalarm)
         n_rings = n_rings + 1
         !call ESMF_TimePrint(time,options='string')
      end if
      
      if (time == start_time) exit
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=status)
   call verify_(status)
   !write(*,*)"*************** end rewind *********************"
   do i=1,nstep*2
      call ESMF_ClockGet(clock,currTime=time)
      esmf_ring = ESMF_NewAlarmIsRinging(esmfnewalarm,rc=status)
      call verify_(status)
      if ( esmf_ring ) then
         !write(*,*)'ringing'
         !call ESMF_TimePrint(time,options='string')
      end if
      call ESMF_ClockAdvance(clock)
   enddo
   if(n_rings == 12) testPass = .true.

    call ESMF_NewAlarmDestroy(esmfnewalarm, rc=status)
    call verify_(status)
    call ESMF_NewAlarmDestroy(firstnewalarm, rc=status)
    call verify_(status)
    call ESMF_ClockDestroy(clock, rc=status)
    call verify_(status)
        
end subroutine Test_ReverseNewAlarms

subroutine Test_NewAlarmHang(testPass, rc)
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
   type(ESMF_NewAlarm) :: esmfnewalarm
   type(ESMF_NewAlarm) :: testnewalarm
   character(len=10) :: iam='test_clock'
   logical :: esmf_ring
   integer :: nnewalarms

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
   call ESMF_ClockGet(clock,currtime=start_time,newalarmCount=nnewalarms,rc=status)
   !write(*,*) "newalarms already in clock: ",nnewalarms
   call verify_(status)
   call ESMF_TimeIntervalSet(tint,h=1,rc=status)
   call verify_(status)
   esmfnewalarm = ESMF_NewAlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name="newalarm2",rc=status)
   !call ESMF_TimeIntervalSet(tint,h=3,rc=status)
   !testnewalarm = ESMF_NewAlarmCreate(clock=clock,name="newalarm3",ringInterval=tint, ringtime=start_time,sticky=.true.,rc=status)
   testnewalarm = ESMF_NewAlarmCreate(clock=clock,name="newalarm3",ringtime=start_time,sticky=.true.,rc=status)
   call verify_(status)
   call ESMF_NewAlarmRingerOff(testnewalarm,rc=status)
   call verify_(status)
   do i=1,nstep
      call ESMF_NewAlarmRingerOn(testnewalarm,rc=status)
      call verify_(status)
      call ESMF_ClockGet(clock,currTime=time)
      esmf_ring = ESMF_NewAlarmIsRinging(esmfnewalarm,rc=status)
      call verify_(status)
      if ( esmf_ring) then
         !write(*,*)'ringing: esmfnewalarm'
         call ESMF_TimePrint(time,options='string')
      end if
      call ESMF_ClockAdvance(clock)
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=status)
   call verify_(status)
   call ESMF_NewAlarmRingerOff(testnewalarm,rc=status)
   call verify_(status)
   !write(*,*)"*************** start rewind *********************"
   do
      call ESMF_ClockAdvance(clock,rc=status)
      call verify_(status)
      call ESMF_ClockGet(clock,currTime=time)
      if (ESMF_NewAlarmIsRinging(esmfnewalarm)) then
         !write(*,*)'rewinding one step: esmfnewalarm ',ESMF_NewAlarmIsRinging(esmfnewalarm)
         call ESMF_TimePrint(time,options='string')
      end if

      if (ESMF_NewAlarmIsRinging(esmfnewalarm)) then
         write(*,*)'rewinding one step: testnewalarm ',ESMF_NewAlarmIsRinging(testnewalarm)
         call ESMF_TimePrint(time,options='string')
      end if
      
      if (time == start_time) exit
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=status)
   call verify_(status)
   !write(*,*)"*************** end rewind *********************"
   do i=1,nstep*2
      call ESMF_ClockGet(clock,currTime=time)
      esmf_ring = ESMF_NewAlarmIsRinging(esmfnewalarm,rc=status)
      call verify_(status)
      if ( esmf_ring ) then
         !write(*,*)'ringing: esmfnewalarm'
         call ESMF_TimePrint(time,options='string')
      end if
      call ESMF_ClockAdvance(clock)
   enddo

    call ESMF_NewAlarmDestroy(esmfnewalarm, rc=status)
    call verify_(status)
    call ESMF_NewAlarmDestroy(testnewalarm, rc=status)
    call verify_(status)
    call ESMF_ClockDestroy(clock, rc=status)
    call verify_(status)

    testPass = .true.
   
end subroutine Test_NewAlarmHang

subroutine Test_NewAlarmAdvRewind(testPass, rc)
  logical, intent(out) :: testPass
  integer :: rc

   type(ESMF_TimeInterval) :: dt
   type(ESMF_Time) :: start_time, clock_start, clock_end
   type(ESMF_Clock) :: clock

   type(ESMF_TimeInterval) :: tint

   integer :: status,i,j,nstep

   logical :: esmf_ring
   character(len=ESMF_MAXSTR) :: newalarm_name
   integer :: nnewalarms
   type(ESMF_NewAlarm), allocatable :: newalarm_list(:)
   type(ESMF_Time) :: time
   type(ESMF_NewAlarm) :: esmfnewalarm
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
   esmfnewalarm = ESMF_NewAlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name='newalarm_900',rc=status)
   call verify_(status)
   ! works if s=1800
   call ESMF_TimeIntervalSet(tint,s=1800)
   esmfnewalarm = ESMF_NewAlarmCreate(clock=clock,ringInterval=tint,ringTime=start_time,sticky=.false.,name='newalarm_1800',rc=status)
   call verify_(status)


   nstep=12
   call ESMF_ClockGet(clock,currtime=start_time,newalarmCount=nnewalarms,rc=status)
   allocate(newalarm_list(nnewalarms))
   call ESMF_ClockGetNewAlarmList(clock, newalarmListFlag=ESMF_NEWALARMLIST_ALL, newalarmList=newalarm_list)
   
   do i=1,nstep
      call ESMF_ClockGet(clock,currTime=time)
      do j = 1,nnewalarms
         call ESMF_NewAlarmGet(newalarm_list(j),name=newalarm_name)
         esmf_ring = ESMF_NewAlarmIsRinging(newalarm_list(j),rc=status)
         call verify_(status)
         if ( esmf_ring) then
            write(*,*)trim(newalarm_name)//' is ringing'
            call ESMF_TimePrint(time,options='string')
         end if
      end do
      call ESMF_ClockAdvance(clock)
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_REVERSE, rc=status)
   call verify_(status)
   write(*,*)"*************** start rewind *********************"
   do
      call ESMF_ClockAdvance(clock,rc=status)
      call verify_(status)
      call ESMF_ClockGet(clock,currTime=time)
      do j=1,nnewalarms
         call ESMF_NewAlarmGet(newalarm_list(j),name=newalarm_name)
         if (ESMF_NewAlarmIsRinging(newalarm_list(j))) then
            write(*,*)trim(newalarm_name)//' is ringing'
            call ESMF_TimePrint(time,options='string')
         end if
      end do
      
      if (time == start_time) exit
   enddo
   call ESMF_ClockSet(clock, direction=ESMF_DIRECTION_FORWARD, rc=status)
   call verify_(status)
   write(*,*)"*************** end rewind *********************"
   do i=1,nstep*2
      call ESMF_ClockGet(clock,currTime=time)
      do j = 1,nnewalarms
         call ESMF_NewAlarmGet(newalarm_list(j),name=newalarm_name)
         esmf_ring = ESMF_NewAlarmIsRinging(newalarm_list(j),rc=status)
         call verify_(status)
         if ( esmf_ring) then
            write(*,*)trim(newalarm_name)//' is ringing'
            call ESMF_TimePrint(time,options='string')
         end if
      end do
      call ESMF_ClockAdvance(clock)
   enddo

  testPass = .false.
     
end subroutine Test_NewAlarmAdvRewind

subroutine Test_RevNewAlarmSticky(dt, testPass, rc)
#define CONTEXT line=__LINE__,file=__FILE__
#define CHECKRC if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,CONTEXT,rcToReturn=rc))then;write(0,*)'app abort: ',__FILE__,__LINE__;return;endif
  logical, intent(out) :: testPass
  integer, parameter :: r8 = SELECTED_REAL_KIND(12)   ! real r8
  real(kind=r8), intent(in) :: dt
  integer :: rc
  
  type(ESMF_Clock)         :: clock
  type(ESMF_NewAlarm)         :: newalarm
  type(ESMF_TimeInterval)  :: esmf_ival
  type(ESMF_Time)          :: time, initial, finish, ring_time
  real(kind=r8)            :: secs
  logical                  :: reverse_clock, sticky_newalarm

  rc = ESMF_SUCCESS
  
  call ESMF_Initialize( defaultcalkind=ESMF_CALKIND_GREGORIAN, rc=rc )         ; CHECKRC
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
  sticky_newalarm  = .not.reverse_clock
  
  write(0,'("reverse =",x,l)') reverse_clock
  write(0,'("sticky  =",x,l)') sticky_newalarm
  
  secs = dt*50
  call ESMF_TimeIntervalSet(esmf_ival,s_r8=secs,rc=rc)  ; CHECKRC
  ring_time = initial + esmf_ival

#if 1
  newalarm = ESMF_NewAlarmCreate(clock, ringTime=ring_time, sticky=sticky_newalarm,  &
                           ringTimeStepCount=1, rc=rc) ; CHECKRC
#else
  newalarm = ESMF_NewAlarmCreate(clock, ringTime=ring_time, sticky=.false.,  &
                           ringTimeStepCount=1, rc=rc) ; CHECKRC
#endif
  
  call ESMF_newalarmPrint(newalarm,options='sticky')

  testPass = .false. ! Because the C++ runtime failure cannot be caught reliably, set this to false.

#undef CONTEXT
#undef CHECKRC
end subroutine Test_RevNewAlarmSticky


      end program ESMF_NewAlarmTest
