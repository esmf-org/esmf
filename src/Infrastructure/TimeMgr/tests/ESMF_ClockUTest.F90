! $Id: ESMF_ClockUTest.F90,v 1.53 2003/10/07 20:14:20 svasquez Exp $
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
      program ESMF_ClockTest

!------------------------------------------------------------------------------
!

#include <ESMF_Macros.inc>
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_ClockTest - Test Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! The code in this file drives F90 Clock unit tests.
! The companion file ESMF\_Clock.F90 contains the definitions for the
! Clock methods.
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
      '$Id: ESMF_ClockUTest.F90,v 1.53 2003/10/07 20:14:20 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YR, days, totalDays, secs, testResults

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1, clock_gregorian, clock_julian, clock_no_leap, clock_360day

      ! Random number
      real :: ranNum
      integer :: seed(32)
      integer :: timevals(8)

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianCalendar, no_leapCalendar, esmf_360dayCalendar
      type(ESMF_CalendarType) :: cal_type

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2
      type(ESMF_Time) :: startTime, stopTime, startTime2, stopTime2
      type(ESMF_Time) :: currentTime, previousTime, syncTime, stopTime3 
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      integer(ESMF_KIND_I8) :: advanceCounts, year, day2, month, minute, second
      integer(ESMF_KIND_I4) :: day, hour


      ! initialize ESMF framework
      call ESMF_FrameworkInitialize(rc)

      ! initialize one calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize secand calendar to be Julian type
      call ESMF_CalendarSet(julianCalendar, ESMF_CAL_JULIANDAY, rc)


      ! initialize third calendar to be No Leap type
      call ESMF_CalendarSet(no_leapCalendar, ESMF_CAL_NOLEAP, rc)


      ! initialize third calendar to be 360 day type
      call ESMF_CalendarSet(esmf_360dayCalendar, ESMF_CAL_360DAY, rc)

!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------



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
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yr=2003, mm=3, dd=13, &
                             	   h=18, m=45, s=27, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the year is set correctly
      write(name, *) "Get Start Time Year Test"
      call ESMF_TimeGet(startTime, yr=YR, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StartTime Month Test"
      call ESMF_TimeGet(startTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the day is set correctly
      write(name, *) "Get StartTime Day Test"
      call ESMF_TimeGet(startTime, dd=DD, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.13), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the hour is set correctly
      write(name, *) "Get StartTime Hours Test"
      call ESMF_TimeGet(startTime, h=H, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Hour not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(H.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the minutes is set correctly
      write(name, *) "Get StartTime Minutes Test"
      call ESMF_TimeGet(startTime, m=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or minutes not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.1125), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = 18 X 60 + 45 = 1125
      print *, "minutes = ", MM

      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! Verify the seconds are set correctly
      write(name, *) "Get StartTime seconds Test"
      call ESMF_TimeGet(startTime, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.67527), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! seconds = 1125 X 60 + 27 = 67527

      print *, "seconds = ", secs
      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! Verify the hours, minutes and seconds  are set correctly
      write(name, *) "Get StartTime in hours, minutes & seconds Test"
      call ESMF_TimeGet(startTime, h=H, m=MM, s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.27).and.(H.eq.18).and.(MM.eq.45), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! Verify the hours and seconds are set correctly
      write(name, *) "Get StartTime in hours, & seconds Test"
      call ESMF_TimeGet(startTime, h=H,  s=secs, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.2727).and.(H.eq.18), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Seconds = 60 X 45 + 27 = 2727

      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! Verify the minutes and seconds are set correctly
      write(name, *) "Get StartTime in minutes, & seconds Test"
      call ESMF_TimeGet(startTime, s=secs,  m=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or seconds not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(secs.eq.27).and.(MM.eq.1125), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! Minutes = 60 X 18 + 45 = 1125

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time
      day = 25
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yr=yr, mm=11, d=day, &
                             	   h=11, m=45, s=18, &
                                   calendar=julianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the day is set correctly
      write(name, *) "Get StartTime Day Test"
      call ESMF_TimeGet(startTime, d=day, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(day.eq.25), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time
      year = 30067
      day2 = 25
      hour = 7
      minute = 33
      second = 51
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yr_i8=year, mm=6, dd=25, &
                                   h=hour, m=56, s_i8=second, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the year is set correctly
      !write(name, *) "Get Start Time Year Test"
      !call ESMF_TimeGet(startTime, yr_i8=YR, rc=rc)
      !write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.30067), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StartTime Month Test"
      call ESMF_TimeGet(startTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.6), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the day is set correctly
      !write(name, *) "Get StartTime Day Test"
      !call ESMF_TimeGet(startTime, d_i8=DD, rc=rc)
      !write(failMsg, *) " Returned ESMF_FAILURE and/or Day not correct value"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(DD.eq.31), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! Verify the calendar is set correctly
      !write(name, *) "Get Calendar Type Test"
      !call ESMF_CalendarGet(gregorianCalendar, type=cal_type, rc=rc)
      !write(failMsg, *) " Did not return ESMF_SUCCESS and/or calendar not correct value"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(cal_type%calendarType.eq.ESMF_CAL_GREGORIAN), &
      !               name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE
      ! This code crashes, bug 79753 has been opened.
      ! Attempt to get un-initialized year from stop time
      ! write(name, *) "Get Uninitialized StopTime Year Test"
      ! call ESMF_TimeGet(stopTime, yr=YR, rc=rc)
      ! write(failMsg, *) " Returned ESMF_SUCCESS"
      ! call ESMF_Test((rc.eq.ESMF_FAILURE), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (0) Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=2003, mm=0, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite month
      write(name, *) "Stop Time Initialization to illegite month (13) Test"
      write(failMsg, *) " Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=2003, mm=13, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time to illegite day
      write(name, *) "Stop Time Initialization to  Feb. 31st. Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yr=2003, mm=2, dd=31, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Set time to lower bound of Fliegel algoritm
      write(name, *) "Test lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=-4800, mm=3, dd=1, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Set time beyond lower bound of Fliegel algoritm
      write(name, *) "Test beyond lower bound of Fliegel algorithm Test"
      write(failMsg, *) " Should return ESMF_FAILURE."
      call ESMF_TimeSet(stopTime, yr=-4800, mm=2, dd=28, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Stop Time Initialization Test"
      call ESMF_TimeSet(stopTime, yr=2003, mm=3, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the year is set correctly
      write(name, *) "Get StopTime Year Test"
      call ESMF_TimeGet(stopTime, yr=YR, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Year not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Verify the month is set correctly
      write(name, *) "Get StopTime Month Test"
      call ESMF_TimeGet(stopTime, mm=MM, rc=rc)
      write(failMsg, *) " Returned ESMF_FAILURE and/or Month not correct value"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(MM.eq.3), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Initialize clock with uninitialized Start Time.
      !EX_UTest
       write(name, *) "Clock Initialization Test with uninitialized startTime"
       write(failMsg, *) " Returned ESMF_SUCCESS"
       call ESMF_ClockSetup(clock, timeStep, startTime2, stopTime, rc=rc)
       call ESMF_Test((rc.eq.ESMF_FAILURE), &
                       name, failMsg, result, ESMF_SRCLINE)
#endif

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Set Start Time Initialization Test"
      call ESMF_TimeSet(startTime, yr=2003, mm=3, dd=13, &
                             	   h=18, m=45, s=27, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      ! print out initialized variables
      ! Test that print subroutine returns ESMF_SUCESS
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Calendar Print Test"
      call ESMF_CalendarPrint(gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Time Interval Print Test"
      call ESMF_TimeIntervalPrint(timeStep, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Print Test"
      call ESMF_ClockPrint(clock, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS and/or bool not False"
      write(name, *) "ClockIsStopTime Test"
      bool = ESMF_ClockIsStopTime(clock, rc)
      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and.(.not.bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool


      ! ----------------------------------------------------------------------------
 
      !NEX_UTest
      write(failMsg, *) " Returned ESMF_FAILURE"
      write(name, *) "Clock Initialization Test"
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	!NEX_UTest
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock, rc))
        	call ESMF_ClockAdvance(clock, rc=rc)
        	call ESMF_ClockPrint(clock, rc=rc)
      	end do

      	bool = ESMF_ClockIsStopTime(clock, rc)
      	print *, "bool = ", bool
      	write(failMsg, *) " Did not return ESMF_SUCCESS and/or bool not True"
      	write(name, *) "Clock Advance Test"
      	call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      	! print out ending clock state
      	call ESMF_ClockPrint(clock, rc=rc)
       end if

      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      call ESMF_ClockAdvance(clock, rc=rc)
      print *, "Print Clock after advancing 4 times"
      write(failMsg, *) " Returned ESMF_FAILURE and/or bool not True"
      write(name, *) "Clock Advanced beyond stopTime Test"
      call ESMF_ClockPrint(clock, rc=rc)
      bool = ESMF_ClockIsStopTime(clock, rc)
      call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(bool)), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "bool = ", bool

      ! ----------------------------------------------------------------------------

       !EX_UTest
       write(name, *) "Get previous time and verify Test"
       call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
       call ESMF_ClockAdvance(clock, rc=rc)
       write(failMsg, *) " Returned ESMF_FAILURE and/or currTime != prevTime"
       call ESMF_ClockGet(clock, prevTime=previousTime, rc=rc)
       call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(currentTime.eq.previousTime)), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
 

       !EX_UTest
       write(name, *) "Get previous SimTime and verify Test"
       call ESMF_ClockGet(clock, currSimTime=currentSimTime, rc=rc)
       call ESMF_ClockAdvance(clock, rc=rc)
       write(failMsg, *) " Returned ESMF_FAILURE and/or currSimTime != prevTime"
       call ESMF_ClockGet(clock, prevSimTime=previousSimTime, rc=rc)
       call ESMF_Test(((rc.eq.ESMF_SUCCESS).and.(currentSimTime.eq.previousSimTime)), &
                       name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "Clock Initialization with stop time set before start time Test"
      call ESMF_TimeSet(stopTime, yr=2002, mm=3, dd=14, &
                                  calendar=gregorianCalendar, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS because timestep is positive."
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(name, *) "Clock Initialization with stop time & start time with different calendars Test" 
      call ESMF_TimeSet(startTime, yr=2000, mm=3, dd=13, &
                                   calendar=julianCalendar, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_ClockSetup(clock, timeStep, startTime, stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with Years = -1000 Test" 
      call ESMF_TimeSet(startTime, yr=-1000, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with hour = zero Test" 
      call ESMF_TimeSet(startTime, yr=-1000, h=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_FAILURE."
      write(name, *) "Time Initialization with hour = negative 3 Test" 
      call ESMF_TimeSet(startTime, h=-3, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with hour = 3 Test" 
      call ESMF_TimeSet(startTime, h=3, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with hour = 25 Test" 
      call ESMF_TimeSet(startTime, h=25, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = zero Test" 
      call ESMF_TimeSet(startTime, m=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = negative 5  Test" 
      call ESMF_TimeSet(startTime, m=-5, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = 5  Test" 
      call ESMF_TimeSet(startTime, m=5, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with minutes = 65 Test" 
      call ESMF_TimeSet(startTime, m=65, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds = 0 Test" 
      call ESMF_TimeSet(startTime, s=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds =  negative 8 Test" 
      call ESMF_TimeSet(startTime, s=-8,calendar=gregorianCalendar,  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds =  8 with no calendar type specified Test" 
      call ESMF_TimeSet(startTime, s=8, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds =  8 with calendar type specified Test" 
      call ESMF_TimeSet(startTime, s=8, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with seconds = 65 Test" 
      call ESMF_TimeSet(startTime, s=65, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with milli-seconds = 0 Test" 
      call ESMF_TimeSet(startTime, ms=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with milli-seconds = 84 Test" 
      call ESMF_TimeSet(startTime, ms=84, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with milli-seconds = 101 Test" 
      call ESMF_TimeSet(startTime, ms=101, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with micro-seconds = 0 Test" 
      call ESMF_TimeSet(startTime, us=0, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with micro-seconds = 55 Test" 
      call ESMF_TimeSet(startTime, us=55, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      write(name, *) "Time Initialization with micro-seconds = 101 Test" 
      call ESMF_TimeSet(startTime, us=101, calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=-100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_ClockSetup(clock_gregorian, timeStep, startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	totalDays=0
      	days=-1
      	call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      	call ESMF_TimePrint(startTime, rc=rc)
      	call ESMF_TimePrint(stopTime, rc=rc)
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock_gregorian, rc))
        	call ESMF_ClockAdvance(clock_gregorian, timeStep=timeStep, rc=rc)
        	totalDays=totalDays+days
      	end do

       end if

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days -73049 from +100 to -100 years in Gregorian Cal. Test"
      call ESMF_Test((totalDays.eq.-73049), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Results Total Counts = ", advanceCounts
      write(name, *) "Total Counts 73049 from -100 to +100 years in Gregorian Cal. Test"
      call ESMF_ClockGet(clock_gregorian, advanceCount=advanceCounts, rc=rc)
      call ESMF_Test((advanceCounts.eq.73049), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

     !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, yy=10, rc=rc)
      write(name, *) "Time Step initialization with years = 10 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_ClockSetup(clock_gregorian, timeStep, startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	call date_and_time(values=timevals)
      	seed=timevals(8)
      	call random_seed(put=seed)
      	testResults = 0
      	do while (.not.ESMF_ClockIsStopTime(clock_gregorian, rc))
        	call random_number(ranNum)
        	days=ranNum*25
        	call random_number(ranNum)
        	H=ranNum*100
        	call random_number(ranNum)
        	MM=ranNum*100
        	call random_number(ranNum)
        	secs=ranNum*100
        	call ESMF_TimeIntervalSet(timeStep, d=days, h=H, m=MM, s=secs, rc=rc)
        	call ESMF_ClockAdvance(clock_gregorian, timeStep=timeStep, rc=rc)
        	call ESMF_ClockGet(clock_gregorian, currTime=currentTime, rc=rc)
        	call ESMF_ClockGet(clock_gregorian, prevTime=previousTime, rc=rc)
        	timeDiff =  currentTime - previousTime 
        	if((timeDiff.ne.timeStep).and.(testResults.eq.0)) then	
	     	testResults=1
             	call ESMF_TimeIntervalPrint(timeStep, rc=rc)
             	call ESMF_TimeIntervalPrint(timeDiff, rc=rc)
             	call ESMF_TimePrint(currentTime, rc=rc)
             	call ESMF_TimePrint(previousTime, rc=rc)
        	end if
      	end do

      end if
		
        !EX_UTest
        write(failMsg, *) "Time comparison failed."
        write(name, *) "Current Time minus PreviousTime = timeStep for Gregorian Cal. Test"
        call ESMF_Test((testResults.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=1, &
                                        calendar=no_leapCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test" 
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=100, mm=1, dd=1, &
                                calendar=no_leapCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_ClockSetup(clock_no_leap, timeStep, startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with above settings Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      if (rc.eq.ESMF_SUCCESS) then
      	totalDays=0
      	days=1
      	call ESMF_TimeIntervalSet(timeStep, d=days, rc=rc)
      	call ESMF_TimePrint(startTime, rc=rc)
      	call ESMF_TimePrint(stopTime, rc=rc)
      	! time step from start time to stop time
      	do while (.not.ESMF_ClockIsStopTime(clock_no_leap, rc))
        	call ESMF_ClockAdvance(clock_no_leap, timeStep=timeStep, rc=rc)
        	totalDays=totalDays+days
      	end do

      end if

      ! ----------------------------------------------------------------------------

      !EX_UTest
      print *, " Total days = ", totalDays
      write(failMsg, *) "Results Total days = ", totalDays
      write(name, *) "Total days 73000 from -100 to +100 years in No Leap Cal. Test"
      call ESMF_Test((totalDays.eq.73000), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Results Total Counts = ",  advanceCounts
      write(name, *) "Total Counts 73000 from -100 to +100 years in No Leap Cal. Test"
      call ESMF_ClockGet(clock_no_leap, advanceCount=advanceCounts, rc=rc)
      call ESMF_Test((advanceCounts.eq.73000), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      call ESMF_TimeSet(startTime, yr=100, mm=1, dd=1, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yr=100, mm=1, dd=1, &
                                calendar=esmf_360dayCalendar, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_ClockSetup(clock_360day, timeStep, startTime, stopTime, rc=rc)
      write(name, *) "Clock initialization with Start Time equal to Stop Time Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeIntervalSet(timeStep, rc=rc)
      write(name, *) "Time Step initialization with nothing set Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=31, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      write(name, *) "Start Time set to illegite day in 360 day Calendar Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=380, &
                                        calendar=esmf_360dayCalendar, rc=rc)
      write(name, *) "Start Time set to illegite day in 360 day Calendar Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------


      !EX_UTest
      write(failMsg, *) "Test failed."
      call ESMF_TimeSet(startTime, yr=100, mm=1, dd=1, m=-9, s=-5, &
                                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSet(stopTime, yr=99, mm=12, dd=31, h=23, m=50, s=55, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Time setting equivalency Test"
      call ESMF_ClockSetup(clock_gregorian, timeStep, startTime, stopTime, rc=rc)
      call ESMF_ClockGet(clock_gregorian, startTime=startTime2, rc=rc)
      call ESMF_ClockGet(clock_gregorian, stopTime=stopTime2, rc=rc)
      call ESMF_Test((startTime2.eq.stopTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime2, rc=rc)
      call ESMF_TimePrint(stopTime2, rc=rc)




#endif


     ! ----------------------------------------------------------------------------

      ! This test is commented out because it sometimes
      ! causes Segmentation errors.
      ! write(failMsg, *) "Should return ESMF_SUCCESS."
      ! call ESMF_TimeSyncToRealTime(syncTime, rc)
      ! write(name, *) "Time Sync to Real Time Test"
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      ! name, failMsg, result, ESMF_SRCLINE)


     ! ----------------------------------------------------------------------------

      
      ! write(failMsg, *) " Returned ESMF_FAILURE"
      ! write(name, *) "Sync Time Print Test"
      ! call ESMF_TimePrint(syncTime, rc=rc)
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      ! name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      
      ! Verify the year is set correctly
      ! write(name, *) "Get Sync Time Year Test"
      ! call ESMF_TimeGet(syncTime, yr=YR, rc=rc)
      ! write(failMsg, *) " Did not return ESMF_SUCCESS and/or Year not correct value"
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(YR.eq.2003), &
                      ! name, failMsg, result, ESMF_SRCLINE)
      
      ! print *, " Sync Time year = ", YR
      ! print *, " Get Sync Time rc  = ", rc
      ! ----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime, yr=100, mm=1, dd=1, &
                                calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = 100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(stopTime, rc=rc)

      ! ----------------------------------------------------------------------------


      !EX_UTest
      ! Test Setting Time Interval
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Time Interval to 73049 days Test"
      call ESMF_TimeIntervalSet(timeStep, d=73049, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimeIntervalPrint(timeStep, rc=rc)

      ! ----------------------------------------------------------------------------

      ! Test Incrementing Time by Time Interval
      write(failMsg, *) " startTime not equal to stopTime"
      write(name, *) "Incrementing starttime by 73049 days Test"
      startTime=startTime+timeStep
      call ESMF_Test((startTime.eq.stopTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimePrint(startTime, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_TimePrint(startTime, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Decrementing Time by Time Interval
      !EX_UTest
      write(failMsg, *) " startTime not equal to stopTime"
      write(name, *) "Decrementing stoptime by 73049 days Test"
      stopTime=stopTime-timeStep
      call ESMF_Test((startTime.eq.stopTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TimePrint(stopTime, rc=rc)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime, yr=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yr=-100, mm=1, dd=1, &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be equal to startTime"
      write(name, *) "Verifying the EQ operator Test"
      call ESMF_Test((startTime2.eq.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be equal to startTime"
      write(name, *) "Verifying the NE operator Test"
      call ESMF_Test(.not.(startTime2.ne.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yr=-100, mm=1, dd=1, s=-1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should not be equal to startTime"
      write(name, *) "Verifying the EQ operator Test"
      call ESMF_Test(.not.(startTime2.eq.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "startTime2 should not be equal to startTime"
      write(name, *) "Verifying the NE operator Test"
      call ESMF_Test((startTime2.ne.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be less than startTime"
      write(name, *) "Verifying the LT operator Test"
      call ESMF_Test((startTime2.lt.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should less than startTime"
      write(name, *) "Verifying the LT operator Test"
      call ESMF_Test(.not.(startTime.lt.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should be greater than startTime2"
      write(name, *) "Verifying the GT operator Test"
      call ESMF_Test((startTime.gt.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should less than startTime2"
      write(name, *) "Verifying the GT operator Test"
      call ESMF_Test(.not.(startTime2.gt.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be less than or equal to startTime"
      write(name, *) "Verifying the LE operator Test"
      call ESMF_Test((startTime2.le.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be less than or equal to startTime"
      write(name, *) "Verifying the LE operator Test"
      call ESMF_Test(.not.(startTime.le.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yr=-100, mm=1, dd=1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should equal startTime2"
      write(name, *) "Verifying the LE operator Test"
      call ESMF_Test((startTime2.le.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should be greater than or equal to startTime2"
      write(name, *) "Verifying the GE operator Test"
      call ESMF_Test((startTime.ge.startTime2), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime2 should be greater than or equal to startTime"
      write(name, *) "Verifying the GE operator Test"
      call ESMF_Test((startTime2.ge.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)
      ! ----------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(startTime2, yr=-100, mm=1, dd=1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Start Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "startTime should equal startTime2"
      write(name, *) "Verifying the GE operator Test"
      call ESMF_Test((startTime2.ge.startTime), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate an uninitialized time
      write(failMsg, *) "Should not return ESMF_SUCCESS "
      write(name, *) "Validate an uninitialzed time Test"
      call ESMF_TimeValidate(stopTime3, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime3, yr=-100, mm=1, dd=1,  &
                                        calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = -100 Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate an initialized time
      write(failMsg, *) "Should return ESMF_SUCCESS "
      write(name, *) "Validate an initialzed time Test"
      call ESMF_TimeValidate(stopTime3, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Should not return ESMF_SUCCESS."
      call ESMF_TimeSet(stopTime3, yr=-4900, mm=2, dd=28, &
                                  calendar=gregorianCalendar, rc=rc)
      write(name, *) "Stop Time initialization with year = -4900 Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Validate an initialized time
      write(failMsg, *) "Should return ESMF_SUCCESS "
      write(name, *) "Validate an initialzed time Test"
      call ESMF_TimeValidate(stopTime3, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif

      ! ----------------------------------------------------------------------------
      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_FrameworkFinalize(rc)

      end program ESMF_ClockTest
