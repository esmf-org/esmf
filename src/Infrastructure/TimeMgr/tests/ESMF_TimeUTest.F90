! $Id: ESMF_TimeUTest.F90,v 1.1 2004/04/21 19:55:06 svasquez Exp $
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
      program ESMF_TimeUTest

!------------------------------------------------------------------------------
!

#include <ESMF.h>
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_TimeTest - Test Time initialization and manipulation.
!
! !DESCRIPTION:
!
! The code in this file drives F90 Time unit tests.
! The companion file ESMF\_Time.F90 contains the definitions for the
! Time methods.
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
      '$Id: ESMF_TimeUTest.F90,v 1.1 2004/04/21 19:55:06 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, M, S, MM, DD, D, YY, days, months, years, totalDays, &
                 hours, minutes, secs, testResults, ans
      logical :: bool

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep, timeStep2, timeStep3
      type(ESMF_Time) :: startTime, stopTime, startTime2, stopTime2
      type(ESMF_Time) :: currentTime, previousTime, syncTime, stopTime3 
      type(ESMF_Calendar) :: gregorianCalendar, julianDayCalendar, &
                             noLeapCalendar, day360Calendar
      type(ESMF_TimeInterval) :: currentSimTime, previousSimTime, timeDiff
      type(ESMF_TimeInterval) :: absoluteTime
      integer(ESMF_KIND_I8) :: advanceCounts, year, days2, month, minute, second
      integer(ESMF_KIND_I4) :: day, hour


!-------------------------------------------------------------------------------
!    The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!    always run. When the environment variable, EXHAUSTIVE, is set to ON then
!    the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!    to OFF, then only the sanity unit tests.
!    Special strings (Non-exhaustive and exhaustive) have been
!    added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

     ! initialize ESMF framework
      call ESMF_Initialize(rc=rc)

      ! ----------------------------------------------------------------------------
      ! Calendar Interval tests
      ! ----------------------------------------------------------------------------
      ! initialize calendars
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      noLeapCalendar = ESMF_CalendarCreate("No Leap", &
                                              ESMF_CAL_NOLEAP, rc)
      day360Calendar = ESMF_CalendarCreate("360 Day", &
                                              ESMF_CAL_360DAY, rc)
      julianDayCalendar = ESMF_CalendarCreate("Julian Day", &
                                              ESMF_CAL_JULIANDAY, rc)


      
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting Start Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(startTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting Stop Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set End Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting Start Time 2
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(startTime2, yy=2004, mm=1, dd=29, h=12, m=17, s=59, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the ==  operator 
      ! resultTime = ESMF_Time(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the ==  operator 
      ! resultTime = ESMF_TimeOperator(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the /=  operator 
      ! resultTime = ESMF_TimeOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the /=  operator 
      ! resultTime = ESMF_TimeIntervalOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 > startTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime2 <= startTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime > stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime < startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 < startTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime2 >= startTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------


      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_Finalize(rc)

      end program ESMF_TimeUTest
