! $Id: ESMF_TimeUTest.F90,v 1.7 2004/08/06 22:34:10 eschwab Exp $
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
      '$Id: ESMF_TimeUTest.F90,v 1.7 2004/08/06 22:34:10 eschwab Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, M, S, MM, DD, YY, D
      logical :: bool

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! to retrieve time in string format
      character(ESMF_MAXSTR) :: timeString

      ! instantiate timestep, start and stop times
      type(ESMF_Time) :: startTime, stopTime, startTime2
      type(ESMF_Calendar) :: gregorianCalendar, julianDayCalendar, &
                             noLeapCalendar, day360Calendar

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

      call ESMF_TimeGet(startTime, yy=YY, mm=MM, dd=DD, h=H, m=M, s=S, &
                        timeString=timeString, rc=rc)
      call ESMF_Test((YY==2004 .and. MM==1 .and. DD==29 .and. &
                      H==12 .and. M==17 .and. S==58 .and. &
                      timeString=="2004-01-29T12:17:58" .and. &
                      rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, "startTime = ", timeString


#ifdef ESMF_EXHAUSTIVE
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set End Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Time have the same calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      bool =  ESMF_TimeIsSameCalendar(stopTime, stopTime, rc=rc)
      write(name, *) "Time Is Same CalendarTest"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Start Time 2
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(startTime2, yy=2004, mm=1, dd=29, h=12, m=17, s=59, &
                                   calendar=gregorianCalendar, rc=rc)
      write(name, *) "Set Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the ==  operator 
      ! resultTime = ESMF_Time(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the ==  operator 
      ! resultTime = ESMF_TimeOperator(==)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time == operator Test"
      bool = startTime == startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /=  operator 
      ! resultTime = ESMF_TimeOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the /=  operator 
      ! resultTime = ESMF_TimeIntervalOperator(/=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time /= operator Test"
      bool = startTime /= startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime > startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >  operator 
      ! resultTime = ESMF_TimeOperator(>)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 > startTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime <= startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <=  operator 
      ! resultTime = ESMF_TimeOperator(<=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time <= operator Test"
      bool = startTime2 <= startTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime > stopTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time < operator Test"
      bool = startTime < startTime2    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the <  operator 
      ! resultTime = ESMF_TimeOperator(<)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time > operator Test"
      bool = startTime2 < startTime    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= stopTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime >= startTime2    
      call ESMF_Test((.not.bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Testing the >=  operator 
      ! resultTime = ESMF_TimeOperator(>=)(time, time)
      write(failMsg, *) "The result is not correct."
      write(name, *) "Time >= operator Test"
      bool = startTime2 >= startTime    
      call ESMF_Test((bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting Stop Time 
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=2004, mm=1, dd=29, h=12, m=17, s=58, &
                                   calendar=julianDayCalendar, rc=rc)
      write(name, *) "Set End Time Initialization Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
  ! ----------------------------------------------------------------------------
      
      !EX_UTest
      ! Test Time have the same calendar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      bool =  ESMF_TimeIsSameCalendar(stopTime, stopTime, rc=rc)
      write(name, *) "Time Is Same CalendarTest"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bool), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Gregorian date to Julian Day
      !   from http://www.friesian.com/numbers.htm
      write(failMsg, *) " Did not return D = 2450713 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1997, mm=9, dd=21, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=D, calendar=julianDayCalendar, rc=rc)
      write(name, *) "Convert Gregorian to Julian Day Test 1"
      call ESMF_Test((D.eq.2450713).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Converting Gregorian date to Julian Day
      !   from Henry F. Fliegel and Thomas C. Van Flandern, in
      !   Communications of the ACM, CACM, volume 11, number 10,
      !   October 1968, p. 657.
      write(failMsg, *) " Did not return D = 2440588 and ESMF_SUCCESS"
      call ESMF_TimeSet(stopTime, yy=1970, mm=1, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(stopTime, d=D, rc=rc)
      write(name, *) "Convert Gregorian to Julian Day Test 2"
      call ESMF_Test((D.eq.2440588).and.(rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

#endif
  
      ! finalize ESMF framework
      call ESMF_Finalize(rc)

      end program ESMF_TimeUTest
