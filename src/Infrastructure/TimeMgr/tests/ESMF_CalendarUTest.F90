! $Id: ESMF_CalendarUTest.F90,v 1.3 2003/09/23 23:05:17 svasquez Exp $
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
      program ESMF_CalendarUTest

!------------------------------------------------------------------------------
!

#include <ESMF_Macros.inc>
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_CalendarUTest - Test Calendar.
!
! !DESCRIPTION:
!
! The code in this file drives F90 Calendar unit tests.
! The companion file ESMF\_Calendar.F90 contains the definitions for the
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
      '$Id: ESMF_CalendarUTest.F90,v 1.3 2003/09/23 23:05:17 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, H, MM, DD, YR, days, totalDays, secs, testResults
      integer(ESMF_KIND_I8) :: year
      integer(ESMF_KIND_I4) :: julianDay

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      logical :: bool
      ! instantiate a clock 
      type(ESMF_Clock) :: clock, clock1, clock_gregorian, clock_julian, clock_no_leap, clock_360day
      type(ESMF_Time) :: startTime, stopTime

      ! Random number
      real :: ranNum
      integer :: seed(32)
      integer :: timevals(8)

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar, julianDayCalendar, no_leapCalendar, esmf_360dayCalendar
      type(ESMF_CalendarType) :: cal_type


!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------



      ! initialize ESMF framework
      call ESMF_FrameworkInitialize(rc)

      !NEX_UTest
      ! initialize one calendar to be Gregorian type
      write(name, *) "Initialize Gregorian Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! initialize secand calendar to be Julian Day type
      write(name, *) "Initialize Julian Day Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(julianDayCalendar, ESMF_CAL_JULIANDAY, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! initialize third calendar to be No Leap type
      write(name, *) "Initialize No Leap Year Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(no_leapCalendar, ESMF_CAL_NOLEAP, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------


      !NEX_UTest
      ! initialize third calendar to be 360 day type
      write(name, *) "Initialize 360 Day Year Type Calendar Test"
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      call ESMF_CalendarSet(esmf_360dayCalendar, ESMF_CAL_360DAY, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time for the Gregorian Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit of Gregorian Calendar Test"
      call ESMF_TimeSet(startTime, yr=-4800, mm=3, dd=1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Start Time for the Gregorian Calencar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit minus 1 second of Gregorian Calendar Test"
      call ESMF_TimeSet(startTime, yr=-4800, mm=3, dd=1, s=-1, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif 
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Stop Time for the Gregorian Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit of Gregorian Calendar Test"
      year=292277019914_ESMF_KIND_I8
      call ESMF_TimeSet(stopTime, yr_i8=year, mm=10, dd=29, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time for the Gregorian Calencar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit plus 86400 second of Gregorian Calendar Test"
      year=292277019914_ESMF_KIND_I8
      call ESMF_TimeSet(stopTime, yr_i8=year, mm=10, dd=29, s=86400, &
                                   calendar=gregorianCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif 


      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Start Time for the Julian Day Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit of Julian Day Calendar Test"
      call ESMF_TimeSet(startTime, d=-32045, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      ! ----------------------------------------------------------------------------
      !EX_UTest
      ! Test Setting the Start Time for the Julian Day Calencar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Set Start Time at lower limit minus 1 day of Julian Day Calendar Test"
      call ESMF_TimeSet(startTime, yr=-32046, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Start Time Print Test"
      call ESMF_TimePrint(startTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif 
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      ! Test Setting the Stop Time for the Julian Day Calencar
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit of Julian Day Calendar Test"
      call ESMF_TimeSet(stopTime, d=julianDay, &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !NEX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      ! ----------------------------------------------------------------------------

      !EX_UTest
      ! Test Setting the Stop Time for the Julian Day Calencar
      write(failMsg, *) " Should not return ESMF_SUCCESS"
      write(name, *) "Set Stop Time at upper limit plus 1 day of Julian Day Calendar Test"
      julianDay=100000000000001_ESMF_KIND_I4
      call ESMF_TimeSet(stopTime, d=julianDay,  &
                                   calendar=julianDayCalendar, rc=rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), & 
                      name, failMsg, result, ESMF_SRCLINE)
      
      ! ----------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) " Did not return ESMF_SUCCESS"
      write(name, *) "Stop Time Print Test"
      call ESMF_TimePrint(stopTime, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
       



#endif 



      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      ! finalize ESMF framework
      call ESMF_FrameworkFinalize(rc)

      end program ESMF_CalendarUTest
