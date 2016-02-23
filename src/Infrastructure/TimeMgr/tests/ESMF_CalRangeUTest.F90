! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_CalRangeUTest

!------------------------------------------------------------------------------
!

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_CalRangeUTest - Calendar range Unit tests
!
! !DESCRIPTION:
!
! This program tests calendar ranges
! 
! A good test reference tool is a day & date calculator at
!   http://www.numerical-recipes.com/julian.html
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

      integer, parameter :: CONVERT_TO_TIME = 1, CONVERT_TO_DATE = 2, &
                            CONVERT_TO_BOTH = 3

      ! instantiate calendars
      type(ESMF_Calendar) :: julianDayCalendar


      ! temp variables
      integer :: rc

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0



      ! days per month array, 12 months per year
      integer, dimension(12) :: DaysPerMonth

#ifdef ESMF_TESTEXHAUSTIVE
      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! temp variables
      integer(ESMF_KIND_I8) :: endDl, rDl, Dl, YYl, rYYl
      integer :: DD, rDD, rMM, MM
      logical :: broken 
      integer(ESMF_KIND_I8) :: endYYl

      ! instantiate time instant
      type(ESMF_Time) :: Time

      ! instantiate calendars
      type(ESMF_Calendar) :: julianCalendar
      type(ESMF_Calendar) :: modifiedJulianDayCalendar
      type(ESMF_Calendar) :: gregorianCalendar

#endif

!-------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests
!     are always run.  When the environment variable, EXHAUSTIVE, is set to ON
!     then the EXHAUSTIVE and sanity tests both run.  If the EXHAUSTIVE
!     variable is set to OFF, then only the sanity unit tests run.
!     Special strings (Non-exhaustive and exhaustive) have been added to allow
!     a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! Calendar months table
      DaysPerMonth(1) = 31
      DaysPerMonth(2) = 28
      DaysPerMonth(3) = 31
      DaysPerMonth(4) = 30
      DaysPerMonth(5) = 31 
      DaysPerMonth(6) = 30
      DaysPerMonth(7) = 31
      DaysPerMonth(8) = 31
      DaysPerMonth(9) = 30
      DaysPerMonth(10) = 31
      DaysPerMonth(11) = 30
      DaysPerMonth(12) = 31


#ifdef ESMF_TESTEXHAUSTIVE

      ! Julian Day Calendar

      ! initialize calendar to be Julian Day kind
      julianDayCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIANDAY, &
        name="JulianDay", rc=rc)

      ! Modified Julian Day Calendar

      ! initialize calendar to be Modified Julian Day kind
      modifiedJulianDayCalendar = &
        ESMF_CalendarCreate(ESMF_CALKIND_MODJULIANDAY, &
        name="ModifiedJulianDay", rc=rc)

      ! Gregorian Calendar

      ! initialize calendar to be Gregorian kind
      gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
        name="Gregorian", rc=rc)

      ! Julian Calendar

      ! initialize calendar to be Julian kind
      julianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIAN, &
        name="Julian", rc=rc)


      !-------------------------------------------------------------------------
      ! Test range of Gregorian/Fliegel algorithm for continuity and consistency
      !-------------------------------------------------------------------------

      !-----------------------------------------------------------------!
      !                Gregorian/Fliegel Low Range Test                 !
      ! Start about 500 years above the bottom of the range and move    !
      ! backward one day at a time until it breaks.  This ensures that  !
      ! the leap year rule is tested at all 3 boundaries: every 4, 100, !
      ! and 400 years.  It also tests positive and negative Julian day  !
      ! numbers (Julian day zero is 11/24/-4713 Gregorian).  The lowest !
      ! valid date is 3/1/-4800 Gregorian, which is Julian Day -32044.  !
      !-----------------------------------------------------------------!

      YYl = -4300
      MM = 1
      DD = 1
      Dl = 150518 
      call ESMF_TimeSet(Time, yy_i8=YYl, mm=MM, dd=DD, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(Time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
    
      !print *, "Low range test"
      !print *, "  Start Time Gregorian = ", rYYl, "/", rMM, "/", rDD
      !print *, "  Start Julian Days = ", rDl
      !print *

      call ESMF_RunTestBackwards(CONVERT_TO_TIME, YYl, MM, DD, Dl, &
                                 gregorianCalendar)

      !EX_UTest
      write(failMsg, *) "Low range (convert to time) endpoint not -4800/2/29 or rc=ESMF_SUCCESS"
      write(name, *) "Gregorian/Fliegel Low Range (convert to time) Test"
      call ESMF_Test((YYl.eq.-4800 .and. MM.eq.2 .and. DD.eq.29 &
                      .and. Dl.eq.-32045 .and. rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *

      ! Convert-To-Date limit actually goes back 100 years further to 3/1/-4900

      call ESMF_RunTestBackwards(CONVERT_TO_DATE, YYl, MM, DD, Dl, &
                                 gregorianCalendar)
      !EX_UTest
      write(failMsg, *) "Low range (convert to date) endpoint not -4900/2/28 or rc=ESMF_SUCCESS"
      write(name, *) "Gregorian/Fliegel Low Range (convert to date) Test"
      call ESMF_Test((YYl.eq.-4900 .and. MM.eq.2 .and. DD.eq.28 &
                      .and. Dl.eq.-68570 .and. rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *

      !------------------------------------------------------------------!
      !                  Gregorian/Fliegel High Range Test               !
      ! Test only 200,000 days (~ 500 years) near the high end, since    !
      ! it would take a few decades of real wall clock time to step      !
      ! through the entire range. (TODO: parallelize?)  This ensures     !
      ! that the leap year rule is tested at all 3 boundaries:  every    !
      ! 4, 100, and 400 years.  For a signed 64-bit representation, the  !
      ! highest valid date is 10/29/292,277,019,914 Gregorian.           !
      !------------------------------------------------------------------!

      ! set days to highest value representable with signed 64-bits:
      ! ((2**63)-1)/86400 = 106,751,991,167,300 days
      Dl = 106751991167300_ESMF_KIND_I8

      ! start back 200000 days, then come forward until it breaks
      Dl = Dl - 200000

      ! matching start date is March 30, 292,277,019,367
      YYl = 292277019367_ESMF_KIND_I8
      MM = 3
      DD = 30

      call ESMF_TimeSet(Time, yy_i8=YYl, mm=MM, dd=DD, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(Time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
      !print *, "High range test"
      !print *, "  Start Time Gregorian = ", rYYl, "/", rMM, "/", rDD
      !print *, "  Start Julian Days = ", rDl
      !print *

      call ESMF_RunTestForwards(YYl, MM, DD, Dl, gregorianCalendar)

      ! test section ended, check expected endpoint year and julian day
      endYYl = 292277019914_ESMF_KIND_I8
      endDl  = 106751991167301_ESMF_KIND_I8
          
      !EX_UTest
      write(failMsg, *) &
        "High range not 10/30/292,277,019,914 or rc=ESMF_SUCCESS"
      write(name, *) "Gregorian/Fliegel High Range Test"
      call ESMF_Test((YYl.eq.endYYl .and. MM.eq.10 .and. DD.eq.30 &
                      .and. Dl.eq.endDl .and. rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !----------------------------------------------------------------------
      ! Test range of Julian/Hatcher algorithm for continuity and consistency
      !----------------------------------------------------------------------

      !-----------------------------------------------------------------!
      !                   Julian/Hatcher Low Range Test                 !
      ! Start at Julian day 36585, 3/1/-4612 (100 years above bottom),  !
      ! and move backward one day at a time until it breaks.            !
      ! This tests the leap year rule at several 4-year boundaries and  !
      ! at one 100-year boundary.  The lowest valid Julian date is      !
      ! 3/1/-4712, which is Julian Day 60.                              !
      !-----------------------------------------------------------------!

      YYl = -4612
      MM = 3
      DD = 1
      Dl = 36585
      call ESMF_TimeSet(Time, yy_i8=YYl, mm=MM, dd=DD, &
                        calendar=julianCalendar, rc=rc)
      call ESMF_TimeGet(Time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
      !print *, "Low range test"
      !print *, "  Start Time Julian = ", rYYl, "/", rMM, "/", rDD
      !print *, "  Start Julian Days = ", rDl
      !print *

      call ESMF_RunTestBackwards(CONVERT_TO_TIME, YYl, MM, DD, Dl, &
                                 julianCalendar)

      !EX_UTest
      write(failMsg, *) "Low range (convert to time) endpoint not -4712/2/29 or rc=ESMF_SUCCESS"
      write(name, *) "Julian/Hatcher Low Range (convert to time) Test"
      call ESMF_Test((YYl.eq.-4712 .and. MM.eq.2 .and. DD.eq.29 &
                      .and. Dl.eq.59 .and. rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *

      ! Convert-To-Date limit actually goes back 1 day further to 2/29/-4712

      call ESMF_RunTestBackwards(CONVERT_TO_DATE, YYl, MM, DD, Dl, &
                                 julianCalendar)

      !EX_UTest
      write(failMsg, *) "Low range (convert to date) endpoint not -4712/2/28 or rc=ESMF_SUCCESS"
      write(name, *) "Julian/Hatcher Low Range (convert to date) Test"
      call ESMF_Test((YYl.eq.-4712 .and. MM.eq.2 .and. DD.eq.28 &
                      .and. Dl.eq.58 .and. rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !print *

      !------------------------------------------------------------------!
      !                     Julian/Hatcher High range test               !
      ! Test only 40,000 days (~ 100 years) near the high end, since it  !
      ! would take a few decades of real wall clock time to step through !
      ! the entire range. (TODO: parallelize?)  This tests the leap year !
      ! rule at several 4-year boundaries and at one 100-year boundary.  !
      ! For a signed 64-bit representation, the highest valid date is    !
      ! 4/24/292,271,018,333 Julian.                                     !
      !------------------------------------------------------------------!

      ! set days to highest value representable with signed 64-bits:
      ! ((2**63)-1)/86400 = 106,751,991,167,300 days
      Dl = 106751991167300_ESMF_KIND_I8

      ! start back 40000 days, then come forward until it breaks
      Dl = Dl - 40000

      ! matching start date is October 19, 292,271,018,223
      YYl = 292271018223_ESMF_KIND_I8
      MM = 10
      DD = 19

      call ESMF_TimeSet(Time, yy_i8=YYl, mm=MM, dd=DD, &
                        calendar=JulianCalendar, rc=rc)
      call ESMF_TimeGet(Time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
      !print *, "High range test"
      !print *, "  Start Time Julian = ", rYYl, "/", rMM, "/", rDD
      !print *, "  Start Julian Days = ", rDl
      !print *

      call ESMF_RunTestForwards(YYl, MM, DD, Dl, julianCalendar)

      ! test section ended, check expected endpoint year and julian day
      endYYl = 292271018333_ESMF_KIND_I8
      endDl  = 106751991167301_ESMF_KIND_I8
          
      !EX_UTest
      write(failMsg, *) &
        "High range not 4/25/292,271,018,333 or rc=ESMF_SUCCESS"
      write(name, *) "Julian/Hatcher High Range Test"
      call ESMF_Test((YYl.eq.endYYl .and. MM.eq.4 .and. DD.eq.25 &
                      .and. Dl.eq.endDl .and. rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-----------------------------------!
      ! Test range of Julian day calendar !
      !-----------------------------------!

      !------------------------------------------------------------------!
      !                   Julian Day Low Range Test                      !
      ! Start 10 days above the bottom and move backward one day at a    !
      ! time until it breaks.  The lowest valid signed 64-bit Julian day !
      ! is -2^63/86400 = -106,751,991,167,300.                           !
      !------------------------------------------------------------------!

      ! set days to lowest value representable with signed 64-bits:
      ! -(2**63)/86400 = -106,751,991,167,300 days
      Dl = -106751991167300_ESMF_KIND_I8
      endDl = Dl - 1

      ! start 10 days above bottom, then come back until it breaks
      Dl = Dl + 10

      call ESMF_TimeSet(Time, d_i8=Dl, calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeGet(Time, d_i8=rDl, rc=rc)
      !print *
      !print *, "Low range test"
      !print *, "  Start Julian Days = ", rDl
      !print *

      broken = .false.
      do while (.not.broken)
        ! calculate what previous Julian Day number should be
        Dl = Dl - 1
        !print *, Dl

        ! set date via ESMF Julian Day calendar
        call ESMF_TimeSet(Time, d_i8=Dl, calendar=julianDayCalendar, rc=rc)

        ! see what we get back
        call ESMF_TimeGet(time, d_i8=rDl, rc=rc)

        if (.not.(rDl.eq.Dl)) then
          broken = .true.
          !print *, "Julian Day Set/Get breaks,"
          !print *, " should be = ", Dl
          !print *, " returned  = ", rDl
          !print *
        end if
      end do
  
      !EX_UTest
      write(failMsg, *) &
        "Low range not -106,751,991,167,301 or rc=ESMF_SUCCESS"
      write(name, *) "Julian Day Low Range Test"
      call ESMF_Test((Dl.eq.endDl .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------!
      !                   Julian Day High Range Test                      !
      ! Start 10 days below the top and move forward one day at a         !
      ! time until it breaks.  The highest valid signed 64-bit Julian day !
      ! is (2^63-1)/86400 = 106,751,991,167,300.                          !
      !-------------------------------------------------------------------!

      ! set days to highest value representable with signed 64-bits:
      ! ((2**63)-1)/86400 = 106,751,991,167,300 days
      Dl = 106751991167300_ESMF_KIND_I8
      endDl = Dl + 1

      ! start 10 days below top, then go forward until it breaks
      Dl = Dl - 10

      call ESMF_TimeSet(Time, d_i8=Dl, calendar=julianDayCalendar, rc=rc)
      call ESMF_TimeGet(Time, d_i8=rDl, rc=rc)
      !print *
      !print *, "High range test"
      !print *, "  Start Julian Days = ", rDl
      !print *

      broken = .false.
      do while (.not.broken)
        ! calculate what next Julian Day number should be
        Dl = Dl + 1
        !print *, Dl

        ! set date via ESMF Julian Day calendar
        call ESMF_TimeSet(Time, d_i8=Dl, calendar=julianDayCalendar, rc=rc)

        ! see what we get back
        call ESMF_TimeGet(time, d_i8=rDl, rc=rc)

        if (.not.(rDl.eq.Dl)) then
          broken = .true.
          !print *, "Julian Day Set/Get breaks,"
          !print *, " should be = ", Dl
          !print *, " returned  = ", rDl
          !print *
        end if
      end do
  
      !EX_UTest
      write(failMsg, *) &
        "High range not 106,751,991,167,301 or rc=ESMF_SUCCESS"
      write(name, *) "Julian Day High Range Test"
      call ESMF_Test((Dl.eq.endDl .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !--------------------------------------------!
      ! Test range of Modified Julian day calendar !
      !--------------------------------------------!

      !------------------------------------------------------------------!
      !               Modified Julian Day Low Range Test                 !
      ! Start 10 days above the bottom and move backward one day at a    !
      ! time until it breaks.  The lowest valid signed 64-bit Modified   !
      ! Julian day is -2^63/86400 - 2,400,001 = -106,751,993,567,301.    !
      !------------------------------------------------------------------!

      ! set days to lowest value representable with signed 64-bits:
      ! -(2**63)/86400 - 2,400,001 = -106,751,993,567,300 days
      Dl = -106751993567301_ESMF_KIND_I8
      endDl = Dl - 1

      ! start 10 days above bottom, then come back until it breaks
      Dl = Dl + 10

      call ESMF_TimeSet(Time, d_i8=Dl, calendar=modifiedJulianDayCalendar, &
                        rc=rc)
      call ESMF_TimeGet(Time, d_i8=rDl, rc=rc)
      !print *
      !print *, "Low range test"
      !print *, "  Start Modified Julian Days = ", rDl
      !print *

      broken = .false.
      do while (.not.broken)
        ! calculate what previous Modified Julian Day number should be
        Dl = Dl - 1
        !print *, Dl

        ! set date via ESMF Modified Julian Day calendar
        call ESMF_TimeSet(Time, d_i8=Dl, calendar=modifiedJulianDayCalendar, &
                          rc=rc)

        ! see what we get back
        call ESMF_TimeGet(time, d_i8=rDl, rc=rc)

        if (.not.(rDl.eq.Dl)) then
          broken = .true.
          !print *, "Modified Julian Day Set/Get breaks,"
          !print *, " should be = ", Dl
          !print *, " returned  = ", rDl
          !print *
        end if
      end do
  
      !EX_UTest
      write(failMsg, *) &
        "Low range not -106,751,993,567,301 or rc=ESMF_SUCCESS"
      write(name, *) "Modified Julian Day Low Range Test"
      call ESMF_Test((Dl.eq.endDl .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------!
      !               Modified Julian Day High Range Test                 !
      ! Start 10 days below the top and move forward one day at a         !
      ! time until it breaks.  The highest valid signed 64-bit Modified   !
      ! Julian day is (2^63-1)/86400 - 2,400,001 = 106,751,988,767,299.   !
      !-------------------------------------------------------------------!

      ! set days to highest value representable with signed 64-bits:
      ! ((2**63)-1)/86400 - 2,400,001 = 106,751,988,767,299 days
      Dl = 106751988767299_ESMF_KIND_I8
      endDl = Dl + 1

      ! start 10 days below top, then go forward until it breaks
      Dl = Dl - 10

      call ESMF_TimeSet(Time, d_i8=Dl, calendar=modifiedJulianDayCalendar, &
                        rc=rc)
      call ESMF_TimeGet(Time, d_i8=rDl, rc=rc)
      !print *
      !print *, "High range test"
      !print *, "  Start Modified Julian Days = ", rDl
      !print *

      broken = .false.
      do while (.not.broken)
        ! calculate what next Modified Julian Day number should be
        Dl = Dl + 1
        !print *, Dl

        ! set date via ESMF Modified Julian Day calendar
        call ESMF_TimeSet(Time, d_i8=Dl, calendar=modifiedJulianDayCalendar, &
                          rc=rc)

        ! see what we get back
        call ESMF_TimeGet(time, d_i8=rDl, rc=rc)

        if (.not.(rDl.eq.Dl)) then
          broken = .true.
          !print *, "Modified Julian Day Set/Get breaks,"
          !print *, " should be = ", Dl
          !print *, " returned  = ", rDl
          !print *
        end if
      end do
  
      !EX_UTest
      write(failMsg, *) &
        "High range not 106,751,988,767,300 or rc=ESMF_SUCCESS"
      write(name, *) "Modified Julian Day High Range Test"
      call ESMF_Test((Dl.eq.endDl .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-------------------!
      ! destroy calendars !
      !-------------------!
      call ESMF_CalendarDestroy(julianCalendar, rc=rc)
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)
      call ESMF_CalendarDestroy(julianDayCalendar, rc=rc)
      call ESMF_CalendarDestroy(modifiedJulianDayCalendar, rc=rc)
#endif

      ! finalize ESMF framework
      call ESMF_TestEnd(ESMF_SRCLINE)

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RunTestBackwards - Run backwards one day at a time until it breaks

! !INTERFACE:
      subroutine ESMF_RunTestBackwards(testType, YYl, MM, DD, Dl, cal)

! !ARGUMENTS:
      integer, intent(in) :: testType
      integer(ESMF_KIND_I8), intent(inout) :: YYl
      integer, intent(inout) :: MM
      integer, intent(inout) :: DD
      integer(ESMF_KIND_I8), intent(inout) :: Dl
      type(ESMF_Calendar), intent(inout) :: cal
        
! !DESCRIPTION:
!     Goes backwards one day at a time until the calendar conversion 
!     algorithm breaks.
!
!EOPI
      logical :: broken 

      broken = .false.
      do while (.not.broken)
    
        ! calculate what previous Julian Day number should be
        Dl = Dl - 1

        ! calculate what the previous date in given calendar should be
        DD = DD - 1
        if (DD.eq.0) then
          MM = MM - 1
          if (MM.eq.0) then
            MM = 12
            YYl = YYl - 1
          end if
          DD = DaysPerMonth(MM)
          ! check if leap year
          if (MM.eq.2) then
            if (ESMF_IsLeapYear(YYl, cal)) then
              DD = 29
            end if
          end if
        end if

        ! check calculated dates against ESMF dates
        broken = ESMF_CheckTime(testType, YYl, MM, DD, Dl, cal, rc)
      end do

      end subroutine ESMF_RunTestBackwards

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RunTestForwards - Run forwards one day at a time until it breaks

! !INTERFACE:
      subroutine ESMF_RunTestForwards(YYl, MM, DD, Dl, cal)

! !ARGUMENTS:
      integer(ESMF_KIND_I8), intent(inout) :: YYl
      integer, intent(inout) :: MM
      integer, intent(inout) :: DD
      integer(ESMF_KIND_I8), intent(inout) :: Dl
      type(ESMF_Calendar), intent(inout) :: cal
        
! !DESCRIPTION:
!     Goes forwards one day at a time until the calendar conversion 
!     algorithm breaks.
!
!EOPI
      integer :: daysInThisMonth
      logical :: broken 

      broken = .false.
      do while (.not.broken)
    
        ! calculate what next Julian Day number should be
        Dl = Dl + 1

        ! calculate what the next date in given calendar should be
        DD = DD + 1
        daysInThisMonth = DaysPerMonth(MM)
        ! check if leap year
        if (MM.eq.2 .and. DD.eq.29) then
          if (ESMF_IsLeapYear(YYl, cal)) then
            daysInThisMonth = 29
          end if
        end if
        if (DD.gt.daysInThisMonth) then
          DD = 1
          MM = MM + 1
          if (MM.eq.13) then
            MM = 1
            YYl = YYl + 1
          end if
        end if

        ! check calculated dates against ESMF dates
        broken = ESMF_CheckTime(CONVERT_TO_BOTH, YYl, MM, DD, Dl, cal, rc)
      end do

      end subroutine ESMF_RunTestForwards

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CheckTime - Check ESMF Time values against given values

! !INTERFACE:
      function ESMF_CheckTime(testType, YYl, MM, DD, Dl, cal, rc)

! !RETURN VALUE:      
      logical :: ESMF_CheckTime

! !ARGUMENTS:
      integer, intent(in) :: testType
      integer(ESMF_KIND_I8), intent(in) :: YYl
      integer, intent(in) :: MM
      integer, intent(in) :: DD
      integer(ESMF_KIND_I8), intent(in) :: Dl
      type(ESMF_Calendar), intent(inout) :: cal
      integer, intent(out), optional :: rc

      !character(ESMF_MAXSTR) :: calName
        
! !DESCRIPTION:
!     Checks given values against those set/get with ESMF
!
!EOPI

      ! ESMF returned variables
      integer(ESMF_KIND_I8) :: rYYl, rDl
      integer :: rMM, rDD
      logical :: broken 
      type(ESMF_Time) :: time

      broken = .false.

      if(testType.eq.CONVERT_TO_TIME .or. testType.eq.CONVERT_TO_BOTH) then

        !
        ! Input calendar conversion test
        !

        ! set date via given ESMF calendar
        call ESMF_TimeSet(time, yy_i8=YYl, mm=MM, dd=DD, calendar=cal, rc=rc)
        if (rc .ne. ESMF_SUCCESS) then
           ESMF_CheckTime = .true.
           return
        endif

        ! see what we get back
        call ESMF_TimeGet(time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl)

        ! must match
        if (.not.(rYYl.eq.YYl .and. rMM.eq.MM .and. rDD.eq.DD .and. &
                  rDl.eq.Dl)) then
          broken = .true.
          !call ESMF_CalendarGet(cal, name=calName)
          !print *, trim(calName), " Set/Get breaks,"
          !print *, " should be = ", YYl, "/", MM, "/", DD, " ", Dl
          !print *, " returned  = ", rYYl, "/", rMM, "/", rDD, " ", rDl
          !print *
        end if

        !
        ! Input calendar Set/Julian Day Get test
        !

        ! see what we get back via Julian Day calendar
        call ESMF_TimeSet(time, calendar=julianDayCalendar)
        call ESMF_TimeGet(time, d_i8=rDl)

        if (.not.(rDl.eq.Dl)) then
          broken = .true.
          !call ESMF_CalendarGet(cal, name=calName)
          !print *, trim(calName), " Set/Julian Day Get breaks,"
          !print *,                " should be = ", Dl
          !print *,                " returned  = ", rDl
          !print *
        end if

      end if

      if(testType.eq.CONVERT_TO_DATE .or. testType.eq.CONVERT_TO_BOTH) then

        !
        ! Julian Day test
        !

        ! set date via ESMF Julian Day calendar
        call ESMF_TimeSet(time, d_i8=Dl, calendar=julianDayCalendar)

        ! see what we get back
        call ESMF_TimeGet(time, d_i8=rDl)

        if (.not.(rDl.eq.Dl)) then
          broken = .true.
          !print *, "Julian Day Set/Get breaks,"
          !print *, " should be = ", Dl
          !print *, " returned  = ", rDl
          !print *
        end if
  
        !
        ! Julian Day Set/Input calendar Get test
        !

        ! see what we get back via input calendar
        call ESMF_TimeSet(time, calendar=cal)
        call ESMF_TimeGet(time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
        if (rc /= ESMF_SUCCESS) then
          broken = .true.
        else
          if (.not.(rYYl.eq.YYl .and. rMM.eq.MM .and. rDD.eq.DD .and. &
                  rDl.eq.Dl)) then
            broken = .true.
          !print *, "Julian Day Set/"
          !call ESMF_CalendarGet(cal, name=calName)
          !print *, trim(calName), " Get breaks,"
          !print *, " should be = ", YYl, "/", MM, "/", DD, " ", Dl
          !print *, " returned  = ", rYYl, "/", rMM, "/", rDD, " ", rDl
          !print *
          end if
        end if

      end if

      ! function return value
      ESMF_CheckTime = broken

      end function ESMF_CheckTime

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IsLeapYear - Check if given year is a leap year in the given calendar

! !INTERFACE:
      function ESMF_IsLeapYear(YYl, cal)

! !RETURN VALUE:      
      logical :: ESMF_IsLeapYear

! !ARGUMENTS:
      integer(ESMF_KIND_I8), intent(in) :: YYl
      type(ESMF_Calendar), intent(inout) :: cal
        
! !DESCRIPTION:
!     Checks given year to see if it is a leap year in the given calendar
!
!EOPI
      integer(ESMF_KIND_I8), parameter :: ly1 = 400, ly2 = 4, ly3 = 100
      logical :: leapYear 

      if (cal .eq. ESMF_CALKIND_GREGORIAN) then
! Note: At least one Absoft compiler (in particular, version 9a SP1 on x86_64 linux)
! may miscompile the following statement - which causes a floating point error at
! runtime.  See ticket #2800140.
        leapYear = (mod(YYl,ly1).eq.0) .or. &
                     (mod(YYl,ly2).eq.0 .and. mod(YYl,ly3).ne.0)
      else if (cal .eq. ESMF_CALKIND_JULIAN) then
        leapYear = mod(YYl,ly2).eq.0
      else
        !print *, "Error:  Leap year rule unknown for given calendar."
      end if

      ! function return value
      ESMF_IsLeapYear = leapYear

      end function ESMF_IsLeapYear

!------------------------------------------------------------------------------

      end program ESMF_CalRangeUTest
