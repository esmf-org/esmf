! $Id: ESMF_CalRangeUTest.F90,v 1.24 2004/12/10 22:39:53 eschwab Exp $
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
      program ESMF_CalRangeUTest

!------------------------------------------------------------------------------
!

#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_CalRangeUTest - Calendar range Unit tests
!
! !DESCRIPTION:
!
! This program tests calendar ranges
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CalRangeUTest.F90,v 1.24 2004/12/10 22:39:53 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate calendars
      type(ESMF_Calendar) :: gregorianCalendar
      type(ESMF_Calendar) :: julianDayCalendar

      ! instantiate time instant
      type(ESMF_Time) :: Time

      integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth

      ! temp variables
      integer(ESMF_KIND_I8) :: YYl, rYYl, Dl, rDl, endYYl, endDl
      integer :: MM, rMM, DD, rDD
      integer(ESMF_KIND_I8), parameter :: ly1 = 400, ly2 = 4, ly3 = 100
      integer(ESMF_KIND_I8), parameter :: tenthousand = 10000
      integer, parameter :: HIGH_LO = 1, HIGH_HI = 2
      integer :: test, first_test, last_test
      logical :: broken, done
      integer :: rc

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

!------------------------------------------------------------------------------- -
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests ar e
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is se t
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      ! initialize ESMF framework
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

#ifdef ESMF_EXHAUSTIVE

      ! Julian Calendar

      ! initialize calendar to be Julian type
      julianDayCalendar = ESMF_CalendarCreate("JulianDay", &
                                              ESMF_CAL_JULIANDAY, rc)

      ! Gregorian Calendar

      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)

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

      !
      ! test range of Fliegel algorithm for continuity and consistency
      !

      !----------------------------------------------------------------!
      !                          Low range test                        !
      ! start at julian day zero, 11/24/-4713, and move backward one   !
      ! day at a time until it breaks                                  !
      !----------------------------------------------------------------!

      YYl = -4713
      MM = 11
      DD = 24
      Dl = 0 
      call ESMF_TimeSet(Time, yy_i8=YYl, mm=MM, dd=DD, &
                        calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(Time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
      print *, "Low range test"
      print *, "  Start Time Gregorian = ", rYYl, "/", rMM, "/", rDD
      print *, "  Start Julian Days = ", rDl
      print *

      broken = .false.
      do while (.not.broken)
    
        ! calculate what previous Julian date should be
        Dl = Dl - 1

        ! calculate what the previous Gregorian date should be
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
            if ((mod(YYl,ly1).eq.0).or. &
                            (mod(YYl,ly2).eq.0 .and. mod(YYl,ly3).ne.0)) then
              DD = 29
            end if
          end if
        end if

        ! check calculated dates against ESMF dates
        broken = ESMF_CheckTime(Time, YYl=YYl, MM=MM, DD=DD, Dl=Dl, &
                                gregCal=gregorianCalendar, &
                                julCal=julianDayCalendar, rc=rc)
      end do
      !EX_UTest
      write(failMsg, *) "Low range endpoint not -4800/2/29 or rc=ESMF_FAILURE"
      write(name, *) "Gregorian/Fliegel Low Range Test"
      call ESMF_Test((YYl.eq.-4800 .and. MM.eq.2 .and. DD.eq.29 &
                      .and. Dl.eq.-32045 .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *

      !------------------------------------------------------------------!
      !                High range tests (low end & high end)             !   
      ! test only a few 10,000 years on either end, since it would take  !
      ! a few decades of real wall clock time to step through the entire !
      ! range (TODO: parallelize?)                                       !
      !------------------------------------------------------------------!

      ! The following values are used for a long exhaustive test.
      !TODO create a flag to allow testing with these values.
      !first_test = HIGH_LO
      !last_test  = HIGH_HI

      !EX_UTest
      first_test = HIGH_HI
      last_test  = HIGH_HI

      do test = first_test, last_test

        if (test.eq.HIGH_LO) then

          !----------------------------------------------------------------!
          !                      High range test (low end)                 !
          ! start at zero, 11/24/-4713, and move forward one day at a time !
          ! until 1/1/20,000                                               !
          !                                                                !
          ! note: to run test up to 1/1/200,000 (TMG 1.4), edit lines      !
          ! 174(200,000), 258(200000), 275(200,000), 277(200000), and      !
          ! 278(74769560)                                                 !
          !----------------------------------------------------------------!

          YYl = -4713
          MM  = 11
          DD  = 24
          Dl  = 0 

        else

          !----------------------------------------------------------------!
          !                     High range test (high end)                 !
          ! start near top end, and move forward one day at a time         !
          !----------------------------------------------------------------!

          ! set days to highest value representable with signed 64-bits:
          ! ((2**63)-1)/86400 = 106,751,991,167,300 days
          Dl = 1067519911  ! break up initialization,
          Dl = Dl * 100000  !  since F90 constants
          Dl = Dl + 67300   !    are 32-bit

	  ! Commented out because od test duration
          !TODO Create a flag to allow runing of this test.
          ! start back a few 10,000 years, then come forward
          !Dl = Dl - 10000000

          ! matching start date is October 3, 292,276,992,535
          !YYl = 292276992  ! break up initialization,
          !YYl = YYl * 1000  !   since F90 constants
          !YYl = YYl + 535   !     are 32-bit
          !MM = 10
          !DD = 3

          ! start back 1000 days, then come forward
          Dl = Dl - 1000

          ! matching start date is February 2, 292,277,019,912
          YYl = 292277019  ! break up initialization,
          YYl = YYl * 1000  !   since F90 constants
          YYl = YYl + 912   !     are 32-bit
          MM = 2
          DD = 2

        endif

        call ESMF_TimeSet(Time, yy_i8=YYl, mm=MM, dd=DD, &
                          calendar=gregorianCalendar, rc=rc)
        call ESMF_TimeGet(Time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)
        print *, "High range test #", test
        print *, "  Start Time Gregorian = ", rYYl, "/", rMM, "/", rDD
        print *, "  Start Julian Days = ", rDl
        print *

        done = .false.
        broken = .false.
        do while (.not.done .and. .not.broken)
    
          ! calculate next what next Julian date should be
          Dl = Dl + 1

          ! calculate what the next Gregorian date should be
          DD = DD + 1
          ! check if leap year
          if (MM.eq.2 .and. DD.eq.29) then
            if ((mod(YYl,ly1).eq.0).or. &
                             (mod(YYl,ly2).eq.0 .and. mod(YYl,ly3).ne.0)) then
              DaysPerMonth(2) = 29
            else
              DaysPerMonth(2) = 28
            end if
          end if
          if (DD.gt.DaysPerMonth(MM)) then
            DD = 1
            MM = MM + 1
            if (MM.eq.13) then
              MM = 1
              YYl = YYl + 1
              if (mod(YYl,tenthousand).eq.0) then
                print *, "YYl = ", YYl, ", Dl = ", Dl
              end if
              if (YYl.eq.20000) then
                done = .true.
              end if
            end if
          end if

          ! check calculated dates against ESMF dates
          broken = ESMF_CheckTime(Time, YYl=YYl, MM=MM, DD=DD, Dl=Dl, &
                                  gregCal=gregorianCalendar, &
                                  julCal=julianDayCalendar, rc=rc)
        end do

        ! test section ended, check results

        if (test.eq.HIGH_LO) then

          write(failMsg, *) &
                "High range (low end) not 1/1/20,000 or rc=ESMF_FAILURE"
          write(name, *) "Gregorian/Fliegel High Range (low end) Test"
          call ESMF_Test((YYl.eq.20000 .and. MM.eq.1 .and. DD.eq.1 &
                          .and. Dl.eq.9025910 .and. rc.eq.ESMF_SUCCESS), &
                          name, failMsg, result, ESMF_SRCLINE)
          print *

        else ! HIGH_HI
          
          ! expected endpoint year and julian day
          endYYl = 292277019
          endYYl = endYYl * 1000
          endYYl = endYYl + 914
          endDl  = 1067519911
          endDl  = endDl * 100000
          endDl  = endDl + 67301
          
          write(failMsg, *) &
            "High range (high end) not 10/30/292,277,019,914 or rc=ESMF_FAILURE"
          write(name, *) "Gregorian/Fliegel High Range (high end) Test"
          call ESMF_Test((YYl.eq.endYYl .and. MM.eq.10 .and. DD.eq.30 &
                          .and. Dl.eq.endDl .and. rc.eq.ESMF_SUCCESS), &
                          name, failMsg, result, ESMF_SRCLINE)
          print *

        end if

      end do

      ! destroy calendars
      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      call ESMF_CalendarDestroy(julianDayCalendar, rc)
#endif

      ! finalize ESMF framework
      call ESMF_TestEnd(result, ESMF_SRCLINE)

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_CheckTime - Check ESMF Time values against given values

! !INTERFACE:
      function ESMF_CheckTime(time, YYl, MM, DD, Dl, gregCal, julCal, rc)

! !RETURN VALUE:      
      logical :: ESMF_CheckTime

! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      integer(ESMF_KIND_I8), intent(in) :: YYl
      integer, intent(in) :: MM
      integer, intent(in) :: DD
      integer(ESMF_KIND_I8), intent(in) :: Dl
      type(ESMF_Calendar), intent(in) :: gregCal
      type(ESMF_Calendar), intent(in) :: julCal
      integer, intent(out), optional :: rc
        
! !DESCRIPTION:
!     Checks given values against those set/get with ESMF
!
!EOPI

      ! ESMF returned variables
      integer(ESMF_KIND_I8) :: rYYl, rDl
      integer :: rMM, rDD

      !
      ! Gregorian (Fliegel) test
      !

      ! set date via ESMF Gregorian calendar
      call ESMF_TimeSet(time, yy_i8=YYl, mm=MM, dd=DD, calendar=gregCal, rc=rc)

      ! see what we get back
      call ESMF_TimeGet(time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)

      if (.not.(rYYl.eq.YYl .and. rMM.eq.MM .and. rDD.eq.DD .and. &
                rDl.eq.Dl)) then
        broken = .true.
        print *, "Gregorian Set/Get"
        print *, " Fliegel breaks, should be = ", YYl, "/", MM, "/", DD, " ", Dl
        print *, "                 returned  = ", rYYl, "/", rMM, "/", rDD, &
                                                 " ", rDl
        print *
      end if

      !
      ! Gregorian Set/Julian Get test
      !

      ! see what we get back via Julian calendar
      call ESMF_TimeSet(time, calendar=julCal, rc=rc)
      call ESMF_TimeGet(time, d_i8=rDl, rc=rc)

      if (.not.(rDl.eq.Dl)) then
        broken = .true.
        print *, "Gregorian Set/Julian Get"
        print *, " breaks, should be = ", Dl
        print *, "         returned  = ", rDl
        print *
      end if

      !
      ! Julian test
      !

      ! set date via ESMF Julian calendar
      call ESMF_TimeSet(time, d_i8=Dl, calendar=julCal, rc=rc)

      ! see what we get back
      call ESMF_TimeGet(time, d_i8=rDl, rc=rc)

      if (.not.(rDl.eq.Dl)) then
        broken = .true.
        print *, "Julian Set/Get"
        print *, " breaks, should be = ", Dl
        print *, "         returned  = ", rDl
        print *
      end if

      !
      ! Julian Set/Gregorian Get test
      !

      ! see what we get back via Gregorian calendar
      call ESMF_TimeSet(time, calendar=gregCal, rc=rc)
      call ESMF_TimeGet(time, yy_i8=rYYl, mm=rMM, dd=rDD, d_i8=rDl, rc=rc)

      if (.not.(rYYl.eq.YYl .and. rMM.eq.MM .and. rDD.eq.DD .and. &
                rDl.eq.Dl)) then
        broken = .true.
        print *, "Julian Set/Gregorian Get"
        print *, " breaks, should be = ", YYl, "/", MM, "/", DD, " ", Dl
        print *, "         returned  = ", rYYl, "/", rMM, "/", rDD, " ", rDl
        print *
      end if

      ! function return value
      ESMF_CheckTime = broken

      end function ESMF_CheckTime

      end program ESMF_CalRangeUTest
