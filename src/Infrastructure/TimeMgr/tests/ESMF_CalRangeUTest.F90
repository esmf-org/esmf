! $Id: ESMF_CalRangeUTest.F90,v 1.2 2003/05/07 16:53:12 eschwab Exp $
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

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_CalRangeUTest - Calendar range Unit tests
!
! !DESCRIPTION:
!
! This program tests calendar ranges
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod      ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CalRangeUTest.F90,v 1.2 2003/05/07 16:53:12 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate calendars
      type(ESMF_Calendar) :: gregorianCalendar
      type(ESMF_Calendar) :: julianCalendar

      ! instantiate time instant
      type(ESMF_Time) :: Time

      integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth

      ! temp variables
      integer(ESMF_IKIND_I8) :: YR, rYR, D, rD, endYR, endD
      integer :: MM, rMM, DD, rDD
      integer(ESMF_IKIND_I8), parameter :: ly1 = 400, ly2 = 4, ly3 = 100
      integer(ESMF_IKIND_I8), parameter :: tenthousand = 10000
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


      ! Julian Calendar

      ! initialize calendar to be Julian type
      call ESMF_CalendarInit(julianCalendar, ESMF_CAL_JULIAN, rc)

      ! Gregorian Calendar

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

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

      YR = -4713
      MM = 11
      DD = 24
      D  = 0 
      call ESMF_TimeInit(Time, YR=YR, MM=MM, DD=DD, &
                         cal=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(Time, YR=rYR, MM=rMM, DD=rDD, D=rD, rc=rc)
      print *, "Low range test"
      print *, "  Start Time Gregorian = ", rYR, "/", rMM, "/", rDD
      print *, "  Start Julian Days = ", rD
      print *

      broken = .false.
      do while (.not.broken)
    
        ! calculate what previous Julian date should be
        D = D - 1

        ! calculate what the previous Gregorian date should be
        DD = DD - 1
        if (DD.eq.0) then
          MM = MM - 1
          if (MM.eq.0) then
            MM = 12
            YR = YR - 1
          end if
          DD = DaysPerMonth(MM)
          ! check if leap year
          if (MM.eq.2) then
            if ((mod(YR,ly1).eq.0).or. &
                             (mod(YR,ly2).eq.0 .and. mod(YR,ly3).ne.0)) then
              DD = 29
            end if
          end if
        end if

        ! check calculated dates against ESMF dates
        broken = ESMF_CheckTime(Time, YR=YR, MM=MM, DD=DD, D=D, &
                                gregCal=gregorianCalendar, &
                                julCal=julianCalendar, rc=rc)
      end do

      write(failMsg, *) "Low range endpoint not -4800/2/29 or rc=ESMF_FAILURE"
      write(name, *) "Gregorian/Fliegel Low Range Test"
      call ESMF_Test((YR.eq.-4800 .and. MM.eq.2 .and. DD.eq.29 &
                      .and. D.eq.-32045 .and. rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *

      !------------------------------------------------------------------!
      !                High range tests (low end & high end)             !   
      ! test only a few 10,000 years on either end, since it would take  !
      ! a few decades of real wall clock time to step through the entire !
      ! range (TODO: parallelize?)                                       !
      !------------------------------------------------------------------!

#ifdef ESMF_EXHAUSTIVE
      first_test = HIGH_LO
      last_test  = HIGH_HI
#else
      first_test = HIGH_HI
      last_test  = HIGH_HI
#endif

      do test = first_test, last_test

        if (test.eq.HIGH_LO) then

          !----------------------------------------------------------------!
          !                      High range test (low end)                 !
          ! start at zero, 11/24/-4713, and move forward one day at a time !
          ! until 1/1/200,000                                              !
          !----------------------------------------------------------------!

          YR = -4713
          MM = 11
          DD = 24
          D  = 0 

        else

          !----------------------------------------------------------------!
          !                     High range test (high end)                 !
          ! start near top end, and move forward one day at a time         !
          !----------------------------------------------------------------!

          ! set days to highest value representable with signed 64-bits:
          ! ((2**63)-1)/86400 = 106,751,991,167,300 days
          D = 1067519911  ! break up initialization,
          D = D * 100000  !  since F90 constants
          D = D + 67300   !    are 32-bit

#ifdef ESMF_EXHAUSTIVE
          ! start back a few 10,000 years, then come forward
          D = D - 20000000

          ! matching start date is September 7, 292,276,965,156
          YR = 292276965  ! break up initialization,
          YR = YR * 1000  !   since F90 constants
          YR = YR + 156   !     are 32-bit
          MM = 9
          DD = 7
#else
          ! start back 1000 days, then come forward
          D = D - 1000

          ! matching start date is February 2, 292,277,019,912
          YR = 292277019  ! break up initialization,
          YR = YR * 1000  !   since F90 constants
          YR = YR + 912   !     are 32-bit
          MM = 2
          DD = 2
#endif

        endif

        call ESMF_TimeInit(Time, YR=YR, MM=MM, DD=DD, &
                         cal=gregorianCalendar, rc=rc)
        call ESMF_TimeGet(Time, YR=rYR, MM=rMM, DD=rDD, D=rD, rc=rc)
        print *, "High range test #", test
        print *, "  Start Time Gregorian = ", rYR, "/", rMM, "/", rDD
        print *, "  Start Julian Days = ", rD
        print *

        done = .false.
        broken = .false.
        do while (.not.done .and. .not.broken)
    
          ! calculate next what next Julian date should be
          D = D + 1

          ! calculate what the next Gregorian date should be
          DD = DD + 1
          ! check if leap year
          if (MM.eq.2 .and. DD.eq.29) then
            if ((mod(YR,ly1).eq.0).or. &
                             (mod(YR,ly2).eq.0 .and. mod(YR,ly3).ne.0)) then
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
              YR = YR + 1
              if (mod(YR,tenthousand).eq.0) then
                print *, "YR = ", YR, ", D = ", D
              end if
              if (YR.eq.200000) then
                done = .true.
              end if
            end if
          end if

          ! check calculated dates against ESMF dates
          broken = ESMF_CheckTime(Time, YR=YR, MM=MM, DD=DD, D=D, &
                                  gregCal=gregorianCalendar, &
                                  julCal=julianCalendar, rc=rc)
        end do

        ! test section ended, check results

        if (test.eq.HIGH_LO) then

          write(failMsg, *) &
                "High range (low end) not 1/1/200,000 or rc=ESMF_FAILURE"
          write(name, *) "Gregorian/Fliegel High Range (low end) Test"
          call ESMF_Test((YR.eq.200000 .and. MM.eq.1 .and. DD.eq.1 &
                          .and. D.eq.74769560 .and. rc.eq.ESMF_SUCCESS), &
                          name, failMsg, result, ESMF_SRCLINE)
          print *

        else ! HIGH_HI
          
          ! expected endpoint year and julian day
          endYR = 292277019
          endYR = endYR * 1000
          endYR = endYR + 914
          endD  = 1067519911
          endD  = endD * 100000
          endD  = endD + 67301
          
          write(failMsg, *) &
            "High range (high end) not 10/30/292,277,019,914 or rc=ESMF_FAILURE"
          write(name, *) "Gregorian/Fliegel High Range (high end) Test"
          call ESMF_Test((YR.eq.endYR .and. MM.eq.10 .and. DD.eq.30 &
                          .and. D.eq.endD .and. rc.eq.ESMF_SUCCESS), &
                          name, failMsg, result, ESMF_SRCLINE)
          print *

        end if

      end do

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CheckTime - Check ESMF Time values against given values

! !INTERFACE:
      function ESMF_CheckTime(time, YR, MM, DD, D, gregCal, julCal, rc)

! !RETURN VALUE:      
      logical :: ESMF_CheckTime

! !ARGUMENTS:
      type(ESMF_Time), intent(inout) :: time
      integer(ESMF_IKIND_I8), intent(in) :: YR
      integer, intent(in) :: MM
      integer, intent(in) :: DD
      integer(ESMF_IKIND_I8), intent(in) :: D
      type(ESMF_Calendar), intent(in) :: gregCal
      type(ESMF_Calendar), intent(in) :: julCal
      integer, intent(out), optional :: rc
        
! !DESCRIPTION:
!     Checks given values against those set/get with ESMF
!
!EOP

      ! ESMF returned variables
      integer(ESMF_IKIND_I8) :: rYR, rD
      integer :: rMM, rDD

      !
      ! Gregorian (Fliegel) test
      !

      ! set date via ESMF Gregorian calendar
      call ESMF_TimeSetCalendar(time, cal=gregCal, rc=rc)
      call ESMF_TimeSet(time, YR=YR, MM=MM, DD=DD, rc=rc)

      ! see what we get back
      call ESMF_TimeGet(time, YR=rYR, MM=rMM, DD=rDD, D=rD, rc=rc)

      if (.not.(rYR.eq.YR .and. rMM.eq.MM .and. rDD.eq.DD .and. rD.eq.D)) then
        broken = .true.
        print *, "Gregorian Set/Get"
        print *, " Fliegel breaks, should be = ", YR, "/", MM, "/", DD, " ", D
        print *, "                 returned  = ", rYR, "/", rMM, "/", rDD, &
                                                 " ", rD
        print *
      end if

      !
      ! Gregorian Set/Julian Get test
      !

      ! see what we get back via Julian calendar
      call ESMF_TimeSetCalendar(time, cal=julCal, rc=rc)
      call ESMF_TimeGet(time, D=rD, rc=rc)

      if (.not.(rD.eq.D)) then
        broken = .true.
        print *, "Gregorian Set/Julian Get"
        print *, " breaks, should be = ", D
        print *, "         returned  = ", rD
        print *
      end if

      !
      ! Julian test
      !

      ! set date via ESMF Julian calendar
      call ESMF_TimeSetCalendar(time, cal=julCal, rc=rc)
      call ESMF_TimeSet(time, D=D, rc=rc)

      ! see what we get back
      call ESMF_TimeGet(time, D=rD, rc=rc)

      if (.not.(rD.eq.D)) then
        broken = .true.
        print *, "Julian Set/Get"
        print *, " breaks, should be = ", D
        print *, "         returned  = ", rD
        print *
      end if

      !
      ! Julian Set/Gregorian Get test
      !

      ! see what we get back via Gregorian calendar
      call ESMF_TimeSetCalendar(time, cal=gregCal, rc=rc)
      call ESMF_TimeGet(time, YR=rYR, MM=rMM, DD=rDD, D=rD, rc=rc)

      if (.not.(rYR.eq.YR .and. rMM.eq.MM .and. rDD.eq.DD .and. rD.eq.D)) then
        broken = .true.
        print *, "Julian Set/Gregorian Get"
        print *, " breaks, should be = ", YR, "/", MM, "/", DD, " ", D
        print *, "         returned  = ", rYR, "/", rMM, "/", rDD, " ", rD
        print *
      end if

      ! function return value
      ESMF_CheckTime = broken

      end function ESMF_CheckTime

      end program ESMF_CalRangeUTest
