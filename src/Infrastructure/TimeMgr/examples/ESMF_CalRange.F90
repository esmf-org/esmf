! $Id: ESMF_CalRange.F90,v 1.1 2003/04/22 20:34:19 eschwab Exp $
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
      program ESMF_CalRange

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_CalRange - Calendar ranges
!
! !DESCRIPTION:
!
! This program shows calendar ranges
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_TimeMod
      use ESMF_CalendarMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CalRange.F90,v 1.1 2003/04/22 20:34:19 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate calendars
      type(ESMF_Calendar) :: gregorianCalendar
      type(ESMF_Calendar) :: noLeapCalendar
      type(ESMF_Calendar) :: day360Calendar
      type(ESMF_Calendar) :: julianCalendar

      integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth

      ! temp variables for Get functions
      integer(ESMF_IKIND_I8) :: YR, rYR, D
      integer :: MM, rMM, DD, rDD, H, M, S
      integer(ESMF_IKIND_I8) :: ly1 = 400, ly2 = 4, ly3 = 100, tenmill = 10000
      integer :: i
      type(ESMF_Time) :: checkTime

      ! result code
      integer :: rc

      !
      ! Julian Calendar
      !
      ! initialize calendar to be Julian type
      call ESMF_CalendarInit(JulianCalendar, ESMF_CAL_JULIAN, rc)

      !
      ! Gregorian Calendar
      !
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

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

      ! Fliegel algorithm find low range end point
      D = 0
      call ESMF_TimeInit(checkTime, D=D, cal=JulianCalendar, rc=rc)
      call ESMF_TimeSetCalendar(checkTime, gregorianCalendar, rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, rc=rc)
      print *, "Check Start Time Gregorian = ", YR, "/", MM, "/", DD

      rYR = YR
      rMM = MM
      rDD = DD
      do while (rYR.eq.YR .and. rMM.eq.MM .and. rDD.eq.DD)
        ! use ESMF calendar to set previous date
        D = D - 1
        call ESMF_TimeInit(checkTime, D=D, cal=JulianCalendar, rc=rc)
        call ESMF_TimeSetCalendar(checkTime, gregorianCalendar, rc)
        call ESMF_TimeGet(checkTime, YR=rYR, MM=rMM, DD=rDD, rc=rc)
    
        ! calculate what the previous date should be
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

      end do

      print *, "Check Time Breaks, should be = ", YR, "/", MM, "/", DD
      print *, "                   returned  = ", rYR, "/", rMM, "/", rDD

      ! Fliegel algorithm find high range end point
      D = 0
      call ESMF_TimeInit(checkTime, D=D, cal=JulianCalendar, rc=rc)
      call ESMF_TimeSetCalendar(checkTime, gregorianCalendar, rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, rc=rc)
      print *, "Check Start Time Gregorian = ", YR, "/", MM, "/", DD

      rYR = YR
      rMM = MM
      rDD = DD
      do while (rYR.eq.YR .and. rMM.eq.MM .and. rDD.eq.DD)
        ! use ESMF calendar to set next date
        D = D + 1
        call ESMF_TimeInit(checkTime, D=D, cal=JulianCalendar, rc=rc)
        call ESMF_TimeSetCalendar(checkTime, gregorianCalendar, rc)
        call ESMF_TimeGet(checkTime, YR=rYR, MM=rMM, DD=rDD, rc=rc)
    
        ! calculate what the next date should be
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
          MM = MM + 1
          if (MM.eq.13) then
            MM = 1
            YR = YR + 1
            if (mod(YR,tenmill).eq.0) then
              print *, "YR = ", YR
            end if
          end if
          DD = 1
        end if

      end do

      print *, "Check Time Breaks, should be = ", YR, "/", MM, "/", DD
      print *, "                   returned  = ", rYR, "/", rMM, "/", rDD

      end program ESMF_CalRange
