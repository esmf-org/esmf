! $Id: ESMF_Calendar.F90,v 1.1 2002/10/07 20:23:35 eschwab Exp $
      module ESMF_CalendarMod
!===============================================================================
!BOP
! !MODULE: ESMF_CalendarMod
!
! !USES:
        use ESMF_TimeInstantMod
!
! !PUBLIC TYPES:
      implicit none

        integer, parameter :: MonthsPerYear = 12

        integer, parameter :: ESMF_GREGORIAN = 1, ESMF_JULIAN = 2, &
                              ESMF_NOLEAP = 3, ESMF_360DAY = 4, &
                              ESMF_GENERIC = 5, ESMF_NOCALENDAR = 6

        type ESMF_DaysPerYear
            private
            sequence
                integer :: D     ! whole days per year
                integer :: Dn    ! fractional days per year numerator
                integer :: Dd    ! fractional days per year denominator
        end type

        type ESMF_Calendar
            private
            sequence
                integer :: Type
                integer, dimension(MonthsPerYear) :: DaysPerMonth
                integer :: SecondsPerDay
                type(ESMF_DaysPerYear) :: DaysPerYear
        end type

! !PUBLIC MEMBER FUNCTIONS:
!        subroutine ESMF_CalendarInit(this, Type, rc)
!        subroutine ESMF_CalendarInitGeneric(this, DaysPerMonth, &
!                                            SecondsPerDay, DaysPerYear, &
!                                            DaysPerYearDn, DaysPerYearDd, rc)
!        subroutine ESMF_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
!                                              MS, US, NS, Sn, Sd, &
!                                              d_, h_, m_, s_, ms_, us_, ns_, &
!                                              timeinstant, rc)
!        subroutine ESMF_CalendarConvertToDate(timeinstant, YR, MM, DD, D, H, &
!                                              M, S, MS, US, NS, Sn, Sd, &
!                                              d_, h_, m_, s_, ms_, us_, ns_, &
!                                              rc)
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!       Part of Time Manager F90 API wrapper of C++ implemenation
!
!       Defines F90 wrapper entry points for corresponding
!        C++ class ESMC\_Calendar
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!

    contains
    
        subroutine ESMF_CalendarInit(this, Type, rc)
            type(ESMF_Calendar), intent(inout) :: this
            integer, intent(in) :: Type
            integer, intent(out), optional :: rc
    
            call c_ESMF_CalendarInit(this, Type, rc)
    
        end subroutine
    
        subroutine ESMF_CalendarInitGeneric(this, DaysPerMonth, &
                                            SecondsPerDay, DaysPerYear, &
                                            DaysPerYearDn, DaysPerYearDd, rc)
            type(ESMF_Calendar), intent(inout) :: this
            integer, dimension(MonthsPerYear), intent(in) :: DaysPerMonth
            integer, intent(in) :: SecondsPerDay, DaysPerYear, &
                                   DaysPerYearDn, DaysPerYearDd
            integer, intent(out), optional :: rc
    
            call c_ESMF_CalendarInitGeneric(this, DaysPerMonth, &
                                            SecondsPerDay, DaysPerYear, &
                                            DaysPerYearDn, DaysPerYearDd, rc)
    
        end subroutine
    
        !
        ! Date <--> Time conversion routines, use F90 optional arguments
        !

        subroutine ESMF_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
                                              MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              time, rc)
            integer, intent(in), optional :: MM, DD, H, M, MS
            integer(int64), intent(in), optional :: S
            integer(int32), intent(in), optional :: YR, D, US, NS, Sn, Sd
            real, intent(in), optional :: d_, h_, m_, s_, ms_, us_, ns_
            type(ESMF_Time) :: time
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
                                              MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              time, rc)
    
        end subroutine

        subroutine ESMF_CalendarConvertToDate(time, YR, MM, DD, D, H, &
                                              M, S, MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              rc)
            integer, intent(out), optional :: MM, DD, H, M, MS
            integer(int64), intent(out), optional :: S
            integer(int32), intent(out), optional :: YR, D, US, NS, Sn, Sd
            real, intent(in), optional :: d_, h_, m_, s_, ms_, us_, ns_
            type(ESMF_Time) :: time
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_CalendarConvertToDate(time, YR, MM, DD, D, H, &
                                              M, S, MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              rc)
    
        end subroutine
!EOP
!===============================================================================
    end module ESMF_CalendarMod
