! $Id: ESMF_Calendar.F90,v 1.2 2002/10/23 16:42:46 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Calendar Module
!
! (all lines below between the !BOP and !EOP markers will be included in
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_TimeMgr.h>

!------------------------------------------------------------------------------
! module definition

     module ESMF_CalendarMod
!
!BOP
! !MODULE: ESMF_CalendarMod
!
!
! !DESCRIPTION:
!
! !USES:
        use ESMF_TimeInstantMod
        implicit none
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:

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
!EOP
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!
!------------------------------------------------------------------------------

    contains

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarInit - Initializes the calendar

! !INTERFACE:
        subroutine ESMF_CalendarInit(this, Type, rc)

! !ARGUMENTS:
        type(ESMF_Calendar), intent(inout) :: this
        integer, intent(in) :: Type
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!    
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
!       invoke C to C++ entry point
        call c_ESMF_CalendarInit(this, Type, rc)
    
        end subroutine
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarInitGeneric

! !INTERFACE:
        subroutine ESMF_CalendarInitGeneric(this, DaysPerMonth, &
                                            SecondsPerDay, DaysPerYear, &
                                            DaysPerYearDn, DaysPerYearDd, rc)
! !ARGUMENTS:
        type(ESMF_Calendar), intent(inout) :: this
        integer, dimension(MonthsPerYear), intent(in) :: DaysPerMonth
        integer, intent(in) :: SecondsPerDay, DaysPerYear, &
                               DaysPerYearDn, DaysPerYearDd
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     
!EOP
! !REQUIREMENTS:  AAAn.n.n

!       invoke C to C++ entry point
        call c_ESMF_CalendarInitGeneric(this, DaysPerMonth, &
                                        SecondsPerDay, DaysPerYear, &
                                        DaysPerYearDn, DaysPerYearDd, rc)
    
        end subroutine
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarConvertToTime

! !INTERFACE:
        subroutine ESMF_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
                                              MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              time, rc)

! !ARGUMENTS:
        integer, intent(in), optional :: MM, DD, H, M, MS
        integer(int64), intent(in), optional :: S
        integer(int32), intent(in), optional :: YR, D, US, NS, Sn, Sd
        real, intent(in), optional :: d_, h_, m_, s_, ms_, us_, ns_
        type(ESMF_Time) :: time
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
! Date <--> Time conversion routines, use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

!       invoke C to C++ entry point
    
        ! use optional args for any subset
         call c_ESMF_CalendarConvertToTime(YR, MM, DD, D, H, M, S, &
                                           MS, US, NS, Sn, Sd, &
                                           d_, h_, m_, s_, ms_, us_, ns_, &
                                           time, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarConvertToDate

! !INTERFACE:
        subroutine ESMF_CalendarConvertToDate(time, YR, MM, DD, D, H, &
                                              M, S, MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              rc)

! !ARGUMENTS:
        integer, intent(out), optional :: MM, DD, H, M, MS
        integer(int64), intent(out), optional :: S
        integer(int32), intent(out), optional :: YR, D, US, NS, Sn, Sd
        real, intent(in), optional :: d_, h_, m_, s_, ms_, us_, ns_
        type(ESMF_Time) :: time
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initialize a TimeInstant with values S, Sn, Sd, calendar & timezone
!EOP
! !REQUIREMENTS:  AAAn.n.n

!       invoke C to C++ entry point
        ! use optional args for any subset
         call c_ESMF_CalendarConvertToDate(time, YR, MM, DD, D, H, &
                                              M, S, MS, US, NS, Sn, Sd, &
                                              d_, h_, m_, s_, ms_, us_, ns_, &
                                              rc)
    
        end subroutine
!EOP
!===============================================================================
    end module ESMF_CalendarMod
