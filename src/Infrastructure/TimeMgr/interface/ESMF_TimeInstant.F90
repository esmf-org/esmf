! $Id: ESMF_TimeInstant.F90,v 1.1 2002/10/07 20:23:35 eschwab Exp $
    module ESMF_TimeInstantMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TimeInstantMod
!
! !USES:
        use ESMF_TimeMod
        use ESMF_TimeIntvMod
            
! !PUBLIC TYPES:
        type ESMF_TimeInstant
            private
            sequence
                type(ESMF_Time) :: time
                integer :: calendar
                integer :: timezone
        end type

!
! !PUBLIC MEMBER FUNCTIONS:
!       subroutine ESMF_TimeInstantInit(this, S, Sn, Sd, calendar, timezone, rc)
!       subroutine ESMF_TimeInstantInit(this, YR, MM, DD, D, H, M, S, &
!                                       MS, US, NS, Sn, Sd, &
!                                       d_, h_, m_, s_, ms_, us_, ns_,
!                                       calendar, timezone, rc)
!       subroutine ESMF_TimeInstantGet(this, YR, MM, DD, D, H, M, S, &
!                                      MS, US, NS, Sn, Sd, &
!                                      d_, h_, m_, s_, ms_, us_, ns_, rc)
!       subroutine ESMF_TimeInstantSet(this, YR, MM, DD, D, H, M, S, &
!                                      MS, US, NS, Sn, Sd, &
!                                      d_, h_, m_, s_, ms_, us_, ns_, rc)
!       subroutine ESMF_TimeInstantGet_D(this, D, rc)
!       subroutine ESMF_TimeInstantSet_D(this, D, rc)
!       subroutine ESMF_TimeInstantGet_d_(this, d, rc)
!       subroutine ESMF_TimeInstantSet_d_(this, d, rc)
!       subroutine ESMF_TimeInstantGet_s_(this, s, rc)
!       subroutine ESMF_TimeInstantSet_s_(this, s, rc)
!       subroutine ESMF_TimeInstantRead_S(this, S)
!       subroutine ESMF_TimeInstantWrite_S(this, S)
!       subroutine ESMF_TimeInstantRead_Sn(this, Sn)
!       subroutine ESMF_TimeInstantWrite_Sn(this, Sn)
!       subroutine ESMF_TimeInstantRead_Sd(this, Sd)
!       subroutine ESMF_TimeInstantWrite_Sd(this, Sd)
!       subroutine ESMF_TimeInstGet_YR_MM_DD_S(this, YR, MM, DD, S, rc)
!       subroutine ESMF_TimeInstSet_YR_MM_DD_S(this, YR, MM, DD, S, rc)
!       subroutine ESMF_TmInstGet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
!                                                H, M, S, rc)
!       subroutine ESMF_TmInstSet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
!                                                H, M, S, rc)
!       subroutine ESMF_TInsGetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
!                                                H, M, S, Sn, Sd, rc)
!       subroutine ESMF_TInsSetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
!                                                H, M, S, Sn, Sd, rc)
!       subroutine ESMF_TimeInstantGetCalendar(this, Calendar, rc)
!       subroutine ESMF_TimeInstantGetTimezone(this, Timezone, rc)
!       subroutine ESMF_TimeInstantGetString(this, Ts, rc)
!       subroutine ESMF_TimeInstantGetDayOfYear(this, DayOfYear, rc)
!       subroutine ESMF_TimeInstantGetDayOfWeek(this, DayOfWeek, rc)
!       subroutine ESMF_TimeInstantGetMidMonth(this, MidMonth, rc)
!       subroutine ESMF_TimeInstantGetRealTime(this, RealTime, rc)
!       function ESMF_TimeInstantEQ(timeinstant1, timeinstant2)
!       function ESMF_TimeInstantNE(timeinstant1, timeinstant2)
!       function ESMF_TimeInstantLT(timeinstant1, timeinstant2)
!       function ESMF_TimeInstantGT(timeinstant1, timeinstant2)
!       function ESMF_TimeInstantLE(timeinstant1, timeinstant2)
!       function ESMF_TimeInstantGE(timeinstant1, timeinstant2)
!		function ESMF_TimeInstantInc(timeinstant, timeinterval)
!       function ESMF_TimeInstantDec(timeinstant, timeinterval)
!       function ESMF_TimeInstantDiff(timeinstant1, timeinstant2)
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!       Part of Time Manager F90 API wrapper of C++ implemenation
!
!       Defines F90 wrapper entry points for corresponding
!        C++ class ESMC\_TimeInstant
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!

		interface ESMF_TimeInstantInit
			module procedure ESMF_TimeInstantInit1
			module procedure ESMF_TimeInstantInit2
		end interface

    contains
    
        subroutine ESMF_TimeInstantInit1(this,S, Sn, Sd, calendar, timezone, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int64), intent(in) :: S
            integer(int32), intent(in) :: Sn, Sd
            integer, intent(in) :: calendar, timezone
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantInit1(this,S, Sn, Sd, calendar, timezone, rc)
    
        end subroutine

        subroutine ESMF_TimeInstantInit2(this, YR, MM, DD, D, H, M, S, &
                                         MS, US, NS, Sn, Sd, &
                                         d_, h_, m_, s_, ms_, us_, ns_, &
                                         calendar, timezone, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(in), optional :: MM, DD, H, M, MS
            integer(int64), intent(in), optional :: S
            integer(int32), intent(in), optional :: YR, D, US, NS, Sn, Sd
            real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
            integer, intent(in) :: calendar, timezone
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_TimeInstantInit2(this, YR, MM, DD, D, H, M, S, &
                                        MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, &
                                        calendar, timezone, rc)
    
        end subroutine
    
        !
        ! generic get/set routines which use F90 optional arguments
        !

        subroutine ESMF_TimeInstantGet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out), optional :: MM, DD, H, M, MS
            integer(int64), intent(out), optional :: S
            integer(int32), intent(out), optional :: YR, D, US, NS, Sn, Sd
            real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_TimeInstantGet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)
    
        end subroutine

        subroutine ESMF_TimeInstantSet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(in), optional :: MM, DD, H, M, MS
            integer(int64), intent(in), optional :: S
            integer(int32), intent(in), optional :: YR, D, US, NS, Sn, Sd
            real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_TimeInstantSet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)
    
        end subroutine

        !
        ! wrappers for "inherited" ESMF_Time base class routines
        !

        subroutine ESMF_TimeInstantGet_D(this, D, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(out) :: D
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_D(this%time, D, rc)

        end subroutine

        subroutine ESMF_TimeInstantSet_D(this, D, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(in) :: D
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_D(this%time, D, rc)

        end subroutine

        subroutine ESMF_TimeInstantGet_d_(this, d, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            real, intent(out) :: d
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_d_(this%time, d, rc)

        end subroutine

        subroutine ESMF_TimeInstantSet_d_(this, d, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            real, intent(in) :: d
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_d_(this%time, d, rc)

        end subroutine

        subroutine ESMF_TimeInstantGet_s_(this, s, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            real, intent(out) :: s
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_s(this%time, s, rc)

        end subroutine

        subroutine ESMF_TimeInstantSet_s_(this, s, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            real, intent(in) :: s
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_s(this%time, s, rc)

        end subroutine

        subroutine ESMF_TimeInstantRead_S(this, S)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int64), intent(out) :: S
    
            print *, "ESMF_TimeTimeInstantRead_S entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_S(this%time, S)

        end subroutine

        subroutine ESMF_TimeInstantWrite_S(this, S)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int64), intent(in) :: S
    
            print *, "ESMF_TimeInstantWrite_S entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_S(this%time, S, rc)

        end subroutine

        subroutine ESMF_TimeInstantRead_Sn(this, Sn)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(out) :: Sn
    
            print *, "ESMF_TimeInstantRead_Sn entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sn(this%time, Sn, rc)

        end subroutine

        subroutine ESMF_TimeInstantWrite_Sn(this, Sn)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int64), intent(in) :: Sn
    
            print *, "ESMF_TimeInstantWrite_Sn entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sn(this%time, Sn, rc)

        end subroutine

        subroutine ESMF_TimeInstantRead_Sd(this, Sd)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(out) :: Sd
    
            print *, "ESMF_TimeInstantRead_Sd entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sd(this%time, Sd, rc)

        end subroutine

        subroutine ESMF_TimeInstantWrite_Sd(this, Sd)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int64), intent(in) :: Sd
    
            print *, "ESMF_TimeInstantWrite_Sd entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sd(this%time, Sd, rc)

        end subroutine

        !
        !  wrappers for ESMF_Time base class overloaded operators
        !

        ! overloaded (==) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantEQ(timeinstant1, timeinstant2)
            logical :: ESMF_TimeInstantEQ
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeEQ(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstantEQ)

        end function

        ! overloaded (/=) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantNE(timeinstant1, timeinstant2)
            logical :: ESMF_TimeInstantNE
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeNE(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstantNE)

        end function

        ! overloaded (<) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantLT(timeinstant1, timeinstant2)
            logical :: ESMF_TimeInstantLT
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeLT(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstantLT)

        end function

        ! overloaded (>) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantGT(timeinstant1, timeinstant2)
            logical :: ESMF_TimeInstantGT
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeGT(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstantGT)

        end function

        ! overloaded (<=) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantLE(timeinstant1, timeinstant2)
            logical :: ESMF_TimeInstantLE
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeLE(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstantLE)

        end function

        ! overloaded (>=) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantGE(timeinstant1, timeinstant2)
            logical :: ESMF_TimeInstantGE
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeGE(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstantGE)

        end function

		! overloaded (+) operator interface function maps to
		!   ESMF_Time base class
		function ESMF_TimeInstantInc(timeinstant, timeinterval)
			type(ESMF_TimeInstant) :: ESMF_TimeInstantInc
			type(ESMF_TimeInstant), intent(in) :: timeinstant
			type(ESMF_TimeIntv), intent(in) :: timeinterval
			type(ESMF_Time) :: time
			integer(int64) :: S
			integer(int32) :: Sn, Sd

			! get time from timeinterval (really need C++ "friend" feature ?? )
			call c_ESMF_TimeIntvGet_S_nd(interval, S, Sn, Sd, rc)
			call c_ESMF_TimeInit(time, S, Sn, Sd, rc)

			! call ESMF_Time base class function
			call c_ESMF_TimeSum(timeinstant%time, time, &
								ESMF_TimeInstantInc%time)

		end function

        ! overloaded (-) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantDec(timeinstant, timeinterval)
            type(ESMF_TimeInstant) :: ESMF_TimeInstantDec
            type(ESMF_TimeInstant), intent(in) :: timeinstant
            type(ESMF_TimeIntv), intent(in) :: timeinterval
            type(ESMF_Time) :: time
            integer(int64) :: S
            integer(int32) :: Sn, Sd

            ! get time from timeinterval (really need C++ "friend" feature ?? )
            call c_ESMF_TimeIntvGet_S_nd(interval, S, Sn, Sd, rc)
            call c_ESMF_TimeInit(time, S, Sn, Sd, rc)

            ! call ESMF_Time base class function
            call c_ESMF_TimeDiff(timeinstant%time, time, &
                                 ESMF_TimeInstantDec%time)

        end function

        ! overloaded (-) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeInstantDiff(timeinstant1, timeinstant2)
            type(ESMF_TimeInstant) :: ESMF_TimeInstantDiff
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

            ! call ESMF_Time base class function
            call c_ESMF_TimeDiff(timeinstant1%time, timeinstant2%time, &
                                 ESMF_TimeInstantDiff%time)

        end function

        !
        ! shortcut routines for common get/set groupings
        !

        subroutine ESMF_TimeInstGet_YR_MM_DD_S(this, YR, MM, DD, S, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(out) :: YR, S
            integer, intent(out) :: MM, DD
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstGet_YR_MM_DD_S(this, YR, MM, DD, S, rc)

        end subroutine

        subroutine ESMF_TimeInstSet_YR_MM_DD_S(this, YR, MM, DD, S, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(in) :: YR, S
            integer, intent(in) :: MM, DD
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstSet_YR_MM_DD_S(this, YR, MM, DD, S, rc)

        end subroutine

        subroutine ESMF_TmInstGet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(out) :: YR
            integer, intent(out) :: MM, DD, H, M, S
            integer, intent(out), optional :: rc
    
            call c_ESMF_TmInstGet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)

        end subroutine

        subroutine ESMF_TmInstSet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(in) :: YR
            integer, intent(in) :: MM, DD, H, M, S
            integer, intent(in), optional :: rc
    
            call c_ESMF_TmInstSet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)

        end subroutine

        subroutine ESMF_TInsGetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(out) :: YR, Sn, Sd
            integer, intent(out) :: MM, DD, H, M, S
            integer, intent(out), optional :: rc
    
            call c_ESMF_TInsGetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)

        end subroutine

        subroutine ESMF_TInsSetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer(int32), intent(in) :: YR, Sn, Sd
            integer, intent(in) :: MM, DD, H, M, S
            integer, intent(in), optional :: rc
    
            call c_ESMF_TInsSetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetCalendar(this, Calendar, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: Calendar
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetCalendar(this, Calendar, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetTimezone(this, Timezone, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: Timezone
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetTimezone(this, Timezone, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetString(this, Ts, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            character, dimension(40), intent(out) :: Ts
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetString(this, Ts, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetDayOfYear(this, DayOfYear, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: DayOfYear
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetDayOfYear(this, DayOfYear, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetDayOfWeek(this, DayOfWeek, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: DayOfWeek
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetDayOfWeek(this, DayOfWeek, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetMidMonth(this, MidMonth, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: MidMonth
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetMidMonth(this, MidMonth, rc)

        end subroutine

        subroutine ESMF_TimeInstantGetRealTime(this, RealTime, rc)
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: RealTime
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeInstantGetRealTime(this, RealTime, rc)

        end subroutine
!EOP
!===============================================================================
    end module ESMF_TimeInstantMod
