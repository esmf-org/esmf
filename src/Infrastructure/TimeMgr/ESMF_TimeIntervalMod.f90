! $Id: ESMF_TimeIntervalMod.f90,v 1.1 2002/08/18 23:22:49 eschwab Exp $
    module ESMF_TimeIntervalMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TimeIntervalMod
!
! !USES:
        use ESMF_TypesMod
        use ESMF_FractionMod
        use ESMF_TimeMod    ! inherit from base class
!
! !PUBLIC TYPES:
        implicit none

        type ESMF_TimeInterval
            private
            sequence
                type(ESMF_Time) :: time
        end type
!
! !PUBLIC MEMBER FUNCTIONS:
!       subroutine ESMF_TimeIntervalInit(this, S, Sn, Sd, rc)
!       subroutine ESMF_TimeIntervalInit(this, D, H, M, S, MS, US, NS, Sn, Sd, &
!                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
!       subroutine ESMF_TimeIntervalGet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
!                                       _d, _h, _m, _s, _ms, _us, _ns, rc)
!       subroutine ESMF_TimeIntervalSet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
!                                       _d, _h, _m, _s, _ms, _us, _ns, rc)
!       subroutine ESMF_TimeIntervalGet_S(this, S, rc)
!       subroutine ESMF_TimeIntervalSet_S(this, S, rc)
!       subroutine ESMF_TimeIntervalGet_s_(this, s, rc)
!       subroutine ESMF_TimeIntervalSet_s_(this, s, rc)
!       subroutine ESMF_TimeIntervalGet_h(this, h, rc)
!       subroutine ESMF_TimeIntervalSet_h(this, h, rc)
!       subroutine ESMF_TimeIntervalRead_S(this, S)
!       subroutine ESMF_TimeIntervalWrite_S(this, S)
!       subroutine ESMF_TimeIntervalRead_Sn(this, Sn)
!       subroutine ESMF_TimeIntervalWrite_Sn(this, Sn)
!       subroutine ESMF_TimeIntervalRead_Sd(this, Sd)
!       subroutine ESMF_TimeIntervalWrite_Sd(this, Sd)
!       subroutine ESMF_TimeIntvGet_S_nd(this, S, Sn, Sd, rc)
!       subroutine ESMF_TimeIntvSet_S_nd(this, S, Sn, Sd, rc)
!       subroutine ESMF_TimeIntvGet_D_S(this, D, S, rc)
!       subroutine ESMF_TimeIntvSet_D_S(this, D, S, rc)
!       subroutine ESMF_TimeIntvGet_D_H_M_S_MS(this, D, H, M, S, MS, rc)
!       subroutine ESMF_TimeIntvSet_D_H_M_S_MS(this, D, H, M, S, MS, rc)
!       subroutine ESMF_TimeIntervalGetString(this, Ts, rc)
!       subroutine ESMF_TimeIntervalGetAbsValue(this, rc)
!       function ESMF_TimeIntervalEQ(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalNE(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalLT(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalGT(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalLE(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalGE(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalSum(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalDiff(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalQuot(timeinterval1, timeinterval2)
!       function ESMF_TimeIntervalQuotI(timeinterval, divisor)
!       function ESMF_TimeIntervalProdI(timeinterval, multiplier)
!       function ESMF_TimeIntervalProdF(timeinterval, multiplier)
!       function ESMF_TimeIntervalProdR(timeinterval, multiplier)
!
! !PUBLIC DATA MEMBERS:
! 
! !DESCRIPTION:
!       Part of Time Manager F90 API wrapper of C++ implemenation
!
!       Defines F90 wrapper entry points for corresponding
!        C++ class ESMC\_TimeInterval
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!
!EOP
!===============================================================================
    
        interface ESMF_TimeIntervalInit
            module procedure ESMF_TimeIntervalInit1
            module procedure ESMF_TimeIntervalInit2
        end interface

    contains
    
        subroutine ESMF_TimeIntervalInit1(this, S, Sn, Sd, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(in) :: S
            integer(int32), intent(in) :: Sn, Sd
            integer, intent(out), optional :: rc

            call c_ESMF_TimeIntervalInit1(this, S, Sn, Sd, rc)
    
        end subroutine
    
        subroutine ESMF_TimeIntervalInit2(this,D, H, M, S, MS, US, NS, Sn, Sd, &
                                          d_, h_, m_, s_, ms_, us_, ns_, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer, intent(in), optional :: H, M, MS
            integer(int64), intent(in), optional :: S
            integer(int32), intent(in), optional :: D, US, NS, Sn, Sd
            real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_TimeIntervalInit2(this,D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)

        end subroutine

        !
        ! generic get/set routines which use F90 optional arguments
        !

        subroutine ESMF_TimeIntervalGet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer, intent(out), optional :: H, M, MS
            integer(int64), intent(out), optional :: S
            integer(int32), intent(out), optional :: D, US, NS, Sn, Sd
            real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_TimeIntervalGet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
    
        end subroutine

        subroutine ESMF_TimeIntervalSet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer, intent(in), optional :: H, M, MS
            integer(int64), intent(in), optional :: S
            integer(int32), intent(in), optional :: D, US, NS, Sn, Sd
            real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
            integer, intent(out), optional :: rc
    
            ! use optional args for any subset
            call c_ESMF_TimeIntervalSet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
    
        end subroutine

        !
        ! wrappers for "inherited" ESMF_Time base class routines
        !

        subroutine ESMF_TimeIntervalGet_S(this, S, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(out) :: S
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_S(this%time, S, rc)

        end subroutine

        subroutine ESMF_TimeIntervalSet_S(this, S, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(in) :: S
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_S(this%time, S, rc)

        end subroutine

        subroutine ESMF_TimeIntervalGet_s_(this, s, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            real, intent(out) :: s
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_s(this%time, s, rc)

        end subroutine

        subroutine ESMF_TimeIntervalSet_s_(this, s, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            real, intent(in) :: s
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_s(this%time, s, rc)

        end subroutine

        subroutine ESMF_TimeIntervalGet_h(this, h, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            real, intent(out) :: h
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_h(this%time, h, rc)

        end subroutine

        subroutine ESMF_TimeIntervalSet_h(this, h, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            real, intent(in) :: h
            integer, intent(out), optional :: rc
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_h(this%time, h, rc)

        end subroutine

        subroutine ESMF_TimeIntervalRead_S(this, S, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(out) :: S
            integer, intent(out), optional :: rc
    
            print *, "ESMF_TimeIntervalRead_S entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_S(this%time, S, rc)

        end subroutine

        subroutine ESMF_TimeIntervalWrite_S(this, S, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(in) :: S
            integer, intent(out), optional :: rc
    
            print *, "ESMF_TimeIntervalWrite_S entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_S(this%time, S, rc)

        end subroutine

        subroutine ESMF_TimeIntervalRead_Sn(this, Sn, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int32), intent(out) :: Sn
            integer, intent(out), optional :: rc
    
            print *, "ESMF_TimeIntervalRead_Sn entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sn(this%time, Sn, rc)

        end subroutine

        subroutine ESMF_TimeIntervalWrite_Sn(this, Sn, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(in) :: Sn
            integer, intent(out), optional :: rc
    
            print *, "ESMF_TimeIntervalWrite_Sn entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sn(this%time, Sn, rc)

        end subroutine

        subroutine ESMF_TimeIntervalRead_Sd(this, Sd, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int32), intent(out) :: Sd
            integer, intent(out), optional :: rc
    
            print *, "ESMF_TimeIntervalRead_Sd entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sd(this%time, Sd, rc)

        end subroutine

        subroutine ESMF_TimeIntervalWrite_Sd(this, Sd, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(in) :: Sd
            integer, intent(out), optional :: rc
    
            print *, "ESMF_TimeIntervalWrite_Sd entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sd(this%time, Sd, rc)

        end subroutine


        !
        !  wrappers for ESMF_Time base class overloaded operators
        !

        ! overloaded (==) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalEQ(timeinterval1, timeinterval2)
            logical :: ESMF_TimeIntervalEQ
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeEQ(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalEQ)

        end function

        ! overloaded (/=) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalNE(timeinterval1, timeinterval2)
            logical :: ESMF_TimeIntervalNE
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeNE(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalNE)

        end function

        ! overloaded (<) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalLT(timeinterval1, timeinterval2)
            logical :: ESMF_TimeIntervalLT
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeLT(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalLT)

        end function

        ! overloaded (>) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalGT(timeinterval1, timeinterval2)
            logical :: ESMF_TimeIntervalGT
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeGT(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalGT)

        end function

        ! overloaded (<=) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalLE(timeinterval1, timeinterval2)
            logical :: ESMF_TimeIntervalLE
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeLE(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalLE)

        end function

        ! overloaded (>=) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalGE(timeinterval1, timeinterval2)
            logical :: ESMF_TimeIntervalGE
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeGE(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalGE)

        end function

		! overloaded (+) operator interface function maps to
		!   ESMF_Time base class
		function ESMF_TimeIntervalSum(timeinterval1, timeinterval2)
			type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum
			type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

			! call ESMF_Time base class function
			call c_ESMF_TimeSum(timeinterval1%time, timeinterval2%time, &
								ESMF_TimeIntervalSum%time)

		end function

        ! overloaded (-) operator interface function maps to
        !   ESMF_Time base class
        function ESMF_TimeIntervalDiff(timeinterval1, timeinterval2)
            type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            ! call ESMF_Time base class function
            call c_ESMF_TimeDiff(timeinterval1%time, timeinterval2%time, &
                                 ESMF_TimeIntervalDiff%time)

        end function

        !
        ! wrappers for ESMF_TimeInterval overloaded operators
        !

        ! overloaded (/) operator, timeinterval input, fraction output
        function ESMF_TimeIntervalQuot(timeinterval1, timeinterval2)
            type(ESMF_Fraction) :: ESMF_TimeIntervalQuot
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

            call c_ESMF_TimeIntervalQuot(timeinterval1, timeinterval2, &
                                         ESMF_TimeIntervalQuot)

        end function

        ! overloaded (/) operator, integer input, timeinterval output
        function ESMF_TimeIntervalQuotI(timeinterval, divisor)
            type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI
            type(ESMF_TimeInterval), intent(in) :: timeinterval
            integer, intent(in) :: divisor

            call c_ESMF_TimeIntervalQuotI(timeinterval, divisor, &
                                          ESMF_TimeIntervalQuotI)

        end function

        ! overloaded (*) operator, integer input, timeinterval output
        function ESMF_TimeIntervalProdI(timeinterval, multiplier)
            type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdI
            type(ESMF_TimeInterval), intent(in) :: timeinterval
            integer, intent(in) :: multiplier

            call c_ESMF_TimeIntervalProdI(timeinterval, multiplier, &
                                          ESMF_TimeIntervalProdI)

        end function

        ! overloaded (*) operator, fraction input, timeinterval output
        function ESMF_TimeIntervalProdF(timeinterval, multiplier)
            type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdF
            type(ESMF_TimeInterval), intent(in) :: timeinterval
            type(ESMF_Fraction), intent(in) :: multiplier

            call c_ESMF_TimeIntervalProdF(timeinterval, multiplier, &
                                          ESMF_TimeIntervalProdF)

        end function

        ! overloaded (*) operator, real input, timeinterval output
        function ESMF_TimeIntervalProdR(timeinterval, multiplier)
            type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdR
            type(ESMF_TimeInterval), intent(in) :: timeinterval
            real, intent(in) :: multiplier

            call c_ESMF_TimeIntervalProdR(timeinterval, multiplier, &
                                          ESMF_TimeIntervalProdR)

        end function

        !
        ! shortcut routines for common get/set groupings
        !

        subroutine ESMF_TimeIntvGet_S_nd(this, S, Sn, Sd, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(out) :: S
            integer(int32), intent(out) :: Sn, Sd
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntvGet_S_nd(this, S, Sn, Sd, rc)

        end subroutine

        subroutine ESMF_TimeIntvSet_S_nd(this, S, Sn, Sd, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int64), intent(in) :: S
            integer(int32), intent(in) :: Sn, Sd
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntvSet_S_nd(this, S, Sn, Sd, rc)

        end subroutine

        subroutine ESMF_TimeIntvGet_D_S(this, D, S, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int32), intent(out) :: D
            integer, intent(out) :: S
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntvGet_D_S(this, D, S, rc)

        end subroutine

        subroutine ESMF_TimeIntvSet_D_S(this, D, S, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int32), intent(in) :: D
            integer, intent(in) :: S
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntvSet_D_S(this, D, S, rc)

        end subroutine

        subroutine ESMF_TimeIntvGet_D_H_M_S_MS(this, D, H, M, S, MS, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int32), intent(out) :: D
            integer, intent(out) :: H, M, S, MS
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntvGet_D_H_M_S_MS(this, D, H, M, S, MS, rc)

        end subroutine

        subroutine ESMF_TimeIntvSet_D_H_M_S_MS(this, D, H, M, S, MS, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer(int32), intent(in) :: D
            integer, intent(in) :: H, M, S, MS
            integer, intent(in), optional :: rc
    
            call c_ESMF_TimeIntvSet_D_H_M_S_MS(this, D, H, M, S, MS, rc)

        end subroutine

        subroutine ESMF_TimeIntervalGetString(this, Ts, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            character, dimension(40), intent(out) :: Ts
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntervalGetString(this, Ts, rc)

        end subroutine

        subroutine ESMF_TimeIntervalGetAbsValue(this, rc)
            type(ESMF_TimeInterval), intent(inout) :: this
            integer, intent(out), optional :: rc
    
            call c_ESMF_TimeIntervalGetAbsValue(this, rc)

        end subroutine

    end module ESMF_TimeIntervalMod
