! $Id: ESMF_TimeInterval.F90,v 1.2 2002/10/28 18:13:55 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Time Interval Module
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

    module ESMF_TimeIntervalMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TimeIntervalMod
! !DESCRIPTION:
!
!
!
!
!
!
! !USES:
        use ESMF_TypesMod
        use ESMF_FractionMod
        use ESMF_TimeMod    ! inherit from base class
!
!------------------------------------------------------------------------------

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
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: '
!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------

    contains

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalInit1

! !INTERFACE:
        subroutine ESMF_TimeIntervalInit1(this, S, Sn, Sd, rc)


! !ARGUMENTS:
       type(ESMF_TimeInterval), intent(inout) :: this
       integer(int64), intent(in) :: S
       integer(int32), intent(in) :: Sn, Sd
       integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_TimeIntervalInit1(this, S, Sn, Sd, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalInit2

! !INTERFACE:
        subroutine ESMF_TimeIntervalInit2(this,D, H, M, S, MS, US, NS, Sn, Sd, &
                                          d_, h_, m_, s_, ms_, us_, ns_, rc)


! !ARGUMENTS:
       type(ESMF_TimeInterval), intent(inout) :: this
       integer, intent(in), optional :: H, M, MS
       integer(int64), intent(in), optional :: S
       integer(int32), intent(in), optional :: D, US, NS, Sn, Sd
       real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
       integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            ! use optional args for any subset
            call c_ESMF_TimeIntervalInit2(this,D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)

        end subroutine
!------------------------------------------------------------------------------
        !
        ! generic get/set routines which use F90 optional arguments
        !
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet

! !INTERFACE:
        subroutine ESMF_TimeIntervalGet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer, intent(out), optional :: H, M, MS
        integer(int64), intent(out), optional :: S
        integer(int32), intent(out), optional :: D, US, NS, Sn, Sd
        real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            ! use optional args for any subset
            call c_ESMF_TimeIntervalGet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet

! !INTERFACE:
        subroutine ESMF_TimeIntervalSet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer, intent(in), optional :: H, M, MS
        integer(int64), intent(in), optional :: S
        integer(int32), intent(in), optional :: D, US, NS, Sn, Sd
        real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            ! use optional args for any subset
            call c_ESMF_TimeIntervalSet(this, D, H, M, S, MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, rc)
    
        end subroutine

!------------------------------------------------------------------------------
        !
        ! wrappers for "inherited" ESMF_Time base class routines
        !
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet_S

! !INTERFACE:
        subroutine ESMF_TimeIntervalGet_S(this, S, rc)


! !ARGUMENTS:
       type(ESMF_TimeInterval), intent(inout) :: this
       integer(int64), intent(out) :: S
       integer, intent(out), optional :: rc

! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_S(this%time, S, rc)

        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet_S

! !INTERFACE:
        subroutine ESMF_TimeIntervalSet_S(this, S, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(in) :: S
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_S(this%time, S, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet_s_

! !INTERFACE:
        subroutine ESMF_TimeIntervalGet_s_(this, s, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        real, intent(out) :: s
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_s(this%time, s, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSet_s_

! !INTERFACE:
        subroutine ESMF_TimeIntervalSet_s_(this, s, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        real, intent(in) :: s
       integer, intent(out), optional :: rc



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_s(this%time, s, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGet_h

! !INTERFACE:
        subroutine ESMF_TimeIntervalGet_h(this, h, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        real, intent(out) :: h
        integer, intent(out), optional :: rc
    


! !DESCRIPTION:
!     
!  
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_h(this%time, h, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSet_h

! !INTERFACE:
        subroutine ESMF_TimeIntervalSet_h(this, h, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        real, intent(in) :: h
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!  
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_h(this%time, h, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRead_S

! !INTERFACE:
        subroutine ESMF_TimeIntervalRead_S(this, S, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(out) :: S
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!  
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            print *, "ESMF_TimeIntervalRead_S entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_S(this%time, S, rc)

        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWrite_S

! !INTERFACE:
        subroutine ESMF_TimeIntervalWrite_S(this, S, rc)



! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(in) :: S
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!  
!  
!EOP
! !REQUIREMENTS:  AAAn.n.n


    
            print *, "ESMF_TimeIntervalWrite_S entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_S(this%time, S, rc)

        end subroutine
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRead_Sn

! !INTERFACE:
        subroutine ESMF_TimeIntervalRead_Sn(this, Sn, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int32), intent(out) :: Sn
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            print *, "ESMF_TimeIntervalRead_Sn entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sn(this%time, Sn, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWrite_Sn

! !INTERFACE:
        subroutine ESMF_TimeIntervalWrite_Sn(this, Sn, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(in) :: Sn
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            print *, "ESMF_TimeIntervalWrite_Sn entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sn(this%time, Sn, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRead_Sd

! !INTERFACE:
        subroutine ESMF_TimeIntervalRead_Sd(this, Sd, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int32), intent(out) :: Sd
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


    
            print *, "ESMF_TimeIntervalRead_Sd entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sd(this%time, Sd, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWrite_Sd

! !INTERFACE:
        subroutine ESMF_TimeIntervalWrite_Sd(this, Sd, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(in) :: Sd
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            print *, "ESMF_TimeIntervalWrite_Sd entered"

            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sd(this%time, Sd, rc)

        end subroutine


!------------------------------------------------------------------------------
        !
        !  wrappers for ESMF_Time base class overloaded operators
        !

        ! overloaded (==) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalEQ

! !INTERFACE:
        function ESMF_TimeIntervalEQ(timeinterval1, timeinterval2)


!
! !RETURN VALUE:
       logical :: ESMF_TimeIntervalEQ

! !ARGUMENTS:
            type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2


!DESCRIPTION:
!     
!
!
!
! !REQUIREMENTS:  AAAn.n.n



            ! call ESMF_Time base class function
            call c_ESMF_TimeEQ(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalEQ)

        end function

!------------------------------------------------------------------------------
        ! overloaded (/=) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNE

! !INTERFACE:
        function ESMF_TimeIntervalNE(timeinterval1, timeinterval2)


!
! !RETURN VALUE:
        logical :: ESMF_TimeIntervalNE


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            ! call ESMF_Time base class function
            call c_ESMF_TimeNE(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalNE)

        end function

!------------------------------------------------------------------------------
        ! overloaded (<) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLT

! !INTERFACE:
        function ESMF_TimeIntervalLT(timeinterval1, timeinterval2)


!
! !RETURN VALUE:
        logical :: ESMF_TimeIntervalLT

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            ! call ESMF_Time base class function
            call c_ESMF_TimeLT(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalLT)

        end function

!------------------------------------------------------------------------------
        ! overloaded (>) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGT

! !INTERFACE:
        function ESMF_TimeIntervalGT(timeinterval1, timeinterval2)


!
! !RETURN VALUE:
        logical :: ESMF_TimeIntervalGT


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n



            ! call ESMF_Time base class function
            call c_ESMF_TimeGT(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalGT)

        end function

!------------------------------------------------------------------------------
        ! overloaded (<=) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLE

! !INTERFACE:
        function ESMF_TimeIntervalLE(timeinterval1, timeinterval2)

!
! !RETURN VALUE:
        logical :: ESMF_TimeIntervalLE

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n




            ! call ESMF_Time base class function
            call c_ESMF_TimeLE(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalLE)

        end function
!------------------------------------------------------------------------------
        ! overloaded (>=) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLE

! !INTERFACE:
        function ESMF_TimeIntervalGE(timeinterval1, timeinterval2)

!
! !RETURN VALUE:
       logical :: ESMF_TimeIntervalGE

! !ARGUMENTS:
       type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            ! call ESMF_Time base class function
            call c_ESMF_TimeGE(timeinterval1%time, timeinterval2%time, &
                               ESMF_TimeIntervalGE)

        end function

!------------------------------------------------------------------------------
		! overloaded (+) operator interface function maps to
		!   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum

! !INTERFACE:
	function ESMF_TimeIntervalSum(timeinterval1, timeinterval2)

!
! !RETURN VALUE:
	type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum


! !ARGUMENTS:
	type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


		! call ESMF_Time base class function
		call c_ESMF_TimeSum(timeinterval1%time, timeinterval2%time, &
								ESMF_TimeIntervalSum%time)

	end function

!------------------------------------------------------------------------------
        ! overloaded (-) operator interface function maps to
        !   ESMF_Time base class
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum
   
! !INTERFACE:
        function ESMF_TimeIntervalDiff(timeinterval1, timeinterval2)

! !RETURN VALUE:
        type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff

! !ARGUMENTS: 
        type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            ! call ESMF_Time base class function
            call c_ESMF_TimeDiff(timeinterval1%time, timeinterval2%time, &
                                 ESMF_TimeIntervalDiff%time)

        end function

!------------------------------------------------------------------------------
        !
        ! wrappers for ESMF_TimeInterval overloaded operators
        !

        ! overloaded (/) operator, timeinterval input, fraction output
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuot

! !INTERFACE:
        function ESMF_TimeIntervalQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
        type(ESMF_Fraction) :: ESMF_TimeIntervalQuot


! !ARGUMENTS: 
         type(ESMF_TimeInterval), intent(in) :: timeinterval1, timeinterval2



! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_TimeIntervalQuot(timeinterval1, timeinterval2, &
                                         ESMF_TimeIntervalQuot)

        end function

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuot
        ! overloaded (/) operator, integer input, timeinterval output

! !INTERFACE:
        function ESMF_TimeIntervalQuotI(timeinterval, divisor)

! !RETURN VALUE:
        type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval
        integer, intent(in) :: divisor


! !DESCRIPTION:
!
! overloaded (/) operator, integer input, timeinterval output
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_TimeIntervalQuotI(timeinterval, divisor, &
                                          ESMF_TimeIntervalQuotI)

        end function

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdI

! !INTERFACE:
        function ESMF_TimeIntervalProdI(timeinterval, multiplier)


! !RETURN VALUE:
        type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdI


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval
        integer, intent(in) :: multiplier

! !DESCRIPTION:
!
! overloaded (/) operator, integer input, timeinterval output
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_TimeIntervalProdI(timeinterval, multiplier, &
                                          ESMF_TimeIntervalProdI)

        end function

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalProdF

! !INTERFACE:
        function ESMF_TimeIntervalProdF(timeinterval, multiplier)

! !RETURN VALUE:
        type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdF

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval
        type(ESMF_Fraction), intent(in) :: multiplier

! !DESCRIPTION:
!
! overloaded (/) operator, integer input, timeinterval output
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_TimeIntervalProdF(timeinterval, multiplier, &
                                          ESMF_TimeIntervalProdF)

        end function
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdR

! !INTERFACE:
        function ESMF_TimeIntervalProdR(timeinterval, multiplier)

! !RETURN VALUE:
        type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdR

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(in) :: timeinterval
        real, intent(in) :: multiplier

! !DESCRIPTION:
!
! overloaded (*) operator, real input, timeinterval output
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_TimeIntervalProdR(timeinterval, multiplier, &
                                          ESMF_TimeIntervalProdR)

        end function

!------------------------------------------------------------------------------
        !
        ! shortcut routines for common get/set groupings
        !
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntvGet_S_nd

! !INTERFACE:
        subroutine ESMF_TimeIntvGet_S_nd(this, S, Sn, Sd, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(out) :: S
        integer(int32), intent(out) :: Sn, Sd
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
! 
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntvGet_S_nd(this, S, Sn, Sd, rc)

        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntvSet_S_nd

! !INTERFACE:
        subroutine ESMF_TimeIntvSet_S_nd(this, S, Sn, Sd, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int64), intent(in) :: S
        integer(int32), intent(in) :: Sn, Sd
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntvSet_S_nd(this, S, Sn, Sd, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntvGet_D_S

! !INTERFACE:
        subroutine ESMF_TimeIntvGet_D_S(this, D, S, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int32), intent(out) :: D
        integer, intent(out) :: S
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntvGet_D_S(this, D, S, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntvSet_D_S

! !INTERFACE:
        subroutine ESMF_TimeIntvSet_D_S(this, D, S, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int32), intent(in) :: D
        integer, intent(in) :: S
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntvSet_D_S(this, D, S, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntvGet_D_H_M_S_MS

! !INTERFACE:
        subroutine ESMF_TimeIntvGet_D_H_M_S_MS(this, D, H, M, S, MS, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int32), intent(out) :: D
        integer, intent(out) :: H, M, S, MS
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntvGet_D_H_M_S_MS(this, D, H, M, S, MS, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntvSet_D_H_M_S_MS

! !INTERFACE:
        subroutine ESMF_TimeIntvSet_D_H_M_S_MS(this, D, H, M, S, MS, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer(int32), intent(in) :: D
        integer, intent(in) :: H, M, S, MS
        integer, intent(in), optional :: rc


! !DESCRIPTION:
!
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntvSet_D_H_M_S_MS(this, D, H, M, S, MS, rc)

        end subroutine



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGetString

! !INTERFACE:
        subroutine ESMF_TimeIntervalGetString(this, Ts, rc)


! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        character, dimension(40), intent(out) :: Ts
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntervalGetString(this, Ts, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGetAbsValue

! !INTERFACE:
        subroutine ESMF_TimeIntervalGetAbsValue(this, rc)

! !ARGUMENTS:
        type(ESMF_TimeInterval), intent(inout) :: this
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!
!
!  
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_TimeIntervalGetAbsValue(this, rc)

        end subroutine
!EOP
!===============================================================================
    end module ESMF_TimeIntervalMod
