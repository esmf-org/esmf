! $Id: ESMF_TimeInstant.F90,v 1.3 2002/10/22 17:59:18 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF TimeInstant Module
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

    module ESMF_TimeInstantMod
!
!BOP
! !MODULE: ESMF_TimeInstantMod - F90 API to C++ ESMC_TimeInstant
!
! !DESCRIPTION:
!       Part of Time Manager F90 API wrapper of C++ implemenation
!
!       Defines F90 wrapper entry points for corresponding
!        C++ class ESMC\_TimeInstant implementation
!
! !USES:
        use ESMF_TimeMod       ! ESMF Time base class
        use ESMF_TimeIntvMod   ! ESMF Time Interval class
        implicit none
!------------------------------------------------------------------------------
            
! !PRIVATE TYPES:
        private

        type ESMF_TimeInstant
            private
            sequence                      ! match C++ storage order
                type(ESMF_Time) :: time   ! inherits from ESMF_Time base class
                integer :: calendar       ! associated calendar type
                integer :: timezone       ! local timezone
        end type

! !PUBLIC TYPES:
        public ESMF_TimeInstant
!
! !PUBLIC MEMBER FUNCTIONS:
        public ESMF_TimeInstInit                 ! shallow class
        public ESMF_TimeInstGet
        public ESMF_TimeInstSet
        public ESMF_TimeInstGet_D
        public ESMF_TimeInstSet_D
        public ESMF_TimeInstGet_d_
        public ESMF_TimeInstSet_d_
        public ESMF_TimeInstGet_s_
        public ESMF_TimeInstSet_s_
        public ESMF_TimeInstRead_S
        public ESMF_TimeInstWrite_S
        public ESMF_TimeInstRead_Sn
        public ESMF_TimeInstWrite_Sn
        public ESMF_TimeInstRead_Sd
        public ESMF_TimeInstWrite_Sd
        public ESMF_TimeInstGet_YR_MM_DD_S
        public ESMF_TimeInstSet_YR_MM_DD_S
        public ESMF_TimeInstGet_YR_MM_DD_H_M_S
        public ESMF_TimeInstSet_YR_MM_DD_H_M_S
        public ESMF_TimeInstGetYR_MM_DD_H_M_S_nd
        public ESMF_TimeInstSetYR_MM_DD_H_M_S_nd
        public ESMF_TimeInstGetCalendar
        public ESMF_TimeInstGetTimezone
        public ESMF_TimeInstGetString
        public ESMF_TimeInstGetDayOfYear
        public ESMF_TimeInstGetDayOfWeek
        public ESMF_TimeInstGetMidMonth
        public ESMF_TimeInstGetRealTime
        public ESMF_TimeInstEQ
        public ESMF_TimeInstNE
        public ESMF_TimeInstLT
        public ESMF_TimeInstGT
        public ESMF_TimeInstLE
        public ESMF_TimeInstGE
 		public ESMF_TimeInstInc
        public ESMF_TimeInstDec
        public ESMF_TimeInstDiff
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: '
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstInit - Generic interface to initialize a TimeInstant
!
! !INTERFACE:
		interface ESMF_TimeInstInit

! !PRIVATE MEMBER FUNCTIONS:
			module procedure ESMF_TimeInstInit1
			module procedure ESMF_TimeInstInit2

! !DESCRIPTION:
!      Provides a single entry point for multiple Init routines
!EOP
        end interface
!------------------------------------------------------------------------------

    contains
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstInit1 - Initialize a new TimeInstant

! !INTERFACE:
        subroutine ESMF_TimeInstInit1(this,S, Sn, Sd, calendar, timezone, rc)

! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this  ! this TimeInstant
        integer(INT64), intent(in) :: S                ! seconds
        integer(INT32), intent(in) :: Sn, Sd           ! fractional seconds
        integer, intent(in) :: calendar, timezone
        integer, intent(out), optional :: rc           ! return code
    
! !DESCRIPTION:
!     Initialize a TimeInstant with values S, Sn, Sd, calendar & timezone
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
!       invoke C to C++ entry point
        call c_ESMF_TimeInstantInit1(this,S, Sn, Sd, calendar, timezone, rc)
!
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstInit2 - Initialize a new TimeInstant

! !INTERFACE:

        subroutine ESMF_TimeInstInit2(this, YR, MM, DD, D, H, M, S, &
                                      MS, US, NS, Sn, Sd, &
                                      d_, h_, m_, s_, ms_, us_, ns_, &
                                      calendar, timezone, rc)
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this  ! this TimeInstant
        integer, intent(in), optional :: MM, DD, H, M, MS
        integer(INT64), intent(in), optional :: S
        integer(INT32), intent(in), optional :: YR, D, US, NS, Sn, Sd
        real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
        integer, intent(in) :: calendar, timezone
        integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Initialize a TimeInstant with date & time, calendar & timezone values
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
!       ! invoke C to C++ entry point
!       ! use optional args for any subset
        call c_ESMF_TimeInstInit2(this, YR, MM, DD, D, H, M, S, &
                                        MS, US, NS, Sn, Sd, &
                                        d_, h_, m_, s_, ms_, us_, ns_, &
                                        calendar, timezone, rc)
        end subroutine
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstGet - Get time instant

! !INTERFACE:
	subroutine ESMF_TimeInstGet(this, YR, MM, DD, D, H, M, S, &
                                    MS, US, NS, Sn, Sd, &
                                    d_, h_, m_, s_, ms_, us_, ns_, rc)
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer, intent(out), optional :: MM, DD, H, M, MS
        integer(INT64), intent(out), optional :: S
        integer(INT32), intent(out), optional :: YR, D, US, NS, Sn, Sd
        real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
        integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!
!	 generic get routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!	    ! invoke C to C++ entry point
!           ! use optional args for any subset
            call c_ESMF_TimeInstantGet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)


           end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstSet - Set time instant

! !INTERFACE:
	subroutine ESMF_TimeInstantSet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)

! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer, intent(in), optional :: MM, DD, H, M, MS
        integer(INT64), intent(in), optional :: S
        integer(INT32), intent(in), optional :: YR, D, US, NS, Sn, Sd
        real, intent(out), optional :: d_, h_, m_, s_, ms_, us_, ns_
        integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!
! 	generic set routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! use optional args for any subset
            call c_ESMF_TimeInstantSet(this, YR, MM, DD, D, H, M, S, &
                                       MS, US, NS, Sn, Sd, &
                                       d_, h_, m_, s_, ms_, us_, ns_, rc)
    
           end subroutine

!
! 	wrappers for "inherited" ESMF_Time base class routines
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstGet_D - Set time instant
    
! !INTERFACE:
	subroutine ESMF_TimeInstGet_D(this, D, rc)

! !ARGUMENTS:
       type(ESMF_TimeInstant), intent(inout) :: this
       integer(INT32), intent(out) :: D
       integer, intent(out), optional :: rc

! !DESCRIPTION:
!
! 	generic set routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_D(this%time, D, rc)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstSet_D - Set time instant

! !INTERFACE:
        subroutine ESMF_TimeInstSet_D(this, D, rc)
    
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT32), intent(in) :: D
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
! 	generic set routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_D(this%time, D, rc)

            end subroutine
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstGet_d_ - Get time instant

! !INTERFACE:
        subroutine ESMF_TimeInstGet_d_(this, d, rc)

! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        real, intent(out) :: d
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
! 	generic get routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_d_(this%time, d, rc)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstSet_d_ - Set time instant

! !INTERFACE:
        subroutine ESMF_TimeInstSet_d_(this, d, rc)
    
! !ARGUMENTS:
       type(ESMF_TimeInstant), intent(inout) :: this
       real, intent(in) :: d
       integer, intent(out), optional :: rc
! !DESCRIPTION:
!
! 	generic set routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_d_(this%time, d, rc)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstGet_s_ - Set time instant

! !INTERFACE:
        subroutine ESMF_TimeInstGet_s_(this, s, rc)
    

! !ARGUMENTS:
       type(ESMF_TimeInstant), intent(inout) :: this
       real, intent(out) :: s
       integer, intent(out), optional :: rc


! !DESCRIPTION:
!
! 	generic set routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeGet_s(this%time, s, rc)

            end subroutine
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstGet_s_ - Set time instant

! !INTERFACE:
        subroutine ESMF_TimeInstSet_s_(this, s, rc)
    
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        real, intent(in) :: s
        integer, intent(out), optional :: rc
! !DESCRIPTION:
! 
! 	generic set routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeSet_s(this%time, s, rc)

            end subroutine
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstRead_S - Read time instant

! !INTERFACE:
        subroutine ESMF_TimeInstRead_S(this, S)
    
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT64), intent(out) :: S


! !DESCRIPTION:
!
! 	generic read routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            print *, "ESMF_TimeTimeInstantRead_S entered"
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_S(this%time, S)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstWrite_S - Write time instant


! !INTERFACE:
        subroutine ESMF_TimeInstWrite_S(this, S)
    
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT64), intent(in) :: S

! !DESCRIPTION:
!
! 	generic write routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            print *, "ESMF_TimeInstWrite_S entered"
!            ! invoke C to C++ entry point
!            ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_S(this%time, S, rc)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstRead_Sn - Read Time instant


! !INTERFACE:
        subroutine ESMF_TimeInstRead_Sn(this, Sn)
    
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
         integer(INT32), intent(out) :: Sn

! !DESCRIPTION:
!
! 	generic read routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            print *, "ESMF_TimeInstRead_Sn entered"
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sn(this%time, Sn, rc)

        end subroutine
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstWrite_Sn - Write time instant


! !INTERFACE:
        subroutine ESMF_TimeInstWrite_Sn(this, Sn)

! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT64), intent(in) :: Sn
    
! !DESCRIPTION:
!
!	 generic write routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            print *, "ESMF_TimeInstWrite_Sn entered"
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sn(this%time, Sn, rc)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstRead_Sd - Read time instant


! !INTERFACE:
        subroutine ESMF_TimeInstRead_Sd(this, Sd)

! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT32), intent(out) :: Sd

! !DESCRIPTION:
!
! 	generic read routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            print *, "ESMF_TimeInstRead_Sd entered"
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeRead_Sd(this%time, Sd, rc)

            end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: subroutine ESMF_TimeInstWrite_Sd - Write time instant


! !INTERFACE:
        subroutine ESMF_TimeInstWrite_Sd(this, Sd)
    
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT64), intent(in) :: Sd

! !DESCRIPTION:
!
! 	generic write routine which use F90 optional arguments
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            print *, "ESMF_TimeInstWrite_Sd entered"
!           ! invoke C to C++ entry point
!           ! call ESMF_Time base class subroutine
            call c_ESMF_TimeWrite_Sd(this%time, Sd, rc)

            end subroutine

!
!  	wrappers for ESMF_Time base class overloaded operators
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstEQ - compare two TimeInstants for equality
!
! !INTERFACE:
        function ESMF_TimeInstEQ(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        logical :: ESMF_TimeInstEQ
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2
!
! !DESCRIPTION:
! 	overloaded (==) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3

!        ! invoke C to C++ entry point for ESMF_Time base class function
        call c_ESMF_TimeEQ(timeinstant1%time, timeinstant2%time, &
                           ESMF_TimeInstEQ)

        end function ESMF_TimeInstEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstNE - compare two TimeInstants for non-equality
!
! !INTERFACE:
        function ESMF_TimeInstNE(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        logical :: ESMF_TimeInstNE
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2
!
! !DESCRIPTION:
! 	overloaded (/=) operator interface function maps to
!   	ESMF_Time base class

!EOP
! !REQUIREMENTS:  TMG 2.4.3

!        ! invoke C to C++ entry point for ESMF_Time base class function
        call c_ESMF_TimeNE(timeinstant1%time, timeinstant2%time, &
                           ESMF_TimeInstantNE)

        end function ESMF_TimeInstNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstLT - 
!
! !INTERFACE:
        function ESMF_TimeInstLT(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        logical :: ESMF_TimeInstLT
!
! !ARGUMENTS:
	type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2
!
! !DESCRIPTION:
! 	overloaded (<) operator interface function maps to
!   	ESMF_Time base class

!EOP
! !REQUIREMENTS:  TMG 2.4.3

!           ! call ESMF_Time base class function
            call c_ESMF_TimeLT(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstLT)

            end function ESMF_TimeInstLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGT -
!
! !INTERFACE:
        function ESMF_TimeInstGT(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        logical :: ESMF_TimeInstGT
!
! !ARGUMENTS:
            type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2
!
! !DESCRIPTION:
!	overloaded (>) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            ! call ESMF_Time base class function
            call c_ESMF_TimeGT(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstGT)

            end function ESMF_TimeInstGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstLE -
!
! !INTERFACE:
        function ESMF_TimeInstLE(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        logical :: ESMF_TimeInstLE
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2
!
! !DESCRIPTION:
! 	overloaded (<=) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3

!           ! call ESMF_Time base class function
            call c_ESMF_TimeLE(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstLE)

            end function ESMF_TimeInstLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGE
!
! !INTERFACE:
        function ESMF_TimeInstGE(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        logical :: ESMF_TimeInstGE
!
! !ARGUMENTS:
         type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2
!
! !DESCRIPTION:
! 	overloaded (>=) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3

!           ! call ESMF_Time base class function
            call c_ESMF_TimeGE(timeinstant1%time, timeinstant2%time, &
                               ESMF_TimeInstGE)

            end function ESMF_TimeInstGE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstInc
!
! !INTERFACE:
	function ESMF_TimeInstInc(timeinstant, timeinterval)
!
! !RETURN VALUE:
	type(ESMF_TimeInstant) :: ESMF_TimeInstInc
!
! !ARGUMENTS:
	type(ESMF_TimeInst), intent(in) :: timeinstant
	type(ESMF_TimeIntv), intent(in) :: timeinterval
!
! !DESCRIPTION:
! 	overloaded (+) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3
		type(ESMF_Time) :: time
		integer(INT64) :: S
		integer(INT32) :: Sn, Sd

!		! get time from timeinterval (really need C++ "friend" feature ?? )
		call c_ESMF_TimeIntvGet_S_nd(timeinterval, S, Sn, Sd, rc)
		call c_ESMF_TimeInit(time, S, Sn, Sd, rc)

!		! call ESMF_Time base class function
		call c_ESMF_TimeSum(timeinstant%time, time, &
								ESMF_TimeInstInc%time)

		end function ESMF_TimeInstInc
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstDec
!
! !INTERFACE:
        function ESMF_TimeInstDec(timeinstant, timeinterval)
!
! !RETURN VALUE:
        type(ESMF_TimeInstant) :: ESMF_TimeInstDec

!
! !ARGUMENTS:
       type(ESMF_TimeInstant) :: ESMF_TimeInstDec
       type(ESMF_TimeInstant), intent(in) :: timeinstant
       type(ESMF_TimeIntv), intent(in) :: timeinterval
!
! !DESCRIPTION:
! 	overloaded (-) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3
            type(ESMF_Time) :: time
            integer(INT64) :: S
            integer(INT32) :: Sn, Sd

!           ! get time from timeinterval (really need C++ "friend" feature ?? )
            call c_ESMF_TimeIntvGet_S_nd(interval, S, Sn, Sd, rc)
            call c_ESMF_TimeInit(time, S, Sn, Sd, rc)

!           ! call ESMF_Time base class function
            call c_ESMF_TimeDiff(timeinstant%time, time, &
                                 ESMF_TimeInstDec%time)

            end function  ESMF_TimeInstDec

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeInstDiff
!
! !INTERFACE:
        function ESMF_TimeInstDiff(timeinstant1, timeinstant2)
!
! !RETURN VALUE:
        type(ESMF_TimeInstant) :: ESMF_TimeInstDiff
!
! !ARGUMENTS:
	type(ESMF_TimeInstant) :: ESMF_TimeInstDiff
        type(ESMF_TimeInstant), intent(in) :: timeinstant1, timeinstant2

!
! !DESCRIPTION:
! 	overloaded (-) operator interface function maps to
!   	ESMF_Time base class
!EOP
! !REQUIREMENTS:  TMG 2.4.3

!           ! call ESMF_Time base class function
            call c_ESMF_TimeDiff(timeinstant1%time, timeinstant2%time, &
                                 ESMF_TimeInstDiff%time)

            end function  ESMF_TimeInstDiff

!
! 	shortcut routines for common get/set groupings
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGet_YR_MM_DD_S
!
! !INTERFACE:
        subroutine ESMF_TimeInstGet_YR_MM_DD_S(this, YR, MM, DD, S, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT32), intent(out) :: YR, S
        integer, intent(out) :: MM, DD
        integer, intent(out), optional :: rc
    
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3


           call c_ESMF_TimeInstGet_YR_MM_DD_S(this, YR, MM, DD, S, rc)


           end subroutine ESMF_TimeInstGet_YR_MM_DD_S

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstSet_YR_MM_DD_S
!
! !INTERFACE:
        subroutine ESMF_TimeInstSet_YR_MM_DD_S(this, YR, MM, DD, S, rc)
!
! !RETURN VALUE:
         integer, intent(out), optional :: rc
!
! !ARGUMENTS:
         type(ESMF_TimeInstant), intent(inout) :: this
         integer(INT32), intent(in) :: YR, S
         integer, intent(in) :: MM, DD
         integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3


    
            call c_ESMF_TimeInstSet_YR_MM_DD_S(this, YR, MM, DD, S, rc)

            end subroutine ESMF_TimeInstSet_YR_MM_DD_S

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TmInstGet_YR_MM_DD_H_M_S
!
! !INTERFACE:
        subroutine ESMF_TmInstGet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT32), intent(out) :: YR
        integer, intent(out) :: MM, DD, H, M, S
        integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            call c_ESMF_TmInstGet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)

            end subroutine ESMF_TmInstGet_YR_MM_DD_H_M_S

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TmInstSet_YR_MM_DD_H_M_S
!
! !INTERFACE:
        subroutine ESMF_TmInstSet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)
!
! !RETURN VALUE:
         integer, intent(in), optional :: rc
!
! !ARGUMENTS:
         type(ESMF_TimeInstant), intent(inout) :: this
         integer(INT32), intent(in) :: YR
         integer, intent(in) :: MM, DD, H, M, S
         integer, intent(in), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            call c_ESMF_TmInstSet_YR_MM_DD_H_M_S(this, YR, MM, DD, &
                                                 H, M, S, rc)

            end subroutine ESMF_TmInstSet_YR_MM_DD_H_M_S

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TInsGetYR_MM_DD_H_M_S_nd
!
! !INTERFACE:
        subroutine ESMF_TInsGetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT32), intent(out) :: YR, Sn, Sd
        integer, intent(out) :: MM, DD, H, M, S
        integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3
    
            call c_ESMF_TInsGetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)

            end subroutine ESMF_TInsGetYR_MM_DD_H_M_S_nd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TInsSetYR_MM_DD_H_M_S_nd
!
! !INTERFACE:
        subroutine ESMF_TInsSetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)
!
! !RETURN VALUE:
        integer, intent(in), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer(INT32), intent(in) :: YR, Sn, Sd
        integer, intent(in) :: MM, DD, H, M, S
        integer, intent(in), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

    
            call c_ESMF_TInsSetYR_MM_DD_H_M_S_nd(this, YR, MM, DD, &
                                                 H, M, S, Sn, Sd, rc)

            end subroutine ESMF_TInsSetYR_MM_DD_H_M_S_nd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetCalendar
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetCalendar(this, Calendar, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer, intent(out) :: Calendar
        integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

    
            call c_ESMF_TimeInstGetCalendar(this, Calendar, rc)

            end subroutine ESMF_TimeInstGetCalendar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetTimezone
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetTimezone(this, Timezone, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer, intent(out) :: Timezone
        integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            call c_ESMF_TimeInstGetTimezone(this, Timezone, rc)

            end subroutine ESMF_TimeInstGetTimezone

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetString
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetString(this, Ts, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        character, dimension(40), intent(out) :: Ts
        integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3
    
            call c_ESMF_TimeInstGetString(this, Ts, rc)

            end subroutine ESMF_TimeInstGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetDayOfYear
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetDayOfYear(this, DayOfYear, rc)
!
! !RETURN VALUE:
          integer, intent(out), optional :: rc
!
! !ARGUMENTS:
            type(ESMF_TimeInstant), intent(inout) :: this
            integer, intent(out) :: DayOfYear
            integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            call c_ESMF_TimeInstGetDayOfYear(this, DayOfYear, rc)

            end subroutine ESMF_TimeInstGetDayOfYear

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetDayOfWeek
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetDayOfWeek(this, DayOfWeek, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
         type(ESMF_TimeInstant), intent(inout) :: this
         integer, intent(out) :: DayOfWeek
         integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3
    
            call c_ESMF_TimeInstGetDayOfWeek(this, DayOfWeek, rc)

            end subroutine ESMF_TimeInstGetDayOfWeek

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetMidMonth
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetMidMonth(this, MidMonth, rc)
!
! !RETURN VALUE:
        integer, intent(out), optional :: rc
!
! !ARGUMENTS:
        type(ESMF_TimeInstant), intent(inout) :: this
        integer, intent(out) :: MidMonth
        integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            call c_ESMF_TimeInstGetMidMonth(this, MidMonth, rc)

            end subroutine ESMF_TimeInstGetMidMonth

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInstGetRealTime
!
! !INTERFACE:
        subroutine ESMF_TimeInstGetRealTime(this, RealTime, rc)
!
! !RETURN VALUE:
       integer, intent(out), optional :: rc
!
! !ARGUMENTS:
       type(ESMF_TimeInstant), intent(inout) :: this
       integer, intent(out) :: RealTime
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  TMG 2.4.3

    
            call c_ESMF_TimeInstGetRealTime(this, RealTime, rc)

            end subroutine ESMF_TimeInstGetRealTime

!EOP
!===============================================================================
    end module ESMF_TimeInstantMod
