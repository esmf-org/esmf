! $Id: ESMF_TimeInterval.F90,v 1.15 2003/04/23 18:46:59 eschwab Exp $
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
!     ESMF TimeInterval Module
      module ESMF_TimeIntervalMod
!
!==============================================================================
!
! This file contains the TimeInterval class definition and all TimeInterval
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>
!
!===============================================================================
!BOP
! !MODULE: ESMF_TimeIntervalMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ implementaion of class ESMC\_TimeInterval
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod         

      ! inherit from base time class
      use ESMF_BaseTimeMod, only : ESMF_BaseTime 

      ! associated derived types
      use ESMF_FractionMod, only : ESMF_Fraction
      use ESMF_CalendarMod, only : ESMF_Calendar

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_TimeInterval
!
!     ! F90 class type to match C++ TimeInterval class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type ESMF_TimeInterval
      sequence                           ! match C++ storage order
      private                            !   (members opaque on F90 side)
        ! keep dimensions even to avoid compiler alignment warnings
        integer(ESMF_IKIND_I8) :: memoryBlock1
        integer, dimension(6)  :: memoryBlock2
      end type

!      ! Equivalent sequence and kind to C++:
!
!      type ESMF_TimeInterval
!      sequence                           ! match C++ storage order
!      private                            !   (members opaque on F90 side)
!        type(ESMF_BaseTime) :: basetime            ! inherit base class
!        type(ESMF_Calendar), pointer :: calendar   ! optional calendar for 
!                                                   !   calendar intervals
!      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TimeInterval
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_TimeIntervalInit
      public ESMF_TimeIntervalGet
      public ESMF_TimeIntervalSet
      public ESMF_TimeIntervalGetCalendar
      public ESMF_TimeIntervalSetCalendar
      public ESMF_TimeIntervalIsSameCal
      public ESMF_TimeIntervalGetString
      public ESMF_TimeIntervalGetAbsValue
      public ESMF_TimeIntervalGetNegAbsVal
      public ESMF_TimeIntervalFQuot

! Required inherited and overridden ESMF_Base class methods

      public ESMF_TimeIntervalRead
      public ESMF_TimeIntervalWrite
      public ESMF_TimeIntervalValidate
      public ESMF_TimeIntervalPrint

! !PRIVATE MEMBER FUNCTIONS:
 
! overloaded operator functions
 
      public operator(/)
      private ESMF_TimeIntervalRQuot
      private ESMF_TimeIntervalQuotI
      private ESMF_TimeIntervalQuotR

      public operator(*)
      private ESMF_TimeIntervalProdI
      private ESMF_TimeIntervalProdF
      private ESMF_TimeIntervalProdR

! Inherited and overloaded from ESMF_BaseTime

      public operator(+)
      private ESMF_TimeIntervalSum

      public operator(-)
      private ESMF_TimeIntervalDiff

      public operator(.EQ.)
      private ESMF_TimeIntervalEQ

      public operator(.NE.)
      private ESMF_TimeIntervalNE

      public operator(.LT.)
      private ESMF_TimeIntervalLT

      public operator(.GT.)
      private ESMF_TimeIntervalGT

      public operator(.LE.)
      private ESMF_TimeIntervalLE

      public operator(.GE.)
      private ESMF_TimeIntervalGE
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_TimeInterval.F90,v 1.15 2003/04/23 18:46:59 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(/)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalRQuot
      module procedure ESMF_TimeIntervalQuotI
      module procedure ESMF_TimeIntervalQuotR

! !DESCRIPTION:
!     This interface overloads the / operator for the {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(*)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdI
      module procedure ESMF_TimeIntervalProdF
      module procedure ESMF_TimeIntervalProdR

! !DESCRIPTION:
!     This interface overloads the * operator for the {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalSum

! !DESCRIPTION:
!     This interface overloads the + operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalDiff

! !DESCRIPTION:
!     This interface overloads the - operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalEQ

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.NE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalNE

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalLT

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGT

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalLE

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGE

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the
!     {\tt TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Init methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalInit - Initialize via user-specified unit set

! !INTERFACE:
      subroutine ESMF_TimeIntervalInit(timeinterval, YY, MO, D, H, M, S, &
                                       MS, US, NS, &
                                       d_, h_, m_, s_, ms_, us_, ns_, &
                                       Sn, Sd, cal, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeinterval
      integer(ESMF_IKIND_I8), intent(in), optional :: YY
      integer(ESMF_IKIND_I8), intent(in), optional :: MO
      integer(ESMF_IKIND_I8), intent(in), optional :: D
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer, intent(in), optional :: S
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      type(ESMF_Calendar), intent(in), optional :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_TimeInterval} with set of user-specified units
!     via F90 optional arguments
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer number of interval years (64-bit)
!     \item[{[MO]}]
!          Integer number of interval months (64-bit)
!     \item[{[D]}]
!          Integer number of interval days (64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[cal]}]
!          Optional associated calendar for calendar intervals
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      ! use optional args for any subset
      call c_ESMC_TimeIntervalInit(timeinterval, YY, MO, D, H, M, S, &
                                   MS, US, NS, &
                                   d_, h_, m_, s_, ms_, us_, ns_, &
                                   Sn, Sd, cal, rc)

      end subroutine ESMF_TimeIntervalInit

!------------------------------------------------------------------------------
!
! This section includes the TimeInterval Get and Set methods.
!
!------------------------------------------------------------------------------
!
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeIntervalGet(timeinterval, YY, MO, D, H, M, S, MS, &
                                      US, NS, d_, h_, m_, s_, ms_, us_, ns_, &
                                      Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer(ESMF_IKIND_I8), intent(out), optional :: YY
      integer(ESMF_IKIND_I8), intent(out), optional :: MO
      integer(ESMF_IKIND_I8), intent(out), optional :: D
      integer, intent(out), optional :: H
      integer, intent(out), optional :: M
      integer, intent(out), optional :: S
      integer, intent(out), optional :: MS
      integer, intent(out), optional :: US
      integer, intent(out), optional :: NS
      double precision, intent(out), optional :: d_
      double precision, intent(out), optional :: h_
      double precision, intent(out), optional :: m_
      double precision, intent(out), optional :: s_
      double precision, intent(out), optional :: ms_
      double precision, intent(out), optional :: us_
      double precision, intent(out), optional :: ns_
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the value of the {\tt TimeInterval} in units specified by the user
!     via F90 optional arguments
!     
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query
!     \item[{[YY]}]
!          Integer years (64-bit)
!     \item[{[MO]}]
!          Integer months (64-bit)
!     \item[{[D]}]
!          Integer days (64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.1
!EOP

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGet(timeinterval, YY, MO, D, H, M, S, MS, US, &
                                  NS, d_, h_, m_, s_, ms_, us_, ns_, &
                                  Sn, Sd, rc)
    
      end subroutine ESMF_TimeIntervalGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Set value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeIntervalSet(timeinterval, YY, MO, D, H, M, S, MS, &
                                      US, NS, d_, h_, m_, s_, ms_, us_, ns_, &
                                      Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeinterval
      integer(ESMF_IKIND_I8), intent(in), optional :: YY
      integer(ESMF_IKIND_I8), intent(in), optional :: MO
      integer(ESMF_IKIND_I8), intent(in), optional :: D
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer, intent(in), optional :: S
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set the value of the {\tt TimeInterval} in units specified by the user
!     via F90 optional arguments
!     
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query
!     \item[{[YY]}]
!          Integer years (64-bit)
!     \item[{[MO]}]
!          Integer months (64-bit)
!     \item[{D]}]
!          Integer days (64-bit)
!     \item[{H]}]
!          Integer hours
!     \item[{M]}]
!          Integer minutes
!     \item[{S]}]
!          Integer seconds
!     \item[{MS]}]
!          Integer milliseconds
!     \item[{US]}]
!          Integer microseconds
!     \item[{NS]}]
!          Integer nanoseconds
!     \item[{d\_]}]
!          Double precision days
!     \item[{h\_]}]
!          Double precision hours
!     \item[{m\_]}]
!          Double precision minutes
!     \item[{s\_]}]
!          Double precision seconds
!     \item[{ms\_]}]
!          Double precision milliseconds
!     \item[{us\_]}]
!          Double precision microseconds
!     \item[{ns\_]}]
!          Double precision nanoseconds
!     \item[{Sn]}]
!          Integer fractional seconds - numerator
!     \item[{Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
      ! use optional args for any subset
       call c_ESMC_TimeIntervalSet(timeinterval, YY, MO, D, H, M, S, MS, US, &
                                   NS, d_, h_, m_, s_, ms_, us_, ns_, &
                                   Sn, Sd, rc)
    
      end subroutine ESMF_TimeIntervalSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGetCalendar - Get associated calendar

! !INTERFACE:
      subroutine ESMF_TimeIntervalGetCalendar(timeinterval, cal, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      type(ESMF_Calendar), intent(out) :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     For Calendar intervals, get the associated calendar
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query
!     \item[{cal}]
!          Associated calendar for calendar intervals
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

!      call c_ESMC_TimeIntervalGetCalendar(timeinterval, cal, rc)
    
      end subroutine ESMF_TimeIntervalGetCalendar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSetCalendar - Set associated calendar

! !INTERFACE:
      subroutine ESMF_TimeIntervalSetCalendar(timeinterval, cal, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeinterval
      type(ESMF_Calendar), intent(in) :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     For Calendar intervals, set the associated calendar
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to set
!     \item[{cal}]
!          Associated calendar for calendar intervals
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

!      call c_ESMC_TimeIntervalSetCalendar(timeinterval, cal, rc)
    
      end subroutine ESMF_TimeIntervalSetCalendar

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalIsSameCal - Compare calendars of two time intervals

! !INTERFACE:
      function ESMF_TimeIntervalIsSameCal(timeinterval1, timeinterval2, rc)

! !RETURN VALUE:
      logical :: ESMF_TimeIntervalIsSameCal

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if both {\tt TimeInterval}'s calendars are the same,
!     false otherwise
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The first object instance to compare
!     \item[timeinterval2]
!          The second object instance to compare
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

       ESMF_TimeIntervalIsSameCal = .true.
!      call c_ESMC_TimeIntervalIsSameCal(timeinterval1, timeinterval2, &
!                                             ESMF_TimeIntervalIsSameCal, rc)
    
      end function ESMF_TimeIntervalIsSameCal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGetString - Get time interval value in string format

! !INTERFACE:
      subroutine ESMF_TimeIntervalGetString(timeinterval, Ts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      character, dimension(ESMF_MAXSTR), intent(out) :: Ts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Convert {\tt TimeInterval}'s value into string format
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to convert
!     \item[Ts]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.9
!EOP

!      call c_ESMC_TimeIntervalGetString(timeinterval, Ts, rc)

      end subroutine ESMF_TimeIntervalGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGetAbsValue - Get the absolute value of a time interval

! !INTERFACE:
      subroutine ESMF_TimeIntervalGetAbsValue(timeinterval, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout) :: timeinterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt TimeInterval}'s absolute value.  Return in the given
!     {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance with which to take the absolute value.
!          Absolute value returned as new value of timeinterval.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
    
!      call c_ESMC_TimeIntervalGetAbsValue(timeinterval, rc)

      end subroutine ESMF_TimeIntervalGetAbsValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGetNegAbsVal - Get the negative absolute value of a time interval

! !INTERFACE:
      subroutine ESMF_TimeIntervalGetNegAbsVal(timeinterval, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout) :: timeinterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt TimeInterval}'s negative absolute value.  Return in the given
!     {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance with which to take the negative absolute value.
!          Negative absolute value returned as new value of timeinterval.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
    
!      call c_ESMC_TimeIntervalGetNegAbsVal(timeinterval, rc)

      end subroutine ESMF_TimeIntervalGetNegAbsVal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalFQuot - Divide two time intervals, return fraction result

! !INTERFACE:
      function ESMF_TimeIntervalFQuot(timeinterval1, timeinterval2, rc)

! !RETURN VALUE:
      type(ESMF_Fraction) :: ESMF_TimeIntervalFQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a fraction quotient.
!     Note:  this method is not overloaded, since it has the same arguments
!     as ESMF\_TimeIntervalRQuot, and also will be less frequently used.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP

       type(ESMF_Fraction) :: temp
       ESMF_TimeIntervalFQuot = temp
!      call c_ESMC_TimeIntervalFQuot(timeinterval1, timeinterval2, &
!                                    ESMF_TimeIntervalFQuot, rc)

      end function ESMF_TimeIntervalFQuot

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since F90 forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRQuot - Divide two time intervals, return double precision result

! !INTERFACE:
      function ESMF_TimeIntervalRQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      double precision :: ESMF_TimeIntervalRQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a double precision
!     number quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP

       ESMF_TimeIntervalRQuot = 0.0
!      call c_ESMC_TimeIntervalRQuot(timeinterval1, timeinterval2, &
!                                    ESMF_TimeIntervalRQuot)

      end function ESMF_TimeIntervalRQuot

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide time interval by an integer, return time interval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotI(timeinterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer, intent(in) :: divisor

! !DESCRIPTION:
!     Divides a {\tt TimeInterval} by an integer divisor, returns quotient as a
!     {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend
!     \item[divisor]
!          Integer divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!EOP

       type(ESMF_TimeInterval) :: temp
       ESMF_TimeIntervalQuotI = temp
!      call c_ESMC_TimeIntervalQuotI(timeinterval, divisor, &
!                                    ESMF_TimeIntervalQuotI)

      end function ESMF_TimeIntervalQuotI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotR - Divide time interval by an double precision, return time interval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotR(timeinterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      double precision, intent(in) :: divisor

! !DESCRIPTION:
!     Divides a {\tt TimeInterval} by an double precision divisor, returns
!     quotient as a {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend
!     \item[divisor]
!          Double precision divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!EOP

       type(ESMF_TimeInterval) :: temp
       ESMF_TimeIntervalQuotR = temp
!      call c_ESMC_TimeIntervalQuotR(timeinterval, divisor, &
!                                    ESMF_TimeIntervalQuotR)

      end function ESMF_TimeIntervalQuotR

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdI - Multiply a time interval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdI(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer, intent(in) :: multiplier

! !DESCRIPTION:
!     Multiply a {\tt TimeInterval} by an integer, return product as a
!     {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Integer multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

       type(ESMF_TimeInterval) :: temp
       ESMF_TimeIntervalProdI = temp
!      call c_ESMC_TimeIntervalProdI(timeinterval, multiplier, &
!                                    ESMF_TimeIntervalProdI)

      end function ESMF_TimeIntervalProdI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalProdF - Multiply a time interval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdF(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdF

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      type(ESMF_Fraction), intent(in) :: multiplier

! !DESCRIPTION:
!     Multiply a {\tt TimeInterval} by a fraction, return product as a
!     {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Fraction multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

       type(ESMF_TimeInterval) :: temp
       ESMF_TimeIntervalProdF = temp
!      call c_ESMC_TimeIntervalProdF(timeinterval, multiplier, &
!                                    ESMF_TimeIntervalProdF)

      end function ESMF_TimeIntervalProdF
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdR - Multiply a time interval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdR(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      double precision, intent(in) :: multiplier

! !DESCRIPTION:
!     Multiply a {\tt TimeInterval} by a double precision number,
!     return product as a {\tt TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Double precision multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

       type(ESMF_TimeInterval) :: temp
       ESMF_TimeIntervalProdR = temp
!      call c_ESMC_TimeIntervalProdR(timeinterval, multiplier, &
!                                    ESMF_TimeIntervalProdR)

      end function ESMF_TimeIntervalProdR

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMF_BaseTime class overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum - Add two time intervals together

! !INTERFACE:
      function ESMF_TimeIntervalSum(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Add two {\tt TimeIntervals}, return sum as a {\tt TimeInterval}.
!     Maps overloaded (+) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The augend 
!     \item[timeinterval2]
!          The addend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, 
!                 TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeSum(timeinterval1, timeinterval2, &
                              ESMF_TimeIntervalSum)

      end function ESMF_TimeIntervalSum

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one time interval from another
   
! !INTERFACE:
      function ESMF_TimeIntervalDiff(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Subtract timeinterval2 from timeinterval1, return remainder as a 
!     {\tt TimeInterval}.
!     Map overloaded (-) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The minuend 
!     \item[timeinterval2]
!          The subtrahend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeDiff(timeinterval1, timeinterval2, &
                               ESMF_TimeIntervalDiff)

      end function ESMF_TimeIntervalDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalEQ - Compare two time intervals for equality

! !INTERFACE:
      function ESMF_TimeIntervalEQ(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalEQ

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

!DESCRIPTION:
!     Return true if both given time intervals are equal, false otherwise.
!     Maps overloaded (==) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeEQ(timeinterval1, timeinterval2, ESMF_TimeIntervalEQ)

      end function ESMF_TimeIntervalEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNE - Compare two time intervals for inequality

! !INTERFACE:
      function ESMF_TimeIntervalNE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalNE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if both given time intervals are not equal, false otherwise.
!     Maps overloaded (/=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(timeinterval1, timeinterval2, ESMF_TimeIntervalNE)

      end function ESMF_TimeIntervalNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLT - Time interval 1 less than time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is less than second time interval,
!     false otherwise.
!     Maps overloaded (<) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(timeinterval1, timeinterval2, ESMF_TimeIntervalLT)

      end function ESMF_TimeIntervalLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGT - Time interval 1 greater than time interval 2?

! !INTERFACE:
      function ESMF_TimeIntervalGT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than second time interval,
!     false otherwise.
!     Maps overloaded (>) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(timeinterval1, timeinterval2, ESMF_TimeIntervalGT)

      end function ESMF_TimeIntervalGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLE - Time interval 1 less than or equal to time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLE(timeinterval1, timeinterval2)

! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is less than or equal to second time
!     interval, false otherwise.
!     Maps overloaded (<=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(timeinterval1, timeinterval2, ESMF_TimeIntervalLE)

      end function ESMF_TimeIntervalLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGE - Time interval 1 greater than or equal to time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalGE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than or equal to second
!     time interval, false otherwise.
!     Maps overloaded (>=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(timeinterval1, timeinterval2, ESMF_TimeIntervalGE)

      end function ESMF_TimeIntervalGE

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRead - Restore a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalRead(timeinterval, S, Sn, Sd, cal, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeinterval
      integer(ESMF_IKIND_I8), intent(in) :: S
      integer, intent(in) :: Sn
      integer, intent(in) :: Sd
      type(ESMF_Calendar), intent(in) :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt TimeInterval}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to restore
!     \item[S]
!          64-bit integer seconds
!     \item[Sn]
!          Integer fractional seconds - numerator
!     \item[Sd]
!          Integer fractional seconds - denominator
!     \item[cal]
!          Associated {\tt Calendar}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeIntervalRead(timeinterval, S, Sn, Sd, cal, rc)

      end subroutine ESMF_TimeIntervalRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWrite - Save a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalWrite(timeinterval, S, Sn, Sd, cal, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer(ESMF_IKIND_I8), intent(out) :: S
      integer, intent(out) :: Sn
      integer, intent(out) :: Sd
      type(ESMF_Calendar), intent(out) :: cal
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on a {\tt TimeInterval}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to save
!     \item[S]
!          64-bit integer seconds
!     \item[Sn]
!          Integer fractional seconds - numerator
!     \item[Sd]
!          Integer fractional seconds - denominator
!     \item[cal]
!          Associated {\tt Calendar}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeIntervalWrite(timeinterval, S, Sn, Sd, cal, rc)

      end subroutine ESMF_TimeIntervalWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalValidate - Validate a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalValidate(timeinterval, opts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt TimeInterval}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
      call c_ESMC_TimeIntervalValidate(timeinterval, opts, rc)

      end subroutine ESMF_TimeIntervalValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print out a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalPrint(timeinterval, opts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt TimeInterval}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
      call c_ESMC_TimeIntervalPrint(timeinterval, opts, rc)

      end subroutine ESMF_TimeIntervalPrint

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalMod
