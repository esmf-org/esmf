! $Id: ESMF_TimeInterval.F90,v 1.40 2003/10/30 20:08:13 eschwab Exp $
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
!BOPI
! !MODULE: ESMF_TimeIntervalMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation.
!
! Defines F90 wrapper entry points for corresponding
! C++ implementaion of class {\tt ESMC\_TimeInterval}.
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod

      ! associated derived types
      use ESMF_FractionMod
      use ESMF_CalendarMod

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

!     ! Equivalent sequence and kind to C++:

      type ESMF_TimeInterval
      sequence                             ! match C++ storage order
      private                              !   (members opaque on F90 side)
        type(ESMF_BaseTime)   :: baseTime  ! inherit base class
        integer(ESMF_KIND_I8) :: yy        ! calendar interval number of years
        integer(ESMF_KIND_I8) :: mo        ! calendar interval number of months
        integer(ESMF_KIND_I8) :: d         ! calendar interval number of days
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TimeInterval
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_TimeIntervalSet
      public ESMF_TimeIntervalGet

      public ESMF_TimeIntervalAbsValue
      public ESMF_TimeIntervalNegAbsValue

! Required inherited and overridden ESMF_Base class methods

      public ESMF_TimeIntervalReadRestart
      public ESMF_TimeIntervalWriteRestart
      public ESMF_TimeIntervalValidate
      public ESMF_TimeIntervalPrint

! !PRIVATE MEMBER FUNCTIONS:
 
! overloaded operator functions
 
      public operator(.DIV.)
      private ESMF_TimeIntervalFQuot

      public MOD
      private ESMF_TimeIntervalRemainder

      public operator(/)
      private ESMF_TimeIntervalRQuot
      private ESMF_TimeIntervalQuotI
      private ESMF_TimeIntervalQuotR

      public operator(*)
      private ESMF_TimeIntervalProdTI
      private ESMF_TimeIntervalProdIT
      private ESMF_TimeIntervalProdTF
      private ESMF_TimeIntervalProdFT
      private ESMF_TimeIntervalProdTR
      private ESMF_TimeIntervalProdRT

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
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_TimeInterval.F90,v 1.40 2003/10/30 20:08:13 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(.DIV.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalFQuot

! !DESCRIPTION:
!     This interface defines a new .DIV. operator for the
!     {\tt ESMF\_TimeInterval} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface MOD

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalRemainder

! !DESCRIPTION:
!     This interface overloads the pre-defined MOD() function for the
!     {\tt ESMF\_TimeInterval} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(/)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalRQuot
      module procedure ESMF_TimeIntervalQuotI
      module procedure ESMF_TimeIntervalQuotR

! !DESCRIPTION:
!     This interface overloads the / operator for the {\tt ESMF\_TimeInterval}
!     class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(*)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTI
      module procedure ESMF_TimeIntervalProdIT
      module procedure ESMF_TimeIntervalProdTF
      module procedure ESMF_TimeIntervalProdFT
      module procedure ESMF_TimeIntervalProdTR
      module procedure ESMF_TimeIntervalProdRT

! !DESCRIPTION:
!     This interface overloads the * operator for the {\tt ESMF\_TimeInterval}
!     class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
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
!     {\tt ESMF\_TimeInterval} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! Generic Set/Get routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalSet(timeInterval, &
                                      yy, yy_i8, &
                                      mo, mo_i8, &
                                      d, d_i8, &
                                      h, m, &
                                      s, s_i8, &
                                      ms, us, ns, &
                                      d_r8, h_r8, m_r8, s_r8, &
                                      ms_r8, us_r8, ns_r8, &
                                      sN, sD, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout)         :: timeInterval
      integer(ESMF_KIND_I4),   intent(in),  optional :: yy
      integer(ESMF_KIND_I8),   intent(in),  optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: mo
      integer(ESMF_KIND_I8),   intent(in),  optional :: mo_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: d
      integer(ESMF_KIND_I8),   intent(in),  optional :: d_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: h
      integer(ESMF_KIND_I4),   intent(in),  optional :: m
      integer(ESMF_KIND_I4),   intent(in),  optional :: s
      integer(ESMF_KIND_I8),   intent(in),  optional :: s_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: ms
      integer(ESMF_KIND_I4),   intent(in),  optional :: us
      integer(ESMF_KIND_I4),   intent(in),  optional :: ns
      real(ESMF_KIND_R8),      intent(in),  optional :: d_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: h_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: m_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: s_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: ms_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: us_r8
      real(ESMF_KIND_R8),      intent(in),  optional :: ns_r8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sN
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via F90 optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally to integers.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to initialize.
!     \item[{[yy]}]
!          Integer years (>= 32-bit).
!     \item[{[yy\_i8]}]
!          Integer years (large, >= 64-bit).
!     \item[{[mo]}]
!          Integer months (>= 32-bit).
!     \item[{[mo\_i8]}]
!          Integer months (large, >= 64-bit).
!     \item[{[d]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h]}]
!          Integer hours.
!     \item[{[m]}]
!          Integer minutes.
!     \item[{[s]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms]}]
!          Integer milliseconds.
!     \item[{[us]}]
!          Integer microseconds.
!     \item[{[ns]}]
!          Integer nanoseconds.
!     \item[{[d\_r8]}]
!          Double precision days.
!     \item[{[h\_r8]}]
!          Double precision hours.
!     \item[{[m\_r8]}]
!          Double precision minutes.
!     \item[{[s\_r8]}]
!          Double precision seconds.
!     \item[{[ms\_r8]}]
!          Double precision milliseconds.
!     \item[{[us\_r8]}]
!          Double precision microseconds.
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.
!     \item[{[sN]}]
!          Integer fractional seconds - numerator.
!     \item[{[sD]}]
!          Integer fractional seconds - denominator.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSet(timeInterval, yy, yy_i8, mo, mo_i8, &
                                  d, d_i8, h, m, s, s_i8, ms, &
                                  us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sD, rc)

      end subroutine ESMF_TimeIntervalSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value 

! !INTERFACE:
      subroutine ESMF_TimeIntervalGet(timeInterval, &
                                      yy, yy_i8, &
                                      mo, mo_i8, &
                                      d, d_i8, &
                                      h, m, &
                                      s, s_i8, &
                                      ms, us, ns, &
                                      d_r8, h_r8, m_r8, s_r8, &
                                      ms_r8, us_r8, ns_r8, &
                                      sN, sD, &
                                      timeString, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeInterval
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: mo
      integer(ESMF_KIND_I8),   intent(out), optional :: mo_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: d
      integer(ESMF_KIND_I8),   intent(out), optional :: d_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: h
      integer(ESMF_KIND_I4),   intent(out), optional :: m
      integer(ESMF_KIND_I4),   intent(out), optional :: s
      integer(ESMF_KIND_I8),   intent(out), optional :: s_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: ms
      integer(ESMF_KIND_I4),   intent(out), optional :: us
      integer(ESMF_KIND_I4),   intent(out), optional :: ns
      real(ESMF_KIND_R8),      intent(out), optional :: d_r8
      real(ESMF_KIND_R8),      intent(out), optional :: h_r8
      real(ESMF_KIND_R8),      intent(out), optional :: m_r8
      real(ESMF_KIND_R8),      intent(out), optional :: s_r8
      real(ESMF_KIND_R8),      intent(out), optional :: ms_r8
      real(ESMF_KIND_R8),      intent(out), optional :: us_r8
      real(ESMF_KIND_R8),      intent(out), optional :: ns_r8
      integer(ESMF_KIND_I4),   intent(out), optional :: sN
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      character (len=*),       intent(out), optional :: timeString
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the value of {\tt timeInterval} in units specified by the
!     user via F90 optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be 1 day, then
!     {\tt ESMF\_TimeIntervalGet(d = days, s = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s = seconds)} would return
!       {\tt seconds = 86400}.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!     
!     For timeString, convert {\tt ESMF\_TimeInterval}'s value into ISO 8601
!     format PyYmMdDThHmMsS.  See ~\cite{ISO}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to query.
!     \item[{[yy]}]
!          Integer years (>= 32-bit).
!     \item[{[yy\_i8]}]
!          Integer years (large, >= 64-bit).
!     \item[{[mo]}]
!          Integer months (>= 32-bit).
!     \item[{[mo\_i8]}]
!          Integer months (large, >= 64-bit).
!     \item[{[d]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h]}]
!          Integer hours.
!     \item[{[m]}]
!          Integer minutes.
!     \item[{[s]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms]}]
!          Integer milliseconds.
!     \item[{[us]}]
!          Integer microseconds.
!     \item[{[ns]}]
!          Integer nanoseconds.
!     \item[{[d\_r8]}]
!          Double precision days.
!     \item[{[h\_r8]}]
!          Double precision hours.
!     \item[{[m\_r8]}]
!          Double precision minutes.
!     \item[{[s\_r8]}]
!          Double precision seconds.
!     \item[{[ms\_r8]}]
!          Double precision milliseconds.
!     \item[{[us\_r8]}]
!          Double precision microseconds.
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.
!     \item[{[sN]}]
!          Integer fractional seconds - numerator.
!     \item[{[sD]}]
!          Integer fractional seconds - denominator.
!     \item[timeString]
!          The string to return.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.1

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGet(timeInterval, yy, yy_i8, mo, mo_i8, &
                                  d, d_i8, h, m, s, s_i8, ms, &
                                  us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sD, timeString, rc)
    
      end subroutine ESMF_TimeIntervalGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a TimeInterval

! !INTERFACE:
      function ESMF_TimeIntervalAbsValue(timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Returns the absolute value of {\tt timeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to take the absolute value of.
!          Absolute value returned as value of function.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.8
    
      call c_ESMC_TimeIntervalAbsValue(timeInterval, ESMF_TimeIntervalAbsValue)

      end function ESMF_TimeIntervalAbsValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Get the negative absolute value of a TimeInterval

! !INTERFACE:
      function ESMF_TimeIntervalNegAbsValue(timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalNegAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Returns the negative absolute value of {\tt timeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to take the negative absolute value of.
!          Negative absolute value returned as value of function.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.8
    
      call c_ESMC_TimeIntervalNegAbsValue(timeInterval, &
                                          ESMF_TimeIntervalNegAbsValue)

      end function ESMF_TimeIntervalNegAbsValue

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since F90 forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalFQuot - Divide two TimeIntervals, return fraction result

! !INTERFACE:
      function ESMF_TimeIntervalFQuot(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_Fraction) :: ESMF_TimeIntervalFQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns {\tt timeInterval1} divided by {\tt timeInterval2} as a 
!     fraction quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          The dividend.
!     \item[timeInterval2]
!          The divisor.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.5

      call c_ESMC_TimeIntervalFQuot(timeInterval1, timeInterval2, &
                                    ESMF_TimeIntervalFQuot)

      end function ESMF_TimeIntervalFQuot

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRQuot - Divide two TimeIntervals, return double precision result

! !INTERFACE:
      function ESMF_TimeIntervalRQuot(timeInterval1, timeInterval2)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_TimeIntervalRQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns {\tt timeInterval1} divided by {\tt timeInterval2} as a double precision
!     quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          The dividend.
!     \item[timeInterval2]
!          The divisor.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.5

      call c_ESMC_TimeIntervalRQuot(timeInterval1, timeInterval2, &
                                    ESMF_TimeIntervalRQuot)

      end function ESMF_TimeIntervalRQuot

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRemainder - Divide two TimeIntervals, return time interval remainder

! !INTERFACE:
      function ESMF_TimeIntervalRemainder(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalRemainder

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns the remainder of {\tt timeInterval1} divided by {\tt timeInterval2} 
!     as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          The dividend.
!     \item[timeInterval2]
!          The divisor.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!

      call c_ESMC_TimeIntervalRemainder(timeInterval1, timeInterval2, &
                                        ESMF_TimeIntervalRemainder)

      end function ESMF_TimeIntervalRemainder

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide TimeInterval by an integer, return TimeInterval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotI(timeInterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      integer(ESMF_KIND_I4),   intent(in) :: divisor

! !DESCRIPTION:
!     Divides {\tt timeInterval} by an integer {\tt divisor}, and returns
!     the quotient as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The dividend.
!     \item[divisor]
!          Integer divisor.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2

      call c_ESMC_TimeIntervalQuotI(timeInterval, divisor, &
                                    ESMF_TimeIntervalQuotI)

      end function ESMF_TimeIntervalQuotI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotR - Divide TimeInterval by a double precision, return TimeInterval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotR(timeInterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      real(ESMF_KIND_R8),      intent(in) :: divisor

! !DESCRIPTION:
!     Divides {\tt timeInterval} by a double precision {\tt divisor}, 
!     and returns the quotient as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The dividend.
!     \item[divisor]
!          Double precision divisor.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2

      call c_ESMC_TimeIntervalQuotR(timeInterval, divisor, &
                                    ESMF_TimeIntervalQuotR)

      end function ESMF_TimeIntervalQuotR

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdTI - Multiply a TimeInterval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdTI(timeInterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdTI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      integer(ESMF_KIND_I4),   intent(in) :: multiplier

! !DESCRIPTION:
!     Multiplies {\tt timeInterval} by an integer {\tt multiplier}, 
!     and returns the product as an {\tt ESMF\_TimeInterval}.
!     Commutative complement to {\tt ESMF\_TimeIntervalProdIT}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The multiplicand.
!     \item[mutliplier]
!          Integer multiplier.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2

      call c_ESMC_TimeIntervalProdTI(timeInterval, multiplier, &
                                     ESMF_TimeIntervalProdTI)

      end function ESMF_TimeIntervalProdTI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdIT - Multiply a TimeInterval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdIT(multiplier, timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdIT

! !ARGUMENTS:
      integer(ESMF_KIND_I4),   intent(in) :: multiplier
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Multiplies {\tt timeInterval} by an integer {\tt multiplier}, 
!     and returns the product as an {\tt ESMF\_TimeInterval}.
!     Commutative complement to {\tt ESMF\_TimeIntervalProdTI}.
!
!     The arguments are:
!     \begin{description}
!     \item[mutliplier]
!          Integer multiplier.
!     \item[timeInterval]
!          The multiplicand.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2

      call c_ESMC_TimeIntervalProdIT(multiplier, timeInterval, &
                                     ESMF_TimeIntervalProdIT)

      end function ESMF_TimeIntervalProdIT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalProdTF - Multiply a TimeInterval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdTF(timeInterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdTF

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      type(ESMF_Fraction),     intent(in) :: multiplier

! !DESCRIPTION:
!     Multiplies {\tt timeInterval} by a fraction {\tt multiplier},
!     and returns the product as an {\tt ESMF\_TimeInterval}.
!     Commutative complement to {\tt ESMF\_TimeIntervalProdFT}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The multiplicand.
!     \item[mutliplier]
!          Fraction multiplier.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2

      call c_ESMC_TimeIntervalProdTF(timeInterval, multiplier, &
                                     ESMF_TimeIntervalProdTF)

      end function ESMF_TimeIntervalProdTF

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalProdFT - Multiply a TimeInterval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdFT(multiplier, timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdFT

! !ARGUMENTS:
      type(ESMF_Fraction),     intent(in) :: multiplier
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Multiplies {\tt timeInterval} by a fraction {\tt multiplier},
!     and returns the product as an {\tt ESMF\_TimeInterval}.
!     Commutative complement to {\tt ESMF\_TimeIntervalProdTF}.
!
!     The arguments are:
!     \begin{description}
!     \item[mutliplier]
!          Fraction multiplier.
!     \item[timeInterval]
!          The multiplicand.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2

      call c_ESMC_TimeIntervalProdFT(multiplier, timeInterval, &
                                     ESMF_TimeIntervalProdFT)

      end function ESMF_TimeIntervalProdFT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdTR - Multiply a TimeInterval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdTR(timeInterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdTR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      real(ESMF_KIND_R8),      intent(in) :: multiplier

! !DESCRIPTION:
!     Multiplies {\tt timeInterval} by a double precision 
!     {\tt multiplier}, and returns the product as an {\tt ESMF\_TimeInterval}.
!     Commutative complement to {\tt ESMF\_TimeIntervalProdRT}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The multiplicand.
!     \item[mutliplier]
!          Double precision multiplier.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2

      call c_ESMC_TimeIntervalProdTR(timeInterval, multiplier, &
                                     ESMF_TimeIntervalProdTR)

      end function ESMF_TimeIntervalProdTR

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdRT - Multiply a TimeInterval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdRT(multiplier, timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdRT

! !ARGUMENTS:
      real(ESMF_KIND_R8),      intent(in) :: multiplier
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Multiplies {\tt timeInterval} by a double precision 
!     {\tt multiplier}, and returns the product as an {\tt ESMF\_TimeInterval}.
!     Commutative complement to {\tt ESMF\_TimeIntervalProdTR}.
!
!     The arguments are:
!     \begin{description}
!     \item[mutliplier]
!          Double precision multiplier.
!     \item[timeInterval]
!          The multiplicand.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2

      call c_ESMC_TimeIntervalProdRT(multiplier, timeInterval, &
                                     ESMF_TimeIntervalProdRT)

      end function ESMF_TimeIntervalProdRT

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMF_BaseTime class overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum - Add two TimeIntervals 

! !INTERFACE:
      function ESMF_TimeIntervalSum(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Adds {\tt timeInterval1} to {\tt timeInterval2} and returns the
!     sum as an {\tt ESMF\_TimeInterval}.  This method is overloaded 
!     with the (+) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          The augend.
!     \item[timeInterval2]
!          The addend.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, 
!                 TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeSum(timeInterval1, timeInterval2, &
                              ESMF_TimeIntervalSum)

      end function ESMF_TimeIntervalSum

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one TimeInterval from another
   
! !INTERFACE:
      function ESMF_TimeIntervalDiff(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Subtract {\tt timeInterval2} from {\tt timeInterval1} and return 
!     the remainder as an {\tt ESMF\_TimeInterval}.  This method is 
!     overloaded with the (-) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          The minuend.
!     \item[timeInterval2]
!          The subtrahend.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeDiff(timeInterval1, timeInterval2, &
                               ESMF_TimeIntervalDiff)

      end function ESMF_TimeIntervalDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalEQ - TimeInterval 1 equal to TimeInterval 2?

! !INTERFACE:
      function ESMF_TimeIntervalEQ(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalEQ

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

!DESCRIPTION:
!     Returns true if {\tt timeInterval1} and {\tt timeInterval2} are equal, 
!     false otherwise.  This method is overloaded with the (==) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          First time interval to compare.
!     \item[timeInterval2]
!          Second time interval to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeEQ(timeInterval1, timeInterval2, ESMF_TimeIntervalEQ)

      end function ESMF_TimeIntervalEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNE - TimeInterval 1 not equal to TimeInterval 2?

! !INTERFACE:
      function ESMF_TimeIntervalNE(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalNE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns true if {\tt timeInterval1} and {\tt timeInterval2} are
!     not equal, false otherwise.  This method is overloaded with the
!     (!=) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          First time interval to compare.
!     \item[timeInterval2]
!          Second time interval to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(timeInterval1, timeInterval2, ESMF_TimeIntervalNE)

      end function ESMF_TimeIntervalNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLT - TimeInterval 1 less than TimeInterval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLT(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns true if {\tt timeInterval1} is less than {\tt timeInterval2},
!     false otherwise. This method is overloaded with the (<) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          First time interval to compare.
!     \item[timeInterval2]
!          Second time interval to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(timeInterval1, timeInterval2, ESMF_TimeIntervalLT)

      end function ESMF_TimeIntervalLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGT - TimeInterval 1 greater than TimeInterval 2?

! !INTERFACE:
      function ESMF_TimeIntervalGT(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns true if {\tt timeInterval1} is greater than {\tt timeInterval2},
!     false otherwise.  This method is overloaded with the (>) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          First time interval to compare.
!     \item[timeInterval2]
!          Second time interval to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(timeInterval1, timeInterval2, ESMF_TimeIntervalGT)

      end function ESMF_TimeIntervalGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLE - TimeInterval 1 less than or equal to TimeInterval 2?

! !INTERFACE:
      function ESMF_TimeIntervalLE(timeInterval1, timeInterval2)

! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns true if {\tt timeInterval1} is less than or equal to 
!     {\tt timeInterval2}, false otherwise.  This method is overloaded
!     with the (<=) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          First time interval to compare.
!     \item[timeInterval2]
!          Second time interval to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(timeInterval1, timeInterval2, ESMF_TimeIntervalLE)

      end function ESMF_TimeIntervalLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGE - TimeInterval1 greater than or equal to TimeInterval2?

! !INTERFACE:
      function ESMF_TimeIntervalGE(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns true if {\tt timeInterval1} is greater than or equal to 
!     {\tt timeInterval2}, false otherwise.  This method is overloaded
!     with the (>=) operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval1]
!          First time interval to compare.
!     \item[timeInterval2]
!          Second time interval to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(timeInterval1, timeInterval2, ESMF_TimeIntervalGE)

      end function ESMF_TimeIntervalGE

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalReadRestart - Restore the contents of a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalReadRestart(timeInterval, &
                                              s, sN, sD, yy, mo, d, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeInterval
      integer(ESMF_KIND_I8),   intent(in)  :: s
      integer(ESMF_KIND_I4),   intent(in)  :: sN
      integer(ESMF_KIND_I4),   intent(in)  :: sD
      integer(ESMF_KIND_I8),   intent(in)  :: yy
      integer(ESMF_KIND_I8),   intent(in)  :: mo
      integer(ESMF_KIND_I8),   intent(in)  :: d 
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!      Restores the contents of an {\tt ESMF\_TimeInterval} for restart.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          {\tt ESMF\_TimeInterval} to restore.
!     \item[s]
!          64-bit integer seconds.
!     \item[sN]
!          Integer fractional seconds - numerator.
!     \item[sD]
!          Integer fractional seconds - denominator.
!     \item[yy]
!          64-bit integer calendar interval number of years.
!     \item[mo]
!          64-bit integer calendar interval number of months.
!     \item[d]
!          64-bit integer calendar interval number of days.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeIntervalReadRestart(timeInterval, &
                              s, sN, sD, yy, mo, d, rc)

      end subroutine ESMF_TimeIntervalReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWriteRestart - Save the contents of a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalWriteRestart(timeInterval, &
                                               s, sN, sD, yy, mo, d, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)  :: timeInterval
      integer(ESMF_KIND_I8),   intent(out) :: s
      integer(ESMF_KIND_I4),   intent(out) :: sN
      integer(ESMF_KIND_I4),   intent(out) :: sD
      integer(ESMF_KIND_I8),   intent(out) :: yy
      integer(ESMF_KIND_I8),   intent(out) :: mo
      integer(ESMF_KIND_I8),   intent(out) :: d 
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Saves the contents of an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          {\tt ESMF\_TimeInterval} to save.
!     \item[s]
!          64-bit integer seconds.
!     \item[sN]
!          Integer fractional seconds - numerator.
!     \item[sD]
!          Integer fractional seconds - denominator.
!     \item[yy]
!          64-bit integer calendar interval number of years.
!     \item[mo]
!          64-bit integer calendar interval number of months.
!     \item[d]
!          64-bit integer calendar interval number of days.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeIntervalWriteRestart(timeInterval, &
                                           s, sN, sD, yy, mo, d, rc)

      end subroutine ESMF_TimeIntervalWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalValidate - Validate a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalValidate(timeInterval, options, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeInterval
      character (len=*),       intent(in),  optional :: options
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Check whether a {\tt timeInterval} is valid.  The options control
!     the type of validation.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          {\tt ESMF\_TimeInterval} to validate.
!     \item[{[options]}]
!          Validate options.  TODO:  To be determined.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_TimeIntervalValidate(timeInterval, options, rc)

      end subroutine ESMF_TimeIntervalValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print the contents of a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalPrint(timeInterval, options, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeInterval
      character (len=*),       intent(in),  optional :: options
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing and debugging, this method prints out the contents
!     of an {\tt ESMF\_TimeInterval}.  The options control the type of
!     information and level of detail.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          Time interval to print out.
!     \item[{[options]}]
!          Print options.  If none specified, prints all TimeInterval
!          property values. \\
!          "string" - prints TimeInterval's value in ISO 8601 format
!                     PyYmMdDThHmMsS.  See ~\cite{ISO}. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_TimeIntervalPrint(timeInterval, options, rc)

      end subroutine ESMF_TimeIntervalPrint

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalMod
