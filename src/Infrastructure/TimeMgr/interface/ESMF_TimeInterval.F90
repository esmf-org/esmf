! $Id: ESMF_TimeInterval.F90,v 1.30 2003/08/08 00:25:49 eschwab Exp $
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
      sequence                           ! match C++ storage order
      private                            !   (members opaque on F90 side)
        type(ESMF_BaseTime) :: baseTime  ! inherit base class
        integer(ESMF_IKIND_I8) :: yy     ! calendar interval number of years
        integer(ESMF_IKIND_I8) :: mo     ! calendar interval number of months
        integer(ESMF_IKIND_I8) :: d      ! calendar interval number of days
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TimeInterval
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_TimeIntervalGet
      public ESMF_TimeIntervalSet
      public ESMF_TimeIntervalGetString
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
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_TimeInterval.F90,v 1.30 2003/08/08 00:25:49 eschwab Exp $'

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
      module procedure ESMF_TimeIntervalProdI
      module procedure ESMF_TimeIntervalProdF
      module procedure ESMF_TimeIntervalProdR

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
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeIntervalGet(timeInterval, &
                                      yy_i4, yy_i8, &
                                      mo_i4, mo_i8, &
                                      d_i4, d_i8, &
                                      h_i4, &
                                      m_i4, &
                                      s_i4, s_i8, &
                                      ms_i4, &
                                      us_i4, &
                                      ns_i4, &
                                      d_r8, &
                                      h_r8, &
                                      m_r8, &
                                      s_r8, &
                                      ms_r8, &
                                      us_r8, &
                                      ns_r8, &
                                      sN, sD, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      integer(ESMF_IKIND_I4), intent(out), optional :: yy_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: yy_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: mo_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: mo_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: d_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: d_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: h_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: m_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: s_i4
      integer(ESMF_IKIND_I8), intent(out), optional :: s_i8
      integer(ESMF_IKIND_I4), intent(out), optional :: ms_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: us_i4
      integer(ESMF_IKIND_I4), intent(out), optional :: ns_i4
      real(ESMF_IKIND_R8),    intent(out), optional :: d_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: h_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: m_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: s_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: ms_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: us_r8
      real(ESMF_IKIND_R8),    intent(out), optional :: ns_r8
      integer,                intent(out), optional :: sN
      integer,                intent(out), optional :: sD
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Get the value of the {\tt ESMF\_TimeInterval} in units specified by the
!     user via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be 1 day, then
!     {\tt ESMF\_TimeIntervalGet(d\_i4 = days, s\_i4 = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s\_i4 = seconds)} would return
!       {\tt seconds = 86400}.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to query.
!     \item[{[yy\_i4]}]
!          Integer years (>= 32-bit).
!     \item[{[yy\_i8]}]
!          Integer years (large, >= 64-bit).
!     \item[{[mo\_i4]}]
!          Integer months (>= 32-bit).
!     \item[{[mo\_i8]}]
!          Integer months (large, >= 64-bit).
!     \item[{[d\_i4]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h\_i4]}]
!          Integer hours.
!     \item[{[m\_i4]}]
!          Integer minutes.
!     \item[{[s\_i4]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms\_i4]}]
!          Integer milliseconds.
!     \item[{[us\_i4]}]
!          Integer microseconds.
!     \item[{[ns\_i4]}]
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
!     TMG1.1

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGet(timeInterval, yy_i4, yy_i8, mo_i4, mo_i8, &
                                  d_i4, d_i8, h_i4, m_i4, s_i4, s_i8, ms_i4, &
                                  us_i4, ns_i4, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sD, rc)
    
      end subroutine ESMF_TimeIntervalGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize via user-specified unit set

! !INTERFACE:
      subroutine ESMF_TimeIntervalSet(timeInterval, &
                                      yy_i4, yy_i8, &
                                      mo_i4, mo_i8, &
                                      d_i4, d_i8, &
                                      h_i4, &
                                      m_i4, &
                                      s_i4, s_i8, &
                                      ms_i4, &
                                      us_i4, &
                                      ns_i4, &
                                      d_r8, &
                                      h_r8, &
                                      m_r8, &
                                      s_r8, &
                                      ms_r8, &
                                      us_r8, &
                                      ns_r8, &
                                      sN, sD, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout) :: timeInterval
      integer(ESMF_IKIND_I4), intent(in), optional :: yy_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: yy_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: mo_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: mo_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: d_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: d_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: h_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: m_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: s_i4
      integer(ESMF_IKIND_I8), intent(in), optional :: s_i8
      integer(ESMF_IKIND_I4), intent(in), optional :: ms_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: us_i4
      integer(ESMF_IKIND_I4), intent(in), optional :: ns_i4
      real(ESMF_IKIND_R8),    intent(in), optional :: d_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: h_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: m_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: s_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: ms_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: us_r8
      real(ESMF_IKIND_R8),    intent(in), optional :: ns_r8
      integer,                intent(in), optional :: sN
      integer,                intent(in), optional :: sD
      integer,                intent(out), optional :: rc

! !DESCRIPTION:
!     Set the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to initialize.
!     \item[{[yy\_i4]}]
!          Integer years (>= 32-bit).
!     \item[{[yy\_i8]}]
!          Integer years (large, >= 64-bit).
!     \item[{[mo\_i4]}]
!          Integer months (>= 32-bit).
!     \item[{[mo\_i8]}]
!          Integer months (large, >= 64-bit).
!     \item[{[d\_i4]}]
!          Integer Julian days (>= 32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).
!     \item[{[h\_i4]}]
!          Integer hours.
!     \item[{[m\_i4]}]
!          Integer minutes.
!     \item[{[s\_i4]}]
!          Integer seconds (>= 32-bit).
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).
!     \item[{[ms\_i4]}]
!          Integer milliseconds.
!     \item[{[us\_i4]}]
!          Integer microseconds.
!     \item[{[ns\_i4]}]
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
      call c_ESMC_TimeIntervalSet(timeInterval, yy_i4, yy_i8, mo_i4, mo_i8, &
                                  d_i4, d_i8, h_i4, m_i4, s_i4, s_i8, ms_i4, &
                                  us_i4, ns_i4, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sD, rc)

      end subroutine ESMF_TimeIntervalSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGetString - Get time interval value in string format

! !INTERFACE:
      subroutine ESMF_TimeIntervalGetString(timeInterval, timeString, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      character, intent(out) :: timeString(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Convert {\tt ESMF\_TimeInterval}'s value into ISO 8601 format 
!     PyYmMdDThHmMsS.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          The object instance to convert.
!     \item[timeString]
!          The string to return.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.9

      call c_ESMC_TimeIntervalGetString(timeInterval, timeString, rc)

      end subroutine ESMF_TimeIntervalGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a time interval

! !INTERFACE:
      function ESMF_TimeIntervalAbsValue(timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Return a {\tt ESMF\_TimeInterval}'s absolute value.
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
! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Get the negative absolute value of a time interval

! !INTERFACE:
      function ESMF_TimeIntervalNegAbsValue(timeInterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalNegAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval

! !DESCRIPTION:
!     Return a {\tt ESMF\_TimeInterval}'s negative absolute value.
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
! !IROUTINE:  ESMF_TimeIntervalFQuot - Divide two time intervals, return fraction result

! !INTERFACE:
      function ESMF_TimeIntervalFQuot(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_Fraction) :: ESMF_TimeIntervalFQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns timeInterval1 divided by timeInterval2 as a fraction quotient.
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
! !IROUTINE:  ESMF_TimeIntervalRQuot - Divide two time intervals, return double precision result

! !INTERFACE:
      function ESMF_TimeIntervalRQuot(timeInterval1, timeInterval2)

! !RETURN VALUE:
      double precision :: ESMF_TimeIntervalRQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns timeInterval1 divided by timeInterval2 as a double precision
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
! !IROUTINE:  ESMF_TimeIntervalRemainder - Divide two time intervals, return time interval remainder

! !INTERFACE:
      function ESMF_TimeIntervalRemainder(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalRemainder

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Returns the remainder of timeInterval1 divided by timeInterval2 as a
!     {\tt ESMF\_TimeInterval}.
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
! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide time interval by an integer, return time interval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotI(timeInterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      integer, intent(in) :: divisor

! !DESCRIPTION:
!     Divides a {\tt ESMF\_TimeInterval} by an integer divisor, returns
!     quotient as a {\tt ESMF\_TimeInterval}.
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
! !IROUTINE:  ESMF_TimeIntervalQuotR - Divide time interval by a double precision, return time interval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotR(timeInterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      double precision, intent(in) :: divisor

! !DESCRIPTION:
!     Divides an {\tt ESMF\_TimeInterval} by a double precision divisor, returns
!     quotient as a {\tt ESMF\_TimeInterval}.
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
! !IROUTINE:   ESMF_TimeIntervalProdI - Multiply a time interval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdI(timeInterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      integer, intent(in) :: multiplier

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by an integer, return product as a
!     {\tt ESMF\_TimeInterval}.
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

      call c_ESMC_TimeIntervalProdI(timeInterval, multiplier, &
                                    ESMF_TimeIntervalProdI)

      end function ESMF_TimeIntervalProdI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalProdF - Multiply a time interval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdF(timeInterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdF

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      type(ESMF_Fraction), intent(in) :: multiplier

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by a fraction, return product as a
!     {\tt ESMF\_TimeInterval}.
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

      call c_ESMC_TimeIntervalProdF(timeInterval, multiplier, &
                                    ESMF_TimeIntervalProdF)

      end function ESMF_TimeIntervalProdF
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdR - Multiply a time interval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdR(timeInterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      double precision, intent(in) :: multiplier

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by a double precision number,
!     return product as a {\tt ESMF\_TimeInterval}.
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

      call c_ESMC_TimeIntervalProdR(timeInterval, multiplier, &
                                    ESMF_TimeIntervalProdR)

      end function ESMF_TimeIntervalProdR

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMF_BaseTime class overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum - Add two time intervals together

! !INTERFACE:
      function ESMF_TimeIntervalSum(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Add two {\tt ESMF\_TimeIntervals}, return sum as a
!     {\tt ESMF\_TimeInterval}.  Maps overloaded (+) operator interface
!     function to {\tt ESMF\_BaseTime} base class.
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
! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one time interval from another
   
! !INTERFACE:
      function ESMF_TimeIntervalDiff(timeInterval1, timeInterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Subtract timeInterval2 from timeInterval1, return remainder as a 
!     {\tt ESMF\_TimeInterval}.
!     Map overloaded (-) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
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
! !IROUTINE: ESMF_TimeIntervalEQ - Compare two time intervals for equality

! !INTERFACE:
      function ESMF_TimeIntervalEQ(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalEQ

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

!DESCRIPTION:
!     Return true if both given time intervals are equal, false otherwise.
!     Maps overloaded (==) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
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
! !IROUTINE:  ESMF_TimeIntervalNE - Compare two time intervals for inequality

! !INTERFACE:
      function ESMF_TimeIntervalNE(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalNE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Return true if both given time intervals are not equal, false otherwise.
!     Maps overloaded (/=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
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
! !IROUTINE:  ESMF_TimeIntervalLT - Time interval 1 less than time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLT(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Return true if first time interval is less than second time interval,
!     false otherwise. Maps overloaded (<) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
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
! !IROUTINE:  ESMF_TimeIntervalGT - Time interval 1 greater than time interval 2?

! !INTERFACE:
      function ESMF_TimeIntervalGT(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than second time interval,
!     false otherwise.  Maps overloaded (>) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
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
! !IROUTINE:  ESMF_TimeIntervalLE - Time interval 1 less than or equal to time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLE(timeInterval1, timeInterval2)

! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Return true if first time interval is less than or equal to second time
!     interval, false otherwise.
!     Maps overloaded (<=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
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
! !IROUTINE:  ESMF_TimeIntervalGE - Time interval 1 greater than or equal to time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalGE(timeInterval1, timeInterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval1
      type(ESMF_TimeInterval), intent(in) :: timeInterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than or equal to second
!     time interval, false otherwise. Maps overloaded (>=) operator interface
!     function to {\tt ESMF\_BaseTime} base class.
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
! !IROUTINE:  ESMF_TimeIntervalReadRestart - Restore a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalReadRestart(timeInterval, &
                                              s, sN, sD, yy, mo, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeInterval
      integer(ESMF_IKIND_I8), intent(in) :: s
      integer, intent(in) :: sN
      integer, intent(in) :: sD
      integer(ESMF_IKIND_I8), intent(in) :: yy
      integer(ESMF_IKIND_I8), intent(in) :: mo
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt ESMF\_TimeInterval}'s properties.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeIntervalReadRestart(timeInterval, s, sN, sD, yy, mo, rc)

      end subroutine ESMF_TimeIntervalReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWriteRestart - Save a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalWriteRestart(timeInterval, &
                                               s, sN, sD, yy, mo, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      integer(ESMF_IKIND_I8), intent(out) :: s
      integer, intent(out) :: sN
      integer, intent(out) :: sD
      integer(ESMF_IKIND_I8), intent(out) :: yy
      integer(ESMF_IKIND_I8), intent(out) :: mo
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on an {\tt ESMF\_TimeInterval}'s properties.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_TimeIntervalWriteRestart(timeInterval, s, sN, sD, yy, mo, rc)

      end subroutine ESMF_TimeIntervalWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalValidate - Validate a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalValidate(timeInterval, opts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_TimeInterval}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          {\tt ESMF\_TimeInterval} to validate.
!     \item[{[opts]}]
!          Validate options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_TimeIntervalValidate(timeInterval, opts, rc)

      end subroutine ESMF_TimeIntervalValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print out a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalPrint(timeInterval, opts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeInterval
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt ESMF\_TimeInterval}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[timeInterval]
!          Time interval to print out.
!     \item[{[opts]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_TimeIntervalPrint(timeInterval, opts, rc)

      end subroutine ESMF_TimeIntervalPrint

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalMod
