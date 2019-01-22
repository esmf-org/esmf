! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_TimeInterval.F90"
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
#include "ESMF_TimeMgr.inc"
!
!===============================================================================
!BOPI
! !MODULE: ESMF_TimeIntervalMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ implementation of class {\tt ESMC\_TimeInterval}.
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_IOUtilMod

      ! associated derived types
      use ESMF_FractionMod
      use ESMF_TimeTypeMod
      use ESMF_CalendarMod

      ! type definition for this module
      use ESMF_TimeIntervalTypeMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_TimeInterval definition in ESMF_TimeIntervalTypeMod to resolve
!     ! mutual dependency with ESMF_TimeInterval
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TimeInterval
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
      public operator(+)
      public operator(-)
      public operator(/)
      public operator(.DIV.)
      public MOD
      public operator(*)
      public operator(==)
      public operator(/=)
      public operator(<)
      public operator(<=)
      public operator(>)
      public operator(>=)
      public ESMF_TimeIntervalAbsValue
      public ESMF_TimeIntervalGet
      public ESMF_TimeIntervalNegAbsValue
      public ESMF_TimeIntervalPrint
      public ESMF_TimeIntervalReadRestart
      public ESMF_TimeIntervalSet
      public ESMF_TimeIntervalValidate
      public ESMF_TimeIntervalWriteRestart

! - ESMF-internal methods:
      public ESMF_TimeIntervalGetInit
      public ESMF_TimeIntervalInit

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_TimeIntervalSum
      private ESMF_TimeIntervalDiff
      private ESMF_TimeIntervalNegate
      private ESMF_TimeIntervalRQuot
      private ESMF_TimeIntervalQuotI
      private ESMF_TimeIntervalQuotR
      private ESMF_TimeIntervalFQuot
      private ESMF_TimeIntervalRemainder
      private ESMF_TimeIntervalProdTI
      private ESMF_TimeIntervalProdIT
      private ESMF_TimeIntervalProdTF
      private ESMF_TimeIntervalProdFT
      private ESMF_TimeIntervalProdRT
      private ESMF_TimeIntervalProdTR
      private ESMF_TimeIntervalEQ
      private ESMF_TimeIntervalNE
      private ESMF_TimeIntervalLT
      private ESMF_TimeIntervalLE
      private ESMF_TimeIntervalGT
      private ESMF_TimeIntervalGE
      private ESMF_TimeIntervalSetDur
      private ESMF_TimeIntervalSetDurStart
      private ESMF_TimeIntervalSetDurCal
      private ESMF_TimeIntervalSetDurCalTyp
      private ESMF_TimeIntervalGetDur
      private ESMF_TimeIntervalGetDurStart
      private ESMF_TimeIntervalGetDurCal
      private ESMF_TimeIntervalGetDurCalTyp

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE:  ESMF_TimeIntervalAssignment(=) - Assign a TimeInterval to another TimeInterval
!
! !INTERFACE:
!     interface assignment(=)
!     timeinterval1 = timeinterval2
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval) :: timeinterval1
!     type(ESMF_TimeInterval) :: timeinterval2
! 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Set {\tt timeinterval1} equal to {\tt timeinterval2}.  This is the default
!     Fortran assignment, which creates a complete, independent copy of
!     {\tt timeinterval2} as {\tt timeinterval1}.  If {\tt timeinterval2} is an
!     invalid {\tt ESMF\_TimeInterval} object then {\tt timeinterval1} will be
!     equally invalid after the assignment.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description} 
!     \item[timeinterval1] 
!          The {\tt ESMF\_TimeInterval} to be set.
!     \item[timeinterval2] 
!          The {\tt ESMF\_TimeInterval} to be copied.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
!     None, documentation only, to describe the behavior of the default 
!     Fortran assignment(=).
!
! !REQUIREMENTS:
!     API review 11/2010.
! 
!     end interface
! 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalOperator(+) - Add two TimeIntervals
!
! !INTERFACE:
      interface operator(+)
!     sum = timeinterval1 + timeinterval2
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: sum
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (+) operator for the {\tt ESMF\_TimeInterval} class to
!     add {\tt timeinterval1} to {\tt timeinterval2} and return the
!     sum as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The augend.
!     \item[timeinterval2]
!          The addend.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalSum   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG5.1, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalOperator(-) - Subtract one TimeInterval from another
!
! !INTERFACE:
      interface operator(-)
!     difference = timeinterval1 - timeinterval2
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: difference
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Overloads the (-) operator for the {\tt ESMF\_TimeInterval} class to
!     subtract {\tt timeinterval2} from {\tt timeinterval1} and return
!     the difference as an {\tt ESMF\_TimeInterval}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The minuend.
!     \item[timeinterval2]
!          The subtrahend.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalDiff   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG5.1, TMG7.2

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalOperator(-) - Perform unary negation on a TimeInterval
!
! !INTERFACE:
!     interface operator(-)
!     timeinterval = -timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: -timeInterval
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (-) operator for the {\tt ESMF\_TimeInterval} class to
!     perform unary negation on {\tt timeinterval} and return the result.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The time interval to be negated.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalNegate   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.10

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalOperator(/) - Divide two TimeIntervals, return double precision quotient
!
! !INTERFACE:
      interface operator(/)
!     quotient = timeinterval1 / timeinterval2
!
! !RETURN VALUE:
!     real(ESMF_KIND_R8) :: quotient
!
! !ARGUMENTS: 
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Overloads the (/) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt timeinterval1} divided by {\tt timeinterval2} as a
!     double precision quotient.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend.
!     \item[timeinterval2]
!          The divisor.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalRQuot   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.5, TMG7.2
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalOperator(/) - Divide a TimeInterval by an integer, return TimeInterval quotient 
!
! !INTERFACE:
!     interface operator(/)
!     quotient = timeinterval / divisor
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: quotient
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!     integer(ESMF_KIND_I4),   intent(in) :: divisor
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (/) operator for the {\tt ESMF\_TimeInterval} class to
!     divide a {\tt timeinterval} by an integer {\tt divisor}, and
!     return the quotient as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend.
!     \item[divisor]
!          Integer divisor.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalQuotI   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!
!------------------------------------------------------------------------------
!BOPI
! TODO: when implemented, change to BOP/EOP
!
! !IROUTINE:  ESMF_TimeIntervalOperator(/) - Divide a TimeInterval by a double precision divisor, return TimeInterval quotient 
!
! !INTERFACE:
!     interface operator(/)
!     quotient = timeinterval / divisor
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: quotient
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!     real(ESMF_KIND_R8),      intent(in) :: divisor
!
! !DESCRIPTION:
!     Overloads the (/) operator for the {\tt ESMF\_TimeInterval} class to
!     divide a {\tt timeinterval} by a double precision {\tt divisor}, 
!     and return the quotient as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend.
!     \item[divisor]
!          Double precision divisor.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalQuotR   ! internal implementation
!
! !REQUIREMENTS:
!     Time Manager API review 6/2003, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! TODO: when implemented, change to BOP/EOP
!
! !IROUTINE:  ESMF_TimeIntervalOperator(.DIV.) - Divide two TimeIntervals, return fraction quotient
!
! !INTERFACE:
      interface operator(.DIV.)
!     quotient = timeinterval1 .DIV. timeinterval2
!
! !RETURN VALUE:
!     type(ESMF_Fraction) :: quotient
!
! !ARGUMENTS: 
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
! !DESCRIPTION:
!     Defines a new operator (.DIV.) for the {\tt ESMF\_TimeInterval} class
!     which returns {\tt timeinterval1} divided by {\tt timeinterval2} as a 
!     fraction quotient.
!
!     Implementation note:  This cannot be overloaded with (/) because the
!     arguments are the same as "Divide two TimeIntervals, return double
!     precision quotient" (see below).  The difference is in the return type
!     ({\tt ESMF\_Fraction} vs. real), which Fortran does not use to distinguish
!     among multiple overloaded methods.  Since the {\tt ESMF\_Fraction} return
!     type is less likely to be used, it was selected for the new
!     .DIV. operator.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend.
!     \item[timeinterval2]
!          The divisor.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalFQuot   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.5
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalFunction(MOD) - Divide two TimeIntervals, return TimeInterval remainder
!
! !INTERFACE:
      interface MOD
!     function MOD(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: MOD
!
! !ARGUMENTS: 
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the Fortran intrinsic MOD() function for the
!     {\tt ESMF\_TimeInterval} class to return the remainder of 
!     {\tt timeinterval1} divided by {\tt timeinterval2} as an 
!     {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend.
!     \item[timeinterval2]
!          The divisor.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalRemainder   ! internal implementation
!
! !REQUIREMENTS:
!     Time Manager API review 6/2003, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalOperator(*) - Multiply a TimeInterval by an integer
!
! !INTERFACE:
      interface operator(*)
!     product = timeinterval * multiplier
!                   OR
!     product = multiplier * timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!     integer(ESMF_KIND_I4),   intent(in) :: multiplier
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to
!     multiply a {\tt timeinterval} by an integer {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]        
!          The multiplicand.
!     \item[multiplier]
!          The integer multiplier.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTI   ! internal implementation
      module procedure ESMF_TimeIntervalProdIT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2     
!
!------------------------------------------------------------------------------
!BOPI
! TODO: when implemented, change to BOP/EOP
!
! !IROUTINE:  ESMF_TimeIntervalOperator(*) - Multiply a TimeInterval by a fraction
!
! !INTERFACE:
!     interface operator(*)
!     product = timeinterval * multiplier
!                   OR
!     product = multiplier * timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!     type(ESMF_Fraction),     intent(in) :: multiplier
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to
!     multiply a {\tt timeinterval} by a fraction {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand.
!     \item[multiplier]
!          The fraction multiplier.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTF   ! internal implementation
      module procedure ESMF_TimeIntervalProdFT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!
!------------------------------------------------------------------------------
!BOPI
! TODO: when implemented, change to BOP/EOP
!
! !IROUTINE:  ESMF_TimeIntervalOperator(*) - Multiply a TimeInterval by a double precision multiplier
!
! !INTERFACE:
!     interface operator(*)
!     product = timeinterval * multiplier
!                   OR
!     product = multiplier * timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!     real(ESMF_KIND_R8),      intent(in) :: multiplier
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to
!     multiply a {\tt timeinterval} by a double precision {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand.
!     \item[multiplier]
!          The double precision multiplier.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTR   ! internal implementation
      module procedure ESMF_TimeIntervalProdRT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalOperator(==) - Test if TimeInterval 1 is equal to TimeInterval 2
!
! !INTERFACE:
      interface operator(==)
!     if (timeinterval1 == timeinterval2) then ... endif
!                  OR
!     result = (timeinterval1 == timeinterval2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:     
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
!DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt .true.} if {\tt timeinterval1} and {\tt timeinterval2} 
!     represent an equal duration of time, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First {\tt ESMF\_TimeInterval} in comparison.
!     \item[timeinterval2]
!          Second {\tt ESMF\_TimeInterval} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalEQ   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG7.2

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalOperator(/=) - Test if TimeInterval 1 is not equal to TimeInterval 2
!
! !INTERFACE:
      interface operator(/=)
!     if (timeinterval1 /= timeinterval2) then ... endif
!                  OR
!     result = (timeinterval1 /= timeinterval2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:     
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
!DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt .true.} if {\tt timeinterval1} and {\tt timeinterval2} do not 
!     represent an equal duration of time, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First {\tt ESMF\_TimeInterval} in comparison.
!     \item[timeinterval2]
!          Second {\tt ESMF\_TimeInterval} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalNE   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG7.2

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalOperator(<) - Test if TimeInterval 1 is less than TimeInterval 2
!
! !INTERFACE:
      interface operator(<)
!     if (timeinterval1 < timeinterval2) then ... endif
!                  OR
!     result = (timeinterval1 < timeinterval2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:     
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
!DESCRIPTION:
!     Overloads the (<) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt .true.} if {\tt timeinterval1} is a lesser duration of time 
!     than {\tt timeinterval2}, and {\tt .false.} otherwise.

!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First {\tt ESMF\_TimeInterval} in comparison.
!     \item[timeinterval2]
!          Second {\tt ESMF\_TimeInterval} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalLT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG7.2

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalOperator(<=) - Test if TimeInterval 1 is less than or equal to TimeInterval 2
!
! !INTERFACE:
      interface operator(<=)
!     if (timeinterval1 <= timeinterval2) then ... endif
!                  OR
!     result = (timeinterval1 <= timeinterval2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:     
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
!DESCRIPTION:
!     Overloads the (<=) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt .true.} if {\tt timeinterval1} is a lesser or equal duration 
!     of time than {\tt timeinterval2}, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First {\tt ESMF\_TimeInterval} in comparison.
!     \item[timeinterval2]
!          Second {\tt ESMF\_TimeInterval} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalLE   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG7.2

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalOperator(>) - Test if TimeInterval 1 is greater than TimeInterval 2
!
! !INTERFACE:
      interface operator(>)
!     if (timeinterval1 > timeinterval2) then ... endif
!                  OR
!     result = (timeinterval1 > timeinterval2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:     
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
!DESCRIPTION:
!     Overloads the (>) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt .true.} if {\tt timeinterval1} is a greater duration of time 
!     than {\tt timeinterval2}, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First {\tt ESMF\_TimeInterval} in comparison.
!     \item[timeinterval2]
!          Second {\tt ESMF\_TimeInterval} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGT   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG7.2

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalOperator(>=) - Test if TimeInterval 1 is greater than or equal to TimeInterval 2
!
! !INTERFACE:
      interface operator(>=)
!     if (timeinterval1 >= timeinterval2) then ... endif
!                  OR
!     result = (timeinterval1 >= timeinterval2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:     
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
!DESCRIPTION:
!     Overloads the (>=) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt .true.} if {\tt timeinterval1} is a greater or equal 
!     duration of time than {\tt timeinterval2}, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First {\tt ESMF\_TimeInterval} in comparison.
!     \item[timeinterval2]
!          Second {\tt ESMF\_TimeInterval} in comparison.
!     \end{description}
! 
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGE   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG7.2

      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval
!
! !INTERFACE:
      interface ESMF_TimeIntervalSet

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalSetDur
      module procedure ESMF_TimeIntervalSetDurStart
      module procedure ESMF_TimeIntervalSetDurCal
      module procedure ESMF_TimeIntervalSetDurCalTyp

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_TimeInterval}
!     Set methods.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value
!
! !INTERFACE:
      interface ESMF_TimeIntervalGet

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGetDur
      module procedure ESMF_TimeIntervalGetDurStart
      module procedure ESMF_TimeIntervalGetDurCal
      module procedure ESMF_TimeIntervalGetDurCalTyp

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_TimeInterval}
!     Get methods.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalAbsValue()"
!BOP
! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a TimeInterval

! !INTERFACE:
      function ESMF_TimeIntervalAbsValue(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Returns the absolute value of {\tt timeinterval}.
!
!     The argument is:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the absolute value of.
!          Absolute value is returned as the value of the function.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.8

      integer :: localrc
    
      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalAbsValue(timeinterval, ESMF_TimeIntervalAbsValue)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalAbsValue)

      end function ESMF_TimeIntervalAbsValue

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGetDur()"
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value 

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalGet()
      subroutine ESMF_TimeIntervalGetDur(timeinterval, keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        startTime, calendar, calkindflag, &
        timeString, timeStringISOFrac, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: mm
      integer(ESMF_KIND_I8),   intent(out), optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(out), optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      integer(ESMF_KIND_I8),   intent(out), optional :: sD_i8
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalKind_Flag), intent(out), optional :: calkindflag
      character (len=*),       intent(out), optional :: timeString
      character (len=*),       intent(out), optional :: timeStringISOFrac
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the value of {\tt timeinterval} in units specified by the
!     user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be one day, then
!     {\tt ESMF\_TimeIntervalGet(d = days, s = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s = seconds)} would return
!       {\tt seconds = 86400}.
!
!     For timeString, converts {\tt ESMF\_TimeInterval}'s value into
!     partial ISO 8601 format PyYmMdDThHmMs[:n/d]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     For timeStringISOFrac, converts {\tt ESMF\_TimeInterval}'s value into
!     full ISO 8601 format PyYmMdDThHmMs[.f]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query.
!     \item[{[yy]}]
!          Integer year (32-bit).
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).
!     \item[{[mm]}]
!          Integer month (32-bit).
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!     \item[{[h]}]
!          Integer hour.
!     \item[{[m]}]
!          Integer minute.
!     \item[{[s]}]
!          Integer second (32-bit).
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).
!     \item[{[ms]}]
!          Integer millisecond.
!     \item[{[us]}]
!          Integer microsecond.
!     \item[{[ns]}]
!          Integer nanosecond.
!     \item[{[d\_r8]}]
!          Double precision day.
!     \item[{[h\_r8]}]
!          Double precision hour.
!     \item[{[m\_r8]}]
!          Double precision minute.
!     \item[{[s\_r8]}]
!          Double precision second.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.
!     \item[{[us\_r8]}]
!          Double precision microsecond.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!     \item[{[startTime]}]
!          Starting time, if set, of an absolute calendar interval
!          (yy, mm, and/or d).
!     \item[{[calendar]}]
!          Associated {\tt Calendar}, if any.
!     \item[{[calkindflag]}]
!          Associated {\tt CalKind\_Flag}, if any.
!     \item[{[timeString]}]
!          \begin{sloppypar}
!          Convert time interval value to format string PyYmMdDThHmMs[:n/d]S,
!          where n/d is numerator/denominator of any fractional seconds and
!          all other units are in ISO 8601 format.  See ~\cite{ISO} and
!          ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!          \end{sloppypar}
!     \item[{[timeStringISOFrac]}]
!          Convert time interval value to strict ISO 8601 format string
!          PyYmMdDThHmMs[.f], where f is decimal form of any fractional
!          seconds.  See ~\cite{ISO} and ~\cite{ISOnotes}.  See also method
!          {\tt ESMF\_TimeIntervalPrint()}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.1

      ! temp time string for C++ to fill
      character (len=ESMF_MAXSTR) :: tempTimeString, tempTimeStringISOFrac

      ! initialize time string lengths to zero for non-existent time string
      integer :: timeStringLen, timeStringLenISOFrac
      integer :: tempTimeStringLen, tempTimeStringLenISOFrac
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeinterval,rc)

      timeStringLen = 0     
      timeStringLenISOFrac = 0     
      tempTimeStringLen = 0
      tempTimeStringLenISOFrac = 0

      ! if used, get length of given timeString for C++ validation
      if (present(timeString)) then
        timeStringLen = len(timeString)
      end if
      if (present(timeStringISOFrac)) then
        timeStringLenISOFrac = len(timeStringISOFrac)
      end if

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGetDur(timeinterval, yy, yy_i8, mm, mm_i8, &
                                     d, d_i8, h, m, s, s_i8, ms, &
                                     us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                     us_r8, ns_r8, sN, sN_i8, sD, sD_i8, &
                                     startTime, calendar, calkindflag, &
                                     timeStringLen, tempTimeStringLen, &
                                     tempTimeString, &
                                     timeStringLenISOFrac, &
                                     tempTimeStringLenISOFrac, &
                                     tempTimeStringISOFrac, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp time string back to given time string to restore
      !   native Fortran storage style
      if (present(timeString)) then
        timeString = tempTimeString(1:tempTimeStringLen)
      endif
      if (present(timeStringISOFrac)) then
        timeStringISOFrac = tempTimeStringISOFrac(1:tempTimeStringLenISOFrac)
      endif
    
      ! mark outputs as successfully initialized
      call ESMF_TimeInit(startTime)
      call ESMF_CalendarSetInitCreated(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalGetDur

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGetDurStart()"
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value 

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalGet()
      subroutine ESMF_TimeIntervalGetDurStart(timeinterval, startTimeIn, &
        keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        startTime, &
        calendar, calkindflag, &
        timeString, timeStringISOFrac, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_Time),         intent(in)            :: startTimeIn ! Input
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: mm
      integer(ESMF_KIND_I8),   intent(out), optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(out), optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      integer(ESMF_KIND_I8),   intent(out), optional :: sD_i8
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalKind_Flag), intent(out), optional :: calkindflag
      character (len=*),       intent(out), optional :: timeString
      character (len=*),       intent(out), optional :: timeStringISOFrac
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the value of {\tt timeinterval} in units specified by the
!     user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be one day, then
!     {\tt ESMF\_TimeIntervalGet(d = days, s = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s = seconds)} would return
!       {\tt seconds = 86400}.
!
!     For timeString, converts {\tt ESMF\_TimeInterval}'s value into
!     partial ISO 8601 format PyYmMdDThHmMs[:n/d]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     For timeStringISOFrac, converts {\tt ESMF\_TimeInterval}'s value into
!     full ISO 8601 format PyYmMdDThHmMs[.f]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query.
!     \item[startTimeIn]
!          INPUT argument:  pins a calendar interval to a specific point
!          in time to allow conversion between relative units (yy, mm, d) and
!          absolute units (d, h, m, s).  Overrides any startTime and/or endTime
!          previously set.  Mutually exclusive with endTimeIn and calendarIn.
!     \item[{[yy]}]
!          Integer year (32-bit).
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).
!     \item[{[mm]}]
!          Integer month (32-bit).
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!     \item[{[h]}]
!          Integer hour.
!     \item[{[m]}]
!          Integer minute.
!     \item[{[s]}]
!          Integer second (32-bit).
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).
!     \item[{[ms]}]
!          Integer millisecond.
!     \item[{[us]}]
!          Integer microsecond.
!     \item[{[ns]}]
!          Integer nanosecond.
!     \item[{[d\_r8]}]
!          Double precision day.
!     \item[{[h\_r8]}]
!          Double precision hour.
!     \item[{[m\_r8]}]
!          Double precision minute.
!     \item[{[s\_r8]}]
!          Double precision second.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.
!     \item[{[us\_r8]}]
!          Double precision microsecond.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!     \item[{[startTime]}]
!          Starting time, if set, of an absolute calendar interval
!          (yy, mm, and/or d).
!     \item[{[calendar]}]
!          Associated {\tt Calendar}, if any.
!     \item[{[calkindflag]}]
!          Associated {\tt CalKind\_Flag}, if any.
!     \item[{[timeString]}]
!          \begin{sloppypar}
!          Convert time interval value to format string PyYmMdDThHmMs[:n/d]S,
!          where n/d is numerator/denominator of any fractional seconds and
!          all other units are in ISO 8601 format.  See ~\cite{ISO} and
!          ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!          \end{sloppypar}
!     \item[{[timeStringISOFrac]}]
!          Convert time interval value to strict ISO 8601 format string
!          PyYmMdDThHmMs[.f], where f is decimal form of any fractional
!          seconds.  See ~\cite{ISO} and ~\cite{ISOnotes}. See also method
!          {\tt ESMF\_TimeIntervalPrint()}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.1

      ! temp time string for C++ to fill
      character (len=ESMF_MAXSTR) :: tempTimeString, tempTimeStringISOFrac

      ! initialize time string lengths to zero for non-existent time string
      integer :: timeStringLen, timeStringLenISOFrac
      integer :: tempTimeStringLen, tempTimeStringLenISOFrac
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,startTimeIn,rc)

      timeStringLen = 0
      timeStringLenISOFrac = 0
      tempTimeStringLen = 0
      tempTimeStringLenISOFrac = 0

      ! if used, get length of given timeString for C++ validation
      if (present(timeString)) then
        timeStringLen = len(timeString)
      end if
      if (present(timeStringISOFrac)) then
        timeStringLenISOFrac = len(timeStringISOFrac)
      end if

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGetDurStart(timeinterval, yy, yy_i8, mm, mm_i8, &
                                       d, d_i8, h, m, s, s_i8, ms, &
                                       us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                       us_r8, ns_r8, sN, sN_i8, sD, sD_i8, &
                                       startTime, &
                                       calendar, calkindflag, &
                                       startTimeIn, &
                                       timeStringLen, tempTimeStringLen, &
                                       tempTimeString, &
                                       timeStringLenISOFrac, &
                                       tempTimeStringLenISOFrac, &
                                       tempTimeStringISOFrac, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp time string back to given time string to restore
      !   native Fortran storage style
      if (present(timeString)) then
        timeString = tempTimeString(1:tempTimeStringLen)
      endif
      if (present(timeStringISOFrac)) then
        timeStringISOFrac = tempTimeStringISOFrac(1:tempTimeStringLenISOFrac)
      endif
    
      ! mark outputs as successfully initialized
      call ESMF_TimeInit(startTime)
      call ESMF_CalendarSetInitCreated(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalGetDurStart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGetDurCal()"
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value 

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalGet()
      subroutine ESMF_TimeIntervalGetDurCal(timeinterval, calendarIn, &
        keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        startTime, &
        calendar, calkindflag, &
        timeString, timeStringISOFrac, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_Calendar),     intent(in)            :: calendarIn ! Input
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: mm
      integer(ESMF_KIND_I8),   intent(out), optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(out), optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      integer(ESMF_KIND_I8),   intent(out), optional :: sD_i8
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalKind_Flag), intent(out), optional :: calkindflag
      character (len=*),       intent(out), optional :: timeString
      character (len=*),       intent(out), optional :: timeStringISOFrac
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the value of {\tt timeinterval} in units specified by the
!     user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be one day, then
!     {\tt ESMF\_TimeIntervalGet(d = days, s = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s = seconds)} would return
!       {\tt seconds = 86400}.
!
!     For timeString, converts {\tt ESMF\_TimeInterval}'s value into
!     partial ISO 8601 format PyYmMdDThHmMs[:n/d]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     For timeStringISOFrac, converts {\tt ESMF\_TimeInterval}'s value into
!     full ISO 8601 format PyYmMdDThHmMs[.f]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query.
!     \item[calendarIn]
!          INPUT argument:  pins a calendar interval to a specific calendar
!          to allow conversion between relative units (yy, mm, d) and
!          absolute units (d, h, m, s).  Mutually exclusive with startTimeIn
!          and endTimeIn since they contain a calendar.  Alternate to, and
!          mutually exclusive with, calkindflagIn below.  Primarily for
!          specifying a custom calendar kind.
!     \item[{[yy]}]
!          Integer year (32-bit).
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).
!     \item[{[mm]}]
!          Integer month (32-bit).
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!     \item[{[h]}]
!          Integer hour.
!     \item[{[m]}]
!          Integer minute.
!     \item[{[s]}]
!          Integer second (32-bit).
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).
!     \item[{[ms]}]
!          Integer millisecond.
!     \item[{[us]}]
!          Integer microsecond.
!     \item[{[ns]}]
!          Integer nanosecond.
!     \item[{[d\_r8]}]
!          Double precision day.
!     \item[{[h\_r8]}]
!          Double precision hour.
!     \item[{[m\_r8]}]
!          Double precision minute.
!     \item[{[s\_r8]}]
!          Double precision second.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.
!     \item[{[us\_r8]}]
!          Double precision microsecond.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit). 
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit). 
!     \item[{[startTime]}]
!          Starting time, if set, of an absolute calendar interval
!          (yy, mm, and/or d).
!     \item[{[calendar]}]
!          Associated {\tt Calendar}, if any.
!     \item[{[calkindflag]}]
!          Associated {\tt CalKind\_Flag}, if any.
!     \item[[{timeString]}]
!          \begin{sloppypar}
!          Convert time interval value to format string PyYmMdDThHmMs[:n/d]S,
!          where n/d is numerator/denominator of any fractional seconds and
!          all other units are in ISO 8601 format.  See ~\cite{ISO} and
!          ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!          \end{sloppypar}
!     \item[{[timeStringISOFrac]}]
!          Convert time interval value to strict ISO 8601 format string
!          PyYmMdDThHmMs[.f], where f is decimal form of any fractional
!          seconds.  See ~\cite{ISO} and ~\cite{ISOnotes}. See also method
!          {\tt ESMF\_TimeIntervalPrint()}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.1

      ! temp time string for C++ to fill
      character (len=ESMF_MAXSTR) :: tempTimeString, tempTimeStringISOFrac

      ! initialize time string lengths to zero for non-existent time string
      integer :: timeStringLen, timeStringLenISOFrac
      integer :: tempTimeStringLen, tempTimeStringLenISOFrac
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendarIn,rc)

      timeStringLen = 0     
      timeStringLenISOFrac = 0     
      tempTimeStringLen = 0
      tempTimeStringLenISOFrac = 0

      ! if used, get length of given timeString for C++ validation
      if (present(timeString)) then
        timeStringLen = len(timeString)
      end if
      if (present(timeStringISOFrac)) then
        timeStringLenISOFrac = len(timeStringISOFrac)
      end if

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGetDurCal(timeinterval, yy, yy_i8, mm, mm_i8, &
                                  d, d_i8, h, m, s, s_i8, ms, &
                                  us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sN_i8, sD, sD_i8, &
                                  startTime, calendar, calkindflag, &
                                  calendarIn, &
                                  timeStringLen, tempTimeStringLen, &
                                  tempTimeString, &
                                  timeStringLenISOFrac, &
                                  tempTimeStringLenISOFrac, &
                                  tempTimeStringISOFrac, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp time string back to given time string to restore
      !   native Fortran storage style
      if (present(timeString)) then
        timeString = tempTimeString(1:tempTimeStringLen)
      endif
      if (present(timeStringISOFrac)) then
        timeStringISOFrac = tempTimeStringISOFrac(1:tempTimeStringLenISOFrac)
      endif
    
      ! mark outputs as successfully initialized
      call ESMF_TimeInit(startTime)
      call ESMF_CalendarSetInitCreated(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalGetDurCal

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGetDurCalTyp()"
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value 

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalGet()
      subroutine ESMF_TimeIntervalGetDurCalTyp(timeinterval, calkindflagIn, &
        keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        startTime, &
        calendar, calkindflag, &
        timeString, &
        timeStringISOFrac, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_CalKind_Flag), intent(in)            :: calkindflagIn ! Input
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: mm
      integer(ESMF_KIND_I8),   intent(out), optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(out), optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      integer(ESMF_KIND_I8),   intent(out), optional :: sD_i8
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalKind_Flag), intent(out), optional :: calkindflag
      character (len=*),       intent(out), optional :: timeString
      character (len=*),       intent(out), optional :: timeStringISOFrac
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the value of {\tt timeinterval} in units specified by the
!     user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally from integers.
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be one day, then
!     {\tt ESMF\_TimeIntervalGet(d = days, s = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s = seconds)} would return
!       {\tt seconds = 86400}.
!
!     For timeString, converts {\tt ESMF\_TimeInterval}'s value into
!     partial ISO 8601 format PyYmMdDThHmMs[:n/d]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     For timeStringISOFrac, converts {\tt ESMF\_TimeInterval}'s value into
!     full ISO 8601 format PyYmMdDThHmMs[.f]S.  See ~\cite{ISO} and
!     ~\cite{ISOnotes}.  See also method {\tt ESMF\_TimeIntervalPrint()}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query.
!     \item[calkindflagIn]
!          INPUT argument:  Alternate to, and mutually exclusive with,
!          calendarIn above.  More convenient way of specifying a built-in
!          calendar kind.
!     \item[{[yy]}]
!          Integer year (32-bit).
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).
!     \item[{[mm]}]
!          Integer month (32-bit).
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!     \item[{[h]}]
!          Integer hour.
!     \item[{[m]}]
!          Integer minute.
!     \item[{[s]}]
!          Integer second (32-bit).
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).
!     \item[{[ms]}]
!          Integer millisecond.
!     \item[{[us]}]
!          Integer microsecond.
!     \item[{[ns]}]
!          Integer nanosecond.
!     \item[{[d\_r8]}]
!          Double precision day.
!     \item[{[h\_r8]}]
!          Double precision hour.
!     \item[{[m\_r8]}]
!          Double precision minute.
!     \item[{[s\_r8]}]
!          Double precision second.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.
!     \item[{[us\_r8]}]
!          Double precision microsecond.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!     \item[{[startTime]}]
!          Starting time, if set, of an absolute calendar interval
!          (yy, mm, and/or d).
!     \item[{[calendar]}]
!          Associated {\tt Calendar}, if any.
!     \item[{[calkindflag]}]
!          Associated {\tt CalKind\_Flag}, if any.
!     \item[[{timeString]}]
!          \begin{sloppypar}
!          Convert time interval value to format string PyYmMdDThHmMs[:n/d]S,
!          where n/d is numerator/denominator of any fractional seconds and
!          all other units are in ISO 8601 format.  See ~\cite{ISO} and
!          ~\cite{ISOnotes}.  See also method
!          {\tt ESMF\_TimeIntervalPrint()}.
!          \end{sloppypar}
!     \item[{[timeStringISOFrac]}]
!          Convert time interval value to strict ISO 8601 format string
!          PyYmMdDThHmMs[.f], where f is decimal form of any fractional
!          seconds.  See ~\cite{ISO} and ~\cite{ISOnotes}. See also method
!          {\tt ESMF\_TimeIntervalPrint()}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.1

      ! temp time string for C++ to fill
      character (len=ESMF_MAXSTR) :: tempTimeString, tempTimeStringISOFrac

      ! initialize time string lengths to zero for non-existent time string
      integer :: timeStringLen, timeStringLenISOFrac
      integer :: tempTimeStringLen, tempTimeStringLenISOFrac
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,rc)

      timeStringLen = 0
      timeStringLenISOFrac = 0
      tempTimeStringLen = 0
      tempTimeStringLenISOFrac = 0

      ! if used, get length of given timeString for C++ validation
      if (present(timeString)) then
        timeStringLen = len(timeString)
      end if
      if (present(timeStringISOFrac)) then
        timeStringLenISOFrac = len(timeStringISOFrac)
      end if

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGetDurCalTyp(timeinterval, yy, yy_i8, mm, mm_i8, &
                                  d, d_i8, h, m, s, s_i8, ms, &
                                  us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sN_i8, sD, sD_i8, &
                                  startTime, calendar, calkindflag, &
                                  calkindflagIn, &
                                  timeStringLen, tempTimeStringLen, &
                                  tempTimeString, &
                                  timeStringLenISOFrac, &
                                  tempTimeStringLenISOFrac, &
                                  tempTimeStringISOFrac, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp time string back to given time string to restore
      !   native Fortran storage style
      if (present(timeString)) then
        timeString = tempTimeString(1:tempTimeStringLen)
      endif
      if (present(timeStringISOFrac)) then
        timeStringISOFrac = tempTimeStringISOFrac(1:tempTimeStringLenISOFrac)
      endif
    
      ! mark outputs as successfully initialized
      call ESMF_TimeInit(startTime)
      call ESMF_CalendarSetInitCreated(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalGetDurCalTyp

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalNegAbsValue()"
!BOP
! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Return the negative absolute value of a TimeInterval

! !INTERFACE:
      function ESMF_TimeIntervalNegAbsValue(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalNegAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Returns the negative absolute value of {\tt timeinterval}.
!
!     The argument is:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the negative absolute value of.
!          Negative absolute value is returned as the value of the function.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG1.5.8

      integer :: localrc

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalNegAbsValue(timeinterval, &
                                          ESMF_TimeIntervalNegAbsValue)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalNegAbsValue)

      end function ESMF_TimeIntervalNegAbsValue

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalPrint()"
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print TimeInterval information

! !INTERFACE:
      subroutine ESMF_TimeIntervalPrint(timeinterval, options, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      character (len=*),       intent(in),  optional :: options
      integer,                 intent(out), optional :: rc

!
! !DESCRIPTION:
!     Prints out the contents of an {\tt ESMF\_TimeInterval} to {\tt stdout},
!     in support of testing and debugging.  The options control the type of
!     information and level of detail. \\
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to be printed out.
!     \item[{[options]}]
!          Print options.  If none specified, prints all {\tt timeinterval}
!          property values. \\
!          "string" - prints {\tt timeinterval}'s value in ISO 8601 format
!                     for all units through seconds.  For any non-zero
!                     fractional seconds, prints in integer rational
!                     fraction form n/d.  Format is PyYmMdDThHmMs[:n/d]S,
!                     where [:n/d] is the integer numerator and denominator
!                     of the fractional seconds value, if present.
!                     See ~\cite{ISO} and ~\cite{ISOnotes}.  See also method
!                     {\tt ESMF\_TimeIntervalGet(..., timeString= , ...)} \\
!          "string isofrac" - prints {\tt timeinterval}'s value in strict
!                     ISO 8601 format for all units, including any fractional
!                     seconds part.  Format is PyYmMdDThHmMs[.f]S, where [.f]
!                     represents fractional seconds in decimal form,
!                     if present.  See ~\cite{ISO} and ~\cite{ISOnotes}.
!                     See also method {\tt ESMF\_TimeIntervalGet(..., timeStringISOFrac= , ...)} \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeinterval,rc)
    
      ! invoke C to C++ entry point
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call c_ESMC_TimeIntervalPrint(timeinterval, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalReadRestart()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalReadRestart - Restore the contents of a TimeInterval (not implemented)

! !INTERFACE:
      subroutine ESMF_TimeIntervalReadRestart(timeinterval, name, &
        keywordEnforcer, rc)
!
! !ARGUMENTS:      
      type(ESMF_TimeInterval), intent(inout)         :: timeinterval
      character (len=*),       intent(in)            :: name
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_TimeInterval} object from the last call to
!     {\tt ESMF\_TimeIntervalWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Restore into this {\tt ESMF\_TimeInterval}.
!     \item[name]
!          Restore from this object name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     TMGn.n.n

      ! get length of given name for C++ validation
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = len_trim(name)

      ! invoke C to C++ entry point to restore timeinterval
      call c_ESMC_TimeIntervalReadRestart(timeinterval, nameLen, name, &
                                          localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(timeinterval)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalSetDur()"
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalSet()
      subroutine ESMF_TimeIntervalSetDur(timeinterval, keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout)         :: timeinterval
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(in),  optional :: yy
      integer(ESMF_KIND_I8),   intent(in),  optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: mm
      integer(ESMF_KIND_I8),   intent(in),  optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(in),  optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD
      integer(ESMF_KIND_I8),   intent(in),  optional :: sD_i8
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Sets the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally to integers.
!
!     Ranges are limited only by machine word size.  Numeric defaults are 0,
!     except for sD, which is 1.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize.
!     \item[{[yy]}]
!          Integer year (32-bit).  Default = 0.
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).  Default = 0.
!     \item[{[mm]}]
!          Integer month (32-bit).  Default = 0.
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).  Default = 0.
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).  Default = 0.
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!          Default = 0.
!     \item[{[h]}]
!          Integer hour.  Default = 0.
!     \item[{[m]}]
!          Integer minute.  Default = 0.
!     \item[{[s]}]
!          Integer second (32-bit).  Default = 0.
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).  Default = 0.
!     \item[{[ms]}]
!          Integer millisecond.  Default = 0.
!     \item[{[us]}]
!          Integer microsecond.  Default = 0.
!     \item[{[ns]}]
!          Integer nanosecond.  Default = 0.
!     \item[{[d\_r8]}]
!          Double precision day.  Default = 0.0.
!     \item[{[h\_r8]}]
!          Double precision hour.  Default = 0.0.
!     \item[{[m\_r8]}]
!          Double precision minute.  Default = 0.0.
!     \item[{[s\_r8]}]
!          Double precision second.  Default = 0.0.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.  Default = 0.0.
!     \item[{[us\_r8]}]
!          Double precision microsecond.  Default = 0.0.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.  Default = 0.0.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!          Default = 0.
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 0.
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!          Default = 1.
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSetDur(timeinterval, yy, yy_i8, &
                                     mm, mm_i8, &
                                     d, d_i8, h, m, s, s_i8, ms, &
                                     us, ns, d_r8, h_r8, m_r8, s_r8, &
                                     ms_r8, us_r8, ns_r8, &
                                     sN, sN_i8, sD, sD_i8, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output variable as successfully initialized
      call ESMF_TimeIntervalInit(timeinterval)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalSetDur

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalSetDurStart()"
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalSet()
      subroutine ESMF_TimeIntervalSetDurStart(timeinterval, startTime, &
        keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout)         :: timeinterval
      type(ESMF_Time),         intent(in)            :: startTime
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(in),  optional :: yy
      integer(ESMF_KIND_I8),   intent(in),  optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: mm
      integer(ESMF_KIND_I8),   intent(in),  optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(in),  optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD
      integer(ESMF_KIND_I8),   intent(in),  optional :: sD_i8
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Sets the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally to integers.
!
!     Ranges are limited only by machine word size.  Numeric defaults are 0,
!     except for sD, which is 1.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize.
!     \item[startTime]
!          Starting time of an absolute calendar interval 
!          (yy, mm, and/or d); pins a calendar interval to a specific point 
!          in time.  If not set, and calendar also not set, calendar interval 
!          "floats" across all calendars and times.
!     \item[{[yy]}]
!          Integer year (32-bit).  Default = 0.
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).  Default = 0.
!     \item[{[mm]}]
!          Integer month (32-bit).  Default = 0.
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).  Default = 0.
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).  Default = 0.
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!          Default = 0.
!     \item[{[h]}]
!          Integer hour.  Default = 0.
!     \item[{[m]}]
!          Integer minute.  Default = 0.
!     \item[{[s]}]
!          Integer second (32-bit).  Default = 0.
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).  Default = 0.
!     \item[{[ms]}]
!          Integer millisecond.  Default = 0.
!     \item[{[us]}]
!          Integer microsecond.  Default = 0.
!     \item[{[ns]}]
!          Integer nanosecond.  Default = 0.
!     \item[{[d\_r8]}]
!          Double precision day.  Default = 0.0.
!     \item[{[h\_r8]}]
!          Double precision hour.  Default = 0.0.
!     \item[{[m\_r8]}]
!          Double precision minute.  Default = 0.0.
!     \item[{[s\_r8]}]
!          Double precision second.  Default = 0.0.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.  Default = 0.0.
!     \item[{[us\_r8]}]
!          Double precision microsecond.  Default = 0.0.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.  Default = 0.0.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!          Default = 0.
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 0.
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!          Default = 1.
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8).
!                                                           (large, 64-bit).
!          Default = 1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,startTime,rc)

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSetDurStart(timeinterval, yy, yy_i8, &
                                          mm, mm_i8, &
                                          d, d_i8, h, m, s, s_i8, ms, &
                                          us, ns, d_r8, h_r8, m_r8, s_r8, &
                                          ms_r8, us_r8, ns_r8, &
                                          sN, sN_i8, sD, sD_i8, &
                                          startTime, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output variable as successfully initialized
      call ESMF_TimeIntervalInit(timeinterval)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalSetDurStart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalSetDurCal()"
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalSet()
      subroutine ESMF_TimeIntervalSetDurCal(timeinterval, calendar, &
        keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout)         :: timeinterval
      type(ESMF_Calendar),     intent(in)            :: calendar
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(in),  optional :: yy
      integer(ESMF_KIND_I8),   intent(in),  optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: mm
      integer(ESMF_KIND_I8),   intent(in),  optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(in),  optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD
      integer(ESMF_KIND_I8),   intent(in),  optional :: sD_i8
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Sets the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally to integers.
!
!     Ranges are limited only by machine word size.  Numeric defaults are 0,
!     except for sD, which is 1.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize.
!     \item[calendar]
!          {\tt Calendar} used to give better definition to 
!          calendar interval (yy, mm, and/or d) for arithmetic, comparison, 
!          and conversion operations.  Allows calendar interval to "float" 
!          across all times on a specific calendar.  Default = NULL; 
!          if startTime also not specified, calendar interval "floats" across 
!          all calendars and times.  Mutually exclusive with startTime since 
!          it contains a calendar.  Alternate to, and mutually exclusive with, 
!          calkindflag below.  Primarily for specifying a custom calendar kind.
!     \item[{[yy]}]
!          Integer year (32-bit).  Default = 0.
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).  Default = 0.
!     \item[{[mm]}]
!          Integer month (32-bit).  Default = 0.
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).  Default = 0.
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).  Default = 0.
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!          Default = 0.
!     \item[{[h]}]
!          Integer hour.  Default = 0.
!     \item[{[m]}]
!          Integer minute.  Default = 0.
!     \item[{[s]}]
!          Integer second (32-bit).  Default = 0.
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).  Default = 0.
!     \item[{[ms]}]
!          Integer millisecond.  Default = 0.
!     \item[{[us]}]
!          Integer microsecond.  Default = 0.
!     \item[{[ns]}]
!          Integer nanosecond.  Default = 0.
!     \item[{[d\_r8]}]
!          Double precision day.  Default = 0.0.
!     \item[{[h\_r8]}]
!          Double precision hour.  Default = 0.0.
!     \item[{[m\_r8]}]
!          Double precision minute.  Default = 0.0.
!     \item[{[s\_r8]}]
!          Double precision second.  Default = 0.0.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.  Default = 0.0.
!     \item[{[us\_r8]}]
!          Double precision microsecond.  Default = 0.0.
!     \item[{[ns\_r8]}]
!          Double precision nanosecond.  Default = 0.0.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!          Default = 0.
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8).
!                                                           (large, 64-bit).
!          Default = 0.
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!          Default = 1.
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8).
!                                                           (large, 64-bit).
!          Default = 1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_CalendarGetInit,calendar,rc)

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSetDurCal(timeinterval, yy, yy_i8, &
                                        mm, mm_i8, &
                                        d, d_i8, h, m, s, s_i8, ms, &
                                        us, ns, d_r8, h_r8, m_r8, s_r8, &
                                        ms_r8, us_r8, ns_r8, &
                                        sN, sN_i8, sD, sD_i8, &
                                        calendar, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output variable as successfully initialized
      call ESMF_TimeIntervalInit(timeinterval)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalSetDurCal

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalSetDurCalTyp()"
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval

! !INTERFACE:
      ! Private name; call using ESMF_TimeIntervalSet()
      subroutine ESMF_TimeIntervalSetDurCalTyp(timeinterval, calkindflag, &
        keywordEnforcer, &
        yy, yy_i8, &
        mm, mm_i8, &
        d, d_i8, &
        h, m, &
        s, s_i8, &
        ms, us, ns, &
        d_r8, h_r8, m_r8, s_r8, &
        ms_r8, us_r8, ns_r8, &
        sN, sN_i8, sD, sD_i8, &
        rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout)         :: timeinterval
      type(ESMF_CalKind_Flag), intent(in)            :: calkindflag
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer(ESMF_KIND_I4),   intent(in),  optional :: yy
      integer(ESMF_KIND_I8),   intent(in),  optional :: yy_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: mm
      integer(ESMF_KIND_I8),   intent(in),  optional :: mm_i8
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
      integer(ESMF_KIND_I8),   intent(in),  optional :: sN_i8
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD
      integer(ESMF_KIND_I8),   intent(in),  optional :: sD_i8
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Sets the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally to integers.
!
!     Ranges are limited only by machine word size.  Numeric defaults are 0,
!     except for sD, which is 1.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize.
!     \item[calkindflag]
!          Alternate to, and mutually exclusive with, 
!          calendar above.  More convenient way of specifying a built-in 
!          calendar kind.
!     \item[{[yy]}]
!          Integer year (32-bit).  Default = 0.
!     \item[{[yy\_i8]}]
!          Integer year (large, 64-bit).  Default = 0.
!     \item[{[mm]}]
!          Integer month (32-bit).  Default = 0.
!     \item[{[mm\_i8]}]
!          Integer month (large, 64-bit).  Default = 0.
!     \item[{[d]}]
!          Integer Julian day, or Modified Julian day (32-bit).  Default = 0.
!     \item[{[d\_i8]}]
!          Integer Julian day, or Modified Julian day (large, 64-bit).
!          Default = 0.
!     \item[{[h]}]
!          Integer hour.  Default = 0.
!     \item[{[m]}]
!          Integer minute.  Default = 0.
!     \item[{[s]}]
!          Integer second (32-bit).  Default = 0.
!     \item[{[s\_i8]}]
!          Integer second (large, 64-bit).  Default = 0.
!     \item[{[ms]}]
!          Integer millisecond.  Default = 0.
!     \item[{[us]}]
!          Integer microsecond.  Default = 0.
!     \item[{[ns]}]
!          Integer nanosecond.  Default = 0.
!     \item[{[d\_r8]}]
!          Double precision day.  Default = 0.0.
!     \item[{[h\_r8]}]
!          Double precision hour.  Default = 0.0.
!     \item[{[m\_r8]}]
!          Double precision minute.  Default = 0.0.
!     \item[{[s\_r8]}]
!          Double precision second.  Default = 0.0.
!     \item[{[ms\_r8]}]
!          Double precision millisecond.  Default = 0.0.
!     \item[{[us\_r8]}]
!          Double precision microsecond.  Default = 0.0.
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.  Default = 0.0.
!     \item[{[sN]}]
!          Integer numerator of fractional second (sN/sD).
!          Default = 0.
!     \item[{[sN\_i8]}]
!          Integer numerator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 0.
!     \item[{[sD]}]
!          Integer denominator of fractional second (sN/sD).
!          Default = 1.
!     \item[{[sD\_i8]}]
!          Integer denominator of fractional second (sN\_i8/sD\_i8)
!                                                           (large, 64-bit).
!          Default = 1.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSetDurCalTyp(timeinterval, yy, yy_i8, &
                                           mm, mm_i8, &
                                           d, d_i8, h, m, s, s_i8, ms, &
                                           us, ns, d_r8, h_r8, m_r8, s_r8, &
                                           ms_r8, us_r8, ns_r8, &
                                           sN, sN_i8, sD, sD_i8, &
                                           calkindflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output variable as successfully initialized
      call ESMF_TimeIntervalInit(timeinterval)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalSetDurCalTyp

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalValidate()"
!BOP
! !IROUTINE:  ESMF_TimeIntervalValidate - Validate a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalValidate(timeinterval, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Checks whether a {\tt timeinterval} is valid.
!     If fractional value, denominator must be non-zero.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          {\tt ESMF\_TimeInterval} to be validated.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code
      character :: options ! dummy options

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,rc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalValidate(timeinterval, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalWriteRestart()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalWriteRestart - Save the contents of a TimeInterval (not implemented)

! !INTERFACE:
      subroutine ESMF_TimeIntervalWriteRestart(timeinterval, keywordEnforcer, &
        rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:  
!     Saves an {\tt ESMF\_TimeInterval} object.  Default options are to select
!     the fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to save.  
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,rc)

      ! invoke C to C++ entry point 
      call c_ESMC_TimeIntervalWriteRestart(timeinterval, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_TimeIntervalWriteRestart

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since Fortran forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalRQuot()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalRQuot - Divide two TimeIntervals, return double precision quotient

! !INTERFACE:
      function ESMF_TimeIntervalRQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_TimeIntervalRQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (/) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(/)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)
   
      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalRQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalRQuot)

      end function ESMF_TimeIntervalRQuot

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalQuotI()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide TimeInterval by an integer, return TimeInterval quotient 

! !INTERFACE:
      function ESMF_TimeIntervalQuotI(timeinterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer(ESMF_KIND_I4),   intent(in) :: divisor

! !DESCRIPTION:
!     This method overloads the (/) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(/)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalQuotI(timeinterval, divisor, &
                                    ESMF_TimeIntervalQuotI)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalQuotI)

      end function ESMF_TimeIntervalQuotI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalQuotR()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalQuotR - Divide TimeInterval by a double precision, return TimeInterval quotient 

! !INTERFACE:
      function ESMF_TimeIntervalQuotR(timeinterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      real(ESMF_KIND_R8),      intent(in) :: divisor

! !DESCRIPTION:
!     This method overloads the (/) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(/)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalQuotR(timeinterval, divisor, &
                                    ESMF_TimeIntervalQuotR)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalQuotR)

      end function ESMF_TimeIntervalQuotR

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalFQuot()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalFQuot - Divide two TimeIntervals, return fraction quotient

! !INTERFACE:
      function ESMF_TimeIntervalFQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_Fraction) :: ESMF_TimeIntervalFQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method defines the (.DIV.) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(.DIV.)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalFQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalFQuot)

      ! mark output as successfully initialized
      call ESMF_FractionInit(ESMF_TimeIntervalFQuot)

      end function ESMF_TimeIntervalFQuot

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalRemainder()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalRemainder - Divide two TimeIntervals, return time interval remainder

! !INTERFACE:
      function ESMF_TimeIntervalRemainder(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalRemainder

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the pre-defined MOD function for the
!     {\tt ESMF\_TimeInterval} class.  See "interface MOD" above for complete
!     description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalRemainder(timeinterval1, timeinterval2, &
                                        ESMF_TimeIntervalRemainder)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalRemainder)

      end function ESMF_TimeIntervalRemainder

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalProdTI()"
!BOPI
! !IROUTINE:   ESMF_TimeIntervalProdTI - Multiply a TimeInterval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdTI(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdTI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer(ESMF_KIND_I4),   intent(in) :: multiplier

! !DESCRIPTION:
!     This method overloads the (*) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(*)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalProdTI(timeinterval, multiplier, &
                                     ESMF_TimeIntervalProdTI)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalProdTI)

      end function ESMF_TimeIntervalProdTI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalProdIT()"
!BOPI
! !IROUTINE:   ESMF_TimeIntervalProdIT - Multiply a TimeInterval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdIT(multiplier, timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdIT

! !ARGUMENTS:
      integer(ESMF_KIND_I4),   intent(in) :: multiplier
      type(ESMF_TimeInterval), intent(in) :: timeinterval

! !DESCRIPTION:
!     This method overloads the (*) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(*)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalProdIT(multiplier, timeinterval, &
                                     ESMF_TimeIntervalProdIT)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalProdIT)

      end function ESMF_TimeIntervalProdIT

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalProdTF()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalProdTF - Multiply a TimeInterval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdTF(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdTF

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      type(ESMF_Fraction),     intent(in) :: multiplier

! !DESCRIPTION:
!     This method overloads the (*) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(*)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_FractionGetInit,multiplier,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalProdTF(timeinterval, multiplier, &
                                     ESMF_TimeIntervalProdTF)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalProdTF)

      end function ESMF_TimeIntervalProdTF

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalProdFT()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalProdFT - Multiply a TimeInterval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdFT(multiplier, timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdFT

! !ARGUMENTS:
      type(ESMF_Fraction),     intent(in) :: multiplier
      type(ESMF_TimeInterval), intent(in) :: timeinterval

! !DESCRIPTION:
!     This method overloads the (*) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(*)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_FractionGetInit,multiplier,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalProdFT(multiplier, timeinterval, &
                                     ESMF_TimeIntervalProdFT)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalProdFT)

      end function ESMF_TimeIntervalProdFT

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalProdTR()"
!BOPI
! !IROUTINE:   ESMF_TimeIntervalProdTR - Multiply a TimeInterval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdTR(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdTR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      real(ESMF_KIND_R8),      intent(in) :: multiplier

! !DESCRIPTION:
!     This method overloads the (*) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(*)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalProdTR(timeinterval, multiplier, &
                                     ESMF_TimeIntervalProdTR)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalProdTR)

      end function ESMF_TimeIntervalProdTR

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalProdRT()"
!BOPI
! !IROUTINE:   ESMF_TimeIntervalProdRT - Multiply a TimeInterval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdRT(multiplier, timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdRT

! !ARGUMENTS:
      real(ESMF_KIND_R8),      intent(in) :: multiplier
      type(ESMF_TimeInterval), intent(in) :: timeinterval

! !DESCRIPTION:
!     This method overloads the (*) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(*)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalProdRT(multiplier, timeinterval, &
                                     ESMF_TimeIntervalProdRT)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalProdRT)

      end function ESMF_TimeIntervalProdRT

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMC_BaseTime class overloaded operators
! internal, private implementation methods
! Note:  these functions do not have a return code, since Fortran forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalSum()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalSum - Add two TimeIntervals 

! !INTERFACE:
      function ESMF_TimeIntervalSum(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (+) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(+)" above for complete description.
!
!EOPI
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalSum(timeinterval1, timeinterval2, &
                                  ESMF_TimeIntervalSum)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalSum)

      end function ESMF_TimeIntervalSum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalDiff()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one TimeInterval from another
   
! !INTERFACE:
      function ESMF_TimeIntervalDiff(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (-) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(-)" above for complete description.
!
!EOPI
! !REQUIREMENTS:
      integer :: localrc

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalDiff(timeinterval1, timeinterval2, &
                                   ESMF_TimeIntervalDiff)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalDiff)

      end function ESMF_TimeIntervalDiff

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalNegate()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalNegate - Perform unary negation on a TimeInterval
   
! !INTERFACE:
      function ESMF_TimeIntervalNegate(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalNegate

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval

! !DESCRIPTION:
!     This method overloads the (-) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(-)" above for complete description.
!
!EOPI
! !REQUIREMENTS:
      integer :: localrc

      ! check input
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalNegate(timeinterval, ESMF_TimeIntervalNegate)

      ! mark output as successfully initialized
      call ESMF_TimeIntervalInit(ESMF_TimeIntervalNegate)

      end function ESMF_TimeIntervalNegate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalEQ()"
!BOPI
! !IROUTINE: ESMF_TimeIntervalEQ - Test if TimeInterval 1 is equal to TimeInterval 2

! !INTERFACE:
      function ESMF_TimeIntervalEQ(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalEQ

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

!DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeIntervalEQ = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalEQ(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalEQ)

      end function ESMF_TimeIntervalEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalNE()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalNE - Test if TimeInterval 1 is not equal to TimeInterval 2

! !INTERFACE:
      function ESMF_TimeIntervalNE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalNE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(/=)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeIntervalNE = .true.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalNE(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalNE)

      end function ESMF_TimeIntervalNE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalLT()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalLT - Test if TimeInterval 1 is less than TimeInterval 2

! !INTERFACE:
      function ESMF_TimeIntervalLT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (<) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(<)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeIntervalLT = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalLT(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalLT)

      end function ESMF_TimeIntervalLT

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalLE()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalLE - Test if TimeInterval 1 is less than or equal to TimeInterval 2

! !INTERFACE:
      function ESMF_TimeIntervalLE(timeinterval1, timeinterval2)

! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (<=) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(<=)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeIntervalLE = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalLE(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalLE)

      end function ESMF_TimeIntervalLE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGT()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalGT - Test if TimeInterval 1 is greater than TimeInterval 2

! !INTERFACE:
      function ESMF_TimeIntervalGT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (>) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(>)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeIntervalGT = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalGT(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalGT)

      end function ESMF_TimeIntervalGT

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TimeIntervalGE()"
!BOPI
! !IROUTINE:  ESMF_TimeIntervalGE - Test if TimeInterval1 is greater than or equal to TimeInterval2

! !INTERFACE:
      function ESMF_TimeIntervalGE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     This method overloads the (>=) operator for the {\tt ESMF\_TimeInterval}
!     class.  See "interface operator(>=)" above for complete description.
!
!EOPI
      integer :: localrc

      ! Initialize output value in case of error
      ESMF_TimeIntervalGE = .false.

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval1,localrc)
      ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_TimeIntervalGetInit,timeinterval2,localrc)

      ! invoke C to C++ entry point
      call c_ESMC_TimeIntervalGE(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalGE)

      end function ESMF_TimeIntervalGE

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalMod
