! $Id: ESMF_TimeInterval.F90,v 1.60 2004/06/13 05:25:44 cdeluca Exp $
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
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ implementaion of class {\tt ESMC\_TimeInterval}.
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseTypesMod
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod

      ! for ReadRestart()/WriteRestart()
      use ESMF_IOSpecMod

      ! associated derived types
      use ESMF_FractionMod
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
!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_TimeIntervalSum
      private ESMF_TimeIntervalDiff
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

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_TimeInterval.F90,v 1.60 2004/06/13 05:25:44 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
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
! !DESCRIPTION:
!     Overloads the (-) operator for the {\tt ESMF\_TimeInterval} class to
!     subtract {\tt timeinterval2} from {\tt timeinterval1} and return
!     the difference as an {\tt ESMF\_TimeInterval}.
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
! !DESCRIPTION:
!     Overloads the (/) operator for the {\tt ESMF\_TimeInterval} class to
!     return {\tt timeinterval1} divided by {\tt timeinterval2} as a
!     double precision quotient.
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
!BOPI
! TODO: when implemented, change to BOP/EOP
!
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
!EOPI
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
!     remainder = MOD(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: remainder
!
! !ARGUMENTS: 
!     type(ESMF_TimeInterval), intent(in) :: timeinterval1
!     type(ESMF_TimeInterval), intent(in) :: timeinterval2
!
! !DESCRIPTION:
!     Overloads the pre-defined MOD() function for the {\tt ESMF\_TimeInterval}
!     class to return the remainder of {\tt timeinterval1} divided by
!     {\tt timeinterval2} as an {\tt ESMF\_TimeInterval}.
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
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!     integer(ESMF_KIND_I4),   intent(in) :: multiplier
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to
!     multiply a {\tt timeinterval} by an integer {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}.
!
!     Commutative complement to overloaded operator (*) below.
!     
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]        
!          The multiplicand.
!     \item[mutliplier]
!          The integer multiplier.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTI   ! internal implementation
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2     
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalOperator(*) - Multiply a TimeInterval by an integer
!
! !INTERFACE:
!     interface operator(*)
!     product = multiplier * timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     integer(ESMF_KIND_I4),   intent(in) :: multiplier
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to      
!     multiply a {\tt timeinterval} by an integer {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}. 
!
!     Commutative complement to overloaded operator (*) above.
!
!     The arguments are:
!     \begin{description}
!     \item[mutliplier]
!          The integer multiplier.
!     \item[timeinterval]
!          The multiplicand.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
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
!     Commutative complement to overloaded operator (*) below.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand.
!     \item[mutliplier]
!          The fraction multiplier.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTF   ! internal implementation
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
!     product = multiplier * timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     type(ESMF_Fraction),     intent(in) :: multiplier
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to
!     multiply a {\tt timeinterval} by a fraction {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}.
!
!     Commutative complement to overloaded operator (*) above.
!
!     The arguments are:
!     \begin{description}
!     \item[mutliplier]
!          The fraction multiplier.
!     \item[timeinterval]
!          The multiplicand.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
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
!     Commutative complement to overloaded operator (*) below.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand.
!     \item[mutliplier]
!          The double precision multiplier.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdTR   ! internal implementation
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
!     product = multiplier * timeinterval
!
! !RETURN VALUE:
!     type(ESMF_TimeInterval) :: product
!
! !ARGUMENTS:
!     real(ESMF_KIND_R8),      intent(in) :: multiplier
!     type(ESMF_TimeInterval), intent(in) :: timeinterval
!
! !DESCRIPTION:
!     Overloads the (*) operator for the {\tt ESMF\_TimeInterval} class to
!     multiply a {\tt timeinterval} by a double precision {\tt multiplier},
!     and return the product as an {\tt ESMF\_TimeInterval}.
!
!     Commutative complement to overloaded operator (*) above.
!
!     The arguments are:
!     \begin{description}
!     \item[mutliplier]
!          The double precision multiplier.
!     \item[timeinterval]
!          The multiplicand.
!     \end{description}
!
!EOPI
! !PRIVATE MEMBER FUNCTIONS:
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
!DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_TimeInterval} class to
!     return true if {\tt timeinterval1} and {\tt timeinterval2} are equal,
!     and false otherwise.
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
!DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_TimeInterval} class to
!     return true if {\tt timeinterval1} and {\tt timeinterval2} are not equal,
!     and false otherwise.
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
!DESCRIPTION:
!     Overloads the (<) operator for the {\tt ESMF\_TimeInterval} class to
!     return true if {\tt timeinterval1} is less than {\tt timeinterval2},
!     and false otherwise.
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
!DESCRIPTION:
!     Overloads the (<=) operator for the {\tt ESMF\_TimeInterval} class to
!     return true if {\tt timeinterval1} is less than or equal to
!     {\tt timeinterval2}, and false otherwise.
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
!DESCRIPTION:
!     Overloads the (<) operator for the {\tt ESMF\_TimeInterval} class to
!     return true if {\tt timeinterval1} is greater than {\tt timeinterval2},
!     and false otherwise.
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
!DESCRIPTION:
!     Overloads the (<=) operator for the {\tt ESMF\_TimeInterval} class to
!     return true if {\tt timeinterval1} is greater than or equal to
!     {\tt timeinterval2}, and false otherwise.
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
!==============================================================================

      contains

!==============================================================================
!BOP
! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a TimeInterval

! !INTERFACE:
      function ESMF_TimeIntervalAbsValue(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval

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
    
      call c_ESMC_TimeIntervalAbsValue(timeinterval, ESMF_TimeIntervalAbsValue)

      end function ESMF_TimeIntervalAbsValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get a TimeInterval value 

! !INTERFACE:
      subroutine ESMF_TimeIntervalGet(timeinterval, &
                                      yy, yy_i8, &
                                      mm, mm_i8, &
                                      d, d_i8, &
                                      h, m, &
                                      s, s_i8, &
                                      ms, us, ns, &
                                      d_r8, h_r8, m_r8, s_r8, &
                                      ms_r8, us_r8, ns_r8, &
                                      sN, sD, &
                                      startTime, endTime, &
                                      calendar, calendarType, &
                                      startTimeIn, endTimeIn, &
                                      calendarIn, calendarTypeIn, &
                                      timeString, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
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
      integer(ESMF_KIND_I4),   intent(out), optional :: ms    ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: us    ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: ns    ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: d_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: h_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: m_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: s_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: ms_r8 ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: us_r8 ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: ns_r8 ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: sN    ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: sD    ! not implemented
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Time),         intent(out), optional :: endTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalendarType), intent(out), optional :: calendarType
      type(ESMF_Time),         intent(in),  optional :: startTimeIn    ! Input
      type(ESMF_Time),         intent(in),  optional :: endTimeIn      ! Input
      type(ESMF_Calendar),     intent(in),  optional :: calendarIn     ! Input
      type(ESMF_CalendarType), intent(in),  optional :: calendarTypeIn ! Input
      character (len=*),       intent(out), optional :: timeString
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the value of {\tt timeinterval} in units specified by the
!     user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally from integers.
!     (Fractions and reals not implemented yet).
!
!     Units are bound (normalized) to the next larger unit specified.  For
!     example, if a time interval is defined to be one day, then
!     {\tt ESMF\_TimeIntervalGet(d = days, s = seconds)} would return
!       {\tt days = 1}, {\tt seconds = 0},
!     whereas {\tt ESMF\_TimeIntervalGet(s = seconds)} would return
!       {\tt seconds = 86400}.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!     
!     For timeString, converts {\tt ESMF\_TimeInterval}'s value into ISO 8601
!     format PyYmMdDThHmMsS.  See ~\cite{ISO}.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query.
!     \item[{[yy]}]
!          Integer years (>= 32-bit).
!     \item[{[yy\_i8]}]
!          Integer years (large, >= 64-bit).
!     \item[{[mm]}]
!          Integer months (>= 32-bit).
!     \item[{[mm\_i8]}]
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
!          Integer milliseconds.  (Not implemented yet).
!     \item[{[us]}]
!          Integer microseconds.  (Not implemented yet).
!     \item[{[ns]}]
!          Integer nanoseconds.  (Not implemented yet).
!     \item[{[d\_r8]}]
!          Double precision days.  (Not implemented yet).
!     \item[{[h\_r8]}]
!          Double precision hours.  (Not implemented yet).
!     \item[{[m\_r8]}]
!          Double precision minutes.  (Not implemented yet).
!     \item[{[s\_r8]}]
!          Double precision seconds.  (Not implemented yet).
!     \item[{[ms\_r8]}]
!          Double precision milliseconds.  (Not implemented yet).
!     \item[{[us\_r8]}]
!          Double precision microseconds.  (Not implemented yet).
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.  (Not implemented yet).
!     \item[{[sN]}]
!          Integer numerator portion of fractional seconds (sN/sD).
!          (Not implemented yet).
!     \item[{[sD]}]
!          Integer denominator portion of fractional seconds (sN/sD).
!          (Not implemented yet).
!     \item[{[startTime]}]
!          Starting time, if set, of an absolute calendar interval
!          (yy, mm, and/or d).
!     \item[{[endTime]}]
!          Ending time, if set, of an absolute calendar interval
!          (yy, mm, and/or d).
!     \item[{[calendar]}]
!          Associated {\tt Calendar}, if any.
!     \item[{[calendarType]}]
!          Associated {\tt CalendarType}, if any.
!     \item[{[startTimeIn]}]
!          INPUT argument:  pins a calendar interval to a specific point
!          in time to allow conversion between relative units (yy, mm, d) and
!          absolute units (d, h, m, s).  Overrides any startTime and/or endTime
!          previously set.  Mutually exclusive with endTimeIn and calendarIn.
!     \item[{[endTimeIn]}]
!          INPUT argument:  pins a calendar interval to a specific point
!          in time to allow conversion between relative units (yy, mm, d) and
!          absolute units (d, h, m, s).  Overrides any startTime and/or endTime
!          previously set.  Mutually exclusive with startTimeIn and calendarIn.
!     \item[{[calendarIn]}]
!          INPUT argument:  pins a calendar interval to a specific calendar
!          to allow conversion between relative units (yy, mm, d) and
!          absolute units (d, h, m, s).  Mutually exclusive with startTimeIn
!          and endTimeIn since they contain a calendar.  Alternate to, and
!          mutually exclusive with, calendarTypeIn below.  Primarily for
!          specifying a custom calendar type.
!     \item[{[calendarTypeIn]}]
!          INPUT argument:  Alternate to, and mutually exclusive with,
!          calendarIn above.  More convenient way of specifying a built-in
!          calendar type.
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
      call c_ESMC_TimeIntervalGet(timeinterval, yy, yy_i8, mm, mm_i8, &
                                  d, d_i8, h, m, s, s_i8, ms, &
                                  us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sD, &
                                  startTime, endTime, &
                                  calendar, calendarType, &
                                  startTimeIn, endTimeIn, &
                                  calendarIn, calendarTypeIn, &
                                  timeString, rc)
    
      end subroutine ESMF_TimeIntervalGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Get the negative absolute value of a TimeInterval

! !INTERFACE:
      function ESMF_TimeIntervalNegAbsValue(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalNegAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval

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
    
      call c_ESMC_TimeIntervalNegAbsValue(timeinterval, &
                                          ESMF_TimeIntervalNegAbsValue)

      end function ESMF_TimeIntervalNegAbsValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print the contents of a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalPrint(timeinterval, options, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      character (len=*),       intent(in),  optional :: options
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Prints out the contents of an {\tt ESMF\_TimeInterval}, in support of
!     testing and debugging.  The options control the type of information and
!     level of detail.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to be printed out.
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
    
      call c_ESMC_TimeIntervalPrint(timeinterval, options, rc)

      end subroutine ESMF_TimeIntervalPrint

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_TimeIntervalReadRestart - Restore the contents of a TimeInterval (not implmented)

! !INTERFACE:
      subroutine ESMF_TimeIntervalReadRestart(timeinterval, name, iospec, rc)
!
! !ARGUMENTS:      
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      character (len=*),       intent(in)            :: name
      type(ESMF_IOSpec),       intent(in),  optional :: iospec
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
!     \item[{[iospec]}]  
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     TMGn.n.n

      ! get length of given name for C++ validation
      integer :: nameLen
      nameLen = len_trim(name)

!     invoke C to C++ entry point to restore timeinterval
      call c_ESMC_TimeIntervalReadRestart(timeinterval, nameLen, name, &
                                          iospec, rc)

      end subroutine ESMF_TimeIntervalReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize or set a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalSet(timeinterval, &
                                      yy, yy_i8, &
                                      mm, mm_i8, &
                                      d, d_i8, &
                                      h, m, &
                                      s, s_i8, &
                                      ms, us, ns, &
                                      d_r8, h_r8, m_r8, s_r8, &
                                      ms_r8, us_r8, ns_r8, &
                                      sN, sD, &
                                      startTime, endTime, &
                                      calendar, calendarType, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(inout)         :: timeinterval
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
      integer(ESMF_KIND_I4),   intent(in),  optional :: ms    ! not implemented
      integer(ESMF_KIND_I4),   intent(in),  optional :: us    ! not implemented
      integer(ESMF_KIND_I4),   intent(in),  optional :: ns    ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: d_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: h_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: m_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: s_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: ms_r8 ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: us_r8 ! not implemented
      real(ESMF_KIND_R8),      intent(in),  optional :: ns_r8 ! not implemented
      integer(ESMF_KIND_I4),   intent(in),  optional :: sN    ! not implemented
      integer(ESMF_KIND_I4),   intent(in),  optional :: sD    ! not implemented
      type(ESMF_Time),         intent(in),  optional :: startTime
      type(ESMF_Time),         intent(in),  optional :: endTime
      type(ESMF_Calendar),     intent(in),  optional :: calendar
      type(ESMF_CalendarType), intent(in),  optional :: calendarType
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via Fortran optional arguments.
!
!     The ESMF Time Manager represents and manipulates time internally with
!     integers to maintain precision.  Hence, user-specified floating point
!     values are converted internally to integers.
!     (Fractions and reals not implemented yet).
!
!     Ranges are limited only by machine word size.  Numeric defaults are 0,
!     except for sD, which is 1.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize.
!     \item[{[yy]}]
!          Integer years (>= 32-bit).  Default = 0
!     \item[{[yy\_i8]}]
!          Integer years (large, >= 64-bit).  Default = 0
!     \item[{[mm]}]
!          Integer months (>= 32-bit).  Default = 0
!     \item[{[mm\_i8]}]
!          Integer months (large, >= 64-bit).  Default = 0
!     \item[{[d]}]
!          Integer Julian days (>= 32-bit).  Default = 0
!     \item[{[d\_i8]}]
!          Integer Julian days (large, >= 64-bit).  Default = 0
!     \item[{[h]}]
!          Integer hours.  Default = 0
!     \item[{[m]}]
!          Integer minutes.  Default = 0
!     \item[{[s]}]
!          Integer seconds (>= 32-bit).  Default = 0
!     \item[{[s\_i8]}]
!          Integer seconds (large, >= 64-bit).  Default = 0
!     \item[{[ms]}]
!          Integer milliseconds.  Default = 0.  (Not implemented yet).
!     \item[{[us]}]
!          Integer microseconds.  Default = 0.  (Not implemented yet).
!     \item[{[ns]}]
!          Integer nanoseconds.  Default = 0.  (Not implemented yet).
!     \item[{[d\_r8]}]
!          Double precision days.  Default = 0.0.  (Not implemented yet).
!     \item[{[h\_r8]}]
!          Double precision hours.  Default = 0.0.  (Not implemented yet).
!     \item[{[m\_r8]}]
!          Double precision minutes.  Default = 0.0.  (Not implemented yet).
!     \item[{[s\_r8]}]
!          Double precision seconds.  Default = 0.0.  (Not implemented yet).
!     \item[{[ms\_r8]}]
!          Double precision milliseconds.  Default = 0.0. (Not implemented yet).
!     \item[{[us\_r8]}]
!          Double precision microseconds.  Default = 0.0. (Not implemented yet).
!     \item[{[ns\_r8]}]
!          Double precision nanoseconds.  Default = 0.0.  (Not implemented yet).
!     \item[{[sN]}]
!          Integer numerator portion of fractional seconds (sN/sD).
!          Default = 0.  (Not implemented yet).
!     \item[{[sD]}]
!          Integer denominator portion of fractional seconds (sN/sD).
!          Default = 1.  (Not implemented yet).
!     \item[{[startTime]}]
!          Starting time of an absolute calendar interval (yy, mm, and/or d);
!          pins a calendar interval to a specific point in time.  If not set,
!          and endTime and calendar also not set, calendar interval "floats"
!          across all calendars and times.
!          Mutually exclusive with calendar since it contains a calendar.
!     \item[{[endTime]}]
!          Ending time of an absolute calendar interval (yy, mm, and/or d);
!          pins a calendar interval to a specific point in time.  If not set,
!          and startTime and calendar also not set, calendar interval "floats"
!          across all calendars and times.
!          Mutually exclusive with calendar since it contains a calendar.
!     \item[{[calendar]}]
!          {\tt Calendar} used to give better definition to calendar interval
!          (yy, mm, and/or d) for arithmetic, comparison, and conversion
!          operations.  Allows calendar interval to "float" across all times
!          on a specific calendar.
!          Default = NULL; if startTime and endTime also not specified, calendar
!          interval "floats" across all calendars and times.
!          Mutually exclusive with startTime and endTime since they contain
!          a calendar.  Alternate to, and mutually exclusive with, calendarType
!          below.  Primarily for specifying a custom calendar type.
!     \item[{[calendarType]}]
!          Alternate to, and mutually exclusive with, calendar above.  More
!          convenient way of specifying a built-in calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSet(timeinterval, yy, yy_i8, mm, mm_i8, &
                                  d, d_i8, h, m, s, s_i8, ms, &
                                  us, ns, d_r8, h_r8, m_r8, s_r8, ms_r8, &
                                  us_r8, ns_r8, sN, sD, &
                                  startTime, endTime, &
                                  calendar, calendarType, rc)

      end subroutine ESMF_TimeIntervalSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalValidate - Validate a TimeInterval

! !INTERFACE:
      subroutine ESMF_TimeIntervalValidate(timeinterval, options, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      character (len=*),       intent(in),  optional :: options
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Checks whether a {\tt timeinterval} is valid.  The options control
!     the type of validation.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          {\tt ESMF\_TimeInterval} to be validated.
!     \item[{[options]}]
!          Validation options.  TODO:  To be determined.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_TimeIntervalValidate(timeinterval, options, rc)

      end subroutine ESMF_TimeIntervalValidate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_TimeIntervalWriteRestart - Save the contents of a TimeInterval (not implemented)

! !INTERFACE:
      subroutine ESMF_TimeIntervalWriteRestart(timeinterval, iospec, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeinterval
      type(ESMF_IOSpec),       intent(in),  optional :: iospec
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:  
!     Saves an {\tt ESMF\_TimeInterval} object.  Default options are to select
!     the fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to save.  
!     \item[{[iospec]}]  
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point 
      call c_ESMC_TimeIntervalWriteRestart(timeinterval, iospec, rc)

      end subroutine ESMF_TimeIntervalWriteRestart

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since Fortran forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalRQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalRQuot)

      end function ESMF_TimeIntervalRQuot

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalQuotI(timeinterval, divisor, &
                                    ESMF_TimeIntervalQuotI)

      end function ESMF_TimeIntervalQuotI

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalQuotR(timeinterval, divisor, &
                                    ESMF_TimeIntervalQuotR)

      end function ESMF_TimeIntervalQuotR

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalFQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalFQuot)

      end function ESMF_TimeIntervalFQuot

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalRemainder(timeinterval1, timeinterval2, &
                                        ESMF_TimeIntervalRemainder)

      end function ESMF_TimeIntervalRemainder

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalProdTI(timeinterval, multiplier, &
                                     ESMF_TimeIntervalProdTI)

      end function ESMF_TimeIntervalProdTI

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalProdIT(multiplier, timeinterval, &
                                     ESMF_TimeIntervalProdIT)

      end function ESMF_TimeIntervalProdIT

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalProdTF(timeinterval, multiplier, &
                                     ESMF_TimeIntervalProdTF)

      end function ESMF_TimeIntervalProdTF

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalProdFT(multiplier, timeinterval, &
                                     ESMF_TimeIntervalProdFT)

      end function ESMF_TimeIntervalProdFT

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalProdTR(timeinterval, multiplier, &
                                     ESMF_TimeIntervalProdTR)

      end function ESMF_TimeIntervalProdTR

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalProdRT(multiplier, timeinterval, &
                                     ESMF_TimeIntervalProdRT)

      end function ESMF_TimeIntervalProdRT

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMF_BaseTime class overloaded operators
! internal, private implementation methods
! Note:  these functions do not have a return code, since Fortran forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalSum(timeinterval1, timeinterval2, &
                                  ESMF_TimeIntervalSum)

      end function ESMF_TimeIntervalSum

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalDiff(timeinterval1, timeinterval2, &
                                   ESMF_TimeIntervalDiff)

      end function ESMF_TimeIntervalDiff

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalEQ(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalEQ)

      end function ESMF_TimeIntervalEQ

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalNE(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalNE)

      end function ESMF_TimeIntervalNE

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalLT(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalLT)

      end function ESMF_TimeIntervalLT

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalLE(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalLE)

      end function ESMF_TimeIntervalLE

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalGT(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalGT)

      end function ESMF_TimeIntervalGT

!------------------------------------------------------------------------------
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

      call c_ESMC_TimeIntervalGE(timeinterval1, timeinterval2, &
                                 ESMF_TimeIntervalGE)

      end function ESMF_TimeIntervalGE

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalMod
