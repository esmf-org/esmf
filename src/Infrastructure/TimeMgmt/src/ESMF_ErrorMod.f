! $Id: ESMF_ErrorMod.f,v 1.1 2003/03/11 03:17:51 cdeluca Exp $
	module ESMF_ErrorMod
!===============================================================================
!BOP
! !MODULE: ESMF_ErrorMod
!
! !USES:
!
! !PUBLIC TYPES:
	implicit none
!
! !PUBLIC MEMBER FUNCTIONS:
!     ESMF_ErrHandlerSetType    
!     ESMF_GetErrString
!     ESMF_ErrorTest

! !DESCRIPTION:
! Simple error handling routines for setting the type of handler, and
! retrieving error messages.
!
!EOP
!===============================================================================
	contains
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_ErrHandlerSetType
!
! !INTERFACE:
      subroutine ESMF_ErrHandlerSetType(type)
    
! !PARAMETERS:
      integer, intent(in) :: type        ! Type of error handling desired 

! !DESCRIPTION:
!     Sets the type of error handling to either {\tt ESMF\_ERR\_RETURN} or
!     {\tt ESMF\_ERR\_EXIT}.  The type of error handling is independent of 
!     whether the optional return code is included in a the argument list 
!     of a procedure call.
!
!EOP
!-------------------------------------------------------------------------------

      call ESMC_ErrHandlerSetType(type) 
      end subroutine ESMF_ErrHandlerSetType

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_ErrPrint
!
! !INTERFACE:
      subroutine ESMF_ErrPrint(rc)
    
! !PARAMETERS:
      integer, intent(in) :: rc        ! Return code to be translated 

! !DESCRIPTION:
!     Prints an error message corresponding to an integer return code.
!
!EOP
!-------------------------------------------------------------------------------

      call ESMC_ErrPrint(rc) 
      end subroutine ESMF_ErrPrint

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_ErrorTest
!
! !INTERFACE:
      subroutine ESMF_ErrorTest(test, str)
      
! !PARAMETERS:
      logical, intent(in) :: test          ! evaluated expression
      character(60), intent(in) :: str     ! string description of test

! !DESCRIPTION:
!     Prints a {\tt PASS TEST} message to stdout if {\tt test} is {\tt .TRUE.}, 
!     and a {\tt FAIL TEST} message if {\tt test} is false.  
!
!EOP
!-------------------------------------------------------------------------------
      if(test) then
        print *, "PASS TEST: ", str
      else
        print *, "FAIL TEST: ", str
      end if

      end subroutine ESMF_ErrorTest
!===============================================================================

	end module ESMF_ErrorMod
