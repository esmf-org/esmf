! $Id: ESMF_StateVa.F90,v 1.3.2.1 2010/02/05 20:05:07 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateVa.F90"
!
!     ESMF StateVa module
      module ESMF_StateVaMod
!
!==============================================================================
!
! This file contains the State Validate method
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateVaMod - State Va Module
!
! !DESCRIPTION:
!
! The code in this file implements the State Validate method.
!
!
! !USES:
      use ESMF_LogErrMod
      use ESMF_StateTypesMod
      use ESMF_InitMacrosMod
      
      implicit none
      
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateValidate

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateVa.F90,v 1.3.2.1 2010/02/05 20:05:07 svasquez Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================


!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateValidate"
!BOP
! !IROUTINE: ESMF_StateValidate - Check validity of a State
!
! !INTERFACE:
      subroutine ESMF_StateValidate(state, options, rc)
!
! !ARGUMENTS:
      type(ESMF_State) :: state
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Validates that the {\tt state} is internally consistent.
!      Currently this method determines if the {\tt state} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[state]
!       The {\tt ESMF\_State} to validate.
!     \item[{[options]}]
!       Validation options are not yet supported.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!
! TODO: code goes here
!
      character (len=6) :: localopts

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit,state,rc)

      localopts = "brief"
      if (present (options)) then
        if (options /= " ")  &
          localopts = options
      end if

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (.not.associated(state%statep)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (state%statep%st .eq. ESMF_STATE_INVALID) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "State uninitialized or already destroyed", &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StateValidate

!------------------------------------------------------------------------------



      end module ESMF_StateVaMod





