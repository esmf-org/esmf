! $Id: ESMF_AttachMethods.F90,v 1.1.2.1 2010/02/05 20:04:52 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_AttachMethods.F90"
!
!     ESMF State module
module ESMF_AttachMethodsMod
!
!==============================================================================
!
! This file contains the State class definition and all State
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateMod - Data exchange between components
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to the {\tt State} class and associated data structures.
!
!
! !USES:
  use ESMF_UtilTypesMod
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
      
  public ESMF_MethodAdd, ESMF_MethodExecute, ESMF_MethodRemove

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_AttachMethods.F90,v 1.1.2.1 2010/02/05 20:04:52 svasquez Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface ESMF_MethodAdd
    module procedure ESMF_MethodAdd
    module procedure ESMF_MethodAddShObj    
  end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodAdd"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodAdd(state, label, userRoutine, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    interface
      subroutine userRoutine(state, rc)
        use ESMF_StateMod
        implicit none
        type(ESMF_State)            :: state        ! must not be optional
        integer, intent(out)        :: rc           ! must not be optional
      end subroutine
    end interface
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Attach {\tt userRoutine}.
!
! The arguments are:
! \begin{description}
! \item[state]
!   The {\tt ESMF\_State} to print.
! \item[label]
!   Label of method.
! \item[userRoutine]
!   The user-supplied subroutine to be associated with the {\tt label}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    call c_ESMC_MethodTableAdd(state%statep%methodTable, label, userRoutine, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodAdd"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method, located in shared object
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodAddShObj(state, label, userRoutine, sharedObj, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    character(len=*), intent(in)            :: userRoutine
    character(len=*), intent(in),  optional :: sharedObj
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Attach {\tt userRoutine}.
!
! The arguments are:
! \begin{description}
! \item[state]
!   The {\tt ESMF\_State} to print.
! \item[label]
!   Label of method.
! \item[userRoutine]
!   Name of user-supplied subroutine to be associated with the {\tt label}.
! \item[{[sharedObj]}]
!   Name of shared object that contains {\tt userRoutine}. If the
!   {\tt sharedObj} argument is not provided the executable itself will be
!   searched for {\tt userRoutine}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    if (present(sharedObj)) then
      call c_ESMC_MethodTableAddShObj(state%statep%methodTable, label, &
        userRoutine, sharedObj, localrc)
    else
      call c_ESMC_MethodTableAddShObj(state%statep%methodTable, label, &
        userRoutine, emptyString, localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodExecute"
!BOP
! !IROUTINE: ESMF_MethodExecute - Execute user method
!
! !INTERFACE:
  subroutine ESMF_MethodExecute(state, label, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    integer,          intent(out), optional :: userRc
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
! Execute attached method.
!
! The arguments are:
! \begin{description}
! \item[state]
!   The {\tt ESMF\_State} to print.
! \item[label]
!   Label of method.
! \item[{[userRc]}]
!   Return code set by attached method before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: localUserRc

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    call c_ESMC_MethodTableExecute(state%statep%methodTable, label, state, &
      localUserRc, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
 
    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodRemove"
!BOP
! !IROUTINE: ESMF_MethodRemove - Remove user method
!
! !INTERFACE:
  subroutine ESMF_MethodRemove(state, label, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Remove attached method.
!
! The arguments are:
! \begin{description}
! \item[state]
!   The {\tt ESMF\_State} to print.
! \item[label]
!   Label of method.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    call c_ESMC_MethodTableRemove(state%statep%methodTable, label, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_AttachMethodsMod
