! $Id: ESMF_AttachMethods.F90,v 1.2 2011/04/28 15:08:15 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_AttachMethods.F90"
!
!     ESMF Attachable Methods module
module ESMF_AttachMethodsMod
!
!==============================================================================
!
! This file contains the Attachable Methods implementation
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_AttachMethodsMod - 
!
! !DESCRIPTION:
!
!
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_StateTypesMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_CplCompMod
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
    '$Id: ESMF_AttachMethods.F90,v 1.2 2011/04/28 15:08:15 theurich Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface ESMF_MethodAdd
    module procedure ESMF_MethodStateAdd
    module procedure ESMF_MethodStateAddShObj    
    module procedure ESMF_MethodGridCompAdd
    module procedure ESMF_MethodGridCompAddShObj    
    module procedure ESMF_MethodCplCompAdd
    module procedure ESMF_MethodCplCompAddShObj    
  end interface
  
  interface ESMF_MethodExecute
    module procedure ESMF_MethodStateExecute
    module procedure ESMF_MethodGridCompExecute
    module procedure ESMF_MethodCplCompExecute
  end interface

  interface ESMF_MethodRemove
    module procedure ESMF_MethodStateRemove
    module procedure ESMF_MethodGridCompRemove
    module procedure ESMF_MethodCplCompRemove
  end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodStateAdd"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method to State
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodStateAdd(state, label, userRoutine, rc)
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
!   The {\tt ESMF\_State} to attach to.
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
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodStateAddShObj"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method, located in shared object, to State
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodStateAddShObj(state, label, userRoutine, sharedObj, rc)
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
!   The {\tt ESMF\_State} to attach to.
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
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodExecuteState"
!BOP
! !IROUTINE: ESMF_MethodExecute - Execute user method attached to State
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodExecute()
  subroutine ESMF_MethodStateExecute(state, label, existflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    logical,          intent(out), optional :: existflag
    integer,          intent(out), optional :: userRc
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
! Execute attached method.
!
! The arguments are:
! \begin{description}
! \item[state]
!   The {\tt ESMF\_State} to attach to.
! \item[label]
!   Label of method.
! \item[{[existflag]}]
!   Returned {\tt .true.} indicates that the method specified by {\tt label}
!   exists and was executed. A return value of {\tt .false.} indicates that
!   the method does not exist and consequently was not executed. By default,
!   i.e. if {\tt existflag} was not specified, the latter condition will lead
!   to {\tt rc} not equal {\tt ESMF\_SUCCESS} being returned. However, if
!   {\tt existflag} was specified, a method not existing is not an error
!   condition.
! \item[{[userRc]}]
!   Return code set by attached method before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer             :: localrc          ! local error status
    integer             :: localUserRc
    type(ESMF_Logical)  :: opt_existflag    ! helper variable

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    if (present(existflag)) then
      call c_ESMC_MethodTableExecuteEF(state%statep%methodTable, label, state, &
        opt_existflag, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      existflag = opt_existflag ! translate logicals
    else
      call c_ESMC_MethodTableExecute(state%statep%methodTable, label, state, &
        localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodStateRemove"
!BOP
! !IROUTINE: ESMF_MethodRemove - Remove user method attached to State
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodRemove()
  subroutine ESMF_MethodStateRemove(state, label, rc)
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
!   The {\tt ESMF\_State} to attach to.
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
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodGridCompAdd"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method to GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodGridCompAdd(gcomp, label, userRoutine, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
    character(len=*), intent(in)            :: label
    interface
      subroutine userRoutine(gcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_GridComp)         :: gcomp        ! must not be optional
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
! \item[gcomp]
!   The {\tt ESMF\_GridComp} to attach to.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    call c_ESMC_MethodTableAdd(gcomp%compp%methodTable, label, userRoutine, &
      localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodGridCompAddShObj"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method, located in shared object, to GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodGridCompAddShObj(gcomp, label, userRoutine, sharedObj, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
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
! \item[gcomp]
!   The {\tt ESMF\_GridComp} to attach to.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    if (present(sharedObj)) then
      call c_ESMC_MethodTableAddShObj(gcomp%compp%methodTable, label, &
        userRoutine, sharedObj, localrc)
    else
      call c_ESMC_MethodTableAddShObj(gcomp%compp%methodTable, label, &
        userRoutine, emptyString, localrc)
    endif
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodCplCompAdd"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method to CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodCplCompAdd(cplcomp, label, userRoutine, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    interface
      subroutine userRoutine(cplcomp, rc)
        use ESMF_CompMod
        implicit none
        type(ESMF_CplComp)          :: cplcomp      ! must not be optional
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
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} to attach to.
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

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    call c_ESMC_MethodTableAdd(cplcomp%compp%methodTable, label, userRoutine, &
      localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodCplCompAddShObj"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method, located in shared object, to CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodCplCompAddShObj(cplcomp, label, userRoutine, sharedObj, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
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
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} to attach to.
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

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    if (present(sharedObj)) then
      call c_ESMC_MethodTableAddShObj(cplcomp%compp%methodTable, label, &
        userRoutine, sharedObj, localrc)
    else
      call c_ESMC_MethodTableAddShObj(cplcomp%compp%methodTable, label, &
        userRoutine, emptyString, localrc)
    endif
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodExecuteGridComp"
!BOP
! !IROUTINE: ESMF_MethodExecute - Execute user method attached to GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodExecute()
  subroutine ESMF_MethodGridCompExecute(gcomp, label, existflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
    character(len=*), intent(in)            :: label
    logical,          intent(out), optional :: existflag
    integer,          intent(out), optional :: userRc
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
! Execute attached method.
!
! The arguments are:
! \begin{description}
! \item[gcomp]
!   The {\tt ESMF\_GridComp} to attach to.
! \item[label]
!   Label of method.
! \item[{[existflag]}]
!   Returned {\tt .true.} indicates that the method specified by {\tt label}
!   exists and was executed. A return value of {\tt .false.} indicates that
!   the method does not exist and consequently was not executed. By default,
!   i.e. if {\tt existflag} was not specified, the latter condition will lead
!   to {\tt rc} not equal {\tt ESMF\_SUCCESS} being returned. However, if
!   {\tt existflag} was specified, a method not existing is not an error
!   condition.
! \item[{[userRc]}]
!   Return code set by attached method before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer             :: localrc          ! local error status
    integer             :: localUserRc
    type(ESMF_Logical)  :: opt_existflag    ! helper variable

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    if (present(existflag)) then
      call c_ESMC_MethodTableExecuteEF(gcomp%compp%methodTable, label, gcomp, &
        opt_existflag, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      existflag = opt_existflag ! translate logicals
    else
      call c_ESMC_MethodTableExecute(gcomp%compp%methodTable, label, gcomp, &
        localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
 
    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodExecuteCplComp"
!BOP
! !IROUTINE: ESMF_MethodExecute - Execute user method attached to CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodExecute()
  subroutine ESMF_MethodCplCompExecute(cplcomp, label, existflag, userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    logical,          intent(out), optional :: existflag
    integer,          intent(out), optional :: userRc
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
! Execute attached method.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} to attach to.
! \item[label]
!   Label of method.
! \item[{[existflag]}]
!   Returned {\tt .true.} indicates that the method specified by {\tt label}
!   exists and was executed. A return value of {\tt .false.} indicates that
!   the method does not exist and consequently was not executed. By default,
!   i.e. if {\tt existflag} was not specified, the latter condition will lead
!   to {\tt rc} not equal {\tt ESMF\_SUCCESS} being returned. However, if
!   {\tt existflag} was specified, a method not existing is not an error
!   condition.
! \item[{[userRc]}]
!   Return code set by attached method before returning.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer             :: localrc          ! local error status
    integer             :: localUserRc
    type(ESMF_Logical)  :: opt_existflag    ! helper variable

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    if (present(existflag)) then
      call c_ESMC_MethodTableExecuteEF(cplcomp%compp%methodTable, label, &
        cplcomp, opt_existflag, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      existflag = opt_existflag ! translate logicals
    else
      call c_ESMC_MethodTableExecute(cplcomp%compp%methodTable, label, &
        cplcomp, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
 
    ! pass back userRc
    if (present(userRc)) userRc = localUserRc

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodGridCompRemove"
!BOP
! !IROUTINE: ESMF_MethodRemove - Remove user method attached to GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodRemove()
  subroutine ESMF_MethodGridCompRemove(gcomp, label, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
    character(len=*), intent(in)            :: label
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Remove attached method.
!
! The arguments are:
! \begin{description}
! \item[gcomp]
!   The {\tt ESMF\_GridComp} to attach to.
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

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    call c_ESMC_MethodTableRemove(gcomp%compp%methodTable, label, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MethodCplCompRemove"
!BOP
! !IROUTINE: ESMF_MethodRemove - Remove user method attached to CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodRemove()
  subroutine ESMF_MethodCplCompRemove(cplcomp, label, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
! Remove attached method.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} to attach to.
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

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    call c_ESMC_MethodTableRemove(cplcomp%compp%methodTable, label, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_AttachMethodsMod
