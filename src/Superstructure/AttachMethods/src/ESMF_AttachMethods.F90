! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
    '$Id$'

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
  subroutine ESMF_MethodStateAdd(state, label, index, userRoutine, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[userRoutine]
!   The user-supplied subroutine to be associated with the {\tt label}.
!
!   The subroutine must have the exact interface shown above
!   for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must
!   match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: indexArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    call c_ESMC_MethodTableAdd(state%statep%methodTable, label, indexArg, &
      userRoutine, localrc)
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
  subroutine ESMF_MethodStateAddShObj(state, label, index, userRoutine, &
    sharedObj, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[userRoutine]
!   Name of user-supplied subroutine to be associated with the {\tt label},
!   specified as a character string.
!
!   The subroutine must have the exact interface shown in {\tt ESMF\_MethodStateAdd}
!   for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must
!   match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}

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
    integer :: indexArg
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    if (present(sharedObj)) then
      call c_ESMC_MethodTableAddShObj(state%statep%methodTable, label, &
        indexArg, userRoutine, sharedObj, localrc)
    else
      call c_ESMC_MethodTableAddShObj(state%statep%methodTable, label, &
        indexArg, userRoutine, emptyString, localrc)
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
  subroutine ESMF_MethodStateExecute(state, label, index, existflag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
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
    integer             :: indexArg
    type(ESMF_Logical)  :: opt_existflag    ! helper variable

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    if (present(existflag)) then
      call c_ESMC_MethodTableExecuteEF(state%statep%methodTable, label, &
        indexArg, state, opt_existflag, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      existflag = opt_existflag ! translate logicals
    else
      call c_ESMC_MethodTableExecute(state%statep%methodTable, label, &
        indexArg, state, localUserRc, localrc)
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
  subroutine ESMF_MethodStateRemove(state, label, index, rc)
!
! !ARGUMENTS:
    type(ESMF_State)                        :: state
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: indexArg
    
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_StateGetInit, state, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    call c_ESMC_MethodTableRemove(state%statep%methodTable, label, indexArg, &
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
#define ESMF_METHOD "ESMF_MethodGridCompAdd"
!BOP
! !IROUTINE: ESMF_MethodAdd - Attach user method to GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodAdd()
  subroutine ESMF_MethodGridCompAdd(gcomp, label, index, userRoutine, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[userRoutine]
!   The user-supplied subroutine to be associated with the {\tt label}.
!
!   The subroutine must have the exact interface shown above
!   for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must
!   match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: indexArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    call c_ESMC_MethodTableAdd(gcomp%compp%methodTable, label, indexArg, &
      userRoutine, localrc)
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
  subroutine ESMF_MethodGridCompAddShObj(gcomp, label, index, userRoutine, &
    sharedObj, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[userRoutine]
!   Name of user-supplied subroutine to be associated with the {\tt label},
!   specified as a character string.
!
!   The subroutine must have the exact interface shown in {\tt ESMF\_MethodGridCompAdd}
!   for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must
!   match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
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
    integer :: indexArg
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    if (present(sharedObj)) then
      call c_ESMC_MethodTableAddShObj(gcomp%compp%methodTable, label, &
        indexArg, userRoutine, sharedObj, localrc)
    else
      call c_ESMC_MethodTableAddShObj(gcomp%compp%methodTable, label, &
        indexArg, userRoutine, emptyString, localrc)
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
  subroutine ESMF_MethodCplCompAdd(cplcomp, label, index, userRoutine, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[userRoutine]
!   The user-supplied subroutine to be associated with the {\tt label}.
!
!   The subroutine must have the exact interface shown above
!   for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must
!   match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: indexArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    call c_ESMC_MethodTableAdd(cplcomp%compp%methodTable, label, indexArg, &
      userRoutine, localrc)
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
  subroutine ESMF_MethodCplCompAddShObj(cplcomp, label, index, userRoutine, &
    sharedObj, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[userRoutine]
!   Name of user-supplied subroutine to be associated with the {\tt label},
!   specified as a character string.
!
!   The subroutine must have the exact interface shown in {\tt ESMF\_MethodCplCompAdd}
!   for the {\tt userRoutine} argument. Arguments in {\tt userRoutine}
!   must not be declared as optional, and the types, intent and order must
!   match.
!   Prior to Fortran-2008, the subroutine must be either a module scope procedure,
!   or an external procedure that has a matching interface block specified for it.
!   An internal procedure which is contained within another procedure must not be used.
!   From Fortran-2008 onwards, an internal procedure contained within either a main program
!   or a module procedure may be used.  If the internal procedure is contained within a
!   module procedure, it is subject to initialization requirements.  See: \ref{sec:AppDriverIntProc}
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
    integer :: indexArg
    character(len=0) :: emptyString

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    if (present(sharedObj)) then
      call c_ESMC_MethodTableAddShObj(cplcomp%compp%methodTable, label, &
        indexArg, userRoutine, sharedObj, localrc)
    else
      call c_ESMC_MethodTableAddShObj(cplcomp%compp%methodTable, label, &
        indexArg, userRoutine, emptyString, localrc)
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
  subroutine ESMF_MethodGridCompExecute(gcomp, label, index, existflag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: gcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
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
    integer             :: indexArg
    type(ESMF_Logical)  :: opt_existflag    ! helper variable

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    if (present(existflag)) then
      call c_ESMC_MethodTableExecuteEF(gcomp%compp%methodTable, label, &
        indexArg, gcomp, opt_existflag, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      existflag = opt_existflag ! translate logicals
    else
      call c_ESMC_MethodTableExecute(gcomp%compp%methodTable, label, indexArg, &
        gcomp, localUserRc, localrc)
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
  subroutine ESMF_MethodCplCompExecute(cplcomp, label, index, existflag, &
    userRc, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
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
    integer             :: indexArg
    type(ESMF_Logical)  :: opt_existflag    ! helper variable

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    if (present(existflag)) then
      call c_ESMC_MethodTableExecuteEF(cplcomp%compp%methodTable, label, &
        indexArg, cplcomp, opt_existflag, localUserRc, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      existflag = opt_existflag ! translate logicals
    else
      call c_ESMC_MethodTableExecute(cplcomp%compp%methodTable, label, &
        indexArg, cplcomp, localUserRc, localrc)
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
  subroutine ESMF_MethodGridCompRemove(gcomp, label, index, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                      :: gcomp
    character(len=*),  intent(in)            :: label
     integer,          intent(in),  optional :: index
   integer,            intent(out), optional :: rc 
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: indexArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    call c_ESMC_MethodTableRemove(gcomp%compp%methodTable, label, indexArg, &
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
#define ESMF_METHOD "ESMF_MethodCplCompRemove"
!BOP
! !IROUTINE: ESMF_MethodRemove - Remove user method attached to CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_MethodRemove()
  subroutine ESMF_MethodCplCompRemove(cplcomp, label, index, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                      :: cplcomp
    character(len=*), intent(in)            :: label
    integer,          intent(in),  optional :: index
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
! \item[{[index]}]
!   Integer modifier to distinguish multiple entries with the same {\tt label}.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: indexArg

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)
    
    indexArg = -987654  ! unique default index
    if (present(index)) indexArg = index
    
    call c_ESMC_MethodTableRemove(cplcomp%compp%methodTable, label, indexArg, &
      localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_AttachMethodsMod
