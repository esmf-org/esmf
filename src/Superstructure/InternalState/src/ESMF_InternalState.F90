! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_InternalState.F90"
!==============================================================================
!
! ESMF InternalState module
module ESMF_InternalStateMod
!
!==============================================================================
!
! This file contains the InternalState implementation
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_InternalStateMod -
!
! !DESCRIPTION:
!
!
! !USES:
  use iso_c_binding

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

  public ESMF_InternalStateAdd
  public ESMF_InternalStateGet

#ifndef ESMF_NO_F2018ASSUMEDTYPE
  public ESMF_GridCompGetInternalState, ESMF_GridCompSetInternalState
  public ESMF_CplCompGetInternalState,  ESMF_CplCompSetInternalState
  public ESMF_UserCompGetInternalState, ESMF_UserCompSetInternalState
#endif

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
! ! The InternalState functionality is implemented on the C++ side. Fortran
! ! interfaces are defined here.

  interface ESMF_InternalStateAdd
    module procedure ESMF_InternalStateCplCompAdd
    module procedure ESMF_InternalStateGridCompAdd
  end interface

  interface ESMF_InternalStateGet
    module procedure ESMF_InternalStateCplCompGet
    module procedure ESMF_InternalStateGridCompGet
    module procedure ESMF_InternalStateCplCompGetL
    module procedure ESMF_InternalStateGridCompGetL
  end interface

#ifndef ESMF_NO_F2018ASSUMEDTYPE

  interface

    !TODO: DEPRECATED -> transition to ESMF_InternalState API
    subroutine ESMF_GridCompSetInternalState(table, internalState, rc)
      type(*)                 :: table
      type(*)                 :: internalState
      integer                 :: rc
    end subroutine

    !TODO: DEPRECATED -> transition to ESMF_InternalState API
    subroutine ESMF_GridCompGetInternalState(table, internalState, rc)
      type(*)                 :: table
      type(*)                 :: internalState
      integer                 :: rc
    end subroutine

    !TODO: DEPRECATED -> transition to ESMF_InternalState API
    subroutine ESMF_CplCompSetInternalState(table, internalState, rc)
      type(*)                 :: table
      type(*)                 :: internalState
      integer                 :: rc
    end subroutine

    !TODO: DEPRECATED -> transition to ESMF_InternalState API
    subroutine ESMF_CplCompGetInternalState(table, internalState, rc)
      type(*)                 :: table
      type(*)                 :: internalState
      integer                 :: rc
    end subroutine

    !TODO: DEPRECATED -> transition to ESMF_InternalState API
    subroutine ESMF_UserCompSetInternalState(table, label, internalState, rc)
      type(*)                 :: table
      character(*), optional  :: label
      type(*)                 :: internalState
      integer                 :: rc
    end subroutine

    !TODO: DEPRECATED -> transition to ESMF_InternalState API
    subroutine ESMF_UserCompGetInternalState(table, label, internalState, rc)
      type(*)                 :: table
      character(*), optional  :: label
      type(*)                 :: internalState
      integer                 :: rc
    end subroutine

    subroutine c_ESMC_InternalStateGetInfo(table, count, maxLen, rc)
      type(*)                 :: table
      integer                 :: count
      integer                 :: maxLen
      integer                 :: rc
    end subroutine

    subroutine c_ESMC_InternalStateGetLabels(table, labelList, rc)
      type(*)                 :: table
      character(*)            :: labelList(*)
      integer                 :: rc
    end subroutine

  end interface

#endif

!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateCplCompAdd"
#ifndef ESMF_NO_F2018ASSUMEDTYPE
!BOP
! !IROUTINE: ESMF_InternalStateAdd - Add an InternalState to CplComp
! \label{esmfinternalstateaddcplcomp}
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateAdd()
  subroutine ESMF_InternalStateCplCompAdd(cplcomp, keywordEnforcer, &
    internalState, label, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: cplcomp
    type(*)                                    :: internalState
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),        intent(in),  optional :: label
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Add an internal state.
!
! CAUTION: This interface is only implemented for compilers that support
! Fortran 2018 assumed-type dummy arguments. Do not use this method in code
! that requires portability with compilers that do not support this feature!
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} object to hold the internal state.
! \item[internalState]
!   The internal state to add.
! \item[{[label]}]
!   The label to associate with the internal state. By default, associated
!   without label.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: count, maxLen

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    call ESMF_UserCompSetInternalState(cplcomp, label, internalState, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
#endif
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateGridCompAdd"
#ifndef ESMF_NO_F2018ASSUMEDTYPE
!BOP
! !IROUTINE: ESMF_InternalStateAdd - Add an InternalState to GridComp
! \label{esmfinternalstateaddgcomp}
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateAdd()
  subroutine ESMF_InternalStateGridCompAdd(gcomp, keywordEnforcer, &
    internalState, label, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gcomp
    type(*)                                    :: internalState
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),        intent(in),  optional :: label
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Add an internal state.
!
! CAUTION: This interface is only implemented for compilers that support
! Fortran 2018 assumed-type dummy arguments. Do not use this method in code
! that requires portability with compilers that do not support this feature!
!
! The arguments are:
! \begin{description}
! \item[gcomp]
!   The {\tt ESMF\_GridComp} object to hold the internal state.
! \item[internalState]
!   The internal state to add.
! \item[{[label]}]
!   The label to associate with the internal state. By default, associated
!   without label.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: count, maxLen

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)

    call ESMF_UserCompSetInternalState(gcomp, label, internalState, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
#endif
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateCplCompGet"
#ifndef ESMF_NO_F2018ASSUMEDTYPE
!BOP
! !IROUTINE: ESMF_InternalStateGet - Get InternalState from CplComp
! \label{esmfinternalstategetcplcomp}
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateGet()
  subroutine ESMF_InternalStateCplCompGet(cplcomp, keywordEnforcer, &
    internalState, label, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),  intent(in)            :: cplcomp
    type(*)                                    :: internalState
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),        intent(in),  optional :: label
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Access the internal state.
!
! CAUTION: This interface is only implemented for compilers that support
! Fortran 2018 assumed-type dummy arguments. Do not use this method in code
! that requires portability with compilers that do not support this feature!
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} object holding the internal state.
! \item[internalState]
!   The returned internal state.
! \item[{[label]}]
!   The label of the internal state accessed. By default, access the last
!   internal state set without label.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: count, maxLen

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    call ESMF_UserCompGetInternalState(cplcomp, label, internalState, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
#endif
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateCplCompGetL"
!BOP
! !IROUTINE: ESMF_InternalStateGet - Get InternalState labels from CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateGet()
  subroutine ESMF_InternalStateCplCompGetL(cplcomp, labelList, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp),            intent(in)            :: cplcomp
    character(len=:), allocatable, intent(out)           :: labelList(:)
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
! Access labels of all the internal states.
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} object holding the internal states.
! \item[labelList]
!   List of labels of {\em all} the internal states. On return, it will be
!   allocated with as many list elements as there are internal states. The
!   length of each label in {\tt labelList} is that of the longest label
!   currently set. Elements with shorter labels are padded with white
!   spaces.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: count, maxLen

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_CplCompGetInit, cplcomp, rc)

    call c_ESMC_InternalStateGetInfo(cplcomp, count, maxLen, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(character(maxLen) :: labelList(count))

    call c_ESMC_InternalStateGetLabels(cplcomp, labelList, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateGridCompGet"
#ifndef ESMF_NO_F2018ASSUMEDTYPE
!BOP
! !IROUTINE: ESMF_InternalStateGet - Get InternalState from GridComp
! \label{esmfinternalstategetgcomp}
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateGet()
  subroutine ESMF_InternalStateGridCompGet(gcomp, keywordEnforcer, &
    internalState, label, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp), intent(in)            :: gcomp
    type(*)                                    :: internalState
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(*),        intent(in),  optional :: label
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
! Access the internal state.
!
! CAUTION: This interface is only implemented for compilers that support
! Fortran 2018 assumed-type dummy arguments. Do not use this method in code
! that requires portability with compilers that do not support this feature!
!
! The arguments are:
! \begin{description}
! \item[cplcomp]
!   The {\tt ESMF\_CplComp} object holding the internal state.
! \item[internalState]
!   The returned internal state.
! \item[{[label]}]
!   The label of the internal state accessed. By default, access the last
!   internal state set without label.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: count, maxLen

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)

    call ESMF_UserCompGetInternalState(gcomp, label, internalState, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
#endif
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateGridCompGetL"
!BOP
! !IROUTINE: ESMF_InternalStateGet - Get InternalState labels from GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateGet()
  subroutine ESMF_InternalStateGridCompGetL(gcomp, labelList, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),           intent(in)            :: gcomp
    character(len=:), allocatable, intent(out)           :: labelList(:)
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
! Access labels of all the internal states.
!
! The arguments are:
! \begin{description}
! \item[gcomp]
!   The {\tt ESMF\_GridComp} object holding the internal states.
! \item[labelList]
!   List of labels of {\em all} the internal states. On return, it will be
!   allocated with as many list elements as there are internal states. The
!   length of each label in {\tt labelList} is that of the longest label
!   currently set. Elements with shorter labels are padded with white
!   spaces.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status
    integer :: count, maxLen

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_GridCompGetInit, gcomp, rc)

    call c_ESMC_InternalStateGetInfo(gcomp, count, maxLen, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(character(maxLen) :: labelList(count))

    call c_ESMC_InternalStateGetLabels(gcomp, labelList, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine
!------------------------------------------------------------------------------

end module ESMF_InternalStateMod
