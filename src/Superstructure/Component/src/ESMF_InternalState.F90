! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
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

  interface ESMF_InternalStateGet
    module procedure ESMF_InternalStateGridCompGetL
    module procedure ESMF_InternalStateCplCompGetL
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
#define ESMF_METHOD "ESMF_InternalStateGridCompGetL"
!BOP
! !IROUTINE: ESMF_InternalStateGet - Get info about internal states in GridComp
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateGet()
  subroutine ESMF_InternalStateGridCompGetL(gcomp, labelList, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp)                                  :: gcomp
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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternalStateCplCompGetL"
!BOP
! !IROUTINE: ESMF_InternalStateGet - Get info about internal states in CplComp
!
! !INTERFACE:
  ! Private name; call using ESMF_InternalStateGet()
  subroutine ESMF_InternalStateCplCompGetL(cplcomp, labelList, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp)                                   :: cplcomp
    character(len=:), allocatable, intent(out)           :: labelList(:)
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
! Access labels of all the internal states.
!
! The arguments are:
! \begin{description}
! \item[gcomp]
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

end module ESMF_InternalStateMod
