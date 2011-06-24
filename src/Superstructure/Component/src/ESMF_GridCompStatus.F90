! $Id: ESMF_GridCompStatus.F90,v 1.1 2011/06/24 05:48:15 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_GridCompStatus.F90"
!==============================================================================
!
! ESMF Gridded Component Status module
module ESMF_GridCompStatusMod
!
!==============================================================================
!
! This file contains the Gridded Component Status class definition and all 
!  Gridded Component Status class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_GridCompStatusMod - Gridded Component Status class.
!
! !DESCRIPTION:
!
!
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_CompMod, &
    ESMF_GridCompStatus => ESMF_CompStatus
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

! - ESMF-public types:
  public ESMF_GridCompStatus

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_GridCompStatusGet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCompStatusGet"
!BOP
! !IROUTINE: ESMF_GridCompStatusGet -- Access the GridCompStatus bits
!
! !INTERFACE:
  recursive subroutine ESMF_GridCompStatusGet(gridCompStatus, keywordEnforcer, &
    clockIsPresent, configIsPresent, configFileIsPresent, vmIsPresent, &
    importStateIsPresent, exportStateIsPresent, gridIsPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_GridCompStatus), intent(in)            :: gridCompStatus
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                   intent(out), optional :: clockIsPresent
    logical,                   intent(out), optional :: configIsPresent
    logical,                   intent(out), optional :: configFileIsPresent
    logical,                   intent(out), optional :: vmIsPresent
    logical,                   intent(out), optional :: importStateIsPresent
    logical,                   intent(out), optional :: exportStateIsPresent
    logical,                   intent(out), optional :: gridIsPresent
    integer,                   intent(out), optional :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Access the GridCompStatus bits.
!
!   The arguments are:
!   \begin{description}
!   \item[gridCompStatus]
!     GridCompStatus object.
!   \item[{[clockIsPresent]}]
!     {\tt .true.} if {\tt clock} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[clockIsPresent]}]
!     {\tt .true.} if {\tt clock} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[configFileIsPresent]}]
!     {\tt .true.} if {\tt configFile} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[vmIsPresent]}]
!     {\tt .true.} if {\tt vm} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[importStateIsPresent]}]
!     {\tt .true.} if {\tt importState} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[exportStateIsPresent]}]
!     {\tt .true.} if {\tt exportState} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[gridIsPresent]}]
!     {\tt .true.} if {\tt grid} was set in GridComp object,
!     {\tt .false.} otherwise.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    
    ! Initialize return code; assume not implemented until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! call into generic Comp level
    call ESMF_CompStatusGet(gridCompStatus, &
      clockIsPresent = clockIsPresent, &
      configIsPresent = configIsPresent, &
      configFileIsPresent = configFileIsPresent, &
      vmIsPresent = vmIsPresent, &
      isIsPresent = importStateIsPresent, &
      esIsPresent = exportStateIsPresent, &
      gridIsPresent = gridIsPresent, &
      rc = localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_GridCompStatusGet
!------------------------------------------------------------------------------

end module ESMF_GridCompStatusMod
