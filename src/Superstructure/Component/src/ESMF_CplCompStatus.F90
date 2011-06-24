! $Id: ESMF_CplCompStatus.F90,v 1.1 2011/06/24 05:48:15 theurich Exp $
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
#define ESMF_FILENAME "ESMF_CplCompStatus.F90"
!==============================================================================
!
! ESMF Gridded Component Status module
module ESMF_CplCompStatusMod
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
! !MODULE: ESMF_CplCompStatusMod - Gridded Component Status class.
!
! !DESCRIPTION:
!
!
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_CompMod, &
    ESMF_CplCompStatus => ESMF_CompStatus
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

! - ESMF-public types:
  public ESMF_CplCompStatus

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_CplCompStatusGet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CplCompStatusGet"
!BOP
! !IROUTINE: ESMF_CplCompStatusGet -- Access the CplCompStatus bits
!
! !INTERFACE:
  recursive subroutine ESMF_CplCompStatusGet(cplCompStatus, keywordEnforcer, &
    clockIsPresent, configIsPresent, configFileIsPresent, vmIsPresent, rc)
!
! !ARGUMENTS:
    type(ESMF_CplCompStatus), intent(in)            :: cplCompStatus
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                  intent(out), optional :: clockIsPresent
    logical,                  intent(out), optional :: configIsPresent
    logical,                  intent(out), optional :: configFileIsPresent
    logical,                  intent(out), optional :: vmIsPresent
    integer,                  intent(out), optional :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Access the CplCompStatus bits.
!
!   The arguments are:
!   \begin{description}
!   \item[cplCompStatus]
!     CplCompStatus object.
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
    call ESMF_CompStatusGet(CplCompStatus, &
      clockIsPresent = clockIsPresent, &
      configIsPresent = configIsPresent, &
      configFileIsPresent = configFileIsPresent, &
      vmIsPresent = vmIsPresent, &
      rc = localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CplCompStatusGet
!------------------------------------------------------------------------------

end module ESMF_CplCompStatusMod
