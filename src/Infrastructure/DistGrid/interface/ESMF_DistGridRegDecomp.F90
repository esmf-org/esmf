! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_DistGridRegDecomp.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_DistGridRegDecompMod
!
!==============================================================================
!
! This file contains the DistGridRegDecomp API implementation.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridRegDecompMod
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_F90InterfaceMod        ! ESMF F90-C++ interface helper
  use ESMF_VMMod
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:

  public ESMF_DistGridRegDecompSetCubic
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridRegDecompSetCubic()"
!BOP
! !IROUTINE: ESMF_DistGridRegDecompSetCubic - Construct a DistGrid regDecomp
! !INTERFACE:
  subroutine ESMF_DistGridRegDecompSetCubic(regDecomp, deCount, keywordEnforcer, rc)
!
! !ARGUMENTS:
    integer,        target, intent(out)           :: regDecomp(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: deCount
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Construct a regular decomposition argument for DistGrid that is most cubic
!   in {\tt dimCount} dimensions, and multiplies out to {\tt deCount} DEs. The
!   factorization is stable monotonic decreasing, ensuring that earlier entries
!   in {\tt regDecomp} are larger or equal to the later entires.
!
!   The arguments are:
!   \begin{description}
!   \item[regDecomp]
!     The regular decomposition description being constructed.
!   \item[{[deCount]}]
!     The number of DEs. Defaults to {\tt petCount}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc      ! local return code
    type(ESMF_InterArray) :: regDecompArg ! helper variable
    integer               :: optDeCount   ! helper variable
    type(ESMF_VM)         :: vm           ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! create InterArrays
    regDecompArg = ESMF_InterArrayCreate(regDecomp, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Deal with optional arguments
    if (present(deCount)) then
      optDeCount = deCount
    else
      call ESMF_VMGetCurrent(vm, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_VMGet(vm, petCount=optDeCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridRDSetCubic(regDecompArg, optDeCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridRegDecompSetCubic
!------------------------------------------------------------------------------

end module ESMF_DistGridRegDecompMod
