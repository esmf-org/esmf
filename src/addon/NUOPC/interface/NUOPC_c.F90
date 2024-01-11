! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/interface/NUOPC_c.F90"
!==============================================================================
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

subroutine f_nuopc_modelsetservices(gcomp, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_modelsetservices"

  use ESMF
  use NUOPC
  use NUOPC_Model, only: SetServices
  implicit none

  type(ESMF_GridComp)   :: gcomp  !in
  integer, intent(out)  :: rc     !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call SetServices(gcomp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  rc = localrc

end subroutine f_nuopc_modelsetservices
