!  $Id$
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
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Util_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_utilversionprint"
subroutine f_esmf_utilversionprint (vFlag, versionFlag, rc)
  use ESMF_IOUtilMod
  use ESMF_LogErrMod
  use ESMF_UtilTypesMod
  implicit none
  type(ESMF_Logical), intent(in) :: vFlag
  type(ESMF_Logical), intent(in) :: versionFlag
  integer, intent(out) :: rc

  integer :: localrc

  ! initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_UtilVersionPrint (vFlag=vFlag == ESMF_TRUE, versionFlag=versionFlag == ESMF_TRUE, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_UtilIOUnitFlush (unit=ESMF_UtilIOStdout, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,                &
      ESMF_CONTEXT, rcToReturn=rc)) return

end subroutine f_esmf_utilversionprint

