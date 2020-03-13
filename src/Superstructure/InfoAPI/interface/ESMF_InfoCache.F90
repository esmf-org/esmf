! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2020, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
! =============================================================================

#define FILENAME "src/Superstructure/InfoAPI/interface/ESMF_InfoCache.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

! =============================================================================
! =============================================================================

module ESMF_InfoCacheMod

use ESMF_UtilTypesMod     ! ESMF utility types
use ESMF_InitMacrosMod    ! ESMF initializer macros
use ESMF_BaseMod          ! ESMF base class
use ESMF_LogErrMod        ! ESMF error handling

use ESMF_VMMod
use ESMF_InfoMod
use ESMF_UtilTypesMod
use ESMF_InfoDescribeMod

use iso_c_binding, only : C_PTR, C_NULL_PTR, C_INT, C_LONG
      
implicit none

! =============================================================================
! =============================================================================

!private
!public

interface ! ===================================================================

! -----------------------------------------------------------------------------
! ISOC Bindings
! -----------------------------------------------------------------------------

function c_infocache_initialize() bind(C, name="ESMC_InfoCacheInitialize")
  use iso_c_binding, only : C_PTR, C_INT
  implicit none
  type(C_PTR) :: c_infocache_initialize
end function c_infocache_initialize

function c_infocache_destroy(infoCache) bind(C, name="ESMC_InfoCacheDestroy")
  use iso_c_binding, only : C_PTR, C_INT
  implicit none
  type(C_PTR), value :: infoCache
  integer(C_INT) :: c_infocache_destroy
end function c_infocache_destroy

function c_infocache_updategeoms(infoCache, infoDesc) bind(C, name="ESMC_InfoCacheUpdateGeoms")
  use iso_c_binding, only : C_PTR, C_INT
  implicit none
  type(C_PTR), value :: infoCache
  type(C_PTR), value :: infoDesc
  integer(C_INT) :: c_infocache_updategeoms
end function c_infocache_updategeoms

end interface ! ===============================================================

type, public :: ESMF_InfoCache
  type(C_PTR) :: ptr = C_NULL_PTR
contains
  procedure, private, pass :: ESMF_InfoCacheInitialize, ESMF_InfoCacheDestroy, &
   ESMF_InfoCacheUpdateGeoms
  generic, public :: Initialize => ESMF_InfoCacheInitialize
  generic, public :: Destroy => ESMF_InfoCacheDestroy
  generic, public :: UpdateGeoms => ESMF_InfoCacheUpdateGeoms
end type ESMF_InfoCache

contains ! ====================================================================

! -----------------------------------------------------------------------------
! Procedure Implementations
! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheInitialize()"
subroutine ESMF_InfoCacheInitialize(self, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  integer, intent(out) :: rc
  
  self%ptr = c_infocache_initialize()

  rc = ESMF_SUCCESS
  
end subroutine ESMF_InfoCacheInitialize

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheDestroy()"
subroutine ESMF_InfoCacheDestroy(self, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  integer, intent(out) :: rc
  
  rc = c_infocache_destroy(self%ptr)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  
  self%ptr = C_NULL_PTR
  
end subroutine ESMF_InfoCacheDestroy

! -----------------------------------------------------------------------------

!tdk:comm
!tdk:todo: consider renaming to UpdateFields since it's actually their metadata that's updated
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheUpdateGeoms()"
subroutine ESMF_InfoCacheUpdateGeoms(self, target, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  type(ESMF_State), intent(in) :: target
  integer, intent(out) :: rc

  type(ESMF_InfoDescribe) :: idesc

  call idesc%Initialize(createInfo=.true., addBaseAddress=.true., rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call idesc%Update(target, "", rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  rc = c_infocache_updategeoms(self%ptr, idesc%info%ptr)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call idesc%Destroy(rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

end subroutine ESMF_InfoCacheUpdateGeoms

end module ESMF_InfoCacheMod ! ================================================
