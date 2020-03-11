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

use iso_c_binding, only : C_PTR, C_NULL_PTR, C_INT
      
implicit none

! =============================================================================
! =============================================================================

!private
!public

type, public :: ESMF_InfoCache
  type(C_PTR) :: cptr = C_NULL_PTR
contains
  procedure, private, pass :: ESMF_InfoCacheInitialize, ESMF_InfoCacheDestroy, &
   ESMF_InfoCacheFindUpdate
  generic, public :: Initialize => ESMF_InfoCacheInitialize
  generic, public :: Destroy => ESMF_InfoCacheDestroy
  generic, public :: FindUpdate => ESMF_InfoCacheFindUpdate
end type ESMF_InfoCache

contains ! ====================================================================

! -----------------------------------------------------------------------------
! ISOC Bindings
! -----------------------------------------------------------------------------

function c_infocache_initialize(infoCache) bind(C, name="ESMC_InfoCacheInitialize")
  use iso_c_binding, only : C_PTR, C_INT
  implicit none
  type(C_PTR), value :: infoCache
  integer(C_INT) :: c_infocache_initialize
end function c_infocache_initialize

function c_infocache_destroy(infoCache) bind(C, name="ESMC_InfoCacheDestroy")
  use iso_c_binding, only : C_PTR, C_INT
  implicit none
  type(C_PTR), value :: infoCache
  integer(C_INT) :: c_infocache_destroy
end function c_infocache_destroy

function c_infocache_findupdate(infoCache, baseAddress, i_found, i_shouldUpdate) bind(C, name="ESMC_InfoCacheFindUpdate")
  use iso_c_binding, only : C_PTR, C_INT, C_LONG
  implicit none
  type(C_PTR), value :: infoCache
  integer(C_LONG), intent(in) :: baseAddress
  integer(C_INT), intent(out) :: i_found
  integer(C_INT), intent(in) :: i_shouldUpdate
  integer(C_INT) :: c_infocache_findupdate
end function c_infocache_findupdate

! -----------------------------------------------------------------------------
! Procedure Implementations
! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheInitialize()"
subroutine ESMF_InfoCacheInitialize(self, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  integer, intent(out) :: rc
  
  rc = c_infocache_initialize(self%cptr)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  
end subroutine ESMF_InfoCacheInitialize

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheDestroy()"
subroutine ESMF_InfoCacheDestroy(self, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  integer, intent(out) :: rc
  
  rc = c_infocache_destroy(self%cptr)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  
  self%cptr = C_NULL_PTR
  
end subroutine ESMF_InfoCacheDestroy

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheFindUpdate()"
function ESMF_InfoCacheFindUpdate(self, base, shouldUpdate, rc) result(found)
  class(ESMF_InfoCache), intent(inout) :: self
  type(ESMF_Base), intent(in) :: base
  logical, intent(in) :: shouldUpdate
  integer, intent(out) :: rc
  logical :: found
  
  integer(C_INT) :: local_found, local_shouldUpdate
  
  found = .false.
  local_found = 0  !false
  local_shouldUpdate = 0  !false
  if (shouldUpdate) local_shouldUpdate = 1  !true
  
  rc = c_infocache_findupdate(self%cptr, base%this%ptr, local_found, local_shouldUpdate)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  
  if (local_found == 1) found = .true.
  
end function ESMF_InfoCacheFindUpdate

end module ESMF_InfoCacheMod ! ================================================
