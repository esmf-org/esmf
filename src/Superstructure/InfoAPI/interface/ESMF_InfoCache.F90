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

function c_infocache_findupdate(infoCache, baseAddress, i_found, i_shouldUpdate) bind(C, name="ESMC_InfoCacheFindUpdate")
  use iso_c_binding, only : C_PTR, C_INT, C_LONG
  implicit none
  type(C_PTR), value :: infoCache
  integer(C_LONG), intent(in) :: baseAddress
  integer(C_INT), intent(out) :: i_found
  integer(C_INT), intent(in) :: i_shouldUpdate
  integer(C_INT) :: c_infocache_findupdate
end function c_infocache_findupdate

end interface ! ===============================================================

type, public :: ESMF_InfoCache
  type(C_PTR) :: cptr = C_NULL_PTR
contains
  procedure, private, pass :: ESMF_InfoCacheInitialize, ESMF_InfoCacheDestroy, &
   ESMF_InfoCacheFindUpdateBase, ESMF_InfoCacheFindUpdateFieldGeom
  generic, public :: Initialize => ESMF_InfoCacheInitialize
  generic, public :: Destroy => ESMF_InfoCacheDestroy
  generic, public :: FindUpdate => ESMF_InfoCacheFindUpdateBase, ESMF_InfoCacheFindUpdateFieldGeom
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
  
  self%cptr = c_infocache_initialize()

  rc = ESMF_SUCCESS
  
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
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheFindUpdateBase()"
function ESMF_InfoCacheFindUpdateBase(self, base, shouldUpdate, rc) result(found)
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

  call ESMF_LogWrite("calling c_infocache_findupdate") !tdk:p
  rc = c_infocache_findupdate(self%cptr, base%this%ptr, local_found, local_shouldUpdate)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  
  if (local_found == 1) found = .true.
  
end function ESMF_InfoCacheFindUpdateBase

! -----------------------------------------------------------------------------

!tdk:comment
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCache%ESMF_InfoCacheFindUpdateFieldGeom()"
function ESMF_InfoCacheFindUpdateFieldGeom(self, target, shouldUpdate, rc) result(found)
  class(ESMF_InfoCache), intent(inout) :: self
  type(ESMF_Field), intent(in) :: target
  logical, intent(in) :: shouldUpdate
  integer, intent(out) :: rc
  logical :: found

  integer(C_INT) :: local_found, local_shouldUpdate
  type(ESMF_InfoDescribe) :: idesc
  character(len=ESMF_MAXSTR) :: ikey_target, ikey_geom
  logical :: is_set
  integer(ESMF_KIND_I8) :: base_address
  type(ESMF_Base) :: base

  call idesc%Initialize(createInfo=.true., addBaseAddress=.true., rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call idesc%Update(target, "", rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_InfoPrint(idesc%info) !tdk:p
  call ESMF_InfoGet(idesc%info, idx=1, ikey=ikey_target)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  is_set = ESMF_InfoIsSet(idesc%info, trim(ikey_target)//"/members", rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (is_set) then
    call ESMF_InfoGet(idesc%info, key=trim(ikey_target)//"/members", idx=1, ikey=ikey_geom)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InfoGet(idesc%info, trim(ikey_target)//"/members/"//trim(ikey_geom)//"/base_address", base_address, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    base%this%ptr = base_address

    call ESMF_LogWrite("calling self%FindUpdate") !tdk:p
    found = self%FindUpdate(base, shouldUpdate, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  else
    found = .false.
  end if

  call idesc%Destroy(rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

end function ESMF_InfoCacheFindUpdateFieldGeom

end module ESMF_InfoCacheMod ! ================================================
