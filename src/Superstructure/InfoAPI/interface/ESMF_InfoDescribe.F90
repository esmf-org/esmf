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
!==============================================================================

#define ESMF_FILENAME "ESMF_InfoDescribeMod.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================

module ESMF_InfoDescribeMod

use ESMF_UtilTypesMod     ! ESMF utility types
use ESMF_InitMacrosMod    ! ESMF initializer macros
use ESMF_BaseMod          ! ESMF base class
use ESMF_LogErrMod        ! ESMF error handling

use ESMF_VMMod
use ESMF_StateMod
use ESMF_StateItemMod
use ESMF_DistGridMod
use ESMF_FieldMod
use ESMF_FieldGetMod
use ESMF_FieldBundleMod
use ESMF_CompMod
use ESMF_GridCompMod
use ESMF_CplCompMod
use ESMF_SciCompMod
use ESMF_ArrayMod
use ESMF_ArrayBundleMod
use ESMF_InfoMod
use ESMF_UtilTypesMod
use ESMF_GeomBaseMod
use ESMF_MeshMod
use ESMF_GridMod
use ESMF_XGridMod
use ESMF_XGridGetMod
use ESMF_LocStreamMod
use ESMF_RHandleMod
implicit none

!==============================================================================
!==============================================================================

!private
!public

!==============================================================================
!==============================================================================

type, public :: ESMF_InfoDescribe
  type(ESMF_Info) :: info
  logical :: addBaseAddress = .false.  ! If true, add the object's base address
  logical :: addObjectInfo = .false.  ! If true, add ESMF_Info map for each object
  logical :: createInfo = .true.  ! If true, also recurse objects with members (i.e. ArrayBundle)
  logical :: is_initialized = .false.  ! If true, the object is initialized
  type(ESMF_Base) :: curr_base  ! Holds a reference to the current update object's base. Will change when recursively updating
  logical :: curr_base_is_valid = .false.  ! If true, the object's base is valid (i.e. can be reinterpret casted)
  logical :: curr_base_is_geom = .false.  ! If true, the Base is for an ESMF Geometry object
contains
  procedure, private :: updateWithState, updateWithArray, updateWithArrayBundle, &
   updateWithField, updateWithFieldBundle, updateWithLocStream, updateWithGrid, &
   updateWithXGrid, updateWithMesh, updateWithRouteHandle, updateWithDistGrid, &
   updateWithGridComp, updateWithCplComp, updateWithSciComp
  generic, public :: Update => updateWithState, updateWithArray, updateWithArrayBundle, &
   updateWithField, updateWithFieldBundle, updateWithLocStream, updateWithGrid, &
   updateWithXGrid, updateWithMesh, updateWithRouteHandle, updateWithDistGrid, &
   updateWithGridComp, updateWithCplComp, updateWithSciComp
  procedure, private :: fillMembersState, fillMembersArrayBundle, fillMembersField, &
   fillMembersFieldBundle
  generic, public :: FillMembers => fillMembersState, fillMembersArrayBundle, fillMembersField, &
   fillMembersFieldBundle
  procedure, private, pass :: ESMF_InfoDescribeDestroy, ESMF_InfoDescribePrint, &
   ESMF_InfoDescribeGetCurrentBase, ESMF_InfoDescribeGetCurrentInfo
  procedure, private :: updateGeneric, ESMF_InfoDescribeIntialize
  procedure, private, nopass :: getInfoArray, getInfoArrayBundle, getInfoCplComp, &
   getInfoGridComp, getInfoSciComp, getInfoDistGrid, getInfoField, getInfoFieldBundle, &
   getInfoGrid, getInfoState, getInfoLocStream
  generic, public :: GetInfo => getInfoArray, getInfoArrayBundle, getInfoCplComp, getInfoGridComp, &
   getInfoSciComp, getInfoDistGrid, getInfoField, getInfoFieldBundle, getInfoGrid, &
   getInfoState, getInfoLocStream
  generic, public :: Initialize => ESMF_InfoDescribeIntialize
  generic, public :: Destroy => ESMF_InfoDescribeDestroy
  generic, public :: Print => ESMF_InfoDescribePrint
  generic, public :: GetCurrentBase => ESMF_InfoDescribeGetCurrentBase
  generic, public :: GetCurrentInfo => ESMF_InfoDescribeGetCurrentInfo
end type ESMF_InfoDescribe

contains !=====================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%ESMF_InfoDescribeIntialize()"
subroutine ESMF_InfoDescribeIntialize(self, addBaseAddress, addObjectInfo, createInfo, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  logical, intent(in), optional :: addBaseAddress
  logical, intent(in), optional :: addObjectInfo
  logical, intent(in), optional :: createInfo
  integer, intent(inout), optional :: rc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  if (self%is_initialized) then
    if (ESMF_LogFoundError(ESMF_FAILURE, msg="Object already initialized", ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if (present(addBaseAddress)) self%addBaseAddress = addBaseAddress
  if (present(addObjectInfo)) self%addObjectInfo = addObjectInfo
  if (present(createInfo)) self%createInfo = createInfo
  if (self%createInfo) then
    self%info = ESMF_InfoCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  endif
  self%is_initialized = .true.

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine ESMF_InfoDescribeIntialize

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%ESMF_InfoDescribeDestroy()"
subroutine ESMF_InfoDescribeDestroy(self, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  integer, intent(inout), optional :: rc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  if (self%is_initialized) then
    if (self%createInfo) then
      call ESMF_InfoDestroy(self%info, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    endif
  endif

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine ESMF_InfoDescribeDestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%ESMF_InfoDescribeGetCurrentBase()"
function ESMF_InfoDescribeGetCurrentBase(self, rc) result(base)
  class(ESMF_InfoDescribe), intent(in) :: self
  integer, intent(inout), optional :: rc
  type(ESMF_Base) :: base
  integer :: localrc
  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  if (self%curr_base_is_valid) then
    base = self%curr_base
  else
    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, msg="Base is not valid", ESMF_CONTEXT, rcToReturn=rc)) return
  endif
  if (present(rc)) rc = ESMF_SUCCESS
end function ESMF_InfoDescribeGetCurrentBase

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%ESMF_InfoDescribeGetCurrentInfo()"
function ESMF_InfoDescribeGetCurrentInfo(self, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  class(ESMF_InfoDescribe), intent(in) :: self
  integer, intent(inout), optional :: rc
  type(ESMF_Base) :: base
  type(ESMF_Info) :: info
  integer :: localrc
  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  base = self%GetCurrentBase(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = ESMF_InfoBaseGetHandle(base)
  info%is_view = .true.
  if (present(rc)) rc = ESMF_SUCCESS
end function ESMF_InfoDescribeGetCurrentInfo

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%ESMF_InfoDescribePrint()"
subroutine ESMF_InfoDescribePrint(self, rc)
  class(ESMF_InfoDescribe), intent(in) :: self
  integer, intent(inout), optional :: rc
  integer :: localrc
  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  call ESMF_InfoPrint(self%info, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_FAILURE
end subroutine ESMF_InfoDescribePrint

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%fillMembersState()"
subroutine fillMembersState(self, state, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_State), intent(in) :: state
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc

  type(ESMF_Array) :: array
  type(ESMF_ArrayBundle) :: arraybundle
  type(ESMF_Field) :: field
  type(ESMF_FieldBundle) :: fieldbundle
  type(ESMF_RouteHandle) :: rh
  type(ESMF_State) :: state_nested
  type(ESMF_StateItem_Flag), dimension(:), allocatable :: stateTypes
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: stateNames
  character(len=ESMF_MAXSTR) :: name
  integer :: ii, jj, itemCount, localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_StateGet(state, itemCount=itemCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  allocate(stateTypes(itemCount), stateNames(itemCount))
  call ESMF_StateGet(state, itemTypeList=stateTypes, itemNameList=stateNames, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  do ii=1,itemCount
    select case (stateTypes(ii)%ot)
    case(ESMF_STATEITEM_ARRAY%ot)
      call ESMF_StateGet(state, trim(stateNames(ii)), array, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call self%Update(array, root_key, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    case(ESMF_STATEITEM_ARRAYBUNDLE%ot)
      call ESMF_StateGet(state, trim(stateNames(ii)), arraybundle, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call self%Update(arraybundle, root_key, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    case(ESMF_STATEITEM_STATE%ot)
     call ESMF_StateGet(state, trim(stateNames(ii)), state_nested, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     call self%Update(state_nested, root_key, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
   case(ESMF_STATEITEM_FIELD%ot)
     call ESMF_StateGet(state, trim(stateNames(ii)), field, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     call self%Update(field, root_key, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
   case(ESMF_STATEITEM_FIELDBUNDLE%ot)
     call ESMF_StateGet(state, trim(stateNames(ii)), fieldbundle, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     call self%Update(fieldbundle, root_key, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
   case(ESMF_STATEITEM_ROUTEHANDLE%ot)
     call ESMF_StateGet(state, trim(stateNames(ii)), rh, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     call self%Update(rh, root_key, rc=localrc)
     if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    case default
      if (ESMF_LogFoundError(ESMF_RC_OBJ_NOT_CREATED, msg="Object type not supported for Inquire", &
       ESMF_CONTEXT, rcToReturn=rc)) return
    end select
  end do
  deallocate(stateTypes, stateNames)

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine fillMembersState

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateGeneric()"
subroutine updateGeneric(self, root_key, name, etype, base, keywordEnforcer, base_is_valid, uname, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  character(*), intent(in) :: root_key
  character(*), intent(in) :: name
  character(*), intent(in) :: etype
  type(ESMF_Base), intent(in) :: base
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  logical, intent(in), optional :: base_is_valid
  character(:), allocatable, optional :: uname
  integer, intent(inout), optional :: rc
  integer :: id_base
  character(:), allocatable :: c_id_base, l_uname

  character(:), allocatable :: local_root_key
  integer :: localrc, ii
  logical :: l_base_is_valid
  type(ESMF_Info) :: object_info
  character(len=9), dimension(4), parameter :: geom_etypes = (/"Grid     ", "Mesh     ", "LocStream", "XGrid    "/)

  localrc = ESMF_FAILURE
  if (.not. self%is_initialized) then
    if (ESMF_LogFoundError(ESMF_RC_OBJ_NOT_CREATED, msg="ESMF_InfoDescribe is not initialized", &
     ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  if (present(base_is_valid)) then
    l_base_is_valid = base_is_valid
  else
    l_base_is_valid = .true.
  end if
  self%curr_base_is_valid = l_base_is_valid
  self%curr_base = base

  self%curr_base_is_geom = .false.
  do ii=1,SIZE(geom_etypes)
    if (trim(etype) == trim(geom_etypes(ii))) then
      self%curr_base_is_geom = .true.
      exit
    end if
  end do

  if (self%createInfo) then
    if (l_base_is_valid) then
      call ESMF_BaseGetId(base, id_base, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call itoa(id_base, c_id_base)
      allocate(character(len(c_id_base)+len(trim(name))+1)::l_uname)
      l_uname = c_id_base//"-"//trim(name)
      deallocate(c_id_base)
    else
      allocate(character(len(trim(name)))::l_uname)
      l_uname = trim(name)
    end if

    allocate(character(len(trim(root_key))+len(l_uname)+1)::local_root_key)
    local_root_key = trim(root_key)//"/"//l_uname

    call ESMF_InfoSet(self%info, local_root_key//"/base_name", trim(name), force=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return


    call ESMF_InfoSet(self%info, local_root_key//"/esmf_type", etype, force=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InfoSet(self%info, local_root_key//"/base_is_valid", l_base_is_valid, force=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InfoSetNULL(self%info, local_root_key//"/members", force=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    if (self%addBaseAddress) then
      call ESMF_InfoSet(self%info, local_root_key//"/base_address", base%this%ptr, force=.false., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    if (l_base_is_valid) then
      call ESMF_InfoSet(self%info, local_root_key//"/base_id", id_base, force=.false., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call ESMF_InfoSetNULL(self%info, local_root_key//"/base_id", force=.false., rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    call ESMF_InfoSet(self%info, local_root_key//"/is_geom", self%curr_base_is_geom, force=.false., rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    if (self%addObjectInfo) then
      if (l_base_is_valid) then
        object_info = ESMF_InfoBaseGetHandle(base)
        call ESMF_InfoSet(self%info, local_root_key//"/info", object_info, force=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_InfoSetNULL(self%info, local_root_key//"/info", force=.false., rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    end if

    if (present(uname)) then
      allocate(character(len(l_uname))::uname)
      uname = l_uname
    end if

    deallocate(local_root_key, l_uname)
  endif

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateGeneric

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithArray()"
subroutine updateWithArray(self, array, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_Array), intent(in) :: array
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  type(ESMF_Base) :: newbase
  type(ESMF_Pointer) :: this

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_ArrayGetThis(array, this, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  newbase%this = this

  call ESMF_ArrayGet(array, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, "Array", newbase, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithArray

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithArrayBundle()"
subroutine updateWithArrayBundle(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_ArrayBundle), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  type(ESMF_Base) :: newbase
  type(ESMF_Pointer) :: this
  character(*), parameter :: etype = "ArrayBundle"
  character(:), allocatable :: uname

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_ArrayBundleGetThis(target, this, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  newbase%this = this

  call ESMF_ArrayBundleGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, newbase, uname=uname, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (self%createInfo) then
    call self%FillMembers(target, root_key//"/"//uname//"/members", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(uname)
  endif

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithArrayBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithDistGrid()"
subroutine updateWithDistGrid(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_DistGrid), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  integer :: localrc
  type(ESMF_Base) :: newbase
  type(ESMF_Pointer) :: this
  character(*), parameter :: etype = "DistGrid"

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_DistGridGetThis(target, this, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  newbase%this = this

  call self%updateGeneric(root_key, "__DistGrid__", etype, newbase, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithDistGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithState()"
subroutine updateWithState(self, state, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_State), intent(in) :: state
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(:), allocatable :: uname

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_StateGet(state, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, "State", state%statep%base, uname=uname, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (self%createInfo) then
    call self%FillMembers(state, root_key//"/"//uname//"/members", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(uname)
  end if

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithState

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithField()"
subroutine updateWithField(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_Field), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="Field"
  character(:), allocatable :: uname

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_FieldGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%ftypep%base, uname=uname, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (self%createInfo) then
    call self%FillMembers(target, root_key//"/"//uname//"/members", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(uname)
  endif

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithField

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithGridComp()"
subroutine updateWithGridComp(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_GridComp), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="GridComp"

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%compp%base, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithGridComp

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithCplComp()"
subroutine updateWithCplComp(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_CplComp), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="CplComp"

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%compp%base, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithCplComp

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithSciComp()"
subroutine updateWithSciComp(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_SciComp), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="SciComp"

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_SciCompGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%compp%base, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithSciComp

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithLocStream()"
subroutine updateWithLocStream(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_LocStream), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="LocStream"

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_LocStreamGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%lstypep%base, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithLocStream

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithGrid()"
subroutine updateWithGrid(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_Grid), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="Grid"
  type(ESMF_Base) :: newbase

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_GridGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  newbase%this = target%this
  call self%updateGeneric(root_key, name, etype, newbase, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithXGrid()"
subroutine updateWithXGrid(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_XGrid), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="XGrid"

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_XGridGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%xgtypep%base, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithXGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithMesh()"
subroutine updateWithMesh(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_Mesh), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="Mesh"
  type(ESMF_Base) :: newbase

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  ! call ESMF_MeshGet(target, name=name, rc=localrc)
  ! if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  newbase%this = -1
  call self%updateGeneric(root_key, "__invalid_mesh_base__", etype, newbase, base_is_valid=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithMesh

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithRouteHandle()"
subroutine updateWithRouteHandle(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_RouteHandle), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype="RouteHandle"
  type(ESMF_Base) :: newbase

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_RouteHandleGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_RouteHandleGetThis(target, newbase%this, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, newbase, base_is_valid=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithRouteHandle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%updateWithFieldBundle()"
subroutine updateWithFieldBundle(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_FieldBundle), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  character(ESMF_MAXSTR) :: name
  integer :: localrc
  character(*), parameter :: etype = "FieldBundle"
  character(:), allocatable :: uname

  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_FieldBundleGet(target, name=name, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call self%updateGeneric(root_key, name, etype, target%this%base, uname=uname, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  if (self%createInfo) then
    call self%FillMembers(target, root_key//"/"//uname//"/members", rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(uname)
  endif

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine updateWithFieldBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%fillMembersArrayBundle()"
subroutine fillMembersArrayBundle(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_ArrayBundle), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Array), dimension(:), allocatable :: targetList
  integer :: ii, targetCount, localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_ArrayBundleGet(target, arrayCount=targetCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(targetList(targetCount))

  call ESMF_ArrayBundleGet(target, arrayList=targetList, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  do ii=1,targetCount
    call self%Update(targetList(ii), root_key, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  end do

  deallocate(targetList)

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine fillMembersArrayBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%fillMembersFieldBundle()"
subroutine fillMembersFieldBundle(self, target, root_key, keywordEnforcer, rc)
  class(ESMF_InfoDescribe), intent(inout) :: self
  type(ESMF_FieldBundle), intent(in) :: target
  character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Field), dimension(:), allocatable :: targetList
  integer :: ii, targetCount, localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_FieldBundleGet(target, fieldCount=targetCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(targetList(targetCount))

  call ESMF_FieldBundleGet(target, fieldList=targetList, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  do ii=1,targetCount
    call self%Update(targetList(ii), root_key, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  end do

  deallocate(targetList)

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine fillMembersFieldBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%fillMembersField()"
subroutine fillMembersField(self, field, root_key, keywordEnforcer, rc)
 class(ESMF_InfoDescribe), intent(inout) :: self
 type(ESMF_Field), intent(in) :: field
 character(*), intent(in) :: root_key
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
 integer, intent(inout), optional :: rc
 type(ESMF_GeomType_Flag) :: geomtype
 type(ESMF_Grid) :: grid
 type(ESMF_LocStream) :: locstream
 type(ESMF_XGrid) :: xgrid
 type(ESMf_Mesh) :: mesh
 integer :: localrc

 localrc = ESMF_FAILURE
 if (present(rc)) rc = ESMF_RC_NOT_IMPL

 if (field%ftypep%status .eq. ESMF_FIELDSTATUS_GRIDSET .or. &
     field%ftypep%status .eq. ESMF_FIELDSTATUS_COMPLETE) then
   call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

   select case (geomtype%type)
     case (ESMF_GEOMTYPE_GRID%type)
       call ESMF_FieldGet(field, grid=grid, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
       call self%Update(grid, root_key, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     case (ESMF_GEOMTYPE_LOCSTREAM%type)
       call ESMF_FieldGet(field, locstream=locstream, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
       call self%Update(locstream, root_key, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     case (ESMF_GEOMTYPE_XGRID%type)
       call ESMF_FieldGet(field, xgrid=xgrid, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
       call self%Update(xgrid, root_key, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     case (ESMF_GEOMTYPE_MESH%type)
       call ESMF_FieldGet(field, mesh=mesh, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
       call self%Update(mesh, root_key, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
     case default
       if (ESMF_LogFoundError(ESMF_RC_OBJ_NOT_CREATED, msg="Geometry type not supported for Inquire", &
        ESMF_CONTEXT, rcToReturn=rc)) return
   end select
 end if

 if (present(rc)) rc = ESMF_SUCCESS

end subroutine fillMembersField

subroutine itoa(i, res)
  ! https://stackoverflow.com/questions/1262695/convert-integers-to-strings-to-create-output-filenames-at-run-time
  character(:), allocatable :: res
  integer,intent(in) :: i
  character(:), allocatable :: tmp

  allocate(character(range(i)+2)::tmp)
  write(tmp,'(i0)') i
  allocate(character(len(trim(tmp)))::res)
  res = trim(tmp)
  deallocate(tmp)
end subroutine

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoArray()"
function getInfoArray(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_Array), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoArray

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoArrayBundle()"
function getInfoArrayBundle(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_ArrayBundle), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoArrayBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoCplComp()"
function getInfoCplComp(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_CplComp), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoCplComp

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoGridComp()"
function getInfoGridComp(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_GridComp), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoGridComp

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoSciComp()"
function getInfoSciComp(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_SciComp), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoSciComp

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoDistGrid()"
function getInfoDistGrid(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_DistGrid), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoDistGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoField()"
function getInfoField(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_Field), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoField

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoFieldBundle()"
function getInfoFieldBundle(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_FieldBundle), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoFieldBundle

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoGrid()"
function getInfoGrid(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_Grid), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoGrid

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoState()"
function getInfoState(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_State), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoState

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDescribe%getInfoLocStream()"
function getInfoLocStream(target, keywordEnforcer, rc) result(info)
  use iso_c_binding, only : C_NULL_PTR
  type(ESMF_LocStream), intent(in) :: target
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer, intent(inout), optional :: rc
  type(ESMF_Info) :: info
  type(ESMF_InfoDescribe) :: eidesc
  integer :: localrc

  localrc = ESMF_FAILURE
  if (present(rc)) rc = ESMF_RC_NOT_IMPL
  info%ptr = C_NULL_PTR
  call eidesc%Initialize(createInfo=.false., rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Update(target, "", rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  info = eidesc%GetCurrentInfo(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  call eidesc%Destroy(rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
  if (present(rc)) rc = ESMF_SUCCESS
end function getInfoLocStream

end module ESMF_InfoDescribeMod
