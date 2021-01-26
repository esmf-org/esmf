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
! =============================================================================

#define ESMF_FILENAME "ESMF_InfoCache.F90"

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

function c_infocache_updatefields(infoCache, infoDesc) bind(C, name="ESMC_InfoCacheUpdateFields")
  use iso_c_binding, only : C_PTR, C_INT
  implicit none
  type(C_PTR), value :: infoCache
  type(C_PTR), value :: infoDesc
  integer(C_INT) :: c_infocache_updatefields
end function c_infocache_updatefields

end interface ! ===============================================================

type, public :: ESMF_InfoCache
  type(C_PTR) :: ptr
contains
  procedure, private, pass :: ESMF_InfoCacheInitialize, ESMF_InfoCacheDestroy, &
   ESMF_InfoCacheUpdateFields
  generic, public :: Initialize => ESMF_InfoCacheInitialize
  generic, public :: Destroy => ESMF_InfoCacheDestroy
  generic, public :: UpdateFields => ESMF_InfoCacheUpdateFields
end type ESMF_InfoCache

contains ! ====================================================================

! -----------------------------------------------------------------------------
! Procedure Implementations
! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheInitialize()"
subroutine ESMF_InfoCacheInitialize(self, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  integer, intent(out) :: rc

  self%ptr = C_NULL_PTR
  self%ptr = c_infocache_initialize()

  rc = ESMF_SUCCESS
  
end subroutine ESMF_InfoCacheInitialize

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheDestroy()"
subroutine ESMF_InfoCacheDestroy(self, rc)
  class(ESMF_InfoCache), intent(inout) :: self
  integer, intent(out) :: rc

  rc = c_infocache_destroy(self%ptr)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  self%ptr = C_NULL_PTR
  
end subroutine ESMF_InfoCacheDestroy

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheUpdateFields()"
!BOPI
! !IROUTINE: ESMF_InfoCacheUpdateFields - Update Field metadata for StateReconcile optimizations
!
! !INTERFACE:
subroutine ESMF_InfoCacheUpdateFields(self, target, vmIdMap, rc)
! !ARGUMENTS:
  class(ESMF_InfoCache), intent(inout) :: self
  type(ESMF_State), intent(in) :: target
  type(ESMF_VMId), dimension(:), pointer, intent(in) :: vmIdMap
  integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Traverse the object hierarchy of \textit{target} updating Field attributes
!     for StateReconcile optimizations.
!
!     The arguments are:
!     \begin{description}
!     \item [self]
!       Class instance.
!     \item [target]
!       Target \texttt{ESMF\_State} to traverse recursively.
!     \item [vmIdMap]
!       An \texttt{ESMF\_VMId} map as computed by \texttt{ESMF\_VMTranslateVMId}.
!     \item [rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI

  type(ESMF_InfoDescribe) :: idesc

  call idesc%Initialize(createInfo=.true., addBaseAddress=.true., vmIdMap=vmIdMap, &
    vmIdMapGeomExc=.true., rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call idesc%Update(target, "", rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  rc = c_infocache_updatefields(self%ptr, idesc%info%ptr)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  call idesc%Destroy(rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

end subroutine ESMF_InfoCacheUpdateFields

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheFindField()"
!BOPI
! !IROUTINE: ESMF_InfoCacheFindField - Find the first instance of a Field in a State
!
! !INTERFACE:
recursive function ESMF_InfoCacheFindField(target, foundField, intVmId, baseID, rc) result(found)
! !ARGUMENTS:
  type(ESMF_State), intent(in) :: target
  type(ESMF_Field), intent(out) :: foundField
  integer, intent(in) :: intVmId
  integer, intent(in) :: baseID
  integer, intent(out) :: rc
! !RETURN VALUE:
  logical :: found
!
! !DESCRIPTION:
!     Find the first instance of an \texttt{ESMF\_Field} by recursively searching
!     the \texttt{ESMF\_State}. Returns true if the field is found. False if not.
!     The found field is only valid if this function returns true.
!
!     The arguments are:
!     \begin{description}
!     \item [target]
!       State to search.
!     \item [foundField]
!       Found field object. Only valid if this function returns true.
!     \item [intVmId]
!       \texttt{ESMF\_VM} identifier as computed by \texttt{ESMF\_VMTranslateVMId}.
!     \item [baseID]
!       \texttt{ESMF\_Base} identifier.
!     \item [rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
  type(ESMF_FieldBundle) :: fb
  type(ESMF_Field) :: field
  type(ESMF_StateItem_Flag), dimension(:), allocatable :: stateTypes
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: stateNames
  integer :: ii, itemCount, jj, curr_integer_vmid
  type(ESMF_State) :: state
  character(len=ESMF_MAXSTR) :: curr_field_name
  integer :: curr_field_base_id, field_count
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: field_name_list
  type(ESMF_Info) :: infoh
  logical :: is_present
  character(len=ESMF_MAXSTR) :: logmsg

  found = .false.
  rc = ESMF_SUCCESS

  call ESMF_StateGet(target, itemCount=itemCount, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(stateTypes(itemCount), stateNames(itemCount))

  call ESMF_StateGet(target, itemTypeList=stateTypes, itemNameList=stateNames, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  do ii=1,itemCount
    select case (stateTypes(ii)%ot)
      case(ESMF_STATEITEM_STATE%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), state, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        found = ESMF_InfoCacheFindField(state, foundField, intVmId, baseID, rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        if (found) return
      case(ESMF_STATEITEM_FIELD%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), field, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, name=curr_field_name, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_BaseGetID(field%ftypep%base, curr_field_base_id, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoGetFromBase(field%ftypep%base, infoh, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        is_present = ESMF_InfoIsPresent(infoh, "_esmf_state_reconcile", rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        if (is_present) then
          call ESMF_InfoGet(infoh, "_esmf_state_reconcile/integer_vmid", &
            curr_integer_vmid, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

          if (curr_field_base_id==baseID .and. curr_integer_vmid==intVmId) then
            found = .true.
            foundField = field
            return
          end if
        end if
      case(ESMF_STATEITEM_FIELDBUNDLE%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), fb, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleGet(fb, fieldCount=field_count, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(field_name_list(field_count))

        call ESMF_FieldBundleGet(fb, fieldNameList=field_name_list, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        do jj=1,field_count
          call ESMF_FieldBundleGet(fb, trim(field_name_list(jj)), field=field, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_BaseGetID(field%ftypep%base, curr_field_base_id, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

          call ESMF_InfoGetFromBase(field%ftypep%base, infoh, rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

          is_present = ESMF_InfoIsPresent(infoh, "_esmf_state_reconcile", rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

          if (is_present) then
            call ESMF_InfoGet(infoh, "/_esmf_state_reconcile/integer_vmid", &
              curr_integer_vmid, rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

            if (curr_field_base_id==baseID .and. curr_integer_vmid==intVmId) then
              found = .true.
              foundField = field
              return
            end if
          end if
        end do

        deallocate(field_name_list)
    end select
  end do

  deallocate(stateTypes, stateNames)
end function

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheReassembleField()"
!BOPI
! !IROUTINE: ESMF_InfoCacheReassembleField - Reconstruct a Field using a State
!
! !INTERFACE:
subroutine ESMF_InfoCacheReassembleField(target, state, rc)
! !ARGUMENTS:
  type(ESMF_Field), intent(inout) :: target
  type(ESMF_State), intent(inout) :: state
  integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Reassemble a Field using its attribute metadata and object data retrieved
!     from an input State. This method assumes Field attributes in the input
!     State have been updated using \texttt{ESMF\_InfoCacheUpdateFields}.
!
!     The arguments are:
!     \begin{description}
!     \item [target]
!       Input Field.
!     \item [state]
!       Input State.
!     \item [rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
  logical :: should_serialize_geom, found
  type(ESMF_Info) :: infoh, infoh_found
  integer :: base_id, integer_vmid
  type(ESMF_Field) :: archetype_field
  character(:), allocatable :: geom_type
  type(ESMF_InfoDescribe) :: idesc
  character(len=ESMF_MAXSTR) :: errmsg

  rc = ESMF_FAILURE

  if (target%ftypep%status .eq. ESMF_FIELDSTATUS_GRIDSET .or. &
      target%ftypep%status .eq. ESMF_FIELDSTATUS_COMPLETE) then
    call ESMF_InfoGetFromBase(target%ftypep%base, infoh, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InfoGet(infoh, "/_esmf_state_reconcile/should_serialize_geom", &
      should_serialize_geom, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

    if (.not. should_serialize_geom) then
      call ESMF_InfoGet(infoh, "/_esmf_state_reconcile/field_archetype_id", &
        base_id, rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_InfoGet(infoh, "/_esmf_state_reconcile/field_archetype_integer_vmid", &
              integer_vmid, rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

#if 0
      call idesc%Initialize(createInfo=.true., addObjectInfo=.true., rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call idesc%Update(state, "", rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_LogWrite(ESMF_InfoDump(idesc%info, indent=4), rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      call idesc%Destroy(rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
#endif

      found = ESMF_InfoCacheFindField(state, archetype_field, integer_vmid, base_id, rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

      if (.not. found) then
#if 0
        call ESMF_LogWrite("esmf infodump=")
        call ESMF_LogWrite(ESMF_InfoDump(infoh))
        write(errmsg, *) "integer_vmid=", integer_vmid
        call ESMF_LogWrite(trim(errmsg))
        write(errmsg, *) "base_id=", base_id
        call ESMF_LogWrite(trim(errmsg))
#endif

        if (ESMF_LogFoundError(ESMF_FAILURE, msg="Archetype Field not found", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if

      call ESMF_InfoGetFromBase(archetype_field%ftypep%base, infoh_found, rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_InfoGetCharAlloc(infoh_found, "/_esmf_state_reconcile/geom_type", geom_type, rc=rc)
      if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

      if (geom_type == "Grid") then
        target%ftypep%geombase%gbcp%grid = archetype_field%ftypep%geombase%gbcp%grid
      else if (geom_type == "Mesh") then
        target%ftypep%geombase%gbcp%mesh = archetype_field%ftypep%geombase%gbcp%mesh
      else if (geom_type == "XGrid") then
        target%ftypep%geombase%gbcp%xgrid = archetype_field%ftypep%geombase%gbcp%xgrid
      else if (geom_type == "LocStream") then
        target%ftypep%geombase%gbcp%locstream = archetype_field%ftypep%geombase%gbcp%locstream
      else
        if (ESMF_LogFoundError(ESMF_RC_ARG_VALUE, msg="Bad geom_type: "//trim(geom_type), &
          ESMF_CONTEXT, rcToReturn=rc)) return
      end if
    end if

  end if

  rc = ESMF_SUCCESS
end subroutine

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheReassembleFieldsFinalize()"
!BOPI
! !IROUTINE: ESMF_InfoCacheReassembleFieldsFinalize - Remove custom attributes used for StateReconcile optimizations
!
! !INTERFACE:
recursive subroutine ESMF_InfoCacheReassembleFieldsFinalize(target, rc)
! !ARGUMENTS:
  type(ESMF_State), intent(inout) :: target
  integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Remove custom attributes in the target State used for \texttt{ESMF\_StateReconcile} !
!     optimizations.
!
!     The arguments are:
!     \begin{description}
!     \item [target]
!       Input State.
!     \item [rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
  type(ESMF_FieldBundle) :: fb
  type(ESMF_Field) :: field
  type(ESMF_StateItem_Flag), dimension(:), allocatable :: stateTypes
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: stateNames
  integer :: ii, itemCount, jj
  type(ESMF_State) :: state
  character(len=ESMF_MAXSTR) :: curr_field_name
  integer :: field_count
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: field_name_list
  type(ESMF_Info) :: infoh
  logical :: isPresent, isPacked

  rc = ESMF_SUCCESS

  call ESMF_StateGet(target, itemCount=itemCount, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(stateTypes(itemCount), stateNames(itemCount))

  call ESMF_StateGet(target, itemTypeList=stateTypes, itemNameList=stateNames, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  do ii=1,itemCount
    select case (stateTypes(ii)%ot)
      case(ESMF_STATEITEM_STATE%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), state, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoCacheReassembleFieldsFinalize(state, rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      case(ESMF_STATEITEM_FIELD%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), field, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoGetFromBase(field%ftypep%base, infoh, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        isPresent = ESMF_InfoIsPresent(infoh, "_esmf_state_reconcile", rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        if (isPresent) then
          call ESMF_InfoRemove(infoh, "_esmf_state_reconcile", rc=rc)
          if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
        end if
      case(ESMF_STATEITEM_FIELDBUNDLE%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), fb, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleGet(fb, fieldCount=field_count, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(field_name_list(field_count))

        call ESMF_FieldBundleGet(fb, fieldNameList=field_name_list, isPacked=isPacked, &
          rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        if (.not. isPacked) then
          do jj=1,field_count
            call ESMF_FieldBundleGet(fb, trim(field_name_list(jj)), field=field, rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_InfoGetFromBase(field%ftypep%base, infoh, rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

            isPresent = ESMF_InfoIsPresent(infoh, "_esmf_state_reconcile", rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

            if (isPresent) then
              call ESMF_InfoRemove(infoh, "_esmf_state_reconcile", rc=rc)
              if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
            end if
          end do
        end if

        deallocate(field_name_list)
    end select
  end do

  deallocate(stateTypes, stateNames)
end subroutine

! -----------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCacheReassembleFields()"
!BOPI
! !IROUTINE: ESMF_InfoCacheReassembleFields - Reassemble all Fields in a State
!
! !INTERFACE:
recursive subroutine ESMF_InfoCacheReassembleFields(target, stateToSearch, rc)
! !ARGUMENTS:
  type(ESMF_State), intent(inout) :: target
  type(ESMF_State), intent(inout) :: stateToSearch
  integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Iterate recursively over the target State and call \texttt{ESMF\_InfoCacheReassembleField}
!     on each Field.
!
!     The arguments are:
!     \begin{description}
!     \item [target]
!       Input State.
!     \item [stateToSearch]
!       The State to search. Used recursively to control the hierarchical search
!       level. Typically, searches should always be performed on the global State.
!     \item [rc]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOPI
  type(ESMF_FieldBundle) :: fb
  type(ESMF_Field) :: field
  type(ESMF_StateItem_Flag), dimension(:), allocatable :: stateTypes
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: stateNames
  integer :: ii, itemCount, jj
  type(ESMF_State) :: state
  character(len=ESMF_MAXSTR) :: curr_field_name
  integer :: field_count
  character(len=ESMF_MAXSTR), dimension(:), allocatable :: field_name_list
  logical :: isPacked

  rc = ESMF_SUCCESS

  call ESMF_StateGet(target, itemCount=itemCount, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(stateTypes(itemCount), stateNames(itemCount))

  call ESMF_StateGet(target, itemTypeList=stateTypes, itemNameList=stateNames, rc=rc)
  if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

  do ii=1,itemCount
    select case (stateTypes(ii)%ot)
      case(ESMF_STATEITEM_STATE%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), state, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoCacheReassembleFields(state, stateToSearch, rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      case(ESMF_STATEITEM_FIELD%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), field, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_InfoCacheReassembleField(field, stateToSearch, rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
      case(ESMF_STATEITEM_FIELDBUNDLE%ot)
        call ESMF_StateGet(target, trim(stateNames(ii)), fb, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleGet(fb, fieldCount=field_count, rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(field_name_list(field_count))

        call ESMF_FieldBundleGet(fb, fieldNameList=field_name_list, isPacked=isPacked, &
          rc=rc)
        if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

        if (.not. isPacked) then
          do jj=1,field_count
            call ESMF_FieldBundleGet(fb, trim(field_name_list(jj)), field=field, rc=rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_InfoCacheReassembleField(field, stateToSearch, rc)
            if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, ESMF_CONTEXT, rcToReturn=rc)) return
          end do
        end if

        deallocate(field_name_list)
    end select
  end do

  deallocate(stateTypes, stateNames)
end subroutine

end module ESMF_InfoCacheMod ! ================================================
