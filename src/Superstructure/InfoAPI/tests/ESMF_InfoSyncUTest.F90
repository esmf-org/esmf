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

#define ESMF_FILENAME "ESMF_InfoSyncUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_InfoSyncUTest

  !============================================================================
  !BOP
  ! !PROGRAM: ESMF_InfoUTest - Test general Info usage
  !
  ! !DESCRIPTION:
  !
  !----------------------------------------------------------------------------
  ! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF

  use ESMF_InfoCacheMod

  implicit none

  !----------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = '$Id$'
  !----------------------------------------------------------------------------

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  ! cumulative result: count failures; no failures equals "all pass"
  integer               :: result = 0, count
  integer :: rc, petCount, localPet, n
  type(ESMF_VM) :: vm

  type(ESMF_Array) :: arr, arr2
  type(ESMF_ArrayBundle) :: ab
  type(ESMF_Pointer) :: eptr
  type(ESMF_Info) :: infoh, desired_info
  type(ESMF_Field) :: field, field2, field3, field4, foundField
  type(ESMF_FieldBundle) :: fb
  type(ESMF_DistGrid) :: distgrid, distgrid1d, distgrid2
  type(ESMF_Grid) :: grid
  type(ESMF_LocStream) :: locstream
  type(ESMF_RouteHandle) :: rh
  type(ESMF_State) :: state, nested_state, nested_state2
  type(ESMF_InfoDescribe) :: eidesc, desired_eidesc, ainq, search_idesc
  integer :: rootPet=0, find_base_id, tk_check
  integer(ESMF_KIND_I8), dimension(:), allocatable :: bases
  type(ESMF_Info) :: search_criteria
  logical :: isDirty, found
  character(len=ESMF_MAXSTR) :: found_field_name
  type(ESMF_TypeKind_Flag) :: tk

  !----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  call ESMF_VMGetCurrent(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying a handle"
  write(failMsg, *) "Did not raise error"
  rc = ESMF_FAILURE

  distgrid2 = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(distgrid2, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(infoh, rc=rc)
  call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_DistGridDestroy(distgrid2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "General"
  write(failMsg, *) "Did not sync successfully"
  rc = ESMF_FAILURE

  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  grid = ESMF_GridCreate(distgrid=distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  distgrid1d = ESMF_DistGridCreate(minIndex=(/1/),maxIndex=(/10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  locstream = ESMF_LocStreamCreate(distgrid=distgrid1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  field2 = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I8, name="search_target", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  field3 = ESMF_FieldCreate(locstream, ESMF_TYPEKIND_I4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fb = ESMF_FieldBundleCreate(fieldList=(/field,field2/), name="mr_field_bundle", &
   rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  arr = ESMF_ArrayCreate(distgrid, ESMF_TYPEKIND_I4, rc=rc, name="iarr_i4")
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arr2 = ESMF_ArrayCreate(distgrid, ESMF_TYPEKIND_I8, rc=rc, name="iarr_i8")
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ab = ESMF_ArrayBundleCreate(arrayList=(/arr,arr2/), name="my_arrays", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  rh = ESMF_RouteHandleCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_RouteHandleSet(rh, name="dummy", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  nested_state2 = ESMF_StateCreate(fieldbundleList=(/fb/), name="nested_state2", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  nested_state = ESMF_StateCreate(arrayList=(/arr, arr2/), routehandleList=(/rh/), &
   nestedStateList=(/nested_state2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  state = ESMF_StateCreate(arrayList=(/arr,arr2/), arrayBundleList=(/ab/), fieldList=(/field3/), &
   fieldbundleList=(/fb/), nestedStateList=(/nested_state/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------

  if (localPet == rootPet) then
    call ESMF_InfoGetFromHost(state, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "state", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(state, "is_32bit", tk_check, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(fb, "is_32bit2", tk_check, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(nested_state2, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "nested_state2", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(field, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "field", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(field2, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "field2", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Add some Field finding metadata for testing. This is StateReconcile
    ! specific.
    call ESMF_InfoSet(infoh, "/_esmf_state_reconcile/integer_vmid", 567, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(field3, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "field3", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(arr, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "arr", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(ab, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "ab", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoGetFromHost(locstream, infoh, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(infoh, "fvarname", "locstream", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  if (localPet == 0) then
    call desired_eidesc%Initialize(addObjectInfo=.true., rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call desired_eidesc%Update(state, "", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    desired_info%ptr = desired_eidesc%info%ptr
  else
    desired_info = ESMF_InfoCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  call ESMF_InfoBroadcast(desired_info, 0, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSync(state, rootPet, vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call eidesc%Initialize(addObjectInfo=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call eidesc%Update(state, "", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  call ESMF_LogWrite("eidesc%info="//ESMF_InfoDump(eidesc%info))
  call ESMF_LogWrite("desired_info="//ESMF_InfoDump(desired_info))
#endif

  call ESMF_Test((eidesc%info == desired_info), name, failMsg, result, ESMF_SRCLINE)

  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check 32-bit preserved on top-level state"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE

  tk = ESMF_TYPEKIND_I8
  call ESMF_AttributeGet(state, "is_32bit", typekind=tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(tk==ESMF_TYPEKIND_I4, name, failMsg, result, ESMF_SRCLINE)

  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check 32-bit preserved on field bundle"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE

  tk = ESMF_TYPEKIND_I8
  call ESMF_AttributeGet(fb, "is_32bit2", typekind=tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(tk==ESMF_TYPEKIND_I4, name, failMsg, result, ESMF_SRCLINE)

  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check Dirty State"
  write(failMsg, *) "Did not remain dirty"
  rc = ESMF_FAILURE

  call ESMF_InfoGetFromHost(locstream, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(infoh, isDirty=isDirty, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(isDirty, name, failMsg, result, ESMF_SRCLINE)

  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mark Clean"
  write(failMsg, *) "Did not clean"
  rc = ESMF_FAILURE

  call ESMF_InfoSync(state, rootPet, vm, markClean=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(infoh, isDirty=isDirty, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(isDirty .neqv. .true., name, failMsg, result, ESMF_SRCLINE)
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Find a Field"
  write(failMsg, *) "Did not succeed"
  rc = ESMF_FAILURE

  call ESMF_BaseGetID(field2%ftypep%base, find_base_id, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  found = ESMF_InfoCacheFindField(state, foundField, 567, find_base_id, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(foundField, name=found_field_name, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((found .and. (trim(found_field_name)=="search_target")), name, failMsg, result, ESMF_SRCLINE)
  ! ---------------------------------------------------------------------------

  call eidesc%Destroy(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (localPet == 0) then
    call desired_eidesc%Destroy(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    call ESMF_InfoDestroy(desired_info, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  call ESMF_StateDestroy(state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(nested_state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(nested_state2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayBundleDestroy(ab, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(arr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(arr2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(fb, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridDestroy(grid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LocStreamDestroy(locstream, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_RouteHandleDestroy(rh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally

end program ESMF_InfoSyncUTest
