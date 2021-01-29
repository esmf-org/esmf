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

#define ESMF_FILENAME "ESMF_InfoCacheUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_InfoCacheUTest

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

  type(ESMF_InfoCache) :: info_cache
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Grid) :: grid
  type(ESMF_Field) :: field, field2, field_empty
  type(ESMF_FieldBundle) :: fb
  type(ESMF_State) :: state
  type(ESMF_InfoDescribe) :: idesc
  integer, dimension(:), allocatable :: ids
  type(ESMF_VMId), dimension(0:0), target :: vmIds
  type(ESMF_VMId), dimension(:), pointer :: vmIds_ptr, vmIdMap_ptr
  type(ESMF_VMId), dimension(:), allocatable, target :: vmIdMap
  logical :: actual_sdflag_archetype, actual_sdflag_referencer
  type(ESMF_Info) :: infoh

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
  write(name, *) "Initialize + Destroy"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE

  call info_cache%Initialize(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call info_cache%Destroy(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Update for State"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE

  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  grid = ESMF_GridCreate(distgrid=distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field_empty = ESMF_FieldEmptyCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fb = ESMF_FieldBundleCreate(fieldList=(/field, field2, field_empty/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  state = ESMF_StateCreate(fieldbundleList=(/fb/), &
                           fieldList=(/field, field2, field_empty/), &
                           rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the vmIdMap
  ! ---------------------------------------------------------------------------
  call ESMF_BaseGetVMId(field%ftypep%base, vmIds(0), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  vmIds_ptr => vmIds

  call ESMF_VMTranslateVMId(vm, vmIds_ptr, ids, vmIdMap=vmIdMap, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  vmIdMap_ptr => vmIdMap
  ! ---------------------------------------------------------------------------

  call info_cache%Initialize(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call info_cache%UpdateFields(state, vmIdMap_ptr, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call idesc%Initialize(createInfo=.true., addObjectInfo=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call idesc%Update(state, "", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("idesc dump=", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite(ESMF_InfoDump(idesc%info, indent=2), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(field, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(infoh, &
                    "/_esmf_state_reconcile/should_serialize_geom", &
                    actual_sdflag_archetype, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(field2, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(infoh, &
                    "/_esmf_state_reconcile/should_serialize_geom", &
                    actual_sdflag_referencer, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc==ESMF_SUCCESS &
                  .and. actual_sdflag_archetype &
                  .and. .not. actual_sdflag_referencer), &
                 name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  call ESMF_VMIdDestroy(vmIdMap, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call idesc%Destroy(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call info_cache%Destroy(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(state, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleDestroy(fb, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldDestroy(field2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridDestroy(grid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally

end program ESMF_InfoCacheUTest
