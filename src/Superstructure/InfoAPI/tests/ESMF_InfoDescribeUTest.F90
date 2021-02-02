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

#define ESMF_FILENAME "src/Superstructure/InfoAPI/tests/ESMF_InfoDescribeUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_InfoDescribeUTest

  use ESMF_TestMod     ! test methods
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF

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

  type(ESMF_InfoDescribe) :: eidesc_create_destroy, eidesc_create_info, eidesc_create_info2, &
    eidesc_create_background, eidesc_double_init, eidesc_mesh
  logical :: isPresent
  type(ESMF_Mesh) :: mesh
  type(ESMF_Info) :: infoh, infoh2
  character(:), allocatable :: actual

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
  write(name, *) "No double initialization"
  write(failMsg, *) "Did not throw error"
  rc = ESMF_FAILURE

  call eidesc_double_init%Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call eidesc_double_init%Initialize(rc=rc)
  call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Initialize/Destroy"
  write(failMsg, *) "Did not sync successfully"
  rc = ESMF_FAILURE

  call eidesc_create_destroy%Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call eidesc_create_destroy%Destroy(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Initialize behavior"
  write(failMsg, *) "Did not behave"
  rc = ESMF_FAILURE

  call eidesc_create_info%Initialize(createInfo=.true., addBaseAddress=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(eidesc_create_info%info, "a", 1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call eidesc_create_info2%Initialize(createInfo=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(eidesc_create_info2%info, "b", 2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  isPresent = ESMF_InfoIsPresent(eidesc_create_info%info, "b", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(((isPresent .eqv. .false.) .and. &
    (eidesc_create_info2%addBaseAddress .eqv. .false.)), &
    name, failMsg, result, ESMF_SRCLINE)

  call eidesc_create_info%Destroy(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call eidesc_create_info2%Destroy(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh is enabled"
  write(failMsg, *) "Did not succeed"
  rc = ESMF_FAILURE

  call eidesc_mesh%Initialize(createInfo=.true., addBaseAddress=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  mesh = ESMF_MeshEmptyCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(mesh, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_InfoSet(infoh, "special", "mesh attribute", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call eidesc_mesh%Update(mesh, "", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  infoh2 = eidesc_mesh%GetInfo(mesh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_InfoGetCharAlloc(infoh2, "special", actual, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((eidesc_mesh%curr_base_is_valid .and. actual=="mesh attribute"), &
    name, failMsg, result, ESMF_SRCLINE)

  call eidesc_mesh%Destroy(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------

  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally

end program ESMF_InfoDescribeUTest
