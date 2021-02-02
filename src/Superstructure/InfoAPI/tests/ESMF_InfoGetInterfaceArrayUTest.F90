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

#define ESMF_FILENAME "ESMF_InfoGetInterfaceArrayUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_ArrayInfoUTest

  !============================================================================
  !BOP
  ! !PROGRAM: ESMF_ArrayInfoUTest - Test general Info usage
  !
  ! !DESCRIPTION:
  !
  !----------------------------------------------------------------------------
  ! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_InfoMod

  implicit none

  !----------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = '$Id$'
  !----------------------------------------------------------------------------

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  character(ESMF_MAXSTR) :: key = "foo"
  integer                :: rc, petCount, ii, nloops = 100
  type(ESMF_VM)          :: vm
  ! cumulative result: count failures; no failures equals "all pass"
  integer                :: result = 0

  integer(ESMF_KIND_I4) :: desired = 999, actual
  type(ESMF_Info) :: info, info2, infoh
  type(ESMF_Array) :: array, array2
  type(ESMF_DistGrid) :: distgrid, distgrid2
  real(ESMF_KIND_R8) :: farray2D(10,10)
  logical :: failed

  !----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Get and manipulate an Array attribute"
  write(failMsg, *) "Did not set/get attribute"
  rc = ESMF_FAILURE

  failed = .false.

  ! Do a number of loop to catch memory leaks, etc.
  do ii=1,nloops
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    array = ESMF_ArrayCreate(distgrid, farray2D, ESMF_INDEX_GLOBAL, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get the attribute object created with the array
    call ESMF_InfoGetFromHost(array, info, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get a different reference to the same attributes object
    call ESMF_InfoGetFromHost(array, info2, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Set a key/value on the attributes object
    call ESMF_InfoSet(info, key, desired, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get the attribute value from the second reference
    call ESMF_InfoGet(info2, key, actual, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Destroy objects
    call ESMF_ArrayDestroy(array, noGarbage=.true., rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, noGarbage=.true., rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Fail if the retrieved attribute value is bad
    if (actual /= desired) then
      failed = .true.
      exit
    end if

  end do

  call ESMF_Test((.not. failed), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Deleting a host object and using a handle/view"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  distgrid2 = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  array2 = ESMF_ArrayCreate(distgrid2, farray2D, ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetFromHost(array2, infoh, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(array2, noGarbage=.false., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(infoh, "key", "value", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(rc==ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_DistGridDestroy(distgrid2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !----------------------------------------------------------------------------

end program ESMF_ArrayInfoUTest
