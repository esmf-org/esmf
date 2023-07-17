! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IO_GDALUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_IO_MultitileUTest - Unit tests of IO on multi-tile fields / arrays
! !DESCRIPTION:
!
! The tests in this file target IO on multi-tile fields / arrays. These tests
! are designed to be run on 8 processors (due to the decompositions used in the
! tests).
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  type(ESMF_VM) :: vm
  integer :: localPet
  type(ESMF_Grid) :: grid6tile
  type(ESMF_DistGrid) :: distgrid3tile

  ! Fields used for writing:
  !
  ! The following fields make up the field bundle:
  type(ESMF_Field) :: field1, field2, field1Copy, field4d
  real(ESMF_KIND_R8), pointer :: field1Data(:,:), field2Data(:,:), field1CopyData(:,:), field4dData(:,:,:,:)
  type(ESMF_FieldBundle) :: fieldBundle
  ! This field is not in the field bundle:
  type(ESMF_Field) :: field3
  real(ESMF_KIND_R8), pointer :: field3Data(:,:)

  ! Fields used for reading:
  !
  ! The following fields make up the field bundle:
  type(ESMF_Field) :: field1Read, field2Read, field1CopyRead, field4dRead
  real(ESMF_KIND_R8), pointer :: field1ReadData(:,:), field2ReadData(:,:), field1CopyReadData(:,:), field4dReadData(:,:,:,:)
  type(ESMF_FieldBundle) :: fieldBundleRead
  ! This field is not in the field bundle:
  type(ESMF_Field) :: field3Read
  real(ESMF_KIND_R8), pointer :: field3ReadData(:,:)

  ! This is used for error testing:
  type(ESMF_Grid) :: gridSingleTile
  type(ESMF_Field) :: fieldSingleTile
  type(ESMF_FieldBundle) :: fieldBundleMixedTileCounts

  ! Arrays used for writing:
  !
  ! The following arrays make up the array bundle:
  type(ESMF_Array) :: array1, array2
  real(ESMF_KIND_R8), pointer :: array1Data(:,:), array2Data(:,:)
  type(ESMF_ArrayBundle) :: arrayBundle
  ! This array is not in the array bundle:
  type(ESMF_Array) :: array3
  real(ESMF_KIND_R8), pointer :: array3Data(:,:)

  ! Arrays used for reading:
  !
  ! The following arrays make up the array bundle:
  type(ESMF_Array) :: array1Read, array2Read
  real(ESMF_KIND_R8), pointer :: array1ReadData(:,:), array2ReadData(:,:)
  type(ESMF_ArrayBundle) :: arrayBundleRead
  ! This array is not in the array bundle:
  type(ESMF_Array) :: array3Read
  real(ESMF_KIND_R8), pointer :: array3ReadData(:,:)

  logical :: allEqual

  character(len=*), parameter :: fileNameFields = "ESMF_IO_GDALUTestFields#.shp"

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! --- 
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Read a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldRead(field3Read, fileName=fileNameFields, iofmt=ESMF_IOFMT_SHP, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_GDAL))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
!>>  write(name, *) "Confirm that Field-read field matches original"
!>>  write(failMsg, *) "Read-in field differs from original"
!>>  allEqual = all(field3ReadData == field3Data)
!>>#if (defined ESMF_PIO && (defined ESMF_GDAL))
!>>  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
!>>#else
!>>  write(failMsg, *) "Comparison did not fail as expected"
!>>  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
!>>#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

end program ESMF_IO_GDALUTest
