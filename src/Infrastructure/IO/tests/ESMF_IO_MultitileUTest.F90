! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IO_MultitileUTest

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
  type(ESMF_Field) :: field1, field2, field1Copy
  real(ESMF_KIND_R8), pointer :: field1Data(:,:), field2Data(:,:), field1CopyData(:,:)
  type(ESMF_FieldBundle) :: fieldBundle
  ! This field is not in the field bundle:
  type(ESMF_Field) :: field3
  real(ESMF_KIND_R8), pointer :: field3Data(:,:)

  ! Fields used for reading:
  !
  ! The following fields make up the field bundle:
  type(ESMF_Field) :: field1Read, field2Read, field1CopyRead
  real(ESMF_KIND_R8), pointer :: field1ReadData(:,:), field2ReadData(:,:), field1CopyReadData(:,:)
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

  character(len=*), parameter :: fileNameFields = "ESMF_IO_MultitileUTestFields#.nc"
  character(len=*), parameter :: fileNameArrays = "ESMF_IO_MultitileUTestArrays#.nc"
  character(len=*), parameter :: fileNameFail = "ESMF_IO_MultitileUTestFail#.nc"

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create fields for multitile IO tests"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call createFields(rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Write a FieldBundle with multi-tile fields"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldBundleWrite(fieldBundle, fileName=fileNameFields, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Read a FieldBundle with multi-tile fields"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldBundleRead(fieldBundleRead, fileName=fileNameFields, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Confirm that FieldBundle-read fields match originals"
  write(failMsg, *) "Some read-in fields differ from originals"
  allEqual = ( &
       all(field1ReadData == field1Data) .and. &
       all(field2ReadData == field2Data) .and. &
       all(field1CopyReadData == field1CopyData))
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
  ! The following tests don't add much code coverage, so are only done when
  ! ESMF_TESTEXHAUSTIVE is set

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Write a multi-tile Field to existing files"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldWrite(field3, fileName=fileNameFields, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! The purpose of this test is to make sure that the code for checking consistency with
  ! an existing field works for multi-tile fields (at least in the case where they *are*
  ! consistent).
  write(name, *) "Rewrite an existing multi-tile Field to existing files"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldWrite(field3, fileName=fileNameFields, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Read a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldRead(field3Read, fileName=fileNameFields, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Confirm that Field-read field matches original"
  write(failMsg, *) "Read-in field differs from original"
  allEqual = all(field3ReadData == field3Data)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create single-tile field for failure testing"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call createSingleTileField(rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Failure test: try to write fields with different tile counts"
  write(failMsg, *) "Did not return ESMF_RC_VAL_WRONG"
  call ESMF_FieldBundleWrite(fieldBundleMixedTileCounts, fileName=fileNameFail, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_RC_VAL_WRONG), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Create arrays for multitile IO tests"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call createArrays(rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Write an ArrayBundle with multi-tile arrays"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleWrite(arrayBundle, fileName=fileNameArrays, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Read an ArrayBundle with multi-tile arrays"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRead(arrayBundleRead, fileName=fileNameArrays, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Confirm that ArrayBundle-read arrays match originals"
  write(failMsg, *) "Some read-in arrays differ from originals"
  allEqual = ( &
       all(array1ReadData == array1Data) .and. &
       all(array2ReadData == array2Data))
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Write a multi-tile Array to existing files"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(array3, fileName=fileNameArrays, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Read a multi-tile Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(array3Read, fileName=fileNameArrays, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Confirm that Array-read array matches original"
  write(failMsg, *) "Read-in array differs from original"
  allEqual = all(array3ReadData == array3Data)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

#endif  ! ESMF_TESTEXHAUSTIVE

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

  subroutine createFields(rc)
    ! Creates Fields and FieldBundles used by the tests in this module
    integer, intent(out) :: rc

    integer :: decompPTile(2,6)
    type(ESMF_ArraySpec) :: arraySpec
    type(ESMF_Array) :: array1

    !------------------------------------------------------------------------
    ! Set up 6-tile grid
    !------------------------------------------------------------------------

    ! Decomposition for 8 PEs: Tiles 1 and 3 each have two DEs (along different
    ! dimensions); the other tiles each have one DE.
    decompPTile(1,:) = [2,1,1,1,1,1]
    decompPTile(2,:) = [1,1,2,1,1,1]
    grid6tile = ESMF_GridCreateCubedSphere( &
         tilesize = 4, &
         regDecompPTile = decompPTile, &
         staggerLocList = [ESMF_STAGGERLOC_CENTER], &
         rc = rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    ! Create fields on the 6-tile grid and associated field bundle
    !------------------------------------------------------------------------

    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field1 = ESMF_FieldCreate(grid6tile, arraySpec, name="field1", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldFill(field1, dataFillScheme='sincos', member=1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field1, farrayPtr=field1Data, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field1Read = ESMF_FieldCreate(grid6tile, arraySpec, name="field1", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field1Read, farrayPtr=field1ReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field2 = ESMF_FieldCreate(grid6tile, arraySpec, name="field2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldFill(field2, dataFillScheme='sincos', member=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field2, farrayPtr=field2Data, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field2Read = ESMF_FieldCreate(grid6tile, arraySpec, name="field2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field2Read, farrayPtr=field2ReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field3 = ESMF_FieldCreate(grid6tile, arraySpec, name="field3", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldFill(field3, dataFillScheme='sincos', member=3, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field3, farrayPtr=field3Data, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field3Read = ESMF_FieldCreate(grid6tile, arraySpec, name="field3", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field3Read, farrayPtr=field3ReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Create a copy of field1 that uses the same array, so we can test writing
    ! the same array twice from a single call.
    call ESMF_FieldGet(field1, array=array1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    field1Copy = ESMF_FieldCreate(grid6tile, array1, name="field1Copy", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field1Copy, farrayPtr=field1CopyData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field1CopyRead = ESMF_FieldCreate(grid6tile, arraySpec, name="field1Copy", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(field1CopyRead, farrayPtr=field1CopyReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    fieldBundle = ESMF_FieldBundleCreate(name="fb", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldBundleAdd(fieldBundle, [field1, field2, field1Copy], rc=rc)
    if (rc /= ESMF_SUCCESS) return

    fieldBundleRead = ESMF_FieldBundleCreate(name="fbRead", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldBundleAdd(fieldBundleRead, [field1Read, field2Read, field1CopyRead], rc=rc)
    if (rc /= ESMF_SUCCESS) return

  end subroutine createFields

  subroutine createSingleTileField(rc)
    ! Creates a single-tile field and associated field bundle for failure testing
    integer, intent(out) :: rc

    type(ESMF_ArraySpec) :: arraySpec

    !------------------------------------------------------------------------
    ! Set up a single-tile grid
    !------------------------------------------------------------------------

    gridSingleTile = ESMF_GridCreateNoPeriDimUfrm( &
         maxIndex = [4,4], &
         minCornerCoord = [0._ESMF_KIND_R8, 0._ESMF_KIND_R8], &
         maxCornerCoord = [4._ESMF_KIND_R8, 4._ESMF_KIND_R8], &
         staggerLocList = [ESMF_STAGGERLOC_CENTER], &
         rc = rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    ! Create a field on the single-tile grid and associated field bundle
    !------------------------------------------------------------------------

    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    fieldSingleTile = ESMF_FieldCreate(gridSingleTile, arraySpec, name="fieldSingleTile", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldFill(fieldSingleTile, dataFillScheme='sincos', rc=rc)
    if (rc /= ESMF_SUCCESS) return

    fieldBundleMixedTileCounts = ESMF_FieldBundleCreate(name="fbmixed", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldBundleAdd(fieldBundleMixedTileCounts, [field1, fieldSingleTile], rc=rc)
    if (rc /= ESMF_SUCCESS) return

  end subroutine createSingleTileField

  subroutine createArrays(rc)
    ! Creates Arrays and ArrayBundles used by the tests in this module
    integer, intent(out) :: rc

    ! Information for a 2-d, 3-tile array
    integer :: minIndexPTile(2,3)
    integer :: maxIndexPTile(2,3)
    integer :: decompPTile(2,3)

    type(ESMF_ArraySpec) :: arraySpec

    !------------------------------------------------------------------------
    ! Set up 3-tile distgrid
    !------------------------------------------------------------------------

    minIndexPTile(:,1) = [11,1]
    maxIndexPTile(:,1) = [20,10]
    minIndexPTile(:,2) = [11,11]
    maxIndexPTile(:,2) = [20,20]
    minIndexPTile(:,3) = [1,11]
    maxIndexPTile(:,3) = [10,20]

    ! Decomposition for 8 PEs: Tiles 1 and 3 each have 2 DEs (along different
    ! dimensions); tile 2 has 4 DEs (2x2)
    decompPTile(:,1) = [1,2]
    decompPTile(:,2) = [2,2]
    decompPTile(:,3) = [2,1]

    distgrid3tile = ESMF_DistGridCreate( &
         minIndexPTile = minIndexPTile, &
         maxIndexPTile = maxIndexPTile, &
         regDecompPTile = decompPTile, &
         rc = rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    ! Create arrays on the 3-tile distgrid and associated array bundle
    !------------------------------------------------------------------------

    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    array1 = ESMF_ArrayCreate(distgrid3tile, arraySpec, name="array1", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(array1, farrayPtr=array1Data, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call fillArray(array1Data, 1)

    array1Read = ESMF_ArrayCreate(distgrid3tile, arraySpec, name="array1", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(array1Read, farrayPtr=array1ReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    array2 = ESMF_ArrayCreate(distgrid3tile, arraySpec, name="array2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(array2, farrayPtr=array2Data, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call fillArray(array2Data, 2)

    array2Read = ESMF_ArrayCreate(distgrid3tile, arraySpec, name="array2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(array2Read, farrayPtr=array2ReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    array3 = ESMF_ArrayCreate(distgrid3tile, arraySpec, name="array3", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(array3, farrayPtr=array3Data, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call fillArray(array3Data, 3)

    array3Read = ESMF_ArrayCreate(distgrid3tile, arraySpec, name="array3", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(array3Read, farrayPtr=array3ReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    arrayBundle = ESMF_ArrayBundleCreate(arrayList=[array1, array2], name="ab", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    arrayBundleRead = ESMF_ArrayBundleCreate(arrayList=[array1Read, array2Read], name="abRead", rc=rc)
    if (rc /= ESMF_SUCCESS) return

  end subroutine createArrays

  subroutine fillArray(array, multiplier)
    ! Fill the given 2-d array based on indices times a multiplier
    real(ESMF_KIND_R8), intent(out) :: array(:,:)
    integer, intent(in) :: multiplier
    integer :: i, j

    do j = 1, size(array, 2)
       do i = 1, size(array, 1)
          array(i,j) = (localPet+1) * multiplier * ((i-1)*size(array,2) + (j-1))
       end do
    end do
  end subroutine fillArray

end program ESMF_IO_MultitileUTest
