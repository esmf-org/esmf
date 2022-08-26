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

  type(ESMF_Grid) :: grid6tile
  ! The following fields make up the field bundle:
  type(ESMF_Field) :: field1, field2, field1Copy
  type(ESMF_FieldBundle) :: fieldBundle
  ! This field is not in the field bundle:
  type(ESMF_Field) :: field3
  ! This is used for error testing:
  type(ESMF_Grid) :: gridSingleTile
  type(ESMF_Field) :: fieldSingleTile
  type(ESMF_FieldBundle) :: fieldBundleMixedTileCounts

  character(len=*), parameter :: fileName = "ESMF_IO_MultitileUTest#.nc"
  character(len=*), parameter :: fileNameFail = "ESMF_IO_MultitileUTestFail#.nc"

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
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
  call ESMF_FieldBundleWrite(fieldBundle, fileName=fileName, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  ! FIXME(wjs, 2022-08-26) Add tests of FieldBundleRead and FieldRead; in
  ! addition to checking rc, also check equality to the original fields

#ifdef ESMF_TESTEXHAUSTIVE
  ! The following tests don't add much code coverage, so are only done when
  ! ESMF_TESTEXHAUSTIVE is set

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Write a multi-tile Field to existing files"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldWrite(field3, fileName=fileName, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
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

  ! FIXME(wjs, 2022-08-26) Add tests of ArrayBundleWrite and ArrayWrite

  ! FIXME(wjs, 2022-08-26) Add tests of ArrayBundleRead and ArrayRead; in
  ! addition to checking rc, also check equality to the original fields

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

    field2 = ESMF_FieldCreate(grid6tile, arraySpec, name="field2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldFill(field2, dataFillScheme='sincos', member=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    field3 = ESMF_FieldCreate(grid6tile, arraySpec, name="field3", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldFill(field3, dataFillScheme='sincos', member=3, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Create a copy of field1 that uses the same array, so we can test writing
    ! the same array twice from a single call.
    call ESMF_FieldGet(field1, array=array1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    field1Copy = ESMF_FieldCreate(grid6tile, array1, name="field1Copy", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    fieldBundle = ESMF_FieldBundleCreate(name="fb", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldBundleAdd(fieldBundle, [field1, field2, field1Copy], rc=rc)
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

end program ESMF_IO_MultitileUTest
