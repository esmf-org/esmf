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
  type(ESMF_Mesh) :: mesh

  ! Fields used for reading:
  !
  ! The following fields make up the field bundle:
  type(ESMF_Field) :: field1Read, field2Read, field1CopyRead, field4dRead
  real(ESMF_KIND_R8), pointer :: field1ReadData(:,:), field2ReadData(:,:), field1CopyReadData(:,:), field4dReadData(:,:,:,:)
  type(ESMF_FieldBundle) :: fieldBundleRead
  ! This field is not in the field bundle:
  type(ESMF_Field) :: field
  real(ESMF_KIND_R8), pointer :: fieldReadData(:)

  type(ESMF_ArraySpec) :: arraySpec

  character(len=*), parameter :: fileNameFields = "data/complex_3.shp"
!  character(len=*), parameter :: fileNameFields = "data/cb_2018_us_county_20m.shp"

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! Create Mesh from shape file
  write(name, *) "Creating a Mesh to use in Field Tests"
  mesh=ESMF_MeshCreate(fileNameFields, &
       fileformat=ESMF_FILEFORMAT_SHAPEFILE, &
       rc=rc)
  if (rc /= ESMF_SUCCESS) return
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------

  write(name, *) "Get a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arraySpec, 1, typekind=ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) return
!  field = ESMF_FieldCreate(mesh, arraySpec, name="GEOID", meshLoc=ESMF_MESHLOC_ELEMENT, rc=rc)
  field = ESMF_FieldCreate(mesh, arraySpec, name="DistFld", meshLoc=ESMF_MESHLOC_ELEMENT, rc=rc)
  if (rc /= ESMF_SUCCESS) return
!  call ESMF_FieldPrint(field, rc=rc)
!  if (rc /= ESMF_SUCCESS) return
  call ESMF_FieldGet(field, farrayPtr=fieldReadData, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_GDAL))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Read a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldRead(field, fileName=fileNameFields, iofmt=ESMF_IOFMT_SHP, rc=rc)
  call ESMF_FieldWrite(field, "test.shp", iofmt=ESMF_IOFMT_SHP, overwrite=.true.,rc=rc)
!  call ESMF_FieldWrite(field, "test.nc", rc=rc)
#if (defined ESMF_PIO && (defined ESMF_GDAL))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

end program ESMF_IO_GDALUTest
