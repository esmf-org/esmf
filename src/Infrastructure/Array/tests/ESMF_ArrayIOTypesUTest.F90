! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArrayIOTypesUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayIOTypesUTest - Tests ArrayWrite() and ArrayRead() on available types
!
! !DESCRIPTION:
! There are two purposes of this program that go beyond what's in ESMF_ArrayIOUTest:
!
! (1) This covers writing and reading additional data types
!
! (2) This works with mpiuni as well as with a real mpi library, and so adds more I/O coverage with mpiuni
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

! -------------------------------------------------------------------------
! -- The sole purpose of the netcdf/pnetcdf use statements is to trigger a
! -- compile-time error in case the ESMF module above were to "leak" NetCDF
! -- symbols.
#if (defined ESMF_NETCDF)
  use netcdf, only: nf90_nowrite, nf90_noerr
#elif (defined ESMF_PNETCDF)
  use pnetcdf, only: nf90_nowrite, nf90_noerr
#endif
! -------------------------------------------------------------------------

  implicit none
   
!-------------------------------------------------------------------------
!=========================================================================

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! local variables
  type(ESMF_VM):: vm
  integer :: rc
  integer :: localPet, petCount
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Array) :: arrayInt, arrayFloat, arrayDouble
  type(ESMF_Array) :: arrayIntRead, arrayFloatRead, arrayDoubleRead
  integer, pointer :: arrayIntData(:,:), arrayIntReadData(:,:)
  real(ESMF_KIND_R4), pointer :: arrayFloatData(:,:), arrayFloatReadData(:,:)
  real(ESMF_KIND_R8), pointer :: arrayDoubleData(:,:), arrayDoubleReadData(:,:)
  logical :: allEqual

  character(len=*), parameter :: fileName = "ESMF_ArrayIOTypesUTest.nc"

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create Arrays"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call createArrays(rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Write an I4 Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(arrayInt, fileName=fileName, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Write an R4 Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(arrayFloat, fileName=fileName, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Write an R8 Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayWrite(arrayDouble, fileName=fileName, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Read an I4 Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(arrayIntRead, fileName=fileName, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Read an R4 Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(arrayFloatRead, fileName=fileName, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Read an R8 Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayRead(arrayDoubleRead, fileName=fileName, rc=rc)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Confirm that Array-read array matches original for I4 Array"
  write(failMsg, *) "Read-in array differs from original"
  allEqual = all(arrayIntReadData == arrayIntData)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Confirm that Array-read array matches original for R4 Array"
  write(failMsg, *) "Read-in array differs from original"
  allEqual = all(arrayFloatReadData == arrayFloatData)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Confirm that Array-read array matches original for R8 Array"
  write(failMsg, *) "Read-in array differs from original"
  allEqual = all(arrayDoubleReadData == arrayDoubleData)
#if (defined ESMF_PIO && (defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test(allEqual, name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as expected"
  call ESMF_Test(.not. allEqual, name, failMsg, result, ESMF_SRCLINE)
#endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

  subroutine createArrays(rc)
    ! Creates Arrays used by the tests used in this module
    integer, intent(out) :: rc

    type(ESMF_ArraySpec) :: arraySpecInt
    type(ESMF_ArraySpec) :: arraySpecFloat
    type(ESMF_ArraySpec) :: arraySpecDouble
    integer :: i, j

    distgrid = ESMF_DistGridCreate(minIndex=[1,1], maxIndex=[3*petCount,4], regDecomp=[petCount,1], rc=rc)
    if (rc /= ESMF_SUCCESS) return

    call ESMF_ArraySpecSet(arraySpecInt, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArraySpecSet(arraySpecFloat, typekind=ESMF_TYPEKIND_R4, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArraySpecSet(arraySpecDouble, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    
    arrayInt = ESMF_ArrayCreate(distgrid, arraySpecInt, name="arrayInt", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(arrayInt, farrayPtr=arrayIntData, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    do j = 1, size(arrayIntData, 2)
      do i = 1, size(arrayIntData, 1)
        arrayIntData(i,j) = (localPet+1) * 17 * ((i-1)*size(arrayIntData,2) + (j-1))
      end do
    end do

    arrayIntRead = ESMF_ArrayCreate(distgrid, arraySpecInt, name="arrayInt", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(arrayIntRead, farrayPtr=arrayIntReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    arrayFloat = ESMF_ArrayCreate(distgrid, arraySpecFloat, name="arrayFloat", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(arrayFloat, farrayPtr=arrayFloatData, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    do j = 1, size(arrayFloatData, 2)
      do i = 1, size(arrayFloatData, 1)
        arrayFloatData(i,j) = (localPet+1) * 27.0 * ((i-1)*size(arrayFloatData,2) + (j-1))
      end do
    end do

    arrayFloatRead = ESMF_ArrayCreate(distgrid, arraySpecFloat, name="arrayFloat", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(arrayFloatRead, farrayPtr=arrayFloatReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    arrayDouble = ESMF_ArrayCreate(distgrid, arraySpecDouble, name="arrayDouble", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(arrayDouble, farrayPtr=arrayDoubleData, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    do j = 1, size(arrayDoubleData, 2)
      do i = 1, size(arrayDoubleData, 1)
        arrayDoubleData(i,j) = (localPet+1) * 37.0 * ((i-1)*size(arrayDoubleData,2) + (j-1))
      end do
    end do

    arrayDoubleRead = ESMF_ArrayCreate(distgrid, arraySpecDouble, name="arrayDouble", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(arrayDoubleRead, farrayPtr=arrayDoubleReadData, rc=rc)
    if (rc /= ESMF_SUCCESS) return

  end subroutine createArrays

end program ESMF_ArrayIOTypesUTest