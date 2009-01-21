! $Id: ESMF_ArrayDataUTest.F90,v 1.8.2.8 2009/01/21 21:25:19 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArrayDataUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayDataTest - Check Array data storage integrity
!
! !DESCRIPTION:
!
! The code in this file drives F90 Array data unit tests.
! The companion file ESMF\_Array.F90 contains the definitions for the
! LocalArray methods.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArrayDataUTest.F90,v 1.8.2.8 2009/01/21 21:25:19 cdeluca Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer :: i, j, petCount
  logical :: looptest

  ! F90 array pointer of 4-byte integers
  integer (ESMF_KIND_I4),dimension(:), pointer :: fdata
  integer (ESMF_KIND_I4),dimension(:), pointer :: fdataSlice
  integer (ESMF_KIND_I4),dimension(:), pointer :: fptr

  type(ESMF_DistGrid) :: distgrid
  type(ESMF_Array)    :: array
  type(ESMF_VM)       :: vm

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! prepare for F90 allocatable array "data"
  allocate(fdata(-12:-6), stat=rc)
  do i = -12, -6
    fdata(i) = i*1000
  enddo

  ! prepare DistGrid
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/7*petCount/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(fdata, distgrid, indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated F90 array pointer using ESMF_DATA_COPY"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(fdata, distgrid, indexflag=ESMF_INDEX_DELOCAL, &
    copyflag=ESMF_DATA_COPY, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)-57) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! test with array slice
  fdataSlice => fdata(:)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated F90 array pointer slice"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(fdataSlice, distgrid, indexflag=ESMF_INDEX_DELOCAL, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! test with array slice
  deallocate(fdata)
  allocate(fdata(-20:10))
  do i = -20, 10
    fdata(i) = i*1000
  enddo
  fdataSlice => fdata(-12:-6)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(fdataSlice, distgrid, indexflag=ESMF_INDEX_DELOCAL, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating an Array from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(fdataSlice, distgrid, indexflag=ESMF_INDEX_DELOCAL, &
    copyflag=ESMF_DATA_COPY, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in Array via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values
  do i = -12, -6
    fdata(i) = fdata(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing Array created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in Array via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    j = i + 12 + lbound(fptr, 1)
    print *, fptr(j), fdata(i)
    if (fptr(j) /= fdata(i)-57) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying Array created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! garbage collection
  deallocate(fdata, stat=rc)

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_ArrayDataUTest
