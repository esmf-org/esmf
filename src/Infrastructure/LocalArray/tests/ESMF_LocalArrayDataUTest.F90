! $Id: ESMF_LocalArrayDataUTest.F90,v 1.3.2.4 2009/01/21 21:25:22 cdeluca Exp $
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
program ESMF_LocalArrayDataUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_LocalArrayDataTest - Check LocalArray data storage integrity
!
! !DESCRIPTION:
!
! The code in this file drives F90 LocalArrayData unit tests.
! The companion file ESMF\_LocalArray.F90 contains the definitions for the
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
    '$Id: ESMF_LocalArrayDataUTest.F90,v 1.3.2.4 2009/01/21 21:25:22 cdeluca Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer :: i, j, imin, jmin, imax, jmax
  logical :: looptest
  type(ESMF_LocalArray) :: la

  ! F90 array pointer of 4-byte integers
  integer (ESMF_KIND_I4),dimension(:), pointer :: data
  integer (ESMF_KIND_I4),dimension(:), pointer :: fptr, fptr2

  ! F90 array pointer of 4-byte integers
  integer (ESMF_KIND_I4),dimension(:,:), pointer :: data2d
  integer (ESMF_KIND_I4),dimension(:,:), pointer :: fptr2d, fptr2d2, fptr2dslice

  ! F90 derived type with F90 array pointer member of 4-byte integers
  type PtrIWrap1
  sequence
    integer (ESMF_KIND_I4),dimension(:), pointer :: data
  end type

  type(PtrIWrap1) :: sizetest1I

    

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------


  ! prepare for F90 allocatable array "data"
  allocate(data(-12:-6), stat=rc)
  do i = -12, -6
    data(i) = i*1000
  enddo

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a LocalArray from an allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  la = ESMF_LocalArrayCreate(data, ESMF_DATA_COPY, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing LocalArray created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayPrint(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    if (fptr(i) /= data(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values via fptr
  do i = -12, -6
    fptr(i) = fptr(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying that original data was not overridden by copy"
  write(failMsg, *) "Data changed incorrectly"
  looptest = .true.
  do i = -12, -6
    if (fptr(i) /= data(i)) looptest = .false.
  enddo
  call ESMF_Test(.not.looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    if (fptr2(i) /= fptr(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying LocalArray created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayDestroy(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! garbage collection
  deallocate(data, stat=rc)


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------


  ! prepare for F90 array pointer member "data" in F90 derived type
  allocate(sizetest1I%data(-12:-6), stat=rc)
  do i = -12, -6
    sizetest1I%data(i) = i*1000 - 11
  enddo

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a LocalArray from an allocated F90 Pointer within ",&
    "derived type"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  la = ESMF_LocalArrayCreate(sizetest1I%data, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing LocalArray created from an allocated F90 Pointer ",&
    "within derived type"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayPrint(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    if (fptr(i) /= sizetest1I%data(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Fill in different values via fptr
  do i = -12, -6
    fptr(i) = fptr(i) + 57
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    if (fptr2(i) /= fptr(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying that original data was overridden by reference"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do i = -12, -6
    if (fptr(i) /= sizetest1I%data(i)) looptest = .false.
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying LocalArray created from an allocated F90 ",&
    "Pointer within derived type"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayDestroy(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! garbage collection
  deallocate(sizetest1I%data, stat=rc)


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------


  ! prepare for F90 allocatable array "data2d"
  allocate(data2d(10,6), stat=rc)
  do j = 1, 6
    do i = 1, 10
      data2d(i,j) = j*1000 + i
    enddo
  enddo

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a LocalArray from an allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  la = ESMF_LocalArrayCreate(data2d, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing LocalArray created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayPrint(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = 1, 6
    do i = 1, 10
      if (fptr2d(i,j) /= data2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values via fptr
  do j = 1, 6
    do i = 1, 10
      fptr2d(i,j) = fptr2d(i,j) + 57
    enddo
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d2, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = 1, 6
    do i = 1, 10
      if (fptr2d2(i,j) /= fptr2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying that original data was overridden by reference"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = 1, 6
    do i = 1, 10
      if (fptr2d(i,j) /= data2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying LocalArray created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayDestroy(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! garbage collection
  deallocate(data2d, stat=rc)


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------


  ! prepare for F90 allocatable array "data2d"
  imin = 1
  imax = 100
  jmin = 1
  jmax = 60
  allocate(data2d(100,60), stat=rc)
  do j = jmin, jmax
    do i = imin, imax
      data2d(i,j) = j*1000 + i
    enddo
  enddo

  fptr2dslice => data2d(:,:)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a LocalArray from an allocated F90 array pointer slice (:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  la = ESMF_LocalArrayCreate(fptr2dslice, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing LocalArray created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayPrint(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d(i,j) /= data2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values via fptr
  do j = jmin, jmax
    do i = imin, imax
      fptr2d(i,j) = fptr2d(i,j) + 57
    enddo
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d2, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d2(i,j) /= fptr2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying that original data was overridden by reference"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d(i,j) /= data2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying LocalArray created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayDestroy(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  ! change slice
  fptr2dslice => data2d(:,20:20)
  jmin=1
  jmax=1  

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a LocalArray from an allocated F90 array pointer slice (:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  la = ESMF_LocalArrayCreate(fptr2dslice, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing LocalArray created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayPrint(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d(i,j) /= fptr2dslice(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values via fptr
  do j = jmin, jmax
    do i = imin, imax
      fptr2d(i,j) = fptr2d(i,j) + 57
    enddo
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d2, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d2(i,j) /= fptr2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying that original data was overridden by reference"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d(i,j) /= fptr2dslice(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying LocalArray created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayDestroy(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! change slice
  fptr2dslice => data2d(:,11:25)
  jmin=1
  jmax=15

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a LocalArray from an allocated F90 array pointer slice (:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  la = ESMF_LocalArrayCreate(fptr2dslice, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Printing LocalArray created from allocated F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayPrint(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d(i,j) /= fptr2dslice(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  ! Fill in different values via fptr
  do j = jmin, jmax
    do i = imin, imax
      fptr2d(i,j) = fptr2d(i,j) + 57
    enddo
  enddo
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtaining access to data in LocalArray via F90 array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayGet(la, fptr2d2, ESMF_DATA_REF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying data in LocalArray via F90 array pointer access"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d2(i,j) /= fptr2d(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verifying that original data was overridden by reference"
  write(failMsg, *) "Incorrect data detected"
  looptest = .true.
  do j = jmin, jmax
    do i = imin, imax
      if (fptr2d(i,j) /= fptr2dslice(i,j)) looptest = .false.
    enddo
  enddo
  call ESMF_Test(looptest, name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying LocalArray created from an allocated F90 ",&
    "array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_LocalArrayDestroy(la, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! garbage collection
  deallocate(data2d, stat=rc)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_LocalArrayDataUTest
