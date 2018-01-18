! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_VMAllToAllUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMAllToAllUTest - Unit test for VMAllToAll Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VMAllToAll tests.
!   It runs on multiple processors.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0


  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! local variables
  integer::  localrc, rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: nlen1, nlen2, nsize, i, j, k
  integer,            allocatable:: iarray1(:),  iarray2(:),  iarray3(:)
  real(ESMF_KIND_R4), allocatable:: r4array1(:), r4array2(:), r4array3(:)
  real(ESMF_KIND_R4), allocatable:: r4array2_expected(:)
  real(ESMF_KIND_R8), allocatable:: r8array1(:), r8array2(:), r8array3(:)
  real(ESMF_KIND_R8), allocatable:: r8array2_expected(:)

     
  real(ESMF_KIND_R4)  :: r4value
  real(ESMF_KIND_R8)  :: r8value

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

  ! allocate arrays
  allocate(iarray1(0:petCount-1))
  allocate(iarray2(0:petCount-1))
  allocate(iarray3(0:petCount-1))

  allocate(r4array1(0:petCount-1))
  allocate(r4array2(0:petCount-1))
  allocate(r4array2_expected(0:petCount-1))
  allocate(r4array3(0:petCount-1))

  allocate(r8array1(0:petCount-1))
  allocate(r8array2(0:petCount-1))
  allocate(r8array2_expected(0:petCount-1))
  allocate(r8array3(0:petCount-1))

  ! prepare data arrays

  iarray1 = localPet
  iarray2 = -1
  iarray3 = -2

  do, i=0, petCount-1
    r4value = real(i, ESMF_KIND_R4) * 1.01_ESMF_KIND_R4
    r8value = real(i, ESMF_KIND_R8) * 1.01_ESMF_KIND_R8
    if (i == localPet) then
      r4array1 = r4value
      r8array1 = r8value
    end if
    r4array2_expected(i) = r4value
    r8array2_expected(i) = r8value
  end do

  r4array2 = -1.1_ESMF_KIND_R4
  r4array3 = -2.2_ESMF_KIND_R4

  r8array2 = -1.1_ESMF_KIND_R8
  r8array3 = -2.2_ESMF_KIND_R8

  !Testing with Integer arguments
  !==============================
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAll Test iarray1 -> iarray2"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAll(vm,  &
      sendData=iarray1, sendCount=1, &
      recvData=iarray2, recvCount=1, &
      rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify iarray2 data after alltoall
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify iarray2 data after alltoall"
  rc = ESMF_SUCCESS
  do i=0, petCount-1
    if (iarray2(i) /= i) then
      rc = ESMF_FAILURE
      print *, i, iarray2(i)
    endif
  enddo
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAll Test iarray2 -> iarray3"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAll(vm,  &
      sendData=iarray2, sendCount=1, &
      recvData=iarray3, recvCount=1, &
      rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify iarray3 data against iarray1 after alltoallv
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify iarray3 data against iarray1 after alltoallv"
  rc = ESMF_SUCCESS
  do i=0, petCount-1
    if (iarray3(i)/=iarray1(i)) then
      rc = ESMF_FAILURE
      print *, i, iarray1(i), iarray3(i)
    endif
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !Testing with single precision real arguments
  !============================================
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAll Test r4array1 -> r4array2"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAll(vm,  &
      sendData=r4array1, sendCount=1, &
      recvData=r4array2, recvCount=1, &
      rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify r4array2 data after alltoall
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify r4array2 data after alltoall"
  rc = ESMF_SUCCESS
  do i=0, petCount-1
    if (r4array2(i) /= r4array2_expected(i)) then
      rc = ESMF_FAILURE
      print *, i, r4array2(i), r4array2_expected(i)
    endif
  enddo
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAll Test r4array2 -> r4array3"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAll(vm,  &
      sendData=r4array2, sendCount=1, &
      recvData=r4array3, recvCount=1, &
      rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify r4array3 data against r4array1 after alltoallv
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify r4array3 data against r4array1 after alltoallv"
  rc = ESMF_SUCCESS
  do i=0, petCount-1
    if (r4array3(i)/=r4array1(i)) then
      rc = ESMF_FAILURE
      print *, i, r4array1(i), r4array3(i)
    endif
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !Testing with double precision real arguments
  !============================================
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAll Test r8array1 -> r8array2"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAll(vm,  &
      sendData=r8array1, sendCount=1, &
      recvData=r8array2, recvCount=1, &
      rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify r8array2 data after alltoall
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify r8array2 data after alltoall"
  rc = ESMF_SUCCESS
  do i=0, petCount-1
    if (r8array2(i) /= r8array2_expected(i)) then
      rc = ESMF_FAILURE
      print *, i, r8array2(i), r8array2_expected(i)
    endif
  enddo
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAll Test r8array2 -> r8array3"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAll(vm,  &
      sendData=r8array2, sendCount=1, &
      recvData=r8array3, recvCount=1, &
      rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify r8array3 data against r8array1 after alltoallv
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify r8array3 data against r8array1 after alltoallv"
  rc = ESMF_SUCCESS
  do i=0, petCount-1
    if (r8array3(i)/=r8array1(i)) then
      rc = ESMF_FAILURE
      print *, i, r8array1(i), r8array3(i)
    endif
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_TestEnd(ESMF_SRCLINE)

  ! garbage collection
  deallocate(iarray1)
  deallocate(iarray2)
  deallocate(iarray3)

  deallocate(r4array1)
  deallocate(r4array2)
  deallocate(r4array2_expected)
  deallocate(r4array3)

  deallocate(r8array1)
  deallocate(r8array2)
  deallocate(r8array2_expected)
  deallocate(r8array3)

end program ESMF_VMAllToAllUTest
