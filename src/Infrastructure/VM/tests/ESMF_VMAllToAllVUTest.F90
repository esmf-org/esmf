! $Id: ESMF_VMAllToAllVUTest.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
program ESMF_VMAllToAllVUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMAllToAllVUTest - Unit test for VMAllToAllV Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VMAllToAllV tests.
!   It runs on multiple processors.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_VMAllToAllVUTest.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0


  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! local variables
  integer::  rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: nlen1, nlen2, nsize, i, j, k
  integer, allocatable:: sendCounts(:), sendOffsets(:)
  integer, allocatable:: recvCounts(:), recvOffsets(:)
  integer, allocatable:: iarray1(:), iarray2(:), iarray3(:)
  real(ESMF_KIND_R4), allocatable:: f4array1(:), f4array2(:), f4array3(:)
  real(ESMF_KIND_R8), allocatable:: f8array1(:), f8array2(:), f8array3(:)
     

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

  ! allocate arrays
  allocate(sendCounts(petCount))
  allocate(sendOffsets(petCount))
  allocate(recvCounts(petCount))
  allocate(recvOffsets(petCount))
  nsize = 33
  nlen1 = 0
  do i=1, petCount
    sendOffsets(i) = nlen1 * nsize
    sendCounts(i) = i * nsize
    recvOffsets(i) = (i-1) * (localPet + 1) * nsize
    recvCounts(i) = (localPet + 1) * nsize
    nlen1 = nlen1 + i
  enddo
  nlen1 = nlen1 * nsize
  nlen2 = (localPet + 1) * nsize * petCount
  allocate(iarray1(nlen1))
  allocate(iarray2(nlen2))
  allocate(iarray3(nlen1))
  allocate(f4array1(nlen1))
  allocate(f4array2(nlen2))
  allocate(f4array3(nlen1))
  allocate(f8array1(nlen1))
  allocate(f8array2(nlen2))
  allocate(f8array3(nlen1))
  

  ! prepare data array1
  k=1
  do i=1, petCount
    do j=1, nsize * i
      iarray1(k) = i + 100 * k + 10000 * j
      f4array1(k) = real(iarray1(k), ESMF_KIND_R4)
      f8array1(k) = real(iarray1(k), ESMF_KIND_R8)
      k = k+1
    enddo
  enddo

  !Testing with Integer arguments
  !==============================
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAllV Test iarray1 -> iarray2"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAllV(vm, sendData=iarray1, sendCounts=sendCounts, &
    sendOffsets=sendOffsets, recvData=iarray2, recvCounts=recvCounts, &
    recvOffsets=recvOffsets, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAllV Test iarray2 -> iarray3"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAllV(vm, sendData=iarray2, sendCounts=recvCounts, &
    sendOffsets=recvOffsets, recvData=iarray3, recvCounts=sendCounts, &
    recvOffsets=sendOffsets, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify iarray3 data against iarray1 after alltoallv
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify iarray3 data against iarray1 after alltoallv"
  rc = ESMF_SUCCESS
  do i=1, nlen1
    if (iarray3(i)/=iarray1(i)) then
      rc = ESMF_FAILURE
      print *, i, iarray1(i), iarray3(i)
    endif
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !Testing with Real*4 arguments
  !==============================
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAllV Test f4array1 -> f4array2"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAllV(vm, sendData=f4array1, sendCounts=sendCounts, &
    sendOffsets=sendOffsets, recvData=f4array2, recvCounts=recvCounts, &
    recvOffsets=recvOffsets, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAllV Test f4array2 -> f4array3"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAllV(vm, sendData=f4array2, sendCounts=recvCounts, &
    sendOffsets=recvOffsets, recvData=f4array3, recvCounts=sendCounts, &
    recvOffsets=sendOffsets, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array3 data against f4array1 after alltoallv
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify f4array3 data against f4array1 after alltoallv"
  rc = ESMF_SUCCESS
  do i=1, nlen1
    if (f4array3(i)/=f4array1(i)) then
      rc = ESMF_FAILURE
      print *, i, f4array1(i), f4array3(i)
    endif
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !Testing with Real*8 arguments
  !==============================
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAllV Test f8array1 -> f8array2"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAllV(vm, sendData=f8array1, sendCounts=sendCounts, &
    sendOffsets=sendOffsets, recvData=f8array2, recvCounts=recvCounts, &
    recvOffsets=recvOffsets, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "AllToAllV Test f8array2 -> f8array3"
  write(failMsg, *) "Did not return ESMF_SUCCESS."
  call ESMF_VMAllToAllV(vm, sendData=f8array2, sendCounts=recvCounts, &
    sendOffsets=recvOffsets, recvData=f8array3, recvCounts=sendCounts, &
    recvOffsets=sendOffsets, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f8array3 data against f8array1 after alltoallv
  write(failMsg, *) "Wrong data."
  write(name, *) "Verify f8array3 data against f8array1 after alltoallv"
  rc = ESMF_SUCCESS
  do i=1, nlen1
    if (f8array3(i)/=f8array1(i)) then
      rc = ESMF_FAILURE
      print *, i, f8array1(i), f8array3(i)
    endif
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  call ESMF_TestEnd(result, ESMF_SRCLINE)

  ! garbage collection
  deallocate(sendCounts)
  deallocate(sendOffsets)
  deallocate(recvCounts)
  deallocate(recvOffsets)
  deallocate(iarray1)
  deallocate(iarray2)
  deallocate(iarray3)
  deallocate(f4array1)
  deallocate(f4array2)
  deallocate(f4array3)
  deallocate(f8array1)
  deallocate(f8array2)
  deallocate(f8array3)

end program ESMF_VMAllToAllVUTest
