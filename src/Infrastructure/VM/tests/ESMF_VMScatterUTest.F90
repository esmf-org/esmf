! $Id: ESMF_VMScatterUTest.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
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

program ESMF_VMScatterUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMScatterUTest - Unit test for VM Scatter Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM Scatter tests.  The VM
!   Scatter  function is complex enough to require a separate test file.
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
    '$Id: ESMF_VMScatterUTest.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: name
  character(ESMF_MAXSTR) :: failMsg

  ! local variables
  integer::  rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: nlen, nsize, i, scatterRoot
  integer, allocatable:: array1(:), array2(:)
  real(ESMF_KIND_R8), allocatable:: farray1(:), farray2(:)
  real(ESMF_KIND_R4), allocatable:: f4array1(:), f4array2(:)
  integer, allocatable:: sendCounts(:), sendOffsets(:)

!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !------------------------------------------------------------------------

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)

  scatterRoot = 0
  ! allocate data arrays
  nsize = 2
  nlen = nsize * petCount
  allocate(array1(nlen))
  allocate(array2(nsize))
  allocate(farray1(nlen))
  allocate(farray2(nsize))
  allocate(f4array1(nlen))
  allocate(f4array2(nsize))

  ! prepare data array1
  do i=1, nlen
    array1(i) = localPet * 100 + i
    farray1(i) = real(array1(i),ESMF_KIND_R8)
    f4array1(i) = farray1(i)
  enddo

  ! prepare data array2
  do i=1, nsize
    array2(i) = 0
    farray2(i) = 0.
    f4array2(i) = 0.
  enddo

  ! Testing with Integer arguments
  !===============================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Scatter from scatterRoot
  write(name, *) "Scatter() Test for Integer"
  write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
  call ESMF_VMScatter(vm, sendData=array1, recvData=array2, count=nsize, &
    root=scatterRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array1 data after scatter
  write(name, *) "Verifying array1 data after Scatter() Test for Interger"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nlen
    if (array1(i)/=(localPet * 100 + i)) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nlen
    print *, localPet," array1: ", array1(i)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array2 data after scatter
  write(name, *) "Verifying array2 data after Scatter() Test for Interger"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (array2(i)/=(scatterRoot * 100 + i + 2 * localPet)) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nsize
    print *, localPet," array2: ", array2(i)
  enddo


  !Testing with ESMF_KIND_R8 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Scatter from scatterRoot
  write(name, *) "Scatter() Test for ESMF_KIND_R8"
  write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
  call ESMF_VMScatter(vm, sendData=farray1, recvData=farray2, count=nsize, &
    root=scatterRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray1 data after scatter
  write(name, *) "Verifying farray1 data after Scatter() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nlen
    if (farray1(i)/=(real(localPet*100+i,ESMF_KIND_R8))) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nlen
    print *, localPet," farray1: ", farray1(i)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray2 data after scatter
  write(name, *) "Verifying farray2 data after Scatter() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (farray2(i)/=(real(scatterRoot*100+i+2*localPet,ESMF_KIND_R8))) &
      rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nsize
    print *, localPet," farray2: ", farray2(i)
  enddo

  !Testing with ESMF_KIND_R4 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Scatter from scatterRoot
  write(name, *) "Scatter() Test for ESMF_KIND_R4"
  write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
  call ESMF_VMScatter(vm, sendData=f4array1, recvData=f4array2, count=nsize, &
    root=scatterRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array1 data after scatter
  write(name, *) "Verifying f4array1 data after Scatter() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nlen
    if (f4array1(i)/=(real(localPet*100+i,ESMF_KIND_R4))) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nlen
    print *, localPet," f4array1: ", f4array1(i)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array2 data after scatter
  write(name, *) "Verifying f4array2 data after Scatter() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (f4array2(i)/=(real(scatterRoot*100+i+2*localPet,ESMF_KIND_R4))) &
      rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nsize
    print *, localPet," f4array2: ", f4array2(i)
  enddo


  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! Next test the same as above but for ScatterV()
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------


  ! prepare data array1
  do i=1, nlen
    array1(i) = localPet * 100 + i
    farray1(i) = real(array1(i),ESMF_KIND_R8)
    f4array1(i) = farray1(i)
  enddo

  ! prepare data array2
  do i=1, nsize
    array2(i) = 0
    farray2(i) = 0.
    f4array2(i) = 0.
  enddo
  
  ! recvCounts and recvOffsets
  allocate(sendCounts(petCount))
  sendCounts = nsize
  allocate(sendOffsets(petCount))
  sendOffsets = 0 ! initialize
  do i=2, petCount
    sendOffsets(i) = sendOffsets(i-1) + sendCounts(i-1)
  enddo


  ! Testing with Integer arguments
  !===============================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Scatter from scatterRoot
  write(name, *) "ScatterV() Test for Integer"
  write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
  call ESMF_VMScatterV(vm, sendData=array1, sendCounts=sendCounts, &
    sendOffsets=sendOffsets, recvData=array2, recvCount=nsize, &
    root=scatterRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array1 data after scatter
  write(name, *) "Verifying array1 data after ScatterV() Test for Interger"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nlen
    if (array1(i)/=(localPet * 100 + i)) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nlen
    print *, localPet," array1: ", array1(i)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array2 data after scatter
  write(name, *) "Verifying array2 data after ScatterV() Test for Interger"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (array2(i)/=(scatterRoot * 100 + i + 2 * localPet)) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nsize
    print *, localPet," array2: ", array2(i)
  enddo


  !Testing with ESMF_KIND_R8 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Scatter from scatterRoot
  write(name, *) "ScatterV() Test for ESMF_KIND_R8"
  write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
  call ESMF_VMScatterV(vm, sendData=farray1, sendCounts=sendCounts, &
    sendOffsets=sendOffsets, recvData=farray2, recvCount=nsize, &
    root=scatterRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray1 data after scatter
  write(name, *) "Verifying farray1 data after ScatterV() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nlen
    if (farray1(i)/=(real(localPet*100+i,ESMF_KIND_R8))) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nlen
    print *, localPet," farray1: ", farray1(i)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray2 data after scatter
  write(name, *) "Verifying farray2 data after ScatterV() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (farray2(i)/=(real(scatterRoot*100+i+2*localPet,ESMF_KIND_R8))) &
      rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nsize
    print *, localPet," farray2: ", farray2(i)
  enddo

  !Testing with ESMF_KIND_R4 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Scatter from scatterRoot
  write(name, *) "ScatterV() Test for ESMF_KIND_R4"
  write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
  call ESMF_VMScatterV(vm, sendData=f4array1, sendCounts=sendCounts, &
    sendOffsets=sendOffsets, recvData=f4array2, recvCount=nsize, &
    root=scatterRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array1 data after scatter
  write(name, *) "Verifying f4array1 data after ScatterV() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nlen
    if (f4array1(i)/=(real(localPet*100+i,ESMF_KIND_R4))) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nlen
    print *, localPet," f4array1: ", f4array1(i)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array2 data after scatter
  write(name, *) "Verifying f4array2 data after ScatterV() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (f4array2(i)/=(real(scatterRoot*100+i+2*localPet,ESMF_KIND_R4))) &
      rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after scatter:"
  do i=1, nsize
    print *, localPet," f4array2: ", f4array2(i)
  enddo


  !------------------------------------------------------------------------
  deallocate(array1)
  deallocate(array2)
  deallocate(farray1)
  deallocate(farray2)
  deallocate(f4array1)
  deallocate(f4array2)
  deallocate(sendCounts)
  deallocate(sendOffsets)

  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

end program ESMF_VMScatterUTest
