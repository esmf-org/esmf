! $Id: ESMF_VMGatherUTest.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
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

program ESMF_VMGatherUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMGatherUTest - Unit test for VM Gather Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM  Gather tests.  The VM
! Gather function is complex enough to require a separate test file.
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
    '$Id: ESMF_VMGatherUTest.F90,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $'
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
  integer:: nlen, nsize, i, gatherRoot
  integer, allocatable:: array1(:), array2(:)
  real(ESMF_KIND_R8), allocatable:: farray1(:), farray2(:)
  real(ESMF_KIND_R4), allocatable:: f4array1(:), f4array2(:)
  integer, allocatable:: recvCounts(:), recvOffsets(:)
     
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

  gatherRoot = petCount - 1
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
    array1    = 0
    farray1   = 0
    f4array1  = 0
  enddo

  ! prepare data array2
  do i=1, nsize
    array2(i)   = 2*localPet + i
    farray2(i)  = real( 2*localPet + i , ESMF_KIND_R8)
    f4array2(i) = farray2(i)
  enddo

  !Testing with Integer arguments
  !==============================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Gather from gatherRoot
  write(name, *) "Gather() Test for Integer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=nsize, &
    root=gatherRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array1 data after gather
  write(name, *) "Verifying array1 data after Gather() Test for Integer"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  if (localPet==gatherRoot) then
    do i=1, nlen
      if (array1(i)/=i) rc = ESMF_FAILURE
    enddo
  else
    do i=1, nlen
      if (array1(i)/=0) rc = ESMF_FAILURE
    enddo
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "Contents after gather (should be 1,2,... at ", &
    "localPet=petCount-1 , 0 elsewhere), :"
  do i=1,nlen
    print *, localPet," array1: ", array1(i)
  end do

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array2 data after gather
  write(name, *) "Verifying array2 data after Gather() Test for Integer"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (array2(i)/=(i + 2 * localPet)) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !Testing with ESMF_KIND_R8 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Gather from gatherRoot
  write(name, *) "Gather() Test for ESMF_KIND_R8"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGather(vm, sendData=farray2, recvData=farray1, count=nsize, &
    root=gatherRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray1 data after gather
  write(name, *) "Verifying farray1 data after Gather() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  if (localPet==gatherRoot) then
    do i=1, nlen
      if (farray1(i)/=real(i,ESMF_KIND_R8)) rc = ESMF_FAILURE
    enddo
  else
    do i=1, nlen
      if (farray1(i)/=0.) rc = ESMF_FAILURE
    enddo
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after gather (should be 1.,2.,... at ", &
    "localPet=petCount-1 , 0. elsewhere), :"
  do i=1,nlen
    print *, localPet," farray1: ", farray1(i)
  end do

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray2 data after gather
  write(name, *) "Verifying farray2 data after Gather() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (farray2(i)/=real(i + 2 * localPet,ESMF_KIND_R8)) rc = ESMF_FAILURE
  enddo

  print *, "contents after gather (should be localPet*2+1, localPet*2+2):"
  do i=1, nsize
    print *, localPet," farray2: ", farray2(i)
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !Testing with ESMF_KIND_R4 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Gather from gatherRoot
  write(name, *) "Gather() Test for ESMF_KIND_R4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGather(vm, sendData=f4array2, recvData=f4array1, count=nsize, &
    root=gatherRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array1 data after gather
  write(name, *) "Verifying f4array1 data after Gather() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  if (localPet==gatherRoot) then
    do i=1, nlen
      if (f4array1(i)/=real(i,ESMF_KIND_R4)) rc = ESMF_FAILURE
    enddo
  else
    do i=1, nlen
      if (f4array1(i)/=0.) rc = ESMF_FAILURE
    enddo
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after gather (should be 1.,2.,... at ", &
    "localPet=petCount-1 , 0. elsewhere), :"
  do i=1,nlen
    print *, localPet," f4array1: ", f4array1(i)
  end do

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array2 data after gather
  write(name, *) "Verifying f4array2 data after Gather() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (f4array2(i)/=real(i + 2 * localPet,ESMF_KIND_R4)) rc = ESMF_FAILURE
  enddo

  print *, "contents after gather (should be localPet*2+1, localPet*2+2):"
  do i=1, nsize
    print *, localPet," f4array2: ", f4array2(i)
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! Next test the same as above but for GatherV()
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------


  ! prepare data array1
  do i=1, nlen
    array1    = 0
    farray1   = 0
    f4array1  = 0
  enddo

  ! prepare data array2
  do i=1, nsize
    array2(i)   = 2*localPet + i
    farray2(i)  = real( 2*localPet + i , ESMF_KIND_R8)
    f4array2(i) = farray2(i)
  enddo
  
  ! recvCounts and recvOffsets
  allocate(recvCounts(petCount))
  recvCounts = nsize
  allocate(recvOffsets(petCount))
  recvOffsets = 0 ! initialize
  do i=2, petCount
    recvOffsets(i) = recvOffsets(i-1) + recvCounts(i-1)
  enddo

  !Testing with Integer arguments
  !==============================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Gather from gatherRoot
  write(name, *) "GatherV() Test for Integer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGatherV(vm, sendData=array2, sendCount=nsize, recvData=array1, &
    recvCounts=recvCounts, recvOffsets=recvOffsets, root=gatherRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array1 data after gather
  write(name, *) "Verifying array1 data after GatherV() Test for Integer"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  if (localPet==gatherRoot) then
    do i=1, nlen
      if (array1(i)/=i) rc = ESMF_FAILURE
    enddo
  else
    do i=1, nlen
      if (array1(i)/=0) rc = ESMF_FAILURE
    enddo
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "Contents after gather (should be 1,2,... at ", &
    "localPet=petCount-1 , 0 elsewhere), :"
  do i=1,nlen
    print *, localPet," array1: ", array1(i)
  end do

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify array2 data after gather
  write(name, *) "Verifying array2 data after GatherV() Test for Integer"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (array2(i)/=(i + 2 * localPet)) rc = ESMF_FAILURE
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !Testing with ESMF_KIND_R8 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Gather from gatherRoot
  write(name, *) "GatherV() Test for ESMF_KIND_R8"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGatherV(vm, sendData=farray2, sendCount=nsize, recvData=farray1, &
    recvCounts=recvCounts, recvOffsets=recvOffsets, root=gatherRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray1 data after gather
  write(name, *) "Verifying farray1 data after GatherV() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  if (localPet==gatherRoot) then
    do i=1, nlen
      if (farray1(i)/=real(i,ESMF_KIND_R8)) rc = ESMF_FAILURE
    enddo
  else
    do i=1, nlen
      if (farray1(i)/=0.) rc = ESMF_FAILURE
    enddo
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after gather (should be 1.,2.,... at ", &
    "localPet=petCount-1 , 0. elsewhere), :"
  do i=1,nlen
    print *, localPet," farray1: ", farray1(i)
  end do

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify farray2 data after gather
  write(name, *) "Verifying farray2 data after GatherV() Test for ESMF_KIND_R8"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (farray2(i)/=real(i + 2 * localPet,ESMF_KIND_R8)) rc = ESMF_FAILURE
  enddo

  print *, "contents after gather (should be localPet*2+1, localPet*2+2):"
  do i=1, nsize
    print *, localPet," farray2: ", farray2(i)
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !Testing with ESMF_KIND_R4 arguments
  !===================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Gather from gatherRoot
  write(name, *) "GatherV() Test for ESMF_KIND_R4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGatherV(vm, sendData=f4array2, sendCount=nsize, recvData=f4array1,&
    recvCounts=recvCounts, recvOffsets=recvOffsets, root=gatherRoot, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array1 data after gather
  write(name, *) "Verifying f4array1 data after GatherV() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  if (localPet==gatherRoot) then
    do i=1, nlen
      if (f4array1(i)/=real(i,ESMF_KIND_R4)) rc = ESMF_FAILURE
    enddo
  else
    do i=1, nlen
      if (f4array1(i)/=0.) rc = ESMF_FAILURE
    enddo
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  print *, "contents after gather (should be 1.,2.,... at ", &
    "localPet=petCount-1 , 0. elsewhere), :"
  do i=1,nlen
    print *, localPet," f4array1: ", f4array1(i)
  end do

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Verify f4array2 data after gather
  write(name, *) "Verifying f4array2 data after GatherV() Test for ESMF_KIND_R4"
  write(failMsg, *) "Wrong data."
  rc = ESMF_SUCCESS
  do i=1, nsize
    if (f4array2(i)/=real(i + 2 * localPet,ESMF_KIND_R4)) rc = ESMF_FAILURE
  enddo

  print *, "contents after gather (should be localPet*2+1, localPet*2+2):"
  do i=1, nsize
    print *, localPet," f4array2: ", f4array2(i)
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  deallocate(array1)
  deallocate(array2)
  deallocate(farray1)
  deallocate(farray2)
  deallocate(f4array1)
  deallocate(f4array2)
  deallocate(recvCounts)
  deallocate(recvOffsets)

  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

end program ESMF_VMGatherUTest
