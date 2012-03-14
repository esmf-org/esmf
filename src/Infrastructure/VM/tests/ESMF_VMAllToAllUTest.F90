! $Id: ESMF_VMAllToAllUTest.F90,v 1.1 2012/03/14 17:08:41 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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
    '$Id: ESMF_VMAllToAllUTest.F90,v 1.1 2012/03/14 17:08:41 w6ws Exp $'
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
  integer, allocatable:: iarray1(:), iarray2(:), iarray3(:)
     
  integer   :: idData
  character :: keyData
  logical   :: all_verify


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
  allocate(iarray1(0:petCount-1))
  allocate(iarray2(0:petCount-1))
  allocate(iarray3(0:petCount-1))

  ! prepare data arrays

  iarray1 = localPet
  iarray2 = -1
  iarray3 = -2

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

  call ESMF_TestEnd(result, ESMF_SRCLINE)

  ! garbage collection
  deallocate(iarray1)
  deallocate(iarray2)
  deallocate(iarray3)

end program ESMF_VMAllToAllUTest
