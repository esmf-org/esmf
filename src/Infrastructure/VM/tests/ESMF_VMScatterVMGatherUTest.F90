! $Id: ESMF_VMScatterVMGatherUTest.F90,v 1.5 2004/12/09 00:25:20 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_VMScatterVMGatherUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMScatterVMGatherUTest - Unit test for VM Scatter Gather Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM Scatter Gather tests.  The VM
!   Scatter Gather function is complex enough to require a separate test file.
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
      '$Id: ESMF_VMScatterVMGatherUTest.F90,v 1.5 2004/12/09 00:25:20 nscollins Exp $'
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
      integer:: nlen, nsize, i, scatterRoot, gatherRoot
      integer, allocatable:: array1(:), array2(:)
     
      integer :: status, myde 

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! exit early if we have less than 4 procs
      if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10
 
      ! get global vm information
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)

      scatterRoot = 0
      gatherRoot = petCount - 1
      ! allocate data arrays
      nsize = 2
      nlen = nsize * petCount
      allocate(array1(nlen))
      allocate(array2(nsize))

      ! prepare data array1
      do i=1, nlen
        array1(i) = localPet * 100 + i
      enddo

      ! prepare data array2
      do i=1, nsize
        array2(i) = 0
      enddo

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify array1 data before scatter
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array1 data before scatter Test"
      rc = ESMF_SUCCESS
      do i=1, nlen
	if (array1(i)/=(localPet * 100 + i)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents before scatter:'
      do i=1, nlen
        print *, localPet,' array1: ', array1(i)
      enddo

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify array2 data before scatter
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array2 data before scatter Test"
      rc = ESMF_SUCCESS
      do i=1, nsize
	if (array2(i)/=0) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents before scatter:'
      do i=1, nsize
        print *, localPet,' array2: ', array2(i)
      enddo

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Scatter from scatterRoot
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Scatter Test"
      call ESMF_VMScatter(vm, sendData=array1, recvData=array2, count=nsize, &
      root=scatterRoot, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify array1 data after scatter
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array1 data after scatter Test"
      rc = ESMF_SUCCESS
      do i=1, nlen
	if (array1(i)/=(localPet * 100 + i)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after scatter:'
      do i=1, nlen
        print *, localPet,' array1: ', array1(i)
      enddo

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify array2 data after scatter
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array2 data after scatter Test"
      rc = ESMF_SUCCESS
      do i=1, nsize
	if (array2(i)/=(scatterRoot * 100 + i + 2 * localPet)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after scatter:'
      do i=1, nsize
        print *, localPet,' array2: ', array2(i)
      enddo


      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Gather from gatherRoot
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Gather Test"
      call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=nsize, &
      root=gatherRoot, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify array1 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array1 data after gather Test"
      rc = ESMF_SUCCESS
      if (localPet==gatherRoot) then
      	do i=1, nlen
		if (array1(i)/=(scatterRoot * 100 + i)) rc = ESMF_FAILURE
      	enddo
      else
      	do i=1, nlen
		if (array1(i)/=(localPet * 100 + i)) rc = ESMF_FAILURE
      	enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after gather:'
      do i=1, nlen
        print *, localPet,' array1: ', array1(i)
      enddo

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify array2 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array2 data after gather Test"
      rc = ESMF_SUCCESS
      do i=1, nsize
	if (array2(i)/=(scatterRoot * 100 + i + 2 * localPet)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after gather:'
      do i=1, nsize
        print *, localPet,' array2: ', array2(i)
      enddo


10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMScatterVMGatherUTest
