! $Id: ESMF_VMGatherUTest.F90,v 1.2.4.3 2007/10/18 02:43:36 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_VMGatherUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

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
      '$Id: ESMF_VMGatherUTest.F90,v 1.2.4.3 2007/10/18 02:43:36 cdeluca Exp $'
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
      integer:: nlen, nsize, i, gatherRoot
      integer, allocatable:: array1(:), array2(:)
      real(ESMF_KIND_R8), allocatable:: farray1(:), farray2(:)
      real(ESMF_KIND_R4), allocatable:: f4array1(:), f4array2(:)
     

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
        array1(i) = 0
        farray1(i) = 0
        f4array1(i) = 0
      enddo

      ! prepare data array2
      do i=1, nsize
        array2(i) = 2*localPet + i
        farray2(i) = real( 2*localPet + i)
        f4array2(i) = farray2(i)
      enddo

      !Testing with Integer arguments
      !==============================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Gather from gatherRoot
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Gather Test"
      call ESMF_VMGather(vm, sendData=array2, recvData=array1, count=nsize, &
      root=gatherRoot, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify array1 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array1 data after gather Test"
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

      print *, 'contents after gather (should be 1,2,... at &
     & localPet=petCount-1 , 0 elsewhere), :'
        do i=1,nlen
          print *, localPet,' array1: ', array1(i)
        end do

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify array2 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array2 data after gather Test"
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
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Gather Test"
      call ESMF_VMGather(vm, sendData=farray2, recvData=farray1, count=nsize, &
      root=gatherRoot, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify farray1 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying farray1 data after gather Test"
      rc = ESMF_SUCCESS
      if (localPet==gatherRoot) then
        do i=1, nlen
                if (farray1(i)/=real(i)) rc = ESMF_FAILURE
        enddo
      else
        do i=1, nlen
                if (farray1(i)/=0.) rc = ESMF_FAILURE
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after gather (should be 1.,2.,... at &
     & localPet=petCount-1 , 0. elsewhere), :'
        do i=1,nlen
          print *, localPet,' farray1: ', farray1(i)
        end do

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify farray2 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying farray2 data after gather Test"
      rc = ESMF_SUCCESS
      do i=1, nsize
        if (farray2(i)/=real(i + 2 * localPet)) rc = ESMF_FAILURE
      enddo

      print *, 'contents after gather (should be localPet*2+1, localPet*2+2):'
      do i=1, nsize
        print *, localPet,' farray2: ', farray2(i)
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !Testing with ESMF_KIND_R4 arguments
      !===================================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Gather from gatherRoot
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Gather Test"
      call ESMF_VMGather(vm, sendData=f4array2, recvData=f4array1, count=nsize, &
      root=gatherRoot, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify f4array1 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying f4array1 data after gather Test"
      rc = ESMF_SUCCESS
      if (localPet==gatherRoot) then
        do i=1, nlen
                if (f4array1(i)/=real(i)) rc = ESMF_FAILURE
        enddo
      else
        do i=1, nlen
                if (f4array1(i)/=0.) rc = ESMF_FAILURE
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after gather (should be 1.,2.,... at &
     & localPet=petCount-1 , 0. elsewhere), :'
        do i=1,nlen
          print *, localPet,' f4array1: ', f4array1(i)
        end do

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify f4array2 data after gather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying f4array2 data after gather Test"
      rc = ESMF_SUCCESS
      do i=1, nsize
        if (f4array2(i)/=real(i + 2 * localPet)) rc = ESMF_FAILURE
      enddo

      print *, 'contents after gather (should be localPet*2+1, localPet*2+2):'
      do i=1, nsize
        print *, localPet,' f4array2: ', f4array2(i)
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMGatherUTest
