! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_VMAllGatherVUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMAllGatherVUTest - Unit test for VM AllGatherV Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM  AllGatherV tests.  The VM
! AllGatherV function is complex enough to require a separate test file.
!   It runs on a single and multiple processors.
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
      integer:: nlen, nsize
      integer:: i, j, idx

      integer, allocatable:: array1(:), array2(:), array3(:), array4(:), array5(:)
      integer(ESMF_KIND_I4), allocatable:: i4array1(:), i4array2(:), i4array5(:)
      real(ESMF_KIND_R8), allocatable:: farray1(:), farray2(:), farray5(:)
      real(ESMF_KIND_R4), allocatable:: f4array1(:), f4array2(:), f4array5(:)
      type(ESMF_VMId),    allocatable:: vmidarray1(:), vmidarray2(:), vmidarray3(:)

      integer   :: idData
      character :: keyData
      logical:: all_verify


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

      ! allocate data arrays
      nsize = 2
      nlen = (petCount * (petCount + 1 ))/2
      allocate(array1(nlen))
      allocate(i4array1(nlen))
      allocate(farray1(nlen))
      allocate(f4array1(nlen))
      allocate(vmidarray1(nlen))

      allocate(array2(localPet+1))
      allocate(i4array2(localPet+1))
      allocate(farray2(localPet+1))
      allocate(f4array2(localPet+1))
      allocate(vmidarray2(localPet+1))

      allocate(array3(petCount))
      allocate(array4(petCount))
      allocate(array5(nlen))
      allocate(i4array5(nlen))
      allocate(farray5(nlen))
      allocate(f4array5(nlen))

      ! prepare data array1
      do i=1, nlen
        array1(i) = 0
        i4array1(i) = 0
        farray1(i) = 0
        f4array1(i) = 0
      enddo

      ! prepare data array2
      do i=1, (localPet + 1)
        array2(i) = i
        i4array2(i) = i
        farray2(i) = real( 2*localPet + i)
        f4array2(i) = farray2(i)
      enddo

      ! prepare data array3
      do i=1, petCount
	array3(i) = i
      enddo

      ! prepare data array4
      if (petCount ==1) then
	array4 = (/0/)
      else
      	array4 = (/0,1,3,6/)
      end if

      ! prepare data array5 
      if (petCount ==1) then
	array5 = (/1/)
	i4array5 = (/1/)
	farray5 = (/1/)
	f4array5 = (/1/)
      else
      	array5 = (/1,1,2,1,2,3,1,2,3,4/)
      	i4array5 = (/1,1,2,1,2,3,1,2,3,4/)
      	farray5 = (/1,3,4,5,6,7,7,8,9,10/)
      	f4array5 = (/1,3,4,5,6,7,7,8,9,10/)
      end if

      !Testing with Integer arguments
      !==============================
      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "AllGatherV Integer Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS."
      call ESMF_VMAllGatherV(vm, sendData=array2, sendCount=(localPet + 1),  &
		recvData=array1, recvCounts=array3, recvOffsets=array4, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify array1 data after allgather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying array1 integer data after allgatherv Test"
      rc = ESMF_SUCCESS
      do i=1, nlen
		if (array1(i)/=array5(i)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after allgather (should be /1,1,2,1,2,3,1,2,3,4/)'
        do i=1,nlen
          print *, localPet,' array1: ', array1(i)
        end do

      !------------------------------------------------------------------------


      !Testing with 4 byte Integer arguments
      !==============================
      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "AllGatherV 4-Byte Integer Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS."
      call ESMF_VMAllGatherV(vm, sendData=i4array2, sendCount=(localPet + 1),  &
                recvData=i4array1, recvCounts=array3, recvOffsets=array4, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify i4array1 data after allgather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying i4array1 4 byte integer data after allgatherv Test"
      rc = ESMF_SUCCESS
      do i=1, nlen
                if (i4array1(i)/=i4array5(i)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after allgather (should be /1,1,2,1,2,3,1,2,3,4/)'
        do i=1,nlen
          print *, localPet,' i4array1: ', i4array1(i)
        end do

      !------------------------------------------------------------------------

      !Testing with ESMF_KIND_R8 arguments
      !===================================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! AllGatherV Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "AllGatherV R8 Test"
      call ESMF_VMAllGatherV(vm, sendData=farray2, sendCount=(localPet + 1),  &
				recvData=farray1, recvCounts=array3, recvOffsets=array4, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify farray1 data after allgather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying farray1 R8 data after allgatherv Test"
      rc = ESMF_SUCCESS
      do i=1, nlen
                if (farray1(i)/=farray5(i)) rc = ESMF_FAILURE
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after allgather (should be /1,3,4,5,6,7,7,8,9,10/)'
        do i=1,nlen
          print *, localPet,' farray1: ', farray1(i)
        end do

      !------------------------------------------------------------------------

      !Testing with ESMF_KIND_R4 arguments
      !===================================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! AllGatherV test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "AllGatherV R4 Test"
      call ESMF_VMAllGatherV(vm, sendData=f4array2, sendCount=(localPet + 1),  &
                                recvData=f4array1, recvCounts=array3, recvOffsets=array4, rc=rc)

      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify f4array1 data after allgather
      write(failMsg, *) "Wrong data."
      write(name, *) "Verifying f4array1 R4 data after allgatherv Test"
      rc = ESMF_SUCCESS
      do i=1, nlen
                if (f4array1(i)/=f4array5(i)) rc = ESMF_FAILURE
       enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after allgather (should be /1,3,4,5,6,7,7,8,9,10/)'
        do i=1,nlen
          print *, localPet,' f4array1: ', f4array1(i)
        end do

      !------------------------------------------------------------------------

      !Testing with ESMF_VMId arguments
      !===================================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Initialize deep data
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Creating VMId solutions array"
      call ESMF_VMIdCreate (vmidarray1, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Insert dummy data for the purposes of the test.  Only
      ! the first character of the key is set.
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Initializing VMId solutions array Test"
      rc = ESMF_SUCCESS
      do, i=1, size (vmidarray1)
        call c_ESMCI_VMIdSet (vmidarray1(i), 0, 0, localrc)
        if (localrc /= ESMF_SUCCESS) rc = localrc
      end do
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Initialize deep data
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Creating VMId source array"
      call ESMF_VMIdCreate (vmidarray2, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Insert dummy data for the purposes of the test.  Only
      ! the first character of the key is set.
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Inserting dummy data into VMId source array Test"
      rc = ESMF_SUCCESS
      do, i=1, size (vmidarray2)
        call c_ESMCI_VMIdSet (vmidarray2(i), i, achar (i+10), localrc)
        if (localrc /= ESMF_SUCCESS) rc = localrc
      end do
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! AllGatherV - each PET broadcasts to all other PETs
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "AllGatherVing VMId data Test"
      call ESMF_VMAllGatherV (vm,  &
          sendData=vmidarray2, sendCount=size (vmidarray2),  &
          recvData=vmidarray1, recvCounts=array3, recvOffsets=array4,  &
          rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM AllGatherV.  Only
      ! the first character of the key is tested.
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "ID/Key extraction VMId data after broadcast Test"
      all_verify = .true.
      rc = ESMF_SUCCESS
      idx = 1
      do, i=1, petCount
        do, j=1, i
          call c_ESMCI_VMIdGet (vmidarray1(idx), idData, keyData, localrc)
          if (localrc /= ESMF_SUCCESS) rc = localrc
          if (idData /= j .or. iachar (keyData) /= j+10) then
            print *, 'PET', localPet, ' - non-compare: index =', i,  &
                     ', idData =', idData, ', keyData =', iachar(keyData)
            all_verify = .false.
          end if
          idx = idx+1
        end do
      end do
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast.  Only
      ! the first character of the key is tested.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify VMId data after broadcast Test"
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Release VMId resources
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Releasing VMId result array Test"
      call ESMF_VMIdDestroy (vmidarray1, rc=rc)
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Release VMId resources
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Releasing VMId source array Test"
      call ESMF_VMIdDestroy (vmidarray2, rc=rc)
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(ESMF_SRCLINE)

      end program ESMF_VMAllGatherVUTest
