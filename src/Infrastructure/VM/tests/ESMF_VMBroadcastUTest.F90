! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_VMBroadcastUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMBroadcastUTest - Unit test for VM Send Receive Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM Broadcast tests.  The VM
!   Send Receive function is complex enough to require a separate test file.
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
      character(len=8) :: strvalue

      ! local variables
      integer:: i, j, k
      integer:: localrc, rc
      type(ESMF_VM):: vm
      integer:: localPet, petCount
      integer:: count1, count2, count3, count, root  
      integer, allocatable            :: localData(:),soln(:)
      real(ESMF_KIND_R8), allocatable :: r8_localData(:),r8_soln(:)
      real(ESMF_KIND_R4), allocatable :: r4_localData(:),r4_soln(:)

      integer, allocatable            :: localData2d(:,:), localData3d(:,:,:)

      type(ESMF_logical), allocatable :: local_logical(:),logical_soln(:)

      type(ESMF_VMId), allocatable :: local_vmids(:), vmids_soln(:)
      integer   :: idData
      character :: keyData
      logical   :: all_verify

      integer :: isum
      real(ESMF_KIND_R8) :: R8Sum
      real(ESMF_KIND_R4) :: R4Sum

      integer, parameter :: n_elements = 4

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! Get count of PETs and which PET number we are
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

      ! Allocate localData
      count1  = 5
      count2  = 4
      count3  = 2
      count   = count1 * count2 * count3
      allocate(localData(count))
      allocate(localData2d(count1, count2*count3))
      allocate(localData3d(count1, count2, count3))
      allocate(r8_localData(count))
      allocate(r4_localData(count))
      allocate(local_logical(count))

      ! Allocate the solution arrays
      allocate(soln(count))
      allocate(r8_soln(count))
      allocate(r4_soln(count))
      allocate(logical_soln(count))

      !Assign values
      do i=1,count
        localData(i)    = localPet*100+i
        r4_localData(i) = real( localData(i) , ESMF_KIND_R4 )
        r8_localData(i) = real( localData(i) , ESMF_KIND_R8 )
        if (mod(localData(i)+localPet,2).eq.0) then
          local_logical(i)= ESMF_TRUE
        else
          local_logical(i)= ESMF_FALSE
        endif
      enddo
      
      do j=1,count2*count3
        do i=1,count1
          localData2d(i,j) = localPet*100+(i+(j-1)*count1)
        enddo
      enddo
      
      do k=1, count3
        do j=1, count2
          do i=1, count1
            localData3d(i,j,k) = localPet*100+(i+(j-1)*count1+(k-1)*count1*count2)
          enddo
        enddo
      enddo

      root=0

      !The solution to test against is..
      do i=1,count
        soln(i)    = root*100+i
        r8_soln(i) = real( soln(i) , ESMF_KIND_R8 ) 
        r4_soln(i) = real( r8_soln(i) )
        if ( mod(soln(i)+root,2) .eq. 0 ) then
          logical_soln(i)= ESMF_TRUE
        else
          logical_soln(i)= ESMF_FALSE
        endif
      enddo 

    !Test with 1D integer argument
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast data from the root processor
      write(name, *) "Broadcasting 1D integer data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBroadcast(vm, bcstData=localData, count=count, rootPet=root, &
        rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      all_verify = .true.
      write(name, *) "Verify local data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      do i=1, count
        if (localData(i) /= soln(i)) all_verify = .false.
      enddo
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

    !Test with 2D integer argument
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast data from the root processor
      write(name, *) "Broadcasting 2D integer data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBroadcast(vm, bcstData=localData2D(:,1), count=count, &
        rootPet=root, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      all_verify = .true.
      write(name, *) "Verify local data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      do j=1, count2*count3
        do i=1, count1
          if (localData2D(i,j) /= soln(i+(j-1)*count1)) all_verify = .false.
        enddo
      enddo
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

    !Test with 3D integer argument
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast data from the root processor
      write(name, *) "Broadcasting 3D integer data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBroadcast(vm, bcstData=localData3D(:,1,1), count=count, &
        rootPet=root, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      all_verify = .true.
      write(name, *) "Verify local data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      do k=1, count3
        do j=1, count2
          do i=1, count1
            if (localData3D(i,j,k) /= soln(i+(j-1)*count1+(k-1)*count1*count2))&
              all_verify = .false.
          enddo
        enddo
      enddo
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

    !Test with 1D REAL_KIND_R4 argument
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast local data from root processor
      write(name, *) "Broadcasting 1D real R4 data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBroadcast(vm, bcstData=r4_localData, count=count, rootPet=root, &
        rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      all_verify = .true.
      write(name, *) "Verify local data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      do i=1, count
        if (r4_localData(i) /= r4_soln(i)) all_verify = .false.
      enddo
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

    !Test with 1D ESMF_KIND_R8 argument
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast  data from root
      write(name, *) "Broadcasting 1D real R8 data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBroadcast(vm, bcstData=r8_localData, count=count, rootPet=root, &
        rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      all_verify = .true.
      write(name, *) "Verify local data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      do i=1, count
        if (r8_localData(i) /= r8_soln(i)) all_verify = .false.
      enddo
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

    !Test with 1D logical argument
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast local data from root
      write(name, *) "Broadcasting 1D logical data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBroadcast(vm, bcstData=local_logical, count=count, &
        rootPet=root, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      all_verify = .true.
      write(name, *) "Verify local data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      do i=1, count
        if (local_logical(i) /= logical_soln(i)) all_verify = .false.
      enddo
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)
      

    !Test with VMId arguments
    !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Create VMid arrays on both root and destinations
      write(name, *) "Creating VMId array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      allocate (local_vmids(n_elements))
      call ESMF_VMIdCreate (local_vmids, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Insert dummy data for the purposes of the test.  Only
      ! the first character of the key is set.
      write(name, *) "Inserting dummy data into VMId array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      rc = ESMF_SUCCESS
      do, i=1, n_elements
        if (localPet == root) then
          call c_ESMCI_VMIdSet (local_vmids(i), i, achar (i+10), localrc)
        else
          call c_ESMCI_VMIdSet (local_vmids(i), -1, achar (127), localrc)
        end if
        if (localrc /= ESMF_SUCCESS) rc = localrc
      end do
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Create VMid solutions array
      write(name, *) "Creating VMId solutions array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      allocate (vmids_soln(n_elements))
      call ESMF_VMIdCreate (vmids_soln, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Insert dummy data for the purposes of the test.  Only
      ! the first character of the key is set.
      write(name, *) "Inserting dummy data into VMId solution array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      rc = ESMF_SUCCESS
      do, i=1, n_elements
        call c_ESMCI_VMIdSet (vmids_soln(i), i, achar (i+10), localrc)
        if (localrc /= ESMF_SUCCESS) rc = localrc
      end do
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast data from the root processor
      write(name, *) "Broadcasting VMId data Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMBcastVMId(vm,  &
          bcstData=local_vmids, count=size (local_vmids),  &
          rootPet=root, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast.  Only
      ! the first character of the key is tested.
      write(name, *) "ID/Key extraction VMId data after broadcast Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      all_verify = .true.
      rc = ESMF_SUCCESS
      do, i=1, n_elements
        call c_ESMCI_VMIdGet (local_vmids(i), idData, keyData, localrc)
        if (localrc /= ESMF_SUCCESS) rc = localrc
        if (idData /= i .or. iachar (keyData) /= i+10) then
          print *, 'non-compare: index =', i,  &
                   ', idData =', idData, ', keyData =', iachar(keyData)
          all_verify = .false.
        end if
      end do
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast.  Only
      ! the first character of the key is tested.
      write(name, *) "Verify VMId data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast with VMIdCompare.
      write(name, *) "Verify VMId data after broadcast Test"
      write(failMsg, *) "Wrong Local Data"
      all_verify = .true.
      do, i=1, n_elements
        if (ESMF_VMIdCompare (local_vmids(i), vmids_soln(i))) cycle
        print *, 'VMIdCompare: non-compare: index =', i
        all_verify = .false.
      end do
      call ESMF_Test(all_verify, name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Release VMId resources
      write(name, *) "Releasing VMId array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMIdDestroy (local_vmids, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      deallocate (local_vmids)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Release VMId resources
      write(name, *) "Releasing VMId solution array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMIdDestroy (vmids_soln, rc=rc)
      call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      deallocate (vmids_soln)

      deallocate(localData)
      deallocate(localData2d)
      deallocate(localData3d)
      deallocate(r8_localData)
      deallocate(r4_localData)
      deallocate(local_logical)

      deallocate(soln)
      deallocate(r8_soln)
      deallocate(r4_soln)
      deallocate(logical_soln)

      !------------------------------------------------------------------------
      call ESMF_TestEnd(ESMF_SRCLINE)

      end program ESMF_VMBroadcastUTest
