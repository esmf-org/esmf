! $Id: ESMF_VMBroadcastUTest.F90,v 1.6.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_VMBroadcastUTest.F90,v 1.6.2.3 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      character(len=8) :: strvalue

      ! local variables
      integer:: i, rc
      type(ESMF_VM):: vm
      integer:: localPet, petCount
      integer:: count,root  
      integer, allocatable:: localData(:),soln(:)
      real(ESMF_KIND_R8), allocatable:: r8_localData(:),r8_soln(:)
      real(ESMF_KIND_R4), allocatable:: r4_localData(:),r4_soln(:)

      type(ESMF_logical), allocatable:: local_logical(:),logical_soln(:)
     
      integer :: isum
      real(ESMF_KIND_R8) :: R8Sum
      real(ESMF_KIND_R4) :: R4Sum

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! Get count of PETs and which PET number we are
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)

      ! Allocate localData
      count = 2
      allocate(localData(count))
      allocate(r8_localData(count))
      allocate(r4_localData(count))
      allocate(local_logical(count))

      ! Allocate the solution arrays
      Allocate(soln(count))
      Allocate(r8_soln(count))
      Allocate(r4_soln(count))
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
      end do 

       root=0

      !The solution to test against is..
      do  i=1,count
        soln(i)    = root*100+i
        r8_soln(i) = real( soln(i) , ESMF_KIND_R8 ) 
        r4_soln(i) = r8_soln(i)
        if ( mod(soln(i)+root,2) .eq. 0 ) then
          logical_soln(i)= ESMF_TRUE
        else
          logical_soln(i)= ESMF_FALSE
        endif
      end do 

     !Test with integer arguments
     !===========================     
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast data from the root processor
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Broadcasting  data Test"
      call ESMF_VMBroadcast(vm, bcstData=localData, count=count, root=root, &
                            rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, localPet," After Broadcast  LocalData is ", &
               localData(1),localData(2),"( Should be ",   &
               soln(1), soln(2)," )"
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      isum=0
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after broadcast Test"
      isum=isum+ (localData(1) - soln(1)) + (localData(2) - soln(2))
      CALL ESMF_Test((isum.eq.0), name, failMsg, result, ESMF_SRCLINE)

     !Test with REAL_KIND_R4 arguments
     !================================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast local data from root processor
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Broadcasting local data Test"
      call ESMF_VMBroadcast(vm, bcstData=r4_localData, count=count, root=root, &
                            rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, localPet,"After Broadcast: R4_LocalData is ", &
               r4_localData(1),r4_localData(2),"( Should be ",&
               r4_soln(1), r4_soln(2)," )"
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      R4Sum=0.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after Broadcast Test"
      print *,localPet, "After Broadcast r4_LocalData is ", &
              r4_localData(1),r4_localData(2)
      R4Sum=(r4_localData(1) - r4_soln(1)) +  &
            (r4_localData(2) - r4_soln(2))
      call ESMF_Test( (R4Sum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

     !Test with ESMF_KIND_R8 arguments
     !================================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast  data from root
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Broadcasting local data Test"
      call ESMF_VMBroadcast(vm, bcstData=r8_localData, count=count, root=root, &
                            rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, localPet,"After Broadcast: R8_LocalData is ", &
               r8_localData(1),r8_localData(2)," (Should be", &
               r8_soln(1), r8_soln(2)," )"
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Broadcast
      R8Sum=0.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after Broadcast Test"
      R8Sum=(r8_localData(1) - r8_soln(1)) +  &
            (r8_localData(2) - r8_soln(2))
      call ESMF_Test( (R8Sum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

     !Test with logical arguments
     !===========================
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Broadcast local data from root
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Broadcasting local data Test"
      call ESMF_VMBroadcast(vm, bcstData=local_logical, count=count, &
                            root=root, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      call ESMF_LogicalString(local_logical(1), strvalue, rc)
      print *, localPet, "after broadcast: Local_Logical(1) is ", trim(strvalue)
      call ESMF_LogicalString(local_logical(2), strvalue, rc)
      print *, localPet, "after broadcast: Local_Logical(2) is ", trim(strvalue)
      call ESMF_LogicalString(logical_soln(1), strvalue, rc)
      print *, localPet, "After broadcast: logical_soln(1) is ", trim(strvalue)
      call ESMF_LogicalString(logical_soln(2), strvalue, rc)
      print *, localPet, "After broadcast: logical_soln(2) is ", trim(strvalue)
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      ISum=0.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after broadcast Test"
      do i=1,count
        if (local_logical(i).ne. logical_soln(i)) ISum= ISum + 1
      end do
      call ESMF_Test( (ISum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMBroadcastUTest
