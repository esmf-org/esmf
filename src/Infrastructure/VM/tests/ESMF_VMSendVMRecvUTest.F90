! $Id: ESMF_VMSendVMRecvUTest.F90,v 1.19.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
      program ESMF_VMSendVMRecvUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMSendVMRecvUTest - Unit test for VM Send Receive Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM Send Receive tests.  The VM
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
      '$Id: ESMF_VMSendVMRecvUTest.F90,v 1.19.2.4 2009/01/21 21:25:24 cdeluca Exp $'
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
      integer:: count, src, dst
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

!------------------------------------------------------------------------------
! IMPORTANT NOTE:
!   The correct operation of this unit test depends on sufficient internal 
!   buffering within VMSend and VMRecv! There will be an implementation 
!   specific threshold for count above which this unit test will start to hang!
!------------------------------------------------------------------------------

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

      src = localPet - 1
      if (src < 0) src = petCount - 1
      
      dst = localPet + 1
      if (dst > petCount -1) dst = 0

      !The solution to test against is..
      do  i=1,count
        soln(i)    = src*100+i
        r8_soln(i) = real( soln(i) , ESMF_KIND_R8 )
        r4_soln(i) = r8_soln(i)
        if ( mod(soln(i)+src,2) .eq. 0 ) then
          logical_soln(i)= ESMF_TRUE
        else
          logical_soln(i)= ESMF_FALSE
        endif
      end do 

     !Test with integer arguments
     !===========================     
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Send local data to dst
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Sending local data Test"
      call ESMF_VMSend(vm, sendData=localData, count=count, dst=dst, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, localPet," Before recv LocalData is ", localData(1),localData(2)
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! dst receives local data from src
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Receiving local data Test"
      call ESMF_VMRecv(vm, recvData=localData, count=count, src=src, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      isum=0
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after receive Test"
      print *,localPet, " After rcv LocalData is ", localData(1),localData(2)
      isum=isum+ (localData(1) - soln(1)) + (localData(2) - soln(2))
      call ESMF_Test( (isum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

     !Test with REAL_KIND_R4 arguments
     !================================
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Send local data to dst
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Sending local data Test"
      call ESMF_VMSend(vm, sendData=r4_localData, count=count, dst=dst, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, localPet,"Before recv: R4_LocalData is ", r4_localData(1),r4_localData(2)
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! dst receives local data from src
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Receiving local data Test"
      call ESMF_VMRecv(vm, recvData=r4_localData, count=count, src=src, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      R4Sum=0.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after receive Test"
      print *,localPet, "After recv LocalData is ", localData(1),localData(2)
      R4Sum=(r4_localData(1) - r4_soln(1)) +  &
            (r4_localData(2) - r4_soln(2))
      call ESMF_Test( (R4Sum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

     !Test with ESMF_KIND_R8 arguments
     !================================
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Send local data to dst
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Sending local data Test"
      call ESMF_VMSend(vm, sendData=r8_localData, count=count, dst=dst, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      print *, localPet,"Before recv: R8_LocalData is ", r8_localData(1),r8_localData(2)
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! dst receives local data from src
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Receiving local data Test"
      call ESMF_VMRecv(vm, recvData=r8_localData, count=count, src=src, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      R8Sum=0.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after receive Test"
      print *,localPet, "After recv LocalData is ", localData(1),localData(2)
      R8Sum=(r8_localData(1) - r8_soln(1)) +  &
            (r8_localData(2) - r8_soln(2))
      call ESMF_Test( (R8Sum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

     !Test with logical arguments
     !===========================
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Send local data to dst
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Sending local data Test"
      call ESMF_VMSend(vm, sendData=local_logical, count=count, dst=dst, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      call ESMF_LogicalString(local_logical(1), strvalue, rc)
      print *, localPet, "before recv: Local_Logical(1) is ", trim(strvalue)
      call ESMF_LogicalString(local_logical(2), strvalue, rc)
      print *, localPet, "before recv: Local_Logical(2) is ", trim(strvalue)
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! dst receives local data from src
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Receiving local data Test"
      call ESMF_VMRecv(vm, recvData=local_logical, count=count, src=src, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      ISum=0.
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after receive Test"
      call ESMF_LogicalString(local_logical(1), strvalue, rc)
      print *, localPet, "After recv: Local_Logical(1) is ", trim(strvalue)
      call ESMF_LogicalString(local_logical(2), strvalue, rc)
      print *, localPet, "After recv: Local_Logical(2) is ", trim(strvalue)
      call ESMF_LogicalString(logical_soln(1), strvalue, rc)
      print *, localPet, "After recv: Logical_soln(1) is ", trim(strvalue)
      call ESMF_LogicalString(logical_soln(2), strvalue, rc)
      print *, localPet, "After recv: logical_soln(2) is ", trim(strvalue)
      do i=1,count
        if (local_logical(i).ne. logical_soln(i)) ISum= ISum + 1
      end do
      call ESMF_Test( (ISum .eq. 0), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMSendVMRecvUTest
