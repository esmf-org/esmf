! $Id: ESMF_VMSendVMRecvUTest.F90,v 1.9 2004/12/09 16:45:16 nscollins Exp $
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
      program ESMF_VMSendVMRecvUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

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
      '$Id: ESMF_VMSendVMRecvUTest.F90,v 1.9 2004/12/09 16:45:16 nscollins Exp $'
!------------------------------------------------------------------------------
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      integer:: i, rc
      type(ESMF_VM):: vm
      integer:: localPet, petCount
      integer:: count, src, dst
      integer, allocatable:: localData(:)
     
      integer :: status, myde, npets

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
 
      ! Get count of PETs and which PET number we are
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)

      ! Allocate localData
      count = 1
      allocate(localData(count))
      localData(1) = localPet+100 
      

      src = localPet - 1
      if (src < 0) src = petCount - 1
      
      dst = localPet + 1
      if (dst > petCount -1) dst = 0
      
      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Send local data to dst
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Sending local data Test"
      call ESMF_VMSend(vm, sendData=localData, count=count, dst=dst, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData before VM Receive
      write(failMsg, *) "Wrong local data"
      write(name, *) "Verify local data before receive Test"
      call ESMF_Test((localData(1).eq.(localPet+100)), name, failMsg, result, ESMF_SRCLINE)

      print *, "LocalData is ", localData(1)
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
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data after receive Test"
      print *, "LocalData is ", localData(1)
      call ESMF_Test((localData(1).eq.src + 100), name, failMsg, result, ESMF_SRCLINE)

10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMSendVMRecvUTest
