! $Id: ESMF_VMSendVMRecvUTest.F90,v 1.2 2004/11/17 23:30:32 svasquez Exp $
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
      '$Id: ESMF_VMSendVMRecvUTest.F90,v 1.2 2004/11/17 23:30:32 svasquez Exp $'
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


      print *, "*************VM SEND RECEIVE UNIT TESTS***************************"
      print *

      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)
      call ESMF_TestStart(petCount, ESMF_SRCLINE)

      ! exit early if we have less than 4 procs
      if (petCount .lt. 4) then
        print *, "This test cannot run with less than 4 processors"
        goto 10
      endif
 
      ! Allocate localData
      count = 1
      allocate(localData(count))
      localData(1) = localPet+100 
      

      src = 0
      dst = petCount - 1

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Send local data from src to dst
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Sending local data Test"
      if (localPet==src) &
      call ESMF_VMSend(vm, sendData=localData, count=count, dst=dst, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData before VM Receive
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data Test"
      call ESMF_Test((localData(1).eq.(localPet+100)), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! dst receives local data from src
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write(name, *) "Receiving local data Test"
      if (localPet==dst) then
      	call ESMF_VMRecv(vm, recvData=localData, count=count, src=src, rc=rc)
      else
      	rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(failMsg, *) "Wrong Local Data"
      write(name, *) "Verify local data Test"
      if (localPet==dst) then
      	call ESMF_Test((localData(1).eq.100), name, failMsg, result, ESMF_SRCLINE)
      else
      	call ESMF_Test((localData(1).eq.(localPet+100)), name, failMsg, result, ESMF_SRCLINE)
      endif

10    print *, "end of VM Send Receive test"

      call ESMF_TestEnd(result, ESMF_SRCLINE)
      call ESMF_Finalize(rc)

      end program ESMF_VMSendVMRecvUTest
