! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_VMEpochLargeMsgUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMSendNbVMRecvNbUTest - Unit test for non-blocking VM Send
!  and Receive Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 non-blocking VM Send Receive tests.
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
      character(ESMF_MAXSTR) :: msgStr

      ! local variables
      integer:: i, rc
      type(ESMF_VM):: vm
      integer:: localPet, petCount
      integer:: count, src, dst

      real(ESMF_KIND_R8),    allocatable  :: r8_data(:)
      real(ESMF_KIND_R8)                  :: R8Sum

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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! check petCount
      if (petCount < 2) then
        call ESMF_LogWrite("Must run test with at least 2 PETs", &
          ESMF_LOGMSG_ERROR, rc=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif

      ! single src PET, single dst PET
      src = 0
      dst = petCount-1

      write(msgStr, *) "src=",src," dst=",dst
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!===============================================================================
! Blocking src -> dst
!===============================================================================

      !Test with ESMF_KIND_R8 arguments
      !================================
!      count = 250000000   ! just below 2GiB limit
      count = 280000000   ! just above 2GiB limit
      if (localPet==src .or. localPet==dst) allocate(r8_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==src) then
        write(name, *) "Sending local data R8 Blocking Test"
        do i=1, count
          r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
        enddo
        call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, rc=rc)
      else
        write(name, *) "Dummy Sending R8 Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==dst) then
        write(name, *) "Receiving local data R8 Blocking Test"
        call ESMF_VMRecv(vm, recvData=r8_data, count=count, srcPet=src, rc=rc)
      else
        write(name, *) "Dummy Receiving R8 Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(failMsg, *) "Wrong Local Data"
      R8Sum=0
      if (localPet==dst) then
        write(name, *) "Verify local data after receive R8 Blocking Test"
        do i=1, count
          R8Sum = R8Sum + (r8_data(i) - real(src*100+i, ESMF_KIND_R8))
        enddo
      else
        write(name, *) "Dummy verify local data after receive R8 Blocking Test"
      endif
      call ESMF_Test( (R8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      if (localPet==src .or. localPet==dst) deallocate(r8_data)

!===============================================================================
! Non-Blocking src -> dst
!===============================================================================

      !Test with ESMF_KIND_R8 arguments
      !================================
!      count = 250000000   ! just below 2GiB limit
      count = 280000000   ! just above 2GiB limit
      if (localPet==src .or. localPet==dst) allocate(r8_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==src) then
        write(name, *) "Sending local data R8 Non-Blocking Test"
        do i=1, count
          r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
        enddo
        call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, &
          syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      else
        write(name, *) "Dummy Sending R8 Non-Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==dst) then
        write(name, *) "Receiving local data R8 Non-Blocking Test"
        call ESMF_VMRecv(vm, recvData=r8_data, count=count, srcPet=src, &
          syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      else
        write(name, *) "Dummy Receiving R8 Non-Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==dst) then
        write(name, *) "Waiting for all outstanding comms for R8 Non-Blocking Test"
        call ESMF_VMCommWaitAll(vm, rc=rc)
      else
        write(name, *) "Dummy Waiting for all outstanding comms for R8 Non-Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(failMsg, *) "Wrong Local Data"
      R8Sum=0
      if (localPet==dst) then
        write(name, *) "Verify local data after receive R8 Non-Blocking Test"
        do i=1, count
          R8Sum = R8Sum + (r8_data(i) - real(src*100+i, ESMF_KIND_R8))
        enddo
      else
        write(name, *) "Dummy verify local data after receive R8 Non-Blocking Test"
      endif
      call ESMF_Test( (R8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (localPet==src .or. localPet==dst) deallocate(r8_data)

!===============================================================================
! Non-Blocking with VMEpoch src -> dst
!===============================================================================

      !Test with ESMF_KIND_R8 arguments
      !================================
!      count = 250000000   ! just below 2GiB limit
      count = 280000000   ! just above 2GiB limit
      if (localPet==src .or. localPet==dst) allocate(r8_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(name, *) "ESMF_VMEpochEnter() Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==src) then
        write(name, *) "Sending local data R8 Non-Blocking Test"
        do i=1, count
          r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
        enddo
        call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, &
          syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      else
        write(name, *) "Dummy Sending R8 Non-Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==dst) then
        write(name, *) "Receiving local data R8 Non-Blocking Test"
        call ESMF_VMRecv(vm, recvData=r8_data, count=count, srcPet=src, &
          syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      else
        write(name, *) "Dummy Receiving R8 Non-Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==dst) then
        write(name, *) "Waiting for all outstanding comms for R8 Non-Blocking Test"
        call ESMF_VMCommWaitAll(vm, rc=rc)
      else
        write(name, *) "Dummy Waiting for all outstanding comms for R8 Non-Blocking Test"
        rc = ESMF_SUCCESS
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(name, *) "ESMF_VMEpochExit() Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMEpochExit(rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(failMsg, *) "Wrong Local Data"
      R8Sum=0
      if (localPet==dst) then
        write(name, *) "Verify local data after receive R8 Non-Blocking Test"
        do i=1, count
          R8Sum = R8Sum + (r8_data(i) - real(src*100+i, ESMF_KIND_R8))
        enddo
      else
        write(name, *) "Dummy verify local data after receive R8 Non-Blocking Test"
      endif
      call ESMF_Test( (R8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      if (localPet==src .or. localPet==dst) deallocate(r8_data)

      !------------------------------------------------------------------------
      call ESMF_TestEnd(ESMF_SRCLINE)

end program ESMF_VMEpochLargeMsgUTest
