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
program ESMF_VMSendNbVMRecvNbUTest

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

      integer, parameter:: countParameter = 2800000

      integer(ESMF_KIND_I4), allocatable  :: i4_data(:), i4_test(:)
      integer(ESMF_KIND_I8), allocatable  :: i8_data(:), i8_test(:)
      real(ESMF_KIND_R4),    allocatable  :: r4_data(:), r4_test(:)
      real(ESMF_KIND_R8),    allocatable  :: r8_data(:), r8_test(:)
      type(ESMF_Logical),    allocatable  :: lg_data(:), lg_test(:)
      character(10)                       :: char_data, char_test, char_soln
      character(10),         allocatable  :: ch_data(:), ch_test(:)

      integer(ESMF_KIND_I4) :: I4Sum
      integer(ESMF_KIND_I8) :: I8Sum
      real(ESMF_KIND_R4)    :: R4Sum
      real(ESMF_KIND_R8)    :: R8Sum
      integer               :: LGSum, CHSum

      type(ESMF_CommHandle):: commhandle

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

      src = localPet - 1
      if (src < 0) src = petCount - 1

      dst = localPet + 1
      if (dst > petCount -1) dst = 0

      write(msgStr, *) "src=",src," dst=",dst
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!===============================================================================
! First round of tests use explicit commhandles and wait on each recv call.
!===============================================================================

      !Test with ESMF_KIND_I4 arguments
      !================================
      count = countParameter
      allocate(i4_data(count), i4_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data I4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        i4_data(i) = int(localPet*100+i, ESMF_KIND_I4)
      enddo
      call ESMF_VMSend(vm, sendData=i4_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data I4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=i4_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for received local data I4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive I4 Test"
      write(failMsg, *) "Wrong Local Data"
      I4Sum=0
      do i=1, count
        I4Sum = I4Sum + (i4_test(i) - int(src*100+i, ESMF_KIND_I4))
      enddo
      call ESMF_Test( (I4Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(i4_data, i4_test)

      !Test with ESMF_KIND_I8 arguments
      !================================
      count = countParameter
      allocate(i8_data(count), i8_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data I8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        i8_data(i) = int(localPet*100+i, ESMF_KIND_I8)
      enddo
      call ESMF_VMSend(vm, sendData=i8_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data I8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=i8_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for received local data I8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive I8 Test"
      write(failMsg, *) "Wrong Local Data"
      I8Sum=0
      do i=1, count
        I8Sum = I8Sum + (i8_test(i) - int(src*100+i, ESMF_KIND_I8))
      enddo
      call ESMF_Test( (I8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(i8_data, i8_test)

      !Test with ESMF_KIND_R4 arguments
      !================================
      count = countParameter
      allocate(r4_data(count), r4_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data R4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        r4_data(i) = real(localPet*100+i, ESMF_KIND_R4)
      enddo
      call ESMF_VMSend(vm, sendData=r4_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data R4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=r4_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      write(name, *) "Waiting for received local data R4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive R4 Test"
      write(failMsg, *) "Wrong Local Data"
      R4Sum=0
      do i=1, count
        R4Sum = R4Sum + (r4_test(i) - real(src*100+i, ESMF_KIND_R4))
      enddo
      call ESMF_Test( (R4Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(r4_data, r4_test)

      !Test with ESMF_KIND_R8 arguments
      !================================
      count = countParameter
      allocate(r8_data(count), r8_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data R8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
      enddo
      call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data R8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=r8_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for received local data R8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive R8 Test"
      write(failMsg, *) "Wrong Local Data"
      R8Sum=0
      do i=1, count
        R8Sum = R8Sum + (r8_test(i) - real(src*100+i, ESMF_KIND_R8))
      enddo
      call ESMF_Test( (R8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(r8_data, r8_test)

      !Test with ESMF_Logical arguments
      !================================
      count = countParameter
      allocate(lg_data(count), lg_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data Logical Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        lg_data(i) = (mod(localPet*100+i,2) == 0)
      enddo
      call ESMF_VMSend(vm, sendData=lg_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data Logical Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=lg_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for received local data Logical Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Logical Test"
      write(failMsg, *) "Wrong Local Data"
      LGSum=0
      do i=1, count
        if ((lg_test(i)==ESMF_TRUE) .neqv. (mod(src*100+i,2)==0)) &
          LGSum = LGSum + 1
      enddo
      call ESMF_Test( (LGSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(lg_data, lg_test)

      !Test with character string arguments
      !====================================

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data Character String Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write (char_data, "(i2.2,i3)") localPet, 1
      call ESMF_VMSend(vm, sendData=char_data, count=len(char_data), &
        dstPet=dst, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data Character String Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=char_test, count=len(char_test), &
        srcPet=src, syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, &
        rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for received local data Character String Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Character String Test"
      write(failMsg, *) "Wrong Local Data"
      CHSum=0
      write (char_soln, "(i2.2,i3)") src, 1
      if (char_test /= char_soln) CHSum = CHSum + 1
      call ESMF_Test( (CHSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with character string array arguments
      !==========================================
      count = min(1000,countParameter)  ! performance issue for too large
      allocate(ch_data(count), ch_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data Character String array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        write (ch_data(i), "(i2.2,i3)") localPet, i
      enddo
      call ESMF_VMSend(vm, sendData=ch_data, count=count*len(ch_data), &
        dstPet=dst, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data Character String array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=ch_test, count=count*len(ch_test), &
        srcPet=src, syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle, &
        rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for received local data Character String array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWait(vm, commhandle, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Character String array Test"
      write(failMsg, *) "Wrong Local Data"
      CHSum=0
      do i=1, count
        write (char_soln, "(i2.2,i3)") src, i
        if (ch_test(i) /= char_soln) CHSum = CHSum + 1
      enddo
      call ESMF_Test( (CHSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call ESMF_VMCommWaitAll(vm, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      deallocate(ch_data, ch_test)

!===============================================================================
! Second round of tests use ESMF's internal request queue to wait on outstanding
! comms.
!===============================================================================

      !Test with ESMF_KIND_I4 arguments
      !================================
      count = countParameter
      allocate(i4_data(count), i4_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data I4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        i4_data(i) = int(localPet*100+i, ESMF_KIND_I4)
      enddo
      call ESMF_VMSend(vm, sendData=i4_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data I4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=i4_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for I4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive I4 Test"
      write(failMsg, *) "Wrong Local Data"
      I4Sum=0
      do i=1, count
        I4Sum = I4Sum + (i4_test(i) - int(src*100+i, ESMF_KIND_I4))
      enddo
      call ESMF_Test( (I4Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(i4_data, i4_test)

      !Test with ESMF_KIND_I8 arguments
      !================================
      count = countParameter
      allocate(i8_data(count), i8_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data I8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        i8_data(i) = int(localPet*100+i, ESMF_KIND_I8)
      enddo
      call ESMF_VMSend(vm, sendData=i8_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data I8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=i8_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for I8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive I8 Test"
      write(failMsg, *) "Wrong Local Data"
      I8Sum=0
      do i=1, count
        I8Sum = I8Sum + (i8_test(i) - int(src*100+i, ESMF_KIND_I8))
      enddo
      call ESMF_Test( (I8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(i8_data, i8_test)

      !Test with ESMF_KIND_R4 arguments
      !================================
      count = countParameter
      allocate(r4_data(count), r4_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data R4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        r4_data(i) = real(localPet*100+i, ESMF_KIND_R4)
      enddo
      call ESMF_VMSend(vm, sendData=r4_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data R4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=r4_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for  R4 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive R4 Test"
      write(failMsg, *) "Wrong Local Data"
      R4Sum=0
      do i=1, count
        R4Sum = R4Sum + (r4_test(i) - real(src*100+i, ESMF_KIND_R4))
      enddo
      call ESMF_Test( (R4Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(r4_data, r4_test)

      !Test with ESMF_KIND_R8 arguments
      !================================
      count = countParameter
      allocate(r8_data(count), r8_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data R8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
      enddo
      call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data R8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=r8_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for R8 Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive R8 Test"
      write(failMsg, *) "Wrong Local Data"
      R8Sum=0
      do i=1, count
        R8Sum = R8Sum + (r8_test(i) - real(src*100+i, ESMF_KIND_R8))
      enddo
      call ESMF_Test( (R8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(r8_data, r8_test)

      !Test with ESMF_Logical arguments
      !================================
      count = countParameter
      allocate(lg_data(count), lg_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data Logical Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        lg_data(i) = (mod(localPet*100+i,2) == 0)
      enddo
      call ESMF_VMSend(vm, sendData=lg_data, count=count, dstPet=dst, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data Logical Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=lg_test, count=count, srcPet=src, &
        syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for Logical Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Logical Test"
      write(failMsg, *) "Wrong Local Data"
      LGSum=0
      do i=1, count
        if ((lg_test(i)==ESMF_TRUE) .neqv. (mod(src*100+i,2)==0)) &
          LGSum = LGSum + 1
      enddo
      call ESMF_Test( (LGSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(lg_data, lg_test)

      !Test with character string arguments
      !====================================

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data Character String Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      write (char_data, "(i2.2,i3)") localPet, 1
      call ESMF_VMSend(vm, sendData=char_data, count=len(char_data), &
        dstPet=dst, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data Character String Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=char_test, count=len(char_test), &
        srcPet=src, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for Character String Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Character String Test"
      write(failMsg, *) "Wrong Local Data"
      CHSum=0
      write (char_soln, "(i2.2,i3)") src, 1
      if (char_test /= char_soln) CHSum = CHSum + 1
      call ESMF_Test( (CHSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with character string array arguments
      !==========================================
      count = min(1000,countParameter)  ! performance issue for too large
      allocate(ch_data(count), ch_test(count))

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Sending local data Character String array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      do i=1, count
        write (ch_data(i), "(i2.2,i3)") localPet, i
      enddo
      call ESMF_VMSend(vm, sendData=ch_data, count=count*len(ch_data), &
        dstPet=dst, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Receiving local data Character String array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMRecv(vm, recvData=ch_test, count=count*len(ch_test), &
        srcPet=src, syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Waiting for all outstanding comms for Character String array Test"
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      call ESMF_VMCommWaitAll(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Character String array Test"
      write(failMsg, *) "Wrong Local Data"
      CHSum=0
      do i=1, count
        write (char_soln, "(i2.2,i3)") src, i
        if (ch_test(i) /= char_soln) CHSum = CHSum + 1
      enddo
      call ESMF_Test( (CHSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(ch_data, ch_test)

      call ESMF_TestEnd(ESMF_SRCLINE)

end program ESMF_VMSendNbVMRecvNbUTest
