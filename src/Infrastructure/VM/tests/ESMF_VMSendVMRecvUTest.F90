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
program ESMF_VMSendVMRecvUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMSendVMRecvUTest - Unit test for blocking VM Send and Receive
!  Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 blocking VM Send Receive tests.
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

      integer(ESMF_KIND_I4), allocatable  :: i4_data(:)
      integer(ESMF_KIND_I8), allocatable  :: i8_data(:)
      real(ESMF_KIND_R4),    allocatable  :: r4_data(:)
      real(ESMF_KIND_R8),    allocatable  :: r8_data(:)
      type(ESMF_Logical),    allocatable  :: lg_data(:)
      character(10)                       :: char_data, char_soln
      character(10),         allocatable  :: ch_data(:)

      integer(ESMF_KIND_I4) :: I4Sum
      integer(ESMF_KIND_I8) :: I8Sum
      real(ESMF_KIND_R4)    :: R4Sum
      real(ESMF_KIND_R8)    :: R8Sum
      integer               :: LGSum, CHSum

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

      src = localPet - 1
      if (src < 0) src = petCount - 1

      dst = localPet + 1
      if (dst > petCount -1) dst = 0

      write(msgStr, *) "src=",src," dst=",dst
      call ESMF_LogWrite(msgStr, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


      !Test with ESMF_KIND_I4 arguments
      !================================
      count = countParameter
      allocate(i4_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data I4 Test"
        do i=1, count
          i4_data(i) = int(localPet*100+i, ESMF_KIND_I4)
        enddo
        call ESMF_VMSend(vm, sendData=i4_data, count=count, dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data I4 Test"
        call ESMF_VMRecv(vm, recvData=i4_data, count=count, srcPet=src, rc=rc)
        I4Sum=0
        do i=1, count
          I4Sum = I4Sum + (i4_data(i) - int(src*100+i, ESMF_KIND_I4))
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data I4 Test"
        call ESMF_VMRecv(vm, recvData=i4_data, count=count, srcPet=src, rc=rc)
        I4Sum=0
        do i=1, count
          I4Sum = I4Sum + (i4_data(i) - int(src*100+i, ESMF_KIND_I4))
        enddo
      else
        write(name, *) "Sending local data I4 Test"
        do i=1, count
          i4_data(i) = int(localPet*100+i, ESMF_KIND_I4)
        enddo
        call ESMF_VMSend(vm, sendData=i4_data, count=count, dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive I4 Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (I4Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(i4_data)

      !Test with ESMF_KIND_I8 arguments
      !================================
      count = countParameter
      allocate(i8_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data I8 Test"
        do i=1, count
          i8_data(i) = int(localPet*100+i, ESMF_KIND_I8)
        enddo
        call ESMF_VMSend(vm, sendData=i8_data, count=count, dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data I8 Test"
        call ESMF_VMRecv(vm, recvData=i8_data, count=count, srcPet=src, rc=rc)
        I8Sum=0
        do i=1, count
          I8Sum = I8Sum + (i8_data(i) - int(src*100+i, ESMF_KIND_I8))
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data I8 Test"
        call ESMF_VMRecv(vm, recvData=i8_data, count=count, srcPet=src, rc=rc)
        I8Sum=0
        do i=1, count
          I8Sum = I8Sum + (i8_data(i) - int(src*100+i, ESMF_KIND_I8))
        enddo
      else
        write(name, *) "Sending local data I8 Test"
        do i=1, count
          i8_data(i) = int(localPet*100+i, ESMF_KIND_I8)
        enddo
        call ESMF_VMSend(vm, sendData=i8_data, count=count, dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive I8 Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (I8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(i8_data)

      !Test with ESMF_KIND_R4 arguments
      !================================
      count = countParameter
      allocate(r4_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data R4 Test"
        do i=1, count
          r4_data(i) = real(localPet*100+i, ESMF_KIND_R4)
        enddo
        call ESMF_VMSend(vm, sendData=r4_data, count=count, dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data R4 Test"
        call ESMF_VMRecv(vm, recvData=r4_data, count=count, srcPet=src, rc=rc)
        R4Sum=0
        do i=1, count
          R4Sum = R4Sum + (r4_data(i) - real(src*100+i, ESMF_KIND_R4))
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data R4 Test"
        call ESMF_VMRecv(vm, recvData=r4_data, count=count, srcPet=src, rc=rc)
        R4Sum=0
        do i=1, count
          R4Sum = R4Sum + (r4_data(i) - real(src*100+i, ESMF_KIND_R4))
        enddo
      else
        write(name, *) "Sending local data R4 Test"
        do i=1, count
          r4_data(i) = real(localPet*100+i, ESMF_KIND_R4)
        enddo
        call ESMF_VMSend(vm, sendData=r4_data, count=count, dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive R4 Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (R4Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(r4_data)

      !Test with ESMF_KIND_R8 arguments
      !================================
      count = countParameter
      allocate(r8_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data R8 Test"
        do i=1, count
          r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
        enddo
        call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data R8 Test"
        call ESMF_VMRecv(vm, recvData=r8_data, count=count, srcPet=src, rc=rc)
        R8Sum=0
        do i=1, count
          R8Sum = R8Sum + (r8_data(i) - real(src*100+i, ESMF_KIND_R8))
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data R8 Test"
        call ESMF_VMRecv(vm, recvData=r8_data, count=count, srcPet=src, rc=rc)
        R8Sum=0
        do i=1, count
          R8Sum = R8Sum + (r8_data(i) - real(src*100+i, ESMF_KIND_R8))
        enddo
      else
        write(name, *) "Sending local data R8 Test"
        do i=1, count
          r8_data(i) = real(localPet*100+i, ESMF_KIND_R8)
        enddo
        call ESMF_VMSend(vm, sendData=r8_data, count=count, dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive R8 Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (R8Sum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(r8_data)

      !Test with ESMF_Logical arguments
      !================================
      count = countParameter
      allocate(lg_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data Logical Test"
        do i=1, count
          lg_data(i) = (mod(localPet*100+i,2) == 0)
        enddo
        call ESMF_VMSend(vm, sendData=lg_data, count=count, dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data Logical Test"
        call ESMF_VMRecv(vm, recvData=lg_data, count=count, srcPet=src, rc=rc)
        LGSum=0
        do i=1, count
          if ((lg_data(i)==ESMF_TRUE) .neqv. (mod(src*100+i,2)==0)) &
            LGSum = LGSum + 1
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data Logical Test"
        call ESMF_VMRecv(vm, recvData=lg_data, count=count, srcPet=src, rc=rc)
        LGSum=0
        do i=1, count
          if ((lg_data(i)==ESMF_TRUE) .neqv. (mod(src*100+i,2)==0)) &
            LGSum = LGSum + 1
        enddo
      else
        write(name, *) "Sending local data Logical Test"
        do i=1, count
          lg_data(i) = (mod(localPet*100+i,2) == 0)
        enddo
        call ESMF_VMSend(vm, sendData=lg_data, count=count, dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Logical Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (LGSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(lg_data)

      !Test with character string arguments
      !====================================

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data Character String Test"
        write (char_data, "(i2.2,i3)") localPet, 1
        call ESMF_VMSend(vm, sendData=char_data, count=len(char_data), &
          dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data Character String Test"
        call ESMF_VMRecv(vm, recvData=char_data, count=len(char_data), &
          srcPet=src, rc=rc)
        CHSum=0
        write (char_soln, "(i2.2,i3)") src, 1
        if (char_data /= char_soln) CHSum = CHSum + 1
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data Character String Test"
        call ESMF_VMRecv(vm, recvData=char_data, count=len(char_data), &
          srcPet=src, rc=rc)
        CHSum=0
        write (char_soln, "(i2.2,i3)") src, 1
        if (char_data /= char_soln) CHSum = CHSum + 1
      else
        write(name, *) "Sending local data Character String Test"
        write (char_data, "(i2.2,i3)") localPet, 1
        call ESMF_VMSend(vm, sendData=char_data, count=len(char_data), &
          dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Character String Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (CHSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !Test with character string array arguments
      !==========================================
      count = min(1000,countParameter)  ! performance issue for too large
      allocate(ch_data(count))

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Sending local data Character String array Test"
        do i=1, count
          write (ch_data(i), "(i2.2,i3)") localPet, i
        enddo
        call ESMF_VMSend(vm, sendData=ch_data, count=count*len(ch_data), &
          dstPet=dst, rc=rc)
      else
        write(name, *) "Receiving local data Character String array Test"
        call ESMF_VMRecv(vm, recvData=ch_data, count=count*len(ch_data), &
          srcPet=src, rc=rc)
        CHSum=0
        do i=1, count
          write (char_soln, "(i2.2,i3)") src, i
          if (ch_data(i) /= char_soln) CHSum = CHSum + 1
        enddo
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
      if (localPet==0) then
        write(name, *) "Receiving local data Character String array Test"
        call ESMF_VMRecv(vm, recvData=ch_data, count=count*len(ch_data), &
          srcPet=src, rc=rc)
        CHSum=0
        do i=1, count
          write (char_soln, "(i2.2,i3)") src, i
          if (ch_data(i) /= char_soln) CHSum = CHSum + 1
        enddo
      else
        write(name, *) "Sending local data Character String array Test"
        do i=1, count
          write (ch_data(i), "(i2.2,i3)") localPet, i
        enddo
        call ESMF_VMSend(vm, sendData=ch_data, count=count*len(ch_data), &
          dstPet=dst, rc=rc)
      endif
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !NEX_UTest_Multi_Proc_Only
      ! Verify localData after VM Receive
      write(name, *) "Verify local data after receive Character String array Test"
      write(failMsg, *) "Wrong Local Data"
      call ESMF_Test( (CHSum == 0), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      deallocate(ch_data)

      call ESMF_TestEnd(ESMF_SRCLINE)

end program ESMF_VMSendVMRecvUTest
