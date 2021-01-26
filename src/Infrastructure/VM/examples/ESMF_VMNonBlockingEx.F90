! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!------------------------------------------------------------------------------
!BOE
!
! \subsubsection{Communication - Non-blocking option and VMEpochs}
!
! The VM communication methods offer the option to execute in non-blocking
! mode. In this mode, both sending and receving calls return immediatly on each
! local PET. A separate synchronization call is needed to assure completion of
! the data transfer.
!
! The separation of initiation and completion of the data transfer provides
! the opportunity for the underlying communication system to progress 
! concurrently with other operations on the same PET. This can be leveraged to
! have profound impact on the performance of an algorithm that requires both
! computation and communication.
!
! Another critical application of the non-blocking communication mode is the
! prevention of deadlocks. In the default blocking mode, a receiving method
! will not return until the data transfer has completed. Sending methods may
! also not return, especially if the message being sent is above the 
! implementation dependent internal buffer size. This behavior makes it often
! hard, if not impossible, to write safe algorithms that guarantee to not
! deadlock when communicating between a group of PETs.
! Using the communication calls in non-blocking mode simplifies this problem
! immensely.
! 
! The following code shows how {\tt ESMF\_VMSend()} and {\tt ESMF\_VMRecv()}
! are used in non-blocking mode by passing in the {\tt ESMF\_SYNC\_NONBLOCKING}
! argument.
!
!EOE
!------------------------------------------------------------------------------

program ESMF_VMNonBlockingEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: i, rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount
  integer:: count, src, dst
  integer, allocatable:: localData(:)
  integer, allocatable:: localData2(:)
  type(ESMF_CommHandle) :: commhandle(2)
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_VMNonBlockingEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
  
  call ESMF_Initialize(vm=vm, defaultlogfilename="VMNonBlockingEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Set up the {\tt localData} array.
!EOE
  count = 10
  allocate(localData(count))
!BOC
  do i=1, count
    localData(i) = localPet*100 + i
  enddo
!EOC
 
  src = petCount - 1
  dst = 0

!BOE
! Initiate the data transfer between {\tt src} PET and {\tt dst} PET.
!EOE

!BOC
  if (localPet==src) then
    call ESMF_VMSend(vm, sendData=localData, count=count, dstPet=dst, &
      syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  if (localPet==dst) then
    call ESMF_VMRecv(vm, recvData=localData, count=count, srcPet=src, &
      syncflag=ESMF_SYNC_NONBLOCKING, rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! There is no garantee at this point that the data transfer has actually 
! started, let along completed. For this reason it is unsafe to overwrite
! the data in the {\tt localData} array on {\tt src} PET, or to access
! the {\tt localData} array on {\tt dst} PET. However both PETs are free
! to engage in other work while the data transfer my proceed concurrently.
!EOE

!BOC
  ! local computational work here, or other communications
!EOC

!BOE
! Wait for the completion of all outstanding non-blocking communication calls
! by issuing the {\tt ESMF\_VMCommWaitAll()} call.
!EOE

!BOC
  call ESMF_VMCommWaitAll(vm, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do i=1, count
    print *, 'localData for PET ',localPet,': ', localData(i)
  enddo 

!BOE
! Finally, on {\tt dst} PET, test the received data for correctness.
!EOE

!BOC
  if (localPet==dst) then
    do i=1, count
      if (localData(i) /= src*100 + i) then
        finalrc = ESMF_RC_VAL_WRONG
      endif
    enddo 
  endif
!EOC

!BOE
! Sometimes it is necessary to wait for individual outstanding communications
! specifically. This can be accomplished by using {\tt ESMF\_CommHandle}
! objects. To demonstrate this, first re-initialize the {\tt localData} array.
!EOE

  allocate(localData2(count))
!BOC
  do i=1, count
    localData(i) = localPet*100 + i
    localData2(i) = localPet*1000 + i
  enddo
!EOC

!BOE
! Initiate the data transfer between {\tt src} PET and {\tt dst} PET, but this
! time also pass the {\tt commhandle} variable of type {\tt ESMF\_CommHandle}.
! Here send two message between {\tt src} and {\tt dst} in order to have
! different outstanding messages to wait for.
!EOE

!BOC
  if (localPet==src) then
    call ESMF_VMSend(vm, sendData=localData, count=count, dstPet=dst, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(1), rc=rc)
    call ESMF_VMSend(vm, sendData=localData2, count=count, dstPet=dst, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(2), rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  if (localPet==dst) then
    call ESMF_VMRecv(vm, recvData=localData, count=count, srcPet=src, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(1), rc=rc)
    call ESMF_VMRecv(vm, recvData=localData2, count=count, srcPet=src, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(2), rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now it is possible to specifically wait for the first data transfer, e.g. on
! the {\tt dst} PET.
!EOE

!BOC
  if (localPet==dst) then
    call ESMF_VMCommWait(vm, commhandle=commhandle(1), rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do i=1, count
    print *, 'localData for PET ',localPet,': ', localData(i)
  enddo 

!BOE
! At this point there are still 2 outstanding communications on the {\tt src}
! PET, and one outstanding communication on the {\tt dst} PET. However, having
! returned from the specific {\tt ESMF\_VMCommWait()} call guarantees that the
! first communication on the {\tt dst} PET has completed, i.e. the data has 
! been received from the {\tt src} PET, and can now be accessed in the
! {\tt localData} array.
!EOE

!BOC
  if (localPet==dst) then
    do i=1, count
      if (localData(i) /= src*100 + i) then
        finalrc = ESMF_RC_VAL_WRONG
      endif
    enddo
  endif
!EOC

!BOE
! Before accessing data from the second transfer, it is necessary to wait on
! the associated commhandle for completion.
!EOE

!BOC
  if (localPet==dst) then
    call ESMF_VMCommWait(vm, commhandle=commhandle(2), rc=rc)
  endif
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do i=1, count
    print *, 'localData2 for PET ',localPet,': ', localData2(i)
  enddo 
!BOC
  if (localPet==dst) then
    do i=1, count
      if (localData2(i) /= src*1000 + i) then
        finalrc = ESMF_RC_VAL_WRONG
      endif
    enddo
  endif
!EOC

!BOE
! Finally the {\tt commhandle} elements on the {\tt src} side need to be
! cleared by waiting for them. This could be done using specific 
! {\tt ESMF\_VMCommWait()} calls, similar to the {\tt dst} side, or simply 
! by waiting for all/any outstanding communications using
! {\tt ESMF\_VMCommWaitAll()} as in the previous example. This call can be
! issued without {\tt commhandle} on all of the PETs.
!EOE
!BOC
  call ESMF_VMCommWaitAll(vm, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

!BOE
! For cases where multiple messages are being sent between the same
! {\tt src}-{\tt dst}
! pairs using non-blocking communications, performance can often
! be improved by aggregating individual messages. An extra buffer
! is needed to hold the collected messages, resulting in only a single data
! transfer for each PET pair. In many cases this can significantly reduce the
! time spent in communications. The ESMF VM class provides access to such a
! buffer technique through the {\tt ESMF\_VMEpoch} API.
!
! The {\tt ESMF\_VMEpoch} API consists of two interfaces:
! {\tt ESMF\_VMEpochEnter()} and {\tt ESMF\_VMEpochExit()}. When entering an
! epoch, the user specifies the type of epoch that is to be entered. Currently
! only {\tt ESMF\_VMEPOCH\_BUFFER} is available. Inside this epoch,
! non-blocking communication calls are aggregated and data transfers on the 
! {\tt src} side are not issued until the epoch is exited. On the {\tt dst side}
! a single data transfer is received, and then divided over the actual
! non-blocking receive calls.
!
! The following code repeates the previous example with two messages between
! {\tt src} and {\tt dst}. It is important that every PET only must act either
! as sender or receiver. A sending PET can send to many different PETs, and a 
! receiving PET can receive from many PETs, but no PET must send {\em and}
! receive within the same epoch!
!
! First re-initialize the {\tt localData} array.
!EOE

!BOC
  do i=1, count
    localData(i) = localPet*100 + i
    localData2(i) = localPet*1000 + i
  enddo
!EOC

!BOE
! Enter the {\tt ESMF\_VMEPOCH\_BUFFER}.
!EOE
!BOC
  call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Now issue non-blocking send and receive calls as usual.
!EOE

!BOC
  if (localPet==src) then
    call ESMF_VMSend(vm, sendData=localData, count=count, dstPet=dst, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(1), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    call ESMF_VMSend(vm, sendData=localData2, count=count, dstPet=dst, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(2), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  endif
  if (localPet==dst) then
    call ESMF_VMRecv(vm, recvData=localData, count=count, srcPet=src, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(1), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    call ESMF_VMRecv(vm, recvData=localData2, count=count, srcPet=src, &
      syncflag=ESMF_SYNC_NONBLOCKING, commhandle=commhandle(2), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  endif
!EOC

!BOE
! No data transfer has been initiated at this point due to the fact that this
! code is inside the {\tt ESMF\_VMEPOCH\_BUFFER}. On the {\tt dst} side the
! same methods are used to wait for the data transfer. However, it is not until
! the exit of the epoch on the {\tt src} side that data is transferred to the
! {\tt dst} side.
!EOE

!BOC
  if (localPet==dst) then
    call ESMF_VMCommWait(vm, commhandle=commhandle(1), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  endif
!EOC

  do i=1, count
    print *, 'localData for PET ',localPet,': ', localData(i)
  enddo
!BOC
  if (localPet==dst) then
    do i=1, count
      if (localData(i) /= src*100 + i) then
        finalrc = ESMF_RC_VAL_WRONG
      endif
    enddo 
  endif
!EOC

!BOC
  if (localPet==dst) then
    call ESMF_VMCommWait(vm, commhandle=commhandle(2), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  endif
!EOC

  do i=1, count
    print *, 'localData2 for PET ',localPet,': ', localData2(i)
  enddo 
!BOC
  if (localPet==dst) then
    do i=1, count
      if (localData2(i) /= src*1000 + i) then
        finalrc = ESMF_RC_VAL_WRONG
      endif
    enddo
  endif
!EOC

!BOE
! Now exit the epoch, to trigger the data transfer on the {\tt src} side.
!EOE
!BOC
  call ESMF_VMEpochExit(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Finally clear the outstanding communication handles on the {\tt src} side.
! This needs to happen first inside the {\em next} {\tt ESMF\_VMEPOCH\_BUFFER}.
! As before, waits could be issued either for the specific {\tt commhandle}
! elements not yet explicitly cleared, or a general call to
! {\tt ESMF\_VMCommWaitAll()} can be used for simplicity.
!EOE
!BOC
  call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_VMCommWaitAll(vm, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_VMEpochExit(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_VMNonBlockingEx.F90"
  else
    print *, "FAIL: ESMF_VMNonBlockingEx.F90"
  endif
  
end program
