! $Id: ESMF_ArrayCommNBEx.F90,v 1.4 2010/07/08 17:58:37 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayCommNBEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer :: rc, petCount, localPet, finalrc
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: srcDistGrid, dstDistGrid
  type(ESMF_Array):: srcArray, dstArray
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_RouteHandle):: routehandle
  logical:: finishflag

  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayCommNBEx.Log", &
                    defaultlogtype=ESMF_LOG_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

! preparation of data objects used in the example

  srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/4,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/1,4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
!
! \subsubsection{Non-blocking Communications}
! \label{Array:CommNB}
! 
! All {\tt ESMF\_RouteHandle} based communcation methods, like 
! {\tt ESMF\_ArrayRedist()}, {\tt ESMF\_ArrayHalo()} and {\tt ESMF\_ArraySMM()}, 
! can be executed in blocking or non-blocking mode. The non-blocking feature is
! useful, for example, to overlap computation with communication, or to
! implement a more loosely synchronized inter-Component interaction scheme than
! is possible with the blocking communication mode.
!
! Access to the non-blocking execution mode is provided uniformly across all
! RouteHandle based communication calls. Every such call contains the optional
! {\tt commflag} argument of type {\tt ESMF\_CommFlag}. Section
! \ref{opt:commflag} lists all of the valid settings for this flag.
!
! It is an execution time decision to select whether to invoke a precomputed
! communication pattern, stored in a RouteHandle, in the blocking or
! non-blocking mode. Neither requires specifically precomputed RouteHandles
! - i.e. a RouteHandle is neither specifically blocking nor specifically
! non-blocking.
!EOE

!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The returned RouteHandle {\tt routehandle} can be used in blocking or 
! non-blocking execution calls. The application is free to switch between
! both modes for the same RouteHandle.
!
! By default {\tt commflag} is set to {\tt ESMF\_COMM\_BLOCKING} in all of the
! RouteHandle execution methods, and the behavior is that of the VM-wide
! collective communication calls described in the previous sections. In the
! blocking mode the user must assume that the communication call will not
! return until all PETs have exchanged the precomputed information. On the
! other hand, the user has no guarante about the exact synchronization 
! behavior, and it is unsafe to make specific assumtions. What is guaranteed
! in the blocking communication mode is that when the call returns on the
! local PET, all data exchanges associated with all local DEs have finished.
! This means that all in-bound data elements are valid and that all out-bound
! data elements can safely be overwritten by the user.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, commflag=ESMF_COMM_BLOCKING, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The same exchange pattern, that is encoded in {\tt routehandle}, can be 
! executed in non-blocking mode, simply by setting the appropriate
! {\tt commflag} when calling into {\tt ESMF\_ArrayRedist()}.
!
! At first sight there are obvious similarities between the non-blocking
! RouteHandle based execution paradigm and the non-blocking message passing
! calls provided by MPI. However, there are significant differences in
! the behavior of the non-blocking point-to-point calls that MPI defines and
! the non-blocking mode of the collective exchange patterns described by ESMF
! RouteHandles.
!
! Setting {\tt commflag} to {\tt ESMF\_COMM\_NBSTART} in any RouteHandle
! execution call returns immediatly after all out-bound data has been moved
! into ESMF internal transfer buffers and the exchange has been initiated.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, commflag=ESMF_COMM_NBSTART, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Once a call with {\tt commflag = ESMF\_COMM\_NBSTART} returns, it is safe
! to modify the out-bound data elements in the {\tt srcArray} object. However,
! no guarantees are made for the in-bound data elements in {\tt dstArray} at
! this phase of the non-blocking execution. It is unsafe to access these
! elements until the exchange has finished locally.
!
! One way to ensure that the exchange has finished locally is to call 
! with {\tt commflag} set to {\tt ESMF\_COMM\_NBWAITFINISH}.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, commflag=ESMF_COMM_NBWAITFINISH, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Calling with {\tt commflag = ESMF\_COMM\_NBWAITFINISH} instructs the
! communication method to wait and block until the previously started
! exchange has finished, and has been processed locally according to 
! the RouteHandle. Once the call returns, it is safe to access both in-bound
! and out-bound data elements in {\tt dstArray} and {\tt srcArray}, 
! respectively.
!
! Some situations require more flexibility than is provided by the 
! {\tt ESMF\_COMM\_NBSTART} - {\tt ESMF\_COMM\_NBWAITFINISH} pair. For
! instance, a Component that needs to interact with several other Components,
! virtually simultanously, would initiated several different exchanges with 
! {\tt ESMF\_COMM\_NBSTART}. Calling with {\tt ESMF\_COMM\_NBWAITFINISH} for
! any of the outstanding exchanges may potentially block for a long time, 
! lowering the throughput. In the worst case a dead lock situation may arrise.
! Calling with {\tt commflag = ESMF\_COMM\_NBTESTFINISH} addresses this problem.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, commflag=ESMF_COMM_NBTESTFINISH, &
    finishedflag=finishflag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! This call tests the locally outstanding data transfer operation in 
! {\tt routehandle}, and finishes the exchange as much as currently possible.
! It does not block until the entire exchange has finished locally, instead
! it returns immediatly after one round of testing has been
! completed. The optional return argument {\tt finishedflag} is set to 
! {\tt .true.} if the exchange is completely finished locally, and set to 
! {\tt .false.} otherwise.
!
! The user code must decide, depending on the value of the returned
! {\tt finishedflag}, whether additional calls are required to finish an
! outstanding non-blocking exchange. If so, it can be done by 
! repeatedly calling with {\tt ESMF\_COMM\_NBTESTFINISH} until 
! {\tt finishedflag} comes back with a value of {\tt .true.}. Such a loop
! allows other pieces of user code to be executed between the calls. 
! Alternatively a call with {\tt ESMF\_COMM\_NBWAITFINISH} can be used to
! block until the exchange has locally finished.
!
! {\em Noteworthy property.}
! It is fine to invoke a RouteHandle based communication call
! with {\tt commflag} set to {\tt ESMF\_COMM\_NBTESTFINISH} or
! {\tt ESMF\_COMM\_NBWAITFINISH} on a specific RouteHandle without there 
! being an outstanding non-blocking exchange. In fact, it is not
! required that there was ever a call made with {\tt ESMF\_COMM\_NBSTART} for
! the RouteHandle. In these cases the calls made with
! {\tt ESMF\_COMM\_NBTESTFINISH} or {\tt ESMF\_COMM\_NBWAITFINISH}  will
! simply return immediatly (with {\tt finishedflag} set to {\tt .true.}).
!
! {\em Noteworthy property.}
! It is fine to mix blocking and non-blocking invokations of the same 
! RouteHandle based communication call across the PETs. This means that it is
! fine for some PETs to issue the call with {\tt ESMF\_COMM\_BLOCKING}
! (or using the default), while other PETs call the same communication call
! with {\tt ESMF\_COMM\_NBSTART}.
!
! {\em Noteworthy restriction.}
! A RouteHandle that is currently involved in an outstanding non-blocking
! exchange may {\em not} be used to start any further exchanges, neither
! blocking nor non-blocking. This restriction is independent of whether the
! newly started RouteHandle based exchange is made for the same or for 
! different data objects.
! 
!EOE 


! cleanup of data objects

  call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(srcDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayCommNBEx.F90"
  else
    print *, "FAIL: ESMF_ArrayCommNBEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
