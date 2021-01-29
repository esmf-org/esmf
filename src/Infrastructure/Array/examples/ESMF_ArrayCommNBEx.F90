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
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayCommNBEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
 
  implicit none
  
  ! local variables
  integer :: rc, petCount, localPet, finalrc, result
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: srcDistGrid, dstDistGrid
  type(ESMF_Array):: srcArray, dstArray
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_RouteHandle):: routehandle
  logical:: finishflag
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg


  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayCommNBEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayCommNBEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

! preparation of data objects used in the example

  srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/4,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/1,4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!
! \subsubsection{Non-blocking Communications}
! \label{Array:CommNB}
! 
! All {\tt ESMF\_RouteHandle} based communication methods, like 
! {\tt ESMF\_ArrayRedist()}, {\tt ESMF\_ArrayHalo()} and {\tt ESMF\_ArraySMM()}, 
! can be executed in blocking or non-blocking mode. The non-blocking feature is
! useful, for example, to overlap computation with communication, or to
! implement a more loosely synchronized inter-Component interaction scheme than
! is possible with the blocking communication mode.
!
! Access to the non-blocking execution mode is provided uniformly across all
! RouteHandle based communication calls. Every such call contains the optional
! {\tt routesyncflag} argument of type {\tt ESMF\_RouteSync\_Flag}. Section
! \ref{const:routesync} lists all of the valid settings for this flag.
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The returned RouteHandle {\tt routehandle} can be used in blocking or 
! non-blocking execution calls. The application is free to switch between
! both modes for the same RouteHandle.
!
! By default {\tt routesyncflag} is set to {\tt ESMF\_ROUTESYNC\_BLOCKING} in all of the
! RouteHandle execution methods, and the behavior is that of the VM-wide
! collective communication calls described in the previous sections. In the
! blocking mode the user must assume that the communication call will not
! return until all PETs have exchanged the precomputed information. On the
! other hand, the user has no guarantee about the exact synchronization 
! behavior, and it is unsafe to make specific assumptions. What is guaranteed
! in the blocking communication mode is that when the call returns on the
! local PET, all data exchanges associated with all local DEs have finished.
! This means that all in-bound data elements are valid and that all out-bound
! data elements can safely be overwritten by the user.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_BLOCKING, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The same exchange pattern, that is encoded in {\tt routehandle}, can be 
! executed in non-blocking mode, simply by setting the appropriate
! {\tt routesyncflag} when calling into {\tt ESMF\_ArrayRedist()}.
!
! At first sight there are obvious similarities between the non-blocking
! RouteHandle based execution paradigm and the non-blocking message passing
! calls provided by MPI. However, there are significant differences in
! the behavior of the non-blocking point-to-point calls that MPI defines and
! the non-blocking mode of the collective exchange patterns described by ESMF
! RouteHandles.
!
! Setting {\tt routesyncflag} to {\tt ESMF\_ROUTESYNC\_NBSTART} in any RouteHandle
! execution call returns immediately after all out-bound data has been moved
! into ESMF internal transfer buffers and the exchange has been initiated.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBSTART, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Once a call with {\tt routesyncflag = ESMF\_ROUTESYNC\_NBSTART} returns, it is safe
! to modify the out-bound data elements in the {\tt srcArray} object. However,
! no guarantees are made for the in-bound data elements in {\tt dstArray} at
! this phase of the non-blocking execution. It is unsafe to access these
! elements until the exchange has finished locally.
!
! \begin{sloppypar}
! One way to ensure that the exchange has finished locally is to call 
! with {\tt routesyncflag} set to {\tt ESMF\_ROUTESYNC\_NBWAITFINISH}.
! \end{sloppypar}
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBWAITFINISH, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Calling with {\tt routesyncflag = ESMF\_ROUTESYNC\_NBWAITFINISH} instructs the
! communication method to wait and block until the previously started
! exchange has finished, and has been processed locally according to 
! the RouteHandle. Once the call returns, it is safe to access both in-bound
! and out-bound data elements in {\tt dstArray} and {\tt srcArray}, 
! respectively.
!
! \begin{sloppypar}
! Some situations require more flexibility than is provided by the 
! {\tt ESMF\_ROUTESYNC\_NBSTART} - {\tt ESMF\_ROUTESYNC\_NBWAITFINISH} pair. For
! instance, a Component that needs to interact with several other Components,
! virtually simultaneously, would initiated several different exchanges with 
! {\tt ESMF\_ROUTESYNC\_NBSTART}. Calling with {\tt ESMF\_ROUTESYNC\_NBWAITFINISH} for
! any of the outstanding exchanges may potentially block for a long time, 
! lowering the throughput. In the worst case a dead lock situation may arise.
! Calling with {\tt routesyncflag = ESMF\_ROUTESYNC\_NBTESTFINISH} addresses this problem.
! \end{sloppypar}
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBTESTFINISH, &
    finishedflag=finishflag, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! This call tests the locally outstanding data transfer operation in 
! {\tt routehandle}, and finishes the exchange as much as currently possible.
! It does not block until the entire exchange has finished locally, instead
! it returns immediately after one round of testing has been
! completed. The optional return argument {\tt finishedflag} is set to 
! {\tt .true.} if the exchange is completely finished locally, and set to 
! {\tt .false.} otherwise.
!
! The user code must decide, depending on the value of the returned
! {\tt finishedflag}, whether additional calls are required to finish an
! outstanding non-blocking exchange. If so, it can be done by 
! calling {\tt ESMF\_ArrayRedist()} repeatedly with 
! {\tt ESMF\_ROUTESYNC\_NBTESTFINISH} until 
! {\tt finishedflag} comes back with a value of {\tt .true.}. Such a loop
! allows other pieces of user code to be executed between the calls. 
! A call with {\tt ESMF\_ROUTESYNC\_NBWAITFINISH} can alternatively be used to
! block until the exchange has locally finished.
!
! {\em Noteworthy property.}
! It is allowable to invoke a RouteHandle based communication call
! with {\tt routesyncflag} set to 
! {\tt ESMF\_ROUTESYNC\_NBTESTFINISH} or
! {\tt ESMF\_ROUTESYNC\_NBWAITFINISH} on a specific RouteHandle without there 
! being an outstanding non-blocking exchange. As a matter of fact, it is not
! required that there was ever a call made with {\tt ESMF\_ROUTESYNC\_NBSTART} for
! the RouteHandle. In these cases the calls made with
! {\tt ESMF\_ROUTESYNC\_NBTESTFINISH} or {\tt ESMF\_ROUTESYNC\_NBWAITFINISH}  will
! simply return immediately (with {\tt finishedflag} set to {\tt .true.}).
!
! {\em Noteworthy property.}
! It is fine to mix blocking and non-blocking invocations of the same 
! RouteHandle based communication call across the PETs. This means that it is
! fine for some PETs to issue the call with {\tt ESMF\_ROUTESYNC\_BLOCKING}
! (or using the default), while other PETs call the same communication call
! with {\tt ESMF\_ROUTESYNC\_NBSTART}.
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(srcDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDistgrid, rc=rc) ! destroy the DistGrid object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue

 ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
 ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

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
