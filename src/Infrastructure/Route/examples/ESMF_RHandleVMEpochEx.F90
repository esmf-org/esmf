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

program ESMF_RHandleVMEpochEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod

  implicit none

  ! local variables
  integer               :: rc, i
  integer               :: petCount, localPet
  type(ESMF_VM)         :: vm
  type(ESMF_DELayout)   :: srcDL, dstDL
  type(ESMF_DistGrid)   :: srcDG, dstDG
  type(ESMF_Array)      :: srcArray, dstArray
  type(ESMF_RouteHandle):: rh

  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_RHandleVMEpochEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(vm=vm, defaultlogfilename="RHandleVMEpochEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 4) then
    finalrc = ESMF_FAILURE
    goto 10
  endif

!BOE
! \subsubsection{Asynchronous RouteHandle communication with VMEpoch}
! \label{RH:asyncVMEpoch}
!
! The RouteHandle based communication calls of the Array and Field classes
! provide the {\tt routesyncflag} argument. This argument allows the user to
! specify that the initiated call should not block. Additional calls are
! necessary to wait for a previously initiated communication call to finish.
! For a detailed discussion see section \ref{Array:CommNB}.
! Building on these primitives, asynchronous communications patterns can be
! implemented in user code. However, a more elegant option to achive
! asynchronous behavior between sending and receiving PETs of RouteHandle based
! communications is provided by the VMEpoch feature.
!
! The VMEpoch is a low level message aggregation and buffering approach.
! The VM level details are discussed under the ESMF\_VM section in \ref{VM:NBVMEpoch}.
!
! There are several advantages of VMEpoch over the direct use of non-blocking
! RouteHandle based communication calls:
!
! \begin{itemize}
!
! \item Multiple different RouteHandles can be aggregated within the same epoch.
!  This allows e.g. to mix Redist(), Regrid(), and SMM() operations,
!  aggregating all of the messages.
!
! \item The {\em same} RouteHandle can be used several times within the same
!  epoch. This is not possible under the direct non-blocking execution.
!
! \item ArrayBundle and FieldBundle communications are support. This is not
!  available for the direct non-blocking execution.
!
! \end{itemize}
!
! The {\tt ESMF\_VMEpoch} API consists of two interfaces:
! {\tt ESMF\_VMEpochEnter()} and {\tt ESMF\_VMEpochExit()}. When entering an
! epoch, the user specifies the type of epoch that is to be entered. Currently
! only {\tt ESMF\_VMEPOCH\_BUFFER} is available. Inside an epoch,
! communication calls are aggregated. Data transfers on the 
! {\tt src} side are not issued until the epoch is exited. On the {\tt dst} side
! a single data transfer is received, and then divided over the individual
! receive calls.
!
! In the following code the {\tt srcArray} has DEs on PET 0 and 1, while
! {\tt dstArray} has DEs on PET 2 and 3. Both Arrays are operating on the same
! global index space. A Redist() RouteHandle {\tt rh} is created in the usual
! manner.
!EOE
  srcDL = ESMF_DELayoutCreate(petList=[0,1], rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  srcDG = ESMF_DistGridCreate([1,1], [10,20], delayout=srcDL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  srcArray = ESMF_ArrayCreate(srcDG, typekind=ESMF_TYPEKIND_R4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dstDL = ESMF_DELayoutCreate(petList=[2,3], rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  dstDG = ESMF_DistGridCreate([1,1], [10,20], delayout=dstDL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  dstArray = ESMF_ArrayCreate(dstDG, typekind=ESMF_TYPEKIND_R4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
  call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle=rh, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The precomputed {\tt rh} can be used as usual. Here the use inside an active
! VMEpoch is demonstrated.
!
! First enter the VMEpoch using {\tt ESMF\_VMEpochEnter()}, specifying the kind
! of epoch. Currently only {\tt ESMF\_VMEPOCH\_BUFFER} is available.
!EOE
!BOC
  call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! {\bf SRC side (PET 0 \& 1):}
! The sending PETs do not block.
!
!
! {\bf DST side (PET 2 \& 3):}
! The receiving PETs do not block.
!
! Notice that ESMF implements a throttle on the VMEpoch as to limit the number
! of queued message between PETs. This is necessary to protect the receiving
! side in the {\tt EAGER} regime where MPI implementations send the
! data to the receiving side, assuming it will be able to buffer. In cases where
! the sending side runs ahead of the receiving side, this behavior can lead to
! increasing memory pressure on the receiving side, ultimatily resulting in
! out-of-memory conditions. The default throttle is set to $10$ outstanding
! message between any two PETs. It can be adjusted by specifying the
! {\tt throttle} argument to {\tt ESMF\_VMEpochEnter()}.
!
! Next the actual communication method, {\tt ESMF\_ArrayRedist()}, is called in
! the usual manner.
!EOE

!BOC
  call ESMF_ArrayRedist(srcArray, dstArray, routehandle=rh, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! {\bf SRC side (PET 0 \& 1):}
! The sending PETs block until the locally needed send buffers are available,
! and all local data manipulations and data movements into the send buffers have
! completed.
!
! Waiting for the send buffers comes into play when a VMEpoch is entered and
! exited multiple times. The same send buffer is reused each time for the 
! same src-dst-PET pairs (and grown automatically if needed). Each send buffer becomes
! available once the MPI layer has indicated that the associated
! {\tt MPI\_Isend()} has completed locally.
!
! Once the send buffer for a specific PET pair is available, the local data
! movements defined by the {\tt rh} must complete before returning. For 
! {\tt Regrid()} and {\tt SMM()} operations the {\tt srcTermProcessing} argument
! specified during {\tt Store()} determines the amount of local data processing.
! Once returned, it is safe to modify the {\tt srcArray} data on the local PET.
!
! {\bf DST side (PET 2 \& 3):}
! The receiving PETs block on the aggregated data from the src side for which
! the local PET has a dependency defined via the {\tt rh}. Once received, the
! data is processed locally, and moved into the final location.
! On return, it is safe to access the data in {\tt dstArray} on the local PET.
!
! Notice that several more RouteHandle based communication calls could be made inside
! the same active VMEpoch. In fact, aggregating messages from multiple exchanges
! is the typical use case of the VMEpoch approach. Additional communication calls
! could either involve different RouteHandles, or even the same {\tt rh} for
! different src/dst Array pairs.
!
! Finally the active VMEpoch is exited by calling {\tt ESMF\_VMEpochExit()}.
!EOE

!BOC
  call ESMF_VMEpochExit(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! {\bf SRC side (PET 0 \& 1):}
! The sending PETs post their local {\tt MPI\_Isend()} calls. This is
! non-blocking.
!
! {\bf DST side (PET 2 \& 3):}
! The receiving PETs do not block.
!
! As part of the final clean-up the {\tt rh} is being released as usual.
!EOE
!BOC
  call ESMF_ArrayRedistRelease(routehandle=rh, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArrayDestroy(srcArray, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridDestroy(srcDG, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DistGridDestroy(dstDG, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DELayoutDestroy(srcDL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(dstDL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_RHandleVMEpochEx.F90"
  else
    print *, "FAIL: ESMF_RHandleVMEpochEx.F90"
  endif

end program
