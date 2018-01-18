! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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

program ESMF_DELayoutEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, i, localPET, petCount, localDeCount, myDe, workDe, localDe
  integer, allocatable:: commWeights(:,:), compWeights(:), localDeToDeMap(:)
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  logical:: oneToOneFlag
  type(ESMF_ServiceReply_Flag):: reply
  ! result code
  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_DELayoutEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS

  
  call ESMF_Initialize(vm=vm, defaultlogfilename="DELayoutEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPET=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Default DELayout}
! 
! Without specifying any of the optional parameters the created 
! {\tt ESMF\_DELayout}
! defaults into having as many DEs as there are PETs in the associated VM 
! object. Consequently the resulting DELayout describes a simple 1-to-1 DE to
! PET mapping.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!BOE
! The default DE to PET mapping is simply:
! \begin{verbatim}
! DE 0  -> PET 0
! DE 1  -> PET 1
! ...
! \end{verbatim}
!
! DELayout objects that are not used any longer should be destroyed.
!BOC
  call ESMF_DELayoutDestroy(delayout, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The optional {\tt vm} argument can be provided to DELayoutCreate() to lower 
! the method's overhead by the amount it takes to determine the current VM.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(vm=vm, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! By default all PETs of the associated VM will be considered. However, if the 
! optional argument {\tt petList} is present DEs will only be mapped against
! the PETs contained in the list. When the following example is executed on
! four PETs it creates a DELayout with four DEs by default that are mapped 
! to the provided PETs in their given order. It is erroneous to specify PETs 
! that are not part of the VM context on which the DELayout is defined. 
!EOE
  if (petCount > 1) then
!BOC
  delayout = ESMF_DELayoutCreate(petList=(/(i,i=petCount-1,1,-1)/), rc=rc)
!EOC
  else
    delayout = ESMF_DELayoutCreate(rc=rc)
  endif
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE  
! Once the end of the petList has been reached the DE to PET mapping 
! continues from the beginning of the list. For a 4 PET VM the above created
! DELayout will end up with the following DE to PET mapping:
!
! \begin{verbatim}
! DE 0  -> PET 3
! DE 1  -> PET 2
! DE 2  -> PET 1
! DE 2  -> PET 3
! \end{verbatim}
!EOE

!BOE
! \subsubsection{DELayout with specified number of DEs}
! 
! The {\tt deCount} argument can be used to specify the number of DEs. In this
! example a DELayout is created that contains four times as many DEs as there 
! are PETs in the VM.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=4*petCount, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Cyclic DE to PET mapping is the default. For 4 PETs this means:
! \begin{verbatim}
! DE 0, 4,  8, 12  -> PET 0
! DE 1, 5,  9, 13  -> PET 1
! DE 2, 6, 10, 14  -> PET 2
! DE 3, 7, 11, 15  -> PET 3
! \end{verbatim}
! The default DE to PET mapping can be overridden by providing the
! {\tt deGrouping} argument. This argument provides a positive integer group 
! number for each DE in the DELayout. All of the DEs of a group will be mapped 
! against the same PET. The actual group index is arbitrary (but must be 
! positive) and its value is of no consequence.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=4*petCount, &
    deGrouping=(/(i/4,i=0,4*petCount-1)/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! This will achieve blocked DE to PET mapping. For 4 PETs this means:
! \begin{verbatim}
! DE  0,  1,  2,  3  -> PET 0
! DE  4,  5,  6,  7  -> PET 1
! DE  8,  9, 10, 11  -> PET 2
! DE 12, 13, 14, 15  -> PET 3
! \end{verbatim}
!EOE



!BOE
! \subsubsection{DELayout with computational and communication weights}
! 
! The quality of the partitioning expressed by the DE to PET mapping depends
! on the amount and quality of information provided during DELayout creation.
! In the following example the {\tt compWeights} argument is used to specify
! relative computational weights for all DEs and communication weights for
! DE pairs are provided by the {\tt commWeights} argument. The example assumes
! four DEs.
!EOE
!BOC
  allocate(compWeights(4))
  allocate(commWeights(4, 4))
  ! setup compWeights and commWeights according to computational problem
  delayout = ESMF_DELayoutCreate(deCount=4, compWeights=compWeights, &
    commWeights=commWeights, rc=rc)
  deallocate(compWeights, commWeights)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The resulting DE to PET mapping depends on the specifics of the VM object and
! the provided compWeights and commWeights arrays.
!EOE

!BOE
! \subsubsection{DELayout from petMap}
! 
! Full control over the DE to PET mapping is provided via the {\tt petMap}
! argument. This example maps the DEs to PETs in reverse order. In the 4-PET
! case this will result in the following mapping:
! \begin{verbatim}
! DE 0 -> PET 3
! DE 1 -> PET 2
! DE 2 -> PET 1
! DE 3 -> PET 0
! \end{verbatim}
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(petMap=(/(i,i=petCount-1,0,-1)/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
!EOE

!BOE
! \subsubsection{DELayout from petMap with multiple DEs per PET}
! 
! The {\tt petMap} argument gives full control over DE to PET mapping. The 
! following example run on 4 or more PETs maps DEs to PETs according to the 
! following table:
! \begin{verbatim}
! DE 0 -> PET 3
! DE 1 -> PET 3
! DE 2 -> PET 1
! DE 3 -> PET 0
! DE 4 -> PET 2
! DE 5 -> PET 1
! DE 6 -> PET 3
! DE 7 -> PET 1
! \end{verbatim}
!EOE
if (petCount == 4) then
!BOC
  delayout = ESMF_DELayoutCreate(petMap=(/3, 3, 1, 0, 2, 1, 3, 1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
endif

!BOE
! \subsubsection{Working with a DELayout - simple 1-to-1 DE-to-PET mapping}
! 
! The simplest case is a DELayout where there is exactly one DE for every PET.
! Of course this implies that the number of DEs equals the number of PETs. 
! This special 1-to-1 DE-to-PET mapping is very common and many applications
! assume it. The following example shows how a DELayout can be queried about
! its mapping.
!
! First a default DELayout is created where the number of DEs equals the number
! of PETs, and are associated 1-to-1.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Next the DELayout is queried for the {\tt oneToOneFlag}, and the user code
! makes a decision based on its value.
!EOE
!BOC
  call ESMF_DELayoutGet(delayout, oneToOneFlag=oneToOneFlag, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (.not. oneToOneFlag) then
    ! handle the unexpected case of not dealing with a 1-to-1 mapping
  else
!EOC
!BOE
! 1-to-1 mapping is guaranteed in this branch and the following code can
! work under the simplifying assumption that every PET holds exactly one DE:
!EOE
!BOC  
    allocate(localDeToDeMap(1))
    call ESMF_DELayoutGet(delayout, localDeToDeMap=localDeToDeMap, rc=rc)
    if (rc /= ESMF_SUCCESS) finalrc=rc
    myDe = localDeToDeMap(1)
    deallocate(localDeToDeMap)
    if (finalrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Working with a DELayout - general DE-to-PET mapping}
! \label{DELayout_general_mapping}
! 
! In general a DELayout may map any number (including zero) of DEs against
! a single PET. The exact situation can be detected by querying the DELayout
! for the {\tt oneToOneFlag}. If this flag comes back as {\tt .true.} then the 
! DELayout maps exactly one DE against each PET, but if it comes back as
! {\tt .false.} the DELayout describes a more general DE-to-PET layout. The 
! following example shows how code can be be written to work for a general
! DELayout.
!
! First a DELayout is created with two more DEs than there are PETs. The 
! DELayout will consequently map some DEs to the same PET.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=petCount+2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The first piece of information needed on each PET is the {\tt localDeCount}.
! This number may be different on each PET and indicates how many DEs are 
! mapped against the local PET.
!EOE
!BOC
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The DELayout can further be queried for a list of DEs that are held by
! the local PET. This information is provided by the {\tt localDeToDeMap}
! argument. In ESMF a {\tt localDe} is an index that enumerates the DEs that
! are associated with the local PET. In many cases the exact bounds of the
! {\tt localDe} index range, e.g. $[0...localDeCount-1]$, or $[1...localDeCount]$ 
! does not matter, since it only affects how user code indexes into variables
! the user allocated, and therefore set the specific bounds. However, there are 
! a few Array and Field level calls that take {\tt localDe} input arguments. In 
! all those cases where the {\tt localDe} index variable is passed into an ESMF
! call as an input argument, it {\em must} be defined with a range starting at
! zero, i.e. $[0...localDeCount-1]$.
!
! For consistency with Array and Field, the following code uses a 
! $[0...localDeCount-1]$ range for the {\tt localDe} index variable, 
! although it is not strictly necessary here:
!BOC
  allocate(localDeToDeMap(0:localDeCount-1))
  call ESMF_DELayoutGet(delayout, localDeToDeMap=localDeToDeMap, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  do localDe=0, localDeCount-1
    workDe = localDeToDeMap(localDe)
!    print *, "I am PET", localPET, " and I am working on DE ", workDe
  enddo
  deallocate(localDeToDeMap)
  if (finalrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Work queue dynamic load balancing}
! 
! The DELayout API includes two calls that can be used to easily implement
! work queue dynamic load balancing. The workload is broken up into DEs
! (more than there are PETs) and processed by the PETs. Load balancing is
! only possible for ESMF multi-threaded VMs and requires that DEs are pinned
! to VASs instead of the PETs (default). The following example will
! run for any VM and DELayout, however, load balancing will only occur under the
! mentioned conditions.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=petCount+2, &
    pinflag=ESMF_PIN_DE_TO_VAS, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!BOC
  call ESMF_DELayoutGet(delayout, vasLocalDeCount=localDeCount, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  allocate(localDeToDeMap(localDeCount))
  call ESMF_DELayoutGet(delayout, vasLocalDeToDeMap=localDeToDeMap, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  do i=1, localDeCount
    workDe = localDeToDeMap(i)
    print *, "I am PET", localPET, &
             " and I am offering service for DE ", workDe
    reply = ESMF_DELayoutServiceOffer(delayout, de=workDe, rc=rc)
    if (rc /= ESMF_SUCCESS) finalrc=rc
    if (reply == ESMF_SERVICEREPLY_ACCEPT) then
      ! process work associated with workDe
      print *, "I am PET", localPET, ", service offer for DE ", workDe, &
        " was accepted."
      call ESMF_DELayoutServiceComplete(delayout, de=workDe, rc=rc)
      if (rc /= ESMF_SUCCESS) finalrc=rc
    endif
  enddo
  deallocate(localDeToDeMap)
  if (finalrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (finalrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  ! shut down ESMF
  call ESMF_Finalize(rc=rc)
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_DELayoutEx.F90"
  else
    print *, "FAIL: ESMF_DELayoutEx.F90"
  endif
  
end program 
