! $Id: ESMF_DELayoutEx.F90,v 1.14.2.3 2009/01/21 21:25:20 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, i, localPET, petCount, localDeCount, myDe, workDe
  integer, allocatable:: commWeights(:,:), compWeights(:), localDeList(:)
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_Logical):: oneToOneFlag
  type(ESMF_DELayoutServiceReply):: reply
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
  call ESMF_VMGet(vm, localPET=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99

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
  if (rc /= ESMF_SUCCESS) goto 99
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
  if (rc /= ESMF_SUCCESS) goto 99
!BOE
! The optional {\tt vm} argument can be provided to DELayoutCreate() to lower 
! the method's overhead by the amount it takes to determine the current VM.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(vm=vm, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) goto 99
!  call ESMF_DELayoutPrint(delayout, rc=rc)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
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
  if (rc /= ESMF_SUCCESS) goto 99
!  call ESMF_DELayoutPrint(delayout, rc=rc)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
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
  if (rc /= ESMF_SUCCESS) goto 99
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
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
  if (rc /= ESMF_SUCCESS) goto 99
!  call ESMF_DELayoutPrint(delayout, rc=rc)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
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
  if (rc /= ESMF_SUCCESS) goto 99
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
!BOE
! The resulting DE to PET mapping depends on the specifics of the VM object and
! the provided compWeights and commWeights arrays.
!EOE

!BOE
! \subsubsection{DELayout from petMap}
! 
! Full control over the DE to PET mapping is provided via the {\tt petMap}
! argument. This example maps the DEs to PETs in reverse order. In the 4 PET
! case this will result in the following mapping:
! \begin{verbatim}
! DE 0 -> PET 3
! DE 1 -> PET 1
! DE 2 -> PET 2
! DE 3 -> PET 0
! \end{verbatim}
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(petMap=(/(i,i=petCount-1,0,-1)/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) goto 99
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
!BOE
!EOE

!BOE
! \subsubsection{DELayout from petMap with multiple DEs per PET}
! 
! The {\tt petMap} argument gives full control over DE to PET mapping. The 
! following example runs on 4 or more PETs maps DEs to PETs according to the 
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
  if (rc /= ESMF_SUCCESS) goto 99
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
endif

!BOE
! \subsubsection{Working with a DELayout - simple 1-to-1 DE to PET mapping}
! 
! The simplest case is a DELayout with as many DEs as PETs where each DE is 
! against a separate PET. This of course implies that the number of
! DEs equals the number of PETs. This special 1-to-1 DE to PET
! mapping is very common and many codes assume this mapping. The following 
! example code shows how a DELayout can be queried about its mapping.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) goto 99
!BOC
  call ESMF_DELayoutGet(delayout, oneToOneFlag=oneToOneFlag, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  if (oneToOneFlag == ESMF_FALSE) then
    ! handle the unexpected case of general DE to PET mapping
  endif
  allocate(localDeList(1))
  call ESMF_DELayoutGet(delayout, localDeList=localDeList, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  myDe = localDeList(1)
  deallocate(localDeList)
!EOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99

!BOE
! \subsubsection{Working with a DELayout - general DE to PET mapping}
! 
! In general a DELayout may describe a DE to PET mapping that is not 1-to-1. The
! following example shows how code can be written in a general form that will 
! work on all PETs for DELayouts with general or 1-to-1 DE to PET mapping.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=petCount+2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) goto 99
!BOC
  call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  allocate(localDeList(localDeCount))
  call ESMF_DELayoutGet(delayout, localDeList=localDeList, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  do i=1, localDeCount
    workDe = localDeList(i)
!    print *, "I am PET", localPET, " and I am working on DE ", workDe
  enddo
  deallocate(localDeList)
!EOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99

!BOE
! \subsubsection{Work queue dynamic load balancing}
! 
! The DELayout API includes two calls that can be used to easily implement
! work queue dynamic load balancing. The work load is broken up into DEs 
! (more than there are PETs) and processed by the PETs. Load balancing is
! only possible for ESMF multi-threaded VMs and requires that DEs are pinned
! to VASs instead of the PETs (default). The following example will
! run for any VM and DELayout, however, load balancing will only occur under the
! mentioned conditions.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(deCount=petCount+2, dePinFlag=ESMF_DE_PIN_VAS,&
    rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) goto 99
!  call ESMF_DELayoutPrint(delayout, rc=rc)
!BOC
  call ESMF_DELayoutGet(delayout, vasLocalDeCount=localDeCount, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  allocate(localDeList(localDeCount))
  call ESMF_DELayoutGet(delayout, vasLocalDeList=localDeList, rc=rc)
  if (rc /= ESMF_SUCCESS) finalrc=rc
  do i=1, localDeCount
    workDe = localDeList(i)
    print *, "I am PET", localPET, " and I am offering service for DE ", workDe
    reply = ESMF_DELayoutServiceOffer(delayout, de=workDe, rc=rc)
    if (rc /= ESMF_SUCCESS) finalrc=rc
    if (reply == ESMF_DELAYOUT_SERVICE_ACCEPT) then
      ! process work associated with workDe
      print *, "I am PET", localPET, ", service offer for DE ", workDe, &
        " was accepted."
      call ESMF_DELayoutServiceComplete(delayout, de=workDe, rc=rc)
      if (rc /= ESMF_SUCCESS) finalrc=rc
    endif
  enddo
  deallocate(localDeList)
!EOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) goto 99
  

  ! shut down ESMF
  call ESMF_Finalize(rc=rc)
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_DELayoutEx.F90"
  else
    print *, "FAIL: ESMF_DELayoutEx.F90"
  endif
  goto 100
  
99 continue   ! Abort
  print *, "FAIL: ESMF_DELayoutEx.F90"
  call ESMF_Finalize(terminationflag=ESMF_ABORT)

100 continue  
end program
