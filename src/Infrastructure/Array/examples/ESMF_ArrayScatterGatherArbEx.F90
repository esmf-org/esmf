! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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

program ESMF_ArrayScatterGatherArbEx
#include "ESMF.h"


  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer :: rc, petCount, localPet, finalrc
  integer :: i, j, result
  integer, allocatable :: arbSeqIndexList(:), farray(:,:)
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid, distgridAux
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: array, arrayAux
  type(ESMF_RouteHandle):: scatterHandle, gatherHandle
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayScatterGatherArbEx"

  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayScatterGatherArbEx.Log", &
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

!BOE
!
! \subsubsection{Communication -- Scatter and Gather, revisited}
! \label{Array:ScatterGatherRevisited}
! 
! The {\tt ESMF\_ArrayScatter()} and {\tt ESMF\_ArrayGather()} calls, 
! introduced in section \ref{Array:ScatterGather}, provide a convenient
! way of communicating data between a Fortran array and all of the DEs of
! a single Array tile. A key requirement of {\tt ESMF\_ArrayScatter()}
! and {\tt ESMF\_ArrayGather()} is that the {\em shape} of the Fortran array
! and the Array tile must match. This means that the {\tt dimCount} must be
! equal, and that the size of each dimension must match. Element reordering
! during scatter and gather is only supported on a per dimension level,
! based on the {\tt decompflag} option available during DistGrid creation.
!
! While the {\tt ESMF\_ArrayScatter()} and {\tt ESMF\_ArrayGather()} methods
! cover a broad, and important spectrum of cases, there are situations that
! require a different set of rules to scatter and gather data between a
! Fortran array and an ESMF Array object. For instance, it is often convenient
! to create an Array on a DistGrid that was created with arbitrary,
! user-supplied sequence indices. See section \ref{DistGrid:ArbitrarySeqInd}
! for more background on DistGrids with arbitrary sequence indices.
!EOE

!BOC
  allocate(arbSeqIndexList(10))   ! each PET will have 10 elements
  
  do i=1, 10
    arbSeqIndexList(i) = (i-1)*petCount + localPet+1 ! initialize unique 
                                                     ! seq. indices
  enddo
  
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  deallocate(arbSeqIndexList)
  
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! This {\tt array} object holds 10 elements on each DE, and there is one DE
! per PET, for a total element count of 10 x {\tt petCount}. The
! {\tt arbSeqIndexList}, used during DistGrid creation, was constructed cyclic
! across all DEs. DE 0, for example, on a 4 PET run, would hold sequence
! indices 1, 5, 9, ... . DE 1 would hold 2, 6, 10, ..., and so on.
!
! The usefulness of the user-specified arbitrary sequence indices becomes
! clear when they are interpreted as global element ids. The ArrayRedist()
! and ArraySMM() communication methods are based on sequence index mapping
! between source and destination Arrays. Other than providing a canonical
! sequence index order via the default sequence scheme, outlined in
! \ref{Array:SparseMatMul}, ESMF does not place any restrictions on the
! sequence indices. Objects that were not created with user supplied
! sequence indices default to the ESMF sequence index order.
!
! A common, and useful interpretation of the arbitrary sequence indices, 
! specified during DistGrid creation, is that of relating them to the 
! canonical ESMF sequence index order of another data object. Within this
! interpretation the {\tt array} object created above could be viewed as an
! arbitrary distribution of a ({\tt petCount} x 10) 2D array. 
!
!EOE

!BOC
  if (localPet == 0) then
    allocate(farray(petCount,10)) ! allocate 2D Fortran array petCount x 10
    do j=1, 10
      do i=1, petCount
        farray(i,j) = 100 + (j-1)*petCount + i    ! initialize to something
      enddo
    enddo
  else
    allocate(farray(0,0)) ! must allocate an array of size 0 on all other PETs
  endif
!EOC
!BOE
! For a 4 PET run, {\tt farray} on PET 0 now holds the following data.
! \begin{verbatim}
!   -----1----2----3------------10-----> j
!   |
!   1   101, 105, 109, ....  , 137
!   |
!   2   102, 106, 110, ....  , 138
!   |
!   3   103, 107, 111, ....  , 139
!   |
!   4   104, 108, 112, ....  , 140
!   |
!   |
!   v
!  i
! \end{verbatim}
!
! On all other PETs {\tt farray} has a zero size allocation.
!
! Following the sequence index interpretation from above, scattering the data
! contained in {\tt farray} on PET 0 across the {\tt array} object created
! further up, seems like a well defined operation. Looking at it a bit closer,
! it becomes clear that it is in fact more of a redistribution than a simple
! scatter operation. The general rule for such a "redist-scatter"  operation,
! of a Fortran array, located on a single PET, into an ESMF Array, is to 
! use the canonical ESMF sequence index scheme to label the elements of the
! Fortran array, and to send the data to the Array element with the same
! sequence index.
!
! The just described "redist-scatter" operation is much more general than
! the standard {\tt ESMF\_ArrayScatter()} method. It does not require shape
! matching, and supports full element reordering based on the sequence indices.
! Before {\tt farray} can be scattered across {\tt array} in the described way,
! it must be wrapped into an ESMF Array object itself, essentially labeling the
! array elements according to the canonical sequence index scheme.
! 
!EOE
  
!BOC  
  distgridAux = ESMF_DistGridCreate(minIndex=(/1,1/), &
    maxIndex=(/petCount,10/), &
    regDecomp=(/1,1/), rc=rc) ! DistGrid with only 1 DE
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! The first step is to create a DistGrid object with only a single DE. This
! DE must be located on the PET on which the Fortran data array resides.
! In this example {\tt farray} holds data on PET 0, which is where the default
! DELayout will place the single DE defined in the DistGrid. If the {\tt farray}
! was setup on a different PET, an explicit DELayout would need to be created
! first, mapping the only DE to the PET on which the data is defined.
!
! Next the Array wrapper object can be created from the {\tt farray} and the
! just created DistGrid object.
!EOE

!BOC
  arrayAux = ESMF_ArrayCreate(farray=farray, distgrid=distgridAux, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! At this point all of the pieces are in place to use {\tt ESMF\_ArrayRedist()}
! to do the "redist-scatter" operation. The typical store/execute/release
! pattern must be followed.
!EOE  
 
!BOC
  call ESMF_ArrayRedistStore(srcArray=arrayAux, dstArray=array, &
    routehandle=scatterHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
!BOC
  call ESMF_ArrayRedist(srcArray=arrayAux, dstArray=array, &
    routehandle=scatterHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! In this example, after {\tt ESMF\_ArrayRedist()} was called, the content
! of {\tt array} on a 4 PET run would look like this:
! \begin{verbatim}
!  PET 0:   101, 105, 109, ....  , 137
!  PET 1:   102, 106, 110, ....  , 138
!  PET 2:   103, 107, 111, ....  , 139
!  PET 3:   104, 108, 112, ....  , 140
! \end{verbatim}
!
! Once set up, {\tt scatterHandle} can be used repeatedly to scatter data
! from {\tt farray} on PET 0 to all the DEs of {\tt array}. All of the
! resources should be released once {\tt scatterHandle} is no longer needed.
!EOE  

!BOC
  call ESMF_ArrayRedistRelease(routehandle=scatterHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! The opposite operation, i.e. {\em gathering} of the {\tt array} data
! into {\tt farray} on PET 0, follows a very similar setup. In fact, the
! {\tt arrayAux} object already constructed for the scatter direction, can
! directly be re-used. The only thing that is different for the "redist-gather",
! are the {\tt srcArray} and {\tt dstArray} argument assignments, reflecting
! the opposite direction of data movement.
!EOE  

!BOC
  call ESMF_ArrayRedistStore(srcArray=array, dstArray=arrayAux, &
    routehandle=gatherHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
!BOC
  call ESMF_ArrayRedist(srcArray=array, dstArray=arrayAux, &
    routehandle=gatherHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! Just as for the scatter case, the {\tt gatherHandle} can be used repeatedly
! to gather data from {\tt array} into {\tt farray} on PET 0. All of the
! resources should be released once {\tt gatherHandle} is no longer needed.
!EOE  

!BOC
  call ESMF_ArrayRedistRelease(routehandle=gatherHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Finally the wrapper Array {\tt arrayAux} and the associated DistGrid object
! can also be destroyed.
!EOE  
  
!BOC
  call ESMF_ArrayDestroy(arrayAux, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgridAux, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOE
! Further, the primary data objects of this example must be deallocated
! and destroyed.
!EOE  
  
!BOC  
  deallocate(farray)
  
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
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
    print *, "PASS: ESMF_ArrayScatterGatherArbEx.F90"
  else
    print *, "FAIL: ESMF_ArrayScatterGatherArbEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
