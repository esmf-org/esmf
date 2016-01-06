! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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

program ESMF_ArrayArbHaloEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid
  type(ESMF_Array):: array, array2
  type(ESMF_RouteHandle):: haloHandle
  integer :: finalrc
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg
  
  
  integer:: i, j, result
  integer:: seqIndexList(5) ! arbitrary seqIndices on each PET

  real(ESMF_KIND_R8), pointer :: farrayPtr1d(:), farrayPtr2d(:,:)
  

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_ArrayArbHaloEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayArbHaloEx.Log", &
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
! \subsubsection{Communication -- Halo for arbitrary distribution}
! \label{Array:ArbHalo}
!
! In the previous section the Array {\em halo} operation was demonstrated 
! for regularly decomposed ESMF Arrays. However, the ESMF halo operation
! is not restricted to regular decompositions. The same Array halo methods
! apply unchanged to Arrays that are created on arbitrarily distributed
! DistGrids. This includes the non-blocking features discussed in section
! \ref{Array:CommNB}.
!
! All of the examples in this section are based on the same arbitrarily
! distributed DistGrid. Section \ref{DistGrid:ArbitrarySeqInd} discusses
! DistGrids with user-supplied, arbitrary sequence indices in detail. Here
! a global index space range from 1 through 20 is decomposed across 4 DEs. 
! There are 4 PETs in this example with 1 DE per PET. Each PET constructs
! its local {\tt seqIndexList} variable.
!EOE
!BOC
  do i=1, 5
    seqIndexList(i) = localPet + (i - 1) * petCount + 1
  enddo
!EOC
!BOE
! This results in the following cyclic distribution scheme:
! \begin{verbatim}
! DE 0 on PET 0: seqIndexList = (/1, 5, 9, 13, 17/)
! DE 1 on PET 1: seqIndexList = (/2, 6, 10, 14, 18/)
! DE 2 on PET 2: seqIndexList = (/3, 7, 11, 15, 19/)
! DE 3 on PET 3: seqIndexList = (/4, 8, 12, 16, 20/)
! \end{verbatim}
!
! The local {\tt seqIndexList} variables are then used to create a
! DistGrid with the indicated arbitrary distribution pattern.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=seqIndexList, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The resulting DistGrid is one-dimensional, although the user code may
! interpret the sequence indices as a 1D map into a problem of higher
! dimensionality. 
! 
! In this example the local DE on each PET is associated with a 5 element
! exclusive region. Providing {\tt seqIndexList} of different size on the
! different PETs is supported and would result in different number of
! exclusive elements on each PET.
!
! Creating an ESMF Array on top of a DistGrid with arbitrary sequence indices
! is in principle no different from creating an Array on a regular DistGrid. 
! However, while an Array that was created on a regular DistGrid automatically
! inherits the index space topology information that is contained within the
! DistGrid object, there is no such topology information available for
! DistGrid objects with arbitrary sequence indices. As a consequence of
! this, Arrays created on arbitrary DistGrids do not automatically have
! the information that is required to associated halo elements with the
! exclusive elements across DEs. Instead the user must supply this information
! explicitly during Array creation.
!
! Multiple ArrayCreate() interfaces exist that allow the creation of an Array
! on a DistGrid with arbitrary sequence indices. The sequence indices for the
! halo region of the local DE are supplied through an additional argument
! with dummy name {\tt haloSeqIndexList}. As in the regular case, the
! ArrayCreate() interfaces differ in the way that the memory allocations for
! the Array elements are passed into the call. The following code shows how 
! an ESMF Array can be wrapped around existing PET-local memory allocations.
! The allocations are of different size on each PET as to accommodate the correct
! number of local Array elements (exclusive region + halo region).
!EOE
!BOC
  allocate(farrayPtr1d(5+localPet+1)) !use explicit Fortran allocate statement
  
  if (localPet==0) then
    array = ESMF_ArrayCreate(distgrid, farrayPtr1d, &
      haloSeqIndexList=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==1) then
    array = ESMF_ArrayCreate(distgrid, farrayPtr1d, &
      haloSeqIndexList=(/1,19/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==2) then
    array = ESMF_ArrayCreate(distgrid, farrayPtr1d, &
      haloSeqIndexList=(/16,6,9/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==3) then
    array = ESMF_ArrayCreate(distgrid, farrayPtr1d, &
      haloSeqIndexList=(/1,3,1,4/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC
!BOE
! The {\tt haloSeqIndexList} arguments are 1D arrays of sequence indices.
! It is through this argument that the user associates the halo elements with
! exclusive Array elements covered by the DistGrid. In this example there
! are different number of halo elements on each DE. They are associated
! with exclusive elements as follows:
!
! \begin{verbatim}
! halo on DE 0 on PET 0: <seqIndex=6>  2nd exclusive element on DE 1
! halo on DE 1 on PET 1: <seqIndex=1>  1st exclusive element on DE 0
!                        <seqIndex=19> 5th exclusive element on DE 2
! halo on DE 2 on PET 2: <seqIndex=16> 4th exclusive element on DE 3
!                        <seqIndex=6>  2nd exclusive element on DE 1
!                        <seqIndex=9>  3rd exclusive element on DE 0
! halo on DE 3 on PET 3: <seqIndex=1>  1st exclusive element on DE 0
!                        <seqIndex=3>  1st exclusive element on DE 2
!                        <seqIndex=1>  1st exclusive element on DE 0
!                        <seqIndex=4>  1st exclusive element on DE 3
! \end{verbatim}
!
! The above {\tt haloSeqIndexList} arguments were constructed very artificially
! in order to show the following general features:
! \begin{itemize}
! \item There is no restriction on the order in which the indices in a
! {\tt haloSeqIndexList} can appear.
! \item The same sequence index may appear in multiple {\tt haloSeqIndexList}
! arguments.
! \item The same sequence index may appear multiple times in the same 
! {\tt haloSeqIndexList} argument.
! \item A local sequence index may appear in a {\tt haloSeqIndexList} argument.
! \end{itemize}
!
! The ArrayCreate() call checks that the provided Fortran memory allocation
! is correctly sized to hold the exclusive elements, as indicated by the
! DistGrid object, plus the halo elements as indicated by the local
! {\tt haloSeqIndexList} argument. The size of the Fortran allocation must
! match exactly or a runtime error will be returned.
!
! Analogous to the case of Arrays on regular DistGrids, it is the exclusive
! region of the local DE that is typically modified by the code running on 
! each PET. All of the ArrayCreate() calls that accept the
! {\tt haloSeqIndexList} argument place the exclusive region at the beginning
! of the memory allocation on each DE and use the remaining space for the halo
! elements. The following loop demonstrates this by filling the exclusive 
! elements on each DE with initial values. Remember that in this example each 
! DE holds 5 exclusive elements associated with different arbitrary sequence 
! indices.
!EOE
!BOC
  do i=1, 5
    farrayPtr1d(i) = seqIndexList(i) / 10.
  enddo
!BOE
! Now the exclusive elements of {\tt array} are initialized on each DE, however,
! the halo elements remain unchanged. A RouteHandle can be set up that encodes
! the required communication pattern for a halo exchange. The halo exchange
! is precomputed according to the arbitrary sequence indices specified for the
! exclusive elements by the DistGrid and the sequence indices provided by the 
! user for each halo element on the local DE in form of the 
! {\tt haloSeqIndexList} argument during ArrayCreate(). 
! 
!EOE
!BOC
  call ESMF_ArrayHaloStore(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Executing this halo operation will update the local halo elements according
! to the associated sequence indices.
!EOE
!BOC
  call ESMF_ArrayHalo(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! As always it is good practice to release the RouteHandle when done with it.
!EOE
!BOC
  call ESMF_ArrayHaloRelease(haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Also the Array object should be destroyed when no longer needed.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Further, since the memory allocation was done explicitly using the Fortran
! {\tt allocate()} statement, it is necessary to explicitly deallocate in order
! to prevent memory leaks in the user application.
!BOC
  deallocate(farrayPtr1d)
!EOC

! ------------------------------------------------------------------------------

!BOE
! Alternatively the exact same Array can be created where ESMF does the
! memory allocation and deallocation. In this case the {\tt typekind} of the 
! Array must be specified explicitly.
!EOE
!BOC  
  if (localPet==0) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==1) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/1,19/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==2) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/16,6,9/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==3) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/1,3,1,4/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC
!BOE
! Use ArrayGet() to gain access to the local memory allocation.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr1d, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! The returned Fortran pointer can now be used to initialize the exclusive
! elements on each DE as in the previous case.
!EOE
!BOC
  do i=1, 5
    farrayPtr1d(i) = seqIndexList(i) / 10.
  enddo
!EOC
!call ESMF_ArrayPrint(array)
!BOE
! Identical halo operations are constructed and used.
!EOE
!BOC
  call ESMF_ArrayHaloStore(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  call ESMF_ArrayHalo(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!BOC
  call ESMF_ArrayHaloRelease(haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!call ESMF_ArrayPrint(array)

!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! ------------------------------------------------------------------------------

!BOE
! A current limitation of the Array implementation restricts DistGrids that
! contain user-specified, arbitrary sequence indices to be exactly 1D
! when used to create Arrays. See section \ref{Array:rest} for a list of 
! current implementation restrictions. However, an Array created on such a
! 1D arbitrary DistGrid is allowed to have undistributed dimensions. The
! following example creates an Array on the same arbitrary DistGrid, with the
! same arbitrary sequence indices for the halo elements as before, but with
! one undistributed dimension with a size of 3.
!EOE
!BOC
  if (localPet==0) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/6/), undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==1) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/1,19/), undistLBound=(/1/), undistUBound=(/3/), &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==2) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/16,6,9/), undistLBound=(/1/), undistUBound=(/3/), &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==3) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      haloSeqIndexList=(/1,3,1,4/), undistLBound=(/1/), undistUBound=(/3/), &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC
!BOE
! By default the DistGrid dimension is mapped to the first Array dimension, 
! associating the remaining Array dimensions with the undistributed dimensions
! in sequence. The dimension order is important when accessing the individual
! Array elements. Here the same initialization as before is extended to 
! cover the undistributed dimension.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2d, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  do j=1, 3
    do i=1, 5
      farrayPtr2d(i,j) = seqIndexList(i) / 10. + 100.*j
    enddo
  enddo
!EOC
!BOE
! In the context of the Array halo operation additional undistributed dimensions
! are treated in a simple factorized manner. The same halo association between
! elements that is encoded in the 1D arbitrary sequence index scheme is
! applied to each undistributed element separately. This is completely 
! transparent on the user level and the same halo methods are used as before.
!EOE
!BOC
  call ESMF_ArrayHaloStore(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayHalo(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC  
  call ESMF_ArrayHaloRelease(haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!call ESMF_ArrayPrint(array)

!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

! ------------------------------------------------------------------------------

!BOE
! In some situations it is more convenient to associate some or all of
! the undistributed dimensions with the first Array dimensions. This can be
! done easily by explicitly mapping the DistGrid dimension to an Array dimension
! other than the first one. The following code creates essentially the same 
! Array as before, but with swapped dimension order.
!EOE
!BOC
  if (localPet==0) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==1) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==2) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==3) then
    array = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3,4/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC
!BOE
! The swapped dimension order results in a swapping of {\tt i} and {\tt j} when
! accessing Array elements in the loop.
!EOE
!BOC  
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2d, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  do j=1, 3
    do i=1, 5
      farrayPtr2d(j,i) = seqIndexList(i) / 10. + 100.*j
    enddo
  enddo
!EOC
!BOE
! Again there is no difference in how the the halo operations are applied.
!EOE
!BOC
  call ESMF_ArrayHaloStore(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  call ESMF_ArrayHalo(array, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
!call ESMF_ArrayPrint(array)

!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
! ------------------------------------------------------------------------------

!BOE
! One of the benefits of mapping the undistributed dimension(s) to the 
! "left side" of the Array dimensions is that Arrays that only differ 
! in the size of the undistributed dimension(s) are weakly congruent in this
! arrangement. Weakly congruent Arrays can reuse the same RouteHandle, saving
! the overhead that is caused by the precompute step. In order to demonstrate 
! this the RouteHandle of the previous halo call was not yet released and will
! be applied to a weakly congruent Array.
!
! The following code creates an Array that is weakly congruent to the the 
! previous Array by using the same input information as before, only that
! the size of the undistributed dimension is now 6 instead of 3.
!EOE
!BOC
  if (localPet==0) then
    array2 = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==1) then
    array2 = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==2) then
    array2 = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  if (localPet==3) then
    array2 = ESMF_ArrayCreate(distgrid=distgrid, typekind=ESMF_TYPEKIND_R8, &
      distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3,4/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
!EOC
!BOE
! Again the exclusive Array elements must be initialized.
!EOE
!BOC
  call ESMF_ArrayGet(array2, farrayPtr=farrayPtr2d, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
  do j=1, 6
    do i=1, 5
      farrayPtr2d(j,i) = seqIndexList(i) / 10. + 100.*j
    enddo
  enddo
!EOC
!BOE
! Now the {\tt haloHandle} that was previously pre-computed for {\tt array} can
! be used directly for the weakly congruent {\tt array2}.
!EOE
!BOC
  call ESMF_ArrayHalo(array2, routehandle=haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOE
! Release the RouteHandle after its last use and clean up the remaining
! Array and DistGrid objects.
!EOE
!BOC
  call ESMF_ArrayHaloRelease(haloHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!call ESMF_ArrayPrint(array2)

!BOC
  call ESMF_ArrayDestroy(array2, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
! ------------------------------------------------------------------------------

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
    print *, "PASS: ESMF_ArrayArbHaloEx.F90"
  else
    print *, "FAIL: ESMF_ArrayArbHaloEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
