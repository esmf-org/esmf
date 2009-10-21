! $Id: ESMF_ArrayScatterGatherArbEx.F90,v 1.1 2009/10/21 05:54:17 theurich Exp $
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
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayScatterGatherArbEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer :: rc, petCount, localPet, finalrc
  integer :: i, j
  integer, allocatable :: arbSeqIndexList(:), farray(:,:)
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid, distgridAux
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: array, arrayAux
  type(ESMF_RouteHandle):: redistHandle

  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
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
! a single Array patch. A key requirement for {\tt ESMF\_ArrayScatter()}
! and {\tt ESMF\_ArrayGather()} is that the {\em shape} of the Fortran array
! and the Array patch must match. This means that the {\tt dimCount} must be
! equal, and that the size of each dimension must match. Element reordering
! during Scatter() and Gather() is only supported on a per dimension level,
! based on the {\tt decompflag} option used during DistGrid creation.
!
! While the {\tt ESMF\_ArrayScatter()} and {\tt ESMF\_ArrayGather()} methods
! cover a broad, and important spectrum of cases, there are situations that
! require a different set of rules to scatter and gather data between a
! Fortran array and an ESMF Array object. For instance, it is often convenient
! to create an Array on a DistGrid that was created with arbitrary,
! user-supplied sequence indices. 
!EOE

!BOC
  allocate(arbSeqIndexList(10))   ! each PET will have 10 elements
  
  do i=1, 10
    arbSeqIndexList(i) = (i-1)*petCount + localPet+1 ! initialize unique seq. indices
  enddo
  
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  deallocate(arbSeqIndexList)
  
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
  if (localPet == 0) then
    allocate(farray(10,petCount)) ! allocate 2D Fortran array 10 x petCount
    do j=1, petCount
      do i=1, 10
        farray(i,j) = 100 + (j-1)*10 + i    ! initialize to something
      enddo
    enddo
  else
    allocate(farray(0,0)) ! must allocate an array of size 0
  endif
  
  ! want to scatter farray defined on root across Array
  ! cannot use ArrayScatter() b/c shape doesn't match, and arb seq indices!
  
  distgridAux = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,petCount/), &
    regDecomp=(/1,1/), rc=rc)  ! DistGrid with only 1 DE
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC

!  call ESMF_DistGridPrint(distgridAux)

  
  arrayAux = ESMF_ArrayCreate(farray=farray, distgrid=distgridAux, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
 !call ESMF_ArrayPrint(array)
 !call ESMF_ArrayPrint(arrayAux)
 
 
!BOC
  call ESMF_ArrayRedistStore(srcArray=arrayAux, dstArray=array, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
!BOC
  call ESMF_ArrayRedist(srcArray=arrayAux, dstArray=array, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOC
  call ESMF_ArrayRedistRelease(routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
 
  !call ESMF_ArrayPrint(array)
 
 
!BOC
  call ESMF_ArrayDestroy(arrayAux, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgridAux, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!BOC  
  deallocate(farray)
!EOC


!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue
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
