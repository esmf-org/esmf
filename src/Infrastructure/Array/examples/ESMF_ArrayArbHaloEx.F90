! $Id: ESMF_ArrayArbHaloEx.F90,v 1.2 2010/07/08 04:48:03 theurich Exp $
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

program ESMF_ArrayArbHaloEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: distgrid
  type(ESMF_Array):: array, array2
  type(ESMF_RouteHandle):: haloHandle
  integer :: finalrc
  
  
  integer:: i, j
  integer:: seqIndexList(5) ! arbitrary seqIndices on each PET

  real(ESMF_KIND_R8), pointer :: farrayPtr1d(:), farrayPtr2d(:,:)
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="ArrayArbHaloEx.Log", &
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

!BOE
!
! \subsubsection{Communication -- Halo for arbitrary distribution}
! \label{Array:ArbHalo}
! 
!EOE

  do i=1, 5
    seqIndexList(i) = localPet + (i - 1) * petCount + 1
  enddo
  
!print *, localPet, seqIndexList
  
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=seqIndexList, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!call ESMF_DistGridPrint(distgrid)

  if (localPet==0) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=1, &
      distgrid=distgrid, haloSeqIndexList=(/1/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==1) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=1, &
      distgrid=distgrid, haloSeqIndexList=(/1,2/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==2) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=1, &
      distgrid=distgrid, haloSeqIndexList=(/1,2,3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==3) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=1, &
      distgrid=distgrid, haloSeqIndexList=(/1,2,3,4/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr1d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  do i=1, 5
    farrayPtr1d(i) = seqIndexList(i) / 10.
  enddo
  
!call ESMF_ArrayPrint(array)

  call ESMF_ArrayHaloStore(array, routehandle=haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayHalo(array, routehandle=haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayHaloRelease(haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!call ESMF_ArrayPrint(array)

  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

! ------------------------------------------------------------------------------
  
  if (localPet==0) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==1) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==2) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==3) then
    array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3,4/), &
      undistLBound=(/1/), undistUBound=(/3/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  do j=1, 3
    do i=1, 5
      farrayPtr2d(j,i) = seqIndexList(i) / 10. + 100.*j
    enddo
  enddo

  call ESMF_ArrayHaloStore(array, routehandle=haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayHalo(array, routehandle=haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!call ESMF_ArrayPrint(array)

  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  
  
  
  if (localPet==0) then
    array2 = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==1) then
    array2 = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==2) then
    array2 = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  if (localPet==3) then
    array2 = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, rank=2, &
      distgrid=distgrid, distgridToArrayMap=(/2/), haloSeqIndexList=(/1,2,3,4/), &
      undistLBound=(/1/), undistUBound=(/6/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif
  
  call ESMF_ArrayGet(array2, farrayPtr=farrayPtr2d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  do j=1, 6
    do i=1, 5
      farrayPtr2d(j,i) = seqIndexList(i) / 10. + 100.*j
    enddo
  enddo

  
  call ESMF_ArrayHalo(array2, routehandle=haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayHaloRelease(haloHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

call ESMF_ArrayPrint(array2)

  call ESMF_ArrayDestroy(array2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  
  
! ------------------------------------------------------------------------------

  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
10 continue
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
