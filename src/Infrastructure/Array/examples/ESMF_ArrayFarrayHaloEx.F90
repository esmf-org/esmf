! $Id: ESMF_ArrayFarrayHaloEx.F90,v 1.7.2.7 2009/01/21 21:25:19 cdeluca Exp $
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

!BOE
! \subsubsection{Array from native Fortran array with elements for halo}
! 
! The example of the previous section showed how easy it is to create an Array
! object from existing PET-local Fortran arrays. The example did, however, not
! define any halos around the DE-local regions. The following code demonstrates
! how an Array object with space for a halo can be set up.
!EOE
!BOC
program ESMF_ArrayFarrayHaloEx

  use ESMF_Mod
  
  implicit none
  
!EOC
!BOE
! The allocatable array {\tt farrayA} will be used to provide the PET-local
! Fortran array for this example.
!EOE
!BOC
  ! local variables
  real(ESMF_KIND_R8), allocatable :: farrayA(:,:)   ! allocatable Fortran array
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)     ! matching Fortran array pointer
  type(ESMF_DistGrid)         :: distgrid           ! DistGrid object
  type(ESMF_Array)            :: array              ! Array object
  integer                     :: rc, i, j
  real                        :: localSum
  
!EOC
  type(ESMF_VM):: vm
  integer:: petCount
  
  ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  
!BOC
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!EOC
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available

!BOE
! The Array is to cover the exact same index space as in the previous
! example. Furthermore decomposition and distribution are also kept the same.
! Hence the same DistGrid object will be created and it is expected to 
! execute this example with 4 PETs.
!
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This DistGrid describes a 40 x 10 index space that will be decomposed into 
! 4 DEs when executed on 4 PETs, associating 1 DE per PET. Each DE-local 
! exclusive region contains 10 x 10 elements. The DistGrid also stores and provides
! information about the relationship between DEs in index space, however,
! DistGrid does not contain information about halos. Arrays contain halo 
! information and it is possible to create multiple Arrays covering the same
! index space with identical decomposition and distribution using the same
! DistGrid object, while defining different, Array-specific halo regions.
!
! The extra memory required to cover the halo in the Array object must be 
! taken into account when allocating the PET-local {\tt farrayA} arrays. For
! a halo of 2 elements in each direction the following allocation will suffice.
!EOE
!BOC
  allocate(farrayA(14,14))    ! Fortran array with halo: 14 = 10 + 2 * 2
!EOC
  farrayA = 36.71d0 ! initialize
!BOE
! The {\tt farrayA} can now be used to create an Array object with enough space
! for a two element halo in each direction. The Array creation method checks for 
! each PET that the local Fortran array can accomodate the requested regions.
!
! The default behavior of ArrayCreate() is to center the exclusive region within
! the total region. Consequently the following call will provide the 2 extra 
! elements on each side of the exclusive 10 x 10 region without having to specify
! any additional arguments.
!EOE
!BOC
  array = ESMF_ArrayCreate(farray=farrayA, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
call ESMF_ArrayPrint(array)
!BOE
! The exclusive Array region on each PET can be accessed through a suitable
! Fortran array pointer. See section \ref{Array_regions_and_default_bounds}
! for more details on Array regions.
!EOE
!BOC
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Following Array bounds convention, which by default puts the beginning of 
! the exclusive region at (1, 1, ...), the following loop will add up the 
! values of the local exclusive region for each DE, regardless of how the bounds
! were chosen for the original PET-local {\tt farrayA} arrays.
!EOE
!BOC
  localSum = 0.
  do j=1, 10
    do i=1, 10
      localSum = localSum + farrayPtr(i, j)
    enddo
  enddo
!EOC
print *, "localSum=", localSum
!BOE
! Elements with $i$ or $j$ in the [-1,0] or [11,12] ranges are located outside the
! exclusive region and may be used to define extra computational points or 
! halo operations.
!
! Cleanup and shut down ESMF.
!EOE
!BOC
  call ESMF_ArrayDestroy(array, rc=rc)
  deallocate(farrayA)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
!EOC
  
10 continue
!BOC
  call ESMF_Finalize(rc=rc)
!EOC
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayFarrayHaloEx.F90"
  else
    print *, "FAIL: ESMF_ArrayFarrayHaloEx.F90"
  endif

!BOC
end program
!EOC
