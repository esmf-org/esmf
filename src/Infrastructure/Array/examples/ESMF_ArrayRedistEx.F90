! $Id: ESMF_ArrayRedistEx.F90,v 1.1.2.2 2007/12/13 06:38:31 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_ArrayRedistEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, petCount, localPet
  type(ESMF_VM):: vm
  type(ESMF_DistGrid):: srcDistGrid, dstDistGrid
  type(ESMF_Array):: srcArray, dstArray
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_RouteHandle):: redistHandle
  integer :: finalrc
  
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

!BOE
!
! \subsubsection{Communication -- Redist}
! \label{Array:Redist}
! 
! Arrays used in different models often cover the same index space region,
! however, the distribution of the Arrays may be different, e.g. the models
! run on exclusive sets of PETs. Even if the Arrays are defined on the same
! list of PETs the decomposition may be different.
!EOE
!BOC
  srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/4,1/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/1,4/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! The number of elements covered by {\tt srcDistgrid} is identical to the number
! of elements covered by {\tt dstDistgrid} -- in fact the index space regions
! covered by both DistGrid objects are congruent.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! By construction {\tt srcArray} and {\tt dstArray} are of identical type and
! kind. Further the number of exclusive elements matches between both Arrays.
! These are the prerequesites for the application of an Array redistribution.
! In order to increase performance of the actual redistribution the
! communication patter must be precomputed and stored.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
!BOE
! The {\tt redistHandle} can now be used repeatedly on the {\tt srcArray}, 
! {\tt dstArray} pair to redistributed data from source to destination Array.
!EOE
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! The use of the {\tt redistHandle} is {\em not} restricted to {\tt srcArray}
! and {\tt dstArray}. The {\tt redistHandle} can be applied to redistribute data
! between any Array pairs that are congruent to the Array pair used during
! precomputation. Arrays are congruent if they are defined on matching DistGrids
! and the shape of local array allocations match for all DEs.
!
!EOE
!BOE
! The resources held by {\tt redistHandle} need to be deallocated by the user
! code before the handle becomes inaccessible.
!EOE
!BOC
  call ESMF_RouteHandleRelease(routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  call ESMF_ArrayDestroy(srcArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! The {\tt ESMF\_ArrayRedist()} operation also applies to Arrays with 
! undistributed dimensions. The additional requirement in this case is that
! the total undistributed element count, i.e. the product of the sizes of all
! undistributed dimensions, be the same for source and destination Array.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=4, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, &
    undistLBound=(/1,1/), undistUBound=(/2,4/), rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2,3/), undistLBound=(/1,1/), undistUBound=(/2,4/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Both {\tt srcArray} and {\tt dstArray} have two undistributed dimensions and
! a total count of undistributed elements of $ 2 \times 4 = 8$.
!
! The Array redistribution operation is defined in terms of sequentialized
! undistributed dimensions. In the above case this means that a unique sequence
! index will be assigned to each of the 8 undistributed elements. The sequence
! indices will be 1, 2, ..., 8, where sequence index 1 is assigned to the first
! element in the first (i.e. fastest varying in memory) undistributed dimension.
! The following undistributed elements are labeled in consecutive order as they
! are stored in memory.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
!BOE
! The redistribution operation by default applies the identity operation between
! the elements of undistributed dimensions. This means that source element with
! sequence index 1 will be mapped against destination element with sequence
! index 1 and so forth. Because of the way source and destination Arrays
! in the current example were constructed this corresponds to a mapping of
! dimensions 3 and 4 on {\tt srcArray} to dimensions 1 and 4 on {\tt dstArray},
! respectively.
!EOE
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_RouteHandleRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArrayDestroy(dstArray, rc=rc) ! destroy the Array object
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Array redistribution does {\em not} require the same number of undistributed
! dimensions in source and destination Array, merely the total number of
! undistributed elements must match.
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOC
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, &
    distgridToArrayMap=(/1,3/), undistLBound=(/11/), undistUBound=(/18/), &
    rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This {\tt dstArray} object only has a single undistributed dimension, while
! the {\tt srcArray}, defined further back, has two undistributed dimensions.
! However, the total undistributed element count for both Arrays is 8.
!EOE
!BOC
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
!BOE
! In this case the default identity operation between the elements of
! undistributed dimensions corresponds to a {\tt merging} of dimensions
! 3 and 4 on {\tt srcArray} into dimension 2 on {\tt dstArray}.
!EOE
!BOC
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=redistHandle, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_RouteHandleRelease(routehandle=redistHandle, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! THIS IS WHERE I NEED TO ADD:
!!!! - example for new ArrayRedistStore() interface that allows override of
!!!!   default identity operation in tensor space -> transpose of undistr. dims.
!!!! - example that demonstrates transposing between distributed and undistr. d


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
    print *, "PASS: ESMF_ArrayRedistEx.F90"
  else
    print *, "FAIL: ESMF_ArrayRedistEx.F90"
  endif
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  
end program
