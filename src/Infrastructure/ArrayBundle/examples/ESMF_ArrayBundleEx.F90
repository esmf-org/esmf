! $Id: ESMF_ArrayBundleEx.F90,v 1.1.2.1 2008/04/29 18:51:20 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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

program ESMF_ArrayBundleEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc, de, i, j, k, petCount, localDeCount, localPet
!  integer:: dim, nodeCount, dimCount
!  integer:: deNeighborCount, linkCount
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_DistGrid):: distgrid, distgrid3D, distgrid2D, distgrid1D
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Array):: array, array1, array2, array1D, array2D, array3D
!  type(ESMF_Array):: arrayTracer, arrayNScalar, arrayNEu, arrayNEv
!  type(ESMF_ArrayBundle):: arrayBundle
!  type(ESMF_Array), allocatable:: arrayList(:)
  type(ESMF_LocalArray), allocatable:: larrayList(:)
  type(ESMF_LocalArray), allocatable:: larrayList1(:), larrayList2(:)
  real(ESMF_KIND_R8), pointer:: myF90Array(:,:)
!  real(ESMF_KIND_R8), pointer:: myF90Array1(:,:), myF90Array2(:,:)
  real(ESMF_KIND_R8), pointer:: myF90Array1D(:), myF90Array3D(:,:,:)
  real(ESMF_KIND_R8), pointer:: myF90Array2D(:,:)
  real(ESMF_KIND_R8):: dummySum
  type(ESMF_IndexFlag):: indexflag
!  integer, allocatable:: dimExtent(:,:), indexList(:), regDecompDeCoord(:)
!  integer, allocatable:: minIndex(:,:), maxIndex(:,:), regDecomp(:,:)
!  integer, allocatable:: deBlockList(:,:), connectionList(:,:), connectionTransformList(:,:)
!  integer, allocatable:: deNeighborList(:), deNeighborInterface(:,:)
!  integer, allocatable:: linkList(:,:)
  integer, allocatable:: arrayToDistGridMap(:)
  integer, allocatable:: localDeList(:)
  integer, allocatable:: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer, allocatable:: totalLWidth(:,:), totalUWidth(:,:)
  integer, allocatable:: totalLBound(:,:), totalUBound(:,:)
!  integer, allocatable:: totalElementMask(:,:)
  integer, allocatable:: computationalLWidth(:,:), computationalUWidth(:,:)
  integer, allocatable:: computationalLBound(:,:), computationalUBound(:,:)
!  integer, allocatable:: haloLDepth(:), haloUDepth(:)
!  type(ESMF_Logical):: regDecompFlag
!  type(ESMF_RouteHandle):: haloHandle, haloHandle2

  ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 4) goto 10 ! TODO: use EXAMPLES_MULTI_ONLY once available
  
!BOE
! \subsubsection{ArrayBundle creation from a list of Arrays}
!
! The examples of the previous sections made the user responsible for 
! providing memory allocations for the PET-local regions of the Array object.
! The user was able to use any of the Fortran90 array methods or go through the
! {\tt ESMF\_LocalArray} interfaces to obtain memory allocations before
! passing them into ArrayCreate(). Alternatively, users may wish for ESMF to
! handle memory allocation of an Array object directly. The following example
! shows the interfaces that are available to the user to do just this.
! 
! To create an {\tt ESMF\_Array} object without providing an existing
! Fortran90 array or {\tt ESMF\_LocalArray} the {\em type, kind and rank}
! (tkr) of the Array must be specified in form of an {\tt ESMF\_ArraySpec}
! argument. Here a 2D Array of double precision real numbers is to be created:
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! Further an {\tt ESMF\_DistGrid} argument must be constructed that holds
! information about the entire domain (patchwork) and the decomposition into 
! DE-local exclusive
! regions. The following line creates a DistGrid for a 5x5 global LR domain 
! that is decomposed into 2 x 3 = 6 DEs.
!EOE
!BOC
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/5,5/), &
    regDecomp=(/2,3/), rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!BOE
! This is enough information to create a Array object with default settings.
!EOE
!BOC
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, rc=rc)
!EOC  
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  call ESMF_ArrayPrint(array, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



  call ESMF_DistGridDestroy(distgrid, rc=rc)


10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_ArrayBundleEx.F90"
  else
    print *, "FAIL: ESMF_ArrayBundleEx.F90"
  endif
  
end program
