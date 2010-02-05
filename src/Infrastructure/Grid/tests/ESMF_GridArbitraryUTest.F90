! $Id: ESMF_GridArbitraryUTest.F90,v 1.10.2.1 2010/02/05 22:35:15 theurich Exp $
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
!
program ESMF_GridArbitraryUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridArbitraryTest - Check Arbitrary Grid Create Routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 Grid Create unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_GridArbitraryUTest.F90,v 1.10.2.1 2010/02/05 22:35:15 theurich Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  integer :: localrc, rc, petCount, myPet, halfPets
  logical :: correct
  type(ESMF_TypeKind) :: typekind

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  type(ESMF_Grid) :: grid
  type(ESMF_DELayout) :: delayout
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_ArraySpec) :: arrayspec1D, arrayspec2D
  integer :: ind1d, xdim, ydim, zdim, total, x, y
  integer :: i, j, remain
  integer :: localCount, localCount1
  integer, allocatable :: localIndices(:,:)
  integer, allocatable :: localIndices1(:,:)
  integer, allocatable :: local1DIndices(:)
  type(ESMF_Array) :: array1D, array1, array2D, array2
  type(ESMF_Array) :: array1D_1,array1D_2
  integer :: coordDimMap(3,3), dimCount, distdimCount
  integer :: undistLBound(2), undistUBound(2)
  integer :: elementCounts(4)
  integer :: lowbound(3), upbound(3)
  type(ESMF_IndexFlag) :: indexflag
  integer :: distgridToGridMap(3), coordDimCount(3), distDim(2)
  integer :: distgridToArrayMap(2)
  real(ESMF_KIND_R8), pointer :: fptr2D(:,:)
  integer :: rank
  integer :: localCount2(1), deList(1), deCount
  integer, allocatable:: minIndex1(:), maxIndex1(:), localCount3(:)
  integer, allocatable:: minIndex(:,:), maxIndex(:,:)
  integer, allocatable:: indexArray(:,:)
  integer ::  index(2), index3(3)
  integer :: index1(2), index2(2)
  integer :: memDimCount, arbDimCount
  type(ESMF_GridDecompType) :: decompType
  REAL(ESMF_KIND_R8), pointer :: dimarray(:), fptr1D(:)
  type(ESMF_Array) :: myarray
  type(ESMF_LocalArray) :: larray
  REAL(ESMF_KIND_R8)  :: coord3(3)
  integer :: badcount

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! grid dimension: xdim and ydim are arbitrarily distributed
  xdim = 100
  ydim = 200
  zdim = 4

  ! calculate the localcount and the local indices based on the total number of PETS
  total = xdim*ydim
  halfPets = petCount/2
  ! let's make the first half pet twice of the cells of the second half
  localCount = total/(petCount+halfPets)
  remain = total-localCount*(petCount+halfPets)
  if (myPet < halfPets) localCount = localCount*2
  if (myPet == petCount-1) localCount = localCount+remain
  ! car deal the cells with the first half of the Pets gets two each time
  ! the remaining cells are given to the last Pet
  allocate(localIndices(localCount,2))

  if (myPet < halfPets) then
     ind1d = myPet*2
     do i=1,localCount,2
       y = mod(ind1d,ydim)+1
       x = ind1d/ydim+1
       localIndices(i,1)=y
       localIndices(i,2)=x
       if (y<ydim) then
         localIndices(i+1,1)=y+1
         localIndices(i+1,2)=x
       else
         localIndices(i+1,1)=1
         localIndices(i+1,2)=x+1
       endif
       ind1d = ind1d+petCount+halfPets
     enddo 
  else
     ind1d=myPet+halfPets
     do i=1,localCount
       y = mod(ind1d,ydim)+1
       x = ind1d/ydim+1
       localIndices(i,1)=y
       localIndices(i,2)=x
       ind1d = ind1d+petCount+halfPets
     enddo
  endif
  if (myPet == petCount-1) then
    ind1d = total-remain+1
    do i=localCount-remain+1,localCount
       y = mod(ind1d,ydim)+1
       x = ind1d/ydim+1
       localIndices(i,1)=y
       localIndices(i,2)=x
       ind1d = ind1d+1
    enddo
  endif

  !----------------------------------------------------------------------------=
  ! Test Set 1:  2D Arbitrary Grid with both dimensions distributed
  !-----------------------------------------------------------------------------
  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
	
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing Grid Validate"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing Grid Get"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! get decomp type
  call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, &
	coordDimMap=coordDimMap, &
         indexflag=indexflag, localArbIndexCount=localCount1, &
         memDimCount=memDimCount, arbDimCount=arbDimCount, &
         rc=localrc)
   !print *, "PE ", myPet, "localArbIndexCount=", localCount1, "coorddimCount=",coordDimCount(1), coordDimCount(2)
   !print *, "PE ", myPet, "distgridToGridMap=", distgridToGridMap, "coorddimMap=",coordDimMap
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get minIndex and maxIndex
  allocate(minIndex1(dimCount), maxIndex1(dimCount))

  ! get bounds for stagger location
  call ESMF_GridGet(grid, staggerloc=ESMF_STAGGERLOC_CENTER, minIndex=minIndex1, maxIndex=maxIndex1, &
	rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  if (decompType .ne. ESMF_GRID_ARBITRARY) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if (memDimCount .ne. 1) correct=.false.
  if (arbDimCount .ne. 2) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  if (localCount .ne. localCount1) correct = .false.
  if ((minIndex1(1) .ne. 1) .and. (minIndex1(2) .ne. 1)) correct = .false. 
  if ((maxIndex1(1) .ne. xdim) .and. (maxIndex1(2) .ne. ydim)) correct = .false. 
  deallocate(minIndex1, maxIndex1)
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Get LocalIndices"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  allocate(localIndices1(localCount,2))
  ! get localindices
  call ESMF_GridGet(grid, distgrid = distgrid, localArbIndex=localIndices1, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  badcount = 0
  do j=1,localCount1
     if (localindices1(j,1) .ne. localIndices(j,1)) then
	print *, "PE ", myPet,"wrong index",j, localindices(j,1), &
	localindices(j,2),",", localindices1(j,1), localindices1(j,2)
        badcount = badcount+1
	correct=.false.
     endif
  enddo
  if (badcount .ne. 0) print *, "PE", myPET, "index mismatch", badcount
  deallocate(localIndices1)
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing auto-created distgrid dimension/count"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! distgrid dimension and count
  call ESMF_DistGridGet(distgrid, delayout=delayout, dimCount=distDimCount, &
	rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  allocate(minIndex(distDimCount,1), maxIndex(distDimCount,1))
  call ESMF_DistGridGet(distgrid, delayout=delayout, dimCount=distDimCount, &
	minIndexPDimPPatch=minIndex, &
	maxIndexPDimPPatch=maxIndex, &
	elementCountPPatch=localCount2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DELayoutGet(delayout, deCount=deCount, localdeList=deList, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  allocate(localCount3(deCount))
  call ESMF_DistGridGet(distgrid,  elementCountPDe=localCount3, &
	rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  ! print *, "distgrid localDE:", deList(1)+1, "localArbIndexCount: ", localCount3(deList(1)+1)

  ! distgrid index
  allocate(local1Dindices(localCount))
  call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=local1Dindices, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  if (localCount2(1) .ne. total) correct=.false.
  if (localCount3(deList(1)+1) .ne. localCount) correct = .false.
  if (distDimCount .ne. 1) correct=.false.
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  deallocate(minIndex, maxIndex, localCount3, local1Dindices)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing ESMF_GridConvertIndex"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS
 
  ! test ESMF_GridConvertIndex()
  index(1) = localIndices(1,1)
  index(2) = localIndices(1,2) 
  call ESMF_GridConvertIndex(grid,gridindex=index, distgridindex=index1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc = ESMF_FAILURE
  if (index1(1) .ne. 1) correct = .false.
  if (index1(1) .ne. 1) print *, "index1", index1
  index(1) = localIndices(localCount,1)
  index(2) = localIndices(localCount,2) 
  call ESMF_GridConvertIndex(grid,gridindex=index, distgridindex=index2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc = ESMF_FAILURE
  if (index2(1) .ne. localCount) correct = .false.
  if (index2(1) .ne. localCount) print *, "index2", index2
  ! if index does not exist, it should not return ESMF_SUCCESS
  index(1) = index(1)+1
  index(2) = index(2)+1
  call ESMF_GridConvertIndex(grid,gridindex=index, distgridindex=index1, rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing GridItem Set/Get"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_I4, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create Array 
  array1D=ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_MASK, &
               array=array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_MASK, &
               array=array1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array1, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 1) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_I4) correct=.false. 

  call ESMF_ArrayDestroy(array1D,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing Grid Destroy"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  !! Check that validate returns true
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Test Set 2:  3D Arbitrary Grid with 2 dimensions distributed, 1 dimension undistributed
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !-----------------------------------------------------------------------------
  ! First grid, using default setting
  !------------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with one undistributed dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  zdim = 4

  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, ydim, zdim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, &
	rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! allocate coordinate array
  call ESMF_GridAddCoord(grid, rc=localrc)

  call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, fptr=fptr1D, &
	computationalLBound=lowbound, computationalUBound=upbound, rc=localrc)

  do i=lowbound(1),upbound(1)
    fptr1D(i) = (localIndices(i,1)-1)*(360/xdim)
  enddo

  call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
	computationalLbound=lowbound, computationalUBound=upbound, rc=localrc)

  call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, fptr=fptr1D, rc=localrc)
  do i=lowbound(1),upbound(1)
    fptr1D(i) = (localIndices(i,2)-1)*(180/ydim)-90.0
  enddo

  call ESMF_GridGetCoord(grid, localDE=0, coordDim=3, &
	computationalLbound=lowbound, computationalUBound=upbound, rc=localrc)

  call ESMF_GridGetCoord(grid, localDE=0, coordDim=3, fptr=fptr1D, rc=localrc)
  do i=lowbound(1),upbound(1)
    fptr1D(i) = i*100
  enddo

  ! Get Coord From Array
  call ESMF_GridGetCoord(grid, coordDim=1,&
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array1, rank=rank, typekind=typekind, rc=localrc)
   !call ESMF_ArrayPrint(array1, rc=localrc)
  call ESMF_ArrayGet(array1, localDe=0, farrayPtr=fptr1D, rc=localrc)
  ! fill the coordinate values
   do i=1,localcount
    if (fptr1D(i) .ne. (localIndices(i,1)-1)*(360/xdim)) then
	print *, "array content don't match", fptr1D(i)
	correct = .false.
    endif
  enddo

  call ESMF_GridGetCoord(grid, coordDim=3,&
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, localDe=0, farrayPtr=fptr1D, rc=localrc)
  ! fill the coordinate values
   do i=lowbound(1),upbound(1)
    if (fptr1D(i) .ne. 100*i) then
	print *, "array content don't match", fptr1D(i)
	correct = .false.
    endif
  enddo

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 1) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing GridGetArrayInfo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Get array info
  call ESMF_GridGetArrayInfo(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToFieldMap=(/2,1,3/), ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
          distgridToArrayMap=distgridToArrayMap, &
          undistLBound=undistLBound, undistUBound=undistUBound, &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!  print *, 'distgridToArrayMap', distgridToArrayMap
!  print *, 'undistbounds', undistLBound, undistUBound
!  print *, 'computationaledgewidths', celw, ceuw

  if ((undistLBound(1) .ne. 1) .or. (undistUBound(1) .ne. 10)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing GridItem Set/Get"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_I4, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create Array 
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_MASK, &
               array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_MASK, &
               array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_I4) correct=.false. 

  ! destroy array
  call ESMF_ArrayDestroy(array2D,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing GridItem using ESMF_GridAddItem"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Add Item to staggerloc_corner should return failure
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_FRAC, &
               rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add item to stagger_center
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_FRAC, &
               rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Item Into Array
  call ESMF_GridGetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_FRAC, &
               array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, &
         distgridToArrayMap=distgridToArrayMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that array info is as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 
  if (distgridToArrayMap(1) .ne. 1)  correct=.false. 
  if (distgridToArrayMap(2) .ne. 2)  correct=.false. 


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  
  !-----------------------------------------------------------------------------
  ! Second grid, the undistributed coord array is 2D
  !------------------------------------------------------------------------------

  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, ydim,zdim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, coordDep3=(/ESMF_GRID_ARBDIM,3/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

	
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid Validate"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid Get"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! get decomp type
  call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, distgrid=distgrid, dimCount=dimCount, &
	 coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, &
	 coordDimMap=coordDimMap, &
         indexflag=indexflag, localArbIndexCount=localCount1, &
	 memDimCount=memDimCount, arbDimCount=arbDimCount, &
         rc=localrc)

   !print *, "PE ", myPet, "localCount=", localCount1, "coorddimCount=",coordDimCount
   !print *, "PE ", myPet, "distgridToGridMap=", distgridToGridMap, "coorddimMap=",coordDimMap
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DistGridGet(distgrid, dimCount=distDimCount, elementCountPDe=elementCounts,rc=localrc)
  !print *, "PE ", myPet, "distgridcount=", distDimCount, "elements", elementCounts
  ! get minIndex and maxIndex
  allocate(minIndex1(dimCount), maxIndex1(dimCount))
  call ESMF_GridGet(grid, ESMF_STAGGERLOC_CENTER, minIndex = minIndex1, maxIndex=maxIndex1, rc=localrc)
  ! print *, "minIndex:",  minIndex1, maxIndex1
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  if (decompType .ne. ESMF_GRID_ARBITRARY) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 3) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  if (localCount .ne. localCount1) correct = .false.
  if (distDimCount .ne. 2) correct = .false.
  if (memDimCount .ne. 2) correct=.false.
  if (arbDimCount .ne. 2) correct=.false.
  if (elementCounts(myPet+1) .ne. localcount * zdim) then
	correct = .false.
	print *, "PE ", myPet, "elements", elementCounts, localcount
  endif
  if ((minIndex1(1) .ne. 1) .and. (minIndex1(2) .ne. 1) .and. (minIndex1(3).ne.1)) correct = .false. 
  if ((maxIndex1(1) .ne. xdim) .and. (maxIndex1(2) .ne. ydim) .and. &
	(maxIndex1(3) .ne. zdim)) correct = .false. 

  deallocate(minIndex1, maxIndex1)

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Get LocalIndices"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  allocate(localIndices1(localCount,2))
  ! get localindices
  call ESMF_GridGet(grid, distgrid = distgrid, localArbIndex=localIndices1, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  badcount = 0
  do j=1,localCount1
     if (localindices1(j,1) .ne. localIndices(j,1)) then
	print *, "PE ", myPet,"wrong index",j, localindices(j,1), &
	localindices(j,2),",", localindices1(j,1), localindices1(j,2)
        badcount = badcount+1
	correct=.false.
     endif
  enddo
  if (badcount .ne. 0) print *, "PE", myPET, "index mismatch", badcount
  deallocate(localIndices1)

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing auto-created distgrid dimension/count"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! distgrid dimension and count
  call ESMF_DistGridGet(distgrid, delayout=delayout, dimCount=distDimCount, &
	rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  allocate(minIndex(distDimCount,1), maxIndex(distDimCount,1))
  call ESMF_DistGridGet(distgrid, delayout=delayout, dimCount=distDimCount, &
	minIndexPDimPPatch=minIndex, &
	maxIndexPDimPPatch=maxIndex, &
	elementCountPPatch=localCount2, rc=localrc)

  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DELayoutGet(delayout, deCount=deCount, localdeList=deList, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  allocate(localCount3(deCount))
  call ESMF_DistGridGet(distgrid,  elementCountPDe=localCount3, &
	rc=localrc)
  if (localrc .ne. ESMF_SUCCESS )call ESMF_Finalize(terminationflag=ESMF_ABORT)
  !print *, "distgrid localcount:", deList(1)+1, localCount3(deList(1)+1),localCount2(1)

  ! distgrid index
  allocate(local1Dindices(localCount))
  call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=local1Dindices, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  if (localCount2(1) .ne. total*zdim) correct=.false.
  if (localCount3(deList(1)+1) .ne. localCount*zdim) correct = .false.
  if (distDimCount .ne. 2) correct=.false.
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  deallocate(minIndex, maxIndex, localCount3)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing ESMF_GridAddCoord/ESMF_GridGetCoord"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! allocate coordinate array
  call ESMF_GridAddCoord(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get 1st dimension coordinate array and set its values
  ! call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, fptr=dimarray, rc=localrc)
  call ESMF_GridGetCoord(grid, coordDim=1, array=myarray, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

  call ESMF_ArrayGet(myarray, localDE=0, farrayPtr=dimarray, rc=localrc)


  ! fill the coordinate values
  do i=1,localcount
    dimarray(i) = localIndices(i,1)*ydim + localIndices(i,2) 
  enddo


  ! get 2nd coordinate array and set its values
  call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, fptr=dimarray, &
	rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

  ! fill the coordinate values
  do i=1,localcount
    dimarray(i) = localIndices(i,2)*xdim + localIndices(i,1) 
  enddo

  ! Spot check the coordinate values
  index3(1)=localIndices(10,1)
  index3(2)=localIndices(10,2)
  index3(3)=2
  call ESMF_GridGetIndCoord(grid, localDE=0, index=index3, coord=coord3, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

  if (coord3(1) .ne. index3(1)*ydim+index3(2)) correct=.false.
  if (coord3(2) .ne. index3(2)*xdim+index3(1)) correct=.false.
  if (coord3(3) .ne. 0) correct=.false.
  if (.not. correct) write(failMsg, *) "ESMF_GridGetIndCoord return wrong values"
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  ! Test getting the 3rd coordinate array
  !NEX_UTest
  write(name, *) "Testing GridGetCoord for non-arbitrary dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_GridGetCoord(grid, localDE=0, coordDim=3, &
        fptr=fptr2D, &
	computationalLBound=lowbound, computationalUBound=upbound, &
	rc=localrc)
  !print *, 'A coorddim 3 bound', lowbound(1), lowbound(2), upbound(1), upbound(2)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 
  ! check the computational lower bound and upper bound
  if ((lowbound(1) .ne. 1) .or. (lowbound(2) .ne. 1)) then
	write(failMsg, *) "computational lower bound is wrong"
	correct = .false.
  endif
  if ((upbound(1) .ne. localcount) .or. (upbound(2) .ne. zdim)) then
	write(failMsg, *) "computational upper bound is wrong"
	correct = .false.
  endif

  if ((size(fptr2D,1) .ne. localcount) .and. (size(fptr2D,2) .ne. zdim)) then
	write(failMsg, *) "coord array dimension is wrong"
	correct = .false.
  endif

  ! fill the coordinate values
  do i=1,localcount
    do j=1,zdim
       fptr2D(i,j) = localIndices(i,1)+localIndices(i,2)+j
    enddo 
  enddo

  ! Spot check the coordinate values
  index3(1)=localIndices(20,1)
  index3(2)=localIndices(20,2)
  index3(3)=3

  call ESMF_GridGetIndCoord(grid, localDE=0, index=index3, coord=coord3, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 
  !print *, 'GridGetIndCoord', index3, coord3 

  if (coord3(1) .ne. index3(1)*ydim+index3(2)) correct=.false.
  if (coord3(2) .ne. index3(2)*xdim+index3(1)) correct=.false.
  if (coord3(3) .ne. index3(1)+index3(2)+index3(3)) correct=.false.
  if (.not. correct) write(failMsg, *) "ESMF_GridGetIndCoord return wrong values"

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing ESMF_GridConvertIndex"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS
 
  ! test ESMF_GridConvertIndex()
  index3(1) = localIndices(10,1)
  index3(2) = localIndices(10,2) 
  index3(3) = 2
  call ESMF_GridConvertIndex(grid,gridindex=index3, distgridindex=index, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc = ESMF_FAILURE

  if (index(1) .ne. 10) correct = .false.
  if (index(2) .ne. 2) correct = .false.

  ! if index does not exist, it should not return ESMF_SUCCESS
  index3(1) = localIndices(localCount,1)+1
  index3(2) = localIndices(localCount,2)+1
  index3(3) = 3 
  index(1) = index(1)+1
  index(2) = index(2)+1
  call ESMF_GridConvertIndex(grid,gridindex=index3, distgridindex=index, rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  ! Third grid, using a non-default distDim
  !------------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "3D Arb Grid: Creating a Grid with non-default distDim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  zdim = 4
  
  ! switch ydim and zdim and set distDim to make xdim and ydim distributed
  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, zdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, distDim=(/1,3/), &
	rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid, &
	distgridToGridMap = distgridToGridMap, rc=localrc)

  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  ! check that output is as expected
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 3)) correct = .false.

  ! allocate coordinate array and set values
  call ESMF_GridAddCoord(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get rid of the grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Setting the gridCoord using ESMF_GridSetCoord"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  ! switch ydim and zdim and set distDim to make xdim and ydim distributed
  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, zdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, distDim=(/1,3/), &
	rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid, &
	distgridToGridMap = distgridToGridMap, rc=localrc)

  ! Set Coordinate using GridSetCoord
  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  array1D = ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	   distgridToArrayMap=(/1,0/), rc=localrc);

  
  call ESMF_GridSetCoord(grid,coordDim=1, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetCoord(grid,coordDim=1,&
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array1, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  if (rank .ne. 1) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  ! Get rid of the grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! destroy array
  call ESMF_ArrayDestroy(array1D,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  ! set the coord for the undistributed dim
  !NEX_UTest
  write(name, *) "Setting the gridCoord for the undistributed dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  ! switch ydim and zdim and set distDim to make xdim and ydim distributed
  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, zdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, distDim=(/1,3/), &
	rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid, &
	distgridToGridMap = distgridToGridMap, rc=localrc)

  ! Set the array for the undistributed dimension
  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  array1D = ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	     distgridToArrayMap=(/0,1/), rc=localrc);

  array1D_1 = ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	   distgridToArrayMap=(/1,0/), rc=localrc);

  array1D_2 = ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	   distgridToArrayMap=(/1,0/), rc=localrc);


  ! set coordinate array for the undistributed dimension
  call ESMF_ArrayGet(array1D, 0, farrayptr=fptr1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! regular z levels
  do j=1,zdim
      fptr1D(j)=j*100
  enddo 


  call ESMF_GridSetCoord(grid,coordDim=2, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Set other coordinate arrays
  call ESMF_GridSetCoord(grid,coordDim=1, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1D_1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridSetCoord(grid,coordDim=3, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1D_2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get Coord From Array
  call ESMF_GridGetCoord(grid,coordDim=2,&
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array1, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  if (rank .ne. 1) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  ! Check coordinate array values
  index3(1)=localIndices(1,1)
  index3(2)=2
  index3(3)=localIndices(1,2)
  call ESMF_GridGetIndCoord(grid, localDE=0, index=index3, coord=coord3, rc=localrc)
  !print *,"PET", myPet, "index:", index3, "coord", coord3
  if (coord3(2) .ne. 200) correct = .false.
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy arrays
  call ESMF_ArrayDestroy(array1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_ArrayDestroy(array1D_1,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_ArrayDestroy(array1D_2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  ! Fourth grid, using GridCreateEmpty() and GridSetCommitShapeTile() to create a 2D arb. grid
  !------------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create an empty grid then set the values"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  grid = ESMF_GridCreateEmpty(rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridSetCommitShapeTile(grid, name="arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Get LocalIndices"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  allocate(localIndices1(localCount,2))
  ! get localindices
  call ESMF_GridGet(grid, distgrid = distgrid, localArbIndex=localIndices1, &
         localArbIndexCount=localCount1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  if (localCount1 .ne. localCount) correct=.false.

  badcount = 0
  do j=1,localCount1
     if (localindices1(j,1) .ne. localIndices(j,1)) then
	print *, "PE ", myPet,"wrong index",j, localindices(j,1), &
	localindices(j,2),",", localindices1(j,1), localindices1(j,2)
        badcount = badcount+1
	correct=.false.
     endif
  enddo
 
  if (badcount > 0)  print *, "PE", myPET, "index mismatch", badcount
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Fifth Grid: Use GridCreateEmpty() and GridSetCommitShapeTile() to create grid with undistributed dimension
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create an empty grid then set the values with undistributed dimension and non-default distdim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  grid = ESMF_GridCreateEmpty(rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridSetCommitShapeTile(grid, name="arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, zdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, &
	distDim=(/1,3/), coordDep2=(/ESMF_GRID_ARBDIM, 2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid, &
	distgridToGridMap = distgridToGridMap, rc=localrc)

  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  ! check that output is as expected
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 3)) correct = .false.
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  ! set the coord for the undistributed dim
  !NEX_UTest
  write(name, *) "Setting the gridCoord for the undistributed dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  ! Set the array for the undistributed dimension
  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  array2D = ESMF_ArrayCreate(arrayspec2D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	    rc=localrc)

  ! get the dimension of the array
  call ESMF_ArrayGet(array2D, 0, larray=larray, rc=localrc)
  call ESMF_LocalArrayGet(larray, lbounds=lowbound, ubounds=upbound, &
	    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set coordinate array for the undistributed dimension
  call ESMF_ArrayGet(array2D, 0, farrayptr=fptr2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! regular z levels
  do i=lowbound(1),upbound(1)
    do j=lowbound(2),upbound(2)
      fptr2D(i,j)=j*100
    enddo
  enddo 
 
  ! Create temporary arrays for the other two coordinate dimensions
  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  array1D_1 = ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	   distgridToArrayMap=(/1,0/), rc=localrc);

  array1D_2 = ESMF_ArrayCreate(arrayspec1D, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
	   distgridToArrayMap=(/1,0/), rc=localrc);

  ! Set coordinate Arrays
  call ESMF_GridSetCoord(grid,coordDim=1, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1D_1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridSetCoord(grid,coordDim=2, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridSetCoord(grid,coordDim=3, &
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1D_2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get Coord From Array
  call ESMF_GridGetCoord(grid,coordDim=2,&
               staggerloc=ESMF_STAGGERLOC_CENTER, array=array1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array1, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  ! Check coordinate array values
  index3(1)=localIndices(1,1)
  index3(2)=2
  index3(3)=localIndices(1,2)
  call ESMF_GridGetIndCoord(grid, localDE=0,index=index3, coord=coord3, rc=localrc)
  !print *,"PET", myPet, "index:", index3, "coord", coord3
  if (coord3(2) .ne. 200) correct = .false.
 
  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! destroy arrays
  call ESMF_ArrayDestroy(array1D_1,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_ArrayDestroy(array2D,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_ArrayDestroy(array1D_2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  ! Seventh grid, Create a Grid from a Dist Grid with one undistributed dimension
  !------------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridCreate()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  distgrid = ESMF_DistGridCreate(local1Dindices, 1, minIndex=(/1/), maxIndex=(/zdim/), rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! bugs in ESMF_DistGrid.F90, need to pass localcount as work around
  allocate(indexArray(2,3))
  indexArray(1,:)=1
  indexArray(2,1)=xdim
  indexArray(2,2)=ydim
  indexArray(2,3)=zdim
  grid = ESMF_GridCreate(distgrid=distgrid, indexArray=indexArray, &
			rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  call ESMF_DistGridDestroy(distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Seventh grid, Create a Grid from a Dist Grid with one undistributed dimension and non-default distDim
  !------------------------------------------------------------------------------
  ! Create grid using ESMF_GridCreate()
  !NEX_UTest
  write(name, *) "Test ESMF_GridCreate() with distgridtogridmap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  distgrid = ESMF_DistGridCreate(local1Dindices, 1, minIndex=(/1/), maxIndex=(/zdim/), rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  distDim(1)=1
  distDim(2)=3
  indexArray(2,1)=xdim
  indexArray(2,2)=zdim
  indexArray(2,3)=ydim
  ! bugs in ESMF_DistGrid.F90, need to pass localcount as work around
  grid = ESMF_GridCreate(distgrid=distgrid, indexArray=indexArray, &
			distDim=distDim, &
			rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  call ESMF_DistGridDestroy(distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  deallocate(indexArray)
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !----------------------------------------------------------------------------=
  ! Test Set 8:  2D Arbitrary Grid with one PET localArbIndexCount = 0
  !-----------------------------------------------------------------------------
  if (myPet .eq. petCount-1) then
	localCount = 0
	deallocate(localIndices)
	allocate(localIndices(localCount,2))
  endif		
  grid = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "2D Arb Grid: Testing Grid Validate"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  !! Destroy grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  

  !-----------------------------------------------------------------------------
  ! Test set 9: Use GridCreateEmpty() and GridSetCommitShapeTile() to create grid with undistributed dimension
  ! with one PET with 0 elements
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create an empty grid then set the values with undistributed dimension and non-default distdim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  grid = ESMF_GridCreateEmpty(rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridSetCommitShapeTile(grid, name="arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, zdim, ydim/), &
	localArbIndex=localIndices,localArbIndexCount=localCount, &
	distDim=(/1,3/), coordDep2=(/ESMF_GRID_ARBDIM, 2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid, &
	distgridToGridMap = distgridToGridMap, rc=localrc)

  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  ! check that output is as expected
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 3)) correct = .false.

  !! Destroy grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  deallocate(localIndices)
  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridArbitraryUTest
