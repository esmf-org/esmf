! $Id: ESMF_GridItemUTest.F90,v 1.7.2.1 2010/02/05 19:57:42 svasquez Exp $
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
program ESMF_GridItemUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridItemUTest - Check Grid Coordinate manipulation routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 Grid Coord unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_GridItemUTest.F90,v 1.7.2.1 2010/02/05 19:57:42 svasquez Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_TypeKind) :: typekind
  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid2D, tmpDistGrid
  type(ESMF_Array) :: array2D, array2
  type(ESMF_ArraySpec) :: arrayspec2D
  integer(ESMF_KIND_I4), pointer :: fptr(:,:), fptr3D(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptrR8(:,:,:)
  real(ESMF_KIND_R4), pointer :: fptrR4(:,:)
  real(kind=ESMF_KIND_R4), parameter :: var=1.0
  integer :: petMap2D(2,2,1)
  integer :: rank
  integer :: lDE, localDECount
  INTEGER, PARAMETER :: globalXcount = 5 
  INTEGER, PARAMETER :: globalYcount = 5 
  integer :: distgridToArrayMap(2)


  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  ! prepare 2D DistGrid
  distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_I4, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Item from Array"
  write(failMsg, *) "Incorrect result"

  ! init flags
  correct=.true.
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get distgrid
  call ESMF_GridGet(grid, ESMF_STAGGERLOC_CORNER, staggerdistgrid=tmpDistGrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array 
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=tmpDistgrid, &
                           indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
               array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
               array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_I4) correct=.false. 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Add Item from Array check distgridToArrayMap matching"
  write(failMsg, *) "Incorrect result"

  ! init flags
  correct=.true.
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid=ESMF_GridCreate(distgrid=distgrid2D, distgridToGridMap=(/2,1/), &
             rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add Item
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
               rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Item Into Array
  call ESMF_GridGetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
               array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, &
         distgridToArrayMap=distgridToArrayMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_I4) correct=.false. 
  if (distgridToArrayMap(1) .ne. 2)  correct=.false. 
  if (distgridToArrayMap(2) .ne. 1)  correct=.false. 

  ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Get fortran pointer from item array"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get distgrid
  call ESMF_GridGet(grid, ESMF_STAGGERLOC_CORNER, staggerdistgrid=tmpDistGrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array 
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=tmpDistgrid, &
            indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Set Coord From Array
  call ESMF_GridSetItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
                array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(fptr)

  ! Get Coord From Grid
  call ESMF_GridGetItem(grid, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
            fptr=fptr, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Check that output is as expected
  correct=.true.
  if (.not. associated(fptr)) correct=.false.

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test item value and type error checking"
  write(failMsg, *) "Incorrect result"

  ! init flags
  rc=ESMF_SUCCESS
  correct=.true.


  ! create 2D test Grid
  grid=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add Item to bad item value
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_INVALID, &
               rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false.

  ! Add item with wrong type
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CORNER, item=ESMF_GRIDITEM_MASK, &
               itemTypeKind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false.


 ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridGetItem getting bounds from grid item"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              gridEdgeLWidth=(/1,2,3/), &
                              gridEdgeUWidth=(/4,5,6/), &  
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate coordinates
  call ESMF_GridAddItem(grid, staggerEdgeLWidth=(/1,2,3/), staggerEdgeUWidth=(/4,5,6/), &
               staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check allocated staggerloc bounds
  call check2DP1Bnds2x2UsingSLoc(grid, staggerloc=ESMF_STAGGERLOC_EDGE2_VFACE, &
           item=ESMF_GRIDITEM_MASK, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 

  ! check unallocated staggerloc bounds
  call check2DP1Bnds2x2UsingSLoc(grid, staggerloc=ESMF_STAGGERLOC_CORNER_VFACE, &
           item=ESMF_GRIDITEM_MASK, &
           localPet=localPet, petCount=petCount,                          &
           ielbnd0=(/1,1,1/),ieubnd0=(/1,3,5/),iloff0=(/1,2,3/),iuoff0=(/0,0,6/), &
           ielbnd1=(/2,1,1/),ieubnd1=(/3,3,5/),iloff1=(/0,2,3/),iuoff1=(/4,0,6/), &
           ielbnd2=(/1,4,1/),ieubnd2=(/1,7,5/),iloff2=(/1,0,3/),iuoff2=(/0,5,6/), &
           ielbnd3=(/2,4,1/),ieubnd3=(/3,7,5/),iloff3=(/0,0,3/),iuoff3=(/4,5,6/), &
           correct=correct, rc=rc) 


 ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridGetItem getting R8 pointer"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Add Item
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get number of localDEs
  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! loop through localDEs
  do lDE=0,localDECount-1
     ! init pointer 
     nullify(fptrR8) 

     ! Get Item pointer
     call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREA, &
                        localDE=lDE, fptr=fptrR8, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! check pointer
     if (.not. associated(fptrR8)) correct=.false.
  enddo

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_GridGetItem getting R4 pointer"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Add Item
  call ESMF_GridAddItem(grid, itemTypeKind=ESMF_TYPEKIND_R4, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get number of localDEs
  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! loop through localDEs
  do lDE=0,localDECount-1
     ! init pointer 
     nullify(fptrR4) 

     ! Get Item pointer
     call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREA, &
                        localDE=lDE, fptr=fptrR4, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! check pointer
     if (.not. associated(fptrR4)) correct=.false.
  enddo

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Adding and Getting each grid item"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/),  &
                              countsPerDeDim3=(/5/),  & 
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Add Items
  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREAM, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridAddItem(grid, &
               staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_FRAC, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get number of localDEs
  call ESMF_GridGet(grid, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! loop through localDEs
  do lDE=0,localDECount-1

     ! init pointer 
     nullify(fptr3D) 

     ! Get Item pointer
     call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_MASK, &
                        localDE=lDE, fptr=fptr3D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! check pointer
     if (.not. associated(fptr3D)) correct=.false.

     ! init pointer 
     nullify(fptrR8) 

     ! Get Item pointer
     call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREA, &
                        localDE=lDE, fptr=fptrR8, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! check pointer
     if (.not. associated(fptrR8)) correct=.false.

     ! init pointer 
     nullify(fptrR8) 

     ! Get Item pointer
     call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_AREAM, &
                        localDE=lDE, fptr=fptrR8, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! check pointer
     if (.not. associated(fptrR8)) correct=.false.

     ! init pointer 
     nullify(fptrR8) 

     ! Get Item pointer
     call ESMF_GridGetItem(grid, staggerloc=ESMF_STAGGERLOC_CENTER, item=ESMF_GRIDITEM_FRAC, &
                        localDE=lDE, fptr=fptrR8, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! check pointer
     if (.not. associated(fptrR8)) correct=.false.
  enddo

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  ! Destroy distgrid
  call ESMF_DistGridDestroy(distgrid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

contains



subroutine check2DP1Bnds2x2UsingSLoc(grid, staggerloc, item, localPet, petCount, &
                          ielbnd0,ieubnd0,iloff0,iuoff0, &
                          ielbnd1,ieubnd1,iloff1,iuoff1, &
                          ielbnd2,ieubnd2,iloff2,iuoff2, &
                          ielbnd3,ieubnd3,iloff3,iuoff3, &
                          correct, rc)

  type (ESMF_Grid) :: grid
  type (ESMF_StaggerLoc),intent(in) :: staggerloc
  type (ESMF_GridItem), intent(in) :: item
  integer,intent(in) :: localPet, petCount
  integer,intent(in) :: ielbnd0(:),ieubnd0(:),iloff0(:),iuoff0(:)
  integer,intent(in) :: ielbnd1(:),ieubnd1(:),iloff1(:),iuoff1(:)
  integer,intent(in) :: ielbnd2(:),ieubnd2(:),iloff2(:),iuoff2(:)
  integer,intent(in) :: ielbnd3(:),ieubnd3(:),iloff3(:),iuoff3(:)
  logical,intent(inout) :: correct
  integer,intent(inout) :: rc
  
  integer :: localrc
  integer :: elbnd(3),eubnd(3),ecnt(3)
  integer :: clbnd(3),cubnd(3),ccnt(3)

  ! Check if bounds are correct for each DE
  if (petCount .eq. 1) then
      ! Note the order of DE's here is dependant on the ordering
      ! in ESMF_GridCreateShapeTile, if that changes then this will
      ! probably have to change also. 

      ! check DE 0
      call ESMF_GridGetItem(grid, localDE=0, &
             staggerLoc=staggerloc, item=item,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd, exclusiveCount=ecnt,       &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!   write(*,*) "0:",clbnd,",",cubnd, correct


     if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

 
      ! check DE 1
      call ESMF_GridGetItem(grid, localDE=1, &
             staggerLoc=staggerloc, item=item,                 &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,     &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "1:",clbnd,",",cubnd, correct

     if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

      ! check DE 2
      call ESMF_GridGetItem(grid, localDE=2, &
             staggerLoc=staggerloc, item=item,                 &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd, exclusiveCount=ecnt,   &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!   write(*,*) "2:",clbnd,",",cubnd, correct


     if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

      ! check DE 3
      call ESMF_GridGetItem(grid, localDE=3, &
             staggerLoc=staggerloc, item=item,                 &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
!    write(*,*) "3:",clbnd,",",cubnd, correct

     if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (elbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
     if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (eubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
     if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
     if (ecnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

     if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
     if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
     if (clbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
     if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
     if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
     if (cubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
     if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
     if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
     if (ccnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

  else  if (petCount .eq. 4) then
      call ESMF_GridGetItem(grid, localDE=0, &
             staggerLoc=staggerloc, item=item,                 &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,  exclusiveCount=ecnt,      &
             computationalLBound=clbnd, computationalUBound=cubnd, computationalCount=ccnt, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     if (localPet .eq. 0) then
        if (elbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd0(1)-iloff0(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd0(2)-iloff0(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd0(3)-iloff0(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd0(1)+iuoff0(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd0(2)+iuoff0(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd0(3)+iuoff0(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd0(1)-ielbnd0(1)+1+iloff0(1)+iuoff0(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd0(2)-ielbnd0(2)+1+iloff0(2)+iuoff0(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd0(3)-ielbnd0(3)+1+iloff0(3)+iuoff0(3)) correct=.false.
     else if (localPet .eq. 1) then
        if (elbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd1(1)-iloff1(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd1(2)-iloff1(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd1(3)-iloff1(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd1(1)+iuoff1(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd1(2)+iuoff1(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd1(3)+iuoff1(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd1(1)-ielbnd1(1)+1+iloff1(1)+iuoff1(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd1(2)-ielbnd1(2)+1+iloff1(2)+iuoff1(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd1(3)-ielbnd1(3)+1+iloff1(3)+iuoff1(3)) correct=.false.
     else if (localPet .eq. 2) then

        if (elbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd2(1)-iloff2(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd2(2)-iloff2(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd2(3)-iloff2(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd2(1)+iuoff2(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd2(2)+iuoff2(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd2(3)+iuoff2(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd2(1)-ielbnd2(1)+1+iloff2(1)+iuoff2(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd2(2)-ielbnd2(2)+1+iloff2(2)+iuoff2(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd2(3)-ielbnd2(3)+1+iloff2(3)+iuoff2(3)) correct=.false.
     else if (localPet .eq. 3) then
        if (elbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (elbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (elbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
        if (eubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (eubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (eubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
        if (ecnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ecnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (ecnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.

        if (clbnd(1) .ne. ielbnd3(1)-iloff3(1)) correct=.false.
        if (clbnd(2) .ne. ielbnd3(2)-iloff3(2)) correct=.false.
        if (clbnd(3) .ne. ielbnd3(3)-iloff3(3)) correct=.false.
        if (cubnd(1) .ne. ieubnd3(1)+iuoff3(1)) correct=.false.
        if (cubnd(2) .ne. ieubnd3(2)+iuoff3(2)) correct=.false.
        if (cubnd(3) .ne. ieubnd3(3)+iuoff3(3)) correct=.false.
        if (ccnt(1) .ne. ieubnd3(1)-ielbnd3(1)+1+iloff3(1)+iuoff3(1)) correct=.false.
        if (ccnt(2) .ne. ieubnd3(2)-ielbnd3(2)+1+iloff3(2)+iuoff3(2)) correct=.false.
        if (ccnt(3) .ne. ieubnd3(3)-ielbnd3(3)+1+iloff3(3)+iuoff3(3)) correct=.false.
     endif
  endif
end subroutine check2DP1Bnds2x2UsingSLoc

end program ESMF_GridItemUTest
