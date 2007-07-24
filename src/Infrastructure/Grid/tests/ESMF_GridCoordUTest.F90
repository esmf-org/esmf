! $Id: ESMF_GridCoordUTest.F90,v 1.4 2007/07/24 19:33:05 oehmke Exp $
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
!
program ESMF_GridCoordUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridCoordUTest - Check Grid Coordinate manipulation routines
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
    '$Id: ESMF_GridCoordUTest.F90,v 1.4 2007/07/24 19:33:05 oehmke Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_TypeKind) :: typekind
  type(ESMF_Grid) :: grid2D,grid2Dp1,grid3D
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid2D, distgrid3D
  integer :: rank
  type(ESMF_Array) :: array2D, array2
  type(ESMF_ArraySpec) :: arrayspec2D
  real(ESMF_KIND_R8), pointer :: fptr(:,:), outfptr(:,:), fptr3D(:,:,:)
  integer :: lbnd(2),ubnd(2),lbnd3d(3),ubnd3d(3)
  integer :: elbnd(3),eubnd(3)
  integer :: slbnd(3),subnd(3)
  integer :: clbnd(3),cubnd(3)
  integer :: tlbnd(3),tubnd(3)
  integer :: petMap2D(2,2,1)

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

  ! prepare 3D DistGrid
  distgrid3D=ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/10,10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! create 2D plus 1 undistributed test Grid
  grid2Dp1=ESMF_GridCreate(distgrid=distgrid2D, lbounds=(/1/), ubounds=(/20/), &
             coordTypeKind=ESMF_TYPEKIND_R8, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! create 3D test Grid
  grid3D=ESMF_GridCreate(distgrid=distgrid3D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Coordinates from Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array 
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=distgrid2D, computationalUWidth=(/1,1/), &
                           indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D,coord=1, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetCoord(grid2D,coord=1,&
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set Coordinates from Array, check bounds checking"
  write(failMsg, *) "Did not catch too small Array"

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array with no extra space
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=distgrid2D, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D,coord=1, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2D, rc=localrc)
  correct=.true.
  if (localrc .eq. ESMF_SUCCESS) correct=.false. ! we should have returned with an error

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Get fortran pointer from coordinate array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array with extra space
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=distgrid2D, computationalUWidth=(/1,1/), &
            indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Set Coord From Array
  call ESMF_GridSetCoord(grid2D,coord=2, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(fptr)

  ! Get Coord From Grid
  call ESMF_GridGetLocalTileCoord(grid2D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CORNER, coord=2, fptr=fptr, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(fptr)) correct=.false.

 ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D GridAllocCoord, by allocating coordinates for every stagger"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! if petCount >1, setup petMap
  if (petCount .gt. 1) then
     petMap2D(:,1,1)=(/0,1/)
     petMap2D(:,2,1)=(/2,3/)

     grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              petMap=petMap2D, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  else
     grid2D=ESMF_GridCreateShape(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  endif

  ! Allocate Staggers
  call ESMF_GridAllocCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAllocCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAllocCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_EDGE2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_GridAllocCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D CoordAlloc and GridGetLocalTileInfo, by making sure default Center bounds are as expected"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! Init correct flag
  correct=.true.

  ! Check if bounds are correct for each DE
  if (petCount .eq. 1) then
      ! Note the order of DE's here is dependant on the ordering
      ! in ESMF_GridCreateShape, if that changes then this will
      ! probably have to change also. 

      ! check DE 0
      call ESMF_GridGetLocalTileInfo(grid2D, coord=1, localDE=0, &
             staggerLoc=ESMF_STAGGERLOC_CENTER,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             staggerLBound=slbnd, staggerUBound=subnd,           &
             computationalLBound=clbnd, computationalUBound=cubnd,  &
             totalLBound=tlbnd, totalUBound=tubnd, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! set pointer to null
     nullify(fptr)

     !! Get Coord Array From Grid
     call ESMF_GridGetLocalTileCoord(grid2D, localDE=0, &
              staggerLoc=ESMF_STAGGERLOC_CENTER, coord=1, fptr=fptr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(fptr)) correct=.false.
     if ((lbound(fptr,1) .ne. tlbnd(1)) .or. (lbound(fptr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(fptr,1) .ne. tubnd(1)) .or. (ubound(fptr,2) .ne. tubnd(2))) correct=.false.
     if ((elbnd(1) .ne. 1) .or. (elbnd(2) .ne. 1)) correct=.false.
     if ((eubnd(1) .ne. 1) .or. (eubnd(2) .ne. 3)) correct=.false.
     if ((slbnd(1) .ne. 1) .or. (slbnd(2) .ne. 1)) correct=.false.
     if ((subnd(1) .ne. 1) .or. (subnd(2) .ne. 3)) correct=.false.
     if ((clbnd(1) .ne. 1) .or. (clbnd(2) .ne. 1)) correct=.false.
     if ((cubnd(1) .ne. 1) .or. (cubnd(2) .ne. 3)) correct=.false.
     if ((tlbnd(1) .ne. 1) .or. (tlbnd(2) .ne. 1)) correct=.false.
     if ((tubnd(1) .ne. 1) .or. (tubnd(2) .ne. 3)) correct=.false.

      ! check DE 1
      call ESMF_GridGetLocalTileInfo(grid2D, coord=1, localDE=1, &
             staggerLoc=ESMF_STAGGERLOC_CENTER,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             staggerLBound=slbnd, staggerUBound=subnd,           &
             computationalLBound=clbnd, computationalUBound=cubnd,  &
             totalLBound=tlbnd, totalUBound=tubnd, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! set pointer to null
     nullify(fptr)

     !! Get Coord From Grid
     call ESMF_GridGetLocalTileCoord(grid2D, localDE=1, &
              staggerLoc=ESMF_STAGGERLOC_CENTER, coord=1, fptr=fptr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(fptr)) correct=.false.
     if ((lbound(fptr,1) .ne. tlbnd(1)) .or. (lbound(fptr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(fptr,1) .ne. tubnd(1)) .or. (ubound(fptr,2) .ne. tubnd(2))) correct=.false.
     if ((elbnd(1) .ne. 2) .or. (elbnd(2) .ne. 1)) correct=.false.
     if ((eubnd(1) .ne. 3) .or. (eubnd(2) .ne. 3)) correct=.false.
     if ((slbnd(1) .ne. 2) .or. (slbnd(2) .ne. 1)) correct=.false.
     if ((subnd(1) .ne. 3) .or. (subnd(2) .ne. 3)) correct=.false.
     if ((clbnd(1) .ne. 2) .or. (clbnd(2) .ne. 1)) correct=.false.
     if ((cubnd(1) .ne. 3) .or. (cubnd(2) .ne. 3)) correct=.false.
     if ((tlbnd(1) .ne. 2) .or. (tlbnd(2) .ne. 1)) correct=.false.
     if ((tubnd(1) .ne. 3) .or. (tubnd(2) .ne. 3)) correct=.false.


      ! check DE 2
      call ESMF_GridGetLocalTileInfo(grid2D, coord=1, localDE=2, &
             staggerLoc=ESMF_STAGGERLOC_CENTER,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             staggerLBound=slbnd, staggerUBound=subnd,           &
             computationalLBound=clbnd, computationalUBound=cubnd,  &
             totalLBound=tlbnd, totalUBound=tubnd, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! set pointer to null
     nullify(fptr)

     !! Get Coord From Grid
     call ESMF_GridGetLocalTileCoord(grid2D, localDE=2, &
              staggerLoc=ESMF_STAGGERLOC_CENTER, coord=1, fptr=fptr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(fptr)) correct=.false.
     if ((lbound(fptr,1) .ne. tlbnd(1)) .or. (lbound(fptr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(fptr,1) .ne. tubnd(1)) .or. (ubound(fptr,2) .ne. tubnd(2))) correct=.false.
     if ((elbnd(1) .ne. 1) .or. (elbnd(2) .ne. 4)) correct=.false.
     if ((eubnd(1) .ne. 1) .or. (eubnd(2) .ne. 7)) correct=.false.
     if ((slbnd(1) .ne. 1) .or. (slbnd(2) .ne. 4)) correct=.false.
     if ((subnd(1) .ne. 1) .or. (subnd(2) .ne. 7)) correct=.false.
     if ((clbnd(1) .ne. 1) .or. (clbnd(2) .ne. 4)) correct=.false.
     if ((cubnd(1) .ne. 1) .or. (cubnd(2) .ne. 7)) correct=.false.
     if ((tlbnd(1) .ne. 1) .or. (tlbnd(2) .ne. 4)) correct=.false.
     if ((tubnd(1) .ne. 1) .or. (tubnd(2) .ne. 7)) correct=.false. 

      ! check DE 3
      call ESMF_GridGetLocalTileInfo(grid2D, coord=1, localDE=3, &
             staggerLoc=ESMF_STAGGERLOC_CENTER,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             staggerLBound=slbnd, staggerUBound=subnd,           &
             computationalLBound=clbnd, computationalUBound=cubnd,  &
             totalLBound=tlbnd, totalUBound=tubnd, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! set pointer to null
     nullify(fptr)

     !! Get Coord From Grid
     call ESMF_GridGetLocalTileCoord(grid2D, localDE=3, &
              staggerLoc=ESMF_STAGGERLOC_CENTER, coord=1, fptr=fptr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     !! Check that output is as expected
     if (.not. associated(fptr)) correct=.false.
     if ((lbound(fptr,1) .ne. tlbnd(1)) .or. (lbound(fptr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(fptr,1) .ne. tubnd(1)) .or. (ubound(fptr,2) .ne. tubnd(2))) correct=.false.
     if ((elbnd(1) .ne. 2) .or. (elbnd(2) .ne. 4)) correct=.false.
     if ((eubnd(1) .ne. 3) .or. (eubnd(2) .ne. 7)) correct=.false.
     if ((slbnd(1) .ne. 2) .or. (slbnd(2) .ne. 4)) correct=.false.
     if ((subnd(1) .ne. 3) .or. (subnd(2) .ne. 7)) correct=.false.
     if ((clbnd(1) .ne. 2) .or. (clbnd(2) .ne. 4)) correct=.false.
     if ((cubnd(1) .ne. 3) .or. (cubnd(2) .ne. 7)) correct=.false.
     if ((tlbnd(1) .ne. 2) .or. (tlbnd(2) .ne. 4)) correct=.false.
     if ((tubnd(1) .ne. 3) .or. (tubnd(2) .ne. 7)) correct=.false. 

  else  if (petCount .eq. 4) then
      call ESMF_GridGetLocalTileInfo(grid2D, coord=1, localDE=0, &
             staggerLoc=ESMF_STAGGERLOC_CENTER,                  &
             exclusiveLBound=elbnd, exclusiveUBound=eubnd,       &
             staggerLBound=slbnd, staggerUBound=subnd,           &
             computationalLBound=clbnd, computationalUBound=cubnd,  &
             totalLBound=tlbnd, totalUBound=tubnd, rc=localrc)
             if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! set pointer to null
      nullify(fptr)

     ! Get Coord From Grid
     call ESMF_GridGetLocalTileCoord(grid2D, localDE=0, &
              staggerLoc=ESMF_STAGGERLOC_CENTER, coord=1, fptr=fptr, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

     ! Check that output is as expected
     if (.not. associated(fptr)) correct=.false.
     if ((lbound(fptr,1) .ne. tlbnd(1)) .or. (lbound(fptr,2) .ne. tlbnd(2))) correct=.false.
     if ((ubound(fptr,1) .ne. tubnd(1)) .or. (ubound(fptr,2) .ne. tubnd(2))) correct=.false.

     if (localPet .eq. 0) then
        if ((elbnd(1) .ne. 1) .or. (elbnd(2) .ne. 1)) correct=.false.
        if ((eubnd(1) .ne. 1) .or. (eubnd(2) .ne. 3)) correct=.false.
        if ((slbnd(1) .ne. 1) .or. (slbnd(2) .ne. 1)) correct=.false.
        if ((subnd(1) .ne. 1) .or. (subnd(2) .ne. 3)) correct=.false.
        if ((clbnd(1) .ne. 1) .or. (clbnd(2) .ne. 1)) correct=.false.
        if ((cubnd(1) .ne. 1) .or. (cubnd(2) .ne. 3)) correct=.false.
        if ((tlbnd(1) .ne. 1) .or. (tlbnd(2) .ne. 1)) correct=.false.
        if ((tubnd(1) .ne. 1) .or. (tubnd(2) .ne. 3)) correct=.false.
     else if (localPet .eq. 1) then
        if ((elbnd(1) .ne. 2) .or. (elbnd(2) .ne. 1)) correct=.false.
        if ((eubnd(1) .ne. 3) .or. (eubnd(2) .ne. 3)) correct=.false.
        if ((slbnd(1) .ne. 2) .or. (slbnd(2) .ne. 1)) correct=.false.
        if ((subnd(1) .ne. 3) .or. (subnd(2) .ne. 3)) correct=.false.
        if ((clbnd(1) .ne. 2) .or. (clbnd(2) .ne. 1)) correct=.false.
        if ((cubnd(1) .ne. 3) .or. (cubnd(2) .ne. 3)) correct=.false.
        if ((tlbnd(1) .ne. 2) .or. (tlbnd(2) .ne. 1)) correct=.false.
        if ((tubnd(1) .ne. 3) .or. (tubnd(2) .ne. 3)) correct=.false.
     else if (localPet .eq. 2) then
        if ((elbnd(1) .ne. 1) .or. (elbnd(2) .ne. 4)) correct=.false.
        if ((eubnd(1) .ne. 1) .or. (eubnd(2) .ne. 7)) correct=.false.
        if ((slbnd(1) .ne. 1) .or. (slbnd(2) .ne. 4)) correct=.false.
        if ((subnd(1) .ne. 1) .or. (subnd(2) .ne. 7)) correct=.false.
        if ((clbnd(1) .ne. 1) .or. (clbnd(2) .ne. 4)) correct=.false.
        if ((cubnd(1) .ne. 1) .or. (cubnd(2) .ne. 7)) correct=.false.
        if ((tlbnd(1) .ne. 1) .or. (tlbnd(2) .ne. 4)) correct=.false.
        if ((tubnd(1) .ne. 1) .or. (tubnd(2) .ne. 7)) correct=.false. 
     else if (localPet .eq. 3) then
        if ((elbnd(1) .ne. 2) .or. (elbnd(2) .ne. 4)) correct=.false.
        if ((eubnd(1) .ne. 3) .or. (eubnd(2) .ne. 7)) correct=.false.
        if ((slbnd(1) .ne. 2) .or. (slbnd(2) .ne. 4)) correct=.false.
        if ((subnd(1) .ne. 3) .or. (subnd(2) .ne. 7)) correct=.false.
        if ((clbnd(1) .ne. 2) .or. (clbnd(2) .ne. 4)) correct=.false.
        if ((cubnd(1) .ne. 3) .or. (cubnd(2) .ne. 7)) correct=.false.
        if ((tlbnd(1) .ne. 2) .or. (tlbnd(2) .ne. 4)) correct=.false.
        if ((tubnd(1) .ne. 3) .or. (tubnd(2) .ne. 7)) correct=.false. 
     endif
  endif

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D plus 1 undistributed GridAllocCoord, by allocating coordinates and then getting fortran pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Set Coord From Array
  call ESMF_GridAllocCoord(grid2Dp1, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(fptr3D)

  ! Get Coord From Grid
  call ESMF_GridGetLocalTileCoord(grid2Dp1, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coord=2, fptr=fptr3D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(fptr3D)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 3D GridAllocCoord, by allocating coordinates and then getting fortran pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Set Coord From Array
  call ESMF_GridAllocCoord(grid3D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(fptr3D)

  ! Get Coord From Grid
  call ESMF_GridGetLocalTileCoord(grid3D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coord=2, fptr=fptr3D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(fptr3D)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  call ESMF_GridDestroy(grid2Dp1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCoordUTest
