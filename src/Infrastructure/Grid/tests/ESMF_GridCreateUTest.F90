! $Id: ESMF_GridCreateUTest.F90,v 1.67.2.9 2008/05/05 16:42:27 oehmke Exp $
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
!
program ESMF_GridCreateUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridCreateTest - Check Grid Create Routines
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
    '$Id: ESMF_GridCreateUTest.F90,v 1.67.2.9 2008/05/05 16:42:27 oehmke Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount
  logical :: correct
  type(ESMF_TypeKind) :: typekind

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name, grid_name

  type(ESMF_Grid) :: grid, grid2
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid,distgrid2
  type(ESMF_Array) :: array
  integer :: coordDimMap(2,2), dimCount, undistLBound(3), undistUBound(3)
  type(ESMF_IndexFlag) :: indexflag
  integer :: distgridToGridMap(2), coordDimCount(2)
  integer :: distgridToArrayMap(3)
  integer :: coordDimCount2(3),coordDimMap2(3,3)
  integer :: gridEdgeLWidth(3),gridEdgeUWidth(3),gridAlign(3)
  integer :: exlbnd(3),exubnd(3)
  integer :: clbnd(3),cubnd(3)
  integer(ESMF_KIND_I4), pointer :: buf(:)
  integer :: bufCount, offset, localDECount, rank

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! prepare DistGrid
  distgrid=ESMF_DistGridCreate(minIndex=(/1,1/),maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Grid Validate"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! First make sure validate fails for an uncreated Grid
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) correct=.false.

  ! Now make sure that a created grid validates successfully
  !! Create Grid
  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !! Check that validate returns true
  call ESMF_GridValidate(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
  
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with only a distgrid and the rest defaults"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc=ESMF_SUCCESS

  ! create a grid with all defaults
  grid=ESMF_GridCreate(distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
         indexflag=indexflag, &
         gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  !TODO: what to do about undistLBound and undistUBound
  if ((coordDimCount(1) .ne. 2) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.
  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 1)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. -1)) correct=.false. 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroying a Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  grid_name="GRID"
  write(name, *) "Creating a Grid with a non-default name"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(name=grid_name, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  grid_name="NOT_GRID"
  call ESMF_GridGet(grid, name=grid_name, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (grid_name .ne. "GRID") correct=.false.
  
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordTypeKind"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid,coordTypeKind=typekind,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_I4) correct=.false.
  
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDimCount"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, coordDimCount=(/1,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((coordDimCount(1) .ne. 1) .or. (coordDimCount(2) .ne. 2)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDimMap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  coordDimMap(1,:)=(/1,0/)
  coordDimMap(2,:)=(/2,1/)
  grid=ESMF_GridCreate(distgrid=distgrid, coordDimCount=(/1,2/), &
                                   coordDimMap=coordDimMap,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount, coordDimMap=coordDimMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((coordDimCount(1) .ne. 1) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if (coordDimMap(1,1) .ne. 1) correct=.false.
  if ((coordDimMap(2,1) .ne. 2) .or. (coordDimMap(2,2) .ne. 1)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default distgridToGridMap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, distgridToGridMap=(/1,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgridToGridMap=distgridToGridMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default indexflag"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, indexflag=indexflag, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
!  CAN"T COMPARE INDEX FLAGS RIGHT NOW. 
!  if (indexflag .ne. ESMF_INDEX_GLOBAL) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default EdgeWidths and Aligns"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreate(distgrid=distgrid, gridEdgeLWidth=(/1,0/), &
         gridEdgeUWidth=(/0,0/), gridAlign=(/1,-1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((gridEdgeLWidth(1) .ne. 1) .or. (gridEdgeLWidth(2) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 0) .or. (gridEdgeUWidth(2) .ne. 0)) correct=.false. 
  if ((gridAlign(1) .ne. 1) .or. (gridAlign(2) .ne. -1)) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------




  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D distributed Grid with CreateShapeTileIrreg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 3D distributed Grid with CreateShapeTileIrreg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    countsPerDeDim3=(/6,8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 1 DE 3D  Grid with CreateShapeTileIrreg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test GetGridStatus"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  correct=.true.

  ! make sure uncreated grid returns uninit
  if (ESMF_GridGetStatus(grid) /=ESMF_GRIDSTATUS_UNINIT) correct=.false. 

  ! Create empty grid
  grid=ESMF_GridCreateEmpty(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure empty grid returns not ready
  if (ESMF_GridGetStatus(grid) /=ESMF_GRIDSTATUS_NOT_READY) correct=.false. 

  ! Commit grid
  call ESMF_GridSetCommitShapeTile(grid, countsPerDEDim1=(/4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure empty grid returns not ready
  if (ESMF_GridGetStatus(grid) /=ESMF_GRIDSTATUS_SHAPE_READY) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 1 DE 3D  Grid with SetCommitShapeTileIrreg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateEmpty(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridSetCommitShapeTile(grid, countsPerDEDim1=(/4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D distributed Grid with 1 non-distributed dim. with CreateShapeTileIrreg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    countsPerDeDim3=(/6/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDeps"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(minIndex=(/1,2,3/), countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/6,8/), &
                                    coordDep1=(/1/), &
                                    coordDep2=(/3,1,2/), &
                                    coordDep3=(/2,1/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount2, coordDimMap=coordDimMap2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.

  if ((coordDimCount2(1) .ne. 1) .or. (coordDimCount2(2) .ne. 3) .or.  &
      (coordDimCount2(3) .ne. 2)) correct=.false.

  if (coordDimMap2(1,1) .ne. 1) correct=.false.
  if ((coordDimMap2(2,1) .ne. 3) .or. (coordDimMap2(2,2) .ne. 1) .or. &
      (coordDimMap2(2,3) .ne. 2)) correct=.false.
  if ((coordDimMap2(3,1) .ne. 2) .or. (coordDimMap2(3,2) .ne. 1)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D distributed Grid with 1 undistributed dim. with CreateShapeTileIrreg and no-default gridEdgeWidths"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/3,4,5/), &
                                    countsPerDeDim3=(/6/), &
                                    gridEdgeLWidth=(/0,1,0/), &
                                    gridEdgeUWidth=(/1,0,0/), &
                                    gridAlign=(/-1,1,1/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 1) .or. &
      (gridEdgeLWidth(3) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 0) .or. & 
      (gridEdgeUWidth(3) .ne. 0)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. 1) .or. &
      (gridAlign(3) .ne. 1)) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with default coordDeps"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(minIndex=(/1,2,3/), countsPerDEDim1=(/1,2,3,4/), &
                                    countsPerDeDim2=(/5/), &
                                    countsPerDeDim3=(/6,8/), &
                                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordDimCount=coordDimCount2, coordDimMap=coordDimMap2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.

  if ((coordDimCount2(1) .ne. 3) .or. (coordDimCount2(2) .ne. 3) .or.  &
      (coordDimCount2(3) .ne. 3)) correct=.false.
  if ((coordDimMap2(1,1) .ne. 1) .or. (coordDimMap2(1,2) .ne. 2) .or. &
      (coordDimMap2(1,3) .ne. 3)) correct=.false.
  if ((coordDimMap2(2,1) .ne. 1) .or. (coordDimMap2(2,2) .ne. 2) .or. &
      (coordDimMap2(2,3) .ne. 3)) correct=.false.
  if ((coordDimMap2(3,1) .ne. 1) .or. (coordDimMap2(3,2) .ne. 2) .or. &
      (coordDimMap2(3,3) .ne. 3)) correct=.false.


  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating/Destroying an Empty Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create empty grid
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateEmpty(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with all defaults with CreateShapeTileReg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(maxIndex=(/4,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with non-default 1D regDecomp with CreateShapeTileReg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(maxIndex=(/4,2/),regDecomp=(/1,2/), &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with non-default 2D regDecomp with CreateShapeTileReg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(maxIndex=(/4,2/),regDecomp=(/2,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  1 DE Grid with CreateShapeTileReg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(maxIndex=(/4,2/), regDecomp=(/1,1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with CreateShapeTileReg and non-default gridEdgeWidths"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(minIndex=(/1,2,3/), &
                                maxIndex=(/3,4,5/), &
                                gridEdgeLWidth=(/0,1,0/), &
                                gridEdgeUWidth=(/1,0,0/), &
                                gridAlign=(/-1,1,1/), &
                                rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 1) .or. &
      (gridEdgeLWidth(3) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 0) .or. & 
      (gridEdgeUWidth(3) .ne. 0)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. 1) .or. &
      (gridAlign(3) .ne. 1)) correct=.false. 

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 3D  Grid with non-default minIndex and non-default regDecomp with CreateShapeTileReg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateShapeTile(minIndex=(/1,2,3/),maxIndex=(/4,5,6/),regDecomp=(/2,1,2/), &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 3) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid using CreateEmpty/Set/Commit with only a distgrid and the rest defaults"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc=ESMF_SUCCESS

  ! create a grid with all defaults
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateEmpty(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridSet(grid=grid, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridCommit(grid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
         indexflag=indexflag, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  !TODO: what to do about undistLBound and undistUBound
  if ((coordDimCount(1) .ne. 2) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a 2D  Grid with all defaults with SetCommitShapeTileReg"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  
  ! Create Empty Grid
  grid=ESMF_GridCreateEmpty(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set information
  call ESMF_GridSetCommitShapeTile(grid, maxIndex=(/4,2/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, distgrid=distgrid2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  call ESMF_DistGridGet(distgrid2, dimCount=dimCount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (dimcount .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ArrayCreateFromGrid with only distributed bounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/),    &
                                countsPerDeDim2=(/3,4/),    &
                                indexflag=ESMF_INDEX_GLOBAL,&
                                rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToArrayMap=(/2,1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get array info and make sure its correct
  call ESMF_ArrayGet(array, distgridToArrayMap=distgridToArrayMap, &
                     rank=rank, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 2) .or. (distgridToArrayMap(2) .ne. 1)) correct=.false.
  if (rank .ne. 2) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ArrayCreateFromGrid with non-default ungriddedBounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! create grid
  grid=ESMF_GridCreateShapeTile(countsPerDEDim1=(/1,2/), &
                              countsPerDeDim2=(/5/),  & 
                              countsPerDeDim3=(/3,4/),  &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create an array on an unallocated stagger location
  array=ESMF_ArrayCreateFromGrid(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
          gridToArrayMap=(/1,4,3/), ungriddedLBound=(/2,3/), ungriddedUBound=(/7,6/), &
          rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! make sure array is valid
  call ESMF_ArrayValidate(array,rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get array info and make sure its correct
  call ESMF_ArrayGet(array,distgridToArrayMap=distgridToArrayMap, &
         undistLBound=undistLBound, undistUBound=undistUBound, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure info is as expected
  if ((distgridToArrayMap(1) .ne. 1) .or. &
      (distgridToArrayMap(2) .ne. 4) .or. &
      (distgridToArrayMap(3) .ne. 3)) correct=.false.
  if ((undistLBound(1) .ne. 2) .or. (undistLBound(2) .ne. 3)) correct=.false.
  if ((undistUBound(1) .ne. 7) .or. (undistUBound(2) .ne. 6)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grid
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Serialize and Deserialize"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc=ESMF_SUCCESS
  correct=.true.

  ! create a grid with all defaults
  grid=ESMF_GridCreate(distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! allocate coord array
  call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! create a buffer to put the grid in
  bufCount=100000
  allocate(buf(bufCount))

  ! Serialize
  offset=0
  call ESMF_GridSerialize(grid, buf, bufCount, offset, rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Deserialize
   offset=0
   grid2=ESMF_GridDeserialize(vm, buf, offset, rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of buffer
  deallocate(buf)

  ! get info from Grid
  call ESMF_GridGet(grid2, dimCount=dimCount, coordTypeKind=typekind, &
         distgridToGridMap=distgridToGridMap, coordDimCount=coordDimCount, coordDimMap=coordDimMap, &
         indexflag=indexflag, &
         gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
         gridAlign=gridAlign, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! check that values are as expected
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (dimCount .ne. 2) correct=.false.
  if ((distgridToGridMap(1) .ne. 1) .or. (distgridToGridMap(2) .ne. 2)) correct=.false.
  !TODO: what to do about undistLBound and undistUBound

  if ((coordDimCount(1) .ne. 2) .or. (coordDimCount(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.

  if ((gridEdgeLWidth(1) .ne. 0) .or. (gridEdgeLWidth(2) .ne. 0)) correct=.false. 
  if ((gridEdgeUWidth(1) .ne. 1) .or. (gridEdgeUWidth(2) .ne. 1)) correct=.false. 
  if ((gridAlign(1) .ne. -1) .or. (gridAlign(2) .ne. -1)) correct=.false. 
  if (localDECount .ne. 0) correct=.false. 

  ! Get Coord Array and see if it looks ok
  call ESMF_GridGetCoord(grid2, staggerloc=ESMF_STAGGERLOC_CENTER, &
         coordDim=1, array=array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Validate Array
  call ESMF_ArrayValidate(array, rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! destroy grids
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridDestroy(grid2,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------






  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCreateUTest
