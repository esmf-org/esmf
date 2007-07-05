! $Id: ESMF_GridCreateUTest.F90,v 1.47 2007/07/05 19:07:50 oehmke Exp $
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
    '$Id: ESMF_GridCreateUTest.F90,v 1.47 2007/07/05 19:07:50 oehmke Exp $'
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

  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid
  integer :: coordDimMap(2,2), rank, lbounds(2), ubounds(2)
  type(ESMF_IndexFlag) :: indexflag
  integer :: gridType, dimmap(2), coordRanks(2)

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
  write(name, *) "Creating a Grid with only a distgrid and the rest defaults"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc=ESMF_SUCCESS

  ! create a grid with all defaults
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info from Grid
  call ESMF_GridGet(grid, rank=rank, coordTypeKind=typekind, &
         dimmap=dimmap, coordRanks=coordRanks, coordDimMap=coordDimMap, &
         indexflag=indexflag, gridtype=gridtype, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that defaults are as expected
  correct=.true.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false.
  if (rank .ne. 2) correct=.false.
  if ((dimmap(1) .ne. 1) .or. (dimmap(2) .ne. 2)) correct=.false.
  !TODO: what to do about lbounds and ubounds
  if ((coordRanks(1) .ne. 2) .or. (coordRanks(2) .ne. 2)) correct=.false.
  if ((coordDimMap(1,1) .ne. 1) .or. (coordDimMap(1,2) .ne. 2) .or. & 
      (coordDimMap(2,1) .ne. 1) .or. (coordDimMap(2,2) .ne. 2)) correct=.false.
!  if (indexflag .ne. ESMF_INDEX_DELOCAL) correct=.false.
  if (gridtype .ne. 0) correct=.false.

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
  write(name, *) "Creating a Grid with a nondefault name"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(name=grid_name, distgrid=distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX____UTest
  ! This test crashes bug 1745580 has been opened
  !grid_name="NOT_GRID"
  !write(name, *) "Getting a name from a grid"
  !write(failMsg, *) "Did not return ESMF_SUCCESS"
  !call ESMF_GridGet(grid, name=grid_name, rc=rc)
  !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordTypeKind"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=localrc)
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
  write(name, *) "Creating a Grid with non-default ubounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, ubounds=(/10,20/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, ubounds=ubounds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((ubounds(1) .ne. 10) .or. (ubounds(2) .ne. 20)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default lbounds and ubounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid,lbounds=(/2,1/), ubounds=(/10,20/), &
                                   rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, lbounds=lbounds, ubounds=ubounds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((lbounds(1) .ne. 2) .or. (lbounds(2) .ne. 1)) correct=.false.
  if ((ubounds(1) .ne. 10) .or. (ubounds(2) .ne. 20)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordRanks"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordRanks=(/1,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordRanks=coordRanks, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((coordRanks(1) .ne. 1) .or. (coordRanks(2) .ne. 2)) correct=.false.

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
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordRanks=(/1,2/), &
                                   coordDimMap=coordDimMap,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, coordRanks=coordRanks, coordDimMap=coordDimMap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((coordRanks(1) .ne. 1) .or. (coordRanks(2) .ne. 2)) correct=.false.
  if (coordDimMap(1,1) .ne. 1) correct=.false.
  if ((coordDimMap(2,1) .ne. 2) .or. (coordDimMap(2,2) .ne. 1)) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default dimmap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, dimmap=(/1,2/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, dimmap=dimmap, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if ((dimmap(1) .ne. 1) .or. (dimmap(2) .ne. 2)) correct=.false.

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
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
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
  write(name, *) "Creating a Grid with non-default gridType"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! create grid with nondefault parameter
  rc=ESMF_SUCCESS
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, gridtype=1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! get info back from grid
  call ESMF_GridGet(grid, gridtype=gridtype, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check that output is as expected
  correct=.true.
  if (gridtype .ne. 1) correct=.false.

  ! destroy grid
  call ESMF_GridDestroy(grid,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCreateUTest
