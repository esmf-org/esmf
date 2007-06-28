! $Id: ESMF_GridCreateUTest.F90,v 1.45 2007/06/28 22:46:10 oehmke Exp $
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
    '$Id: ESMF_GridCreateUTest.F90,v 1.45 2007/06/28 22:46:10 oehmke Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer :: i, j, petCount
  logical :: looptest

  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid
  integer :: coordDimMap(2,2)

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
  write(name, *) "Creating a Grid with only a distgrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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
  write(name, *) "Creating a Grid with non-default coordTypeKind"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_I4,rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default ubounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, ubounds=(/10,10/),rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default lbounds and ubounds"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid,lbounds=(/2,2/), ubounds=(/10,10/),rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordRanks"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordRanks=(/1,1/),rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default coordDimMap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  coordDimMap(1,:)=(/1,0/)
  coordDimMap(2,:)=(/2,0/)
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordRanks=(/1,1/),coordDimMap=coordDimMap,rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default dimmap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, dimmap=(/1,2/),rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default indexflag"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_GridDestroy(grid)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Creating a Grid with non-default gridType"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, gridtype=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCreateUTest
