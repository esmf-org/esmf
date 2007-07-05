! $Id: ESMF_GridCoordUTest.F90,v 1.1 2007/07/05 19:07:50 oehmke Exp $
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
    '$Id: ESMF_GridCoordUTest.F90,v 1.1 2007/07/05 19:07:50 oehmke Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_TypeKind) :: typekind
  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid
  integer :: rank
  type(ESMF_Array) :: array, array2
  type(ESMF_ArraySpec) :: arrayspec

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! prepare DistGrid
  distgrid=ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! create test Grid
  grid=ESMF_GridCreateFromDistGrid(distgrid=distgrid, coordTypeKind=ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Coordinates from Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! set arrayspec
  call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create Array 
  array=ESMF_ArrayCreate(arrayspec, distgrid=distgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Set Coord From Array
  call ESMF_GridSetCoordFromArray(grid,coord=1, &
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Coord From Array
  call ESMF_GridGetCoordIntoArray(grid,coord=1,&
               staggerloc=ESMF_STAGGERLOC_CORNER, array=array2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info to do a partial sanity check that the array is the same
  call ESMF_ArrayGet(array2, rank=rank, typekind=typekind, rc=localrc)

  ! Check that array info is as expected
  correct=.true.
  if (rank .ne. 2) correct=.false.
  if (typekind .ne. ESMF_TYPEKIND_R8) correct=.false. 

  ! destroy test array
  call ESMF_ArrayDestroy(array, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCoordUTest
