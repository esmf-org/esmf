! $Id: ESMF_GridCoordUTest.F90,v 1.3 2007/07/20 00:27:29 oehmke Exp $
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
    '$Id: ESMF_GridCoordUTest.F90,v 1.3 2007/07/20 00:27:29 oehmke Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount

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
  real(ESMF_KIND_R8), pointer :: fptr(:,:), fptr3D(:,:,:)
  integer :: lbnd(2),ubnd(2),lbnd3d(3),ubnd3d(3)

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! prepare 2D DistGrid
  distgrid2D=ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! prepare 3D DistGrid
  distgrid3D=ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/10,10,10/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! create 2D test Grid
  grid2D=ESMF_GridCreate(distgrid=distgrid2D, coordTypeKind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_GLOBAL, rc=rc)
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

  ! Create Array 
  array2D=ESMF_ArrayCreate(arrayspec2D, distgrid=distgrid2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Coordinates from Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS


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

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Get fortran pointer from coordinate array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

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

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D GridAllocCoord, by allocating coordinates and then getting fortran pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  rc=ESMF_SUCCESS

  ! Set Coord From Array
  call ESMF_GridAllocCoord(grid2D, &
               staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! set pointer to null
  nullify(fptr)

  ! Get Coord From Grid
  call ESMF_GridGetLocalTileCoord(grid2D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coord=2, fptr=fptr, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(fptr)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test 2D GridGetLocalTileInfo"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Note that this test depends on coordinates allocated above
  ! and the fact that the grid was created with the ESMF_INDEX_GLOBAL flag

  ! init success flag
  rc=ESMF_SUCCESS

  ! set pointer to null
  nullify(fptr)

  ! Get Coord From Grid
  call ESMF_GridGetLocalTileCoord(grid2D, localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CORNER, coord=2, fptr=fptr, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get info about local piece of coordinate array
  call ESMF_GridGetLocalTileInfo(grid2D, coord=2, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
          totalLBound=lbnd, totalUBound=ubnd, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check that output is as expected
  correct=.true.
  if (.not. associated(fptr)) correct=.false.
  if ((lbound(fptr,1) .ne. lbnd(1)) .or. (lbound(fptr,2) .ne. lbnd(2))) correct=.false.
  if ((ubound(fptr,1) .ne. ubnd(1)) .or. (ubound(fptr,2) .ne. ubnd(2))) correct=.false.

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


  ! Destroy Test Grid
  call ESMF_GridDestroy(grid2D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridDestroy(grid2Dp1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridDestroy(grid3D, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  ! destroy test array
  call ESMF_ArrayDestroy(array2D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GridCoordUTest
