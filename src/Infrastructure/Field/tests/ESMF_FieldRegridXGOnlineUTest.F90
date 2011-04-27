! $Id: ESMF_FieldRegridXGOnlineUTest.F90,v 1.2 2011/04/27 21:14:08 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridXGOnlineUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridXGOnlineUTest - Unit tests for Field RegridXG Testing
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field RegridXG Testing unit tests.
! The companion folder Field\/src contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
#ifdef ESMF_TESTEXHAUSTIVE

        !------------------------------------------------------------------------
        !EX_UTest
        call test_regrid2xg_online(10,10,14,14,0.1,0.1,0.06,0.06,rc)
        write(failMsg, *) ""
        write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !E-disable-X_UTest_Multi_Proc_Only
        !call test_regrid2xgSph(rc)
        !write(failMsg, *) ""
        !write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
        !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"

!------------------------------------------------------------------------
  subroutine test_regrid2xg_online(atm_nx, atm_ny, ocn_nx, ocn_ny, atm_dx, atm_dy, &
    ocn_dx, ocn_dy, rc)
    ! arguments
    integer, intent(in)             :: atm_nx, atm_ny, ocn_nx, ocn_ny
    real(ESMF_KIND_R4), intent(in)  :: atm_dx, atm_dy, ocn_dx, ocn_dy
    integer, intent(out)            :: rc

    ! local variables
    type(ESMF_Grid)                 :: grid_atm, grid_ocn
    type(ESMF_Field)                :: f_atm, f_ocn, f_xgrid
    real(ESMF_KIND_R8)              :: startx, starty
    integer                         :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer     :: weights(:)
    integer(ESMF_KIND_I4), pointer  :: indices(:,:)
    real(ESMF_KIND_R8), pointer     :: coordX(:), coordY(:)
    type(ESMF_XGrid)                :: xgrid
    type(ESMF_XGridSpec)            :: sparseMatA2X(1)
    integer                         :: gn(2), simax, dimax
    real(ESMF_KIND_R8), pointer     :: atm(:,:), ocn(:,:), exf(:)
    type(ESMF_RouteHandle)          :: rh_a2x, rh_x2o, rh_o2x, rh_x2a
    type(ESMF_DistGrid)             :: distgridM

    type(ESMF_Field)                :: fa_atm, fa_ocn, fa_xgrid
    type(ESMF_Field)                :: aa_atm, aa_ocn, aa_xgrid
    type(ESMF_Mesh)                 :: mesh_atm, mesh_ocn, mesh_xgrid
    
    type(ESMF_VM)   :: vm

    localrc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !------------- ATM --------------
    ! atm grid, horizontally decomposed
    grid_atm = ESMF_GridCreateShapeTile(maxIndex=(/atm_nx, atm_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      !regDecomp=(/npet, 1/), &
      coordDep1=(/1/), &
      coordDep2=(/2/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridAddCoord(grid_atm, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridAddCoord(grid_atm, staggerloc=ESMF_STAGGERLOC_CORNER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! global indexing
    ! atm grid is not decomposed in the y direction
    !startx = lpet*atm_nx/npet*atm_dx
    startx = 0.
    starty = 0.
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      coordX(i) = startx + atm_dx/2. + (i-1)*atm_dx
    enddo
    ! X corner
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      coordX(i) = startx + (i-1)*atm_dx
    enddo
    !print *, 'startx: ', startx, lbound(coordX, 1), 'coordX: ', coordX
    ! Y center
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordY,1), ubound(coordY,1)
      coordY(i) = starty + atm_dy/2. + (i-1)*atm_dy
    enddo
    ! Y corner
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordY,1), ubound(coordY,1)
      coordY(i) = starty + (i-1)*atm_dy
    enddo

    !------------- OCN --------------
    ! ocn grid, horizontally decomposed
    grid_ocn = ESMF_GridCreateShapeTile(maxIndex=(/ocn_nx, ocn_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      !regDecomp=(/npet, 1/), &
      coordDep1=(/1/), &
      coordDep2=(/2/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridAddCoord(grid_ocn, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridAddCoord(grid_ocn, staggerloc=ESMF_STAGGERLOC_CORNER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! ocn grid is not decomposed in the y direction
    !startx = lpet*ocn_nx/npet*ocn_dx
    startx = 0.
    starty = 0.
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      coordX(i) = startx + ocn_dx/2. + (i-1)*ocn_dx
    enddo
    ! X corner
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      coordX(i) = startx + (i-1)*ocn_dx
    enddo
    ! Y center 
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordY,1), ubound(coordY,1)
      coordY(i) = starty + ocn_dy/2. + (i-1)*ocn_dy
    enddo
    ! Y corner
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordY,1), ubound(coordY,1)
      coordY(i) = starty + (i-1)*ocn_dy
    enddo

    ! build Fields on the Grids
    f_atm = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    f_ocn = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
      unmappedDstAction = ESMF_UNMAPPEDACTION_IGNORE, &
      indices=indices, weights=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! make sure the numbers are consistent
    print *, lpet, 'weights', size(weights)
    print *, lpet, 'indices', size(indices,1),'-', size(indices,2)
    do j = 1, size(indices,1)
         print *, indices(j,1), '->', indices(j,2), weights(j)
    enddo

    ! Call into online generation
    xgrid = ESMF_XGridCreate(sideA=(/grid_atm/), sideB=(/grid_ocn/), &
        sideAScheme=ESMF_XGRID_CARTESIAN2D, &
        sideBScheme=ESMF_XGRID_CARTESIAN2D, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridGet(xgrid, distgridM=distgridM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_DistGridPrint(distgridM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! create a Field on the xgrid
    f_xgrid = ESMF_FieldCreate(xgrid=xgrid, TYPEKIND=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! regrid through the xgrid
    ! set up src flux
    call ESMF_FieldGet(f_atm, farrayPtr=atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(f_xgrid, farrayPtr=exf, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(f_ocn, farrayPtr=ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! compute regrid routehandle
    call ESMF_FieldRegridStore(xgrid, f_atm, f_xgrid, routehandle=rh_a2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridStore(xgrid, f_xgrid, f_atm, routehandle=rh_x2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridStore(xgrid, f_ocn, f_xgrid, routehandle=rh_o2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridStore(xgrid, f_xgrid, f_ocn, routehandle=rh_x2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! execute regrid
    ! once area information is retrieved from mesh regrid store, then
    ! one can perform variable flux exchange and check flux conservation
    ! for now the src flux is a constant term, this results in const dst flux
    atm = 2.
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh_a2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, 'a2x exf', lpet, exf

    call ESMF_FieldRegrid(f_xgrid, f_ocn, routehandle=rh_x2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, 'x2o ocn', lpet, ocn

    ocn = 4.
    call ESMF_FieldRegrid(f_ocn, f_xgrid, routehandle=rh_o2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, 'o2x exf', lpet, exf

    call ESMF_FieldRegrid(f_xgrid, f_atm, routehandle=rh_x2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, 'x2a atm', lpet, atm

    ! clean up
    call ESMF_FieldDestroy(f_atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldDestroy(f_ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridDestroy(grid_atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridDestroy(grid_ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldDestroy(f_xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_XGridDestroy(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! routehandles
    call ESMF_FieldRegridRelease(rh_a2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridRelease(rh_x2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridRelease(rh_o2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridRelease(rh_x2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xg_online

!------------------------------------------------------------------------

  subroutine test_regrid2xgSph(rc)
    integer, intent(out) :: rc

    type(ESMF_Grid) :: grid_atm, grid_ocn
    type(ESMF_Field) :: f_atm, f_ocn
    type(ESMF_XGrid) :: xgrid
    real(ESMF_KIND_R8) :: atm_dx, atm_dy, ocn_dx, ocn_dy, startx, starty
    real(ESMF_KIND_R8) :: atm_sx, atm_sy, ocn_sx, ocn_sy
    integer             :: atm_nx, atm_ny, ocn_nx, ocn_ny
    integer             :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer :: weights(:)
    integer(ESMF_KIND_I4), pointer :: indices(:,:)
    real(ESMF_KIND_R8), pointer :: coordX(:,:), coordY(:,:)

    type(ESMF_Grid) :: sideA(1), sideB(1)
    type(ESMF_XGridSpec) :: A2X(1)
    
    type(ESMF_VM)   :: vm

    localrc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Grid constants
    ! Atmosphere covers the area (-165,30) - (-15, 50)
    ! North Pacific Ocean covers (-165,30) - (-120, 50)
    ! Running the Grids from NW corner to SE corner, 
    ! change the starting Y coord and dy
    !
    ! 1 degree atmosphere, 1 degree ocean
    atm_nx = 5
    atm_ny = 20
    atm_dx = 2.
    atm_dy = 1.
    ocn_nx = 5
    ocn_ny = 20
    ocn_dx = 1.
    ocn_dy = 1.
    
    atm_sx = -165.
    atm_sy = 30.
    ocn_sx = -165.
    ocn_sy = 30.
    
    !------------- ATM --------------
    ! atm grid, horizontally decomposed
    grid_atm = ESMF_GridCreateShapeTile(maxIndex=(/atm_nx, atm_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      regDecomp=(/npet, 1/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridAddCoord(grid_atm, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridAddCoord(grid_atm, staggerloc=ESMF_STAGGERLOC_CORNER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! global indexing
    ! atm grid is not decomposed in the y direction
    !startx = lpet*atm_nx/npet*atm_dx
    startx = atm_sx
    starty = atm_sy
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + atm_dx/2. + (i-1)*atm_dx
        coordY(i,j) = starty + atm_dy/2. + (j-1)*atm_dy
      enddo
    enddo
    print *, 'startx: ', startx, lbound(coordX, 1), 'coordX: ', coordX(:,1), coordX(1,:)
    ! X corner
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y corner
    call ESMF_GridGetCoord(grid_atm, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + (i-1)*atm_dx
        coordY(i,j) = starty + (j-1)*atm_dy
      enddo
    enddo

    !------------- OCN --------------
    ! ocn grid, horizontally decomposed
    grid_ocn = ESMF_GridCreateShapeTile(maxIndex=(/ocn_nx, ocn_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      regDecomp=(/npet, 1/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridAddCoord(grid_ocn, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridAddCoord(grid_ocn, staggerloc=ESMF_STAGGERLOC_CORNER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! ocn grid is not decomposed in the y direction
    !startx = lpet*ocn_nx/npet*ocn_dx
    startx = ocn_sx
    starty = ocn_sy
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + ocn_dx/2. + (i-1)*ocn_dx
        coordY(i,j) = starty + ocn_dy/2. + (j-1)*ocn_dy
      enddo
    enddo
    print *, 'startx: ', startx, lbound(coordX, 1), 'coordX: ', coordX(:,1), coordX(1,:)
    ! X corner
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y corner
    call ESMF_GridGetCoord(grid_ocn, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + (i-1)*ocn_dx
        coordY(i,j) = starty + (j-1)*ocn_dy
      enddo
    enddo

    ! build Fields on the Grids
    f_atm = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    f_ocn = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call RegridStore here to compute SMM weights and indices
    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
      unmappedDstAction = ESMF_UNMAPPEDACTION_IGNORE, &
      regridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      indices=indices, weights=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_FieldDestroy(f_atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldDestroy(f_ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridDestroy(grid_atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridDestroy(grid_ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    print *, lpet, '-', associated(weights), '-', size(weights), '-', weights
    print *, lpet, '-', associated(indices), '-', size(indices),'-', indices

    sideA(1) = grid_ocn
    sideB(1) = grid_atm
    A2X(1)%factorIndexList = indices
    A2X(1)%factorList = weights
    xgrid = ESMF_XGridCreate(sideA, sideB, sparseMatA2X=A2X, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xgSph

end program ESMF_FieldRegridXGOnlineUTest
