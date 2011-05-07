! $Id: ESMF_FieldRegridXGOnlineUTest.F90,v 1.4 2011/05/07 02:06:16 feiliu Exp $
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
    call test_regrid2xg_online(3,3,4,4,1.,1.,0.6,0.6,tag='small test', &
      maxnpet=2, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(4,4,3,3,0.6,0.6,1.,1.,tag='reverse small test', &
      maxnpet=2, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(10,10,14,14,0.1,0.1,0.06,0.06,tag='medium size test', &
      maxnpet=4, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(10,10,14,14,0.1,0.1,0.06,0.06,tag='reverse medium size test', &
      maxnpet=4, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(3,4,4,6,1.,1.,0.6,0.6,tag='uneven small test', &
      maxnpet=2, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(4,6,3,4,0.6,0.6,1.,1.,tag='reverse uneven small test', &
      maxnpet=2, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(40,40,80,80,1.,1.,0.5,0.5,tag='reverse exact large test', &
      maxnpet=8, minnpet=4, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(6,5,6,4,1.,1.,1.,1.,165.,30.,165.,30.,&
      maxnpet=2, tag='small regional sphere exact', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(6,5,6,4,1.,1.,1.,1.,-165.,30.,-165.,30.,&
      maxnpet=2, tag='small regional sphere exact negative lon', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(5,6,5,6,2.,1.,1.,1.,-165.,30.,-165.,30.,&
      maxnpet=2, tag='small regional sphere contain', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(10,20,20,20,1.,1.,0.5,1.,-165.,30.,-165.,30.,&
      maxnpet=8, tag='medium regional sphere cut', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(10,20,20,20,1.,1.,0.6,1.,-165.,30.,-165.,30.,&
      maxnpet=8, tag='medium regional sphere overlap', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(10,20,20,20,1.,1.,0.6,1.,-165.,30.,-166.,30.,&
      maxnpet=8, tag='medium regional sphere overlap2', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(40,40,80,80,1.,1.,0.6,0.6,-165.,30.,-168.,25.,&
      maxnpet=8, minnpet=4, tag='large regional sphere overlap', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(80,80,40,40,0.6,0.6,1.,1.,-168.,25.,-165.,30.,&
      maxnpet=8, minnpet=4, tag='reverse large regional sphere overlap', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"

!------------------------------------------------------------------------
  subroutine test_regrid2xg_online(atm_nx, atm_ny, ocn_nx, ocn_ny, atm_dx, atm_dy, &
    ocn_dx, ocn_dy, atm_sx, atm_sy, ocn_sx, ocn_sy, tag, maxnpet, minnpet, rc)
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny, ocn_nx, ocn_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy, ocn_dx, ocn_dy
    real(ESMF_KIND_R4), intent(in), optional  :: atm_sx, atm_sy, ocn_sx, ocn_sy
    character(len=*), intent(in), optional    :: tag
    integer, intent(in) , optional            :: maxnpet, minnpet
    integer, intent(out), optional            :: rc

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
    integer                         :: gn(2), simax, dimax, l_minnpet, l_maxnpet
    real(ESMF_KIND_R8), pointer     :: atm(:,:), ocn(:,:), exf(:), xArea(:), xFrac(:)
    real(ESMF_KIND_R8), pointer     :: srcFracPtr(:,:), dstFracPtr(:,:)
    real(ESMF_KIND_R8), pointer     :: srcArea(:,:), dstArea(:,:)
    real(ESMF_KIND_R8), pointer     :: srcAreaTmp(:), dstAreaTmp(:)
    type(ESMF_RouteHandle)          :: rh_a2o, rh_o2a, rh_a2x, rh_x2o, rh_o2x, rh_x2a
    type(ESMF_DistGrid)             :: distgridM, distgrid1, distgrid2
    type(ESMF_XGridSpec)            :: sparseMat(1)

    type(ESMF_Field)                :: fa_atm, fa_ocn, fa_xgrid
    type(ESMF_Field)                :: aa_atm, aa_ocn, aa_xgrid
    type(ESMF_Field)                :: srcFrac, dstFrac
    type(ESMF_Mesh)                 :: mesh_atm, mesh_ocn, mesh_xgrid
    real(ESMF_KIND_R8)              :: srcsum(2), allsrcsum(2)
    
    type(ESMF_VM)   :: vm

    localrc = ESMF_SUCCESS
    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Don't run this test if there isn't enough pets or too many cuts
    l_minnpet = 1
    l_maxnpet = 128
    if(present(minnpet)) l_minnpet = minnpet
    if(present(maxnpet)) l_maxnpet = maxnpet
    if(npet > l_maxnpet .or. npet < l_minnpet) return

    if(lpet == 0 .and. present(tag)) then
      print *, '!------------------------------------'
      print *, '!', tag
      print *, '!------------------------------------'
    endif

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

    !call ESMF_GridGet(grid_atm, distgrid=distgrid1, rc=localrc)
    !call ESMF_distgridprint(distgrid1)
    !distgrid2=ESMF_distgridcreate(distgrid1)
    !call ESMF_distgridprint(distgrid2)

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
    print *, 'coordX: ', coordX
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

    srcFrac = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    dstFrac = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
      routehandle=rh_a2o, &
      unmappedDstAction = ESMF_UNMAPPEDACTION_IGNORE, &
      srcFracField=srcFrac, dstFracField=dstFrac, & 
      indices=indices, weights=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! make sure the numbers are consistent
    !print *, lpet, 'weights', size(weights)
    !print *, lpet, 'indices', size(indices,1),'-', size(indices,2)
    !do j = 1, size(indices,1)
    !     print *, indices(j,1), '->', indices(j,2), weights(j)
    !enddo

    call ESMF_FieldRegridStore(srcField=f_ocn, dstField=f_atm, &
      regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
      routehandle=rh_o2a, &
      unmappedDstAction = ESMF_UNMAPPEDACTION_IGNORE, &
      indices=indices, weights=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! make sure the numbers are consistent
    !print *, lpet, 'weights', size(weights)
    !print *, lpet, 'indices', size(indices,1),'-', size(indices,2)
    !do j = 1, size(indices,1)
    !     print *, indices(j,1), '->', indices(j,2), weights(j)
    !enddo

    !----------------------------------------------------
    ! Call into online generation
    !----------------------------------------------------
    xgrid = ESMF_XGridCreate(sideA=(/grid_atm/), sideB=(/grid_ocn/), &
        sideAToXGridScheme=ESMF_REGRID_SCHEME_NATIVE, &
        sideBToXGridScheme=ESMF_REGRID_SCHEME_NATIVE, &
        storeOverlay=.true.,&
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call checkProxy(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridGet(xgrid, distgridM=distgridM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !call ESMF_DistGridPrint(distgridM, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return

    ! create a Field on the xgrid
    f_xgrid = ESMF_FieldCreate(xgrid=xgrid, TYPEKIND=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !----------------------------------------------------
    ! regrid through the xgrid
    ! set up src flux
    !----------------------------------------------------
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
    call ESMF_FieldGet(srcFrac, farrayPtr=srcFracPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(dstFrac, farrayPtr=dstFracPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'src: ', srcFracPtr(:,1)
    !print *, 'dst: ', dstFracPtr

    allocate(srcAreaTmp(size(srcFracPtr,1)*size(srcFracPtr, 2)))
    allocate(dstAreaTmp(size(dstFracPtr,1)*size(dstFracPtr, 2)))
    allocate(srcArea(lbound(srcFracPtr,1):ubound(srcFracPtr, 1), &
      lbound(srcFracPtr, 2):ubound(srcFracPtr, 2)))
    allocate(dstArea(lbound(dstFracPtr,1):ubound(dstFracPtr, 1), &
      lbound(dstFracPtr, 2):ubound(dstFracPtr, 2)))
    mesh_atm = ESMF_GridToMesh(grid_atm, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshGetElemArea(mesh_atm, srcAreaTmp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    mesh_ocn = ESMF_GridToMesh(grid_ocn, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshGetElemArea(mesh_ocn, dstAreaTmp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = lbound(srcFracPtr,1), ubound(srcFracPtr, 1)
      do j = lbound(srcFracPtr, 2), ubound(srcFracPtr, 2)
        srcArea(i,j) = srcAreaTmp(i+j*(ubound(srcFracPtr, 1)-lbound(srcFracPtr,1)))
      enddo
    enddo
    do i = lbound(dstFracPtr,1), ubound(dstFracPtr, 1)
      do j = lbound(dstFracPtr, 2), ubound(dstFracPtr, 2)
        dstArea(i,j) = dstAreaTmp(i+j*(ubound(dstFracPtr, 1)-lbound(dstFracPtr,1)))
      enddo
    enddo

    !----------------------------------------------------
    ! Compute flux integrals
    !----------------------------------------------------
    call init_src(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridGetCoord(grid_atm, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center 
    call ESMF_GridGetCoord(grid_atm, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    srcsum = 0.
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordY,1), ubound(coordY,1)
        !atm(i,j) = 0.5*sin(coordX(i)/(atm_nx*atm_dx)*2*3.14159)*cos(coordY(j)&
        !/(atm_ny*atm_dy)*4*3.14159)
        !atm(i,j) = i
        srcsum(1) = srcsum(1) + atm(i,j)*srcArea(i,j)*srcFracPtr(i,j)
        srcsum(2) = srcsum(2) +          srcArea(i,j)*srcFracPtr(i,j)
      enddo
    enddo
    call ESMF_VMAllReduce(vm, srcsum, allsrcsum, 2, ESMF_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, lpet, ' src flux and area: ', atm
    if(lpet == 0) print *, ' src flux and area: ', allsrcsum
    call compute_flux2D(vm, atm, srcArea, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' src flux and area sub: ', allsrcsum

    !----------------------------------------------------
    ! call direct regrid
    !----------------------------------------------------
    call ESMF_FieldRegrid(srcField=f_atm, dstField=f_ocn, routehandle=rh_a2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, ocn, dstArea, dstFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area from direct Regrid: ', allsrcsum
    call ESMF_FieldRegrid(srcField=f_ocn, dstField=f_atm, routehandle=rh_o2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, atm, srcArea, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area from direct Regrid: ', allsrcsum

    !----------------------------------------------------
    ! compute regrid routehandle
    !----------------------------------------------------
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

    !----------------------------------------------------
    ! execute regrid
    ! reinitialize atm data because we called regridstore/regrid on f_atm
    !----------------------------------------------------
    call init_src(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh_a2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'a2x exf', lpet, exf

    allocate(xArea(lbound(exf,1):ubound(exf,1)))
    allocate(xFrac(lbound(exf,1):ubound(exf,1)))
    call ESMF_XGridGet(xgrid, area=xArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    xFrac = 1.0
    call compute_flux1D(vm, exf, xArea, xFrac, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' xgrid flux and area: ', allsrcsum
    !call display_flux1D(exf, xArea, xFrac)
    call ESMF_XGridGet(xgrid, sparseMatA2X=sparseMat, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !do j = 1, size(sparseMat(1)%factorList,1)
    !     print *, sparseMat(1)%factorIndexList(1,j), '->', &
    !      sparseMat(1)%factorIndexList(2,j), sparseMat(1)%factorList(j)
    !enddo
    

    call ESMF_FieldRegrid(f_xgrid, f_ocn, routehandle=rh_x2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'x2o ocn', lpet, ocn

    ! Compute flux integrals
    call compute_flux2D(vm, ocn, dstArea, dstFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area: ', allsrcsum

    !call ESMF_FieldWrite(f_atm, 'atm', rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return
    !call ESMF_FieldWrite(f_ocn, 'ocn', rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return

    !----------------------------------------------------
    ! Now reverse the regridding direction through xgrid
    !----------------------------------------------------
    !call display_flux1D(exf, xArea, xFrac)
    call ESMF_FieldRegrid(f_ocn, f_xgrid, routehandle=rh_o2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'o2x exf', lpet, exf
    call compute_flux1D(vm, exf, xArea, xFrac, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed xgrid flux and area: ', allsrcsum
    !call display_flux1D(exf, xArea, xFrac)

    !call display_flux2D(atm, srcArea, srcFracPtr)
    call ESMF_FieldRegrid(f_xgrid, f_atm, routehandle=rh_x2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'x2a atm', lpet, atm
    call compute_flux2D(vm, atm, srcArea, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area: ', allsrcsum
    !call display_flux2D(atm, srcArea, srcFracPtr)

    !----------------------------------------------------
    ! clean up
    !----------------------------------------------------
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

    call ESMF_MeshDestroy(mesh_atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshDestroy(mesh_ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldDestroy(srcFrac, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldDestroy(dstFrac, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(srcArea, srcAreaTmp)
    deallocate(dstArea, dstAreaTmp)
    deallocate(xArea, xFrac)

    !----------------------------------------------------
    ! release routehandles
    !----------------------------------------------------
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
    call ESMF_FieldRegridRelease(rh_a2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridRelease(rh_o2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine test_regrid2xg_online

!------------------------------------------------------------------------

  subroutine test_regrid2xgSph(atm_nx, atm_ny, ocn_nx, ocn_ny, atm_dx, atm_dy, &
    ocn_dx, ocn_dy, atm_sx, atm_sy, ocn_sx, ocn_sy, tag, maxnpet, minnpet, rc)
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny, ocn_nx, ocn_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy, ocn_dx, ocn_dy
    real(ESMF_KIND_R4), intent(in), optional  :: atm_sx, atm_sy, ocn_sx, ocn_sy
    character(len=*), intent(in), optional    :: tag
    integer, intent(in) , optional            :: maxnpet, minnpet
    integer, intent(out), optional            :: rc

    ! local variables
    type(ESMF_Grid)                 :: grid_atm, grid_ocn
    type(ESMF_Field)                :: f_atm, f_ocn, f_xgrid
    real(ESMF_KIND_R8)              :: startx, starty
    integer                         :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer     :: weights(:)
    integer(ESMF_KIND_I4), pointer  :: indices(:,:)
    real(ESMF_KIND_R8), pointer     :: coordX(:,:), coordY(:,:)
    type(ESMF_XGrid)                :: xgrid

    type(ESMF_XGridSpec)            :: sparseMatA2X(1)
    integer                         :: gn(2), simax, dimax, l_minnpet, l_maxnpet
    real(ESMF_KIND_R8), pointer     :: atm(:,:), ocn(:,:), exf(:), xArea(:), xFrac(:)
    real(ESMF_KIND_R8), pointer     :: srcFracPtr(:,:), dstFracPtr(:,:)
    real(ESMF_KIND_R8), pointer     :: srcArea(:,:), dstArea(:,:)
    real(ESMF_KIND_R8), pointer     :: srcAreaTmp(:), dstAreaTmp(:)
    type(ESMF_RouteHandle)          :: rh_a2o, rh_o2a, rh_a2x, rh_x2o, rh_o2x, rh_x2a
    type(ESMF_DistGrid)             :: distgridM, distgrid1, distgrid2
    type(ESMF_XGridSpec)            :: sparseMat(1)

    type(ESMF_Field)                :: fa_atm, fa_ocn, fa_xgrid
    type(ESMF_Field)                :: aa_atm, aa_ocn, aa_xgrid
    type(ESMF_Field)                :: srcFrac, dstFrac
    type(ESMF_Mesh)                 :: mesh_atm, mesh_ocn, mesh_xgrid
    real(ESMF_KIND_R8)              :: srcsum(3), allsrcsum(3), scale

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

    ! Don't run this test if there isn't enough pets or too many cuts
    l_minnpet = 1
    l_maxnpet = 128
    if(present(minnpet)) l_minnpet = minnpet
    if(present(maxnpet)) l_maxnpet = maxnpet
    if(npet > l_maxnpet .or. npet < l_minnpet) return

    if(lpet == 0 .and. present(tag)) then
      print *, '!------------------------------------'
      print *, '!', tag
      print *, '!------------------------------------'
    endif

    ! Grid constants
    ! Atmosphere covers the area (-165,30) - (-15, 50)
    ! North Pacific Ocean covers (-165,30) - (-120, 50)
    ! Running the Grids from NW corner to SE corner, 
    ! change the starting Y coord and dy
    !
    ! 1 degree atmosphere, 1 degree ocean
    !atm_nx = 5
    !atm_ny = 20
    !atm_dx = 2.
    !atm_dy = 1.
    !ocn_nx = 5
    !ocn_ny = 20
    !ocn_dx = 1.
    !ocn_dy = 1.
    !
    !atm_sx = -165.
    !atm_sy = 30.
    !ocn_sx = -165.
    !ocn_sy = 30.
    
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
    !print *, 'startx: ', startx, lbound(coordX, 1), 'coordX: ', coordX
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
    !print *, 'startx: ', startx, lbound(coordX, 1), 'coordX: ', coordX(:,1), coordX(1,:)
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

    srcFrac = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    dstFrac = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call RegridStore here to compute SMM weights and indices
    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      routehandle=rh_a2o, &
      regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
      unmappedDstAction = ESMF_UNMAPPEDACTION_IGNORE, &
      regridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      srcFracField=srcFrac, dstFracField=dstFrac, & 
      indices=indices, weights=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! make sure the numbers are consistent
    !print *, lpet, 'weights', size(weights)
    !print *, lpet, 'indices', size(indices,1),'-', size(indices,2)
    !do j = 1, size(indices,1)
    !     print *, indices(j,1), '->', indices(j,2), weights(j)
    !enddo

    call ESMF_FieldRegridStore(srcField=f_ocn, dstField=f_atm, &
      routehandle=rh_o2a, &
      regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
      unmappedDstAction = ESMF_UNMAPPEDACTION_IGNORE, &
      regridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      indices=indices, weights=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! make sure the numbers are consistent
    !print *, lpet, 'weights', size(weights)
    !print *, lpet, 'indices', size(indices,1),'-', size(indices,2)
    !do j = 1, size(indices,1)
    !     print *, indices(j,1), '->', indices(j,2), weights(j)
    !enddo

    !----------------------------------------------------
    ! Call into online generation
    !----------------------------------------------------
    xgrid = ESMF_XGridCreate(sideA=(/grid_atm/), sideB=(/grid_ocn/), &
        sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
        sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
        storeOverlay=.true.,&
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call checkProxy(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridGet(xgrid, distgridM=distgridM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !call ESMF_DistGridPrint(distgridM, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return

    ! create a Field on the xgrid
    f_xgrid = ESMF_FieldCreate(xgrid=xgrid, TYPEKIND=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !----------------------------------------------------
    ! regrid through the xgrid
    ! set up src flux
    !----------------------------------------------------
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
    call ESMF_FieldGet(srcFrac, farrayPtr=srcFracPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(dstFrac, farrayPtr=dstFracPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'src: ', srcFracPtr(:,1)
    !print *, 'dst: ', dstFracPtr

    allocate(srcAreaTmp(size(srcFracPtr,1)*size(srcFracPtr, 2)))
    allocate(dstAreaTmp(size(dstFracPtr,1)*size(dstFracPtr, 2)))
    allocate(srcArea(lbound(srcFracPtr,1):ubound(srcFracPtr, 1), &
      lbound(srcFracPtr, 2):ubound(srcFracPtr, 2)))
    allocate(dstArea(lbound(dstFracPtr,1):ubound(dstFracPtr, 1), &
      lbound(dstFracPtr, 2):ubound(dstFracPtr, 2)))
    mesh_atm = ESMF_GridToMesh(grid_atm, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshGetElemArea(mesh_atm, srcAreaTmp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    mesh_ocn = ESMF_GridToMesh(grid_ocn, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshGetElemArea(mesh_ocn, dstAreaTmp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = lbound(srcFracPtr,1), ubound(srcFracPtr, 1)
      do j = lbound(srcFracPtr, 2), ubound(srcFracPtr, 2)
        srcArea(i,j) = srcAreaTmp(i+j*(ubound(srcFracPtr, 1)-lbound(srcFracPtr,1)))
      enddo
    enddo
    do i = lbound(dstFracPtr,1), ubound(dstFracPtr, 1)
      do j = lbound(dstFracPtr, 2), ubound(dstFracPtr, 2)
        dstArea(i,j) = dstAreaTmp(i+j*(ubound(dstFracPtr, 1)-lbound(dstFracPtr,1)))
      enddo
    enddo

    !----------------------------------------------------
    ! Compute flux integrals
    !----------------------------------------------------
    call init_src_sph(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridGetCoord(grid_atm, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center 
    call ESMF_GridGetCoord(grid_atm, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    srcsum = 0.
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX,2), ubound(coordX,2)
        !atm(i,j) = 0.5*sin(coordX(i)/(atm_nx*atm_dx)*2*3.14159)*cos(coordY(j)&
        !/(atm_ny*atm_dy)*4*3.14159)
        !atm(i,j) = i
        srcsum(1) = srcsum(1) + atm(i,j)*srcArea(i,j)*srcFracPtr(i,j)
        srcsum(2) = srcsum(2) +          srcArea(i,j)*srcFracPtr(i,j)
        srcsum(3) = srcsum(2) +          srcArea(i,j)
      enddo
    enddo
    call ESMF_VMAllReduce(vm, srcsum, allsrcsum, 3, ESMF_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, lpet, ' src flux and area: ', atm
    scale = 3.1415926535897932/180.
    if(lpet == 0) print *, ' src flux and area: ', allsrcsum, &
      (sin(scale*(atm_sy+atm_ny*atm_dy))-sin(scale*atm_sy))*(atm_nx*atm_dx)*scale
    call compute_flux2D(vm, atm, srcArea, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' src flux and area sub: ', allsrcsum

    !----------------------------------------------------
    ! call direct regrid
    !----------------------------------------------------
    !call display_flux2D(atm, srcArea, srcFracPtr)
    call ESMF_FieldRegrid(srcField=f_atm, dstField=f_ocn, routehandle=rh_a2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, ocn, dstArea, dstFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area from direct Regrid: ', allsrcsum
    !call display_flux2D(ocn, dstArea, dstFracPtr)
    call ESMF_FieldRegrid(srcField=f_ocn, dstField=f_atm, routehandle=rh_o2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, atm, srcArea, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area from direct Regrid: ', allsrcsum


    !----------------------------------------------------
    ! compute regrid routehandle
    !----------------------------------------------------
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

    !----------------------------------------------------
    ! execute regrid
    ! reinitialize atm data because we called regridstore/regrid on f_atm
    !----------------------------------------------------
    call init_src_sph(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh_a2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'a2x exf', lpet, exf

    allocate(xArea(lbound(exf,1):ubound(exf,1)))
    allocate(xFrac(lbound(exf,1):ubound(exf,1)))
    call ESMF_XGridGet(xgrid, area=xArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    xFrac = 1.0
    call compute_flux1D(vm, exf, xArea, xFrac, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' xgrid flux and area: ', allsrcsum
    !call display_flux1D(exf, xArea, xFrac)
    call ESMF_XGridGet(xgrid, sparseMatA2X=sparseMat, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !do j = 1, size(sparseMat(1)%factorList,1)
    !     print *, sparseMat(1)%factorIndexList(1,j), '->', &
    !      sparseMat(1)%factorIndexList(2,j), sparseMat(1)%factorList(j)
    !enddo
    

    call ESMF_FieldRegrid(f_xgrid, f_ocn, routehandle=rh_x2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'x2o ocn', lpet, ocn

    ! Compute flux integrals
    call compute_flux2D(vm, ocn, dstArea, dstFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area: ', allsrcsum

    !call ESMF_FieldWrite(f_atm, 'atm', rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return
    !call ESMF_FieldWrite(f_ocn, 'ocn', rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return

    !----------------------------------------------------
    ! Now reverse the regridding direction through xgrid
    !----------------------------------------------------
    !call display_flux1D(exf, xArea, xFrac)
    call ESMF_FieldRegrid(f_ocn, f_xgrid, routehandle=rh_o2x, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'o2x exf', lpet, exf
    call compute_flux1D(vm, exf, xArea, xFrac, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed xgrid flux and area: ', allsrcsum
    !call display_flux1D(exf, xArea, xFrac)

    !call display_flux2D(atm, srcArea, srcFracPtr)
    call ESMF_FieldRegrid(f_xgrid, f_atm, routehandle=rh_x2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'x2a atm', lpet, atm
    call compute_flux2D(vm, atm, srcArea, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area: ', allsrcsum
    !call display_flux2D(atm, srcArea, srcFracPtr)

    !----------------------------------------------------
    ! clean up
    !----------------------------------------------------
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

    call ESMF_MeshDestroy(mesh_atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshDestroy(mesh_ocn, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldDestroy(srcFrac, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldDestroy(dstFrac, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(srcArea, srcAreaTmp)
    deallocate(dstArea, dstAreaTmp)
    deallocate(xArea, xFrac)

    !----------------------------------------------------
    ! release routehandles
    !----------------------------------------------------
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
    call ESMF_FieldRegridRelease(rh_a2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldRegridRelease(rh_o2a, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine test_regrid2xgSph

  subroutine test_regrid180vs360_xgrid(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: grid360
  type(ESMF_Grid) :: grid180
  type(ESMF_Field) :: srcField360
  type(ESMF_Field) :: dstField360
  type(ESMF_Field) :: field180
  type(ESMF_Field) :: errorField
  type(ESMF_Array) :: array180
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArray360
  type(ESMF_Array) :: srcArray360, dstArray360
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: rangle, xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: RAD2DEG

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  type(ESMF_XGrid)               :: xgrid
  type(ESMF_RouteHandle)         :: rh_s2x, rh_x2d, rh_d2x, rh_x2s

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids

  dst_nx = 90
  dst_ny = 50

  src_nx = 90
  src_ny = 50

  ! setup source grid
  grid360=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  grid180=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   field180 = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(grid360, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(grid180, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(grid360, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! array180
  call ESMF_FieldGet(field180, array=array180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArray360
  call ESMF_FieldGet(srcField360, array=srcArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! dstArray360
  call ESMF_FieldGet(dstField360, array=dstArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  !! get longitude array
  call ESMF_GridGetCoord(grid360, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=lonArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Write results to a mesh
  num_arrays = 1

! Test interpolation on the sphere
! Set the source grid coordinates to be a 0 to 360 grid

  src_dx = 360./src_nx
  src_dy = 180./src_ny

  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

  ! Get memory and set coords for src
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field pointer
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,   rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        ! (something relatively smooth, that varies everywhere)
        farrayPtr(i1,i2) = x+y+z+15.0

         ! initialize src destination field
         farrayPtr2(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  rangle = DEG2RAD*20.

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(field180, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error dst clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error dst clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error dst cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error dst cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set destination coordinates as -180 to 180
        farrayPtrXC(i1,i2) = -180. + (REAL(i1-1)*dst_dx)
        farrayPtrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! init destination mesh to 0
        farrayPtr(i1,i2) = 0.
     
     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcField360, dstField=field180, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField360, field180, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  !!!!!!!! Regrid back from the -180 to 180 grid to the 0 to 360 grid
  ! Regrid store
  call ESMF_FieldRegridStore(field180, dstField=dstField360, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(field180, dstField360, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (farrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2) - farrayPtr2(i1,i2))/farrayPtr(i1,i2))
        else
           errorfarrayPtr(i1,i2)=(farrayPtr(i1,i2) - farrayPtr2(i1,i2))
        endif
        if (ABS(errorfarrayPtr(i1,i2)) .gt. 0.01) then
            correct=.false. 
        endif

     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, grid360, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray360, dstArray360, errorArray, lonArray360, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
  call ESMF_MeshIO(vm, grid180, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", array180, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(field180, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(grid360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(grid180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

  end subroutine test_regrid180vs360_xgrid

  !------------------------------------------------------------------------

  subroutine compute_flux1D(vm, flux_density, area, fraction, allsum, rc)
    type(ESMF_VM), intent(in)        :: vm
    real(ESMF_KIND_R8), pointer      :: flux_density(:) 
    real(ESMF_KIND_R8), pointer      :: area(:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:) 
    real(ESMF_KIND_R8), intent(out)  :: allsum(2)
    integer, intent(out), optional   :: rc

    real(ESMF_KIND_R8)               :: sum(2)
    integer                          :: i,j, localrc

    if(present(rc)) rc = ESMF_SUCCESS

    sum = 0.
    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      sum(1) = sum(1) + flux_density(i)*area(i)*fraction(i)
      sum(2) = sum(2) +                 area(i)*fraction(i)
    enddo

    call ESMF_VMAllReduce(vm, sum, allsum, 2, ESMF_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine compute_flux1D

  subroutine compute_flux2D(vm, flux_density, area, fraction, allsum, rc)
    type(ESMF_VM), intent(in)        :: vm
    real(ESMF_KIND_R8), pointer      :: flux_density(:,:) 
    real(ESMF_KIND_R8), pointer      :: area(:,:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:,:) 
    real(ESMF_KIND_R8), intent(out)  :: allsum(2)
    integer, intent(out), optional   :: rc

    real(ESMF_KIND_R8)               :: sum(2)
    integer                          :: i,j, localrc

    if(present(rc)) rc = ESMF_SUCCESS

    sum = 0.
    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      do j = lbound(flux_density, 2), ubound(flux_density, 2)
        sum(1) = sum(1) + flux_density(i,j)*area(i,j)*fraction(i,j)
        sum(2) = sum(2) +                 area(i,j)*fraction(i,j)
      enddo
    enddo

    call ESMF_VMAllReduce(vm, sum, allsum, 2, ESMF_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine compute_flux2D

  subroutine display_flux1D(flux_density, area, fraction, rc)
    real(ESMF_KIND_R8), pointer      :: flux_density(:) 
    real(ESMF_KIND_R8), pointer      :: area(:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:) 
    integer, intent(out), optional   :: rc

    integer                          :: i,localrc, npet, lpet
    type(ESMF_VM)                    :: vm

    localrc = ESMF_SUCCESS
    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      !print *, i,flux_density(i), area(i) !, fraction(i)
      write(*,'(I3,I7,2F13.10)') lpet,i,flux_density(i), area(i)
    enddo

  end subroutine display_flux1D

  subroutine display_flux2D(flux_density, area, fraction, rc)
    real(ESMF_KIND_R8), pointer      :: flux_density(:,:) 
    real(ESMF_KIND_R8), pointer      :: area(:,:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:,:) 
    integer, intent(out), optional   :: rc

    integer                          :: i,j, localrc, npet, lpet
    type(ESMF_VM)                    :: vm

    localrc = ESMF_SUCCESS
    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      do j = lbound(flux_density, 2), ubound(flux_density, 2)
        write(*,'(I3,2I5,3F13.10)') lpet, i,j,flux_density(i,j), area(i,j), fraction(i,j)
      enddo
    enddo

  end subroutine display_flux2D

  subroutine init_src(grid, coordX, coordY, nx, ny, dx, dy, src, rc)
    type(ESMF_Grid), intent(in)      :: grid
    real(ESMF_KIND_R8), pointer      :: coordX(:)
    real(ESMF_KIND_R8), pointer      :: coordY(:)
    integer, intent(in)              :: nx, ny
    real, intent(in)                 :: dx, dy
    real(ESMF_KIND_R8), pointer      :: src(:,:)
    integer, intent(out), optional   :: rc

    integer                          :: localrc, i,j 

    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center 
    call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordY,1), ubound(coordY,1)
        src(i,j) = 0.5*sin(coordX(i)/(nx*dx)*2*3.14159)*cos(coordY(j)&
        /(ny*dy)*4*3.14159)
        !src(i,j) = i
      enddo
    enddo

  end subroutine init_src

  subroutine init_src_sph(grid, coordX, coordY, nx, ny, dx, dy, src, rc)
    type(ESMF_Grid), intent(in)      :: grid
    real(ESMF_KIND_R8), pointer      :: coordX(:,:)
    real(ESMF_KIND_R8), pointer      :: coordY(:,:)
    integer, intent(in)              :: nx, ny
    real, intent(in)                 :: dx, dy
    real(ESMF_KIND_R8), pointer      :: src(:,:)
    integer, intent(out), optional   :: rc

    integer                          :: localrc, i,j 

    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center 
    call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX,2), ubound(coordX,2)
        src(i,j) = 0.5*sin(coordX(i,j)/(nx*dx)*2*3.14159)*cos(coordY(i,j)&
        /(ny*dy)*4*3.14159)
        !src(i,j) = i
      enddo
    enddo

  end subroutine init_src_sph

  subroutine checkProxy(xgrid, rc)
    type(ESMF_XGrid), intent(in)    :: xgrid
    integer, intent(out), optional  :: rc
    
    type(ESMF_XGrid)                :: xgrid1
    character, pointer              :: buffer(:)
    integer                         :: buff_length, offset, localrc

    if(present(rc)) rc = ESMF_SUCCESS
  
    ! Allocate serialization buffer

    buff_length = 1
    allocate (buffer(buff_length))
    offset = 0
    call ESMF_XGridSerialize(xgrid, buffer, buff_length, offset, &
        inquireflag=ESMF_INQUIREONLY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate (buffer)

    buff_length = offset
    allocate (buffer(buff_length))

    ! call serialize and deserialize and verify again

    offset = 0
    call ESMF_XGridSerialize(xgrid, buffer, buff_length, offset, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    offset = 0

    xgrid1 = ESMF_XGridDeserialize(buffer, offset, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate (buffer)

    call ESMF_XGridValidate(xgrid1, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine checkProxy

end program ESMF_FieldRegridXGOnlineUTest
