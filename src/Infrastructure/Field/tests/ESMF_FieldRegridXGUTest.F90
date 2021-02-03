! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridXGUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridXGUTest - Unit tests for Field RegridXG Testing
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
    use ESMF

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(3,3,4,4,1.,1.,0.6,0.6,tag='small test', &
      maxnpet=2, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(4,4,3,3,0.6,0.6,1.,1.,tag='reverse small test', &
      maxnpet=2, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(10,10,14,14,0.1,0.1,0.06,0.06,tag='medium size test A1', &
      maxnpet=4, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(10,10,10,10,0.1,0.1,0.06,0.06,tag='medium size test A2', &
      maxnpet=4, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(10,10,10,10,0.1,0.1,0.1,0.1,tag='medium size test A3', &
      maxnpet=4, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(10,10,14,14,0.1,0.1,0.06,0.06,tag='reverse medium size test', &
      maxnpet=4, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(3,4,4,6,1.,1.,0.6,0.6,tag='uneven small test', &
      maxnpet=2, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(4,6,3,4,0.6,0.6,1.,1.,tag='reverse uneven small test', &
      maxnpet=2, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(80,80,40,40,0.5,0.5,1.,1.,tag='exact large test A2', &
      maxnpet=8, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xg_online(40,40,80,80,1.,1.,0.5,0.5,tag='reverse exact large test', &
      maxnpet=8, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid online and regrid through xgrid, overlapping cut"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(6,5,6,4,1.,1.,1.,1.,165.,30.,165.,30., &
      maxnpet=2, tag='small regional sphere exact', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(6,5,6,4,1.,1.,1.,1.,-165.,30.,-165.,30., &
      maxnpet=2, tag='small regional sphere exact negative lon', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(5,6,5,6,2.,1.,1.,1.,-165.,30.,-165.,30., &
      maxnpet=2, tag='small regional sphere contain', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(10,20,20,20,1.,1.,0.5,1.,-165.,30.,-165.,30., &
      maxnpet=8, tag='medium regional sphere cut B1', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(10,20,20,20,1.,1.,0.6,1.,-165.,30.,-165.,30., &
      maxnpet=8, tag='medium regional sphere overlap', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(10,20,20,20,1.,1.,0.6,1.,-165.,30.,-166.,30., &
      maxnpet=8, tag='medium regional sphere overlap2', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(40,40,80,80,1.,1.,0.6,0.6,-165.,30.,-168.,25., &
      tag='large regional sphere overlap B2', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(80,80,40,40,0.6,0.6,1.,1.,-168.,25.,-165.,30., &
      tag='reverse large regional sphere overlap', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(45,45,90,90,8.,4.,4.,2.,-180.,-90.,0.,-90., &
      scheme=ESMF_REGRID_SCHEME_FULL3D, tag='large full sphere cut C1', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, full spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(90,90,45,45,4.,2.,8.,4.,0.,-90.,-180.,-90., &
      scheme=ESMF_REGRID_SCHEME_FULL3D, &
      tag='reverse large full sphere cut', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, full spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(60,60,90,90,6.,3.,4.,2.,-180.,-90.,0.,-90., &
      scheme=ESMF_REGRID_SCHEME_FULL3D, tag='large full sphere latlonclip C2', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, full spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest
    call test_regrid2xgSph(90,90,60,60,4.,2.,6.,3.,0.,-90.,-180.,-90., &
      scheme=ESMF_REGRID_SCHEME_FULL3D, &
      tag='reverse large full sphere latlonclip', rc=rc)
    write(failMsg, *) ""
    write(name, *) "Regrid then create xgrid and regrid through xgrid, full spherical grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"

!------------------------------------------------------------------------
  subroutine test_regrid2xg_online(atm_nx, atm_ny, ocn_nx, ocn_ny, atm_dx, atm_dy, &
    ocn_dx, ocn_dy, atm_sx, atm_sy, ocn_sx, ocn_sy, tag, maxnpet, minnpet, indexflag, rc)
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny, ocn_nx, ocn_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy, ocn_dx, ocn_dy
    real(ESMF_KIND_R4), intent(in), optional  :: atm_sx, atm_sy, ocn_sx, ocn_sy
    character(len=*), intent(in), optional    :: tag
    integer, intent(in) , optional            :: maxnpet, minnpet
    type(ESMF_INDEX_FLAG), intent(in), optional :: indexflag
    integer, intent(out), optional            :: rc

    ! local variables
    type(ESMF_Grid)                 :: grid_atm, grid_ocn
    type(ESMF_Field)                :: f_atm, f_ocn, f_xgrid, f_tmp
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
    real(ESMF_KIND_R8), pointer     :: srcAreaPtr(:,:), dstAreaPtr(:,:)
    type(ESMF_RouteHandle)          :: rh_a2o, rh_o2a, rh_a2x, rh_x2o, rh_o2x, rh_x2a
    type(ESMF_DistGrid)             :: distgridM, distgrid1, distgrid2
    type(ESMF_XGridSpec)            :: sparseMat(1)

    type(ESMF_Field)                :: fa_atm, fa_ocn, fa_xgrid
    type(ESMF_Field)                :: aa_atm, aa_ocn, aa_xgrid
    type(ESMF_Field)                :: srcFrac, dstFrac, srcArea, dstArea
    type(ESMF_Mesh)                 :: mesh_atm, mesh_ocn, mesh_xgrid
    real(ESMF_KIND_R8)              :: srcsum(3), allsrcsum(3), dstFlux_reg, dstFlux, error
    integer                         :: eleCount, totCount(1)
    
    type(ESMF_VM)   :: vm

    localrc = ESMF_SUCCESS
    if(present(rc)) rc = ESMF_SUCCESS

    !------------------------------------
    ! Prepare for the calculation
    !------------------------------------
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

    !------------- ATM ------------------
    ! atm grid, horizontally decomposed
    !------------------------------------
    grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
      coordSys=ESMF_COORDSYS_CART, &
      indexflag=indexflag, &
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

    !------------- OCN ------------------
    ! ocn grid, horizontally decomposed
    !------------------------------------
    grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
      coordSys=ESMF_COORDSYS_CART, &
      indexflag=indexflag, &
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

    !------------------------------------
    ! build Fields on the Grids
    !------------------------------------
    f_atm = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldFill(f_atm, dataFillScheme="one", member=1, step=1, rc=localrc)
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

    srcArea = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    dstArea = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      routehandle=rh_a2o, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      srcFracField=srcFrac, dstFracField=dstFrac, & 
      factorIndexList=indices, factorList=weights, rc=localrc)
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
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      routehandle=rh_o2a, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
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
    ! Call into online XGrid generation
    !----------------------------------------------------
    xgrid = ESMF_XGridCreate(sideAGrid=(/grid_atm/), sideBGrid=(/grid_ocn/), &
        storeOverlay=.false.,&
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    f_tmp = ESMF_FieldCreate(xgrid, xgridside=ESMF_XGRIDSIDE_A, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldDestroy(f_tmp)

    ! make sure serialize and deserialize works
    call checkProxy(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! useful statistics
    call ESMF_XGridGet(xgrid, localDE=0, elementCount=eleCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_VMAllReduce(vm, (/eleCount/), totCount, 1, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) write(*, '(A, I4)') 'Total # of XGrid cells: ', totCount(1)

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

    call ESMF_FieldRegridGetArea(srcArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(srcArea, farrayPtr=srcAreaPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridGetArea(dstArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(dstArea, farrayPtr=dstAreaPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    mesh_atm = ESMF_GridToMesh(grid_atm, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    mesh_ocn = ESMF_GridToMesh(grid_ocn, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !----------------------------------------------------
    ! Compute flux integrals
    !----------------------------------------------------
    call init_src(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if(lpet == 0) write(*, '(A, E17.10)') 'Total src flux: ', sum(atm)*atm_dx*atm_dy
    !if(atm_nx == 80 .or. (atm_nx == 4 .and. atm_ny == 6)) then
    !  call display_flux2D(atm, srcAreaPtr, srcFracPtr)
    !  call ESMF_FieldWrite(f_atm, file='src.nc', rc=localrc)
    !  if (ESMF_LogFoundError(localrc, &
    !      ESMF_ERR_PASSTHRU, &
    !      ESMF_CONTEXT, rcToReturn=rc)) return
    !  if(lpet == 0) write(*, '(A, E17.10)') 'Total src flux: ', sum(atm)*atm_dx*atm_dy
    !endif                getline(cin, cmd);
    !  error = 0.
    !  write(*, '(A,4I5)') 'bounds: ', lbound(coordX,1), ubound(coordX,1), &
    !    lbound(coordY,1), ubound(coordY,1)
    !  do i = lbound(coordX,1), ubound(coordX,1)
    !    do j = lbound(coordY,1), ubound(coordY,1)
    !      error = error + at                getline(cin, cmd);
    !    enddo
    !  enddo
    !  if(lpet == 0) print *, ' src flux: ', error*atm_dx*atm_dy

    ! X center 
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
        srcsum(1) = srcsum(1) + atm(i,j)*srcAreaPtr(i,j)*srcFracPtr(i,j)
        srcsum(2) = srcsum(2) +          srcAreaPtr(i,j)*srcFracPtr(i,j)
        srcsum(3) = srcsum(3) +          srcAreaPtr(i,j)
      enddo
    enddo
    call ESMF_VMAllReduce(vm, srcsum, allsrcsum, 3, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' src flux and area: ', allsrcsum
    call compute_flux2D(vm, atm, srcAreaPtr, srcFracPtr, allsrcsum, rc=localrc)
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
    call compute_flux2D(vm, ocn, dstAreaPtr, dstFracPtr, allsrcsum, dstflux=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area from direct Regrid: ', allsrcsum
    dstFlux_reg = allsrcsum(1)
    call ESMF_FieldRegrid(srcField=f_ocn, dstField=f_atm, routehandle=rh_o2a, &
      zeroregion=ESMF_REGION_SELECT, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, atm, srcAreaPtr, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area from direct Regrid: ', allsrcsum

    !----------------------------------------------------
    ! compute regrid routehandle
    !----------------------------------------------------
    !call ESMF_LogWrite(tag, ESMF_LOGMSG_INFO)
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
    call compute_flux2D(vm, ocn, dstAreaPtr, dstFracPtr, allsrcsum, dstflux=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area: ', allsrcsum
    dstFlux = allsrcsum(1)

    error = abs(dstFlux - dstFlux_reg)/abs(dstFlux_reg) 
    if(lpet == 0) write(*,'(A,3E18.10)') 'Verify: ', dstFlux_reg, dstFlux, error
    if(error > 1.e-10) then
      print *, 'Regrid through XGrid doesnot agree with direct Regrid'
      if(present(rc)) rc = ESMF_RC_NOT_VALID
      return
    endif

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
    call ESMF_FieldRegrid(f_ocn, f_xgrid, routehandle=rh_o2x, &
      zeroregion=ESMF_REGION_SELECT, rc=localrc)
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

    !call display_flux2D(atm, srcAreaPtr, srcFracPtr)
    call ESMF_FieldRegrid(f_xgrid, f_atm, routehandle=rh_x2a, &
      zeroregion=ESMF_REGION_SELECT, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'x2a atm', lpet, atm
    call compute_flux2D(vm, atm, srcAreaPtr, srcFracPtr, allsrcsum, dstflux=.true.,rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area: ', allsrcsum
    !call display_flux2D(atm, srcAreaPtr, srcFracPtr)

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

!BOB    deallocate(xArea, xFrac)

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
    ocn_dx, ocn_dy, atm_sx, atm_sy, ocn_sx, ocn_sy, tag, maxnpet, minnpet, scheme, rc)
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny, ocn_nx, ocn_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy, ocn_dx, ocn_dy
    real(ESMF_KIND_R4), intent(in), optional  :: atm_sx, atm_sy, ocn_sx, ocn_sy
    character(len=*), intent(in), optional    :: tag
    integer, intent(in) , optional            :: maxnpet, minnpet
    integer, intent(in) , optional            :: scheme
    integer, intent(out), optional            :: rc

    ! local variables
    type(ESMF_Grid)                 :: grid_atm, grid_ocn
    type(ESMF_Field)                :: f_atm, f_ocn, f_xgrid, f_error, f_ocn_dir
    real(ESMF_KIND_R8)              :: startx, starty
    integer                         :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer     :: weights(:)
    integer(ESMF_KIND_I4), pointer  :: indices(:,:)
    real(ESMF_KIND_R8), pointer     :: coordX(:,:), coordY(:,:)
    type(ESMF_XGrid)                :: xgrid

    type(ESMF_XGridSpec)            :: sparseMatA2X(1)
    integer                         :: gn(2), simax, dimax, l_minnpet, l_maxnpet
    real(ESMF_KIND_R8), pointer     :: atm(:,:), ocn(:,:), ptr_error(:,:), exf(:), xArea(:), xFrac(:)
    real(ESMF_KIND_R8), pointer     :: ocn_dir(:,:)
    real(ESMF_KIND_R8), pointer     :: srcFracPtr(:,:), dstFracPtr(:,:)
    real(ESMF_KIND_R8), pointer     :: srcAreaPtr(:,:), dstAreaPtr(:,:)
    type(ESMF_RouteHandle)          :: rh_a2o, rh_o2a, rh_a2x, rh_x2o, rh_o2x, rh_x2a
    type(ESMF_DistGrid)             :: distgridM, distgrid1, distgrid2
    type(ESMF_XGridSpec)            :: sparseMat(1)

    type(ESMF_Field)                :: fa_atm, fa_ocn, fa_xgrid
    type(ESMF_Field)                :: aa_atm, aa_ocn, aa_xgrid, fieldM
    type(ESMF_Field)                :: srcFrac, dstFrac, srcArea, dstArea
    type(ESMF_Mesh)                 :: mesh_atm, mesh_ocn, mesh_xgrid, meshM
    real(ESMF_KIND_R8)              :: srcsum(3), allsrcsum(3), scale
    real(ESMF_KIND_R8)              :: dstFlux_reg, dstFlux, totalXArea, totalSrcArea, error
    integer                         :: l_scheme, eleCount

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

    l_scheme = ESMF_REGRID_SCHEME_REGION3D
    if(present(scheme)) l_scheme = scheme

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
    if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
      grid_atm = ESMF_GridCreate1PeriDim(maxIndex=(/atm_nx, atm_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
        regDecomp=(/npet, 1/), &
        rc=localrc)
    else
      grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
        regDecomp=(/npet, 1/), &
        rc=localrc)
    endif 
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
    print *, 'startx: ', startx, lbound(coordX, 1), ubound(coordX, 1), 'coordX: ', coordX(:,1)
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
    if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
      grid_ocn = ESMF_GridCreate1PeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
        regDecomp=(/npet, 1/), &
        rc=localrc)
    else
      grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
        regDecomp=(/npet, 1/), &
        rc=localrc)
    endif 
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

    f_error = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    f_ocn = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    f_ocn_dir = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
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

    srcArea = ESMF_FieldCreate(grid_atm, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    dstArea = ESMF_FieldCreate(grid_ocn, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !----------------------------------------------------
    ! Call RegridStore here to compute SMM weights and indices and direct RHs
    !----------------------------------------------------

    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      routehandle=rh_a2o, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      srcFracField=srcFrac, dstFracField=dstFrac, & 
      factorIndexList=indices, factorList=weights, rc=localrc)
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
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
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
    xgrid = ESMF_XGridCreate(sideAGrid=(/grid_atm/), sideBGrid=(/grid_ocn/), &
        storeOverlay=.true.,&
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call checkProxy(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Query the XGrid for the overlay mesh
    call ESMF_XGridGet(xgrid, mesh=meshM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create a field on the overlay mesh
    fieldM = ESMF_FieldCreate(mesh=meshM, typekind=ESMF_TYPEKIND_R8, &
        rc=localrc)
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
    !----------------------------------------------------

    !----------------------------------------------------
    ! set up src flux
    !----------------------------------------------------
    call ESMF_FieldGet(f_atm, farrayPtr=atm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(f_error, farrayPtr=ptr_error, rc=localrc)
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
    call ESMF_FieldGet(f_ocn_dir, farrayPtr=ocn_dir, rc=localrc)
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

    call ESMF_XGridGet(xgrid, elementCount=eleCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    write(*, *) 'eleCount = ', eleCount, 'size(fptr) = ', size(exf)
    if(eleCount /= size(exf)) then
      print *, 'elementCount is not equal to size(exf)'
      if(present(rc)) rc = ESMF_RC_NOT_VALID
      return
    endif

    call ESMF_FieldRegridGetArea(srcArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(srcArea, farrayPtr=srcAreaPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_FieldRegridGetArea(dstArea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(dstArea, farrayPtr=dstAreaPtr, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    mesh_atm = ESMF_GridToMesh(grid_atm, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    mesh_ocn = ESMF_GridToMesh(grid_ocn, &
      ESMF_STAGGERLOC_CORNER, 0, .false., &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

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
        srcsum(1) = srcsum(1) + atm(i,j)*srcAreaPtr(i,j)*srcFracPtr(i,j)
        srcsum(2) = srcsum(2) +          srcAreaPtr(i,j)*srcFracPtr(i,j)
        srcsum(3) = srcsum(3) +          srcAreaPtr(i,j)
      enddo
    enddo
    call ESMF_VMAllReduce(vm, srcsum, allsrcsum, 3, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, lpet, ' src flux and area: ', atm
    scale = 3.1415926535897932/180.
    totalSrcArea = allsrcsum(3)
    if(lpet == 0) print *, ' src flux and area: ', allsrcsum, &
      (sin(scale*(atm_sy+atm_ny*atm_dy))-sin(scale*atm_sy))*(atm_nx*atm_dx)*scale
    call compute_flux2D(vm, atm, srcAreaPtr, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' src flux and area sub: ', allsrcsum

    !----------------------------------------------------
    ! call direct regrid
    !----------------------------------------------------
    !call display_flux2D(atm, srcAreaPtr, srcFracPtr)
    call ESMF_FieldRegrid(srcField=f_atm, dstField=f_ocn, routehandle=rh_a2o, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, ocn, dstAreaPtr, dstFracPtr, allsrcsum, dstflux=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area from direct Regrid: ', allsrcsum
    dstFlux_reg = allsrcsum(1)
    ocn_dir = ocn

    !call display_flux2D(ocn, dstAreaPtr, dstFracPtr)
    call ESMF_FieldRegrid(srcField=f_ocn, dstField=f_atm, routehandle=rh_o2a, &
      zeroregion=ESMF_REGION_SELECT, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call compute_flux2D(vm, atm, srcAreaPtr, srcFracPtr, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area from direct Regrid: ', allsrcsum

    call init_src_sph(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, ptr_error, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call diff_ptr_sph(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, ptr_error, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
    !  call ESMF_FieldWrite(f_error, file='error.nc', rc=localrc)
    !  if (ESMF_LogFoundError(localrc, &
    !      ESMF_ERR_PASSTHRU, &
    !      ESMF_CONTEXT, rcToReturn=rc)) return
    !endif

    !----------------------------------------------------
    ! compute xgrid regrid routehandle
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
    totalXArea = allsrcsum(3)
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
    call compute_flux2D(vm, ocn, dstAreaPtr, dstFracPtr, allsrcsum, dstflux=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' dst flux and area: ', allsrcsum
    dstFlux = allsrcsum(1)

    !----------------------------------------------------
    ! Verify
    !----------------------------------------------------
    call diff_ptr_sph(grid_ocn, coordX, coordY, ocn_nx, ocn_ny, ocn_dx, ocn_dy, ocn, ocn_dir, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
    !  call ESMF_FieldWrite(f_ocn_dir, file='xgrid_regrid_diff.nc', rc=localrc)
    !  if (ESMF_LogFoundError(localrc, &
    !      ESMF_ERR_PASSTHRU, &
    !      ESMF_CONTEXT, rcToReturn=rc)) return
    !endif
    error = abs(dstFlux - dstFlux_reg)/abs(dstFlux_reg) 
    if(lpet == 0) write(*,'(A,4E18.10)') 'Verify: ', dstFlux_reg, dstFlux, error, abs(totalXArea-totalSrcArea)/totalSrcArea
    if(error > 1.e-4) then
      print *, 'Regrid through XGrid doesnot agree with direct Regrid'
      if(present(rc)) rc = ESMF_RC_NOT_VALID
      return
    endif

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
    call ESMF_FieldRegrid(f_ocn, f_xgrid, routehandle=rh_o2x, &
      zeroregion=ESMF_REGION_SELECT, rc=localrc)
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

    !call display_flux2D(atm, srcAreaPtr, srcFracPtr)
    call ESMF_FieldRegrid(f_xgrid, f_atm, routehandle=rh_x2a, &
      zeroregion=ESMF_REGION_SELECT, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, 'x2a atm', lpet, atm
    call compute_flux2D(vm, atm, srcAreaPtr, srcFracPtr, allsrcsum, dstflux=.true., rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' recomputed src flux and area: ', allsrcsum
    !call display_flux2D(atm, srcAreaPtr, srcFracPtr)

    call init_src_sph(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, ptr_error, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call diff_ptr_sph(grid_atm, coordX, coordY, atm_nx, atm_ny, atm_dx, atm_dy, atm, ptr_error, rc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
    !  call ESMF_FieldWrite(f_error, file='error1.nc', rc=localrc)
    !  if (ESMF_LogFoundError(localrc, &
    !      ESMF_ERR_PASSTHRU, &
    !      ESMF_CONTEXT, rcToReturn=rc)) return
    !endif

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

!BOB    deallocate(xArea, xFrac)

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

  !------------------------------------------------------------------------

  subroutine compute_flux1D(vm, flux_density, area, fraction, allsum, rc)
    type(ESMF_VM), intent(in)        :: vm
    real(ESMF_KIND_R8), pointer      :: flux_density(:) 
    real(ESMF_KIND_R8), pointer      :: area(:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:) 
    real(ESMF_KIND_R8), intent(out)  :: allsum(3)
    integer, intent(out), optional   :: rc

    real(ESMF_KIND_R8)               :: sum(3)
    integer                          :: i,j, localrc

    if(present(rc)) rc = ESMF_SUCCESS

    sum = 0.
    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      sum(1) = sum(1) + flux_density(i)*area(i)*fraction(i)
      sum(2) = sum(2) +                 area(i)*fraction(i)
      sum(3) = sum(3) +                 area(i)
    enddo

    call ESMF_VMAllReduce(vm, sum, allsum, 3, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine compute_flux1D

  subroutine compute_flux2D(vm, flux_density, area, fraction, allsum, dstflux, rc)
    type(ESMF_VM), intent(in)        :: vm
    real(ESMF_KIND_R8), pointer      :: flux_density(:,:) 
    real(ESMF_KIND_R8), pointer      :: area(:,:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:,:) 
    real(ESMF_KIND_R8), intent(out)  :: allsum(3)
    logical, intent(in) , optional   :: dstflux
    integer, intent(out), optional   :: rc

    real(ESMF_KIND_R8)               :: sum(3)
    integer                          :: i,j, localrc, npet, lpet
    logical                          :: l_dstflux

    if(present(rc)) rc = ESMF_SUCCESS
    l_dstflux = .false.
    if(present(dstflux)) l_dstflux = dstflux

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  
    !if(lpet == 0) write(*, '(A, 4I5)') 'compute_flux2D bounds: ', &
    !  lbound(flux_density, 1), ubound(flux_density, 1), &
    !  lbound(flux_density, 2), ubound(flux_density, 2)

    sum = 0.
    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      do j = lbound(flux_density, 2), ubound(flux_density, 2)
        if(l_dstflux) then
          sum(1) = sum(1) + flux_density(i,j)*area(i,j)
        else
          sum(1) = sum(1) + flux_density(i,j)*area(i,j)*fraction(i,j)
        endif
        sum(2) = sum(2) +                 area(i,j)*fraction(i,j)
        sum(3) = sum(3) +                 area(i,j)
      enddo
    enddo

    call ESMF_VMAllReduce(vm, sum, allsum, 3, ESMF_REDUCE_SUM, rc=localrc)
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
      write(*,'(I3,I7,2F14.10)') lpet,i,flux_density(i), area(i)
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
  
    !if(lpet == 0) write(*, '(A, 4I5)') 'display_flux2D bounds: ', &
    !  lbound(flux_density, 1), ubound(flux_density, 1), &
    !  lbound(flux_density, 2), ubound(flux_density, 2)

    do i = lbound(flux_density, 1), ubound(flux_density, 1)
      do j = lbound(flux_density, 2), ubound(flux_density, 2)
        write(*,'(I3,2I5,3F14.10)') lpet, i,j,flux_density(i,j), area(i,j), fraction(i,j)
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

    ! X center 
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
        src(i,j) = 0.5*sin(coordX(i)/(nx*dx)*3*3.14159)*cos(coordY(j)&
        /(ny*dy)*5*3.14159)+0.5
        !src(i,j) = 2.
      enddo
    enddo

  end subroutine init_src

!------------------------------------------------------------------------

  subroutine init_src_sph(grid, coordX, coordY, nx, ny, dx, dy, src, rc)
    type(ESMF_Grid), intent(in)      :: grid
    real(ESMF_KIND_R8), pointer      :: coordX(:,:)
    real(ESMF_KIND_R8), pointer      :: coordY(:,:)
    integer, intent(in)              :: nx, ny
    real, intent(in)                 :: dx, dy
    real(ESMF_KIND_R8), pointer      :: src(:,:)
    integer, intent(out), optional   :: rc

    integer                          :: localrc, i,j,npet, lpet
    type(ESMF_VM)                    :: vm

    if(present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! X center 
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

    write(*, '(A, 5I5)') 'init_src_sph bounds: ', lpet, &
      lbound(src, 1), ubound(src, 1), &
      lbound(src, 2), ubound(src, 2)

    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX,2), ubound(coordX,2)
        !src(i,j) = 0.5*sin(coordX(i,j)/(nx*dx)*3*3.14159)*cos(coordY(i,j)&
        !/(ny*dy)*5*3.14159)+10.5
        src(i,j) = 2.
      enddo
    enddo

  end subroutine init_src_sph

!------------------------------------------------------------------------

  subroutine diff_ptr_sph(grid, coordX, coordY, nx, ny, dx, dy, src, error, rc)
    type(ESMF_Grid), intent(in)      :: grid
    real(ESMF_KIND_R8), pointer      :: coordX(:,:)
    real(ESMF_KIND_R8), pointer      :: coordY(:,:)
    integer, intent(in)              :: nx, ny
    real, intent(in)                 :: dx, dy
    real(ESMF_KIND_R8), pointer      :: src(:,:), error(:,:)
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
        error(i,j) = src(i,j) - error(i,j)
      enddo
    enddo

  end subroutine diff_ptr_sph

!------------------------------------------------------------------------

  subroutine checkProxy(xgrid, rc)
    type(ESMF_XGrid), intent(in)    :: xgrid
    integer, intent(out), optional  :: rc
    
    type(ESMF_XGrid)                :: xgrid1
    character, pointer              :: buffer(:)
    integer                         :: buff_length, offset, localrc

    if(present(rc)) rc = ESMF_SUCCESS
  
    ! Allocate serialization buffer

    buff_length = 1
    allocate (buffer(0:buff_length-1))
    offset = 0
    call ESMF_XGridSerialize(xgrid, buffer, buff_length, offset, &
        inquireflag=ESMF_INQUIREONLY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate (buffer)

    buff_length = offset
    allocate (buffer(0:buff_length-1))

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

end program ESMF_FieldRegridXGUTest
