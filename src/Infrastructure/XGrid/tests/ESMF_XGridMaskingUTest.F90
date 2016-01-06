! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_XGridMaskingUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_XGridMaskingUTest - Unit tests for Field Create and Get methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Create and Get unit tests.
! The companion folder Fieldsrc contains the definitions for the
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
 
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Create an XGrid in 2D
    print *, 'Starting test2d_masking_single'
    call test2d_masking_single(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid from 2 adjacent Grids with masking"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Create an XGrid in 3D
    print *, 'Starting test3d_masking_regional'
    call test3d_masking_regional(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid in 3D with Grid masking"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Create an XGrid in 3D
    print *, 'Starting test3d_masking_global'
    call test3d_masking_global(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid in 3D with global Grid masking"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(ESMF_SRCLINE)
  
contains 
#define ESMF_METHOD "ESMF_TESTS"

!------------------------------------------------------------------------
  subroutine test2d_masking_single(rc)
    integer, intent(out)                :: rc
    integer                             :: localrc, npet, lpet
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(1), sideB(1)

    type(ESMF_VM)                       :: vm
    type(ESMF_Field)                    :: srcField(2), dstField(1)

    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, localpet=lpet, petcount=npet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    sideA(1) = make_grid(4,4,1.,1.,0.,0.,&
      msx=3., mex=5., msy=0., mey=5., maskvalue=2, field=srcField(1),rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    sideB(1) = make_grid(4,8,1.,0.5,0.,0.,&
      msx=0., mex=1., msy=0., mey=5., maskvalue=2, field=dstField(1),rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !xgrid = ESMF_XGridCreate(sideA, sideB, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(sideAGrid=sideA, sideBGrid=sideB, &
      sideAMaskValues=(/2/), sideBMaskValues=(/2/), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange(xgrid, srcField(1:1), dstField(1:1), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(  &
      sideAGrid=(/make_grid(4,4,1.,1.,0.,0.,msx=3.,mex=4., msy=0., mey=4., &
          maskvalue=2, field=srcField(1), rc=localrc), &
        make_grid(4,4,1.,1.,0.,0.,msx=0.,mex=1., msy=0., mey=4., &
          maskvalue=2, field=srcField(2), rc=localrc)/), &
      sideBGrid=(/make_grid(8,8,1.,1.,0.,0.,field=dstField(1), rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)

    call flux_exchange(xgrid, srcField(1:2), dstField(1:1), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine test2d_masking_single

  subroutine test3d_masking_regional(rc)
    integer, intent(out)                :: rc
    integer                             :: localrc, npet
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)

    type(ESMF_VM)                       :: vm

    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petcount=npet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! partially overlap
    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(4,4,1.,1.,0.,0.,msx=3.,mex=4., msy=0., mey=4., maskvalue=2, rc=localrc), &
        make_grid_sph(4,4,1.,1.,0.,0.,msx=0.,mex=1., msy=0., mey=4., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(8,8,1.,1.,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Grids near north pole
    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(4,4,1.,1.,0.,86.,msx=3.,mex=4., msy=-90., mey=90., maskvalue=2, rc=localrc), &
        make_grid_sph(4,4,1.,1.,0.,86.,msx=0.,mex=1., msy=-90., mey=90., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(8,8,1.,1.,0.,82.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Grids near south pole
    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(4,4,1.,1.,0.,-90.,msx=3.,mex=4., msy=-90., mey=90., maskvalue=2, rc=localrc), &
        make_grid_sph(4,4,1.,1.,0.,-90.,msx=0.,mex=1., msy=-90., mey=90., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(8,8,1.,1.,0.,-90.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(40,40,1.,1.,0.,0.,msx=30.,mex=40., msy=0., mey=40., maskvalue=2, rc=localrc), &
        make_grid_sph(40,40,1.,1.,0.,0.,msx=0.,mex=10., msy=0., mey=40., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(80,80,1.,1.,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(40,40,1.,1.,0.,0.,rc=localrc), &
        make_grid_sph(40,40,1.,1.,20.5,30.5,rc=localrc)/), &
      sideBGrid=(/make_grid_sph(80,80,1.,1.,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! With masking in XGrid
    ! partially overlap
    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(4,4,1.,1.,0.,0.,msx=3.,mex=4., msy=0., mey=4., maskvalue=2, rc=localrc), &
        make_grid_sph(4,4,1.,1.,0.,0.,msx=0.,mex=1., msy=0., mey=4., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(8,8,1.,1.,0.,0.,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Grids near south pole
    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(4,4,1.,1.,0.,-90.,msx=3.,mex=4., msy=-90., mey=90., maskvalue=2, rc=localrc), &
        make_grid_sph(4,4,1.,1.,0.,-90.,msx=0.,mex=1., msy=-90., mey=90., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(8,8,1.,1.,0.,-90.,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(40,40,1.,1.,0.,0.,msx=30.,mex=40., msy=0., mey=40., maskvalue=2, rc=localrc), &
        make_grid_sph(40,40,1.,1.,0.,0.,msx=0.,mex=10., msy=0., mey=40., maskvalue=2, rc=localrc)/), &
      sideBGrid=(/make_grid_sph(80,80,1.,1.,0.,0.,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/make_grid_sph(40,40,1.,1.,0.,0.,rc=localrc), &
        make_grid_sph(40,40,1.,1.,20.5,30.5,rc=localrc)/), &
      sideBGrid=(/make_grid_sph(80,80,1.,1.,0.,0.,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !xgrid = ESMF_XGridCreate((/make_grid_sph(40,90,1.,2.,0.,-90.,rc=localrc), &
    !    make_grid_sph(40,60,1.,3.,20.5,-90.,rc=localrc)/), &
    !  (/make_grid_sph(80,80,1.,1.,0.,0.,rc=localrc)/), &
    !  rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return
    !call flux_exchange_sph(xgrid, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine test3d_masking_regional

  subroutine test3d_masking_global(rc)
    integer, intent(out)                :: rc
    integer                             :: localrc, npet
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)

    type(ESMF_VM)                       :: vm

    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VMGet(vm, petcount=npet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/ &
        make_grid_sph(12,9,30.,20.,0.,-90.,msx=0., mex=160., msy=-90., mey=90., &
          maskvalue=3, scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc), &
        make_grid_sph(9,6,40.,30.,0.,-90., msx=200., mex=360., msy=-90., mey=90., &
          maskvalue=4, scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc)/), &
      sideBGrid=(/make_grid_sph(12,18,30.,10.,0.,-90.,scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/ &
        make_grid_sph(12,9,30.,20.,0.,-90.,msx=0., mex=160., msy=-90., mey=90., &
          maskvalue=3, scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc), &
        make_grid_sph(9,9,40.,20.,0.,-90., msx=200., mex=360., msy=-90., mey=90., &
          maskvalue=4, scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc)/), &
      sideBGrid=(/make_grid_sph(12,18,30.,10.,0.,-90.,scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    xgrid = ESMF_XGridCreate(&
      sideAGrid=(/ &
        make_grid_sph(120,90,3.,2.,0.,-90.,msx=0., mex=160., msy=-90., mey=90., &
          maskvalue=3, scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc), &
        make_grid_sph(45,120,8.,1.5,0.,-90., msx=200., mex=360., msy=-90., mey=90., &
          maskvalue=4, scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc)/), &
      sideBGrid=(/make_grid_sph(120,120,3.,1.5,0.,-90.,scheme=ESMF_REGRID_SCHEME_FULL3D,rc=localrc)/), &
      sideAMaskValues=(/2,3,4/), sideBMaskValues=(/2,3,4/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call flux_exchange_sph(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine test3d_masking_global

  !------------------------------------------------------------------------
  ! Utility methods
  !------------------------------------------------------------------------

  function make_grid(atm_nx, atm_ny, atm_dx, atm_dy, atm_sx, atm_sy, &
    msx, msy, mex, mey, maskvalue, field, rc)

    ! return value
    type(ESMF_Grid)                           :: make_grid
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy
    real(ESMF_KIND_R4), intent(in)            :: atm_sx, atm_sy
    real(ESMF_KIND_R4), intent(in), optional  :: msx, msy, mex, mey
    integer(ESMF_KIND_I4), intent(in), optional :: maskvalue
    type(ESMF_Field), intent(out), optional   :: field
    integer, intent(out), optional            :: rc

    ! local variables
    integer                                   :: localrc, i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:), coordY(:)
    integer(ESMF_KIND_I4), pointer            :: maskptr(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty

    make_grid = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
      coordSys=ESMF_COORDSYS_CART, &
      indexflag=ESMF_INDEX_GLOBAL, &
      coordDep1=(/1/), &
      coordDep2=(/2/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridAddItem(make_grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
           itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_GridGetItem(make_grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskptr, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
    endif
    maskptr = 0

    call ESMF_GridAddCoord(make_grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridAddCoord(make_grid, staggerloc=ESMF_STAGGERLOC_CORNER, &
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
    call ESMF_GridGetCoord(make_grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      coordX(i) = startx + atm_dx/2. + (i-1)*atm_dx
      if(present(maskvalue)) then
        if((coordX(i) .ge. msx .and. coordX(i) .le. mex)) then
          maskptr(i,:) = maskvalue
        else
          maskptr(i,:) = 0
        endif
      endif
    enddo
    !print *, 'coordX: ', coordX
    ! X corner
    call ESMF_GridGetCoord(make_grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      coordX(i) = startx + (i-1)*atm_dx
    enddo
    !print *, 'startx: ', startx, lbound(coordX, 1), 'coordX: ', coordX
    ! Y center
    call ESMF_GridGetCoord(make_grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordY,1), ubound(coordY,1)
      coordY(i) = starty + atm_dy/2. + (i-1)*atm_dy
      if(present(maskvalue)) then
        if(.not. (coordY(i) .ge. msy .and. coordY(i) .le. mey)) then
          maskptr(:,i) = 0
        endif
      endif
    enddo
    ! Y corner
    call ESMF_GridGetCoord(make_grid, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordY,1), ubound(coordY,1)
      coordY(i) = starty + (i-1)*atm_dy
    enddo

    if(present(field)) then
      field = ESMF_FieldCreate(make_grid, typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(rc)) rc = ESMF_SUCCESS

  end function make_grid

  function make_grid_sph(atm_nx, atm_ny, atm_dx, atm_dy, atm_sx, atm_sy, &
    msx, msy, mex, mey, maskvalue, tag, scheme, rc)

    ! return value
    type(ESMF_Grid)                           :: make_grid_sph
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy
    real(ESMF_KIND_R4), intent(in)            :: atm_sx, atm_sy
    real(ESMF_KIND_R4), intent(in), optional  :: msx, msy, mex, mey
    integer(ESMF_KIND_I4), intent(in), optional :: maskvalue
    character(len=*), intent(in), optional    :: tag
    integer, intent(in) , optional            :: scheme
    integer, intent(out), optional            :: rc

    ! local variables
    integer                                   :: localrc, i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:,:), coordY(:,:)
    integer(ESMF_KIND_I4), pointer            :: maskptr(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty
    integer                                   :: l_scheme

    l_scheme = ESMF_REGRID_SCHEME_REGION3D
    if(present(scheme)) l_scheme = scheme

    if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
      make_grid_sph = ESMF_GridCreate1PeriDim(maxIndex=(/atm_nx, atm_ny/), &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        indexflag=ESMF_INDEX_GLOBAL, &
        !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
        !regDecomp=(/npet, 1/), &
        rc=localrc)
    else
      make_grid_sph = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
        !regDecomp=(/npet, 1/), &
        rc=localrc)
    endif 
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridAddItem(make_grid_sph, staggerloc=ESMF_STAGGERLOC_CENTER, &
           itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_GridGetItem(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                          itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskptr, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
    endif
    maskptr = 0

    call ESMF_GridAddCoord(make_grid_sph, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridAddCoord(make_grid_sph, staggerloc=ESMF_STAGGERLOC_CORNER, &
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
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y center
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordY, 2), ubound(coordY, 2)
        coordX(i,j) = startx + atm_dx/2. + (i-1)*atm_dx
        coordY(i,j) = starty + atm_dy/2. + (j-1)*atm_dy
      enddo
    enddo
    if(present(maskvalue) .and. present(msx) .and. present(mex) .and. &
       present(msy) .and. present(mey)) then
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordY, 2), ubound(coordY, 2)
          if((coordX(i,j) .ge. msx .and. coordX(i,j) .le. mex) .and. &
             (coordY(i,j) .ge. msy .and. coordY(i,j) .le. mey) ) then
            maskptr(i,j) = maskvalue
          endif
        enddo
      enddo
    endif
    !print *, 'startx: ', startx, lbound(coordX, 1), ubound(coordX, 1), 'coordX: ', coordX(:,1)
    ! X corner
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Y corner
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + (i-1)*atm_dx
      enddo
    enddo
    do i = lbound(coordY,1), ubound(coordY,1)
      do j = lbound(coordY, 2), ubound(coordY, 2)
        coordY(i,j) = starty + (j-1)*atm_dy
      enddo
    enddo

    if(present(rc)) rc = ESMF_SUCCESS

  end function make_grid_sph

  subroutine flux_exchange(xgrid, srcField, dstField, rc)

    type(ESMF_XGrid), intent(inout)           :: xgrid
    type(ESMF_Field), intent(inout)           :: srcField(:)
    type(ESMF_Field), intent(inout)           :: dstField(:)
    integer, intent(out), optional            :: rc


    integer                                   :: localrc, i, j, nsrc, ndst, lpet, npet
    type(ESMF_Field)                          :: f_xgrid
    type(ESMF_Grid), allocatable              :: srcGrid(:)
    type(ESMF_Field), allocatable             :: srcFrac(:), srcArea(:)
    type(ESMF_Grid), allocatable              :: dstGrid(:)
    type(ESMF_Field), allocatable             :: dstFrac(:), dstArea(:)
    type(ESMF_Field), allocatable             :: srcFrac2(:), dstFrac2(:)
    type(ESMF_RouteHandle), allocatable       :: s2d_rh(:,:)
    type(ESMF_RouteHandle), allocatable       :: d2s_rh(:,:)
    type(ESMF_RouteHandle), allocatable       :: s2x_rh(:), x2s_rh(:)
    type(ESMF_RouteHandle), allocatable       :: d2x_rh(:), x2d_rh(:)
    real(ESMF_KIND_R8), pointer               :: src(:,:), dst(:,:), exf(:)
    real(ESMF_KIND_R8), pointer               :: src_area(:,:), dst_area(:,:), exf_area(:)
    real(ESMF_KIND_R8), pointer               :: src_frac(:,:), dst_frac(:,:), exf_frac(:)
    real(ESMF_KIND_R8), pointer               :: src_frac2(:,:), dst_frac2(:,:)
    integer(ESMF_KIND_I4), pointer            :: src_mask(:,:), dst_mask(:,:)
    real(ESMF_KIND_R8)                        :: srcsum(3), allsrcsum(3), scale=2.0, exf_tarea, exf_tflux
    type(ESMF_VM)                             :: vm

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_VMGet(vm, localpet=lpet, petCount=npet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !------------------------------------
    ! build Fields on the Grids
    !------------------------------------

    ! create a Field on the xgrid
    f_xgrid = ESMF_FieldCreate(xgrid=xgrid, TYPEKIND=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(f_xgrid, farrayPtr=exf, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    nsrc = size(srcField)
    ndst = size(dstField)
    allocate(srcGrid(nsrc), srcFrac(nsrc), srcFrac2(nsrc), srcArea(nsrc))
    allocate(dstGrid(ndst), dstFrac(ndst), dstFrac2(ndst), dstArea(ndst))
    do i = 1, size(srcField)
      call ESMF_FieldGet(srcField(i), grid=srcGrid(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcFrac(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcFrac2(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcArea(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridGetArea(srcArea(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    do i = 1, size(dstField)
      call ESMF_FieldGet(dstField(i), grid=dstGrid(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dstFrac(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dstFrac2(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstFrac(i), localDe=0, farrayPtr=dst_frac, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dstArea(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridGetArea(dstArea(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    allocate(s2d_rh(size(srcField), size(dstField)), d2s_rh(size(dstField), size(srcField)))
    allocate(s2x_rh(size(srcField)), x2s_rh(size(srcField)))
    allocate(d2x_rh(size(dstField)), x2d_rh(size(dstField)))

    do i = 1, size(srcField)
      do j = 1, size(dstField)
        call ESMF_FieldRegridStore(srcField=srcField(i), dstField=dstField(j), &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          routehandle=s2d_rh(i,j), &
          unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
          srcFracField=srcFrac(i), dstFracField=dstFrac(j), & 
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRegridStore(srcField=dstField(j), dstField=srcField(i), &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          routehandle=d2s_rh(j,i), &
          unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
          srcFracField=dstFrac(j), dstFracField=srcFrac(i), & 
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    enddo

    do i = 1, size(srcField)
      call ESMF_FieldRegridStore(xgrid, srcField=srcField(i), dstField=f_xgrid, &
        routehandle=s2x_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridStore(xgrid, srcField=f_xgrid, dstField=srcField(i), &
        routehandle=x2s_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    do i = 1, size(dstField)
      call ESMF_FieldRegridStore(xgrid, srcField=dstField(i), dstField=f_xgrid, &
        routehandle=d2x_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridStore(xgrid, srcField=f_xgrid, dstField=dstField(i), &
        routehandle=x2d_rh(i), dstFracField=dstFrac(i), dstMergeFracField=dstFrac2(i), &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    !----------------------------------------------------
    ! Compute flux integrals
    ! Initialize src flux to constant
    !----------------------------------------------------
    exf = 0.
    do i = 1, size(srcField)
      call ESMF_FieldGet(srcField(i), farrayPtr=src, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      src = scale*i
    enddo

    ! Perform flux exchange
    do i = 1, size(srcField)
      call ESMF_FieldRegrid(srcField=srcField(i), dstField=f_xgrid, &
        routehandle=s2x_rh(i), zeroregion=ESMF_REGION_EMPTY, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! make sure flux is conserved on XGrid
    allocate(exf_area(lbound(exf,1):ubound(exf,1)))
    allocate(exf_frac(lbound(exf,1):ubound(exf,1)))
    call ESMF_XGridGet(xgrid, area=exf_area, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    exf_frac = 1.0
    call compute_flux1D(vm, exf, exf_area, exf_frac, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' xgrid flux and area: ', allsrcsum
    ! the following no longer holds with masking, only check xgrid flux with dst field flux
    !if(abs(allsrcsum(1) - allsrcsum(2)*scale) .gt. 1.e-10) then
    !  call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
    !     msg="- inconsistent flux and area found", &
    !     ESMF_CONTEXT, rcToReturn=rc) 
    !  return
    !endif
    exf_tflux = allsrcsum(1)
    exf_tarea = allsrcsum(2)
    deallocate(exf_area, exf_frac)

    !make sure flux is conserved on dst Fields
    do i = 1, size(dstField)
      call ESMF_FieldRegrid(srcField=f_xgrid, dstField=dstField(i), &
        routehandle=x2d_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstField(i), farrayPtr=dst, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ! fraction
      call ESMF_FieldGet(dstFrac(i), farrayPtr=dst_frac, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstFrac2(i), farrayPtr=dst_frac2, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ! area
      call ESMF_FieldGet(dstArea(i), farrayPtr=dst_area, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ! mask
      call ESMF_GridGetItem(dstGrid(i), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dst_mask, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif

      call compute_flux2D(vm, dst, dst_area, dst_frac, dst_frac2, allsrcsum, dst_mask, dstflux=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(lpet == 0) print *, 'dst flux and area: ', allsrcsum
      if((abs(exf_tarea - allsrcsum(2)) .gt. 1.e-10) .or. &
         (abs(exf_tflux - allsrcsum(1)) .gt. 1.e-10)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- inconsistent flux and area found", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    enddo

    do i = 1, size(dstField)
      call ESMF_FieldRegrid(srcField=dstField(i), dstField=f_xgrid, &
        routehandle=d2x_rh(i), &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    do i = 1, size(srcField)
      call ESMF_FieldRegrid(srcField=f_xgrid, dstField=srcField(i), &
        routehandle=x2s_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(srcField(i), farrayPtr=src, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    !----------------------------------------------------
    ! clean up
    !----------------------------------------------------
    do i = 1, size(srcField)
      call ESMF_FieldDestroy(srcField(i), rc=localrc)
      call ESMF_FieldDestroy(srcArea(i), rc=localrc)
      call ESMF_FieldDestroy(srcFrac(i), rc=localrc)
      call ESMF_FieldDestroy(srcFrac2(i), rc=localrc)
      call ESMF_RoutehandleRelease(s2x_rh(i), rc=localrc)
      call ESMF_RoutehandleRelease(x2s_rh(i), rc=localrc)
    enddo
    do i = 1, size(dstField)
      call ESMF_FieldDestroy(dstField(i), rc=localrc)
      call ESMF_FieldDestroy(dstArea(i), rc=localrc)
      call ESMF_FieldDestroy(dstFrac(i), rc=localrc)
      call ESMF_FieldDestroy(dstFrac2(i), rc=localrc)
      call ESMF_RoutehandleRelease(d2x_rh(i), rc=localrc)
      call ESMF_RoutehandleRelease(x2d_rh(i), rc=localrc)
    enddo
    do i = 1, size(srcField)
      do j = 1, size(dstField)
        call ESMF_RoutehandleRelease(s2d_rh(i,j), rc=localrc)
        call ESMF_RoutehandleRelease(d2s_rh(j,i), rc=localrc)
      enddo
    enddo

    deallocate(srcArea, srcFrac, dstArea, dstFrac)
    deallocate(s2d_rh, d2s_rh)
    deallocate(s2x_rh, x2s_rh)
    deallocate(d2x_rh, x2d_rh)

    call ESMF_XGridDestroy(xgrid,rc=localrc)

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine flux_exchange

!----------------------------------------------------
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

  subroutine compute_flux2D(vm, flux_density, area, fraction, fraction2, allsum, maskptr, dstflux, rc)
    type(ESMF_VM), intent(in)        :: vm
    real(ESMF_KIND_R8), pointer      :: flux_density(:,:) 
    real(ESMF_KIND_R8), pointer      :: area(:,:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:,:) 
    real(ESMF_KIND_R8), pointer      :: fraction2(:,:) 
    real(ESMF_KIND_R8), intent(out)  :: allsum(3)
    integer(ESMF_KIND_I4), pointer   :: maskptr(:,:)
    logical, intent(in),  optional   :: dstflux
    integer, intent(out), optional   :: rc

    real(ESMF_KIND_R8)               :: sum(3)
    integer                          :: i,j, localrc, npet, lpet, mask
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
        ! non-masked cell has a masking value 0
        if(maskptr(i,j) == 0) then
          mask = 1
        else
          mask = 0
        endif
        if(l_dstflux) then
          sum(1) = sum(1) + flux_density(i,j)*area(i,j)*fraction2(i,j)*mask
        else
          sum(1) = sum(1) + flux_density(i,j)*area(i,j)*fraction(i,j)*fraction2(i,j)*mask
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

  subroutine flux_exchange_sph(xgrid, scheme, rc)

    type(ESMF_XGrid), intent(inout)           :: xgrid
    integer, intent(in),  optional            :: scheme
    integer, intent(out), optional            :: rc


    integer                                   :: localrc, i, j, nsrc, ndst, lpet, npet
    type(ESMF_Field)                          :: f_xgrid
    type(ESMF_Grid), allocatable              :: srcGrid(:)
    type(ESMF_Field), allocatable             :: srcFrac(:), srcArea(:)
    type(ESMF_Grid), allocatable              :: dstGrid(:)
    type(ESMF_Field), allocatable             :: dstFrac(:), dstArea(:)
    type(ESMF_Field), allocatable             :: srcFrac2(:)
    type(ESMF_Field), allocatable             :: dstFrac2(:)
    type(ESMF_RouteHandle), allocatable       :: s2d_rh(:,:)
    type(ESMF_RouteHandle), allocatable       :: d2s_rh(:,:)
    type(ESMF_RouteHandle), allocatable       :: s2x_rh(:), x2s_rh(:)
    type(ESMF_RouteHandle), allocatable       :: d2x_rh(:), x2d_rh(:)
    real(ESMF_KIND_R8), pointer               :: src(:,:), dst(:,:), exf(:)
    real(ESMF_KIND_R8), pointer               :: src_area(:,:), dst_area(:,:), exf_area(:)
    real(ESMF_KIND_R8), pointer               :: src_frac(:,:), dst_frac(:,:), exf_frac(:)
    real(ESMF_KIND_R8), pointer               :: src_frac2(:,:), dst_frac2(:,:)
    integer(ESMF_KIND_I4), pointer            :: src_mask(:,:), dst_mask(:,:)
    real(ESMF_KIND_R8)                        :: srcsum(3), allsrcsum(3), scale=2.0, exf_tarea, exf_tflux
    type(ESMF_VM)                             :: vm
    type(ESMF_Field), allocatable             :: srcField(:)
    type(ESMF_Field), allocatable             :: dstField(:)
    integer                                   :: l_scheme
    real(ESMF_KIND_R8)                        :: global_sum

    l_scheme = ESMF_REGRID_SCHEME_REGION3D
    if(present(scheme)) l_scheme = scheme

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_VMGet(vm, localpet=lpet, petCount=npet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    !------------------------------------
    ! build Fields on the Grids
    !------------------------------------

    ! create a Field on the xgrid
    f_xgrid = ESMF_FieldCreate(xgrid=xgrid, TYPEKIND=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(f_xgrid, farrayPtr=exf, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridGet(xgrid, ngridA=nsrc, ngridB=ndst, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(srcGrid(nsrc), srcField(nsrc), srcFrac(nsrc), srcFrac2(nsrc), srcArea(nsrc))
    allocate(dstGrid(ndst), dstField(ndst), dstFrac(ndst), dstFrac2(ndst), dstArea(ndst))

    call ESMF_XGridGet(xgrid, sideAGrid=srcGrid, sideBGrid=dstGrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = 1, nsrc
      srcField(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcFrac(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcFrac2(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcArea(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridGetArea(srcArea(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    do i = 1, ndst
      dstField(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dstFrac(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dstFrac2(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstFrac(i), localDe=0, farrayPtr=dst_frac, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      dstArea(i) = ESMF_FieldCreate(dstGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridGetArea(dstArea(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    allocate(s2d_rh(size(srcField), size(dstField)), d2s_rh(size(dstField), size(srcField)))
    allocate(s2x_rh(size(srcField)), x2s_rh(size(srcField)))
    allocate(d2x_rh(size(dstField)), x2d_rh(size(dstField)))

    do i = 1, size(srcField)
      do j = 1, size(dstField)
        call ESMF_FieldRegridStore(srcField=srcField(i), dstField=dstField(j), &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          routehandle=s2d_rh(i,j), &
          unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
          srcFracField=srcFrac(i), dstFracField=dstFrac(j), & 
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRegridStore(srcField=dstField(j), dstField=srcField(i), &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          routehandle=d2s_rh(j,i), &
          unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
          srcFracField=dstFrac(j), dstFracField=srcFrac(i), & 
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
    enddo

    do i = 1, size(srcField)
      call ESMF_FieldRegridStore(xgrid, srcField=srcField(i), dstField=f_xgrid, &
        routehandle=s2x_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridStore(xgrid, srcField=f_xgrid, dstField=srcField(i), &
        routehandle=x2s_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    do i = 1, size(dstField)
      call ESMF_FieldRegridStore(xgrid, srcField=dstField(i), dstField=f_xgrid, &
        routehandle=d2x_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldRegridStore(xgrid, srcField=f_xgrid, dstField=dstField(i), &
        routehandle=x2d_rh(i), dstFracField=dstFrac(i), dstMergeFracField=dstFrac2(i), &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    !----------------------------------------------------
    ! Compute flux integrals
    ! Initialize src flux to constant
    !----------------------------------------------------
    exf = 0.
    do i = 1, size(srcField)
      call ESMF_FieldGet(srcField(i), farrayPtr=src, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      src = scale
    enddo

    ! Perform flux exchange
    do i = 1, size(srcField)
      call ESMF_FieldRegrid(srcField=srcField(i), dstField=f_xgrid, &
        routehandle=s2x_rh(i), zeroregion=ESMF_REGION_EMPTY, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! make sure flux is conserved on XGrid
    allocate(exf_area(lbound(exf,1):ubound(exf,1)))
    allocate(exf_frac(lbound(exf,1):ubound(exf,1)))
    call ESMF_XGridGet(xgrid, area=exf_area, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    exf_frac = 1.0
    call compute_flux1D(vm, exf, exf_area, exf_frac, allsrcsum, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(lpet == 0) print *, ' xgrid flux and area: ', allsrcsum
    !if(abs(allsrcsum(1) - allsrcsum(2)*scale) .gt. 1.e-10) then
    !  call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
    !     msg="- inconsistent flux and area found", &
    !     ESMF_CONTEXT, rcToReturn=rc) 
    !  return
    !endif
    exf_tflux = allsrcsum(1)
    exf_tarea = allsrcsum(2)
    deallocate(exf_area, exf_frac)

    !make sure flux is conserved on dst Fields
    global_sum = 0.
    do i = 1, size(dstField)
      call ESMF_FieldRegrid(srcField=f_xgrid, dstField=dstField(i), &
        routehandle=x2d_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstField(i), farrayPtr=dst, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ! fraction
      call ESMF_FieldGet(dstFrac(i), farrayPtr=dst_frac, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(dstFrac2(i), farrayPtr=dst_frac2, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ! area
      call ESMF_FieldGet(dstArea(i), farrayPtr=dst_area, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      ! mask
      call ESMF_GridGetItem(dstGrid(i), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dst_mask, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif

      call compute_flux2D(vm, dst, dst_area, dst_frac, dst_frac2, allsrcsum, dst_mask, dstflux=.true., rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(lpet == 0) print *, 'dst flux and area: ', allsrcsum
      if(ndst == 1) then
        if((abs(exf_tarea - allsrcsum(2))/exf_tarea .gt. 1.e-5) .or. &
           (abs(exf_tflux - allsrcsum(1))/exf_tflux .gt. 1.e-5)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- inconsistent flux and area found", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      else
        call compute_flux2D(vm, dst, dst_area, dst_frac, dst_frac2, allsrcsum, dst_mask, dstflux=.true., rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if(lpet == 0) print *, 'dst flux and area using frac2: ', allsrcsum
        global_sum = global_sum + allsrcsum(1)
      endif

    enddo

    ! make sure going to multiple Grids also conserve global flux
    if(ndst .gt. 1) then
        if ((abs(exf_tflux - global_sum)/exf_tflux .gt. 1.e-5)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- inconsistent flux and area found", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    endif

    do i = 1, size(dstField)
      call ESMF_FieldRegrid(srcField=dstField(i), dstField=f_xgrid, &
        routehandle=d2x_rh(i), &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    do i = 1, size(srcField)
      call ESMF_FieldRegrid(srcField=f_xgrid, dstField=srcField(i), &
        routehandle=x2s_rh(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_FieldGet(srcField(i), farrayPtr=src, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    !----------------------------------------------------
    ! clean up
    !----------------------------------------------------
    do i = 1, size(srcField)
      call ESMF_FieldDestroy(srcField(i), rc=localrc)
      call ESMF_FieldDestroy(srcArea(i), rc=localrc)
      call ESMF_FieldDestroy(srcFrac(i), rc=localrc)
      call ESMF_FieldDestroy(srcFrac2(i), rc=localrc)
      call ESMF_RoutehandleRelease(s2x_rh(i), rc=localrc)
      call ESMF_RoutehandleRelease(x2s_rh(i), rc=localrc)
    enddo
    do i = 1, size(dstField)
      call ESMF_FieldDestroy(dstField(i), rc=localrc)
      call ESMF_FieldDestroy(dstArea(i), rc=localrc)
      call ESMF_FieldDestroy(dstFrac(i), rc=localrc)
      call ESMF_FieldDestroy(dstFrac2(i), rc=localrc)
      call ESMF_RoutehandleRelease(d2x_rh(i), rc=localrc)
      call ESMF_RoutehandleRelease(x2d_rh(i), rc=localrc)
    enddo
    do i = 1, size(srcField)
      do j = 1, size(dstField)
        call ESMF_RoutehandleRelease(s2d_rh(i,j), rc=localrc)
        call ESMF_RoutehandleRelease(d2s_rh(j,i), rc=localrc)
      enddo
    enddo

    deallocate(srcArea, srcFrac, dstArea, dstFrac)
    deallocate(s2d_rh, d2s_rh)
    deallocate(s2x_rh, x2s_rh)
    deallocate(d2x_rh, x2d_rh)

    call ESMF_XGridDestroy(xgrid,rc=localrc)

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine flux_exchange_sph

end program ESMF_XGridMaskingUTest
