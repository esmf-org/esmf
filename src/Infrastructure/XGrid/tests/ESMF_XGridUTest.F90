! $Id: ESMF_XGridUTest.F90,v 1.31 2012/01/06 20:18:38 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_XGridUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_XGridUTest - Unit tests for Field Create and Get methods
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
      '$Id'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Create an empty XGrid with area/centroid, sparseMatA2X
    print *, 'Starting test3'
    call test3(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid with area/centroid, sparseMatA2X"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Create an XGrid from 2 adjacent Grids
    print *, 'Starting test4'
    call test4(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid from 2 adjacent Grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_Finalize(rc=rc)
  
contains 
#define ESMF_METHOD "ESMF_TESTS"

!------------------------------------------------------------------------
    subroutine test3(rc)
        integer, intent(out)                :: rc
        integer                             :: localrc, i
        type(ESMF_XGrid)                    :: xgrid
        type(ESMF_Grid)                     :: sideA(2), sideB(1)
        type(ESMF_DistGrid)                 :: sideAdg(2), sideBdg(1), distgrid
        real(ESMF_KIND_R8)                  :: centroid(12,2), area(12)
        type(ESMF_XGridSpec)                :: sparseMatA2X(2), sparseMatX2B(1)

        type(ESMF_Grid)                     :: l_sideA(2), l_sideB(1)
        type(ESMF_DistGrid)                 :: l_sideAdg(2), l_sideBdg(1)
        real(ESMF_KIND_R8)                  :: l_centroid(12,2), l_area(12)
        type(ESMF_XGridSpec)                :: l_sparseMatA2X(2), l_sparseMatX2B(1)
        type(ESMF_Field)                    :: field, srcField(2), dstField(1)

        type(ESMF_VM)                       :: vm
        integer                             :: lpet, eleCount, ngridA, ngridB
        integer                             :: elb, eub, ec

        real(ESMF_KIND_R8), pointer         :: fptr(:,:), xfptr(:)
        real(ESMF_KIND_R8)                  :: xgrid_area(12), B_area(2,2)
        integer                             :: xlb(1), xub(1)
        type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)

        type(ESMF_XGrid)                    :: xgridAlias
        logical                             :: xgridBool

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm=vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        sideAdg(1) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        sideAdg(2) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/1,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        sideBdg = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 2
            sideA(i) = ESMF_GridCreate(distgrid=sideAdg(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
        do i = 1, 1
            sideB(i) = ESMF_GridCreate(distgrid=sideBdg(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        allocate(sparseMatA2X(1)%factorIndexList(2,9), sparseMatA2X(1)%factorList(9))
        allocate(sparseMatA2X(2)%factorIndexList(2,3), sparseMatA2X(2)%factorList(3))
        allocate(sparseMatX2B(1)%factorIndexList(2,12), sparseMatX2B(1)%factorList(12))
    
        ! factorIndexList
        ! setting up mapping between A1 -> X
        sparseMatA2X(1)%factorIndexList(1,1)=1
        sparseMatA2X(1)%factorIndexList(1,2)=2
        sparseMatA2X(1)%factorIndexList(1,3)=2
        sparseMatA2X(1)%factorIndexList(1,4)=3
        sparseMatA2X(1)%factorIndexList(1,5)=4
        sparseMatA2X(1)%factorIndexList(1,6)=4
        sparseMatA2X(1)%factorIndexList(1,7)=3
        sparseMatA2X(1)%factorIndexList(1,8)=4
        sparseMatA2X(1)%factorIndexList(1,9)=4
        sparseMatA2X(1)%factorIndexList(2,1)=1
        sparseMatA2X(1)%factorIndexList(2,2)=2
        sparseMatA2X(1)%factorIndexList(2,3)=3
        sparseMatA2X(1)%factorIndexList(2,4)=4
        sparseMatA2X(1)%factorIndexList(2,5)=5
        sparseMatA2X(1)%factorIndexList(2,6)=6
        sparseMatA2X(1)%factorIndexList(2,7)=7
        sparseMatA2X(1)%factorIndexList(2,8)=8
        sparseMatA2X(1)%factorIndexList(2,9)=9
        ! setting up mapping between A2 -> X
        sparseMatA2X(2)%factorIndexList(1,1)=1
        sparseMatA2X(2)%factorIndexList(1,2)=2
        sparseMatA2X(2)%factorIndexList(1,3)=2
        sparseMatA2X(2)%factorIndexList(2,1)=10
        sparseMatA2X(2)%factorIndexList(2,2)=11
        sparseMatA2X(2)%factorIndexList(2,3)=12

        ! Note that the weights are dest area weighted
        ! factorList
        ! setting up mapping between A1 -> X
        sparseMatA2X(1)%factorList(1)=1
        sparseMatA2X(1)%factorList(2)=1
        sparseMatA2X(1)%factorList(3)=1
        sparseMatA2X(1)%factorList(4)=1
        sparseMatA2X(1)%factorList(5)=1
        sparseMatA2X(1)%factorList(6)=1
        sparseMatA2X(1)%factorList(7)=1
        sparseMatA2X(1)%factorList(8)=1
        sparseMatA2X(1)%factorList(9)=1
        ! setting up mapping between A2 -> X
        sparseMatA2X(2)%factorList(1)=1
        sparseMatA2X(2)%factorList(2)=1
        sparseMatA2X(2)%factorList(3)=1
    
        ! factorIndexList
        ! setting up mapping between X -> B
        sparseMatX2B(1)%factorIndexList(1,1)=1
        sparseMatX2B(1)%factorIndexList(1,2)=2
        sparseMatX2B(1)%factorIndexList(1,3)=3
        sparseMatX2B(1)%factorIndexList(1,4)=4
        sparseMatX2B(1)%factorIndexList(1,5)=5
        sparseMatX2B(1)%factorIndexList(1,6)=6
        sparseMatX2B(1)%factorIndexList(1,7)=7
        sparseMatX2B(1)%factorIndexList(1,8)=8
        sparseMatX2B(1)%factorIndexList(1,9)=9
        sparseMatX2B(1)%factorIndexList(1,10)=10
        sparseMatX2B(1)%factorIndexList(1,11)=11
        sparseMatX2B(1)%factorIndexList(1,12)=12
        sparseMatX2B(1)%factorIndexList(2,1)=1
        sparseMatX2B(1)%factorIndexList(2,2)=1
        sparseMatX2B(1)%factorIndexList(2,3)=2
        sparseMatX2B(1)%factorIndexList(2,4)=1
        sparseMatX2B(1)%factorIndexList(2,5)=1
        sparseMatX2B(1)%factorIndexList(2,6)=2
        sparseMatX2B(1)%factorIndexList(2,7)=3
        sparseMatX2B(1)%factorIndexList(2,8)=3
        sparseMatX2B(1)%factorIndexList(2,9)=4
        sparseMatX2B(1)%factorIndexList(2,10)=3
        sparseMatX2B(1)%factorIndexList(2,11)=3
        sparseMatX2B(1)%factorIndexList(2,12)=4

        ! factorList
        ! setting up mapping between X -> B
        sparseMatX2B(1)%factorList(1)=4./9
        sparseMatX2B(1)%factorList(2)=2./9
        sparseMatX2B(1)%factorList(3)=2./3
        sparseMatX2B(1)%factorList(4)=2./9
        sparseMatX2B(1)%factorList(5)=1./9
        sparseMatX2B(1)%factorList(6)=1./3
        sparseMatX2B(1)%factorList(7)=2./9
        sparseMatX2B(1)%factorList(8)=1./9
        sparseMatX2B(1)%factorList(9)=1./3
        sparseMatX2B(1)%factorList(10)=4./9
        sparseMatX2B(1)%factorList(11)=2./9
        sparseMatX2B(1)%factorList(12)=2./3

        ! set up destination areas to adjust weighted flux
        xgrid_area(1) = 1.
        xgrid_area(2) = 0.5
        xgrid_area(3) = 0.5
        xgrid_area(4) = 0.5
        xgrid_area(5) = 0.25
        xgrid_area(6) = 0.25
        xgrid_area(7) = 0.5
        xgrid_area(8) = 0.25
        xgrid_area(9) = 0.25
        xgrid_area(10) = 1.
        xgrid_area(11) = 0.5
        xgrid_area(12) = 0.5

        B_area(1,1) = 9./4
        B_area(2,1) = 3./4
        B_area(1,2) = 9./4
        B_area(2,2) = 3./4

        ! Finally ready to do an flux exchange from A side to B side
        xgrid = ESMF_XGridCreate(sideA, sideB, offline=.true., &
            area=xgrid_area, centroid=centroid, &
            sparseMatA2X=sparseMatA2X, sparseMatX2B=sparseMatX2B, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

	    !------------------------------------------------------------------------
	    !NEX_UTest
	    write(name, *) "XGrid equality before assignment Test"
	    write(failMsg, *) "Did not return ESMF_SUCCESS"
	    xgridBool = (xgridAlias.eq.xgrid)
	    call ESMF_Test(.not.xgridBool, name, failMsg, result, ESMF_SRCLINE)

	    !------------------------------------------------------------------------
	    !NEX_UTest
	    ! Testing ESMF_XGridAssignment(=)()
	    write(name, *) "XGrid assignment and equality Test"
	    write(failMsg, *) "Did not return ESMF_SUCCESS"
	    xgridAlias = xgrid
	    xgridBool = (xgridAlias.eq.xgrid)
	    call ESMF_Test(xgridBool, name, failMsg, result, ESMF_SRCLINE)


        call ESMF_XGridGet(xgrid, ngridA=ngridA, ngridB=ngridB, &
            sideA=l_sideA, sideB=l_sideB, area=l_area, &
            centroid=l_centroid, distgridA=l_sideAdg, &
            distgridM = distgrid, sparseMatA2X=l_sparseMatA2X, &
            sparseMatX2B=l_sparseMatX2B, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_XGridGet(xgrid, localDe=0, elementCount=eleCount, &
            exclusiveCount=ec, exclusiveLBound=elb, exclusiveUBound=eub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        print *, lpet, eleCount, elb, eub

        call ESMF_DistGridPrint(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 2
            call ESMF_DistGridPrint(l_sideAdg(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRIDSIDE_A, gridIndex=1, &
            distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRIDSIDE_A, gridIndex=2, &
            distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRIDSIDE_B, gridIndex=1, &
            distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldGet(field, farrayPtr=xfptr, &
            exclusiveLBound=xlb, exclusiveUBound=xub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        xfptr = 0.0

        call ESMF_FieldPrint(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

	    !------------------------------------------------------------------------
	    !NEX_UTest
	    write(name, *) "XGridDestroy Test"
	    write(failMsg, *) "Did not return ESMF_SUCCESS"
	    call ESMF_XGridDestroy(xgrid, rc=rc)
	    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

	    !------------------------------------------------------------------------
	    !NEX_UTest
	    ! Testing ESMF_XGridOperator(==)()
	    write(name, *) "XGrid equality after destroy Test"
	    write(failMsg, *) "Did not return ESMF_SUCCESS"
	    xgridBool = (xgridAlias==xgrid)
	    call ESMF_Test(.not.xgridBool, name, failMsg, result, ESMF_SRCLINE)

	    !------------------------------------------------------------------------
	    !NEX_UTest
	    ! Testing ESMF_XGridOperator(/=)()
	    write(name, *) "XGrid non-equality after destroy Test"
	    write(failMsg, *) "Did not return ESMF_SUCCESS"
	    xgridBool = (xgridAlias/=xgrid)
	    call ESMF_Test(xgridBool, name, failMsg, result, ESMF_SRCLINE)

        do i = 1, 2
            call ESMF_GridDestroy(sideA(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        do i = 1, 1
            call ESMF_GridDestroy(sideB(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
        deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
        deallocate(sparseMatX2B(1)%factorIndexList, sparseMatX2B(1)%factorList)

    end subroutine test3

!------------------------------------------------------------------------
  subroutine test4(rc)
    integer, intent(out)                :: rc
    integer                             :: localrc, i
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)

    type(ESMF_VM)                       :: vm
    real(ESMF_KIND_R8)                  :: xgrid_area(12), B_area(2,2)
    type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)

    type(ESMF_Mesh)                     :: mesh
    type(ESMF_Field)                    :: field1, field2
    type(ESMF_RouteHandle)              :: rh

    rc = ESMF_SUCCESS
    localrc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm=vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Sew mesh
    ! right, left
    xgrid = ESMF_XGridCreate((/make_grid(4,2,1.,1.,0.,0.,localrc), make_grid(4,2,0.5,1.,4.,0.,localrc)/), &
      (/make_grid(8,8,0.7,0.7,0.,0.,localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Bigger Grids
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,localrc), make_grid(4,4,0.5,1.,4.,0.,localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! right, left
    xgrid = ESMF_XGridCreate((/make_grid(4,4,0.5,1.,4.,0.,localrc), make_grid(4,4,1.,1.,0.,0.,localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    ! down, up
    xgrid = ESMF_XGridCreate((/make_grid(4,4,0.5,1.,0.,-4.,localrc), make_grid(4,4,1.,1.,0.,0.,localrc)/), &
      (/make_grid(8,8,1.,1.,0.,-4.,localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! up, down
    xgrid = ESMF_XGridCreate((/make_grid(4,4,0.5,1.,0.,0.,localrc), make_grid(4,4,1.,1.,0.,-4.,localrc)/), &
      (/make_grid(8,8,1.,1.,0.,-4.,localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    !xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,localrc), make_grid(4,4,0.5,1.,3.5,3.5,localrc)/), &
    !  (/make_grid(8,8,1.,1.,0.,0.,localrc)/), &
    !  rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !  ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine test4

  function make_grid(atm_nx, atm_ny, atm_dx, atm_dy, atm_sx, atm_sy, rc)

    ! return value
    type(ESMF_Grid)                           :: make_grid
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy
    real(ESMF_KIND_R4), intent(in)            :: atm_sx, atm_sy
    integer, intent(out), optional            :: rc

    ! local variables
    integer                                   :: localrc, i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:), coordY(:)
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
    enddo
    print *, 'coordX: ', coordX
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
  
    if(present(rc)) rc = ESMF_SUCCESS

  end function make_grid

end program ESMF_XGridUTest
