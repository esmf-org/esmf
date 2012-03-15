! $Id: ESMF_XGridUTest.F90,v 1.39 2012/03/15 19:27:52 feiliu Exp $
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
    ! Create an XGrid in 2D
    print *, 'Starting test4'
    call test4(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid from 2 adjacent Grids"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! Create an XGrid in 3D
    print *, 'Starting test5'
    call test5(rc)
    write(failMsg, *) ""
    write(name, *) "Creating an XGrid in 3D with Grid merging"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(result, ESMF_SRCLINE)
  
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

        !print *, lpet, eleCount, elb, eub

        !call ESMF_DistGridPrint(distgrid, rc=localrc)
        !if (ESMF_LogFoundError(localrc, &
        !    ESMF_ERR_PASSTHRU, &
        !    ESMF_CONTEXT, rcToReturn=rc)) return

        !do i = 1, 2
        !    call ESMF_DistGridPrint(l_sideAdg(i), rc=localrc)
        !    if (ESMF_LogFoundError(localrc, &
        !        ESMF_ERR_PASSTHRU, &
        !        ESMF_CONTEXT, rcToReturn=rc)) return
        !enddo

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

        !call ESMF_FieldPrint(field, rc=localrc)
        !if (ESMF_LogFoundError(localrc, &
        !    ESMF_ERR_PASSTHRU, &
        !    ESMF_CONTEXT, rcToReturn=rc)) return

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
    integer                             :: localrc, i, npet
    type(ESMF_XGrid)                    :: xgrid
    type(ESMF_Grid)                     :: sideA(2), sideB(1)

    type(ESMF_VM)                       :: vm
    real(ESMF_KIND_R8)                  :: xgrid_area(12), B_area(2,2)
    type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)

    type(ESMF_Mesh)                     :: mesh
    type(ESMF_Field)                    :: srcField(3), dstField(3)
    type(ESMF_RouteHandle)              :: rh

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

    ! Sew mesh
    ! right, left
    xgrid = ESMF_XGridCreate((/make_grid(4,2,1.,1.,0.,0.,rc=localrc), make_grid(4,2,0.5,1.,4.,0.,rc=localrc)/), &
      (/make_grid(8,8,0.7,0.7,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Bigger Grids
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,rc=localrc), make_grid(4,4,0.5,1.,4.,0.,rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! right, left
    xgrid = ESMF_XGridCreate((/make_grid(4,4,0.5,1.,4.,0.,rc=localrc), make_grid(4,4,1.,1.,0.,0.,rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    ! down, up
    xgrid = ESMF_XGridCreate((/make_grid(4,4,0.5,1.,0.,-4.,rc=localrc), make_grid(4,4,1.,1.,0.,0.,rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,-4.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! up, down
    xgrid = ESMF_XGridCreate((/make_grid(4,4,0.5,1.,0.,0.,rc=localrc), make_grid(4,4,1.,1.,0.,-4.,rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,-4.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,field=srcField(1),rc=localrc), &
                               make_grid(4,4,0.5,1.,3.5,3.5,field=srcField(2),rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,field=dstField(1),rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call flux_exchange(xgrid, srcField(1:2), dstField(1:1), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,field=srcField(1),rc=localrc), &
                               make_grid(4,4,0.5,1.,3.3,3.4,field=srcField(2),rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,field=dstField(1),rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call flux_exchange(xgrid, srcField(1:2), dstField(1:1), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,   field=srcField(1),rc=localrc), &
                               make_grid(4,4,0.5,1.,3.3,2.4,field=srcField(2),rc=localrc)/), &
      (/make_grid(30,30,0.3,0.3,0.,0.,field=dstField(1),rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call flux_exchange(xgrid, srcField(1:2), dstField(1:1), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,   field=srcField(1),rc=localrc), &
                               make_grid(4,4,0.5,1.,3.3,2.4,field=srcField(2),rc=localrc)/), &
      (/make_grid(8,8,1.,1.,0.,0.,field=dstField(1),rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call flux_exchange(xgrid, srcField(1:2), dstField(1:1), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap subject smaller cell
    xgrid = ESMF_XGridCreate((/make_grid(4,4,1.,1.,0.,0.,rc=localrc), make_grid(4,4,0.5,1.,2.8,1.4,rc=localrc)/), &
      (/make_grid(30,30,0.3,0.3,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap subject bigger cell
    xgrid = ESMF_XGridCreate((/ &
        make_grid(4,4,0.5,1.,2.8,1.4,rc=localrc), &
        make_grid(4,4,1.,1.,0.,0.,rc=localrc) &
      /), &
      (/ &
        make_grid(30,30,0.3,0.3,0.,0.,rc=localrc) &
      /), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate( &
      (/ &
        make_grid(30,30,0.3,0.3,0.,0.,rc=localrc) &
      /), &
      (/ &
        make_grid(4,4,0.5,1.,2.8,1.4,rc=localrc), &
        make_grid(4,4,1.,1.,0.,0.,rc=localrc) &
      /), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(npet == 1) then
    ! complicated merging, these triggers a condition in rend mesh that currently does not support two distant Grids
    xgrid = ESMF_XGridCreate((/make_grid(4,2,1.,1.,0.,0.,rc=localrc), make_grid(4,2,0.5,1.,4.,0.,rc=localrc), &
                               make_grid(4,2,1.,1.,6.,0.,rc=localrc)/), &
      (/make_grid(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid(8,8,0.7,0.7,0.,5.6,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid(4,2,1.,1.,0.,0.,rc=localrc), make_grid(4,2,0.5,1.,3.,0.3,rc=localrc), &
                               make_grid(4,4,1.,1.,-2.,-2.,rc=localrc)/), &
      (/make_grid(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid(8,8,0.7,0.7,0.,5.6,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      (/make_grid(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid(8,8,0.7,0.7,0.,5.6,rc=localrc)/), &
      (/make_grid(4,2,1.,1.,0.,0.,rc=localrc), make_grid(4,2,0.5,1.,3.,0.3,rc=localrc), &
                               make_grid(4,4,1.,1.,-2.,-2.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid(4,2,1.,1.,0.,0.,rc=localrc), make_grid(4,2,0.5,1.,3.,0.3,rc=localrc), &
                               make_grid(4,4,1.,1.,-2.,-2.,rc=localrc)/), &
      (/make_grid(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid(8,8,0.5,0.7,0.9,3.6,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  end subroutine test4

  subroutine test5(rc)
    integer, intent(out)                :: rc
    integer                             :: localrc, i, npet
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

    call ESMF_VMGet(vm, petcount=npet, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Sew mesh
    ! right, left
    xgrid = ESMF_XGridCreate((/make_grid_sph(4,2,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,2,0.5,1.,4.,0.,rc=localrc)/), &
      (/make_grid_sph(8,8,0.7,0.7,0.,0.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! up, down
    xgrid = ESMF_XGridCreate((/make_grid_sph(4,4,0.5,1.,0.,0.,rc=localrc), make_grid_sph(4,4,1.,1.,0.,-4.,rc=localrc)/), &
      (/make_grid_sph(8,8,1.,1.,0.,-4.,rc=localrc)/), &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap
    xgrid = ESMF_XGridCreate((/make_grid_sph(4,4,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,4,0.6,1.,3.5,3.5,rc=localrc)/), &
      (/make_grid_sph(8,8,1.,1.,0.,0.,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid_sph(4,4,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,4,0.6,1.,2.9,3.5,rc=localrc)/), &
      (/make_grid_sph(8,8,1.,1.,0.,0.,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid_sph(4,4,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,4,0.6,1.,2.9,2.5,rc=localrc)/), &
      (/make_grid_sph(8,8,1.,1.,0.,0.,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid_sph(4,4,1.,1.,0.,0.,rc=localrc), make_grid_sph(8,4,0.6,1.,1.9,1.5,rc=localrc)/), &
      (/make_grid_sph(8,8,1.,1.,0.,0.,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! partially overlap subject bigger cell
    xgrid = ESMF_XGridCreate((/ &
        make_grid_sph(4,4,0.5,1.,2.8,1.4,rc=localrc), &
        make_grid_sph(4,4,1.,1.,0.,0.,rc=localrc) &
      /), &
      (/make_grid_sph(30,30,0.3,0.3,0.,0.,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if(npet == 1) then
    ! complicated merging
    xgrid = ESMF_XGridCreate((/make_grid_sph(4,2,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,2,0.5,1.,4.,0.,rc=localrc), &
                               make_grid_sph(4,2,1.,1.,6.,0.,rc=localrc)/), &
      (/make_grid_sph(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid_sph(8,8,0.7,0.7,0.,5.6,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid_sph(4,2,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,2,0.5,1.,3.,0.3,rc=localrc), &
                               make_grid_sph(4,4,1.,1.,-2.,-2.,rc=localrc)/), &
      (/make_grid_sph(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid_sph(8,8,0.7,0.7,0.,5.6,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate(&
      (/make_grid_sph(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid_sph(8,8,0.7,0.7,0.,5.6,rc=localrc)/), &
      (/make_grid_sph(4,2,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,2,0.5,1.,3.,0.3,rc=localrc), &
                               make_grid_sph(4,4,1.,1.,-2.,-2.,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    xgrid = ESMF_XGridCreate((/make_grid_sph(4,2,1.,1.,0.,0.,rc=localrc), make_grid_sph(4,2,0.5,1.,3.,0.3,rc=localrc), &
                               make_grid_sph(4,4,1.,1.,-2.,-2.,rc=localrc)/), &
      (/make_grid(8,8,0.7,0.7,0.,0.,rc=localrc), make_grid(8,8,0.5,0.7,0.9,3.6,rc=localrc)/), &
      sideAToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      sideBToXGridScheme=ESMF_REGRID_SCHEME_REGION3D, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    endif

  end subroutine test5

  !------------------------------------------------------------------------
  ! Utility methods
  !------------------------------------------------------------------------

  function make_grid(atm_nx, atm_ny, atm_dx, atm_dy, atm_sx, atm_sy, field, rc)

    ! return value
    type(ESMF_Grid)                           :: make_grid
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy
    real(ESMF_KIND_R4), intent(in)            :: atm_sx, atm_sy
    type(ESMF_Field), intent(out), optional   :: field
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

  function make_grid_sph(atm_nx, atm_ny, atm_dx, atm_dy, atm_sx, atm_sy, tag, scheme, rc)

    ! return value
    type(ESMF_Grid)                           :: make_grid_sph
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy
    real(ESMF_KIND_R4), intent(in)            :: atm_sx, atm_sy
    character(len=*), intent(in), optional    :: tag
    integer, intent(in) , optional            :: scheme
    integer, intent(out), optional            :: rc

    ! local variables
    integer                                   :: localrc, i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:,:), coordY(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty
    integer                                   :: l_scheme

    l_scheme = ESMF_REGRID_SCHEME_REGION3D
    if(present(scheme)) l_scheme = scheme

    if(l_scheme == ESMF_REGRID_SCHEME_FULL3D) then
      make_grid_sph = ESMF_GridCreate1PeriDim(maxIndex=(/atm_nx, atm_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
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
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + atm_dx/2. + (i-1)*atm_dx
        coordY(i,j) = starty + atm_dy/2. + (j-1)*atm_dy
      enddo
    enddo
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
        coordY(i,j) = starty + (j-1)*atm_dy
      enddo
    enddo

    if(present(rc)) rc = ESMF_SUCCESS

  end function make_grid_sph

  subroutine flux_exchange(xgrid, srcField, dstField, rc)

    type(ESMF_XGrid), intent(in)              :: xgrid
    type(ESMF_Field), intent(inout)           :: srcField(:)
    type(ESMF_Field), intent(inout)           :: dstField(:)
    integer, intent(out), optional            :: rc


    integer                                   :: localrc, i, j, nsrc, ndst, lpet, npet
    type(ESMF_Field)                          :: f_xgrid
    type(ESMF_Grid), allocatable              :: srcGrid(:)
    type(ESMF_Field), allocatable             :: srcFrac(:), srcArea(:)
    type(ESMF_Grid), allocatable              :: dstGrid(:)
    type(ESMF_Field), allocatable             :: dstFrac(:), dstArea(:)
    type(ESMF_RouteHandle), allocatable       :: s2d_rh(:,:)
    type(ESMF_RouteHandle), allocatable       :: d2s_rh(:,:)
    type(ESMF_RouteHandle), allocatable       :: s2x_rh(:), x2s_rh(:)
    type(ESMF_RouteHandle), allocatable       :: d2x_rh(:), x2d_rh(:)
    real(ESMF_KIND_R8), pointer               :: src(:,:), dst(:,:), exf(:)
    real(ESMF_KIND_R8), pointer               :: src_area(:,:), dst_area(:,:), exf_area(:)
    real(ESMF_KIND_R8), pointer               :: src_frac(:,:), dst_frac(:,:), exf_frac(:)
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
    allocate(srcGrid(nsrc), srcFrac(nsrc), srcArea(nsrc))
    allocate(dstGrid(ndst), dstFrac(ndst), dstArea(ndst))
    do i = 1, size(srcField)
      call ESMF_FieldGet(srcField(i), grid=srcGrid(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      srcFrac(i) = ESMF_FieldCreate(srcGrid(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
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
        routehandle=x2d_rh(i), dstFracField=dstFrac(i), rc=localrc)
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
    if(abs(allsrcsum(1) - allsrcsum(2)*scale) .gt. 1.e-10) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- inconsistent flux and area found", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif
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

      ! area
      call ESMF_FieldGet(dstArea(i), farrayPtr=dst_area, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      call compute_flux2D(vm, dst, dst_area, dst_frac, allsrcsum, rc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(lpet == 0) print *, 'dst flux and area: ', allsrcsum
      ! In general, dst flux cannot be guarannteed to be conserved when
      ! interpolating from high resolution Grid/XGrid to low resolution Grid
      ! due to undersampling nature of the process
      if(abs(exf_tarea - allsrcsum(2)) .gt. 1.e-10) then
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
      call ESMF_RoutehandleRelease(s2x_rh(i), rc=localrc)
      call ESMF_RoutehandleRelease(x2s_rh(i), rc=localrc)
    enddo
    do i = 1, size(dstField)
      call ESMF_FieldDestroy(dstField(i), rc=localrc)
      call ESMF_FieldDestroy(dstArea(i), rc=localrc)
      call ESMF_FieldDestroy(dstFrac(i), rc=localrc)
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

  subroutine compute_flux2D(vm, flux_density, area, fraction, allsum, rc)
    type(ESMF_VM), intent(in)        :: vm
    real(ESMF_KIND_R8), pointer      :: flux_density(:,:) 
    real(ESMF_KIND_R8), pointer      :: area(:,:) 
    real(ESMF_KIND_R8), pointer      :: fraction(:,:) 
    real(ESMF_KIND_R8), intent(out)  :: allsum(3)
    integer, intent(out), optional   :: rc

    real(ESMF_KIND_R8)               :: sum(3)
    integer                          :: i,j, localrc, npet, lpet

    if(present(rc)) rc = ESMF_SUCCESS

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
        sum(1) = sum(1) + flux_density(i,j)*area(i,j)*fraction(i,j)
        sum(2) = sum(2) +                 area(i,j)*fraction(i,j)
        sum(3) = sum(3) +                 area(i,j)
      enddo
    enddo

    call ESMF_VMAllReduce(vm, sum, allsum, 3, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine compute_flux2D

end program ESMF_XGridUTest
