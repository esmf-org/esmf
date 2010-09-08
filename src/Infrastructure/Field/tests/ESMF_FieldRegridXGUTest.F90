! $Id: ESMF_FieldRegridXGUTest.F90,v 1.3 2010/09/08 13:15:46 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
        !E-disable-X_UTest
        ! Create an field stress test
        call test_regridxg(rc)
        write(failMsg, *) ""
        write(name, *) "Create xgrid and regrid through xgrid"
        !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"
    subroutine test_regridxg(rc)
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

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm=vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpet, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        sideAdg(1) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        sideAdg(2) = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/1,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        sideBdg = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/2,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        do i = 1, 2
            sideA(i) = ESMF_GridCreate(distgrid=sideAdg(i), destroyDistGrid=.true., rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_GridAddCoord(sideA(i), staggerLoc=ESMF_STAGGERLOC_CENTER, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo
        do i = 1, 1
            sideB(i) = ESMF_GridCreate(distgrid=sideBdg(i), destroyDistGrid=.true., rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_GridAddCoord(sideB(i), staggerLoc=ESMF_STAGGERLOC_CENTER, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
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
        xgrid = ESMF_XGridCreate(sideA, sideB, area=area, centroid=centroid, &
            sparseMatA2X=sparseMatA2X, sparseMatX2B=sparseMatX2B, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_XGridGet(xgrid, ngridA=ngridA, ngridB=ngridB, &
            sideA=l_sideA, sideB=l_sideB, area=l_area, &
            centroid=l_centroid, distgridA=l_sideAdg, &
            distgridM = distgrid, sparseMatA2X=l_sparseMatA2X, &
            sparseMatX2B=l_sparseMatX2B, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_XGridGet(xgrid, localDe=0, elementCount=eleCount, &
            exclusiveCount=ec, exclusiveLBound=elb, exclusiveUBound=eub, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        print *, lpet, eleCount, elb, eub

        call ESMF_DistGridPrint(distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        do i = 1, 2
            call ESMF_DistGridPrint(l_sideAdg(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo

        call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRID_SIDEA, gridIndex=1, &
            distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRID_SIDEA, gridIndex=2, &
            distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_XGridGet(xgrid, xgridSide=ESMF_XGRID_SIDEB, gridIndex=1, &
            distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(xgrid, typekind=ESMF_TYPEKIND_R8, rank=1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_FieldGet(field, farrayPtr=xfptr, &
            exclusiveLBound=xlb, exclusiveUBound=xub, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        xfptr = 0.0

        call ESMF_FieldPrint(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! setup and initialize src and dst Fields
        do i = 1, 2
            srcField(i) = ESMF_FieldCreate(sideA(i), typekind=ESMF_TYPEKIND_R8, rank=2, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldGet(srcField(i), farrayPtr=fptr, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            fptr = i
        enddo
        do i = 1, 1
            dstField(i) = ESMF_FieldCreate(sideB(i), typekind=ESMF_TYPEKIND_R8, rank=2, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldGet(dstField(i), farrayPtr=fptr, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            fptr = 0.0
        enddo

        ! use field on Xgrid to do smm from src to dst transformation
        ! dst = W'*W*src
        print *, '- before SMM from A -> X'
        print *, xfptr

        ! from A -> X
        do i = 1, 2
            call ESMF_FieldRegridStore(xgrid, srcField=srcField(i), dstField=field, &
                routehandle=rh_src2xgrid(i), &
                regridScheme=ESMF_REGRID_SCHEME_NATIVE, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        
            call ESMF_FieldRegrid(srcField=srcField(i), dstField=field, routehandle=rh_src2xgrid(i), &
                zeroflag=ESMF_REGION_SELECT, &
                checkflag=.TRUE.,&
                rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            print *, '- SMM from A -> X'
            print *, xfptr
        enddo

        ! xfptr should be all 1. at this point
        ! To get the surface integral of flux on XGrid, adjust by dst area

        !do i = xlb(1), xub(1)
        !    xfptr(i) = xfptr(i) * xgrid_area(i) 
        !enddo

        print *, '- after SMM from A -> X'
        print *, xfptr ! should be xgrid_area

        print *, '- B before SMM from X -> B'
        print *, fptr ! should be 0.

        ! from X -> B
        do i = 1, 1
            call ESMF_FieldRegridStore(xgrid, srcField=field, dstField=dstField(i), &
                routehandle=rh_xgrid2dst(i), &
                regridScheme=ESMF_REGRID_SCHEME_NATIVE, &
                rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldRegrid(srcField=field, dstField=dstField(i), routehandle=rh_xgrid2dst(i), &
                zeroflag=ESMF_REGION_EMPTY, &
                checkflag=.TRUE.,&
                rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo

        print *, '- B after SMM from X -> B'
        print *, fptr ! should be 1/B_area

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_XGridDestroy(xgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        do i = 1, 2
            call ESMF_FieldDestroy(srcField(i), rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_GridDestroy(sideA(i), rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo

        do i = 1, 1
            call ESMF_FieldDestroy(dstField(i), rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_GridDestroy(sideB(i), rc = localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo

        deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
        deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
        deallocate(sparseMatX2B(1)%factorIndexList, sparseMatX2B(1)%factorList)

    end subroutine test_regridxg

end program ESMF_FieldRegridXGUTest
