! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridXGSMMUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridXGSMMUTest - Unit tests for Field RegridXG Testing
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
        !EX_UTest_Multi_Proc_Only
        call test_regridxg_const(rc)
        write(failMsg, *) ""
        write(name, *) "Create xgrid and regrid through xgrid, const src flux"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regridxg(rc)
        write(failMsg, *) ""
        write(name, *) "Create xgrid and regrid through xgrid, variable src flux"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regrid2xg_half(rc)
        write(failMsg, *) ""
        write(name, *) "Regrid then create xgrid and regrid through xgrid, half overlap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regrid2xg_contain(rc)
        write(failMsg, *) ""
        write(name, *) "Regrid then create xgrid and regrid through xgrid, check containment"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regrid2xg_clip(rc)
        write(failMsg, *) ""
        write(name, *) "Regrid then create xgrid and regrid through xgrid, check clipping"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regrid2xg(10,10,8,8,0.1,0.1,0.1,0.1,rc)
        write(failMsg, *) ""
        write(name, *) "Regrid then create xgrid and regrid through xgrid, exact cut"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regrid2xg(10,10,14,14,0.1,0.1,0.06,0.06,rc)
        write(failMsg, *) ""
        write(name, *) "Regrid then create xgrid and regrid through xgrid, overlapping cut"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !E-disable-X_UTest_Multi_Proc_Only
        !call test_regrid2xgSph(rc)
        !write(failMsg, *) ""
        !write(name, *) "Regrid then create xgrid and regrid through xgrid, spherical grids"
        !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"
    subroutine test_regridxg_const(rc)
        integer, intent(out)                :: rc
        integer                             :: localrc, i
        type(ESMF_XGrid)                    :: xgrid
        type(ESMF_Grid)                     :: sideA(2), sideB(1)
        type(ESMF_DistGrid)                 :: distgrid
        real(ESMF_KIND_R8)                  :: centroid(12,2), area(12)
        type(ESMF_XGridSpec)                :: sparseMatA2X(2), sparseMatX2B(1)

        type(ESMF_Grid)                     :: l_sideA(2), l_sideB(1)
        type(ESMF_DistGrid)                 :: l_sideAdg(2), l_sideBdg(1)
        real(ESMF_KIND_R8)                  :: l_centroid(12,2), l_area(12)
        type(ESMF_XGridSpec)                :: l_sparseMatA2X(2), l_sparseMatX2B(1)
        type(ESMF_Field)                    :: field, srcField(2), dstField(1)

        integer                             :: eleCount
        integer                             :: sideAGC, sideBGC, sideAMC, sideBMC
        integer                             :: elb, eub, ec, lpet, npet

        real(ESMF_KIND_R8), pointer         :: farrayPtr(:,:), xfarrayPtr(:)
        real(ESMF_KIND_R8)                  :: xgrid_area(12), B_area(2,2)
        integer                             :: xlb(1), xub(1)
        type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)
        type(ESMF_VM)                       :: vm

        real(ESMF_KIND_R8)                  :: centroidA1X(2), centroidA1Y(2)
        real(ESMF_KIND_R8)                  :: centroidA2X(2), centroidA2Y(1)
        real(ESMF_KIND_R8)                  :: centroidBX(2), centroidBY(2)
        real(ESMF_KIND_R8), pointer         :: coordX(:), coordY(:)
        character(len=16)                   :: gridNameA(2), gridNameB(1)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm=vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(npet /= 2) return

        gridNameA(1) = 'srcGrid 1'
        gridNameA(2) = 'srcGrid 2'
        sideA(1) = ESMF_GridCreateNoPeriDim(maxIndex=(/2,2/), &
            coordDep1=(/1/), &
            coordDep2=(/2/), &
            regDecomp=(/1,2/), &
            name=gridNameA(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sideA(2) = ESMF_GridCreateNoPeriDim(maxIndex=(/2,1/), &
            coordDep1=(/1/), &
            coordDep2=(/2/), &
            regDecomp=(/2,1/), &
            name=gridNameA(2), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, 2
            call ESMF_GridAddCoord(sideA(i), staggerloc=ESMF_STAGGERLOC_CENTER, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! SideA first grid
        centroidA1X=(/0.5, 1.5/)
        centroidA1Y=(/0.5, 1.5/)
        call ESMF_GridGetCoord(sideA(1), localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, farrayPtr=coordX, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordX = centroidA1X
        call ESMF_GridGetCoord(sideA(1), localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=coordY, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordY = centroidA1Y(lpet+1)

        ! SideA second grid
        centroidA2X=(/0.5, 1.5/)
        centroidA2Y=(/2.5/)
        call ESMF_GridGetCoord(sideA(2), localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, farrayPtr=coordX, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordX = centroidA2X(lpet+1)
        call ESMF_GridGetCoord(sideA(2), localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=coordY, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if(lpet == 0) coordY = centroidA2Y

        gridNameB(1) = 'dstGrid 1'
        sideB(1) = ESMF_GridCreateNoPeriDim(maxIndex=(/2,2/), &
            coordDep1=(/1/), coordDep2=(/2/), &
            regDecomp=(/1,2/), &
            name=gridNameB(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, 1
            call ESMF_GridAddCoord(sideB(i), &
                staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! SideB grid
        centroidBX=(/0.75, 1.75/)
        centroidBY=(/0.75, 2.25/)
        call ESMF_GridGetCoord(sideB(1), localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, farrayPtr=coordX, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordX = centroidBX
        call ESMF_GridGetCoord(sideB(1), localDE=0, &
            staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, farrayPtr=coordY, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordY = centroidBY(lpet+1)

        if(lpet == 0) then
            allocate(sparseMatA2X(1)%factorIndexList(2,6), sparseMatA2X(1)%factorList(6))
        else
            allocate(sparseMatA2X(1)%factorIndexList(2,7:9), sparseMatA2X(1)%factorList(7:9))
        endif
        if(lpet == 0) then
            allocate(sparseMatA2X(2)%factorIndexList(2,1), sparseMatA2X(2)%factorList(1))
        else
            allocate(sparseMatA2X(2)%factorIndexList(2,2:3), sparseMatA2X(2)%factorList(2:3))
        endif
        if(lpet == 0) then
            allocate(sparseMatX2B(1)%factorIndexList(2,6), sparseMatX2B(1)%factorList(6))
        else
            allocate(sparseMatX2B(1)%factorIndexList(2,7:12), sparseMatX2B(1)%factorList(7:12))
        endif
    
        ! factorIndexList
        ! setting up mapping between A1 -> X
        if(lpet == 0) then
            sparseMatA2X(1)%factorIndexList(1,1)=1
            sparseMatA2X(1)%factorIndexList(1,2)=2
            sparseMatA2X(1)%factorIndexList(1,3)=2
            sparseMatA2X(1)%factorIndexList(1,4)=3
            sparseMatA2X(1)%factorIndexList(1,5)=4
            sparseMatA2X(1)%factorIndexList(1,6)=4
        else
            sparseMatA2X(1)%factorIndexList(1,7)=3
            sparseMatA2X(1)%factorIndexList(1,8)=4
            sparseMatA2X(1)%factorIndexList(1,9)=4
        endif
        if(lpet == 0) then
            sparseMatA2X(1)%factorIndexList(2,1)=1
            sparseMatA2X(1)%factorIndexList(2,2)=2
            sparseMatA2X(1)%factorIndexList(2,3)=3
            sparseMatA2X(1)%factorIndexList(2,4)=4
            sparseMatA2X(1)%factorIndexList(2,5)=5
            sparseMatA2X(1)%factorIndexList(2,6)=6
        else
            sparseMatA2X(1)%factorIndexList(2,7)=7
            sparseMatA2X(1)%factorIndexList(2,8)=8
            sparseMatA2X(1)%factorIndexList(2,9)=9
        endif
        ! setting up mapping between A2 -> X
        if(lpet == 0) then
            sparseMatA2X(2)%factorIndexList(1,1)=1
        else
            sparseMatA2X(2)%factorIndexList(1,2)=2
            sparseMatA2X(2)%factorIndexList(1,3)=2
        endif
        if(lpet == 0) then
            sparseMatA2X(2)%factorIndexList(2,1)=10
        else
            sparseMatA2X(2)%factorIndexList(2,2)=11
            sparseMatA2X(2)%factorIndexList(2,3)=12
        endif

        ! Note that the weights are dest area weighted
        ! factorList
        ! setting up mapping between A1 -> X
        if(lpet == 0) then
            sparseMatA2X(1)%factorList(1)=1
            sparseMatA2X(1)%factorList(2)=1
            sparseMatA2X(1)%factorList(3)=1
            sparseMatA2X(1)%factorList(4)=1
            sparseMatA2X(1)%factorList(5)=1
            sparseMatA2X(1)%factorList(6)=1
        else
            sparseMatA2X(1)%factorList(7)=1
            sparseMatA2X(1)%factorList(8)=1
            sparseMatA2X(1)%factorList(9)=1
        endif
        ! setting up mapping between A2 -> X
        if(lpet == 0) then
            sparseMatA2X(2)%factorList(1)=1
        else
            sparseMatA2X(2)%factorList(2)=1
            sparseMatA2X(2)%factorList(3)=1
        endif
    
        ! factorIndexList
        ! setting up mapping between X -> B
        if(lpet == 0) then
            sparseMatX2B(1)%factorIndexList(1,1)=1
            sparseMatX2B(1)%factorIndexList(1,2)=2
            sparseMatX2B(1)%factorIndexList(1,3)=3
            sparseMatX2B(1)%factorIndexList(1,4)=4
            sparseMatX2B(1)%factorIndexList(1,5)=5
            sparseMatX2B(1)%factorIndexList(1,6)=6
        else
            sparseMatX2B(1)%factorIndexList(1,7)=7
            sparseMatX2B(1)%factorIndexList(1,8)=8
            sparseMatX2B(1)%factorIndexList(1,9)=9
            sparseMatX2B(1)%factorIndexList(1,10)=10
            sparseMatX2B(1)%factorIndexList(1,11)=11
            sparseMatX2B(1)%factorIndexList(1,12)=12
        endif
        if(lpet == 0) then
            sparseMatX2B(1)%factorIndexList(2,1)=1
            sparseMatX2B(1)%factorIndexList(2,2)=1
            sparseMatX2B(1)%factorIndexList(2,3)=2
            sparseMatX2B(1)%factorIndexList(2,4)=1
            sparseMatX2B(1)%factorIndexList(2,5)=1
            sparseMatX2B(1)%factorIndexList(2,6)=2
        else
            sparseMatX2B(1)%factorIndexList(2,7)=3
            sparseMatX2B(1)%factorIndexList(2,8)=3
            sparseMatX2B(1)%factorIndexList(2,9)=4
            sparseMatX2B(1)%factorIndexList(2,10)=3
            sparseMatX2B(1)%factorIndexList(2,11)=3
            sparseMatX2B(1)%factorIndexList(2,12)=4
        endif

        ! factorList
        ! setting up mapping between X -> B
        if(lpet == 0) then
            sparseMatX2B(1)%factorList(1)=4./9
            sparseMatX2B(1)%factorList(2)=2./9
            sparseMatX2B(1)%factorList(3)=2./3
            sparseMatX2B(1)%factorList(4)=2./9
            sparseMatX2B(1)%factorList(5)=1./9
            sparseMatX2B(1)%factorList(6)=1./3
        else
            sparseMatX2B(1)%factorList(7)=2./9
            sparseMatX2B(1)%factorList(8)=1./9
            sparseMatX2B(1)%factorList(9)=1./3
            sparseMatX2B(1)%factorList(10)=4./9
            sparseMatX2B(1)%factorList(11)=2./9
            sparseMatX2B(1)%factorList(12)=2./3
        endif

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
        xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=sideA, sideBGrid=sideB, &
            area=xgrid_area, centroid=centroid, &
            sparseMatA2X=sparseMatA2X, sparseMatX2B=sparseMatX2B, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_XGridGet(xgrid, &
            sideAGridCount=sideAGC, sideBGridCount=sideBGC, &
            sideAMeshCount=sideAMC, sideBMeshCount=sideBMC, &
            sideAGrid=l_sideA, sideBGrid=l_sideB, area=l_area, &
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

        !print *, eleCount, elb, eub

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
        call ESMF_FieldGet(field, farrayPtr=xfarrayPtr, &
            exclusiveLBound=xlb, exclusiveUBound=xub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        xfarrayPtr = 0.0
        call ESMF_XGridGet(xgrid, elementCount=eleCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldPrint(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! setup and initialize src and dst Fields
        do i = 1, 2
            srcField(i) = ESMF_FieldCreate(sideA(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(srcField(i), farrayPtr=farrayPtr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            farrayPtr = 1.
        enddo
        do i = 1, 1
            dstField(i) = ESMF_FieldCreate(sideB(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(dstField(i), farrayPtr=farrayPtr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            farrayPtr = 0.0
        enddo

        ! use field on Xgrid to do smm from src to dst transformation
        ! dst = W'*W*src
        !print *, '- before SMM from A -> X'
        !print *, xlb, xub, xfarrayPtr

        ! from A -> X
        do i = 1, 2
            call ESMF_FieldRegridStore(xgrid, srcField(i), field, routehandle=rh_src2xgrid(i), &
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldRegrid(srcField(i), field, routehandle=rh_src2xgrid(i), &
                zeroregion=ESMF_REGION_SELECT, &
                checkflag=.TRUE.,&
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !print *, '- SMM from A -> X'
            !print *, xfarrayPtr
        enddo

        ! xfarrayPtr should be all 1. at this point
        ! To get the surface integral of flux on XGrid, adjust by dst area

        !do i = xlb(1), xub(1)
        !    xfarrayPtr(i) = xfarrayPtr(i) * xgrid_area(i) 
        !enddo

        !print *, '- after SMM from A -> X'
        !print *, xfarrayPtr ! should be xgrid_area

        !print *, '- B before SMM from X -> B'
        !print *, farrayPtr ! should be 0.

        ! from X -> B
        do i = 1, 1
            call ESMF_FieldRegridStore(xgrid, field, dstField(i), routehandle=rh_xgrid2dst(i), &
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldRegrid(field, dstField(i), routehandle=rh_xgrid2dst(i), &
                zeroregion=ESMF_REGION_SELECT, &
                checkflag=.TRUE.,&
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        !print *, '- B after SMM from X -> B'
        !print *, farrayPtr ! should be 1/B_area

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_XGridDestroy(xgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 2
            call ESMF_FieldDestroy(srcField(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridDestroy(sideA(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        do i = 1, 1
            call ESMF_FieldDestroy(dstField(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridDestroy(sideB(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
        deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
        deallocate(sparseMatX2B(1)%factorIndexList, sparseMatX2B(1)%factorList)

    end subroutine test_regridxg_const

    subroutine test_regridxg(rc)
        integer, intent(out)                :: rc
        integer                             :: localrc, i
        type(ESMF_XGrid)                    :: xgrid
        type(ESMF_Grid)                     :: sideA(2), sideB(1)
        type(ESMF_DistGrid)                 :: distgrid
        real(ESMF_KIND_R8)                  :: centroid(12,2), area(12)
        type(ESMF_XGridSpec)                :: sparseMatA2X(2), sparseMatX2B(1)

        type(ESMF_Grid)                     :: l_sideA(2), l_sideB(1)
        type(ESMF_DistGrid)                 :: l_sideAdg(2), l_sideBdg(1)
        real(ESMF_KIND_R8)                  :: l_centroid(12,2), l_area(12)
        type(ESMF_XGridSpec)                :: l_sparseMatA2X(2), l_sparseMatX2B(1)
        type(ESMF_Field)                    :: field, srcField(2), dstField(1)

        integer                             :: eleCount
        integer                             :: sideAGC, sideBGC, sideAMC, sideBMC
        integer                             :: elb, eub, ec, lpet, npet

        real(ESMF_KIND_R8), pointer         :: farrayPtr(:,:), xfarrayPtr(:)
        real(ESMF_KIND_R8)                  :: xgrid_area(12), B_area(2,2)
        integer                             :: xlb(1), xub(1)
        type(ESMF_RouteHandle)              :: rh_src2xgrid(2), rh_xgrid2dst(1)
        type(ESMF_VM)                       :: vm

        real(ESMF_KIND_R8)                  :: centroidA1X(2), centroidA1Y(2)
        real(ESMF_KIND_R8)                  :: centroidA2X(2), centroidA2Y(1)
        real(ESMF_KIND_R8)                  :: centroidBX(2), centroidBY(2)
        real(ESMF_KIND_R8), pointer         :: coordX(:), coordY(:)
        character(len=16)                   :: gridNameA(2), gridNameB(1)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm=vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, petCount=npet, localPet=lpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(npet /= 2) return

        gridNameA(1) = 'srcGrid 1'
        gridNameA(2) = 'srcGrid 2'
        sideA(1) = ESMF_GridCreateNoPeriDim(maxIndex=(/2,2/), &
            coordDep1=(/1/), &
            coordDep2=(/2/), &
            regDecomp=(/1,2/), &
            name=gridNameA(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sideA(2) = ESMF_GridCreateNoPeriDim(maxIndex=(/2,1/), &
            coordDep1=(/1/), &
            coordDep2=(/2/), &
            regDecomp=(/2,1/), &
            name=gridNameA(2), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, 2
            call ESMF_GridAddCoord(sideA(i), staggerloc=ESMF_STAGGERLOC_CENTER, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! SideA first grid
        centroidA1X=(/0.5, 1.5/)
        centroidA1Y=(/0.5, 1.5/)
        call ESMF_GridGetCoord(sideA(1), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            coordDim=1, farrayPtr=coordX, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordX = centroidA1X
        call ESMF_GridGetCoord(sideA(1), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            coordDim=2, farrayPtr=coordY, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordY = centroidA1Y(lpet+1)

        ! SideA second grid
        centroidA2X=(/0.5, 1.5/)
        centroidA2Y=(/2.5/)
        call ESMF_GridGetCoord(sideA(2), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            coordDim=1, farrayPtr=coordX, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordX = centroidA2X(lpet+1)
        call ESMF_GridGetCoord(sideA(2), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            coordDim=2, farrayPtr=coordY, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if(lpet == 0) coordY = centroidA2Y

        gridNameB(1) = 'dstGrid 1'
        sideB(1) = ESMF_GridCreateNoPeriDim(maxIndex=(/2,2/), &
            coordDep1=(/1/), coordDep2=(/2/), &
            regDecomp=(/1,2/), &
            name=gridNameB(1), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, 1
            call ESMF_GridAddCoord(sideB(i), staggerloc=ESMF_STAGGERLOC_CENTER, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! SideB grid
        centroidBX=(/0.75, 1.75/)
        centroidBY=(/0.75, 2.25/)
        call ESMF_GridGetCoord(sideB(1), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            coordDim=1, farrayPtr=coordX, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordX = centroidBX
        call ESMF_GridGetCoord(sideB(1), localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            coordDim=2, farrayPtr=coordY, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        coordY = centroidBY(lpet+1)

        if(lpet == 0) then
            allocate(sparseMatA2X(1)%factorIndexList(2,6), sparseMatA2X(1)%factorList(6))
        else
            allocate(sparseMatA2X(1)%factorIndexList(2,7:9), sparseMatA2X(1)%factorList(7:9))
        endif
        if(lpet == 0) then
            allocate(sparseMatA2X(2)%factorIndexList(2,1), sparseMatA2X(2)%factorList(1))
        else
            allocate(sparseMatA2X(2)%factorIndexList(2,2:3), sparseMatA2X(2)%factorList(2:3))
        endif
        if(lpet == 0) then
            allocate(sparseMatX2B(1)%factorIndexList(2,6), sparseMatX2B(1)%factorList(6))
        else
            allocate(sparseMatX2B(1)%factorIndexList(2,7:12), sparseMatX2B(1)%factorList(7:12))
        endif
    
        ! factorIndexList
        ! setting up mapping between A1 -> X
        if(lpet == 0) then
            sparseMatA2X(1)%factorIndexList(1,1)=1
            sparseMatA2X(1)%factorIndexList(1,2)=2
            sparseMatA2X(1)%factorIndexList(1,3)=2
            sparseMatA2X(1)%factorIndexList(1,4)=3
            sparseMatA2X(1)%factorIndexList(1,5)=4
            sparseMatA2X(1)%factorIndexList(1,6)=4
        else
            sparseMatA2X(1)%factorIndexList(1,7)=3
            sparseMatA2X(1)%factorIndexList(1,8)=4
            sparseMatA2X(1)%factorIndexList(1,9)=4
        endif
        if(lpet == 0) then
            sparseMatA2X(1)%factorIndexList(2,1)=1
            sparseMatA2X(1)%factorIndexList(2,2)=2
            sparseMatA2X(1)%factorIndexList(2,3)=3
            sparseMatA2X(1)%factorIndexList(2,4)=4
            sparseMatA2X(1)%factorIndexList(2,5)=5
            sparseMatA2X(1)%factorIndexList(2,6)=6
        else
            sparseMatA2X(1)%factorIndexList(2,7)=7
            sparseMatA2X(1)%factorIndexList(2,8)=8
            sparseMatA2X(1)%factorIndexList(2,9)=9
        endif
        ! setting up mapping between A2 -> X
        if(lpet == 0) then
            sparseMatA2X(2)%factorIndexList(1,1)=1
        else
            sparseMatA2X(2)%factorIndexList(1,2)=2
            sparseMatA2X(2)%factorIndexList(1,3)=2
        endif
        if(lpet == 0) then
            sparseMatA2X(2)%factorIndexList(2,1)=10
        else
            sparseMatA2X(2)%factorIndexList(2,2)=11
            sparseMatA2X(2)%factorIndexList(2,3)=12
        endif

        ! Note that the weights are dest area weighted
        ! factorList
        ! setting up mapping between A1 -> X
        if(lpet == 0) then
            sparseMatA2X(1)%factorList(1)=1
            sparseMatA2X(1)%factorList(2)=1
            sparseMatA2X(1)%factorList(3)=1
            sparseMatA2X(1)%factorList(4)=1
            sparseMatA2X(1)%factorList(5)=1
            sparseMatA2X(1)%factorList(6)=1
        else
            sparseMatA2X(1)%factorList(7)=1
            sparseMatA2X(1)%factorList(8)=1
            sparseMatA2X(1)%factorList(9)=1
        endif
        ! setting up mapping between A2 -> X
        if(lpet == 0) then
            sparseMatA2X(2)%factorList(1)=1
        else
            sparseMatA2X(2)%factorList(2)=1
            sparseMatA2X(2)%factorList(3)=1
        endif
    
        ! factorIndexList
        ! setting up mapping between X -> B
        if(lpet == 0) then
            sparseMatX2B(1)%factorIndexList(1,1)=1
            sparseMatX2B(1)%factorIndexList(1,2)=2
            sparseMatX2B(1)%factorIndexList(1,3)=3
            sparseMatX2B(1)%factorIndexList(1,4)=4
            sparseMatX2B(1)%factorIndexList(1,5)=5
            sparseMatX2B(1)%factorIndexList(1,6)=6
        else
            sparseMatX2B(1)%factorIndexList(1,7)=7
            sparseMatX2B(1)%factorIndexList(1,8)=8
            sparseMatX2B(1)%factorIndexList(1,9)=9
            sparseMatX2B(1)%factorIndexList(1,10)=10
            sparseMatX2B(1)%factorIndexList(1,11)=11
            sparseMatX2B(1)%factorIndexList(1,12)=12
        endif
        if(lpet == 0) then
            sparseMatX2B(1)%factorIndexList(2,1)=1
            sparseMatX2B(1)%factorIndexList(2,2)=1
            sparseMatX2B(1)%factorIndexList(2,3)=2
            sparseMatX2B(1)%factorIndexList(2,4)=1
            sparseMatX2B(1)%factorIndexList(2,5)=1
            sparseMatX2B(1)%factorIndexList(2,6)=2
        else
            sparseMatX2B(1)%factorIndexList(2,7)=3
            sparseMatX2B(1)%factorIndexList(2,8)=3
            sparseMatX2B(1)%factorIndexList(2,9)=4
            sparseMatX2B(1)%factorIndexList(2,10)=3
            sparseMatX2B(1)%factorIndexList(2,11)=3
            sparseMatX2B(1)%factorIndexList(2,12)=4
        endif

        ! factorList
        ! setting up mapping between X -> B
        if(lpet == 0) then
            sparseMatX2B(1)%factorList(1)=4./9
            sparseMatX2B(1)%factorList(2)=2./9
            sparseMatX2B(1)%factorList(3)=2./3
            sparseMatX2B(1)%factorList(4)=2./9
            sparseMatX2B(1)%factorList(5)=1./9
            sparseMatX2B(1)%factorList(6)=1./3
        else
            sparseMatX2B(1)%factorList(7)=2./9
            sparseMatX2B(1)%factorList(8)=1./9
            sparseMatX2B(1)%factorList(9)=1./3
            sparseMatX2B(1)%factorList(10)=4./9
            sparseMatX2B(1)%factorList(11)=2./9
            sparseMatX2B(1)%factorList(12)=2./3
        endif

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
        xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=sideA, sideBGrid=sideB, &
            area=xgrid_area, centroid=centroid, &
            sparseMatA2X=sparseMatA2X, sparseMatX2B=sparseMatX2B, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_XGridGet(xgrid, &
            sideAGridCount=sideAGC, sideBGridCount=sideBGC, &
            sideAMeshCount=sideAMC, sideBMeshCount=sideBMC, &
            sideAGrid=l_sideA, sideBGrid=l_sideB, area=l_area, &
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

        print *, eleCount, elb, eub

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
        call ESMF_FieldGet(field, farrayPtr=xfarrayPtr, &
            exclusiveLBound=xlb, exclusiveUBound=xub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        xfarrayPtr = 0.0

        call ESMF_FieldPrint(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! setup and initialize src and dst Fields
        do i = 1, 2
            srcField(i) = ESMF_FieldCreate(sideA(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(srcField(i), farrayPtr=farrayPtr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            farrayPtr = i
        enddo
        do i = 1, 1
            dstField(i) = ESMF_FieldCreate(sideB(i), typekind=ESMF_TYPEKIND_R8, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(dstField(i), farrayPtr=farrayPtr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            farrayPtr = 0.0
        enddo

        ! use field on Xgrid to do smm from src to dst transformation
        ! dst = W'*W*src
        print *, '- before SMM from A -> X'
        print *, xlb, xub, xfarrayPtr

        ! from A -> X
        do i = 1, 2
            call ESMF_FieldRegridStore(xgrid, srcField(i), field, routehandle=rh_src2xgrid(i), &
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldRegrid(srcField(i), field, routehandle=rh_src2xgrid(i), &
                zeroregion=ESMF_REGION_SELECT, &
                checkflag=.TRUE.,&
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            print *, '- SMM from A -> X'
            print *, xfarrayPtr
        enddo

        ! xfarrayPtr should be all 1. at this point
        ! To get the surface integral of flux on XGrid, adjust by dst area

        !do i = xlb(1), xub(1)
        !    xfarrayPtr(i) = xfarrayPtr(i) * xgrid_area(i) 
        !enddo

        print *, '- after SMM from A -> X'
        print *, xfarrayPtr ! should be xgrid_area

        print *, '- B before SMM from X -> B'
        print *, farrayPtr ! should be 0.

        ! from X -> B
        do i = 1, 1
            call ESMF_FieldRegridStore(xgrid, field, dstField(i), routehandle=rh_xgrid2dst(i), &
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldRegrid(field, dstField(i), routehandle=rh_xgrid2dst(i), &
                zeroregion=ESMF_REGION_SELECT, &
                checkflag=.TRUE.,&
                rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        print *, '- B after SMM from X -> B'
        print *, farrayPtr ! should be 1/B_area

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_XGridDestroy(xgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 2
            call ESMF_FieldDestroy(srcField(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridDestroy(sideA(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        do i = 1, 1
            call ESMF_FieldDestroy(dstField(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridDestroy(sideB(i), rc = localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        deallocate(sparseMatA2X(1)%factorIndexList, sparseMatA2X(1)%factorList)
        deallocate(sparseMatA2X(2)%factorIndexList, sparseMatA2X(2)%factorList)
        deallocate(sparseMatX2B(1)%factorIndexList, sparseMatX2B(1)%factorList)

    end subroutine test_regridxg

!------------------------------------------------------------------------
  ! in this test, side B grid only overlaps with half of the side A grid on
  ! the first PET
  subroutine test_regrid2xg_half(rc)
    integer, intent(out) :: rc

    type(ESMF_Grid) :: grid_atm, grid_ocn
    type(ESMF_Field) :: f_atm, f_ocn, f_xgrid
    real(ESMF_KIND_R8) :: atm_dx, atm_dy, ocn_dx, ocn_dy, startx, starty
    integer             :: atm_nx, atm_ny, ocn_nx, ocn_ny
    integer             :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer :: weights(:)
    integer(ESMF_KIND_I4), pointer :: indices(:,:)
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    type(ESMF_XGrid)     :: xgrid
    type(ESMF_XGridSpec) :: sparseMatA2X(1)
    integer              :: gn(2)
    real(ESMF_KIND_R8), pointer :: atm(:,:), ocn(:,:), exf(:)
    type(ESMF_RouteHandle)      :: rh
    
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

    if(npet /= 2) return

    ! Grid constants
    atm_nx = 10
    atm_ny = 10
    atm_dx = 0.1
    atm_dy = 0.1
    ocn_nx = 4   ! change to 100 to force a match
    ocn_ny = 4   ! change to 100 to force a match
    ocn_dx = 0.1
    ocn_dy = 0.1
    
    !------------- ATM --------------
    ! atm grid, horizontally decomposed
    grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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
    grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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

    ! Call RegridStore here to compute SMM weights and indices
    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! make sure the numbers are consistent
    print *, lpet, 'weights: ', size(weights), '-', weights
    print *, lpet, 'indices: ', size(indices,1),'-', size(indices,2)
    do j = 1, size(indices,2)
         print *, indices(1,j), '->', indices(2,j)
    enddo

    call ESMF_VMAllReduce(vm, (/size(weights), size(indices,2) /), gn, 2, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


    print *, gn(1), gn(2)
    if(gn(1) /= gn(2) .or. gn(1) /= ocn_nx*ocn_ny) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! use the weights to generate an XGrid
    allocate(sparseMatA2X(1)%factorIndexList(size(indices,1), size(indices,2)))
    do i = 1, size(indices,1)
      do j = 1, size(indices,2)
         sparseMatA2X(1)%factorIndexList(i,j) = indices(i,j)
      enddo
    enddo
    sparseMatA2X(1)%factorList=>weights
    xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=(/grid_atm/), sideBGrid=(/grid_ocn/), &
        sparseMatA2X=sparseMatA2X, rc=localrc)
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

    ! compute regrid routehandle
    call ESMF_FieldRegridStore(xgrid, f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! execute regrid
    ! once area information is retrieved from mesh regrid store, then
    ! one can perform variable flux exchange and check flux conservation
    ! for now the src flux is a constant term, this results in const dst flux
    atm = 2.
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, lpet, 'flux on xgrid: ', exf

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

    deallocate(sparseMatA2X(1)%factorIndexList)

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xg_half

!------------------------------------------------------------------------
  ! in this test, side B grid's resolution is 10 timer higher than side A grid
  ! in this test, side B grid is contained in a single cell in side A grid
  subroutine test_regrid2xg_contain(rc)
    integer, intent(out) :: rc

    type(ESMF_Grid) :: grid_atm, grid_ocn
    type(ESMF_Field) :: f_atm, f_ocn, f_xgrid
    real(ESMF_KIND_R8) :: atm_dx, atm_dy, ocn_dx, ocn_dy, startx, starty
    integer             :: atm_nx, atm_ny, ocn_nx, ocn_ny
    integer             :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer :: weights(:)
    integer(ESMF_KIND_I4), pointer :: indices(:,:)
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    type(ESMF_XGrid)     :: xgrid
    type(ESMF_XGridSpec) :: sparseMatA2X(1)
    integer              :: gn(2)
    real(ESMF_KIND_R8), pointer :: atm(:,:), ocn(:,:), exf(:)
    type(ESMF_RouteHandle)      :: rh
    
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
    atm_nx = 10
    atm_ny = 10
    atm_dx = 0.1
    atm_dy = 0.1
    ocn_nx = 8   ! change to 100 to force a match
    ocn_ny = 8   ! change to 100 to force a match
    ocn_dx = 0.01
    ocn_dy = 0.01
    
    !------------- ATM --------------
    ! atm grid, horizontally decomposed
    grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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
    grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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

    ! Call RegridStore here to compute SMM weights and indices
    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! make sure the numbers are consistent
    print *, lpet, 'weights: ', size(weights), '-', weights
    print *, lpet, 'indices: ', size(indices,1),'-', size(indices,2)
    do j = 1, size(indices,1)
         print *, indices(j,1), '->', indices(j,2)
    enddo
    call ESMF_VMAllReduce(vm, (/size(weights), size(indices,2) /), gn, 2, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    print *, gn(1), gn(2)
    if(gn(1) /= gn(2) .or. gn(1) /= ocn_nx*ocn_ny) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! use the weights to generate an XGrid
    allocate(sparseMatA2X(1)%factorIndexList(size(indices,1), size(indices,2)))
    do i = 1, size(indices,1)
      do j = 1, size(indices,2)
         sparseMatA2X(1)%factorIndexList(i,j) = indices(i,j)
      enddo
    enddo
    sparseMatA2X(1)%factorList=>weights
    xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=(/grid_atm/), sideBGrid=(/grid_ocn/), &
        sparseMatA2X=sparseMatA2X, rc=localrc)
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

    ! compute regrid routehandle
    call ESMF_FieldRegridStore(xgrid, f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! execute regrid
    ! once area information is retrieved from mesh regrid store, then
    ! one can perform variable flux exchange and check flux conservation
    ! for now the src flux is a constant term, this results in const dst flux
    atm = 2.
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, lpet, 'flux on xgrid: ', exf

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

    deallocate(sparseMatA2X(1)%factorIndexList)

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xg_contain
!------------------------------------------------------------------------
  ! in this test, side B grid's resolution is 10 timer higher than side A grid
  subroutine test_regrid2xg_clip(rc)
    integer, intent(out) :: rc

    type(ESMF_Grid) :: grid_atm, grid_ocn
    type(ESMF_Field) :: f_atm, f_ocn, f_xgrid
    real(ESMF_KIND_R8) :: atm_dx, atm_dy, ocn_dx, ocn_dy, startx, starty
    integer             :: atm_nx, atm_ny, ocn_nx, ocn_ny
    integer             :: localrc, npet, i, j, lpet
    real(ESMF_KIND_R8), pointer :: weights(:)
    integer(ESMF_KIND_I4), pointer :: indices(:,:)
    real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
    type(ESMF_XGrid)     :: xgrid
    type(ESMF_XGridSpec) :: sparseMatA2X(1)
    integer              :: gn(2)
    real(ESMF_KIND_R8), pointer :: atm(:,:), ocn(:,:), exf(:)
    type(ESMF_RouteHandle)      :: rh
    
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
    atm_nx = 10
    atm_ny = 10
    atm_dx = 0.1
    atm_dy = 0.1
    ocn_nx = 80   ! change to 100 to force a match
    ocn_ny = 80   ! change to 100 to force a match
    ocn_dx = 0.01
    ocn_dy = 0.01
    
    !------------- ATM --------------
    ! atm grid, horizontally decomposed
    grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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
    grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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

    ! Call RegridStore here to compute SMM weights and indices
    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! make sure the numbers are consistent
    print *, lpet, 'weights: ', size(weights)
    print *, lpet, 'indices: ', size(indices,2)
    !do j = 1, size(indices,2)
    !     print *, indices(1,j), '->', indices(2,j)
    !enddo
    call ESMF_VMAllReduce(vm, (/size(weights), size(indices,2) /), gn, 2, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    print *, gn(1), gn(2)
    print *, lpet, 'src idx range: ', minval(indices(1,:)), maxval(indices(1,:))
    print *, lpet, 'dst idx range: ', minval(indices(2,:)), maxval(indices(2,:))
    !if(gn(1) /= gn(2) .or. gn(1) /= ocn_nx*ocn_ny) then
    !  call ESMF_LogSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)
    !  return
    !endif

    ! use the weights to generate an XGrid
    allocate(sparseMatA2X(1)%factorIndexList(size(indices,1), size(indices,2)))
    do i = 1, size(indices,1)
      do j = 1, size(indices,2)
         sparseMatA2X(1)%factorIndexList(i,j) = indices(i,j)
      enddo
    enddo
    sparseMatA2X(1)%factorList=>weights
    xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=(/grid_atm/), sideBGrid=(/grid_ocn/), &
        sparseMatA2X=sparseMatA2X, rc=localrc)
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

    ! compute regrid routehandle
    call ESMF_FieldRegridStore(xgrid, f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! execute regrid
    ! once area information is retrieved from mesh regrid store, then
    ! one can perform variable flux exchange and check flux conservation
    ! for now the src flux is a constant term, this results in const dst flux
    atm = 2.
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    !print *, lpet, 'flux on xgrid: ', exf

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

    deallocate(sparseMatA2X(1)%factorIndexList)

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xg_clip

!------------------------------------------------------------------------
  subroutine test_regrid2xg(atm_nx, atm_ny, ocn_nx, ocn_ny, atm_dx, atm_dy, &
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
    type(ESMF_RouteHandle)          :: rh
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

    ! Grid constants
    ! These are input arguments from upper level now
    !atm_nx = 10
    !atm_ny = 10
    !atm_dx = 0.1
    !atm_dy = 0.1
    !ocn_nx = 14   ! change to 100 to force a match
    !ocn_ny = 14   ! change to 100 to force a match
    !ocn_dx = 0.06
    !ocn_dy = 0.06
    
    !------------- ATM --------------
    ! atm grid, horizontally decomposed
    grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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
    grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
      indexflag=ESMF_INDEX_GLOBAL, &
      !gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      regDecomp=(/npet, 1/), &
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

    ! Call RegridStore here to compute SMM weights and indices
    call ESMF_FieldRegridStore(srcField=f_atm, dstField=f_ocn, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! make sure the numbers are consistent
    print *, lpet, size(weights), '-', weights
    print *, lpet, size(indices,1),'-', size(indices,2)
    do j = 1, size(indices,2)
         print *, indices(1,j), '->', indices(2,j)
    enddo

    simax = maxval(indices(1,:))
    dimax = maxval(indices(2,:))
    call ESMF_VMAllReduce(vm, (/ simax, dimax /), gn, 2, ESMF_REDUCE_MAX, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    print *, gn(1), gn(2)
    if(gn(2) /= ocn_nx*ocn_ny) then
      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! analyze the returned the weights based on area information

    ! use the weights to generate an XGrid
    allocate(sparseMatA2X(1)%factorIndexList(size(indices,1), size(indices,2)))
    do i = 1, size(indices,1)
      do j = 1, size(indices,2)
         sparseMatA2X(1)%factorIndexList(i,j) = indices(i,j)
      enddo
    enddo
    sparseMatA2X(1)%factorList=>weights
    xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=(/grid_atm/), sideBGrid=(/grid_ocn/), &
        sparseMatA2X=sparseMatA2X, rc=localrc)
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

    ! compute regrid routehandle
    call ESMF_FieldRegridStore(xgrid, f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! execute regrid
    ! once area information is retrieved from mesh regrid store, then
    ! one can perform variable flux exchange and check flux conservation
    ! for now the src flux is a constant term, this results in const dst flux
    atm = 2.
    call ESMF_FieldRegrid(f_atm, f_xgrid, routehandle=rh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    print *, lpet, exf

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

    deallocate(sparseMatA2X(1)%factorIndexList)

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xg

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
    grid_atm = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
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
    grid_ocn = ESMF_GridCreateNoPeriDim(maxIndex=(/ocn_nx, ocn_ny/), &
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
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
      factorIndexList=indices, factorList=weights, rc=localrc)
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
    xgrid = ESMF_XGridCreateFromSparseMat(sideAGrid=sideA, sideBGrid=sideB, &
      sparseMatA2X=A2X, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridDestroy(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    rc = ESMF_SUCCESS

  end subroutine test_regrid2xgSph

end program ESMF_FieldRegridXGSMMUTest
