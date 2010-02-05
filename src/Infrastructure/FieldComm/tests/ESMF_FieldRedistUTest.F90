! $Id: ESMF_FieldRedistUTest.F90,v 1.12.2.1 2010/02/05 19:56:55 svasquez Exp $
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
program ESMF_FieldRedistUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRedistUTest - This test verifies FieldRedist functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledRedist.
!EOPI
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldRedistUTest.F90,v 1.12.2.1 2010/02/05 19:56:55 svasquez Exp $'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

#ifdef ESMF_TESTEXHAUSTIVE

    ! individual test name
    character(ESMF_MAXSTR) :: name

    ! individual test failure messages
    character(ESMF_MAXSTR*2) :: failMsg
#endif

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if(rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_1d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist basic test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_3d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist 3d fields with ungridded bounds and halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_5d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist congruent 5d fields with ungridded bounds and halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_5dt(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist congruent 5d fields with ungridded bounds and halos " // &
            " srcToDstTransposeMap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_redist_1d"
    subroutine test_redist_1d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_Array)                            :: srcArray, dstArray
        integer                                     :: localrc, lpe, i

        integer, allocatable                        :: src_farray(:), dst_farray(:)
        integer                                     :: fa_shape(1)
        integer, pointer                            :: fptr(:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create distgrid and grid
        distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
            regDecomp=(/4/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, &
            gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
            name="grid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create src_farray, srcArray, and srcField
        allocate(src_farray(fa_shape(1)) )
        src_farray = lpe
        srcArray = ESMF_ArrayCreate(src_farray, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcField = ESMF_FieldCreate(grid, srcArray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create dst_farray, dstArray, and dstField
        allocate(dst_farray(fa_shape(1)) )
        dst_farray = 0
        dstArray = ESMF_ArrayCreate(dst_farray, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstField = ESMF_FieldCreate(grid, dstArray, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldRedist(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! Verify that the redistributed data in dstField is correct.
        ! Before the redist op, the dst Field contains all 0. 
        ! The redist op reset the values to the PE value, verify this is the case.
        do i = lbound(fptr, 1), ubound(fptr, 1)
            if(fptr(i) .ne. lpe) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_ArrayDestroy(srcArray)
        call ESMF_ArrayDestroy(dstArray)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        deallocate(src_farray, dst_farray)

        rc = ESMF_SUCCESS
    end subroutine test_redist_1d

#undef ESMF_METHOD
#define ESMF_METHOD "test_redist_3d"
    subroutine test_redist_3d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: field, srcField, dstField
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j, k
        integer                                     :: exLB(3), exUB(3)

        integer(ESMF_KIND_I4), pointer              :: srcfptr(:,:,:), dstfptr(:,:,:), fptr(:,:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            maxHaloLWidth=(/1,1/), maxHaloUWidth=(/1,2/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcField = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            maxHaloLWidth=(/1,1/), maxHaloUWidth=(/1,2/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcfptr = lpe

        dstField = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            maxHaloLWidth=(/1,1/), maxHaloUWidth=(/1,2/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstfptr = 0

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldRedist(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! Verify that the redistributed data in dstField is correct.
        ! Before the redist op, the dst Field contains all 0. 
        ! The redist op reset the values to the PE value, verify this is the case.
        ! MUST use exclusive bounds because Redist operates within excl. region.
        do k = exLB(3), exUB(3)
            do j = exLB(2), exUB(2)
                do i = exLB(1), exUB(1)
                   if(fptr(i,j,k) .ne. lpe) localrc = ESMF_FAILURE
                enddo
            enddo
        enddo
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(field)
        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)

        rc = ESMF_SUCCESS
    end subroutine test_redist_3d

#undef ESMF_METHOD
#define ESMF_METHOD "test_redist_5d"
    subroutine test_redist_5d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Grid)                             :: srcGrid, dstGrid
        type(ESMF_DistGrid)                         :: srcDistgrid, dstDistgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe
        !integer                                     :: i, j, k, l, m
        integer                                     :: exLB(5), exUB(5)

        integer(ESMF_KIND_I4), pointer              :: srcfptr(:,:,:,:,:), dstfptr(:,:,:,:,:)
        integer(ESMF_KIND_I4), pointer              :: fptr(:,:,:,:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create src and dst Grids which are congruent but different in decomposition
        srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,8,4/), &
            regDecomp=(/2,1,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcGrid = ESMF_GridCreate(distgrid=srcDistgrid, name="srcgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,8,4/), &
            regDecomp=(/2,2,1/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstGrid = ESMF_GridCreate(distgrid=dstDistgrid, name="dstgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, 5, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create src and dst Fields
        srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            maxHaloLWidth=(/1,1,1/), maxHaloUWidth=(/1,2,3/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcfptr = lpe

        dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            maxHaloLWidth=(/1,1,1/), maxHaloUWidth=(/1,2,3/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstfptr = 0

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle=routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldRedist(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! Verify that the redistributed data in dstField is correct.
        ! Before the redist op, the dst Field contains all 0. 
        ! The redist op reset the values to the PE value, verify this is the case.
        ! MUST use exclusive bounds because Redist operates within excl. region.
        ! This test is somewhat difficult to verify when the values depend on
        ! decomposition pattern of PETs. 
        ! do m = exLB(5), exUB(5)
        !     do l = exLB(4), exUB(4)
        !         do k = exLB(3), exUB(3)
        !             do j = exLB(2), exUB(2)
        !                 do i = exLB(1), exUB(1)
        !                    if(fptr(i,j,k,l,m) .ne. lpe) localrc = ESMF_FAILURE
        !                 enddo
        !             enddo
        !         enddo
        !     enddo
        ! enddo
        ! if (ESMF_LogMsgFoundError(localrc, &
        !     ESMF_ERR_PASSTHRU, &
        !     ESMF_CONTEXT, rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_GridDestroy(srcGrid)
        call ESMF_GridDestroy(dstGrid)
        call ESMF_DistGridDestroy(srcDistgrid)
        call ESMF_DistGridDestroy(dstDistgrid)

        rc = ESMF_SUCCESS
    end subroutine test_redist_5d

#undef ESMF_METHOD
#define ESMF_METHOD "test_redist_5dt"
    subroutine test_redist_5dt(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Grid)                             :: srcGrid, dstGrid
        type(ESMF_DistGrid)                         :: srcDistgrid, dstDistgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j, k, l, m
        integer                                     :: exLB(5), exUB(5)

        integer(ESMF_KIND_I4), pointer              :: srcfptr(:,:,:,:,:), dstfptr(:,:,:,:,:)
        integer(ESMF_KIND_I4), pointer              :: fptr(:,:,:,:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create src and dst Grids which are congruent but different in decomposition
        srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,4,8/), &
            regDecomp=(/2,1,2/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcGrid = ESMF_GridCreate(distgrid=srcDistgrid, name="srcgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,8,4/), &
            regDecomp=(/2,2,1/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstGrid = ESMF_GridCreate(distgrid=dstDistgrid, name="dstgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_ArraySpecSet(arrayspec, 5, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create src and dst Fields
        srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            maxHaloLWidth=(/1,1,1/), maxHaloUWidth=(/1,3,2/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        srcfptr = lpe

        dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            maxHaloLWidth=(/1,1,1/), maxHaloUWidth=(/1,2,3/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstfptr = 0

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, &
            srcToDstTransposeMap=(/1,3,2,4,5/), routehandle=routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldRedist(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! Verify that the redistributed data in dstField is correct.
        ! Before the redist op, the dst Field contains all 0. 
        ! The redist op reset the values to the PE value, verify this is the case.
        ! MUST use exclusive bounds because Redist operates within excl. region.
        do m = exLB(5), exUB(5)
            do l = exLB(4), exUB(4)
                do k = exLB(3), exUB(3)
                    do j = exLB(2), exUB(2)
                        do i = exLB(1), exUB(1)
                           if(fptr(i,j,k,l,m) .ne. lpe) localrc = ESMF_FAILURE
                        enddo
                    enddo
                enddo
            enddo
        enddo
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_GridDestroy(srcGrid)
        call ESMF_GridDestroy(dstGrid)
        call ESMF_DistGridDestroy(srcDistgrid)
        call ESMF_DistGridDestroy(dstDistgrid)

        rc = ESMF_SUCCESS
    end subroutine test_redist_5dt
 
#endif

end program ESMF_FieldRedistUTest
