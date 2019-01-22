! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
    use ESMF
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
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
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
    call ESMF_LogSet(flush=.true.)

#ifdef ESMF_TESTEXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_1d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist basic test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_1dweak(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist basic test with compliant fields"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_3d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist 3d fields with ungridded bounds and halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
#ifndef ESMF_NO_GREATER_THAN_4D
        call test_redist_5d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist congruent 5d fields with ungridded bounds and halos"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
#ifndef ESMF_NO_GREATER_THAN_4D
        call test_redist_5dt(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist congruent 5d fields with ungridded bounds and halos " // &
            " srcToDstTransposeMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_redist_mesh(rc)
        write(failMsg, *) "results not correct"
        write(name, *) "FieldRedist on Fields built on Meshes"

        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create distgrid and grid
        distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
            regDecomp=(/4/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, &
            gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
            name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src_farray, srcArray, and srcField
        allocate(src_farray(fa_shape(1)) )
        src_farray = lpe
        srcArray = ESMF_ArrayCreate(distgrid, src_farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcField = ESMF_FieldCreate(grid, srcArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create dst_farray, dstArray, and dstField
        allocate(dst_farray(fa_shape(1)) )
        dst_farray = 0
        dstArray = ESMF_ArrayCreate(distgrid, dst_farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstField = ESMF_FieldCreate(grid, dstArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRedist(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the redistributed data in dstField is correct.
        ! Before the redist op, the dst Field contains all 0. 
        ! The redist op reset the values to the PE value, verify this is the case.
        do i = lbound(fptr, 1), ubound(fptr, 1)
            if(fptr(i) .ne. lpe) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
#define ESMF_METHOD "test_redist_1dweak"
    subroutine test_redist_1dweak(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Field)                            :: srcFieldA, dstFieldA
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_Array)                            :: srcArray, dstArray
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe

        integer, allocatable                        :: src_farray(:), dst_farray(:)
        integer, pointer                            :: f1(:,:), f2(:,:)
        integer                                     :: fa_shape(1)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create distgrid and grid
        distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
            regDecomp=(/4/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, &
            gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
            name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src_farray, srcArray, and srcField
        allocate(src_farray(fa_shape(1)) )
        src_farray = lpe
        srcArray = ESMF_ArrayCreate(distgrid, src_farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcField = ESMF_FieldCreate(grid, srcArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(srcFieldA, farrayPtr=f1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        f1 = lpe

        ! create dst_farray, dstArray, and dstField
        allocate(dst_farray(fa_shape(1)) )
        dst_farray = 0
        dstArray = ESMF_ArrayCreate(distgrid, dst_farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstField = ESMF_FieldCreate(grid, dstArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstFieldA, farrayPtr=f2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRedist(srcfieldA, dstFieldA, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        !print *, lpe, lbound(f2,2), ubound(f2,2), f2

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_FieldDestroy(srcFieldA)
        call ESMF_FieldDestroy(dstFieldA)
        call ESMF_ArrayDestroy(srcArray)
        call ESMF_ArrayDestroy(dstArray)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        deallocate(src_farray, dst_farray)

        rc = ESMF_SUCCESS
    end subroutine test_redist_1dweak

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcField = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcfptr = lpe

        dstField = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstfptr = -99

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRedist(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field)
        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)

        rc = ESMF_SUCCESS
    end subroutine test_redist_3d

#ifndef ESMF_NO_GREATER_THAN_4D

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src and dst Grids which are congruent but different in decomposition
        srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,8,4/), &
            regDecomp=(/2,1,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcGrid = ESMF_GridCreate(distgrid=srcDistgrid, name="srcgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,8,4/), &
            regDecomp=(/2,2,1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstGrid = ESMF_GridCreate(distgrid=dstDistgrid, name="dstgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, 5, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src and dst Fields
        srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            totalLWidth=(/1,1,1/), totalUWidth=(/1,2,3/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcfptr = lpe

        dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            totalLWidth=(/1,1,1/), totalUWidth=(/1,2,3/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstfptr = -99

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle=routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRedist(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
        ! if (ESMF_LogFoundError(localrc, &
        !     ESMF_ERR_PASSTHRU, &
        !     ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src and dst Grids which are congruent but different in decomposition
        srcDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,4,8/), &
            regDecomp=(/2,1,2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcGrid = ESMF_GridCreate(distgrid=srcDistgrid, name="srcgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstDistgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/4,8,4/), &
            regDecomp=(/2,2,1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstGrid = ESMF_GridCreate(distgrid=dstDistgrid, name="dstgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, 5, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src and dst Fields
        srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            totalLWidth=(/1,1,1/), totalUWidth=(/1,3,2/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(srcField, localDe=0, farrayPtr=srcfptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcfptr = lpe

        dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
            ungriddedLBound=(/1,1/), ungriddedUBound=(/3,4/), &
            totalLWidth=(/1,1,1/), totalUWidth=(/1,2,3/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=dstfptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstfptr = -99

        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, &
            srcToDstTransposeMap=(/1,3,2,4,5/), routehandle=routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRedist(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify redist
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)
        call ESMF_GridDestroy(srcGrid)
        call ESMF_GridDestroy(dstGrid)
        call ESMF_DistGridDestroy(srcDistgrid)
        call ESMF_DistGridDestroy(dstDistgrid)

        rc = ESMF_SUCCESS
    end subroutine test_redist_5dt
#endif

#undef ESMF_METHOD
#define ESMF_METHOD "test_redist_mesh"
    subroutine test_redist_mesh(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Mesh)                             :: srcMesh, dstMesh
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        integer                                     :: localrc, lpe, i
        integer(ESMF_KIND_I4), pointer                            :: src_farray(:), dst_farray(:)
        integer, pointer :: nodeIds(:),nodeOwners(:)
        real(ESMF_KIND_R8), pointer :: nodeCoords(:)
        real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
        integer :: numNodes, numOwnedNodes, numOwnedNodesTst
        integer :: numElems,numOwnedElemsTst
        integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
        integer :: i1,i2, localPet, petCount

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! SETUP MESH INFO
        !!!!!!!!!!!!!!!!!!!!!
        ! 
        !              Mesh Ids
        !
        !  2.0   7 ------- 8 -------- 9
        !        |         |          |
        !        |    3    |    4     |
        !        |         |          |
        !  1.0   4 ------- 5 -------- 6
        !        |         |          |
        !        |    1    |    2     |
        !        |         |          |
        !  0.0   1 ------- 2 -------- 3
        !
        !       0.0       1.0        2.0 
        !
        !      Node Ids at corners
        !      Element Ids in centers
        ! 
        !!!!! 
        !             Mesh Owners
        !
        !  2.0   2 ------- 2 -------- 3
        !        |         |          |
        !        |    2    |    3     |
        !        |         |          |
        !  1.0   0 ------- 0 -------- 1
        !        |         |          |
        !        |    0    |    1     |
        !        |         |          |
        !  0.0   0 ------- 0 -------- 1
        !
        !       0.0       1.0        2.0 
        !
        !      Node Owners at corners
        !      Element Owners in centers
        ! 
        
        ! Only do this if we have 4 PETs
        if (petCount .ne. 4) then
           call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="- this test is being run with the wrong number of processors", &
                ESMF_CONTEXT, rcToReturn=rc)
           return
        endif
        
        ! Setup src mesh data depending on PET
        if (localPet .eq. 0) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=4

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/1,2,4,5/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/0.0,0.0, &
                1.0,0.0, &
                0.0,1.0, &
                1.0,1.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/0,0,0,0/) ! everything on proc 0

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/1/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)
        else if (localPet .eq. 1) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=2

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/2,3,5,6/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/1.0,0.0, &
                2.0,0.0, &
                1.0,1.0, &
                2.0,1.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/0,1,0,1/) 

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/2/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)
        else if (localPet .eq. 2) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=2

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/4,5,7,8/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/0.0,1.0, &
                1.0,1.0, &
                0.0,2.0, &
                1.0,2.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/0,0,2,2/) 

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/3/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)  
        else 
           ! Fill in node data
           numNodes=4
           numOwnedNodes=1

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/5,6,8,9/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/1.0,1.0, &
                2.0,1.0, &
                1.0,2.0, &
                2.0,2.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/0,1,2,3/) 

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/4/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)  
        endif

        ! Create Mesh structure in 1 step
        srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
             nodeIds=nodeIds, nodeCoords=nodeCoords, &
             nodeOwners=nodeOwners, elementIds=elemIds,&
             elementTypes=elemTypes, elementConn=elemConn, &
             rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return


        ! Create the src Field
        srcField = ESMF_FieldCreate(srcMesh, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return


        ! Load test data into the source Field
        call ESMF_FieldGet(srcField, farrayPtr=src_farray,  rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! set interpolated function
        i2=1
        do i1=1,numNodes
           if (nodeOwners(i1) .eq. localPet) then
              
              ! Set source function
              src_farray(i2) = nodeIds(i1)
              
              ! Advance to next owner
              i2=i2+1
           endif
        enddo


        ! Deallocate Arrays from above
        deallocate(nodeIds)
        deallocate(nodeCoords)
        deallocate(nodeOwners)
        deallocate(elemIds)
        deallocate(elemTypes)
        deallocate(elemConn)


        ! Same Mesh, but on different processors
        if (localPet .eq. 0) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=1

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/5,6,8,9/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/1.0,1.0, &
                2.0,1.0, &
                1.0,2.0, &
                2.0,2.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/1,2,3,0/) 

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/4/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)  
        else if (localPet .eq. 1) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=4

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/1,2,4,5/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/0.0,0.0, &
                1.0,0.0, &
                0.0,1.0, &
                1.0,1.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/1,1,1,1/) ! everything on proc 0

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/1/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)
        else if (localPet .eq. 2) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=2

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/2,3,5,6/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/1.0,0.0, &
                2.0,0.0, &
                1.0,1.0, &
                2.0,1.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/1,2,1,2/) 

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/2/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)
        else if (localPet .eq. 3) then
           ! Fill in node data
           numNodes=4
           numOwnedNodes=2

           !! node ids
           allocate(nodeIds(numNodes))
           nodeIds=(/4,5,7,8/) 

           !! node Coords
           allocate(nodeCoords(numNodes*2))
           nodeCoords=(/0.0,1.0, &
                1.0,1.0, &
                0.0,2.0, &
                1.0,2.0/)

           !! node owners
           allocate(nodeOwners(numNodes))
           nodeOwners=(/1,1,3,3/) 

           ! Fill in elem data
           numElems=1

           !! elem ids
           allocate(elemIds(numElems))
           elemIds=(/3/) 

           !! elem type
           allocate(elemTypes(numElems))
           elemTypes=ESMF_MESHELEMTYPE_QUAD

           !! elem conn
           allocate(elemConn(numElems*4))
           elemConn=(/1,2,4,3/)  
        endif

        ! Create dst Mesh structure 
        dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
             nodeIds=nodeIds, nodeCoords=nodeCoords, &
             nodeOwners=nodeOwners, elementIds=elemIds,&
             elementTypes=elemTypes, elementConn=elemConn, &
             rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return


        ! Create the dst Field
        dstField = ESMF_FieldCreate(dstMesh, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return


        ! Load test data into the source Field
        call ESMF_FieldGet(dstField, farrayPtr=dst_farray,  rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

         ! Init dst_farray
         dst_farray(:)=0


        ! perform redist
        call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldRedist(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return


        ! Check results
        i2=1
        do i1=1,numNodes
           if (nodeOwners(i1) .eq. localPet) then
              
              ! Check results
              if (dst_farray(i2) .ne. nodeIds(i1)) then
                 call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
                      msg="- redisted values not correct ", &
                      ESMF_CONTEXT, rcToReturn=rc)
                 return
              endif
              
              ! Advance to next owner
              i2=i2+1
           endif
        enddo


        ! Deallocate Arrays from above
        deallocate(nodeIds)
        deallocate(nodeCoords)
        deallocate(nodeOwners)
        deallocate(elemIds)
        deallocate(elemTypes)
        deallocate(elemConn)

        ! Clean up classes
        call ESMF_FieldDestroy(srcField)
        call ESMF_FieldDestroy(dstField)

        call ESMF_MeshDestroy(srcMesh)
        call ESMF_MeshDestroy(dstMesh)

        rc = ESMF_SUCCESS
    end subroutine test_redist_mesh


 
#endif

end program ESMF_FieldRedistUTest
