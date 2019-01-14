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
program ESMF_FieldSMMUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldSMMUTest - This test verifies FieldSMM functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledSMM.
!EOPI
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_FieldSMMMod
  
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

#ifdef ESMF_TESTEXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldSMM basic test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1dweak(rc)
        write(failMsg, *) ""
        write(name, *) "FieldSMM basic test with varying ungridded bounds"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1da(rc)
        write(failMsg, *) ""
        write(name, *) "FieldSMM basic test using lpe for dst"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1db(rc)
        write(failMsg, *) ""
        write(name, *) "FieldSMM basic test using lpe for both src and dst"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1db_bfb(rc)
        write(failMsg, *) ""
        write(name, *) "FieldSMM basic test using lpe for both src and dst"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1dc(rc)
        write(failMsg, *) ""
        write(name, *) "FieldSMM basic test using lpe for both src and dst, with halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1d"
    subroutine test_smm_1d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_Array)                            :: srcArray, dstArray
        integer                                     :: localrc, lpe

        integer, allocatable                        :: src_farray(:), dst_farray(:)
        integer                                     :: fa_shape(1)
        integer, pointer                            :: fptr(:)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

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

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src_farray, srcArray, and srcField
        allocate(src_farray(fa_shape(1)) )
        src_farray = lpe+1
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

        ! initialize factorList and factorIndexList
        if(lpe == 0) then
            allocate(factorList(1))
            allocate(factorIndexList(2,1))
            factorList = (/3/)
            factorIndexList(1,:) = (/1/)
            factorIndexList(2,:) = (/3/)
            call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                factorList, factorIndexList, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else
            call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! perform smm
        call ESMF_FieldSMM(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the smm data in dstField is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the PE value, verify this is the case.
        ! print *, lpe, fptr
        if(lpe == 0) then
            if(fptr(3) .ne. 3) localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! release route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
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
        if(lpe == 0) deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1d

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1dweak"
    subroutine test_smm_1dweak(rc)
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
        integer, pointer                            :: fptr(:)
        integer                                     :: fa_shape(1)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

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

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create src_farray, srcArray, and srcField
        allocate(src_farray(fa_shape(1)) )
        src_farray = lpe+1
        srcArray = ESMF_ArrayCreate(distgrid, src_farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcField = ESMF_FieldCreate(grid, srcArray, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/5/), rc=localrc)
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

        dstFieldA = ESMF_FieldCreate(grid, arrayspec, gridToFieldMap=(/2/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/5/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! initialize factorList and factorIndexList
        if(lpe == 0) then
            allocate(factorList(1))
            allocate(factorIndexList(2,1))
            factorList = (/3/)
            factorIndexList(1,:) = (/1/)
            factorIndexList(2,:) = (/3/)
            call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                factorList, factorIndexList, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else
            call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! perform smm with a different pair of Fields
        call ESMF_FieldSMM(srcfieldA, dstFieldA, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the smm data in dstField is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the PE value, verify this is the case.
        ! print *, lpe, fptr
        if(lpe == 0) then
            if(fptr(3) .ne. 3) localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        ! release route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
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
        if(lpe == 0) deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1dweak

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1da"
    subroutine test_smm_1da(rc)
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
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

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
        src_farray = lpe+1
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

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/1,2,3,4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
            factorList, factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform smm
        call ESMF_FieldSMM(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the smm data in dstField is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the PE value, verify this is the case.
        ! print *, lpe, fptr
        do i = lbound(fptr, 1), ubound(fptr, 1)
            if(fptr(i) /= i) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
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
        deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1da
 
#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1db"
    subroutine test_smm_1db(rc)
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
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

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
        src_farray = lpe+1
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

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
            factorList, factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform smm
        call ESMF_FieldSMM(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the smm data in dstField is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the index value, verify this is the case.
        ! print *, lpe, fptr
        do i = lbound(fptr, 1), ubound(fptr, 1)
            if(fptr(i) .ne. i*(lpe+1)) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
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
        deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1db

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1db_bfb"
    subroutine test_smm_1db_bfb(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: srcField, dstField
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_Array)                            :: srcArray, dstArray
        integer                                     :: localrc, lpe, i, stp, pld

        integer, allocatable                        :: src_farray(:), dst_farray(:)
        integer                                     :: fa_shape(1)
        integer, pointer                            :: fptr(:)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        stp = 2
        pld = 2

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
        src_farray = lpe+1
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

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
            factorList, factorIndexList, srcTermProcessing=stp, pipeLineDepth=pld, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform smm
        call ESMF_FieldSMM(srcfield, dstField, routehandle, &
          termorderflag=ESMF_TERMORDER_SRCPET, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the smm data in dstField is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the index value, verify this is the case.
        ! print *, lpe, fptr
        do i = lbound(fptr, 1), ubound(fptr, 1)
            if(fptr(i) .ne. i*(lpe+1)) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
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
        deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1db_bfb

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1dc"
    subroutine test_smm_1dc(rc)
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
        integer                                     :: fa_shape(1), exlb(1), exub(1)
        integer, pointer                            :: fptr(:)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

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
        src_farray = lpe+1
        srcArray = ESMF_ArrayCreate(distgrid, src_farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcField = ESMF_FieldCreate(grid, srcArray, &
            totalLWidth=(/1/), totalUWidth=(/1/), rc=localrc)
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

        dstField = ESMF_FieldCreate(grid, dstArray, &
            totalLWidth=(/1/), totalUWidth=(/1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldSMMStore(srcField, dstField, routehandle, &
            factorList, factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform smm
        call ESMF_FieldSMM(srcfield, dstField, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, &
            exclusiveLBound=exlb, exclusiveUBound=exub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! Verify that the smm data in dstField is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the index value, verify this is the case.
        ! print *, lpe, fptr
        do i = exlb(1), exub(1)
            if(fptr(i) /= i*(lpe+1)) localrc = ESMF_FAILURE
        enddo
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release route handle
        call ESMF_FieldSMMRelease(routehandle, rc=localrc)
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
        deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1dc
#endif

end program ESMF_FieldSMMUTest
