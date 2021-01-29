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
program ESMF_FieldGatherUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldGatherUTest - This test verifies FieldGather functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledGather.
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

#ifdef ESMF_TESTEXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Gather test
        call test_gather_1d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldGather 1d test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Gather test
        call test_gather_2d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldGather 2d test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Gather test
        call test_gather_3d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldGather 3d test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Scatter test
        call test_scatter_2d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldScatter 2d test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_1d"
    subroutine test_gather_1d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: field
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        !type(ESMF_ArraySpec)                        :: arrayspec
        type(ESMF_Array)                            :: array
        integer                                     :: localrc, lpe, i, j

        integer, allocatable                        :: farray(:)
        integer, allocatable                        :: farrayDst(:)
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

        distgrid = ESMF_DistGridCreate(minIndex =(/1/), maxIndex=(/16/), &
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

        allocate(farray(fa_shape(1)))
        farray = lpe
        array = ESMF_ArrayCreate(distgrid, farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(lpe .eq. 0) then
          allocate(farrayDst(16))
        else
          allocate(farrayDst(0))
        end if
        call ESMF_FieldGather(field, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do i = 1, 4
                do j = 1, 4
                    if(farrayDst((i-1)*4+j) .ne. i-1) localrc=ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        call ESMF_FieldDestroy(field)
        call ESMF_GridDestroy(grid)
        call ESMF_ArrayDestroy(array)
        call ESMF_DistGridDestroy(distgrid)
        deallocate(farray)
        if(lpe .eq. 0) deallocate(farrayDst)
        rc = ESMF_SUCCESS
    end subroutine test_gather_1d

#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_2d"
    subroutine test_gather_2d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: field
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        !type(ESMF_ArraySpec)                        :: arrayspec
        type(ESMF_Array)                            :: array
        integer                                     :: localrc, lpe, i, j

        integer, allocatable                        :: farray(:,:)
        integer, allocatable                        :: farrayDst(:,:)
        integer                                     :: fa_shape(2)

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

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(fa_shape(1), fa_shape(2)))
        farray = lpe
        array = ESMF_ArrayCreate(distgrid, farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(lpe .eq. 0) then
          allocate(farrayDst(10,20))
        else
          allocate(farrayDst(0,0))
        end if
        call ESMF_FieldGather(field, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do i = 1, 2
                do j = 1, 2
                    if(farrayDst(i*5, j*10) .ne. (i-1)+(j-1)*2) localrc=ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        call ESMF_FieldDestroy(field)
        call ESMF_GridDestroy(grid)
        call ESMF_ArrayDestroy(array)
        deallocate(farray)
        if(lpe .eq. 0) deallocate(farrayDst)
        rc = ESMF_SUCCESS
    end subroutine test_gather_2d

#undef ESMF_METHOD
#define ESMF_METHOD "test_gather_3d"
    subroutine test_gather_3d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: field
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        !type(ESMF_ArraySpec)                        :: arrayspec
        type(ESMF_Array)                            :: array
        integer                                     :: localrc, lpe, i, j, k

        integer, allocatable                        :: farray(:,:,:)
        integer, allocatable                        :: farrayDst(:,:,:)
        integer                                     :: fa_shape(3)

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

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/), maxIndex=(/10,20,5/), &
            regDecomp=(/2,2,1/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(fa_shape(1), fa_shape(2), fa_shape(3)))
        farray = lpe
        array = ESMF_ArrayCreate(distgrid, farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(lpe .eq. 0) then
          allocate(farrayDst(10,20,5))
        else
          allocate(farrayDst(0,0,0))
        end if
        call ESMF_FieldGather(field, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do k = 1, 5
                do j = 1, 2
                    do i = 1, 2
                        if(farrayDst(i*5, j*10, k) .ne. (i-1)+(j-1)*2) localrc=ESMF_FAILURE
                    enddo
                enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        call ESMF_FieldDestroy(field)
        call ESMF_GridDestroy(grid)
        call ESMF_ArrayDestroy(array)
        deallocate(farray)
        if(lpe .eq. 0) deallocate(farrayDst)
        rc = ESMF_SUCCESS
    end subroutine test_gather_3d

#undef ESMF_METHOD
#define ESMF_METHOD "test_scatter_2d"
    subroutine test_scatter_2d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_Field)                            :: field
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        !type(ESMF_ArraySpec)                        :: arrayspec
        type(ESMF_Array)                            :: array
        integer                                     :: localrc, lpe, i, j

        integer, allocatable                        :: farray(:,:)
        integer, allocatable                        :: farraySrc(:,:)
        integer                                     :: fa_shape(2)
        integer, pointer                            :: fptr(:,:)

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

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalCount=fa_shape, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(fa_shape(1), fa_shape(2)))
        farray = lpe
        array = ESMF_ArrayCreate(distgrid, farray, indexflag=ESMF_INDEX_DELOCAL, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! initialize values to be scattered
        if(lpe .eq. 0) then
            allocate(farraySrc(10,20))
            farraySrc(1:5,1:10) = 0
            farraySrc(6:10,1:10) = 1
            farraySrc(1:5,11:20) = 2
            farraySrc(6:10,11:20) = 3
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        else
          allocate (farraySrc(0,0))
        endif

        call ESMF_FieldScatter(field, farraySrc, rootPet=0, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify that the scattered data is properly distributed
        do i = lbound(fptr, 1), ubound(fptr, 1)
            do j = lbound(fptr, 2), ubound(fptr, 2)
                if(fptr(i, j) .ne. lpe) localrc = ESMF_FAILURE
            enddo
        enddo
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field)
        call ESMF_GridDestroy(grid)
        call ESMF_ArrayDestroy(array)
        deallocate(farray)
        if(lpe .eq. 0) deallocate(farraySrc)
        rc = ESMF_SUCCESS
    end subroutine test_scatter_2d
 
#endif

end program ESMF_FieldGatherUTest
