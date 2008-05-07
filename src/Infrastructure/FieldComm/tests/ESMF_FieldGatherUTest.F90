! $Id: ESMF_FieldGatherUTest.F90,v 1.25 2008/05/07 18:33:50 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldGatherUTest - This test verifies FieldGather functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledGather.
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    use ESMF_FieldGetMod
    use ESMF_FieldGatherMod
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldGatherUTest.F90,v 1.25 2008/05/07 18:33:50 feiliu Exp $'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

    ! individual test name
    character(ESMF_MAXSTR) :: name

    ! individual test failure messages
    character(ESMF_MAXSTR*2) :: failMsg

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if(rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Scatter test
        call test_gather_2d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldGather basic test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Scatter test
        call test_scatter_2d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldScatter basic test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains

#ifdef ESMF_TESTEXHAUSTIVE

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
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
            regDecomp=(/2,2/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            name="grid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(grid, localDe=0, allocCount=fa_shape, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(fa_shape(1), fa_shape(2)))
        farray = lpe
        array = ESMF_ArrayCreate(farray, distgrid=distgrid, &
            staggerloc=0, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, array, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(lpe .eq. 0) allocate(farrayDst(10,20))
        call ESMF_FieldGather(field, farrayDst, rootPet=0, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! check that the values gathered on rootPet are correct
        if(lpe .eq. 0) then
            do i = 1, 2
                do j = 1, 2
                    if(farrayDst(i*5, j*10) .ne. (i-1)+(j-1)*2) localrc=ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        endif

        call ESMF_FieldDestroy(field)
        call ESMF_GridDestroy(grid)
        call ESMF_ArrayDestroy(array)
        rc = ESMF_SUCCESS
    end subroutine test_gather_2d

#undef ESMF_METHOD
#define ESMF_METHOD "test_scatter_2d"
    subroutine test_scatter_2d(rc)
        integer, intent(out)                        :: rc
        ! local arguments used to create field
        !type(ESMF_Field)                            :: field
        !type(ESMF_Grid)                             :: grid
        !type(ESMF_DistGrid)                         :: distgrid
        !type(ESMF_ArraySpec)                        :: arrayspec
        !integer                                     :: localrc

        rc = ESMF_SUCCESS
    end subroutine test_scatter_2d
 
#endif

end program ESMF_FieldGatherUTest
