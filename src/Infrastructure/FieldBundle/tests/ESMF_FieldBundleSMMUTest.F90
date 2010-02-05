! $Id: ESMF_FieldBundleSMMUTest.F90,v 1.5.2.1 2010/02/05 19:56:36 svasquez Exp $
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
program ESMF_FieldBundleSMMUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldBundleSMMUTest - This test verifies FieldBundleSMM functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledSMM.
!EOPI
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    use ESMF_FieldBundleSMMMod
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldBundleSMMUTest.F90,v 1.5.2.1 2010/02/05 19:56:36 svasquez Exp $'
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
        call test_smm_1db(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleSMM test using lpe for both src and dst, with halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1d"
 
    subroutine test_smm_1db(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_FieldBundle)                      :: srcFieldBundle, dstFieldBundle
        type(ESMF_Field)                            :: srcField(3), dstField(3)
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, l

        integer, pointer                            :: srcfptr(:), dstfptr(:)
        integer, pointer                            :: fptr(:)
        integer                                     :: exlb(1), exub(1)
        
        integer(ESMF_KIND_I4), allocatable          :: factorList(:)
        integer, allocatable                        :: factorIndexList(:,:)

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

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create field bundles and fields
        srcFieldBundle = ESMF_FieldBundleCreate(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstFieldBundle = ESMF_FieldBundleCreate(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        do i = 1, 3
            srcField(i) = ESMF_FieldCreate(grid, arrayspec, &
                maxHaloLWidth=(/1/), maxHaloUWidth=(/2/), &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            call ESMF_FieldGet(srcField(i), localDe=0, farrayPtr=srcfptr, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            srcfptr = 1

            call ESMF_FieldBundleAdd(srcFieldBundle, srcField(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            dstField(i) = ESMF_FieldCreate(grid, arrayspec, &
                maxHaloLWidth=(/1/), maxHaloUWidth=(/2/), &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            call ESMF_FieldGet(dstField(i), localDe=0, farrayPtr=dstfptr, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            dstfptr = 0

            call ESMF_FieldBundleAdd(dstFieldBundle, dstField(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, routehandle, &
            factorList, factorIndexList, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! perform smm
        call ESMF_FieldBundleSMM(srcFieldBundle, dstFieldBundle, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify smm
        do l = 1, 3
            call ESMF_FieldGet(dstField(l), localDe=0, farrayPtr=fptr, &
                exclusiveLBound=exlb, exclusiveUBound=exub, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            ! Verify that the smm data in dstField(l) is correct.
            ! Before the smm op, the dst Field contains all 0. 
            ! The smm op reset the values to the index value, verify this is the case.
            !write(*, '(9I3)') l, lpe, fptr
            do i = exlb(1), exub(1)
                if(fptr(i) .ne. i) localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo

        ! release SMM route handle
        call ESMF_FieldBundleSMMRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! release all acquired resources
        call ESMF_FieldBundleDestroy(srcFieldBundle)
        call ESMF_FieldBundleDestroy(dstFieldBundle)
        do l = 1, 3
            call ESMF_FieldDestroy(srcField(l))
            call ESMF_FieldDestroy(dstField(l))
        enddo
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1db
#endif

end program ESMF_FieldBundleSMMUTest
