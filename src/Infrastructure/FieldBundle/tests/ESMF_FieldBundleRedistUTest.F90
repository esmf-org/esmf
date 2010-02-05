! $Id: ESMF_FieldBundleRedistUTest.F90,v 1.14.2.1 2010/02/05 19:56:36 svasquez Exp $
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
program ESMF_RedistUTest

#include "ESMF.h"
#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RedistUTest - Data redistribution tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Redist unit tests, using the Route code.
!
!  "Redist" is sending data from one field to another, where the grids 
!   themselves are identical, but the decompositions (which subsets of the
!   grid are located on each processor) are different.  Redist sends data
!   from one processor to another with no interpolation.  See Regrid for
!   routines which do data interpolation from one igrid to another.
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    use ESMF_FieldBundleRedistMod
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldBundleRedistUTest.F90,v 1.14.2.1 2010/02/05 19:56:36 svasquez Exp $'
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
        call test_redist_3d(rc)
        write(failMsg, *) ""
        write(name, *) "FieldRedist basic test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_redist_3d"
    subroutine test_redist_3d(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_FieldBundle)                      :: srcFieldBundle, dstFieldBundle
        type(ESMF_Field)                            :: srcField(3), dstField(3)
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j, k, l
        integer                                     :: exLB(3), exUB(3)

        integer(ESMF_KIND_I4), pointer              :: srcfptr(:,:,:), dstfptr(:,:,:), fptr(:,:,:)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        ! retrieve VM and its context info such as PET number
        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create distgrid and grid for field and fieldbundle creation
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

        ! create src and dst FieldBundles pair
        srcFieldBundle = ESMF_FieldBundleCreate(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        dstFieldBundle = ESMF_FieldBundleCreate(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! create src and dst Fields and add the Fields into FieldBundles
        do i = 1, 3
            srcField(i) = ESMF_FieldCreate(grid, arrayspec, &
                ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
                maxHaloLWidth=(/1,1/), maxHaloUWidth=(/1,2/), &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            call ESMF_FieldGet(srcField(i), localDe=0, farrayPtr=srcfptr, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            srcfptr = lpe

            call ESMF_FieldBundleAdd(srcFieldBundle, srcField(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

            dstField(i) = ESMF_FieldCreate(grid, arrayspec, &
                ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
                maxHaloLWidth=(/1,1/), maxHaloUWidth=(/1,2/), &
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

        ! perform redist
        call ESMF_FieldBundleRedistStore(srcFieldBundle, dstFieldBundle, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleRedist(srcFieldBundle, dstFieldBundle, routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! verify redist
        do l = 1, 3
            call ESMF_FieldGet(dstField(l), localDe=0, farrayPtr=fptr, &
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
        enddo

        ! release route handle
        call ESMF_FieldRedistRelease(routehandle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleDestroy(srcFieldBundle)
        call ESMF_FieldBundleDestroy(dstFieldBundle)
        do i = 1, 3
            call ESMF_FieldDestroy(srcField(i))
            call ESMF_FieldDestroy(dstField(i))
        enddo
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)

        rc = ESMF_SUCCESS
    end subroutine test_redist_3d
 
#endif

! -------- end of unit test code ------------------------
end program ESMF_RedistUTest
