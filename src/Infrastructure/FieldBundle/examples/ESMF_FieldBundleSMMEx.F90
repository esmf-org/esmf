! $Id: ESMF_FieldBundleSMMEx.F90,v 1.6.2.1 2010/02/05 19:56:19 svasquez Exp $
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

     program FieldBundleSMMEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldBundleSMMEx - FieldBundle Sparse Matrix Multiplication
!     
! !DESCRIPTION:
!     
! This program shows examples of FieldBundle interfaces for SMM of data.
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSMMEx"
     ! ESMF Framework module
     use ESMF_Mod
     use ESMF_TestMod
     implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldBundleSMMEx.F90,v 1.6.2.1 2010/02/05 19:56:19 svasquez Exp $'
!------------------------------------------------------------------------------

    ! Local variables
    type(ESMF_FieldBundle)                      :: srcFieldBundle, dstFieldBundle
    type(ESMF_Field)                            :: srcField(3), dstField(3)
    type(ESMF_Grid)                             :: grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_VM)                               :: vm
    type(ESMF_RouteHandle)                      :: routehandle
    type(ESMF_ArraySpec)                        :: arrayspec
    integer                                     :: rc, finalrc, lpe, i, l

    integer, pointer                            :: srcfptr(:), dstfptr(:)
    integer, pointer                            :: fptr(:)
    integer                                     :: exlb(1), exub(1)
    
    integer(ESMF_KIND_I4), allocatable          :: factorList(:)
    integer, allocatable                        :: factorIndexList(:,:)


    rc = ESMF_SUCCESS
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Perform Sparse Matrix Multiplication from source FieldBundle 
!  to destination FieldBundle}
! \label{sec:fieldbundle:usage:smm_1dptr}
!
! A user can use {\tt ESMF\_FieldBundleSMM} interface to perform SMM from 
! source FieldBundle to destination FieldBundle. This interface is overloaded by type and kind;
! 
! In this example, we first create two FieldBundles, a source FieldBundle and a destination
! FieldBundle. Then we use {\tt ESMF\_FieldBundleSMM} to
! perform sparse matrix multiplication from source FieldBundle to destination FieldBundle.
!
! The operation performed in this example is better illustrated in 
! section \ref{sec:field:usage:smm_1dptr}.
! 
! Section \ref{Array:SparseMatMul} provides a detailed discussion of the 
! sparse matrix mulitiplication operation implemented in ESMF.
!EOE
!BOC 
    call ESMF_VMGetCurrent(vm, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create distgrid and grid
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/16/), &
        regDecomp=(/4/), &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    grid = ESMF_GridCreate(distgrid=distgrid, &
        gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
        name="grid", rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create field bundles and fields
    srcFieldBundle = ESMF_FieldBundleCreate(grid, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    dstFieldBundle = ESMF_FieldBundleCreate(grid, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    do i = 1, 3
        srcField(i) = ESMF_FieldCreate(grid, arrayspec, &
            maxHaloLWidth=(/1/), maxHaloUWidth=(/2/), &
            rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGet(srcField(i), localDe=0, farrayPtr=srcfptr, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        srcfptr = 1

        call ESMF_FieldBundleAdd(srcFieldBundle, srcField(i), rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        dstField(i) = ESMF_FieldCreate(grid, arrayspec, &
            maxHaloLWidth=(/1/), maxHaloUWidth=(/2/), &
            rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGet(dstField(i), localDe=0, farrayPtr=dstfptr, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        dstfptr = 0

        call ESMF_FieldBundleAdd(dstFieldBundle, dstField(i), rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    enddo

    ! initialize factorList and factorIndexList
    allocate(factorList(4))
    allocate(factorIndexList(2,4))
    factorList = (/1,2,3,4/)
    factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
    factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
    call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, routehandle, &
        factorList, factorIndexList, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! perform smm
    call ESMF_FieldBundleSMM(srcFieldBundle, dstFieldBundle, routehandle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! verify smm
    do l = 1, 3
        call ESMF_FieldGet(dstField(l), localDe=0, farrayPtr=fptr, &
            exclusiveLBound=exlb, exclusiveUBound=exub, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        ! Verify that the smm data in dstField(l) is correct.
        ! Before the smm op, the dst Field contains all 0. 
        ! The smm op reset the values to the index value, verify this is the case.
        !write(*, '(9I3)') l, lpe, fptr
        do i = exlb(1), exub(1)
            if(fptr(i) .ne. i) finalrc = ESMF_FAILURE
        enddo
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    enddo

    ! release SMM route handle
    call ESMF_FieldBundleSMMRelease(routehandle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! release all acquired resources
    call ESMF_FieldBundleDestroy(srcFieldBundle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldBundleDestroy(dstFieldBundle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    do l = 1, 3
        call ESMF_FieldDestroy(srcField(l), rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
        call ESMF_FieldDestroy(dstField(l), rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    enddo
    call ESMF_GridDestroy(grid, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(factorList, factorIndexList)

!EOC

     call ESMF_Finalize(rc=rc)

     if (rc.NE.ESMF_SUCCESS) then
       finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldBundleSMMEx.F90"
     else
       print *, "FAIL: ESMF_FieldBundleSMMEx.F90"
     end if

    end program FieldBundleSMMEx
