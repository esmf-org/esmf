! $Id: ESMF_FieldRedistEx.F90,v 1.27 2009/09/29 19:44:51 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

     program FieldRedistEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldRedistEx - Field Redistribution
!     
! !DESCRIPTION:
!     
! This program shows examples of Field interfaces for redistribution of data.
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldRedistEx"
     ! ESMF Framework module
     use ESMF_Mod
     use ESMF_TestMod
     implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldRedistEx.F90,v 1.27 2009/09/29 19:44:51 feiliu Exp $'
!------------------------------------------------------------------------------

    ! Local variables
    integer :: rc, finalrc
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
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(terminationflag=ESMF_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Redistribute data from source Field to destination Field}
! \label{sec:field:usage:redist_1dptr}
!
! User can use {\tt ESMF\_FieldRedist} interface to redistribute data from 
! source Field to destination Field. This interface is overloaded by type and kind;
! In the version of {\tt ESMF\_FieldRedist} without factor argument, a default value
! of 1 is used.
! 
! In this example, we first create two 1D Fields, a source Field and a destination
! Field. Then we use {\tt ESMF\_FieldRedist} to
! redistribute data from source Field to destination Field.
!EOE
!BOC 

    ! Get current VM and pet number
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

    call ESMF_FieldGet(grid, localDe=0, totalCount=fa_shape, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create src_farray, srcArray, and srcField
    ! +--------+--------+--------+--------+
    !      0        1        2        3            ! value
    ! 1        4        8        12       16       ! bounds
    allocate(src_farray(fa_shape(1)) )
    src_farray = lpe
    srcArray = ESMF_ArrayCreate(src_farray, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    srcField = ESMF_FieldCreate(grid, srcArray, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! create dst_farray, dstArray, and dstField
    ! +--------+--------+--------+--------+
    !      0        0        0        0            ! value
    ! 1        4        8        12       16       ! bounds
    allocate(dst_farray(fa_shape(1)) )
    dst_farray = 0
    dstArray = ESMF_ArrayCreate(dst_farray, distgrid=distgrid, indexflag=ESMF_INDEX_DELOCAL, &
        rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    dstField = ESMF_FieldCreate(grid, dstArray, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! perform redist
    ! 1. setup routehandle from source Field to destination Field
    call ESMF_FieldRedistStore(srcField, dstField, routehandle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! 2. use precomputed routehandle to redistribute data
    call ESMF_FieldRedist(srcfield, dstField, routehandle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! verify redist
    call ESMF_FieldGet(dstField, localDe=0, farrayPtr=fptr, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! Verify that the redistributed data in dstField is correct.
    ! Before the redist op, the dst Field contains all 0. 
    ! The redist op reset the values to the PE value, verify this is the case.
    do i = lbound(fptr, 1), ubound(fptr, 1)
        if(fptr(i) .ne. lpe) localrc = ESMF_FAILURE
    enddo
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! release route handle
    call ESMF_FieldRedistRelease(routehandle, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    ! destroy all objects created in this example to prevent memory leak
    call ESMF_FieldDestroy(srcField, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_FieldDestroy(dstField, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(srcArray, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_ArrayDestroy(dstArray, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_GridDestroy(grid, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
    deallocate(src_farray, dst_farray)
!EOC

     call ESMF_Finalize(rc=rc)

     if (rc.NE.ESMF_SUCCESS) then
       finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldRedistEx.F90"
     else
       print *, "FAIL: ESMF_FieldRedistEx.F90"
     end if

    end program FieldRedistEx
