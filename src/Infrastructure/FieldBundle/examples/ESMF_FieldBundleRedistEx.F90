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

     program FieldBundleRedistEx

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================
!
! !PROGRAM: ESMF_FieldBundleRedistEx - FieldBundle Redistribution
!     
! !DESCRIPTION:
!     
! This program shows examples of FieldBundle interfaces for redistribution of data.
!-----------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRedistEx"
     ! ESMF Framework module
     use ESMF
     use ESMF_TestMod
     implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

    ! Local variables
    type(ESMF_FieldBundle)                      :: srcFieldBundle, dstFieldBundle
    type(ESMF_Field)                            :: srcField(3), dstField(3)
    type(ESMF_Grid)                             :: grid
    type(ESMF_DistGrid)                         :: distgrid
    type(ESMF_VM)                               :: vm
    type(ESMF_RouteHandle)                      :: routehandle
    type(ESMF_ArraySpec)                        :: arrayspec
    integer                                     :: rc, finalrc, lpe, i, j, k, l, result
    integer                                     :: exLB(3), exUB(3)

    integer(ESMF_KIND_I4), pointer              :: srcfptr(:,:,:), dstfptr(:,:,:), fptr(:,:,:)

    character(ESMF_MAXSTR)                      :: testname
    character(ESMF_MAXSTR)                      :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_FieldBundleRedistEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


    rc = ESMF_SUCCESS
    finalrc = ESMF_SUCCESS
!------------------------------------------------------------------------------
    call ESMF_Initialize(defaultlogfilename="FieldBundleRedistEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!------------------------------------------------------------------------------
!BOE
! \subsubsection{Redistribute data from a source FieldBundle to a destination FieldBundle}
! \label{sec:fieldbundle:usage:redist_1dptr}
!
! The {\tt ESMF\_FieldBundleRedist} interface can be used to redistribute data from
! source FieldBundle to destination FieldBundle. This interface is overloaded by type and kind;
! In the version of {\tt ESMF\_FieldBundleRedist} without factor argument, a default value
! of factor 1 is used.
! 
! In this example, we first create two FieldBundles, a source FieldBundle and a destination
! FieldBundle. Then we use {\tt ESMF\_FieldBundleRedist} to
! redistribute data from source FieldBundle to destination FieldBundle.
!EOE
    ! retrieve VM and its context info such as PET number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create distgrid and grid for field and fieldbundle creation
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
        regDecomp=(/2,2/), rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_I4, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create src and dst FieldBundles pair
    srcFieldBundle = ESMF_FieldBundleCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    dstFieldBundle = ESMF_FieldBundleCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create src and dst Fields and add the Fields into FieldBundles
    do i = 1, 3
        srcField(i) = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
            rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(srcField(i), localDe=0, farrayPtr=srcfptr, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        srcfptr = lpe

        call ESMF_FieldBundleAdd(srcFieldBundle, (/srcField(i)/), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        dstField(i) = ESMF_FieldCreate(grid, arrayspec, &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
            rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call ESMF_FieldGet(dstField(i), localDe=0, farrayPtr=dstfptr, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        dstfptr = -99

        call ESMF_FieldBundleAdd(dstFieldBundle, (/dstField(i)/), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

!BOC
    ! perform redist
    call ESMF_FieldBundleRedistStore(srcFieldBundle, dstFieldBundle, &
         routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleRedist(srcFieldBundle, dstFieldBundle, &
         routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! verify redist
    do l = 1, 3
        call ESMF_FieldGet(dstField(l), localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exLB, exclusiveUBound=exUB, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        ! Verify that the redistributed data in dstField is correct.
        ! Before the redist op, the dst Field contains all 0. 
        ! The redist op reset the values to the PE value, 
        ! verify this is the case.
        ! MUST use exclusive bounds because Redist operates 
        ! within excl. region.
        do k = exLB(3), exUB(3)
            do j = exLB(2), exUB(2)
                do i = exLB(1), exUB(1)
                   if(fptr(i,j,k) .ne. lpe) rc = ESMF_FAILURE
                enddo
            enddo
        enddo
        if (rc /= ESMF_SUCCESS) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="Validation check failed!", &
            ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
    enddo

    ! release route handle
    call ESMF_FieldRedistRelease(routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleDestroy(srcFieldBundle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleDestroy(dstFieldBundle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do i = 1, 3
        call ESMF_FieldDestroy(srcField(i), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_FieldDestroy(dstField(i), rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

!BOE
! \subsubsection{Redistribute data from a packed source FieldBundle to a packed destination FieldBundle}
! \label{sec:fieldbundle:usage:redist_packed}
!
! The {\tt ESMF\_FieldBundleRedist} interface can be used to redistribute data from
! source FieldBundle to destination FieldBundle when both Bundles are packed with same
! number of fields.  
!
! In this example, we first create two packed FieldBundles, a source FieldBundle and a destination
! FieldBundle. Then we use {\tt ESMF\_FieldBundleRedist} to
! redistribute data from source FieldBundle to destination FieldBundle.
!
! The same Grid is used where the source and destination packed FieldBundle are built upon. Source
! and destination Bundle have different memory layout.
!EOE

!BOC
    allocate(srcfptr(3,5,10), dstfptr(10,5,3))
    srcfptr = lpe
    srcFieldBundle = ESMF_FieldBundleCreate((/'field01', 'field02', 'field03'/), &
      srcfptr, grid, 1, gridToFieldMap=(/2,3/), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC
    dstFieldBundle = ESMF_FieldBundleCreate((/'field01', 'field02', 'field03'/), &
      dstfptr, grid, 3, gridToFieldMap=(/2,1/), rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    ! perform redist
    call ESMF_FieldBundleRedistStore(srcFieldBundle, dstFieldBundle, &
         routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOC
    call ESMF_FieldBundleRedist(srcFieldBundle, dstFieldBundle, &
         routehandle, rc=rc)
!EOC
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleGet(dstFieldBundle, localDe=0, farrayPtr=fptr, &
      rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! verify redist
    do k = 1, 3
        do j = lbound(fptr,2), ubound(fptr,2)
            do i = lbound(fptr, 1), ubound(fptr, 1)
               if(fptr(i,j,k) .ne. lpe) rc = ESMF_FAILURE
            enddo
        enddo
        if (rc /= ESMF_SUCCESS) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="Validation check failed!", &
            ESMF_CONTEXT)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
    enddo

    ! release route handle
    call ESMF_FieldRedistRelease(routehandle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_FieldBundleDestroy(srcFieldBundle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_FieldBundleDestroy(dstFieldBundle, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    deallocate(srcfptr, dstfptr)

    ! Release the Grid and Distgrid
    call ESMF_GridDestroy(grid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
    ! file that the scripts grep for.
    call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)



     call ESMF_Finalize(rc=rc)

     if (rc.NE.ESMF_SUCCESS) then
       finalrc = ESMF_FAILURE
     end if

     if (finalrc.EQ.ESMF_SUCCESS) then
       print *, "PASS: ESMF_FieldBundleRedistEx.F90"
     else
       print *, "FAIL: ESMF_FieldBundleRedistEx.F90"
     end if

    end program FieldBundleRedistEx
