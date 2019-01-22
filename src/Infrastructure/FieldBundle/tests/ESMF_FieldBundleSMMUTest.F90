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

#define FILENAME "src/Infrastructure/FieldBundle/tests/ESMF_FieldBundleSMMUTest.F90"

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

    ! test error messages
    character(ESMF_MAXSTR) :: failMsg, name

    ! used for field bundle SMM store tests
    integer :: srcTermProcessingSingle(1), srcTermProcessingDouble(2), ii

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if(rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE

    ! ------------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    write(name, *) "ESMF_FieldBundleSMMStoreFromFile Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call test_field_bundle_smm_store_from_file(rc)
#ifdef ESMF_NETCDF
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
    call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

    ! ------------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    write(name, *) "ESMF_FieldBundleSMMStoreFromFile Test (srcTermProcessing=(/-1/))"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    srcTermProcessingSingle = (/-1/)
    call test_field_bundle_smm_store_from_file(rc, &
      srcTermProcessing=srcTermProcessingSingle)
    ! The source term processing value should be adjusted by the store call
    ! during optimizations.
    if (rc == ESMF_SUCCESS .and. srcTermProcessingSingle(1) .lt. 0) then
      rc = ESMF_FAILURE
    endif
#ifdef ESMF_NETCDF
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
    call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

    ! ------------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    write(name, *) "ESMF_FieldBundleSMMStore Source Term Processing Test - (/-1, -1/)"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    srcTermProcessingDouble = (/-1, -1/)
    call test_field_bundle_smm_source_term_processing(srcTermProcessingDouble, rc)
    if (rc == ESMF_SUCCESS) then
      do ii=1,2
        if (srcTermProcessingDouble(ii) .lt. 0) then
          rc = ESMF_FAILURE
          exit
        endif
      enddo
    endif
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! ------------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    write(name, *) "ESMF_FieldBundleSMMStore Source Term Processing Test - (/1, 1/)"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    srcTermProcessingDouble = (/1, 1/)
    call test_field_bundle_smm_source_term_processing(srcTermProcessingDouble, rc)
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    ! ------------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    write(name, *) "ESMF_FieldBundleSMMStore Source Term Processing Test - (/1/)"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    srcTermProcessingSingle = (/1/)
    call test_field_bundle_smm_source_term_processing(srcTermProcessingSingle, rc)
    if (srcTermProcessingSingle(1) .ne. 1) then
      rc = ESMF_FAILURE
    endif
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        ! --------------------------------------------------------------------------

        if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1db(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleSMM test using lpe for both src and dst, with halos"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_smm_1dbweak(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleSMM test using lpe for both src and dst, with halos compatible"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1db"
 
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

        call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field bundles and fields
        srcFieldBundle = ESMF_FieldBundleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstFieldBundle = ESMF_FieldBundleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 3
            srcField(i) = ESMF_FieldCreate(grid, arrayspec, &
                totalLWidth=(/1/), totalUWidth=(/2/), &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(srcField(i), localDe=0, farrayPtr=srcfptr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            srcfptr = 1

            call ESMF_FieldBundleAdd(srcFieldBundle, (/srcField(i)/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            dstField(i) = ESMF_FieldCreate(grid, arrayspec, &
                totalLWidth=(/1/), totalUWidth=(/2/), &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(dstField(i), localDe=0, farrayPtr=dstfptr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            dstfptr = 0

            call ESMF_FieldBundleAdd(dstFieldBundle, (/dstField(i)/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, &
            routehandle, factorList, factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform smm
        call ESMF_FieldBundleSMM(srcFieldBundle, dstFieldBundle, routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        do l = 1, 3
            call ESMF_FieldGet(dstField(l), localDe=0, farrayPtr=fptr, &
                exclusiveLBound=exlb, exclusiveUBound=exub, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! Verify that the smm data in dstField(l) is correct.
            ! Before the smm op, the dst Field contains all 0. 
            ! The smm op reset the values to the index value, verify this is the case.
            !write(*, '(9I3)') l, lpe, fptr
            do i = exlb(1), exub(1)
                if(fptr(i) .ne. i) localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! release SMM route handle
        call ESMF_FieldBundleSMMRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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

!------------------------------------------------------------------------

#undef ESMF_METHOD
#define ESMF_METHOD "test_smm_1dbweak"
 
    subroutine test_smm_1dbweak(rc)
        integer, intent(out)                        :: rc

        ! local arguments used to create field etc
        type(ESMF_FieldBundle)                      :: srcFieldBundle, dstFieldBundle
        type(ESMF_FieldBundle)                      :: srcFieldBundleA, dstFieldBundleA
        type(ESMF_Field)                            :: srcField(3), dstField(3)
        type(ESMF_Field)                            :: srcFieldA(3), dstFieldA(3)
        type(ESMF_Grid)                             :: grid
        type(ESMF_DistGrid)                         :: distgrid
        type(ESMF_VM)                               :: vm
        type(ESMF_RouteHandle)                      :: routehandle
        type(ESMF_ArraySpec)                        :: arrayspec
        integer                                     :: localrc, lpe, i, j, l

        integer, pointer                            :: srcfptr(:,:), dstfptr(:,:)
        integer, pointer                            :: fptr(:,:)
        integer                                     :: exlb(2), exub(2)
        
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
            regDecomp=(/4/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, &
            gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), &
            name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field bundles and fields
        srcFieldBundle = ESMF_FieldBundleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstFieldBundle = ESMF_FieldBundleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        srcFieldBundleA = ESMF_FieldBundleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstFieldBundleA = ESMF_FieldBundleCreate(rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 3
            srcField(i) = ESMF_FieldCreate(grid, arrayspec, &
                ungriddedLBound=(/1/), ungriddedUBound=(/8/), &
                totalLWidth=(/1/), totalUWidth=(/2/), &
                gridToFieldMap=(/2/), &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            srcFieldA(i) = ESMF_FieldCreate(grid, arrayspec, &
                ungriddedLBound=(/1/), ungriddedUBound=(/5/), &
                totalLWidth=(/1/), totalUWidth=(/2/), &
                gridToFieldMap=(/2/), &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(srcFieldA(i), localDe=0, farrayPtr=srcfptr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            srcfptr = 1

            call ESMF_FieldBundleAdd(srcFieldBundle, (/srcField(i)/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldBundleAdd(srcFieldBundleA, (/srcFieldA(i)/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            dstField(i) = ESMF_FieldCreate(grid, arrayspec, &
                ungriddedLBound=(/1/), ungriddedUBound=(/8/), &
                totalLWidth=(/1/), totalUWidth=(/2/), &
                gridToFieldMap=(/2/), &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            dstFieldA(i) = ESMF_FieldCreate(grid, arrayspec, &
                ungriddedLBound=(/1/), ungriddedUBound=(/5/), &
                totalLWidth=(/1/), totalUWidth=(/2/), &
                gridToFieldMap=(/2/), &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(dstFieldA(i), localDe=0, farrayPtr=dstfptr, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            dstfptr = 0

            call ESMF_FieldBundleAdd(dstFieldBundle, (/dstField(i)/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldBundleAdd(dstFieldBundleA, (/dstFieldA(i)/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! initialize factorList and factorIndexList
        allocate(factorList(4))
        allocate(factorIndexList(2,4))
        factorList = (/1,2,3,4/)
        factorIndexList(1,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        factorIndexList(2,:) = (/lpe*4+1,lpe*4+2,lpe*4+3,lpe*4+4/)
        call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, routehandle, &
            factorList, factorIndexList, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! perform smm
        call ESMF_FieldBundleSMM(srcFieldBundleA, dstFieldBundleA, &
             routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! verify smm
        do l = 1, 3
            call ESMF_FieldGet(dstFieldA(l), localDe=0, farrayPtr=fptr, &
                exclusiveLBound=exlb, exclusiveUBound=exub, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! Verify that the smm data in dstField(l) is correct.
            ! Before the smm op, the dst Field contains all 0. 
            ! The smm op reset the values to the index value, verify 
            ! this is the case.
            ! write(*, '(9I3)') l, lpe, fptr
            do i = exlb(1), exub(1)
              do j = exlb(2), exub(2)
                if(fptr(i,j) .ne. j) localrc = ESMF_FAILURE
              enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo

        ! release SMM route handle
        call ESMF_FieldBundleSMMRelease(routehandle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! release all acquired resources
        call ESMF_FieldBundleDestroy(srcFieldBundle)
        call ESMF_FieldBundleDestroy(dstFieldBundle)
        call ESMF_FieldBundleDestroy(srcFieldBundleA)
        call ESMF_FieldBundleDestroy(dstFieldBundleA)
        do l = 1, 3
            call ESMF_FieldDestroy(srcField(l))
            call ESMF_FieldDestroy(dstField(l))
            call ESMF_FieldDestroy(srcFieldA(l))
            call ESMF_FieldDestroy(dstFieldA(l))
        enddo
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        deallocate(factorList, factorIndexList)
        rc = ESMF_SUCCESS
    end subroutine test_smm_1dbweak

! ==================================================================================

#undef ESMF_METHOD
#define ESMF_METHOD "test_field_bundle_smm_store_from_file"

subroutine test_field_bundle_smm_store_from_file(rc, srcTermProcessing)
  use ESMF
  use ESMF_IOScripMod

  integer, intent(inout) :: rc
  integer, intent(inout), optional :: srcTermProcessing(:)

  character(*), parameter :: weightFile = 'test_fb_weights.nc'
  real, parameter :: tol = 10E-15
  integer(ESMF_KIND_I4), pointer :: factorIndexList(:,:)
  real(ESMF_KIND_R8), pointer :: factorList(:), coordX(:), coordY(:), &
    farrayPtrMem(:, :), farrayPtrFile(:, :)
  type(ESMF_Field) :: srcField, dstField, srcFields(1), dstFields(1)
  type(ESMF_FieldBundle) :: srcFieldBundle, dstFieldBundle
  type(ESMF_Grid) :: srcGrid, dstGrid
  type(ESMF_RouteHandle) :: routehandleMem, routehandleFile
  type(ESMF_Array) :: arrayMem, arrayMemCopy, arrayFile
  type(ESMF_ArraySpec) :: arrayMemCopySpec
  type(ESMF_DistGrid) :: arrayMemDistGrid
  integer :: shp(2), ii, jj, lbnd(3), ubnd(3), countFail

  rc = ESMF_FAILURE

  ! --------------------------------------------------------------------------------
  ! Generate fields for regrid store and write the factor arrays to netCDF. This
  ! output netCDF containing the factors is read in to test the field bundle SMM
  ! store from file.

  srcGrid = grid_create_no_peri_dim_by_max_index((/20,40/), rc)
  srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, name="srcField", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  dstGrid = grid_create_no_peri_dim_by_max_index((/10,20/), rc)
  dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, name="dstField", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldFill(srcField, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
    factorIndexList=factorIndexList, factorList=factorList, &
    routehandle=routehandleMem, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_OutputSimpleWeightFile(weightFile, factorList, factorIndexList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------
  ! Create field bundles and the route handle from file.

  srcFields(1) = srcField
  srcFieldBundle = ESMF_FieldBundleCreate(fieldList=srcFields, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  dstFields(1) = dstField
  dstFieldBundle = ESMF_FieldBundleCreate(fieldList=dstFields, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  if (present(srcTermProcessing)) then
    call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, weightFile, &
      routehandleFile, rc=rc, srcTermProcessing=srcTermProcessing)
  else
    call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, weightFile, &
      routehandleFile, rc=rc)
  endif
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------
  ! Test regrid operation with original fields against factors read from file.

  ! The in-memory route handle.
  call ESMF_FieldBundleRegrid(srcFieldBundle, dstFieldBundle, routehandleMem, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! The desired result of the from file store regrid operation.
  call ESMF_FieldGet(dstField, array=arrayMem, arrayspec=arrayMemCopySpec, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! In-memory regrid operation result.
  call ESMF_ArrayGet(arrayMem, distgrid=arrayMemDistGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! Copy the output array from the in-memory regrid operation. The array values
  ! would change after the from-file regrid operation.
  arrayMemCopy = ESMF_ArrayCreate(arrayMemDistGrid, arrayMemCopySpec, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_ArrayCopy(arrayMemCopy, arrayMem, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_ArrayGet(arrayMemCopy, farrayPtr=farrayPtrMem, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! The from-file route handle.
  call ESMF_FieldBundleRegrid(srcFieldBundle, dstFieldBundle, routehandleFile, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! The result of the from file store regrid operation.
  call ESMF_FieldGet(dstField, array=arrayFile, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_ArrayGet(arrayFile, farrayPtr=farrayPtrFile, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! If the absolute value of the differences exceeds the tolerances, then the test
  ! will fail. The count is evaluated at the end of the subroutine.
  countFail = 0
  shp = shape(farrayPtrMem)
  do jj=1,shp(1)
    do ii=1,shp(2)
      if (abs(farrayPtrMem(jj,ii) - farrayPtrFile(jj,ii)) .ge. tol) then
        countFail = countFail + 1
      endif
    enddo
  enddo

  ! --------------------------------------------------------------------------------

  deallocate(factorList)
  deallocate(factorIndexList)

  call ESMF_ArrayDestroy(arrayMemCopy, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldBundleSMMRelease(routehandleMem, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldBundleDestroy(srcFieldBundle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldBundleDestroy(dstFieldBundle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldDestroy(srcField, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldDestroy(dstField, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_GridDestroy(srcGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_GridDestroy(dstGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------

  if (countFail .eq. 0) then
    rc = ESMF_SUCCESS
  else
    rc = ESMF_FAILURE
  endif

end subroutine test_field_bundle_smm_store_from_file

! ==================================================================================

#undef ESMF_METHOD
#define ESMF_METHOD "test_field_bundle_smm_source_term_processing"

subroutine test_field_bundle_smm_source_term_processing(srcTermProcessing, rc)
  use ESMF

  integer, parameter :: fieldCount = 2
  integer, parameter :: dstMaxIndex(2) = (/10, 20/)
  real(ESMF_KIND_R8), parameter :: tol = 1.D-4
  real(ESMF_KIND_R8)            :: absDiff

  integer, intent(inout) :: srcTermProcessing(:), rc

  type(ESMF_Grid) :: srcGrid, dstGrid
  type(ESMF_FieldBundle) :: srcFieldBundle, dstFieldBundle, desiredDstFieldBundle
  type(ESMF_RouteHandle) :: routehandle
  type(ESMF_Field) :: srcFieldList(fieldCount), dstFieldList(fieldCount), &
                      desiredDstFieldList(fieldCount)
  integer(ESMF_KIND_I4), pointer :: factorIndexList(:,:)
  real(ESMF_KIND_R8), pointer :: factorList(:), farrayPtrSrc(:, :), &
                                 farrayPtrDst(:, :)
  integer :: ii, jj, kk, failCount, shp(2)
  character(len=160) :: msgString

  ! --------------------------------------------------------------------------------
  ! Create grids and field bundles. The source grid is slightly larger than than the
  ! destination ensuring no unmapped points in the destination.

  srcGrid = grid_create_no_peri_dim_by_max_index((/11, 21/), rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  dstGrid = grid_create_no_peri_dim_by_max_index(dstMaxIndex, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  srcFieldBundle = field_bundle_create(srcGrid, fieldCount, .true., rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  dstFieldBundle = field_bundle_create(dstGrid, fieldCount, .false., rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! Note fill is true in the desired destination field. This is the field we test
  ! regridding errors against.
  desiredDstFieldBundle = field_bundle_create(dstGrid, fieldCount, .true., rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------
  ! Get access to the internal field in the bundles.

  call ESMF_FieldBundleGet(srcFieldBundle, fieldList=srcFieldList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldBundleGet(dstFieldBundle, fieldList=dstFieldList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldBundleGet(desiredDstFieldBundle, fieldList=desiredDstFieldList, &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------
  ! Retrieve the factor lists from the regrid store call and use those to create a
  ! route handle using the provided source term processing flag.

  call ESMF_FieldRegridStore(srcFieldList(1), dstFieldList(1), &
    factorList=factorList, factorIndexList=factorIndexList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, routehandle, &
    factorList, factorIndexList, srcTermProcessing=srcTermProcessing, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------
  ! Execute the sparse matrix multiplication the test the results against the
  ! desired, filled field.

  call ESMF_FieldBundleSMM(srcFieldBundle, dstFieldBundle, routehandle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  failCount = 0
  do kk=1,fieldCount

    call ESMF_FieldGet(dstFieldList(kk), farrayPtr=farrayPtrSrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

    call ESMF_FieldGet(desiredDstFieldList(kk), farrayPtr=farrayPtrDst, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

    shp = shape(farrayPtrSrc)
    do ii=1,shp(1)
      do jj=1,shp(2)
        absDiff = abs(farrayPtrSrc(ii, jj) - farrayPtrDst(ii, jj))
        if (absDiff .ge. tol) then
          failCount = failCount + 1
          write(msgString,*) "Absolute difference above tolerance: ", farrayPtrSrc(ii, jj), &
            " - ", farrayPtrDst(ii, jj), " = ", absDiff, " > ", tol
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
    enddo
  enddo

  ! --------------------------------------------------------------------------------
  ! Release resources.

  deallocate(factorList, factorIndexList)

  call ESMF_FieldBundleSMMRelease(routehandle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call field_bundle_destroy(srcFieldBundle, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call field_bundle_destroy(dstFieldBundle, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call field_bundle_destroy(desiredDstFieldBundle, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_GridDestroy(srcGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_GridDestroy(dstGrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------
  ! If there are any fail counts in the field value comparisons, the test will fail.

!  print *, "failCount", failCount
  if (failCount .eq. 0) then
    rc = ESMF_SUCCESS
  else
    rc = ESMF_FAILURE
  endif

end subroutine test_field_bundle_smm_source_term_processing

! ==================================================================================

#undef ESMF_METHOD
#define ESMF_METHOD "grid_create_no_peri_dim_by_max_index"

type(ESMF_Grid) function grid_create_no_peri_dim_by_max_index(maxIndex, rc) &
  result(grid)

  use ESMF

  integer, intent(in)    :: maxIndex(2)
  integer, intent(inout) :: rc

  integer :: ii, lbnd(3), ubnd(3)
  real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)

  grid = ESMF_GridCreateNoPeriDim(maxIndex=maxIndex, coordDep1=(/1/), &
    coordDep2=(/2/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_GridAddCoord(grid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  call ESMF_GridGetCoord(grid, coordDim=1, computationalLBound=lbnd, &
    computationalUBound=ubnd, farrayPtr=coordX, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  do ii=lbnd(1),ubnd(1)
    coordX(ii) = ii*10.0
  enddo

  call ESMF_GridGetCoord(grid, coordDim=2, computationalLBound=lbnd, &
    computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  do ii=lbnd(1),ubnd(1)
    coordY(ii) = ii*20.0
  enddo

end function grid_create_no_peri_dim_by_max_index

! ==================================================================================

#undef ESMF_METHOD
#define ESMF_METHOD "field_bundle_create"

type(ESMF_FieldBundle) function field_bundle_create(grid, fieldCount, shouldFill, &
  rc) result(fieldBundle)

  use ESMF

  type(ESMF_Grid), intent(in) :: grid
  integer, intent(in) :: fieldCount
  integer, intent(inout) :: rc
  logical, intent(in) :: shouldFill

  type(ESMF_Field) :: fieldList(fieldCount), field
  integer :: ii

  do ii=1,fieldCount
    field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

    if (shouldFill) then
      call ESMF_FieldFill(field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return
    endif

    fieldList(ii) = field
  enddo

  fieldBundle = ESMF_FieldBundleCreate(fieldList=fieldList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

end function field_bundle_create

! ==================================================================================

#undef ESMF_METHOD
#define ESMF_METHOD "field_bundle_destroy"

subroutine field_bundle_destroy(fieldBundle, rc)

  use ESMF

  type(ESMF_FieldBundle), intent(inout) :: fieldBundle
  integer, intent(inout) :: rc

  type(ESMF_Field), allocatable :: fieldList(:)
  integer :: ii, fieldCount

  call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

  allocate(fieldList(fieldCount))

  call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return

  do ii=1,fieldCount
    call ESMF_FieldDestroy(fieldList(ii), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return
  enddo

  call ESMF_FieldBundleDestroy(fieldBundle, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  deallocate(fieldList)

end subroutine field_bundle_destroy

! ==================================================================================

#endif

end program ESMF_FieldBundleSMMUTest
