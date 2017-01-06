! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
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

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if(rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE

        ! --------------------------------------------------------------------------
        !EX_UTest
        write(name, *) "ESMF_FieldBundleSMMStoreFromFile Test"
        write(failMsg, *) "Did not return ESMF_SUCCESS"
        call test_field_bundle_smm_store_from_file(rc)
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
        write(name, *) "FieldBundleSMM test using lpe for both src and dst, with halos weakly congruent"
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
#endif

end program ESMF_FieldBundleSMMUTest

! ----------------------------------------------------------------------------------

subroutine test_field_bundle_smm_store_from_file(rc)
  use ESMF
  use ESMF_IOScripMod

  integer, intent(inout) :: rc

  character(*), parameter :: weightFile = 'test_fb_weights.nc'
  real, parameter :: tol = 10E-15
  integer(ESMF_KIND_I4), pointer :: factorIndexList(:,:)
  real(ESMF_KIND_R8), pointer :: factorList(:), coordX(:), coordY(:), &
    farrayPtrMem(:, :), farrayPtrFile(:, :)
  type(ESMF_Field) :: srcField, dstField, srcFields(1), dstFields(1)
  type(ESMF_FieldBundle) :: srcFieldBundle, dstFieldBundle
  type(ESMF_Grid) :: grid
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

  grid = ESMF_GridCreateNoPeriDim(maxIndex=(/10,20/), coordDep1=(/1/), &
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

  do i=lbnd(1),ubnd(1)
    coordX(i) = i*10.0
  enddo

  call ESMF_GridGetCoord(grid, coordDim=2, computationalLBound=lbnd, &
    computationalUBound=ubnd, farrayPtr=coordY, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  do i=lbnd(1),ubnd(1)
    coordY(i) = i*20.0
  enddo

  srcField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, name="srcField", &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  dstField = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, name="dstField", &
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

  call ESMF_FieldBundleSMMStore(srcFieldBundle, dstFieldBundle, weightFile, &
    routehandleFile, rc=rc)
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

  call ESMF_GridDestroy(grid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=FILENAME)) return

  ! --------------------------------------------------------------------------------

  if (countFail .eq. 0) then
    rc = ESMF_SUCCESS
  else
    rc = ESMF_FAILURE
  endif

end subroutine test_field_bundle_smm_store_from_file

! ----------------------------------------------------------------------------------
