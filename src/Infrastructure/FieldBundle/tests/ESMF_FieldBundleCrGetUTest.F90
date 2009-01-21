! $Id: ESMF_FieldBundleCrGetUTest.F90,v 1.1.2.9 2009/01/21 21:25:21 cdeluca Exp $
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
!
      program ESMF_FieldBundleCrGetUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldBundleCrGetUTest - Unit tests for FieldBundle Create and Get methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 FieldBundle Create and Get unit tests.
! The companion folder FieldBundle\/src contains the definitions for the
! FieldBundle methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    use ESMF_FieldGetMod

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
#ifdef ESMF_TESTEXHAUSTIVE
    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_REF)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATA_REF"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_COPY)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATA_COPY"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_REF, do_slicing=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATA_REF, slicing"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_COPY, do_slicing=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATA_COPY, slicing"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_REF, do_slicing=.true., do_slicing1=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATA_REF, slicing type 0 and 1"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_COPY, do_slicing=.true., do_slicing1=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATA_COPY, slicing type 0 and 1"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 

#define ESMF_METHOD "ESMF_TESTS"

    subroutine bundle_test1_generic(rc, copyflag, do_slicing, do_slicing1)
        integer             :: rc
        type(ESMF_CopyFlag), optional, intent(in)   :: copyflag
        logical, optional, intent(in)               :: do_slicing
        logical, optional, intent(in)               :: do_slicing1

        type(ESMF_FieldBundle)   :: bundle
        type(ESMF_Grid)     :: grid
        integer             :: localrc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                regDecomp=(/2,2/), name="landgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        bundle = ESMF_FieldBundleCreate(grid, 'mybundle', rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call assemble_bundle(bundle, grid, copyflag, do_slicing, do_slicing1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call retrieve_bundle_dataptr(bundle, copyflag, do_slicing, do_slicing1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call serialize_bundle(bundle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleDestroy(bundle, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

    end subroutine bundle_test1_generic

    subroutine assemble_bundle(bundle, grid, copyflag, do_slicing, do_slicing1, rc)

        type(ESMF_FieldBundle)   :: bundle
        type(ESMF_Grid)     :: grid
        type(ESMF_CopyFlag), optional, intent(in)   :: copyflag
        logical, optional, intent(in)               :: do_slicing
        logical, optional, intent(in)               :: do_slicing1
        integer, optional, intent(out)   :: rc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray4
        type(ESMF_Field)    :: f1, f2, f3, f4, f5
        type(ESMF_DistGrid) :: distgrid
        integer           :: i, j, localrc
        logical           :: ldo_slicing = .false.
        logical           :: ldo_slicing1 = .false.

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        if(present(do_slicing)) ldo_slicing = do_slicing
        if(present(do_slicing1)) ldo_slicing1 = do_slicing1

        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        allocate(farray1(5,10))
        allocate(farray2(5,10))
        allocate(farray3(5,20))
        allocate(farray4(10,20))

        do i = 1, 5
            do j = 1, 10
                farray1(i, j) = i + j * 2
                farray2(i, j) = i + j * 3
                farray3(i, j) = i + j * 4
                farray3(i, j+10) = i + (j+10) * 4
            enddo
        enddo
        do i = 1, 10
            do j = 1, 20
                farray4(i, j) = i + j * 5
            enddo
        enddo

        f1 = ESMF_FieldCreate(grid, farray1, name='field1', rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleAdd(bundle, f1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        f2 = ESMF_FieldCreate(grid, farray2(:,:), ESMF_INDEX_DELOCAL, copyflag, name='field2', rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleAdd(bundle, f2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        if(ldo_slicing) then

            f3 = ESMF_FieldCreate(grid, farray3(:, 4:13), ESMF_INDEX_DELOCAL, copyflag, name='field3', rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldBundleAdd(bundle, f3, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
        endif

        if(ldo_slicing1) then
            f4 = ESMF_FieldCreate(grid, farray4(3:7, 4:13), ESMF_INDEX_DELOCAL, copyflag, name='field4', rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldBundleAdd(bundle, f4, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            f5 = ESMF_FieldCreate(grid, farray4(3:7, ::2), ESMF_INDEX_DELOCAL, copyflag, name='field5', rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldBundleAdd(bundle, f5, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
        endif

    end subroutine assemble_bundle

    subroutine retrieve_bundle_dataptr(bundle, copyflag, do_slicing, do_slicing1, rc)
        type(ESMF_FieldBundle)   :: bundle
        type(ESMF_CopyFlag), optional, intent(in)   :: copyflag
        logical, optional, intent(in)               :: do_slicing
        logical, optional, intent(in)               :: do_slicing1
        integer, optional   :: rc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray4
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray5
        type(ESMF_Field)    :: f1, f2, f3, f4, f5
        integer           :: fc, i, j, localrc
        logical           :: ldo_slicing = .false.
        logical           :: ldo_slicing1 = .false.

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        if(present(do_slicing)) ldo_slicing = do_slicing
        if(present(do_slicing1)) ldo_slicing1 = do_slicing1

        call ESMF_FieldBundleGet(bundle, 'field1', f1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(f1, localDe=0, farray=farray1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleGet(bundle, 'field2', f2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(f2, localDe=0, farray=farray2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        do i = 1, 5
            do j = 1, 10
                if( farray1(i, j) .ne. i + j * 2) localrc = ESMF_FAILURE
            enddo
        enddo
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        do i = 1, 5
            do j = 1, 10
                if( farray2(i, j) .ne. i + j * 3) localrc = ESMF_FAILURE
            enddo
        enddo
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        if(ldo_slicing) then
            ! test field3 created from farray3 :, 4:13
            ! contiguous slice -> will work in DATA_REF and DATA_COPY mode              
            call ESMF_FieldBundleGet(bundle, 'field3', f3, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldGet(f3, localDe=0, farray=farray3, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
            do i = 1, 5
                do j = 1, 10
                    if( farray3(i, j) .ne. i + (j+3) * 4) localrc = ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
        endif
        if(ldo_slicing1) then
            ! test field4 created from farray4 3:7, 4:13
            ! discontiguous slice -> DATA_COPY will work, DATA_REF will not work
            call ESMF_FieldBundleGet(bundle, 'field4', f4, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldGet(f4, localDe=0, farray=farray4, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
            do i = 1, 5
                do j = 1, 10
                    if( farray4(i, j) .ne. i + 2 + (j+3) * 5) localrc = ESMF_FAILURE
                enddo
            enddo
            if (present(copyflag) .and. copyflag.eq.ESMF_DATA_COPY) then
              ! only DATA_COPY is expected to work correctly
              if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
            else
              if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) continue
              rc = ESMF_SUCCESS ! reset
            endif

            ! test field5 created from farray4 3:7, ::2
            ! discontiguous slice -> DATA_COPY will work, DATA_REF will not work
            call ESMF_FieldBundleGet(bundle, 'field5', f5, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldGet(f5, localDe=0, farray=farray5, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
            do i = 1, 5
                do j = 1, 10
                    if( farray5(i, j) .ne. i + 2 + (j*2-1) * 5) localrc = ESMF_FAILURE
                enddo
            enddo
            if (present(copyflag) .and. copyflag.eq.ESMF_DATA_COPY) then
              ! only DATA_COPY is expected to work correctly
              if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
            else
              if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) continue
              rc = ESMF_SUCCESS ! reset
            endif
        endif


        call ESMF_FieldBundleGet(bundle, fieldcount=fc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

    end subroutine retrieve_bundle_dataptr

    subroutine serialize_bundle(bundle, rc)
        ! inout variables
        type(ESMF_FieldBundle)                      :: bundle
        integer, optional                           :: rc

        ! local variables
        type(ESMF_FieldBundle)                      :: bundle1
        type(ESMF_VM)                               :: vm

        integer(ESMF_KIND_I4), pointer              :: buffer(:)
        integer                                     :: length, offset, localrc

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        length = 102400
        offset = 0
        allocate(buffer(length))

        call ESMF_VMGetCurrent(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        ! call serialize and deserialize and verify again
        call ESMF_FieldBundleSerialize(bundle, buffer, length, offset, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        offset = 0

        bundle1 = ESMF_FieldBundleDeserialize(vm, buffer, offset, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldBundleValidate(bundle1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(present(rc)) rc = ESMF_SUCCESS
    end subroutine serialize_bundle

end program ESMF_FieldBundleCrGetUTest

