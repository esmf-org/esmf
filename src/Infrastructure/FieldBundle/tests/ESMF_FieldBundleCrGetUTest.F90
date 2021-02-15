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
    use ESMF
    use ESMF_FieldGetMod

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

#ifdef ESMF_TESTEXHAUSTIVE
    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
#endif

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
#ifdef ESMF_TESTEXHAUSTIVE
    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATACOPY_REFERENCE)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATACOPY_REFERENCE"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATACOPY_VALUE)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATACOPY_VALUE"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATACOPY_REFERENCE, do_slicing=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATACOPY_REFERENCE, slicing"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATACOPY_VALUE, do_slicing=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATACOPY_VALUE, slicing"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATACOPY_REFERENCE, do_slicing=.true., do_slicing1=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATACOPY_REFERENCE, slicing type 0 and 1"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATACOPY_VALUE, do_slicing=.true., do_slicing1=.true.)
    write(name, *) "Creating FieldBundle, add Fields, retrieve data pointers, ESMF_DATACOPY_VALUE, slicing type 0 and 1"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 

#define ESMF_METHOD "ESMF_TESTS"

    subroutine bundle_test1_generic(rc, datacopyflag, do_slicing, do_slicing1)
        integer             :: rc
        type(ESMF_DataCopy_Flag), optional, intent(in)   :: datacopyflag
        logical, optional, intent(in)               :: do_slicing
        logical, optional, intent(in)               :: do_slicing1

        type(ESMF_FieldBundle)   :: bundle
        type(ESMF_Grid)     :: grid
        integer             :: localrc
        
        integer :: fieldCount, i
        type(ESMF_Field) :: field

        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray4

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                regDecomp=(/2,2/), name="landgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        bundle = ESMF_FieldBundleCreate(name='mybundle', rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call assemble_bundle(bundle, grid, datacopyflag, farray1, farray2, farray3, farray4, do_slicing, do_slicing1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call retrieve_bundle_dataptr(bundle, datacopyflag, do_slicing, do_slicing1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call serialize_bundle(bundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return


        call ESMF_FieldBundleGet(bundle, fieldCount=fieldCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, fieldCount
            call ESMF_FieldBundleGet(bundle, fieldIndex=i, field=field, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldDestroy(field, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

        enddo

        call ESMF_FieldBundleDestroy(bundle, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray1, farray2, farray3, farray4)

    end subroutine bundle_test1_generic

    subroutine assemble_bundle(bundle, grid, datacopyflag, farray1, farray2, farray3, farray4, do_slicing, do_slicing1, rc)

        type(ESMF_FieldBundle)   :: bundle
        type(ESMF_Grid)     :: grid
        type(ESMF_DataCopy_Flag), optional, intent(in)   :: datacopyflag
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray4
        logical, optional, intent(in)               :: do_slicing
        logical, optional, intent(in)               :: do_slicing1
        integer, optional, intent(out)   :: rc

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
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

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

        f1 = ESMF_FieldCreate(grid, farray1, ESMF_INDEX_DELOCAL, &
               datacopyflag=datacopyflag, name='field1', rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleAdd(bundle, (/f1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        f2 = ESMF_FieldCreate(grid, farray2(:,:), ESMF_INDEX_DELOCAL, &
               datacopyflag=datacopyflag, name='field2', rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleAdd(bundle, (/f2/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        if(ldo_slicing) then

            f3 = ESMF_FieldCreate(grid, farray3(:, 4:13), ESMF_INDEX_DELOCAL, &
                   datacopyflag=datacopyflag, name='field3', rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldBundleAdd(bundle, (/f3/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(ldo_slicing1) then
            f4 = ESMF_FieldCreate(grid, farray4(3:7, 4:13), ESMF_INDEX_DELOCAL, &
                   datacopyflag=datacopyflag, name='field4', rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldBundleAdd(bundle, (/f4/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            f5 = ESMF_FieldCreate(grid, farray4(3:7, ::2), ESMF_INDEX_DELOCAL, &
                   datacopyflag=datacopyflag, name='field5', rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldBundleAdd(bundle, (/f5/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        
    end subroutine assemble_bundle

    subroutine retrieve_bundle_dataptr(bundle, datacopyflag, do_slicing, do_slicing1, rc)
        type(ESMF_FieldBundle)   :: bundle
        type(ESMF_DataCopy_Flag), optional, intent(in)   :: datacopyflag
        logical, optional, intent(in)               :: do_slicing
        logical, optional, intent(in)               :: do_slicing1
        integer, optional   :: rc

        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray1
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

        call ESMF_FieldBundleGet(bundle, 'field1', field=f1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(f1, localDe=0, farrayPtr=farray1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleGet(bundle, 'field2', field=f2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(f2, localDe=0, farrayPtr=farray2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 5
            do j = 1, 10
                if( farray1(i, j) .ne. i + j * 2) localrc = ESMF_FAILURE
            enddo
        enddo
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        do i = 1, 5
            do j = 1, 10
                if( farray2(i, j) .ne. i + j * 3) localrc = ESMF_FAILURE
            enddo
        enddo
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

        if(ldo_slicing) then
            ! test field3 created from farray3 :, 4:13
            ! contiguous slice -> will work in DATA_REF and DATA_COPY mode              
            call ESMF_FieldBundleGet(bundle, 'field3', field=f3, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(f3, localDe=0, farrayPtr=farray3, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, 5
                do j = 1, 10
                    if( farray3(i, j) .ne. i + (j+3) * 4) localrc = ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(ldo_slicing1) then
            ! test field4 created from farray4 3:7, 4:13
            ! discontiguous slice -> DATA_COPY will work, DATA_REF will not work
            call ESMF_FieldBundleGet(bundle, 'field4', field=f4, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(f4, localDe=0, farrayPtr=farray4, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, 5
                do j = 1, 10
                    write(*, *) 'line 400: farray4: ', i, j, farray4(i,j), i + 2 + (j+3) * 5
                    if( farray4(i, j) .ne. i + 2 + (j+3) * 5) localrc = ESMF_FAILURE
                enddo
            enddo
            if (present(datacopyflag)) then
              if (datacopyflag.eq.ESMF_DATACOPY_VALUE) then
                ! only DATA_COPY is expected to work correctly
                if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
              else
                if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) continue
                rc = ESMF_SUCCESS ! reset
              end if
            else
              if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) continue
              rc = ESMF_SUCCESS ! reset
            endif

            ! test field5 created from farray4 3:7, ::2
            ! discontiguous slice -> DATA_COPY will work, DATA_REF will not work
            call ESMF_FieldBundleGet(bundle, 'field5', field=f5, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(f5, localDe=0, farrayPtr=farray5, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, 5
                do j = 1, 10
                    write(*, *) 'line 436: farray5: ', i, j, farray5(i,j), i + 2 + (j*2-1) * 5
                    if( farray5(i, j) .ne. i + 2 + (j*2-1) * 5) localrc = ESMF_FAILURE
                enddo
            enddo
            if (present(datacopyflag)) then
              if (datacopyflag.eq.ESMF_DATACOPY_VALUE) then
              ! only DATA_COPY is expected to work correctly
                if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) return
              else
                if (ESMF_LogFoundError(localrc, &
                      ESMF_ERR_PASSTHRU, &
                      ESMF_CONTEXT, rcToReturn=rc)) continue
                rc = ESMF_SUCCESS ! reset
              end if
            else
              if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) continue
              rc = ESMF_SUCCESS ! reset
            endif
        endif

        call ESMF_FieldBundleGet(bundle, fieldcount=fc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine retrieve_bundle_dataptr

    subroutine serialize_bundle(bundle, rc)
        ! inout variables
        type(ESMF_FieldBundle)                      :: bundle
        integer, optional                           :: rc

        ! local variables
        type(ESMF_FieldBundle)                      :: bundle1

        character, pointer                          :: buffer(:)
        integer                                     :: buff_size, offset, localrc

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        ! Inquire for buffer size
        buff_size = 1
        offset = 0
        allocate (buffer(buff_size))
        call ESMF_FieldBundleSerialize(bundle, buffer, buff_size, offset, &
            inquireflag=ESMF_INQUIREONLY, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate (buffer)


        ! call serialize and deserialize and verify again
        buff_size = offset
        print *, 'ESMF_FieldBundleCrGetUTest: computed serialization buffer size =', buff_size
        allocate (buffer(buff_size))
        offset = 0
        call ESMF_FieldBundleSerialize(bundle, buffer, buff_size, offset, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        offset = 0

        bundle1 = ESMF_FieldBundleDeserialize(buffer, offset, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleValidate(bundle1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldBundleDestroy(bundle1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(buffer)

        if(present(rc)) rc = ESMF_SUCCESS
    end subroutine serialize_bundle

end program ESMF_FieldBundleCrGetUTest

