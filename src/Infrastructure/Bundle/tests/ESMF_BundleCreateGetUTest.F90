! $Id: ESMF_BundleCreateGetUTest.F90,v 1.1.2.7 2008/03/20 22:57:13 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_BundleCreateGetUTest

!------------------------------------------------------------------------------

#include <ESMF.h>

!==============================================================================
!BOPI
! !PROGRAM: ESMF_BundleCreateGetUTest - Unit tests for Bundle Create and Get methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Bundle Create and Get unit tests.
! The companion folder Bundle\/src contains the definitions for the
! Bundle methods.
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
 
#ifdef ESMF_EXHAUSTIVE
    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_REF)
    write(name, *) "Creating Bundle, add Fields, retrieve data pointers, ESMF_DATA_REF"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_COPY)
    write(name, *) "Creating Bundle, add Fields, retrieve data pointers, ESMF_DATA_COPY"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_REF, do_slicing=.true.)
    write(name, *) "Creating Bundle, add Fields, retrieve data pointers, ESMF_DATA_REF, slicing"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !EX_UTest_Multi_Proc_Only
    ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
    call bundle_test1_generic(rc, ESMF_DATA_COPY, do_slicing=.true.)
    write(name, *) "Creating Bundle, add Fields, retrieve data pointers, ESMF_DATA_COPY, slicing"
    write(failMsg, *) "Did not return SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 

#define ESMF_METHOD "ESMF_TESTS"

    subroutine bundle_test1_generic(rc, copyflag, do_slicing)
        integer             :: rc
        type(ESMF_CopyFlag), optional, intent(in)   :: copyflag
        logical, optional, intent(in)               :: do_slicing

        type(ESMF_Bundle)   :: bundle
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

        bundle = ESMF_BundleCreate(grid, 'mybundle', rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call assemble_bundle(bundle, grid, copyflag, do_slicing, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call retrieve_bundle_dataptr(bundle, do_slicing, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

    end subroutine bundle_test1_generic

    subroutine assemble_bundle(bundle, grid, copyflag, do_slicing, rc)

        type(ESMF_Bundle)   :: bundle
        type(ESMF_Grid)     :: grid
        type(ESMF_CopyFlag), optional, intent(in)   :: copyflag
        logical, optional, intent(in)               :: do_slicing
        integer, optional, intent(out)   :: rc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        type(ESMF_Field)    :: f1, f2, f3
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)  :: array8, array
        integer           :: i, j, localrc
        logical           :: ldo_slicing = .false.

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        if(present(do_slicing)) ldo_slicing = do_slicing

        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        allocate(farray1(5,10))
        allocate(farray2(5,10))
        allocate(farray3(5,20))

        do i = 1, 5
            do j = 1, 10
                farray1(i, j) = i + j * 2
                farray2(i, j) = i + j * 3
                farray3(i, j) = i + j * 4
                farray3(i, j+10) = i + (j+10) * 4
            enddo
        enddo

        f1 = ESMF_FieldCreate(grid, farray1, name='field1', rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_BundleAddField(bundle, f1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        f2 = ESMF_FieldCreate(grid, farray2(:,:), copyflag, name='field2', rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_BundleAddField(bundle, f2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        if(ldo_slicing) then

            f3 = ESMF_FieldCreate(grid, farray3(:, 4:13), copyflag, name='field3', rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_BundleAddField(bundle, f3, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
        endif

    end subroutine assemble_bundle

    subroutine retrieve_bundle_dataptr(bundle, do_slicing, rc)
        type(ESMF_Bundle)   :: bundle
        logical, optional, intent(in)               :: do_slicing
        integer, optional   :: rc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        type(ESMF_Field)    :: f1, f2, f3
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)  :: array8, array
        integer           :: fc, i, j, localrc
        logical           :: ldo_slicing = .false.

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        if(present(do_slicing)) ldo_slicing = do_slicing

        call ESMF_BundleGetField(bundle, 'field1', f1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldGetDataPtr(f1, farray1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_BundleGetField(bundle, 'field2', f2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

        call ESMF_FieldGetDataPtr(f2, farray2, rc=localrc)
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
            call ESMF_BundleGetField(bundle, 'field3', f3, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return

            call ESMF_FieldGetDataPtr(f3, farray3, rc=localrc)
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


        call ESMF_BundleGet(bundle, fieldcount=fc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return

    end subroutine retrieve_bundle_dataptr
end program ESMF_BundleCreateGetUTest

