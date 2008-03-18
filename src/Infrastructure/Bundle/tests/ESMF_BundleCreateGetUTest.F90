! $Id: ESMF_BundleCreateGetUTest.F90,v 1.1.2.1 2008/03/18 18:39:31 feiliu Exp $
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
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
#ifdef ESMF_EXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
        call bundle_test1(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a bundle, add some fields and then retrieve the data pointers from the bundle"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a bundle, add some fields and then retrieve the data pointers from the bundle
        call bundle_test2(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a bundle, add some fields and then retrieve the data pointers from the bundle " // &
            "with array slicing"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 

#define ESMF_METHOD "ESMF_TESTS"

    subroutine bundle_test1(finalrc)
        type(ESMF_Bundle)   :: bundle
        type(ESMF_Grid)     :: grid
        integer           :: rc, finalrc

        finalrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                regDecomp=(/2,2/), name="landgrid", rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        bundle = ESMF_BundleCreate(grid, 'mybundle', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
        call assemble_bundle(bundle, grid, rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call retrieve_bundle_dataptr(bundle, rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    end subroutine bundle_test1

    subroutine assemble_bundle(bundle, grid, finalrc)

        type(ESMF_Bundle)   :: bundle
        type(ESMF_Grid)     :: grid
        integer, optional, intent(out)   :: finalrc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        type(ESMF_Field)    :: f1, f2, f3
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)  :: array8, array
        integer           :: rc, i, j

        finalrc = ESMF_SUCCESS

        call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        allocate(farray1(5,10))
        allocate(farray2(5,10))
        allocate(farray3(10,20))

        do i = 1, 5
            do j = 1, 10
                farray1(i, j) = i + j * 2
                farray2(i, j) = i + j * 3
                farray3(i*2, j*2) = i + j * 4
            enddo
        enddo

        f1 = ESMF_FieldCreate(grid, farray1, name='field1', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleAddField(bundle, f1, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        f2 = ESMF_FieldCreate(grid, farray2(:,:), name='field2', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleAddField(bundle, f2, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    end subroutine assemble_bundle

    subroutine retrieve_bundle_dataptr(bundle, finalrc)
        type(ESMF_Bundle)   :: bundle
        integer, optional   :: finalrc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        type(ESMF_Field)    :: f1, f2, f3
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)  :: array8, array
        integer           :: rc, fc, i, j

        finalrc = ESMF_SUCCESS

        call ESMF_BundleGetField(bundle, 'field1', f1, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGetDataPtr(f1, farray1, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleGetField(bundle, 'field2', f2, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGetDataPtr(f2, farray2, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        do i = 1, 5
            do j = 1, 10
                if( farray1(i, j) .ne. i + j * 2) finalrc = ESMF_FAILURE
                if( farray2(i, j) .ne. i + j * 3) finalrc = ESMF_FAILURE
            enddo
        enddo

        call ESMF_BundleGet(bundle, fieldcount=fc, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    end subroutine retrieve_bundle_dataptr

    subroutine bundle_test2(finalrc)
        type(ESMF_Bundle)   :: bundle
        type(ESMF_Grid)     :: grid
        integer           :: rc, finalrc

        finalrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                regDecomp=(/2,2/), name="landgrid", rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        bundle = ESMF_BundleCreate(grid, 'mybundle', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE
        call assemble_bundle2(bundle, grid, rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call retrieve_bundle_dataptr2(bundle, rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    end subroutine bundle_test2

    subroutine assemble_bundle2(bundle, grid, finalrc)

        type(ESMF_Bundle)   :: bundle
        type(ESMF_Grid)     :: grid
        integer, optional, intent(out)   :: finalrc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        type(ESMF_Field)    :: f1, f2, f3
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)  :: array8, array
        integer           :: rc, i, j

        finalrc = ESMF_SUCCESS

        call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        allocate(farray1(5,10))
        allocate(farray2(5,10))
        allocate(farray3(10,20))

        do i = 1, 5
            do j = 1, 10
                farray1(i, j) = i + j * 2
                farray2(i, j) = i + j * 3
                farray3(i*2, j*2) = i + j * 4
            enddo
        enddo

        f1 = ESMF_FieldCreate(grid, farray1, name='field1', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleAddField(bundle, f1, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        f2 = ESMF_FieldCreate(grid, farray2(:,:), name='field2', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleAddField(bundle, f2, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        f3 = ESMF_FieldCreate(grid, farray3(3:7, 4:13), name='field3', rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleAddField(bundle, f3, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    end subroutine assemble_bundle2

    subroutine retrieve_bundle_dataptr2(bundle, finalrc)
        type(ESMF_Bundle)   :: bundle
        integer, optional   :: finalrc

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray2
        real(ESMF_KIND_R4), dimension(:,:), pointer :: farray3
        type(ESMF_Field)    :: f1, f2, f3
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)  :: array8, array
        integer           :: rc, fc, i, j

        finalrc = ESMF_SUCCESS

        call ESMF_BundleGetField(bundle, 'field1', f1, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGetDataPtr(f1, farray1, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleGetField(bundle, 'field2', f2, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGetDataPtr(f2, farray2, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_BundleGetField(bundle, 'field3', f3, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        call ESMF_FieldGetDataPtr(f3, farray3, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

        do i = 1, 5
            do j = 1, 10
                if( farray1(i, j) .ne. i + j * 2) finalrc = ESMF_FAILURE
                if( farray2(i, j) .ne. i + j * 3) finalrc = ESMF_FAILURE
                if( farray3(i*2, j*2) .ne. i + j * 4) finalrc = ESMF_FAILURE
            enddo
        enddo

        call ESMF_BundleGet(bundle, fieldcount=fc, rc=rc)
        if(rc .ne. ESMF_SUCCESS) finalrc = ESMF_FAILURE

    end subroutine retrieve_bundle_dataptr2
end program ESMF_BundleCreateGetUTest

