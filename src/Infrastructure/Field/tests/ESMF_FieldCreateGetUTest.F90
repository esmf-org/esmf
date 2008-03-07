! $Id: ESMF_FieldCreateGetUTest.F90,v 1.1.2.1 2008/03/07 01:10:26 feiliu Exp $
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
      program ESMF_FieldCreateGetUTest

!------------------------------------------------------------------------------

#include <ESMF.h>

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldCreateGetUTest - Unit tests for Field Create and Get methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Create and Get unit tests.
! The companion folder Field\/src contains the definitions for the
! Field methods.
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

    real, dimension(:,:), pointer :: fptr2d
    real, dimension(:,:,:), pointer :: fptr
    real, dimension(20,22)  :: farray
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: grid, grid1
    type(ESMF_Array)        :: array
    type(ESMF_TypeKind)     :: typekind
    integer                 :: rank
    type(ESMF_StaggerLoc)   :: staggerloc
    integer, dimension(ESMF_MAXDIM) :: gridToFieldMap
    integer, dimension(ESMF_MAXDIM) :: ungriddedLBound 
    integer, dimension(ESMF_MAXDIM) :: ungriddedUBound 
    integer, dimension(ESMF_MAXDIM) :: maxHaloLWidth
    integer, dimension(ESMF_MAXDIM) :: maxHaloUWidth

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
#ifdef ESMF_EXHAUSTIVE
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create an empty field
        call test1(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an empty Field"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Get info associated with the empty field
        call test2(rc)
        write(failMsg, *) ""
        write(name, *) "Get info associated with the empty field"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2a(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2a_bigarray(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed, " // &
            "array size is bigger than Array total bounds"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2a_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed, " // &
            "with bad array size"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2b(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed, " // &
            "distgrid size equal to index space size"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2c(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed with halowidth"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed with halowidth, " // &
            "distgrid size equal to index space size"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d 1st dim distributed with halowidth, " // &
            "distgrid size equal to index space size, bad array size"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "with halo width, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            fieldget=.true., &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "1,2 dimension swapped, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed and divisible, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/17,21/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 2d both dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2e(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 3d 1st dim distributed, " // &
            "2nd dimension undistributed, 3rd dimension ungridded"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2e_ugb(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 3d 1st dim distributed, " // &
            "2nd dimension undistributed, 3rd dimension ungridded with specified ungriddedbounds"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2e_ugb_bigarray(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 3d 1st dim distributed, " // &
            "2nd dimension undistributed, 3rd dimension ungridded with smaller pecified ungriddedbounds"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2e_ugb_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 3d 1st dim distributed, " // &
            "2nd dimension undistributed, 3rd dimension ungridded with bigger specified ungriddedbounds"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2e_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array 3d 2st dim distributed, " // &
            "1nd dimension undistributed, 3rd dimension ungridded"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!        !------------------------------------------------------------------------
!        !E-X_UTest_Multi_Proc_Only
!        ! Create a field from an fortran array 2d 1st dimension distributed
!!        allocate(fptr(20, 10, 12))
!        call test2e_generic(rc, minindex=(/1,1/), maxindex=(/10,20/), &
!            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
!            regDecomp=(/2,2/), &
!            staggerloc=ESMF_STAGGERLOC_CENTER, &
!            gridToFieldMap=(/3,1/), &
!            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/))
!!        deallocate(fptr)
!        write(failMsg, *) ""
!        write(name, *) "Creating an Field from a fortran array 3d 1,3 dim distributed, " // &
!            "2nd dimension undistributed, 1,3 dimension swapped"
!        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!
!        !------------------------------------------------------------------------
!        !E-X_UTest_Multi_Proc_Only
!        ! Create a field from an fortran array 2d 1st dimension distributed
!!        allocate(fptr(10, 20, 12))
!        call test2e_generic(rc, minindex=(/1,1/), maxindex=(/10,20/), &
!            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
!            regDecomp=(/2,2/), &
!            staggerloc=ESMF_STAGGERLOC_CENTER, &
!            gridToFieldMap=(/3,2/), &
!            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/))
!!        deallocate(fptr)
!        write(failMsg, *) ""
!        write(name, *) "Creating an Field from a fortran array 3d 2,3 dim distributed, " // &
!            "1st dimension undistributed, 2,3 dimension swapped"
!        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3a(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 2d, both dimensions distributed"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3b(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 2d, both dimensions distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        ! Need to add test in case where grid and array have same size dimensions distributed
        call test3b_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 2d, both dimensions distributed, " // &
            "with halo width, gridToFieldMap has dimensions swapped"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3c(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 2d, both dimensions distributed, " // &
            "with halo width and gridToFieldMap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3d(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 3d, 1,2 dimensions distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3e(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 3d, 1,2 dimensions distributed, " // &
            "with halo width and gridToFieldMap /2,1/"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        ! Need to update FieldValidate code, dimCount is count of distgrid dims 
        call test3f(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 3d, 1,3 dimensions distributed, " // &
            "with halo width and gridToFieldMap /1,3/"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        ! Need to update FieldValidate code, dimCount is count of distgrid dims 
        call test3g(rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array, 3d, 1,3 dimensions distributed, " // &
            "with halo width and gridToFieldMap /3,1/"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"
    subroutine test1(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        field = ESMF_FieldCreateEmpty(rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

    end subroutine test1

    subroutine test2(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: rank
        type(ESMF_StaggerLoc)   :: staggerloc
        integer, dimension(ESMF_MAXDIM) :: gridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: ungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: ungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: maxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: maxHaloUWidth

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        field = ESMF_FieldCreateEmpty(rc=localrc) 
        if(localrc /= ESMF_SUCCESS) rc = ESMF_FAILURE
        call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
            rank=rank, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

    end subroutine test2

    subroutine test2a(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        type(ESMF_DistGrid)     :: distgrid
        real, dimension(:,:), allocatable   :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
!        print *, 'localPet = ', lpe
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(ec(1), ec(2)))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=sloc, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)
    end subroutine test2a

    subroutine test2a_bigarray(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(8,28)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2a_bigarray

    subroutine test2a_fail(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(4,20)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2a_fail

    subroutine test2b(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(4,20)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if(localrc /= ESMF_SUCCESS) rc = ESMF_FAILURE
    end subroutine test2b

    subroutine test2c(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(9,25)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/2,2/), maxHaloUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2c

    subroutine test2d(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(9,25)   :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/2,2/), maxHaloUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2d

    subroutine test2d_fail(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(8,25)   :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/2,2/), maxHaloUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2d_fail

    ! test2d_generic provides an generic interface to test fieldCreateFromDataPtr
    ! based on 2d real type fortran array
    subroutine test2d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        copyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, &
        fieldget)
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_CopyFlag), optional     :: copyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: maxHaloLWidth, maxHaloUWidth
        logical, optional                 :: fieldget
        integer, intent(inout)  :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i, j

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: rank
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(ESMF_MAXDIM) :: lgridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: lungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: lungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloUWidth

        integer, dimension(:,:), pointer  :: farray
        integer, dimension(:,:), pointer  :: farray1
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe

        integer, dimension(ESMF_MAXDIM)             :: ec, cc, g2fm, mhlw, mhuw
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        integer, dimension(ESMF_MAXDIM)             :: fsize
        integer, dimension(ESMF_MAXDIM)             :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(ESMF_MAXDIM)             :: fec, fcc, ftc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxIndex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridGet(grid, localDe=0, staggerloc=staggerloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(present(gridToFieldMap)) then
            g2fm = gridToFieldMap
        else
            do i = 1, ESMF_MAXDIM
                g2fm(i) = i
            enddo
        endif
        if(present(maxHaloLWidth)) then
            mhlw = maxHaloLWidth
        else
            mhlw = 0
        endif
        if(present(maxHaloUWidth)) then
            mhuw = maxHaloUWidth
        else
            mhuw = 0
        endif
        fsize=0
        do i = 1, 2
            !fsize(i) = max(cc(g2fm(i))+mhlw(i)+mhuw(i), ec(g2fm(i)))
            fsize(g2fm(i)) = max(cc(i)+mhlw(g2fm(i))+mhuw(g2fm(i)), ec(i))
        enddo

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !write(*, "(22I3)") lpe, ec, cc, fsize

        allocate(farray(fsize(1), fsize(2)))
        if(present(fieldget)) then
          if(fieldget) then
            do i = 1, fsize(1)
                do j = 1, fsize(2)
                    farray(i, j) = i+j*2
                enddo
            enddo
          endif
        endif

        field = ESMF_FieldCreate(grid, farray, copyflag=copyflag, &
            staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                rank=rank, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                maxHaloLWidth=lmaxHaloLWidth, maxHaloUWidth=lmaxHaloUWidth, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldGetDataPtr(field, farray=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            !write(*, "(42I3)") felb, feub, fclb, fcub, ftlb, ftub
            do i = ftlb(1), ftub(1)
                do j = ftlb(2), ftub(2)
                    if(farray1(i, j) .ne. ((i-ftlb(1)+1)+(j-ftlb(2)+1)*2) ) localrc = ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
          endif
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test2d_generic

    subroutine test2e(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:,:), allocatable  :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        allocate(farray(ec(1), ec(2), 10))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test2e

    subroutine test2e_ugb(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:,:), allocatable  :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(ec(1), ec(2), 10))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test2e_ugb

    subroutine test2e_ugb_bigarray(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(5,21,15)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2e_ugb_bigarray

    subroutine test2e_ugb_fail(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(5,21,5)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2e_ugb_fail

    subroutine test2e_fail(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(5,21,10)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            gridToFieldMap=(/2,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2e_fail

    ! test2e_generic provides an generic interface to test fieldCreateFromDataPtr
    ! based on 3d real type fortran array
    subroutine test2e_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        copyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, &
        fieldget)
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_CopyFlag), optional     :: copyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: maxHaloLWidth, maxHaloUWidth
        logical, optional                 :: fieldget
        integer, intent(inout)  :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: rank
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(ESMF_MAXDIM) :: lgridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: lungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: lungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloUWidth

        real, dimension(:,:,:), allocatable    :: farray
        type(ESMF_VM)                          :: vm
        integer                                :: lpe

        integer, dimension(ESMF_MAXDIM)        :: ec, cc, g2fm, mhlw, mhuw
        integer, dimension(ESMF_MAXDIM)        :: gelb, geub, gclb, gcub
        integer, dimension(ESMF_MAXDIM)        :: fsize

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxindex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridGet(grid, localDe=0, staggerloc=staggerloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(present(gridToFieldMap)) then
            g2fm = gridToFieldMap
        else
            do i = 1, ESMF_MAXDIM
                g2fm(i) = i
            enddo
        endif
        if(present(maxHaloLWidth)) then
            mhlw = maxHaloLWidth
        else
            mhlw = 0
        endif
        if(present(maxHaloUWidth)) then
            mhuw = maxHaloUWidth
        else
            mhuw = 0
        endif
        fsize = 10
        do i = 1, 3
            fsize(g2fm(i)) = max(cc(i)+mhlw(g2fm(i))+mhuw(g2fm(i)), ec(i))
        enddo
        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !write(*, "(22I3)") lpe, ec, cc, fsize

        allocate(farray(fsize(1), fsize(2), fsize(3)))

        field = ESMF_FieldCreate(grid, farray, copyflag=copyflag, &
            staggerloc=staggerloc,  gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                rank=rank, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                maxHaloLWidth=lmaxHaloLWidth, maxHaloUWidth=lmaxHaloUWidth, &
                rc=localrc)
                if (ESMF_LogMsgFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rc)) return
          endif
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test2e_generic

    subroutine test3a(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:), allocatable  :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(max(cc(1)+5, ec(1)), max(cc(2)+5, ec(2))))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/2,2/), maxHaloUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3a

    subroutine test3b(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)

        allocate(farray(max(cc(1)+14, ec(1)), max(cc(2)+16, ec(2))))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            !staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            !ungriddedLBound = (/2,3/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3b

    subroutine test3b_fail(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        real, dimension(20,27)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            gridToFieldMap=(/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    end subroutine test3b_fail

    subroutine test3c(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        !allocate(farray(max(cc(1)+14, ec(1)), max(cc(2)+16, ec(2))))
        allocate(farray(max(cc(2)+14, ec(2)), max(cc(1)+16, ec(1))))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3c

    subroutine test3d(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(max(cc(1)+7, ec(1)), max(cc(2)+10, ec(2)), 10))

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, & !gridToFieldMap = (/3,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3d

    subroutine test3e(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(max(cc(2)+7, ec(2)), max(cc(1)+10, ec(1)), 10))
        !write(*, "(22I3)") lpe, ec(1:2), cc(1:2)

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3e

    subroutine test3f(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        !allocate(farray(max(cc(2)+7, ec(2)), 10, max(cc(1)+10, ec(1))))
        allocate(farray(max(cc(1)+7, ec(1)), 10, max(cc(2)+10, ec(2))))
        !write(*, "(22I3)") lpe, ec(1:2), cc(1:2)

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3f

    subroutine test3g(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        real, dimension(:,:, :), pointer  :: farray1
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        integer, dimension(ESMF_MAXDIM)             :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(ESMF_MAXDIM)             :: fec, fcc, ftc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(max(cc(2)+7, ec(2)), 10, max(cc(1)+10, ec(1))))
        !write(*, "(22I3)") lpe, ec(1:2), cc(1:2)

        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/3,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGetDataPtr(field, farray=farray1, &
            exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
            computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
            totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        !write(*, "(42I3)") lpe, felb, feub, fclb, fcub, ftlb, ftub
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        deallocate(farray)

    end subroutine test3g

end program ESMF_FieldCreateGetUTest
