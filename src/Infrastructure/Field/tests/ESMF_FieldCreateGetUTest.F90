! $Id: ESMF_FieldCreateGetUTest.F90,v 1.1.2.18 2008/03/31 16:55:07 feiliu Exp $
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
    integer                 :: dimCount
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
        ! Create a field from an fortran 2d array
        call test2a(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2a_get(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d, " // &
            "get fails with smaller totalCount. "
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2a_bigarray(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d, " // &
            "array size is bigger than Array total bounds"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2a_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d, " // &
            "with bad array size"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2b(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d, " // &
            "distgrid size equal to index space size"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2c(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d with halowidth"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d with halowidth, " // &
            "distgrid size equal to index space size"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d with halowidth, " // &
            "distgrid size equal to index space size, bad array size"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with halo width, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with halo width, using ESMF_STAGGERLOC_EDGE1, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with halo width, no distgrid padding, using ESMF_STAGGERLOC_EDGE1, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap = (/2,1/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed and remapped, " // &
            "with halo width, no distgrid padding, using ESMF_STAGGERLOC_EDGE1, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap = (/2,1/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed and remapped, " // &
            "with halo width, distgrid padding, using ESMF_STAGGERLOC_EDGE1, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            fieldget=.true., &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "1,2 dimension swapped, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, corner stagger"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, corner stagger, get test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!        !------------------------------------------------------------------------
!        !E-X_UTest_Multi_Proc_Only
!        ! Create a field from an fortran 2d array
!        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
!            regDecomp=(/4,1/), &
!            staggerloc=ESMF_STAGGERLOC_CENTER, &
!            fieldget=.true., &
!            distdim=(/.true., .false./), &
!            maxHaloLWidth=(/6/), maxHaloUWidth=(/8/))
!        write(failMsg, *) ""
!        write(name, *) "Creating a Field from a fortran array 2d 1st dim distributed, " // &
!            "with extra padding and halo, center stagger, get test"
!        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, corner stagger, get test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, corner stagger, get test, grid without padding"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed and divisible, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/17,21/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/17,21/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding and halo, non unique gridToFieldMap"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "2nd dimension undistributed, 3rd dimension ungridded"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_ugb(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "2nd dimension undistributed, 3rd dimension ungridded with specified ungriddedbounds"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_ugb_bigarray(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "2nd dimension undistributed, 3rd dimension ungridded with smaller pecified ungriddedbounds"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_ugb_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
          "2nd dimension undistributed, 3rd dimension ungridded with bigger specified ungriddedbounds"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "1nd dimension undistributed, 3rd dimension ungridded"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has extra padding"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2f_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has extra padding, use GetAllocBounds internally"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has no extra padding"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2f_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has extra padding, use GetAllocBounds internally"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has extra padding, cornor stagger"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2f_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has extra padding, cornor stagger, use GetAllocBounds internally"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has no extra padding, corner stagger"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2f_generic(rc, minIndex=(/1,1,1/), maxIndex=(/10,20,30/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            regDecomp=(/4,1,1/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "grid has no extra padding, corner stagger, use GetAllocBounds internally"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2e_generic(rc, minindex=(/1,1/), maxindex=(/10,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            gridToFieldMap=(/3,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d 1,3 dim distributed, " // &
            "2nd dimension undistributed, 1,3 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2f_generic(rc, minindex=(/1,1/), maxindex=(/10,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            gridToFieldMap=(/3,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d 1,3 dim distributed, " // &
            "2nd dimension undistributed, 1,3 dimension swapped, use GetAllocBounds internally"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2e_generic(rc, minindex=(/1,1/), maxindex=(/10,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            gridToFieldMap=(/3,2/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d 2,3 dim distributed, " // &
            "1st dimension undistributed, 2,3 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d 1st dimension distributed
        call test2f_generic(rc, minindex=(/1,1/), maxindex=(/10,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            gridToFieldMap=(/3,2/), &
            ungriddedLBound=(/3/), ungriddedUBound=(/9/), &
            maxHaloLWidth=(/2,4/), maxHaloUWidth=(/5,6/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d 2,3 dim distributed, " // &
            "1st dimension undistributed, 2,3 dimension swapped, use GetAllocBounds internally"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3a(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 2d, both dimensions distributed"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3b(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 2d, both dimensions distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        ! Need to add test in case where grid and array have same size dimensions distributed
        call test3b_fail(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 2d, both dimensions distributed, " // &
            "with halo width, gridToFieldMap has dimensions swapped"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 2d both dimensions distributed
        call test3c(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 2d, both dimensions distributed, " // &
            "with halo width and gridToFieldMap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 3d
        call test3d(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 3d, 1,2 dimensions distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 3d
        call test3e(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 3d, 1,2 dimensions distributed, " // &
            "with halo width and gridToFieldMap /2,1/"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 3d
        call test3f(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 3d, 1,3 dimensions distributed, " // &
            "with halo width and gridToFieldMap /1,3/"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array 3d
        call test3g(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array, 3d, 1,3 dimensions distributed, " // &
            "with halo width and gridToFieldMap /3,1/"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            regDecomp=(/4,1,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "with halo width"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            regDecomp=(/4,1,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "with halo width, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            fieldget=.true., &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "1,2 dimension swapped, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            regDecomp=(/4,1,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d grid 2d distgrid 2d, " // &
            "with extra padding and halo"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            regDecomp=(/4,1,1/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "with extra padding and halo, corner stagger"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            regDecomp=(/4,1,1/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "with extra padding and halo, corner stagger, get test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!        !------------------------------------------------------------------------
!        !E-X_UTest_Multi_Proc_Only
!        ! Create a field from an fortran 3d array
!        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
!            regDecomp=(/4,1,1/), &
!            staggerloc=ESMF_STAGGERLOC_CENTER, &
!            fieldget=.true., &
!            distdim=(/.true., .false., .false./), &
!            maxHaloLWidth=(/6/), maxHaloUWidth=(/8/))
!        write(failMsg, *) ""
!        write(name, *) "Creating a Field from a fortran array 3d 1st dim distributed, " // &
!            "with extra padding and halo, center stagger, get test"
!        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            regDecomp=(/2,2,1/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "with extra padding and halo, corner stagger, get test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            regDecomp=(/2,2,1/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "with halo, corner stagger, get test, grid without padding"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed and divisible, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/17,21/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding and halo, 1,2 dimension swapped"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/16,20,32/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            regDecomp=(/2,2,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,1,2/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "neither dimension divisible, " // &
            "with halo, non unique gridToFieldMap"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/17,20,33/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            regDecomp=(/2,1,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3,2/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "neither dimension divisible, " // &
            "with halo"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/1,1,1/), maxindex=(/17,20,33/), &
            regDecomp=(/2,1,2/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3,2/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding and halo"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/4,5,6/), maxindex=(/17,20,33/), &
            gridEdgeLWidth=(/0,0,0/), gridEdgeUWidth=(/0,0,0/), &
            regDecomp=(/2,1,2/), &
            copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3,2/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "neither dimension divisible, minindex starts from numbers greater than 1, " // &
            "with halo and data copy"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 3d array
        call test3d_generic(rc, minindex=(/3,3,5/), maxindex=(/17,20,33/), &
            regDecomp=(/2,1,2/), &
            copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3,2/), &
            fieldget=.true., &
            maxHaloLWidth=(/6,7,3/), maxHaloUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding, halo, and data copy"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d1(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d2(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "allocate the Fortran array from bounds instead of counts"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        ! fieldget is very expensive for 7D grid, only this test has it set true
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, fieldget=true"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!        !------------------------------------------------------------------------
!        !E-X_UTest_Multi_Proc_Only
!        ! Create a 7D field from a 5D grid and 2D ungridded bounds
!        ! The following test fails with Intel compiler v 10.0.23 
!        ! due to run time memory requirement
!        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
!            regDecomp=(/2,1,2,1,1/), &
!            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
!            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
!            copyflag=ESMF_DATA_COPY, &
!            gridToFieldMap=(/1,2,4,5,7/) &
!            )
!        write(failMsg, *) ""
!        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
!            "using generic interface, irregular gridToFieldMap, data copy"
!        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/4,2,4,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,1,2,1,1/), &
            copyflag=ESMF_DATA_COPY, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, data copy"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, edge1 stagger"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            gridEdgeLWidth=(/1,0,1,0,1/), gridEdgeUWidth=(/0,1,2,1,0/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, edge1 stagger " // &
            "distgrid padded"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            distgridToGridMap=(/3,2,5,4,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,5,4,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
        call test7d_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/10,20,10,20/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,5,3/), &
            maxHaloLWidth=(/1,1,1,2/), maxHaloUWidth=(/2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
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
        field = ESMF_FieldCreate(rc=localrc) 
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
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: staggerloc
        integer, dimension(ESMF_MAXDIM) :: gridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: ungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: ungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: maxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: maxHaloUWidth

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        field = ESMF_FieldCreate(rc=localrc) 
        if(localrc /= ESMF_SUCCESS) rc = ESMF_FAILURE
        call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
            dimCount=dimCount, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
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

    subroutine test2a_get(rc)
        integer, intent(inout)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        type(ESMF_DistGrid)     :: distgrid
        real, dimension(:,:), allocatable   :: farray
        real, dimension(:,:), pointer       :: farray1

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(ESMF_MAXDIM)             :: ec, cc
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        integer                                     :: totalCount(1:1)

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

        call ESMF_FieldGet(field, farray1, totalCount=totalCount, rc=localrc)
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
    end subroutine test2a_get

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
    ! on 2d grid and 2d arrays
    subroutine test2d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        copyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, &
        distdim, &
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
        logical, dimension(:), optional   :: distdim
        logical, optional                 :: fieldget
        integer, intent(inout)  :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i, j

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: dimCount
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
        integer, dimension(2)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(2)                       :: fec, fcc, ftc
        integer                                     :: gridDistDimCount

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxIndex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, distdim=distdim, name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridGet(grid, distDimCount=gridDistDimCount, rc=localrc)
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
            g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        else
            do i = 1, ESMF_MAXDIM
                g2fm(i) = i
            enddo
        endif
        mhlw = 0
        if(present(maxHaloLWidth)) then
            mhlw(1:gridDistDimCount) = maxHaloLWidth(1:gridDistDimCount)
        endif
        mhuw = 0
        if(present(maxHaloUWidth)) then
            mhuw(1:gridDistDimCount) = maxHaloUWidth(1:gridDistDimCount)
        endif
        fsize=0
        do i = 1, 2
            !fsize(i) = max(cc(g2fm(i))+mhlw(i)+mhuw(i), ec(g2fm(i)))
            !fsize(g2fm(i)) = max(cc(i)+mhlw(g2fm(i))+mhuw(g2fm(i)), ec(i))
            ! now halowidth is in array dimension order
            fsize(i) = max(cc(g2fm(i))+mhlw(i)+mhuw(i), ec(g2fm(i)))
        enddo

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
!        write(*, "(A5, 13I3)") 'MZZ: ', lpe, ec(1:2), cc(1:2), fsize(1:2),&
!             mhlw(1:2), mhuw(1:2), g2fm(1:2)

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
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                maxHaloLWidth=lmaxHaloLWidth, maxHaloUWidth=lmaxHaloUWidth, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldGet(field, farray=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            !write(*, "(A5, 42I3)") 'MZY: ', felb, feub, fclb, fcub, ftlb, ftub
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
        integer, intent(inout)  :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_CopyFlag),   optional   :: copyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: maxHaloLWidth, maxHaloUWidth
        logical, optional                 :: fieldget

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(ESMF_MAXDIM) :: lgridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: lungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: lungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloUWidth

        real, dimension(:,:,:), allocatable    :: farray
        type(ESMF_VM)                          :: vm
        integer                                :: lpe

        integer, dimension(ESMF_MAXDIM)        :: ec, cc, g2fm, mhlw, mhuw, dg2fm, f2dgm, dg2gm
        integer, dimension(ESMF_MAXDIM)        :: gelb, geub, gclb, gcub
        integer, dimension(ESMF_MAXDIM)        :: fsize
        integer                                :: gridDistDimCount, forderIndex

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxindex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridGet(grid, distDimCount=gridDistDimCount, distgridToGridMap=dg2gm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_GridGet(grid, localDe=0, staggerloc=staggerloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
!        call ESMF_GridGetArrayUndistInfo(grid, &
!             staggerloc=staggerloc, & 
!             gridToArrayMap=g2fm, &
!             distgridToArrayMap=dg2fm, &
!             rc=localrc)
!        if (ESMF_LogMsgFoundError(localrc, &
!            ESMF_ERR_PASSTHRU, &
!            ESMF_CONTEXT, rc)) return

        if(present(gridToFieldMap)) then
            g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        else
            do i = 1, ESMF_MAXDIM
                g2fm(i) = i
            enddo
        endif
        mhlw = 0
        if(present(maxHaloLWidth)) then
            mhlw(1:gridDistDimCount) = maxHaloLWidth(1:gridDistDimCount)
        endif
        mhuw = 0
        if(present(maxHaloUWidth)) then
            mhuw(1:gridDistDimCount) = maxHaloUWidth(1:gridDistDimCount)
        endif
        fsize = 10
        ! create a couple of mappings following index mapping lemma 1 and lemma 7
        do i = 1, gridDistDimCount
            dg2fm(i) = g2fm(dg2gm(i))
        enddo
        f2dgm = 0
        do i = 1, gridDistDimCount
            f2dgm(dg2fm(i)) = i
        enddo
        forderIndex = 1
        do i = 1, 3
            !fsize(g2fm(i)) = max(cc(i)+mhlw(g2fm(i))+mhuw(g2fm(i)), ec(i))
            if(f2dgm(i) .gt. 0) then
                fsize(i) = max(cc(f2dgm(i))+mhlw(forderIndex)+mhuw(forderIndex), ec(f2dgm(i)))
                forderIndex = forderIndex + 1
            endif
        enddo
        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
!        write(*, "(A7, 17I3)") 'MZS: ', lpe, ec(1:3), cc(1:3), fsize(1:3), &
!            dg2fm(1:3), f2dgm(1:3), gridDistDimCount

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
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
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

    ! test2e_generic provides an generic interface to test fieldCreateFromDataPtr
    ! based on 3d real type fortran array
    ! The fortran array shape is automatically computed from FieldGetAllocBounds
    subroutine test2f_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        copyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, &
        fieldget)
        integer, intent(inout)  :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_CopyFlag),   optional   :: copyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: maxHaloLWidth, maxHaloUWidth
        logical, optional                 :: fieldget

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(ESMF_MAXDIM) :: lgridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: lungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: lungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloUWidth

        real, dimension(:,:,:), allocatable    :: farray
        type(ESMF_VM)                          :: vm
        integer                                :: lpe

        integer, dimension(ESMF_MAXDIM)        :: ec, cc, g2fm, mhlw, mhuw, dg2fm, f2dgm, dg2gm
        integer, dimension(ESMF_MAXDIM)        :: gelb, geub, gclb, gcub
        integer, dimension(3)                  :: fsize
        integer                                :: gridDistDimCount, forderIndex

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxindex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(grid, staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            allocCount=fsize, rc=localrc)
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
!        write(*, "(A7, 17I3)") 'MZS: ', lpe, ec(1:3), cc(1:3), fsize(1:3), &
!            dg2fm(1:3), f2dgm(1:3), gridDistDimCount

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
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
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
    end subroutine test2f_generic

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

        allocate(farray(max(cc(1)+7, ec(1)), 10, max(cc(2)+10, ec(2))))
        !write(*, "(A7, 22I3)") 'MZS: ', lpe, ec(1:2), cc(1:2), max(cc(1)+7, ec(1)), max(cc(2)+10, ec(2))

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

        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc

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

        call ESMF_FieldGet(field, farray=farray1, &
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

    ! test3d_generic provides a generic interface to test fieldCreateFromDataPtr
    ! on 3d grid and 3d arrays
    subroutine test3d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        copyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, &
        distdim, &
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
        logical, dimension(:), optional   :: distdim
        logical, optional                 :: fieldget
        integer, intent(inout)  :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i, j, k

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(ESMF_MAXDIM) :: lgridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: lungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: lungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloUWidth

        integer, dimension(:,:,:), pointer  :: farray
        integer, dimension(:,:,:), pointer  :: farray1
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe

        integer, dimension(ESMF_MAXDIM)             :: ec, cc, g2fm, mhlw, mhuw
        integer, dimension(ESMF_MAXDIM)             :: gelb, geub, gclb, gcub
        integer, dimension(ESMF_MAXDIM)             :: fsize
        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc
        integer                                     :: gridDistDimCount

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxIndex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, distdim=distdim, name="testgrid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_GridGet(grid, distDimCount=gridDistDimCount, rc=localrc)
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
            g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        else
            do i = 1, ESMF_MAXDIM
                g2fm(i) = i
            enddo
        endif
        mhlw = 0
        if(present(maxHaloLWidth)) then
            mhlw(1:gridDistDimCount) = maxHaloLWidth(1:gridDistDimCount)
        endif
        mhuw = 0
        if(present(maxHaloUWidth)) then
            mhuw(1:gridDistDimCount) = maxHaloUWidth(1:gridDistDimCount)
        endif
        fsize=0
        do i = 1, 3
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
        !write(*, "(A5, 22I3)") 'MZZ: ', lpe, ec, cc, fsize
        !write(*, "(A5, 22I3)") 'MZZ: ', lpe, mhlw, mhuw

        allocate(farray(fsize(1), fsize(2), fsize(3)))
        if(present(fieldget)) then
          if(fieldget) then
            do i = 1, fsize(1)
                do j = 1, fsize(2)
                    do k = 1, fsize(3)
                        farray(i, j, k) = i+j*2
                    enddo
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
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                maxHaloLWidth=lmaxHaloLWidth, maxHaloUWidth=lmaxHaloUWidth, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldGet(field, farray=farray1, &
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
                    do k = ftlb(3), ftub(3)
                        if(farray1(i, j, k) .ne. ((i-ftlb(1)+1)+(j-ftlb(2)+1)*2) ) &
                            localrc = ESMF_FAILURE
                    enddo
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

    end subroutine test3d_generic

    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetDataBounds
    subroutine test7d1(rc)
        integer, intent(inout) :: rc

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: f8
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)    :: array8, array
        integer             :: localrc
        integer             :: fsize(7)

        localrc = ESMF_SUCCESS
        distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

! Cannot use the following numbers, cause overflow on most systems due to memory requirement
!        call ESMF_FieldGet(grid, ungriddedLBound=(/1,2/), ungriddedUBound=(/10,23/), &
!            maxHaloLWidth=(/2,3,4,5,6/), maxHaloUWidth=(/10,11,12,13,14/), &
!            allocCount=fsize, &
!            rc=localrc)
        call ESMF_FieldGet(grid, ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            allocCount=fsize, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(fsize(1), fsize(2), fsize(3), fsize(4), fsize(5), fsize(6), fsize(7)))

        f8 = ESMF_FieldCreate(grid, farray, &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(f8)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        call ESMF_ArrayDestroy(array8)
    end subroutine test7d1

    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetDataBounds
    subroutine test7d2(rc)
        integer, intent(inout) :: rc

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: f8
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)    :: array8, array
        integer             :: localrc
        integer             :: flb(7), fub(7)

        localrc = ESMF_SUCCESS
        distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
            regDecomp=(/2,1,2,1,1/), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(grid, ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            allocLBound=flb, allocUBound=fub, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )

        f8 = ESMF_FieldCreate(grid, farray, &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/1,1,1,2,2/), maxHaloUWidth=(/1,2,3,4,5/), &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldDestroy(f8)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        call ESMF_ArrayDestroy(array8)
    end subroutine test7d2

    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetDataBounds
    subroutine test7d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        copyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        maxHaloLWidth, maxHaloUWidth, &
        fieldget)
        integer, intent(inout) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_CopyFlag), optional     :: copyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: maxHaloLWidth, maxHaloUWidth
        logical, optional                 :: fieldget

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)    :: array8
        integer             :: localrc
        integer             :: fsize(7)

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(ESMF_MAXDIM) :: lgridToFieldMap
        integer, dimension(ESMF_MAXDIM) :: lungriddedLBound 
        integer, dimension(ESMF_MAXDIM) :: lungriddedUBound 
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloLWidth
        integer, dimension(ESMF_MAXDIM) :: lmaxHaloUWidth

        integer                                     :: ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n

        localrc = ESMF_SUCCESS
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        call ESMF_FieldGet(grid, ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            gridToFieldMap=gridToFieldMap, &
            allocCount=fsize, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        allocate(farray(fsize(1), fsize(2), fsize(3), fsize(4), fsize(5), fsize(6), fsize(7)))
        write(*, '(7I4)') fsize

        if(present(fieldget)) then
          if(fieldget) then
            do ii = 1, fsize(1)
             do ij = 1, fsize(2)
              do ik = 1, fsize(3)
               do il = 1, fsize(4)
                do im = 1, fsize(5)
                 do io = 1, fsize(6)
                  do ip = 1, fsize(7)
                    farray(ii,ij,ik,il,im,io,ip) = ii+ij*2+ik+il*2+im+io*2+ip
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
          endif
        endif

        field = ESMF_FieldCreate(grid, farray, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            gridToFieldMap=gridToFieldMap, &
            copyflag=copyflag, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                maxHaloLWidth=lmaxHaloLWidth, maxHaloUWidth=lmaxHaloUWidth, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            call ESMF_FieldGet(field, farray=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
            !write(*, "(A5, 42I3)") 'MZY: ', felb, feub, fclb, fcub, ftlb, ftub
            do ii = ftlb(1), ftub(1)
             do ij = ftlb(2), ftub(2)
              do ik = ftlb(3), ftub(3)
               do il = ftlb(4), ftub(4)
                do im = ftlb(5), ftub(5)
                 do io = ftlb(6), ftub(6)
                  do ip = ftlb(7), ftub(7)
                    n=(ii-ftlb(1)+1)+(ij-ftlb(2)+1)*2+ &
                      (ik-ftlb(3)+1)+(il-ftlb(4)+1)*2+ &
                      (im-ftlb(5)+1)+(io-ftlb(6)+1)*2+ &
                      (ip-ftlb(7)+1)
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field)
        call ESMF_GridDestroy(grid)
        call ESMF_DistGridDestroy(distgrid)
        call ESMF_ArrayDestroy(array8)
    end subroutine test7d_generic

end program ESMF_FieldCreateGetUTest
