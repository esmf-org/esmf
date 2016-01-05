! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldCreateGetUTest - Unit tests for Field Create and Get methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Create and Get unit tests.
! The companion folder Fieldsrc contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_GridUtilMod


    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
#ifdef ESMF_TESTEXHAUSTIVE
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
        ! Create a 2D field with global indices
        call test_globalindex(rc)
        write(failMsg, *) "Test unsuccessful"
        write(name, *) "Creating a 2D Field with a global index"
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed and remapped, " // &
            "with halo width, distgrid padding, using ESMF_STAGGERLOC_EDGE1, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            gridToFieldMap = (/2,1/), &
            fieldget=.true., &
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed and remapped, " // &
            "with halo width, distgrid padding, using ESMF_STAGGERLOC_CENTER, fieldget checked"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
            regDecomp=(/4,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 2d both dims distributed, " // &
            "with extra padding and halo, corner stagger, get test"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran 2d array
        call test2d_generic(rc, minindex=(/1,1/), maxindex=(/16,20/), &
            regDecomp=(/2,2/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/2,4/), totalUWidth=(/5,6/))
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
            totalLWidth=(/2,4/), totalUWidth=(/5,6/))
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
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            totalLWidth=(/2,4/), totalUWidth=(/5,6/))
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
            totalLWidth=(/2,4/), totalUWidth=(/5,6/))
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
        ! Create a field from an fortran array pointer 2d both dimensions distributed
        call test3a_fptr(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array pointer, 2d, both dimensions distributed"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a field from an fortran array pointer 2d one dimension distributed
        call test3a2_fptr(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array pointer, 2d, one dimension distributed"
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
!            totalLWidth=(/6/), totalUWidth=(/8/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7/), totalUWidth=(/8,9/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3,2/), &
            fieldget=.true., &
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
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
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3,2/), &
            fieldget=.true., &
            totalLWidth=(/6,7,3/), totalUWidth=(/8,9,2/))
        write(failMsg, *) ""
        write(name, *) "Creating a Field from a fortran array 3d all dims distributed, " // &
            "neither dimension divisible, " // &
            "with extra padding, halo, and data copy"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d1(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2(rc)
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "allocate the Fortran array from bounds instead of counts"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,2,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
        ! fieldget is very expensive for 7D grid, only this test has it set true
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, fieldget=true"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, fieldget=true"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,1/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!        !------------------------------------------------------------------------
!        !E-X_UTest_Multi_Proc_Only
!        ! Create a 7D field from a 5D grid and 2D ungridded bounds
!        ! The following test fails with Intel compiler v 10.0.23 
!        ! due to run time memory requirement
!        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/10,20,10,20,10/), &
!            regDecomp=(/2,1,2,1,1/), &
!            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
!            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
!            datacopyflag=ESMF_DATACOPY_VALUE, &
!            gridToFieldMap=(/1,2,4,5,7/) &
!            )
!        write(failMsg, *) ""
!        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
!            "using generic interface, irregular gridToFieldMap, data copy"
!        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/4,2,4,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,1,2,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, data copy"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/4,2,2,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,1,2,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, data copy"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, edge1 stagger"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, edge1 stagger"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            gridEdgeLWidth=(/1,0,1,0,1/), gridEdgeUWidth=(/0,1,2,1,0/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, edge1 stagger " // &
            "distgrid padded"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/2,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            gridEdgeLWidth=(/1,0,1,0,1/), gridEdgeUWidth=(/0,1,2,1,0/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, edge1 stagger " // &
            "distgrid padded"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            distgridToGridMap=(/3,2,5,4,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,1,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            distgridToGridMap=(/3,2,5,4,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,5,4,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,5,4,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,1/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 1D grid and 6D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d_generic(rc, minIndex=(/1/), maxIndex=(/6/), &
            regDecomp=(/4/), &
            ungriddedLBound=(/1,2,1,2,1,2/), ungriddedUBound=(/4,2,3,3,2,4/), &
            totalLWidth=(/1/), totalUWidth=(/2/), &
            fieldget=.true., &
            distgridToGridMap=(/1/), &
            gridToFieldMap=(/4/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 1D grid and 6D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 1D grid and 6D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic(rc, minIndex=(/1/), maxIndex=(/3/), &
            regDecomp=(/4/), &
            ungriddedLBound=(/1,2,1,2,1,2/), ungriddedUBound=(/2,2,3,3,2,4/), &
            totalLWidth=(/1/), totalUWidth=(/2/), &
            fieldget=.true., &
            distgridToGridMap=(/1/), &
            gridToFieldMap=(/4/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 1D grid and 6D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,1,1,1,1,1/), &
            maxIndex=(/2,2,2,2,2,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            fieldget=.false. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " no fieldget"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth, ESMF_DATACOPY_VALUE"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth, ESMF_DATACOPY_VALUE, CORNER STAGGER"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d2_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth, EDGE1 STAGGER"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,1,1,1,1,1/), &
            maxIndex=(/2,2,2,2,2,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            fieldget=.false. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " no fieldget"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth, ESMF_DATACOPY_VALUE"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth, ESMF_DATACOPY_VALUE, CORNER STAGGER"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! CreateFromPtr test
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_fptr(rc, minIndex=(/1,1,2,1,2,1,1/), &
            maxIndex=(/2,2,3,2,3,2,6/), &
            gridEdgeLWidth=(/0,0,0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0,0,0/), &
            regDecomp=(/2,2,1,1,1,1,1/), &
            staggerloc=ESMF_STAGGERLOC_EDGE1, &
            gridToFieldMap=(/1,2,4,7,5,6,3/), &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            totalLWidth=(/1,1,1,2,2,1,1/), totalUWidth=(/1,1,2,1,1,1,1/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field using generic interface, arbitrary gridToFieldMap" // &
            " arbitrary distgridToGridMap, arbitrary haloWidth, EDGE1 STAGGER"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !  Begin testing field create/get from grid and arrayspec
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, fieldget=true"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/4,2,4,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,1,2,1,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, data copy"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 1D grid and 6D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1/), maxIndex=(/6/), &
            regDecomp=(/4/), &
            ungriddedLBound=(/1,2,1,2,1,2/), ungriddedUBound=(/4,2,3,3,2,4/), &
            totalLWidth=(/1/), totalUWidth=(/2/), &
            fieldget=.true., &
            distgridToGridMap=(/1/), &
            gridToFieldMap=(/4/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 1D grid and 6D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 7D grid
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1,1,1,1/), maxIndex=(/3,4,3,4,3,3,3/), &
            regDecomp=(/2,1,2,1,1,1,1/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, center stagger"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_repdim(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1,1/), ungriddedUBound=(/4,2,3,1/), &
            totalLWidth=(/1,1,2/), totalUWidth=(/1,4,5/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,4,0,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 4D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 3D field from a 4D grid and 1D ungridded bounds
        call test3d_generic_repdim(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/4/), &
            totalLWidth=(/1,1/), totalUWidth=(/4,5/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,0,2,0/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 3D field from a 4D grid and 1D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 3D field from a 4D grid
        call test3d_generic_repdim(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            totalLWidth=(/1,1,0/), totalUWidth=(/4,5,0/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,3,2,0/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 3D field from a 4D grid " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d3_generic_repdim_sct(rc, &
            minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1,1/), ungriddedUBound=(/4,2,3,1/), &
            totalLWidth=(/1,1,2/), totalUWidth=(/1,4,5/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,4,0,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Complete a 7D field from a 4D grid and 4D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 3D field from a 4D grid
        call test3d_generic_repdim_sct(rc, &
            minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            totalLWidth=(/1,1,2/), totalUWidth=(/1,4,5/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,3,0,2/) &
            )
        write(failMsg, *) ""
        write(name, *) "Complete a 3D field from a 4D grid " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 3D field from a 4D grid
        call test3d_generic_repdim_sct(rc, &
            minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            totalLWidth=(/1,2/), totalUWidth=(/1,4/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/2/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,0,0,2/) &
            )
        write(failMsg, *) ""
        write(name, *) "Complete a 3D field from a 4D grid and 1D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 3D field from a 4D grid
        call test3d_generic_sctptr(rc, &
            minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            totalLWidth=(/1,2,1/), totalUWidth=(/1,3,4/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/3,0,1,2/) &
            )
        write(failMsg, *) ""
        write(name, *) "Complete a 3D field from a 4D grid " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger, replicated dimension"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 2D field from a 3D grid
        call test2d_generic_sctptr(rc, &
            minIndex=(/1,1,1/), maxIndex=(/4,3,4/), &
            regDecomp=(/2,2,1/), &
            totalLWidth=(/1,2/), totalUWidth=(/3,4/), &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1/), &
            gridToFieldMap=(/0,2,1/) &
            )
        write(failMsg, *) ""
        write(name, *) "Complete a 2D field from a 3D grid " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "corner stagger, replicated dimension"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !  Begin testing field create/get from grid and array
        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            gridEdgeLWidth=(/0,0,0,0,0/), gridEdgeUWidth=(/0,0,0,0,0/), &
            fieldget=.true., &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface"
        ! Why should this test fail - BOB?
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        !call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            gridEdgeLWidth=(/0,0,0,0,0/), gridEdgeUWidth=(/1,1,1,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            fieldget=.true. &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, fieldget=true"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,4,5/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 5D grid and 2D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1,1/), maxIndex=(/4,2,4,2,2/), &
            regDecomp=(/2,1,2,1,1/), &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,1,2,1,1/), &
            gridToFieldMap=(/1,2,4,5,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 5D grid and 2D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap, data copy"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 1D grid and 6D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1/), maxIndex=(/6/), &
            regDecomp=(/4/), &
            ungriddedLBound=(/1,2,1,2,1,2/), ungriddedUBound=(/4,2,3,3,2,4/), &
            totalLWidth=(/1/), totalUWidth=(/2/), &
            fieldget=.true., &
            distgridToGridMap=(/1/), &
            gridToFieldMap=(/4/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 1D grid and 6D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,4/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/4,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 7D grid
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1,1,1,1/), maxIndex=(/3,4,3,4,3,3,3/), &
            regDecomp=(/2,1,2,1,1,1,1/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4,7,5,6/), &
            gridToFieldMap=(/1,2,4,7,5,6,3/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        ! Create a 7D field from a 4D grid and 3D ungridded bounds
        ! this test fails since grid padding is currently hardcoded in test7d4
#ifndef ESMF_NO_GREATER_THAN_4D
        call test7d4_generic(rc, minIndex=(/1,1,1,1/), maxIndex=(/3,4,3,2/), &
            regDecomp=(/2,1,2,1/), &
            ungriddedLBound=(/1,2,1/), ungriddedUBound=(/2,2,3/), &
            totalLWidth=(/1,1,1,2/), totalUWidth=(/2,3,4,5/), &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            fieldget=.true., &
            distgridToGridMap=(/3,2,1,4/), &
            gridToFieldMap=(/1,2,4,7/) &
            )
        write(failMsg, *) ""
        write(name, *) "Creating a 7D field from a 4D grid and 3D ungridded bounds " // &
            "using generic interface, irregular gridToFieldMap and distgridToGridMap " // &
            "data copy, corner stagger"
#else
        rc = ESMF_SUCCESS
        write(name, *) "Skipped test because: ESMF_NO_GREATER_THAN_4D"
#endif
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
        !------------------------------------------------------------------------
        !NEX_UTest_Multi_Proc_Only
        !    create a 3d array.
        !    add it to a field with the ESMF_DATACOPY_VALUE flag set (not REF).
        !    delete the original array� (i should have made a copy inside the
        !    field)

        !    get the array from the field
        !    get a data pointer from the array
        !    it crashes for him here.
        call test_atnas_gridindex(rc)
        write(failMsg, *) ""
        write(name, *) "Testing Atnas's case #3556962"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !NEX_UTest_Multi_Proc_Only
        !    create a 3d array.
        !    add it to a field with the ESMF_DATACOPY_VALUE flag set (not REF).
        !    delete the original array� (i should have made a copy inside the
        !    field)

        !    get the array from the field
        !    get a data pointer from the array
        !    it crashes for him here.
        call test_eric_klusek(rc)
        write(failMsg, *) ""
        write(name, *) "Testing Eric Klusek's case #852717"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !NEX_UTest_Multi_Proc_Only
        ! Creating a field from uninitialized array returns failure #1704598
        call test_uninit_array(rc)
        write(failMsg, *) ""
        write(name, *) "Testing field create from uninit array"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !NEX_UTest_Multi_Proc_Only
        ! Creating a field where gridToFieldMap are all 0 #2896114
        call test_allrep1(rc)
        write(failMsg, *) ""
        write(name, *) "Testing field create when all entries in map are 0 allocatable"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !NEX_UTest_Multi_Proc_Only
        ! Creating a field where gridToFieldMap are all 0 #2896114
        call test_allrep2(rc)
        write(failMsg, *) ""
        write(name, *) "Testing field create when all entries in map are 0 pointer"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"
    subroutine test1(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        field = ESMF_FieldEmptyCreate(rc=localrc) 
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test1

    subroutine test2(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: staggerloc
        integer, dimension(1) :: gridToFieldMap
        integer, dimension(2) :: ungriddedLBound 
        integer, dimension(3) :: ungriddedUBound 
        integer, dimension(4,1) :: totalLWidth
        integer, dimension(5,1) :: totalUWidth

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        field = ESMF_FieldEmptyCreate(rc=localrc) 
        if(localrc /= ESMF_SUCCESS) rc = ESMF_FAILURE
        call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
            dimCount=dimCount, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test2

    subroutine test2a(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:), allocatable   :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
!        print *, 'localPet = ', lpe
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(ec(1), ec(2)))

        field = ESMF_FieldCreate(grid, farray, indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=sloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)
    end subroutine test2a


    subroutine test2a_get(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:), allocatable   :: farray
        real, dimension(:,:), pointer       :: farray1

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        integer                                     :: totalCount(1:1)

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
!        print *, 'localPet = ', lpe
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(ec(1), ec(2)))

        field = ESMF_FieldCreate(grid, farray, indexflag=ESMF_INDEX_DELOCAL, &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=sloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, totalCount=totalCount, &
          rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)
    end subroutine test2a_get

    subroutine test2a_bigarray(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(8,28)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, indexflag=ESMF_INDEX_DELOCAL, &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2a_bigarray

    subroutine test2a_fail(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(3,19)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2a_fail

    subroutine test2b(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(4,20)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if(localrc /= ESMF_SUCCESS) rc = ESMF_FAILURE
    end subroutine test2b

    subroutine test2c(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(9,25)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            totalLWidth=(/2,2/), totalUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2c

    subroutine test2d(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(9,25)   :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            totalLWidth=(/2,2/), totalUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2d

    subroutine test2d_fail(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(8,25)   :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            totalLWidth=(/2,2/), totalUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2d_fail

    ! test2d_generic provides an generic interface to test fieldCreateFromDataPtr
    ! on 2d grid and 2d arrays
    subroutine test2d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget
        integer, intent(out)  :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i, j

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(2) :: lgridToFieldMap
        integer, dimension(2) :: lungriddedLBound 
        integer, dimension(2) :: lungriddedUBound 
        integer, dimension(2,1) :: ltotalLWidth
        integer, dimension(2,1) :: ltotalUWidth

        integer, dimension(:,:), pointer  :: farray
        integer, dimension(:,:), pointer  :: farray1
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe

        integer, dimension(2)             :: ec, cc, g2fm, mhlw, mhuw
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        integer, dimension(2)                       :: fsize
        integer, dimension(2)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(2)                       :: fec, fcc, ftc
        integer                                     :: gridDimCount

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, dimCount=gridDimCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridGet(grid, localDe=0, staggerloc=staggerloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(gridToFieldMap)) then
            g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        else
            do i = 1, 2
                g2fm(i) = i
            enddo
        endif
        mhlw = 0
        if(present(totalLWidth)) then
            mhlw(1:gridDimCount) = totalLWidth(1:gridDimCount)
        endif
        mhuw = 0
        if(present(totalUWidth)) then
            mhuw(1:gridDimCount) = totalUWidth(1:gridDimCount)
        endif
        fsize=0
        do i = 1, 2
            ! now halowidth is in array dimension order
            fsize(i) = max(cc(g2fm(i))+mhlw(i)+mhuw(i), ec(g2fm(i)))
        enddo

        call ESMF_GridGetFieldBounds(grid, localDe=0, staggerloc=staggerloc, &
            gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            totalCount=fsize, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
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

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=datacopyflag, &
            staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGetBounds(field, exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !write(*, "(A5, 42I3)") 'MZY: ', felb, feub, fclb, fcub, ftlb, ftub
            do i = ftlb(1), ftub(1)
                do j = ftlb(2), ftub(2)
                    if(farray1(i, j) .ne. ((i-ftlb(1)+1)+(j-ftlb(2)+1)*2) ) localrc = ESMF_FAILURE
                enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test2d_generic

    subroutine test2e(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:,:), allocatable  :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(farray(ec(1), ec(2), 10))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test2e

    subroutine test2e_ugb(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:,:), allocatable  :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(ec(1), ec(2), 10))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test2e_ugb

    subroutine test2e_ugb_bigarray(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(5,21,15)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2e_ugb_bigarray

    subroutine test2e_ugb_fail(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(5,21,5)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2e_ugb_fail

    subroutine test2e_fail(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(5,21,10)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/4,1/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            gridToFieldMap=(/2,1/), &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test2e_fail

    ! test2e_generic provides an generic interface to test fieldCreateFromDataPtr
    ! based on 3d real type fortran array
    subroutine test2e_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, intent(out)  :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_DataCopy_Flag),   optional   :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(3) :: lgridToFieldMap
        integer, dimension(3) :: lungriddedLBound 
        integer, dimension(3) :: lungriddedUBound 
        integer, dimension(3,1) :: ltotalLWidth
        integer, dimension(3,1) :: ltotalUWidth

        real, dimension(:,:,:), allocatable    :: farray
        type(ESMF_VM)                          :: vm
        integer                                :: lpe

        integer, dimension(3)        :: ec, cc, g2fm, mhlw, mhuw, dg2fm, f2dgm, dg2gm
        integer, dimension(3)        :: gelb, geub, gclb, gcub
        integer, dimension(3)        :: fsize
        integer                                :: gridDimCount, forderIndex

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxindex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, dimCount=gridDimCount, distgridToGridMap=dg2gm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridGet(grid, localDe=0, staggerloc=staggerloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
!        call ESMF_GridGetArrayUndistInfo(grid, &
!             staggerloc=staggerloc, & 
!             gridToArrayMap=g2fm, &
!             distgridToArrayMap=dg2fm, &
!             rc=localrc)
!        if (ESMF_LogFoundError(localrc, &
!            ESMF_ERR_PASSTHRU, &
!            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(gridToFieldMap)) then
            g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        else
            do i = 1, 3
                g2fm(i) = i
            enddo
        endif
        mhlw = 0
        if(present(totalLWidth)) then
            mhlw(1:gridDimCount) = totalLWidth(1:gridDimCount)
        endif
        mhuw = 0
        if(present(totalUWidth)) then
            mhuw(1:gridDimCount) = totalUWidth(1:gridDimCount)
        endif
        fsize = 10
        ! create a couple of mappings following index mapping lemma 1 and lemma 7
        do i = 1, gridDimCount
            dg2fm(i) = g2fm(dg2gm(i))
        enddo
        f2dgm = 0
        do i = 1, gridDimCount
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
!        write(*, "(A7, 17I3)") 'MZS: ', lpe, ec(1:3), cc(1:3), fsize(1:3), &
!            dg2fm(1:3), f2dgm(1:3), gridDimCount

        allocate(farray(fsize(1), fsize(2), fsize(3)))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=datacopyflag, &
            staggerloc=staggerloc,  gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)
    end subroutine test2e_generic

    ! test2e_generic provides an generic interface to test fieldCreateFromDataPtr
    ! based on 3d real type fortran array
    ! The fortran array shape is automatically computed from FieldGetAllocBounds
    subroutine test2f_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, intent(out)  :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_DataCopy_Flag),   optional   :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(3) :: lgridToFieldMap
        integer, dimension(3) :: lungriddedLBound 
        integer, dimension(3) :: lungriddedUBound 
        integer, dimension(3,1) :: ltotalLWidth
        integer, dimension(3,1) :: ltotalUWidth

        real, dimension(:,:,:), allocatable    :: farray
        type(ESMF_VM)                          :: vm
        integer                                :: lpe

        integer, dimension(3)                  :: fsize

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxindex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            totalCount=fsize, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
!        write(*, "(A7, 17I3)") 'MZS: ', lpe, ec(1:3), cc(1:3), fsize(1:3), &
!            dg2fm(1:3), f2dgm(1:3), gridDimCount

        allocate(farray(fsize(1), fsize(2), fsize(3)))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=datacopyflag, &
            staggerloc=staggerloc,  gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)
    end subroutine test2f_generic

    subroutine test3a(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:), allocatable  :: farray

        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(max(cc(1)+5, ec(1)), max(cc(2)+5, ec(2))))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            totalLWidth=(/2,2/), totalUWidth=(/3,3/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3a

    subroutine test3a_fptr(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real, dimension(:,:), pointer  :: farray
        real, dimension(:,:), pointer  :: farray1

        integer, dimension(2)               :: tlb, tub, felb, feub, fclb, fcub
        integer, dimension(2)               :: gelb, geub, gclb, gcub
        logical                             :: t
        integer                             :: i

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalLBound=tlb, totalUBound=tub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(tlb(1):tub(1), tlb(2):tub(2)))

        field = ESMF_FieldCreate(grid, farrayPtr=farray, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
            exclusiveLBound=felb, exclusiveUBound=feub, &
            computationalLBound=fclb, computationalUBound=fcub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! test pointer equivalence
        t = associated(farray, farray1)
        do i = 1, 2
            t = t .and. (lbound(farray, i) .eq. tlb(i))
            t = t .and. (ubound(farray, i) .eq. tub(i))
        enddo

        if(.not. t) then
          call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
            msg="- pointer queried from object is not equivalent to the one passed in)", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        ! test field and grid bounds
        call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
            exclusiveLBound=gelb, exclusiveUBound=geub, &
            computationalLBound=gclb, computationalUBound=gcub, &
            rc=localrc) 
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        t = .true.
        do i = 1, 2
            t = t .and. (gelb(i) .eq. felb(i))
            t = t .and. (geub(i) .eq. feub(i))
            t = t .and. (gclb(i) .eq. fclb(i))
            t = t .and. (gcub(i) .eq. fcub(i))
        enddo
        if(.not. t) then
          call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
            msg="- bounds queried from grid different from those queried from field)", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray1)

    end subroutine test3a_fptr

    subroutine test3a2_fptr(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        type(ESMF_DistGrid)     :: distgrid
        real, dimension(:,:), pointer  :: farray
        real, dimension(:,:), pointer  :: farray1

        integer, dimension(1)               :: tlb, tub
        integer, dimension(1)               :: gelb, geub, gclb, gcub
        integer, dimension(2)               :: felb, feub, fclb, fcub
        logical                             :: t
        integer                             :: i

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/20/), &
                                  regDecomp=(/4/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, &
                                  name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, totalLBound=tlb, totalUBound=tub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(tlb(1):tub(1), -3:3))

        field = ESMF_FieldCreate(grid, farrayPtr=farray, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
            exclusiveLBound=felb, exclusiveUBound=feub, &
            computationalLBound=fclb, computationalUBound=fcub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! test pointer equivalence
        t = associated(farray, farray1)
        do i = 1, 1
            t = t .and. (lbound(farray, i) .eq. tlb(i))
            t = t .and. (ubound(farray, i) .eq. tub(i))
        enddo

        if(.not. t) then
          call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
            msg="- pointer queried from object is not equivalent to the one passed in)", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

        ! test field and grid bounds
        call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
            exclusiveLBound=gelb, exclusiveUBound=geub, &
            computationalLBound=gclb, computationalUBound=gcub, &
            rc=localrc) 
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        t = .true.
        do i = 1, 1
            t = t .and. (gelb(i) .eq. felb(i))
            t = t .and. (geub(i) .eq. feub(i))
            t = t .and. (gclb(i) .eq. fclb(i))
            t = t .and. (gcub(i) .eq. fcub(i))
        enddo
        if(.not. t) then
          call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
            msg="- bounds queried from grid different from those queried from field)", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray1)

    end subroutine test3a2_fptr

    subroutine test3b(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)

        allocate(farray(max(cc(1)+14, ec(1)), max(cc(2)+16, ec(2))))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            !staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            !ungriddedLBound = (/2,3/), ungriddedUBound=(/4,5/), &
            totalLWidth=(/6,7/), totalUWidth=(/8,9/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3b

    subroutine test3b_fail(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        real, dimension(20,27)  :: farray
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            gridToFieldMap=(/2,1/), &
            totalLWidth=(/6,7/), totalUWidth=(/8,9/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    end subroutine test3b_fail

    subroutine test3c(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        !allocate(farray(max(cc(1)+14, ec(1)), max(cc(2)+16, ec(2))))
        allocate(farray(max(cc(2)+14, ec(2)), max(cc(1)+16, ec(1))))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            totalLWidth=(/6,7/), totalUWidth=(/8,9/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3c

    subroutine test3d(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(max(cc(1)+7, ec(1)), max(cc(2)+10, ec(2)), 10))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, & !gridToFieldMap = (/3,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            totalLWidth=(/2,4/), totalUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3d

    subroutine test3e(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(max(cc(2)+7, ec(2)), max(cc(1)+10, ec(1)), 10))
        !write(*, "(22I3)") lpe, ec(1:2), cc(1:2)

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            totalLWidth=(/2,4/), totalUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3e

    subroutine test3f(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(max(cc(1)+7, ec(1)), 10, max(cc(2)+10, ec(2))))
        !write(*, "(A7, 22I3)") 'MZS: ', lpe, ec(1:2), cc(1:2), max(cc(1)+7, ec(1)), max(cc(2)+10, ec(2))

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/1,3/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            totalLWidth=(/2,4/), totalUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3f

    subroutine test3g(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid

        real, dimension(:,:, :), allocatable  :: farray
        real, dimension(:,:, :), pointer  :: farray1
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe
        integer, dimension(2)             :: ec, cc
        integer, dimension(2)             :: gelb, geub, gclb, gcub
        type(ESMF_StaggerLoc)                       :: sloc

        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        sloc = ESMF_STAGGERLOC_CENTER
        call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(max(cc(2)+7, ec(2)), 10, max(cc(1)+10, ec(1))))
        !write(*, "(22I3)") lpe, ec(1:2), cc(1:2)

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/3,1/), &
            ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
            totalLWidth=(/2,4/), totalUWidth=(/5,6/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
            exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
            computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
            totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        !write(*, "(42I3)") lpe, felb, feub, fclb, fcub, ftlb, ftub
        
        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3g

    ! test3d_generic provides a generic interface to test fieldCreateFromDataPtr
    ! on 3d grid and 3d arrays
    subroutine test3d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget
        integer, intent(out)  :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        integer                 :: localrc, i, j, k

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(3) :: lgridToFieldMap
        integer, dimension(3) :: lungriddedLBound 
        integer, dimension(3) :: lungriddedUBound 
        integer, dimension(3,1) :: ltotalLWidth
        integer, dimension(3,1) :: ltotalUWidth

        integer, dimension(:,:,:), pointer  :: farray
        integer, dimension(:,:,:), pointer  :: farray1
        type(ESMF_VM)                               :: vm
        integer                                     :: lpe

        integer, dimension(3)             :: ec, cc, g2fm, mhlw, mhuw
        integer, dimension(3)             :: gelb, geub, gclb, gcub
        integer, dimension(3)             :: fsize
        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc
        integer                                     :: gridDimCount

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, maxIndex=maxIndex, &
                                  gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
                                  regDecomp=regDecomp, name="testgrid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, dimCount=gridDimCount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridGet(grid, localDe=0, staggerloc=staggerloc, &
           exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=ec,  &
           computationalLBound=gclb, computationalUBound=gcub, computationalCount=cc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(gridToFieldMap)) then
            g2fm(1:size(gridToFieldMap)) = gridToFieldMap
        else
            do i = 1, 3
                g2fm(i) = i
            enddo
        endif
        mhlw = 0
        if(present(totalLWidth)) then
            mhlw(1:gridDimCount) = totalLWidth(1:gridDimCount)
        endif
        mhuw = 0
        if(present(totalUWidth)) then
            mhuw(1:gridDimCount) = totalUWidth(1:gridDimCount)
        endif
        fsize=0
        do i = 1, 3
            !fsize(i) = max(cc(g2fm(i))+mhlw(i)+mhuw(i), ec(g2fm(i)))
            fsize(g2fm(i)) = max(cc(i)+mhlw(g2fm(i))+mhuw(g2fm(i)), ec(i))
        enddo

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_VMGet(vm, localPet=lpe, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
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

        field = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=datacopyflag, &
            staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !write(*, "(42I3)") felb, feub, fclb, fcub, ftlb, ftub
            do i = ftlb(1), ftub(1)
                do j = ftlb(2), ftub(2)
                    do k = ftlb(3), ftub(3)
                        if(farray1(i, j, k) .ne. ((i-ftlb(1)+1)+(j-ftlb(2)+1)*2) ) &
                            localrc = ESMF_FAILURE
                    enddo
                enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        deallocate(farray)

    end subroutine test3d_generic

#ifndef ESMF_NO_GREATER_THAN_4D
    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetAllocBounds
    subroutine test7d1(rc)
        integer, intent(out) :: rc

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: f8
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: fsize(7)

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

! Cannot use the following numbers, cause overflow on most systems due to memory requirement
!        call ESMF_GridGetFieldBounds(grid, ungriddedLBound=(/1,2/), ungriddedUBound=(/10,23/), &
!            totalLWidth=(/2,3,4,5,6/), totalUWidth=(/10,11,12,13,14/), &
!            totalCount=fsize, &
!            rc=localrc)
        call ESMF_GridGetFieldBounds(grid, localDe=0, ungriddedLBound=(/1,2/), &
            ungriddedUBound=(/2,3/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,2,1/), &
            totalCount=fsize, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(fsize(1), fsize(2), fsize(3), fsize(4), fsize(5), fsize(6), fsize(7)))

        f8 = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/2,3/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,2,1/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(f8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        deallocate(farray)
    end subroutine test7d1

    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetAllocBounds
    subroutine test7d2(rc)
        integer, intent(out) :: rc

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: f8
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: flb(7), fub(7)

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1,1/), maxIndex=(/3,4,3,4,2/), &
            regDecomp=(/2,1,2,1,1/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, ungriddedLBound=(/1,2/), &
            ungriddedUBound=(/2,3/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,2,1/), &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )

        f8 = ESMF_FieldCreate(grid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, &
            ungriddedLBound=(/1,2/), ungriddedUBound=(/2,3/), &
            totalLWidth=(/1,1,1,2,2/), totalUWidth=(/1,2,3,2,1/), &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(f8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
            
        deallocate(farray)
    end subroutine test7d2

    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetAllocBounds
    subroutine test7d_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: fsize(7)

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        integer                                     :: ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, staggerloc=staggerloc, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalCount=fsize, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(fsize(1), fsize(2), fsize(3), fsize(4), fsize(5), fsize(6), fsize(7)))
        !write(*, '(7I4)') fsize

        if(present(fieldget)) then
          if(fieldget) then
            ! reverse looping order to make this a little faster by improving data locality
            do ip = 1, fsize(7)
             do io = 1, fsize(6)
              do im = 1, fsize(5)
               do il = 1, fsize(4)
                do ik = 1, fsize(3)
                 do ij = 1, fsize(2)
                  do ii = 1, fsize(1)
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
            indexflag=ESMF_INDEX_DELOCAL, &
            datacopyflag=datacopyflag, &
            staggerloc=staggerloc, &
            gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !write(*, "(A5, 42I3)") 'MZY: ', felb, feub, fclb, fcub, ftlb, ftub
            ! reverse looping order to make this a little faster by improving data locality
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
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
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray)
    end subroutine test7d_generic
!------------------------------------------------------------------------
    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetAllocBounds
    ! use allocBounds to verify field create
    subroutine test7d2_generic_fptr(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_StaggerLoc), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: flb(7), fub(7)

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        integer                                     :: i, ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        integer, dimension(7)                       :: gclb, gcub, gcc, gelb, geub, gec
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        logical                                     :: t
        type(ESMF_STAGGERLOC)                       :: localStaggerLoc

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
        nullify(farray1)

        if(present(staggerloc)) then
            localStaggerLoc=staggerloc
        else
            localStaggerLoc=ESMF_STAGGERLOC_CENTER
        endif

        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, localDe=0, staggerloc=localStaggerLoc, &
            exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=gec, &
            computationalLBound=gclb, computationalUBound=gcub, computationalCount=gcc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=localStaggerLoc, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )

        if(present(fieldget)) then
          if(fieldget) then
            ! reverse looping order to make this a little faster by improving data locality
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
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
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            datacopyflag=datacopyflag, &
            staggerloc=localStaggerLoc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! test pointer equivalence
            t = associated(farray, farray1)
            do i = 1, 7
                t = t .and. (lbound(farray1, i) .eq. flb(i)) .and. (ubound(farray1, i) .eq. fub(i))
            enddo

            if(present(datacopyflag)) then
                if(datacopyflag==ESMF_DATACOPY_VALUE) t = .true.
            endif

            if(.not. t) then
              call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
                msg="- pointer queried from object is not equivalent to the one passed in)", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
            endif

            ! test field and grid bounds
            call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
                exclusiveLBound=gelb, exclusiveUBound=geub, &
                computationalLBound=gclb, computationalUBound=gcub, &
                rc=localrc) 
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            t = .true.
            do i = 1, 2
                t = t .and. (gelb(i) .eq. felb(i))
                t = t .and. (geub(i) .eq. feub(i))
                t = t .and. (gclb(i) .eq. fclb(i))
                t = t .and. (gcub(i) .eq. fcub(i))
            enddo
    
            if(.not. t) then
              call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
                msg="- bounds queried from grid different from those queried from field)", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
            endif

            ! reverse looping order to make this a little faster by improving data locality
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        if(associated(farray1, farray)) then
            deallocate(farray1)
        else
            deallocate(farray)
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test7d2_generic_fptr
!------------------------------------------------------------------------
    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetAllocBounds
    ! use allocBounds to verify field create
    subroutine test7d3_generic_fptr(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_StaggerLoc), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: flb(7), fub(7)

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        integer                                     :: i, ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        integer, dimension(7)                       :: gclb, gcub, gcc, gelb, geub, gec
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        logical                                     :: t
        type(ESMF_STAGGERLOC)                       :: localStaggerLoc

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
        nullify(farray1)

        if(present(staggerloc)) then
            localStaggerLoc=staggerloc
        else
            localStaggerLoc=ESMF_STAGGERLOC_CENTER
        endif

        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGet(grid, localDe=0, staggerloc=localStaggerLoc, &
            exclusiveLBound=gelb, exclusiveUBound=geub, exclusiveCount=gec, &
            computationalLBound=gclb, computationalUBound=gcub, computationalCount=gcc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=localStaggerLoc, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )

        if(present(fieldget)) then
          if(fieldget) then
            ! reverse looping order to make this a little faster by improving data locality
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
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

        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldEmptyComplete(field, grid, farray, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            datacopyflag=datacopyflag, &
            staggerloc=localStaggerLoc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! test pointer equivalence
            t = associated(farray, farray1)
            do i = 1, 7
                t = t .and. (lbound(farray1, i) .eq. flb(i)) .and. (ubound(farray1, i) .eq. fub(i))
            enddo

            if(present(datacopyflag)) then
                if(datacopyflag==ESMF_DATACOPY_VALUE) t = .true.
            endif

            if(.not. t) then
              call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
                msg="- pointer queried from object is not equivalent to the one passed in)", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
            endif

            ! test field and grid bounds
            call ESMF_GridGet(grid, localDe=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
                exclusiveLBound=gelb, exclusiveUBound=geub, &
                computationalLBound=gclb, computationalUBound=gcub, &
                rc=localrc) 
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            t = .true.
            do i = 1, 2
                t = t .and. (gelb(i) .eq. felb(i))
                t = t .and. (geub(i) .eq. feub(i))
                t = t .and. (gclb(i) .eq. fclb(i))
                t = t .and. (gcub(i) .eq. fcub(i))
            enddo
    
            if(.not. t) then
              call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
                msg="- bounds queried from grid different from those queried from field)", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
            endif

            ! reverse looping order to make this a little faster by improving data locality
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        if(associated(farray1, farray)) then
            deallocate(farray1)
        else
            deallocate(farray)
        endif

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test7d3_generic_fptr

    ! create a 7d Field from 5d grid and 2d ungridded bounds using ESMF_FieldGetAllocBounds
    ! use allocBounds to verify field create
    subroutine test7d2_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), allocatable :: farray
        type(ESMF_Field)    :: field, dstField
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: flb(7), fub(7)

        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        integer                                     :: ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, staggerloc=staggerloc, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(farray(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )

        if(present(fieldget)) then
          if(fieldget) then
            ! reverse looping order to make this a little faster by improving data locality
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
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

        field = ESMF_FieldCreate(grid, farray=farray, indexflag=ESMF_INDEX_DELOCAL, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            datacopyflag=datacopyflag, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        dstField = ESMF_FieldCreate(grid, farray=farray, indexflag=ESMF_INDEX_DELOCAL, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            datacopyflag=ESMF_DATACOPY_VALUE, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_FieldGet(dstField, localDe=0, farrayPtr=farray1, &
          totalLBound=ftlb, totalUBound=ftub, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        farray1 = 0.

        call ESMF_FieldCopy(dstField, field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
  
        ! Verify fieldCopy copies the values correctly
        if(present(fieldget)) then
          if(fieldget) then
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) /= n) then
                      call ESMF_LogSetError(ESMF_RC_PTR_BAD, &
                        msg="- dst Field pointer value is different from src Field value", &
                        ESMF_CONTEXT, rcToReturn=rc)
                      return
                    endif
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
          endif
        endif

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! reverse looping order to make this a little faster by improving data locality
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray)
    end subroutine test7d2_generic

    ! create a 7d Field using grid and arrayspec
    ! use allocBounds to verify field create
    subroutine test7d3_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_ArraySpec):: arrayspec
        integer             :: localrc
        integer             :: flb(7), fub(7)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(7,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create arrayspec
        call ESMF_ArraySpecSet(arrayspec, 7, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldCreate(grid, arrayspec, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return


        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            allocate(audlb(arank-adimCount), audub(arank-adimCount))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return

            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 7
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
                    farray(ii,ij,ik,il,im,io,ip) = ii+ij*2+ik+il*2+im+io*2+ip
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo

            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test7d3_generic

!------------------------------------------------------------------------
    ! create a 7d Field using grid and arrayspec
    ! use allocBounds to verify field create
    subroutine test7d3_generic_repdim(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_ArraySpec):: arrayspec
        integer             :: localrc
        integer             :: flb(7), fub(7)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount, gridrank_repdim
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(7,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create arrayspec
        call ESMF_ArraySpecSet(arrayspec, 7, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldCreate(grid, arrayspec, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            gridrank_repdim = 0
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
                enddo
            endif
            allocate(audlb(arank-adimCount+gridrank_repdim), audub(arank-adimCount+gridrank_repdim))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return
            
            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 7
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
                    farray(ii,ij,ik,il,im,io,ip) = ii+ij*2+ik+il*2+im+io*2+ip
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test7d3_generic_repdim
#endif
!------------------------------------------------------------------------
    ! create a 3d Field using grid and arrayspec
    ! use allocBounds to verify field create
    subroutine test3d_generic_repdim(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_ArraySpec):: arrayspec
        integer             :: localrc
        integer             :: flb(3), fub(3)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount, gridrank_repdim
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik
        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(3,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create arrayspec
        call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldCreate(grid, arrayspec, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            gridrank_repdim = 0
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
                enddo
            endif
            allocate(audlb(arank-adimCount+gridrank_repdim), audub(arank-adimCount+gridrank_repdim))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return
            
            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 3
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ik = flb(3), fub(3)
             do ij = flb(2), fub(2)
              do ii = flb(1), fub(1)
                farray(ii,ij,ik) = ii+ij*2+ik
              enddo
             enddo
            enddo
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ik = ftlb(3), ftub(3)
             do ij = ftlb(2), ftub(2)
              do ii = ftlb(1), ftub(1)
                n = ii+ij*2+ik
                if(farray1(ii,ij,ik) .ne. n ) localrc = ESMF_FAILURE
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test3d_generic_repdim

#ifndef ESMF_NO_GREATER_THAN_4D
!------------------------------------------------------------------------
    ! create a 7d Field using grid and arrayspec
    ! use allocBounds to verify field create
    subroutine test7d3_generic_repdim_sct(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field, field1
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_ArraySpec):: arrayspec
        integer             :: localrc
        integer             :: flb(7), fub(7)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount, gridrank_repdim
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), allocatable :: farray_cr
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(7,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create arrayspec
        call ESMF_ArraySpecSet(arrayspec, 7, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
        allocate(farray_cr(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )
        call ESMF_FieldEmptyComplete(field, grid, farray_cr, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field1 = ESMF_FieldEmptyCreate(name="field1", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldEmptySet(field1, grid=grid, staggerloc=staggerloc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldEmptyComplete(field1, farray_cr, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            gridrank_repdim = 0
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
                enddo
            endif
            allocate(audlb(arank-adimCount+gridrank_repdim), audub(arank-adimCount+gridrank_repdim))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return
            
            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 7
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
                    farray(ii,ij,ik,il,im,io,ip) = ii+ij*2+ik+il*2+im+io*2+ip
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray_cr)
    end subroutine test7d3_generic_repdim_sct
#endif

!------------------------------------------------------------------------
    ! create a 3d Field using grid and arrayspec
    ! use allocBounds to verify field create
    subroutine test3d_generic_repdim_sct(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_ArraySpec):: arrayspec
        integer             :: localrc
        integer             :: flb(3), fub(3)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount, gridrank_repdim
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik
        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:), allocatable :: farray_cr
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(3,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create arrayspec
        call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
        allocate(farray_cr(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3)))
        call ESMF_FieldEmptyComplete(field, grid, farray_cr, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            gridrank_repdim = 0
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
                enddo
            endif
            allocate(audlb(arank-adimCount+gridrank_repdim), audub(arank-adimCount+gridrank_repdim))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return
            
            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 3
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ik = flb(3), fub(3)
             do ij = flb(2), fub(2)
              do ii = flb(1), fub(1)
                farray(ii,ij,ik) = ii+ij*2+ik
              enddo
             enddo
            enddo
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ik = ftlb(3), ftub(3)
             do ij = ftlb(2), ftub(2)
              do ii = ftlb(1), ftub(1)
                n = ii+ij*2+ik
                if(farray1(ii,ij,ik) .ne. n ) localrc = ESMF_FAILURE
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray_cr)
    end subroutine test3d_generic_repdim_sct

!------------------------------------------------------------------------
    ! create a 3d Field using grid
    ! use allocBounds to verify field create
    subroutine test3d_generic_sctptr(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field, field1
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_ArraySpec):: arrayspec
        integer             :: localrc
        integer             :: flb(3), fub(3)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount, gridrank_repdim
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(3) :: lgridToFieldMap
        integer, dimension(3) :: lungriddedLBound 
        integer, dimension(3) :: lungriddedUBound 
        integer, dimension(3,1) :: ltotalLWidth
        integer, dimension(3,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik
        integer, dimension(3)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(3)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray_cr
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(3,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create arrayspec
        call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
        allocate(farray_cr(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3)))
        call ESMF_FieldEmptyComplete(field, grid, farray_cr, gridToFieldMap=gridToFieldMap, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field1 = ESMF_FieldEmptyCreate(name="field1", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldEmptySet(field1, grid=grid, staggerloc=staggerloc, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldEmptyComplete(field1, farray_cr, &
            gridToFieldMap=gridToFieldMap, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            gridrank_repdim = 0
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
                enddo
            endif
            allocate(audlb(arank-adimCount+gridrank_repdim), audub(arank-adimCount+gridrank_repdim))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return
            
            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 3
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ik = flb(3), fub(3)
             do ij = flb(2), fub(2)
              do ii = flb(1), fub(1)
                farray(ii,ij,ik) = ii+ij*2+ik
              enddo
             enddo
            enddo
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ik = ftlb(3), ftub(3)
             do ij = ftlb(2), ftub(2)
              do ii = ftlb(1), ftub(1)
                n = ii+ij*2+ik
                if(farray1(ii,ij,ik) .ne. n ) localrc = ESMF_FAILURE
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray_cr)
    end subroutine test3d_generic_sctptr

!------------------------------------------------------------------------
    ! create a 3d Field using grid
    ! use allocBounds to verify field create
    subroutine test2d_generic_sctptr(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        staggerloc, &
        gridToFieldMap, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        integer             :: localrc
        integer             :: flb(2), fub(2)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1
        type(ESMF_Array)        :: array
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount, gridrank_repdim
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(2) :: lgridToFieldMap
        integer, dimension(2) :: lungriddedLBound 
        integer, dimension(2) :: lungriddedUBound 
        integer, dimension(2,1) :: ltotalLWidth
        integer, dimension(2,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij
        integer, dimension(2)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(2)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray_cr
        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray
        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(2,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
    
        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
        allocate(farray_cr(flb(1):fub(1), flb(2):fub(2)))
        call ESMF_FieldEmptyComplete(field, grid, farray_cr, gridToFieldMap=gridToFieldMap, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            gridrank_repdim = 0
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(gridToFieldMap(i) == 0) gridrank_repdim = gridrank_repdim + 1
                enddo
            endif
            allocate(audlb(arank-adimCount+gridrank_repdim), audub(arank-adimCount+gridrank_repdim))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return
            
            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 2
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the data in the created Field can be accessed, updated and verified
            ! access and update
            do ij = flb(2), fub(2)
             do ii = flb(1), fub(1)
               farray(ii,ij) = ii+ij*2
             enddo
            enddo
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ij = ftlb(2), ftub(2)
             do ii = ftlb(1), ftub(1)
               n = ii+ij*2
               if(farray1(ii,ij) .ne. n ) localrc = ESMF_FAILURE
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray_cr)
    end subroutine test2d_generic_sctptr

#ifndef ESMF_NO_GREATER_THAN_4D
!------------------------------------------------------------------------
    ! create a 7d Field using grid and array
    ! use allocBounds to verify field create
    subroutine test7d4_generic(rc, minindex, maxindex, &
        gridEdgeLWidth, gridEdgeUWidth, &
        regDecomp, &
        distgridToGridMap, &
        datacopyflag, &
        staggerloc, &
        gridToFieldMap, &
        ungriddedLBound, ungriddedUBound, &
        totalLWidth, totalUWidth, &
        fieldget)

        ! input arguments
        integer, intent(out) :: rc
        integer, dimension(:)   :: minIndex
        integer, dimension(:)   :: maxIndex
        integer, dimension(:), optional   :: gridEdgeLWidth, gridEdgeUWidth
        integer, dimension(:), optional   :: regDecomp
        integer, dimension(:), optional   :: distgridToGridMap
        type(ESMF_DataCopy_Flag), optional     :: datacopyflag
        type(ESMF_STAGGERLOC), optional   :: staggerloc
        integer, dimension(:), optional   :: gridToFieldMap
        integer, dimension(:), optional   :: ungriddedLBound, ungriddedUBound
        integer, dimension(:), optional   :: totalLWidth, totalUWidth
        logical, optional                 :: fieldget

        ! local arguments used to create field
        type(ESMF_Field)    :: field
        type(ESMF_Array)    :: array
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_DistGrid) :: staggerDistgrid
        integer             :: localrc
        integer             :: flb(7), fub(7)

        ! local arguments used to get info from field
        type(ESMF_Grid)         :: grid1, grid2
        type(ESMF_Array)        :: array1, array2
        type(ESMF_TypeKind_Flag)     :: typekind
        integer                 :: dimCount
        type(ESMF_StaggerLoc)   :: lstaggerloc
        integer, dimension(7) :: lgridToFieldMap
        integer, dimension(7) :: lungriddedLBound 
        integer, dimension(7) :: lungriddedUBound 
        integer, dimension(7,1) :: ltotalLWidth
        integer, dimension(7,1) :: ltotalUWidth

        ! local arguments used to verify field get
        integer                                     :: i, ii, ij, ik, il, im, io, ip
        integer, dimension(7)                       :: felb, feub, fclb, fcub, ftlb, ftub
        integer, dimension(7)                       :: fec, fcc, ftc
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), allocatable :: farray
        real(ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: farray1
        real(ESMF_KIND_R8)                          :: n
        integer, dimension(7,1)                     :: aelb, aeub, aclb, acub, atlb, atub
        integer                                     :: ldec, ldel(1)
        integer, dimension(:), allocatable          :: audlb, audub
        integer                                     :: arank, adimCount, gdimCount
        integer, dimension(:), allocatable          :: l_g2fm, l_dg2gm, distgridToArrayMap
        integer, dimension(:), allocatable          :: l_mhlw, l_mhuw, celw, ceuw

        type(ESMF_Field)                            :: field1
        character, pointer                          :: buffer(:)
        integer                                     :: buff_length, offset

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS

        ! create distgrid
        distgrid = ESMF_DistGridCreate(minIndex=minIndex, maxIndex=maxIndex, &
            regDecomp=regDecomp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create grid
        grid = ESMF_GridCreate(distgrid=distgrid, name="grid", &
            distgridToGridMap=distgridToGridMap, &
            gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! get distgrid for staggerloc
        if (present(staggerloc)) then
           call ESMF_GridGet(grid,staggerloc, &
                  distgrid=staggerdistgrid,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
        else 
           call ESMF_GridGet(grid,ESMF_STAGGERLOC_CENTER, &
                  distgrid=staggerdistgrid,rc=localrc)
           if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
        endif


        call ESMF_GridGetFieldBounds(grid, localDe=0, &
            staggerloc=staggerloc,&
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            gridToFieldMap=gridToFieldMap, &
            totalLBound=flb, totalUBound=fub, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! prepare input to ESMF_ArrayCreate
        ! array pointer
        allocate(farray(flb(1):fub(1), flb(2):fub(2), flb(3):fub(3), &
            flb(4):fub(4), flb(5):fub(5), flb(6):fub(6), flb(7):fub(7)) )

        if(present(fieldget)) then
          if(fieldget) then
            ! reverse looping order to make this a little faster by improving data locality
            do ip = flb(7), fub(7)
             do io = flb(6), fub(6)
              do im = flb(5), fub(5)
               do il = flb(4), fub(4)
                do ik = flb(3), fub(3)
                 do ij = flb(2), fub(2)
                  do ii = flb(1), fub(1)
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

        gdimCount = size(minIndex, 1)
        allocate( distgridToArrayMap(gdimCount), l_g2fm(gdimCount), l_dg2gm(gdimCount) )
        allocate( l_mhlw(gdimCount), l_mhuw(gdimCount), celw(gdimCount), ceuw(gdimCount) )
    
        if(present(gridToFieldMap)) then
            l_g2fm(1:gdimCount) = gridToFieldMap(1:gdimCount)
        else
            do i = 1, gdimCount
                l_g2fm(i) = i
            enddo
        endif

        if(present(distgridToGridMap)) then
            l_dg2gm(1:gdimCount) = distgridToGridMap(1:gdimCount)
        else
            do i = 1, gdimCount
                l_dg2gm(i) = i
            enddo
        endif

        if(present(totalLWidth)) then
            l_mhlw(1:gdimCount) = totalLWidth(1:gdimCount)
        else
            l_mhlw = 0
        endif
        if(present(totalUWidth)) then
            l_mhuw(1:gdimCount) = totalUWidth(1:gdimCount)
        else
            l_mhuw = 0
        endif

        ! hardcode these, or call GridGetUndistInfo
        celw = 0
        ceuw = -1

        do i = 1, gdimCount
            distgridToArrayMap(i) = l_g2fm(l_dg2gm(i))
        enddo
        ! create array
        array = ESMF_ArrayCreate(staggerdistgrid, farray, &
            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=datacopyflag, &
            distgridToArrayMap=distgridToArrayMap, &
            undistLBound=ungriddedLBound, undistUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, & 
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! create field
        field = ESMF_FieldCreate(grid, array, datacopyflag=datacopyflag, &
            gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
            staggerloc=staggerloc, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        if(present(fieldget)) then
          if(fieldget) then
            ! verify FieldGetDataBounds are correct
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                exclusiveLBound=felb, exclusiveUBound=feub, exclusiveCount=fec, &
                computationalLBound=fclb, computationalUBound=fcub, computationalCount=fcc, &
                totalLBound=ftlb, totalUBound=ftub, totalCount=ftc, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(field, grid=grid1, array=array1, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify parameters from getdefault are correct
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(lgridToFieldMap(i) .ne. gridToFieldMap(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(ungriddedLBound)) then
                do i = 1, size(ungriddedLBound)
                    if(lungriddedLBound(i) .ne. ungriddedLBound(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(ungriddedUBound)) then
                do i = 1, size(ungriddedUBound)
                    if(lungriddedUBound(i) .ne. ungriddedUBound(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(totalLWidth)) then
                do i = 1, size(totalLWidth)
                    if(ltotalLWidth(i,1) .ne. totalLWidth(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(totalUWidth)) then
                do i = 1, size(totalUWidth)
                    if(ltotalUWidth(i,1) .ne. totalUWidth(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif

            ! Allocate serialization buffer

            buff_length = 1
            allocate (buffer(buff_length))
            offset = 0
            call ESMF_FieldSerialize(field, buffer, buff_length, offset, &
                inquireflag=ESMF_INQUIREONLY, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate (buffer)

            buff_length = offset
            allocate (buffer(buff_length))

            ! call serialize and deserialize and verify again

            offset = 0
            call ESMF_FieldSerialize(field, buffer, buff_length, offset, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            offset = 0

            field1 = ESMF_FieldDeserialize(buffer, offset, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            deallocate (buffer)

            call ESMF_FieldValidate(field1, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldGet(field1, grid=grid2, array=array2, typekind=typekind, &
                dimCount=dimCount, staggerloc=lstaggerloc, gridToFieldMap=lgridToFieldMap, &
                ungriddedLBound=lungriddedLBound, ungriddedUBound=lungriddedUBound, &
                totalLWidth=ltotalLWidth, totalUWidth=ltotalUWidth, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! verify parameters from getdefault are correct from a deseriailzed field
            if(present(gridToFieldMap)) then
                do i = 1, size(gridToFieldMap)
                    if(lgridToFieldMap(i) .ne. gridToFieldMap(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(ungriddedLBound)) then
                do i = 1, size(ungriddedLBound)
                    if(lungriddedLBound(i) .ne. ungriddedLBound(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(ungriddedUBound)) then
                do i = 1, size(ungriddedUBound)
                    if(lungriddedUBound(i) .ne. ungriddedUBound(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(totalLWidth)) then
                do i = 1, size(totalLWidth)
                    if(ltotalLWidth(i,1) .ne. totalLWidth(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif
            if(present(totalUWidth)) then
                do i = 1, size(totalUWidth)
                    if(ltotalUWidth(i,1) .ne. totalUWidth(i)) localrc = ESMF_FAILURE
                enddo
                if (ESMF_LogFoundError(localrc, &
                    ESMF_ERR_PASSTHRU, &
                    ESMF_CONTEXT, rcToReturn=rc)) return
            endif

            ! verify that the field and array bounds agree with each other
            call ESMF_ArrayGet(array, rank=arank, dimCount=adimCount, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            allocate(audlb(arank-adimCount), audub(arank-adimCount))
            call ESMF_ArrayGet(array, exclusiveLBound=aelb, exclusiveUBound=aeub, &
                computationalLBound=aclb, computationalUBound=acub, &
                totalLBound=atlb, totalUBound=atub, &
                localDeCount=ldec, localDeToDeMap=ldel, &
                undistLBound=audlb, undistUBound=audub, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! verify the numbers returned are correct
            if(ldec .ne. 1)  localrc = ESMF_FAILURE
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            !ldel(1) is PET dependent 
            !if(ldel(1) .ne. 0) localrc = ESMF_FAILURE
            !if (ESMF_LogFoundError(localrc, &
            !    ESMF_ERR_PASSTHRU, &
            !    ESMF_CONTEXT, rcToReturn=rc)) return

            do i = 1, arank-adimCount
                if(lungriddedLBound(i) .ne. audlb(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do i = 1, arank-adimCount
                if(lungriddedUBound(i) .ne. audub(i) ) &
                    localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            ! compare the total bounds computed from FieldGetAllocBounds and FieldGetDataBounds
            do i = 1, 7
                if( (ftlb(i) .ne. flb(i)) .or. (ftub(i) .ne. fub(i)) ) &
                        localrc = ESMF_FAILURE
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            ! access and verify
            call ESMF_FieldGet(field, localDe=0, farrayPtr=farray1, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            do ip = ftlb(7), ftub(7)
             do io = ftlb(6), ftub(6)
              do im = ftlb(5), ftub(5)
               do il = ftlb(4), ftub(4)
                do ik = ftlb(3), ftub(3)
                 do ij = ftlb(2), ftub(2)
                  do ii = ftlb(1), ftub(1)
                    n = ii+ij*2+ik+il*2+im+io*2+ip
                    if(farray1(ii,ij,ik,il,im,io,ip) .ne. n ) localrc = ESMF_FAILURE
                  enddo
                 enddo
                enddo
               enddo
              enddo
             enddo
            enddo
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          endif ! fieldget = .true.
        endif ! present(fieldget) = .true.

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(farray)

    end subroutine test7d4_generic
#endif

!----------------------------------------------------------------------------------

    subroutine test_atnas_gridindex(rc)


    
    ! Arguments
    integer, intent(OUT) :: RC

    ! Local variables

    type(ESMF_VM)                   :: vm
    type(ESMF_Grid)                 :: grid
    type(ESMF_DistGrid)             :: distgrid
    type(ESMF_Field)                :: field, f
    type (ESMF_Array)               :: array
    integer                         :: counts(7)
    integer                         :: npets
    integer                         :: myid
    integer                         :: i, j, ntiles, extra
    integer                         :: fieldRank, gridRank
    integer, allocatable            :: tileIndex(:)
    integer, allocatable            :: gridToFieldMap(:)
    real(kind=ESMF_KIND_R4), pointer:: VAR_1D(:)
    integer                         :: arbIndexCount
    integer, allocatable            :: arbIndex(:,:)
    integer, parameter              :: ntilesGlobal = 100

    type(ESMF_INDEX_FLAG)           :: idx

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_VmGet(VM, petCount=npets, localPet=myid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
  ! set up abritrary index 
  ! for this example it won't be arbitrary
  ! but it does not matter. The real usage is for MAPL_LocStream

    J = 0

    extra = 1
    if(mod(ntilesGlobal,npets) <= myid) extra=0
    ntiles = ntilesGlobal / npets + extra

    allocate(tileIndex(ntiles), stat=rc)
    do I = 1, ntilesGlobal
       if (mod(I-1,npets) == myid) then
          J = J + 1
          tileIndex(J) = I
       end if
    end do
    if(ntiles /= J) then
      rc = ESMF_FAILURE
      return
    endif

  ! the actual preproducer

  ! create distGrid for arbSeqIndexList
    distgrid=ESMF_DistGridCreate(arbSeqIndexList=tileIndex, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! create grid for the distgrid
  ! IMPORTANT the indexFlag has to be ESMF_INDEX_USER

    GRID = ESMF_GridEmptyCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
           
    arbIndexCount = ntiles
    allocate(arbIndex(arbIndexCount,1), stat=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    arbIndex(:,1) = tileIndex
    deallocate(tileIndex)

    call ESMF_GridSet(grid,  &
         name="tile_grid",    &
         distgrid=distgrid, & 
         gridMemLBound=(/1/), &
         indexFlag=ESMF_INDEX_USER, &
         distDim = (/1/), &
         localArbIndexCount=arbIndexCount, &
         localArbIndex=arbIndex, &
         minIndex=(/1/), &
         maxIndex=(/ntilesGlobal/), &
         rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(arbIndex)
    call ESMF_GridCommit(grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_GridGet(GRID, dimCount=gridRank, indexflag=idx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    idx = ESMF_INDEX_USER


    call ESMF_GridGet(GRID, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         computationalCount=COUNTS, RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(VAR_1D(counts(1)), stat=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! create a field
    FIELD = ESMF_FieldCreate(GRID, farrayPtr=VAR_1D,    &
         name = "tilevar1D",                         &
         datacopyFlag = ESMF_DATACOPY_REFERENCE,     &
         rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  !    print *,'PE ',myid,' returned from FieldCreateF90',rc

  ! Now we query the field and get the grid and the esmf array
  ! then we want to create a new field (possibly different name)
  ! and use the same grid and esmf array
    call ESMF_FieldGet(FIELD, grid=GRID, dimCount=fieldRank, RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_GridGet(GRID, dimCount=gridRank, indexflag=idx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    if(fieldRank /= gridRank) then
      rc = ESMF_FAILURE
      return
    endif
    allocate(gridToFieldMap(gridRank), stat=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_FieldGet(FIELD, Array=Array, gridToFieldMap=gridToFieldMap, RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! this call fails
  ! the reason is that ESMF "thinks" gridIndex is different from arrayIndex
  !
    F = ESMF_FieldCreate(GRID, ARRAY = Array, &
         name="newName", gridToFieldMap=gridToFieldMap, RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(gridToFieldMap)
    
    end subroutine
  
!----------------------------------------------------------------------------------
    subroutine test_eric_klusek(rc)

        integer, intent(out)                :: rc

        type(ESMF_Array)                    :: array, o_array
        type(ESMF_Field)                    :: field
        type(ESMF_ArraySpec)                :: arrayspec
        type(ESMF_DistGrid)                 :: distgrid
        type(ESMF_Grid)                     :: grid
        integer(ESMF_KIND_I4), pointer      :: fptr(:,:,:)
        integer                             :: localrc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        call ESMF_ArraySpecSet(arrayspec, rank=3, typekind=ESMF_TYPEKIND_I4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/10, 10/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        array = ESMF_ArrayCreate(distgrid, arrayspec, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
        grid = ESMF_GridCreate(distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, array, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        call ESMF_ArrayDestroy(array, rc=localrc) 
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldGet(field, array=o_array, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_ArrayGet(array, localDe=0, farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test_eric_klusek

!----------------------------------------------------------------------------------
    subroutine test_allrep1(rc)

        integer, intent(out)                :: rc

        real, allocatable                   :: fa(:)
        type(ESMF_Field)                    :: field, field1
        type(ESMF_DistGrid)                 :: distgrid
        type(ESMF_Grid)                     :: grid
        integer                             :: localrc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/16, 16/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        grid = ESMF_GridCreate(distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(fa(4))
        call ESMF_FieldEmptyComplete(field, grid, fa, gridToFieldMap=(/0,0/), indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field1 = ESMF_FieldCreate(grid, fa, gridToFieldMap=(/0,0/), & 
          indexflag=ESMF_INDEX_DELOCAL, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(fa)

    end subroutine test_allrep1
!----------------------------------------------------------------------------------
    subroutine test_allrep2(rc)

        integer, intent(out)                :: rc

        real, pointer                       :: fa(:)
        type(ESMF_Field)                    :: field, field1
        type(ESMF_DistGrid)                 :: distgrid
        type(ESMF_Grid)                     :: grid
        integer                             :: localrc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/16, 16/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        grid = ESMF_GridCreate(distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldEmptyCreate(name="field", rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        allocate(fa(4))
        call ESMF_FieldEmptyComplete(field, grid, fa, gridToFieldMap=(/0,0/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field1 = ESMF_FieldCreate(grid, fa, gridToFieldMap=(/0,0/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_FieldDestroy(field, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_GridDestroy(grid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_DistGridDestroy(distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        deallocate(fa)

    end subroutine test_allrep2

!----------------------------------------------------------------------------------
    subroutine test_uninit_array(rc)

        integer, intent(out)                :: rc

        type(ESMF_Array)                    :: array
        type(ESMF_Field)                    :: field
        type(ESMF_DistGrid)                 :: distgrid
        type(ESMF_Grid)                     :: grid
        integer                             :: localrc

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS

        distgrid = ESMF_DistGridCreate(minIndex=(/1, 1/), maxIndex=(/10, 10/), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        
        grid = ESMF_GridCreate(distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        field = ESMF_FieldCreate(grid, array, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    end subroutine test_uninit_array


    subroutine test_globalindex(rc)
        integer, intent(out)  :: rc
        integer                 :: localrc
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: grid
        real (ESMF_KIND_R8), pointer   :: farray(:,:)
        type(ESMF_VM)                               :: vm
        integer                                     :: localPet, petCount
        integer                                     :: compLBnd(2), compUBnd(2)
        type(ESMF_ArraySpec)                        :: arrayspec
        logical                   :: correct

        rc = ESMF_SUCCESS
        localrc = ESMF_SUCCESS
        correct=.true.

        call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

        ! only do this if there is 4 Pets
        if (petCount .eq. 4) then
           ! create grid with global indices
           grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
                                  regDecomp=(/2,2/), indexflag=ESMF_INDEX_GLOBAL , rc=localrc)
           if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

            ! set arrayspec
            call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=localrc)
           if (ESMF_LogFoundError(localrc, &
               ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

            ! create field on grid
            field = ESMF_FieldCreate(grid, arrayspec, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
       
            ! Get field bounds
            call ESMF_FieldGet(field, localde=0, farrayPtr=farray, &
                computationalLBound=compLBnd, computationalUBound=compUBnd, &
                rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
           
            ! check bounds
            if (localpet .eq. 0) then
               if (compLBnd(1) .ne. 1) correct=.false.
               if (compLBnd(2) .ne. 1) correct=.false.
               if (compUBnd(1) .ne. 8) correct=.false.
               if (compUBnd(2) .ne. 10) correct=.false.
            else if (localpet .eq. 1) then
               if (compLBnd(1) .ne. 9) correct=.false.
               if (compLBnd(2) .ne. 1) correct=.false.
               if (compUBnd(1) .ne. 16) correct=.false.
               if (compUBnd(2) .ne. 10) correct=.false.
            else if (localpet .eq. 2) then
               if (compLBnd(1) .ne. 1) correct=.false.
               if (compLBnd(2) .ne. 11) correct=.false.
               if (compUBnd(1) .ne. 8) correct=.false.
               if (compUBnd(2) .ne. 20) correct=.false.
            else if (localpet .eq. 3) then
               if (compLBnd(1) .ne. 9) correct=.false.
               if (compLBnd(2) .ne. 11) correct=.false.
               if (compUBnd(1) .ne. 16) correct=.false.
               if (compUBnd(2) .ne. 20) correct=.false.
            endif   

            call ESMF_FieldDestroy(field, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_GridDestroy(grid, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
       endif


       ! return rc based on correct
       if (correct) then
         rc=ESMF_SUCCESS
       else
         rc=ESMF_FAILURE
       endif


    end subroutine test_globalindex


end program ESMF_FieldCreateGetUTest
