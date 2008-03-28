! $Id: ESMF_FieldCreateGetUTest.F90,v 1.4 2008/03/28 06:48:27 theurich Exp $
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
    character(ESMF_MAXSTR) :: name

    integer, dimension(:,:), pointer :: fptr
    real, dimension(10,20)  :: farray
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
#if 0
        grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                  regDecomp=(/2,2/), name="testgrid", rc=rc)

        !------------------------------------------------------------------------
        !EX_removeUTest_Multi_Proc_Only
        ! Create an empty field
        field = ESMF_FieldCreate(rc=rc) 
        write(failMsg, *) ""
        write(name, *) "Creating an empty Field"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
        !------------------------------------------------------------------------
        !EX_removeUTest_Multi_Proc_Only
        ! Get info associated with the empty field
        call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
            rank=rank, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            rc=rc)
        write(failMsg, *) ""
        write(name, *) "Get info associated with the empty field"
        call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
        call ESMF_FieldDestroy(field, rc=rc)
  
        !------------------------------------------------------------------------
        !EX_removeUTest_Multi_Proc_Only
        ! Create a field from an fortran array
        field = ESMF_FieldCreate(grid, farray, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            ungriddedLBound = (/2,3/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/), rc=rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran array"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        call ESMF_FieldPrint(field, rc=rc)
        !------------------------------------------------------------------------
        !EX_removeUTest_Multi_Proc_Only
        ! Get info associated with the field created from fortran array
        call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
            rank=rank, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            rc=rc)
        write(failMsg, *) ""
        write(name, *) "Get info associated with the field created from fortran array"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
        call ESMF_FieldDestroy(field, rc=rc)

        !------------------------------------------------------------------------
        !EX_removeUTest_Multi_Proc_Only
        ! Create a field from an fortran pointer
        allocate(fptr(10, 20))
        field = ESMF_FieldCreate(grid, fptr, copyflag=ESMF_DATA_COPY, &
            staggerloc=ESMF_STAGGERLOC_CENTER, gridToFieldMap = (/2,1/), &
            ungriddedLBound = (/2,3/), ungriddedUBound=(/4,5/), &
            maxHaloLWidth=(/6,7/), maxHaloUWidth=(/8,9/), rc=rc)
        write(failMsg, *) ""
        write(name, *) "Creating an Field from a fortran ptr"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_removeUTest_Multi_Proc_Only
        ! Get info associated with the field created from fortran pointer
        call ESMF_FieldGet(field, grid=grid1, array=array, typekind=typekind, &
            rank=rank, staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
            ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
            maxHaloLWidth=maxHaloLWidth, maxHaloUWidth=maxHaloUWidth, &
            rc=rc)
        write(failMsg, *) ""
        write(name, *) "Get info associated with the field created from fortran pointer"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        deallocate(fptr)
        call ESMF_FieldDestroy(field, rc=rc)

        call ESMF_GridDestroy(grid, rc=rc)
#endif
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

end program ESMF_FieldCreateGetUTest
