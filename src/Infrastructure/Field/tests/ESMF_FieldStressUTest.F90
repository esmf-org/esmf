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
      program ESMF_FieldStressUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldStressUTest - Unit tests for Field Stress Testing
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Stress Testing unit tests.
! The companion folder Field\/src contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF

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
        !EX_UTest
        ! Create an field stress test
        call test_field_create(rc, 10)
        write(failMsg, *) ""
        write(name, *) "Create a simple Field, repeat 10 times"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest
        ! Create an field stress test
        call test_field_create(rc, 100)
        write(failMsg, *) ""
        write(name, *) "Create a simple Field, repeat 100 times"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest
        ! Create an field stress test
        call test_field_create(rc, 1000)
        write(failMsg, *) ""
        write(name, *) "Create a simple Field, repeat 1000 times"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest
        ! Create an field stress test
        call test_field_create(rc, 10000)
        write(failMsg, *) ""
        write(name, *) "Create a simple Field, repeat 10000 times"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"
    subroutine test_field_create(rc, n_loop)
        integer, intent(inout) :: rc
        integer, intent(in)    :: n_loop

        real(ESMF_KIND_R8), dimension(:,:), pointer :: farray
        type(ESMF_Field)    :: field
        type(ESMF_Grid)     :: grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Array)    :: array
        integer             :: localrc, i

        integer             :: gcc(2), gec(2)

        localrc = ESMF_SUCCESS
        rc = ESMF_SUCCESS
        do i = 1, n_loop
            grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
                                      regDecomp=(/1,1/), name="landgrid", rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_GridGet(grid, localde=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
                computationalCount=gcc, exclusiveCount=gec, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            allocate(farray(max(gcc(1), gec(1)), max(gcc(2), gec(2))) )

            array = ESMF_ArrayCreate(distgrid, farray, &
                indexflag=ESMF_INDEX_DELOCAL, &
                computationalEdgeUWidth=(/-1,-1/), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            field = ESMF_FieldCreate(grid, array, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return

            call ESMF_FieldDestroy(field, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_ArrayDestroy(array, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            call ESMF_GridDestroy(grid, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
            deallocate(farray)
        enddo
        rc = localrc

    end subroutine test_field_create

end program ESMF_FieldStressUTest
