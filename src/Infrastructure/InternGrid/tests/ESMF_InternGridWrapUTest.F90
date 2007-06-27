! $Id: ESMF_InternGridWrapUTest.F90,v 1.3 2007/06/27 20:36:07 cdeluca Exp $
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
    program ESMF_IGridWrapUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOC
! !PROGRAM: ESMF_IGridWrapUTest - Tests related to periodic igrids
!
! !DESCRIPTION:
!
! The code in this file tests things specific to periodic igrids.
!
!-----------------------------------------------------------------------------

    ! ESMF Framework module, and test methods
    use ESMF_Mod
    use ESMF_TestMod

    implicit none
    
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternGridWrapUTest.F90,v 1.3 2007/06/27 20:36:07 cdeluca Exp $'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0
      
    ! individual test result code
    integer :: rc
      
    ! individual test name 
    character(ESMF_MAXSTR) :: name
      
    ! individual test failure messages
    character(ESMF_MAXSTR*2) :: failMsg

    ! Local variables
    integer :: igridCount(2), igridStart(2)
    real (ESMF_KIND_R8), dimension(2) :: origin
    type(ESMF_DELayout)     :: layout
    type(ESMF_IGrid)         :: igrid
    type(ESMF_VM)           :: vm


!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_VMGetGlobal(vm, rc)
    write(name, *) "ESMF_VMGetGlobal Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    layout = ESMF_DELayoutCreate(vm, (/ 3, 2 /), rc=rc)
    write(name, *) "ESMF_DELayoutCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    origin = (/ 0.0, 0.0 /)

    !--------------------------------------------------------------------------
    !NEX_UTest
    igrid = ESMF_IGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_FALSE, ESMF_FALSE /), &
                                    name="atmigrid", rc=rc)
    write(name, *) "ESMF_IGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_IGridDistribute(igrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_IGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    ! the igridStart lines should all print the same values (and do not).
    ! this is support request 1095990, bug 1156870.
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=igridCount, &
                                 globalStartPerDim=igridStart, rc=rc)
    print *, "F/F igridStart=", igridStart
    write(name, *) "ESMF_IGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    igrid = ESMF_IGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_FALSE, ESMF_TRUE /), &
                                    name="atmigrid", rc=rc)
    write(name, *) "ESMF_IGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_IGridDistribute(igrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_IGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=igridCount, &
                                 globalStartPerDim=igridStart, rc=rc)
    print *, "F/T igridStart=", igridStart
    write(name, *) "ESMF_IGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    igrid = ESMF_IGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_TRUE, ESMF_FALSE /), &
                                    name="atmigrid", rc=rc)
    write(name, *) "ESMF_IGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_IGridDistribute(igrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_IGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=igridCount, &
                                 globalStartPerDim=igridStart, rc=rc)
    print *, "T/F igridStart=", igridStart
    write(name, *) "ESMF_IGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    igrid = ESMF_IGridCreateHorzXYUni((/ 50, 30 /), origin, &
                                    deltaPerDim=(/ 1.0d0, 1.0d0 /), &
	                            periodic=(/ ESMF_TRUE, ESMF_TRUE /), &
                                    name="atmigrid", rc=rc)
    write(name, *) "ESMF_IGridCreate Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    call ESMF_IGridDistribute(igrid, delayout=layout, rc=rc)
    write(name, *) "ESMF_IGridDistribute Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !NEX_UTest
    ! get igrid information used to calculate global indices
    call ESMF_IGridGetDELocalInfo(igrid, horzrelloc=ESMF_CELL_CENTER, &
                                 localCellCountPerDim=igridCount, &
                                 globalStartPerDim=igridStart, rc=rc)
    print *, "T/T igridStart=", igridStart
    write(name, *) "ESMF_IGridGetDELocalInfo Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                    name, failMsg, result, ESMF_SRCLINE)

    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------


    call ESMF_TestEnd(result, ESMF_SRCLINE)


    end program ESMF_IGridWrapUTest

    
