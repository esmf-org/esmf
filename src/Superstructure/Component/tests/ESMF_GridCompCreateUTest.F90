! $Id: ESMF_GridCompCreateUTest.F90,v 1.21.2.4 2009/01/21 21:25:24 cdeluca Exp $
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
      program ESMF_GridCompCreateUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GridCompCreateUTest - Unit test for Components.
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Component Create code.
!
!-------------------------------------------------------------------------
!
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname, bname
    type(ESMF_GridComp) :: comp1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

    ! Internal State Variables
    type testData
    sequence
        integer :: testNumber
    end type

    type dataWrapper
    sequence
        type(testData), pointer :: p
    end type

    type (dataWrapper) :: wrap1, wrap2
    type(testData), target :: data1, data2


!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------
        
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Test creation of a Component
    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
                                             configFile="grid.rc", rc=rc)  

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
!   !
    !NEX_UTest
!   !  Destroying a component

    call ESMF_GridCompDestroy(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name from a destroyed component

    call ESMF_GridCompGet(comp1, name=bname, rc=rc)

    write(failMsg, *) "Returned ESMF_SUCCESS incorrectly"
    write(name, *) "Getting  a Component name Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    cname = "Atmosphere"
    comp1 = ESMF_GridCompCreate(name=cname, gridcompType=ESMF_ATM, &
                                             configFile="grid.rc", rc=rc)  

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test validate a component

    call ESMF_GridCompValidate(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Validating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------
!   !
    !EX_UTest
    ! Wait for a concurrent component to finish executing.

    call ESMF_GridCompWait(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Waiting for a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name

    call ESMF_GridCompGet(comp1, name=bname, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting  a Component name Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify the name is correct


    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying the correct Component name was returned Test"
    call ESMF_Test((bname.eq."Atmosphere"), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !  Set Internal State
    !EX_UTest
    data1%testnumber=4567
    wrap1%p=>data1

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Set Internal State Test"
    call ESMF_GridCompSetInternalState(comp1, wrap1, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Get Internal State
    !EX_UTest
    !wrap2%p=>data2
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Get Internal State Test"
    call ESMF_GridCompGetInternalState(comp1, wrap2, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
!   !  Verify Internal State
    !EX_UTest
    data2 = wrap2%p 
    write(failMsg, *) "Did not return correct data"
    write(name, *) "Verify Internal State Test"
    call ESMF_Test((data2%testnumber.eq.4567), name, failMsg, result, ESMF_SRCLINE)
    print *, "data2%testnumber = ", data2%testnumber


!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test printing a component

    call ESMF_GridCompPrint(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component

    call ESMF_GridCompDestroy(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_GridCompCreateUTest
    
