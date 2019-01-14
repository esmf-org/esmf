! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_SciCompCreateUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_SciCompCreateUTest - Unit test for Components.
!
! !DESCRIPTION:
! Tests, cursory and exahustive, for Component Create code.
!
!-------------------------------------------------------------------------
!
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    implicit none
    
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: cname
    type(ESMF_SciComp) :: comp1, scicompAlias
    logical:: scicompBool
    logical:: isCreated

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0


!! #ifdef ESMF_TESTEXHAUSTIVE
    character(ESMF_MAXSTR) :: bname
!! #endif

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------
        
    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing SciComp IsCreated for uncreated object"
    write(failMsg, *) "Did not return .false."
    isCreated = ESMF_SciCompIsCreated(comp1)
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing SciComp IsCreated for uncreated object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_SciCompIsCreated(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    cname = "Atmosphere"
    comp1 = ESMF_SciCompCreate(name=cname, rc=rc)  
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Science Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing SciComp IsCreated for created object"
    write(failMsg, *) "Did not return .true."
    isCreated = ESMF_SciCompIsCreated(comp1)
    call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing SciComp IsCreated for created object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_SciCompIsCreated(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "SciComp equality before assignment Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    scicompBool = (scicompAlias.eq.comp1)
    call ESMF_Test(.not.scicompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_SciCompAssignment(=)()
    write(name, *) "SciComp assignment and equality Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    scicompAlias = comp1
    scicompBool = (scicompAlias.eq.comp1)
    call ESMF_Test(scicompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "SciCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_SciCompDestroy(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing SciComp IsCreated for destroyed object"
    write(failMsg, *) "Did not return .false."
    isCreated = ESMF_SciCompIsCreated(comp1)
    call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing SciComp IsCreated for destroyed object"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isCreated = ESMF_SciCompIsCreated(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_SciCompOperator(==)()
    write(name, *) "SciComp equality after destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    scicompBool = (scicompAlias==comp1)
    call ESMF_Test(.not.scicompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    ! Testing ESMF_SciCompOperator(/=)()
    write(name, *) "SciComp non-equality after destroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    scicompBool = (scicompAlias/=comp1)
    call ESMF_Test(scicompBool, name, failMsg, result, ESMF_SRCLINE)
    
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Double SciCompDestroy through alias Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_SciCompDestroy(scicompAlias, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!-------------------------------------------------------------------------
#ifdef ESMF_TESTEXHAUSTIVE
!   !
    !EX_UTest
!   !  Test get a Component name from a destroyed component

    call ESMF_SciCompGet(comp1, name=bname, rc=rc)

    write(failMsg, *) "Did return ESMF_SUCCESS"
    write(name, *) "Getting a Component name Test"
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    comp1 = ESMF_SciCompCreate(rc=rc)  

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
    write(name, *) "SciCompDestroy Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_SciCompDestroy(comp1, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test creation of a Component
    cname = "Atmosphere"
    comp1 = ESMF_SciCompCreate(name=cname, rc=rc)  

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Creating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test validate a component

    call ESMF_SciCompValidate(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Validating a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test get a Component name

    call ESMF_SciCompGet(comp1, name=bname, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Getting a Component name Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Verify the name is correct

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Verifying the correct Component name was returned Test"
print *, "cname = ", cname
print *, "bname = ", bname
    call ESMF_Test((bname.eq.cname), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test Set a Science Component name
    cname = "CAM"
    call ESMF_SciCompSet(comp1, name=cname, rc=rc)
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Setting  a Science Component name Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Test printing a component

    call ESMF_SciCompPrint(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Printing a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------
!   !
    !EX_UTest
!   !  Destroying a component

    call ESMF_SciCompDestroy(comp1, rc=rc)

    write(failMsg, *) "Did not return ESMF_SUCCESS"
    write(name, *) "Destroying a Component Test"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program ESMF_SciCompCreateUTest
    
