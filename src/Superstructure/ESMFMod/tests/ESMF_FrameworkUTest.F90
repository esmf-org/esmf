! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FrameworkUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FrameworkUTest - Unit test for Framework.
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
    integer :: rc, rcI, rcF
    character(ESMF_MAXSTR) :: cname
    logical :: isInitialized, isFinalized

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name
    integer :: result = 0

#ifdef ESMF_TESTEXHAUSTIVE
#endif

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

    ! special case here to call before ESMF_TestStart()        
    isInitialized = ESMF_IsInitialized(rc=rcI)
    isFinalized   = ESMF_IsFinalized(rc=rcF)

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsInitialized() before ESMF_Initialize() was called"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rcI.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsInitialized() before ESMF_Initialize() was called"
    write(failMsg, *) "Did not return .false."
    call ESMF_Test((isInitialized .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsFinalized() before ESMF_Initialize() was called"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Test((rcF.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsFinalized() before ESMF_Initialize() was called"
    write(failMsg, *) "Did not return .false."
    call ESMF_Test((isFinalized .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsInitialized() after ESMF_Initialize() was called"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isInitialized = ESMF_IsInitialized(rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsInitialized() after ESMF_Initialize() was called"
    write(failMsg, *) "Did not return .true."
    call ESMF_Test((isInitialized .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsFinalized() after ESMF_Initialize() was called"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    isFinalized   = ESMF_IsFinalized(rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !-----------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Testing IsFinalized() after ESMF_Initialize() was called"
    write(failMsg, *) "Did not return .false."
    call ESMF_Test((isFinalized .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
    !TODO: somehow figure out how to test anything _after_ ESMF_Finalize()
#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program ESMF_FrameworkUTest
    
