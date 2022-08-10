! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_NamedAliasUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_NamedAliasUTest - This unit test file tests ESMF_NamedAlias().
! !DESCRIPTION:
!
! The code in this file drives F90 NamedAlias() unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  logical                 :: testFlag
  character(ESMF_MAXSTR)  :: name1, name2

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  call TestStateNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestGridCompNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestCplCompNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestSciCompNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestFieldBundleNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestFieldNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestArrayBundleNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call TestArrayNamedAlias(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
10 continue
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains !======================================================================

  subroutine TestStateNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_State)      :: object1, object2
    type(ESMF_State)      :: state

    object1 = ESMF_StateCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias State Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_StateGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for State Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateSet(object2, name="Test Name 2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_StateGet(object1, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for State not affecting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for State correct setting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (trim(name2) == "Test Name 2")
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is still an Alias State Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object State Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_StateGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_StateCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias State Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_StateGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for State Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias State Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object State Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_StateGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestGridCompNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_GridComp)   :: object1, object2

    object1 = ESMF_GridCompCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias GridComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_GridCompGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_GridCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for GridComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_GridCompSet(object2, name="Test Name 2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_GridCompGet(object1, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for GridComp not affecting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_GridCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for GridComp correct setting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (trim(name2) == "Test Name 2")
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is still an Alias GridComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object GridComp Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_GridCompGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_GridCompCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias GridComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_GridCompGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_GridCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for GridComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_GridCompDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object GridComp Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_GridCompGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestCplCompNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_CplComp)    :: object1, object2

    object1 = ESMF_CplCompCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for CplComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias CplComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_CplCompGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_CplCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for CplComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_CplCompSet(object2, name="Test Name 2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_CplCompGet(object1, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for CplComp not affecting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_CplCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for CplComp correct setting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (trim(name2) == "Test Name 2")
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is still an Alias CplComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias CplComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_CplCompDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object CplComp Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_CplCompGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_CplCompCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for CplComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias CplComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_CplCompGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_CplCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for CplComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias CplComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_CplCompDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object CplComp Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_CplCompGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestSciCompNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_SciComp)    :: object1, object2

    object1 = ESMF_SciCompCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for SciComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias SciComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_SciCompGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_SciCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for SciComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_SciCompSet(object2, name="Test Name 2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_SciCompGet(object1, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for SciComp not affecting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_SciCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for SciComp correct setting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (trim(name2) == "Test Name 2")
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is still an Alias SciComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias SciComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_SciCompDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object SciComp Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_SciCompGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_SciCompCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for SciComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias SciComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_SciCompGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_SciCompGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for SciComp Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias SciComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_SciCompDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object SciComp Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_SciCompGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestFieldBundleNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_FieldBundle):: object1, object2
    type(ESMF_State)      :: state

    object1 = ESMF_FieldBundleCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias FieldBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_FieldBundleGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for FieldBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_FieldBundleDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object FieldBundle Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_FieldBundleGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_FieldBundleCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias FieldBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_FieldBundleGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldBundleGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for FieldBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias FieldBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_FieldBundleDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object FieldBundle Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_FieldBundleGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestFieldNamedAlias(rc)
    integer, intent(out)        :: rc
    type(ESMF_Field)            :: object1, object2
    type(ESMF_Grid)             :: grid
    type(ESMF_State)            :: state

    grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), rc=rc)
    if (rc /= ESMF_SUCCESS) return
    object1 = ESMF_FieldCreate(name="Test Name 1", typekind=ESMF_TYPEKIND_R8, &
      grid=grid, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias Field Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_FieldGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for Field Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_FieldSet(object2, name="Test Name 2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(object1, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for Field not affecting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_FieldGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for Field correct setting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (trim(name2) == "Test Name 2")
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is still an Alias Field Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_FieldDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object Field Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_FieldGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_FieldCreate(name="Test Name 1", typekind=ESMF_TYPEKIND_R8, &
      grid=grid, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias Field Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_FieldGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_FieldGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for Field Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias Field Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_FieldDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object Field Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_FieldGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestArrayBundleNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_ArrayBundle):: object1, object2
    type(ESMF_State)      :: state

    object1 = ESMF_ArrayBundleCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias ArrayBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_ArrayBundleGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayBundleGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for ArrayBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_ArrayBundleDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object ArrayBundle Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_ArrayBundleGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_ArrayBundleCreate(name="Test Name 1", rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias ArrayBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_ArrayBundleGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayBundleGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for ArrayBundle Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias ArrayBundle Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_ArrayBundleDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object ArrayBundle Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_ArrayBundleGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

  subroutine TestArrayNamedAlias(rc)
    integer, intent(out)        :: rc
    type(ESMF_Array)            :: object1, object2
    type(ESMF_DistGrid)         :: distgrid
    type(ESMF_State)            :: state

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/16,20/), rc=rc)
    if (rc /= ESMF_SUCCESS) return
    object1 = ESMF_ArrayCreate(name="Test Name 1", typekind=ESMF_TYPEKIND_R8, &
      distgrid=distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with default name for Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias Array Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_ArrayGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Same default name for NamedAlias for Array Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_ArraySet(object2, name="Test Name 2", rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(object1, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for Array not affecting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 == name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_ArrayGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Name change on NamedAlias for Array correct setting Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (trim(name2) == "Test Name 2")
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is still an Alias Array Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_ArrayDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object Array Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_ArrayGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    object1 = ESMF_ArrayCreate(name="Test Name 1", typekind=ESMF_TYPEKIND_R8, &
      distgrid=distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias() with new name for Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    object2 = ESMF_NamedAlias(object1, name="Test Name 2", rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NamedAlias is an Alias Array Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (object1 == object2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_ArrayGet(object1, name=name1, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_ArrayGet(object2, name=name2, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Different name for NamedAlias for Array Test"
    write(failMsg, *) "Incorrect result"
    testFlag = (name1 /= name2)
    call ESMF_Test(testFlag, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    state = ESMF_StateCreate(rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Add Alias and NamedAlias to State Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_StateAdd(state, (/object1, object2/), rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve Alias from State Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name1, object1, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Retrieve NamedAlias from State Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_Stateget(state, name2, object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    call ESMF_StateDestroy(state, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroy object through NamedAlias Array Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call ESMF_ArrayDestroy(object2, rc=rc)
    call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "Destroying NamedAlias destroys object Array Test"
    write(failMsg, *) "Returns ESMF_SUCCESS, but should not"
    call ESMF_ArrayGet(object1, name=name1, rc=rc)
    call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    ! return successfully
    rc = ESMF_SUCCESS

  end subroutine

  !============================================================================

end program ESMF_NamedAliasUTest
