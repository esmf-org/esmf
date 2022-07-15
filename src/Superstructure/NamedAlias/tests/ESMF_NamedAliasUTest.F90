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

  !------------------------------------------------------------------------
10 continue
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains !======================================================================

  subroutine TestStateNamedAlias(rc)
    integer, intent(out)  :: rc
    type(ESMF_State)      :: object1, object2

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

end program ESMF_NamedAliasUTest
