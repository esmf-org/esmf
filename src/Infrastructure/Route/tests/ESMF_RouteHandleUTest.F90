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

program ESMF_RouteHandleUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_RouteHandleUTest - This unit test file verifies Route methods.
!
! !DESCRIPTION:
!
! The code in this file drives F90 Route unit tests.
! The companion file ESMF\_Route.F90 contains the definitions for the
! Route methods.
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
  integer                 :: rc
  type(ESMF_VM)           :: vm
  integer                 :: petCount
  type(ESMF_Grid)         :: gridA, gridB
  type(ESMF_Field)        :: fieldA, fieldB
  type(ESMF_RouteHandle)  :: rh1, rh2
  logical                 :: isCreated

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! start testing framework
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! prepare auxiliary data objects
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  gridA = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360, 160/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/petCount,1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldA = ESMF_FieldCreate(gridA, ESMF_TYPEKIND_R8, name="fieldA", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  gridB = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360, 160/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/1,petCount/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldB = ESMF_FieldCreate(gridB, ESMF_TYPEKIND_R8, name="fieldB", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleIsCreated()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_RouteHandleIsCreated(rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleIsCreated() return value"
  write(failMsg, *) "Incorrect return value"
  call ESMF_Test((.not.isCreated), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create RouteHandle"
  write(failMsg, *) "RouteHandleCreate failed"
  call ESMF_FieldRedistStore(srcField=fieldA, dstField=fieldB, &
    routehandle=rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleIsCreated()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_RouteHandleIsCreated(rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleIsCreated() return value"
  write(failMsg, *) "Incorrect return value"
  call ESMF_Test((isCreated), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleWrite()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_RouteHandleWrite(rh1, fileName="testWrite.RH", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleDestroy()"
  write(failMsg, *) "RouteHandleDestroy failed"
  call ESMF_RouteHandleDestroy(rh1, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! delay for file to be available on disk
  call ESMF_VMWtimeDelay(20.d0, rc=rc)  ! wait for 20s
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleCreate(from file)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rh2 = ESMF_RouteHandleCreate(fileName="testWrite.RH", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Apply the read in Routehandle"
  write(failMsg, *) "ESMF_FieldRedist failed"
  call ESMF_FieldRedist(srcField=fieldA, dstField=fieldB, &
    routehandle=rh2, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test RouteHandleDestroy() for the read in Routehandle"
  write(failMsg, *) "RouteHandleDestroy failed"
  call ESMF_RouteHandleDestroy(rh2, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  call ESMF_LogFlush(rc=rc)

  !------------------------------------------------------------------------
10 continue
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_RouteHandleUTest
