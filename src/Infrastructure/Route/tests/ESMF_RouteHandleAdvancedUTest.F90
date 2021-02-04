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

module compAmod
  use ESMF
  implicit none
  private
  public SetServices
  
 contains
 
  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine Initialize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompGet(comp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360, 160/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="fieldA", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldFill(field, dataFillScheme="one", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 0
    call ESMF_FieldWrite(field, fileName="srcField.nc", &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
  end subroutine
  
  subroutine Finalize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_StateGet(exportState, field=field, itemName="fieldA", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldDestroy(field, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

end module compAmod

!-------------------------------------------------------------------------------

module compBmod
  use ESMF
  implicit none
  private
  public SetServices
  
 contains
 
  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine Initialize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_GridCompGet(comp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/360, 160/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      regDecomp=(/petCount,1/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name="fieldB", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateAdd(importState, (/field/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine Finalize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    integer               :: petCount
    type(ESMF_Grid)       :: grid
    type(ESMF_Field)      :: field
    !
    rc = ESMF_SUCCESS
    call ESMF_StateGet(importState, field=field, itemName="fieldB", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 0
    call ESMF_FieldWrite(field, fileName="dstField.nc", &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    call ESMF_FieldDestroy(field, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

end module compBmod

!-------------------------------------------------------------------------------

#ifndef ESMF_NO_DYNMASKOVERLOAD

module dynMaskmod
  use ESMF
  implicit none
  private
  public dynMaskR4R4R4
  public dynMaskR4R4R4V
  public dynMaskR4R8R4V
  
 contains
 
  subroutine dynMaskR4R4R4(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR4R4R4), pointer        :: dynamicMaskList(:)
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    ! dummy routine for unit test that does nothing
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine

  subroutine dynMaskR4R4R4V(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR4R4R4V), pointer       :: dynamicMaskList(:)
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    ! dummy routine for unit test that does nothing
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine
 
  subroutine dynMaskR4R8R4V(dynamicMaskList, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
    type(ESMF_DynamicMaskElementR4R8R4V), pointer       :: dynamicMaskList(:)
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
    integer,                       intent(out)          :: rc
    ! dummy routine for unit test that does nothing
    ! return successfully
    rc = ESMF_SUCCESS
  end subroutine
 
end module dynMaskmod

#endif

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

program ESMF_RouteHandleAdvancedUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_RouteHandleAdvancedUTest
!                  - This unit test file verifies Route methods.
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

  use compAmod, only: ssA => SetServices
  use compBmod, only: ssB => SetServices
#ifndef ESMF_NO_DYNMASKOVERLOAD  
  use dynMaskmod
#endif

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer                 :: rc, urc
  type(ESMF_VM)           :: vm
  integer                 :: i
  integer                 :: petCount, petCountR, petCountA, petCountB1
  integer, allocatable    :: petListA(:), petListB1(:), petListB2(:)
  integer, allocatable    :: originPetList(:), targetPetList(:)
  type(ESMF_State)        :: stateAB1, stateAB2
  type(ESMF_GridComp)     :: compA, compB1, compB2
  type(ESMF_Field)        :: fieldA, fieldB1, fieldB2
  type(ESMF_RouteHandle)  :: rh1, rh2
  logical                 :: isCreated
  type(ESMF_DynamicMask)  :: dynamicMask

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  stateAB1 = ESMF_StateCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  stateAB2 = ESMF_StateCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
  petCountA = petCount/2  ! component A gets half the PETs

  allocate(petListA(petCountA))
  do i=1, petCountA
    petListA(i) = i-1 ! PETs are base 0
  enddo
  
  ! split the remaining PETs evenly between component B + C
  petCountR = petCount - petCountA
  petCountB1 = petCountR / 2
  
  allocate(petListB1(petCountB1))
  do i=1, petCountB1
    petListB1(i) = petCountA + i-1 ! PETs are base 0
  enddo

  allocate(petListB2(petCountR-petCountB1))
  do i=1, petCountR-petCountB1
    petListB2(i) = petCountA + petCountB1 + i-1 ! PETs are base 0
  enddo

  compA = ESMF_GridCompCreate(petList=petListA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  compB1 = ESMF_GridCompCreate(petList=petListB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  compB2 = ESMF_GridCompCreate(petList=petListB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compA, ssA, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compB1, ssB, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompSetServices(compB2, ssB, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(compA, exportState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompInitialize(compB1, importState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_GridCompInitialize(compB2, importState=stateAB2, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_StateReconcile(stateAB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateReconcile(stateAB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateGet(stateAB1, field=fieldA, itemName="fieldA", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_StateGet(stateAB1, field=fieldB1, itemName="fieldB", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateGet(stateAB2, field=fieldB2, itemName="fieldB", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create RouteHandle for the original petList"
  write(failMsg, *) "RouteHandleCreate failed"
  call ESMF_FieldRedistStore(srcField=fieldA, dstField=fieldB1, &
    routehandle=rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleIsCreated()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_RouteHandleIsCreated(rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleIsCreated() return value"
  write(failMsg, *) "Incorrect return value"
  call ESMF_Test((isCreated), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleGet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_RouteHandleGet(rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleWrite()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_RouteHandleWrite(rh1, fileName="testWrite.RH", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleDestroy()"
  write(failMsg, *) "RouteHandleDestroy failed"
  call ESMF_RouteHandleDestroy(rh1, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleCreate(from file)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rh2 = ESMF_RouteHandleCreate(fileName="testWrite.RH", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Apply the read in Routehandle"
  write(failMsg, *) "ESMF_FieldRedist failed"
  call ESMF_FieldRedist(srcField=fieldA, dstField=fieldB1, &
    routehandle=rh2, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleDestroy() for the read in Routehandle"
  write(failMsg, *) "RouteHandleDestroy failed"
  call ESMF_RouteHandleDestroy(rh2, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! Prepare to test create RH from RH
  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create RouteHandle for the original petList"
  write(failMsg, *) "RouteHandleCreate failed"
  call ESMF_FieldRedistStore(srcField=fieldA, dstField=fieldB1, &
    routehandle=rh1, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  ! construct originPetList
  allocate(originPetList(size(petListA)+size(petListB1)))
  originPetList(1:size(petListA)) = petListA(:)
  originPetList(size(petListA)+1:) = petListB1(:)
  ! construct targetPetList
  allocate(targetPetList(size(petListA)+size(petListB2)))
  targetPetList(1:size(petListA)) = petListA(:)
  targetPetList(size(petListA)+1:) = petListB2(:)

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Transfer RouteHandle to a different petList"
  write(failMsg, *) "RouteHandleCreate failed"
  rh2 = ESMF_RouteHandleCreate(rh1, originPetList=originPetList, &
    targetPetList=targetPetList, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(originPetList, targetPetList)
  !-----------------------------------------------------------------------------

  call ESMF_RouteHandlePrint(rh1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldFill(fieldA, dataFillScheme="sincos", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldFill(fieldB1, dataFillScheme="one", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldFill(fieldB2, dataFillScheme="one", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleDestroy()"
  write(failMsg, *) "RouteHandleDestroy failed"
  call ESMF_RouteHandleDestroy(rh1, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  ! apply the RH
  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Apply the copied Routehandle"
  write(failMsg, *) "ESMF_FieldRedist failed"
  call ESMF_FieldRedist(srcField=fieldA, dstField=fieldB2, &
    routehandle=rh2, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test RouteHandleDestroy() for the copied Routehandle"
  write(failMsg, *) "RouteHandleDestroy failed"
  call ESMF_RouteHandleDestroy(rh2, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  call ESMF_GridCompFinalize(compA, exportState=stateAB1, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompFinalize(compB2, importState=stateAB2, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(stateAB1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(stateAB2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(petListA)
  deallocate(petListB1)
  deallocate(petListB2)

#ifndef ESMF_NO_DYNMASKOVERLOAD

 !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test ESMF_DynamicMaskSetR4R4R4()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DynamicMaskSetR4R4R4(dynamicMask, &
    dynamicMaskRoutine=dynMaskR4R4R4, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

 !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test ESMF_DynamicMaskSetR4R4R4V()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DynamicMaskSetR4R4R4V(dynamicMask, &
    dynamicMaskRoutine=dynMaskR4R4R4V, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

 !-----------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Test ESMF_DynamicMaskSetR4R8R4V()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DynamicMaskSetR4R8R4V(dynamicMask, &
    dynamicMaskRoutine=dynMaskR4R8R4V, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
#else

  write(name, *) "Dummy test to satisfy scripts for ESMF_NO_DYNMASKOVERLOAD"
  write(failMsg, *) "Did not succeed" 
  do i=1,3
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  enddo

#endif

  !------------------------------------------------------------------------
10 continue
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_RouteHandleAdvancedUTest
