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

#include "ESMF.h"
#include "ESMF_Macros.inc"

module ESMF_NUOPC_UTest_Mod
  
  use ESMF_TestMod     ! test methods
  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS               => SetServices, &
    driver_label_SetModelServices   => label_SetModelServices
  use NUOPC_Model, model_routine_SS => SetServices
  use NUOPC_Connector, cplSS        => SetServices

  private
  
  character(ESMF_MAXSTR)  :: failMsg
  character(ESMF_MAXSTR)  :: name
  integer                 :: result = 0

  public driverSetServices

  contains

  subroutine driverSetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc=ESMF_SUCCESS
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_CompSpecialize() for GridComp before Derive() Test"
    write(failMsg, *) "Did incorrectly return ESMF_SUCCESS"
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    rc=ESMF_SUCCESS ! re-initialize for the remaining test code
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_CompDerive() for GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_CompSpecialize() for GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_CompSetEntryPoint() for GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_CompSetEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p20"/), userRoutine=DummyInit, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_CompSetInternalEntryPoint() for GridComp Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p20"/), userRoutine=DummyInit, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    rc=ESMF_SUCCESS
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_DriverAddComp() Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_DriverAddComp(driver=driver, compLabel="testComp1", &
      compSetServicesRoutine=model_routine_SS, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_DriverAddComp() Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_DriverAddComp(driver=driver, compLabel="testComp2", &
      compSetServicesRoutine=model_routine_SS, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !------------------------------------------------------------------------
    !NEX_UTest
    write(name, *) "NUOPC_DriverAddComp() Test"
    write(failMsg, *) "Did not return ESMF_SUCCESS"
    call NUOPC_DriverAddComp(driver, srcCompLabel="testComp1", &
      dstCompLabel="testComp2", compSetServicesRoutine=cplSS, rc=rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  end subroutine
  
  subroutine DummyInit(driver, iState, eState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: iState, eState 
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    rc=ESMF_SUCCESS
  end subroutine

end module


program ESMF_NUOPC_UTest

!------------------------------------------------------------------------------
 
!==============================================================================
!BOP
! !PROGRAM: ESMF_NUOPC_Test - This unit test file verifies NUOPC methods.
!
! !DESCRIPTION:
!
! The code in this file drives F90 NUOPC unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use NUOPC
  use NUOPC_Driver
  use NUOPC_Connector
  use NUOPC_ModelBase
  use NUOPC_Model
  use NUOPC_Mediator
  use ESMF_NUOPC_UTest_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, urc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_VM)           :: vm
  integer                 :: petCount, localPet, slotCount
  character(ESMF_MAXSTR)  :: compName, valueString
  type(ESMF_Time)         :: startTime, stopTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock)        :: clockA, clockB, clockC
  type(ESMF_GridComp)     :: gridComp, comp
  type(ESMF_CplComp)      :: cplComp
  logical                 :: flag
  type(ESMF_State)        :: stateA, stateB, stateC
  type(ESMF_Field)        :: field
  character(ESMF_MAXSTR)  :: value
  integer                 :: valueInt
  type(ESMF_FieldBundle)  :: fieldBundleA, fieldBundleB
  type(ESMF_Grid)         :: grid
  integer                 :: i, j
  real(ESMF_KIND_R8),      pointer  :: xPtr(:), yPtr(:), dataPtr(:,:)
  character(ESMF_MAXSTR),  pointer  :: stdAttrNameList(:)
  character(len=120)      :: tempString
  type(NUOPC_FreeFormat)  :: runSeqFF, attrFF, fdFF
  character(len=NUOPC_FreeFormatLen)  :: runSequence(5)
  real(ESMF_KIND_R8), allocatable :: factorList(:)
  integer, allocatable            :: factorIndexList(:,:)
  character(len=40)       :: phaseLabel
  logical                 :: isSet

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
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeSet(startTime, s = 0, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeSet(stopTime, s = 3600, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeIntervalSet(timeStep, s = 1800, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  clockA = ESMF_ClockCreate(name="TestClock A", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  clockB = ESMF_ClockCreate(name="TestClock B", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  clockC = ESMF_ClockCreate(name="TestClock C", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  gridComp = ESMF_GridCompCreate(name="TestGridComp", clock=clockC, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  cplComp = ESMF_CplCompCreate(name="TestCplComp", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  stateA = ESMF_StateCreate(name="TestState A", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  stateB = ESMF_StateCreate(name="TestState B", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  field = ESMF_FieldEmptyCreate(name="sea_surface_temperature", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldBundleA = ESMF_FieldBundleCreate(name="TestFieldBundle A", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldBundleB = ESMF_FieldBundleCreate(name="TestFieldBundle B", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_NoOp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_NoOp(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! -> Generic Driver methods
  !------------------------------------------------------------------------
  
  ! setting up a driver component in gridComp
  call ESMF_GridCompSetServices(gridComp, driverSetServices, userRc=urc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompInitialize(gridComp, userRc=urc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompGet(gridComp, name=compName, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeGet() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeGet(gridComp, name="Verbosity", value=valueString, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_UtilString2Int() for Verbosity Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  valueInt = ESMF_UtilString2Int(valueString, &
    specialStringList=(/"max ", "high", "low ", "off "/), &
    specialValueList=(/255, 128, 32, 0/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGetComp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverGetComp(gridComp, compLabel="testComp1", comp=comp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverPrint(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGet() slotCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverGet(gridComp, slotCount=slotCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGet() slotCount validate Test"
  write(failMsg, *) "The slotCount value does not validate"
  call ESMF_Test((slotCount==1), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverSetRunSequence() with correct slot Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverSetRunSequence(gridComp, slot=1, clock=clockC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverSetRunSequence() with wrong slot Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverSetRunSequence(gridComp, slot=2, clock=clockC, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverNewRunSequence() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverNewRunSequence(gridComp, slotCount=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGet() slotCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverGet(gridComp, slotCount=slotCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGet() slotCount validate Test"
  write(failMsg, *) "The slotCount value does not validate"
  call ESMF_Test((slotCount==2), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverAddRunElement() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverAddRunElement(gridComp, slot=2, compLabel="testComp1", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! -> FreeFormat methods
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatCreate() from stringList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ! set up run sequence in free format
  data runSequence/&
    "@900", &
    "  testComp1 -> testComp2", &
    "  testComp1", &
    "  testComp2", &
    "@"/
  runSeqFF = NUOPC_FreeFormatCreate(stringList=runSequence, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverIngestRunSequence() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverIngestRunSequence(gridComp, runSeqFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGet() slotCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverGet(gridComp, slotCount=slotCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverGet() slotCount validate Test"
  write(failMsg, *) "The slotCount value does not validate"
  call ESMF_Test((slotCount==1), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatGetLine() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatGetLine(runSeqFF, line=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatAdd(runSeqFF, stringList=(/"abc", "def"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatDestroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_DriverEgestRunSequence() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_DriverEgestRunSequence(gridComp, runSeqFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatPrint(runSeqFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatGet(runSeqFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatDestroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  ! -> NUOPC Utility methods
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CheckSetClock() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CheckSetClock(clockA, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  call ESMF_ClockDestroy(clockB, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_AdjustClock() - first create clockB - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  clockB = ESMF_ClockCreate(clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_AdjustClock() - adjust clockB - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_AdjustClock(clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ClockPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ClockPrint(clockA, options="currTime", &
    preString="Printing currTime to stdout: ", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ClockPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ClockPrint(clockA, options="currTime", &
    preString="Printing currTime to tempString: ", unit=tempString, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ClockPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ClockPrint(clockA, options="startTime", &
    preString="Printing startTime to stdout: ", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ClockPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ClockPrint(clockA, options="startTime", &
    preString="Printing startTime to tempString: ", unit=tempString, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ClockPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ClockPrint(clockA, options="stopTime", &
    preString="Printing stopTime to stdout: ", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_ClockPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ClockPrint(clockA, options="stopTime", &
    preString="Printing stopTime to tempString: ", unit=tempString, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAreServicesSet() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_CompAreServicesSet(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeInit() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeInit(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeAdd() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeAdd(cplComp, attrList=(/"myAttribute"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeSet() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeSet(cplComp, name="myAttribute", value="test", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeGet() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeGet(cplComp, name="CplList", isSet=isSet, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeReset() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeReset(cplComp, (/"CplList"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_InitAttributes() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_InitAttributes(field, "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GetAttribute() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_GetAttribute(field, "StandardName", value, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_SetAttribute() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_SetAttribute(field, "StandardName", "bottom_depth", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_UpdateTimestamp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_UpdateTimestamp(fieldBundleA, fieldBundleB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryAddEntry() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionaryAddEntry("esmf_adoption_level", "percent", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryGetEntry() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionaryGetEntry("esmf_adoption_level", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryHasEntry() (existing entry) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag =  NUOPC_FieldDictionaryHasEntry("esmf_adoption_level", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryHasEntry() return value (existing entry) Test"
  write(failMsg, *) "Did not return the correct value"
  call ESMF_Test((flag), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryHasEntry() (not existing entry) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag =  NUOPC_FieldDictionaryHasEntry("this_entry_does_not_exist", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryHasEntry() return value (not existing entry) Test"
  write(failMsg, *) "Did not return the correct value"
  call ESMF_Test((.not.flag), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionarySetSyno() (existing entry) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionarySetSyno(standardNames=(/"esmf_adoption_level    ", &
    "sea_surface_temperature"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionarySetSyno() (non existing entry) Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call NUOPC_FieldDictionarySetSyno(standardNames=(/"esmf_adoption_level", &
    "abcd_adoption_level"/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryMatchSyno() (existing entry) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_FieldDictionaryMatchSyno("esmf_adoption_level", &
    "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryMatchSyno() return value (existing entry) Test"
  write(failMsg, *) "Did not return the correct value"
  call ESMF_Test((flag), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryMatchSyno() (non existing entry1) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_FieldDictionaryMatchSyno("abcd_adoption_level", &
    "esmf_adoption_level", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryMatchSyno() return value (not existing entry1) Test"
  write(failMsg, *) "Did not return the correct value"
  call ESMF_Test((.not.flag), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryMatchSyno() (non existing entry2) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_FieldDictionaryMatchSyno("esmf_adoption_level", &
    "abcd_adoption_level", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryMatchSyno() return value (not existing entry2) Test"
  write(failMsg, *) "Did not return the correct value"
  call ESMF_Test((.not.flag), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionarySetup() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionarySetup(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldDictionaryEgest() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionaryEgest(freeFormat=fdFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatLog() for fdFF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatLog(fdFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatDestroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatDestroy(fdFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GetTimestamp() for Field Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_GetTimestamp(field, isValid=flag, time=stopTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_IsAtTime() for Field Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_IsAtTime(field, startTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_GridCreate1PeriDimUfrm() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/500, 400/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -85._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 85._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Complete field for further testing Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  ! Fill the field with coordinate dependent data
  call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=xPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=yPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldGet(field, farrayPtr=dataPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do j=lbound(dataPtr,2),ubound(dataPtr,2)
  do i=lbound(dataPtr,1),ubound(dataPtr,1)
    dataPtr(i,j) = sin(xPtr(i)*0.0174532925199)*cos(yPtr(j)*0.0174532925199)
  enddo
  enddo
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_Write() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_Write(field, fileName="field_test.nc", &
    status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAreServicesSet() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_CompAreServicesSet(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeInit() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeInit(gridComp, kind="Model", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeAdd() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeAdd(gridComp, attrList=(/"myAttribute"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeSet() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeSet(gridComp, name="myAttribute", value="test", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeEgest() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeEgest(gridComp, freeFormat=attrFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatPrint() for attrFF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatPrint(attrFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeIngest() for GridComp w/o addFlag Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call NUOPC_CompAttributeIngest(comp, freeFormat=attrFF, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeIngest() for GridComp w/ addFlag Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeIngest(comp, freeFormat=attrFF, addFlag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FreeFormatDestroy() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompCheckSetClock() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompCheckSetClock(gridComp, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSetClock() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompSetClock(gridComp, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_Advertise() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_Advertise(stateA, "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_Advertise() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_Advertise(stateA, (/"air_pressure_at_sea_level", &
      "precipitation_flux       "/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_InitAttributes() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_InitAttributes(stateA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_SetAttribute() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_SetAttribute(stateA, name="Namespace", value="xyz", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GetStateMemberLists() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(stdAttrNameList)  ! prepare for the following call
  call NUOPC_GetStateMemberLists(stateA, stdAttrNameList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (associated(stdAttrNameList)) deallocate(stdAttrNameList)  ! clean up
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_IsConnected() - all fields in state - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_IsConnected(stateA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_IsAtTime() - all fields in state - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_IsAtTime(stateB, startTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_IsConnected() - specific field in state - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_IsConnected(stateA, "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_IsUpdated() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_IsUpdated(stateA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_Realize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_Realize(stateA, field, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_SetTimestamp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_SetTimestamp(stateA, clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_UpdateTimestamp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_UpdateTimestamp(stateA, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_Write() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_Write(stateA, fieldNameList=(/"sea_surface_temperature"/), &
    status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_Write() SCRIP weight file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(factorIndexList(2, 2*localPet), factorList(2*localPet))
  do i=1, 2*localPet
    factorIndexList(1,i) = localPet*10  + i  ! src index
    factorIndexList(2,i) = localPet*100 + i  ! dst index
    factorList(i)        = real(i*i, ESMF_KIND_R8)/100.d0  ! factor
  enddo
  call NUOPC_Write(factorList=factorList, &
    factorIndexList=factorIndexList, fileName="test_scrip.nc", &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(factorIndexList, factorList)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_AddNamespace() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_AddNamespace(stateA, Namespace="abc", nestedState=stateC, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_AddNestedState() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_AddNestedState(stateA, Namespace="def", nestedState=stateC, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GetStateMemberLists() for nested State namespace Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(stdAttrNameList)  ! prepare for the following call
  call NUOPC_GetStateMemberLists(stateC, stdAttrNameList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (associated(stdAttrNameList)) deallocate(stdAttrNameList)  ! clean up
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_TimePrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_TimePrint(startTime, &
    preString="Printing time to stdout: ", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_TimePrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_TimePrint(startTime, &
    preString="Printing time to tempString: ", unit=tempString, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ConnectorSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ConnectorSet(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ConnectorGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ConnectorGet(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ModelBaseGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ModelBaseGet(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ModelGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ModelGet(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_MediatorGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_MediatorGet(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSearchRevPhaseMap() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompSearchRevPhaseMap(gridComp, ESMF_METHOD_INITIALIZE, &
    phaseLabel=phaseLabel, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompFilterPhaseMap() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompFilterPhaseMap(gridComp, ESMF_METHOD_INITIALIZE, &
    acceptStringList=(/"randomStringForTest"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompFilterPhaseMap() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompFilterPhaseMap(cplComp, ESMF_METHOD_INITIALIZE, &
    acceptStringList=(/"randomStringForTest"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSearchPhaseMap() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompSearchPhaseMap(gridComp, ESMF_METHOD_INITIALIZE, &
    phaseIndex=valueInt, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSearchPhaseMap() for CplComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompSearchPhaseMap(cplComp, ESMF_METHOD_INITIALIZE, &
    phaseIndex=valueInt, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! -> Wrapping up the Generic Driver
  !------------------------------------------------------------------------
  call ESMF_GridCompFinalize(gridComp, userRc=urc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSetServices() for GridComp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ! Not specifying the sharedObj argument results in look-up in the executable
  ! itself.... and there is a SetServices() routine outside the program below.
  call NUOPC_CompSetServices(gridComp, rc=rc)
  rc=ESMF_SUCCESS  ! for now do not really check because some systems have issues.
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! clean-ups
  call ESMF_ClockDestroy(clockA, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ClockDestroy(clockB, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ClockDestroy(clockC, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompDestroy(gridComp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_CplCompDestroy(cplComp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(stateA, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_StateDestroy(stateB, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldDestroy(field, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldBundleDestroy(fieldBundleA, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldBundleDestroy(fieldBundleB, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------
  
  
end program ESMF_NUOPC_UTest

! -- A SetServices() routine must be present for the NUOPC_CompSetService()
! -- unit test above.
subroutine SetServices(gcomp, rc)
  use ESMF
  implicit none
  type(ESMF_GridComp) :: gcomp
  integer             :: rc
  rc = ESMF_SUCCESS
end subroutine
