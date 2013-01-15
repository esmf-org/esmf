! $Id: ESMF_NUOPC_UTest.F90,v 1.3 2012/11/20 06:51:42 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2013, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_NUOPC_UTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"
#include "ESMF_Macros.inc"

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

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_NUOPC_UTest.F90,v 1.3 2012/11/20 06:51:42 theurich Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_VM)           :: vm
  integer                 :: petCount, localPet
  type(ESMF_Time)         :: startTime, stopTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock)        :: clockA, clockB
  type(ESMF_GridComp)     :: gridComp
  type(ESMF_CplComp)      :: cplComp
  logical                 :: flag
  type(ESMF_State)        :: stateA, stateB
  type(ESMF_Field)        :: field
  character(ESMF_MAXSTR)  :: value
  type(ESMF_FieldBundle)  :: fieldBundleA, fieldBundleB
  character(ESMF_MAXSTR)  :: cplList(10)
  integer                 :: count
  type(ESMF_Grid)         :: grid
  character(ESMF_MAXSTR), pointer   :: stdAttrNameList(:)
  type(NUOPC_RunSequence), pointer  :: runSeq(:)
  type(NUOPC_RunElement),  pointer  :: runE
  
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
  call ESMF_TimeSet(stopTime, s = 60, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeIntervalSet(timeStep, s = 10, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  clockA = ESMF_ClockCreate(name="TestClock A", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  clockB = ESMF_ClockCreate(name="TestClock B", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  gridComp = ESMF_GridCompCreate(name="TestGridComp", clock=clockA, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  cplComp = ESMF_CplCompCreate(name="TestCplComp", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  stateA = ESMF_StateCreate(name="TestState A", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  stateB = ESMF_StateCreate(name="TestState B", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  field = ESMF_FieldEmptyCreate(name="sst", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldBundleA = ESMF_FieldBundleCreate(name="TestFieldBundle A", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fieldBundleB = ESMF_FieldBundleCreate(name="TestFieldBundle B", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! -> NUOPC Utility methods
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ClockCheckSetClock() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ClockCheckSetClock(clockA, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ClockInitialize() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  clockB = NUOPC_ClockInitialize(clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ClockPrintCurrTime() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ClockPrintCurrTime(clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ClockPrintStartTime() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ClockPrintStartTime(clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_ClockPrintStopTime() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_ClockPrintStopTime(clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CplCompAreServicesSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_CplCompAreServicesSet(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CplCompAttributeAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CplCompAttributeAdd(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CplCompAttributeGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CplCompAttributeGet(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CplCompAttributeSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CplCompAttributeSet(cplComp, stateA, stateB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldAttributeAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldAttributeAdd(field, "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldAttributeGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldAttributeGet(field, "StandardName", value, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldAttributeSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldAttributeSet(field, "StandardName", "bottom_depth", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldBundleUpdateTime() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldBundleUpdateTime(fieldBundleA, fieldBundleB, rc=rc)
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
  write(name, *) "NUOPC_FieldDictionarySetup() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionarySetup(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FillCplList() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FillCplList(stateA, stateB, cplList, count, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GridCompAreServicesSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_GridCompAreServicesSet(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GridCompAttributeAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_GridCompAttributeAdd(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GridCompCheckSetClock() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_GridCompCheckSetClock(gridComp, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GridCompSetClock() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_GridCompSetClock(gridComp, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GridCreateSimpleXY() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid = NUOPC_GridCreateSimpleXY( &
    0._ESMF_KIND_R8, 5.75_ESMF_KIND_R8, &
    -1.5_ESMF_KIND_R8, 2.0_ESMF_KIND_R8, &
    100, 100, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_IsCreated() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_IsCreated(clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateAdvertiseField() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateAdvertiseField(stateA, "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateBuildStdList() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(stdAttrNameList)  ! prepare for the following call
  call NUOPC_StateBuildStdList(stateA, stdAttrNameList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (associated(stdAttrNameList)) deallocate(stdAttrNameList)  ! clean up
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateIsAllConnected() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_StateIsAllConnected(stateA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateIsAtTime() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_StateIsAtTime(stateB, startTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateIsFieldConnected() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_StateIsFieldConnected(stateA, "sst", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateRealizeField() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateRealizeField(stateA, field, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateSetTimestamp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateSetTimestamp(stateA, clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateUpdateTimestamp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateUpdateTimestamp(stateA, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_TimePrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_TimePrint(startTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! -> NUOPC RunSequence methods
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequenceAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(runSeq)
  call NUOPC_RunSequenceAdd(runSeq, 1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunElementAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunElementAdd(runSeq(1), i=2, j=1, phase=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunElementPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunElementPrint(runSeq(1)%first, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequenceSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequenceSet(runSeq(1), clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequenceIterate() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(runE)
  flag = NUOPC_RunSequenceIterate(runSeq, runSeqIndex=1, runElement=runE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequencePrint() single element Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequencePrint(runSeq(1), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequencePrint() entire vector Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequencePrint(runSeq, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequenceSingleDeall() single element Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequenceDeallocate(runSeq(1), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_RunSequenceSingleDeall() entire vector Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequenceDeallocate(runSeq, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_NUOPC_UTest
