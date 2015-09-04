! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2015, University Corporation for Atmospheric Research,
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
  type(ESMF_VM)           :: vm
  integer                 :: petCount, localPet
  type(ESMF_Time)         :: startTime, stopTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock)        :: clockA, clockB
  type(ESMF_GridComp)     :: gridComp
  type(ESMF_CplComp)      :: cplComp
  logical                 :: flag
  type(ESMF_State)        :: stateA, stateB, stateC
  type(ESMF_Field)        :: field
  character(ESMF_MAXSTR)  :: value
  type(ESMF_FieldBundle)  :: fieldBundleA, fieldBundleB
  type(ESMF_Grid)         :: grid
  integer                 :: i, j
  real(ESMF_KIND_R8),      pointer  :: xPtr(:,:), yPtr(:,:), dataPtr(:,:)
  character(ESMF_MAXSTR),  pointer  :: stdAttrNameList(:)
!TODO: completely remove after CSC okay:  type(NUOPC_RunSequence), pointer  :: runSeq(:)
!TODO: completely remove after CSC okay:  type(NUOPC_RunElement),  pointer  :: runE
  
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
  
  field = ESMF_FieldEmptyCreate(name="sea_surface_temperature", rc=rc)
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
  write(name, *) "NUOPC_CompAreServicesSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_CompAreServicesSet(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeInit() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeInit(cplComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeAdd(cplComp, attrList=(/"myAttribute"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeGet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeGet(cplComp, name="CplList", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldAttributeInit() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldAttributeInit(field, "sea_surface_temperature", rc=rc)
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
  call NUOPC_FieldDictionarySetSyno(standardNames=(/"esmf_adoption_level"/), &
    rc=rc)
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
  write(name, *) "NUOPC_FieldDictionarySetup() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldDictionarySetup(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldIsAtTime() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_FieldIsAtTime(field, startTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_GridCreateSimpleSph() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  grid = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
    360._ESMF_KIND_R8, 85._ESMF_KIND_R8, 500, 400, &
    scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
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
    dataPtr(i,j) = sin(xPtr(i,j)*0.0174532925199)*cos(yPtr(i,j)*0.0174532925199)
  enddo
  enddo
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_FieldWrite() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_FieldWrite(field, file="field_test.nc", &
    status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAreServicesSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_CompAreServicesSet(gridComp, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeInit() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeInit(gridComp, kind="Model", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompAttributeAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompAttributeAdd(gridComp, attrList=(/"myAttribute"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompCheckSetClock() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompCheckSetClock(gridComp, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSetClock() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_CompSetClock(gridComp, clockB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_CompSetServices() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ! Not specifying the sharedObj argument results in look-up in the executable
  ! itself.... and there is a SetServices() routine outside the program below.
  call NUOPC_CompSetServices(gridComp, rc=rc)
  rc=ESMF_SUCCESS  ! for now do not really check because some systems have issues.
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
  write(name, *) "NUOPC_StateAdvertiseFields() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateAdvertiseFields(stateA, &
    (/"air_pressure_at_sea_level", &
      "precipitation_flux       "/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateAttributeInit() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateAttributeInit(stateA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateAttributeSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateAttributeSet(stateA, name="Namespace", value="xyz", rc=rc)
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
  flag = NUOPC_StateIsFieldConnected(stateA, "sea_surface_temperature", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateIsUpdated() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  flag = NUOPC_StateIsUpdated(stateA, rc=rc)
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
  write(name, *) "NUOPC_StateWrite() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateWrite(stateA, fieldNameList=(/"sea_surface_temperature"/), &
    status=ESMF_FILESTATUS_REPLACE, relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateNamespaceAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_StateNamespaceAdd(stateA, namespace="abc", nestedState=stateC, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_StateBuildStdList() for nested State namespace Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(stdAttrNameList)  ! prepare for the following call
  call NUOPC_StateBuildStdList(stateC, stdAttrNameList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (associated(stdAttrNameList)) deallocate(stdAttrNameList)  ! clean up
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "NUOPC_TimePrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_TimePrint(startTime, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------


!TODO: completely remove after CSC okay:
#if 0
  !------------------------------------------------------------------------
  ! -> NUOPC RunSequence methods
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequenceAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(runSeq)
  call NUOPC_RunSequenceAdd(runSeq, 1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunElementAdd() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunElementAdd(runSeq(1), i=2, j=1, phase=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunElementAddComp() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunElementAddComp(runSeq(1), i=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunElementAddLink() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunElementAddLink(runSeq(1), slot=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunElementPrint() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunElementPrint(runSeq(1)%first, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequenceSet() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequenceSet(runSeq(1), clockA, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequenceIterate() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(runE)
  flag = NUOPC_RunSequenceIterate(runSeq, runSeqIndex=1, runElement=runE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequencePrint() single element Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequencePrint(runSeq(1), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequencePrint() entire vector Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequencePrint(runSeq, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequenceSingleDeall() single element Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequenceDeallocate(runSeq(1), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
 
  !------------------------------------------------------------------------
  !NEX_disabled_UTest
  write(name, *) "NUOPC_RunSequenceSingleDeall() entire vector Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call NUOPC_RunSequenceDeallocate(runSeq, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
#endif

  !------------------------------------------------------------------------
  ! clean-ups
  call ESMF_ClockDestroy(clockA, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ClockDestroy(clockB, rc=rc)
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
