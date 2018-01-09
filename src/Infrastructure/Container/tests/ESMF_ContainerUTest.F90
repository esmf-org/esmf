! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ContainerUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ContainerUTest - This unit test file tests ESMF_Container
!  methods
!
! !DESCRIPTION:
!
! The ESMF_Container class is an internally used utility class
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF
  use ESMF_TestMod      ! test methods

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc, stat

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! type definitions
  type TestTypeStruct
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    character(len=120)            :: string
    integer                       :: index
    type(ESMF_Field)              :: field
  end type

  type TestType
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    type(TestTypeStruct), pointer :: wrap
  end type

  !LOCAL VARIABLES:
  type(ESMF_Container)            :: container
  type(ESMF_Field), allocatable   :: fieldList(:)
  type(ESMF_Field)                :: field, fieldOut
  type(ESMF_Field), pointer       :: fieldListOut(:)
  type(ESMF_Field), pointer       :: fieldGarbageList(:)
  integer, allocatable            :: fieldListOutIndexMap(:)
  character(ESMF_MAXSTR)          :: iString
  character(ESMF_MAXSTR)          :: fieldName
  integer, parameter              :: fieldCount = 5
  integer                         :: fieldCountOut
  integer                         :: i
  integer                         :: garbageCount, itemCount
  logical                         :: isPresent, loopResult
  type(TestType)                  :: tt
  
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
  !NEX_UTest
  write(name, *) "Container Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  container = ESMF_ContainerCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !- prepare fieldList
  allocate(fieldList(fieldCount))
  do i=1, fieldCount
    write(iString, *) i
    fieldList(i) = ESMF_FieldEmptyCreate(name="testField"//&
      trim(adjustl(iString)), rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add Field Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=fieldList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add Field 2nd time strict Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=fieldList, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add Field 2nd time relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=fieldList, relaxedflag=.true., &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container turn garbage feature ON Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageOn(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container clear garbage Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageClear(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add Field again relaxed - with garbage ON Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=fieldList, relaxedflag=.true., &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container garbage Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(fieldGarbageList)
  call ESMF_ContainerGarbageGet(container, garbageList=fieldGarbageList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify garbageList Test"
  write(failMsg, *) "garbageList incorrect"
  call ESMF_Test((size(fieldGarbageList)==fieldCount), name, failMsg, result, ESMF_SRCLINE)

  print *, "size of fieldGarbageList: ", size(fieldGarbageList)  
  do i=1, size(fieldGarbageList)
    call ESMF_FieldGet(fieldGarbageList(i), name=fieldName, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "fieldGarbageList(",i,")=",fieldName
  enddo
  
  if (associated(fieldGarbageList)) deallocate(fieldGarbageList)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container turn garbage feature OFF Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageOff(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container clear garbage Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageClear(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Print Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerPrint(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField3", item=field, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !- get name out of queried Field object in preparation of verify test
  call ESMF_FieldGet(field, name=fieldName, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item Test"
  write(failMsg, *) "fieldName incorrect"
  call ESMF_Test((trim(fieldName)=="testField3"), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get isPresent Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField3", &
    isPresent=isPresent, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get isPresent Test"
  write(failMsg, *) "isPresent incorrect"
  call ESMF_Test((isPresent.eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  call ESMF_ContainerGarbageOn(container, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ContainerGarbageClear(container, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"testField3"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container garbage Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(fieldGarbageList)
  call ESMF_ContainerGarbageGet(container, garbageList=fieldGarbageList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify garbageList Test"
  write(failMsg, *) "garbageList incorrect"
  call ESMF_Test((size(fieldGarbageList)==1), name, failMsg, result, ESMF_SRCLINE)

  print *, "size of fieldGarbageList: ", size(fieldGarbageList)  
  do i=1, size(fieldGarbageList)
    call ESMF_FieldGet(fieldGarbageList(i), name=fieldName, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "fieldGarbageList(",i,")=",fieldName
  enddo
  
  if (associated(fieldGarbageList)) deallocate(fieldGarbageList)

  !------------------------------------------------------------------------
  call ESMF_ContainerGarbageOff(container, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ContainerGarbageClear(container, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item 2nd time strict Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"testField3"/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item 2nd time relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"testField3"/), &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Print Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerPrint(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get isPresent Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField3", &
    isPresent=isPresent, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get isPresent Test"
  write(failMsg, *) "isPresent incorrect"
  call ESMF_Test((isPresent.eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get count Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemCount=fieldCountOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldCountOut Test"
  write(failMsg, *) "fieldCountOut incorrect"
  call ESMF_Test(fieldCountOut==4, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace Field with one not present strict Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerReplace(container, itemList=fieldList, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace Field with one not present relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerReplace(container, itemList=fieldList, &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container AddReplace Field Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAddReplace(container, itemList=fieldList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace Field with all present strict Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerReplace(container, itemList=fieldList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Print Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerPrint(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(fieldListOut)
  call ESMF_ContainerGet(container, itemList=fieldListOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==5, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    loopResult = .true. ! initialize
    do i=1, size(fieldListOut)
      if (fieldListOut(i)/=fieldList(i)) loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemList=fieldListOut, &
    itemorderflag=ESMF_ITEMORDER_ABC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==5, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    loopResult = .true. ! initialize
    do i=1, size(fieldListOut)
      if (fieldListOut(i)/=fieldList(i)) loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemList=fieldListOut, &
    itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==5, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  ! the 3rd item was removed and later replaceadd'ed, putting it last
  allocate(fieldListOutIndexMap(fieldCount))
  fieldListOutIndexMap(1) = 1
  fieldListOutIndexMap(2) = 2
  fieldListOutIndexMap(3) = 4
  fieldListOutIndexMap(4) = 5
  fieldListOutIndexMap(5) = 3
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    loopResult = .true. ! initialize
    do i=1, size(fieldListOut)
      if (fieldListOut(i)/=fieldList(fieldListOutIndexMap(i))) &
        loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOutIndexMap)

  !------------------------------------------------------------------------
  ! final garbage collection
  do i=1, size(fieldListOut)
    call ESMF_FieldGet(fieldListOut(i), name=fieldName, rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "fieldListOut(",i,")=",fieldName
    call ESMF_FieldDestroy(fieldListOut(i), rc=rc)
    if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

  if (allocated(fieldList)) deallocate(fieldList)
  if (associated(fieldListOut)) deallocate(fieldListOut)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerDestroy(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  container = ESMF_ContainerCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  field = ESMF_FieldEmptyCreate(name="testField1", rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add Field Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=(/field/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField1", item=fieldOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add same Field without multiflag Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=(/field/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add same Field with multiflag Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=(/field/), multiflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item from multiple item container Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField1", item=fieldOut, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get isPresent from multiple item container Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField1", isPresent=isPresent,&
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item from multiple item container relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"testField1"/), &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item from multiple item container Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"testField1"/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item from multiple item container multi Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"testField1"/), &
    multiflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add Field after multi Remove Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=(/field/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add same Field two more times with multiflag Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=(/field, field/), &
    multiflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace Field in multiple item container relaxed Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerReplace(container, itemList=(/field/), &
    relaxedflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace Field in multiple item container strict Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ContainerReplace(container, itemList=(/field/), rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  allocate(fieldList(1))
  fieldList(1) = ESMF_FieldEmptyCreate(name="testField0", rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ContainerAdd(container, itemList=fieldList, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container itemCount Test"
  write(failMsg, *) "fieldCountOut incorrect"
  call ESMF_ContainerGet(container, itemCount=fieldCountOut, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test(fieldCountOut==4, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container itemCount with itemName Test"
  write(failMsg, *) "fieldCountOut incorrect"
  call ESMF_ContainerGet(container, itemName="testField1", &
    itemCount=fieldCountOut, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test(fieldCountOut==3, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList for itemName Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(fieldListOut)
  call ESMF_ContainerGet(container, itemName="testField1", &
    itemList=fieldListOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==fieldCountOut, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    loopResult = .true. ! initialize
    do i=1, size(fieldListOut)
      if (fieldListOut(i)/=field) loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList for itemName w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField1", &
    itemorderflag=ESMF_ITEMORDER_ABC, itemList=fieldListOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==fieldCountOut, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    loopResult = .true. ! initialize
    do i=1, size(fieldListOut)
      if (fieldListOut(i)/=field) loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList for itemName w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemName="testField1", &
    itemorderflag=ESMF_ITEMORDER_ADDORDER, itemList=fieldListOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==fieldCountOut, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    loopResult = .true. ! initialize
    do i=1, size(fieldListOut)
      if (fieldListOut(i)/=field) loopResult = .false.
    enddo
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace Field in multiple item container multi Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerReplace(container, itemList=(/field/), multiflag=.true., &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container itemCount Test"
  write(failMsg, *) "fieldCountOut incorrect"
  call ESMF_ContainerGet(container, itemCount=fieldCountOut, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test(fieldCountOut==2, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemList=fieldListOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==2, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    if (size(fieldListOut)==2) then
      loopResult = .true. ! initialize
      ! default order will be alphabetical
      if (fieldListOut(1)/=fieldList(1)) loopResult = .false. ! "testField0"
      if (fieldListOut(2)/=field) loopResult = .false.        ! "testField1"
    endif
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)
  deallocate (fieldListOut)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemList=fieldListOut, &
    itemorderflag=ESMF_ITEMORDER_ABC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==2, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) w/ ESMF_ITEMORDER_ABC Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    if (size(fieldListOut)==2) then
      loopResult = .true. ! initialize
      ! explicitly requested order to be alphabetical
      if (fieldListOut(1)/=fieldList(1)) loopResult = .false. ! "testField0"
      if (fieldListOut(2)/=field) loopResult = .false.        ! "testField1"
    endif
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get fieldList w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemList=fieldListOut, &
    itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (associated) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut not associated"
  call ESMF_Test(associated(fieldListOut), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (size) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut not right size"
  if (associated(fieldListOut)) then
    call ESMF_Test(size(fieldListOut)==2, name, failMsg, result, ESMF_SRCLINE)
  else
    call ESMF_Test(.false., name, failMsg, result, ESMF_SRCLINE)
  endif
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get fieldListOut (item) w/ ESMF_ITEMORDER_ADDORDER Test"
  write(failMsg, *) "fieldListOut does not contain correct Fields"
  loopResult = .false. ! initialize
  if (associated(fieldListOut)) then
    if (size(fieldListOut)==2) then
      loopResult = .true. ! initialize
      ! explicitly requested order of how items were added
      if (fieldListOut(1)/=field) loopResult = .false.        ! "testField1"
      if (fieldListOut(2)/=fieldList(1)) loopResult = .false. ! "testField0"
    endif
  endif
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  deallocate (fieldListOut)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerDestroy(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  call ESMF_FieldDestroy(field, rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (allocated(fieldList)) then
    do i=1, size(fieldList)
      call ESMF_FieldDestroy(fieldList(i), rc=rc)
      if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
    deallocate(fieldList)
  endif
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Create UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  container = ESMF_ContainerCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container turn garbage feature ON UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageOn(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  allocate(tt%wrap,stat=stat)
  if (stat/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  tt%wrap%string = "string in myUDT1"
  tt%wrap%index = 1
  tt%wrap%field = ESMF_FieldEmptyCreate(name="field in myUDT1", rc=rc)
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add UDT (user derived type) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAddUDT(container, trim("myUDT1"), tt, rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  allocate(tt%wrap,stat=stat)
  if (stat/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  tt%wrap%string = "string in myUDT2"
  tt%wrap%index = 2
  field = ESMF_FieldEmptyCreate(name="field in myUDT2", rc=rc)
  tt%wrap%field = field
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add 2nd UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAddUDT(container, trim("myUDT2"), tt, rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Remove item UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerRemove(container, itemNameList=(/"myUDT1"/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(tt%wrap)
  call ESMF_ContainerGetUDT(container, trim("myUDT2"), tt, rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item - string - UDT Test"
  write(failMsg, *) "string incorrect"
  call ESMF_Test(trim(tt%wrap%string)=="string in myUDT2", name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item - index - UDT Test"
  write(failMsg, *) "index incorrect"
  call ESMF_Test(tt%wrap%index==2, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item - field - UDT Test"
  write(failMsg, *) "field incorrect"
  call ESMF_Test(tt%wrap%field==field, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  allocate(tt%wrap,stat=stat)
  if (stat/=0) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  tt%wrap%string = "string in myUDT2 replacement"
  tt%wrap%index = 20
  field = ESMF_FieldEmptyCreate(name="field in myUDT2 replacement", rc=rc)
  tt%wrap%field = field
  if (rc/=ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Replace UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerReplaceUDT(container, trim("myUDT2"), tt, rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item UDT 2nd Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  nullify(tt%wrap)
  call ESMF_ContainerGetUDT(container, trim("myUDT2"), tt, rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item - string - UDT 2nd Test"
  write(failMsg, *) "string incorrect"
  call ESMF_Test(trim(tt%wrap%string)=="string in myUDT2 replacement", name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item - index - UDT 2nd Test"
  write(failMsg, *) "index incorrect"
  call ESMF_Test(tt%wrap%index==20, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item - field - UDT 2nd Test"
  write(failMsg, *) "field incorrect"
  call ESMF_Test(tt%wrap%field==field, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Print UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerPrint(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container garbage Get UDT before Clear() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageGet(container, garbageCount=garbageCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "before Clear() garbageCount=", garbageCount  
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get UDT before Clear() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemCount=itemCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "before Clear() itemCount=", itemCount  
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Clear UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerClear(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container garbage Get UDT after Clear() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGarbageGet(container, garbageCount=garbageCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "after Clear() garbageCount=", garbageCount  
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get UDT after Clear() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerGet(container, itemCount=itemCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "after Clear() itemCount=", itemCount  
  
  ! - Remove garbage before destroying the Container to prevent memory leaks.
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container garbage collection UDT Test"
  write(failMsg, *) "something went wrong"
  loopResult = .true. ! initialize
  do i=1, garbageCount
    call ESMF_ContainerGarbageGetUDT(container, i, tt, rc)
    if (rc/=ESMF_SUCCESS) then
      loopResult = .false.
      exit
    endif
    deallocate(tt%wrap, stat=stat)
    if (stat/=0) then
      loopResult = .false.
      exit
    endif
  enddo
  call ESMF_Test(loopResult, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Destroy UDT Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerDestroy(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ContainerUTest
