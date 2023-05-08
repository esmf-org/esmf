! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_HConfigUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_HConfigUTest - This unit test file tests HConfig class and
!   methods.
! !DESCRIPTION:
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
  type(ESMF_VM)     :: vm
  integer           :: i, j, petCount, localPet
  type(ESMF_HConfig):: hconfig, hconfig2, hconfig3
  type(ESMF_HConfigIter):: hconfigIter, hconfigIterEnd
  type(ESMF_HConfigIter):: hconfigIter2, hconfigIter2End
  type(ESMF_HConfigIter):: hconfigIter3, hconfigIter3End
  type(ESMF_Config) :: config1, config2
  logical           :: compareOK
  integer           :: intVar1, intVar2, count1, count2
  real(ESMF_KIND_R4):: r4Var1, r4Var2
  real(ESMF_KIND_R8):: r8Var1, r8Var2
  character(80)     :: strVar1, strVar2
  real              :: realList1(5), realList2(5)
  character(80)     :: strList1(3), strList2(3)
  real              :: realTable1(7,3), realTable2(7,3)

  logical :: raw = .false. ! switch ConfigLog() into "raw" mode or not

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

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigCreate() Empty"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  hconfig = ESMF_HConfigCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigSet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigSet(hconfig, content="[1, 2, 3]", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfig Iterator test after HConfigSet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call HConfigIterationTest(hconfig, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigLoadFile()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigLoadFile(hconfig, fileName="sample.yaml", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfig Iterator test after HConfigLoadFile()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call HConfigIterationTest(hconfig, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test HConfig"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigDestroy(hconfig, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigCreate() with Load content"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  hconfig = ESMF_HConfigCreate(content="[abc, TRUE, 123]", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test HConfig"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigDestroy(hconfig, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigCreate() with LoadFile fileName"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  hconfig = ESMF_HConfigCreate(fileName="sample.yaml", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test HConfig"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigDestroy(hconfig, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ConfigCreate()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  config1 = ESMF_ConfigCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ConfigLoadFile() from sample.rc"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigLoadFile(config1, fileName="sample.rc", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ConfigLog() from sample.rc"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigLog(config1, prefix="RC LoadFile:", raw=raw, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ConfigCreate()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  config2 = ESMF_ConfigCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ConfigLoadFile() from sample.yaml"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigLoadFile(config2, fileName="sample.yaml", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ConfigLog() from sample.yaml"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigLog(config2, prefix="YAML LoadFile:", raw=raw, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare values in Config from sample.rc vs sampe.yaml"
  write(failMsg, *) "Not all values matching"

  compareOK = .true.  ! assume success until found otherwise

  call ESMF_ConfigGetAttribute(config1, intVar1, label="parameter_1:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, intVar2, label="parameter_1:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (intVar1 /= intVar2) compareOK = .false.

  call ESMF_ConfigGetAttribute(config1, r4Var1, label="parameter_2:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, r4Var2, label="parameter_2:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (r4Var1 /= r4Var2) compareOK = .false.

  call ESMF_ConfigGetAttribute(config1, r8Var1, label="parameter_2:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, r8Var2, label="parameter_2:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (r8Var1 /= r8Var2) compareOK = .false.

  call ESMF_ConfigFindLabel(config1, "my_file_names:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  count1 = ESMF_ConfigGetLen(config1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigFindLabel(config2, "my_file_names:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  count2 = ESMF_ConfigGetLen(config2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (count1 /= 3) compareOK = .false.
  if (count1 /= count2) compareOK = .false.

  call ESMF_ConfigFindLabel(config1, "my_file_names:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigFindLabel(config2, "my_file_names:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do i=1, count1
    call ESMF_ConfigGetAttribute(config1, strVar1, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ConfigGetAttribute(config2, strVar1, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (strVar1 /= strVar1) compareOK = .false.
  enddo

  call ESMF_ConfigGetAttribute(config1, realList1, count=2, &
    label="constants:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, realList2, count=2, &
    label="constants:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do i=1, 2
    if (realList1(i) /= realList2(i)) compareOK = .false.
  enddo

  call ESMF_ConfigGetAttribute(config1, strList1, count=3, &
    label="my_favorite_colors:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, strList2, count=3, &
    label="my_favorite_colors:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do i=1, 3
    if (strList1(i) /= strList2(i)) compareOK = .false.
  enddo

  call ESMF_ConfigFindLabel(config1, "my_table_name::", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do i = 1, 7
    call ESMF_ConfigNextLine(config1, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do j = 1, 3
      call ESMF_ConfigGetAttribute(config1, realTable1(i,j), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
  enddo

  call ESMF_ConfigFindLabel(config2, "my_table_name::", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  do i = 1, 7
    call ESMF_ConfigNextLine(config2, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do j = 1, 3
      call ESMF_ConfigGetAttribute(config2, realTable2(i,j), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
  enddo

  if (any(realTable1 /= realTable2)) compareOK = .false.

  call ESMF_Test((compareOK), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set I4 Attribute in Config from sample.rc"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigSetAttribute(config1, 1234, label="new_integer:", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set I4 Attribute in Config from sample.yaml"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigSetAttribute(config2, 1234, label="new_integer:", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set String Attribute in Config from sample.rc"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigSetAttribute(config1, "a silly string", label="new_string:", &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set String Attribute in Config from sample.yaml"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigSetAttribute(config2, "a silly string", label="new_string:", &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Compare values after Attrbutes added"
  write(failMsg, *) "Not all values matching"

  compareOK = .true.  ! assume success until found otherwise

  call ESMF_ConfigGetAttribute(config1, intVar1, label="new_integer:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, intVar2, label="new_integer:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (intVar1 /= intVar2) compareOK = .false.

  call ESMF_ConfigGetAttribute(config1, strVar1, label="new_string:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config2, strVar2, label="new_string:", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (strVar1 /= strVar2) compareOK = .false.

  call ESMF_Test((compareOK), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test Config from sample.rc"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigDestroy(config1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test Config from sample.yaml"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ConfigDestroy(config2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

 contains
 
  subroutine HConfigIterationTest(hconfig, rc)
    type(ESMF_HConfig)    :: hconfig
    integer, intent(out)  :: rc

    logical                       :: flag, valueL
    logical                       :: asOkayL, asOkayS, asOkayI4, asOkayI8
    logical                       :: asOkayR4, asOkayR8
    character(len=:), allocatable :: string, tag
    character(160)                :: msgString
    integer(ESMF_KIND_I4)         :: valueI4
    integer(ESMF_KIND_I8)         :: valueI8
    real(ESMF_KIND_R4)            :: valueR4
    real(ESMF_KIND_R8)            :: valueR8
    integer                       :: i, size

    rc = ESMF_SUCCESS

    size = ESMF_HConfigGetSize(hconfig, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    write(msgString, *) "Size: ", size
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    flag = ESMF_HConfigIsSequence(hconfig, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    if (flag) then
      do i=1, size
        ! sequence element as I4
        valueI4 = ESMF_HConfigAsI4(hconfig, index=i, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        write(msgString, *) "I4: ", valueI4
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        ! sequence element as I4 through CreateAt()
        hconfig2 = ESMF_HConfigCreateAt(hconfig, index=i, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        valueI4 = ESMF_HConfigAsI4(hconfig2, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        write(msgString, *) "I4 through CreateAt(): ", valueI4
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        call ESMF_HConfigDestroy(hconfig2, rc=rc)
        if (rc /= ESMF_SUCCESS) return
      enddo
    else
      ! direct access via key, here through CreateAt()
      hconfig2 = ESMF_HConfigCreateAt(hconfig, keyString="radius_of_the_earth", &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueR4 = ESMF_HConfigAsR4(hconfig2, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "R4 'radius_of_the_earth' through CreateAt(): ", &
        valueR4
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      call ESMF_HConfigDestroy(hconfig2, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      ! direct access via key, here directly through AsR8(keyString=...)
      valueR8 = ESMF_HConfigAsR8(hconfig, keyString="radius_of_the_earth", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "R8 'radius_of_the_earth' through AsR8(keyString=...): ", &
        valueR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      ! direct access via key, through AsString(keyString=...)
      ! value_one
      string = ESMF_HConfigAsString(hconfig, keyString="value_one", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      tag = ESMF_HConfigGetTag(hconfig, keyString="value_one", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueL = ESMF_HConfigAsLogical(hconfig, keyString="value_one", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "String 'value_one' through AsString(keyString=...): ", &
        string, "  [", tag, "] through AsLogical(keyString=...):", valueL
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      string = ESMF_HConfigAsString(hconfig, keyString="value_one", asOkay=asOkayS, &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueL = ESMF_HConfigAsLogical(hconfig, keyString="value_one", asOkay=asOkayL, &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueI4 = ESMF_HConfigAsI4(hconfig, keyString="value_one", asOkay=asOkayI4, &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueI8 = ESMF_HConfigAsI8(hconfig, keyString="value_one", asOkay=asOkayI8, &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueR4 = ESMF_HConfigAsR4(hconfig, keyString="value_one", asOkay=asOkayR4, &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueR8 = ESMF_HConfigAsR8(hconfig, keyString="value_one", asOkay=asOkayR8, &
        rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "'value_one'", &
        " asOkayS: ", asOkayS, " asOkayL: ", asOkayL, &
        " asOkayI4: ", asOkayI4, " asOkayI8: ", asOkayI8, &
        " asOkayR4: ", asOkayR4, " asOkayR8: ", asOkayR8
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      ! value_two
      string = ESMF_HConfigAsString(hconfig, keyString="value_two", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      tag = ESMF_HConfigGetTag(hconfig, keyString="value_two", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      valueL = ESMF_HConfigAsLogical(hconfig, keyString="value_two", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "String 'value_two' through AsString(keyString=...): ", &
        string, "  [", tag, "] through AsLogical(keyString=...):", valueL
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      ! value_three
      string = ESMF_HConfigAsString(hconfig, keyString="value_three", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      tag = ESMF_HConfigGetTag(hconfig, keyString="value_three", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "String 'value_three' through AsString(keyString=...): ", &
        string, "  [", tag, "]"
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
      ! value_four
      string = ESMF_HConfigAsString(hconfig, keyString="value_four", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      tag = ESMF_HConfigGetTag(hconfig, keyString="value_four", rc=rc)
      if (rc /= ESMF_SUCCESS) return
      write(msgString, *) "String 'value_four' through AsString(keyString=...): ", &
        string, "  [", tag, "]"
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) return
    endif

    hconfigIter = ESMF_HConfigIterBegin(hconfig, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    hconfigIterEnd = ESMF_HConfigIterEnd(hconfig, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    do while(hconfigIter /= hconfigIterEnd)

      flag = ESMF_HConfigIterIsSequence(hconfigIter, rc=rc)
      if (rc /= ESMF_SUCCESS) return

      if (flag) then
        ! sequence iteration
        size = ESMF_HConfigGetSize(hconfigIter, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        write(msgString, *) "Size: ", size
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) return

        flag = ESMF_HConfigIsScalar(hconfigIter, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        if (flag) then
          ! as string
          string = ESMF_HConfigAsString(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          call ESMF_LogWrite("String: "//string, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          ! as string through CreateAt()
          hconfig3 = ESMF_HConfigCreateAt(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          string = ESMF_HConfigAsString(hconfig3, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          call ESMF_LogWrite("String through CreateAt(): "//string, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          call ESMF_HConfigDestroy(hconfig3, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          ! as I4
          valueI4 = ESMF_HConfigAsI4(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          write(msgString, *) "I4: ", valueI4
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          ! as I8
          valueI8 = ESMF_HConfigAsI8(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          write(msgString, *) "I8: ", valueI8
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          ! as R4
          valueR4 = ESMF_HConfigAsR4(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          write(msgString, *) "R4: ", valueR4
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          ! as R8
          valueR8 = ESMF_HConfigAsR8(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          write(msgString, *) "R8: ", valueR8
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
        endif
      else
        ! map iteration
        size = ESMF_HConfigGetSizeMapKey(hconfigIter, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        write(msgString, *) "Size: ", size
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) return

        flag = ESMF_HConfigIsScalarMapKey(hconfigIter, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        if (flag) then
          string = ESMF_HConfigAsStringMapKey(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          tag = ESMF_HConfigGetTagMapKey(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          write(msgString, *) "MapKey string: ", string, "  [", tag, "]"
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
        endif

        size = ESMF_HConfigGetSizeMapVal(hconfigIter, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        write(msgString, *) "Size: ", size
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) return

        flag = ESMF_HConfigIsScalarMapVal(hconfigIter, rc=rc)
        if (rc /= ESMF_SUCCESS) return
        if (flag) then
          string = ESMF_HConfigAsStringMapVal(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          tag = ESMF_HConfigGetTagMapVal(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          write(msgString, *) "MapVal string: ", string, "  [", tag, "]"
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) return
        else
          hconfigIter2 = ESMF_HConfigIterBeginMapVal(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return
          hconfigIter2End = ESMF_HConfigIterEndMapVal(hconfigIter, rc=rc)
          if (rc /= ESMF_SUCCESS) return

          do while(hconfigIter2 /= hconfigIter2End)
            flag = ESMF_HConfigIsScalar(hconfigIter2, rc=rc)
            if (rc /= ESMF_SUCCESS) return
            if (flag) then
              string = ESMF_HConfigAsString(hconfigIter2, rc=rc)
              if (rc /= ESMF_SUCCESS) return
              tag = ESMF_HConfigGetTag(hconfigIter2, rc=rc)
              if (rc /= ESMF_SUCCESS) return
              write(msgString, *) "String: ", string, "  [", tag, "]"
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
              if (rc /= ESMF_SUCCESS) return
            else

              hconfigIter3 = ESMF_HConfigIterBegin(ESMF_HConfigIterAsHConfig(hconfigIter2), rc=rc)
              if (rc /= ESMF_SUCCESS) return
              hconfigIter3End = ESMF_HConfigIterEnd(ESMF_HConfigIterAsHConfig(hconfigIter2), rc=rc)
              if (rc /= ESMF_SUCCESS) return

              do while(hconfigIter3 /= hconfigIter3End)
                flag = ESMF_HConfigIsScalar(hconfigIter3, rc=rc)
                if (rc /= ESMF_SUCCESS) return
                if (flag) then
                  string = ESMF_HConfigAsString(hconfigIter3, rc=rc)
                  if (rc /= ESMF_SUCCESS) return
                  tag = ESMF_HConfigGetTag(hconfigIter3, rc=rc)
                  if (rc /= ESMF_SUCCESS) return
                  write(msgString, *) "String: ", string, "  [", tag, "]"
                  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
                  if (rc /= ESMF_SUCCESS) return
                endif
                call ESMF_HConfigIterNext(hconfigIter3, rc=rc)
                if (rc /= ESMF_SUCCESS) return
              enddo
            endif
            call ESMF_HConfigIterNext(hconfigIter2, rc=rc)
            if (rc /= ESMF_SUCCESS) return
          enddo
        endif
      endif

      call ESMF_HConfigIterNext(hconfigIter, rc=rc)
      if (rc /= ESMF_SUCCESS) return
    enddo

  end subroutine

end program ESMF_HConfigUTest
