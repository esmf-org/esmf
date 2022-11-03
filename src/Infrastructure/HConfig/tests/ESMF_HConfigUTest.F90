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
  type(ESMF_HConfig):: hconfig
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
  write(name, *) "HConfigCreate()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  hconfig = ESMF_HConfigCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigLoad()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigLoad(hconfig, content="[1, 2, 3]", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "HConfigLoadFile()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_HConfigLoadFile(hconfig, fileName="sample.yaml", rc=rc)
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

end program ESMF_HConfigUTest
