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

#define ESMF_FILENAME "ESMF_InfoUTest.F90"

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================
!==============================================================================

program ESMF_InfoUTest

  !============================================================================
  !BOP
  ! !PROGRAM: ESMF_InfoUTest - Test general Info usage
  !
  ! !DESCRIPTION:
  !
  !----------------------------------------------------------------------------
  ! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_InfoMod

  implicit none

  !----------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = '$Id$'
  !----------------------------------------------------------------------------

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  character(ESMF_MAXSTR) :: key, key_i8, key_r4, key_r8, actual_char
  character(len=55) :: key_char, value_char, desired_char, def_desired_char, &
                       def_key_char, jsonType
  character(len=33) :: def_value_char
  character(len=22) :: key_empty_char
  character(len=2) :: desired_empty_char, empty_value_char
  character(ESMF_MAXSTR) :: to_parse
  character(:), allocatable :: actual_charalloc
  integer               :: rc, petCount, i
  integer, allocatable  :: petList(:)
  type(ESMF_VM)         :: vm
  type(ESMF_GridComp)   :: gcomp
  ! cumulative result: count failures; no failures equals "all pass"
  integer               :: result = 0, count, localPet

  integer(ESMF_KIND_I4) :: value, actual, actual2, actual3, arr_i4_get_count, &
                           actual4, ir4=0, implicit_i4
  integer(ESMF_KIND_I4), dimension(1) :: implicit_i4_list
  integer(ESMF_KIND_I8) :: desired_i8, value_i8
  real :: actual_rw_val, desired_rw_val
  real(ESMF_KIND_R4) :: desired_r4, value_r4
  real(ESMF_KIND_R8) :: desired_r8, value_r8
  real(ESMF_KIND_R8), dimension(3) :: values_r8
  integer(ESMF_KIND_I4), dimension(3) :: arr_i4, tk_value
  integer(ESMF_KIND_I4), dimension(:), allocatable :: arr_i4_get
  type(ESMF_Info) :: info, info2, info3, info4, info5, info6, &
                     info7, info8, info9, info10, info_copy_src, &
                     info_copy_dst, info_w, info_r, info_logical, &
                     info_types, info_obj_dst, info_obj_src, &
                     info_obj_new, info_parse, info_update_lhs, &
                     info_update_rhs, info_inq, info_eq_lhs, &
                     info_eq_rhs, irecurse, ipkey, info_charalloc, &
                     info_uninit, info_dirty, info_implicit, info_copy, &
                     info_cbk, info_cbk_base, info_tk
  logical :: is_present, failed, is_set, is_present_copy_test, actual_logical, &
             desired_logical, isArray, isDirty
  logical, dimension(2) :: fails_obj
  type(ESMF_TypeKind_Flag) :: actual_tk

  !----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  call ESMF_VMGetCurrent(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoCreate"
  write(failMsg, *) "Did not create Info"
  rc = ESMF_FAILURE

  info = ESMF_InfoCreate(rc=rc)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoDestroy"
  write(failMsg, *) "Did not destroy Info"
  rc = ESMF_FAILURE;

  call ESMF_InfoDestroy(info, rc=rc)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoSet"
  write(failMsg, *) "Did not set key"
  rc = ESMF_FAILURE

  info2 = ESMF_InfoCreate(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key = "testKey"
  actual = 333
  call ESMF_InfoSet(info2, key, actual, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_i8 = "testKeyI8"
  desired_i8 = HUGE(desired_i8)
  call ESMF_InfoSet(info2, key_i8, desired_i8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_r4 = "testKeyR4"
  desired_r4 = 333.0 + (1.0/3.0)
  call ESMF_InfoSet(info2, key_r4, desired_r4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_r8 = "testKeyR8"
  desired_r8 = 1.797693e-10
  call ESMF_InfoSet(info2, key_r8, desired_r8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_char = "testKeyChar"
  desired_char = "i am char"
  call ESMF_InfoSet(info2, key_char, desired_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetI4"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(info2, key, value, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((value == actual), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetI8"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(info2, key_i8, value_i8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((value_i8 == desired_i8), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetR4"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(info2, key_r4, value_r4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((ABS(value_r4 - desired_r4) < 1e-16), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetR8"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(info2, key_r8, value_r8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((ABS(value_r8 - desired_r8) < 1e-16), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetCH with Default"
  write(failMsg, *) "Did not get default key"

  def_desired_char = "this is default!"
  def_key_char = "does not exist in info"
  call ESMF_InfoGet(info2, def_key_char, def_value_char, default=def_desired_char, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((def_value_char==def_desired_char), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoSet/Get Empty Character"
  write(failMsg, *) "Did not set/get empty string key"

  key_empty_char = "so empty inside"
  desired_empty_char = ""
  call ESMF_InfoSet(info2, key_empty_char, desired_empty_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info2, key_empty_char, empty_value_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((desired_empty_char==empty_value_char), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetCH"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(info2, key_char, value_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((desired_char==value_char), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  ! Test we can overload the value if force is true
  write(name, *) "ESMF_InfoSet Force Flag"
  write(failMsg, *) "Could not overload value"
  rc = ESMF_FAILURE

  info3 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key = "testKey"
  actual = 333

  call ESMF_InfoSet(info3, "foobar", 123, force=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info3, "foobar", 123, force=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  ! Test creating and destroying a bunch of attributes
  rc = ESMF_FAILURE
  write(name, *) "Create+Destroy Loop"
  write(failMsg, *) "Failure during loop test"

  do i=1, 100000
    info4 = ESMF_InfoCreate(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(info4, "foobar", 123, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoDestroy(info4, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end do

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  ! Test get with a default value
  write(name, *) "ESMF_InfoGet with Default Value"
  write(failMsg, *) "Did not get default value"
  rc = ESMF_FAILURE

  info5 = ESMF_InfoCreate(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info5, "doesNotExist", actual2, rc=rc, default=5897)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual2 == 5897), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info5, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoPrint"
  write(failMsg, *) "Print somehow not successful"

  rc = ESMF_FAILURE

  info6 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info6, "/i/am/nested", 111, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info6, "top-level", 222, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  print *, ""
  print *, "===== ESMF_InfoPrint Test Start ====="
  print *, ""

  call ESMF_InfoPrint(info6, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoPrint(info6, indent=1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  print *, ""
  print *, "===== ESMF_InfoPrint Test End ====="
  print *, ""

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info6, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoRemove Child From Parent"
  write(failMsg, *) "Child not erased from parent"

  rc = ESMF_FAILURE

  info7 = ESMF_InfoCreate(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info7, "this/is/erase/test", 111, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoRemove(info7, "this/is/erase", keyChild="test", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info7, "/this/is/erase/test", actual3, &
                          default=-999, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual3 == -999), name, failMsg, result, ESMF_SRCLINE)

!  call ESMF_InfoPrint(info7, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoRemove Root"
  write(failMsg, *) "Did not erase from root"

  call ESMF_InfoRemove(info7, "this", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(info7, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info7, "this", actual3, default=-888, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual3 == -888), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info7, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoIsPresent False"
  write(failMsg, *) "Attribute key should not be present"

  info8 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present = ESMF_InfoIsPresent(info8, "this", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. is_present), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoIsPresent True"
  write(failMsg, *) "Attribute key is actually present"

  info8 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info8, "this", 11, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present = ESMF_InfoIsPresent(info8, "this", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((is_present), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoIsPresent True w/ Pointer"
  write(failMsg, *) "Attribute key is present using pointer syntax"

  info8 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info8, "/this/is/nested", 11, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present = ESMF_InfoIsPresent(info8, "/this/is/nested", &
    isPointer=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((is_present), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Array Setting/Getting"
  write(failMsg, *) "Array operations failed"
  failed = .false.

  arr_i4(1) = 123
  arr_i4(2) = 456
  arr_i4(3) = 789

  info9 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info9, "the-key", arr_i4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  i = 3
  call ESMF_InfoGetArrayMeta(info9, "the-key", isArray, i, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  failed = .not. isArray

  call ESMF_InfoGetAlloc(info9, "the-key", arr_i4_get, &
    itemcount=arr_i4_get_count, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Testing ESMF_InfoAssignment(=)()
  info = info9

  ! Testing ESMF_InfoOperator(/=)()
  if (info9 /= info) failed = .true.

  info = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Testing ESMF_InfoOperator(==)()
  if (info9 == info) failed = .true.

  do i=1, 3
    if (arr_i4(i) /= arr_i4_get(i)) then
      failed = .true.
    end if
  end do

  deallocate(arr_i4_get)

  call ESMF_InfoDestroy(info, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(info9, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. failed), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoSetNULL and ESMF_InfoIsSet"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  failed = .false.

  info10 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSetNULL(info10, "is-the-null", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_set = ESMF_InfoIsSet(info10, "is-the-null", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (is_set) then
    failed = .true.
  end if

  call ESMF_InfoSet(info10, "is-the-null", 5, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_set = ESMF_InfoIsSet(info10, "is-the-null", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (.not. is_set) then
    failed = .true.
  end if

  call ESMF_InfoDestroy(info10, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. failed), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoCreateFromInfo"
  write(failMsg, *) "Did not copy Info"
  rc = ESMF_FAILURE

  info_copy_src = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  info_copy_dst = ESMF_InfoCreate(info_copy_src, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_copy_dst, "a-key", 22, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present_copy_test = ESMF_InfoIsPresent(info_copy_src, "a-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(info_copy_src, rc=rc)
  call ESMF_InfoDestroy(info_copy_dst, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((is_present_copy_test .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoWrite & ESMF_InfoRead"
  write(failMsg, *) "Did not read/write Info"
  rc = ESMF_FAILURE

  info_w = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  desired_rw_val = 22.3
  call ESMF_InfoSet(info_w, "a-key", desired_rw_val, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (localPet == 0) then
    call ESMF_InfoWriteJSON(info_w, "test-esmf-info-write.json", rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  call ESMF_VMBarrier(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  info_r = ESMF_InfoReadJSON("test-esmf-info-write.json", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_r, "a-key", actual_rw_val, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(info_w, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(info_r, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((ABS(actual_rw_val-desired_rw_val) < 1e-16), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Logical Scalar"
  write(failMsg, *) "Did not set/get logical type"
  rc = ESMF_FAILURE

  info_logical = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  desired_logical = .false.
  call ESMF_InfoSet(info_logical, "logical-key", desired_logical, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(info_logical, 2)

  actual_logical = .true.  ! Set to true to make sure it is updated in the call
  call ESMF_InfoGet(info_logical, "logical-key", actual_logical, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(info_logical, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual_logical .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Type Checking"
  write(failMsg, *) "Did not catch type error"
  rc = ESMF_FAILURE

  info_types = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_types, "key", 111, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(info_types, 2)

  call ESMF_InfoSet(info_types, "key", 111.0, rc=rc)
  call ESMF_Test((rc==ESMC_RC_ARG_BAD), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_types, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Set/Get Object"
  write(failMsg, *) "Did not set/get Info object"
  rc = ESMF_FAILURE
  failed = .false.

  info_obj_dst = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  info_obj_src = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_obj_src, "src-key", 11167, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_obj_dst, "dst-key", info_obj_src, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  info_obj_new = ESMF_InfoCreate(info_obj_dst, "dst-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(info_obj_dst, 2)
!  call ESMF_InfoPrint(info_obj_new, 2)

  fails_obj(1) = ESMF_InfoIsPresent(info_obj_new, "dst-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_obj_src, "another-key", 44, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fails_obj(2) = ESMF_InfoIsPresent(info_obj_new, "another-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. any(fails_obj)), name, failMsg, result, ESMF_SRCLINE)

!  call ESMF_InfoPrint(info_obj_dst, 2)
!  call ESMF_InfoPrint(info_obj_new, 2)
!  print *, fails_obj

  call ESMF_InfoDestroy(info_obj_dst, rc=rc)
  call ESMF_InfoDestroy(info_obj_src, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create By Parse"
  write(failMsg, *) "Did not create Info by parse"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"hello": "fortran parser", "multiple-types": 55}'

  info_parse = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_parse, "multiple-types", actual4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual4==55), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoUpdate"
  write(failMsg, *) "Did not update"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"please": "update-me"}'
  info_update_lhs = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  to_parse = '{"i-am-new": 111}'
  info_update_rhs = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoUpdate(info_update_lhs, info_update_rhs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_update_lhs, "i-am-new", actual, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual==111), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_update_lhs, rc=rc)
  call ESMF_InfoDestroy(info_update_rhs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGet"
  write(failMsg, *) "Did not inquire right"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"ask": "questions please", "number": 1}'
  info_inq = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_inq, size=count, isArray=isArray, &
    isDirty=isDirty, jsonType=jsonType, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((trim(jsonType)=="object" .and. &
                 (count==2) .and. &
                 (isArray .eqv. .false.) .and. &
                 (isDirty .eqv. .false.)), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_inq, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoEqual/NotEqual Operator"
  write(failMsg, *) "Did not evaluate equality"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"ask": "questions please", "number": 1}'
  info_eq_lhs = ESMF_InfoCreate(to_parse, rc=rc)
  info_eq_rhs = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(((info_eq_lhs==info_eq_rhs) .and. &
    (.not. info_eq_lhs/=info_eq_rhs)), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_eq_lhs, rc=rc)
  call ESMF_InfoDestroy(info_eq_rhs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------
  
  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGet Recursive"
  write(failMsg, *) "Did not get recursive"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"ask": "questions please", "number": 1, "nest": {"a": 5}}'
  irecurse = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  call ESMF_InfoGet(irecurse, "a", ir4, attnestflag=ESMF_ATTNEST_ON, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(ir4==5, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(irecurse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoSet with Parent Key"
  write(failMsg, *) "Did not set with parent key"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"parent": {"storage": {"foo": null}}}'
  ipkey = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(ipkey, "foo", "foo-value", rc=rc, pkey="/parent/storage")
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(ipkey, "/parent/storage/foo", actual_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(trim(actual_char)=="foo-value", name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(ipkey, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGet Character Allocatable"
  write(failMsg, *) "Did not get character allocatable"
  rc = ESMF_FAILURE
  failed = .false.

  info_charalloc = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_charalloc, "ca", "ca-value", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetCharAlloc(info_charalloc, "ca", actual_charalloc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(actual_charalloc=="ca-value", name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_charalloc, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check Initialization"
  write(failMsg, *) "Did not handle uninitialized object"
  rc = ESMF_FAILURE
  failed = .false.

  call ESMF_InfoSet(info_uninit, "not-init", "empty-value", rc=rc)

  call ESMF_Test(rc==ESMC_RC_OBJ_NOT_CREATED, name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Dirty State"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_dirty = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSetDirty(info_dirty, .true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  isDirty = .false.
  call ESMF_InfoGet(info_dirty, isDirty=isDirty, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(isDirty, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_dirty, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Implicit Conversion"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_implicit = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_implicit, "float", 33.33, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_implicit, "float", implicit_i4, rc=rc)
  call ESMF_Test(rc/=ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_implicit, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Implicit Conversion w/ Scalar to List"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_implicit = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_implicit, "float", 33.33, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_implicit, "float", implicit_i4_list, scalarToArray=.true., &
    rc=rc)
  call ESMF_Test(rc/=ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_implicit, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Copy while preserving 32-bit"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_copy = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_copy, "is_32bit", 33, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_copy, key="is_32bit", typekind=actual_tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(actual_tk == ESMF_TYPEKIND_I4, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_copy, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create by key while preserving 32-bit"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_cbk_base = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_cbk_base, "/ESMF/General/is_32bit", 33, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  info_cbk = ESMF_InfoCreate(info_cbk_base, "ESMF", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(info_cbk, key="is_32bit", typekind=actual_tk, &
    attnestflag=ESMF_ATTNEST_ON, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(actual_tk == ESMF_TYPEKIND_I4, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_cbk_base, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(info_cbk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "InfoGetTK for I4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_tk = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  tk_value = 11
  call ESMF_InfoSet(info_tk, "/NUOPC/Instance/Verbosity", tk_value, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  actual_tk = ESMF_InfoGetTK(info_tk, "/NUOPC/Instance/Verbosity", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(actual_tk == ESMF_TYPEKIND_I4, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "InfoGetTK for Character"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rc = ESMF_FAILURE
  failed = .false.

  info_tk = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(info_tk, "/NUOPC/Instance/Verbosity", "max", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  actual_tk = ESMF_InfoGetTK(info_tk, "/NUOPC/Instance/Verbosity", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(actual_tk == ESMF_TYPEKIND_CHARACTER, name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(info_tk, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !----------------------------------------------------------------------------

end program ESMF_InfoUTest
