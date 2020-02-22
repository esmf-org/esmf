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

#define FILENAME "src/Infrastructure/Attribute/test/ESMF_InfoUTest.F90"

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
  integer               :: result = 0, count

  integer(ESMF_KIND_I4) :: value, actual, actual2, actual3, arr_i4_get_count, &
                           actual4, ir4=0
  integer(ESMF_KIND_I8) :: desired_i8, value_i8
  real :: actual_rw_val, desired_rw_val
  real(ESMF_KIND_R4) :: desired_r4, value_r4
  real(ESMF_KIND_R8) :: desired_r8, value_r8
  integer(ESMF_KIND_I4), dimension(3) :: arr_i4
  integer(ESMF_KIND_I4), dimension(:), allocatable :: arr_i4_get
  type(ESMF_Info) :: attrs, attrs2, attrs3, attrs4, attrs5, attrs6, &
                     attrs7, attrs8, attrs9, attrs10, attrs_copy_src, &
                     attrs_copy_dst, attrs_w, attrs_r, attrs_logical, &
                     attrs_types, attrs_obj_dst, attrs_obj_src, &
                     attrs_obj_new, attrs_parse, attrs_update_lhs, &
                     attrs_update_rhs, attrs_inq, attrs_eq_lhs, &
                     attrs_eq_rhs, irecurse, ipkey, info_charalloc, &
                     info_uninit, info_dirty

  logical :: is_present, failed, is_set, is_present_copy_test, actual_logical, &
             desired_logical, isArray, isDirty
  logical, dimension(2) :: fails_obj

  !----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoCreate"
  write(failMsg, *) "Did not create Info"
  rc = ESMF_FAILURE

  attrs = ESMF_InfoCreate(rc=rc)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoDestroy"
  write(failMsg, *) "Did not destroy Info"
  rc = ESMF_FAILURE;

  call ESMF_InfoDestroy(attrs, rc=rc)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoSet"
  write(failMsg, *) "Did not set key"
  rc = ESMF_FAILURE

  attrs2 = ESMF_InfoCreate(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key = "testKey"
  actual = 333
  call ESMF_InfoSet(attrs2, key, actual, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_i8 = "testKeyI8"
  desired_i8 = 92233720
  call ESMF_InfoSet(attrs2, key_i8, desired_i8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_r4 = "testKeyR4"
  desired_r4 = 333.0 + (1.0/3.0)
  call ESMF_InfoSet(attrs2, key_r4, desired_r4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_r8 = "testKeyR8"
  desired_r8 = 1.797693e-10
  call ESMF_InfoSet(attrs2, key_r8, desired_r8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key_char = "testKeyChar"
  desired_char = "i am char"
  call ESMF_InfoSet(attrs2, key_char, desired_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetI4"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(attrs2, key, value, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((value == actual), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetI8"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(attrs2, key_i8, value_i8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((value_i8 == desired_i8), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetR4"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(attrs2, key_r4, value_r4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((ABS(value_r4 - desired_r4) < 1e-16), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetR8"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(attrs2, key_r8, value_r8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((ABS(value_r8 - desired_r8) < 1e-16), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetCH with Default"
  write(failMsg, *) "Did not get default key"

  def_desired_char = "this is default!"
  def_key_char = "does not exist in attrs"
  call ESMF_InfoGet(attrs2, def_key_char, def_value_char, default=def_desired_char, &
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
  call ESMF_InfoSet(attrs2, key_empty_char, desired_empty_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs2, key_empty_char, empty_value_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((desired_empty_char==empty_value_char), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGetCH"
  write(failMsg, *) "Did not get key"

  call ESMF_InfoGet(attrs2, key_char, value_char, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((desired_char==value_char), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  ! Test we can overload the value if force is true
  write(name, *) "ESMF_InfoSet Force Flag"
  write(failMsg, *) "Could not overload value"
  rc = ESMF_FAILURE

  attrs3 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  key = "testKey"
  actual = 333

  call ESMF_InfoSet(attrs3, "foobar", 123, force=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs3, "foobar", 123, force=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  ! Test creating and destroying a bunch of attributes
  rc = ESMF_FAILURE
  write(name, *) "ESMF_Info Create+Destroy Loop"
  write(failMsg, *) "Failure during loop test"

  do i=1, 100000
    attrs4 = ESMF_InfoCreate(rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoSet(attrs4, "foobar", 123, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_InfoDestroy(attrs4, rc=rc)
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

  attrs5 = ESMF_InfoCreate(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs5, "doesNotExist", actual2, rc=rc, default=5897)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual2 == 5897), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs5, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoPrint"
  write(failMsg, *) "Print somehow not successful"

  rc = ESMF_FAILURE

  attrs6 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs6, "/i/am/nested", 111, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs6, "top-level", 222, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  print *, ""
  print *, "===== ESMF_InfoPrint Test Start ====="
  print *, ""

  call ESMF_InfoPrint(attrs6, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoPrint(attrs6, indent=1, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  print *, ""
  print *, "===== ESMF_InfoPrint Test End ====="
  print *, ""

  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs6, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoRemove Child From Parent"
  write(failMsg, *) "Child not erased from parent"

  rc = ESMF_FAILURE

  attrs7 = ESMF_InfoCreate(rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs7, "this/is/erase/test", 111, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoRemove(attrs7, "this/is/erase", keyChild="test", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs7, "/this/is/erase/test", actual3, &
                          default=-999, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual3 == -999), name, failMsg, result, ESMF_SRCLINE)

!  call ESMF_InfoPrint(attrs7, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoRemove Root"
  write(failMsg, *) "Did not erase from root"

  call ESMF_InfoRemove(attrs7, "this", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(attrs7, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs7, "this", actual3, default=-888, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual3 == -888), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs7, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoIsPresent False"
  write(failMsg, *) "Attribute key should not be present"

  attrs8 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present = ESMF_InfoIsPresent(attrs8, "this", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. is_present), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoIsPresent True"
  write(failMsg, *) "Attribute key is actually present"

  attrs8 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs8, "this", 11, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present = ESMF_InfoIsPresent(attrs8, "this", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((is_present), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoIsPresent True w/ Pointer"
  write(failMsg, *) "Attribute key is present using pointer syntax"

  attrs8 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs8, "/this/is/nested", 11, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present = ESMF_InfoIsPresent(attrs8, "/this/is/nested", &
    isPointer=.true., rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((is_present), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_Info Array Setting/Getting"
  write(failMsg, *) "Array operations failed"
  failed = .false.

  arr_i4(1) = 123
  arr_i4(2) = 456
  arr_i4(3) = 789

  attrs9 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs9, "the-key", arr_i4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGetAlloc(attrs9, "the-key", arr_i4_get, itemcount=arr_i4_get_count, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do i=1, 3
    if (arr_i4(i) /= arr_i4_get(i)) then
      failed = .true.
    end if
  end do

  deallocate(arr_i4_get)

  call ESMF_Test((.not. failed), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs9, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoSetNULL and ESMF_InfoIsSet"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  failed = .false.

  attrs10 = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSetNULL(attrs10, "is-the-null", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_set = ESMF_InfoIsSet(attrs10, "is-the-null", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (is_set) then
    failed = .true.
  end if

  call ESMF_InfoSet(attrs10, "is-the-null", 5, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_set = ESMF_InfoIsSet(attrs10, "is-the-null", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (.not. is_set) then
    failed = .true.
  end if

  call ESMF_InfoDestroy(attrs10, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. failed), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoCopy"
  write(failMsg, *) "Did not copy Info"
  rc = ESMF_FAILURE

  attrs_copy_src = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  attrs_copy_dst = ESMF_InfoCopy(attrs_copy_src, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs_copy_dst, "a-key", 22, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  is_present_copy_test = ESMF_InfoIsPresent(attrs_copy_src, "a-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(attrs_copy_src, rc=rc)
  call ESMF_InfoDestroy(attrs_copy_dst, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((is_present_copy_test .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoWrite & ESMF_InfoRead"
  write(failMsg, *) "Did not read/write Info"
  rc = ESMF_FAILURE

  attrs_w = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  desired_rw_val = 22.3
  call ESMF_InfoSet(attrs_w, "a-key", desired_rw_val, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoWriteJSON(attrs_w, "test-esmf-attrs-write.json", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  attrs_r = ESMF_InfoReadJSON("test-esmf-attrs-write.json", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs_r, "a-key", actual_rw_val, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(attrs_w, rc=rc)
  call ESMF_InfoDestroy(attrs_r, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((ABS(actual_rw_val-desired_rw_val) < 1e-16), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_Info Logical Scalar"
  write(failMsg, *) "Did not set/get logical type"
  rc = ESMF_FAILURE

  attrs_logical = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  desired_logical = .false.
  call ESMF_InfoSet(attrs_logical, "logical-key", desired_logical, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(attrs_logical, 2)

  actual_logical = .true.  ! Set to true to make sure it is updated in the call
  call ESMF_InfoGet(attrs_logical, "logical-key", actual_logical, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoDestroy(attrs_logical, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual_logical .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_Info Test Type Checking"
  write(failMsg, *) "Did not catch type error"
  rc = ESMF_FAILURE

  attrs_types = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs_types, "key", 111, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(attrs_types, 2)

  call ESMF_InfoSet(attrs_types, "key", 111.0, rc=rc)
  call ESMF_Test((rc==ESMC_RC_ARG_BAD), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs_types, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_Info Set/Get Object"
  write(failMsg, *) "Did not set/get Info object"
  rc = ESMF_FAILURE
  failed = .false.

  attrs_obj_dst = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  attrs_obj_src = ESMF_InfoCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs_obj_src, "src-key", 11167, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs_obj_dst, "dst-key", attrs_obj_src, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  attrs_obj_new = ESMF_InfoCreate(attrs_obj_dst, "dst-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!  call ESMF_InfoPrint(attrs_obj_dst, 2)
!  call ESMF_InfoPrint(attrs_obj_new, 2)

  fails_obj(1) = ESMF_InfoIsPresent(attrs_obj_new, "dst-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoSet(attrs_obj_src, "another-key", 44, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fails_obj(2) = ESMF_InfoIsPresent(attrs_obj_new, "another-key", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((.not. any(fails_obj)), name, failMsg, result, ESMF_SRCLINE)

!  call ESMF_InfoPrint(attrs_obj_dst, 2)
!  call ESMF_InfoPrint(attrs_obj_new, 2)
!  print *, fails_obj

  call ESMF_InfoDestroy(attrs_obj_dst, rc=rc)
  call ESMF_InfoDestroy(attrs_obj_src, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_Info Create By Parse"
  write(failMsg, *) "Did not create Info by parse"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"hello": "fortran parser", "multiple-types": 55}'

  attrs_parse = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs_parse, "multiple-types", actual4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual4==55), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoUpdate"
  write(failMsg, *) "Did not update"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"please": "update-me"}'
  attrs_update_lhs = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  to_parse = '{"i-am-new": 111}'
  attrs_update_rhs = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoUpdate(attrs_update_lhs, attrs_update_rhs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs_update_lhs, "i-am-new", actual, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((actual==111), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs_update_lhs, rc=rc)
  call ESMF_InfoDestroy(attrs_update_rhs, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoGet"
  write(failMsg, *) "Did not inquire right"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"ask": "questions please", "number": 1}'
  attrs_inq = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_InfoGet(attrs_inq, size=count, isArray=isArray, &
    isDirty=isDirty, jsonType=jsonType, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test((trim(jsonType)=="object" .and. &
                 (count==2) .and. &
                 (isArray .eqv. .false.) .and. &
                 (isDirty .eqv. .false.)), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs_inq, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_InfoEqual/NotEqual Operator"
  write(failMsg, *) "Did not evaluate equality"
  rc = ESMF_FAILURE
  failed = .false.

  to_parse = '{"ask": "questions please", "number": 1}'
  attrs_eq_lhs = ESMF_InfoCreate(to_parse, rc=rc)
  attrs_eq_rhs = ESMF_InfoCreate(to_parse, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_Test(((attrs_eq_lhs==attrs_eq_rhs) .and. &
    (.not. attrs_eq_lhs/=attrs_eq_rhs)), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_InfoDestroy(attrs_eq_lhs, rc=rc)
  call ESMF_InfoDestroy(attrs_eq_rhs, rc=rc)
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
  write(name, *) "ESMF_Info Check Initialization"
  write(failMsg, *) "Did not handle uninitialized object"
  rc = ESMF_FAILURE
  failed = .false.

  call ESMF_InfoSet(info_uninit, "not-init", "empty-value", rc=rc)

  call ESMF_Test(rc==ESMF_RC_OBJ_NOT_CREATED, name, failMsg, result, ESMF_SRCLINE)
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_Info Dirty State"
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
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !----------------------------------------------------------------------------

end program ESMF_InfoUTest
