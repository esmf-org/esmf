! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

    program FeatureTest
    
#include "ESMF.h"

    use ESMF
    use ESMF_TestMod
    use ESMF_FeatureTR15581Subr_mod
    implicit none

    integer :: rc, result
    character(len=ESMF_MAXSTR) :: failMsg, name

    real,    allocatable :: a(:), b(:), c(:)
    integer, allocatable :: indicies(:)
    logical, allocatable :: tfs(:)
    character(8), allocatable :: strings(:)
    type(ESMF_AllocDType), allocatable :: dts(:)

#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    character(:), allocatable :: alloc_string
    character(:), allocatable :: alloc_string_array(:)
#endif

    type(ESMF_Logical) :: tf_c
    logical :: tf

    integer :: i
    integer :: memstat

    interface
      subroutine ESMC_Present_test (arg, present_val)
        use ESMF, only: ESMF_Logical
        implicit none
        integer, intent(in), optional :: arg
        type(ESMF_Logical), intent(out) :: present_val
      end subroutine
    end interface

!------------------------------------------------------------------------
! Tests for various Fortran features and implementation issues that ESMF
! depends on.

    result = 0

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------
! Test to ensure that NULL is used for an OPTIONAL argument when the
! argument is not present.  NULL conforms to the value specified in
! Section 8.7 of Fortran TS 29113 (WG5/N1942 - 9/19/2012.)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_UTest
    name = "Fortran argument present in C test"
    failMsg = "Argument was not indicated as present when present"
    call ESMC_Present_test (arg=42, present_val=tf_c)
    tf = tf_c == ESMF_TRUE
    call ESMF_Test(tf, name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_UTest
    name = "Fortran argument not present in C test"
    failMsg = "Argument was indicated as present when not present"
    call ESMC_Present_test (present_val=tf_c)
    tf = tf_c == ESMF_TRUE
    call ESMF_Test(.not. tf, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Test to ensure that the F95+TR15581 (and F2003) allocatable features are supported.
! These include:
!   - Allocatable derived type components
!   - Allocatable dummy arguments
!   - Allocatable function return values

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_UTest
    name = "Fortran allocatable arguments call test"
    failMsg = "Did not return ESMF_SUCCESS"
    call ESMF_FeatureAllocArg (42, a, indicies, tfs, strings, dts, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_UTest
    name = "Fortran allocatable arguments size test"
    failMsg = "Incorrect allocated size"
    tf = size (a) == 42 .and. size (indicies) == 42 .and. size (tfs) == 42  &
        .and. size (dts) == 42
#if defined (ALLOC_STRING_TEST)
    tf = tf .and. size (strings) == 42
#endif
    rc = merge (ESMF_SUCCESS, ESMF_RC_ARG_SIZE, tf)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    deallocate (a, indicies, tfs, dts)

    ! NOTE: Some compilers may require a special command line argument for
    ! the following tests to work.  This is needed to turn on the F2003
    ! semantics for reassignment of allocatable arrays to different sizes
    ! than originally allocated.  For example PGI requires -Mallocatable=03
    ! and Intel requires -assume realloc_lhs.

    ! TODO: Tests are currently disabled due to failures on older versions
    ! (pre-4.6 or so) and with g95.  Re-enable when ESMF support for these
    ! older compilers is no longer required.

#if 0
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_xxxUTest
    name = "Fortran allocatable function result test"
    failMsg = "Did not return ESMF_SUCCESS"
    b = ESMF_FeatureAllocFRet (420, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_xxxUTest
    name = "Fortran allocatable function return size test"
    failMsg = "Incorrect allocated size"
    rc = merge (ESMF_SUCCESS, ESMF_RC_ARG_SIZE, size (b) == 420)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    print *, 'result size (b) =', size (b)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_xxxUTest
    name = "Fortran resize existing allocatable array via assignment test"
    failMsg = "Incorrect allocated size"
    a = b
    rc = merge (ESMF_SUCCESS, ESMF_RC_ARG_SIZE, size (a) == 420)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    print *, 'resized size (a) =', size (a)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_xxxUTest
    name = "Fortran resize unallocated allocatable array via assignment test"
    failMsg = "Incorrect allocated size"
    c = b
    rc = merge (ESMF_SUCCESS, ESMF_RC_ARG_SIZE, size (c) == 420)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    print *, 'new size (c) =', size (c)
#endif

#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

!------------------------------------------------------------------------
! F2003 allocatable length character string support.

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string scalar assignment test"
    failMsg = "Did not return correct length"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    alloc_string = '123456'
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, len (alloc_string) == 6)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string scalar deallocate test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    deallocate (alloc_string, stat=memstat)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, memstat == 0)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string scalar allocate test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    allocate (character(len=42)::alloc_string, stat=memstat)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, memstat == 0)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string scalar allocate length test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, len (alloc_string) == 42)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string array assignment test"
    failMsg = "Did not return correct length"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    alloc_string_array = (/ '123456', '789012', '345678', '901234', '567890' /)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, len (alloc_string_array) == 6)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string array size test"
    failMsg = "Did not return correct length"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, size (alloc_string_array) == 5)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string array deallocate test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    deallocate (alloc_string_array, stat=memstat)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, memstat == 0)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string array allocate test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    allocate (character(len=42)::alloc_string_array(24), stat=memstat)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, memstat == 0)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string array allocate length test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, len (alloc_string_array) == 42)
    write (failMsg,*) "Expected string length of 42, returned", len (alloc_string_array)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! EX_UTest
    name = "Fortran allocatable length string array allocate size test"
    failMsg = "Did not return success"
#if !defined (ESMF_NO_F2003_ALLOC_STRING_LENS)
    rc = merge (ESMF_SUCCESS, ESMF_FAILURE, size (alloc_string_array) == 24)
    write (failMsg,*) "Expected string array size of 24, returned", size (alloc_string_array)
#else
    name = "Bypassed " // name
    rc = ESMF_SUCCESS
#endif
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------


#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program FeatureTest
    
