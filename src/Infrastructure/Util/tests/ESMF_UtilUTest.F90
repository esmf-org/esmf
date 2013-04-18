! $Id$
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
      program ESMF_UtilUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_UtilUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Util unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF         ! the ESMF Framework
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

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! VM variables
      type(ESMF_VM) :: vm
      integer :: localPet

      ! local variables needed to pass into function/subroutine calls
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options
      !type(ESMF_UtilConfig) :: config_set
      !type(ESMF_UtilConfig) :: config_get
      !character(ESMF_MAXSTR) :: name_set, name_get

      ! misc local variables
      character(2) :: major_version, minor_version, revision
      character(len (ESMF_VERSION_STRING)) :: evs
      character(16) :: filename
      integer :: evs_dotpos
      integer :: i
      integer :: funits(5)
      integer :: ioerr

#ifdef ESMF_TESTEXHAUSTIVE
      character(ESMF_MAXSTR) :: arg
      character(2) :: argshort
      integer :: arg_len
      integer :: nargs
      character(ESMF_MAXSTR) :: program_path
      integer :: argindex

      real(ESMF_KIND_R8) :: random_values(50)
      integer :: sorted_ints (size (random_values))
      real    :: sorted_reals(size (random_values))
      integer(ESMF_KIND_I8) :: sorted_dblints (size (random_values))
      real(ESMF_KIND_R8)    :: sorted_dblreals(size (random_values))

      integer, allocatable :: seeds(:)
      integer :: seed_size

      character(8) :: sort_array(4)
      character(8), parameter :: sort_input(4) =  &
          (/ "this", "is  ", "a   ", "test" /)
      character(8), parameter :: sort_ascend(4) =  &
          (/ "a   ", "is  ", "test", "this" /)
      character(8), parameter :: sort_descend(4) =  &
          (/ "this", "test", "is  ", "a   " /)

      character(ESMF_MAXSTR) :: pathname
      logical :: relaxedFlag

      type(ESMF_MapPtr) :: mapcontainer
      integer :: newvalue
      integer :: mapsize
      logical :: isfound
#endif

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)

      ! add tests here

!Test version numbers
!====================

    ! Print all the constants, just for the record.

      print *
      print *, ' ESMF_VERSION_MAJOR =', ESMF_VERSION_MAJOR
      print *, ' ESMF_VERSION_MINOR =', ESMF_VERSION_MINOR
      print *, ' ESMF_VERSION_REVISION      =', ESMF_VERSION_REVISION
      print *, ' ESMF_VERSION_PATCHLEVEL    =', ESMF_VERSION_PATCHLEVEL
      print *, ' ESMF_VERSION_STRING=', trim (ESMF_VERSION_STRING)

    !NEX_UTest
    ! Compare numeric major version to the string
    write (major_version,'(i2)') ESMF_VERSION_MAJOR
    evs = ESMF_VERSION_STRING
    evs_dotpos = index (evs, '.')
    write(failMsg, *) "Numeric and character major_version mismatch"
    write(name, *) "Comparing numeric and character major version"
    call ESMF_Test(evs(:evs_dotpos-1) == adjustl (major_version),  &
      name, failMsg, result, ESMF_SRCLINE)
    !
    !NEX_UTest
    ! Compare numeric minor version to the string
    write (minor_version,'(i2)') ESMF_VERSION_MINOR
    evs = evs(evs_dotpos+1:)
    evs_dotpos = index (evs, '.')
    write(failMsg, *) "Numeric and character minor_version mismatch"
    write(name, *) "Comparing numeric and character minor version"
    call ESMF_Test(evs(:evs_dotpos-1) == adjustl (minor_version),  &
      name, failMsg, result, ESMF_SRCLINE)
    !
    !NEX_UTest
    ! Compare numeric revision to the string
    write (revision,'(i2)') ESMF_VERSION_REVISION
    evs = evs(evs_dotpos+1:)
    do, i=1, len (evs)
      if (scan (evs(i:i), '0123456789') /= 1) exit
    end do
    write(failMsg, *) "Numeric and character revision mismatch"
    write(name, *) "Comparing numeric and character revision"
    if (i >= 2) then
      evs_dotpos = i
      call ESMF_Test(evs(:evs_dotpos-1) == adjustl (revision),  &
        name, failMsg, result, ESMF_SRCLINE)
    else
      write (failMsg, *) 'Could not find revision number in string'
      call ESMF_Test(i >= 2, name, failMsg, result, ESMF_SRCLINE)      
    end if

! Test Fortran unit numbers
! =========================

    !
    !NEX_UTest
    ! Obtain a few Fortran units
    funits = -1
    do, i=1, size (funits)
      call ESMF_UtilIOUnitGet(funits(i), rc=rc)
      ioerr = 0
      if (rc == ESMF_SUCCESS) then
        write (filename,'(a,2i2.2)') 'IOtempfile', localPet, i
        open (funits(i), file=filename, iostat=ioerr)
      end if
      if (rc /= ESMF_SUCCESS .or. ioerr /= 0) exit
    end do

    write (name, *) "Testing ESMF_UtilIOUnitGet, obtaining and opening units"
    if (i > size (funits)) then
      write (failMsg, *) "Could not obtain a unit."
    else
      write (failMsg, *) "Could not obtain unit:", funits(i)
    end if
    call ESMF_Test(rc == ESMF_SUCCESS .or. ioerr /= 0, name, failMsg, result, ESMF_SRCLINE)

    do, i=1, size (funits)
      if (funits(i) /= -1) then
        close (funits(i), status='delete', iostat=ioerr)
      end if
    end do

!Flush data to a file
!====================

    !
    !NEX_UTest
    ! Get a unit number for flush
    write (name, *) "Testing ESMF_UtilIOUnitFlush, get a free unit"
    call ESMF_UtilIOUnitGet (funits(1), rc=rc)
    write (failMsg, *) "Obtaining a fresh unit"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !
    !NEX_UTest
    ! Open it
    write (filename,'(a,i2.2)') 'IOtempfile_', localPet
    write (name, *) "ESMF_UtilIOUnitFlush, open scratch file: ", trim (filename)
    write (failMsg, *) "Opening scratch unit"
    open (funits(1), file=filename, form='formatted', iostat=ioerr)
    call ESMF_Test(ioerr == 0, name, failMsg, result, ESMF_SRCLINE)

    !
    !NEX_UTest
    ! Flush it
    write (name, *) "ESMF_UtilIOUnitFlush, flush the scratch file"
    write (funits(1), *) 'Testing ESMF_UtilIOUnitFlush'
    write (failMsg, *) 'calling ESMF_UtilIOUnitFlush'
    call ESMF_UtilIOUnitFlush (funits(1), rc=rc)
    call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    close (funits(1), status='delete', iostat=ioerr)


#ifdef ESMF_TESTEXHAUSTIVE
!Command line arguments
!====================

    !
    !EX_UTest
    ! Get command line argument count
    write (name, *) "Testing ESMF_UtilGetArgC, command line argument count"
    write (failMsg, *) "Obtaining the command line argument count"
    call ESMF_UtilGetArgC (nargs)
    call ESMF_Test(nargs >= 0, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Test bad command line argument index
    write (name, *) "Testing ESMF_UtilGetArgC, argindex < 0"
    write (failMsg, *) "wrong rc when argindex < 0"
    call ESMF_UtilGetArg (argindex=-1, rc=rc)
    call ESMF_Test(rc == ESMF_RC_ARG_VALUE, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Get command name
    write (name, *) "Testing ESMF_UtilGetArgC, get command name"
    write (failMsg, *) "Obtaining the command name"
    call ESMF_UtilGetArg (argindex=0, argvalue=arg, rc=rc)
    print *, 'command name = ', trim (arg)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Get command line arguments
    write (name, *) "Testing ESMF_UtilGetArgC, get truncated command name"
    write (failMsg, *) "Obtaining truncated command name"
    call ESMF_UtilGetArg (argindex=0, argvalue=argshort, rc=rc)
    call ESMF_Test(rc == ESMF_RC_ARG_SIZE, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Get command line arguments
    write (name, *) "Testing ESMF_UtilGetArgC, compare arg lengths"
    write (failMsg, *) "Returned arg length does not match actual arg length"
    call ESMF_UtilGetArg (argindex=0, argvalue=arg, arglength=arg_len, rc=rc)
    print *, 'arg_len =', arg_len, ', len_trim (arg) =', len_trim (arg)
    call ESMF_Test(rc == ESMF_SUCCESS .and. arg_len == len_trim (arg),  &
        name, failMsg, result, ESMF_SRCLINE)

    program_path = arg

    !
    !EX_UTest
    ! Test bad command line argument index
    write (name, *) "Testing ESMF_UtilGetArgC, argindex > nargs"
    write (failMsg, *) "wrong rc when argindex > nargs"
    call ESMF_UtilGetArg (argindex=nargs+1, rc=rc)
    call ESMF_Test(rc == ESMF_RC_ARG_VALUE, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Test command line argument index with the program name
    write (name, *) "Testing ESMF_UtilGetArgIndex for program path"
    write (failMsg, *) "did not find program path"
    call ESMF_UtilGetArgIndex (argvalue=program_path, argindex=argindex, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS .and. argindex == 0,  &
      name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Test command line argument index with unknown value
    write (name, *) "Testing ESMF_UtilGetArgIndex for program path"
    write (failMsg, *) "did not return -1"
    call ESMF_UtilGetArgIndex (argvalue="esmf_xyzzy", argindex=argindex, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS .and. argindex == -1,  &
      name, failMsg, result, ESMF_SRCLINE)


!Sorting
!=======

    call random_seed (size=seed_size)
    allocate (seeds(seed_size))
    seeds = (/ (i*1234, i=1,seed_size) /)
    call random_seed (put=seeds)
    call random_number (random_values)

! Reals

    !EX_UTest
    ! Test ascending sort
    write (name, *) "Testing ascending real sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_reals = random_values
    call ESMF_UtilSort (sorted_reals, direction=ESMF_SORTFLAG_ASCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted ascending real results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_reals)
      if (sorted_reals(i-1) < sorted_reals(i)) cycle
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test descending sort
    write (name, *) "Testing descending real sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_reals = random_values
    call ESMF_UtilSort (sorted_reals, direction=ESMF_SORTFLAG_DESCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted descending real results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_reals)
      if (sorted_reals(i-1) > sorted_reals(i)) cycle
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

! Double precision reals

    !EX_UTest
    ! Test ascending sort
    write (name, *) "Testing ascending double precision real sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_dblreals = random_values
    call ESMF_UtilSort (sorted_dblreals, direction=ESMF_SORTFLAG_ASCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted ascending double precision real results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_dblreals)
      if (sorted_dblreals(i-1) < sorted_dblreals(i)) cycle
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test descending sort
    write (name, *) "Testing descending double precision real sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_dblreals = random_values
    call ESMF_UtilSort (sorted_dblreals, direction=ESMF_SORTFLAG_DESCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted descending double precision real results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_dblreals)
      if (sorted_dblreals(i-1) > sorted_dblreals(i)) cycle
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

! Integers

    !EX_UTest
    ! Test ascending sort
    write (name, *) "Testing ascending integer sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_ints = random_values * 123456
    call ESMF_UtilSort (sorted_ints, direction=ESMF_SORTFLAG_ASCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted ascending integer results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_ints)
      if (sorted_ints(i-1) < sorted_ints(i)) cycle
      print *, 'failed testing: ', sorted_ints(i-1), " < ", sorted_ints(i)
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test descending sort
    write (name, *) "Testing descending integer sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_ints = random_values * 123456
    call ESMF_UtilSort (sorted_ints, direction=ESMF_SORTFLAG_DESCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted descending integer results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_ints)
      if (sorted_ints(i-1) > sorted_ints(i)) cycle
      print *, 'failed testing: ', sorted_ints(i-1), " > ", sorted_ints(i)
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

! Double integers

    !EX_UTest
    ! Test ascending sort
    write (name, *) "Testing ascending double integer sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_dblints = random_values * 123456
    call ESMF_UtilSort (sorted_dblints, direction=ESMF_SORTFLAG_ASCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted ascending double integer results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_dblints)
      if (sorted_dblints(i-1) < sorted_dblints(i)) cycle
      print *, 'failed testing: ', sorted_dblints(i-1), " < ", sorted_dblints(i)
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test descending sort
    write (name, *) "Testing descending double integer sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sorted_dblints = random_values * 123456
    call ESMF_UtilSort (sorted_dblints, direction=ESMF_SORTFLAG_DESCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted descending double integer results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    rc = ESMF_SUCCESS
    do, i=2, size (sorted_dblints)
      if (sorted_dblints(i-1) > sorted_dblints(i)) cycle
      print *, 'failed testing: ', sorted_dblints(i-1), " > ", sorted_dblints(i)
      rc = ESMF_FAILURE
      exit
    end do
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

! Character strings

    !EX_UTest
    ! Test ascending sort
    write (name, *) "Testing ascending character string sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sort_array = sort_input
    call ESMF_UtilSort (sort_array, direction=ESMF_SORTFLAG_ASCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted ascending character string results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    if (all (sort_array == sort_ascend)) then
      rc = ESMF_SUCCESS
    else
      rc = ESMF_FAILURE
    end if
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test descending sort
    write (name, *) "Testing descending character string sort"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    sort_array = sort_input
    call ESMF_UtilSort (sort_array, direction=ESMF_SORTFLAG_DESCENDING, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test for correctly sorted results
    write (name, *) "Testing sorted descending character string results"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    if (all (sort_array == sort_descend)) then
      rc = ESMF_SUCCESS
    else
      rc = ESMF_FAILURE
    end if
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)


! File system directory creation and removal
!===========================================

    write (pathname,'(a,i3.3)') 'ESMF_rocks_', localPet

    !EX_UTest
    ! Test creating a directory
    write (name, *) "Testing creating a directory"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilIOMkDir (pathname, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test creating a directory which already exists
    write (name, *) "Testing creating a directory which already exists"
    write (failMsg, *) "did not return ESMF_FAILURE"
    call ESMF_UtilIOMkDir (pathname, rc=rc)
    call ESMF_Test(rc /= ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test creating a directory
    write (name, *) "Testing creating a directory which already exists w/relaxedFlag"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilIOMkDir (pathname, relaxedFlag=.true., rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test removing a directory
    write (name, *) "Testing removing a directory"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilIORmDir (pathname, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test removing a directory which does not exist
    write (name, *) "Testing removing a directory which does not exist"
    write (failMsg, *) "did not return failure"
    call ESMF_UtilIORmDir (pathname, rc=rc)
    call ESMF_Test(rc /= ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test removing a directory which does not exist, relaxed
    write (name, *) "Testing removing a directory which does not exist, relaxed"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilIORmDir (pathname, relaxedFlag=.true., rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)


#endif

      ! This calls finalize before returning, so it must be the last
      ! ESMF-related thing the test does.
      call ESMF_TestEnd(ESMF_SRCLINE)
  
      end program ESMF_UtilUTest
