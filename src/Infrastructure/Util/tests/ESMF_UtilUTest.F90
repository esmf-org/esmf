! $Id: ESMF_UtilUTest.F90,v 1.27 2010/11/03 04:58:51 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
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
      use ESMF_IOUtilMod   ! Internal Fortran I/O routines
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_UtilUTest.F90,v 1.27 2010/11/03 04:58:51 theurich Exp $'
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

      type(ESMF_MapName) :: mapcontainer
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

      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)

      ! add tests here

!Test version numbers
!====================

    ! Print all the constants, just for the record.

      print *
      print *, ' ESMF_MAJOR_VERSION =', ESMF_MAJOR_VERSION
      print *, ' ESMF_MINOR_VERSION =', ESMF_MINOR_VERSION
      print *, ' ESMF_REVISION      =', ESMF_REVISION
      print *, ' ESMF_PATCHLEVEL    =', ESMF_PATCHLEVEL
      print *, ' ESMF_VERSION_STRING=', trim (ESMF_VERSION_STRING)

    !NEX_UTest
    ! Compare numeric major version to the string
    write (major_version,'(i2)') ESMF_MAJOR_VERSION
    evs = ESMF_VERSION_STRING
    evs_dotpos = index (evs, '.')
    write(failMsg, *) "Numeric and character major_version mismatch"
    write(name, *) "Comparing numeric and character major version"
    call ESMF_Test(evs(:evs_dotpos-1) == adjustl (major_version),  &
      name, failMsg, result, ESMF_SRCLINE)
    !
    !NEX_UTest
    ! Compare numeric minor version to the string
    write (minor_version,'(i2)') ESMF_MINOR_VERSION
    evs = evs(evs_dotpos+1:)
    evs_dotpos = index (evs, '.')
    write(failMsg, *) "Numeric and character minor_version mismatch"
    write(name, *) "Comparing numeric and character minor version"
    call ESMF_Test(evs(:evs_dotpos-1) == adjustl (minor_version),  &
      name, failMsg, result, ESMF_SRCLINE)
    !
    !NEX_UTest
    ! Compare numeric revision to the string
    write (revision,'(i2)') ESMF_REVISION
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
      call ESMF_IOUnitGet(funits(i), rc)
      ioerr = 0
      if (rc == ESMF_SUCCESS) then
        write (filename,'(a,2i2.2)') 'IOtempfile', localPet, i
        open (funits(i), file=filename, iostat=ioerr)
      end if
      if (rc /= ESMF_SUCCESS .or. ioerr /= 0) exit
    end do

    write (name, *) "Testing ESMF_IOUnitGet, obtaining and opening units"
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
    write (name, *) "Testing ESMF_IOUnitFlush, get a free unit"
    call ESMF_IOUnitGet(funits(1), rc)
    write (failMsg, *) "Obtaining a fresh unit"
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !
    !NEX_UTest
    ! Open it
    write (filename,'(a,i2.2)') 'IOtempfile_', localPet
    write (name, *) "ESMF_IOUnitFlush, open scratch file: ", trim (filename)
    write (failMsg, *) "Opening scratch unit"
    open (funits(1), file=filename, form='formatted', iostat=ioerr)
    call ESMF_Test(ioerr == 0, name, failMsg, result, ESMF_SRCLINE)

    !
    !NEX_UTest
    ! Flush it
    write (name, *) "ESMF_IOUnitFlush, flush the scratch file"
    write (funits(1), *) 'Testing ESMF_IOUnitFlush'
    write (failMsg, *) 'calling ESMF_IOUnitFlush'
    call ESMF_IOUnitFlush(funits(1), rc)
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
    nargs = ESMF_UtilGetArgC ()
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
    call ESMF_UtilGetArg (argindex=0, value=arg, rc=rc)
    print *, 'command name = ', trim (arg)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Get command line arguments
    write (name, *) "Testing ESMF_UtilGetArgC, get truncated command name"
    write (failMsg, *) "Obtaining truncated command name"
    call ESMF_UtilGetArg (argindex=0, value=argshort, rc=rc)
    call ESMF_Test(rc == ESMF_RC_ARG_SIZE, name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Get command line arguments
    write (name, *) "Testing ESMF_UtilGetArgC, compare arg lengths"
    write (failMsg, *) "Returned arg length does not match actual arg length"
    call ESMF_UtilGetArg (argindex=0, value=arg, length=arg_len, rc=rc)
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
    call ESMF_UtilGetArgIndex (value=program_path, argindex=argindex, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS .and. argindex == 0,  &
      name, failMsg, result, ESMF_SRCLINE)

    !
    !EX_UTest
    ! Test command line argument index with unknown value
    write (name, *) "Testing ESMF_UtilGetArgIndex for program path"
    write (failMsg, *) "did not return -1"
    call ESMF_UtilGetArgIndex (value="esmf_xyzzy", argindex=argindex, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS .and. argindex == -1,  &
      name, failMsg, result, ESMF_SRCLINE)


!MapName Containers
!==================

! Note: These are internal ESMF routines, subject to change, and
! not intended to be end-user callable.

    !
    !EX_UTest
    ! Test creation of a MapName container
    write (name, *) "Testing creation of a MapName container"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameCreate (mapcontainer, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test adding a name/value pair
    write (name, *) "Testing adding a name/value pair"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameAdd (mapcontainer,  &
        name="Temperature", value=1, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test adding a second name/value pair
    write (name, *) "Testing adding a name/value pair #2"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameAdd (mapcontainer,  &
        name="Pressure", value=42, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test obtaining the size of the map
    write (name, *) "Testing obtaining size"
    write (failMsg, *) "did not return correct size"
    mapsize = ESMF_UtilMapNameSize (mapcontainer)
    call ESMF_Test(mapsize == 2, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test obtaining the size of the map
    write (name, *) "Testing obtaining size return code"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    mapsize = ESMF_UtilMapNameSize (mapcontainer, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test printing the name/value pairs
    write (name, *) "Testing debug printout to stdout"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNamePrint (mapcontainer, title=name, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a name/value pair (part 1)
    write (name, *) "Testing looking up a name/value pair (part 1)"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameLookup (mapcontainer,  &
        name="Temperature", value=newvalue, foundFlag=isfound, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a name/value pair (part 2)
    write (name, *) "Testing looking up a name/value pair (part 2)"
    write (failMsg, *) "did not return correct foundFlag"
    call ESMF_Test(isfound, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a name/value pair (part 3)
    write (name, *) "Testing looking up a name/value pair (part 3)"
    write (failMsg, *) "did not return correct value"
    call ESMF_Test(newvalue == 1, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a second name/value pair (part 1)
    write (name, *) "Testing looking up a second name/value pair (part 1)"
    write (failMsg, *) "did not return ESMF_SUCCESS or correct value"
    call ESMF_UtilMapNameLookup (mapcontainer,  &
        name="Pressure", value=newvalue, foundFlag=isfound, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a second name/value pair (part 2)
    write (name, *) "Testing looking up a second name/value pair (part 2)"
    write (failMsg, *) "did not return correct foundFlag"
    call ESMF_Test(isfound, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a second name/value pair (part 3)
    write (name, *) "Testing looking up a second name/value pair (part 3)"
    write (failMsg, *) "did not return correct value"
    call ESMF_Test(newvalue == 42, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test removing a name/value pair
    write (name, *) "Testing removing a name/value pair"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameRemove (mapcontainer,  &
        name="Temperature", rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a removed name/value pair (part 1)
    write (name, *) "Testing looking up a removed name/value pair (part 1)"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameLookup (mapcontainer,  &
        name="Temperature", value=newvalue, foundFlag=isfound, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS,  &
        name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test looking up a removed name/value pair (part 2)
    write (name, *) "Testing looking up a removed name/value pair (part 2)"
    write (failMsg, *) "indicated a removed pair was found"
    call ESMF_Test(.not. isfound,  &
        name, failMsg, result, ESMF_SRCLINE)

    !EX_UTest
    ! Test destruction of a MapName container
    write (name, *) "Testing destruction of a MapName container"
    write (failMsg, *) "did not return ESMF_SUCCESS"
    call ESMF_UtilMapNameDestroy (mapcontainer, rc=rc)
    call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

#endif

      ! This calls finalize before returning, so it must be the last
      ! ESMF-related thing the test does.
      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      end program ESMF_UtilUTest
