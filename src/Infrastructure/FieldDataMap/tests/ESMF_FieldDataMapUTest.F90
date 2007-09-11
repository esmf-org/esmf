! $Id: ESMF_FieldDataMapUTest.F90,v 1.15 2007/09/11 03:59:32 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldDataMapUTest

!-----------------------------------------------------------------------
! Include file, for ESMF_SRCLINE which must be done by the preprocessor

#include <ESMF_Macros.inc>

!-----------------------------------------------------------------------
!BOP
! !PROGRAM: ESMF_FieldDataMapUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 FieldDataMap unit tests.
! The companion file ESMF\_FieldDataMap.F90 contains the definitions for the
! FieldDataMap methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF framework, including the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
   '$Id: ESMF_FieldDataMapUTest.F90,v 1.15 2007/09/11 03:59:32 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc, datarank

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options
      type(ESMF_StaggerLoc) :: staggerloc

      ! instantiate a FieldDataMap 
      type(ESMF_FieldDataMap) :: fieldDataMap1

!------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      datarank=1

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test FieldDataMap Initialization
      call ESMF_FieldDataMapSetDefault(fieldDataMap1, datarank, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Default FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !NEX_UTest
      ! Test FieldDataMap Invalid
      call ESMF_FieldDataMapSetInvalid(fieldDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Invalid FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

 
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test FieldDataMap Initialization
      call ESMF_FieldDataMapSetDefault(fieldDataMap1, datarank, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test FieldDataMap Get
      call ESMF_FieldDataMapGet(fieldDataMap1, staggerloc=staggerloc, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS and/or staggerloc incorrect"
      write(name, *) "Get FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(staggerloc==ESMF_STAGGERLOC_CENTER), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test FieldDataMap Print
      call ESMF_FieldDataMapPrint(fieldDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test FieldDataMap Validate
      call ESMF_FieldDataMapValidate(fieldDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validate FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test FieldDataMap Print
      call ESMF_FieldDataMapPrint(fieldDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print FieldDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FieldDataMapUTest
