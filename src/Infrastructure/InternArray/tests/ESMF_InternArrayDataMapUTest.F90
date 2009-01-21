! $Id: ESMF_InternArrayDataMapUTest.F90,v 1.5.2.4 2009/01/21 21:25:22 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_ArrayDataMapUTest

!-----------------------------------------------------------------------
! Include file, for ESMF_SRCLINE which must be done by the preprocessor

#include "ESMF_Macros.inc"

!-----------------------------------------------------------------------
!BOP
! !PROGRAM: ESMF_ArrayDataMapUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 ESMF_ArrayDataMap unit tests.
! The companion file ESMF\_ArrayDataMap.F90 contains the definitions for the
! ArrayDataMap methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF framework, including the class to test
      use ESMF_InternArrayMod
      use ESMF_InternArrayDataMapMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternArrayDataMapUTest.F90,v 1.5.2.4 2009/01/21 21:25:22 cdeluca Exp $'
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
      ! when get/set value routines enabled, comment these in and set
      ! the appropriate values, and remove the temporary integers.
      !<value type> :: value_set, value_get
      !integer :: value_set, value_get

      ! instantiate a ESMF_ArrayDataMap
      type(ESMF_InternArrayDataMap) :: ArrayDataMap

      !------------------------------------------------------------------------
      ! The unit tests are divided into Sanity and Exhaustive. The Sanity tests
      !  are always run. When the environment variable, EXHAUSTIVE, is set to 
      !  ON then the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE 
      !  variable is set to OFF, then only the sanity unit tests.
      ! Special strings (Non-exhaustive and exhaustive) have been
      ! added to allow a script to count the number and types of unit tests.
      !------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      datarank = 1

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! Test ArrayDataMap Initialization
      call ESMF_ArrayDataMapSetDefault(ArrayDataMap, datarank, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Default ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! Test ArrayDataMap Invalid
      call ESMF_ArrayDataMapSetInvalid(ArrayDataMap, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Invalid ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


#ifdef ESMF_TESTEXHAUSTIVE


      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Initialization
      call ESMF_ArrayDataMapSetDefault(ArrayDataMap, datarank, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Get
      call ESMF_ArrayDataMapGet(ArrayDataMap,  datarank=datarank, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS and/or datarank incorrect"
      write(name, *) "Get ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(datarank.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Print
      call ESMF_ArrayDataMapPrint(ArrayDataMap, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Validate
      call ESMF_ArrayDataMapValidate(ArrayDataMap, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validate ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Set
      call ESMF_ArrayDataMapSet(ArrayDataMap, datarank=2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Get
      call ESMF_ArrayDataMapGet(ArrayDataMap,  datarank, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS and/or horzRelloc incorrect"
      write(name, *) "Get ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(datarank.eq.2),  &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test ArrayDataMap Print
      call ESMF_ArrayDataMapPrint(ArrayDataMap, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print ArrayDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_ArrayDataMapUTest
