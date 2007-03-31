! $Id: ESMF_BundleDataMapUTest.F90,v 1.18 2007/03/31 05:50:55 cdeluca Exp $
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
      program ESMF_BundleDataMapUTest

!-----------------------------------------------------------------------
! Include file, for ESMF_SRCLINE which must be done by the preprocessor

#include <ESMF_Macros.inc>

!-----------------------------------------------------------------------
!BOP
! !PROGRAM: ESMF_BundleDataMapUTest - Verifies the functionality of the class.
!
! !DESCRIPTION:
!
! The code in this file drives F90 BundleDataMap unit tests.
! The companion file ESMF\_BundleDataMap.F90 contains the definitions for the
! BundleDataMap methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF framework, including the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_BundleDataMapUTest.F90,v 1.18 2007/03/31 05:50:55 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      type(ESMF_BundleDataMap) :: bundleDataMap1
      type(ESMF_InterleaveFlag) :: interleave

      !------------------------------------------------------------------------
      ! The unit tests are divided into Sanity and Exhaustive. The Sanity tests
      ! are always run. When the environment variable, EXHAUSTIVE, is set to 
      ! ON then the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE 
      ! variable is set to OFF, then only the sanity unit tests.
      ! Special strings (Non-exhaustive and exhaustive) have been
      ! added to allow a script to count the number and types of unit tests.
      !------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test BundleDataMap Initialization
      call ESMF_BundleDataMapSetDefault(bundleDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Default BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !NEX_UTest
      ! Test BundleDataMap Invalid
      call ESMF_BundleDataMapSetInvalid(bundleDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Invalid BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Initialization
      call ESMF_BundleDataMapSetDefault(bundleDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Get
      call ESMF_BundleDataMapGet(bundleDataMap1,  bundleInterleave=interleave, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS and/or interleave incorrect"
      write(name, *) "Get BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(interleave.eq.ESMF_INTERLEAVE_BY_BLOCK), &
           name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Print
      call ESMF_BundleDataMapPrint(bundleDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Validate
      call ESMF_BundleDataMapValidate(bundleDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validate BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Set
      call ESMF_BundleDataMapSet(bundleDataMap1, bundleInterleave=ESMF_INTERLEAVE_BY_ITEM, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Get
      call ESMF_BundleDataMapGet(bundleDataMap1,  bundleInterleave=interleave, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS and/or interleave incorrect"
      write(name, *) "Get BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(interleave.eq.ESMF_INTERLEAVE_BY_ITEM), &
           name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test BundleDataMap Print
      call ESMF_BundleDataMapPrint(bundleDataMap1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Print BundleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
      
      call ESMF_TestEnd(result, ESMF_SRCLINE)
      
      end program ESMF_BundleDataMapUTest
