! $Id: ESMF_BundleDataMapUTest.F90,v 1.2 2004/05/07 21:46:09 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
      '$Id: ESMF_BundleDataMapUTest.F90,v 1.2 2004/05/07 21:46:09 svasquez Exp $'
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
      type(ESMF_BundleDataMap) :: bundleDataMap1, bundleDataMap2, bundleDataMap3

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------------
      !NEX_UTest
      ! Test BumdleDataMap Init
      call ESMF_BundleDataMapInit(bundleDataMap1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Init BumdleDataMap Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_BundleDataMapUTest
