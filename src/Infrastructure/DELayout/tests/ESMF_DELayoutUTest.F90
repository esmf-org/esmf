! $Id: ESMF_DELayoutUTest.F90,v 1.6 2004/12/17 21:57:24 theurich Exp $
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
      program ESMF_DELayoutUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_DELayoutTest - This unit test file verifies DELayout methods.
!
! !DESCRIPTION:
!
! The code in this file drives F90 DELayout unit tests.
! The companion file ESMF\_DELayout.F90 contains the definitions for the
! DELayout methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_DELayoutUTest.F90,v 1.6 2004/12/17 21:57:24 theurich Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_VM):: vm
      type(ESMF_DELayout):: delayout

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      call ESMF_VMGetGlobal(vm, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "DELayout Create Test"
      delayout = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)


      end program ESMF_DELayoutUTest
