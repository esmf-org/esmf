! $Id: ESMF_ArraySpecUTest.F90,v 1.6.2.1 2010/02/05 19:53:25 svasquez Exp $
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
      program ESMF_ArraySpecUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArraySpecUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 ArraySpec unit tests.
! The companion file ESMF\_ArraySpec.F90 contains the definitions for the
! ArraySpec methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ArraySpecUTest.F90,v 1.6.2.1 2010/02/05 19:53:25 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1
#ifdef ESMF_TESTEXHAUSTIVE 
      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      type(ESMF_ArraySpec)                        :: arrayspec
#endif

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


#ifdef ESMF_TESTEXHAUSTIVE
      
      !------------------------------------------------------------------------
      !EX_UTest 
      ! ESMF_ArraySpecPrint test ArraySpecPrint public interface
      call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
      call ESMF_ArraySpecPrint(arrayspec, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test ESMF_ArraySpecPrint public interface"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_ArraySpecUTest
