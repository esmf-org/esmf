! $Id: ESMF_FieldGather1DUTest.F90,v 1.19 2008/04/30 17:27:42 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldGather1DUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldGather1DUTest - This test verifies FieldGather functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledGather with
! a 1 dimension DELayout.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldGather1DUTest.F90,v 1.19 2008/04/30 17:27:42 feiliu Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      !character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      !character(ESMF_MAXSTR*2) :: failMsg

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_FieldGather1DUTest
