! $Id: ESMF_VMUTest.F90,v 1.1 2004/08/24 21:44:40 svasquez Exp $
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
      program ESMF_VMUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 VM unit tests.
! The companion file ESMF\_VM.F90 contains the definitions for the
! VM methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_VMUTest.F90,v 1.1 2004/08/24 21:44:40 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      integer:: rc
      type(ESMF_VM):: vm
      type(ESMF_GridComp):: gcomp
      type(ESMF_Clock):: clock
      type(ESMF_State):: dummystate


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      print *, "Starting job"

      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "ESMF_Initialize Test"
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Global Test"
      call ESMF_VMGetGlobal(vm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_Finalize(rc)

      print *, "******  End of VMUTest  ******"

      end program ESMF_VMUTest
