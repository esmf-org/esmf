! $Id: ESMF_LogErrHaltUTest.F90,v 1.1 2005/04/05 16:43:37 svasquez Exp $
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
      program ESMF_LogErrUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>
#include <ESMF.h>

!==============================================================================
!BOP
! !PROGRAM: ESMF_LogErrHaltUTest - This test verifies the Halt on Error feature.
!
! !DESCRIPTION:
!
! The code in this file drives F90 LogErr halt on Error unit tests.
! The companion file ESMF\_LogErr.F90 contains the definitions for the
! LogErr methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_LogErrHaltUTest.F90,v 1.1 2005/04/05 16:43:37 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_Clock) :: clock1
      type(ESMF_Alarm) :: alarm1
      type(ESMF_Log) :: log1


!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------
      print *, "Starting LogErr Tests"

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      ! Set Log HaltOnError
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_LogSet(log=log1, halt=ESMF_LOG_HALTERROR, rc=rc)
      write(name, *) "Log Set Halt on Error Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc

      !------------------------------------------------------------------------
      !EX_UTest
      ! Create Alarm without ringTime to create an error to test the
      ! halt on error feature. Execution should halt on the ESMF_ALARM Create
      ! Print a pass assuming that execution will stop.
      write(failMsg, *) "Should never fail."
      write(name, *) "Force an Error Test"
      call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
      alarm1 = ESMF_AlarmCreate(name="WAKEUP", clock=clock1,  rc=rc)

      ! If the following code is executed, the HALT ON ERROR test failed.
      ! Print a special Fail message which will be handled by the test scripts.
      write(failMsg, *) "The Halt on Error test failed."
      write(name, *) "HALT_FAILED"
      call ESMF_Test((.false.), name, failMsg, result, ESMF_SRCLINE)

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)


      end program ESMF_LogErrUTest
