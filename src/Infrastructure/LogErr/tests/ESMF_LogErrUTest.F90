! $Id: ESMF_LogErrUTest.F90,v 1.1 2004/08/16 22:22:38 svasquez Exp $
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

!==============================================================================
!BOP
! !PROGRAM: ESMF_LogErrTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 LogErr unit tests.
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
      '$Id: ESMF_LogErrUTest.F90,v 1.1 2004/08/16 22:22:38 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      print *, "Starting job"

      call ESMF_Initialize(rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Log Write
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      rc=ESMF_LogWrite("Log Write 2",ESMF_LOG_INFO)
      write(name, *) "Use of default log Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      print *, " rc = ", rc
      !------------------------------------------------------------------------


#ifdef ESMF_EXHAUSTIVE

#endif

      call ESMF_Finalize(rc)

      print *, "******  End of LogErrUTest  ******"

      end program ESMF_LogErrUTest
