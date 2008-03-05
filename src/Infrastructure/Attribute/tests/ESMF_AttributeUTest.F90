! $Id: ESMF_AttributeUTest.F90,v 1.3 2008/03/05 17:21:16 rokuingh Exp $
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
program ESMF_AttributeUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_AttributeUTest - Attribute Unit Tests
!
! !DESCRIPTION:
!
! The code in this file drives F90 Attribute unit tests.
! The companion file ESMF\_Attribute.F90 contains the definitions for the
! Attribute methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF Framework
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_AttributeUTest.F90,v 1.3 2008/03/05 17:21:16 rokuingh Exp $'
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------
!=========================================================================

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables needed to pass into function/subroutine calls
!      character(ESMF_MAXSTR) :: name_set, name_get, value_set, value_get
      type(ESMF_DataValue) :: data_value
      type(ESMF_Base) :: base

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      ! preparations
      call ESMF_BaseCreate(base, "Base", "test object", 0, rc)
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      !EX_UTest
      ! test setting of ESMF Base attribute values
      call ESMF_AttributeSet(base, name, data_value, rc)
      write(name, *) "Testing non-implemented Attribute Set"
      write(failMsg, *) "Did not return ESMF_RC_NOT_IMPL"
      call ESMF_Test((rc.eq.ESMF_RC_NOT_IMPL), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! clean up
      call ESMF_BaseDestroy(base, rc)
      if (rc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
                  
#endif

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
end program ESMF_AttributeUTest
