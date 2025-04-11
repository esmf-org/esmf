! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_InternalStateEx

!==============================================================================
! !PROGRAM: ESMF_InternalStateEx - Demonstrate Attachable Methods API
!
! !DESCRIPTION:
!
! This program shows examples of Attachable Methods.
!-----------------------------------------------------------------------------
#include "ESMF.h"

  ! ESMF Framework module
  use ESMF
  use ESMF_TestMod
  use producerMod
  use consumerMod
  implicit none

  ! Local variables
  integer :: rc, userRc

  integer :: finalrc, result
  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    write(failMsg, *) "Example failure"
    write(testname, *) "Example ESMF_InternalStateEx"

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS


  call ESMF_Initialize(defaultlogfilename="InternalStateEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Components Set and Get their Internal State}
!
! TODO: description goes here
!EOE
!BOC
! TODO: code goes here
!EOC

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_InternalStateEx.F90"
  else
    print *, "FAIL: ESMF_InternalStateEx.F90"
  endif

end program ESMF_InternalStateEx
