! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
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

program ESMF_HConfigEx
#include "ESMF.h"

  use ESMF
  use ESMF_TestMod

  implicit none

  ! local variables
  integer                     :: rc
  type(ESMF_HConfig)          :: hconfig
  ! result code
  integer                     :: finalrc, result
  character(ESMF_MAXSTR)      :: testname
  character(ESMF_MAXSTR)      :: failMsg


  finalrc = ESMF_SUCCESS

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_HConfigEx"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  call ESMF_Initialize(defaultlogfilename="HConfigEx.Log", &
    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! \subsubsection{Create an empty HConfig object}
!
! By default, {\tt ESMF\_HConfigCreate()} creates an empty HConfig object.
!EOE
!BOC
  hconfig = ESMF_HConfigCreate(rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! When done with {\tt hconfig}, it should be destroyed in the usual manner.
!EOE
!BOC
  call ESMF_HConfigDestroy(hconfig, rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

10 continue

  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_HConfigEx.F90"
  else
    print *, "FAIL: ESMF_HConfigEx.F90"
  endif

end program
