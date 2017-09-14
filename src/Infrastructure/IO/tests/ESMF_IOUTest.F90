! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IOUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

use ESMF_IOScripMod
!==============================================================================
!BOP
! !PROGRAM: ESMF_IOUTest -  Tests some basic ESMF IO configuration and usage
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  integer :: result = 0

  ! local variables
  type(ESMF_VM):: vm
  integer :: localPet, petCount
  integer :: rc

  real(ESMF_KIND_R8) :: factorList(10)
  integer(ESMF_KIND_I4) :: factorIndexList(2,10)
  character(32) :: filename

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! Set up
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  factorList = (/0,1,2,3,4,5,6,7,8,9/)
  factorIndexList = reshape((/1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2/), &
                            shape(factorIndexList))
  filename = "doodle.nc"

  !NEX_UTest
  call ESMF_OutputWeightFile(filename, factorList, factorIndexList, rc=rc)
  write(failMsg, *) "did not return ESMF_SUCCESS"
  write(name, *) "call ESMF_OutputWeightFile"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_IOUTest
