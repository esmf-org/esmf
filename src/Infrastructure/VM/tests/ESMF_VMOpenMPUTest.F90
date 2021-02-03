! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_VMOpenMPUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMOpenMP - Check ESMF OpenMP compatibility and capability
!
! !DESCRIPTION:
!
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  integer :: petCount, tid
  logical :: openMPEnabled
  
!$  integer :: omp_get_thread_num

  type(ESMF_VM)       :: vm

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check that OpenMP functions correctly if it is available"
  write(failMsg, *) "OpenMP inconsistency"
  tid = 1 ! initialize to something different than OpenMP would
!$ tid = omp_get_thread_num()
  if (tid /= 1) then
    rc = ESMF_FAILURE
  else
    rc = ESMF_SUCCESS
  endif
!$  if (tid /= 0) then
!$    rc = ESMF_FAILURE
!$  else
!$   rc = ESMF_SUCCESS
!$  endif
  print *, "tid = ", tid
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Determine whether ESMF library was compiled with OpenMP"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_VMGet(vm, openMPEnabledFlag=openMPEnabled, rc=rc)
  if (openMPEnabled) then
    print *, "ESMF was compiled with OpenMP"
  else
    print *, "ESMF was NOT compiled with OpenMP"
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_VMOpenMPUTest
