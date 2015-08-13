! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2014, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

program ESMF_ValIteratorUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ValIteratorUTest - Unit test for Value Iterator util functions
!
! !DESCRIPTION:
!
! This file contains unit tests for the ESMF Value iterator utility functions
! The tests are run on multiple processes.
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ISO_C_BINDING

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test failure message
  character(ESMF_MAXSTR) :: name
  character(ESMF_MAXSTR) :: logMsg

  ! local variables
  integer::  rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount, ssiId, accDeviceCount
  type(c_ptr) :: val_hnd

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! get global vm information
  call ESMF_VMGetGlobal(vm, rc=rc)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)


  !Try creating a value iterator
  !===========================================
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Creating a value iterator test
  write(name, *) "Test creating a value iterator"
  val_hnd = CreateRangeValIterator(0,30,2)
  !call ESMF_VMGet(vm, localPet, accDeviceCount=accDeviceCount, ssiId=ssiId, rc=rc)
  !write(logMsg, *) "Number of accelerator devices =", accDeviceCount
  !call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)

  write(logMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((c_associated(val_hnd)), name, logMsg, result, ESMF_SRCLINE)

  if (c_associated(val_hnd)) then
    !call PrintValIterator(val_hnd)
    call DeleteValIterator(val_hnd)
  end if


  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !------------------------------------------------------------------------

end program ESMF_ValIteratorUTest
