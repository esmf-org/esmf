! $Id: ESMF_MeshUTest.F90,v 1.1 2008/02/29 22:04:27 dneckels Exp $
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
program ESMF_MeshUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_MeshUTest - Check Building a mesh from fortran
!
! !DESCRIPTION:
!
! The code in this file drives F90 Mesh unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_MeshMod
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_MeshUTest.F90,v 1.1 2008/02/29 22:04:27 dneckels Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: vm

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  mesh = ESMF_MeshCreate(2, 3, localrc)

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
end program ESMF_MeshUTest
