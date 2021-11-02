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
program ESMF_MeshFileIOUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_MeshUTest - This unit test file tests Mesh methods.
! !DESCRIPTION:
!
! The code in this file drives F90 MeshCreate() unit tests.
! The companion file ESMF\_Mesh.F90 contains the definitions for the
! Mesh methods.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_MeshMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

   ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  logical :: correct


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! Make sure MOAB is off
  call ESMF_MeshSetMOAB(.false.)


 ! This surrounds all the tests to make turning off everything but one test easier
#if 1

  !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh create from file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call test_mesh_create_from_file(correct, rc)

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

#endif

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains


subroutine  test_mesh_create_from_file(correct, rc)
  logical :: correct
  integer :: rc
  type(ESMF_Mesh) :: mesh
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid
  integer, allocatable :: elemIds(:)
  integer :: numElems, numPerPet, numThisPet
  integer :: minId,maxId
  integer :: i, pos

  ! Init correct
  correct=.true.

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) return
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) return

  ! Number of elems in file
  numElems=936

  ! Approx. number per PET
  numPerPet=numElems/petCount

  ! Figure out range for this PET
  minId=localPet*numPerPet+1
  maxId=(localPet+1)*numPerPet
  
  ! If this Pet is last, put the rest there
  if (localPet .eq. petCount-1) maxId=numElems
  
  ! Allocate elemIds
  allocate(elemIds(maxId-minId+1))
  
  ! Fill
  pos=1
  do i=minId,maxId
     elemIds(pos)=i
     pos=pos+1
  enddo


  ! Create element Distgrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
  if (rc /= ESMF_SUCCESS) return
  
  
  ! Create Mesh from file
  mesh=ESMF_MeshCreateFromFileNew("data/ne4np4-esmf.nc", &
       fileformat=ESMF_FILEFORMAT_ESMFMESH, &
       elementDistgrid=elemDistgrid, &
       rc=rc)
  if (rc /= ESMF_SUCCESS) return


  ! UNCOMMENT WHEN ABOVE IS WORKING
  ! Get rid of Mesh
  ! call ESMF_MeshDestroy(mesh, rc=rc)
  ! if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemIds
  deallocate(elemIds)

   ! Return success
   rc=ESMF_SUCCESS

end subroutine test_mesh_create_from_file

end program ESMF_MeshFileIOUTest
