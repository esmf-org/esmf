! $Id: ESMF_MeshUTest.F90,v 1.5 2008/10/21 21:36:36 rosalind Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
  use ESMF_Mod
  use ESMF_MeshMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_MeshUTest.F90,v 1.5 2008/10/21 21:36:36 rosalind Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_Mesh) :: meshSrc

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
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "MeshCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  meshSrc = ESMF_MeshCreate(2,3,rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! TODO: "Activate once the mesh is fully created. ESMF_MeshWrite is not meant
  !  to be called until then".
  !UTest
  write(name, *) "MeshWrite Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_MeshWrite(meshSrc, filename="mesh_out", rc=rc)
 !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_MeshUTest

