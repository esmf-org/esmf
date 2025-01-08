! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_GeomUTest

!------------------------------------------------------------------------------

#include "ESMF.h"
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_GeomTest - Check Grid Create Routines
!
! !DESCRIPTION:
!
! The code in this file drives F90 Grid Create unit tests.
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
  integer :: localrc, rc, localPet, petCount
  type(ESMF_VM) :: vm

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name, grid_name

  type(ESMF_Grid) :: grid1, grid2, gridAlias
  type(ESMF_Geom) :: geom1, geom2, geomAlias
  logical :: shouldBeFalse, shouldBeTrue
  type(ESMF_GeomMatch_Flag) :: geomMatchFlag  

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create two different Grids to be used for testing below
  grid1=ESMF_GridCreateNoPeriDim(maxIndex=(/100,100/), rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  grid2=ESMF_GridCreateNoPeriDim(maxIndex=(/200,200/), rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_GeomOperator(==)()    
  write(name, *) "Geom equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  shouldBeFalse = (geom1 == geom2)
  call ESMF_Test(.not. shouldBeFalse, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Geom non-equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  shouldBeTrue = (geom1 /= geom2)
  call ESMF_Test(shouldBeTrue, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_GeomAssignment(=)() 
  write(name, *) "Geom equality with alias test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Init
  rc=ESMF_SUCCESS
  
  ! Create geom
  geom1=ESMF_GeomCreate(grid1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Assign alias
  geomAlias=geom1
  
  ! Test equality
  shouldBeTrue = (geom1 == geomAlias)

  ! Get rid of geom1
  call ESMF_GeomDestroy(geom1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE
  
  call ESMF_Test((shouldBeTrue .and. (rc==ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_GeomOperator(/=)()
  write(name, *) "Geom inequality with two different geoms"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Init
  rc=ESMF_SUCCESS
  
  ! Create geom1
  geom1=ESMF_GeomCreate(grid1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create geom2
  geom2=ESMF_GeomCreate(grid2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Test equality
  shouldBeTrue = (geom1 /= geom2)

  ! Get rid of geom1
  call ESMF_GeomDestroy(geom1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of geom2
  call ESMF_GeomDestroy(geom2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE
  
  call ESMF_Test((shouldBeTrue .and. (rc==ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Geom match with the same geoms"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Init
  rc=ESMF_SUCCESS
  
  ! Create geom1
  geom1=ESMF_GeomCreate(grid1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Match
  geomMatchFlag=ESMF_GeomMatch(geom1, geom1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

 
  ! Get rid of geom1
  call ESMF_GeomDestroy(geom1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE
  
  call ESMF_Test((geomMatchFlag == ESMF_GEOMMATCH_ALIAS) .and. (rc == ESMF_SUCCESS), &
       name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Geom match with different geoms with the same grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Init
  rc=ESMF_SUCCESS
  
  ! Create geom1
  geom1=ESMF_GeomCreate(grid1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create geom2
  geom2=ESMF_GeomCreate(grid1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Match
  geomMatchFlag=ESMF_GeomMatch(geom1, geom2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of geom1
  call ESMF_GeomDestroy(geom1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of geom2
  call ESMF_GeomDestroy(geom2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE
  
  call ESMF_Test((geomMatchFlag == ESMF_GEOMMATCH_GEOMALIAS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Geom match with different geoms with different grids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! Init
  rc=ESMF_SUCCESS
  
  ! Create geom1
  geom1=ESMF_GeomCreate(grid1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create geom2
  geom2=ESMF_GeomCreate(grid2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Match
  geomMatchFlag=ESMF_GeomMatch(geom1, geom2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of geom1
  call ESMF_GeomDestroy(geom1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of geom2
  call ESMF_GeomDestroy(geom2, rc=localrc)
  if (localrc /= ESMF_SUCCESS) rc=ESMF_FAILURE
  
  call ESMF_Test((geomMatchFlag == ESMF_GEOMMATCH_NONE), name, failMsg, result, ESMF_SRCLINE)

 !------------------------ Cleanup after testing --------------------------------------------
  
  ! Get rid of test Grids
  call ESMF_GridDestroy(grid1)
  call ESMF_GridDestroy(grid2)
 
  
  !-----------------------------------------------------------------------------
  ! Stop testing
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_GeomUTest
