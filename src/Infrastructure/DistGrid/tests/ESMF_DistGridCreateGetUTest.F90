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
program ESMF_DistGridCreateGetUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_DistGridCreateGetUTest - This unit test file tests 
!   DistGridCreate() and DistGridGet() methods.
! !DESCRIPTION:
!
! The code in this file drives F90 DistGridCreate(), DistGridGet() unit tests.
! The companion file ESMF\_DistGrid.F90 contains the definitions for the
! DistGrid methods.
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

  !LOCAL VARIABLES:
  type(ESMF_VM):: vm
  integer:: petCount, localPet, i, localDeCount
  type(ESMF_DistGrid):: distgrid, distgrid2, distgrid3, distgridAlias
  type(ESMF_DELayout):: delayout
  integer:: dimCount, tileCount, deCount
  logical:: regDecompFlag
  logical:: isCreated
  integer:: elementCount, localStart
  integer, allocatable:: elementCountPTile(:), deToTileMap(:), elementCountPDe(:)
  integer, allocatable:: minIndexPTile(:,:), maxIndexPTile(:,:)
  integer, allocatable:: minIndexPDe(:,:), maxIndexPDe(:,:)
  integer, allocatable:: indexCountPDe(:,:), localDeToDeMap(:)
  integer, allocatable:: indexList(:), seqIndexList(:)
  integer, allocatable:: deBlockList(:,:,:)
  integer, allocatable:: arbSeqIndexList(:)
  integer, allocatable:: collocation(:)
  logical:: loopResult
  type(ESMF_DistGridMatch_Flag):: matchResult
  logical:: arbSeqIndexFlag
  logical:: distgridBool
  integer:: connectionCount
  type(ESMF_DistGridConnection), allocatable:: connectionList(:)


  character, allocatable :: buffer(:)
  integer :: buff_len, offset
  integer :: alloc_err
  type(ESMF_InquireFlag) :: inquireflag

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
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing DistGrid IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_DistGridIsCreated(distgrid)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing DistGrid IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_DistGridIsCreated(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test DistGrid for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing DistGrid IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_DistGridIsCreated(distgrid)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing DistGrid IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_DistGridIsCreated(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test DistGrid for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing DistGrid IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_DistGridIsCreated(distgrid)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing DistGrid IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_DistGridIsCreated(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGrid equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgridBool = (distgridAlias.eq.distgrid)
  call ESMF_Test(.not.distgridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_DistGridAssignment(=)()
  write(name, *) "DistGrid assignment and equality Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgridAlias = distgrid
  distgridBool = (distgridAlias.eq.distgrid)
  call ESMF_Test(distgridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_DistGridOperator(==)()
  write(name, *) "DistGrid equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgridBool = (distgridAlias==distgrid)
  call ESMF_Test(.not.distgridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_DistGridOperator(/=)()
  write(name, *) "DistGrid non-equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgridBool = (distgridAlias/=distgrid)
  call ESMF_Test(distgridBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Double DistGridDestroy through alias Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgridAlias, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - dimCount, tileCount, deCount, regDecompFlag, DELayout"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
    deCount=deCount, regDecompFlag=regDecompFlag, delayout=delayout, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify dimCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((dimCount == 1), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify tileCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((tileCount == 1), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify deCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((deCount == petCount), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify regDecompFlag"
  write(failMsg, *) "Wrong result"
  call ESMF_Test(regDecompFlag, &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DELayoutGet() - deCount, localDeCount"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DELayoutGet(delayout, deCount=deCount, localDeCount=localDeCount, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify deCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((deCount == petCount), &
    name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify localDeCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((localDeCount == 1), &
    name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - minIndexPTile(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(minIndexPTile(dimCount,tileCount))
  call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify minIndexPTile(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((minIndexPTile(1,1) == 1), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(minIndexPTile)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - maxIndexPTile(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(maxIndexPTile(dimCount,tileCount))
  call ESMF_DistGridGet(distgrid, maxIndexPTile=maxIndexPTile, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify maxIndexPTile(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((maxIndexPTile(1,1) == 1000), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(maxIndexPTile)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - elementCountPTile(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(elementCountPTile(tileCount))
  call ESMF_DistGridGet(distgrid, elementCountPTile=elementCountPTile, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify elementCountPTile(:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((elementCountPTile(1) == 1000), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(elementCountPTile)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - minIndexPDe(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(minIndexPDe(dimCount,deCount))
  call ESMF_DistGridGet(distgrid, minIndexPDe=minIndexPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify minIndexPDe(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((minIndexPDe(1,1) == 1), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(minIndexPDe)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - maxIndexPDe(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(maxIndexPDe(dimCount,deCount))
  call ESMF_DistGridGet(distgrid, maxIndexPDe=maxIndexPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify maxIndexPDe(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((maxIndexPDe(1,deCount) == 1000), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(maxIndexPDe)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - elementCountPDe(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(elementCountPDe(0:deCount-1))
  call ESMF_DistGridGet(distgrid, elementCountPDe=elementCountPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify elementCountPDe(:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=0, deCount-1
    if ((elementCountPDe(i) < 1000/deCount) .or. &
      (elementCountPDe(i) > 1000/deCount + 1000 - (1000/deCount)*deCount)) &
      loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - deToTileMap(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(deToTileMap(0:deCount-1))
  call ESMF_DistGridGet(distgrid, deToTileMap=deToTileMap, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify deToTileMap(:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=0, deCount-1
    if (deToTileMap(i) /= 1) loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(deToTileMap)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - indexCountPDe(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(indexCountPDe(dimCount,0:deCount-1))
  call ESMF_DistGridGet(distgrid, indexCountPDe=indexCountPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify indexCountPDe(:,:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=0, deCount-1
    if ((indexCountPDe(1,i) < 1000/deCount) .or. &
      (indexCountPDe(1,i) > 1000/deCount + 1000 - (1000/deCount)*deCount)) &
      loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DELayoutGet() - localDeToDeMap"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(localDeToDeMap(0:localDeCount-1))
  call ESMF_DELayoutGet(delayout, localDeToDeMap=localDeToDeMap, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify localDeToDeMap"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((localDeToDeMap(0) == localPet), &
    name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - indexList(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(indexList(indexCountPDe(1,localDeToDeMap(0))))
  call ESMF_DistGridGet(distgrid, localDe=0, dim=1, indexList=indexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(indexList)
  deallocate(indexCountPDe)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - elementCount"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, localDe=0, elementCount=elementCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "verify elementCount"
  write(failMsg, *) "Did not match"
  call ESMF_Test((elementCount.eq.elementCountPDe(localDeToDeMap(0))),  &
      name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - seqIndexList(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(seqIndexList(elementCountPDe(localDeToDeMap(0))))
  call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=seqIndexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify seqIndexList(:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=1, elementCountPDe(localDeToDeMap(0))
    if (seqIndexList(i) /= seqIndexList(1)+(i-1)) &
      loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(seqIndexList)
  deallocate(elementCountPDe)
  deallocate(localDeToDeMap)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - connectionCount"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, connectionCount=connectionCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "verify connectionCount"
  write(failMsg, *) "Did not match"
  call ESMF_Test((connectionCount == 0), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - connectionList"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(connectionList(connectionCount))
  call ESMF_DistGridGet(distgrid, connectionList=connectionList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(connectionList)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrids - matchResult"
  write(failMsg, *) "matchResult not ESMF_DISTGRIDMATCH_ALIAS"
  call ESMF_Test(matchResult==ESMF_DISTGRIDMATCH_ALIAS, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - invalid DistGrid object"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - invalid DistGrid object - matchResult"
  write(failMsg, *) "matchResult not ESMF_DISTGRIDMATCH_INVALID"
  call ESMF_Test(matchResult==ESMF_DISTGRIDMATCH_INVALID, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrid aliases"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = distgrid
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrid aliases - matchResult"
  write(failMsg, *) "matchResult not ESMF_DISTGRIDMATCH_ALIAS"
  call ESMF_Test(matchResult==ESMF_DISTGRIDMATCH_ALIAS, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids that are exact match"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids that are exact match - matchResult"
  write(failMsg, *) "matchResult not ESMF_DISTGRIDMATCH_EXACT"
  call ESMF_Test(matchResult==ESMF_DISTGRIDMATCH_EXACT, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - from DistGrid (deep copy)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid3 = ESMF_DistGridCreate(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - from DistGrid re-created copy"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid3 = ESMF_DistGridCreate(distgrid, indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - from DistGrid with extra edge elements"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid3 = ESMF_DistGridCreate(distgrid, firstExtra=(/1/), lastExtra=(/5/), &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ESMF_DistGridConnectionSet() - Set a single connection"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(connectionList(1))
  call ESMF_DistGridConnectionSet(connectionList(1), &
    tileIndexA=1, tileIndexB=1, positionVector=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridConnectionPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridConnectionPrint(connectionList(1), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile Default with connectionList"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = ESMF_DistGridCreate(minIndex=(/0/), maxIndex=(/999/), &
    connectionList=connectionList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(connectionList)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - connectionCount"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid2, connectionCount=connectionCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "verify connectionCount"
  write(failMsg, *) "Did not match"
  call ESMF_Test((connectionCount == 1), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - connectionList"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(connectionList(connectionCount))
  call ESMF_DistGridGet(distgrid2, connectionList=connectionList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridConnectionPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridConnectionPrint(connectionList(1), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(connectionList)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids - matchResult"
  write(failMsg, *) "matchResult not ESMF_DISTGRIDMATCH_NONE"
  call ESMF_Test(matchResult==ESMF_DISTGRIDMATCH_NONE, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile Default - multiple of 4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/100/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile Default - no multiple of 4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/90/), &
    regDecompFirstExtra=(/3/), regDecompLastExtra=(/9/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile w/ deBlockList incorrect deCount"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  allocate(deBlockList(1,2,petCount+1))
  delayout = ESMF_DELayoutCreate(rc=rc) ! creates DELayout with petCount DEs
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/100/), &
    deBlockList=deBlockList, delayout=delayout, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(deBlockList)
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile w/ deBlockList out-of-range"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  allocate(deBlockList(1,2,4))
  deBlockList(1,:,1) = (/1,20/)
  deBlockList(1,:,2) = (/21,40/)
  deBlockList(1,:,3) = (/41,60/)
  deBlockList(1,:,4) = (/61,101/)
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/100/), &
    deBlockList=deBlockList, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(deBlockList)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Tile w/ deBlockList"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(deBlockList(1,2,4))
  deBlockList(1,:,1) = (/1,20/)
  deBlockList(1,:,2) = (/21,40/)
  deBlockList(1,:,3) = (/41,60/)
  deBlockList(1,:,4) = (/61,100/)
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/100/), &
    deBlockList=deBlockList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(deBlockList)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - from DistGrid re-created copy"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid3 = ESMF_DistGridCreate(distgrid, indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - from DistGrid with extra edge elements"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid3 = ESMF_DistGridCreate(distgrid, firstExtra=(/2/), lastExtra=(/3/), &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D arbitrary seq indices"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  localStart = localPet*10
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=(/localStart, localStart+1/), &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - insert 1D arbitrary seq indices into regular 2D tile at arbDim=2 -> 3D total"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  localStart = localPet*10
  allocate(arbSeqIndexList((localPet+1)*2))
  do i=1,(localPet+1)*2
    arbSeqIndexList(i)=localStart+i
  enddo
  distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, &
    arbDim=2, minIndexPTile=(/1,1/), maxIndexPTile=(/5,7/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(arbSeqIndexList)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridPrint(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - dimCount, collocation"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(collocation(3))  ! dimCount
  call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
    collocation=collocation, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify dimCount"
  write(failMsg, *) "Wrong dimCount"
  call ESMF_Test((dimCount.eq.3), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtain arbSeqIndexFlag for dim=1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, localDe=0, collocation=collocation(1), &
    arbSeqIndexFlag=arbSeqIndexFlag, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify arbSeqIndexFlag for dim=1"
  write(failMsg, *) "Wrong arbSeqIndexFlag"
  call ESMF_Test((.not.arbSeqIndexFlag), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtain arbSeqIndexFlag for dim=2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, localDe=0, collocation=collocation(2), &
    arbSeqIndexFlag=arbSeqIndexFlag, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify arbSeqIndexFlag for dim=2"
  write(failMsg, *) "Wrong arbSeqIndexFlag"
  call ESMF_Test((arbSeqIndexFlag), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Obtain arbSeqIndexFlag for dim=3"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, localDe=0, collocation=collocation(3), &
    arbSeqIndexFlag=arbSeqIndexFlag, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify arbSeqIndexFlag for dim=3"
  write(failMsg, *) "Wrong arbSeqIndexFlag"
  call ESMF_Test((.not.arbSeqIndexFlag), name, failMsg, result, ESMF_SRCLINE)

  deallocate(collocation)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! test the serialize inquire-only option
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  write(name, *) "Computing space for serialization buffer"
  write(failMsg, *) "Size could not be determined"
  buff_len = 1
  allocate (buffer(buff_len))
  offset = 0
  inquireflag  = ESMF_INQUIREONLY
  call c_esmc_distgridserialize (distgrid, buffer, buff_len, offset,  &
      inquireflag, rc)
  print *, 'computed serialization buffer length =', offset, ' bytes'
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate (buffer)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Allocate serialization buffer"
  write(failMsg, *) "Size was illegal"
  buff_len = offset
  allocate (buffer(buff_len), stat=alloc_err)
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, alloc_err == 0)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  ! test actually doing the serialization
  ! WARNING: This is testing an INTERNAL method.  It is NOT
  ! part of the supported ESMF user API!
  write(name, *) "Serialization DistGrid data"
  write(failMsg, *) "Serialization failed"
  buff_len = size (buffer)
  offset = 0
  inquireflag  = ESMF_NOINQUIRE
  call c_esmc_distgridserialize (distgrid, buffer, buff_len, offset,  &
      inquireflag, rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !-----------------------------------------------------------------------------
  ! Multi tile tests
  !-----------------------------------------------------------------------------

  dimCount  = 2
  tileCount = 4
  allocate(minIndexPTile(dimCount,tileCount))
  allocate(maxIndexPTile(dimCount,tileCount))
  minIndexPTile(:,1) = (/11,1/)
  maxIndexPTile(:,1) = (/20,10/)
  minIndexPTile(:,2) = (/11,11/)
  maxIndexPTile(:,2) = (/20,20/)
  minIndexPTile(:,3) = (/1,11/)
  maxIndexPTile(:,3) = (/10,20/)
  minIndexPTile(:,4) = (/1,1/)
  maxIndexPTile(:,4) = (/10,10/)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 2D Multi Tile Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
    maxIndexPTile=maxIndexPTile, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
  deallocate(minIndexPTile, maxIndexPTile)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - elementCount"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, localDe=0, elementCount=elementCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - seqIndexList(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(seqIndexList(elementCount))
  call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=seqIndexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify seqIndexList(:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=1, elementCount
    if (seqIndexList(i) /= localPet*100 + i) &
      loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(seqIndexList)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
10 continue
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_DistGridCreateGetUTest
