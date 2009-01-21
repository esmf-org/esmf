! $Id: ESMF_DistGridCreateGetUTest.F90,v 1.4.2.8 2009/01/21 21:25:20 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_DistGridCreateGetUTest.F90,v 1.4.2.8 2009/01/21 21:25:20 cdeluca Exp $'
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
  type(ESMF_DistGrid):: distgrid, distgrid2
  type(ESMF_DELayout):: delayout
  integer:: dimCount, patchCount, deCount
  type(ESMF_Logical):: regDecompFlag
  integer, allocatable:: elementCountPPatch(:), patchListPDe(:), elementCountPDe(:)
  integer, allocatable:: minIndexPDimPPatch(:,:), maxIndexPDimPPatch(:,:)
  integer, allocatable:: minIndexPDimPDe(:,:), maxIndexPDimPDe(:,:)
  integer, allocatable:: indexCountPDimPDe(:,:), localDeList(:)
  integer, allocatable:: indexList(:), seqIndexList(:)
  integer, allocatable:: deBlockList(:,:,:)
  logical:: loopResult
  type(ESMF_Logical):: matchResult

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
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Patch Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - dimCount, patchCount, regDecompFlag, DELayout"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridGet(distgrid, dimCount=dimCount, patchCount=patchCount, &
    regDecompFlag=regDecompFlag, delayout=delayout, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify dimCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((dimCount == 1), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify patchCount"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((patchCount == 1), &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify regDecompFlag"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((regDecompFlag == ESMF_TRUE), &
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
  write(name, *) "DistGridGet() - minIndexPDimPPatch(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(minIndexPDimPPatch(dimCount,patchCount))
  call ESMF_DistGridGet(distgrid, minIndexPDimPPatch=minIndexPDimPPatch, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify minIndexPDimPPatch(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((minIndexPDimPPatch(1,1) == 1), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(minIndexPDimPPatch)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - maxIndexPDimPPatch(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(maxIndexPDimPPatch(dimCount,patchCount))
  call ESMF_DistGridGet(distgrid, maxIndexPDimPPatch=maxIndexPDimPPatch, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify maxIndexPDimPPatch(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((maxIndexPDimPPatch(1,1) == 1000), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(maxIndexPDimPPatch)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - elementCountPPatch(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(elementCountPPatch(patchCount))
  call ESMF_DistGridGet(distgrid, elementCountPPatch=elementCountPPatch, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify elementCountPPatch(:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((elementCountPPatch(1) == 1000), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(elementCountPPatch)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - minIndexPDimPDe(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(minIndexPDimPDe(dimCount,deCount))
  call ESMF_DistGridGet(distgrid, minIndexPDimPDe=minIndexPDimPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify minIndexPDimPDe(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((minIndexPDimPDe(1,1) == 1), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(minIndexPDimPDe)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - maxIndexPDimPDe(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(maxIndexPDimPDe(dimCount,deCount))
  call ESMF_DistGridGet(distgrid, maxIndexPDimPDe=maxIndexPDimPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify maxIndexPDimPDe(:,:)"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((maxIndexPDimPDe(1,deCount) == 1000), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(maxIndexPDimPDe)

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
  write(name, *) "DistGridGet() - patchListPDe(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(patchListPDe(0:deCount-1))
  call ESMF_DistGridGet(distgrid, patchListPDe=patchListPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify patchListPDe(:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=0, deCount-1
    if (patchListPDe(i) /= 1) loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(patchListPDe)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - indexCountPDimPDe(:,:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(indexCountPDimPDe(dimCount,0:deCount-1))
  call ESMF_DistGridGet(distgrid, indexCountPDimPDe=indexCountPDimPDe, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify indexCountPDimPDe(:,:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=0, deCount-1
    if ((indexCountPDimPDe(1,i) < 1000/deCount) .or. &
      (indexCountPDimPDe(1,i) > 1000/deCount + 1000 - (1000/deCount)*deCount)) &
      loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DELayoutGet() - localDeList"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(localDeList(0:localDeCount-1))
  call ESMF_DELayoutGet(delayout, localDeList=localDeList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify localDeList"
  write(failMsg, *) "Wrong result"
  call ESMF_Test((localDeList(0) == localPet), &
    name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - indexList(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(indexList(indexCountPDimPDe(1,localDeList(0))))
  call ESMF_DistGridGet(distgrid, localDe=0, dim=1, indexList=indexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(indexList)
  deallocate(indexCountPDimPDe)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridGet() - seqIndexList(:)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(seqIndexList(elementCountPDe(localDeList(0))))
  call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=seqIndexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify seqIndexList(:)"
  write(failMsg, *) "Wrong result"
  loopResult = .true.
  do i=1, elementCountPDe(localDeList(0))
    if (seqIndexList(i) /= seqIndexList(1)+(i-1)) &
      loopResult = .false.
  enddo
  call ESMF_Test(loopResult, &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(seqIndexList)
  deallocate(elementCountPDe)
  deallocate(localDeList)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrids - matchResult"
  write(failMsg, *) "matchResult not ESMF_TRUE"
  call ESMF_Test((matchResult.eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - invalid DistGrid object"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = distgrid
  matchResult = ESMF_DistGridMatch(distgrid, distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - identical DistGrids - matchResult"
  write(failMsg, *) "matchResult not ESMF_TRUE"
  call ESMF_Test((matchResult.eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Patch Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1000/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids that are the same"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids that are the same - matchResult"
  write(failMsg, *) "matchResult not ESMF_TRUE"
  call ESMF_Test((matchResult.eq.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Patch Default"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2 = ESMF_DistGridCreate(minIndex=(/0/), maxIndex=(/999/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  matchResult = ESMF_DistGridMatch(distgrid, distgrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridMatch() - different DistGrids - matchResult"
  write(failMsg, *) "matchResult not ESMF_FALSE"
  call ESMF_Test((matchResult.ne.ESMF_TRUE), name, failMsg, result, ESMF_SRCLINE)
  
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
  write(name, *) "DistGridCreate() - 1D Single Patch Default - multiple of 4"
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
  write(name, *) "DistGridCreate() - 1D Single Patch Default - no multiple of 4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/90/), &
    decompflag=(/ESMF_DECOMP_HOMOGEN/), &
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
  write(name, *) "DistGridCreate() - 1D Single Patch w/ deBlockList incorrect deCount"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  allocate(deBlockList(1,2,petCount+1))
  delayout = ESMF_DELayoutCreate(rc=rc) ! creates DELayout with petCount DEs
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/100/), &
    deBlockList=deBlockList, delayout=delayout, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(deBlockList)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "DistGridCreate() - 1D Single Patch w/ deBlockList out-of-range"
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
  write(name, *) "DistGridCreate() - 1D Single Patch w/ deBlockList"
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
  write(name, *) "DistGridDestroy()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
10 continue
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_DistGridCreateGetUTest
