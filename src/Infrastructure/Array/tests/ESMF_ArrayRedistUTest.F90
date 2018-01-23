! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArrayRedistUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayRedistUTest -  Tests ArrayRedist()
!
! !DESCRIPTION:
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

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  ! Local variables
  type(ESMF_VM)         :: vm
  type(ESMF_DistGrid)   :: srcDistgrid, dstDistgrid
  type(ESMF_Array)      :: srcArray, dstArray
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_RouteHandle):: routehandle
  integer(ESMF_KIND_I4), pointer :: farrayPtr(:)  ! matching Fortran array pointer
#ifdef ESMF_TESTEXHAUSTIVE
  integer, allocatable  :: deBlockList(:,:,:)
  type(ESMF_DistGrid)   :: srcDistgridWHoles
  type(ESMF_DistGrid)   :: srcDistgrid2, dstDistgridWrong
  type(ESMF_Array)      :: srcArray2, srcArray3
  type(ESMF_Array)      :: dstArray2, dstArray3, dstArrayWrong
  type(ESMF_ArraySpec)  :: arrayspec3
  type(ESMF_ArraySpec)  :: arrayspec4, arrayspec5
  type(ESMF_Array)      :: srcArray4, dstArray5
  type(ESMF_Array)      :: srcArray6, dstArray6, dstArray6p
  type(ESMF_Array)      :: srcArray7, dstArray7
  type(ESMF_Array)      :: srcArray8, dstArray8
  real(ESMF_KIND_R8), pointer :: farrayPtr4(:)  ! matching Fortran array pointer
  real(ESMF_KIND_R4), pointer :: farrayPtr5(:)  ! matching Fortran array pointer
  type(ESMF_RouteHandle):: routehandle45
  type(ESMF_RouteHandle):: routehandle3, routehandle36
  type(ESMF_RouteHandle):: routehandle66, routehandle66p
  integer(ESMF_KIND_I4), pointer :: farrayPtr2D(:,:)! matching Fortran array pointer
  integer               :: j
  logical               :: finishedflag, cancelledflag, evalflag
#endif
  integer               :: rc, i, petCount, localPet
  integer, allocatable  :: srcIndices(:)
  logical               :: isCreated
  character(1024)       :: msgString

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0


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
  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  if (petCount /= 6) then
    print *, "This system test needs to run on exactly 6 PETs, petCount = ", &
      petCount
    goto 10
  endif
  
!  call ESMF_LogSet(flush=.true.)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/42/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstArray Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The dstDistgrid evenly divides 42 elements across the 6 DEs (becaues default
  ! is 1 DE per PET and there are 6 PETs running this test).
  ! The default sequenceIndex of dstDistGrid is determined by the default rule
  ! of simply enumerating the elements within the tile, starting at 1:
  !
  ! PET   localDE   DE    indices
  ! 0     0         0     1, 2, 3, 4, 5, 6, 7
  ! 1     0         1     8, 9, 10, 11, 12, 13, 14
  ! 2     0         2     15, 16, 17, 18, 19, 20, 21
  ! 3     0         3     22, 23, 24, 25, 26, 27, 28
  ! 4     0         4     29, 30, 31, 32, 33, 34, 35
  ! 5     0         5     36, 37, 38, 39, 40, 41, 42
  !
  ! The dstArray created on the dstDistgrid has the following shape and
  ! initialization:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     random, random, random, random, random, random, random
  ! 1     0         1     random, random, random, random, random, random, random
  ! 2     0         2     random, random, random, random, random, random, random
  ! 3     0         3     random, random, random, random, random, random, random
  ! 4     0         4     random, random, random, random, random, random, random
  ! 5     0         5     random, random, random, random, random, random, random

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray2 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec rank=2 Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec3, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray3 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray3 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=dstDistgrid, &
    undistLBound=(/1/), undistUBound=(/2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec rank=1, R4 Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec5, typekind=ESMF_TYPEKIND_R4, rank=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray5 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray5 = ESMF_ArrayCreate(arrayspec=arrayspec5, distgrid=dstDistgrid, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray6 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2/), undistLBound=(/1/), undistUBound=(/2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6p Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray6p = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2/), undistLBound=(/1/), undistUBound=(/2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray7 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray7 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=dstDistgrid, &
    distgridToArrayMap=(/2/), undistLBound=(/1/), undistUBound=(/3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray8 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray8 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=dstDistgrid, &
    distgridToArrayMap=(/1/), undistLBound=(/1/), undistUBound=(/3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcDistgrid = ESMF_DistGridCreate(minIndex=(/11/), maxIndex=(/52/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcArray Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
    farrayPtr(i) = localPet * 10 + i    ! initialize
  enddo
  
  ! The lbound(farrayPtr, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray contents are locally set to:
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     1, 2, 3, 4, 5, 6, 7
  ! 1     0         1     11, 12, 13, 14, 15, 16, 17
  ! 2     0         2     21, 22, 23, 24, 25, 26, 27
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57
  

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing RouteHandle IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_RouteHandleIsCreated(routehandle)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing RouteHandle IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_RouteHandleIsCreated(routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing RouteHandle IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_RouteHandleIsCreated(routehandle)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing RouteHandle IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_RouteHandleIsCreated(routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray -> dstArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, checkflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray into dstArray is:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     1, 2, 3, 4, 5, 6, 7
  ! 1     0         1     11, 12, 13, 14, 15, 16, 17
  ! 2     0         2     21, 22, 23, 24, 25, 26, 27
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstArray Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  call ESMF_ArrayPrint(dstArray)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.1).and. &
      (farrayPtr(2).eq.2).and.(farrayPtr(3).eq.3).and. &
      (farrayPtr(4).eq.4).and.(farrayPtr(5).eq.5).and. &
      (farrayPtr(6).eq.6).and.(farrayPtr(7).eq.7)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.11).and. &
      (farrayPtr(2).eq.12).and.(farrayPtr(3).eq.13).and. &
      (farrayPtr(4).eq.14).and.(farrayPtr(5).eq.15).and. &
      (farrayPtr(6).eq.16).and.(farrayPtr(7).eq.17)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.21).and. &
      (farrayPtr(2).eq.22).and.(farrayPtr(3).eq.23).and. &
      (farrayPtr(4).eq.24).and.(farrayPtr(5).eq.25).and. &
      (farrayPtr(6).eq.26).and.(farrayPtr(7).eq.27)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.31).and. &
      (farrayPtr(2).eq.32).and.(farrayPtr(3).eq.33).and. &
      (farrayPtr(4).eq.34).and.(farrayPtr(5).eq.35).and. &
      (farrayPtr(6).eq.36).and.(farrayPtr(7).eq.37)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.41).and. &
      (farrayPtr(2).eq.42).and.(farrayPtr(3).eq.43).and. &
      (farrayPtr(4).eq.44).and.(farrayPtr(5).eq.45).and. &
      (farrayPtr(6).eq.46).and.(farrayPtr(7).eq.47)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.51).and. &
      (farrayPtr(2).eq.52).and.(farrayPtr(3).eq.53).and. &
      (farrayPtr(4).eq.54).and.(farrayPtr(5).eq.55).and. &
      (farrayPtr(6).eq.56).and.(farrayPtr(7).eq.57)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing RouteHandle IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_RouteHandleIsCreated(routehandle)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing RouteHandle IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_RouteHandleIsCreated(routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore w/ srcToDstTransposeMap Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    srcToDstTransposeMap=(/1/), routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray -> dstArray w/ srcToDstTransposeMap Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farrayPtr = -999  ! initialize dstArray to something obvious
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray into dstArray is:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     1, 2, 3, 4, 5, 6, 7
  ! 1     0         1     11, 12, 13, 14, 15, 16, 17
  ! 2     0         2     21, 22, 23, 24, 25, 26, 27
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.1).and. &
      (farrayPtr(2).eq.2).and.(farrayPtr(3).eq.3).and. &
      (farrayPtr(4).eq.4).and.(farrayPtr(5).eq.5).and. &
      (farrayPtr(6).eq.6).and.(farrayPtr(7).eq.7)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.11).and. &
      (farrayPtr(2).eq.12).and.(farrayPtr(3).eq.13).and. &
      (farrayPtr(4).eq.14).and.(farrayPtr(5).eq.15).and. &
      (farrayPtr(6).eq.16).and.(farrayPtr(7).eq.17)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.21).and. &
      (farrayPtr(2).eq.22).and.(farrayPtr(3).eq.23).and. &
      (farrayPtr(4).eq.24).and.(farrayPtr(5).eq.25).and. &
      (farrayPtr(6).eq.26).and.(farrayPtr(7).eq.27)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.31).and. &
      (farrayPtr(2).eq.32).and.(farrayPtr(3).eq.33).and. &
      (farrayPtr(4).eq.34).and.(farrayPtr(5).eq.35).and. &
      (farrayPtr(6).eq.36).and.(farrayPtr(7).eq.37)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.41).and. &
      (farrayPtr(2).eq.42).and.(farrayPtr(3).eq.43).and. &
      (farrayPtr(4).eq.44).and.(farrayPtr(5).eq.45).and. &
      (farrayPtr(6).eq.46).and.(farrayPtr(7).eq.47)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.51).and. &
      (farrayPtr(2).eq.52).and.(farrayPtr(3).eq.53).and. &
      (farrayPtr(4).eq.54).and.(farrayPtr(5).eq.55).and. &
      (farrayPtr(6).eq.56).and.(farrayPtr(7).eq.57)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcArray Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

!------------------------------------------------------------------------
  ! Testing a srcArray where the DEs do not fully cover the index space
  ! defined by the DistGrid minIndex/maxIndex. Such a DistGrid with holes
  ! can be set up by using a deBlockList.

  allocate(deBlockList(1,2,0:5))  ! dimCount, 2, deCount
  deBlockList(1,:,0) = (/103,110/)  ! DE 0
  deBlockList(1,:,1) = (/122,125/)  ! DE 1
  deBlockList(1,:,2) = (/117,120/)  ! DE 2
  deBlockList(1,:,3) = (/126,131/)  ! DE 3
  deBlockList(1,:,4) = (/139,141/)  ! DE 4
  deBlockList(1,:,5) = (/133,138/)  ! DE 5

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgridWHoles Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcDistgridWHoles = ESMF_DistGridCreate(minIndex=(/101/), maxIndex=(/142/), &
    deBlockList=deBlockList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgridWHoles, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
    farrayPtr(i) = i    ! initialize with a value equal to the global index
  enddo
  
  ! The array bounds are in global index space. Therefore:
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     103, 104, 105, 106, 107, 108, 109, 110
  ! 1     0         1     122, 123, 124, 125
  ! 2     0         2     117, 118, 119, 120
  ! 3     0         3     126, 127, 128, 129, 130, 131
  ! 4     0         4     139, 140, 141
  ! 5     0         5     133, 134, 135, 136, 137, 138
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray -> dstArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, checkflag=.true., rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! To understand the expected redistribution result keep these points in mind:
  !   1) Sequence indices are tied to the tile order, and the canonical order
  !      of elements within the tiles. Sequence indices are unaffected by 
  !      decomposition and distribution! Therefore, on the srcArray side with
  !      the srcDistgridWHoles, the crazy deBlockList has no effect on how
  !      the elements are labeled by sequence indices. It is still simply
  !      a DistGrid with one tile that has (142-101+1) = 42 elements. Therefore
  !      the sequence index goes from 1->42, simply labeling the elements.
  !      However, not all 42 elements are present, but that does not affect
  !      the labeling.
  !   2) It does not matter which DE on the src side holds a specific index
  !      space range of the tile, the sequence index is connected to the 
  !      index space and not the DE numbering.
  !   3) The dstArray is _not_ being zero'ed out during the Redist, and since 
  !      there are holes in the srcArray some of the dstArray elements stay 
  !      unchanged, i.e. they keep the value of the previous Redist operation.
  ! The expected result of the redistribution of srcArray into dstArray is:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     1, 2, 103, 104, 105, 106, 107
  ! 1     0         1     108, 109, 110, 14, 15, 16, 17
  ! 2     0         2     21, 22, 117, 118, 119, 120, 27
  ! 3     0         3     122, 123, 124, 125, 126, 127, 128
  ! 4     0         4     129, 130, 131, 44, 133, 134, 135
  ! 5     0         5     136, 137, 138, 139, 140, 141, 57
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.1).and. &
      (farrayPtr(2).eq.2).and.(farrayPtr(3).eq.103).and. &
      (farrayPtr(4).eq.104).and.(farrayPtr(5).eq.105).and. &
      (farrayPtr(6).eq.106).and.(farrayPtr(7).eq.107)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.108).and. &
      (farrayPtr(2).eq.109).and.(farrayPtr(3).eq.110).and. &
      (farrayPtr(4).eq.14).and.(farrayPtr(5).eq.15).and. &
      (farrayPtr(6).eq.16).and.(farrayPtr(7).eq.17)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.21).and. &
      (farrayPtr(2).eq.22).and.(farrayPtr(3).eq.117).and. &
      (farrayPtr(4).eq.118).and.(farrayPtr(5).eq.119).and. &
      (farrayPtr(6).eq.120).and.(farrayPtr(7).eq.27)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.122).and. &
      (farrayPtr(2).eq.123).and.(farrayPtr(3).eq.124).and. &
      (farrayPtr(4).eq.125).and.(farrayPtr(5).eq.126).and. &
      (farrayPtr(6).eq.127).and.(farrayPtr(7).eq.128)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.129).and. &
      (farrayPtr(2).eq.130).and.(farrayPtr(3).eq.131).and. &
      (farrayPtr(4).eq.44).and.(farrayPtr(5).eq.133).and. &
      (farrayPtr(6).eq.134).and.(farrayPtr(7).eq.135)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.136).and. &
      (farrayPtr(2).eq.137).and.(farrayPtr(3).eq.138).and. &
      (farrayPtr(4).eq.139).and.(farrayPtr(5).eq.140).and. &
      (farrayPtr(6).eq.141).and.(farrayPtr(7).eq.57)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Reverse ArrayRedistStore with holes Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ! This direction requires ignoreUnmatchedIndices=.true. because the
  ! sparse matrix is costructed from the srcArray, but now the dstArray
  ! has holes, and that means the sparse matrix has entries with indices
  ! that are not found on the dstArray side.
  call ESMF_ArrayRedistStore(srcArray=dstArray, dstArray=srcArray, &
    ignoreUnmatchedIndices=.true., routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgridWHoles Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(srcDistgridWHoles, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  deallocate(deBlockList)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Array Spec rank=1, R8 Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec4, typekind=ESMF_TYPEKIND_R8, rank=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray4 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcArray4 = ESMF_ArrayCreate(arrayspec=arrayspec4, distgrid=srcDistgrid, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray4, farrayPtr=farrayPtr4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr4, 1), ubound(farrayPtr4, 1)
    farrayPtr4(i) = real(localPet * 10 + i, ESMF_KIND_R8)    ! initialize
  enddo
  
  ! The lbound(farrayPtr, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray contents are locally set to:
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     1, 2, 3, 4, 5, 6, 7
  ! 1     0         1     11, 12, 13, 14, 15, 16, 17
  ! 2     0         2     21, 22, 23, 24, 25, 26, 27
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray4, dstArray=dstArray5, &
    routehandle=routehandle45, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray4 -> dstArray5 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray4, dstArray=dstArray5, &
    routehandle=routehandle45, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray into dstArray is:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     1, 2, 3, 4, 5, 6, 7
  ! 1     0         1     11, 12, 13, 14, 15, 16, 17
  ! 2     0         2     21, 22, 23, 24, 25, 26, 27
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray5 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray5, farrayPtr=farrayPtr5, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  call ESMF_ArrayPrint(dstArray5)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr5(1).eq.1).and. &
      (farrayPtr5(2).eq.2).and.(farrayPtr5(3).eq.3).and. &
      (farrayPtr5(4).eq.4).and.(farrayPtr5(5).eq.5).and. &
      (farrayPtr5(6).eq.6).and.(farrayPtr5(7).eq.7)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr5(1).eq.11).and. &
      (farrayPtr5(2).eq.12).and.(farrayPtr5(3).eq.13).and. &
      (farrayPtr5(4).eq.14).and.(farrayPtr5(5).eq.15).and. &
      (farrayPtr5(6).eq.16).and.(farrayPtr5(7).eq.17)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr5(1).eq.21).and. &
      (farrayPtr5(2).eq.22).and.(farrayPtr5(3).eq.23).and. &
      (farrayPtr5(4).eq.24).and.(farrayPtr5(5).eq.25).and. &
      (farrayPtr5(6).eq.26).and.(farrayPtr5(7).eq.27)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr5(1).eq.31).and. &
      (farrayPtr5(2).eq.32).and.(farrayPtr5(3).eq.33).and. &
      (farrayPtr5(4).eq.34).and.(farrayPtr5(5).eq.35).and. &
      (farrayPtr5(6).eq.36).and.(farrayPtr5(7).eq.37)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr5(1).eq.41).and. &
      (farrayPtr5(2).eq.42).and.(farrayPtr5(3).eq.43).and. &
      (farrayPtr5(4).eq.44).and.(farrayPtr5(5).eq.45).and. &
      (farrayPtr5(6).eq.46).and.(farrayPtr5(7).eq.47)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr5(1).eq.51).and. &
      (farrayPtr5(2).eq.52).and.(farrayPtr5(3).eq.53).and. &
      (farrayPtr5(4).eq.54).and.(farrayPtr5(5).eq.55).and. &
      (farrayPtr5(6).eq.56).and.(farrayPtr5(7).eq.57)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle45 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle45, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray4 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray5 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray5, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgrid Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(srcDistGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  ! Prepare for arbitrary sequence indices
  allocate(srcIndices((localPet+1)*2))   ! sizes: 2, 4, 6, 8, 10, 12
  
  if (localPet == 0) then
    do i=1,2
      srcIndices(i) = 42 - (i - 1)
    enddo
  else if (localPet == 1) then
    do i=1,4
      srcIndices(i) = 40 - (i - 1)
    enddo
  else if (localPet == 2) then
    do i=1,6
      srcIndices(i) = 36 - (i - 1)
    enddo
  else if (localPet == 3) then
    do i=1,8
      srcIndices(i) = 30 - (i - 1)
    enddo
  else if (localPet == 4) then
    do i=1,10
      srcIndices(i) = 22 - (i - 1)
    enddo
  else if (localPet == 5) then
    do i=1,12
      srcIndices(i) = 12 - (i - 1)
    enddo
  endif
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcDistgrid = ESMF_DistGridCreate(arbSeqIndexList=srcIndices, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!  call ESMF_DistGridPrint(srcDistgrid, rc=rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! The srcDistgrid has 1 DE per PET, i.e. 6 DEs. Each DE has a different
  ! number of local elements in the DistGrid. The arbitrary sequence indices are
  ! constructed to look like this:
  !
  ! PET   localDE   DE    indices
  ! 0     0         0     42, 41
  ! 1     0         1     40, 39, 38, 37
  ! 2     0         2     36, 35, 34, 33, 32, 31
  ! 3     0         3     30, 29, 28, 27, 26, 25, 24, 23
  ! 4     0         4     22, 21, 20, 19, 18, 17, 16, 15, 14, 13
  ! 5     0         5     12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
  
#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgrid2 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcDistgrid2 = ESMF_DistGridCreate(arbSeqIndexList=srcIndices, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

  deallocate(srcIndices)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcArray Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray2 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  srcArray2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=srcDistgrid2, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray3 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  srcArray3 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=srcDistgrid2, &
    undistLBound=(/0/), undistUBound=(/1/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray6 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  srcArray6 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=srcDistgrid2, &
    distgridToArrayMap=(/2/), undistLBound=(/0/), undistUBound=(/1/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray7 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  srcArray7 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=srcDistgrid2, &
    distgridToArrayMap=(/2/), undistLBound=(/0/), undistUBound=(/2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray8 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  srcArray8 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=srcDistgrid2, &
    distgridToArrayMap=(/1/), undistLBound=(/0/), undistUBound=(/2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
    farrayPtr(i) = localPet * 10 + i    ! initialize
  enddo
  
  ! The lbound(farrayPtr, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray contents are locally set to:
  !
  ! PET   localDE   DE    srcArray contents
  ! 0     0         0     1, 2
  ! 1     0         1     11, 12, 13, 14
  ! 2     0         2     21, 22, 23, 24, 25, 26
  ! 3     0         3     31, 32, 33, 34, 35, 36, 37, 38
  ! 4     0         4     41, 42, 43, 44, 45, 46, 47, 48, 49, 50
  ! 5     0         5     51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62
  
#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray2, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr, 1), ubound(farrayPtr, 1)
    farrayPtr(i) = localPet * 10 + i - 1   ! initialize
  enddo
  
  ! The lbound(farrayPtr, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray2 contents are locally set to:
  !
  ! PET   localDE   DE    srcArray2 contents
  ! 0     0         0     0, 1
  ! 1     0         1     10, 11, 12, 13
  ! 2     0         2     20, 21, 22, 23, 24, 25
  ! 3     0         3     30, 31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     40, 41, 42, 43, 44, 45, 46, 47, 48, 49
  ! 5     0         5     50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray3, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do j = lbound(farrayPtr2D, 2), ubound(farrayPtr2D, 2)
  do i = lbound(farrayPtr2D, 1), ubound(farrayPtr2D, 1)
    farrayPtr2D(i,j) = localPet * 10 + i - 1 + j*100  ! initialize
  enddo
  enddo
  
  ! The lbound(farrayPtr2D, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray3 contents are locally set to:
  !
  ! PET   localDE   DE    srcArray3 contents, tensor dimension j=0
  ! 0     0         0     0, 1
  ! 1     0         1     10, 11, 12, 13
  ! 2     0         2     20, 21, 22, 23, 24, 25
  ! 3     0         3     30, 31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     40, 41, 42, 43, 44, 45, 46, 47, 48, 49
  ! 5     0         5     50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61
  !
  ! PET   localDE   DE    srcArray3 contents, tensor dimension j=1
  ! 0     0         0     100, 101
  ! 1     0         1     110, 111, 112, 113
  ! 2     0         2     120, 121, 122, 123, 124, 125
  ! 3     0         3     130, 131, 132, 133, 134, 135, 136, 137
  ! 4     0         4     140, 141, 142, 143, 144, 145, 146, 147, 148, 149
  ! 5     0         5     150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 
  !                       160, 161

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray6 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray6, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr2D, 2), ubound(farrayPtr2D, 2)
  do j = lbound(farrayPtr2D, 1), ubound(farrayPtr2D, 1)
    farrayPtr2D(j,i) = localPet * 10 + i - 1 + j*100  ! initialize
  enddo
  enddo
  
  ! The lbound(farrayPtr2D, 2) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray6 contents are locally set to:
  !
  ! PET   localDE   DE    srcArray6 contents, tensor dimension j=0
  ! 0     0         0     0, 1
  ! 1     0         1     10, 11, 12, 13
  ! 2     0         2     20, 21, 22, 23, 24, 25
  ! 3     0         3     30, 31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     40, 41, 42, 43, 44, 45, 46, 47, 48, 49
  ! 5     0         5     50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61
  !
  ! PET   localDE   DE    srcArray6 contents, tensor dimension j=1
  ! 0     0         0     100, 101
  ! 1     0         1     110, 111, 112, 113
  ! 2     0         2     120, 121, 122, 123, 124, 125
  ! 3     0         3     130, 131, 132, 133, 134, 135, 136, 137
  ! 4     0         4     140, 141, 142, 143, 144, 145, 146, 147, 148, 149
  ! 5     0         5     150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 
  !                       160, 161

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray7 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray7, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr2D, 2), ubound(farrayPtr2D, 2)
  do j = lbound(farrayPtr2D, 1), ubound(farrayPtr2D, 1)
    farrayPtr2D(j,i) = localPet * 10 + i - 1 + j*100  ! initialize
  enddo
  enddo
  
  ! The lbound(farrayPtr2D, 2) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray7 contents are locally set to:
  !
  ! PET   localDE   DE    srcArray7 contents, tensor dimension j=0
  ! 0     0         0     0, 1
  ! 1     0         1     10, 11, 12, 13
  ! 2     0         2     20, 21, 22, 23, 24, 25
  ! 3     0         3     30, 31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     40, 41, 42, 43, 44, 45, 46, 47, 48, 49
  ! 5     0         5     50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61
  !
  ! PET   localDE   DE    srcArray7 contents, tensor dimension j=1
  ! 0     0         0     100, 101
  ! 1     0         1     110, 111, 112, 113
  ! 2     0         2     120, 121, 122, 123, 124, 125
  ! 3     0         3     130, 131, 132, 133, 134, 135, 136, 137
  ! 4     0         4     140, 141, 142, 143, 144, 145, 146, 147, 148, 149
  ! 5     0         5     150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 
  !                       160, 161
  !
  ! PET   localDE   DE    srcArray7 contents, tensor dimension j=2
  ! 0     0         0     200, 201
  ! 1     0         1     210, 211, 212, 213
  ! 2     0         2     220, 221, 222, 223, 224, 225
  ! 3     0         3     230, 231, 232, 233, 234, 235, 236, 237
  ! 4     0         4     240, 241, 242, 243, 244, 245, 246, 247, 248, 249
  ! 5     0         5     250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 
  !                       260, 261

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(srcArray8, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  do i = lbound(farrayPtr2D, 2), ubound(farrayPtr2D, 2)
  do j = lbound(farrayPtr2D, 1), ubound(farrayPtr2D, 1)
    farrayPtr2D(j,i) = localPet * 10 + j - 1 + i*100  ! initialize
  enddo
  enddo
  
  ! The lbound(farrayPtr2D, 1) = 1 because ArrayCreate() by default sets local
  ! bounds starting at 1. Thus the srcArray8 contents are locally set to:
  !
  ! PET   localDE   DE    srcArray8 contents, tensor dimension j=0
  ! 0     0         0     0, 1
  ! 1     0         1     10, 11, 12, 13
  ! 2     0         2     20, 21, 22, 23, 24, 25
  ! 3     0         3     30, 31, 32, 33, 34, 35, 36, 37
  ! 4     0         4     40, 41, 42, 43, 44, 45, 46, 47, 48, 49
  ! 5     0         5     50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61
  !
  ! PET   localDE   DE    srcArray8 contents, tensor dimension j=1
  ! 0     0         0     100, 101
  ! 1     0         1     110, 111, 112, 113
  ! 2     0         2     120, 121, 122, 123, 124, 125
  ! 3     0         3     130, 131, 132, 133, 134, 135, 136, 137
  ! 4     0         4     140, 141, 142, 143, 144, 145, 146, 147, 148, 149
  ! 5     0         5     150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 
  !                       160, 161
  !
  ! PET   localDE   DE    srcArray8 contents, tensor dimension j=2
  ! 0     0         0     200, 201
  ! 1     0         1     210, 211, 212, 213
  ! 2     0         2     220, 221, 222, 223, 224, 225
  ! 3     0         3     230, 231, 232, 233, 234, 235, 236, 237
  ! 4     0         4     240, 241, 242, 243, 244, 245, 246, 247, 248, 249
  ! 5     0         5     250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 
  !                       260, 261

#endif
    
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray -> dstArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray into dstArray is:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     62, 61, 60, 59, 58, 57, 56
  ! 1     0         1     55, 54, 53, 52, 51, 50, 49
  ! 2     0         2     48, 47, 46, 45, 44, 43, 42
  ! 3     0         3     41, 38, 37, 36, 35, 34, 33
  ! 4     0         4     32, 31, 26, 25, 24, 23, 22
  ! 5     0         5     21, 14, 13, 12, 11, 2, 1
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstArray Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  write(msgString,*) "dstArray: ", farrayPtr
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.62).and. &
      (farrayPtr(2).eq.61).and.(farrayPtr(3).eq.60).and. &
      (farrayPtr(4).eq.59).and.(farrayPtr(5).eq.58).and. &
      (farrayPtr(6).eq.57).and.(farrayPtr(7).eq.56)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.55).and. &
      (farrayPtr(2).eq.54).and.(farrayPtr(3).eq.53).and. &
      (farrayPtr(4).eq.52).and.(farrayPtr(5).eq.51).and. &
      (farrayPtr(6).eq.50).and.(farrayPtr(7).eq.49)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.48).and. &
      (farrayPtr(2).eq.47).and.(farrayPtr(3).eq.46).and. &
      (farrayPtr(4).eq.45).and.(farrayPtr(5).eq.44).and. &
      (farrayPtr(6).eq.43).and.(farrayPtr(7).eq.42)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.41).and. &
      (farrayPtr(2).eq.38).and.(farrayPtr(3).eq.37).and. &
      (farrayPtr(4).eq.36).and.(farrayPtr(5).eq.35).and. &
      (farrayPtr(6).eq.34).and.(farrayPtr(7).eq.33)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.32).and. &
      (farrayPtr(2).eq.31).and.(farrayPtr(3).eq.26).and. &
      (farrayPtr(4).eq.25).and.(farrayPtr(5).eq.24).and. &
      (farrayPtr(6).eq.23).and.(farrayPtr(7).eq.22)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.21).and. &
      (farrayPtr(2).eq.14).and.(farrayPtr(3).eq.13).and. &
      (farrayPtr(4).eq.12).and.(farrayPtr(5).eq.11).and. &
      (farrayPtr(6).eq.2).and.(farrayPtr(7).eq.1)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray2 into dstArray2 is:
  !
  ! PET   localDE   DE    dstArray2 contents
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray2 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray2, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  write(msgString,*) "dstArray2: ", farrayPtr
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray2 (RRA) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.61).and. &
      (farrayPtr(2).eq.60).and.(farrayPtr(3).eq.59).and. &
      (farrayPtr(4).eq.58).and.(farrayPtr(5).eq.57).and. &
      (farrayPtr(6).eq.56).and.(farrayPtr(7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.54).and. &
      (farrayPtr(2).eq.53).and.(farrayPtr(3).eq.52).and. &
      (farrayPtr(4).eq.51).and.(farrayPtr(5).eq.50).and. &
      (farrayPtr(6).eq.49).and.(farrayPtr(7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.47).and. &
      (farrayPtr(2).eq.46).and.(farrayPtr(3).eq.45).and. &
      (farrayPtr(4).eq.44).and.(farrayPtr(5).eq.43).and. &
      (farrayPtr(6).eq.42).and.(farrayPtr(7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.40).and. &
      (farrayPtr(2).eq.37).and.(farrayPtr(3).eq.36).and. &
      (farrayPtr(4).eq.35).and.(farrayPtr(5).eq.34).and. &
      (farrayPtr(6).eq.33).and.(farrayPtr(7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.31).and. &
      (farrayPtr(2).eq.30).and.(farrayPtr(3).eq.25).and. &
      (farrayPtr(4).eq.24).and.(farrayPtr(5).eq.23).and. &
      (farrayPtr(6).eq.22).and.(farrayPtr(7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.20).and. &
      (farrayPtr(2).eq.13).and.(farrayPtr(3).eq.12).and. &
      (farrayPtr(4).eq.11).and.(farrayPtr(5).eq.10).and. &
      (farrayPtr(6).eq.1).and.(farrayPtr(7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  ! Do the same ESMF_ArrayRedist() srcArray2->dstArray2 again, but this time
  ! using the non-blocking ArrayRedist() paradigm.

  farrayPtr = -99 ! reset to something that would be caught during verification

  ! The following barrier call holds up PET 0 from calling into ArrayRedist() 
  ! until all other PETs have called in with ESMF_ROUTESYNC_NBSTART, and have done
  ! one round of calling in with ESMF_ROUTESYNC_NBTESTFINISH. Doing this tests the
  ! non-blocking mode of ArrayRedist().
  if (localPet==0) call ESMF_VMBarrier(vm)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) ESMF_ROUTESYNC_NBSTART Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBSTART, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) NBTESTFINISH Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBTESTFINISH, &
    finishedflag=finishedflag, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) NBTESTFINISH finishedflag Test"
  write(failMsg, *) "Incorrect value of finishedflag"
  evalflag = .true. ! assume success
  if (localPet==0) then
    ! PET 0 should return with finishedflag .true. because when it comes
    ! into ArrayRedist() with NBTESTFINISH all other PETs have already been here.
    ! There is always a slight chance that finishedflag comes back with .false.
    ! even at this point (execution effects). But under normal circumstances
    ! one expects the exchange to be finished here.
    if (.not. finishedflag) evalflag = .false.
  else if (localPet==5) then
    ! PET 5 depends on data from PET 0, and should return with 
    ! finishedflag .false. because PET 0 is still blocked by the barrier,
    ! not able to enter ArrayRedist().
    if (finishedflag) evalflag = .false.
  endif
  ! The status of finishedflag on PETs 1, 2, 3, 4 is non-determanistic at this
  ! point. None of them depend on PET 0, so they could in principle be done
  ! with data exchange between PETs 1, 2, 3, 4, 5, however, the exact timing is
  ! execution dependent and can vary. This test only checks the determanistic
  ! results.
  call ESMF_Test(evalflag, name, failMsg, result, ESMF_SRCLINE)
  
  write(msgString,*) "ESMF_ROUTESYNC_NBTESTFINISH: finishedflag=", finishedflag
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
  ! The folling barrier call releases PET 0 which was waiting on the barrier
  ! call before the first call to ArrayRedist() above. Releasing PET 0 now will
  ! allow the following call with ESMF_ROUTESYNC_NBWAITFINISH to finish up, where
  ! the finishedflag on all PETs will be .true. on return.
  if (localPet/=0) call ESMF_VMBarrier(vm)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) NBWAITFINISH Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBWAITFINISH, &
    finishedflag=finishedflag, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) NBWAITFINISH finishedflag Test"
  write(failMsg, *) "Incorrect value of finishedflag"
  ! Now all PETs should return with finishedflag .true.
  call ESMF_Test(finishedflag, name, failMsg, result, ESMF_SRCLINE)
  
  write(msgString,*) "ESMF_ROUTESYNC_NBWAITFINISH: finishedflag=", finishedflag
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
  ! The expected result of the redistribution of srcArray2 into dstArray2 is:
  !
  ! PET   localDE   DE    dstArray2 contents
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray2 (RRA) non-blocking Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.61).and. &
      (farrayPtr(2).eq.60).and.(farrayPtr(3).eq.59).and. &
      (farrayPtr(4).eq.58).and.(farrayPtr(5).eq.57).and. &
      (farrayPtr(6).eq.56).and.(farrayPtr(7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.54).and. &
      (farrayPtr(2).eq.53).and.(farrayPtr(3).eq.52).and. &
      (farrayPtr(4).eq.51).and.(farrayPtr(5).eq.50).and. &
      (farrayPtr(6).eq.49).and.(farrayPtr(7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.47).and. &
      (farrayPtr(2).eq.46).and.(farrayPtr(3).eq.45).and. &
      (farrayPtr(4).eq.44).and.(farrayPtr(5).eq.43).and. &
      (farrayPtr(6).eq.42).and.(farrayPtr(7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.40).and. &
      (farrayPtr(2).eq.37).and.(farrayPtr(3).eq.36).and. &
      (farrayPtr(4).eq.35).and.(farrayPtr(5).eq.34).and. &
      (farrayPtr(6).eq.33).and.(farrayPtr(7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.31).and. &
      (farrayPtr(2).eq.30).and.(farrayPtr(3).eq.25).and. &
      (farrayPtr(4).eq.24).and.(farrayPtr(5).eq.23).and. &
      (farrayPtr(6).eq.22).and.(farrayPtr(7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.20).and. &
      (farrayPtr(2).eq.13).and.(farrayPtr(3).eq.12).and. &
      (farrayPtr(4).eq.11).and.(farrayPtr(5).eq.10).and. &
      (farrayPtr(6).eq.1).and.(farrayPtr(7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif


#if 0

! --- comment out b/c CANCEL implementation is incomplete/broken

!------------------------------------------------------------------------
  !EX_disable_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray2 -> dstArray2 (RRA) with cancel Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 

  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBSTART, &
    finishedflag=finishedflag, cancelledflag=cancelledflag, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
write(100+localPet,*) "NBSTART: finishedflag = ", finishedflag, &
"cancelledflag = ", cancelledflag

  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_CANCEL, &
    finishedflag=finishedflag, cancelledflag=cancelledflag, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

write(100+localPet,*) "CANCEL: finishedflag = ", finishedflag, &
"cancelledflag = ", cancelledflag

  call ESMF_ArrayRedist(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, routesyncflag=ESMF_ROUTESYNC_NBWAITFINISH, &
    finishedflag=finishedflag, cancelledflag=cancelledflag, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
write(100+localPet,*) "NBWAITFINISH: finishedflag = ", finishedflag, &
"cancelledflag = ", cancelledflag

#endif


!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore with tensor dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray3, dstArray=dstArray3, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray3 -> dstArray3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray3, dstArray=dstArray3, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray3 into dstArray3 is:
  !
  ! PET   localDE   DE    dstArray3 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray3 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray3 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray3, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  write(msgString,*) "dstArray3: ", farrayPtr2D
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray3 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(2,1).eq.60).and.(farrayPtr2D(3,1).eq.59).and. &
      (farrayPtr2D(4,1).eq.58).and.(farrayPtr2D(5,1).eq.57).and. &
      (farrayPtr2D(6,1).eq.56).and.(farrayPtr2D(7,1).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(2,1).eq.53).and.(farrayPtr2D(3,1).eq.52).and. &
      (farrayPtr2D(4,1).eq.51).and.(farrayPtr2D(5,1).eq.50).and. &
      (farrayPtr2D(6,1).eq.49).and.(farrayPtr2D(7,1).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(2,1).eq.46).and.(farrayPtr2D(3,1).eq.45).and. &
      (farrayPtr2D(4,1).eq.44).and.(farrayPtr2D(5,1).eq.43).and. &
      (farrayPtr2D(6,1).eq.42).and.(farrayPtr2D(7,1).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(2,1).eq.37).and.(farrayPtr2D(3,1).eq.36).and. &
      (farrayPtr2D(4,1).eq.35).and.(farrayPtr2D(5,1).eq.34).and. &
      (farrayPtr2D(6,1).eq.33).and.(farrayPtr2D(7,1).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(2,1).eq.30).and.(farrayPtr2D(3,1).eq.25).and. &
      (farrayPtr2D(4,1).eq.24).and.(farrayPtr2D(5,1).eq.23).and. &
      (farrayPtr2D(6,1).eq.22).and.(farrayPtr2D(7,1).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(2,1).eq.13).and.(farrayPtr2D(3,1).eq.12).and. &
      (farrayPtr2D(4,1).eq.11).and.(farrayPtr2D(5,1).eq.10).and. &
      (farrayPtr2D(6,1).eq.1).and.(farrayPtr2D(7,1).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray3 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(3,2).eq.159).and. &
      (farrayPtr2D(4,2).eq.158).and.(farrayPtr2D(5,2).eq.157).and. &
      (farrayPtr2D(6,2).eq.156).and.(farrayPtr2D(7,2).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(3,2).eq.152).and. &
      (farrayPtr2D(4,2).eq.151).and.(farrayPtr2D(5,2).eq.150).and. &
      (farrayPtr2D(6,2).eq.149).and.(farrayPtr2D(7,2).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(3,2).eq.145).and. &
      (farrayPtr2D(4,2).eq.144).and.(farrayPtr2D(5,2).eq.143).and. &
      (farrayPtr2D(6,2).eq.142).and.(farrayPtr2D(7,2).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(3,2).eq.136).and. &
      (farrayPtr2D(4,2).eq.135).and.(farrayPtr2D(5,2).eq.134).and. &
      (farrayPtr2D(6,2).eq.133).and.(farrayPtr2D(7,2).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(3,2).eq.125).and. &
      (farrayPtr2D(4,2).eq.124).and.(farrayPtr2D(5,2).eq.123).and. &
      (farrayPtr2D(6,2).eq.122).and.(farrayPtr2D(7,2).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(3,2).eq.112).and. &
      (farrayPtr2D(4,2).eq.111).and.(farrayPtr2D(5,2).eq.110).and. &
      (farrayPtr2D(6,2).eq.101).and.(farrayPtr2D(7,2).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle3 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore with mismatching tensor count input Test"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray3, dstArray=dstArray2, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle3 Release (delete routehandle) Test"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgridWrong Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstDistgridWrong = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/43/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArrayWrong Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArrayWrong = ESMF_ArrayCreate(arrayspec=arrayspec, &
    distgrid=dstDistgridWrong, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore with mismatching exclusive elements input Test"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray2, dstArray=dstArrayWrong, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore with tensor dims reorder Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray3, dstArray=dstArray6, &
    routehandle=routehandle36, srcToDstTransposeMap=(/2,1/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray6, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  farrayPtr2D = -999 ! initialize to something obvious
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray3 -> dstArray6 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray3, dstArray=dstArray6, &
    routehandle=routehandle36, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray3 into dstArray6 is:
  !
  ! PET   localDE   DE    dstArray6 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray6 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  
!------------------------------------------------------------------------

  write(msgString,*) "dstArray6: ", farrayPtr2D
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray6 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(1,2).eq.60).and.(farrayPtr2D(1,3).eq.59).and. &
      (farrayPtr2D(1,4).eq.58).and.(farrayPtr2D(1,5).eq.57).and. &
      (farrayPtr2D(1,6).eq.56).and.(farrayPtr2D(1,7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(1,2).eq.53).and.(farrayPtr2D(1,3).eq.52).and. &
      (farrayPtr2D(1,4).eq.51).and.(farrayPtr2D(1,5).eq.50).and. &
      (farrayPtr2D(1,6).eq.49).and.(farrayPtr2D(1,7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(1,2).eq.46).and.(farrayPtr2D(1,3).eq.45).and. &
      (farrayPtr2D(1,4).eq.44).and.(farrayPtr2D(1,5).eq.43).and. &
      (farrayPtr2D(1,6).eq.42).and.(farrayPtr2D(1,7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(1,2).eq.37).and.(farrayPtr2D(1,3).eq.36).and. &
      (farrayPtr2D(1,4).eq.35).and.(farrayPtr2D(1,5).eq.34).and. &
      (farrayPtr2D(1,6).eq.33).and.(farrayPtr2D(1,7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(1,2).eq.30).and.(farrayPtr2D(1,3).eq.25).and. &
      (farrayPtr2D(1,4).eq.24).and.(farrayPtr2D(1,5).eq.23).and. &
      (farrayPtr2D(1,6).eq.22).and.(farrayPtr2D(1,7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(1,2).eq.13).and.(farrayPtr2D(1,3).eq.12).and. &
      (farrayPtr2D(1,4).eq.11).and.(farrayPtr2D(1,5).eq.10).and. &
      (farrayPtr2D(1,6).eq.1).and.(farrayPtr2D(1,7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray6 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(2,3).eq.159).and. &
      (farrayPtr2D(2,4).eq.158).and.(farrayPtr2D(2,5).eq.157).and. &
      (farrayPtr2D(2,6).eq.156).and.(farrayPtr2D(2,7).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(2,3).eq.152).and. &
      (farrayPtr2D(2,4).eq.151).and.(farrayPtr2D(2,5).eq.150).and. &
      (farrayPtr2D(2,6).eq.149).and.(farrayPtr2D(2,7).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(2,3).eq.145).and. &
      (farrayPtr2D(2,4).eq.144).and.(farrayPtr2D(2,5).eq.143).and. &
      (farrayPtr2D(2,6).eq.142).and.(farrayPtr2D(2,7).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(2,3).eq.136).and. &
      (farrayPtr2D(2,4).eq.135).and.(farrayPtr2D(2,5).eq.134).and. &
      (farrayPtr2D(2,6).eq.133).and.(farrayPtr2D(2,7).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(2,3).eq.125).and. &
      (farrayPtr2D(2,4).eq.124).and.(farrayPtr2D(2,5).eq.123).and. &
      (farrayPtr2D(2,6).eq.122).and.(farrayPtr2D(2,7).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(2,3).eq.112).and. &
      (farrayPtr2D(2,4).eq.111).and.(farrayPtr2D(2,5).eq.110).and. &
      (farrayPtr2D(2,6).eq.101).and.(farrayPtr2D(2,7).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle36 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle36, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore with tensor dims vector same decomp Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=dstArray6, dstArray=dstArray6p, &
    routehandle=routehandle66p, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6p Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray6p, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  farrayPtr2D = -999 ! initialize to something obvious

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: dstArray6 -> dstArray6p Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=dstArray6, dstArray=dstArray6p, &
    routehandle=routehandle66p, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of dstArray6 into dstArray6p is:
  !
  ! PET   localDE   DE    dstArray6p contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray6p contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray6p: ", farrayPtr2D
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray6p (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(1,2).eq.60).and.(farrayPtr2D(1,3).eq.59).and. &
      (farrayPtr2D(1,4).eq.58).and.(farrayPtr2D(1,5).eq.57).and. &
      (farrayPtr2D(1,6).eq.56).and.(farrayPtr2D(1,7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(1,2).eq.53).and.(farrayPtr2D(1,3).eq.52).and. &
      (farrayPtr2D(1,4).eq.51).and.(farrayPtr2D(1,5).eq.50).and. &
      (farrayPtr2D(1,6).eq.49).and.(farrayPtr2D(1,7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(1,2).eq.46).and.(farrayPtr2D(1,3).eq.45).and. &
      (farrayPtr2D(1,4).eq.44).and.(farrayPtr2D(1,5).eq.43).and. &
      (farrayPtr2D(1,6).eq.42).and.(farrayPtr2D(1,7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(1,2).eq.37).and.(farrayPtr2D(1,3).eq.36).and. &
      (farrayPtr2D(1,4).eq.35).and.(farrayPtr2D(1,5).eq.34).and. &
      (farrayPtr2D(1,6).eq.33).and.(farrayPtr2D(1,7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(1,2).eq.30).and.(farrayPtr2D(1,3).eq.25).and. &
      (farrayPtr2D(1,4).eq.24).and.(farrayPtr2D(1,5).eq.23).and. &
      (farrayPtr2D(1,6).eq.22).and.(farrayPtr2D(1,7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(1,2).eq.13).and.(farrayPtr2D(1,3).eq.12).and. &
      (farrayPtr2D(1,4).eq.11).and.(farrayPtr2D(1,5).eq.10).and. &
      (farrayPtr2D(1,6).eq.1).and.(farrayPtr2D(1,7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray6p (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(2,3).eq.159).and. &
      (farrayPtr2D(2,4).eq.158).and.(farrayPtr2D(2,5).eq.157).and. &
      (farrayPtr2D(2,6).eq.156).and.(farrayPtr2D(2,7).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(2,3).eq.152).and. &
      (farrayPtr2D(2,4).eq.151).and.(farrayPtr2D(2,5).eq.150).and. &
      (farrayPtr2D(2,6).eq.149).and.(farrayPtr2D(2,7).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(2,3).eq.145).and. &
      (farrayPtr2D(2,4).eq.144).and.(farrayPtr2D(2,5).eq.143).and. &
      (farrayPtr2D(2,6).eq.142).and.(farrayPtr2D(2,7).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(2,3).eq.136).and. &
      (farrayPtr2D(2,4).eq.135).and.(farrayPtr2D(2,5).eq.134).and. &
      (farrayPtr2D(2,6).eq.133).and.(farrayPtr2D(2,7).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(2,3).eq.125).and. &
      (farrayPtr2D(2,4).eq.124).and.(farrayPtr2D(2,5).eq.123).and. &
      (farrayPtr2D(2,6).eq.122).and.(farrayPtr2D(2,7).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(2,3).eq.112).and. &
      (farrayPtr2D(2,4).eq.111).and.(farrayPtr2D(2,5).eq.110).and. &
      (farrayPtr2D(2,6).eq.101).and.(farrayPtr2D(2,7).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle66p Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle66p, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore with tensor dims vector Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistStore(srcArray=srcArray6, dstArray=dstArray6, &
    routehandle=routehandle66, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray6, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  farrayPtr2D = -999 ! initialize to something obvious

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray6 -> dstArray6 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray6, dstArray=dstArray6, &
    routehandle=routehandle66, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray6 into dstArray6 is:
  !
  ! PET   localDE   DE    dstArray6 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray6 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray6: ", farrayPtr2D
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray6 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(1,2).eq.60).and.(farrayPtr2D(1,3).eq.59).and. &
      (farrayPtr2D(1,4).eq.58).and.(farrayPtr2D(1,5).eq.57).and. &
      (farrayPtr2D(1,6).eq.56).and.(farrayPtr2D(1,7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(1,2).eq.53).and.(farrayPtr2D(1,3).eq.52).and. &
      (farrayPtr2D(1,4).eq.51).and.(farrayPtr2D(1,5).eq.50).and. &
      (farrayPtr2D(1,6).eq.49).and.(farrayPtr2D(1,7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(1,2).eq.46).and.(farrayPtr2D(1,3).eq.45).and. &
      (farrayPtr2D(1,4).eq.44).and.(farrayPtr2D(1,5).eq.43).and. &
      (farrayPtr2D(1,6).eq.42).and.(farrayPtr2D(1,7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(1,2).eq.37).and.(farrayPtr2D(1,3).eq.36).and. &
      (farrayPtr2D(1,4).eq.35).and.(farrayPtr2D(1,5).eq.34).and. &
      (farrayPtr2D(1,6).eq.33).and.(farrayPtr2D(1,7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(1,2).eq.30).and.(farrayPtr2D(1,3).eq.25).and. &
      (farrayPtr2D(1,4).eq.24).and.(farrayPtr2D(1,5).eq.23).and. &
      (farrayPtr2D(1,6).eq.22).and.(farrayPtr2D(1,7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(1,2).eq.13).and.(farrayPtr2D(1,3).eq.12).and. &
      (farrayPtr2D(1,4).eq.11).and.(farrayPtr2D(1,5).eq.10).and. &
      (farrayPtr2D(1,6).eq.1).and.(farrayPtr2D(1,7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray6 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(2,3).eq.159).and. &
      (farrayPtr2D(2,4).eq.158).and.(farrayPtr2D(2,5).eq.157).and. &
      (farrayPtr2D(2,6).eq.156).and.(farrayPtr2D(2,7).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(2,3).eq.152).and. &
      (farrayPtr2D(2,4).eq.151).and.(farrayPtr2D(2,5).eq.150).and. &
      (farrayPtr2D(2,6).eq.149).and.(farrayPtr2D(2,7).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(2,3).eq.145).and. &
      (farrayPtr2D(2,4).eq.144).and.(farrayPtr2D(2,5).eq.143).and. &
      (farrayPtr2D(2,6).eq.142).and.(farrayPtr2D(2,7).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(2,3).eq.136).and. &
      (farrayPtr2D(2,4).eq.135).and.(farrayPtr2D(2,5).eq.134).and. &
      (farrayPtr2D(2,6).eq.133).and.(farrayPtr2D(2,7).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(2,3).eq.125).and. &
      (farrayPtr2D(2,4).eq.124).and.(farrayPtr2D(2,5).eq.123).and. &
      (farrayPtr2D(2,6).eq.122).and.(farrayPtr2D(2,7).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(2,3).eq.112).and. &
      (farrayPtr2D(2,4).eq.111).and.(farrayPtr2D(2,5).eq.110).and. &
      (farrayPtr2D(2,6).eq.101).and.(farrayPtr2D(2,7).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray7 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray7, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  farrayPtr2D = -999 ! initialize to something obvious
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray7 -> dstArray7 using routehandle66 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray7, dstArray=dstArray7, &
    routehandle=routehandle66, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray7 into dstArray7 is:
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=3
  ! 0     0         0     261, 260, 259, 258, 257, 256, 255
  ! 1     0         1     254, 253, 252, 251, 250, 249, 248
  ! 2     0         2     247, 246, 245, 244, 243, 242, 241
  ! 3     0         3     240, 237, 236, 235, 234, 233, 232
  ! 4     0         4     231, 230, 225, 224, 223, 222, 221
  ! 5     0         5     220, 213, 212, 211, 210, 201, 200
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray7: ", farrayPtr2D
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(1,2).eq.60).and.(farrayPtr2D(1,3).eq.59).and. &
      (farrayPtr2D(1,4).eq.58).and.(farrayPtr2D(1,5).eq.57).and. &
      (farrayPtr2D(1,6).eq.56).and.(farrayPtr2D(1,7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(1,2).eq.53).and.(farrayPtr2D(1,3).eq.52).and. &
      (farrayPtr2D(1,4).eq.51).and.(farrayPtr2D(1,5).eq.50).and. &
      (farrayPtr2D(1,6).eq.49).and.(farrayPtr2D(1,7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(1,2).eq.46).and.(farrayPtr2D(1,3).eq.45).and. &
      (farrayPtr2D(1,4).eq.44).and.(farrayPtr2D(1,5).eq.43).and. &
      (farrayPtr2D(1,6).eq.42).and.(farrayPtr2D(1,7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(1,2).eq.37).and.(farrayPtr2D(1,3).eq.36).and. &
      (farrayPtr2D(1,4).eq.35).and.(farrayPtr2D(1,5).eq.34).and. &
      (farrayPtr2D(1,6).eq.33).and.(farrayPtr2D(1,7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(1,2).eq.30).and.(farrayPtr2D(1,3).eq.25).and. &
      (farrayPtr2D(1,4).eq.24).and.(farrayPtr2D(1,5).eq.23).and. &
      (farrayPtr2D(1,6).eq.22).and.(farrayPtr2D(1,7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(1,2).eq.13).and.(farrayPtr2D(1,3).eq.12).and. &
      (farrayPtr2D(1,4).eq.11).and.(farrayPtr2D(1,5).eq.10).and. &
      (farrayPtr2D(1,6).eq.1).and.(farrayPtr2D(1,7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(2,3).eq.159).and. &
      (farrayPtr2D(2,4).eq.158).and.(farrayPtr2D(2,5).eq.157).and. &
      (farrayPtr2D(2,6).eq.156).and.(farrayPtr2D(2,7).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(2,3).eq.152).and. &
      (farrayPtr2D(2,4).eq.151).and.(farrayPtr2D(2,5).eq.150).and. &
      (farrayPtr2D(2,6).eq.149).and.(farrayPtr2D(2,7).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(2,3).eq.145).and. &
      (farrayPtr2D(2,4).eq.144).and.(farrayPtr2D(2,5).eq.143).and. &
      (farrayPtr2D(2,6).eq.142).and.(farrayPtr2D(2,7).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(2,3).eq.136).and. &
      (farrayPtr2D(2,4).eq.135).and.(farrayPtr2D(2,5).eq.134).and. &
      (farrayPtr2D(2,6).eq.133).and.(farrayPtr2D(2,7).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(2,3).eq.125).and. &
      (farrayPtr2D(2,4).eq.124).and.(farrayPtr2D(2,5).eq.123).and. &
      (farrayPtr2D(2,6).eq.122).and.(farrayPtr2D(2,7).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(2,3).eq.112).and. &
      (farrayPtr2D(2,4).eq.111).and.(farrayPtr2D(2,5).eq.110).and. &
      (farrayPtr2D(2,6).eq.101).and.(farrayPtr2D(2,7).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=3) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.261).and. &
      (farrayPtr2D(3,2).eq.260).and.(farrayPtr2D(3,3).eq.259).and. &
      (farrayPtr2D(3,4).eq.258).and.(farrayPtr2D(3,5).eq.257).and. &
      (farrayPtr2D(3,6).eq.256).and.(farrayPtr2D(3,7).eq.255)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.254).and. &
      (farrayPtr2D(3,2).eq.253).and.(farrayPtr2D(3,3).eq.252).and. &
      (farrayPtr2D(3,4).eq.251).and.(farrayPtr2D(3,5).eq.250).and. &
      (farrayPtr2D(3,6).eq.249).and.(farrayPtr2D(3,7).eq.248)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.247).and. &
      (farrayPtr2D(3,2).eq.246).and.(farrayPtr2D(3,3).eq.245).and. &
      (farrayPtr2D(3,4).eq.244).and.(farrayPtr2D(3,5).eq.243).and. &
      (farrayPtr2D(3,6).eq.242).and.(farrayPtr2D(3,7).eq.241)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.240).and. &
      (farrayPtr2D(3,2).eq.237).and.(farrayPtr2D(3,3).eq.236).and. &
      (farrayPtr2D(3,4).eq.235).and.(farrayPtr2D(3,5).eq.234).and. &
      (farrayPtr2D(3,6).eq.233).and.(farrayPtr2D(3,7).eq.232)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.231).and. &
      (farrayPtr2D(3,2).eq.230).and.(farrayPtr2D(3,3).eq.225).and. &
      (farrayPtr2D(3,4).eq.224).and.(farrayPtr2D(3,5).eq.223).and. &
      (farrayPtr2D(3,6).eq.222).and.(farrayPtr2D(3,7).eq.221)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.220).and. &
      (farrayPtr2D(3,2).eq.213).and.(farrayPtr2D(3,3).eq.212).and. &
      (farrayPtr2D(3,4).eq.211).and.(farrayPtr2D(3,5).eq.210).and. &
      (farrayPtr2D(3,6).eq.201).and.(farrayPtr2D(3,7).eq.200)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle66 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle66, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------

  farrayPtr2D = -999 ! initialize to something obvious
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray7 -> dstArray7 re-using routehandle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray7, dstArray=dstArray7, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray7 into dstArray7 is:
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=3
  ! 0     0         0     261, 260, 259, 258, 257, 256, 255
  ! 1     0         1     254, 253, 252, 251, 250, 249, 248
  ! 2     0         2     247, 246, 245, 244, 243, 242, 241
  ! 3     0         3     240, 237, 236, 235, 234, 233, 232
  ! 4     0         4     231, 230, 225, 224, 223, 222, 221
  ! 5     0         5     220, 213, 212, 211, 210, 201, 200
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray7: ", farrayPtr2D
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(1,2).eq.60).and.(farrayPtr2D(1,3).eq.59).and. &
      (farrayPtr2D(1,4).eq.58).and.(farrayPtr2D(1,5).eq.57).and. &
      (farrayPtr2D(1,6).eq.56).and.(farrayPtr2D(1,7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(1,2).eq.53).and.(farrayPtr2D(1,3).eq.52).and. &
      (farrayPtr2D(1,4).eq.51).and.(farrayPtr2D(1,5).eq.50).and. &
      (farrayPtr2D(1,6).eq.49).and.(farrayPtr2D(1,7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(1,2).eq.46).and.(farrayPtr2D(1,3).eq.45).and. &
      (farrayPtr2D(1,4).eq.44).and.(farrayPtr2D(1,5).eq.43).and. &
      (farrayPtr2D(1,6).eq.42).and.(farrayPtr2D(1,7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(1,2).eq.37).and.(farrayPtr2D(1,3).eq.36).and. &
      (farrayPtr2D(1,4).eq.35).and.(farrayPtr2D(1,5).eq.34).and. &
      (farrayPtr2D(1,6).eq.33).and.(farrayPtr2D(1,7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(1,2).eq.30).and.(farrayPtr2D(1,3).eq.25).and. &
      (farrayPtr2D(1,4).eq.24).and.(farrayPtr2D(1,5).eq.23).and. &
      (farrayPtr2D(1,6).eq.22).and.(farrayPtr2D(1,7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(1,2).eq.13).and.(farrayPtr2D(1,3).eq.12).and. &
      (farrayPtr2D(1,4).eq.11).and.(farrayPtr2D(1,5).eq.10).and. &
      (farrayPtr2D(1,6).eq.1).and.(farrayPtr2D(1,7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(2,3).eq.159).and. &
      (farrayPtr2D(2,4).eq.158).and.(farrayPtr2D(2,5).eq.157).and. &
      (farrayPtr2D(2,6).eq.156).and.(farrayPtr2D(2,7).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(2,3).eq.152).and. &
      (farrayPtr2D(2,4).eq.151).and.(farrayPtr2D(2,5).eq.150).and. &
      (farrayPtr2D(2,6).eq.149).and.(farrayPtr2D(2,7).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(2,3).eq.145).and. &
      (farrayPtr2D(2,4).eq.144).and.(farrayPtr2D(2,5).eq.143).and. &
      (farrayPtr2D(2,6).eq.142).and.(farrayPtr2D(2,7).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(2,3).eq.136).and. &
      (farrayPtr2D(2,4).eq.135).and.(farrayPtr2D(2,5).eq.134).and. &
      (farrayPtr2D(2,6).eq.133).and.(farrayPtr2D(2,7).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(2,3).eq.125).and. &
      (farrayPtr2D(2,4).eq.124).and.(farrayPtr2D(2,5).eq.123).and. &
      (farrayPtr2D(2,6).eq.122).and.(farrayPtr2D(2,7).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(2,3).eq.112).and. &
      (farrayPtr2D(2,4).eq.111).and.(farrayPtr2D(2,5).eq.110).and. &
      (farrayPtr2D(2,6).eq.101).and.(farrayPtr2D(2,7).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=3) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.261).and. &
      (farrayPtr2D(3,2).eq.260).and.(farrayPtr2D(3,3).eq.259).and. &
      (farrayPtr2D(3,4).eq.258).and.(farrayPtr2D(3,5).eq.257).and. &
      (farrayPtr2D(3,6).eq.256).and.(farrayPtr2D(3,7).eq.255)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.254).and. &
      (farrayPtr2D(3,2).eq.253).and.(farrayPtr2D(3,3).eq.252).and. &
      (farrayPtr2D(3,4).eq.251).and.(farrayPtr2D(3,5).eq.250).and. &
      (farrayPtr2D(3,6).eq.249).and.(farrayPtr2D(3,7).eq.248)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.247).and. &
      (farrayPtr2D(3,2).eq.246).and.(farrayPtr2D(3,3).eq.245).and. &
      (farrayPtr2D(3,4).eq.244).and.(farrayPtr2D(3,5).eq.243).and. &
      (farrayPtr2D(3,6).eq.242).and.(farrayPtr2D(3,7).eq.241)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.240).and. &
      (farrayPtr2D(3,2).eq.237).and.(farrayPtr2D(3,3).eq.236).and. &
      (farrayPtr2D(3,4).eq.235).and.(farrayPtr2D(3,5).eq.234).and. &
      (farrayPtr2D(3,6).eq.233).and.(farrayPtr2D(3,7).eq.232)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.231).and. &
      (farrayPtr2D(3,2).eq.230).and.(farrayPtr2D(3,3).eq.225).and. &
      (farrayPtr2D(3,4).eq.224).and.(farrayPtr2D(3,5).eq.223).and. &
      (farrayPtr2D(3,6).eq.222).and.(farrayPtr2D(3,7).eq.221)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.220).and. &
      (farrayPtr2D(3,2).eq.213).and.(farrayPtr2D(3,3).eq.212).and. &
      (farrayPtr2D(3,4).eq.211).and.(farrayPtr2D(3,5).eq.210).and. &
      (farrayPtr2D(3,6).eq.201).and.(farrayPtr2D(3,7).eq.200)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------

  farrayPtr2D = -999 ! initialize to something obvious
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray8 -> dstArray7 re-using routehandle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray8, dstArray=dstArray7, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the redistribution of srcArray8 into dstArray7 is:
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  !
  ! PET   localDE   DE    dstArray7 contents, tensor dimension j=3
  ! 0     0         0     261, 260, 259, 258, 257, 256, 255
  ! 1     0         1     254, 253, 252, 251, 250, 249, 248
  ! 2     0         2     247, 246, 245, 244, 243, 242, 241
  ! 3     0         3     240, 237, 236, 235, 234, 233, 232
  ! 4     0         4     231, 230, 225, 224, 223, 222, 221
  ! 5     0         5     220, 213, 212, 211, 210, 201, 200
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray7(j=1): ", farrayPtr2D(1,:)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*) "dstArray7(j=2): ", farrayPtr2D(2,:)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*) "dstArray7(j=3): ", farrayPtr2D(3,:)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(1,2).eq.60).and.(farrayPtr2D(1,3).eq.59).and. &
      (farrayPtr2D(1,4).eq.58).and.(farrayPtr2D(1,5).eq.57).and. &
      (farrayPtr2D(1,6).eq.56).and.(farrayPtr2D(1,7).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(1,2).eq.53).and.(farrayPtr2D(1,3).eq.52).and. &
      (farrayPtr2D(1,4).eq.51).and.(farrayPtr2D(1,5).eq.50).and. &
      (farrayPtr2D(1,6).eq.49).and.(farrayPtr2D(1,7).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(1,2).eq.46).and.(farrayPtr2D(1,3).eq.45).and. &
      (farrayPtr2D(1,4).eq.44).and.(farrayPtr2D(1,5).eq.43).and. &
      (farrayPtr2D(1,6).eq.42).and.(farrayPtr2D(1,7).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(1,2).eq.37).and.(farrayPtr2D(1,3).eq.36).and. &
      (farrayPtr2D(1,4).eq.35).and.(farrayPtr2D(1,5).eq.34).and. &
      (farrayPtr2D(1,6).eq.33).and.(farrayPtr2D(1,7).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(1,2).eq.30).and.(farrayPtr2D(1,3).eq.25).and. &
      (farrayPtr2D(1,4).eq.24).and.(farrayPtr2D(1,5).eq.23).and. &
      (farrayPtr2D(1,6).eq.22).and.(farrayPtr2D(1,7).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(1,2).eq.13).and.(farrayPtr2D(1,3).eq.12).and. &
      (farrayPtr2D(1,4).eq.11).and.(farrayPtr2D(1,5).eq.10).and. &
      (farrayPtr2D(1,6).eq.1).and.(farrayPtr2D(1,7).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(2,3).eq.159).and. &
      (farrayPtr2D(2,4).eq.158).and.(farrayPtr2D(2,5).eq.157).and. &
      (farrayPtr2D(2,6).eq.156).and.(farrayPtr2D(2,7).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(2,3).eq.152).and. &
      (farrayPtr2D(2,4).eq.151).and.(farrayPtr2D(2,5).eq.150).and. &
      (farrayPtr2D(2,6).eq.149).and.(farrayPtr2D(2,7).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(2,3).eq.145).and. &
      (farrayPtr2D(2,4).eq.144).and.(farrayPtr2D(2,5).eq.143).and. &
      (farrayPtr2D(2,6).eq.142).and.(farrayPtr2D(2,7).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(2,3).eq.136).and. &
      (farrayPtr2D(2,4).eq.135).and.(farrayPtr2D(2,5).eq.134).and. &
      (farrayPtr2D(2,6).eq.133).and.(farrayPtr2D(2,7).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(2,3).eq.125).and. &
      (farrayPtr2D(2,4).eq.124).and.(farrayPtr2D(2,5).eq.123).and. &
      (farrayPtr2D(2,6).eq.122).and.(farrayPtr2D(2,7).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(2,1).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(2,3).eq.112).and. &
      (farrayPtr2D(2,4).eq.111).and.(farrayPtr2D(2,5).eq.110).and. &
      (farrayPtr2D(2,6).eq.101).and.(farrayPtr2D(2,7).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray7 (tensor dim j=3) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.261).and. &
      (farrayPtr2D(3,2).eq.260).and.(farrayPtr2D(3,3).eq.259).and. &
      (farrayPtr2D(3,4).eq.258).and.(farrayPtr2D(3,5).eq.257).and. &
      (farrayPtr2D(3,6).eq.256).and.(farrayPtr2D(3,7).eq.255)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.254).and. &
      (farrayPtr2D(3,2).eq.253).and.(farrayPtr2D(3,3).eq.252).and. &
      (farrayPtr2D(3,4).eq.251).and.(farrayPtr2D(3,5).eq.250).and. &
      (farrayPtr2D(3,6).eq.249).and.(farrayPtr2D(3,7).eq.248)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.247).and. &
      (farrayPtr2D(3,2).eq.246).and.(farrayPtr2D(3,3).eq.245).and. &
      (farrayPtr2D(3,4).eq.244).and.(farrayPtr2D(3,5).eq.243).and. &
      (farrayPtr2D(3,6).eq.242).and.(farrayPtr2D(3,7).eq.241)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.240).and. &
      (farrayPtr2D(3,2).eq.237).and.(farrayPtr2D(3,3).eq.236).and. &
      (farrayPtr2D(3,4).eq.235).and.(farrayPtr2D(3,5).eq.234).and. &
      (farrayPtr2D(3,6).eq.233).and.(farrayPtr2D(3,7).eq.232)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.231).and. &
      (farrayPtr2D(3,2).eq.230).and.(farrayPtr2D(3,3).eq.225).and. &
      (farrayPtr2D(3,4).eq.224).and.(farrayPtr2D(3,5).eq.223).and. &
      (farrayPtr2D(3,6).eq.222).and.(farrayPtr2D(3,7).eq.221)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(3,1).eq.220).and. &
      (farrayPtr2D(3,2).eq.213).and.(farrayPtr2D(3,3).eq.212).and. &
      (farrayPtr2D(3,4).eq.211).and.(farrayPtr2D(3,5).eq.210).and. &
      (farrayPtr2D(3,6).eq.201).and.(farrayPtr2D(3,7).eq.200)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray8 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray8, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  farrayPtr2D = -999 ! initialize to something obvious
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray7 -> dstArray8 re-using routehandle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray7, dstArray=dstArray8, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! The expected result of the redistribution of srcArray7 into dstArray8 is:
  !
  ! PET   localDE   DE    dstArray8 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray8 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  !
  ! PET   localDE   DE    dstArray8 contents, tensor dimension j=3
  ! 0     0         0     261, 260, 259, 258, 257, 256, 255
  ! 1     0         1     254, 253, 252, 251, 250, 249, 248
  ! 2     0         2     247, 246, 245, 244, 243, 242, 241
  ! 3     0         3     240, 237, 236, 235, 234, 233, 232
  ! 4     0         4     231, 230, 225, 224, 223, 222, 221
  ! 5     0         5     220, 213, 212, 211, 210, 201, 200
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray8(j=1): ", farrayPtr2D(:,1)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*) "dstArray8(j=2): ", farrayPtr2D(:,2)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*) "dstArray8(j=3): ", farrayPtr2D(:,3)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray8 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(2,1).eq.60).and.(farrayPtr2D(3,1).eq.59).and. &
      (farrayPtr2D(4,1).eq.58).and.(farrayPtr2D(5,1).eq.57).and. &
      (farrayPtr2D(6,1).eq.56).and.(farrayPtr2D(7,1).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(2,1).eq.53).and.(farrayPtr2D(3,1).eq.52).and. &
      (farrayPtr2D(4,1).eq.51).and.(farrayPtr2D(5,1).eq.50).and. &
      (farrayPtr2D(6,1).eq.49).and.(farrayPtr2D(7,1).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(2,1).eq.46).and.(farrayPtr2D(3,1).eq.45).and. &
      (farrayPtr2D(4,1).eq.44).and.(farrayPtr2D(5,1).eq.43).and. &
      (farrayPtr2D(6,1).eq.42).and.(farrayPtr2D(7,1).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(2,1).eq.37).and.(farrayPtr2D(3,1).eq.36).and. &
      (farrayPtr2D(4,1).eq.35).and.(farrayPtr2D(5,1).eq.34).and. &
      (farrayPtr2D(6,1).eq.33).and.(farrayPtr2D(7,1).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(2,1).eq.30).and.(farrayPtr2D(3,1).eq.25).and. &
      (farrayPtr2D(4,1).eq.24).and.(farrayPtr2D(5,1).eq.23).and. &
      (farrayPtr2D(6,1).eq.22).and.(farrayPtr2D(7,1).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(2,1).eq.13).and.(farrayPtr2D(3,1).eq.12).and. &
      (farrayPtr2D(4,1).eq.11).and.(farrayPtr2D(5,1).eq.10).and. &
      (farrayPtr2D(6,1).eq.1) .and.(farrayPtr2D(7,1).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray8 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(3,2).eq.159).and. &
      (farrayPtr2D(4,2).eq.158).and.(farrayPtr2D(5,2).eq.157).and. &
      (farrayPtr2D(6,2).eq.156).and.(farrayPtr2D(7,2).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(3,2).eq.152).and. &
      (farrayPtr2D(4,2).eq.151).and.(farrayPtr2D(5,2).eq.150).and. &
      (farrayPtr2D(6,2).eq.149).and.(farrayPtr2D(7,2).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(3,2).eq.145).and. &
      (farrayPtr2D(4,2).eq.144).and.(farrayPtr2D(5,2).eq.143).and. &
      (farrayPtr2D(6,2).eq.142).and.(farrayPtr2D(7,2).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(3,2).eq.136).and. &
      (farrayPtr2D(4,2).eq.135).and.(farrayPtr2D(5,2).eq.134).and. &
      (farrayPtr2D(6,2).eq.133).and.(farrayPtr2D(7,2).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(3,2).eq.125).and. &
      (farrayPtr2D(4,2).eq.124).and.(farrayPtr2D(5,2).eq.123).and. &
      (farrayPtr2D(6,2).eq.122).and.(farrayPtr2D(7,2).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(3,2).eq.112).and. &
      (farrayPtr2D(4,2).eq.111).and.(farrayPtr2D(5,2).eq.110).and. &
      (farrayPtr2D(6,2).eq.101).and.(farrayPtr2D(7,2).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray8 (tensor dim j=3) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.261).and. &
      (farrayPtr2D(2,3).eq.260).and.(farrayPtr2D(3,3).eq.259).and. &
      (farrayPtr2D(4,3).eq.258).and.(farrayPtr2D(5,3).eq.257).and. &
      (farrayPtr2D(6,3).eq.256).and.(farrayPtr2D(7,3).eq.255)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.254).and. &
      (farrayPtr2D(2,3).eq.253).and.(farrayPtr2D(3,3).eq.252).and. &
      (farrayPtr2D(4,3).eq.251).and.(farrayPtr2D(5,3).eq.250).and. &
      (farrayPtr2D(6,3).eq.249).and.(farrayPtr2D(7,3).eq.248)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.247).and. &
      (farrayPtr2D(2,3).eq.246).and.(farrayPtr2D(3,3).eq.245).and. &
      (farrayPtr2D(4,3).eq.244).and.(farrayPtr2D(5,3).eq.243).and. &
      (farrayPtr2D(6,3).eq.242).and.(farrayPtr2D(7,3).eq.241)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.240).and. &
      (farrayPtr2D(2,3).eq.237).and.(farrayPtr2D(3,3).eq.236).and. &
      (farrayPtr2D(4,3).eq.235).and.(farrayPtr2D(5,3).eq.234).and. &
      (farrayPtr2D(6,3).eq.233).and.(farrayPtr2D(7,3).eq.232)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.231).and. &
      (farrayPtr2D(2,3).eq.230).and.(farrayPtr2D(3,3).eq.225).and. &
      (farrayPtr2D(4,3).eq.224).and.(farrayPtr2D(5,3).eq.223).and. &
      (farrayPtr2D(6,3).eq.222).and.(farrayPtr2D(7,3).eq.221)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.220).and. &
      (farrayPtr2D(2,3).eq.213).and.(farrayPtr2D(3,3).eq.212).and. &
      (farrayPtr2D(4,3).eq.211).and.(farrayPtr2D(5,3).eq.210).and. &
      (farrayPtr2D(6,3).eq.201).and.(farrayPtr2D(7,3).eq.200)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  
  farrayPtr2D = -999 ! initialize to something obvious
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist: srcArray8 -> dstArray8 re-using routehandle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedist(srcArray=srcArray8, dstArray=dstArray8, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  ! The expected result of the redistribution of srcArray8 into dstArray8 is:
  !
  ! PET   localDE   DE    dstArray8 contents, tensor dimension j=1
  ! 0     0         0     61, 60, 59, 58, 57, 56, 55
  ! 1     0         1     54, 53, 52, 51, 50, 49, 48
  ! 2     0         2     47, 46, 45, 44, 43, 42, 41
  ! 3     0         3     40, 37, 36, 35, 34, 33, 32
  ! 4     0         4     31, 30, 25, 24, 23, 22, 21
  ! 5     0         5     20, 13, 12, 11, 10, 1, 0
  !
  ! PET   localDE   DE    dstArray8 contents, tensor dimension j=2
  ! 0     0         0     161, 160, 159, 158, 157, 156, 155
  ! 1     0         1     154, 153, 152, 151, 150, 149, 148
  ! 2     0         2     147, 146, 145, 144, 143, 142, 141
  ! 3     0         3     140, 137, 136, 135, 134, 133, 132
  ! 4     0         4     131, 130, 125, 124, 123, 122, 121
  ! 5     0         5     120, 113, 112, 111, 110, 101, 100
  !
  ! PET   localDE   DE    dstArray8 contents, tensor dimension j=3
  ! 0     0         0     261, 260, 259, 258, 257, 256, 255
  ! 1     0         1     254, 253, 252, 251, 250, 249, 248
  ! 2     0         2     247, 246, 245, 244, 243, 242, 241
  ! 3     0         3     240, 237, 236, 235, 234, 233, 232
  ! 4     0         4     231, 230, 225, 224, 223, 222, 221
  ! 5     0         5     220, 213, 212, 211, 210, 201, 200
!------------------------------------------------------------------------
  
  write(msgString,*) "dstArray8(j=1): ", farrayPtr2D(:,1)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*) "dstArray8(j=2): ", farrayPtr2D(:,2)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*) "dstArray8(j=3): ", farrayPtr2D(:,3)
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray8 (tensor dim j=1) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.61).and. &
      (farrayPtr2D(2,1).eq.60).and.(farrayPtr2D(3,1).eq.59).and. &
      (farrayPtr2D(4,1).eq.58).and.(farrayPtr2D(5,1).eq.57).and. &
      (farrayPtr2D(6,1).eq.56).and.(farrayPtr2D(7,1).eq.55)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.54).and. &
      (farrayPtr2D(2,1).eq.53).and.(farrayPtr2D(3,1).eq.52).and. &
      (farrayPtr2D(4,1).eq.51).and.(farrayPtr2D(5,1).eq.50).and. &
      (farrayPtr2D(6,1).eq.49).and.(farrayPtr2D(7,1).eq.48)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.47).and. &
      (farrayPtr2D(2,1).eq.46).and.(farrayPtr2D(3,1).eq.45).and. &
      (farrayPtr2D(4,1).eq.44).and.(farrayPtr2D(5,1).eq.43).and. &
      (farrayPtr2D(6,1).eq.42).and.(farrayPtr2D(7,1).eq.41)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.40).and. &
      (farrayPtr2D(2,1).eq.37).and.(farrayPtr2D(3,1).eq.36).and. &
      (farrayPtr2D(4,1).eq.35).and.(farrayPtr2D(5,1).eq.34).and. &
      (farrayPtr2D(6,1).eq.33).and.(farrayPtr2D(7,1).eq.32)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.31).and. &
      (farrayPtr2D(2,1).eq.30).and.(farrayPtr2D(3,1).eq.25).and. &
      (farrayPtr2D(4,1).eq.24).and.(farrayPtr2D(5,1).eq.23).and. &
      (farrayPtr2D(6,1).eq.22).and.(farrayPtr2D(7,1).eq.21)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.20).and. &
      (farrayPtr2D(2,1).eq.13).and.(farrayPtr2D(3,1).eq.12).and. &
      (farrayPtr2D(4,1).eq.11).and.(farrayPtr2D(5,1).eq.10).and. &
      (farrayPtr2D(6,1).eq.1) .and.(farrayPtr2D(7,1).eq.0)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray8 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.161).and. &
      (farrayPtr2D(2,2).eq.160).and.(farrayPtr2D(3,2).eq.159).and. &
      (farrayPtr2D(4,2).eq.158).and.(farrayPtr2D(5,2).eq.157).and. &
      (farrayPtr2D(6,2).eq.156).and.(farrayPtr2D(7,2).eq.155)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.154).and. &
      (farrayPtr2D(2,2).eq.153).and.(farrayPtr2D(3,2).eq.152).and. &
      (farrayPtr2D(4,2).eq.151).and.(farrayPtr2D(5,2).eq.150).and. &
      (farrayPtr2D(6,2).eq.149).and.(farrayPtr2D(7,2).eq.148)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.147).and. &
      (farrayPtr2D(2,2).eq.146).and.(farrayPtr2D(3,2).eq.145).and. &
      (farrayPtr2D(4,2).eq.144).and.(farrayPtr2D(5,2).eq.143).and. &
      (farrayPtr2D(6,2).eq.142).and.(farrayPtr2D(7,2).eq.141)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.140).and. &
      (farrayPtr2D(2,2).eq.137).and.(farrayPtr2D(3,2).eq.136).and. &
      (farrayPtr2D(4,2).eq.135).and.(farrayPtr2D(5,2).eq.134).and. &
      (farrayPtr2D(6,2).eq.133).and.(farrayPtr2D(7,2).eq.132)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.131).and. &
      (farrayPtr2D(2,2).eq.130).and.(farrayPtr2D(3,2).eq.125).and. &
      (farrayPtr2D(4,2).eq.124).and.(farrayPtr2D(5,2).eq.123).and. &
      (farrayPtr2D(6,2).eq.122).and.(farrayPtr2D(7,2).eq.121)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.120).and. &
      (farrayPtr2D(2,2).eq.113).and.(farrayPtr2D(3,2).eq.112).and. &
      (farrayPtr2D(4,2).eq.111).and.(farrayPtr2D(5,2).eq.110).and. &
      (farrayPtr2D(6,2).eq.101).and.(farrayPtr2D(7,2).eq.100)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray8 (tensor dim j=3) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.261).and. &
      (farrayPtr2D(2,3).eq.260).and.(farrayPtr2D(3,3).eq.259).and. &
      (farrayPtr2D(4,3).eq.258).and.(farrayPtr2D(5,3).eq.257).and. &
      (farrayPtr2D(6,3).eq.256).and.(farrayPtr2D(7,3).eq.255)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.254).and. &
      (farrayPtr2D(2,3).eq.253).and.(farrayPtr2D(3,3).eq.252).and. &
      (farrayPtr2D(4,3).eq.251).and.(farrayPtr2D(5,3).eq.250).and. &
      (farrayPtr2D(6,3).eq.249).and.(farrayPtr2D(7,3).eq.248)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.247).and. &
      (farrayPtr2D(2,3).eq.246).and.(farrayPtr2D(3,3).eq.245).and. &
      (farrayPtr2D(4,3).eq.244).and.(farrayPtr2D(5,3).eq.243).and. &
      (farrayPtr2D(6,3).eq.242).and.(farrayPtr2D(7,3).eq.241)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.240).and. &
      (farrayPtr2D(2,3).eq.237).and.(farrayPtr2D(3,3).eq.236).and. &
      (farrayPtr2D(4,3).eq.235).and.(farrayPtr2D(5,3).eq.234).and. &
      (farrayPtr2D(6,3).eq.233).and.(farrayPtr2D(7,3).eq.232)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.231).and. &
      (farrayPtr2D(2,3).eq.230).and.(farrayPtr2D(3,3).eq.225).and. &
      (farrayPtr2D(4,3).eq.224).and.(farrayPtr2D(5,3).eq.223).and. &
      (farrayPtr2D(6,3).eq.222).and.(farrayPtr2D(7,3).eq.221)), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,3).eq.220).and. &
      (farrayPtr2D(2,3).eq.213).and.(farrayPtr2D(3,3).eq.212).and. &
      (farrayPtr2D(4,3).eq.211).and.(farrayPtr2D(5,3).eq.210).and. &
      (farrayPtr2D(6,3).eq.201).and.(farrayPtr2D(7,3).eq.200)), &
      name, failMsg, result, ESMF_SRCLINE)
  endif

#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcArray Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgrid Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(srcDistGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray2 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray3 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray6 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray6, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray7 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray7, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcArray8 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "srcDistgrid2 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(srcDistGrid2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArrayWrong Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArrayWrong, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgridWrong Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(dstDistgridWrong, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstArray Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray2 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray3 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray6, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray6p Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray6p, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray7 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray7, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray8 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(dstArray8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstDistgrid Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(dstDistGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayRedistUTest
    
    
