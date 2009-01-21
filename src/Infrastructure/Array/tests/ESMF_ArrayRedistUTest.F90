! $Id: ESMF_ArrayRedistUTest.F90,v 1.3.2.8 2009/01/21 21:25:19 cdeluca Exp $
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
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArrayRedistUTest.F90,v 1.3.2.8 2009/01/21 21:25:19 cdeluca Exp $'
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
  integer(ESMF_KIND_I4), pointer :: farrayPtr(:)  ! matching F90 array pointer
#ifdef ESMF_TESTEXHAUSTIVE
  type(ESMF_DistGrid)   :: srcDistgrid2
  type(ESMF_Array)      :: srcArray2, srcArray3
  type(ESMF_Array)      :: dstArray2, dstArray3
  type(ESMF_ArraySpec)  :: arrayspec3
  type(ESMF_ArraySpec)  :: arrayspec4, arrayspec5
  type(ESMF_Array)      :: srcArray4, dstArray5
  real(ESMF_KIND_R8), pointer :: farrayPtr4(:)  ! matching F90 array pointer
  real(ESMF_KIND_R4), pointer :: farrayPtr5(:)  ! matching F90 array pointer
  type(ESMF_RouteHandle):: routehandle45
  type(ESMF_RouteHandle):: routehandle3
  integer(ESMF_KIND_I4), pointer :: farrayPtr2D(:,:)! matching F90 array pointer
#endif
  integer               :: rc, i, j, petCount, localPet
  integer, allocatable  :: srcIndices(:)

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
  !------------------------------------------------------------------------
  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  if (petCount /= 6) then
    print *, "This system test needs to run on exactly 6 PETs, petCount = ", &
      petCount
    goto 10
  endif

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
  write(name, *) "srcArray Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(srcArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
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
    call ESMF_Test(((farrayPtr(1).eq.31).and. &
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
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

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
  
  do j=0,1
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
  
  print *, "localPet: ",localPet," dstArray: ",farrayPtr
  
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
  
  print *, "localPet: ",localPet," dstArray2: ",farrayPtr
  
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
  ! 0     0         0     2x134 - 3x110 = -62, 0
  ! 1     0         1     0, 1x101 - 4x158 + 2x148 = -235
  ! 2     0         2     7x133 = 931, -2x142 = -284
  ! 3     0         3     1x111 + 1x122 + 1x145 - 4x157 = -250, 0
  ! 4     0         4     100x141 - 2x161 - 5x130 + 22x125 = 15878, -11x136 = -1496
  ! 5     0         5     5x150 = 750, -1x100 = -100
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray3 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray3, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "localPet: ",localPet," dstArray3: ",farrayPtr2D
  
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
  write(name, *) "ArrayRedistStore with mismatching input Test"
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
  write(name, *) "srcDistgrid2 Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(srcDistGrid2, rc=rc)
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
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayRedistUTest
    
    
