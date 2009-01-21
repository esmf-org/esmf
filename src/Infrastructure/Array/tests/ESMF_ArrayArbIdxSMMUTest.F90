! $Id: ESMF_ArrayArbIdxSMMUTest.F90,v 1.7.2.8 2009/01/21 21:25:19 cdeluca Exp $
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
program ESMF_ArrayArbIdxSMMUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayArbIdxUTest -  Tests a combination of DistGrid and Array.
!
! !DESCRIPTION:
!
! Test the use of 1D arrays with 'user supplied' arbitrary indexing for  the 
! array entries.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArrayArbIdxSMMUTest.F90,v 1.7.2.8 2009/01/21 21:25:19 cdeluca Exp $'
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
  integer(ESMF_KIND_I4) :: factorList(3)
  integer               :: factorIndexList(2,3)
#ifdef ESMF_TESTEXHAUSTIVE
  type(ESMF_DistGrid)   :: srcDistgrid2
  type(ESMF_Array)      :: srcArray2, srcArray3
  type(ESMF_Array)      :: dstArray2, dstArray3
  type(ESMF_ArraySpec)  :: arrayspec3
  type(ESMF_RouteHandle):: routehandle3
  integer(ESMF_KIND_I4), pointer :: farrayPtr2D(:,:)! matching F90 array pointer
  integer(ESMF_KIND_I4) :: factorList2(1)
  integer               :: factorIndexList2(4,1)
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
  ! Prepare for arbitrary sequence indices
  allocate(srcIndices((localPet+1)*2))   ! sizes: 2, 4, 6, 8, 10, 12
  
  do i=1,(localPet+1)*2
    srcIndices(i) = localPet*20 + (21 - i) ! set arbitrary but unique seq. indices
  enddo 

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
  ! 0     0         0     20, 19
  ! 1     0         1     40, 39, 38, 37
  ! 2     0         2     60, 59, 58, 57, 56, 55
  ! 3     0         3     80, 79, 78, 77, 76, 75, 74, 73
  ! 4     0         4     100, 99, 98, 97, 96, 95, 94, 93, 92, 91
  ! 5     0         5     120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109

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
  write(name, *) "Array Spec Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
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
  write(name, *) "Array Spec rank=2 Set Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec3, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
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
  write(name, *) "dstDistgrid Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstDistgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/12/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "dstArray Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The dstDistgrid evenly divides 12 elements across the 6 DEs (becaues default
  ! is 1 DE per PET and there are 6 PETs running this example).
  ! The default sequenceIndex of dstDistGrid is determined by the default rule
  ! of simply enumerating the elements within the tile, starting at 1:
  !
  ! PET   localDE   DE    indices
  ! 0     0         0     1, 2
  ! 1     0         1     3, 4
  ! 2     0         2     5, 6
  ! 3     0         3     7, 8
  ! 4     0         4     9, 10
  ! 5     0         5     11, 12
  !
  ! The dstArray created on the dstDistgrid has the following shape and
  ! initialization:
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     random, random
  ! 1     0         1     random, random
  ! 2     0         2     random, random
  ! 3     0         3     random, random
  ! 4     0         4     random, random
  ! 5     0         5     random, random

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray2 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray2 = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=dstDistgrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray3 Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  dstArray3 = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=dstDistgrid, &
    undistLBound=(/1/), undistUBound=(/2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#endif

!------------------------------------------------------------------------
! Initialize the factorList(:) and factorIndexList(:,:) arrays
  if (localPet == 0) then
    factorList(1) = 2
    factorIndexList(1,1) = 76   ! src seq index
    factorIndexList(2,1) = 1    ! dst seq index
    factorList(2) = -3
    factorIndexList(1,2) = 40   ! src seq index
    factorIndexList(2,2) = 1    ! dst seq index
    factorList(3) = 1
    factorIndexList(1,3) = 19   ! src seq index
    factorIndexList(2,3) = 4    ! dst seq index
  else if (localPet == 1) then
    factorList(1) = -4
    factorIndexList(1,1) = 112  ! src seq index
    factorIndexList(2,1) = 4    ! dst seq index
    factorList(2) = 2
    factorIndexList(1,2) = 92   ! src seq index
    factorIndexList(2,2) = 4    ! dst seq index
    factorList(3) = 7
    factorIndexList(1,3) = 77   ! src seq index
    factorIndexList(2,3) = 5    ! dst seq index
  else if (localPet == 2) then
    factorList(1) = -2
    factorIndexList(1,1) = 98   ! src seq index
    factorIndexList(2,1) = 6    ! dst seq index
    factorList(2) = 1
    factorIndexList(1,2) = 39   ! src seq index
    factorIndexList(2,2) = 7    ! dst seq index
    factorList(3) = 1
    factorIndexList(1,3) = 58   ! src seq index
    factorIndexList(2,3) = 7    ! dst seq index
  else if (localPet == 3) then
    factorList(1) = 1 
    factorIndexList(1,1) = 95   ! src seq index
    factorIndexList(2,1) = 7    ! dst seq index
    factorList(2) = -4
    factorIndexList(1,2) = 113  ! src seq index
    factorIndexList(2,2) = 7    ! dst seq index
    factorList(3) = -1
    factorIndexList(1,3) = 20   ! src seq index
    factorIndexList(2,3) = 12   ! dst seq index
  else if (localPet == 4) then
    factorList(1) = 100
    factorIndexList(1,1) = 99   ! src seq index
    factorIndexList(2,1) = 9    ! dst seq index
    factorList(2) = -2
    factorIndexList(1,2) = 109  ! src seq index
    factorIndexList(2,2) = 9    ! dst seq index
    factorList(3) = -5
    factorIndexList(1,3) = 80   ! src seq index
    factorIndexList(2,3) = 9    ! dst seq index
  else if (localPet == 5) then
    factorList(1) = 22
    factorIndexList(1,1) = 55   ! src seq index
    factorIndexList(2,1) = 9    ! dst seq index
    factorList(2) = 5
    factorIndexList(1,2) = 120  ! src seq index
    factorIndexList(2,2) = 11   ! dst seq index
    factorList(3) = -11
    factorIndexList(1,3) = 74 ! src seq index
    factorIndexList(2,3) = 10   ! dst seq index
  endif
  
  ! Each of the 6 PETs defines 3 different factors of the sparse matrix
  ! multiplication for a total of 6 x 3 = 18 non-zero entries in the 
  ! 12 x 42 = 504 matrix.
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMMStore Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMStore(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, factorList=factorList, &
    factorIndexList=factorIndexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMM: srcArray -> dstArray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMM(srcArray=srcArray, dstArray=dstArray, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the sparse matrix multiplication in dstArray is:
  ! (note: by default ArraySMM() initializes _all_ destination elements
  ! to zero before adding in sparse matrix terms.)
  !
  ! PET   localDE   DE    dstArray contents
  ! 0     0         0     2x35 - 3x11 = 37, 0
  ! 1     0         1     0, 1x2 - 4x59 + 2x49 = -136
  ! 2     0         2     7x34 = 238, -2x43 = -86
  ! 3     0         3     1x12 + 1x23 + 1x46 - 4x58 = -151, 0
  ! 4     0         4     100x42 - 2x62 - 5x31 + 22x26 = 4493, -11x37 = -407
  ! 5     0         5     5x51 = 255, -1x1 = 1
  
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
    call ESMF_Test(((farrayPtr(1).eq.37).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.0).and.(farrayPtr(2).eq.-136)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.238).and.(farrayPtr(2).eq.-86)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.-151).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.4493).and.(farrayPtr(2).eq.-407)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.255).and.(farrayPtr(2).eq.-1)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMM: srcArray2 -> dstArray2 (RRA) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMM(srcArray=srcArray2, dstArray=dstArray2, &
    routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the sparse matrix multiplication in dstArray2 is:
  ! (note: by default ArraySMM() initializes _all_ destination elements
  ! to zero before adding in sparse matrix terms.)
  !
  ! PET   localDE   DE    dstArray2 contents
  ! 0     0         0     2x34 - 3x10 = 38, 0
  ! 1     0         1     0, 1x1 - 4x58 + 2x48 = -135
  ! 2     0         2     7x33 = 231, -2x42 = -84
  ! 3     0         3     1x11 + 1x22 + 1x45 - 4x57 = -150, 0
  ! 4     0         4     100x41 - 2x61 - 5x30 + 22x25 = 4378, -11x36 = -396
  ! 5     0         5     5x50 = 250, -1x0 = 0
  
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
    call ESMF_Test(((farrayPtr(1).eq.38).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.0).and.(farrayPtr(2).eq.-135)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.231).and.(farrayPtr(2).eq.-84)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.-150).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.4378).and.(farrayPtr(2).eq.-396)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.250).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMMStore with tensor dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMStore(srcArray=srcArray3, dstArray=dstArray3, &
    routehandle=routehandle3, factorList=factorList, &
    factorIndexList=factorIndexList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMM: srcArray3 -> dstArray3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMM(srcArray=srcArray3, dstArray=dstArray3, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the sparse matrix multiplication in dstArray3 is:
  ! (note: by default ArraySMM() initializes _all_ destination elements
  ! to zero before adding in sparse matrix terms.)
  !
  ! PET   localDE   DE    dstArray3 contents, tensor dimension j=1
  ! 0     0         0     2x34 - 3x10 = 38, 0
  ! 1     0         1     0, 1x1 - 4x58 + 2x48 = -135
  ! 2     0         2     7x33 = 231, -2x42 = -84
  ! 3     0         3     1x11 + 1x22 + 1x45 - 4x57 = -150, 0
  ! 4     0         4     100x41 - 2x61 - 5x30 + 22x25 = 4378, -11x36 = -396
  ! 5     0         5     5x50 = 250, -1x0 = 0
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
    call ESMF_Test(((farrayPtr2D(1,1).eq.38).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.0).and.(farrayPtr2D(2,1).eq.-135)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.231).and.(farrayPtr2D(2,1).eq.-84)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.-150).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.4378).and.(farrayPtr2D(2,1).eq.-396)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.250).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray3 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.-62).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.0).and.(farrayPtr2D(2,2).eq.-235)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.931).and.(farrayPtr2D(2,2).eq.-284)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.-250).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.15878).and.(farrayPtr2D(2,2).eq.-1496)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.750).and.(farrayPtr2D(2,2).eq.-100)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle3 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMRelease(routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMMStore with mismatching input Test"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_ArraySMMStore(srcArray=srcArray3, dstArray=dstArray2, &
    routehandle=routehandle3, factorList=factorList, &
    factorIndexList=factorIndexList, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle3 Release (delete routehandle) Test"
  write(failMsg, *) "Did return ESMF_SUCCESS" 
  call ESMF_ArraySMMRelease(routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the factorList2(:) and factorIndexList2(:,:) arrays
  if (localPet == 0) then
    factorList2(1) = 5
    factorIndexList2(:,1) = (/77,2,10,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 1) then
    factorList2(1) = -2
    factorIndexList2(:,1) = (/120,1,1,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 2) then
    factorList2(1) = 3
    factorIndexList2(:,1) = (/120,2,1,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 3) then
    factorList2(1) = 2
    factorIndexList2(:,1) = (/99,2,4,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 4) then
    factorList2(1) = 23
    factorIndexList2(:,1) = (/37,2,7,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 5) then
    factorList2(1) = -12
    factorIndexList2(:,1) = (/110,1,7,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  endif
  
  ! Each of the 6 PETs defines 1 factor of the sparse matrix
  ! multiplication for a total of 6 x 1 = 6 non-zero entries.
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMMStore with tensor mixing factorIndexList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMStore(srcArray=srcArray3, dstArray=dstArray2, &
    routehandle=routehandle3, factorList=factorList2, &
    factorIndexList=factorIndexList2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMM: srcArray3 -> dstArray2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMM(srcArray=srcArray3, dstArray=dstArray2, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  ! The expected result of the sparse matrix multiplication in dstArray2 is:
  ! (note: by default ArraySMM() initializes _all_ destination elements
  ! to zero before adding in sparse matrix terms.)
  !
  ! PET   localDE   DE    dstArray2 contents
  ! 0     0         0     -2 * 50 + 3 * 150 = 350, 0
  ! 1     0         1     0, 2 * 141 = 282
  ! 2     0         2     0, 0
  ! 3     0         3     23 * 113 - 12 * 60 = 1879, 0
  ! 4     0         4     0, 5 * 133 = 665
  ! 5     0         5     0, 0
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "dstArray2 Get Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray2, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "localPet: ",localPet," dstArray2: ",farrayPtr
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray2 (tensor mixing) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr(1).eq.350).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr(1).eq.0).and.(farrayPtr(2).eq.282)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr(1).eq.0).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr(1).eq.1879).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr(1).eq.0).and.(farrayPtr(2).eq.665)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr(1).eq.0).and.(farrayPtr(2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle3 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMRelease(routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the factorList2(:) and factorIndexList2(:,:) arrays
  if (localPet == 0) then
    factorList2(1) = 5
    factorIndexList2(:,1) = (/77,2,10,2/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 1) then
    factorList2(1) = -2
    factorIndexList2(:,1) = (/120,1,1,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 2) then
    factorList2(1) = 3
    factorIndexList2(:,1) = (/120,2,1,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 3) then
    factorList2(1) = 2
    factorIndexList2(:,1) = (/99,2,4,1/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 4) then
    factorList2(1) = 23
    factorIndexList2(:,1) = (/37,2,7,2/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  else if (localPet == 5) then
    factorList2(1) = -12
    factorIndexList2(:,1) = (/110,1,7,2/) ! srcSeq, srcTSeq, dstSeq, dstTSeq
  endif
  
  ! Each of the 6 PETs defines 1 factor of the sparse matrix
  ! multiplication for a total of 6 x 1 = 6 non-zero entries.
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMMStore with tensor mixing factorIndexList Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMStore(srcArray=srcArray3, dstArray=dstArray3, &
    routehandle=routehandle3, factorList=factorList2, &
    factorIndexList=factorIndexList2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArraySMM: srcArray3 -> dstArray3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMM(srcArray=srcArray3, dstArray=dstArray3, &
    routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
call ESMF_ArrayPrint(dstArray3)

  ! The expected result of the sparse matrix multiplication in dstArray3 is:
  ! (note: by default ArraySMM() initializes _all_ destination elements
  ! to zero before adding in sparse matrix terms.)
  !
  ! PET   localDE   DE    dstArray3 contents, tensor dimension j=1
  ! 0     0         0     -2 * 50 + 3 * 150 = 350, 0
  ! 1     0         1     0, 2 * 141 = 282
  ! 2     0         2     0, 0
  ! 3     0         3     0, 0
  ! 4     0         4     0, 0
  ! 5     0         5     0, 0
  !
  ! PET   localDE   DE    dstArray3 contents, tensor dimension j=2
  ! 0     0         0     0, 0
  ! 1     0         1     0, 0
  ! 2     0         2     0, 0
  ! 3     0         3     23 * 113 - 12 * 60 = 1879, 0
  ! 4     0         4     0, 5 * 133 = 665
  ! 5     0         5     0, 0
  
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
    call ESMF_Test(((farrayPtr2D(1,1).eq.350).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.0).and.(farrayPtr2D(2,1).eq.282)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.0).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.0).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.0).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,1).eq.0).and.(farrayPtr2D(2,1).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Verify results in dstArray3 (tensor dim j=2) Test"
  write(failMsg, *) "Wrong results" 
  if (localPet == 0) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.0).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 1) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.0).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 2) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.0).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 3) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.1879).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 4) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.0).and.(farrayPtr2D(2,2).eq.665)), &
		     name, failMsg, result, ESMF_SRCLINE)
  else if (localPet == 5) then
    call ESMF_Test(((farrayPtr2D(1,2).eq.0).and.(farrayPtr2D(2,2).eq.0)), &
		     name, failMsg, result, ESMF_SRCLINE)
  endif
  
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "routehandle3 Release Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySMMRelease(routehandle=routehandle3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

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
  call ESMF_ArraySMMRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayArbIdxSMMUTest
    
    
