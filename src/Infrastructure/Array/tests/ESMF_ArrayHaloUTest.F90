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
program ESMF_ArrayHaloUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayHaloUTest -  Tests ArrayHalo()
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
  type(ESMF_DELayout)   :: delayout
  type(ESMF_DistGrid)   :: distgrid
  type(ESMF_Array)      :: array
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_RouteHandle):: routehandle
  integer(ESMF_KIND_I4), pointer :: farrayPtr(:,:)
  integer(ESMF_KIND_I4), pointer :: farrayPtr3d(:,:,:)
  integer               :: rc, i, j, m, verifyValue
  integer               :: petCount, localPet, localDeCount, lde
  integer, allocatable  :: localDeToDeMap(:)
  logical               :: verifyFlag
  integer               :: eLB(2,1), eUB(2,1)
  integer               :: cLB(2,1), cUB(2,1)
  integer               :: tLB(2,1), tUB(2,1)
  integer               :: hLB(2,1), hUB(2,1)
  integer               :: uLB(1), uUB(1)
  integer, allocatable  :: eLBde(:,:), eUBde(:,:), tLBde(:,:), tUBde(:,:)
  type(ESMF_DistGridConnection), allocatable :: connectionList(:)

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
  
  if (petCount /= 4) then
    print *, "This system test needs to run on exactly 4 PETs, petCount = ", &
      petCount
    goto 10
  endif

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Test-1: 1D decomposition, non-overlapping halo regions

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/1,4/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/2,2/), computationalUWidth=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 1 x 4 = 4 DEs. This means that each DE holds a 10 x 5
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
! +-------------------+   +-------------------+   +-------------------+   +-------------------+
! | \       2       / |   | \       2       / |   | \       2       / |   | \       2       / |
! |  +-------------+  |   |  +-------------+  |   |  +-------------+  |   |  +-------------+  |
! |  |     DE 0    |  |   |  |     DE 1    |  |   |  |     DE 2    |  |   |  |     DE 3    |  |
! |  |             |  |   |  |             |  |   |  |             |  |   |  |             |  |
! |  |   10 x 5    |  |   |  |   10 x 5    |  |   |  |   10 x 5    |  |   |  |   10 x 5    |  |
! |  |             |  |   |  |             |  |   |  |             |  |   |  |             |  |
! |2 |             | 2|<->|2 |             | 2|<->|2 |             | 2|<->|2 |             | 2|
! |  |             |  |   |  |             |  |   |  |             |  |   |  |             |  |
! |  |             |  |   |  |             |  |   |  |             |  |   |  |             |  |
! |  |             |  |   |  |             |  |   |  |             |  |   |  |             |  |
! |  |             |  |   |  |             |  |   |  |             |  |   |  |             |  |
! |  +-------------+  |   |  +-------------+  |   |  +-------------+  |   |  +-------------+  |
! | /       2       \ |   | /       2       \ |   | /       2       \ |   | /       2       \ |
! +-------------------+   +-------------------+   +-------------------+   +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-1"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      if (farrayPtr(i,j) /= localPet) then
        verifyFlag = .false.
        print *, "Found wrong exclusive element"
        exit
      endif
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  verifyValue = localPet
  if (verifyFlag) then
    do j=tLB(2,1), eLB(2,1)-1
      do i=tLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=tLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  verifyValue = localPet
  if (verifyFlag) then
    do j=tLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  verifyValue = localPet
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  verifyValue = localPet
  if (verifyFlag) then
    do j=eUB(2,1)+1, tUB(2,1)
      do i=eUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  verifyValue = localPet
  if (verifyFlag) then
    do j=eUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  verifyValue = localPet
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-2: 2D decomposition, overlapping halo regions

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/2,2/), computationalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-2"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      if (farrayPtr(i,j) /= localPet) then
        verifyFlag = .false.
        print *, "Found wrong exclusive element"
        exit
      endif
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), eLB(2,1)-1
      do i=tLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eUB(2,1)+1, tUB(2,1)
      do i=eUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-2b: 2D decomposition, overlapping halo regions, multiple DEs/PET

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DELayout Create Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  delayout = ESMF_DELayoutCreate(petMap=(/0,3,2,0/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), delayout=delayout, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/2,2/), computationalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get localDeCount Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  allocate(eLBde(2,0:localDeCount-1))
  allocate(eUBde(2,0:localDeCount-1))
  allocate(tLBde(2,0:localDeCount-1))
  allocate(tUBde(2,0:localDeCount-1))
  
  allocate(localDeToDeMap(0:localDeCount-1))

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLBde, exclusiveUBound=eUBde, &
    totalLBound=tLBde, totalUBound=tUBde, localDeToDeMap=localDeToDeMap, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. However, a DELayout is
! explicitly created to map DEs 0 and 3 agains PET 0, DE 1 against PET 3 and
! DE 2 against PET 2. Thus PET 0 holds 2 DEs, PET 1 holds no DEs, and PET 2 and
! 3 each holds 1 DE.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

  do lde=0, localDeCount-1
    call ESMF_ArrayGet(array, localDe=lde, farrayPtr=farrayPtr, rc=rc)
    ! Initialize the entire Array piece to its DE number
    farrayPtr = localDeToDeMap(lde)
  enddo

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-2b"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  do lde=0, localDeCount-1
  
    call ESMF_ArrayGet(array, localDe=lde, farrayPtr=farrayPtr, rc=rc)

    ! verify elements within exclusive region
    do j=eLBde(2,lde), eUBde(2,lde)
      do i=eLBde(1,lde), eUBde(1,lde)
        if (farrayPtr(i,j) /= localDeToDeMap(lde)) then
          verifyFlag = .false.
          print *, "Found wrong exclusive element"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  
    ! verify all eight sections outside the exclusive region
    ! section 1 staring in NW corner and going counter clock wise
    ! verify section 1
    if (localDeToDeMap(lde) == 0) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = 0
    endif
    do j=tLBde(2,lde), eLBde(2,lde)-1
      do i=tLBde(1,lde), eLBde(1,lde)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 1 for DE", localDeToDeMap(lde), &
            " on PET", localPet, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 2
    if (localDeToDeMap(lde) == 0) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = 0
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = 1
    endif
    do j=tLBde(2,lde), eLBde(2,lde)-1
      do i=eLBde(1,lde), eUBde(1,lde)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 2 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 3
    if (localDeToDeMap(lde) == 0) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = 1
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = localDeToDeMap(lde)
    endif
    do j=tLBde(2,lde), eLBde(2,lde)-1
      do i=eUBde(1,lde)+1, tUBde(1,lde)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 3 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 4
    if (localDeToDeMap(lde) == 0) then
      verifyValue = 1
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = 3
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = localDeToDeMap(lde)
    endif
    do j=eLBde(2,lde), eUBde(2,lde)
      do i=eUBde(1,lde)+1, tUBde(1,lde)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 4 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 5
    if (localDeToDeMap(lde) == 0) then
      verifyValue = 3
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = localDeToDeMap(lde)
    endif
    do j=eUBde(2,lde)+1, tUBde(2,lde)
      do i=eUBde(1,lde)+1, tUBde(1,lde)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 5 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 6
    if (localDeToDeMap(lde) == 0) then
      verifyValue = 2
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = 3
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = localDeToDeMap(lde)
    endif
    do j=eUBde(2,lde)+1, tUBde(2,lde)
      do i=eLBde(1,lde), eUBde(1,lde)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 6 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 7
    if (localDeToDeMap(lde) == 0) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = 2
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = localDeToDeMap(lde)
    endif
    do j=eUBde(2,lde)+1, tUBde(2,lde)
      do i=tLBde(1,lde), eLBde(1,lde)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 7 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    ! verify section 8
    if (localDeToDeMap(lde) == 0) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 1) then
      verifyValue = 0
    else if (localDeToDeMap(lde) == 2) then
      verifyValue = localDeToDeMap(lde)
    else if (localDeToDeMap(lde) == 3) then
      verifyValue = 2
    endif
    do j=eLBde(2,lde), eUBde(2,lde)
      do i=tLBde(1,lde), eLBde(1,lde)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong value in section 8 for DE", localDeToDeMap(lde), &
            " on PET", localPet, i, j, farrayPtr(i,j), verifyValue
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    
  enddo ! - local DE loop
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)

  deallocate (localDeToDeMap)
  deallocate (eLBde, eUBde, tLBde, tUBde)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "DELayout Destroy Test-2b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DELayoutDestroy(delayout, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-3: 2D decomposition, overlapping halo regions, asymmetric haloDepth

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, &
    startregion=ESMF_STARTREGION_COMPUTATIONAL, &
    haloLDepth=(/0,1/), haloUDepth=(/2,3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  hLB(1,1) = max(eLB(1,1)-0, tLB(1,1))
  hLB(2,1) = max(eLB(2,1)-1, tLB(2,1))
  hUB(1,1) = min(eUB(1,1)+2, tUB(1,1))
  hUB(2,1) = min(eUB(2,1)+3, tUB(2,1)) 
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-3"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      if (farrayPtr(i,j) /= localPet) then
        verifyFlag = .false.
        print *, "Found wrong exclusive element"
        exit
      endif
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=hLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-3"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-4: 2D decomposition, overlapping halo regions, asymmetric haloDepth
!         and use of startregion

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/1,1/), computationalUWidth=(/1,1/), &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    computationalLBound=cLB, computationalUBound=cUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, &
    startregion=ESMF_STARTREGION_COMPUTATIONAL, &
    haloLDepth=(/0,1/), haloUDepth=(/2,3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  hLB(1,1) = max(cLB(1,1)-0, tLB(1,1))
  hLB(2,1) = max(cLB(2,1)-1, tLB(2,1))
  hUB(1,1) = min(cUB(1,1)+2, tUB(1,1))
  hUB(2,1) = min(cUB(2,1)+3, tUB(2,1)) 
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
! debugging ---------------  
!call ESMF_ArrayPrint(array)  
!print *, farrayPtr  
! debugging ---------------  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-4"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      if (farrayPtr(i,j) /= localPet) then
        verifyFlag = .false.
        print *, "Found wrong exclusive element"
        exit
      endif
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), cLB(2,1)-1
      do i=hLB(1,1), cLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cLB(2,1), eLB(2,1)-1
      do i=cLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), cLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), cLB(2,1)-1
      do i=cUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, cUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=cUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, cUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cUB(2,1)+1, hUB(2,1)
      do i=cUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, cUB(2,1)
      do i=eUB(1,1)+1, cUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cUB(2,1)+1, hUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, cUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cUB(2,1)+1, hUB(2,1)
      do i=hLB(1,1), cLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, cUB(2,1)
      do i=cLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=hLB(1,1), cLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=cLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-4"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-5: 2D decomposition with periodic boundary condition along 2nd dimension,
!         overlapping halo regions, asymmetric haloDepth and
!         use of startregion

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Connection Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  allocate(connectionList(1))  ! single connection
  call ESMF_DistGridConnectionSet(connection=connectionList(1), &
     tileIndexA=1, tileIndexB=1, &
     positionVector=(/0, 20/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), connectionList=connectionList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(connectionList)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    computationalLWidth=(/1,1/), computationalUWidth=(/1,1/), &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    computationalLBound=cLB, computationalUBound=cUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!             +-------------------+       +-------------------+
!             | \       2       / |       | \       2       / |
!             |  +-------------+  |       |  +-------------+  |
!             |  |     DE 0    |  |       |  |     DE 2    |  |
!             |  |             |  |       |  |             |  |
! periodic <- |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2| -> periodic
!             |  |             |  |       |  |             |  |
!             |  |             |  |       |  |             |  |
!             |  +-------------+  |       |  +-------------+  |
!             | /       2       \ |       | /       2       \ |
!             +-------------------+       +-------------------+
!
!                       ^            \/             ^
!                       |            /\             |
!                       v                           v
!
!             +-------------------+       +-------------------+
!             | \       2       / |       | \       2       / |
!             |  +-------------+  |       |  +-------------+  |
!             |  |     DE 1    |  |       |  |     DE 3    |  |
!             |  |             |  |       |  |             |  |
! periodic <- |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2| -> periodic
!             |  |             |  |       |  |             |  |
!             |  |             |  |       |  |             |  |
!             |  +-------------+  |       |  +-------------+  |
!             | /       2       \ |       | /       2       \ |
!             +-------------------+       +-------------------+
!
! Explicitly set outer connections and implicit inner connections in the 
! DistGrid define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, &
    startregion=ESMF_STARTREGION_COMPUTATIONAL, &
    haloLDepth=(/0,1/), haloUDepth=(/2,3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  hLB(1,1) = max(cLB(1,1)-0, tLB(1,1))
  hLB(2,1) = max(cLB(2,1)-1, tLB(2,1))
  hUB(1,1) = min(cUB(1,1)+2, tUB(1,1))
  hUB(2,1) = min(cUB(2,1)+3, tUB(2,1)) 
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-5"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      if (farrayPtr(i,j) /= localPet) then
        verifyFlag = .false.
        print *, "Found wrong exclusive element"
        exit
      endif
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), cLB(2,1)-1
      do i=hLB(1,1), cLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cLB(2,1), eLB(2,1)-1
      do i=cLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 1"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), cLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 2"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), cLB(2,1)-1
      do i=cUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, cUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 3"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=cUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, cUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 4"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cUB(2,1)+1, hUB(2,1)
      do i=cUB(1,1)+1, hUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, cUB(2,1)
      do i=eUB(1,1)+1, cUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 5"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cUB(2,1)+1, hUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, cUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 6"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=cUB(2,1)+1, hUB(2,1)
      do i=hLB(1,1), cLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, cUB(2,1)
      do i=cLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 7"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong outside value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=hLB(1,1), cLB(1,1)-1
        if (farrayPtr(i,j) /= verifyValue) then
          verifyFlag = .false.
          print *, "Found wrong halo value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=cLB(1,1), eLB(1,1)-1
        if (farrayPtr(i,j) /= localPet) then
          verifyFlag = .false.
          print *, "Found wrong inside value in section 8"
          exit
        endif
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-5"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-6: 2D decomposition, overlapping halo regions, asymmetric haloDepth,
!         Array with undistributed dimension (last dim, not weakly congruent)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), &
    undistLBound=(/-3/), undistUBound=(/5/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, undistLBound=uLB, undistUBound=uUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3d, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  do m=uLB(1), uUB(1)
    farrayPtr3d(:,:,m) = localPet + 10*m
  enddo

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, &
    startregion=ESMF_STARTREGION_COMPUTATIONAL, &
    haloLDepth=(/0,1/), haloUDepth=(/2,3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  hLB(1,1) = max(eLB(1,1)-0, tLB(1,1))
  hLB(2,1) = max(eLB(2,1)-1, tLB(2,1))
  hUB(1,1) = min(eUB(1,1)+2, tUB(1,1))
  hUB(2,1) = min(eUB(2,1)+3, tUB(2,1)) 
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
! debugging ---------------  
!call ESMF_ArrayPrint(array)  
!print *, farrayPtr  
! debugging ---------------  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-6"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      do m=uLB(1), uUB(1)
        if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
          verifyFlag = .false.
          print *, "Found wrong exclusive element"
          exit
        endif
      enddo
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 1"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 1"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 2"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 2"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 3"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 3"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 4"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 4"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 5"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 5"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 6"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 6"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 7"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 7"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 8"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(i,j,m) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 8"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-6"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
! Test-7: 2D decomposition, overlapping halo regions, asymmetric haloDepth,
!         Array with undistributed dimension (first dim, weakly congruent)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create Test-7"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySpec Set Test-7"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create Test-7a"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    distgridToArrayMap=(/2,3/), &
    undistLBound=(/-2/), undistUBound=(/5/), &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-7a"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, undistLBound=uLB, undistUBound=uUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Array object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Array further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Array object.
! 
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 0    |  |       |  |     DE 2    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
!                    ^            \/             ^
!                    |            /\             |
!                    v                           v
!
!          +-------------------+       +-------------------+
!          | \       2       / |       | \       2       / |
!          |  +-------------+  |       |  +-------------+  |
!          |  |     DE 1    |  |       |  |     DE 3    |  |
!          |  |             |  |       |  |             |  |
!          |2 |    5 x 10   | 2|  <->  |2 |    5 x 10   | 2|
!          |  |             |  |       |  |             |  |
!          |  |             |  |       |  |             |  |
!          |  +-------------+  |       |  +-------------+  |
!          | /       2       \ |       | /       2       \ |
!          +-------------------+       +-------------------+
!
! Without the explicit definition of boundary conditions only the indicated
! inner connections define valid Halo paths for the Array object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-7a"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3d, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  do m=uLB(1), uUB(1)
    farrayPtr3d(m,:,:) = localPet + 10*m
  enddo

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHaloStore Test-7"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloStore(array=array, routehandle=routehandle, &
    startregion=ESMF_STARTREGION_COMPUTATIONAL, &
    haloLDepth=(/0,1/), haloUDepth=(/2,3/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  hLB(1,1) = max(eLB(1,1)-0, tLB(1,1))
  hLB(2,1) = max(eLB(2,1)-1, tLB(2,1))
  hUB(1,1) = min(eUB(1,1)+2, tUB(1,1))
  hUB(2,1) = min(eUB(2,1)+3, tUB(2,1)) 
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo Test-7a"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-7a"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      do m=uLB(1), uUB(1)
        if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
          verifyFlag = .false.
          print *, "Found wrong exclusive element"
          exit
        endif
      enddo
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 1", localPet, m, i, j, farrayPtr3d(m,i,j)
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 1"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 2"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 2"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 3"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 3"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 4"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 4"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 5"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 5"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 6"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 6"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 7"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 7"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 8"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 8"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-7a"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Create weakly congruent Test-7b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    distgridToArrayMap=(/2,3/), &
    undistLBound=(/1/), undistUBound=(/15/), &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Get Test-7b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, undistLBound=uLB, undistUBound=uUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Array Test-7b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3d, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initialize the entire Array piece on every PET to the localPet number
!------------------------------------------------------------------------
  do m=uLB(1), uUB(1)
    farrayPtr3d(m,:,:) = localPet + 10*m
  enddo

  hLB(1,1) = max(eLB(1,1)-0, tLB(1,1))
  hLB(2,1) = max(eLB(2,1)-1, tLB(2,1))
  hUB(1,1) = min(eUB(1,1)+2, tUB(1,1))
  hUB(2,1) = min(eUB(2,1)+3, tUB(2,1)) 
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayHalo weakly congruent Test-7b"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayHalo(array=array, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array elements after Halo() Test-7b"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do j=eLB(2,1), eUB(2,1)
    do i=eLB(1,1), eUB(1,1)
      do m=uLB(1), uUB(1)
        if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
          verifyFlag = .false.
          print *, "Found wrong exclusive element"
          exit
        endif
      enddo
    enddo
    if (.not. verifyFlag) exit
  enddo
  
  ! verify all eight sections outside the exclusive region
  ! section 1 staring in NW corner and going counter clock wise
  ! verify section 1
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 0
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 1"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 1"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 2
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 0
  else if (localPet == 3) then
    verifyValue = 1
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 2"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 2"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 3
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 1
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=tLB(2,1), hLB(2,1)-1
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 3"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=hLB(2,1), eLB(2,1)-1
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 3"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 4
  if (localPet == 0) then
    verifyValue = 1
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = 3
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 4"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 4"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 5
  if (localPet == 0) then
    verifyValue = 3
  else if (localPet == 1) then
    verifyValue = localPet
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=hUB(1,1)+1, tUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 5"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eUB(1,1)+1, hUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 5"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 6
  if (localPet == 0) then
    verifyValue = 2
  else if (localPet == 1) then
    verifyValue = 3
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 6"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=eLB(1,1), eUB(1,1)
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 6"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 7
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 2
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = localPet
  endif
  if (verifyFlag) then
    do j=hUB(2,1)+1, tUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 7"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eUB(2,1)+1, hUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 7"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
  ! verify section 8
  if (localPet == 0) then
    verifyValue = localPet
  else if (localPet == 1) then
    verifyValue = 0
  else if (localPet == 2) then
    verifyValue = localPet
  else if (localPet == 3) then
    verifyValue = 2
  endif
  if (verifyFlag) then
    do j=eLB(2,1), eUB(2,1)
      do i=tLB(1,1), hLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= localPet + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong outside value in section 8"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
    do j=eLB(2,1), eUB(2,1)
      do i=hLB(1,1), eLB(1,1)-1
        do m=uLB(1), uUB(1)
          if (farrayPtr3d(m,i,j) /= verifyValue + 10*m) then
            verifyFlag = .false.
            print *, "Found wrong halo value in section 8"
            exit
          endif
        enddo
      enddo
      if (.not. verifyFlag) exit
    enddo
  endif
    
  call ESMF_Test(verifyFlag, name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array Destroy Test-7b"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "routehandle Release Test-7"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Destroy Test-7"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_DistGridDestroy(distGrid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayHaloUTest
