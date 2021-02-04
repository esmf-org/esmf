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
program ESMF_FieldHaloUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldHaloUTest -  Tests FieldHalo()
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
  type(ESMF_Grid)       :: grid
  type(ESMF_Field)      :: field
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_RouteHandle):: routehandle
  integer(ESMF_KIND_I4), pointer :: farrayPtr(:,:)  ! matching Fortran array pointer
  integer               :: rc, i, k, verifyValue, petCount, localPet
  logical               :: verifyFlag
  integer               :: eLB(2), eUB(2)
  integer               :: tLB(2), tUB(2)

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
    print *, "This unit test needs to run on exactly 4 PETs, petCount = ", &
      petCount
    goto 10
  endif

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "grid Create Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
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
  write(name, *) "Field Create Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  field = ESMF_FieldCreate(arrayspec=arrayspec, grid=grid, &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Field Get Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGetBounds(field, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Field object is defined on a 10 x 20 index space which is regularily
! decomposed into 1 x 4 = 4 DEs. This means that each DE holds a 10 x 5
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Field further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Field object.
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
! inner connections define valid Halo paths for the Field object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Field Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initailize the entire Field piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "FieldHaloStore Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldHaloStore(field, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "FieldHalo Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldHalo(field, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
! debugging ---------------  
call ESMF_FieldPrint(field)  
!print *, farrayPtr  
! debugging ---------------  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Field elemens after Halo() Test-1"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do k=eLB(2), eUB(2)
    do i=eLB(1), eUB(1)
      if (farrayPtr(i,k) /= localPet) then
        verifyFlag = .false.
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
    do k=tLB(2), eLB(2)-1
      do i=tLB(1), eLB(1)-1
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=tLB(2), eLB(2)-1
      do i=eLB(1), eUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=tLB(2), eLB(2)-1
      do i=eUB(1)+1, tUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eLB(2), eUB(2)
      do i=eUB(1)+1, tUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eUB(2)+1, tUB(2)
      do i=eUB(1)+1, tUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eUB(2)+1, tUB(2)
      do i=eLB(1), eUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eUB(2)+1, tUB(2)
      do i=tLB(1), eLB(1)-1
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eLB(2), eUB(2)
      do i=tLB(1), eLB(1)-1
        if (farrayPtr(i,k) /= verifyValue) then
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
  call ESMF_FieldHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Field Destroy Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldDestroy(field, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Grid Destroy Test-1"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "grid Create Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
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
  write(name, *) "Field Create Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  field = ESMF_FieldCreate(arrayspec=arrayspec, grid=grid, &
    totalLWidth=(/2,2/), totalUWidth=(/2,2/), rc=rc)

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Field Get Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGetBounds(field, exclusiveLBound=eLB, exclusiveUBound=eUB, &
    totalLBound=tLB, totalUBound=tUB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! The Field object is defined on a 10 x 20 index space which is regularily
! decomposed into 2 x 2 = 4 DEs. This means that each DE holds a 5 x 10 
! piece of the index space.
! The unit test is set up to run on exactly 4 PETs. There are, therefore,
! exactly one DE per PET.
! The Field further defines a computational width of 2 elements in each
! direction around the exclusive region. This area of 2 elements around
! the exclusive region provides the destination elements for Halo operations
! defined on the Field object.
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
! inner connections define valid Halo paths for the Field object.
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Get farrayPtr from Field Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Initailize the entire Field piece on every PET to the localPet number
!------------------------------------------------------------------------
  farrayPtr = localPet

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "FieldHaloStore Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldHaloStore(field, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "FieldHalo Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldHalo(field, routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
! debugging ---------------  
call ESMF_FieldPrint(field)  
!print *, farrayPtr  
! debugging ---------------  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Field elemens after Halo() Test-2"
  write(failMsg, *) "Wrong results" 
  
  verifyFlag = .true. ! assume all is correct until error is found
  
  ! verify elements within exclusive region
  do k=eLB(2), eUB(2)
    do i=eLB(1), eUB(1)
      if (farrayPtr(i,k) /= localPet) then
        verifyFlag = .false.
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
    do k=tLB(2), eLB(2)-1
      do i=tLB(1), eLB(1)-1
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=tLB(2), eLB(2)-1
      do i=eLB(1), eUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=tLB(2), eLB(2)-1
      do i=eUB(1)+1, tUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eLB(2), eUB(2)
      do i=eUB(1)+1, tUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eUB(2)+1, tUB(2)
      do i=eUB(1)+1, tUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eUB(2)+1, tUB(2)
      do i=eLB(1), eUB(1)
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eUB(2)+1, tUB(2)
      do i=tLB(1), eLB(1)-1
        if (farrayPtr(i,k) /= verifyValue) then
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
    do k=eLB(2), eUB(2)
      do i=tLB(1), eLB(1)-1
        if (farrayPtr(i,k) /= verifyValue) then
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
  call ESMF_FieldHaloRelease(routehandle=routehandle, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Field Destroy Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldDestroy(field, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Grid Destroy Test-2"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!TODO: consider the same Fields above, but with various boundary conditions
!TODO: on the edges of the underlying Grid.

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_FieldHaloUTest
