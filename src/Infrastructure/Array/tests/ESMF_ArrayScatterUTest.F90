! $Id: ESMF_ArrayScatterUTest.F90,v 1.12 2007/02/22 23:42:17 theurich Exp $
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
program ESMF_ArrayScatterUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayScatterUTest - This unit test file tests ArrayScatter()
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayScatter unit tests.
! The companion file ESMF\_Array.F90 contains the definitions for the
! Array methods.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_ArrayScatterUTest.F90,v 1.12 2007/02/22 23:42:17 theurich Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  real(ESMF_KIND_R8), parameter :: double_min = 1e-10_ESMF_KIND_R8
  type(ESMF_VM):: vm
  integer:: petCount, localPet, i, j
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_DistGrid)   :: distgrid
  type(ESMF_Array)      :: array
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)     ! matching F90 array pointer
  real(ESMF_KIND_R8), allocatable :: srcfarray(:,:)
  real(ESMF_KIND_R8):: value
#ifdef ESMF_EXHAUSTIVE
  integer:: k, kk
  real(ESMF_KIND_R8), pointer :: farrayPtr3d(:,:,:) ! matching F90 array pointer
  real(ESMF_KIND_R8), allocatable :: srcfarray3d(:,:,:)
  integer:: exclusiveLBound(2,1), exclusiveUBound(2,1)
#endif

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
  call ESMF_VMGetGlobal(vm, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  ! this unit test requires to be run on exactly 4 PETs
  if (petCount /= 4) goto 10
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_ArraySpecSet(arrayspec, type=ESMF_DATA_REAL, kind=ESMF_TYPEKIND_R8, rank=2, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr = real(localPet,ESMF_KIND_R8)  ! initialize each DE-local data chunk of Array
!print *, "farrayPtr:", farrayPtr
  ! prepare srcfarray on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray(1:15, 1:23))
  do j=1, 23
    do i=1, 15
      srcfarray(i,j) = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
                       321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8))
    enddo
  enddo
!print *, "srcfarray:", srcfarray
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "2D ArrayScatter() Test"
  call ESMF_ArrayScatter(array, srcfarray, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify srcfarray data after scatter
  write(failMsg, *) "Source data was modified."
  write(name, *) "Verifying srcfarray data after 2D ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do j=1, 23
    do i=1, 15
      value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
               321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8))
      value = srcfarray(i,j) / value - 1.d0
!print *, value
      if (abs(value) > double_min) then
!print *, "Found large value"
        rc = ESMF_FAILURE
      endif
   enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(failMsg, *) "Array data wrong."
  write(name, *) "Verifying Array data after 2D ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr,2), ubound(farrayPtr,2)
    do i=lbound(farrayPtr,1), ubound(farrayPtr,1)
      if (abs(farrayPtr(i,j) - srcfarray(i,j)) > double_min) rc = ESMF_FAILURE
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(srcfarray)
  
  
  !------------------------------------------------------------------------
  ! preparations for same test as above but with a DistGrid that has less
  ! cells in the first dimension than DEs requested in the regDecomp argument.
  ! -> there will be DEs not associated with DistGrid cells, namely DE 1 and 3.
  call ESMF_ArraySpecSet(arrayspec, type=ESMF_DATA_REAL, kind=ESMF_TYPEKIND_R8, rank=2, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/1,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr = real(localPet,ESMF_KIND_R8)  ! initialize each DE-local data chunk of Array
!print *, "farrayPtr:", farrayPtr
  ! prepare srcfarray on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray(1:1, 1:23))
  do j=1, 23
    do i=1, 1
      srcfarray(i,j) = 123.d0*sin(real(i,ESMF_KIND_R8)) +  &
                       321.d0*cos(real(j,ESMF_KIND_R8))
    enddo
  enddo
!print *, "srcfarray:", srcfarray
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "2D ArrayScatter() with unassociated DEs Test"
  call ESMF_ArrayScatter(array, srcfarray, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify srcfarray data after scatter
  write(failMsg, *) "Source data was modified."
  write(name, *) "Verifying srcfarray data after 2D ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do j=1, 23
    do i=1, 1
      value = 123.d0*sin(real(i,ESMF_KIND_R8)) +  &
              321.d0*cos(real(j,ESMF_KIND_R8))
      value = srcfarray(i,j) / value - 1.d0
!print *, value
      if (abs(value) > double_min) then
!print *, "Found large value"
        rc = ESMF_FAILURE
      endif
   enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(failMsg, *) "Array data wrong."
  write(name, *) "Verifying Array data after 2D ArrayScatter() with unassociated DEs Test"
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr,2), ubound(farrayPtr,2)
    do i=lbound(farrayPtr,1), ubound(farrayPtr,1)
      if (abs(farrayPtr(i,j) - srcfarray(i,j)) > double_min) rc = ESMF_FAILURE
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(srcfarray)

#ifdef ESMF_EXHAUSTIVE

  !------------------------------------------------------------------------
  ! preparations for testing ArrayScatter() for a 
  ! 2D+1 Array, i.e. an Array with 3D data rank but 2D decomposition
  call ESMF_ArraySpecSet(arrayspec, type=ESMF_DATA_REAL, kind=ESMF_TYPEKIND_R8, rank=3, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minCorner=(/1,1/), maxCorner=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, lbounds=(/-5/), ubounds=(/4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr3d = real(localPet,ESMF_KIND_R8)  ! initialize each DE-local data chunk of Array
!print *, "farrayPtr3d:", farrayPtr3d
  ! prepare srcfarray on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray3d(1:15, 1:23, 10))
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        srcfarray3d(i,j,k) = 123.d0*sin(real(i,ESMF_KIND_R8)) +  &
                             321.d0*cos(real(j,ESMF_KIND_R8)) +  &
                             20.d0*real(k,ESMF_KIND_R8)
      enddo
    enddo
  enddo
!print *, "srcfarray3d:", srcfarray3d

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "2D+1 ArrayScatter() Test"
  call ESMF_ArrayScatter(array, srcfarray3d, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify srcfarray3d data after scatter
  write(failMsg, *) "Source data was modified."
  write(name, *) "Verifying srcfarray3d data after 2D+1 ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        value = 123.d0*sin(real(i,ESMF_KIND_R8)) +   &
                321.d0*cos(real(j,ESMF_KIND_R8)) +   &
                20.d0*real(k,ESMF_KIND_R8)
        value = srcfarray3d(i,j,k) / value - 1.d0
        if (abs(value) > double_min) rc = ESMF_FAILURE
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(failMsg, *) "Array data wrong."
  write(name, *) "Verifying Array data after 2D+1 ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do k=lbound(farrayPtr3d,3), ubound(farrayPtr3d,3)
    kk = k - lbound(farrayPtr3d,3) + lbound(srcfarray3d,3)
    do j=lbound(farrayPtr3d,2), ubound(farrayPtr3d,2)
      do i=lbound(farrayPtr3d,1), ubound(farrayPtr3d,1)
        if (abs(farrayPtr3d(i,j,k) - srcfarray3d(i,j,kk)) > double_min) then
          rc = ESMF_FAILURE
        endif
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(srcfarray3d)
  
  !------------------------------------------------------------------------
  ! preparations for testing ArrayScatter() for a 2D+1 Array with 
  ! non-contiguous exclusive region 
  call ESMF_ArraySpecSet(arrayspec, type=ESMF_DATA_REAL, kind=ESMF_TYPEKIND_R8, rank=3, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,3/), totalUWidth=(/3,4/), &
    indexflag=ESMF_INDEX_GLOBAL, lbounds=(/-5/), ubounds=(/4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr3d = real(localPet,ESMF_KIND_R8)  ! initialize each DE-local data chunk of Array
!print *, "farrayPtr3d:", farrayPtr3d
  ! prepare srcfarray on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray3d(1:15, 1:23, 10))
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        srcfarray3d(i,j,k) = 123.d0*sin(real(i,ESMF_KIND_R8)) + &
                             321.d0*cos(real(j,ESMF_KIND_R8)) + &
                             20.d0*real(k,ESMF_KIND_R8)
      enddo
    enddo
  enddo
!print *, "srcfarray3d:", srcfarray3d

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "2D+1 non-contiguous exclusive region ArrayScatter() Test"
  call ESMF_ArrayScatter(array, srcfarray3d, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!call ESMF_ArrayPrint(array)  
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "ArrayGet() Test"
  call ESMF_ArrayGet(array, exclusiveLBound=exclusiveLBound, &
    exclusiveUBound=exclusiveUBound, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify srcfarray3d data after scatter
  write(failMsg, *) "Source data was modified."
  write(name, *) "Verifying srcfarray3d data after 2D+1 non-contiguous exclusive region ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        value = 123.d0*sin(real(i,ESMF_KIND_R8)) +  &
                321.d0*cos(real(j,ESMF_KIND_R8)) +  &
                 20.d0*real(k,ESMF_KIND_R8)
        value = srcfarray3d(i,j,k) / value - 1.d0
        if (abs(value) > double_min) rc = ESMF_FAILURE
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(failMsg, *) "Array data wrong."
  write(name, *) "Verifying Array data after 2D+1 non-contiguous exclusive region ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do k=lbound(farrayPtr3d,3), ubound(farrayPtr3d,3)
    kk = k - lbound(farrayPtr3d,3) + lbound(srcfarray3d,3)
    do j=exclusiveLBound(2,1), exclusiveUBound(2,1)
      do i=exclusiveLBound(1,1), exclusiveUBound(1,1)
        if (abs(farrayPtr3d(i,j,k) - srcfarray3d(i,j,kk)) > double_min) then
          rc = ESMF_FAILURE
        endif
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(srcfarray3d)  
  
#endif
  

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayScatterUTest
