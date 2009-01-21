! $Id: ESMF_ArrayScatterUTest.F90,v 1.30.2.6 2009/01/21 21:25:19 cdeluca Exp $
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
program ESMF_ArrayScatterUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

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
    '$Id: ESMF_ArrayScatterUTest.F90,v 1.30.2.6 2009/01/21 21:25:19 cdeluca Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  real(ESMF_KIND_R8), parameter :: min_R8 = 1e-10_ESMF_KIND_R8
  real(ESMF_KIND_R4), parameter :: min_R4 = 1e-4_ESMF_KIND_R4
  type(ESMF_VM):: vm
  integer:: petCount, localPet, i, j
  type(ESMF_ArraySpec)  :: arrayspec
  type(ESMF_DistGrid)   :: distgrid
  type(ESMF_Array)      :: array
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)     ! matching F90 array pointer
  real(ESMF_KIND_R8), allocatable :: srcfarray(:,:)
  real(ESMF_KIND_R8):: value
  real(ESMF_KIND_R4), pointer :: farrayPtr_R4(:,:)     ! matching F90 array pointer
  real(ESMF_KIND_R4), allocatable :: srcfarray_R4(:,:)
  real(ESMF_KIND_R4):: value_R4
#ifdef ESMF_TESTEXHAUSTIVE
  integer:: k, kk, ii, jj, dimExtent1, dimExtent2
  integer, allocatable:: indexList1(:), indexList2(:)
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
  
print *, min_R4, min_R8
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
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
  write(name, *) "2D ESMF_TYPEKIND_R8 ArrayScatter() w/ incompatible Fortran Array (typekind) Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayScatter(array, srcfarray_R4, rootPet=0, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
#ifdef ESMF_TESTEXHAUSTIVE
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "2D ESMF_TYPEKIND_R8 ArrayScatter() w/ incompatible Fortran Array (rank) Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayScatter(array, srcfarray3d, rootPet=0, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2D ESMF_TYPEKIND_R8 ArrayScatter() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayScatter(array, srcfarray, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify srcfarray data after scatter
  write(name, *) "Verifying srcfarray data after 2D ESMF_TYPEKIND_R8 ArrayScatter() Test"
  write(failMsg, *) "Source data was modified."
  rc = ESMF_SUCCESS
  do j=1, 23
    do i=1, 15
      value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
               321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8))
      value = value - srcfarray(i,j)
!print *, value
      if (abs(value) > min_R8) then
!print *, "Found large value"
        rc = ESMF_FAILURE
      endif
   enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(name, *) "Verifying destination Array data after 2D ESMF_TYPEKIND_R8 ArrayScatter() Test"
  write(failMsg, *) "Array data wrong."
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr,2), ubound(farrayPtr,2)
    do i=lbound(farrayPtr,1), ubound(farrayPtr,1)
      !print *, i, j, farrayPtr(i,j), srcfarray(i,j)
      if (abs(farrayPtr(i,j) - srcfarray(i,j)) > min_R8) rc = ESMF_FAILURE
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
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! preparations for same test as above but omit farray on PETs not root
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
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
  write(name, *) "2D ESMF_TYPEKIND_R8 ArrayScatter() with omitted srcfarray Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  if (localPet==0) then
    call ESMF_ArrayScatter(array, farray=srcfarray, rootPet=0, rc=rc)
  else
    call ESMF_ArrayScatter(array, rootPet=0, rc=rc)
  endif
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify srcfarray data after scatter
  write(name, *) "Verifying srcfarray data after 2D ESMF_TYPEKIND_R8 ArrayScatter() ",&
    "with omitted srcfarray Test"
  write(failMsg, *) "Source data was modified."
  rc = ESMF_SUCCESS
  do j=1, 23
    do i=1, 15
      value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
               321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8))
      value = value - srcfarray(i,j)
!print *, value
      if (abs(value) > min_R8) then
!print *, "Found large value"
        rc = ESMF_FAILURE
      endif
   enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(name, *) "Verifying destination Array data after 2D ESMF_TYPEKIND_R8 ",&
    "ArrayScatter() with omitted srcfarray Test"
  write(failMsg, *) "Array data wrong."
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr,2), ubound(farrayPtr,2)
    do i=lbound(farrayPtr,1), ubound(farrayPtr,1)
      !print *, i, j, farrayPtr(i,j), srcfarray(i,j)
      if (abs(farrayPtr(i,j) - srcfarray(i,j)) > min_R8) rc = ESMF_FAILURE
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
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! preparations for same test as above but with ESMF_TYPEKIND_R4
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R4, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr_R4, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr_R4 = real(localPet,ESMF_KIND_R4)  ! initialize each DE-local data chunk of Array
!print *, "farrayPtr_R4:", farrayPtr_R4
  ! prepare srcfarray_R4 on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray_R4(1:15, 1:23))
  do j=1, 23
    do i=1, 15
      srcfarray_R4(i,j) = 123._ESMF_KIND_R4*sin(real(i,ESMF_KIND_R4)) +  &
                       321._ESMF_KIND_R4*cos(real(j,ESMF_KIND_R4))
    enddo
  enddo
!print *, "srcfarray_R4:", srcfarray_R4
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2D ESMF_TYPEKIND_R4 ArrayScatter() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayScatter(array, srcfarray_R4, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify srcfarray_R4 data after scatter
  write(name, *) "Verifying srcfarray_R4 data after 2D ESMF_TYPEKIND_R4 ArrayScatter() Test"
  write(failMsg, *) "Source data was modified."
  rc = ESMF_SUCCESS
  do j=1, 23
    do i=1, 15
      value_R4 = 123._ESMF_KIND_R4*sin(real(i,ESMF_KIND_R4)) +  &
               321._ESMF_KIND_R4*cos(real(j,ESMF_KIND_R4))
      value_R4 = value_R4 - srcfarray_R4(i,j)
!print *, value_R4
      if (abs(value_R4) > min_R4) then
!print *, "Found large value", i, j, value_R4
        rc = ESMF_FAILURE
      endif
   enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(name, *) "Verifying destination Array data after 2D ESMF_TYPEKIND_R4 ArrayScatter() Test"
  write(failMsg, *) "Array data wrong."
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr_R4,2), ubound(farrayPtr_R4,2)
    do i=lbound(farrayPtr_R4,1), ubound(farrayPtr_R4,1)
      !print *, i, j, farrayPtr_R4(i,j), srcfarray_R4(i,j)
      if (abs(farrayPtr_R4(i,j) - srcfarray_R4(i,j)) > min_R4) rc = ESMF_FAILURE
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  ! cleanup  
  call ESMF_ArrayDestroy(array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  deallocate(srcfarray_R4)
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! preparations for same test as above but with a DistGrid that has less
  ! elements in the first dimension than DEs requested in the regDecomp argument.
  ! -> there will be DEs not associated with DistGrid elements, namely DE 1 and
  ! DE 3.
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/1,23/), &
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
      srcfarray(i,j) = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
                       321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8))
    enddo
  enddo
!print *, "srcfarray:", srcfarray
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2D ESMF_TYPEKIND_R8 ArrayScatter() with unassociated DEs Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
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
      value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
              321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8))
      value = value - srcfarray(i,j)
!print *, value
      if (abs(value) > min_R8) then
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
  write(name, *) "Verifying destination Array data after 2D ArrayScatter() with unassociated DEs Test"
  rc = ESMF_SUCCESS
  do j=lbound(farrayPtr,2), ubound(farrayPtr,2)
    do i=lbound(farrayPtr,1), ubound(farrayPtr,1)
      if (abs(farrayPtr(i,j) - srcfarray(i,j)) > min_R8) rc = ESMF_FAILURE
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

#ifdef ESMF_TESTEXHAUSTIVE

  !------------------------------------------------------------------------
  ! preparations for testing ArrayScatter() for a 
  ! 2D+1 Array, i.e. an Array with 3D data rank but 2D decomposition
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, undistLBound=(/-5/), undistUBound=(/4/), &
    rc=rc)
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
        srcfarray3d(i,j,k) = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
                             321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8)) +  &
                             20._ESMF_KIND_R8*real(k,ESMF_KIND_R8)
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
        value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +   &
                321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8)) +   &
                20._ESMF_KIND_R8*real(k,ESMF_KIND_R8)
        value = value - srcfarray3d(i,j,k)
        if (abs(value) > min_R8) rc = ESMF_FAILURE
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(failMsg, *) "Array data wrong."
  write(name, *) "Verifying destination Array data after 2D+1 ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do k=lbound(farrayPtr3d,3), ubound(farrayPtr3d,3)
    kk = k - lbound(farrayPtr3d,3) + lbound(srcfarray3d,3)
    do j=lbound(farrayPtr3d,2), ubound(farrayPtr3d,2)
      do i=lbound(farrayPtr3d,1), ubound(farrayPtr3d,1)
        if (abs(farrayPtr3d(i,j,k) - srcfarray3d(i,j,kk)) > min_R8) then
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
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! preparations for testing ArrayScatter() for a 2D+1 Array with 
  ! non-contiguous exclusive region 
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,3/), totalUWidth=(/3,4/), indexflag=ESMF_INDEX_GLOBAL, &
    undistLBound=(/-5/), undistUBound=(/4/), rc=rc)
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
        srcfarray3d(i,j,k) = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) + &
                             321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8)) + &
                             20._ESMF_KIND_R8*real(k,ESMF_KIND_R8)
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
  write(name, *) "Verifying srcfarray3d data after 2D+1 non-contiguous exclusive ",&
    "region ArrayScatter() Test"
  rc = ESMF_SUCCESS
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
                321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8)) +  &
                 20._ESMF_KIND_R8*real(k,ESMF_KIND_R8)
        value = value - srcfarray3d(i,j,k)
        if (abs(value) > min_R8) rc = ESMF_FAILURE
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(name, *) "Verifying destination Array data after 2D+1 non-contiguous ",&
    "exclusive region ArrayScatter() Test"
  write(failMsg, *) "Array data wrong."
  rc = ESMF_SUCCESS
  do k=lbound(farrayPtr3d,3), ubound(farrayPtr3d,3)
    kk = k - lbound(farrayPtr3d,3) + lbound(srcfarray3d,3)
    do j=exclusiveLBound(2,1), exclusiveUBound(2,1)
      do i=exclusiveLBound(1,1), exclusiveUBound(1,1)
        if (abs(farrayPtr3d(i,j,k) - srcfarray3d(i,j,kk)) > min_R8) then
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

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! preparations for testing ArrayScatter() for a 2D+1 Array with 
  ! non-contiguous exclusive region and cyclic decomposition
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/0,1/), maxIndex=(/14,23/), &
    regDecomp=(/2,2/), decompflag=(/ESMF_DECOMP_DEFAULT,ESMF_DECOMP_CYCLIC/),&
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    totalLWidth=(/2,3/), totalUWidth=(/3,4/), &
    undistLBound=(/-5/), undistUBound=(/4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
!call ESMF_ArrayPrint(array)
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3d, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  farrayPtr3d = real(localPet,ESMF_KIND_R8)  ! initialize each DE-local data chunk of Array
!print *, "farrayPtr3d:", farrayPtr3d
  ! prepare srcfarray on all PETs -> serves as ref. in comparison after scatter
  allocate(srcfarray3d(15, 23, 10))
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        srcfarray3d(i,j,k) = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) + &
                             321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8)) + &
                             20._ESMF_KIND_R8*real(k,ESMF_KIND_R8)
      enddo
    enddo
  enddo
!print *, "srcfarray3d:", srcfarray3d

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "2D+1 non-contiguous exclusive region and cyclic decomposition ",&
    "ArrayScatter() Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayScatter(array, srcfarray3d, rootPet=0, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
!call ESMF_ArrayPrint(array)  
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "ArrayGet() Test"
  call ESMF_ArrayGet(array, localDe=0, dim=1, indexCount=dimExtent1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  allocate(indexList1(dimExtent1))
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "ArrayGet() Test"
  call ESMF_ArrayGet(array, localDe=0, dim=1, indexList=indexList1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "ArrayGet() Test"
  call ESMF_ArrayGet(array, localDe=0, dim=2, indexCount=dimExtent2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  allocate(indexList2(dimExtent2))
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "ArrayGet() Test"
  call ESMF_ArrayGet(array, localDe=0, dim=2, indexList=indexList2, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify srcfarray3d data after scatter
  write(name, *) "Verifying srcfarray3d data after 2D+1 non-contiguous ",&
    "exclusive region and cyclic decomposition ArrayScatter() Test"
  write(failMsg, *) "Source data was modified."
  rc = ESMF_SUCCESS
  do k=lbound(srcfarray3d,3), ubound(srcfarray3d,3)
    do j=lbound(srcfarray3d,2), ubound(srcfarray3d,2)
      do i=lbound(srcfarray3d,1), ubound(srcfarray3d,1)
        value = 123._ESMF_KIND_R8*sin(real(i,ESMF_KIND_R8)) +  &
                321._ESMF_KIND_R8*cos(real(j,ESMF_KIND_R8)) +  &
                 20._ESMF_KIND_R8*real(k,ESMF_KIND_R8)
        value = value - srcfarray3d(i,j,k)
        if (abs(value) > min_R8) rc = ESMF_FAILURE
      enddo
    enddo
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  ! Verify Array data after scatter
  write(name, *) "Verifying destination Array data after 2D+1 non-contiguous ",&
    "exclusive region and cyclic decomposition ArrayScatter() Test"
  write(failMsg, *) "Array data wrong."
  rc = ESMF_SUCCESS
  do k=lbound(farrayPtr3d,3), ubound(farrayPtr3d,3)
    ! correct wrt lbound
    kk = k - lbound(farrayPtr3d,3) + lbound(srcfarray3d,3)
    do j=1, dimExtent2
      ! dereference via indexList and correct wrt lbound 
      jj = indexList2(j) - 1 + lbound(srcfarray3d,2)
      do i=1, dimExtent1
        ! dereference via indexList and correct wrt lbound 
        ii = indexList1(i) - 0 + lbound(srcfarray3d,1)
        if (abs(farrayPtr3d(i,j,k) - srcfarray3d(ii,jj,kk)) > min_R8) then
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
  deallocate(indexList1)
  deallocate(indexList2)
  
#endif
  

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayScatterUTest
