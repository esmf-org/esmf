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
program ESMF_ArrayBundleRedistUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayBundleRedistUTest 
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayBundleCreate() unit tests.
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
  type(ESMF_VM)                 :: vm
  integer                       :: petCount, i
  type(ESMF_DistGrid)           :: srcDG, dstDG
  type(ESMF_Array), allocatable :: srcArrayList(:), dstArrayList(:)
  type(ESMF_Array), allocatable :: checkArrayList(:)
  type(ESMF_ArrayBundle)        :: srcAB, dstAB
  type(ESMF_RouteHandle)        :: rh, rhRev
  logical                       :: match
  
!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 

  ! start the test environment
  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  
  ! prepare auxiliary data objects
  !------------------------------------------------------------------------
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  call ESMF_VMGet(vm, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  srcDG = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/150,230/), &
    regDecomp=(/petCount,1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  dstDG = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/150,230/), &
    regDecomp=(/1,petCount/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  allocate(srcArrayList(3))
  ! - 1
  srcArrayList(1) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArrayR8(srcArrayList(1), scale=1.d0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 2
  srcArrayList(2) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArrayR8(srcArrayList(2), scale=2.d0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 3
  srcArrayList(3) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArrayR4(srcArrayList(3), scale=3.e0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  allocate(checkArrayList(3))
  ! - 1
  checkArrayList(1) = ESMF_ArrayCreate(srcArrayList(1), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayCopy(checkArrayList(1), srcArrayList(1), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 2
  checkArrayList(2) = ESMF_ArrayCreate(srcArrayList(2), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayCopy(checkArrayList(2), srcArrayList(2), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 3
  checkArrayList(3) = ESMF_ArrayCreate(srcArrayList(3), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArrayCopy(checkArrayList(3), srcArrayList(3), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  allocate(dstArrayList(3))
  ! - 1
  dstArrayList(1) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 2
  dstArrayList(2) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 3
  dstArrayList(3) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create src ArrayBundle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  srcAB = ESMF_ArrayBundleCreate(arrayList=srcArrayList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create dst ArrayBundle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  dstAB = ESMF_ArrayBundleCreate(arrayList=dstArrayList, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check for data match between src and check Arrays Test"
  write(failMsg, *) "Found data mismatch"
  match = dataMatchArrayLists(srcArrayList, checkArrayList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test((match), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedistStore src->dst Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedistStore(srcAB, dstAB, routehandle=rh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check for data match between src and check Arrays Test"
  write(failMsg, *) "Found data mismatch"
  match = dataMatchArrayLists(srcArrayList, checkArrayList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test((match), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedist src->dst Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedist(srcAB, dstAB, routehandle=rh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check for data match between src and check Arrays Test"
  write(failMsg, *) "Found data mismatch"
  match = dataMatchArrayLists(srcArrayList, checkArrayList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test((match), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedistStore dst->src Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedistStore(dstAB, srcAB, routehandle=rhRev, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check for data match between src and check Arrays Test"
  write(failMsg, *) "Found data mismatch"
  match = dataMatchArrayLists(srcArrayList, checkArrayList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test((match), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  ! scramble the data in the src arrays
  call fillArrayR8(srcArrayList(1), scale=10.d0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArrayR8(srcArrayList(2), scale=10.d0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArrayR4(srcArrayList(3), scale=10.e0, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedist dst->src Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedist(dstAB, srcAB, routehandle=rhRev, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Check for data match between src and check Arrays Test"
  write(failMsg, *) "Found data mismatch"
  match = dataMatchArrayLists(srcArrayList, checkArrayList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_Test((match), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedist Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedist(srcAB, dstAB, routehandle=rh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RouteHandleWrite Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_RouteHandleWrite(rh, fileName="abRedistTest.RH", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedistRelease Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedistRelease(rh, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "RouteHandleCreate(from file) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  rh = ESMF_RouteHandleCreate(fileName="abRedistTest.RH", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedist after reading Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedist(srcAB, dstAB, routehandle=rh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedistRelease after reading Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedistRelease(rh, noGarbage=.true., rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy src ArrayBundle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(srcAB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy dst ArrayBundle Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(dstAB, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------


  !------------------------------------------------------------------------
  ! cleanup helper data objects
  do i=1, size(srcArrayList)
    call ESMF_ArrayDestroy(srcArrayList(i), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  deallocate(srcArrayList)
  do i=1, size(dstArrayList)
    call ESMF_ArrayDestroy(dstArrayList(i), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  deallocate(dstArrayList)
  
10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------
  
 contains
 
  subroutine fillArrayR8(array, scale, rc)
    type(ESMF_Array)    :: array
    real(ESMF_KIND_R8)  :: scale
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R8), pointer   :: fptr(:,:)
    integer                       :: i, j
    call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    do j=lbound(fptr,2),ubound(fptr,2)
    do i=lbound(fptr,1),ubound(fptr,1)
      fptr(i,j) = scale*(sin(real(i,ESMF_KIND_R8)) + cos(real(j,ESMF_KIND_R8)))
    enddo
    enddo
  end subroutine

  subroutine fillArrayR4(array, scale, rc)
    type(ESMF_Array)    :: array
    real(ESMF_KIND_R4)  :: scale
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R4), pointer   :: fptr(:,:)
    integer                       :: i, j
    call ESMF_ArrayGet(array, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    do j=lbound(fptr,2),ubound(fptr,2)
    do i=lbound(fptr,1),ubound(fptr,1)
      fptr(i,j) = scale*(sin(real(i,ESMF_KIND_R4)) + cos(real(j,ESMF_KIND_R4)))
    enddo
    enddo
  end subroutine

  function dataMatchArrays(array1, array2, rc)
    logical             :: dataMatchArrays
    type(ESMF_Array)    :: array1
    type(ESMF_Array)    :: array2
    integer             :: rc
    !-----------------------------------------
    type(ESMF_TypeKind_Flag) :: tk
    call ESMF_ArrayGet(array1, typekind=tk, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    dataMatchArrays = .true.
    if (tk==ESMF_TYPEKIND_R8) then
      dataMatchArrays = dataMatchR8(array1, array2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    else
      dataMatchArrays = dataMatchR4(array1, array2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif
  end function

  function dataMatchR8(array1, array2, rc)
    logical             :: dataMatchR8
    type(ESMF_Array)    :: array1
    type(ESMF_Array)    :: array2
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R8), pointer   :: fptr1(:,:), fptr2(:,:)
    real(ESMF_KIND_R8)            :: diff
    integer                       :: i, j
    character(len=160)            :: msg
    call ESMF_ArrayGet(array1, farrayPtr=fptr1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_ArrayGet(array2, farrayPtr=fptr2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    dataMatchR8 = .true.  ! initialize
    do j=lbound(fptr1,2),ubound(fptr1,2)
    do i=lbound(fptr1,1),ubound(fptr1,1)
      diff = abs(fptr1(i,j) - fptr2(i,j))
      if (diff > 1.d-10) then
        dataMatchR8 = .false.
        write(msg,*) "Found data mismatch at (",i,",",j,"), diff=", diff
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      endif
    enddo
    enddo
  end function

  function dataMatchR4(array1, array2, rc)
    logical             :: dataMatchR4
    type(ESMF_Array)    :: array1
    type(ESMF_Array)    :: array2
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R4), pointer   :: fptr1(:,:), fptr2(:,:)
    real(ESMF_KIND_R4)            :: diff
    integer                       :: i, j
    character(len=160)            :: msg
    call ESMF_ArrayGet(array1, farrayPtr=fptr1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_ArrayGet(array2, farrayPtr=fptr2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    dataMatchR4 = .true.  ! initialize
    do j=lbound(fptr1,2),ubound(fptr1,2)
    do i=lbound(fptr1,1),ubound(fptr1,1)
      diff = abs(fptr1(i,j) - fptr2(i,j))
      if (diff > 1.e-6) then
        dataMatchR4 = .false.
        write(msg,*) "Found data mismatch at (",i,",",j,"), diff=", diff
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      endif
    enddo
    enddo
  end function
  
  function dataMatchArrayLists(arrayList1, arrayList2, rc)
    logical             :: dataMatchArrayLists
    type(ESMF_Array)    :: arrayList1(:)
    type(ESMF_Array)    :: arrayList2(:)
    integer             :: rc
    !-----------------------------------------
    integer                       :: i, j
    character(len=160)            :: msg
    dataMatchArrayLists = .true.
    do i=1, size(arrayList1)
      write(msg,*) "Checking array pair #", i
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      dataMatchArrayLists = &
        dataMatchArrays(srcArrayList(i), checkArrayList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (.not.dataMatchArrayLists) exit
    enddo
  end function

end program ESMF_ArrayBundleRedistUTest
