! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArrayCreateGetUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayCreateGetUTest - This unit test file tests ArrayCreate()
!   and ArrayGet() methods.
! !DESCRIPTION:
!
! The code in this file drives Fortran ArrayCreate(), ArrayGet() unit tests.
! The companion file ESMF\_Array.F90 contains the definitions for the
! Array methods.
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
  character(ESMF_MAXSTR) :: msg

  !LOCAL VARIABLES:
  type(ESMF_VM):: vm
  integer:: i,j,k,l, next, rank, dimCount
  integer:: undistDimCount, replicatedDimCount
  integer:: petCount, localPet, deCount, de, localDeCount, lde, ssiLocalDeCount
  integer, allocatable  :: regDecomp(:)
  type(ESMF_ArraySpec)  :: arrayspec, arrayspec2
  type(ESMF_LocalArray), allocatable :: localArrayList(:)
  type(ESMF_Array):: array, arrayAlias, arrayDup, arrayUnInit
  type(ESMF_DELayout):: delayout
  type(ESMF_DistGrid):: distgrid
  real(ESMF_KIND_R8)      :: diffR8
  real(ESMF_KIND_R4)      :: diffR4
  real(ESMF_KIND_R8)      :: farray1D(10)
  real(ESMF_KIND_R8)      :: farray2D(10,10)
  real(ESMF_KIND_R4)      :: farray3D(10,10,10)
  integer(ESMF_KIND_I4)   :: farray4D(10,10,10,10)
  real(ESMF_KIND_R8), pointer     :: farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer     :: farrayPtr2D(:,:), farrayPtr2DCpy(:,:)
  real(ESMF_KIND_R8), pointer     :: farrayPtr3DR8(:,:,:)
  real(ESMF_KIND_R4), pointer     :: farrayPtr3D(:,:,:)
  real(ESMF_KIND_R4), pointer     :: farrayPtr3Dx(:,:,:)
  integer(ESMF_KIND_I4), pointer  :: farrayPtr4D(:,:,:,:)
  real(ESMF_KIND_R4), pointer     :: farrayPtr4DR4(:,:,:,:)
  character (len=80)      :: arrayName
  integer, allocatable:: totalLWidth(:,:), totalUWidth(:,:)
  integer, allocatable:: totalLBound(:,:), totalUBound(:,:)
  integer, allocatable:: computationalLWidth(:,:), computationalUWidth(:,:)
  integer, allocatable:: minIndexPDe(:,:), maxIndexPDe(:,:)
  integer, allocatable:: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer, allocatable:: localDeToDeMap(:), arrayToDistGridMap(:)
  integer, allocatable:: undistLBound(:), undistUBound(:)
  logical:: arrayBool
  logical:: isCreated
  logical:: dataCorrect
  logical:: ssiSharedMemoryEnabled
  logical:: isESMFAllocated

  integer:: count

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
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, &
    ssiSharedMemoryEnabledFlag=ssiSharedMemoryEnabled, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! this unit test requires to be run on exactly 4 PETs
  if (petCount /= 4) goto 10

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing Array IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_ArrayIsCreated(array)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing Array IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_ArrayIsCreated(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! DistGrid preparation
  allocate(regDecomp(2))
  call ESMF_DistGridRegDecompSetCubic(regDecomp, rc=rc) ! expect 2 x 2
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=regDecomp, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  deallocate(regDecomp)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test Array for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing Array IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_ArrayIsCreated(array)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing Array IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_ArrayIsCreated(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy test Array for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing Array IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_ArrayIsCreated(array)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Testing Array IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_ArrayIsCreated(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Array equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayBool = (arrayAlias.eq.array)
  call ESMF_Test(.not.arrayBool, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Testing ESMF_ArrayAssignment(=)()
  write(name, *) "Array assignment and equality Test"
  write(failMsg, *) "Did not produce alias"
  arrayAlias = array
  arrayBool = (arrayAlias.eq.array)
  call ESMF_Test(arrayBool, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Testing ESMF_ArrayOperator(==)()
  write(name, *) "Array equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayBool = (arrayAlias==array)
  call ESMF_Test(.not.arrayBool, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Testing ESMF_ArrayOperator(/=)()
  write(name, *) "Array non-equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayBool = (arrayAlias/=array)
  call ESMF_Test(arrayBool, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Double ArrayDestroy through alias Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayAlias, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 rank inconsistency Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, undistLBound=(/0/), undistUBound=(/2,2/), &
    name="MyArray", rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySet(array, name="MyArrayNewName", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! ArraySpec preparation
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test w/ ArraySpec"
  write(failMsg, *) "Incorrectly returned ESMF_SUCCESS"
  ! Should not return ESMF_SUCCESS due to / in name
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray w/ ArraySpec", rc=rc)
  call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test with ArraySpec"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray with ArraySpec", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayLog 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayLog(array, prefix="ArrayLog 2D ESMF_TYPEKIND_R8: ", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet arrayspec and name, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, arrayspec=arrayspec2, name=arrayName, rc=rc)
  print *, "Array name: ", arrayname
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify ArraySpec returned from Array"
  write(failMsg, *) "Incorrect ArraySpec"
  call ESMF_Test((arrayspec2==arrayspec), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify name returned from Array"
  write(failMsg, *) "Incorrect name"
  call ESMF_Test((trim(arrayName)=="MyArray with ArraySpec"), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet replicatedDimCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, replicatedDimCount=replicatedDimCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify replicatedDimCount returned from Array with 0 replicated dims"
  write(failMsg, *) "Incorrect replicatedDimCount"
  call ESMF_Test(replicatedDimCount==0, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Getting Attribute count from an Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeGet(array, count, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Attribute count from an Array"
  write(failMsg, *) "Incorrect count"
  call ESMF_Test((count.eq.0), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy, uninitialized Array Test"
  write(failMsg, *) "Incorrectly returned ESMF_SUCCESS"
  arrayDup = ESMF_ArrayCreate(arrayUnInit, rc=rc)
  call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (ALLOC), 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ! In most circumstances it is best to avoid using random_number in unit tests.
  ! In this case farrayPtr2D will be compared to an uninitialized array, which
  ! is already effectively random. Filling farrayPtr2D with random numbers
  ! reduces the chance of a value collision to near zero.
  call random_number(farrayPtr2D) ! fill with data to check
  farrayPtr2D = farrayPtr2D * 1000.0_ESMF_KIND_R8
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_ALLOC, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet arrayspec from Array Copy (ALLOC) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, arrayspec=arrayspec2, name=arrayName, rc=rc)
  print *, "Array name: ", arrayname
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify ArraySpec returned from Array (ALLOC) Copy"
  write(failMsg, *) "Incorrect ArraySpec"
  call ESMF_Test((arrayspec2==arrayspec), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, from Array Copy (ALLOC) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2DCpy, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array vs Array Copy (ALLOC) no data copy"
  write(failMsg, *) "Unexpected data copy"
  dataCorrect = .false.
  do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
  do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
    write (msg,*) "farrayPtr2D(",i,",",j,")=", farrayPtr2D(i,j), &
      "  farrayPtr2DCpy(",i,",",j,")=", farrayPtr2DCpy(i,j)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (abs(farrayPtr2D(i,j)-farrayPtr2DCpy(i,j)) >= 1.d-10) dataCorrect=.true.
  enddo
  enddo
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array vs Array Copy (ALLOC) separate memory allocation"
  write(failMsg, *) "Unexpected reference sharing"
  farrayPtr2D     = real(localPet+10, ESMF_KIND_R8)
  farrayPtr2DCpy  = real(localPet+100, ESMF_KIND_R8)
  dataCorrect = .true.
  do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
  do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
    write (msg,*) "farrayPtr2D(",i,",",j,")=", farrayPtr2D(i,j), &
      "  farrayPtr2DCpy(",i,",",j,")=", farrayPtr2DCpy(i,j)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (abs(farrayPtr2D(i,j)-farrayPtr2DCpy(i,j)) < 1.d-10) dataCorrect=.false.
  enddo
  enddo
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint from Copy (ALLOC) after original destroy, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(arrayDup, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy of Copy (ALLOC) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test with ArraySpec"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray with ArraySpec", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (VALUE), 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farrayPtr2D     = real(localPet+20, ESMF_KIND_R8)  ! fill with data to check
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet isESMFAllocated from Array Copy (VALUE) Test"
  write(failMsg, *) "Did not return .true."
  call ESMF_ArrayGet(arrayDup, isESMFAllocated=isESMFAllocated, rc=rc)
  print *, "Array is allocated internally: ", isESMFAllocated
  call ESMF_Test(isESMFAllocated, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet arrayspec from Array Copy (VALUE) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, arrayspec=arrayspec2, name=arrayName, rc=rc)
  print *, "Array name: ", arrayname
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify ArraySpec returned from Array (VALUE) Copy"
  write(failMsg, *) "Incorrect ArraySpec"
  call ESMF_Test((arrayspec2==arrayspec), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, from Array Copy (VALUE) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2DCpy, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array vs Array Copy (VALUE) data copy"
  write(failMsg, *) "Unexpected data copy"
  dataCorrect = .true.
  do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
  do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
    write (msg,*) "farrayPtr2D(",i,",",j,")=", farrayPtr2D(i,j), &
      "  farrayPtr2DCpy(",i,",",j,")=", farrayPtr2DCpy(i,j)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (abs(farrayPtr2D(i,j)-farrayPtr2DCpy(i,j)) > 1.d-10) dataCorrect=.false.
  enddo
  enddo
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array vs Array Copy (VALUE) separate memory allocation"
  write(failMsg, *) "Unexpected reference sharing"
  farrayPtr2D     = real(localPet+20, ESMF_KIND_R8)
  farrayPtr2DCpy  = real(localPet+200, ESMF_KIND_R8)
  dataCorrect = .true.
  do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
  do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
    write (msg,*) "farrayPtr2D(",i,",",j,")=", farrayPtr2D(i,j), &
      "  farrayPtr2DCpy(",i,",",j,")=", farrayPtr2DCpy(i,j)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (abs(farrayPtr2D(i,j)-farrayPtr2DCpy(i,j)) < 1.d-10) dataCorrect=.false.
  enddo
  enddo
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy of Copy (VALUE) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test with ArraySpec"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray with ArraySpec", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (REF), 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  farrayPtr2D     = real(localPet+30, ESMF_KIND_R8)  ! fill with data to check
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet isESMFAllocated from Array Copy (REF) Test"
  write(failMsg, *) "Did not return .false."
  call ESMF_ArrayGet(arrayDup, isESMFAllocated=isESMFAllocated, rc=rc)
  print *, "Array is allocated internally: ", isESMFAllocated
  call ESMF_Test(.not.isESMFAllocated, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet arrayspec from Array Copy (REF) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, arrayspec=arrayspec2, name=arrayName, rc=rc)
  print *, "Array name: ", arrayname
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify ArraySpec returned from Array Copy (REF)"
  write(failMsg, *) "Incorrect ArraySpec"
  call ESMF_Test((arrayspec2==arrayspec), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, from Array Copy (REF) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2DCpy, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array vs Array Copy (REF) shared allocation Test1"
  write(failMsg, *) "Unexpected separate allocations"
  dataCorrect = .true.
  do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
  do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
    write (msg,*) "farrayPtr2D(",i,",",j,")=", farrayPtr2D(i,j), &
      "  farrayPtr2DCpy(",i,",",j,")=", farrayPtr2DCpy(i,j)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (abs(farrayPtr2D(i,j)-farrayPtr2DCpy(i,j)) > 1.d-10) dataCorrect=.false.
  enddo
  enddo
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify Array vs Array Copy (REF) shared allocation Test2"
  write(failMsg, *) "Unexpected separate allocations"
  farrayPtr2D     = real(localPet+30, ESMF_KIND_R8)
  farrayPtr2DCpy  = real(localPet+300, ESMF_KIND_R8)
  dataCorrect = .true.
  do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
  do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
    write (msg,*) "farrayPtr2D(",i,",",j,")=", farrayPtr2D(i,j), &
      "  farrayPtr2DCpy(",i,",",j,")=", farrayPtr2DCpy(i,j)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (abs(farrayPtr2D(i,j)-farrayPtr2DCpy(i,j)) > 1.d-10) dataCorrect=.false.
  enddo
  enddo
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy of Copy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Ptr with 3D farray on 2D DistGrid Test as Ptr"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(-2:6,12,3:10))
  array = ESMF_ArrayCreate(farrayPtr=farrayPtr3D, distgrid=distgrid, &
    name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint for ArrayCreate from Ptr Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3Dx, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Deallocate returned pointer Test"
  write(failMsg, *) "Did not return success"
  deallocate(farrayPtr3Dx, stat=rc)
  call ESMF_Test((rc.eq.0), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 3D farray on 2D DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(8,12,10))
  array = ESMF_ArrayCreate(farray=farrayPtr3D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(farrayPtr3D)
  nullify(farrayPtr3D)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 3D farray on 2D DistGrid w/ ESMF_DATACOPY_VALUE Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(8,12,10))
  array = ESMF_ArrayCreate(farray=farrayPtr3D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(farrayPtr3D)
  nullify(farrayPtr3D)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 3D farrayPtr on 2D DistGrid w/ ESMF_DATACOPY_VALUE Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(8,12,10))
  array = ESMF_ArrayCreate(farrayPtr=farrayPtr3D, distgrid=distgrid, &
    name="MyArray", datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(farrayPtr3D)
  nullify(farrayPtr3D)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 3D farray on 2D DistGrid w/ distgridToArrayMap Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr3D(12,13,10))
  array = ESMF_ArrayCreate(farray=farrayPtr3D, distgrid=distgrid, &
    distgridToArrayMap=(/2,1/), computationalLWidth=(/0,5/), &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  deallocate(farrayPtr3D)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate with 1D farray on 2D DistGrid all replicated dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(farrayPtr1D(-10:15))
  array = ESMF_ArrayCreate(farrayPtr=farrayPtr1D, distgrid=distgrid, &
    distgridToArrayMap=(/0,0/), name="MyArrayAllReplicated", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet for all replicated dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, rank=rank, dimCount=dimCount, &
    undistDimCount=undistDimCount, replicatedDimCount=replicatedDimCount, &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank==1 for all replicated dims Test"
  write(failMsg, *) "Rank is wrong: ", rank
  call ESMF_Test((rank==1), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate dimCount==2 for all replicated dims Test"
  write(failMsg, *) "dimCount is wrong:", dimCount
  call ESMF_Test((dimCount==2), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate undistDimCount==1 for all replicated dims Test"
  write(failMsg, *) "undistDimCount is wrong: ", undistDimCount
  call ESMF_Test((undistDimCount==1), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate replicatedDimCount==2 for all replicated dims Test"
  write(failMsg, *) "replicatedDimCount is wrong: ", replicatedDimCount
  call ESMF_Test((replicatedDimCount==2), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet undist bounds for all replicated dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(undistLBound(undistDimCount),undistUBound(undistDimCount))
  call ESMF_ArrayGet(array, undistLBound=undistLBound, &
    undistUBound=undistUBound, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate undistLBound==[-10] for all replicated dims Test"
  write(failMsg, *) "undistLBound is wrong: ", undistLBound
  call ESMF_Test(all(undistLBound==[-10]), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate undistUBound==[15] for all replicated dims Test"
  write(failMsg, *) "undistUBound is wrong: ", undistUBound
  call ESMF_Test(all(undistUBound==[15]), name, failMsg, result, ESMF_SRCLINE)

  deallocate(undistLBound,undistUBound)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy for all replicated dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 w/ negative computational widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, computationalLWidth=(/-1,-1/), &
    computationalUWidth=(/-2,-3/), name="MyArray Negative", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 w/ computational widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 w/ computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, computationalEdgeLWidth=(/0,-1/), &
    computationalEdgeUWidth=(/-2,+1/), name="MyArray Negative Edge", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 w/ computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet 2D ESMF_TYPEKIND_R8 w/ computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(totalLWidth(2,1))
  allocate(totalUWidth(2,1))
  allocate(computationalLWidth(2,1))
  allocate(computationalUWidth(2,1))
  call ESMF_ArrayGet(array, totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
    computationalLWidth=computationalLWidth, &
    computationalUWidth=computationalUWidth, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check total widths for 2D ESMF_TYPEKIND_R8 w/ computationalEdge widths Test"
  write(failMsg, *) "Total widths are wrong"
  call ESMF_Test((totalLWidth(1,1)==max(0,computationalLWidth(1,1))&
    .and.totalLWidth(2,1)==max(0,computationalLWidth(2,1))&
    .and.totalUWidth(1,1)==max(0,computationalUWidth(1,1))&
    .and.totalUWidth(2,1)==max(0,computationalUWidth(2,1))), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(totalLWidth, totalUWidth)
  deallocate(computationalLWidth, computationalUWidth)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, computationalEdgeLWidth=(/0,-1/), &
    computationalEdgeUWidth=(/-2,+1/), totalLWidth=(/1,2/), totalUWidth=(/3,4/),&
    name="MyArray Negative Edge", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate(totalLWidth(2,1))
  allocate(totalUWidth(2,1))
  allocate(totalLBound(2,1))
  allocate(totalUBound(2,1))
  call ESMF_ArrayGet(array, totalLWidth=totalLWidth, totalUWidth=totalUWidth, &
    totalLBound=totalLBound, totalUBound=totalUBound, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check total widths for 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Total widths are wrong"
  call ESMF_Test((totalLWidth(1,1)==1.and.totalLWidth(2,1)==2.and.&
    totalUWidth(1,1)==3.and.totalUWidth(2,1)==4), &
    name, failMsg, result, ESMF_SRCLINE)
  deallocate(totalLWidth, totalUWidth)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Check total bounds for 2D ESMF_TYPEKIND_R8 w/ computationalEdge and total widths Test"
  write(failMsg, *) "Total bounds are wrong"
  if (localPet==0) then
    call ESMF_Test((totalLBound(1,1)==0.and.totalLBound(2,1)==-1.and.&
    totalUBound(1,1)==11.and.totalUBound(2,1)==16), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet==1) then
    call ESMF_Test((totalLBound(1,1)==8.and.totalLBound(2,1)==-1.and.&
    totalUBound(1,1)==18.and.totalUBound(2,1)==16), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet==2) then
    call ESMF_Test((totalLBound(1,1)==0.and.totalLBound(2,1)==11.and.&
    totalUBound(1,1)==11.and.totalUBound(2,1)==27), &
      name, failMsg, result, ESMF_SRCLINE)
  else if (localPet==3) then
    call ESMF_Test((totalLBound(1,1)==8.and.totalLBound(2,1)==11.and.&
    totalUBound(1,1)==18.and.totalUBound(2,1)==27), &
      name, failMsg, result, ESMF_SRCLINE)
  endif
  deallocate(totalLBound, totalUBound)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test Array with ESMF_PIN_DE_TO_PET"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, pinflag=ESMF_PIN_DE_TO_PET, name="MyArray", &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_PET Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
  call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
  call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for array with ESMF_PIN_DE_TO_PET"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test Array with ESMF_PIN_DE_TO_VAS"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, pinflag=ESMF_PIN_DE_TO_VAS, name="MyArray", &
    rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for array with ESMF_PIN_DE_TO_VAS"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test Array with ESMF_PIN_DE_TO_SSI"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, pinflag=ESMF_PIN_DE_TO_SSI, name="MyArray", &
    rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMC_RC_INTNRL_BAD), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet ssiLocalDeCount ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, ssiLocalDeCount=ssiLocalDeCount, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  ! initialize the data on this PETs first localDE
  if (ssiSharedMemoryEnabled) then
    do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
    do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
      farrayPtr2D(i,j) = real(localDeToDeMap(1)+5,ESMF_KIND_R8) &
        * sin(real(i,ESMF_KIND_R8)) &
        * sin(real(j,ESMF_KIND_R8))
    enddo
    enddo
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(array, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "LocalArrayGet Fortran array pointer for next ssiLocalDe for ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  next = localPet + 2
  if (next > ssiLocalDeCount) next = 1
  call ESMF_LocalArrayGet(localArrayList(next), &
    farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap(next)=", localDeToDeMap(next)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for next ssiLocalDe for ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
    do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
      write (msg,*) "data(",i,",",j,")=", farrayPtr2D(i,j)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      diffR8 = farrayPtr2D(i,j) - &
        real(localDeToDeMap(next)+5,ESMF_KIND_R8) &
        * sin(real(i,ESMF_KIND_R8)) &
        * sin(real(j,ESMF_KIND_R8))
      if (abs(diffR8) > 1.d-10) then
        dataCorrect=.false.
        write (msg,*) "diffR8=", diffR8
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(array, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (REF), ESMF_PIN_DE_TO_SSI Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_REFERENCE, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet ssiLocalDeCount ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, ssiLocalDeCount=ssiLocalDeCount, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  ! initialize the data on this PETs first localDE
  if (ssiSharedMemoryEnabled) then
    do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
    do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
      farrayPtr2D(i,j) = real(10*(localDeToDeMap(1)+5),ESMF_KIND_R8) &
        * sin(real(i,ESMF_KIND_R8)) &
        * sin(real(j,ESMF_KIND_R8))
    enddo
    enddo
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "LocalArrayGet Fortran array pointer for next ssiLocalDe for ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  next = localPet + 2
  if (next > ssiLocalDeCount) next = 1
  call ESMF_LocalArrayGet(localArrayList(next), &
    farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap(next)=", localDeToDeMap(next)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for next ssiLocalDe for ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
    do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
      write (msg,*) "data(",i,",",j,")=", farrayPtr2D(i,j)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      diffR8 = farrayPtr2D(i,j) - &
        real(10*(localDeToDeMap(next)+5),ESMF_KIND_R8) &
        * sin(real(i,ESMF_KIND_R8)) &
        * sin(real(j,ESMF_KIND_R8))
      if (abs(diffR8) > 1.d-10) then
        dataCorrect=.false.
        write (msg,*) "diffR8=", diffR8
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(arrayDup, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for arrayDup with ESMF_PIN_DE_TO_SSI"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (REF), ESMF_PIN_DE_TO_SSI w/ DELayout Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  delayout = ESMF_DELayoutCreate((/0,0,2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_REFERENCE, &
    delayout=delayout, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint from Copy (REF), ESMF_PIN_DE_TO_SSI w/ DELayout Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(arrayDup, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet ssiLocalDeCount ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, ssiLocalDeCount=ssiLocalDeCount, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for all DEs for ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    do lde=1, ssiLocalDeCount
      call ESMF_LocalArrayGet(localArrayList(lde), farrayPtr=farrayPtr2D, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
      do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
        write (msg,*) "localDE=",lde-1," DE=", localDeToDeMap(lde), &
          " data(",i,",",j,")=", farrayPtr2D(i,j)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        diffR8 = farrayPtr2D(i,j) - &
          real(10*(localDeToDeMap(lde)+5),ESMF_KIND_R8) &
          * sin(real(i,ESMF_KIND_R8)) &
          * sin(real(j,ESMF_KIND_R8))
        if (abs(diffR8) > 1.d-10) then
          dataCorrect=.false.
          write (msg,*) "diffR8=", diffR8
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo
      enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(arrayDup, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI arrayDup Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for arrayDup with ESMF_PIN_DE_TO_SSI w/ DELayout"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for array with ESMF_PIN_DE_TO_SSI"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test 2D+1 Array with ESMF_PIN_DE_TO_SSI"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, pinflag=ESMF_PIN_DE_TO_SSI, name="MyArray", &
    undistLBound=[1], undistUBound=[3], rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMC_RC_INTNRL_BAD), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3DR8, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr3DR8)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr3DR8)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet rank, ssiLocalDeCount ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, rank=rank, ssiLocalDeCount=ssiLocalDeCount, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    rank=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))
  allocate(arrayToDistGridMap(rank))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank for ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Rank is wrong"
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rank==3), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "rank=", rank
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! dummy test call
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, arrayToDistGridMap=arrayToDistGridMap, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "arrayToDistGridMap=", arrayToDistGridMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  ! initialize the data on this PETs first localDE
  if (ssiSharedMemoryEnabled) then
    do k=lbound(farrayPtr3DR8,3), ubound(farrayPtr3DR8,3)
    do j=lbound(farrayPtr3DR8,2), ubound(farrayPtr3DR8,2)
    do i=lbound(farrayPtr3DR8,1), ubound(farrayPtr3DR8,1)
      farrayPtr3DR8(i,j,k) = real(localDeToDeMap(1)+5,ESMF_KIND_R8) &
        * sin(real(i,ESMF_KIND_R8)) &
        * sin(real(j,ESMF_KIND_R8)) &
        * sin(real(k,ESMF_KIND_R8))
    enddo
    enddo
    enddo
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(array, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "LocalArrayGet Fortran array pointer for next ssiLocalDe "//&
    "for ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  next = localPet + 2
  if (next > ssiLocalDeCount) next = 1
  call ESMF_LocalArrayGet(localArrayList(next), &
    farrayPtr=farrayPtr3DR8, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap(next)=", localDeToDeMap(next)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr3DR8)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr3DR8)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for next ssiLocalDe for "//&
    "ESMF_PIN_DE_TO_SSI 2D+1 Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    do k=lbound(farrayPtr3DR8,3), ubound(farrayPtr3DR8,3)
    do j=lbound(farrayPtr3DR8,2), ubound(farrayPtr3DR8,2)
    do i=lbound(farrayPtr3DR8,1), ubound(farrayPtr3DR8,1)
      write (msg,*) "data(",i,",",j,",",k,")=", farrayPtr3DR8(i,j,k)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      diffR8 = farrayPtr3DR8(i,j,k) - &
        real(localDeToDeMap(next)+5,ESMF_KIND_R8) &
        * sin(real(i,ESMF_KIND_R8)) &
        * sin(real(j,ESMF_KIND_R8)) &
        * sin(real(k,ESMF_KIND_R8))
      if (abs(diffR8) > 1.d-10) then
        dataCorrect=.false.
        write (msg,*) "diffR8=", diffR8
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
    enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(array, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)
  deallocate(arrayToDistGridMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (REF), ESMF_PIN_DE_TO_SSI "//&
    "2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_REFERENCE, &
    trailingUndistSlice=[1], rc=rc) ! create a slice
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI "// &
    "arrayDup 2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet rank, ssiLocalDeCount ESMF_PIN_DE_TO_SSI "//&
    "arrayDup 2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, rank=rank, ssiLocalDeCount=ssiLocalDeCount, &
    rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    rank=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))
  allocate(arrayToDistGridMap(rank))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank for ESMF_PIN_DE_TO_SSI arrayDup 2D+1->2D "//&
    "Slice at k=1 Test"
  write(failMsg, *) "Rank is wrong"
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rank==2), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "rank=", rank
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! dummy test call
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, arrayToDistGridMap=arrayToDistGridMap, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "arrayToDistGridMap=", arrayToDistGridMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for all DEs for "//&
    "ESMF_PIN_DE_TO_SSI arrayDup 2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    k=1
    do lde=1, ssiLocalDeCount
      call ESMF_LocalArrayGet(localArrayList(lde), farrayPtr=farrayPtr2D, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
      do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
        write (msg,*) "localDE=",lde-1," DE=", localDeToDeMap(lde), &
          " data(",i,",",j,")=", farrayPtr2D(i,j)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        diffR8 = farrayPtr2D(i,j) - &
          real(localDeToDeMap(lde)+5,ESMF_KIND_R8) &
          * sin(real(i,ESMF_KIND_R8)) &
          * sin(real(j,ESMF_KIND_R8)) &
          * sin(real(k,ESMF_KIND_R8))
        if (abs(diffR8) > 1.d-10) then
          dataCorrect=.false.
          write (msg,*) "diffR8=", diffR8
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo
      enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(arrayDup, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)
  deallocate(arrayToDistGridMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for arrayDup with ESMF_PIN_DE_TO_SSI "//&
    "2D+1->2D Slice at k=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (REF), ESMF_PIN_DE_TO_SSI "//&
    "2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_REFERENCE, &
    trailingUndistSlice=[2], rc=rc) ! create a slice
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI "// &
    "arrayDup 2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet rank, ssiLocalDeCount ESMF_PIN_DE_TO_SSI "//&
    "arrayDup 2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, rank=rank, ssiLocalDeCount=ssiLocalDeCount, &
    rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    rank=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))
  allocate(arrayToDistGridMap(rank))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank for ESMF_PIN_DE_TO_SSI arrayDup 2D+1->2D "//&
    "Slice at k=2 Test"
  write(failMsg, *) "Rank is wrong"
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rank==2), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "rank=", rank
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! dummy test call
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, arrayToDistGridMap=arrayToDistGridMap, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "arrayToDistGridMap=", arrayToDistGridMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for all DEs for "//&
    "ESMF_PIN_DE_TO_SSI arrayDup 2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    k=2
    do lde=1, ssiLocalDeCount
      call ESMF_LocalArrayGet(localArrayList(lde), farrayPtr=farrayPtr2D, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
      do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
        write (msg,*) "localDE=",lde-1," DE=", localDeToDeMap(lde), &
          " data(",i,",",j,")=", farrayPtr2D(i,j)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        diffR8 = farrayPtr2D(i,j) - &
          real(localDeToDeMap(lde)+5,ESMF_KIND_R8) &
          * sin(real(i,ESMF_KIND_R8)) &
          * sin(real(j,ESMF_KIND_R8)) &
          * sin(real(k,ESMF_KIND_R8))
        if (abs(diffR8) > 1.d-10) then
          dataCorrect=.false.
          write (msg,*) "diffR8=", diffR8
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo
      enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(arrayDup, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)
  deallocate(arrayToDistGridMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for arrayDup with ESMF_PIN_DE_TO_SSI "//&
    "2D+1->2D Slice at k=2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (value), ESMF_PIN_DE_TO_SSI "//&
    "2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_VALUE, &
    trailingUndistSlice=[3], rc=rc) ! create a slice
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI "// &
    "arrayDup 2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr2D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr2D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet rank, ssiLocalDeCount ESMF_PIN_DE_TO_SSI "//&
    "arrayDup 2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, rank=rank, ssiLocalDeCount=ssiLocalDeCount, &
    rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    rank=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))
  allocate(arrayToDistGridMap(rank))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank for ESMF_PIN_DE_TO_SSI arrayDup 2D+1->2D "//&
    "Slice at k=3 Test"
  write(failMsg, *) "Rank is wrong"
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rank==2), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "rank=", rank
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! dummy test call
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, arrayToDistGridMap=arrayToDistGridMap, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "arrayToDistGridMap=", arrayToDistGridMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for all DEs for "//&
    "ESMF_PIN_DE_TO_SSI arrayDup 2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    k=3
    do lde=1, ssiLocalDeCount
      call ESMF_LocalArrayGet(localArrayList(lde), farrayPtr=farrayPtr2D, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do j=lbound(farrayPtr2D,2), ubound(farrayPtr2D,2)
      do i=lbound(farrayPtr2D,1), ubound(farrayPtr2D,1)
        write (msg,*) "localDE=",lde-1," DE=", localDeToDeMap(lde), &
          " data(",i,",",j,")=", farrayPtr2D(i,j)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        diffR8 = farrayPtr2D(i,j) - &
          real(localDeToDeMap(lde)+5,ESMF_KIND_R8) &
          * sin(real(i,ESMF_KIND_R8)) &
          * sin(real(j,ESMF_KIND_R8)) &
          * sin(real(k,ESMF_KIND_R8))
        if (abs(diffR8) > 1.d-10) then
          dataCorrect=.false.
          write (msg,*) "diffR8=", diffR8
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo
      enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(arrayDup, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)
  deallocate(arrayToDistGridMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for arrayDup with ESMF_PIN_DE_TO_SSI "//&
    "2D+1->2D Slice at k=3 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for array with ESMF_PIN_DE_TO_SSI "//&
    "2D+1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test 2D+2 Array with ESMF_PIN_DE_TO_SSI"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R4, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, pinflag=ESMF_PIN_DE_TO_SSI, name="MyArray", &
    undistLBound=[1,1], undistUBound=[3,4], rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMC_RC_INTNRL_BAD), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr4DR4, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr4DR4)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr4DR4)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet rank, ssiLocalDeCount ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, rank=rank, ssiLocalDeCount=ssiLocalDeCount, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    rank=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))
  allocate(arrayToDistGridMap(rank))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank for ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Rank is wrong"
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rank==4), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "rank=", rank
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! dummy test call
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, arrayToDistGridMap=arrayToDistGridMap, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "arrayToDistGridMap=", arrayToDistGridMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  ! initialize the data on this PETs first localDE
  if (ssiSharedMemoryEnabled) then
    do l=lbound(farrayPtr4DR4,4), ubound(farrayPtr4DR4,4)
    do k=lbound(farrayPtr4DR4,3), ubound(farrayPtr4DR4,3)
    do j=lbound(farrayPtr4DR4,2), ubound(farrayPtr4DR4,2)
    do i=lbound(farrayPtr4DR4,1), ubound(farrayPtr4DR4,1)
      farrayPtr4DR4(i,j,k,l) = real(localDeToDeMap(1)+5,ESMF_KIND_R4) &
        * sin(real(i,ESMF_KIND_R4)) &
        * sin(real(j,ESMF_KIND_R4)) &
        * sin(real(k,ESMF_KIND_R4)) &
        * sin(real(l,ESMF_KIND_R4))
    enddo
    enddo
    enddo
    enddo
  endif
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(array, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "LocalArrayGet Fortran array pointer for next ssiLocalDe "//&
    "for ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  next = localPet + 2
  if (next > ssiLocalDeCount) next = 1
  call ESMF_LocalArrayGet(localArrayList(next), &
    farrayPtr=farrayPtr4DR4, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap(next)=", localDeToDeMap(next)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr4DR4)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr4DR4)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for next ssiLocalDe for "//&
    "ESMF_PIN_DE_TO_SSI 2D+2 Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    do l=lbound(farrayPtr4DR4,4), ubound(farrayPtr4DR4,4)
    do k=lbound(farrayPtr4DR4,3), ubound(farrayPtr4DR4,3)
    do j=lbound(farrayPtr4DR4,2), ubound(farrayPtr4DR4,2)
    do i=lbound(farrayPtr4DR4,1), ubound(farrayPtr4DR4,1)
      write (msg,*) "data(",i,",",j,",",k,",",l,")=", farrayPtr4DR4(i,j,k,l)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      diffR4 = farrayPtr4DR4(i,j,k,l) - &
        real(localDeToDeMap(next)+5,ESMF_KIND_R4) &
        * sin(real(i,ESMF_KIND_R4)) &
        * sin(real(j,ESMF_KIND_R4)) &
        * sin(real(k,ESMF_KIND_R4)) &
        * sin(real(l,ESMF_KIND_R4))
      if (abs(diffR4) > 1.d-6) then
        dataCorrect=.false.
        write (msg,*) "diffR4=", diffR4
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
    enddo
    enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(array, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)
  deallocate(arrayToDistGridMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate from Copy (REF), ESMF_PIN_DE_TO_SSI "//&
    "2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arrayDup = ESMF_ArrayCreate(array, datacopyflag=ESMF_DATACOPY_REFERENCE, &
    trailingUndistSlice=[1], rc=rc) ! create a slice
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer for ESMF_PIN_DE_TO_SSI "// &
    "arrayDup 2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, farrayPtr=farrayPtr3D, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "Local Array lbounds=", lbound(farrayPtr3D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "Local Array ubounds=", ubound(farrayPtr3D)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet rank, ssiLocalDeCount ESMF_PIN_DE_TO_SSI "//&
    "arrayDup 2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, rank=rank, ssiLocalDeCount=ssiLocalDeCount, &
    rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "ssiLocalDeCount=", ssiLocalDeCount
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ssiLocalDeCount=1
    rank=1
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif
  allocate(localDeToDeMap(ssiLocalDeCount))
  allocate(localArrayList(ssiLocalDeCount))
  allocate(arrayToDistGridMap(rank))

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate rank for ESMF_PIN_DE_TO_SSI arrayDup 2D+2->2D+1 "//&
    "Slice at l=1 Test"
  write(failMsg, *) "Rank is wrong"
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rank==3), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "rank=", rank
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! dummy test call
    call ESMF_Test((.true.), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet localDeToDeMap, etc. ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(arrayDup, localDeToDeMap=localDeToDeMap, &
    localarrayList=localArrayList, arrayToDistGridMap=arrayToDistGridMap, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    write (msg,*) "localDeToDeMap=", localDeToDeMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    write (msg,*) "arrayToDistGridMap=", arrayToDistGridMap
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Validate data in LocalArray for all DEs for "//&
    "ESMF_PIN_DE_TO_SSI arrayDup 2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Data not correct"
  dataCorrect = .true.  ! initialize
  if (ssiSharedMemoryEnabled) then
    l=1
    do lde=1, ssiLocalDeCount
      call ESMF_LocalArrayGet(localArrayList(lde), farrayPtr=farrayPtr3D, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do k=lbound(farrayPtr3D,3), ubound(farrayPtr3D,3)
      do j=lbound(farrayPtr3D,2), ubound(farrayPtr3D,2)
      do i=lbound(farrayPtr3D,1), ubound(farrayPtr3D,1)
        write (msg,*) "localDE=",lde-1," DE=", localDeToDeMap(lde), &
          " data(",i,",",j,",",k,")=", farrayPtr3D(i,j,k)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        diffR4 = farrayPtr3D(i,j,k) - &
          real(localDeToDeMap(lde)+5,ESMF_KIND_R4) &
          * sin(real(i,ESMF_KIND_R4)) &
          * sin(real(j,ESMF_KIND_R4)) &
          * sin(real(k,ESMF_KIND_R4)) &
          * sin(real(l,ESMF_KIND_R4))
        if (abs(diffR4) > 1.d-6) then
          dataCorrect=.false.
          write (msg,*) "diffR4=", diffR4
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      enddo
      enddo
      enddo
    enddo
  else
    ! dummy test
  endif
  call ESMF_Test((dataCorrect), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArraySync(arrayDup, rc=rc) ! prevent race condition with below
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  deallocate(localDeToDeMap)
  deallocate(localArrayList)
  deallocate(arrayToDistGridMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArraySync() for ESMF_PIN_DE_TO_SSI arrayDup "//&
    "2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySync(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for arrayDup with ESMF_PIN_DE_TO_SSI "//&
    "2D+2->2D+1 Slice at l=1 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(arrayDup, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test for array with ESMF_PIN_DE_TO_SSI "//&
    "2D+2 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  if (ssiSharedMemoryEnabled) then
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  else
    write(failMsg, *) "Did not return the correct RC"
    call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)
  endif

  !------------------------------------------------------------------------
  ! cleanup
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/40/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray1D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr1D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet w/ incompatible Fortran array pointer, 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint AssmdShape 1D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 1D ESMF_TYPEKIND_R8 w/ negative computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray1D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, computationalEdgeLWidth=(/-1/), &
    computationalEdgeUWidth=(/-1/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint AssmdShape 1D ESMF_TYPEKIND_R8 w/ negative computationalEdge widths Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/40,10/), &
    regDecomp=(/petCount,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray2D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet w/ incompatible Fortran array pointer, 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3D, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/40,10,10/), &
    regDecomp=(/petCount,1,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 3D ESMF_TYPEKIND_R4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray3D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 3D ESMF_TYPEKIND_R4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr3D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! preparations
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1,1,1/), &
    maxIndex=(/40,10,10,10/), regDecomp=(/petCount,1,1,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate AssmdShape 4D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farray4D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer, 4D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr4D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D with 4D DistGrid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    ! Dimensions 1 and 3 are replicated dimensions
    distgridToArrayMap = [0,1,0,2], &
    indexflag=ESMF_INDEX_GLOBAL, name="2D Array with 4D DistGrid", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet replicatedDimCount Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, replicatedDimCount=replicatedDimCount, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Verify replicatedDimCount returned from Array with 2 replicated dims"
  write(failMsg, *) "Incorrect replicatedDimCount"
  call ESMF_Test(replicatedDimCount==2, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayGet Fortran array pointer from Array with 2 replicated dims Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array, farrayPtr=farrayPtr2D, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! prepare a 2D DistGrid
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridGet(distgrid, deCount=deCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  allocate(minIndexPDe(2,0:deCount-1), maxIndexPDe(2,0:deCount-1))

  call ESMF_DistGridGet(distgrid, minIndexPDe=minIndexPDe, &
    maxIndexPDe=maxIndexPDe, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do de=0, deCount-1
    write (msg,*) "DistGrid DE=",de," minIndexPDe=", minIndexPDe(:,de), &
      " maxIndexPDe=", maxIndexPDe(:,de)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

  deallocate(minIndexPDe, maxIndexPDe)

  !------------------------------------------------------------------------
  ! prepare a 2D DistGrid with with extra edge elements
  distgrid = ESMF_DistGridCreate(distgrid, &
    firstExtra=(/1,2/), lastExtra=(/3,4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_DistGridGet(distgrid, deCount=deCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  allocate(minIndexPDe(2,0:deCount-1), maxIndexPDe(2,0:deCount-1))

  call ESMF_DistGridGet(distgrid, minIndexPDe=minIndexPDe, &
    maxIndexPDe=maxIndexPDe, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do de=0, deCount-1
    write (msg,*) "DistGrid DE=",de," minIndexPDe=", minIndexPDe(:,de), &
      " maxIndexPDe=", maxIndexPDe(:,de)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

  deallocate(minIndexPDe, maxIndexPDe)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create test Array for extra edge element test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  allocate(exclusiveLBound(2,0:localDeCount-1))
  allocate(exclusiveUBound(2,0:localDeCount-1))
  allocate(localDeToDeMap(0:localDeCount-1))

  call ESMF_ArrayGet(array, exclusiveLBound=exclusiveLBound, &
    exclusiveUBound=exclusiveUBound, localDeToDeMap=localDeToDeMap, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do lde=0, localDeCount-1
    de = localDeToDeMap(lde)
    write (msg,*) "Array DE=",de," exclusiveLBound=", exclusiveLBound(:,lde), &
      " exclusiveUBound=", exclusiveUBound(:,lde)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo

  deallocate(exclusiveLBound, exclusiveUBound)
  deallocate(localDeToDeMap)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! prepare a 1D DistGrid with only a single DE, mapped to PET 0 by default
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/4/), &
    regDecomp=(/1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create Array on single DE DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayPrint Array on single DE DistGrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayPrint(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! prepare Fortran allocations on each PET to match DistGrid
  if (localPet==0) then
    allocate(farrayPtr1D(4))
  else
    allocate(farrayPtr1D(0))
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create Array on single DE DistGrid with Fortran allocation"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farrayPtr1D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  deallocate(farrayPtr1D)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! prepare a 1D DistGrid with 4 DE, but only DE 0 holds data
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), &
    regDecomp=(/4/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  ! prepare Fortran allocations on each PET to match DistGrid
  if (localPet==0) then
    allocate(farrayPtr1D(1))
  else
    allocate(farrayPtr1D(0))
  endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create Array on 4 DE DistGrid with only DE 0 elements, with Fortran allocation"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array = ESMF_ArrayCreate(farray=farrayPtr1D, distgrid=distgrid, &
    indexflag=ESMF_INDEX_DELOCAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! cleanup
  deallocate(farrayPtr1D)
  call ESMF_DistGridDestroy(distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ArrayCreateGetUTest
