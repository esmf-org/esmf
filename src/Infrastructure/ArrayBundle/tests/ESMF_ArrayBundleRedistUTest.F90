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
  allocate(srcArrayList(15))
  ! - 1
  srcArrayList(1) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(1), scale=1., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 2
  srcArrayList(2) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(2), scale=2., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 3
  srcArrayList(3) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(3), scale=3., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 4
  srcArrayList(4) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, &
    distgridToArrayMap=(/2,3/), undistLBound=(/1/), undistUBound=(/10/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(4), scale=4., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 5
  srcArrayList(5) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, &
    distgridToArrayMap=(/1,3/), undistLBound=(/1/), undistUBound=(/10/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(5), scale=5., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 6
  srcArrayList(6) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, &
    distgridToArrayMap=(/1,2/), undistLBound=(/1/), undistUBound=(/10/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(6), scale=6., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 7
  srcArrayList(7) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, &
    distgridToArrayMap=(/2,4/), undistLBound=(/1,1/), undistUBound=(/3,4/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(7), scale=7., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 8
  srcDG = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/30,50,23/), &
    regDecomp=(/1,petCount,1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  srcArrayList(8) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(8), scale=8., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 9
  srcArrayList(9) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, &
    distgridToArrayMap=(/2,1,3/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(9), scale=9., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 10
  srcArrayList(10) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, &
    distgridToArrayMap=(/1,2,4/), undistLBound=(/1/), undistUBound=(/1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(10), scale=10., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 11
  srcArrayList(11) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R8, &
    totalLWidth=(/0,3,1/), totalUWidth=(/1,0,5/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(11), scale=11., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 12
  srcDG = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/500/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  srcArrayList(12) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(12), scale=12., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 13
  srcArrayList(13) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, &
    distgridToArrayMap=(/2/), undistLBound=(/1,1/), undistUBound=(/2,4/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(13), scale=13., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 14
  srcArrayList(14) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, &
    totalLWidth=(/3/), totalUWidth=(/5/), &
    distgridToArrayMap=(/2/), undistLBound=(/1,1/), undistUBound=(/1,1/), &
    rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(14), scale=14., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 15
  srcArrayList(15) = ESMF_ArrayCreate(srcDG, ESMF_TYPEKIND_R4, &
    totalLWidth=(/3/), totalUWidth=(/5/), &
    distgridToArrayMap=(/2/), undistLBound=(/1,1/), undistUBound=(/1,4/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call fillArray(srcArrayList(15), scale=15., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  allocate(checkArrayList(size(srcArrayList)))
  do i=1, size(checkArrayList)
    checkArrayList(i) = ESMF_ArrayCreate(srcArrayList(i), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ArrayCopy(checkArrayList(i), srcArrayList(i), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  !------------------------------------------------------------------------
  allocate(dstArrayList(15))
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
  ! - 4
  dstArrayList(4) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, &
    undistLBound=(/1/), undistUBound=(/10/), distgridToArrayMap=(/2,3/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 5
  dstArrayList(5) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R4, &
    undistLBound=(/1/), undistUBound=(/10/), distgridToArrayMap=(/2,3/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 6
  dstArrayList(6) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, &
    undistLBound=(/1/), undistUBound=(/10/), distgridToArrayMap=(/1,2/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 7
  dstArrayList(7) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, &
    undistLBound=(/1/), undistUBound=(/12/), distgridToArrayMap=(/1,3/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 8
  dstArrayList(8) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 9
  dstArrayList(9) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 10
  dstArrayList(10) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 11
  dstArrayList(11) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 12
  dstDG = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/500/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  dstArrayList(12) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 13
  dstArrayList(13) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R4, &
    distgridToArrayMap=(/2/), undistLBound=(/1,1/), undistUBound=(/2,4/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 14
  dstArrayList(14) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R4, &
    undistLBound=(/1,1/), undistUBound=(/1,1/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  ! - 15
  dstArrayList(15) = ESMF_ArrayCreate(dstDG, ESMF_TYPEKIND_R4, &
    undistLBound=(/1,1/), undistUBound=(/1,4/), rc=rc)
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

  !------------------------------------------------------------------------
  ! scramble the data in the src arrays
  do i=1, size(srcArrayList)
    call fillArray(srcArrayList(i), scale=-99., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
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
  ! scramble the data in the dst arrays
  do i=1, size(dstArrayList)
    call fillArray(dstArrayList(i), scale=-99., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  enddo
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "ArrayBundleRedist src->dst after reading Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleRedist(srcAB, dstAB, routehandle=rh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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
  
 !-------------------------------------------------------------------------
 contains
 !-------------------------------------------------------------------------
 
  subroutine fillArray(array, scale, rc)
    type(ESMF_Array)    :: array
    real                :: scale
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R8)  :: scaleR8
    real(ESMF_KIND_R4)  :: scaleR4
    type(ESMF_TypeKind_Flag) :: tk
    call ESMF_ArrayGet(array, typekind=tk, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (tk==ESMF_TYPEKIND_R8) then
      scaleR8 = real(scale,ESMF_KIND_R8)
      call fillArrayR8(array, scaleR8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    else
      scaleR4 = real(scale,ESMF_KIND_R4)
      call fillArrayR4(array, scaleR4, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif
  end subroutine

  subroutine fillArrayR8(array, scale, rc)
    type(ESMF_Array)    :: array
    real(ESMF_KIND_R8)  :: scale
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R8), pointer   :: fptr1d(:)
    real(ESMF_KIND_R8), pointer   :: fptr2d(:,:)
    real(ESMF_KIND_R8), pointer   :: fptr3d(:,:,:)
    real(ESMF_KIND_R8), pointer   :: fptr4d(:,:,:,:)
    integer                       :: rank, i, j, k, l
    call ESMF_ArrayGet(array, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (rank==1) then
      call ESMF_ArrayGet(array, farrayPtr=fptr1d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=lbound(fptr1d,1),ubound(fptr1d,1)
        fptr1d(i) = scale*sin(real(i,ESMF_KIND_R8))
      enddo
    else if(rank==2) then
      call ESMF_ArrayGet(array, farrayPtr=fptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do j=lbound(fptr2d,2),ubound(fptr2d,2)
      do i=lbound(fptr2d,1),ubound(fptr2d,1)
        fptr2d(i,j) = scale * ( &
          sin(real(i,ESMF_KIND_R8)) + &
          cos(real(j,ESMF_KIND_R8)))
      enddo
      enddo
    else if(rank==3) then
      call ESMF_ArrayGet(array, farrayPtr=fptr3d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do k=lbound(fptr3d,3),ubound(fptr3d,3)
      do j=lbound(fptr3d,2),ubound(fptr3d,2)
      do i=lbound(fptr3d,1),ubound(fptr3d,1)
        fptr3d(i,j,k) = scale * ( &
          sin(real(i,ESMF_KIND_R8)) + &
          cos(real(j,ESMF_KIND_R8)) + &
          sin(2.d0*real(k,ESMF_KIND_R8)))
      enddo
      enddo
      enddo
    else if(rank==4) then
      call ESMF_ArrayGet(array, farrayPtr=fptr4d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do l=lbound(fptr4d,4),ubound(fptr4d,4)
      do k=lbound(fptr4d,3),ubound(fptr4d,3)
      do j=lbound(fptr4d,2),ubound(fptr4d,2)
      do i=lbound(fptr4d,1),ubound(fptr4d,1)
        fptr4d(i,j,k,l) = scale * ( &
          sin(real(i,ESMF_KIND_R8)) + &
          cos(real(j,ESMF_KIND_R8)) + &
          sin(2.d0*real(k,ESMF_KIND_R8)) + &
          cos(2.d0*real(l,ESMF_KIND_R8)))
      enddo
      enddo
      enddo
      enddo
    endif
  end subroutine

  subroutine fillArrayR4(array, scale, rc)
    type(ESMF_Array)    :: array
    real(ESMF_KIND_R4)  :: scale
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R4), pointer   :: fptr1d(:)
    real(ESMF_KIND_R4), pointer   :: fptr2d(:,:)
    real(ESMF_KIND_R4), pointer   :: fptr3d(:,:,:)
    real(ESMF_KIND_R4), pointer   :: fptr4d(:,:,:,:)
    integer                       :: rank, i, j, k, l
    call ESMF_ArrayGet(array, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (rank==1) then
      call ESMF_ArrayGet(array, farrayPtr=fptr1d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=lbound(fptr1d,1),ubound(fptr1d,1)
        fptr1d(i) = scale*sin(real(i,ESMF_KIND_R4))
      enddo
    else if(rank==2) then
      call ESMF_ArrayGet(array, farrayPtr=fptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do j=lbound(fptr2d,2),ubound(fptr2d,2)
      do i=lbound(fptr2d,1),ubound(fptr2d,1)
        fptr2d(i,j) = scale * ( &
          sin(real(i,ESMF_KIND_R4)) + &
          cos(real(j,ESMF_KIND_R4)))
      enddo
      enddo
    else if(rank==3) then
      call ESMF_ArrayGet(array, farrayPtr=fptr3d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do k=lbound(fptr3d,3),ubound(fptr3d,3)
      do j=lbound(fptr3d,2),ubound(fptr3d,2)
      do i=lbound(fptr3d,1),ubound(fptr3d,1)
        fptr3d(i,j,k) = scale * ( &
          sin(real(i,ESMF_KIND_R4)) + &
          cos(real(j,ESMF_KIND_R4)) + &
          sin(2.e0*real(k,ESMF_KIND_R4)))
      enddo
      enddo
      enddo
    else if(rank==4) then
      call ESMF_ArrayGet(array, farrayPtr=fptr4d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do l=lbound(fptr4d,4),ubound(fptr4d,4)
      do k=lbound(fptr4d,3),ubound(fptr4d,3)
      do j=lbound(fptr4d,2),ubound(fptr4d,2)
      do i=lbound(fptr4d,1),ubound(fptr4d,1)
        fptr4d(i,j,k,l) = scale * ( &
          sin(real(i,ESMF_KIND_R4)) + &
          cos(real(j,ESMF_KIND_R4)) + &
          sin(2.e0*real(k,ESMF_KIND_R4)) + &
          cos(2.e0*real(l,ESMF_KIND_R4)))
      enddo
      enddo
      enddo
      enddo
    endif
  end subroutine

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

  function dataMatchArrays(array1, array2, rc)
    logical             :: dataMatchArrays
    type(ESMF_Array)    :: array1
    type(ESMF_Array)    :: array2
    integer             :: rc
    !-----------------------------------------
    type(ESMF_TypeKind_Flag) :: tk
    dataMatchArrays = .true.
    call ESMF_ArrayGet(array1, typekind=tk, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
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
    real(ESMF_KIND_R8)            :: diff
    real(ESMF_KIND_R8), pointer   :: fptr1d1(:), fptr1d2(:)
    real(ESMF_KIND_R8), pointer   :: fptr2d1(:,:), fptr2d2(:,:)
    real(ESMF_KIND_R8), pointer   :: fptr3d1(:,:,:), fptr3d2(:,:,:)
    real(ESMF_KIND_R8), pointer   :: fptr4d1(:,:,:,:), fptr4d2(:,:,:,:)
    integer                       :: rank, dimCount
    integer                       :: i, j, k, l
    integer, allocatable          :: arrayToDistGridMap(:)
    integer, allocatable          :: distgridToPackedArrayMap(:)
    integer, allocatable          :: exclusiveLBound(:,:), exclusiveUBound(:,:)
    integer                       :: lb(4), ub(4)
    character(len=160)            :: msg
    dataMatchR8 = .true.  ! initialize
    call ESMF_ArrayGet(array1, rank=rank, dimCount=dimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    allocate(arrayToDistGridMap(rank))
    allocate(distgridToPackedArrayMap(dimCount))
    allocate(exclusiveLBound(dimCount,1),exclusiveUBound(dimCount,1))
    call ESMF_ArrayGet(array1, arrayToDistGridMap=arrayToDistGridMap, &
      distgridToPackedArrayMap=distgridToPackedArrayMap, &
      exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (rank == 1) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr1d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr1d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr1d1,i)
          ub(i) = ubound(fptr1d1,i)
        else
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do i=lb(1),ub(1)
        diff = abs(fptr1d1(i) - fptr1d2(i))
        if (diff > 1.d-10) then
          dataMatchR8 = .false.
          write(msg,*) "Found data mismatch at (",i,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
    else if (rank == 2) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr2d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr2d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr2d1,i)
          ub(i) = ubound(fptr2d1,i)
        else
          ! distributed dim
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        diff = abs(fptr2d1(i,j) - fptr2d2(i,j))
        if (diff > 1.d-10) then
          dataMatchR8 = .false.
          write(msg,*) "Found data mismatch at (",i,",",j,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
      enddo
    else if (rank == 3) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr3d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr3d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr3d1,i)
          ub(i) = ubound(fptr3d1,i)
        else
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do k=lb(3),ub(3)
      do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        diff = abs(fptr3d1(i,j,k) - fptr3d2(i,j,k))
        if (diff > 1.d-10) then
          dataMatchR8 = .false.
          write(msg,*) "Found data mismatch at (",i,",",j,",",k,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
      enddo
      enddo
    else if (rank == 4) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr4d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr4d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr4d1,i)
          ub(i) = ubound(fptr4d1,i)
        else
          ! distributed dim
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do l=lb(4),ub(4)
      do k=lb(3),ub(3)
      do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        diff = abs(fptr4d1(i,j,k,l) - fptr4d2(i,j,k,l))
        if (diff > 1.d-10) then
          dataMatchR8 = .false.
          write(msg,*) &
            "Found data mismatch at (",i,",",j,",",k,",",l,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
      enddo
      enddo
      enddo
    endif
    deallocate(arrayToDistGridMap)
    deallocate(exclusiveLBound,exclusiveUBound)
  end function

  function dataMatchR4(array1, array2, rc)
    logical             :: dataMatchR4
    type(ESMF_Array)    :: array1
    type(ESMF_Array)    :: array2
    integer             :: rc
    !-----------------------------------------
    real(ESMF_KIND_R4)            :: diff
    real(ESMF_KIND_R4), pointer   :: fptr1d1(:), fptr1d2(:)
    real(ESMF_KIND_R4), pointer   :: fptr2d1(:,:), fptr2d2(:,:)
    real(ESMF_KIND_R4), pointer   :: fptr3d1(:,:,:), fptr3d2(:,:,:)
    real(ESMF_KIND_R4), pointer   :: fptr4d1(:,:,:,:), fptr4d2(:,:,:,:)
    integer                       :: rank, dimCount
    integer                       :: i, j, k, l
    integer, allocatable          :: arrayToDistGridMap(:)
    integer, allocatable          :: distgridToPackedArrayMap(:)
    integer, allocatable          :: exclusiveLBound(:,:), exclusiveUBound(:,:)
    integer                       :: lb(4), ub(4)
    character(len=160)            :: msg
    dataMatchR4 = .true.  ! initialize
    call ESMF_ArrayGet(array1, rank=rank, dimCount=dimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    allocate(arrayToDistGridMap(rank))
    allocate(distgridToPackedArrayMap(dimCount))
    allocate(exclusiveLBound(dimCount,1),exclusiveUBound(dimCount,1))
    call ESMF_ArrayGet(array1, arrayToDistGridMap=arrayToDistGridMap, &
      distgridToPackedArrayMap=distgridToPackedArrayMap, &
      exclusiveLBound=exclusiveLBound, exclusiveUBound=exclusiveUBound, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (rank == 1) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr1d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr1d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr1d1,i)
          ub(i) = ubound(fptr1d1,i)
        else
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do i=lb(1),ub(1)
        diff = abs(fptr1d1(i) - fptr1d2(i))
        if (diff > 1.d-10) then
          dataMatchR4 = .false.
          write(msg,*) "Found data mismatch at (",i,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
    else if (rank == 2) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr2d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr2d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr2d1,i)
          ub(i) = ubound(fptr2d1,i)
        else
          ! distributed dim
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        diff = abs(fptr2d1(i,j) - fptr2d2(i,j))
        if (diff > 1.d-10) then
          dataMatchR4 = .false.
          write(msg,*) "Found data mismatch at (",i,",",j,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
      enddo
    else if (rank == 3) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr3d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr3d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr3d1,i)
          ub(i) = ubound(fptr3d1,i)
        else
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do k=lb(3),ub(3)
      do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        diff = abs(fptr3d1(i,j,k) - fptr3d2(i,j,k))
        if (diff > 1.d-10) then
          dataMatchR4 = .false.
          write(msg,*) "Found data mismatch at (",i,",",j,",",k,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
      enddo
      enddo
    else if (rank == 4) then
      call ESMF_ArrayGet(array1, farrayPtr=fptr4d1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayGet(array2, farrayPtr=fptr4d2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      do i=1, rank
        j = arrayToDistGridMap(i)
        if (j==0) then
          ! undistributed dim
          lb(i) = lbound(fptr4d1,i)
          ub(i) = ubound(fptr4d1,i)
        else
          ! distributed dim
          ! distributed dim
          k = distgridToPackedArrayMap(j)
          lb(i) = exclusiveLBound(k,1)
          ub(i) = exclusiveUBound(k,1)
        endif
      enddo
      do l=lb(4),ub(4)
      do k=lb(3),ub(3)
      do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        diff = abs(fptr4d1(i,j,k,l) - fptr4d2(i,j,k,l))
        if (diff > 1.d-10) then
          dataMatchR4 = .false.
          write(msg,*) &
            "Found data mismatch at (",i,",",j,",",k,",",l,"), diff=", diff
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        endif
      enddo
      enddo
      enddo
      enddo
    endif
    deallocate(arrayToDistGridMap)
    deallocate(exclusiveLBound,exclusiveUBound)
  end function

end program ESMF_ArrayBundleRedistUTest
