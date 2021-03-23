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
program ESMF_ArrayRedistPerfUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayRedistPerfUTest -  Tests ArrayRedist() performance
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
  character(ESMF_MAXSTR)      :: failMsg
  character(ESMF_MAXSTR)      :: name

  ! Local variables
  type(ESMF_VM)               :: vm
  integer                     :: rc, petCount, localPet
#ifdef ESMF_TESTEXHAUSTIVE
  character(1024)             :: msgString
  type(ESMF_Grid)             :: grid
  type(ESMF_Field)            :: field
  type(ESMF_Array)            :: srcArray, dstArray, tstArray
  type(ESMF_RouteHandle)      :: rh
  real(ESMF_KIND_R8), pointer :: dstPtr(:,:), tstPtr(:,:)
  integer                     :: lrc, i, j
  logical                     :: mismatch
  real(ESMF_KIND_R8)          :: t0, t1, t0_store, t1_store, dt, dtTest
#endif

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
  
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "GridCreate on src side - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/3600, 160/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/1,petCount/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "FieldCreate on src side - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "FieldFill on src side - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldFill(field, dataFillScheme="sincos", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get srcArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGet(field, array=srcArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "GridCreate on dst side - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/3600, 160/), &
    minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
    maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
    staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
    regDecomp=(/petCount,1/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "FieldCreate on dst side - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "FieldFill on dst side - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldFill(field, dataFillScheme="one", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get dstArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGet(field, array=dstArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "FieldCreate on dst side for tstArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "FieldFill on dst side for tstArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldFill(field, dataFillScheme="sincos", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get tstArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_FieldGet(field, array=tstArray, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedistStore() - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_VMBarrier(vm, rc=lrc)
  call ESMF_VMWtime(t0_store, rc=lrc)
  call ESMF_ArrayRedistStore(srcArray, dstArray, routehandle=rh, rc=rc)
  call ESMF_VMWtime(t1_store, rc=lrc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "ArrayRedist() - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_VMBarrier(vm, rc=lrc)
  call ESMF_VMWtime(t0, rc=lrc)
  call ESMF_ArrayRedist(srcArray, dstArray, routehandle=rh, rc=rc)
  call ESMF_VMWtime(t1, rc=lrc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get data pointer into dstArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(dstArray, farrayPtr=dstPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Get data pointer into tstArray - Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS" 
  call ESMF_ArrayGet(tstArray, farrayPtr=tstPtr, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Check dst data - Test"
  write(failMsg, *) "Incorrect data detected!" 
  mismatch=.false.
  do j=lbound(dstPtr,2), ubound(dstPtr,2)
  do i=lbound(dstPtr,1), ubound(dstPtr,1)
    if (abs(dstPtr(i,j)-tstPtr(i,j)) > 1.d-13) then
      print *, "mismatch detected: ", dstPtr(i,j), " vs. ", &
        tstPtr(i,j), " diff=", dstPtr(i,j)-tstPtr(i,j)
      mismatch=.true. ! indicate mismatch
      exit  ! break out of check loop
    endif
  enddo
  enddo
  call ESMF_Test(.not.mismatch, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Check ArrayRedistStore() performance - Test"
  dt = t1_store - t0_store
  write(msgString,*) "ArrayRedistStore() performance: ", dt, " seconds."
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
#ifdef ESMF_BOPT_g
  dtTest = 20.d0  ! 20s is expected to pass in debug mode
#else
  dtTest = 2.d0   ! 2s is expected to pass in optimized mode
#endif
  write(failMsg, *) "ArrayRedistStore() performance problem! ", dt, ">", dtTest
  call ESMF_Test((dt<dtTest), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only
  write(name, *) "Check ArrayRedist() performance - Test"
  dt = t1 - t0
  write(msgString,*) "ArrayRedist() performance: ", dt, " seconds."
  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
#ifdef ESMF_BOPT_g
  dtTest = 0.1d0    ! 0.1s is expected to pass in debug mode
#else
  dtTest = 0.01d0   ! 0.01s is expected to pass in optimized mode
#endif
  write(failMsg, *) "ArrayRedist() performance problem! ", dt, ">", dtTest
  call ESMF_Test((dt<dtTest), name, failMsg, result, ESMF_SRCLINE)

#endif

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


end program ESMF_ArrayRedistPerfUTest
    
    
