! $Id: ESMF_FieldArbGridUTest.F90,v 1.1 2009/02/17 06:14:39 peggyli Exp $
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
      program ESMF_FieldArbGridUTest

!------------------------------------------------------------------------------

#include "ESMF.h"
#define ESMF_METHOD "ESMF_TESTS"
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldArbGridUTest - Unit tests for Field Create and Get methods on a arbitrarily distributed grid
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Arbitrary Grid unit tests .
! The companion folder Fieldsrc contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    use ESMF_GridUtilMod
    use ESMF_LogErrMod


    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id'
    
  type(ESMF_Grid) :: grid2d, grid3d
  type(ESMF_DELayout) :: delayout
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_ArraySpec) :: arrayspec1D, arrayspec2D, arrayspec3D
  integer :: ind1d, xdim, ydim, zdim, total, x, y
  integer :: i, j, remain
  integer :: myPet, petCount, halfPets
  integer :: localCount, localCount1
  integer, allocatable :: localIndices(:,:)
  integer                 :: localrc
  type(ESMF_Field)        :: field
  real, dimension(:,:), allocatable   :: farray
  logical :: correct
  integer :: memDimCount, dimCount

  integer, dimension(ESMF_MAXDIM) :: ec, cc
  integer, dimension(ESMF_MAXDIM) :: gelb, geub, gclb, gcub
  type(ESMF_StaggerLoc)           :: sloc
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

  ! Calculate localIndices and localCount for a 100x200 2D arbitrary grid with 
  ! an optional undistributed 3rd dimenison of size 4
  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! grid dimension: xdim and ydim are arbitrarily distributed
  xdim = 100
  ydim = 200
  zdim = 4

  ! calculate the localcount and the local indices based on the total number of PETS
  total = xdim*ydim
  halfPets = petCount/2
  ! let's make the first half pet twice of the cells of the second half
  localCount = total/(petCount+halfPets)
  remain = total-localCount*(petCount+halfPets)
  if (myPet < halfPets) localCount = localCount*2
  if (myPet == petCount-1) localCount = localCount+remain
  ! car deal the cells with the first half of the Pets gets two each time
  ! the remaining cells are given to the last Pet
  allocate(localIndices(localCount,2))

  if (myPet < halfPets) then
     ind1d = myPet*2
     do i=1,localCount,2
       y = mod(ind1d,ydim)+1
       x = ind1d/ydim+1
       localIndices(i,1)=y
       localIndices(i,2)=x
       if (y<ydim) then
         localIndices(i+1,1)=y+1
         localIndices(i+1,2)=x
       else
         localIndices(i+1,1)=1
         localIndices(i+1,2)=x+1
       endif
       ind1d = ind1d+petCount+halfPets
     enddo 
  else
     ind1d=myPet+halfPets
     do i=1,localCount
       y = mod(ind1d,ydim)+1
       x = ind1d/ydim+1
       localIndices(i,1)=y
       localIndices(i,2)=x
       ind1d = ind1d+petCount+halfPets
     enddo
  endif
  if (myPet == petCount-1) then
    ind1d = total-remain+1
    do i=localCount-remain+1,localCount
       y = mod(ind1d,ydim)+1
       x = ind1d/ydim+1
       localIndices(i,1)=y
       localIndices(i,2)=x
       ind1d = ind1d+1
    enddo
  endif
 
  !----------------------------------------------------------------------------=
  ! Test Set 1:  Create a 2D field on a 2D Arbitrary Grid 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create an field using an 2D arb. grid and 1D arrayspec"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  grid2d = ESMF_GridCreateShapeTile("arbgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
	localIndices=localIndices,localCount=localCount,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  field = ESMF_FieldCreate(grid2d, arrayspec1D, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(field, memDimCount=memDimCount, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field memDimCount, dimCount', memDimCount, dimCount
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) correct = .false.

  if (memDimCount .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldDestroy(field)

  !----------------------------------------------------------------------------=
  ! Test Set 2:  Create a 2D field on a 3D Arbitrary Grid with one replicated dim
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 2D field on a 3D arb grid with one replicated dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  grid3d = ESMF_GridCreateShapeTile("arb3dgrid", coordTypeKind=ESMF_TYPEKIND_R8, &
	minIndex=(/1,1,1/), maxIndex=(/xdim, ydim,zdim/), &
	localIndices=localIndices,localCount=localCount,rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  field = ESMF_FieldCreate(grid3d, arrayspec1D, gridToFieldMap =(/1,2,0/),rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(field, memDimCount=memDimCount, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field memDimCount, dimCount', memDimCount, dimCount
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) correct = .false.

  if (memDimCount .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldDestroy(field)

  !----------------------------------------------------------------------------=
  ! Test Set 3:  Create a 3D field on a 3D Arbitrary Grid 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 3D field on a 3D arb. grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  field = ESMF_FieldCreate(grid3d, arrayspec2D, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(field, memDimCount=memDimCount, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field memDimCount, dimCount', memDimCount, dimCount
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) correct = .false.

  if (memDimCount .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldDestroy(field)

  !----------------------------------------------------------------------------=
  ! Test Set 4:  Create a 4D field on a 3D Arbitrary Grid 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 4D field on a 3D arb grid with one ungridded dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_ArraySpecSet(arrayspec3D, rank=3, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  field = ESMF_FieldCreate(grid3d, arrayspec3D, ungriddedLBound=(/1/), &
	ungriddedUBound=(/10/), rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(field, memDimCount=memDimCount, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field memDimCount, dimCount', memDimCount, dimCount
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) correct = .false.

  if (memDimCount .ne. 3) correct = .false.
  if (dimCount .ne. 4) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldDestroy(field)

  !----------------------------------------------------------------------------=
  ! Test Set 5:  Create a 3D field on a 3D Arbitrary Grid with one replicated grid 
  !              dimension and one ungridded field dimension 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 3D field on a 3D arb grid with one ungridded dim. and one rep. dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  field = ESMF_FieldCreate(grid3d, arrayspec2D, ungriddedLBound=(/1/), &
	ungriddedUBound=(/10/), gridToFieldMap=(/1,2,0/), rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(field, memDimCount=memDimCount, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field memDimCount, dimCount', memDimCount, dimCount
  if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) correct = .false.

  if (memDimCount .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldDestroy(field)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


end program ESMF_FieldArbGridUTest
