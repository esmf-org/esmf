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
      program ESMF_FieldRedistArbUTest

!------------------------------------------------------------------------------

#include "ESMF.h"
#define ESMF_METHOD "ESMF_TESTS"
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRedistArbUTest - Unit tests for FieldRedist methods on a arbitrarily distributed grid
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Redist on Arbitrary Grid unit tests .
! The companion folder Fieldsrc contains the definitions for the
! Field methods.
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_GridUtilMod
    use ESMF_LogErrMod


    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'
    
    type(ESMF_Grid) :: srcgrid, dstgrid, srcgrid2D, dstgrid2D
    type(ESMF_VM) :: vm
    type(ESMF_ArraySpec) :: arrayspec1D, arrayspec2D
    integer :: ind1d, xdim, ydim, zdim, total, x, y
    integer :: i, remain
    integer :: myPet, petCount, halfPets
    integer :: localCount, localCount1
    integer, allocatable :: localIndices(:,:), localIndices1(:,:)
    integer                 :: localrc
    type(ESMF_Field)        :: srcfield, dstfield, srcfield2D, dstfield2D
    type(ESMF_RouteHandle)  :: rhandle, rhandle2D
    real, dimension(:,:), pointer   :: fptr
    real, dimension(:), pointer   :: fptr1D
    integer :: lbnd(2), ubnd(2), cnt(2)
    integer :: lbnd1(1), ubnd1(1), cnt1(1)
    logical :: correct

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Calculate localIndices and localCount for a 100x200 2D arbitrary grid with 
  ! an optional undistributed 3rd dimenison of size 4
  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, petCount=petCount, localPet=myPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! grid dimension: xdim and ydim are arbitrarily distributed
  xdim = 100
  ydim = 200
  zdim = 4

  ! --------------------------------------------------------
  ! Set up distribution for the source grid
  ! --------------------------------------------------------
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
       x = ind1d/ydim+1
       y = mod(ind1d,ydim)+1
       localIndices(i,1)=x
       localIndices(i,2)=y
       if (y<ydim) then
         localIndices(i+1,1)=x
         localIndices(i+1,2)=y+1
       else
         localIndices(i+1,1)=x+1
         localIndices(i+1,2)=y
       endif
       ind1d = ind1d+petCount+halfPets
     enddo 
  else
     ind1d=myPet+halfPets
     do i=1,localCount
       x = ind1d/ydim+1
       y = mod(ind1d,ydim)+1
       localIndices(i,1)=x
       localIndices(i,2)=y
       ind1d = ind1d+petCount+halfPets
     enddo
  endif
  if (myPet == petCount-1) then
    ind1d = total-remain
    do i=localCount-remain+1,localCount
       x = ind1d/ydim+1
       y = mod(ind1d,ydim)+1
       localIndices(i,1)=x
       localIndices(i,2)=y
       ind1d = ind1d+1
    enddo
  endif
 
  !----------------------------------------------------------------------------=
  ! Test Set 1:  Create a 2D Field with 2D Arbitrary Grid
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 2D src Field on a 2D arb grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  
  srcgrid2D = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R4, &
    minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
    arbIndexList=localIndices, arbIndexCount=localCount, &
    name="srcgrid2D", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  srcfield2D = ESMF_FieldCreate(srcgrid2D, arrayspec1D, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc = ESMF_FAILURE

  ! Set field values
  call ESMF_FieldGet(srcfield2D, farrayPtr=fptr1D, computationalLBound=lbnd1, &
    computationalUBound=ubnd1, computationalCount=cnt1, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc = ESMF_FAILURE

  print *, myPet, " 2D Source field computational bound", lbnd1, ubnd1, cnt1
  if (cnt1(1) /= localcount) correct = .false.
  
  ! Set the field values to be the same as its local index
  do i=lbnd1(1),ubnd1(1)
    fptr1D(i) = (localindices(i,2)-1)*ydim+localIndices(i,1)
  enddo

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------=
  ! Test Set 2:  Create a 3D src Field with 2D Arbitrary Grid
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 3D Field on a 3D arb grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  srcgrid = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R4, &
    minIndex=(/1,1,1/), maxIndex=(/xdim, ydim,zdim/), &
    arbIndexList=localIndices, arbIndexCount=localCount, &
    name="srcgrid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  srcfield = ESMF_FieldCreate(srcgrid, arrayspec2D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct = .false.

  ! Set field values
  call ESMF_FieldGet(srcfield, farrayPtr=fptr, computationalLBound=lbnd, &
    computationalUBound=ubnd, computationalCount=cnt, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct = .false.

  print *, myPet, " 3D Source field computational bound", lbnd, ubnd, cnt
  if (cnt(1) .ne. localcount) correct = .false.
  if (cnt(2) .ne. zdim) correct = .false.
  
  ! Set the field values to be the same as its local index
  do i=lbnd(1),ubnd(1)
    fptr(i,:) = (localindices(i,2)-1)*ydim+localIndices(i,1)
  enddo

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  ! --------------------------------------------------------
  ! Set up distribution for the destination grid
  ! --------------------------------------------------------
  write(name, *) "Create a 2D Field on a 2D destination arb grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  ! calculate the localcount and the local indices based on the total number of PETS
  ! this time, they are evenly distributed with index card dealed
  localCount1 = total/petCount
  remain = total-localCount1*(petCount)
  if (myPet == petCount-1) localCount1 = localCount1+remain
  ! car deal the cells with the first half of the Pets gets two each time
  ! the remaining cells are given to the last Pet
  allocate(localIndices1(localCount1,2))

  ind1d=myPet
  do i=1,localCount1
    x = ind1d/ydim+1
    y = mod(ind1d,ydim)+1
    localIndices1(i,1)=x
    localIndices1(i,2)=y
    ind1d = ind1d+petCount
  enddo
  if (myPet == petCount-1) then
    ind1d = total-remain
    do i=localCount1-remain+1,localCount1
       x = ind1d/ydim+1
       y = mod(ind1d,ydim)+1
       localIndices1(i,1)=x
       localIndices1(i,2)=y
       ind1d = ind1d+1
    enddo
  endif

  !----------------------------------------------------------------------------=
  ! Test Set 3:  Create a 2D dest Field with 2D Arbitrary Grid using a different distribution
  !-----------------------------------------------------------------------------
  !NEX_UTest
    write(name, *) "Create a 2D dest Field on a 2D destination arb grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  dstgrid2D = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
    minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
    arbIndexList=localIndices1, arbIndexCount=localCount1, &
    name="dstgrid2D", rc=rc)
  if (rc /= ESMF_SUCCESS) correct=.false.

  dstfield2D = ESMF_FieldCreate(dstgrid2D, arrayspec1D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------=
  ! Test Set 4:  Create a 3D dest Field with 3D Arbitrary Grid
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 3D dest Field on a 3D destination arb grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  dstgrid = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
    minIndex=(/1,1,1/), maxIndex=(/xdim, ydim,zdim/), &
    arbIndexList=localIndices1, arbIndexCount=localCount1, &
    name="dstgrid", rc=rc)
  if (rc /= ESMF_SUCCESS) correct=.false.

!  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R4, &
!       rc=localrc)
!  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  dstfield = ESMF_FieldCreate(dstgrid, arrayspec2D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct=.false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------=
  ! Regrid the 2D Srcgrid to 2D Dst Grid
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Redist the 2D src field to the 2D dest field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  ! Do a redistStore
  call ESMF_FieldRedistStore(srcfield2D, dstfield2D, routehandle=rhandle2D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct = .false.

  ! Now a redist
  call ESMF_FieldRedist(srcfield2D, dstfield2D, routehandle=rhandle2D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct = .false.

  ! Check the destination field
  call ESMF_FieldGet(dstfield2D, localDe=0, farrayPtr=fptr1D, computationalLBound=lbnd1, &
    computationalUBound=ubnd1, computationalCount=cnt1, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct = .false.

  print *, myPet, " 2D Destination field computational bound", lbnd1, ubnd1, cnt1

  ! check values
  total = 0
  do i=lbnd1(1),ubnd1(1)
     if (fptr1D(i) .ne. (localindices1(i,2)-1)*ydim+localIndices1(i,1)) then 
    total = total+1
        print *, myPet, 'element',i, 'does not match', fptr1D(i), (localindices1(i,2)-1)*ydim+localIndices1(i,1)
    correct = .false.
     endif  
  enddo

  print *, myPet, " total incorrect results ", total
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

! 3D Arb Grid Redist is not working, comment it out for now (8/11/09)
#if 0
  !----------------------------------------------------------------------------=
  ! Regrid the 3D Src field to 3D Dst field
  !-----------------------------------------------------------------------------
  !NEX_disable_UTest
  write(name, *) "Redist the 2D src field to the 2D dest field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  ! Do a redistStore
  call ESMF_FieldRedistStore(srcfield, dstfield, routehandle=rhandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct=.false.

  ! Now a redist
  call ESMF_FieldRedist(srcfield, dstfield, routehandle=rhandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct=.false.

  ! Check the destination field
  call ESMF_FieldGet(dstfield, localDe=0, farrayPtr=fptr, computationalLBound=lbnd, &
    computationalUBound=ubnd, computationalCount=cnt, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct=.false.

  print *, myPet, " 3D Destination field computational bound", lbnd, ubnd, cnt

  ! check values
  total = 0
  do i=lbnd(1),ubnd(1)
     if (fptr(i,1) .ne. (localindices1(i,2)-1)*ydim+localIndices1(i,1)) total = total+1
  enddo

  print *, myPet, " total incorrect results ", total

  if (total > 0) correct = .false.

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldRedistRelease(rhandle, rc=localrc)
#endif

  call ESMF_FieldDestroy(srcfield, rc=localrc)
  call ESMF_FieldDestroy(dstfield, rc=localrc)
  call ESMF_FieldDestroy(srcfield2D, rc=localrc)
  call ESMF_FieldDestroy(dstfield2D, rc=localrc)
  call ESMF_GridDestroy(srcgrid, rc=localrc)
  call ESMF_GridDestroy(dstgrid, rc=localrc)
  call ESMF_GridDestroy(srcgrid2D, rc=localrc)
  call ESMF_GridDestroy(dstgrid2D, rc=localrc)

  call ESMF_FieldRedistRelease(rhandle2D, rc=localrc)

  deallocate(localIndices, localIndices1)
  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_FieldRedistArbUTest
