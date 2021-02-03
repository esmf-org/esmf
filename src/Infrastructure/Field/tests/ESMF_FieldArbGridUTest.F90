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
    use ESMF
    use ESMF_GridUtilMod
    use ESMF_LogErrMod


    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'
    
    type(ESMF_Grid) :: grid2d, grid3d
    type(ESMF_VM) :: vm
    type(ESMF_ArraySpec) :: arrayspec1D, arrayspec2D, arrayspec3D
    type(ESMF_Array)     :: array1d, array2d, array3d
    integer :: ind1d, xdim, ydim, zdim, total, x, y
    integer :: i, remain
    integer :: myPet, petCount, halfPets
    integer :: localCount
    integer, allocatable :: localIndices(:,:)
    integer                 :: localrc
    type(ESMF_Field)        :: field, field1, field2, field3, field4, field5
    real, dimension(:), pointer   :: fptr1d
    real, dimension(:,:), pointer   :: fptr2d
    real, dimension(:,:,:), pointer   :: fptr3d
    logical :: correct
    integer :: rank, dimCount

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
  write(name, *) "Create a Field using a 2D arb. grid and 1D arrayspec"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  grid2d = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
    minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
    arbIndexList=localIndices,arbIndexCount=localCount, &
    name="arbgrid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid2d, arrayspec1D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using a 2D arb. grid and 1D Array"
  call ESMF_FieldGet(field, array=array1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid2d, array1d, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using a 2D arb. grid and 1D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid2d, farray=fptr1d, &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using a 2D arb. grid and 1D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid2d, farray=fptr1d, &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using a 2D arb. grid and 1D Fortran array pointer - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field3 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field3, grid2d, farrayPtr=fptr1d, &
      datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field3, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using a 2D arb. grid and 1D Fortran array pointer"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field5 = ESMF_FieldCreate(grid2d, farrayPtr=fptr1d, &
      datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field5, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field (created using a 2D arb. grid and 1D arrayspec)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field (created using a 2D arb. grid and 1D array)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field (created using a 2D arb. grid and 1D Fortran array) - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field (created using a 2D arb. grid and 1D Fortran array)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field (created using a 2D arb. grid and 1D Fortran array pointer) - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field3, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field (created using a 2D arb. grid and 1D Fortran array pointer)"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field5, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------=
  ! Test Set 2:  Create a 2D field on a 3D Arbitrary Grid with one replicated dim
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 2D Field on a 3D arb grid with one replicated dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  grid3d = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
    minIndex=(/1,1,1/), maxIndex=(/xdim, ydim,zdim/), &
    arbIndexList=localIndices,arbIndexCount=localCount, &
    name="arb3dgrid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid3d, arrayspec1D, gridToFieldMap =(/1,2,0/),rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a 2D Field on a 3D arb grid with one replicated dim and 1d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldGet(field, array=array1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid3d, array1d, datacopyflag=ESMF_DATACOPY_VALUE, &
        gridToFieldMap=(/1,2,0/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid3d, farray=fptr1d, gridToFieldMap=(/1,2,0/), &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid3d, farray=fptr1d, gridToFieldMap=(/1,2,0/), &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array pointer - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field3 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field3, grid3d, farrayPtr=fptr1d, gridToFieldMap=(/1,2,0/), &
      datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field3, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array pointer"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field5 = ESMF_FieldCreate(grid3d, farrayPtr=fptr1d, gridToFieldMap=(/1,2,0/), &
      datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field5, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 2D Field on a 3D arb grid with one replicated dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 2D Field on a 3D arb grid with one replicated dim and 1d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array pointer - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field3, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field5, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !----------------------------------------------------------------------------=
  ! Test Set 3:  Create a 3D field on a 3D Arbitrary Grid 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 3D Field on a 3D arb. grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_ArraySpecSet(arrayspec2D, rank=2, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid3d, arrayspec2D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  
  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a 3D Field on a 3D arb. grid with 2d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldGet(field, array=array2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid3d, array2d, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid3d, farray=fptr2d, &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid3d, farray=fptr2d, &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array pointer - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field3 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field3, grid3d, farrayPtr=fptr2d, &
      datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field3, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array pointer"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field5 = ESMF_FieldCreate(grid3d, farrayPtr=fptr2d, &
      datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field5, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 3D Field on a 3D arb. grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 3D Field on a 3D arb. grid with 2d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array pointer - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field3, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field5, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------=
  ! Test Set 4:  Create a 4D field on a 3D Arbitrary Grid 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 4D Field on a 3D arb grid with one ungridded dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  call ESMF_ArraySpecSet(arrayspec3D, rank=3, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid3d, arrayspec3D, ungriddedLBound=(/1/), &
    ungriddedUBound=(/10/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 3) correct = .false.
  if (dimCount .ne. 4) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a 4D Field on a 3D arb grid with one ungridded dimension and 3d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldGet(field, array=array3d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid3d, array3d, datacopyflag=ESMF_DATACOPY_VALUE, ungriddedLBound=(/1/), &
    ungriddedUBound=(/10/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 3) correct = .false.
  if (dimCount .ne. 4) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 3D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr3d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid3d, farray=fptr3d, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 3) correct = .false.
  if (dimCount .ne. 4) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 3D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr3d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid3d, farray=fptr3d, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 3) correct = .false.
  if (dimCount .ne. 4) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 4D Field on a 3D arb grid with one ungridded dimension"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 4D Field on a 3D arb grid with one ungridded dimension and 3d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 3D Fortran array - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 3D Fortran array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------=
  ! Test Set 5:  Create a 3D field on a 3D Arbitrary Grid with one replicated grid 
  !              dimension and one ungridded field dimension 
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 3D Field on a 3D arb grid with one ungridded dim. and one rep. dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  field = ESMF_FieldCreate(grid3d, arrayspec2D, ungriddedLBound=(/1/), &
    ungriddedUBound=(/10/), gridToFieldMap=(/1,2,0/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a 3D Field on a 3D arb grid with one ungridded dim. and one rep. dim and 2d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldGet(field, array=array2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid3d, array2d, datacopyflag=ESMF_DATACOPY_VALUE, ungriddedLBound=(/1/), &
    ungriddedUBound=(/10/), gridToFieldMap=(/1,2,0/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid3d, farray=fptr2d, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/1,2,0/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid3d, farray=fptr2d, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/1,2,0/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 3) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 3D Field on a 3D arb grid with one ungridded dim. and one rep. dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 3D Field on a 3D arb grid with one ungridded dim. and one rep. dim and 2d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  ! Test Set 6:  Create a 1D field on a 3D Arbitrary Grid with the arb.dimensions as the
  !  replicated dimension
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 1D Field on a 3D arb grid with the arb. dims as the replicated dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  field = ESMF_FieldCreate(grid3d, arrayspec1D,gridToFieldMap=(/0,0,1/), rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 1) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a 1D Field on a 3D arb grid with the arb. dims as the replicated dim and 1d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldGet(field, array=array1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid3d, array1d, datacopyflag=ESMF_DATACOPY_VALUE, gridToFieldMap=(/0,0,1/), rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 1) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid3d, farray=fptr1d, &
    gridToFieldMap=(/0,0,1/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 1) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid3d, farray=fptr1d, &
    gridToFieldMap=(/0,0,1/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 1) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array pointer - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field3 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field3, grid3d, farrayPtr=fptr1d, &
    gridToFieldMap=(/0,0,1/), &
    datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field3, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 1) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 1D Fortran array pointer"
  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field5 = ESMF_FieldCreate(grid3d, farrayPtr=fptr1d, &
    gridToFieldMap=(/0,0,1/), &
    datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field5, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 1) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 1D Field on a 3D arb grid with the arb. dims as the replicated dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 1D Field on a 3D arb grid with the arb. dims as the replicated dim and 1d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array pointer - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field3, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array pointer"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field5, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !----------------------------------------------------------------------------
  ! Test Set 7:  Create a 2D field on a 3D Arbitrary Grid with the arb.dimensions as the
  !  replicated dimension and one ungridded dim
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create a 2D Field on a 3D arb grid with the arb.dims as the replicated dim and one ungridded dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS

  field = ESMF_FieldCreate(grid3d, arrayspec2D,gridToFieldMap=(/0,0,1/), &
          ungriddedLBound=(/1/), ungriddedUBound=(/10/),rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a 2D Field on a 3D arb grid with the arb.dims as the replicated dim and one ungridded dim and 2d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldGet(field, array=array2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field1 = ESMF_FieldCreate(grid3d, array2d, datacopyflag=ESMF_DATACOPY_VALUE, gridToFieldMap=(/0,0,1/), &
          ungriddedLBound=(/1/), ungriddedUBound=(/10/),rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field1, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array - empty/complete"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid3d, farray=fptr2d, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/0,0,1/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Create a Field using an 3D arb. grid and 2D Fortran array"
  call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field4 = ESMF_FieldCreate(grid3d, farray=fptr2d, &
    ungriddedLBound=(/1/), ungriddedUBound=(/10/), &
    gridToFieldMap=(/0,0,1/), &
    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field4, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 2) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 2D Field on a 3D arb grid with the arb.dims as the replicated dim and one ungridded dim"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a 2D Field on a 3D arb grid with the arb.dims as the replicated dim and one ungridded dim and 2d Array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field1, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 1D Fortran array - empty/complete"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !NEX_UTest
  write(name, *) "Destroy a Field using an 3D arb. grid and 2D Fortran array"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(field4, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_GridDestroy(grid2d, rc=localrc);
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! set the grid such that one PET has no localindices
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (myPet .eq. petCount-1) then
    localCount = 0
    deallocate(localIndices)
    allocate(localIndices(localCount,2))
  endif

  !NEX_UTest
  write(name, *) "Create a Field using a 2D arb. grid with one PET without any grid points"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  correct=.true.
  rc=ESMF_SUCCESS
  grid2d = ESMF_GridCreateNoPeriDim(coordTypeKind=ESMF_TYPEKIND_R8, &
    minIndex=(/1,1/), maxIndex=(/xdim, ydim/), &
    arbIndexList=localIndices,arbIndexCount=localCount, &
    name="arbgrid", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ArraySpecSet(arrayspec1D, rank=1, typekind=ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field = ESMF_FieldCreate(grid2d, arrayspec1D, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  field2 = ESMF_FieldEmptyCreate(rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_FieldEmptyComplete(field2, grid2d, farray=fptr1d, &
      indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldGet(field2, rank=rank, dimCount=dimCount, rc=localrc)
  if (myPet .eq. 0) print *, 'Field rank, dimCount', rank, dimCount
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT,  &
            rcToReturn=rc)) correct = .false.

  if (rank .ne. 1) correct = .false.
  if (dimCount .ne. 2) correct = .false.  

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  deallocate (localIndices)

  call ESMF_FieldDestroy(field, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_FieldDestroy(field2, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)
  call ESMF_GridDestroy(grid2d, rc=localrc)
  call ESMF_LogSetError(localrc, ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rcToReturn=rc)

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

end program ESMF_FieldArbGridUTest
