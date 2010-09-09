! $Id: ESMF_FieldIOUTest.F90,v 1.6 2010/09/09 20:12:22 samsoncheung Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_FieldIOUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldIOUTest -  Tests FieldWrite()
!
! !DESCRIPTION:
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
 
  ! local variables
  type(ESMF_VM):: vm
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Field) :: field_w, field_r
  type(ESMF_Grid) :: grid
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_w, Farray_r
  type(ESMF_Array)                        :: array
  type(ESMF_StaggerLoc)                       :: sloc
  integer                                 :: rc, rc_tmp, de
  integer, allocatable :: computationalLBound(:),computationalUBound(:)
  integer, allocatable :: exclusiveLBound(:), exclusiveUBound(:)
  integer      :: localDeCount, localPet, petCount
  integer :: i,j,k
  real :: Maxvalue, diff

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !-----------------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Grid can be created
  grid = ESMF_GridCreateShapeTile(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
    name="landgrid", rc=rc)
  write(failMsg, *) ""
  write(name, *) "Creating a Grid to use in Field Tests"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  ! Allocate array
  allocate(Farray_w(5,10))

  localDeCount = 1
  allocate(exclusiveLBound(2))         ! dimCount=2
  allocate(exclusiveUBound(2))         ! dimCount=2
  allocate(computationalLBound(2))
  allocate(computationalUBound(2))

  sloc = ESMF_STAGGERLOC_CENTER
  call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
     exclusiveLBound=exclusiveLBound, &
     exclusiveUBound=exclusiveUBound, & 
     computationalLBound=computationalLBound, &
     computationalUBound=computationalUBound, rc=rc)

! Set values of fortran array
  Farray_w = 0.02  ! halo points will have value 0.02
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_w(i,j) = sin(dble(i)/5.0)*tan(dble(j)/5.0)
  enddo
  enddo
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  field_w=ESMF_FieldCreate(grid, farray=Farray_w, indexflag=ESMF_INDEX_DELOCAL, name="temperature",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Obtain ESMF_Array
  call ESMF_FieldGet(field_w, array=array, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Obtain ESMF_Array from Field"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Write Fortran array in Field
  call ESMF_FieldWrite(field_w, file="field.nc", rc=rc)
  write(failMsg, *) ""
  write(name, *) "Write Fortran array in Field"
  if(rc==ESMF_RC_LIB_NOT_PRESENT) then
   call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
  else
   call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
!------------------------------------------------------------------------


!------------------------------------------------------------------------
!------------------------------------------------------------------------

! Test ESMF_FieldRead() 
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create a ArraySpec
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8,   &
                         rank=2, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Array Spec Set "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create a new Field
  field_r = ESMF_FieldCreate(grid, arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
             name="temperature",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create new Field_r"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Read data to Object Field_r
  write(failMsg, *) ""
  write(name, *) "Read data to object field_r"
  call ESMF_FieldRead(field_r, file="field.nc", rc=rc)
  rc_tmp = rc   ! for later use in comparison
  if(rc==ESMF_RC_LIB_NOT_PRESENT) then
   call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
  else
   call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Obtain the Fortran pointer
  call ESMF_FieldGet(field_r, localDe=0, farrayPtr=Farray_r, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Point data to Fortran pointer Farray_r"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    diff = abs(Farray_w(i,j) - Farray_r(i,j))
    if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
  write(name, *) "Compare readin data to the existing data"
  write(failMsg, *) "Comparison failed"
  if(rc_tmp==ESMF_RC_LIB_NOT_PRESENT) then
   write(failMsg, *) "Comparison did not failed as was expected"
   call ESMF_Test((Maxvalue .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
  else
   write(*,*)"Maximum Error  = ", Maxvalue
   call ESMF_Test((Maxvalue .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
  endif

!------------------------------------------------------------------------


  deallocate (computationalLBound, computationalUBound)
  deallocate (exclusiveLBound, exclusiveUBound)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Field with no data can be destroyed
  call ESMF_FieldDestroy(field_w, rc=rc)
  call ESMF_FieldDestroy(field_r, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Destroying a Field with no data Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Array "
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayDestroy(array, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_FieldIOUTest
