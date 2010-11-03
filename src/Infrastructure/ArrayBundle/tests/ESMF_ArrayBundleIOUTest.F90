! $Id: ESMF_ArrayBundleIOUTest.F90,v 1.6 2010/11/03 04:58:46 theurich Exp $
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
program ESMF_ArrayBundleIOUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArrayBundleIOUTest 
! !DESCRIPTION:
!
! The code in this file drives F90 ArrayBundleIOUTest() unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

  implicit none

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_VM):: vm
  integer:: petCount, localPet, localDeCount
  type(ESMF_ArraySpec):: arrayspec1, arrayspec2, arrayspec3
  type(ESMF_DistGrid):: distgrid1, distgrid3
  type(ESMF_Array):: array_w(3), array_r(3)
  integer:: arrayCount
  type(ESMF_ArrayBundle):: arraybundle_w, arraybundle_r
  real(ESMF_KIND_R8), pointer, dimension(:,:) :: farrayPtr1, farrayPtr1_r
  integer(ESMF_KIND_I4), pointer, dimension(:,:) :: farrayPtr2, farrayPtr2_r
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) :: farrayPtr3, farrayPtr3_r
  
  integer, allocatable :: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer :: i,j,rc
  real :: Maxvalue, diff

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0


  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  ! this unit test requires to be run on exactly 4 PETs
  if (petCount /= 4) goto 10
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_ArraySpecSet(arrayspec1, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArraySpecSet(arrayspec2, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArraySpecSet(arrayspec3, typekind=ESMF_TYPEKIND_I4, rank=3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid1 = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid3 = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/15,23,3/), &
    regDecomp=(/2,2,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_w(1) = ESMF_ArrayCreate(arrayspec=arrayspec1, distgrid=distgrid1, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain real Fortran Array Pointer for array 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_w(1), localDe=0, farrayPtr=farrayPtr1, rc=rc)
  farrayPtr1 = 0.4
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_w(2) = ESMF_ArrayCreate(arrayspec=arrayspec2, distgrid=distgrid1, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray2", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain integer Fortran Array Pointer for array 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_w(2), localDe=0, farrayPtr=farrayPtr2, rc=rc)
  farrayPtr2 = 2
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 3D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_w(3) = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=distgrid3, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray3", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Integer Fortran Array Pointer for array 2"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_w(3), localDe=0, farrayPtr=farrayPtr3, rc=rc)
  farrayPtr3 = 4
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle_w = ESMF_ArrayBundleCreate(arrayList=array_w, arrayCount=3, &
    name="MyArrayBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleWrite Single file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleWrite(arraybundle_w, file="bundle.nc", rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleWrite Multiple files Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS or ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_ArrayBundleWrite(arraybundle_w, file="sep.nc",  &
                             singleFile=.false., rc=rc)
  if(rc==ESMF_RC_LIB_NOT_PRESENT) then
   call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
  else
   call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  endif


  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------

  ! Read data back

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_R8 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_r(1) = ESMF_ArrayCreate(arrayspec=arrayspec1, distgrid=distgrid1, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_r(2) = ESMF_ArrayCreate(arrayspec=arrayspec2, distgrid=distgrid1, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray2", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 3D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_r(3) = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=distgrid3, &
    indexflag=ESMF_INDEX_GLOBAL, name="MyArray3", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle_r = ESMF_ArrayBundleCreate(arrayList=array_r, arrayCount=3, &
    name="EmptyBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRead Single file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS or ESMF_RC_LIB_NOT_PRESENT"
  !call ESMF_ArrayBundleRead(arraybundle_r, file="bundle.nc", rc=rc)
  call ESMF_ArrayBundleRead(arraybundle_r, file="sep.nc",   &
                            singleFile=.false., rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ESMF_ArrayGet test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_r(1), localDe=0, farrayPtr=farrayPtr1_r, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ESMF_ArrayGet test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  localDeCount = 1
  allocate(exclusiveLBound(2,localDeCount))         ! dimCount=2
  allocate(exclusiveUBound(2,localDeCount))         ! dimCount=2
  call ESMF_ArrayGet(array_r(1), exclusiveLBound=exclusiveLBound, &
                     exclusiveUBound=exclusiveUBound, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  !  Compare Fortran array
  !  Data is type ESMF_KIND_R8
  write(name, *) "Compare readin data from a different distgrid"
  write(failMsg, *) "Comparison failed"
  Maxvalue = 0.0
  do j=exclusiveLBound(2,1),exclusiveUBound(2,1)
  do i=exclusiveLBound(1,1),exclusiveUBound(1,1)
   diff = abs(farrayPtr1_r(i,j) - farrayPtr1(i,j) )
   if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error  = ", Maxvalue
  call ESMF_Test((Maxvalue .lt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-6), name, failMsg, result,ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

  !------------------------------------------------------------------------
  ! cleanup  
  deallocate (exclusiveLBound, exclusiveUBound)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(arraybundle_w, rc=rc)
  call ESMF_ArrayBundleDestroy(arraybundle_r, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do i=1,3
  call ESMF_ArrayDestroy(array_w(i), rc=rc)
  call ESMF_ArrayDestroy(array_r(i), rc=rc)
  enddo
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "GridDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid1, rc=rc)
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ArrayBundleIOUTest
