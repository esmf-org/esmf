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

  type(ESMF_DistGrid) :: distgrid2_2DE, distgrid3_2DE
  type(ESMF_Array) :: array_2DE_w(2), array_2DE_r(2)
  type(ESMF_ArrayBundle) :: arraybundle_2DE_w, arraybundle_2DE_r
  integer(ESMF_KIND_I4), pointer, dimension(:,:) :: farrayPtr2_DE0, farrayPtr2_DE1
  integer(ESMF_KIND_I4), pointer, dimension(:,:) :: farrayPtr2_DE0_r, farrayPtr2_DE1_r
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) :: farrayPtr3_DE0, farrayPtr3_DE1
  integer(ESMF_KIND_I4), pointer, dimension(:,:,:) :: farrayPtr3_DE0_r, farrayPtr3_DE1_r

  integer, allocatable :: exclusiveLBound(:,:), exclusiveUBound(:,:)
  integer :: i,j,rc, rc1, rc2
  real(ESMF_KIND_R8) :: Maxvalue, diff

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0


  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  ! preparations
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  ! this unit test requires to be run on exactly 4 PETs
  if (petCount /= 4) then
    print *, 'PET count must be 4'
    goto 10
  end if

  !------------------------------------------------------------------------
  ! preparations
  call ESMF_ArraySpecSet(arrayspec1, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArraySpecSet(arrayspec2, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArraySpecSet(arrayspec3, typekind=ESMF_TYPEKIND_I4, rank=3, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  distgrid1 = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  distgrid3 = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/15,23,3/), &
    regDecomp=(/2,2,1/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
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
  farrayPtr1 = 0.4_ESMF_KIND_R8
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
  arraybundle_w = ESMF_ArrayBundleCreate(arrayList=array_w(1:3), &
    name="MyArrayBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleWrite Single file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleWrite(arraybundle_w, fileName="bundle.nc",         &
                             status=ESMF_FILESTATUS_REPLACE, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleWrite Single file with timeslice Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleWrite(arraybundle_w, fileName="bundle_ts.nc", timeslice=1,  &
                             status=ESMF_FILESTATUS_REPLACE, rc=rc)
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
  call ESMF_ArrayBundleWrite(arraybundle_w, fileName="sep.nc",      &
                             status=ESMF_FILESTATUS_REPLACE,    &
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
  arraybundle_r = ESMF_ArrayBundleCreate(arrayList=array_r(1:3), &
    name="EmptyBundle", rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRead Multiple file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS or ESMF_RC_LIB_NOT_PRESENT"
  !call ESMF_ArrayBundleRead(arraybundle_r, fileName="bundle.nc", rc=rc)
  call ESMF_ArrayBundleRead(arraybundle_r, fileName="sep.nc",   &
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
  call ESMF_Test((Maxvalue .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
! Multiple DEs per PET tests
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Distgrid Create 2 DE/Pet Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  distgrid2_2DE = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/4,3/), rc=rc1)
  distgrid3_2DE = ESMF_DistGridCreate(minIndex=(/1,1,1/), maxIndex=(/10,5,5/), &
    regDecomp=(/4,3,1/), rc=rc2)
  call ESMF_Test(rc1 == ESMF_SUCCESS .and. rc2 == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "2D/2 DE Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_2DE_w(1) = ESMF_ArrayCreate(arrayspec=arrayspec2, distgrid=distgrid2_2DE, &
          computationalLWidth=(/0,0/), computationalUWidth=(/0,0/), &
          totalLWidth=(/0,0/), totalUWidth=(/0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='temp', rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "3D/2 DE Array without Halo Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_2DE_w(2) = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=distgrid3_2DE, &
          computationalLWidth=(/0,0,0/), computationalUWidth=(/0,0,0/), &
          totalLWidth=(/0,0,0/), totalUWidth=(/0,0,0/), &
          indexflag=ESMF_INDEX_GLOBAL, name='velocity', rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)  
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 2D array DE 0"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_w(1), localDe=0, farrayPtr=farrayPtr2_DE0, rc=rc)
  farrayPtr2_DE0 = 1
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 2D array DE 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_w(1), localDe=1, farrayPtr=farrayPtr2_DE1, rc=rc)
  farrayPtr2_DE1 = 100
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 3D array DE 0"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_w(2), localDe=0, farrayPtr=farrayPtr3_DE0, rc=rc)
  farrayPtr3_DE0 = 10
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 3D array DE 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_w(2), localDe=1, farrayPtr=farrayPtr3_DE1, rc=rc)
  farrayPtr3_DE1 = 1000
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleCreate 2 DE Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle_2DE_w = ESMF_ArrayBundleCreate(arrayList=array_2DE_w, &
    name="2DEbundle", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleWrite 2 DE Single file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleWrite(arraybundle_2DE_w, fileName="abundle2DE.nc",         &
                             status=ESMF_FILESTATUS_REPLACE, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 2D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_2DE_r(1) = ESMF_ArrayCreate(arrayspec=arrayspec2, distgrid=distgrid2_2DE, &
    indexflag=ESMF_INDEX_GLOBAL, name="temp", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayCreate Allocate 3D ESMF_TYPEKIND_I4 Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  array_2DE_r(2) = ESMF_ArrayCreate(arrayspec=arrayspec3, distgrid=distgrid3_2DE, &
    indexflag=ESMF_INDEX_GLOBAL, name="velocity", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleCreate Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  arraybundle_2DE_r = ESMF_ArrayBundleCreate(arrayList=array_2DE_r, &
    name="2DEBundle", rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleRead 2 DE Single file Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS or ESMF_RC_LIB_NOT_PRESENT"
  !call ESMF_ArrayBundleRead(arraybundle_r, fileName="bundle.nc", rc=rc)
  call ESMF_ArrayBundleRead(arraybundle_2DE_r, fileName="abundle2DE.nc",   &
                            singleFile=.true., rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 2D array DE 0"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_r(1), localDe=0, farrayPtr=farrayPtr2_DE0_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Comparison test for 2D array DE 0"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (farrayPtr2_DE0_r == 1))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 2D array DE 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_r(1), localDe=1, farrayPtr=farrayPtr2_DE1_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Comparison test for 2D array DE 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (farrayPtr2_DE1_r == 100))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 3D array DE 0"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_r(2), localDe=0, farrayPtr=farrayPtr3_DE0_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Comparison test for 3D array DE 0"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (farrayPtr3_DE0_r == 10))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Obtain Fortran Array Pointer for 3D array DE 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayGet(array_2DE_r(2), localDe=1, farrayPtr=farrayPtr3_DE1_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Comparison test for 3D array DE 1"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (farrayPtr3_DE1_r == 1000))
#else
  rc = ESMF_SUCCESS
#endif
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  ! cleanup  
  deallocate (exclusiveLBound, exclusiveUBound)

  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayBundleDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArrayBundleDestroy(arraybundle_w, rc=rc)
  call ESMF_ArrayBundleDestroy(arraybundle_r, rc=rc)
  call ESMF_ArrayBundleDestroy(arraybundle_2DE_w, rc=rc)
  call ESMF_ArrayBundleDestroy(arraybundle_2DE_r, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "ArrayDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do i=1,3
    call ESMF_ArrayDestroy(array_w(i), rc=rc)
    call ESMF_ArrayDestroy(array_r(i), rc=rc)
  end do
  do, i=1,2
    call ESMF_ArrayDestroy(array_2DE_w(i), rc=rc)
    call ESMF_ArrayDestroy(array_2DE_r(i), rc=rc)
  end do
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "GridDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_DistGridDestroy(distgrid1, rc=rc)
  call ESMF_DistGridDestroy(distgrid3, rc=rc)
  call ESMF_DistGridDestroy(distgrid2_2DE, rc=rc)
  call ESMF_DistGridDestroy(distgrid3_2DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
10 continue
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

end program ESMF_ArrayBundleIOUTest
