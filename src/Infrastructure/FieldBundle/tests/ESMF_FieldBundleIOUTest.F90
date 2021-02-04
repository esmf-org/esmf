! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National
! Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldBundleIOUTest

!------------------------------------------------------------------------------
!
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldBundleIOUTest - FieldBundle IO Unit Tests
!
! !DESCRIPTION:
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

  ! Local variables
  integer :: rc, petCount,localPet
  type(ESMF_VM) :: vm
  type(ESMF_ArraySpec):: arrayspec
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_1w, Farray_2w
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_1r, Farray_2r
  real(ESMF_KIND_R8), pointer :: Farray_DE0_w(:,:), Farray_DE0_r(:,:)
  real(ESMF_KIND_R8), pointer :: Farray_DE1_w(:,:), Farray_DE1_r(:,:)
  type(ESMF_Grid) :: grid, grid_2DE
  type(ESMF_StaggerLoc)                       :: sloc
  integer :: i, j, localrc
  real(ESMF_KIND_R8) :: Maxvalue, diff
  type(ESMF_Field) :: fieldRd(3),fieldTst(3)
  type(ESMF_FieldBundle) :: bundleRd, bundleTst
  integer, allocatable :: exclusiveLBound(:), exclusiveUBound(:)

  character(16), parameter :: apConv = 'Attribute_IO'
  character(16), parameter :: apPurp = 'attributes'
  type nameval_t
    character(ESMF_MAXSTR) :: name
    character(ESMF_MAXSTR) :: value
  end type

  type(nameval_t), allocatable :: attrNameVals(:)


  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: countfail = 0

  !---------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize()
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Grid can be created
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
    name="landgrid", rc=rc)
  write(failMsg, *) ""
  write(name, *) "Creating a Grid to use in Field Tests"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create dimensions attribute package on shared Grid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeAdd (grid,  &
      convention=apConv, purpose=apPurp,  &
      attrList=(/ ESMF_ATT_GRIDDED_DIM_LABELS /), rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set dimensions attribute package on Grid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeSet (grid,  &
      name=ESMF_ATT_GRIDDED_DIM_LABELS,  &
      valueList=(/ "x_axis", "y_axis" /), &
      convention=apConv, purpose=apPurp,  &
      rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Grid with 2 DEs/PET can be created
  grid_2DE = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/8,1/), gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
    name="landgrid", rc=rc)
  write(failMsg, *) ""
  write(name, *) "Creating a 2 DE/PET Grid to use in Field Tests"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create dimensions attribute package on 2 DE/PET Grid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeAdd (grid_2DE,  &
      convention=apConv, purpose=apPurp,  &
      attrList=(/ ESMF_ATT_GRIDDED_DIM_LABELS /), rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set dimensions attribute package on Grid Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_AttributeSet (grid_2DE,  &
      name=ESMF_ATT_GRIDDED_DIM_LABELS,  &
      valueList=(/ "x_axis_2DE", "y_axis_2DE" /), &
      convention=apConv, purpose=apPurp,  &
      rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!------------------------------------------------------------------------
  ! Allocate array
  allocate(Farray_1w(5,10))
  allocate(Farray_2w(5,10))
  allocate(Farray_1r(5,10))
  allocate(Farray_2r(5,10))

  allocate(exclusiveLBound(2))         ! dimCount=2
  allocate(exclusiveUBound(2))         ! dimCount=2

  sloc = ESMF_STAGGERLOC_CENTER
  call ESMF_GridGet(grid, localDe=0, staggerloc=sloc, &
     exclusiveLBound=exclusiveLBound, &
     exclusiveUBound=exclusiveUBound, rc=rc)

! Set values of fortran array
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_1w(i,j) = sin(dble(i)/5.0)*tan(dble(j)/5.0)
    Farray_2w(i,j) = cos(dble(i)/5.0)*tan(dble(j)/5.0)
  enddo
  enddo

  Farray_1r = 0.0
  Farray_2r = 0.0
!------------------------------------------------------------------------


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  fieldTst(1)=ESMF_FieldCreate(grid, farray=Farray_1w,  &
       indexflag=ESMF_INDEX_DELOCAL, name="temperature",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create attribute package on Field(1) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  allocate (attrNameVals(2))
  attrNameVals(1) = nameval_t ('long_name',      'Temperature CONUS')
  attrNameVals(2) = nameval_t ('units',          'F')
  call ESMF_AttributeAdd (fieldTst(1),  &
      convention=apConv, purpose=apPurp,  &
      attrList=attrNameVals%name, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set attribute values on Field(1) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do, i=1, size (attrNameVals)
    call ESMF_AttributeSet (fieldTst(1),  &
        attrNameVals(i)%name, attrNameVals(i)%value,  &
        convention=apConv, purpose=apPurp, rc=rc)
    if (rc /= ESMF_SUCCESS) exit
  end do
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  fieldTst(2)=ESMF_FieldCreate(grid, farray=Farray_2w,  &
       indexflag=ESMF_INDEX_DELOCAL, name="velocity",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create attribute package on Field(2) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  attrNameVals(1) = nameval_t ('long_name',      'Velocity CONUS')
  attrNameVals(2) = nameval_t ('units',          'MPH')
  call ESMF_AttributeAdd (fieldTst(2),  &
      convention=apConv, purpose=apPurp,  &
      attrList=attrNameVals%name, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set attribute values on Field(2) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do, i=1, size (attrNameVals)
    call ESMF_AttributeSet (fieldTst(2),  &
        attrNameVals(i)%name, attrNameVals(i)%value,  &
        convention=apConv, purpose=apPurp, rc=rc)
    if (rc /= ESMF_SUCCESS) exit
  end do
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create a ArraySpec
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8,   &
                         rank=2, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Array Spec Set "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field 
  fieldTst(3)=ESMF_FieldCreate(grid_2DE, arrayspec,  &
           name="temperature_2de_case",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from 2 DE/PET grid"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create attribute package on Field(3) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  attrNameVals(1) = nameval_t ('long_name',      'Temperature CONUS 2DE/PET')
  attrNameVals(2) = nameval_t ('units',          'F')
  call ESMF_AttributeAdd (fieldTst(3),  &
      convention=apConv, purpose=apPurp,  &
      attrList=attrNameVals%name, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set attribute values on Field(3) Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do, i=1, size (attrNameVals)
    call ESMF_AttributeSet (fieldTst(3),  &
        attrNameVals(i)%name, attrNameVals(i)%value,  &
        convention=apConv, purpose=apPurp, rc=rc)
    if (rc /= ESMF_SUCCESS) exit
  end do
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  call ESMF_FieldGet(fieldTst(3), localDe=0, farrayPtr=Farray_DE0_w, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get and fill Farray_w from field DE 0"
  Farray_DE0_w = 0.1_ESMF_KIND_R8
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  call ESMF_FieldGet(fieldTst(3), localDe=1, farrayPtr=Farray_DE1_w, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get and fill Farray_w from field DE 1"
  Farray_DE1_w = 1.1_ESMF_KIND_R8
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Try creating a bundle of these fields
  bundleTst=ESMF_FieldBundleCreate(fieldList=fieldTst,rc=rc)      
  write(failMsg, *) ""
  write(name, *) "Create a bundle from 3 fields "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create attribute package on FieldBundle single Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  attrNameVals(1) = nameval_t ('file_name', 'single.nc')
  attrNameVals(2) = nameval_t ('date',      'Apr 27, 2017')
  call ESMF_AttributeAdd (bundleTst,  &
      convention=apConv, purpose=apPurp,  &
      attrList=attrNameVals%name, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set attribute values on FieldBundle single Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do, i=1, size (attrNameVals)
    call ESMF_AttributeSet (bundleTst,  &
        attrNameVals(i)%name, attrNameVals(i)%value,  &
        convention=apConv, purpose=apPurp, rc=rc)
    if (rc /= ESMF_SUCCESS) exit
  end do
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Write to a single file Test
  call ESMF_FieldBundleWrite(bundleTst, fileName="single.nc",      &
      convention=apConv, purpose=apPurp,  &
      iofmt=ESMF_IOFMT_NETCDF_64BIT_OFFSET,  &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Writing a FieldBundle to a single file Test"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Write to a single file Test with timeslice
  call ESMF_FieldBundleWrite(bundleTst, fileName="single_ts.nc", timeslice=1,  &
      status=ESMF_FILESTATUS_REPLACE, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Writing a FieldBundle to a single file with timeslice Test"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Write a second timeslice to a single file Test
  call ESMF_FieldBundleWrite(bundleTst, fileName="single_ts.nc", timeslice=2,  &
      status=ESMF_FILESTATUS_OLD, overwrite=.true., rc=rc)
  !NOTE: For NetCDF overwrite=.true. must be set. For PNetCDF either setting
  !of overwrite seems to work. Must check if both result in correctly written
  !data.
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Writing a FieldBundle to a single file with timeslice Test"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Create attribute package on FieldBundle multi Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  attrNameVals(1) = nameval_t ('file_name', 'multi.nc')
  attrNameVals(2) = nameval_t ('date',      'Apr 27, 2017')
  call ESMF_AttributeAdd (bundleTst,  &
      convention=apConv, purpose=apPurp,  &
      attrList=attrNameVals%name, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Set attribute values on FieldBundle multi Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  do, i=1, size (attrNameVals)
    call ESMF_AttributeSet (bundleTst,  &
        attrNameVals(i)%name, attrNameVals(i)%value,  &
        convention=apConv, purpose=apPurp, rc=rc)
    if (rc /= ESMF_SUCCESS) exit
  end do
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Write to multiple files Test
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Writing a FieldBundle to multiple files Test"
  call ESMF_FieldBundleWrite(bundleTst, fileName="multi.nc",  &
      convention=apConv, purpose=apPurp,  &
       iofmt=ESMF_IOFMT_NETCDF_64BIT_OFFSET,  &
      status=ESMF_FILESTATUS_REPLACE, singleFile=.false., rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
!----- Read data back and compare ---------------------------------------
!------------------------------------------------------------------------
!------------------------------------------------------------------------

  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  fieldRd(1)=ESMF_FieldCreate(grid, farray=Farray_1r,  &
       indexflag=ESMF_INDEX_DELOCAL, name="temperature",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  fieldRd(2)=ESMF_FieldCreate(grid, farray=Farray_2r,  &
       indexflag=ESMF_INDEX_DELOCAL, name="velocity",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field 
  fieldRd(3)=ESMF_FieldCreate(grid_2DE, arrayspec,  &
           name="temperature_2de_case",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from 2 DE/PET grid"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Try creating a bundle of these fields
  bundleRd=ESMF_FieldBundleCreate(fieldList=fieldRd,rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a bundle from 3 fields "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Read from a single file Test
  call ESMF_FieldBundleRead(bundleRd, fileName="single.nc", rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Reading a FieldBundle from a single file Test"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  !  Compare Fortran array
  !  Data is type ESMF_KIND_R8
  write(name, *) "Compare read-in data with Farray_1"
  write(failMsg, *) "Comparison failed"
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
   diff = abs(Farray_1r(i,j) - Farray_1w(i,j) )
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
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Read from multiple files Test
  call ESMF_FieldBundleRead(bundleRd, fileName="multi.nc", &
                           singleFile=.false., rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or ESMF_RC_LIB_NOT_PRESENT"
  write(name, *) "Reading a FieldBundle from multiple files Test"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  !  Compare Fortran array
  !  Data is type ESMF_KIND_R8
  write(name, *) "Compare read-in data with Farray_2"
  write(failMsg, *) "Comparison failed"
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
   diff = abs(Farray_2r(i,j) - Farray_2w(i,j) )
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
  !NEX_UTest_Multi_Proc_Only
  ! Get DE 0 Array pointer from Field
  call ESMF_FieldGet(fieldRd(3), localDe=0, farrayPtr=Farray_DE0_r, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_r from field DE 0"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! DE 0 Array comparison test
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray_DE0_r == 0.1_ESMF_KIND_R8))
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "DE 0 Array comparison test"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get DE 1 Array pointer from Field
  call ESMF_FieldGet(fieldRd(3), localDe=1, farrayPtr=Farray_DE1_r, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_r from field DE 1"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! DE 1 Array comparison test
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray_DE1_r == 1.1_ESMF_KIND_R8))
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "DE 1 Array comparison test"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  ! cleanup 
   deallocate ( exclusiveLBound, exclusiveUBound ) 

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Grid Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "FieldBundle Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldBundleDestroy(bundleTst, rc=rc)
  call ESMF_FieldBundleDestroy(bundleRd, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Field Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_FieldDestroy(fieldRd(1), rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(fieldRd(2), rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(fieldRd(3), rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(fieldTst(1), rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(fieldTst(2), rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(fieldTst(3), rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_Test(countfail == 0, name, failMsg, result, ESMF_SRCLINE)

  deallocate(Farray_1w)
  deallocate(Farray_2w)
  deallocate(Farray_1r)
  deallocate(Farray_2r)
  deallocate(attrNameVals)

!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy globally indexed 2DE Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid_2DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
!------------------------------------------------------------------------


  end program ESMF_FieldBundleIOUTest
