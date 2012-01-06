! $Id: ESMF_FieldBundleIOUTest.F90,v 1.22 2010/03/04 18:57:43 svasquez Exp
! $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_1w, Farray_2w
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_1r, Farray_2r
  type(ESMF_Grid) :: grid
  type(ESMF_StaggerLoc)                       :: sloc
  integer :: i, j, localrc
  real(ESMF_KIND_R8) :: Maxvalue, diff
  type(ESMF_Field) :: fieldRd(2),fieldTst(2)
  type(ESMF_FieldBundle) :: bundleRd, bundleTst
  integer, allocatable :: exclusiveLBound(:), exclusiveUBound(:)


  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0


  !---------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize()
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
  ! Create Field
  fieldTst(2)=ESMF_FieldCreate(grid, farray=Farray_2w,  &
       indexflag=ESMF_INDEX_DELOCAL, name="velocity",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Try creating a bundle of these fields
  bundleTst=ESMF_FieldBundleCreate(fieldList=fieldTst,rc=rc)      
  write(failMsg, *) ""
  write(name, *) "Create a bundle from 2 fields "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Write to a single file Test
  call ESMF_FieldBundleWrite(bundleTst, file="single.nc", rc=rc)
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
  ! FieldBundle Write to multiple files Test
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Writing a FieldBundle to multiple files Test"
  call ESMF_FieldBundleWrite(bundleTst, file="multi.nc",  &
      singleFile=.false., rc=rc)
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
  ! Try creating a bundle of these fields
  bundleRd=ESMF_FieldBundleCreate(fieldList=fieldRd,rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a bundle from 2 fields "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! FieldBundle Read from a single file Test
  call ESMF_FieldBundleRead(bundleRd, file="single.nc", rc=rc)
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
  call ESMF_FieldBundleRead(bundleRd, file="multi.nc", &
                           singleFile=.false., rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS or ESMF_RC_LIB_NOT_PRESENT"
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
  call ESMF_FieldDestroy(fieldRd(2), rc=rc)
  call ESMF_FieldDestroy(fieldTst(1), rc=rc)
  call ESMF_FieldDestroy(fieldTst(2), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
!------------------------------------------------------------------------


  end program ESMF_FieldBundleIOUTest
