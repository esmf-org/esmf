! $Id: ESMF_FieldIOUTest.F90,v 1.13 2010/11/20 19:30:09 samsoncheung Exp $
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
  type(ESMF_Field) :: field_w, field_r, field_t, field_tr
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_w, Farray_r
  real(ESMF_KIND_R8), pointer, dimension(:,:) ::  Farray_tw, Farray_tr
  ! Note: 
  ! field_w---Farray_w; field_r---Farray_r; 
  ! field_t---Farray_tw; field_tr---Farray_tr 
  type(ESMF_Grid) :: grid
  type(ESMF_StaggerLoc)                       :: sloc
  integer                                 :: rc, de
  integer, allocatable :: computationalLBound(:),computationalUBound(:)
  integer, allocatable :: exclusiveLBound(:), exclusiveUBound(:)
  integer      :: localDeCount, localPet, petCount
  integer :: i,j, t, endtime
  real*8 :: Maxvalue, diff

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: countfail = 0

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
  allocate(Farray_tw(5,10))
  allocate(Farray_tr(5,10))

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
  ! Write Fortran array in Field
  call ESMF_FieldWrite(field_w, file="field.nc", rc=rc)
  write(failMsg, *) ""
  write(name, *) "Write Fortran array in Field"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!
! Test multiple time slices that making use of NETCDF's unlimited dimension
!
!------------------------------------------------------------------------

  !NEX_UTest_Multi_Proc_Only
! ! Write data at time t on file, total number of time=endtime 
#ifdef ESMF_MPICH
  !TODO: Remove this once timeslicing is fixed for multi-PET with MPICH
  endtime = 1
#else
  endtime = 5
#endif
  
  do t = 1, endtime

! Set values of fortran array
  Farray_tw = 0.02  ! halo points will have value 0.02
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_tw(i,j) = dble(t)*(sin(dble(i)/5.0)*tan(dble(j)/5.0))
  enddo
  enddo

!------------------------------------------------------------------------
    ! Create Field
    field_t=ESMF_FieldCreate(grid, farray=Farray_tw, &
      indexflag=ESMF_INDEX_DELOCAL,name="temperature",  rc=rc)
    if(rc.ne.ESMF_SUCCESS) then
      countfail = countfail + 1
      exit
    endif
!------------------------------------------------------------------------

    ! Write Fortran array in Field
    call ESMF_FieldWrite(field_t, file="field_time.nc", timeslice=t, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
    if(rc.ne.ESMF_SUCCESS) then
      countfail = countfail + 1
    endif
#else
    if(rc.ne.ESMF_RC_LIB_NOT_PRESENT) then
      countfail = countfail + 1
      exit
    endif
#endif
  enddo  ! t

! Loop of time is ended. Check for failure.
  write(name, *) "Write Farray_tw at different time t in a loop"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((countfail==0), name, failMsg, result, ESMF_SRCLINE)


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
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
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
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (read-write) = ", Maxvalue
  call ESMF_Test((Maxvalue .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
!------------------------------------------------------------------------

! Read back the time slices of the field from file.

!------------------------------------------------------------------------
! Recall my Fortran array at time=t=... :
#ifdef ESMF_MPICH
  !TODO: Remove this once timeslicing is fixed for multi-PET with MPICH
  t = 1
#else
  t = 3
#endif
  Farray_tw = 0.02  ! halo points will have value 0.02
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_tw(i,j) = dble(t)*(sin(dble(i)/5.0)*tan(dble(j)/5.0))
  enddo
  enddo

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create an empty new Field
  field_tr = ESMF_FieldCreate(grid, arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
             name="temperature",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create new Field_r"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Read data at time=t to Object Field_r
  write(failMsg, *) ""
  write(name, *) "Read data time=t to object field_r per slice"
  call ESMF_FieldRead(field_tr, file="field_time.nc", timeslice=t, rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, &
                  failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Obtain the Fortran pointer
  call ESMF_FieldGet(field_tr, localDe=0, farrayPtr=Farray_tr, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Point data to Fortran pointer Farray_tr"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    diff = abs(Farray_tw(i,j) - Farray_tr(i,j))
    if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
  write(name, *) "Compare readin data to the existing data"
  write(failMsg, *) "Comparison failed"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (read-write) = ", Maxvalue
  call ESMF_Test((Maxvalue .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not failed as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#endif



  deallocate (computationalLBound, computationalUBound)
  deallocate (exclusiveLBound, exclusiveUBound)
  deallocate (Farray_w)

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
  call ESMF_FieldDestroy(field_t, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Destroying a Field with no data Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_FieldIOUTest
