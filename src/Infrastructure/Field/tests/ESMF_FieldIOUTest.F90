! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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
  use ESMF

  implicit none

!-------------------------------------------------------------------------
!=========================================================================

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
 
  ! local variables
  type(ESMF_VM):: vm
  type(ESMF_ArraySpec):: arrayspec
  type(ESMF_Field) :: field_w, field_r, field_t, field_s, field_tr, field_sr, field
  type(ESMF_Field) :: field_w_nohalo
  type(ESMF_Field) :: field_gw, field_gr, field_gr2, field_gr3
  type(ESMF_Field) :: field_r2de, field_w2de
  real(ESMF_KIND_R8), pointer :: Farray_w(:,:) => null (), Farray_r(:,:) => null ()
  real(ESMF_KIND_R8), pointer :: Farray_tw(:,:) => null (), Farray_tr(:,:) => null ()
  real(ESMF_KIND_R8), pointer :: Farray_sw(:,:) => null (), Farray_sr(:,:) => null ()
  real(ESMF_KIND_R4), pointer :: fptr(:,:) => null ()
  ! Note: 
  ! field_w---Farray_w; field_r---Farray_r; 
  ! field_t---Farray_tw; field_tr---Farray_tr 
  ! field_s---Farray_sw; field_sr---Farray_sr
  type(ESMF_Grid) :: grid, grid_g, grid_2DE

  real(ESMF_KIND_R8), pointer :: Farray_DE0_w(:,:) => null (), Farray_DE0_r(:,:) => null ()
  real(ESMF_KIND_R8), pointer :: Farray_DE1_w(:,:) => null (), Farray_DE1_r(:,:) => null ()

  integer                                 :: rc
  integer, allocatable :: computationalLBound(:),computationalUBound(:)
  integer, allocatable :: exclusiveLBound(:), exclusiveUBound(:)
  integer      :: localPet, petCount, tlb(2), tub(2)
  integer :: i,j, t, endtime, k
  real(ESMF_KIND_R8) :: Maxvalue, diff

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: countfail = 0
  ! Changing status for writing file in loop
  type(ESMF_FileStatus_Flag) :: statusFlag = ESMF_FILESTATUS_UNKNOWN

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !-----------------------------------------------------------------------------

  ! Set up
  ! *******
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


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
  ! Verifying that a Grid can be created
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/2,2/), gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
    name="landgrid", rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Creating a Grid to use in Field Tests"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  ! Allocate array
!  allocate(Farray_w(5,10))  ! it is done automatically with halo
!  allocate(Farray_tw(5,10)) ! it is done automatically with halo
  allocate(Farray_sw(5,10))  ! do it by hand for without halo case

  allocate(exclusiveLBound(2))         ! dimCount=2
  allocate(exclusiveUBound(2))         ! dimCount=2
  allocate(computationalLBound(2))
  allocate(computationalUBound(2))

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  field_w=ESMF_FieldCreate(grid, arrayspec=arrayspec, &
           totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
           name="temperature",  rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Create a field from grid and fortran dummy array"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  call ESMF_FieldGet(field_w, localDe=0, farrayPtr=Farray_w, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_w from field"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

  !print '(a,2(a,i0.1,a,i0.1),a)', 'field_w Farray_w bounds = ',  &
  !    '(', lbound (Farray_w,1), ':', ubound (Farray_w,1),  &
  !    ',', lbound (Farray_w,2), ':', ubound (Farray_w,2),')'
  !print '(a,2(a,i0.1,a,i0.1),a)', 'field_w exclusive bounds = ',  &
  !    '(', exclusiveLBound(1), ':', exclusiveUBound(1),  &
  !    ',', exclusiveLBound(2), ':', exclusiveUBound(2),')'
! Set values of fortran array
  Farray_w = 0.02  ! halo points will have value 0.02
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_w(i,j) = sin(dble(i)/5.0)*tan(dble(j)/5.0)
  enddo
  enddo


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Write Fortran array in Field
  call ESMF_FieldWrite(field_w, fileName="field.nc",        &
       status=ESMF_FILESTATUS_REPLACE, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Write Fortran array in Field"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field without halo region
  field_w_nohalo=ESMF_FieldCreate(grid, arrayspec=arrayspec, &
           name="temperature",  rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Create a field from grid and fortran dummy array without halo"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from nohalo Field
  call ESMF_FieldGet(field_w_nohalo, localDe=0, farrayPtr=Farray_w, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_w from nohalo field"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------
  !print '(a,2(a,i0.1,a,i0.1),a)', 'field_w_nohalo Farray_w bounds = ',  &
  !    '(', lbound (Farray_w,1), ':', ubound (Farray_w,1),  &
  !    ',', lbound (Farray_w,2), ':', ubound (Farray_w,2),')'
  !print '(a,2(a,i0.1,a,i0.1),a)', 'field_w_nohalo exclusive bounds = ',  &
  !    '(', exclusiveLBound(1), ':', exclusiveUBound(1),  &
  !    ',', exclusiveLBound(2), ':', exclusiveUBound(2),')'

! Set values of fortran array
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_w(i,j) = sin(dble(i)/5.0)*tan(dble(j)/5.0)
  enddo
  enddo

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Write Fortran array in nohalo Field
  call ESMF_FieldWrite(field_w_nohalo, fileName="fieldNoHalo.nc",        &
       status=ESMF_FILESTATUS_REPLACE, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Write Fortran array in nohalo Field"
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

  endtime = 5
  ! The first time through, we need to replace these files
  statusFlag = ESMF_FILESTATUS_REPLACE
  
  do t = 1, endtime

!------------------------------------------------------------------------
    ! Create Field with Halo
    field_t=ESMF_FieldCreate(grid, arrayspec=arrayspec, &
           totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
           name="temperature",  rc=rc)
    if(rc.ne.ESMF_SUCCESS) then
      countfail = countfail + 1
      exit
    endif
!------------------------------------------------------------------------
    ! Get Info from Field with Halo
    call ESMF_FieldGet(field_t, localDe=0, farrayPtr=Farray_tw, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
!------------------------------------------------------------------------
!     Set values of fortran array with Halo
    Farray_tw = 0.02  ! halo points will have value 0.02
    do j=exclusiveLBound(2),exclusiveUBound(2)
    do i=exclusiveLBound(1),exclusiveUBound(1)
      Farray_tw(i,j) = dble(t)*(sin(dble(i)/5.0)*tan(dble(j)/5.0))
    enddo
    enddo
!------------------------------------------------------------------------
    ! Write Fortran array in Field
    ! After two timesteps, test the auto-increment feature.
    ! Also, stop using the status flag after t = 3
    select case (t)
    case (1,2)
      call ESMF_FieldWrite(field_t, fileName="field_time.nc", timeslice=t,     &
           status=statusFlag, overwrite=.true., rc=rc)
    case (3)
      call ESMF_FieldWrite(field_t, fileName="field_time.nc",                  &
           status=statusFlag, overwrite=.true., rc=rc)
    case (4:)
      call ESMF_FieldWrite(field_t, fileName="field_time.nc",                  &
           overwrite=.true., rc=rc)
    end select
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
!------------------------------------------------------------------------

    ! Set values of fortran array for without halo case
    do j=exclusiveLBound(2),exclusiveUBound(2)
    do i=exclusiveLBound(1),exclusiveUBound(1)
      Farray_sw(i,j) = dble(t)*(sin(dble(i)/5.0)*tan(dble(j)/5.0))
    enddo
    enddo
!------------------------------------------------------------------------
    ! Create Field without Halo
    field_s=ESMF_FieldCreate(grid, farray=Farray_sw, &
           indexflag=ESMF_INDEX_DELOCAL,name="temperature",  rc=rc)
    if(rc.ne.ESMF_SUCCESS) then
      countfail = countfail + 1
      exit
    endif
!------------------------------------------------------------------------
    ! Write Fortran array in Field without halo
    call ESMF_FieldWrite(field_s, fileName="fieldNoHalo_time.nc", timeslice=t,   &
         status=statusFlag, overwrite=.true., rc=rc)
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
    ! Next time through the loop, we expect the file to be there
    statusFlag = ESMF_FILESTATUS_OLD

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
  write(failMsg, *) "Did not return ESMF_SUCCESS"
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
  call ESMF_FieldRead(field_r, fileName="field.nc", rc=rc)
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

#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    diff = abs(Farray_w(i,j) - Farray_r(i,j))
    if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
#else
  Maxvalue = 1.0
#endif

  write(name, *) "Compare readin data to the existing data"
  write(failMsg, *) "Comparison failed"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error (read-write) = ", Maxvalue
  call ESMF_Test((Maxvalue .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
!------------------------------------------------------------------------

! Read back the time slices of the field from file.

!------------------------------------------------------------------------
! Recall my Fortran array at time=t=... :
#ifdef ESMF_MPICH
  !TODO: Remove this once timeslicing is fixed for multi-PET with MPICH
  ! right now under MPICH always the first slice is pulled out by FieldRead()
  t = 1
#else
  t = 3
#endif
  ! Note that in exclusive region, Farray_tw and Farray_sw are identical.
  ! So for comparison purpose, just recall Farray_tw is enough.
  Farray_tw = 0.02  ! halo points will have value 0.02
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    Farray_tw(i,j) = dble(t)*(sin(dble(i)/5.0)*tan(dble(j)/5.0))
  enddo
  enddo

!          Compare Field with Halos
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
  ! Read data at time=t to Object Field_r with Halos
  write(failMsg, *) ""
  write(name, *) "Read data time=t to object field_r per slice"
  call ESMF_FieldRead(field_tr, fileName="field_time.nc", timeslice=t, rc=rc)
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
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    diff = abs(Farray_tw(i,j) - Farray_tr(i,j))
    if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
#else
  Maxvalue = 1.0
#endif

  write(name, *) "Compare readin data to the existing data"
  write(failMsg, *) "Comparison failed"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error with Halos (read-write) = ", Maxvalue
  call ESMF_Test((Maxvalue .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#endif


!        Compare fields without Halos
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create an empty new Field
  field_sr = ESMF_FieldCreate(grid, arrayspec, indexflag=ESMF_INDEX_DELOCAL, &
             name="temperature",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create new Field_s for without Halo"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Read data at time=t to Object Field_r without Halos
  write(failMsg, *) ""
  write(name, *) "Read data time=t to object field_r per slice"
  call ESMF_FieldRead(field_sr, fileName="fieldNoHalo_time.nc", timeslice=t, rc=rc)
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
  call ESMF_FieldGet(field_sr, localDe=0, farrayPtr=Farray_sr, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Point data to Fortran pointer Farray_sr"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
! ! Compare readin and the existing file
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  Maxvalue = 0.0
  do j=exclusiveLBound(2),exclusiveUBound(2)
  do i=exclusiveLBound(1),exclusiveUBound(1)
    diff = abs(Farray_tw(i,j) - Farray_sr(i,j))
    if (Maxvalue.le.diff) Maxvalue=diff
  enddo
  enddo
#else
  Maxvalue = 1.0
#endif

  write(name, *) "Compare readin data to the existing data"
  write(failMsg, *) "Comparison failed"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  write(*,*)"Maximum Error without Halos (read-write) = ", Maxvalue
  call ESMF_Test((Maxvalue .lt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((Maxvalue .gt. 1.e-14), name, failMsg, result,ESMF_SRCLINE)
#endif


  deallocate (computationalLBound, computationalUBound)
  deallocate (exclusiveLBound, exclusiveUBound)
  deallocate (Farray_sw)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
! Test Field with STAGGERLOC_EDGE1 specified
!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  rc = ESMF_SUCCESS                   ! Initialize
  write(name, *) "Write Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  ESMF_BLOCK(write_field_test)
    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/44, 8/), gridEdgeLWidth=(/0,0/), &
      rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit write_field_test

    field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R4, &
      staggerLoc=ESMF_STAGGERLOC_EDGE1, name="velocity", &
      totalLWidth=(/1,1/), totalUWidth=(/1,1/), rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit write_field_test

    call ESMF_FieldGet(field, farrayPtr=fptr, &
      totalLBound=tlb, totalUBound=tub, &
      rc=rc)
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit write_field_test

    if (.not. associated (fptr)) rc = ESMF_RC_PTR_NOTALLOC
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit write_field_test

!    print *, tlb, tub

    ! Replace file first time through
    statusFlag = ESMF_FILESTATUS_REPLACE
    do k = 1, 5
      do i = tlb(1), tub(1)
        do j = tlb(2), tub(2)
          fptr(i,j) = ((i-1)*(tub(2)-tlb(2))+j)*(10**(k-1))
        enddo
      enddo

      call ESMF_FieldWrite(field, fileName='halof.nc', timeslice=k,   &
           status=statusFlag, overwrite=.true., rc=rc)
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
    if (ESMF_LogFoundError (rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=ESMF_FILENAME)) exit write_field_test
#else
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, rc == ESMF_RC_LIB_NOT_PRESENT)
      exit write_field_test
#endif
      ! Next time through the loop, write to same file
      statusFlag = ESMF_FILESTATUS_OLD

    enddo
  ESMF_ENDBLOCK(write_field_test)
  call ESMF_Test(rc == ESMF_SUCCESS, name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Test with ESMF_INDEX_GLOBAL
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Grid
  grid_g = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/16,20/), &
      regDecomp=(/2,2/), indexflag=ESMF_INDEX_GLOBAL,  &
      rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a gloablly indexed grid"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  field_gw=ESMF_FieldCreate(grid_g, arrayspec=arrayspec, &
           indexflag=ESMF_INDEX_GLOBAL,  &
           name="temperature_g",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a gloablly indexed field from grid and fortran dummy array"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  allocate(exclusiveLBound(2))         ! dimCount=2
  allocate(exclusiveUBound(2))         ! dimCount=2
  call ESMF_FieldGet(field_gw, localDe=0, farrayPtr=Farray_w, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_gw from field"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Fill array and write
  Farray_w = localPet ! Fill
  call ESMF_FieldWrite(field_gw, fileName="field_globalindex.nc",        &
       status=ESMF_FILESTATUS_REPLACE, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Write globally indexed Field"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  field_gr=ESMF_FieldCreate(grid_g, arrayspec=arrayspec, &
           indexflag=ESMF_INDEX_GLOBAL,  &
           name="temperature_g",  rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Create a globally indexed field from grid and fortran dummy array"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! read array into Field.
  call ESMF_FieldRead(field_gr, fileName="field_globalindex.nc",        &
       rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Read globally indexed Field data"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  call ESMF_FieldGet(field_gr, localDe=0, farrayPtr=Farray_r, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_gr from field"
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Compare read-in data with expected
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray_r == localPet))
#else
  rc = ESMF_FAILURE
#endif
  write(failMsg, *) "Failed comparison check"
  write(name, *) "Compare read-in data with expected"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Comparison did not fail as was expected"
  call ESMF_Test((rc==ESMF_FAILURE), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field with different name
  field_gr2=ESMF_FieldCreate(grid_g, arrayspec=arrayspec, &
           indexflag=ESMF_INDEX_GLOBAL,  &
           name="temperature_g2",  rc=rc)
  write(failMsg, *) ""
  write(name, *) "Create a gloablly indexed field with different name"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Attempt to read array into Field with wrong field name.
  call ESMF_FieldRead(field_gr2, fileName="field_globalindex.nc",        &
       rc=rc)
  write(failMsg, *) "Did not fail"
  write(name, *) "Read globally indexed Field data"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc/=ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif


!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy globally indexed Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid_g, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Multiple DEs per PET tests
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Grid can be created
  grid_2DE = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), &
    regDecomp=(/8,1/), gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
    name="landgrid", rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Creating a Grid with 2DEs/PET to use in Field Tests"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  field_w2DE=ESMF_FieldCreate(grid_2DE, arrayspec=arrayspec, &
           totalLWidth=(/1,1/), totalUWidth=(/1,2/), &
           name="temperature",  rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Create a field from 2DE grid and fortran dummy array"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  call ESMF_FieldGet(field_w2DE, localDe=0, farrayPtr=Farray_DE0_w, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get and fill Farray_w from field DE 0"
  Farray_DE0_w = 0.1
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get Array pointer from Field
  call ESMF_FieldGet(field_w2DE, localDe=1, farrayPtr=Farray_DE1_w, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get and fill Farray_w from field DE 1"
  Farray_DE1_w = 1.1
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Write Fortran 2DE array in Field
  call ESMF_FieldWrite(field_w2DE, fileName="field_2DE.nc",        &
       iofmt=ESMF_IOFMT_NETCDF,  &
       status=ESMF_FILESTATUS_REPLACE, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Write Fortran 2DE array in Field"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Create Field
  field_r2DE=ESMF_FieldCreate(grid_2DE, arrayspec=arrayspec, &
           name="temperature",  rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Create a globally indexed field from grid and fortran dummy array"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

  ! Make sure read-only files can be read.
  call ESMF_VMBarrier (vm)
  if (localPet == 0) then
    call c_ESMC_UtilSystem ("chmod 444 field_2DE.nc", rc)
    call c_ESMC_UtilSystem ("ls -l field_2DE.nc", rc)
  end if
  call ESMF_VMBarrier (vm)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! read array into Field.
  call ESMF_FieldRead(field_r2DE, fileName="field_2DE.nc",        &
       rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Read 2DE Field Array data"
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
!------------------------------------------------------------------------

  ! change permission back to read-write to avoid issues with test cleanup.
  call ESMF_VMBarrier (vm)
  if (localPet == 0) then
    call c_ESMC_UtilSystem ("chmod 644 field_2DE.nc", rc)
    call c_ESMC_UtilSystem ("ls -l field_2DE.nc", rc)
  end if
  call ESMF_VMBarrier (vm)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Get DE 0 Array pointer from Field
  call ESMF_FieldGet(field_r2DE, localDe=0, farrayPtr=Farray_DE0_r, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_r from field DE 0"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! DE 0 Array comparison test
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray_DE0_r == 0.1))
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
  call ESMF_FieldGet(field_r2DE, localDe=1, farrayPtr=Farray_DE1_r, &
      exclusiveLBound=exclusiveLBound, &
      exclusiveUBound=exclusiveUBound, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Get Farray_r from field DE 1"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! DE 1 Array comparison test
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (Farray_DE1_r == 1.1))
#else
  rc = ESMF_SUCCESS
#endif
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "DE 1 Array comparison test"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  write(name, *) "Destroy globally indexed 2DE Grid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_GridDestroy(grid_2DE, rc=rc)
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
! Destroy all Fields and cleanup
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Field with no data can be destroyed
  call ESMF_FieldDestroy(field, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Destroying a Field "
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!------------------------------------------------------------------------
  !NEX_UTest_Multi_Proc_Only
  ! Verifying that a Field with no data can be destroyed
  countfail = 0
  call ESMF_FieldDestroy(field_r, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_t, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_tr, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_w_nohalo, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_w, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_gw, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_w2DE, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  call ESMF_FieldDestroy(field_r2DE, rc=rc)
  if (rc /= ESMF_SUCCESS) countfail = countfail + 1
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Destroying all Fields"
  call ESMF_Test(countfail == 0, name, failMsg, result, ESMF_SRCLINE)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

10 continue

  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !-----------------------------------------------------------------------------

  end program ESMF_FieldIOUTest
