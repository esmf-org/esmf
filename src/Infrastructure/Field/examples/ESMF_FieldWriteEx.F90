! Id: ESMF_FieldWriteEx.F90,v 1.5 2004/06/16 14:03:13 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_FieldWriteEx

!-------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
!
! !DESCRIPTION:
! Example/test code which writes out a Field to a netCDF file.
! See the following code fragments for examples of how to output a Field.
!
  ! ESMF Framework module
  use ESMF_Mod
  implicit none
    
  ! Local variables
  integer :: status, rc
  type(ESMF_GridHorzStagger) :: horz_stagger
  integer, dimension(2) :: counts
  real(ESMF_KIND_R8), dimension(2) :: min, max
  type(ESMF_DELayout) :: layout
  type(ESMF_Grid) :: grid
  type(ESMF_VM) :: vm
  character (len = ESMF_MAXSTR) :: name
  real (ESMF_KIND_R8), dimension(2) :: origin
  real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: u2
  type(ESMF_Field) :: field_u2
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Array) :: array_temp
  type(ESMF_Time) :: timestamp
  type(ESMF_IOSpec) :: iospec
  integer i,j
!EOC

  integer :: finalrc
  finalrc = ESMF_SUCCESS
  
  call ESMF_Initialize(rc=rc)

  if (rc.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_Initialize' failed"
  endif
  

  call ESMF_VMGetGlobal(vm, rc)

  if (rc.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "' call ESMF_VMGetGlobal' failed"
  endif
    

!-------------------------------------------------------------------------
!   ! Example 1:
!   !
!   !  The user creates a simple horizontal Grid internally by passing all
!   !  necessary information through the CreateInternal argument list.

  counts(1) = 10
  counts(2) = 12
  horz_stagger = ESMF_GRID_HORZ_STAGGER_A
  min(1) = 0.0
  max(1) = 10.0
  min(2) = 0.0
  max(2) = 12.0
  name = "test grid 1"
 
  ! Create a 2 x 2 layout for the Grid
  layout = ESMF_DELayoutCreate(vm, (/ 2, 2 /), rc=status)

 
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'layout=ESMF_DELayoutCreate' failed"
  endif


  grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=name, rc=status)
      if (status.NE.ESMF_SUCCESS) then
         print*, "'call ESMF_GridCreateHorzXYUni' failed"
         finalrc = ESMF_FAILURE
      else
         print*, "'call ESMF_GridCreateHorzXYUni' succeeded"
      end if

  call ESMF_GridDistribute(grid, delayout=layout, rc=status)

 
  if (status.NE.ESMF_SUCCESS) then
     print*, "'call ESMF_GridDistribute' failed"
         finalrc = ESMF_FAILURE
      else
         print*, "'call ESMF_GridDistribute' succeeded"
  endif
  
!BOE
!\subsubsection{Write Field data to a netCDF File}

! This example shows how to set the options and write an {\tt ESMF\_Field}
! out to a netCDF file.
!EOE

!BOC
  call ESMF_IOSpecSet(iospec, filename='foo.nc', &
                      iofileformat=ESMF_IO_FILEFORMAT_UNSPECIFIED, &
                      rc=status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     print*, "'call ESMF_IOSpecSet' failed"
     finalrc = ESMF_FAILURE
  else
     print*, "'call ESMF_IOSpecSet' succeeded"
  endif

  call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
       kind=ESMF_R4, rc=status)
  if (status.NE.ESMF_SUCCESS) then
     print*, "' call ESMF_ArraySpecSet' failed"
     finalrc = ESMF_FAILURE
  else
     print*, "' call ESMF_ArraySpecSet' succeeded"
  endif
  
  field_u2  = ESMF_FieldCreate(grid, arrayspec, allocflag=ESMF_ALLOC, &
       horzRelloc=ESMF_CELL_CENTER, &
       haloWidth=0, name="u2", iospec=iospec, rc=status)
  if (status.NE.ESMF_SUCCESS) then
     print*, "'field_u2  = ESMF_FieldCreate' failed"
  else
     print*, "'field_u2  = ESMF_FieldCreate' succeeded"
  endif

!!$  call ESMF_FieldGetDataPointer(field_u2, u2, rc=status)
  call ESMF_FieldGetArray( field_u2, array_temp, rc=status)
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_FieldGetArray( field_u2, array_temp, rc=status)' failed"
  endif

call ESMF_ArrayPrint(array_temp)

  call ESMF_ArrayGetData(array_temp, u2, ESMF_DATA_REF, status)
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_ArrayGetData(array_temp, u2, ESMF_DATA_REF, status)' failed"
  endif

  print *,'testWRFWrite u2 (5,6)   = ',u2(5,6)

!!$  u2 = 999.0

!!$     do j= 6, 6
!!$        do i= 5, 5
!!$           u2(i,j) = float(10*i+j)
!!$        enddo
!!$     enddo
!!$
  u2(5,6) = float( 10*5+6)

!!$  print*, shape(u2)

  print *,'testWRFWrite u2 (5,6)   = ',u2(5,6)

  call ESMF_ArrayPrint(array_temp)

!BOC
  call ESMF_TimeSet(timestamp, calendarType=ESMF_CAL_GREGORIAN, rc=status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_TimeSet(timestamp, calendarType=ESMF_CAL_GREGORIAN, status)' failed"
  else
     print*, "'call ESMF_TimeSet(timestamp, calendarType=ESMF_CAL_GREGORIAN, status)' succeeded"
  endif

!BOC
  call ESMF_TimeSyncToRealTime(timestamp, status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_TimeSyncToRealTime(timestamp, status)' failed"
  else
     print*, "'call ESMF_TimeSyncToRealTime(timestamp, status)' succeeded"
  endif

!BOC
  call ESMF_FieldWrite( field_u2, iospec, timestamp, status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     print*, "'call ESMF_FieldWrite( field_u2, iospec, timestamp, status)' failed"
  else
     print*, "'call ESMF_FieldWrite( field_u2, iospec, timestamp, status)' succeeded"
  endif
  

      print *, "Grid example 1 returned"

      call ESMF_GridDestroy(grid, rc)

      print *, "Grid example 1 destroyed"


      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
          print*, "'call ESMF_GridDestroy' failed"
      end if


      if (rc.ne.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
          print*, "'call ESMF_Finalize' failed"
      end if

     if (finalrc.eq.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldWriteEx.F90"
     else
        print *, "FAIL: ESMF_FieldWriteEx.F90"
     end if

      call ESMF_Finalize(rc)
!BOC
   end program ESMF_FieldWriteEx
!EOC   
