! Id: ESMF_FieldWriteEx.F90,v 1.5 2004/06/16 14:03:13 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldWriteEx

!-------------------------------------------------------------------------
!ESMF_EXremoveAMPLE        String used by test script to count examples.
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
  integer :: status, rc, petcount
  type(ESMF_IGridHorzStagger) :: horz_stagger
  integer, dimension(2) :: counts
  real(ESMF_KIND_R8), dimension(2) :: min, max
  type(ESMF_DELayout) :: layout
  type(ESMF_IGrid) :: igrid
  type(ESMF_VM) :: vm
  character (len = ESMF_MAXSTR) :: name
  real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: u2
  type(ESMF_Field) :: field_u2
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Time) :: timestamp
  type(ESMF_IOSpec) :: iospec
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
!   !  The user creates a simple horizontal IGrid internally by passing all
!   !  necessary information through the CreateInternal argument list.

  counts(1) = 10
  counts(2) = 12
  horz_stagger = ESMF_IGRID_HORZ_STAGGER_A
  min(1) = 0.0
  max(1) = 10.0
  min(2) = 0.0
  max(2) = 12.0
  name = "test igrid 1"
 
  ! Create an 2 x N layout for the IGrid, if possible (if running with >= 2 PETs)
  call ESMF_VMGet(vm, petCount=petcount, rc=rc)
  if (petcount .ge. 2) then
    layout = ESMF_DELayoutCreate(vm, (/ 2, petcount/2 /), rc=status)
  else
    layout = ESMF_DELayoutCreate(vm, (/ 1, 1 /), rc=status)
  endif

 
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'layout=ESMF_DELayoutCreate' failed"
  endif


  igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=min, &
                              maxGlobalCoordPerDim=max, &
                              horzStagger=horz_stagger, &
                              name=name, rc=status)
      if (status.NE.ESMF_SUCCESS) then
         print*, "'call ESMF_IGridCreateHorzXYUni' failed"
         finalrc = ESMF_FAILURE
      end if

  call ESMF_IGridDistribute(igrid, delayout=layout, rc=status)

 
  if (status.NE.ESMF_SUCCESS) then
     print*, "'call ESMF_IGridDistribute' failed"
         finalrc = ESMF_FAILURE
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
  endif

  call ESMF_ArraySpecSet(arrayspec, rank=2, &
       typekind=ESMF_TYPEKIND_R4, rc=status)
  if (status.NE.ESMF_SUCCESS) then
     print*, "' call ESMF_ArraySpecSet' failed"
     finalrc = ESMF_FAILURE
  endif
  
  field_u2  = ESMF_FieldCreate(igrid, arrayspec, &
       horzRelloc=ESMF_CELL_CENTER, &
       haloWidth=0, name="u2", iospec=iospec, rc=status)
  if (status.NE.ESMF_SUCCESS) then
     print*, "'field_u2  = ESMF_FieldCreate' failed"
     finalrc = ESMF_FAILURE
  endif

  call ESMF_FieldGetDataPointer(field_u2, u2, ESMF_DATA_REF, rc=status)
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_FieldGetDataPointer(field_u2, array_temp, ESMF_DATA_REF, rc=status)' failed"
  endif

!BOC
  call ESMF_TimeSet(timestamp, calendarType=ESMF_CAL_GREGORIAN, rc=status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_TimeSet(timestamp, calendarType=ESMF_CAL_GREGORIAN, status)' failed"
  endif

!BOC
  call ESMF_TimeSyncToRealTime(timestamp, status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     finalrc = ESMF_FAILURE
     print*, "'call ESMF_TimeSyncToRealTime(timestamp, status)' failed"
  endif

!BOC
  call ESMF_FieldWrite( field_u2, iospec, timestamp, status)
!EOC
  if (status.NE.ESMF_SUCCESS) then
     print*, "'call ESMF_FieldWrite( field_u2, iospec, timestamp, status)' failed"
  endif
  

      call ESMF_IGridDestroy(igrid, rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
          print*, "'call ESMF_IGridDestroy' failed"
      end if

     if (finalrc.eq.ESMF_SUCCESS) then
        print *, "PASS: ESMF_FieldWriteEx.F90"
     else
        print *, "FAIL: ESMF_FieldWriteEx.F90"
     end if

      call ESMF_Finalize(rc=rc)
!BOC
   end program ESMF_FieldWriteEx
!EOC   
