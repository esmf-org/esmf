! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_IO_LocStreamRegrid

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_IO_MultitileUTest - Unit tests of IO on multi-tile fields / arrays
! !DESCRIPTION:
!
! The tests in this file target IO on multi-tile fields / arrays. These tests
! are designed to be run on 8 processors (due to the decompositions used in the
! tests).
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use NETCDF
  use MPI
  
  implicit none

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0
  integer :: rc

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  type(ESMF_VM) :: vm
  integer :: localPet, petCount

  type(ESMF_LocStream)   :: LocStream
  type(ESMF_Grid)        :: Grid
  type(ESMF_Field)       :: LocStrField
  type(ESMF_Field)       :: GridField
  type(ESMF_ArraySpec)   :: arraySpec
  type(ESMF_RouteHandle) :: routeHandle1,routeHandle2
  real(ESMF_KIND_R4), pointer :: mptr(:), gptr(:,:)
  integer :: localDECount, lDE
  real(ESMF_KIND_R4) :: gridMax,localGridMax, locstrMax,localLocStrMax
  integer, allocatable :: decomptile(:,:)
  integer :: dims(ESMF_MAXDIM)
  integer :: ndims

  real :: start, finish

!  character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/CROSSWALK/unzipped/oilandgas_18_2024_point_DEG.shp" 
!  character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/CROSSWALK/unzipped/oilandgas_02_2024_point_DEG.shp" 
!  character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/CROSSWALK/unzipped/combined_oilandgas_2024_points_DEG.shp" 
  character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/CROSSWALK/unzipped/oag_2X_DEG.shp"
!  character(ESMF_MAXSTR) :: filename = "/home/ilcentro/Work/NASA/ALI/data/CROSSWALK/unzipped/oag_spread_DEG.shp"

  ! NetCDF stuff
  integer :: ncid, xdimid, ydimid, xvarid, yvarid, varid, dimids

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 1. Initialize ESMF & VM
  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 2. Initialize & create the cubed sphere grid
  !------------------------------------------------------------------------
  allocate(decomptile(2,6))
  decomptile(:,1)=(/2,2/) ! Tile 1
  decomptile(:,2)=(/2,2/) ! Tile 2
  decomptile(:,3)=(/1,2/) ! Tile 3
  decomptile(:,4)=(/1,2/) ! Tile 4
  decomptile(:,5)=(/1,2/) ! Tile 5
  decomptile(:,6)=(/1,2/) ! Tile 6
  ! Create cubed sphere grid
  call cpu_time(start)
  write(name, *) "Creating a cubed-sphere grid"
  Grid = ESMF_GridCreateCubedSphere(               &
                                     tileSize=180, &
                                     regDecompPTile=decomptile, &
                                     staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
                                     coordSys=ESMF_COORDSYS_SPH_RAD, &
                                     rc=rc)
  if (allocated(decomptile)) deallocate(decomptile)

  call ESMF_GridGet(grid,dimCount=ndims,coordDimCount=dims,rc=rc)
  write(*,*) '<<>> DIMS: ', ndims,shape(dims),dims!(1),dims(2)

  write(failMsg, *) "ESMF_GridCreateCubedSphere did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call cpu_time(finish)
  write(*,*) 'timer - ESMF_GridCreateCubedSphere = ', finish-start
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 3. Initialize & create the shapefile mesh
  !------------------------------------------------------------------------
  call cpu_time(start)
  write(name, *) "Creating a SHP LocStream to use in Field Tests"
  locstream = ESMF_LocStreamCreate(filename=trim(filename),fileformat=ESMF_FILEFORMAT_SHAPEFILE, name='co2',rc=rc)
  call ESMF_ArraySpecSet(arraySpec, 1, typekind=ESMF_TYPEKIND_R4, rc=rc)
  
  write(failMsg, *) "ESMF_MeshCreate did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call cpu_time(finish)
  write(*,*) 'timer - ESMF_LocStreamCreate = ', finish-start
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 4. Initialize & create the grid and mesh fields
  !------------------------------------------------------------------------
  ! -- Grid field
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)
  GridField = ESMF_FieldCreate( Grid, &
                                arrayspec, &
                                staggerloc=ESMF_STAGGERLOC_CENTER, &
                                name="dest", &
                                rc=rc)

  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    write(*,*) 'Failed at grid FieldCreate'
    return
  endif

  ! -- Mesh field
  call ESMF_ArraySpecSet(arraySpec, 1, typekind=ESMF_TYPEKIND_R4, rc=rc)
  LocStrField  = ESMF_FieldCreate(locstream, arrayspec, name='co2', rc=rc)
  call ESMF_FieldGet(LocStrField, farrayPtr=mptr, rc=rc)
  write(*,*) 'Initialized LocStrField: ', minval(mptr), maxval(mptr)
  mptr => null()

  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    write(*,*) 'Failed at LocStream FieldCreate'
    return
  endif

  !------------------------------------------------------------------------
  ! 4a. Access the field pointers and give them some data
  ! Get localDECount for GridField
  call ESMF_FieldGet(GridField, localDECount=localDECount, rc=rc)

  ! Loop over DEs setting them to a value
  if (localPet .eq. 0) then
  do lDE=0,localDECount-1
     call ESMF_FieldGet(GridField, localDE=lDE, farrayPtr=gptr, rc=rc)
!     gptr=0. ! Set the value
     localGridMax=maxval(gptr)
     if (localGridMax>gridMax) gridMax=localGridMax
     write(*,*) 'set grid DE: ', lDE, localGridMax, gridMax, shape(gptr)
     gptr => null()
  enddo
  endif
  
  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 5. Create the regrid route handle
  !------------------------------------------------------------------------
  ! Regrid store
  call cpu_time(start)
  call ESMF_FieldRegridStore( srcField=LocStrField, &
                              dstField=GridField, &
                              routeHandle=routeHandle2, &
                              regridmethod=ESMF_REGRIDMETHOD_NEAREST_DTOS, &
                              unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!                             checkFlag = .true., &
                              rc=rc)
  write(name, *) "REGRIDSTORE"
  write(failMsg, *) "RegridStore failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call cpu_time(finish)
  write(*,*) 'timer - ESMF_FieldRegridStore = ', finish-start

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 6. Read the mesh data
  !------------------------------------------------------------------------
  call cpu_time(start)
  call ESMF_FieldRead( LocStrField, &
       fileName=fileName, &
       iofmt=ESMF_IOFMT_SHP, &
       rc=rc)
  write(name,*) 'FieldRead'
  write(failMsg, *) "LocStream ESMF_FieldRead failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call cpu_time(finish)
  write(*,*) 'timer - ESMF_FieldRead = ', finish-start
  !------------------------------------------------------------------------

  gridMax=-10000.00
  if (localPet .eq. 0) then
  do lDE=0,localDECount-1
     call ESMF_FieldGet(GridField, localDE=lDE, farrayPtr=gptr, rc=rc)
     localGridMax=maxval(gptr)
     if (localGridMax>gridMax) gridMax=localGridMax
!     gptr = 0.
     gptr => null()
     write(*,*) 'pre grid DE: ', lDE, localGridMax, gridMax, shape(gptr)
  enddo
  endif

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 7a. Regrid the mesh onto the grid
  !------------------------------------------------------------------------
  !call ESMF_FieldRegrid(LocStrField,GridField,routeHandle,rc=rc)

  !------------------------------------------------------------------------
  ! 7b. OR regrid the grid onto the mesh
  !------------------------------------------------------------------------
  call cpu_time(start)
  call ESMF_FieldRegrid(LocStrField,GridField,routeHandle2,rc=rc)
  write(name, *) "REGRID"
  write(failMsg, *) "Regrid failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call cpu_time(finish)
  write(*,*) 'timer - ESMF_FieldRegrid = ', finish-start

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 8. Write out the VTK files
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 9. Write the gridded data from grid to NetCDF
  !------------------------------------------------------------------------
  call ESMF_FieldWrite(GridField, fileName="test*.nc",  &
!       iofmt=ESMF_IOFMT_NETCDF,  &
       overwrite=.true.,  &
!       timeslice=1, &
       status=ESMF_FILESTATUS_UNKNOWN, rc=rc)

  ! X. Dump some data after regrid

  ! Loop over GridField getting max value
  gridMax=-10000.00
  !if (localPet .eq. 0) then
  do lDE=0,localDECount-1
     call ESMF_FieldGet(GridField, localDE=lDE, farrayPtr=gptr, rc=rc)
     localGridMax=maxval(gptr)
     if (localGridMax>gridMax) gridMax=localGridMax
     write(*,*) 'post grid DE: ', lDE, minval(gptr), maxval(gptr), sum(gptr)
     gptr => null()
  enddo
  !endif

  call ESMF_FieldGet(LocStrField, localDECount=localDECount, rc=rc)
  !if (localPet .eq. 0) then
  do lDE=0,localDECount-1
     call ESMF_FieldGet( Locstrfield, farrayPtr=mptr, rc=rc)
     localLocStrMax=maxval(mptr)
     if (localLocStrMax>locstrMax) locstrMax=localLocStrMax
     write(*,*) 'locstr DE: ', lDE, minval(mptr), maxval(mptr), sum(mptr), shape(mptr)
     mptr => null()
  enddo
  !endif

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 10. Cleanup and end
  !------------------------------------------------------------------------

  call ESMF_LocStreamDestroy(locstream, noGarbage = .TRUE., rc=rc)
  write(failMsg, *) "ESMF_LocStrDestroy of .nc locstr did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldDestroy(Gridfield, noGarbage = .TRUE., rc=rc)
  write(failMsg, *) "ESMF_FieldDestroy of GridField did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldDestroy(LocStrfield, noGarbage = .TRUE., rc=rc)
  write(failMsg, *) "ESMF_FieldDestroy of LocStrField did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  ! Fin
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

end program ESMF_IO_LocStreamRegrid
