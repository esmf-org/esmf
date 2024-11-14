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
program ESMF_IO_GDALUTest

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

  type(ESMF_Mesh)        :: Mesh
  type(ESMF_Grid)        :: Grid
  type(ESMF_Field)       :: MeshField
  type(ESMF_Field)       :: GridField
  type(ESMF_ArraySpec)   :: arraySpec
  type(ESMF_RouteHandle) :: routeHandle1,routeHandle2
  real(ESMF_KIND_R4), pointer :: mptr(:), gptr(:,:)
  integer :: localDECount, lDE
  real(ESMF_KIND_R4) :: gridMax,localGridMax, meshMax,localMeshMax
  integer, allocatable :: decomptile(:,:)

!  character(len=*), parameter :: shapefileName = "data/esmf_3x3_multimesh.shp"
!  character(len=*), parameter :: shapefileName = "data/esmf_3x3_mesh.shp"
!  character(len=*), parameter :: shapefileName = "data/cb_2018_us_region_20m.shp"
  character(len=*), parameter :: shapefileName = "data/cb_2018_us_county_20m.shp"
!  character(len=*), parameter :: shapefileName = "data/test3_simple.shp"

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
  write(name, *) "Creating a cubed-sphere grid"
  Grid = ESMF_GridCreateCubedSphere(               &
                                     tileSize=180, &
                                     regDecompPTile=decomptile, &
                                     staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
                                     coordSys=ESMF_COORDSYS_SPH_RAD, &
                                     rc=rc)
  if (allocated(decomptile)) deallocate(decomptile)

  write(failMsg, *) "ESMF_GridCreateCubedSphere did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 3. Initialize & create the shapefile mesh
  !------------------------------------------------------------------------
  write(name, *) "Creating a SHP Mesh to use in Field Tests"
  mesh=ESMF_MeshCreate( shapefileName, &
                        fileformat=ESMF_FILEFORMAT_SHAPEFILE, &
                        coordSys=ESMF_COORDSYS_SPH_RAD, &
                        rc=rc)

  write(failMsg, *) "ESMF_MeshCreate did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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

  Meshfield = ESMF_FieldCreate( Mesh, &
                                arraySpec, &
                                name="DE", &
!                                name="nhgis0003_", &
                                meshLoc=ESMF_MESHLOC_ELEMENT, &
                                rc=rc)

  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    write(*,*) 'Failed at Mesh FieldCreate'
    return
  endif

  !------------------------------------------------------------------------
  ! 4a. Access the field pointers and give them some data
!  call ESMF_FieldGet( Meshfield, farrayPtr=mptr, rc=rc)
!  mptr    = 0.0
!  mptr(1) = 11.5
!  mptr = 11.5
!  write(*,*) "pet: ", localpet, " mptr: ", shape(mptr)
!  mptr => null()

  ! Get localDECount for GridField
  call ESMF_FieldGet(GridField, localDECount=localDECount, rc=rc)

  ! Loop over DEs setting them to a value
  do lDE=0,localDECount-1
     call ESMF_FieldGet(GridField, localDE=lDE, farrayPtr=gptr, rc=rc)
     gptr=lDE+1.101 ! Set the value
!     gptr = 18.
     write(*,*) 'SET DE: ', lDE, maxval(gptr)
     gptr => null()
  enddo
  
  call ESMF_MeshWrite(mesh,"shapefile_mesh",rc=rc)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 5. Create the regrid route handle
  !------------------------------------------------------------------------
  ! Regrid store
  call ESMF_FieldRegridStore( GridField, &
                              dstField=MeshField, &
                              routeHandle=routeHandle2, &
                              regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                              unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!                             checkFlag = .true., &
                              rc=rc)
!  call ESMF_FieldRegridStore( MeshField, &
!                              dstField=GridField, &
!                              routeHandle=routeHandle1, &
!                              regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
!                              unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!!                              checkFlag = .true., &
!                              rc=rc)
  write(name, *) "REGRIDSTORE"
  write(failMsg, *) "RegridStore failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 6. Read the mesh data
  !------------------------------------------------------------------------
  !  call ESMF_FieldRead( MeshField, &
  !                       fileName=shapefileName, &
  !                       iofmt=ESMF_IOFMT_SHP, &
  !                       rc=rc)
  !  write(failMsg, *) "Mesh ESMF_FieldRead failed"
  !  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  gridMax=-10000.00
  do lDE=0,localDECount-1
     call ESMF_FieldGet(GridField, localDE=lDE, farrayPtr=gptr, rc=rc)
     localGridMax=maxval(gptr)
     if (localGridMax>gridMax) gridMax=localGridMax
     gptr => null()
     write(*,*) 'pre grid DE: ', lDE, localGridMax, gridMax
  enddo

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 7a. Regrid the mesh onto the grid
  !------------------------------------------------------------------------
  !call ESMF_FieldRegrid(MeshField,GridField,routeHandle,rc=rc)

  !------------------------------------------------------------------------
  ! 7b. OR regrid the grid onto the mesh
  !------------------------------------------------------------------------
  call ESMF_FieldRegrid(GridField,MeshField,routeHandle2,rc=rc)
  write(name, *) "REGRID"
  write(failMsg, *) "Regrid failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 8. Write out the VTK files
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 9. Write the gridded data from grid to NetCDF
  !------------------------------------------------------------------------
  call ESMF_FieldWrite(MeshField, fileName="cb_2018_us_county_20m.shp",  &
       iofmt=ESMF_IOFMT_SHP,  &
       overwrite=.true.,  &
       timeslice=1, &
       status=ESMF_FILESTATUS_UNKNOWN, rc=rc)

  ! X. Dump some data after regrid

  ! Loop over GridField getting max value
  gridMax=-10000.00
  do lDE=0,localDECount-1
     call ESMF_FieldGet(GridField, localDE=lDE, farrayPtr=gptr, rc=rc)
     localGridMax=maxval(gptr)
     if (localGridMax>gridMax) gridMax=localGridMax
     gptr => null()
     write(*,*) 'post grid DE: ', lDE, localGridMax, gridMax
  enddo

  call ESMF_FieldGet(MeshField, localDECount=localDECount, rc=rc)
  do lDE=0,localDECount-1
     call ESMF_FieldGet( Meshfield, farrayPtr=mptr, rc=rc)
     localMeshMax=maxval(mptr)
     if (localMeshMax>meshMax) meshMax=localMeshMax
     mptr => null()
     write(*,*) 'mesh DE: ', lDE, localMeshMax, meshMax
  enddo

  !  write(*,*) 'SHP to Gridfield: ', maxval(gptr), maxval(mptr)
  write(*,*) 'Src (SHP) Mesh maxval=', meshMax
  write(*,*) 'Dst Grid maxval=', gridMax

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 10. Cleanup and end
  !------------------------------------------------------------------------

  call ESMF_MeshDestroy(mesh, noGarbage = .TRUE., rc=rc)
  write(failMsg, *) "ESMF_MeshDestroy of .nc mesh did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldDestroy(Gridfield, noGarbage = .TRUE., rc=rc)
  write(failMsg, *) "ESMF_FieldDestroy of GridField did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  call ESMF_FieldDestroy(Meshfield, noGarbage = .TRUE., rc=rc)
  write(failMsg, *) "ESMF_FieldDestroy of MeshField did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  ! Fin
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

end program ESMF_IO_GDALUTest
