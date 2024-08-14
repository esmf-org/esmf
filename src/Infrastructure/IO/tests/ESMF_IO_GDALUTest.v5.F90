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
  type(ESMF_RouteHandle) :: routeHandle
  real(ESMF_KIND_R4), pointer :: mptr(:), gptr(:,:)

  integer, allocatable :: decomptile(:,:)

!  character(len=*), parameter :: shapefileName = "data/esmf_3x3_multimesh.shp"
!  character(len=*), parameter :: shapefileName = "data/esmf_3x3_mesh.shp"
  character(len=*), parameter :: shapefileName = "data/cb_2018_us_county_20m.shp"

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
                                     staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CORNER/), &
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
                                name="DistFld", &
                                meshLoc=ESMF_MESHLOC_ELEMENT, &
                                rc=rc)

  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    write(*,*) 'Failed at Mesh FieldCreate'
    return
  endif

  !------------------------------------------------------------------------
  ! 4a. Access the field pointers and give them some data
  call ESMF_FieldGet( Meshfield, farrayPtr=mptr, rc=rc)
  mptr    = 0.0
  mptr(1) = 11.5
  write(*,*) "pet: ", localpet, " mptr: ", shape(mptr)
  mptr => null()

  call ESMF_FieldGet( Gridfield, farrayPtr=gptr, rc=rc)
  gptr = 0.0
  gptr => null()

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 5. Create the regrid route handle
  !------------------------------------------------------------------------
  ! Regrid store
   call ESMF_FieldRegridStore( MeshField, &
                               dstField=GridField, &
                               routeHandle=routeHandle, &
                               regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                               unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                               rc=rc)
  write(failMsg, *) "RegridStore failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 6. Read the mesh data
  !------------------------------------------------------------------------
!>>  call ESMF_FieldRead( MeshField, &
!>>                       fileName=shapefileName, &
!>>                       iofmt=ESMF_IOFMT_SHP, &
!>>                       rc=rc)
!>>  write(failMsg, *) "Mesh ESMF_FieldRead failed"
!>>  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 7. Regrid the mesh onto the grid
  !------------------------------------------------------------------------
  call ESMF_FieldRegrid(MeshField,GridField,routeHandle,rc=rc)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 8. Write out the VTK files
  !------------------------------------------------------------------------
  call ESMF_MeshWrite(mesh,"shapefile_mesh",rc=rc)

  !------------------------------------------------------------------------
  !------------------------------------------------------------------------
  ! 9. Write the gridded data from grid to NetCDF
  !------------------------------------------------------------------------
!>>  if (localpet == 0 ) then
!>>     !------------------------------------------------------------------------
!>>     ! Write the regridded field to file
!>>     ! -- Create file
!>>     rc = nf90_create("test.nc",NF90_CLOBBER, ncid)
!>>     ! -- Define dims, etc
!>>     rc = nf90_def_dim(ncid, "lon", 180,  xdimid)
!>>     rc = nf90_def_dim(ncid, "lat", 1080, ydimid)
!>>     ! -- Define coords
!>>     rc = nf90_def_var(ncid, "lon", NF90_REAL, xdimid, xvarid)
!>>     rc = nf90_def_var(ncid, "lat", NF90_REAL, ydimid, yvarid)
!>>     ! -- Define variable
!>>     rc = nf90_def_var(ncid, "DistFld", NF90_FLOAT, dimids, varid)
!>>     ! -- Write coordinates
!>>     ! -- Write data     
!>>     ! -- Close file
!>>     rc = nf90_close(ncid)
!>>  endif

!>>  call ESMF_FieldWrite(GridField, fileName="mesh#.nc",  &
!>>       iofmt=ESMF_IOFMT_NETCDF,  &
!>>       overwrite=.true.,  &
!>>       status=ESMF_FILESTATUS_UNKNOWN, rc=rc)

  ! X. Dump some data after regrid

  call ESMF_FieldGet( Gridfield, farrayPtr=gptr, rc=rc)
  call ESMF_FieldGet( Meshfield, farrayPtr=mptr, rc=rc)
  write(*,*) 'SHP to Gridfield: ', maxval(gptr), maxval(mptr)
  gptr => null()
  mptr => null()

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
