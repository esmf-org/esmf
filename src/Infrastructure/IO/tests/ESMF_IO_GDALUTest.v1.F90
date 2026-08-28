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
  type(ESMF_Mesh) :: mesh, esmfmesh

  ! Fields used for reading:
  !
  ! The following fields make up the field bundle:
  type(ESMF_Field) :: field1Read, field2Read, field1CopyRead, field4dRead
  real(ESMF_KIND_R4), pointer :: field1ReadData(:,:), field2ReadData(:,:), field1CopyReadData(:,:), field4dReadData(:,:,:,:)
  type(ESMF_FieldBundle) :: fieldBundleRead
  ! This field is not in the field bundle:
  type(ESMF_Field) :: field
  real(ESMF_KIND_R4), pointer :: fieldReadData(:), dstFieldData(:,:)
  real(ESMF_KIND_R8), allocatable :: meshcoords(:)
  integer sd,nc
  integer, allocatable :: decomptile(:,:)

  type(ESMF_ArraySpec) :: arraySpec

!  character(len=*), parameter :: fileNameFields = "data/complex_3.shp"
!  character(len=*), parameter :: fileNameFields = "data/cb_2018_us_county_20m.shp"
!  character(len=*), parameter :: fileNameFields = "data/cb_2018_us_region_20m.shp"
  character(len=*), parameter :: fileNameFields = "data/esmf_3x3_mesh.shp"
!  character(len=*), parameter :: fileNameFields = "data/cb_2018_us_state_5m.shp"
!  character(len=*), parameter :: fileNameFields = "data/States_shapefile.shp"

  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_Array) :: gridXCoord, gridYCoord
  type(ESMF_RouteHandle) :: routeHandle
  integer dst_nx, dst_ny
  integer :: lDE, localDECount
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  real(ESMF_KIND_R4) :: dst_minx,dst_miny
  real(ESMF_KIND_R4) :: dst_maxx,dst_maxy
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R4), pointer :: farrayPtr(:,:),farrayPtr2(:,:),farrayPtr1D(:)

  ! NetCDF stuff
  integer :: ncid, xdimid, ydimid, xvarid, yvarid, varid, dimids

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
  
! Create Mesh from shape file
  write(name, *) "Creating a Mesh to use in Field Tests"
  mesh=ESMF_MeshCreate(fileNameFields, &
       fileformat=ESMF_FILEFORMAT_SHAPEFILE, &
       coordSys=ESMF_COORDSYS_SPH_RAD, &!       coordSys=ESMF_COORDSYS_CART, &
       rc=rc)

  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_MeshGet(mesh, spatialdim=sd, nodecount=nc, rc=rc)
  write(*,*) 'mesh dims: ', sd
  allocate(meshcoords(nc*sd))
  call ESMF_MeshGet(mesh, nodecoords=meshcoords, rc=rc)
!  write(*,*) 'mesh coords: ', meshcoords

  !------------------------------------------------------------------------
  ! setup dest. grid
  allocate(decomptile(2,6))
  decomptile(:,1)=(/2,2/) ! Tile 1
  decomptile(:,2)=(/2,2/) ! Tile 2
  decomptile(:,3)=(/1,2/) ! Tile 3
  decomptile(:,4)=(/1,2/) ! Tile 4
  decomptile(:,5)=(/1,2/) ! Tile 5
  decomptile(:,6)=(/1,2/) ! Tile 6
  ! Create cubed sphere grid
  dstGrid = ESMF_GridCreateCubedSphere(tileSize=180, regDecompPTile=decomptile, &
       staggerLocList=(/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CORNER/), &
       coordSys=ESMF_COORDSYS_SPH_RAD, rc=rc)
  
  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
!!    write(*,*) 'Failed at GridCreate'
    return
  endif

  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R4, rc=rc)

  dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
       staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=rc)
  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    write(*,*) 'Failed at grid FieldCreate'
    return
  endif

  call ESMF_GridGet(dstGrid, localDECount=localDECount, dimCount=sd, rc=rc)
  write(*,*) 'grid dims: ', sd
  
  !------------------------------------------------------------------------


  write(name, *) "Get a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_ArraySpecSet(arraySpec, 1, typekind=ESMF_TYPEKIND_R4, rc=rc)
  if (rc /= ESMF_SUCCESS)     write(*,*) 'Failed at arrayspecset'

  field = ESMF_FieldCreate(mesh, arraySpec, name="DistFld", meshLoc=ESMF_MESHLOC_ELEMENT, rc=rc)
  if (rc /= ESMF_SUCCESS) write(*,*) 'Failed at mesh fieldcreate'

  call ESMF_FieldGet(field, farrayPtr=fieldReadData, rc=rc)
  fieldReadData = 100.

!  dstFieldData = 0.
#if (defined ESMF_PIO && (defined ESMF_GDAL))
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc == ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  !EX_UTest_Multi_Proc_Only

  write(name, *) "Read a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_FieldRead(field, fileName=fileNameFields, iofmt=ESMF_IOFMT_SHP, rc=rc)
  write(*,*) 'Field read: ', rc

  !! Write mesh for debugging
  call ESMF_MeshWrite(mesh,"shpmesh",rc=rc)

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
   call ESMF_FieldRegridStore( &
          field, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=rc)
  write(failMsg, *) "RegridStore failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_FieldRegrid(field,dstField,routeHandle,rc=rc)
  write(*,*) 'FieldRegrid: ', rc
  call ESMF_FieldGet(dstField, farrayPtr=dstFieldData, rc=rc)
  write(*,*) 'FieldGet: ', rc, maxval(fieldReadData), maxval(dstFieldData)
  
  call ESMF_MeshWrite(mesh,"shpmesh_afterregridstore",rc=rc)

  !------------------------------------------------------------------------
  ! I/O
  ! 1) get the coords to root pet

  if (localpet == 0 ) then
     !------------------------------------------------------------------------
     ! Write the regridded field to file
     ! -- Create file
     rc = nf90_create("test.nc",NF90_CLOBBER, ncid)
     ! -- define dims, etc
     rc = nf90_def_dim(ncid, "lon", 180,  xdimid)
     rc = nf90_def_dim(ncid, "lat", 1080, ydimid)
     ! -- define coords
     rc = nf90_def_var(ncid, "lon", NF90_REAL, xdimid, xvarid)
     rc = nf90_def_var(ncid, "lat", NF90_REAL, ydimid, yvarid)
     ! -- define variable
     rc = nf90_def_var(ncid, "DistFld", NF90_FLOAT, dimids, varid)
     ! -- Write data     
     ! -- close file
     rc = nf90_close(ncid)
  endif

  write(*,*) 'FieldWrite: ', rc
 
  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

end program ESMF_IO_GDALUTest
