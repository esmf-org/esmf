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
  integer :: localPet
  type(ESMF_Mesh) :: mesh

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
  character(len=*), parameter :: fileNameFields = "data/cb_2018_us_region_20m.shp"
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

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R4), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  ! Set number of nodes
  numNodes=9
  
  ! Allocate and fill the node id array.
  allocate(nodeIds(numNodes))
  nodeIds=(/1,2,3,4,5,6,7,8,9/)
  
  ! Allocate and fill node coordinate array.
  ! Since this is a 2D Mesh the size is 2x the
  ! number of nodes.
  allocate(nodeCoords(2*numNodes))
  nodeCoords=(/0.0,0.0, & ! node id 1
               45.0,0.0, & ! node id 2
               90.0,0.0, & ! node id 3
               0.0,45.0, & ! node id 4
               45.0,45.0, & ! node id 5
               90.0,45.0, & ! node id 6
               0.0,90.0, & ! node id 7
               45.0,90.0, & ! node id 8
               90.0,90.0 /) ! node id 9
  
  ! Allocate and fill the node owner array.
  ! Since this Mesh is all on PET 0, it's just set to all 0.
  allocate(nodeOwners(numNodes))
  nodeOwners=0 ! everything on PET 0
  
  ! Set the number of each type of element, plus the total number.
  numQuadElems=3
  numTriElems=2
  numTotElems=numQuadElems+numTriElems
  
  ! Allocate and fill the element id array.
  allocate(elemIds(numTotElems))
  elemIds=(/1,2,3,4,5/)
  
  
  ! Allocate and fill the element topology type array.
  allocate(elemTypes(numTotElems))
  elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
       ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
       ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
       ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
       ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5
  
  
  ! Allocate and fill the element connection type array.
  ! Note that entries in this array refer to the
  ! positions in the nodeIds, etc. arrays and that
  ! the order and number of entries for each element
  ! reflects that given in the Mesh options
  ! section for the corresponding entry
  ! in the elemTypes array.
  allocate(elemConn(4*numQuadElems+3*numTriElems))
  elemConn=(/1,2,5,4, &  ! elem id 1
       2,3,5,   &  ! elem id 2
       3,6,5,   &  ! elem id 3
       4,5,8,7, &  ! elem id 4
       5,6,9,8/)   ! elem id 5
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  ! Create Mesh from shape file
  write(name, *) "Creating a Mesh to use in Field Tests"
  mesh=ESMF_MeshCreate(fileNameFields, &
       fileformat=ESMF_FILEFORMAT_SHAPEFILE, &
!       coordSys=ESMF_COORDSYS_CART, &
       rc=rc)
    ! Create Mesh structure in 1 step
!>>    mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
!>>        coordSys=ESMF_COORDSYS_SPH_DEG, &
!>>         nodeIds=nodeIds, nodeCoords=nodeCoords, &
!>>         nodeOwners=nodeOwners, elementIds=elemIds,&
!>>         elementTypes=elemTypes, elementConn=elemConn, rc=rc)
!  if (rc /= ESMF_SUCCESS) return
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  call ESMF_MeshGet(mesh, spatialdim=sd, nodecount=nc, rc=rc)
  write(*,*) 'mesh dims: ', sd
  allocate(meshcoords(nc*sd))
  call ESMF_MeshGet(mesh, nodecoords=meshcoords, rc=rc)
!  write(*,*) 'mesh coords: ', meshcoords

  !------------------------------------------------------------------------
  ! setup dest. grid
!  write(*,*) 'dest grid'
  dst_minx = -180
  dst_miny = -90

  dst_maxx = 179.9
  dst_maxy = 90

  dst_nx = 6
  dst_ny = 6
!  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/2,2/), &
!                                  coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
!                                  rc=rc)
  allocate(decomptile(2,6))
  decomptile(:,1)=(/2,2/) ! Tile 1
  decomptile(:,2)=(/2,2/) ! Tile 2
  decomptile(:,3)=(/1,2/) ! Tile 3
  decomptile(:,4)=(/1,2/) ! Tile 4
  decomptile(:,5)=(/1,2/) ! Tile 5
  decomptile(:,6)=(/1,2/) ! Tile 6
  ! Create cubed sphere grid
  dstGrid = ESMF_GridCreateCubedSphere(tileSize=48, regDecompPTile=decomptile, &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CENTER/), &
       coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
  
  if (rc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
!!    write(*,*) 'Failed at GridCreate'
    return
  endif

!>>  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
!>>  if (rc /=ESMF_SUCCESS) then
!>>    rc=ESMF_FAILURE
!>>!    write(*,*) 'Failed at GridAddCoord'
!>>    return
!>>  endif

!  write(*,*) 'Fin grid'
  
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
  
  ! Get memory and set coords for dst
  do lDE=0,localDECount-1

     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=rc)
     if (rc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        write(*,*) 'fail at GetCoord1'
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=rc)
     if (rc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        write(*,*) 'fail at GetCoord2'
        return
     endif

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=rc)
     if (rc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        write(*,*) 'fail at fieldget'
        return
     endif

     farrayPtr = 0.0e0

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
!        farrayPtrXC(i1,i2) = -180. + (REAL(i1-1)*dst_dx)
!        farrayPtrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
!        farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
!        farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

        write(*,*) farrayPtrXC(i1,i2), farrayPtrYC(i1,i2)
        ! initialize destination field
!       farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

!  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
!  if (rc /=ESMF_SUCCESS) then
!    write(*,*) 'Failed at GridAddCoord'
!    rc=ESMF_FAILURE
!    return
!  endif
  !------------------------------------------------------------------------


  write(name, *) "Get a multi-tile Field"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  call ESMF_ArraySpecSet(arraySpec, 1, typekind=ESMF_TYPEKIND_R4, rc=rc)
  if (rc /= ESMF_SUCCESS)     write(*,*) 'Failed at arrayspecset'

  field = ESMF_FieldCreate(mesh, arraySpec, name="GEOID", meshLoc=ESMF_MESHLOC_ELEMENT, rc=rc)

  if (rc /= ESMF_SUCCESS) write(*,*) 'Failed at mesh fieldcreate'

  call ESMF_FieldGet(field, farrayPtr=fieldReadData, rc=rc)
  call ESMF_FieldGet(dstField, farrayPtr=dstFieldData, rc=rc)

  dstFieldData = 0.
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

  !! Write mesh for debugging
  call ESMF_MeshWrite(mesh,"meshinit",rc=rc)
  call ESMF_GridWriteVTK(dstGrid,filename="gridtest",rc=rc)
  write(failMsg, *) "GridWriteVTK failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!  call ESMF_FieldGet(field, dimCount=sd, rank=nc)
!  write(*,*) 'mesh field dim/rank: ', sd, nc
!
!  call ESMF_FieldGet(dstField, dimCount=sd, rank=nc)
!  write(*,*) 'grid field dim/rank: ', sd, nc

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
   call ESMF_FieldRegridStore( &
          field, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=rc)
  write(failMsg, *) "RegridStore failed"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!  if (rc /=ESMF_SUCCESS) then
!      rc=ESMF_FAILURE
!      return
!   endif

  call ESMF_MeshWrite(mesh,"meshtest",rc=rc)

  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

end program ESMF_IO_GDALUTest
