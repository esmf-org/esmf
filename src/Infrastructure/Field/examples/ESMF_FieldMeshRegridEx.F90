! $Id: ESMF_FieldMeshRegridEx.F90,v 1.11.2.1 2010/02/05 19:55:38 svasquez Exp $
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
program ESMF_MeshEx

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================

!BOE
! \subsubsection{Example: Create a Mesh}~\label{sec:usage:ex:adv:reg}
!
! This example uses the mesh creation API to create a mesh, and then to create
! a field on the mesh.
!EOE

!  !PROGRAM: ESMF_MeshEx - Example of Mesh creation.
!
!  !DESCRIPTION: 
!
! This program shows examples of Mesh creation



#include "ESMF_Macros.inc"

! !USES:
  use ESMF_Mod
  use ESMF_TestMod     ! test methods
  use ESMF_MeshMod
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_GridUtilMod

  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_FieldMeshRegridEx.F90,v 1.11.2.1 2010/02/05 19:55:38 svasquez Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: finalrc, rc, petCount,localPet,localrc

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_VM) :: vm
  type(ESMF_Mesh) :: meshSrc
  integer :: num_elem, num_node, conn_size
  integer :: i
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, xtmp, x, y


!BOE
! The following arrays are used to declare the mesh to the ESMF framework.
!EOE
!BOC
  integer, allocatable :: nodeId(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoord(:), nodeCoord1(:)
  integer, allocatable :: nodeOwner(:)

  integer, allocatable :: elemId(:)
  integer, allocatable :: elemType(:)
  integer, allocatable :: elemConn(:)
!EOC

  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Array)     :: dstArray, srcArray
  type(ESMF_Field)  :: dstField, srcField
  type(ESMF_Grid) :: gridDst
  integer dst_nx, dst_ny
  integer num_arrays, lDE, localDECount
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: clbnd1(1),cubnd1(1)
  integer :: i1,i2

  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:)
  real(ESMF_KIND_R8), pointer :: fptr1(:)
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  type(ESMF_RouteHandle) :: routeHandle



  finalrc = ESMF_SUCCESS
  call  ESMF_Initialize(vm=vm, rc=rc)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  if (petCount .ne. 6) then
    print *, 'Example must run on 6 processors since mesh is stored as such'
    call ESMF_Finalize(terminationflag=ESMF_ABORT)
  endif

  write(name, *) "Test GridToMesh"

  ! Call this function if you want to turn on mesh logging.  Use
  ! Par::Out() in the mesh code to print to the file for a specific processor.
  !i1 = 1
  !call C_ESMC_MeshInit("MESHREGRID", i1)

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

!BOE
! \subsubsection{Mesh Creation as a three step process}
!
! Here we create a mesh first by defining the mesh structure, then adding
! nodes (with node coordinates) and finally adding the element types and
! connectivities.
! 
! The benefit of having this as a three step process is that the node arrays
! may be freed (if desired) before creating and using the element arrays. This
! may be desirable in a low-memory scenario.
!EOE
!BOC
!EOC

  call C_ESMC_MeshVTKHeader("ESMF_FieldMeshRegridExData/testmesh1", &
    num_elem, num_node, conn_size, rc)
!BOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Allocate the arrays to describe Mesh

  allocate(nodeId(num_node))
  allocate(nodeCoord(3*num_node))
  allocate(nodeCoord1(2*num_node))
  allocate(nodeOwner(num_node))

  allocate(elemId(num_elem))
  allocate(elemType(num_elem))
  allocate(elemConn(conn_size))


  ! Get the arrays from the test mesh
  call C_ESMC_MeshVTKBody("ESMF_FieldMeshRegridExData/testmesh1", &
    nodeId(1), nodeCoord(1), nodeOwner(1), elemId(1), elemType(1), &
    elemConn(1), rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! VTKBody returns zero based elemConn, so make them 1 based
  do i=1, conn_size
    elemConn(i) = elemConn(i)+1
  enddo

  ! VTK reads coordinates as 3d, but mesh interface expects them to be
  ! of the same dim as the spatial dim of mesh, which I am declaring as
  ! 2d here so that it matches with the grid.
  ! There is nothing wrong with having the parametric dim of the mesh
  ! be 2 and the spatial to be 3.  In math we call this a manifold.
  ! The library supports regridding between manifolds and from a 3d
  ! mesh to a manifold.
  do i =1,num_node
    nodeCoord1(2*(i-1)+1) = nodeCoord(3*(i-1)+1)
    nodeCoord1(2*(i-1)+2) = nodeCoord(3*(i-1)+2)
  enddo

!BOE
! Now create the mesh
!EOE
!BOC
  meshSrc = ESMF_MeshCreate(2,2,nodeId, nodeCoord1, nodeOwner, &
                           elemId, elemType, elemConn, rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Here we create a field that lives on the nodes of the mesh.
!
! Create a field on the Mesh
!EOE
!BOC
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

  srcField = ESMF_FieldCreate(meshSrc, arrayspec, &
         name="test_var", rc=rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !call ESMF_FieldPrint(srcField)


  ! Done with node arrays, zap.  We are going to keep
  ! the nodeCoord around so we can assign a function to
  ! the ESMF_Field.
  deallocate(nodeId)

  ! free the element arrays
  deallocate(elemId)
  deallocate(elemType)
  deallocate(elemConn)


  ! Set up a grid
  dst_nx = 75
  dst_ny = 50
  dst_dx = 1.0 / (REAL(dst_nx)-1)
  dst_dy = 1.0 / (REAL(dst_ny)-1)

  gridDst=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/petCount,1/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   dstField = ESMF_FieldCreate(gridDst, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dst", &
                         rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridAddCoord(gridDst, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  ! Get number of local DEs
  call ESMF_GridGet(gridDst, localDECount=localDECount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)



  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords 
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        x = REAL((i1-1)*dst_dx)
        y = REAL((i2-1)*dst_dy)
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y

        fptr(i1,i2) = 0.    ! set destination field to zero
     enddo
     enddo

     ! Set field values

  enddo    ! lDE


  ! Load a function on the mesh.  This is a bit weird.  In reality, the
  ! user has their own data structure and fills out srcField.  Here we
  ! use nodeCoords1 and fill out the fill, since the mesh api guarentees
  ! that the ordering of the field will match that of the declaration
  ! (something I just added).
  ! We skip non-locally owned nodes, since the field stores only the
  ! locally owned indicies.
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  lDE = 0
  call ESMF_FieldGet(srcField, lDE, fptr1, computationalLBound=clbnd1, &
                          computationalUBound=cubnd1,  rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !print *, localPet, 'cbnd', clbnd1(1), cubnd1(1)
  i1 = 1
  do i=1, num_node

    if (nodeOwner(i) .ne. localPet) cycle

    x = nodeCoord1(2*(i-1)+1)
    y = nodeCoord1(2*(i-1)+2)

    fptr1(i1) = sin(2*3.14*x) + cos(4*3.14*y)

    i1 = i1 + 1

  enddo

  !print *, localPet, i1

  ! Form the regrid operator
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                             routeHandle=routeHandle, &
                             regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
                             rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Apply the regrid operator
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Write out the mesh
  call ESMF_MeshIO(vm, gridDst, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Release the operator
  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! clean up
  call ESMF_MeshDestroy(meshSrc, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridDestroy(gridDst, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  deallocate(nodeCoord)
  deallocate(nodeCoord1)
  deallocate(nodeOwner)

10   continue
  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_FieldMeshRegridEx.F90"
  else
    print *, "FAIL: ESMF_FieldMeshRegridEx.F90"
  endif



end program ESMF_MeshEx
