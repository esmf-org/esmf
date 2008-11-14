! $Id: ESMF_MeshEx.F90,v 1.14 2008/11/14 05:24:11 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
!ESMF_EXAMPLE        String used by test script to count examples.
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
    '$Id: ESMF_MeshEx.F90,v 1.14 2008/11/14 05:24:11 theurich Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: finalrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_VM) :: vm
  type(ESMF_Mesh) :: meshSrc
  integer :: num_elem, num_node, conn_size
  integer :: i

!BOE
! The following arrays are used to declare the mesh to the ESMF framework.
!EOE
!BOC
  integer, allocatable :: nodeId(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoord(:)
  integer, allocatable :: nodeOwner(:)

  integer, allocatable :: elemId(:)
  integer, allocatable :: elemType(:)
  integer, allocatable :: elemConn(:)
!EOC

  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Field)  :: srcField


  finalrc = ESMF_SUCCESS
  call  ESMF_Initialize(vm=vm, rc=rc)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  write(name, *) "Test GridToMesh"

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
  meshSrc = ESMF_MeshCreate(2,3,rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call C_ESMC_MeshVTKHeader("data/testmesh", num_elem, num_node, conn_size, rc)
!BOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Allocate the arrays to describe Mesh

  allocate(nodeId(num_node))
  allocate(nodeCoord(3*num_node))
  allocate(nodeOwner(num_node))

  allocate(elemId(num_elem))
  allocate(elemType(num_elem))
  allocate(elemConn(conn_size))


  ! Get the arrays from the test mesh
  call C_ESMC_MeshVTKBody("data/testmesh", nodeId(1), nodeCoord(1), nodeOwner(1), &
          elemId(1), elemType(1), elemConn(1), rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! VTKBody returns zero based elemConn, so make them 1 based
  do i=1, conn_size
    elemConn(i) = elemConn(i)+1
  enddo

!BOE
! Declare the nodes
!EOE
!BOC
  call ESMF_MeshAddNodes(meshSrc, nodeId, nodeCoord, nodeOwner, rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Declare the elements
!EOE
!BOC
  call ESMF_MeshAddElements(meshSrc, elemId, elemType, elemConn, rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Write the mesh for debug
!EOE
!BOC
  call ESMF_MeshWrite(meshSrc, "outmesh", rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Destroy the mesh.
!EOE
!BOC
  call ESMF_MeshDestroy(meshSrc, rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! Now create, but with the all in one function.
!EOE
!BOC
  meshSrc = ESMF_MeshCreate(2,3,nodeId, nodeCoord, nodeOwner, &
                           elemId, elemType, elemConn, rc)
!EOC
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Write the mesh for debug
  call ESMF_MeshWrite(meshSrc, "outmesh1", rc)
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

  call ESMF_FieldPrint(srcField)

  call ESMF_MeshDestroy(meshSrc, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Done with node arrays, zap
  deallocate(nodeId)
  deallocate(nodeCoord)
  deallocate(nodeOwner)

  ! free the element arrays
  deallocate(elemId)
  deallocate(elemType)
  deallocate(elemConn)


10   continue
  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_MeshEx.F90"
  else
    print *, "FAIL: ESMF_MeshEx.F90"
  endif



end program ESMF_MeshEx
