! $Id: ESMF_MeshEx.F90,v 1.12 2008/08/29 17:09:42 dneckels Exp $
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
!ESMF____EXAMPLE        String used by test script to count examples.
!==============================================================================




!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

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
    '$Id: ESMF_MeshEx.F90,v 1.12 2008/08/29 17:09:42 dneckels Exp $'
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

  integer, allocatable :: nodeId(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoord(:)
  integer, allocatable :: nodeOwner(:)

  integer, allocatable :: elemId(:)
  integer, allocatable :: elemType(:)
  integer, allocatable :: elemConn(:)

  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Field)  :: srcField


  finalrc = ESMF_SUCCESS
  call  ESMF_Initialize(vm=vm, rc=rc)

  ! Temporary bridge to mesh code initialization
  !call C_ESMC_MeshInit("MeshTest", 1)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  !-----------------------------------------------------------------------------
!  !NEX_Ex
  write(name, *) "Test GridToMesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

!  meshSrc = ESMF_MeshCreate(2,3,rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call C_ESMC_MeshVTKHeader("data/testmesh", num_elem, num_node, conn_size, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !print *, 'num_elem:', num_elem, 'num_node:', num_node, 'conn_size:', conn_size

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

  ! Declare the nodes
!  call ESMF_MeshAddNodes(meshSrc, nodeId, nodeCoord, nodeOwner, rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Declare the elements
!  call ESMF_MeshAddElements(meshSrc, elemId, elemType, elemConn, rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Write the mesh for debug
!  call ESMF_MeshWrite(meshSrc, "outmesh", rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  call ESMF_MeshDestroy(meshSrc, rc)
!  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Now do the same, but with the all in one function
  meshSrc = ESMF_MeshCreate(2,3,nodeId, nodeCoord, nodeOwner, &
                           elemId, elemType, elemConn, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Write the mesh for debug
  call ESMF_MeshWrite(meshSrc, "outmesh1", rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create a field on the Mesh
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

  srcField = ESMF_FieldCreate(meshSrc, arrayspec, &
         name="test_var", rc=rc)
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
