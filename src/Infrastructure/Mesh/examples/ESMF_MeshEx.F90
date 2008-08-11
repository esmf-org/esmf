! $Id: ESMF_MeshEx.F90,v 1.9 2008/08/11 22:21:27 dneckels Exp $
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
program ESMF_FieldRegridEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
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
    '$Id: ESMF_MeshEx.F90,v 1.9 2008/08/11 22:21:27 dneckels Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

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

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  call C_ESMC_MeshInit("MeshTest", 1)

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  !-----------------------------------------------------------------------------
!  !NEX_Ex
  write(name, *) "Test GridToMesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS


  meshSrc = ESMF_MeshCreate(2,3,localrc)
  write(failMsg, *) "ESMF_MeshCreate fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  
  call C_ESMC_MeshVTKHeader("data/testmesh", num_elem, num_node, conn_size, localrc)
  write(failMsg, *) "C_ESMC_MeshVTKHeader fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10


  print *, 'num_elem:', num_elem, 'num_node:', num_node, 'conn_size:', conn_size

  ! Allocate the arrays to describe Mesh

  allocate(nodeId(num_node))
  allocate(nodeCoord(3*num_node))
  allocate(nodeOwner(num_node))

  allocate(elemId(num_elem))
  allocate(elemType(num_elem))
  allocate(elemConn(conn_size))


  ! Get the arrays from the test mesh
  call C_ESMC_MeshVTKBody("data/testmesh", nodeId(1), nodeCoord(1), nodeOwner(1), &
          elemId(1), elemType(1), elemConn(1), localrc)
  write(failMsg, *) "C_ESMC_MeshVTKBody fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! VTKBody returns zero based elemConn, so make them 1 based
  do i=1, conn_size
    elemConn(i) = elemConn(i)+1
  enddo

  ! Declare the nodes
  call ESMF_MeshAddNodes(meshSrc, nodeId, nodeCoord, nodeOwner, localrc)
  write(failMsg, *) "ESMF_MeshAddNodes fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Declare the elements
  call ESMF_MeshAddElements(meshSrc, elemId, elemType, elemConn, localrc)
  write(failMsg, *) "ESMF_MeshAddElements fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Write the mesh for debug
  call ESMF_MeshWrite(meshSrc, "outmesh", localrc)
  write(failMsg, *) "ESMF_MeshWrite fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  call ESMF_MeshDestroy(meshSrc, localrc)
  write(failMsg, *) "ESMF_MeshDelete fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Now do the same, but with the all in one function
  meshSrc = ESMF_MeshCreate(2,3,nodeId, nodeCoord, nodeOwner, &
                           elemId, elemType, elemConn, localrc)
  write(failMsg, *) "ESMF_MeshCreate1 fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10
  ! Write the mesh for debug
  call ESMF_MeshWrite(meshSrc, "outmesh1", localrc)
  write(failMsg, *) "ESMF_MeshWrite fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  call ESMF_MeshDestroy(meshSrc, localrc)
  write(failMsg, *) "ESMF_MeshDelete fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10


  ! Done with node arrays, zap
  deallocate(nodeId)
  deallocate(nodeCoord)
  deallocate(nodeOwner)

  ! free the element arrays
  deallocate(elemId)
  deallocate(elemType)
  deallocate(elemConn)


10   continue


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
end program ESMF_FieldRegridEx
