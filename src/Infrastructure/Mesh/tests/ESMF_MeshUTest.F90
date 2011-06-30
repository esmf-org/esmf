! $Id: ESMF_MeshUTest.F90,v 1.31 2011/06/30 05:59:18 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_MeshUTest

!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_MeshUTest - This unit test file tests Mesh methods.
! !DESCRIPTION:
!
! The code in this file drives F90 MeshCreate() unit tests.
! The companion file ESMF\_Mesh.F90 contains the definitions for the
! Mesh methods.

!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF
  use ESMF_MeshMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_MeshUTest.F90,v 1.31 2011/06/30 05:59:18 theurich Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_Mesh) :: mesh, mesh2, meshAlias
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid
  logical :: correct
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Field)  ::  field
  logical :: isMemFreed
  integer :: bufCount, offset
  character, pointer :: buf(:)
  integer :: i,j,pntCount
  real(ESMF_KIND_R8), pointer :: pntList(:)
  integer, pointer :: petList(:)
  integer :: spatialDim, parametricDim
  logical:: meshBool


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)  ! calls ESMF_Initialize() internally
  !------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a small 2x2 2D QUAD Mesh in 3 steps on 1 proc"
  write(failMsg, *) "Incorrect result"

  !!!!!!!!!!!!!!!!!!!!!
  ! 
  !              Mesh Ids
  !
  !  2.0   7 ------- 8 -------- 9
  !        |         |          |
  !        |    3    |    4     |
  !        |         |          |
  !  1.0   4 ------- 5 -------- 6
  !        |         |          |
  !        |    1    |    2     |
  !        |         |          |
  !  0.0   1 ------- 2 -------- 3
  !
  !       0.0       1.0        2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  !
  !!!
  !      ( Everything owned by PET 0) 
  !!!!! 

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

  ! Create Mesh structure
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Fill in node data
  numNodes=9

  !! node ids
  allocate(nodeIds(numNodes))
  nodeIds=(/1,2,3,4,5,6,7,8,9/) 

  !! node Coords
  allocate(nodeCoords(numNodes*2))
  nodeCoords=(/0.0,0.0, &
               1.0,0.0, &
               2.0,0.0, &
               0.0,1.0, &
               1.0,1.0, &
               2.0,1.0, &
               0.0,2.0, &
               1.0,2.0, &
               2.0,2.0 /)

  !! node owners
  allocate(nodeOwners(numNodes))
  nodeOwners=0 ! everything on proc 0

  ! Add nodes
  call ESMF_MeshAddNodes(mesh,nodeIds,nodeCoords,nodeOwners,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)

  ! Fill in elem data
  numElems=4

  !! elem ids
  allocate(elemIds(numElems))
  elemIds=(/1,2,3,4/) 

  !! elem types
  allocate(elemTypes(numElems))
  elemTypes=ESMF_MESHELEMTYPE_QUAD

  !! elem conn
  allocate(elemConn(numElems*4))
  elemConn=(/1,2,5,4, & 
             2,3,6,5, & 
             4,5,8,7, & 
             5,6,9,8/)

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)

  !! Write mesh for debugging
  ! call ESMF_MeshWrite(mesh,"tmesh",rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!  call ESMF_MeshDestroy(mesh, rc=rc)

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest

  ! init success flag
  meshBool = .false.

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

  write(name, *) "Mesh equality before assignment Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  meshBool = (meshAlias.eq.mesh)

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(.not.meshBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_MeshAssignment(=)()

  ! init success flag
  meshBool=.true.

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then
  write(name, *) "Mesh assignment and equality Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  meshAlias = mesh
  meshBool = (meshAlias.eq.mesh)

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(meshBool, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest

  ! init success flag
  rc=ESMF_SUCCESS

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

  write(name, *) "MeshDestroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_MeshDestroy(mesh, rc=rc)

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_MeshOperator(==)()

  ! init success flag
  meshBool = .false.

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

  write(name, *) "Mesh equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  meshBool = (meshAlias==mesh)

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(.not.meshBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  ! Testing ESMF_MeshOperator(/=)()

  ! init success flag
  meshBool=.true.

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

  write(name, *) "Mesh non-equality after destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  meshBool = (meshAlias/=mesh)

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(meshBool, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a small 2x2 2D QUAD Mesh in 1 step on 4 procs and MeshGet"
  write(failMsg, *) "Incorrect result"

  !!!!!!!!!!!!!!!!!!!!!
  ! 
  !              Mesh Ids
  !
  !  2.0   7 ------- 8 -------- 9
  !        |         |          |
  !        |    3    |    4     |
  !        |         |          |
  !  1.0   4 ------- 5 -------- 6
  !        |         |          |
  !        |    1    |    2     |
  !        |         |          |
  !  0.0   1 ------- 2 -------- 3
  !
  !       0.0       1.0        2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  !             Mesh Owners
  !
  !  2.0   2 ------- 2 -------- 3
  !        |         |          |
  !        |    2    |    3     |
  !        |         |          |
  !  1.0   0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !  0.0   0 ------- 0 -------- 1
  !
  !       0.0       1.0        2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 


  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! Only do this if we have 4 PETs
  if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4
        numOwnedNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4
        numOwnedNodes=2

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4
        numOwnedNodes=2

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4
        numOwnedNodes=1

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif

  ! Create Mesh structure in 1 step
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Allocate space for coords
  allocate(ownedNodeCoords(2*numOwnedNodes))

  ! Test Mesh Get
  call ESMF_MeshGet(mesh, parametricDim=parametricDim, spatialDim=spatialDim, &
                   nodalDistgrid=nodeDistgrid, elementDistgrid=elemDistgrid, &
                   numOwnedNodes=numOwnedNodesTst, ownedNodeCoords=ownedNodeCoords, &
                   numOwnedElements=numOwnedElemsTst, &
                   isMemFreed=isMemFreed, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check results
  if (spatialDim .ne. 2) correct=.false.
  if (parametricDim .ne. 2) correct=.false.
  if (numOwnedNodesTst .ne. numOwnedNodes) correct=.false.
  if (numOwnedElemsTst .ne. numElems) correct=.false. ! all elements are owned
  if (isMemFreed) correct=.false. ! Hasn't been freed yet

  ! check coords
  j=1
  do i=1,numNodes
     if (nodeOwners(i) .eq. localPet) then
         if (nodeCoords(2*i-1) .ne. ownedNodeCoords(2*j-1)) correct=.false.    
         if (nodeCoords(2*i) .ne. ownedNodeCoords(2*j)) correct=.false.    
         j=j+1
     endif
  enddo

  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)

  ! deallocate owned node coords
  deallocate(ownedNodeCoords)

  ! Make sure node distgrid is ok
  call ESMF_DistGridValidate(nodeDistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Make sure element distgrid is ok
  call ESMF_DistGridValidate(elemDistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Free memory
  call ESMF_MeshFreeMemory(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Test isMemFreed flag
  call ESMF_MeshGet(mesh, isMemFreed=isMemFreed, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! now it should indicate freed memory
  if (.not. isMemFreed) correct=.false. ! Has been freed 

  !! Write mesh for debugging
  !! call ESMF_MeshWrite(mesh,"tmesh",rc=localrc)
  !! if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! endif for skip for != 4 procs
  endif 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)



  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test ESMF_MeshFreeMemory() "
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! Create Test mesh
  call createTestMesh1(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Test MeshFreeMemory
  call ESMF_MeshFreeMemory(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Should still be able to create a Field on mesh
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  field = ESMF_FieldCreate(mesh, arrayspec,  rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check field
  call ESMF_FieldValidate(field,rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 ! Get rid of field
  call ESMF_FieldDestroy(field, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test error checking"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! Only do this if we have 1 processor
  if (petCount .eq. 1) then

  ! Create Mesh structure
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Fill in node data
  numNodes=9

  !! node ids
  allocate(nodeIds(numNodes))
  nodeIds=(/1,2,3,4,5,6,7,8,9/) 

  !! node Coords
  allocate(nodeCoords(numNodes*2))
  nodeCoords=(/0.0,0.0, &
               1.0,0.0, &
               2.0,0.0, &
               0.0,1.0, &
               1.0,1.0, &
               2.0,1.0, &
               0.0,2.0, &
               1.0,2.0, &
               2.0,2.0 /)

  !! node owners
  allocate(nodeOwners(numNodes))
  nodeOwners=0 ! everything on proc 0

  ! Fill in elem data
  numElems=4

  !! elem ids
  allocate(elemIds(numElems))
  elemIds=(/1,2,3,4/) 

  !! elem types
  allocate(elemTypes(numElems))
  elemTypes=ESMF_MESHELEMTYPE_QUAD


  !! elem conn
  allocate(elemConn(numElems*4))
  elemConn=(/1,2,5,4, & 
             2,3,6,5, & 
             4,5,8,7, & 
             5,6,9,8/)

  ! Shouldn't be able to do this here before AddNodes()
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Add nodes
  call ESMF_MeshAddNodes(mesh,nodeIds,nodeCoords,nodeOwners,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Shouldn't be able to do this twice
  call ESMF_MeshAddNodes(mesh,nodeIds,nodeCoords,nodeOwners,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Mesh Get shouldn't work here
  call ESMF_MeshGet(mesh, nodalDistgrid=nodeDistgrid, elementDistgrid=elemDistgrid, &
                   numOwnedNodes=numOwnedNodesTst, numOwnedElements=numOwnedElemsTst, rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Try using a bad element type
  elemTypes(1)=ESMF_MESHELEMTYPE_HEX

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Use a better element
  elemTypes(1)=ESMF_MESHELEMTYPE_QUAD

  ! Try using a bad connectivity
  elemConn(1)=5 

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,rc=localrc)
  if (localrc .eq. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Try using a bad connectivity
  elemConn(1)=1

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)


  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)

  !! Write mesh for debugging
  ! call ESMF_MeshWrite(mesh,"tmesh",rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  ! NOTE THAT SERIALIZE/DESERIALIZE IS AN INTERNAL INTERFACE AND NOT INTENDED FOR PUBLIC USE
  !NEX_UTest
  write(name, *) "Test Mesh Serialize/Deserialize"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh1(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !! Write mesh for debugging
  ! call ESMF_MeshWrite(mesh,"tmesh",rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create a buffer to put the locstream in
  offset=0
  allocate (buf(1))
  call ESMF_MeshSerialize(mesh, buf, bufCount, offset,  &
    inquireflag=ESMF_INQUIREONLY, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE                            

  deallocate (buf)
  bufCount=2*offset ! a little more room
  print *, 'ESMF_MeshUTest: serialization buffer size =', bufCount
  allocate(buf(bufCount))

  ! Serialize
  offset=0
  call ESMF_MeshSerialize(mesh, buf, bufCount, offset, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Deserialize
  offset=0
  mesh2=ESMF_MeshDeserialize(buf, offset, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Check loc stream info
  ! Test Mesh Get
  call ESMF_MeshGet(mesh2, nodalDistgrid=nodeDistgrid, elementDistgrid=elemDistgrid, &
                   numOwnedNodes=numOwnedNodesTst, numOwnedElements=numOwnedElemsTst, &
                   isMemFreed=isMemFreed, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check results
  if (numOwnedNodesTst .ne. 0) correct=.false.
  if (numOwnedElemsTst .ne. 0) correct=.false.
  if (isMemFreed) correct=.false. 

  ! Make sure node distgrid is ok
  call ESMF_DistGridValidate(nodeDistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Make sure element distgrid is ok
  call ESMF_DistGridValidate(elemDistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.

  ! Get rid of buffer
  deallocate(buf)

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  ! NOTE THAT MeshFindPnt IS AN INTERNAL INTERFACE AND NOT INTENDED FOR PUBLIC USE
  !NEX_UTest
  write(name, *) "Test Mesh Find Point"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh1(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Setup lists
if (localPet .eq. 0) then

  pntCount=1
  allocate(pntList(2*pntCount))
  allocate(petList(pntCount))

  ! Set point coords
  pntList(1)=1.0
  pntList(2)=2.5


else if (localPet .eq. 1) then

  pntCount=1
  allocate(pntList(2*pntCount))
  allocate(petList(pntCount))

  pntList(1)=1.0
  pntList(2)=1.0


else if (localPet .eq. 2) then

  pntCount=2
  allocate(pntList(2*pntCount))
  allocate(petList(pntCount))

  pntList(1)=1.5
  pntList(2)=1.5
  pntList(3)=0.5
  pntList(4)=1.5


else if (localPet .eq. 3) then

  pntCount=0
  allocate(pntList(2*pntCount))
  allocate(petList(pntCount))

endif

  ! Get points
  call ESMF_MeshFindPnt(mesh, unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                        pntDim=2, pntCount=pntCount, pntList=pntList, &
                        petList=petList, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!  ! output 
!  do i=1,pntCount
!     write(*,*) i," :: ",pntList(2*i-1),pntList(2*i)," >>>> pet=",petList(i)
!  enddo

  ! Deallocate
  deallocate(pntList)
  deallocate(petList)


  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------



  !------------------------------------------------------------------------
  ! TODO: "Activate once the mesh is fully created. ESMF_MeshWrite is not meant
  !  to be called until then".
  !UTest
  !write(name, *) "MeshWrite Test"
  !write(failMsg, *) "Did not return ESMF_SUCCESS"
  !  call ESMF_MeshWrite(meshSrc, filename="mesh_out", rc=rc)
  !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------


contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !  2.0   7 ------- 8 -------- 9
  !        |         |          |
  !        |    3    |    4     |
  !        |         |          |
  !  1.0   4 ------- 5 -------- 6
  !        |         |          |
  !        |    1    |    2     |
  !        |         |          |
  !  0.0   1 ------- 2 -------- 3
  !
  !       0.0       1.0        2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
  ! 
  !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !             Mesh Owners
  !
  !  2.0   2 ------- 2 -------- 3
  !        |         |          |
  !        |    2    |    3     |
  !        |         |          |
  !  1.0   0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !  0.0   0 ------- 0 -------- 1
  !
  !       0.0       1.0        2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 
subroutine createTestMesh1(mesh, rc)
  type(ESMF_Mesh), intent(out) :: mesh
  integer :: rc

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm


  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) return
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) return

  ! return with an error if not 1 or 4 PETs
  if ((petCount /= 1) .and. (petCount /=4)) then
     rc=ESMF_FAILURE
     return
  endif


  ! Setup mesh info depending on the 
  ! number of PETs
  if (petCount .eq. 1) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, &
                 1.0,0.0, &
                 2.0,0.0, &
                 0.0,1.0, &
                 1.0,1.0, &
                 2.0,1.0, &
                 0.0,2.0, &
                 1.0,2.0, &
                 2.0,2.0 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numElems=4

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then
        ! Fill in node data
        numNodes=4
        numOwnedNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,0.0, &
                    1.0,0.0, &
                    0.0,1.0, &
                    1.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4
        numOwnedNodes=2

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
                    2.0,0.0, &
                    1.0,1.0, &
                    2.0,1.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,0,1/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4
        numOwnedNodes=2

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/4,5,7,8/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.0,1.0, &
                    1.0,1.0, &
                    0.0,2.0, &
                    1.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,2,2/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4
        numOwnedNodes=1

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,1.0, &
                    2.0,1.0, &
                    1.0,2.0, &
                    2.0,2.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numElems=1

       !! elem ids
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif

   
   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

end subroutine createTestMesh1



end program ESMF_MeshUTest

