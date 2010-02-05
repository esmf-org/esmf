! $Id: ESMF_MeshUTest.F90,v 1.19.2.1 2010/02/05 19:59:52 svasquez Exp $
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
  use ESMF_Mod
  use ESMF_MeshMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_MeshUTest.F90,v 1.19.2.1 2010/02/05 19:59:52 svasquez Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_Mesh) :: mesh,mesh2
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid
  logical :: correct
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Field)  ::  field
  logical :: isMemFreed
  integer :: bufCount, offset
  character, pointer :: buf(:)

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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

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

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

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


  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)

  ! Test Mesh Get
  call ESMF_MeshGet(mesh, nodalDistgrid=nodeDistgrid, elementDistgrid=elemDistgrid, &
                   numOwnedNodes=numOwnedNodesTst, numOwnedElements=numOwnedElemsTst, &
                   isMemFreed=isMemFreed, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check results
  if (numOwnedNodesTst .ne. numOwnedNodes) correct=.false.
  if (numOwnedElemsTst .ne. numElems) correct=.false. ! all elements are owned
  if (isMemFreed) correct=.false. ! Hasn't been freed yet

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


  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)


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

  ! endif for skip for != 4 procs
  endif 

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

  ! Create Mesh
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

write(*,*) "E1"
  ! Check loc stream info
  ! Test Mesh Get
  call ESMF_MeshGet(mesh2, nodalDistgrid=nodeDistgrid, elementDistgrid=elemDistgrid, &
                   numOwnedNodes=numOwnedNodesTst, numOwnedElements=numOwnedElemsTst, &
                   isMemFreed=isMemFreed, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
write(*,*) "E2",rc,correct
  ! check results
  if (numOwnedNodesTst .ne. 0) correct=.false.
  if (numOwnedElemsTst .ne. 0) correct=.false.
  if (isMemFreed) correct=.false. 

  ! Make sure node distgrid is ok
  call ESMF_DistGridValidate(nodeDistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
write(*,*) "E3",rc,correct
  ! Make sure element distgrid is ok
  call ESMF_DistGridValidate(elemDistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) correct=.false.
write(*,*) "E4",rc,correct
  ! Get rid of buffer
  deallocate(buf)
write(*,*) "E5",rc,correct
  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
write(*,*) "E6",rc,correct
  ! endif for skip for >1 proc
  endif 

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

end program ESMF_MeshUTest

