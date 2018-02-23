! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
    '$Id$'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

   ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

   !LOCAL VARIABLES:
  type(ESMF_Mesh) :: mesh, mesh2, meshAlias, meshDual
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid
  logical :: correct
  logical :: isCreated
  integer, pointer :: nodeIds(:),nodeOwners(:)
   real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
   real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer(ESMF_KIND_I4) :: localNumOwnedElems(1), globalNumOwnedElems(1)
  integer :: numElems,numOwnedElemsTst,numOwnedElems
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_Field)  ::  field, areaField
  type(ESMF_Field)  ::  nodeField, elemField
  logical :: isMemFreed
  integer :: bufCount, offset
  character, pointer :: buf(:)
  integer :: i,j,pntCount
  real(ESMF_KIND_R8), pointer :: pntList(:)
   real(ESMF_KIND_R8), pointer :: elemAreas(:)
  real(ESMF_KIND_R8), pointer :: fieldAreaPtr(:)
  integer, pointer :: petList(:)
  integer :: spatialDim, parametricDim, cu(1),cl(1)
  logical:: meshBool
  integer :: sizeOfList
  type(ESMF_CoordSys_Flag) :: coordSys
  integer :: dimCount, localDECount

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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  !------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Mesh IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_MeshIsCreated(mesh)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Mesh IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_MeshIsCreated(mesh, rc=rc)
   call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Create test Mesh for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, rc=localrc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Mesh IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_MeshIsCreated(mesh)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Mesh IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_MeshIsCreated(mesh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Destroy test Mesh for IsCreated"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_MeshDestroy(mesh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Mesh IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_MeshIsCreated(mesh)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing Mesh IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_MeshIsCreated(mesh, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
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
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, rc=localrc)
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

  allocate(elemCoords(numElems*2))
  elemCoords= (/0.5,0.5, &
                1.5,0.5, &
                0.5,1.5, &
                1.5,1.5 /)
  !! elem types
  allocate(elemTypes(numElems))
  elemTypes=ESMF_MESHELEMTYPE_QUAD

  !! elem area
  allocate(elemAreas(numElems))
  elemAreas=(/2.0,3.0,4.0,5.0/)

   !! elem conn
  allocate(elemConn(numElems*4))
  elemConn=(/1,2,5,4, & 
             2,3,6,5, & 
             4,5,8,7, & 
             5,6,9,8/)

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,&
                            elementCoords=elemCoords, &
                            elementArea=elemAreas, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)
  deallocate(elemAreas)
  deallocate(elemCoords)

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

       !! elem coords
       allocate(elemCoords(numElems*2))
       elemCoords=(/0.5,0.5/)
     
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

       !! elem coords
       allocate(elemCoords(numElems*2))
       elemCoords=(/1.5,0.5/)
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

       !! elem coords
       allocate(elemCoords(numElems*2))
       elemCoords=(/1.5,0.5/)
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
 
       !! elem coords
        allocate(elemCoords(numElems*2))
       elemCoords=(/1.5,1.5/)
     endif

  ! Create Mesh structure in 1 step
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords, coordSys=ESMF_COORDSYS_CART, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Allocate space for coords
  allocate(ownedNodeCoords(2*numOwnedNodes))
  allocate(ownedElemCoords(2*numElems))

  ! Test Mesh Get
  call ESMF_MeshGet(mesh, parametricDim=parametricDim, spatialDim=spatialDim, &
                   nodalDistgrid=nodeDistgrid, elementDistgrid=elemDistgrid, &
                   numOwnedNodes=numOwnedNodesTst, ownedNodeCoords=ownedNodeCoords, &
                   numOwnedElements=numOwnedElemsTst, &
                   ownedElemCoords=ownedElemCoords, &
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
  do i=1,numElems
     if (elemCoords(2*i-1) .ne. ownedElemCoords(2*i-1)) correct=.false.    
     if (elemCoords(2*i) .ne. ownedElemCoords(2*i)) correct=.false.    
  enddo

  ! deallocate node data
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)
  deallocate(elemCoords)

  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)

  ! deallocate owned node coords
   deallocate(ownedNodeCoords)
  deallocate(ownedElemCoords)

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
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, rc=localrc)
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
  call ESMF_MeshGet(mesh, nodalDistgrid=nodeDistgrid, &
    elementDistgrid=elemDistgrid, numOwnedNodes=numOwnedNodesTst, &
    numOwnedElements=numOwnedElemsTst, rc=localrc)
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
!  call createTestMeshPH(mesh, rc=localrc)
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
                   spatialDim=spatialDim, parametricDim=parametricDim, &
                   isMemFreed=isMemFreed, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check results
  if (numOwnedNodesTst .ne. 0) correct=.false.
  if (numOwnedElemsTst .ne. 0) correct=.false.
  if (spatialDim .ne. 2) correct=.false.
  if (parametricDim .ne. 2) correct=.false.
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
  !NEX_UTest
  write(name, *) "Test setting and getting area of Mesh"
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
  mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, rc=localrc)
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

  !! elem area
  allocate(elemAreas(numElems))
  elemAreas=(/2.0,3.0,4.0,5.0/)

  !! elem conn
  allocate(elemConn(numElems*4))
  elemConn=(/1,2,5,4, & 
             2,3,6,5, & 
             4,5,8,7, & 
             5,6,9,8/)

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,&
                            elementArea=elemAreas, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create field to hold area
  areaField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
                                rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

  ! Get area
  call ESMF_FieldRegridGetArea(areaField, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 

  ! get pointer to area
  call ESMF_FieldGet(areaField, 0, fieldAreaPtr,       & 
       computationalLBound=cl, computationalUBound=cu, & 
        rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE 


  ! Init results
  correct=.true.
   
  ! Check results
  if ((cu(1)-cl(1)+1) .ne. numElems) correct=.false.

  j=1
  do i=cl(1), cu(1)
     if (fieldAreaPtr(i) .ne. elemAreas(j)) correct=.false.
     j=j+1
  enddo
   
  ! deallocate elem data
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)
  deallocate(elemAreas)


  ! Destroy the area Field
  call ESMF_FieldDestroy(areaField, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE  

  ! Destroy the Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE  

  ! endif for skip for >1 proc
  endif 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh1(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Setup lists
  if (petCount .eq. 1) then  
     allocate(nodeIds(9))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

      allocate(elemIds(4))
      elemIds=(/1,2,3,4/) 

  else if (petCount .eq. 4) then  
      if (localPet .eq. 0) then
        allocate(elemIds(1))
        elemIds(1)=4
        
        allocate(nodeIds(4))
        nodeIds=(/5,6,8,9/)
        
     else if (localPet .eq. 1) then
        allocate(elemIds(1))
        elemIds(1)=3
        
        allocate(nodeIds(2))
        nodeIds=(/7,4/)
        
     else if (localPet .eq. 2) then
        allocate(elemIds(1))
        elemIds(1)=2
        
        allocate(nodeIds(2))
        nodeIds=(/2,3/)
        
     else if (localPet .eq. 3) then
        allocate(elemIds(1))
        elemIds(1)=1
        
        allocate(nodeIds(1))
        nodeIds=(/1/)
     endif
  endif


  ! Create node Distgrid
  nodedistgrid=ESMF_DistGridCreate(nodeIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 
  ! Create element Distgrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, nodalDistgrid=nodedistgrid, &
    elementDistgrid=elemdistgrid, rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure nodes in nodeIds are the same as local nodes in mesh2
   sizeOfList=size(nodeIds)
   call c_esmc_meshchecknodelist(mesh2%this, sizeOfList, nodeIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure elems in elemIds are the same as local elems in mesh2
   sizeOfList=size(elemIds)
   call c_esmc_meshcheckelemlist(mesh2%this, sizeOfList, elemIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 

  ! Deallocate
  deallocate(elemIds)
  deallocate(nodeIds)


  ! Check Output mesh
  call ESMF_MeshGet(mesh2, parametricDim=parametricDim, &
                    spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Distgrids
  call ESMF_DistgridDestroy(nodedistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DistgridDestroy(elemdistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist: Different Location Node Element"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh1(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Setup lists
  if (petCount .eq. 1) then  
     allocate(nodeIds(9))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

      allocate(elemIds(4))
      elemIds=(/1,2,3,4/) 
  else if (petCount .eq. 4) then  
     if (localPet .eq. 0) then
        allocate(elemIds(1))
        elemIds(1)=4
        
        allocate(nodeIds(5))
        nodeIds=(/5,6,8,9,7/)  ! Put node 7 here because home
                               ! element (3) isn't here
                               ! (testing node without home elem redist)
      else if (localPet .eq. 1) then
        allocate(elemIds(1))
        elemIds(1)=3
        
        allocate(nodeIds(1))
        nodeIds=(/4/)        
        
     else if (localPet .eq. 2) then
        allocate(elemIds(1))
        elemIds(1)=2
        
        allocate(nodeIds(2))
        nodeIds=(/2,3/)
         
     else if (localPet .eq. 3) then
        allocate(elemIds(1))
        elemIds(1)=1
        
        allocate(nodeIds(1))
        nodeIds=(/1/)
     endif
  endif


  ! Create node Distgrid
  nodedistgrid=ESMF_DistGridCreate(nodeIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 
  ! Create element Distgrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, nodalDistgrid=nodedistgrid, &
    elementDistgrid=elemdistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure nodes in nodeIds are the same as local nodes in mesh2
   sizeOfList=size(nodeIds)
   call c_esmc_meshchecknodelist(mesh2%this, sizeOfList, nodeIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 
  ! Make sure elems in elemIds are the same as local elems in mesh2
   sizeOfList=size(elemIds)
   call c_esmc_meshcheckelemlist(mesh2%this, sizeOfList, elemIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Deallocate
  deallocate(elemIds)
  deallocate(nodeIds)


  ! Check Output mesh
  call ESMF_MeshGet(mesh2, parametricDim=parametricDim, &
                     spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Distgrids
  call ESMF_DistgridDestroy(nodedistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_DistgridDestroy(elemdistgrid, rc=localrc)
   if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist with just element distgrid "
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh1(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Setup lists
  if (petCount .eq. 1) then  
      allocate(elemIds(4))
      elemIds=(/1,2,3,4/) 

  else if (petCount .eq. 4) then  
     if (localPet .eq. 0) then
        allocate(elemIds(1))
        elemIds(1)=4

     else if (localPet .eq. 1) then
        allocate(elemIds(1))
        elemIds(1)=3
        
     else if (localPet .eq. 2) then
        allocate(elemIds(1))
        elemIds(1)=2
         
     else if (localPet .eq. 3) then
        allocate(elemIds(1))
        elemIds(1)=1
        
     endif
  endif
 

  ! Create element Distgrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, elementDistgrid=elemdistgrid, rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure elems in elemIds are the same as local elems in mesh2
   sizeOfList=size(elemIds)
   call c_esmc_meshcheckelemlist(mesh2%this, sizeOfList, elemIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE



  ! Deallocate
  deallocate(elemIds)

  ! Check Output mesh
   call ESMF_MeshGet(mesh2, parametricDim=parametricDim, &
                    spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of distgrid
  call ESMF_DistgridDestroy(elemdistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist with just node distgrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh3x3(mesh, rc=localrc)
   if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Setup lists
  if (petCount .eq. 1) then  
      allocate(nodeIds(16))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/) 

  else if (petCount .eq. 4) then  
     if (localPet .eq. 0) then

        allocate(nodeIds(13))
        nodeIds=(/4,5,6,7,8,9,10,11,12,13,14,15,16/) 

     else if (localPet .eq. 1) then

        allocate(nodeIds(1))
        nodeIds=(/1/)        
        
     else if (localPet .eq. 2) then
        
        allocate(nodeIds(1))
        nodeIds=(/2/)
        
     else if (localPet .eq. 3) then
        allocate(nodeIds(1))
        nodeIds=(/3/)
     endif
  endif


  ! Create node Distgrid
   nodedistgrid=ESMF_DistGridCreate(nodeIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
 
  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, nodalDistgrid=nodedistgrid, &
                        rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure nodes in nodeIds are the same as local nodes in mesh2
   sizeOfList=size(nodeIds)
   call c_esmc_meshchecknodelist(mesh2%this, sizeOfList, nodeIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Deallocate
  deallocate(nodeIds)

  ! Check Output mesh
  call ESMF_MeshGet(mesh2, numOwnedElements=localNumOwnedElems(1), &
                   spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Sum across procs
  call ESMF_VMAllReduce(vm,localNumOwnedElems, globalNumOwnedElems, 1, &
                       ESMF_REDUCE_SUM, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure the global number of elements is still the same
  if (globalNumOwnedElems(1) .ne. 10) correct=.false.
 
   !write(*,*) localPet, " number of local elems=",localnumOwnedElems(1)
  !write(*,*) localPet, " number of global elems=",globalnumOwnedElems(1)

  ! Try accessing coordinates
  allocate(elemCoords(spatialDim*localNumOwnedElems(1)))
  call ESMF_MeshGet(mesh2, ownedElemCoords=elemCoords, &
                   spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  deallocate(elemCoords)

  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Distgrids
  call ESMF_DistgridDestroy(nodedistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist with just node distgrid and >4 sided elements"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMeshPH(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Setup lists
  if (petCount .eq. 1) then  
     allocate(nodeIds(12))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

  else if (petCount .eq. 4) then  
     if (localPet .eq. 0) then

        allocate(nodeIds(7))
        nodeIds=(/5,6,8,9,7,10,12/)

     else if (localPet .eq. 1) then

        allocate(nodeIds(2))
        nodeIds=(/4,11/)        
        
     else if (localPet .eq. 2) then
        
        allocate(nodeIds(2))
        nodeIds=(/2,3/)
        
     else if (localPet .eq. 3) then
        allocate(nodeIds(1))
        nodeIds=(/1/)
     endif
  endif


  ! Create node Distgrid
   nodedistgrid=ESMF_DistGridCreate(nodeIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  

  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, nodalDistgrid=nodedistgrid, &
                        rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure nodes in nodeIds are the same as local nodes in mesh2
   sizeOfList=size(nodeIds)
   call c_esmc_meshchecknodelist(mesh2%this, sizeOfList, nodeIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Deallocate
  deallocate(nodeIds)

  ! Check Output mesh
  call ESMF_MeshGet(mesh2, parametricDim=parametricDim, &
                    spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Get rid of Distgrids
  call ESMF_DistgridDestroy(nodedistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist with no distgrids"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh3x3(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 
  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Check Output mesh
  call ESMF_MeshGet(mesh2, numOwnedElements=localNumOwnedElems(1), &
                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Sum across procs
  call ESMF_VMAllReduce(vm,localNumOwnedElems, globalNumOwnedElems, 1, &
                        ESMF_REDUCE_SUM, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Make sure the global number of elements is still the same
  if (globalNumOwnedElems(1) .ne. 10) correct=.false.

  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create Redist: >1 localDECount Distgrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh3x3_notris(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Create node Distgrid
  nodedistgrid=ESMF_DistGridCreate(minIndex=(/1,1/), &
                                   maxIndex=(/4,4/), &
                                   regDecomp=(/3,2/),&
                                   indexflag=ESMF_INDEX_GLOBAL, &
                                   rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create elem Distgrid
  elemdistgrid=ESMF_DistGridCreate(minIndex=(/1,1/), &
                                    maxIndex=(/3,3/), &
                                   regDecomp=(/2,2/),&
                                   indexflag=ESMF_INDEX_GLOBAL, &
                                   rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

 
  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, nodalDistgrid=nodedistgrid, &
     elementDistgrid=elemdistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Test creating field
  nodeField = ESMF_FieldCreate(mesh2, typekind=ESMF_TYPEKIND_R8, &
       meshloc=ESMF_MESHLOC_NODE, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check field
  call ESMF_FieldValidate(nodeField,rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Field info
  call ESMF_FieldGet(nodeField, dimCount=dimCount, &
       localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check information
  if (dimCount .ne. 2) correct=.false.
  if (petCount .eq. 1) then
     if (localDECount .ne. 6) correct=.false.
  else if (petCount .eq. 4) then
    if (localPET .eq. 0) then
       if (localDECount .ne. 2) correct=.false.
    else if (localPET .eq. 1) then
       if (localDECount .ne. 2) correct=.false.
    else if (localPET .eq. 2) then
       if (localDECount .ne. 1) correct=.false.
    else if (localPET .eq. 3) then
       if (localDECount .ne. 1) correct=.false.
    endif
  endif

  ! Test creating field
  elemField = ESMF_FieldCreate(mesh2, typekind=ESMF_TYPEKIND_R8, &
       meshloc=ESMF_MESHLOC_ELEMENT, &
       rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! check field
  call ESMF_FieldValidate(elemField,rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get Field info
  call ESMF_FieldGet(elemField, dimCount=dimCount, &
       localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !  write(*,*) "dimCount=",dimCount,&
  !        " localDECount=",localDECount

  ! check information
  if (dimCount .ne. 2) correct=.false.
  if (petCount .eq. 1) then
     if (localDECount .ne. 4) correct=.false.
  else if (petCount .eq. 4) then
     if (localDECount .ne. 1) correct=.false.
  endif

  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Distgrids
  call ESMF_DistgridDestroy(nodedistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of fields
  call ESMF_FieldDestroy(nodeField, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_FieldDestroy(elemField, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create >1 localDECount Distgrid"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

 ! XMRKX

  ! initialize check variables
  correct=.false.
  rc=ESMF_FAILURE
 
  ! Do test
  call test_mesh_create_gt_1localde(correct, rc)

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh Create with coordSys ESMF_COORDSYS_SPH_DEG"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMeshSphDeg(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Check CoordSys flag
  call ESMF_MeshGet(mesh, coordSys=coordSys, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


  ! Check owned coordinates
  call ESMF_MeshGet(mesh, numOwnedNodes=numOwnedNodes, &
                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  allocate(ownedNodeCoords(2*numOwnedNodes))

  call ESMF_MeshGet(mesh, &
                    ownedNodeCoords=ownedNodeCoords, &
                    rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
  ! Check coords
  if (petCount .eq. 1) then
      if (numOwnedNodes .ne. 9) correct=.false.
    
      if (.not. ALL(ownedNodeCoords .eq. &
             (/0.0,0.0, &
                 10.0,0.0, &
                 20.0,0.0, &
                 0.0,10.0, &
                 10.0,10.0, &
                 20.0,10.0, &
                 0.0,20.0, &
                 10.0,20.0, &
                 20.0,20.0 /))) correct=.false.

   else if (petCount .eq. 4) then
     if (localPet .eq. 0) then
        if (numOwnedNodes .ne. 4) correct=.false.

       if (.not. ALL(ownedNodeCoords .eq. &
               (/0.0,0.0, &
                10.0,0.0, &
                 0.0,10.0, &
                 10.0,10.0/))) correct=.false.

     else if (localPet .eq. 1) then
        if (numOwnedNodes .ne. 2) correct=.false.

        if (.not. ALL(ownedNodeCoords .eq. &
                      (/20.0,0.0, &
                        20.0,10.0/))) correct=.false.

     else if (localPet .eq. 2) then
        if (numOwnedNodes .ne. 2) correct=.false.

        if (.not. ALL(ownedNodeCoords .eq. &
                       (/0.0,20.0, &
                       10.0,20.0/))) correct=.false.

     else 
       if (numOwnedNodes .ne. 1) correct=.false.

        if (.not. ALL(ownedNodeCoords .eq. &
            (/20.0,20.0/))) correct=.false.
     endif
   endif


  ! deallocate coords
  deallocate(ownedNodeCoords)

  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh Create with a pentagon and hexagon element"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMeshPH(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !  call ESMF_MeshWrite(mesh, filename="meshPH", rc=rc)

  ! Get element coords to make sure that that works
  call ESMF_MeshGet(mesh, numOwnedElements=numOwnedElems, &
       spatialDim=spatialDim, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Allocate
  allocate(ownedElemCoords(spatialDim*numOwnedElems))

  ! Get coords
  call ESMF_MeshGet(mesh, ownedElemCoords=ownedElemCoords, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

!  write(*,*) localPet," owned elem Coords=",ownedElemCoords

  ! Deallocate
  deallocate(ownedElemCoords)

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh Create Dual with a pentagon and hexagon element"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMeshPH(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create dual mesh
  meshDual=ESMF_MeshCreateDual(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
 ! call ESMF_MeshWrite(mesh, filename="mesh", rc=rc)
 ! call ESMF_MeshWrite(meshDual, filename="meshDual", rc=rc)

  call ESMF_MeshDestroy(meshDual, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------




  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh Create and then Redist with a pentagon and hexagon element"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
   call createTestMeshPH(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !call ESMF_MeshWrite(mesh, filename="meshPH", rc=rc)

  ! Setup lists
  if (petCount .eq. 1) then  
      allocate(elemIds(5))
      elemIds=(/1,2,3,4,5/) 

  else if (petCount .eq. 4) then  
     if (localPet .eq. 0) then
        allocate(elemIds(1))
        elemIds(1)=4

     else if (localPet .eq. 1) then
        allocate(elemIds(2))
        elemIds=(/3,5/)
        
     else if (localPet .eq. 2) then
        allocate(elemIds(1))
        elemIds(1)=2
        
     else if (localPet .eq. 3) then
        allocate(elemIds(1))
        elemIds(1)=1
        
     endif
  endif

  ! Create element Distgrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Create redisted mesh
  mesh2=ESMF_MeshCreate(mesh, elementDistgrid=elemdistgrid, rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Make sure elems in elemIds are the same as local elems in mesh2
   sizeOfList=size(elemIds)
   call c_esmc_meshcheckelemlist(mesh2%this, sizeOfList, elemIds, &
                                 localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Deallocate
  deallocate(elemIds)


  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_MeshDestroy(mesh2, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Get rid of distgrid
  call ESMF_DistgridDestroy(elemdistgrid, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! NOTE THAT MeshCreateDual IS CURRENTLY AN INTERNAL INTERFACE AND NOT INTENDED FOR PUBLIC USE
  !
  !NEX_UTest
  write(name, *) "Mesh Create Dual"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMesh3x3(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !call ESMF_MeshWrite(mesh, filename="mesh", rc=rc)

  ! Create dual mesh
  meshDual=ESMF_MeshCreateDual(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  

  !call ESMF_MeshWrite(meshDual, filename="meshDual", rc=rc)


  ! Get rid of Meshes
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Meshes
  call ESMF_MeshDestroy(meshDual, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh Create with a multipart element"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! Create Test mesh
  call createTestMeshMultiElem(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  !  call ESMF_MeshWrite(mesh, filename="meshPH", rc=rc)

  ! Get rid of Meshs
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh create easy elems"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! do test
  call test_mesh_create_easy_elems(correct, rc)

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Mesh create easy elems 1 type"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  ! do test
  call test_mesh_create_ee_1type(correct, rc)

  call ESMF_Test(((rc .eq. ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------


#if 1
  ! OFF UNTIL I SEE IF THE OLD STUFF WORKS

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a MOAB Mesh"
  write(failMsg, *) "Incorrect result"

  ! init success flag
  rc=ESMF_SUCCESS
  correct=.true.

  ! Don't test if MOAB isn't available
#if defined ESMF_MOAB

  call ESMF_MeshSetMOAB(.true., rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Create test mesh
  call createTestMesh3x3(mesh, rc=localrc)  
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

#if 0
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

  !! elem area
  allocate(elemAreas(numElems))
  elemAreas=(/2.0,3.0,4.0,5.0/)

  !! elem conn
    allocate(elemConn(numElems*4))
  elemConn=(/1,2,5,4, & 
             2,3,6,5, & 
             4,5,8,7, & 
             5,6,9,8/)

  ! Add Elements
  call ESMF_MeshAddElements(mesh,elemIds,elemTypes,elemConn,&
                             elementArea=elemAreas, rc=localrc)
   if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! deallocate elem data
   deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)
  deallocate(elemAreas)

  !! Write mesh for debugging
  !call ESMF_MeshWrite(mesh,"tmesh",rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE
  
!  call ESMF_MeshDestroy(mesh, rc=rc)

  ! endif for skip for >1 proc
  endif 
#endif


  ! XMRKX
  ! Test creating field
   field = ESMF_FieldCreate(mesh, arrayspec,  meshloc=ESMF_MESHLOC_ELEMENT, &
         rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! check field
  call ESMF_FieldValidate(field,rc=localrc) 
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  call ESMF_MeshSetMOAB(.false., rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

#endif 


  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)
#endif

  !------------------------------------------------------------------------
  ! TODO: "Activate once the mesh is fully created. ESMF_MeshWrite is not meant
  !  to be called until then".
  !UTest
  !write(name, *) "MeshWrite Test"
  !write(failMsg, *) "Did not return ESMF_SUCCESS"
  !  call ESMF_MeshWrite(meshSrc, filename="mesh_out", rc=rc)
  !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
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
  !   2.0   7 ------- 8 -------- 9
  !         |         |          |
  !         |    3    |    4     |
  !         |         |          |
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0        2.0 
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
  !   2.0  2 ------- 2 -------- 3
  !        |         |          |
  !        |    2    |    3     |
  !        |         |          |
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
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
        coordSys=ESMF_COORDSYS_CART, &
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



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   20.0   7 ------- 8 -------- 9
  !          |         |          |
  !          |    3    |    4     |
  !          |         |          |
  !   10.0   4 ------- 5 -------- 6
  !          |         |          |
  !          |    1    |    2     |
  !          |         |          |
  !   0.0    1 ------- 2 -------- 3
  !
  !        0.0       10.0        20.0 
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
  !   20.0  2 ------- 2 -------- 3
  !         |         |          |
  !         |    2    |    3     |
  !         |         |          |
  !   10.0  0 ------- 0 -------- 1
  !         |         |          |
  !         |    0    |    1     |
  !         |         |          |
  !   0.0   0 ------- 0 -------- 1
  !
  !        0.0       10.0       20.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 
subroutine createTestMeshSphDeg(mesh, rc)
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
                 10.0,0.0, &
                 20.0,0.0, &
                 0.0,10.0, &
                 10.0,10.0, &
                 20.0,10.0, &
                 0.0,20.0, &
                 10.0,20.0, &
                 20.0,20.0 /)

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
                    10.0,0.0, &
                    0.0,10.0, &
                    10.0,10.0/)

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
       nodeCoords=(/10.0,0.0, &
                    20.0,0.0, &
                    10.0,10.0, &
                    20.0,10.0/)

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
       nodeCoords=(/0.0,10.0, &
                    10.0,10.0, &
                    0.0,20.0, &
                    10.0,20.0/)

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
       nodeCoords=(/10.0,10.0, &
                    20.0,10.0, &
                    10.0,20.0, &
                    20.0,20.0/)

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
        coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

end subroutine createTestMeshSphDeg


!
!  2.5        8        10 --------11   
!          /     \   /            |
!  2.1   7         9              12
!        |         |      5       /
!        |    4    |            /
!        |         |          /
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
! -0.1   1 ------- 2 ------- 3
!
!      -0.1       1.0       2.1   2.5 
! 
!        Node Id labels at corners
!       Element Id labels in centers
subroutine createTestMeshPH(mesh, rc)
  type(ESMF_Mesh), intent(out) :: mesh
  integer :: rc

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
   integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

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

  if (petCount .eq. 1) then
      ! Set number of nodes
     numNodes=12

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   0.5, 2.5, & ! node id 8
                   1.0, 2.1, & ! node id 9
                   1.5, 2.5, & ! node id 10
                   2.5, 2.5, & ! node id 11
                    2.5, 2.1/)  ! node id 12

      ! Allocate and fill the node owner array.
      ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus tot and num conn.
     numQuadElems=1
     numTriElems=2
     numPentElems=1
     numHexElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems+6*numHexElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                  5, &                      ! elem id 4
                 6/)                       ! elem id 5


     ! Allocate and fill elem coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(elemCoords(2*numTotElems))
     elemCoords=(/ 0.45, 0.45, & ! elem id 1
                   1.37, 0.27, & ! elem id 2
                   1.73, 0.63, & ! elem id 3
                   0.46, 1.74, & ! elem id 4
                   1.76, 1.87/)  ! elem id 5



     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
      ! positions in the nodeIds, etc. arrays and that
      ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &       ! elem id 1
                2,3,5,   &       ! elem id 2
                3,6,5,   &       ! elem id 3
                4,5,9,8,7, &     ! elem id 4
                5,6,12,11,10,9/) ! elem id 5

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=1
       numTriElems=0
       numPentElems=0
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems
       
       ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

     ! Allocate and fill elem coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(elemCoords(2*numTotElems))
     elemCoords=(/ 0.45, 0.45/)  ! elem id 1


       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=2
       numPentElems=0
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 
 
        ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill elem coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(elemCoords(2*numTotElems))
       elemCoords=(/1.37, 0.27, & ! elem id 2
                    1.73, 0.63/) ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      0.5,2.5, & ! node id 8
                      1.0,2.1 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     2/)  ! node id 9

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=0
       numPentElems=1
       numHexElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+6*numHexElems

        ! Allocate and fill the element id array.
          allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/5/) ! elem id 4

        ! Allocate and fill elem coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/0.46, 1.74/)  ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,5,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=6

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,9,10,11,12/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 9
                     1.5,2.5, &  ! node id 10
                     2.5,2.5, &  ! node id 11
                     2.5,2.1 /)  ! node id 12

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 9
                     3, & ! node id 10
                     3, & ! node id 11
                     3/)  ! node id 12
 

        ! Set the number of each type of element, plus tot and num conn.
        numQuadElems=0
        numTriElems=0
        numPentElems=0
        numHexElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems+numHexElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+6*numHexElems


        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/6/) ! elem id 5

        ! Allocate and fill elem coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(elemCoords(2*numTotElems))
        elemCoords=(/1.76, 1.87/)  ! elem id 5


        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,6,5,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, &
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

end subroutine createTestMeshPH



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0   7 ------- 8 -------- 9
  !         |         |          |
  !         |    3    |    4     |
  !         |         |          |
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0        2.0 
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
  !   2.0  2 ------- 2 -------- 3
  !        |         |          |
  !        |    2    |    3     |
  !        |         |          |
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       1.0        2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 
subroutine createTestMeshForDual(mesh, rc)
  type(ESMF_Mesh), intent(out) :: mesh
  integer :: rc

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
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

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5, &
                   1.5,0.5, &
                   0.5,1.5, &
                   1.5,1.5/)

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

       !! elem coords
       allocate(elemCoords(2*numElems))
       elemCoords=(/0.5,0.5/)

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

       !! elem coords
       allocate(elemCoords(2*numElems))
       elemCoords=(/1.5,0.5/)

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

       !! elem coords
       allocate(elemCoords(2*numElems))
       elemCoords=(/0.5,1.5/)

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

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.5,1.5/)

       !! elem conn
       allocate(elemConn(numElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif
   
   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)

end subroutine createTestMeshForDual



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   3.0   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  10    / |
  !   2.5   |    7    |    8     |     /    |
  !         |         |          |  /    9  |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    4    |    5     |    6     |
  !         |         |          |          |
  !   1.0   5 ------- 6 -------- 7 -------- 8
  !         |         |          |          |
  !   0.5   |    1    |    2     |    3     |
  !         |         |          |          |
  !   0.0   1 ------- 2 -------- 3 -------- 4
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5   3.0
   !
  !               Node Ids at corners
  !              Element Ids in centers
  ! 
   !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !                   Mesh Owners
  !
  !   3.0   2 ------- 2 -------- 3 -------- 3
  !         |         |          |  3    /  |
  !         |    2    |    2     |     /    |
  !         |         |          |  /    3  |
  !   2.0   2 ------- 2 -------- 3 -------- 3
  !         |         |          |          |
  !         |    2    |    2     |    3     |
  !         |         |          |          |
  !   1.0   0 ------- 0 -------- 1 -------- 1
  !         |         |          |          |
  !         |    0    |    1     |    1     |
  !         |         |          |          |
  !   0.0   0 ------- 0 -------- 1 -------- 1
  !
  !        0.0       1.0        2.0        3.0
  !
  !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine createTestMesh3x3(mesh, rc)
  type(ESMF_Mesh), intent(out) :: mesh
  integer :: rc

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
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
     numNodes=16

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8, &
               9,10,11,12,13,14,&
               15,16/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 2.0,0.0, &  ! 3
                 3.0,0.0, &  ! 4
                 0.0,1.0, &  ! 5
                 1.0,1.0, &  ! 6
                 2.0,1.0, &  ! 7
                 3.0,1.0, &  ! 8
                 0.0,2.0, &  ! 9
                 1.0,2.0, &  ! 10
                 2.0,2.0, &  ! 11
                 3.0,2.0, &  ! 12
                  0.0,3.0, &  ! 13
                 1.0,3.0, &  ! 14
                 2.0,3.0, &  ! 15
                 3.0,3.0 /)  ! 16
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTriElems=2
      numQuadElems=8
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4,5,6,7,8,9,10/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 1
                  ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD, & ! 3
                  ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 6
                   ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 8
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5, & ! 1
                   1.5,0.5, & ! 2
                   2.5,0.5, & ! 3
                   0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                   2.5,1.5, & ! 6
                   0.5,2.5, & ! 7
                   1.5,2.5, & ! 8
                   2.75,2.25,& ! 9
                   2.25,2.75/)  ! 10

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,6,5,   & ! 1
                 2,3,7,6,   & ! 2
                 3,4,8,7,   & ! 3
                 5,6,10,9,  & ! 4
                 6,7,11,10, & ! 5
                 7,8,12,11, & ! 6
                 9,10,14,13, & ! 7
                 10,11,15,14, & ! 8
                 11,12,16, & ! 9
                  11,16,15/) ! 10

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,5,6/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 0.0,1.0, &  ! 5
                 1.0,1.0 /)  ! 6 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems
 
      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! 1

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5/)  ! 1

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3/) ! 1

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,4,6,7,8/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/1.0,0.0, &  ! 2
                   2.0,0.0, &  ! 3
                  3.0,0.0, &  ! 4
                  1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  3.0,1.0 /)  ! 8

 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 2
                   1, & ! 3
                   1, & ! 4
                   0, & ! 6
                   1, & ! 7
                   1/)  ! 8

      ! Fill in elem data
      numTriElems=0
      numQuadElems=2
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/2,3/)

      !! elem types
      allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD/)  ! 3

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.5,0.5, & ! 2
                   2.5,0.5/)  ! 3


      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,   & ! 2
                 2,3,6,5/)    ! 3

     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/5,6,7,   &
               9,10,11, &
               13,14,15/)

     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,1.0, &  ! 5
                   1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  0.0,2.0, &  ! 9 
                  1.0,2.0, &  ! 10
                  2.0,2.0, &  ! 11
                  0.0,3.0, &  ! 13
                  1.0,3.0, &  ! 14
                  2.0,3.0/)  ! 15
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 5
                   0, & ! 6
                   1, & ! 7
                   2, & ! 9
                   2, & ! 10
                   3, & ! 11
                   2, & ! 13
                   2, & ! 14
                   3/)  ! 15


      ! Fill in elem data
      numTriElems=0
      numQuadElems=4
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

       !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/4,5,7,8/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD/)  ! 8


      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                   0.5,2.5, & ! 7
                   1.5,2.5/)  ! 8

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,  & ! 4
                 2,3,6,5,  & ! 5
                 4,5,8,7,  & ! 7
                 5,6,9,8/)   ! 8
     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6
 
     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/7,8,11,12,15,16/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/2.0,1.0, &  ! 7
                  3.0,1.0, &  ! 8
                  2.0,2.0, &  ! 11
                  3.0,2.0, &  ! 12
                  2.0,3.0, &  ! 15
                  3.0,3.0 /)  ! 16
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/1, & ! 7
                   1, & ! 8
                   3, & ! 11
                   3, & ! 12
                   3, & ! 15
                   3/)  ! 16

      ! Fill in elem data
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems
 
      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,9,10/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 6
                   2.75,2.25,& ! 9
                   2.25,2.75/)  ! 10

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3, & ! 6
                 3,4,6, & ! 9
                 3,6,5/) ! 10
     endif
   endif

   
   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)

end subroutine createTestMesh3x3

!
!  2.5        8        10 --------11   
!          /     \         \      |
!  2.1   7         9           \  12
!        |         | \    5       
!        |    4    |   \
!        |         |      \
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
! -0.1   1 ------- 2 ------- 3
!
!      -0.1       1.0       2.1   2.5 
! 
!        Node Id labels at corners
!       Element Id labels in centers
subroutine createTestMeshMultiElem(mesh, rc)
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
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numMultiElems,numTotElems
  integer :: numElemConn

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

  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=12

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12/) 

      ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   0.5, 2.5, & ! node id 8
                   1.0, 2.1, & ! node id 9
                   1.5, 2.5, & ! node id 10
                   2.5, 2.5, & ! node id 11
                   2.5, 2.1/)  ! node id 12

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus tot and num conn.
     numQuadElems=1
     numTriElems=2
     numPentElems=1
     numMultiElems=1
     numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
     numElemConn=3*numTriElems+4*numQuadElems+ &
                 5*numPentElems+7*numMultiElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                  ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 5, &                      ! elem id 4
                 7/)                       ! elem id 5


     ! Allocate and fill the element connection type array.
      ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(numElemConn))
     elemConn=(/1,2,5,4, &       ! elem id 1
                2,3,5,   &       ! elem id 2
                3,6,5,   &       ! elem id 3
                4,5,9,8,7, &     ! elem id 4
                5,6,9,ESMF_MESH_POLYBREAK,12,11,10/) ! elem id 5

 else if (petCount .eq. 4) then
      ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
        ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=1
       numTriElems=0
       numPentElems=0
       numMultiElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+7*numMultiElems
       
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
        allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                     2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=2
       numPentElems=0
       numMultiElems=0
       numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+7*numMultiElems
 
        ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(numElemConn))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=5

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
         nodeIds=(/4,5,7,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      0.5,2.5, & ! node id 8
                      1.0,2.1 /) ! node id 9

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
         allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     2/)  ! node id 9
 
       ! Set the number of each type of element, plus tot and num conn.
       numQuadElems=0
       numTriElems=0
       numPentElems=1
       numMultiElems=0
        numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
       numElemConn=3*numTriElems+4*numQuadElems+ &
            5*numPentElems+7*numMultiElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/5/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
         elemConn=(/1,2,5,4,3/) ! elem id 4

      else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=6

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,9,10,11,12/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 9
                     1.5,2.5, &  ! node id 10
                     2.5,2.5, &  ! node id 11
                     2.5,2.1 /)  ! node id 12
 
        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 9
                     3, & ! node id 10
                     3, & ! node id 11
                     3/)  ! node id 12
 
 
        ! Set the number of each type of element, plus tot and num conn.
        numQuadElems=0
         numTriElems=0
        numPentElems=0
        numMultiElems=1
        numTotElems=numTriElems+numQuadElems+numPentElems+numMultiElems
        numElemConn=3*numTriElems+4*numQuadElems+ &
             5*numPentElems+7*numMultiElems

 
        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/7/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,3,ESMF_MESH_POLYBREAK,6,5,4/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
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

end subroutine createTestMeshMultiElem

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   3.0   13 ------ 14 ------- 15 ------- 16
  !         |         |          |          |
  !   2.5   |    7    |    8     |     9    |
  !         |         |          |          |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    4    |    5     |    6     |
  !         |         |          |          |
  !   1.0   5 ------- 6 -------- 7 -------- 8
  !         |         |          |          |
  !   0.5   |    1    |    2     |    3     |
  !         |         |          |          |
  !   0.0   1 ------- 2 -------- 3 -------- 4
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5   3.0
   !
  !               Node Ids at corners
  !              Element Ids in centers
  ! 
   !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !                   Mesh Owners
  !
  !   3.0   2 ------- 2 -------- 3 -------- 3
  !         |         |          |          |
  !         |    2    |    2     |     3    |
   !         |         |          |          |
  !   2.0   2 ------- 2 -------- 3 -------- 3
  !         |         |          |          |
  !         |    2    |    2     |    3     |
  !         |         |          |          |
  !   1.0   0 ------- 0 -------- 1 -------- 1
  !         |         |          |          |
  !         |    0    |    1     |    1     |
  !         |         |          |          |
  !   0.0   0 ------- 0 -------- 1 -------- 1
  !
  !        0.0       1.0        2.0        3.0
  !
   !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine createTestMesh3x3_notris(mesh, rc)
  type(ESMF_Mesh), intent(out) :: mesh
  integer :: rc

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
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
     numNodes=16

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8, &
               9,10,11,12,13,14,&
                15,16/) 
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 2.0,0.0, &  ! 3
                 3.0,0.0, &  ! 4
                 0.0,1.0, &  ! 5
                 1.0,1.0, &  ! 6
                 2.0,1.0, &  ! 7
                 3.0,1.0, &  ! 8
                 0.0,2.0, &  ! 9
                  1.0,2.0, &  ! 10
                 2.0,2.0, &  ! 11
                 3.0,2.0, &  ! 12
                  0.0,3.0, &  ! 13
                 1.0,3.0, &  ! 14
                 2.0,3.0, &  ! 15
                 3.0,3.0 /)  ! 16
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTriElems=0
       numQuadElems=9
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4,5,6,7,8,9/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 1
                  ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD, & ! 3
                   ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 6
                   ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 8
                  ESMF_MESHELEMTYPE_QUAD/)   ! 9

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5, & ! 1
                   1.5,0.5, & ! 2
                   2.5,0.5, & ! 3
                   0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                   2.5,1.5, & ! 6
                   0.5,2.5, & ! 7
                    1.5,2.5, & ! 8
                   2.5,2.5/)  ! 9

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,6,5,   & ! 1
                 2,3,7,6,   & ! 2
                 3,4,8,7,   & ! 3
                 5,6,10,9,  & ! 4
                 6,7,11,10, & ! 5
                 7,8,12,11, & ! 6
                 9,10,14,13, & ! 7
                 10,11,15,14, & ! 8
                 11,12,16,15/) ! 9

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,5,6/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 0.0,1.0, &  ! 5
                 1.0,1.0 /)  ! 6 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems
 
      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! 1

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5/)  ! 1

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3/) ! 1

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,4,6,7,8/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/1.0,0.0, &  ! 2
                   2.0,0.0, &  ! 3
                  3.0,0.0, &  ! 4
                  1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  3.0,1.0 /)  ! 8

  

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 2
                   1, & ! 3
                   1, & ! 4
                   0, & ! 6
                   1, & ! 7
                   1/)  ! 8

      ! Fill in elem data
      numTriElems=0
       numQuadElems=2
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/2,3/)

      !! elem types
      allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD/)  ! 3

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.5,0.5, & ! 2
                   2.5,0.5/)  ! 3


      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,   & ! 2
                 2,3,6,5/)    ! 3

     else if (localPet .eq. 2) then

      ! Fill in node data
     numNodes=9

      !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/5,6,7,   &
               9,10,11, &
               13,14,15/)

     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,1.0, &  ! 5
                   1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  0.0,2.0, &  ! 9 
                  1.0,2.0, &  ! 10
                  2.0,2.0, &  ! 11
                  0.0,3.0, &  ! 13
                  1.0,3.0, &  ! 14
                  2.0,3.0/)  ! 15
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 5
                   0, & ! 6
                   1, & ! 7
                   2, & ! 9
                    2, & ! 10
                   3, & ! 11
                   2, & ! 13
                   2, & ! 14
                   3/)  ! 15


      ! Fill in elem data
      numTriElems=0
      numQuadElems=4
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

       !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/4,5,7,8/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD/)  ! 8


      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                    0.5,2.5, & ! 7
                   1.5,2.5/)  ! 8

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,  & ! 4
                 2,3,6,5,  & ! 5
                 4,5,8,7,  & ! 7
                 5,6,9,8/)   ! 8
     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6
 
     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/7,8,11,12,15,16/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/2.0,1.0, &  ! 7
                  3.0,1.0, &  ! 8
                  2.0,2.0, &  ! 11
                  3.0,2.0, &  ! 12
                  2.0,3.0, &  ! 15
                  3.0,3.0 /)  ! 16
 

      !! node owners
       allocate(nodeOwners(numNodes))
      nodeOwners=(/1, & ! 7
                   1, & ! 8
                   3, & ! 11
                   3, & ! 12
                   3, & ! 15
                   3/)  ! 16

      ! Fill in elem data
      numTriElems=0
      numQuadElems=2
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems
 
      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,9/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_QUAD/)   ! 9

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 6
                   2.5,2.5/)  ! 9

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3, & ! 6
                 3,4,6,5/) ! 9
     endif
   endif

   
   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)

end subroutine createTestMesh3x3_notris


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   3.0   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  10    / |
  !   2.5   |    7    |    8     |     /    |
  !         |         |          |  /    9  |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    4    |    5     |    6     |
  !         |         |          |          |
  !   1.0   5 ------- 6 -------- 7 -------- 8
  !         |         |          |          |
  !   0.5   |    1    |    2     |    3     |
  !         |         |          |          |
  !   0.0   1 ------- 2 -------- 3 -------- 4
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5   3.0
   !
  !               Node Ids at corners
  !              Element Ids in centers
  ! 
   !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !                   Mesh Owners
  !
  !   3.0   2 ------- 2 -------- 3 -------- 3
  !         |         |          |  3    /  |
  !         |    2    |    2     |     /    |
  !         |         |          |  /    3  |
  !   2.0   2 ------- 2 -------- 3 -------- 3
  !         |         |          |          |
  !         |    2    |    2     |    3     |
  !         |         |          |          |
  !   1.0   0 ------- 0 -------- 1 -------- 1
  !         |         |          |          |
  !         |    0    |    1     |    1     |
  !         |         |          |          |
  !   0.0   0 ------- 0 -------- 1 -------- 1
  !
  !        0.0       1.0        2.0        3.0
  !
  !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine test_mesh_create_gt_1localde(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: nodeLocalDECount
  type(ESMF_PtrInt1D), allocatable :: nodeArbSeqIndexL(:)
  integer :: elemLocalDECount
  type(ESMF_PtrInt1D), allocatable :: elemArbSeqIndexL(:)
  integer :: i
  integer :: dimCount, localDECount

 ! XMRKX

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
     numNodes=16

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8, &
               9,10,11,12,13,14,&
               15,16/) 
     

     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 2.0,0.0, &  ! 3
                 3.0,0.0, &  ! 4
                 0.0,1.0, &  ! 5
                 1.0,1.0, &  ! 6
                 2.0,1.0, &  ! 7
                 3.0,1.0, &  ! 8
                 0.0,2.0, &  ! 9
                 1.0,2.0, &  ! 10
                 2.0,2.0, &  ! 11
                 3.0,2.0, &  ! 12
                  0.0,3.0, &  ! 13
                 1.0,3.0, &  ! 14
                 2.0,3.0, &  ! 15
                 3.0,3.0 /)  ! 16
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0

      !! Build node localDE lists
      nodeLocalDECount=2
      allocate(nodeArbSeqIndexL(nodeLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(nodeArbSeqIndexL(1)%ptr(6))
      nodeArbSeqIndexL(1)%ptr=(/1,2,3,4,5,6/)

      allocate(nodeArbSeqIndexL(2)%ptr(10))
      nodeArbSeqIndexL(2)%ptr=(/7,8,9,10,11,12,13,14,15,16/)

      ! Fill in elem data
      numTriElems=2
      numQuadElems=8
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4,5,6,7,8,9,10/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 1
                  ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD, & ! 3
                  ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 8
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5, & ! 1
                   1.5,0.5, & ! 2
                   2.5,0.5, & ! 3
                   0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                   2.5,1.5, & ! 6
                   0.5,2.5, & ! 7
                   1.5,2.5, & ! 8
                   2.75,2.25,& ! 9
                   2.25,2.75/)  ! 10

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,6,5,   & ! 1
                 2,3,7,6,   & ! 2
                 3,4,8,7,   & ! 3
                 5,6,10,9,  & ! 4
                 6,7,11,10, & ! 5
                 7,8,12,11, & ! 6
                 9,10,14,13, & ! 7
                 10,11,15,14, & ! 8
                 11,12,16, & ! 9
                  11,16,15/) ! 10

      !! Build node localDE lists
      elemLocalDECount=2
      allocate(elemArbSeqIndexL(elemLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(elemArbSeqIndexL(1)%ptr(4))
      elemArbSeqIndexL(1)%ptr=(/1,2,3,4/)

      allocate(elemArbSeqIndexL(2)%ptr(6))
      elemArbSeqIndexL(2)%ptr=(/5,6,7,8,9,10/)


   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,5,6/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 0.0,1.0, &  ! 5
                 1.0,1.0 /)  ! 6 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0

      !! Build node localDE lists
      nodeLocalDECount=2
      allocate(nodeArbSeqIndexL(nodeLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(nodeArbSeqIndexL(1)%ptr(2))
      nodeArbSeqIndexL(1)%ptr=(/1,2/)

      allocate(nodeArbSeqIndexL(2)%ptr(2))
      nodeArbSeqIndexL(2)%ptr=(/5,6/)

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems
 
      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! 1

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5/)  ! 1

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3/) ! 1

      !! Build node localDE lists
      elemLocalDECount=1
      allocate(elemArbSeqIndexL(elemLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(elemArbSeqIndexL(1)%ptr(1))
      elemArbSeqIndexL(1)%ptr=(/1/)

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,4,6,7,8/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/1.0,0.0, &  ! 2
                   2.0,0.0, &  ! 3
                  3.0,0.0, &  ! 4
                  1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  3.0,1.0 /)  ! 8

 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 2
                   1, & ! 3
                   1, & ! 4
                   0, & ! 6
                   1, & ! 7
                   1/)  ! 8


      !! Build node localDE lists
      nodeLocalDECount=2
      allocate(nodeArbSeqIndexL(nodeLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(nodeArbSeqIndexL(1)%ptr(2))
      nodeArbSeqIndexL(1)%ptr=(/3,4/)

      allocate(nodeArbSeqIndexL(2)%ptr(2))
      nodeArbSeqIndexL(2)%ptr=(/7,8/)


      ! Fill in elem data
      numTriElems=0
      numQuadElems=2
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/2,3/)

      !! elem types
      allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD/)  ! 3

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.5,0.5, & ! 2
                   2.5,0.5/)  ! 3


      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,   & ! 2
                 2,3,6,5/)    ! 3


      !! Build node localDE lists
      elemLocalDECount=2
      allocate(elemArbSeqIndexL(elemLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(elemArbSeqIndexL(1)%ptr(1))
      elemArbSeqIndexL(1)%ptr=(/2/)

      allocate(elemArbSeqIndexL(2)%ptr(1))
      elemArbSeqIndexL(2)%ptr=(/3/)

     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/5,6,7,   &
               9,10,11, &
               13,14,15/)

     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,1.0, &  ! 5
                   1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  0.0,2.0, &  ! 9 
                  1.0,2.0, &  ! 10
                  2.0,2.0, &  ! 11
                  0.0,3.0, &  ! 13
                  1.0,3.0, &  ! 14
                  2.0,3.0/)  ! 15
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 5
                   0, & ! 6
                   1, & ! 7
                   2, & ! 9
                   2, & ! 10
                   3, & ! 11
                   2, & ! 13
                   2, & ! 14
                   3/)  ! 15

      !! Build node localDE lists
      nodeLocalDECount=2
      allocate(nodeArbSeqIndexL(nodeLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(nodeArbSeqIndexL(1)%ptr(2))
      nodeArbSeqIndexL(1)%ptr=(/9,10/)

      allocate(nodeArbSeqIndexL(2)%ptr(2))
      nodeArbSeqIndexL(2)%ptr=(/13,14/)

      ! Fill in elem data
      numTriElems=0
      numQuadElems=4
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

       !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/4,5,7,8/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD/)  ! 8


      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                   0.5,2.5, & ! 7
                   1.5,2.5/)  ! 8

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,  & ! 4
                 2,3,6,5,  & ! 5
                 4,5,8,7,  & ! 7
                 5,6,9,8/)   ! 8

      !! Build node localDE lists
      elemLocalDECount=2
      allocate(elemArbSeqIndexL(elemLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(elemArbSeqIndexL(1)%ptr(3))
      elemArbSeqIndexL(1)%ptr=(/4,5,7/)

      allocate(elemArbSeqIndexL(2)%ptr(1))
      elemArbSeqIndexL(2)%ptr=(/8/)

     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6
 
     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/7,8,11,12,15,16/)
     
     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/2.0,1.0, &  ! 7
                  3.0,1.0, &  ! 8
                  2.0,2.0, &  ! 11
                  3.0,2.0, &  ! 12
                  2.0,3.0, &  ! 15
                  3.0,3.0 /)  ! 16
 

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/1, & ! 7
                   1, & ! 8
                   3, & ! 11
                   3, & ! 12
                   3, & ! 15
                   3/)  ! 16

      !! Build node localDE lists
      nodeLocalDECount=1
      allocate(nodeArbSeqIndexL(nodeLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(nodeArbSeqIndexL(1)%ptr(4))
      nodeArbSeqIndexL(1)%ptr=(/11,12,15,16/)

      ! Fill in elem data
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems
 
      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,9,10/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 6
                   2.75,2.25,& ! 9
                   2.25,2.75/)  ! 10

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3, & ! 6
                 3,4,6, & ! 9
                 3,6,5/) ! 10

      !! Build node localDE lists
      elemLocalDECount=2
      allocate(elemArbSeqIndexL(elemLocalDeCount)) 
  
      !! Allocate and fill first list
      allocate(elemArbSeqIndexL(1)%ptr(1))
      elemArbSeqIndexL(1)%ptr=(/6/)

      allocate(elemArbSeqIndexL(2)%ptr(2))
      elemArbSeqIndexL(2)%ptr=(/9,10/)

     endif
   endif


   ! Create node distgrid
   nodeDistgrid = ESMF_DistGridCreate(arbSeqIndexList=nodeArbSeqIndexL, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Create elem distgrid
   elemDistgrid = ESMF_DistGridCreate(arbSeqIndexList=elemArbSeqIndexL, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, nodalDistgrid=nodeDistgrid, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)

   ! Deallocate sequence lists
   do i=1,nodeLocalDECount
      deallocate(nodeArbSeqIndexL(i)%ptr)
   enddo
   deallocate(nodeArbSeqIndexL)

   do i=1,elemLocalDECount
      deallocate(elemArbSeqIndexL(i)%ptr)
   enddo
   deallocate(elemArbSeqIndexL)

   ! Init correct to true before looking for problems
   correct=.true.

  ! Test creating field
  nodeField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, &
       meshloc=ESMF_MESHLOC_NODE, &
       rc=rc)
  if (rc .ne. ESMF_SUCCESS) return

  ! check field
  call ESMF_FieldValidate(nodeField, rc=rc) 
  if (rc .ne. ESMF_SUCCESS) return

  ! Get Field info
  call ESMF_FieldGet(nodeField, dimCount=dimCount, &
       localDECount=localDECount, rc=rc)
  if (rc .ne. ESMF_SUCCESS) return

  ! check info
  if (dimCount .ne. 1) correct=.false.
  if (localDECount .ne. nodeLocalDECount) correct=.false.


  ! Test creating field
  elemField = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, &
       meshloc=ESMF_MESHLOC_ELEMENT, &
       rc=rc)
  if (rc .ne. ESMF_SUCCESS) return

  ! check field
  call ESMF_FieldValidate(elemField, rc=rc) 
  if (rc .ne. ESMF_SUCCESS) return

  ! Get Field info
  call ESMF_FieldGet(elemField, dimCount=dimCount, &
       localDECount=localDECount, rc=rc)
  if (rc .ne. ESMF_SUCCESS) return

  ! check info
  if (dimCount .ne. 1) correct=.false.
  if (localDECount .ne. elemLocalDECount) correct=.false.


   ! Get rid of node distgrid
   call ESMF_DistgridDestroy(nodeDistgrid, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Get rid of elem distgrid
   call ESMF_DistgridDestroy(elemDistgrid, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Get rid of test fields
   call ESMF_FieldDestroy(nodeField, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   call ESMF_FieldDestroy(elemField, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine test_mesh_create_gt_1localde



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   3.0   * ------- *          * -------  *
  !         |         |          |  10    / |
  !   2.5   |    7    |          |     /    |
  !         |         |          |  /    9  |
  !   2.0   * ------- *          * -------  *
  !         
  !   1.5   
  !         
  !   1.0   * ------- *          * -------- *
  !         |         |          |          |
  !   0.5   |    1    |          |    3     |
  !         |         |          |          |
  !   0.0   * ------- *          * -------- 4
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5   3.0
  !
  !               Node Ids at corners
  !              Element Ids in centers
  ! 
   !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !                   Mesh Owners
  !
  !   3.0   * ------- *          * -------  *
  !         |         |          |  2     / |
  !   2.5   |    2    |          |     /    |
  !         |         |          |  /    2  |
  !   2.0   * ------- *          * -------  *
  !         
  !   1.5   
  !         
  !   1.0   * ------- *          * -------- *
  !         |         |          |          |
  !   0.5   |    0    |          |    1     |
  !         |         |          |          |
  !   0.0   * ------- *          * -------- 4
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5   3.0
  !
  !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine test_mesh_create_easy_elems(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer :: numElems,numOwnedElemsTst
  integer :: numElemCorners, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:,:)
  real(ESMF_KIND_R8), pointer :: elemCornerCoords(:,:)
  integer, pointer :: elemIds(:),elemTypes(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_Field)  ::  field

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

      ! Fill in elem data
      numTriElems=2
      numQuadElems=3
      numElems=numTriElems+numQuadElems
      numElemCorners=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,3,7,9,10/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 1
                  ESMF_MESHELEMTYPE_QUAD, & ! 3
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/0.5,0.5/)   ! 1
      elemCoords(:,2)=(/2.5,0.5/)   ! 3
      elemCoords(:,3)=(/0.5,2.5/)   ! 7
      elemCoords(:,4)=(/2.75,2.25/) ! 9
      elemCoords(:,5)=(/2.25,2.75/) ! 10
     
     !! elem corner Coords
     allocate(elemCornerCoords(2,numElemCorners))
     elemCornerCoords(:,1)=(/0.0,0.0/) ! 1
     elemCornerCoords(:,2)=(/1.0,0.0/) ! 1
     elemCornerCoords(:,3)=(/1.0,1.0/)  ! 1
     elemCornerCoords(:,4)=(/0.0,1.0/)  ! 1
     elemCornerCoords(:,5)=(/2.0,0.0/)  ! 3
     elemCornerCoords(:,6)=(/3.0,0.0/)  ! 3
     elemCornerCoords(:,7)=(/3.0,1.0/)  ! 3
     elemCornerCoords(:,8)=(/2.0,1.0/)  ! 3
     elemCornerCoords(:,9)=(/0.0,2.0/)  ! 7
     elemCornerCoords(:,10)=(/1.0,2.0/)  ! 7
     elemCornerCoords(:,11)=(/1.0,3.0/)  ! 7
     elemCornerCoords(:,12)=(/0.0,3.0/)  ! 7
     elemCornerCoords(:,13)=(/2.0,2.0/)  ! 9
     elemCornerCoords(:,14)=(/3.0,2.0/)  ! 9
     elemCornerCoords(:,15)=(/3.0,3.0/)  ! 9
     elemCornerCoords(:,16)=(/2.0,2.0/)  ! 10
     elemCornerCoords(:,17)=(/3.0,3.0/)  ! 10
     elemCornerCoords(:,18)=(/2.0,3.0 /)  ! 10
 
   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemCorners=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! 1

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/0.5,0.5/) ! 1

     
     !! elem corner Coords
     allocate(elemCornerCoords(2,numElemCorners))
     elemCornerCoords(:,1)=(/0.0,0.0/)  ! 1
     elemCornerCoords(:,2)=(/1.0,0.0/)  ! 1
     elemCornerCoords(:,3)=(/1.0,1.0/)  ! 1
     elemCornerCoords(:,4)=(/0.0,1.0/)   ! 1

     else if (localPet .eq. 1) then

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemCorners=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/3/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/)  ! 3

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/2.5,0.5/) ! 3

     !! elem corner Coords
     allocate(elemCornerCoords(2,numElemCorners))
     elemCornerCoords(:,1)=(/2.0,0.0/)  ! 3
     elemCornerCoords(:,2)=(/3.0,0.0/)  ! 3
     elemCornerCoords(:,3)=(/3.0,1.0/)  ! 3
     elemCornerCoords(:,4)=(/2.0,1.0/)  ! 3

     else if (localPet .eq. 2) then

      ! Fill in elem data
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemCorners=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/7,9,10/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/0.5,2.5/) ! 7
      elemCoords(:,2)=(/2.75,2.25/) ! 9
      elemCoords(:,3)=(/2.25,2.75/) ! 10

     !! elem corner Coords
     allocate(elemCornerCoords(2,numElemCorners))
     elemCornerCoords(:,1)=(/0.0,2.0/)  ! 7
     elemCornerCoords(:,2)=(/1.0,2.0/)  ! 7
     elemCornerCoords(:,3)=(/1.0,3.0/)  ! 7
     elemCornerCoords(:,4)=(/0.0,3.0/)  ! 7
     elemCornerCoords(:,5)=(/2.0,2.0/)  ! 9
     elemCornerCoords(:,6)=(/3.0,2.0/)  ! 9
     elemCornerCoords(:,7)=(/3.0,3.0/)  ! 9
     elemCornerCoords(:,8)=(/2.0,2.0/)  ! 10
     elemCornerCoords(:,9)=(/3.0,3.0/)  ! 10
     elemCornerCoords(:,10)=(/2.0,3.0 /)  ! 10

     else if (localPet .eq. 3) then

      ! Fill in elem data
      numTriElems=0
      numQuadElems=0
      numElems=numTriElems+numQuadElems
      numElemCorners=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))

      !! elem types
      allocate(elemTypes(numElems))

      !! elem coords
      allocate(elemCoords(2,numElems))

     !! elem corner Coords
     allocate(elemCornerCoords(2,numElemCorners))

     endif
   endif

   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        elementTypes=elemTypes,&
        elementCoords=elemCoords,&
        elementCornerCoords=elemCornerCoords, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemCornerCoords)

   ! Output Mesh for debugging
   !call ESMF_MeshWrite(mesh,"meshee",rc=localrc)
   !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Return success
   rc=ESMF_SUCCESS

end subroutine test_mesh_create_easy_elems



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   3.0   * ------- *          * ------- *
  !         |         |          |         |
  !   2.5   |    7    |          |    9    |
  !         |         |          |         |
  !   2.0   * ------- *          * ------- *
  !         
  !   1.5   
  !         
  !   1.0   * ------- *          * ------- *
  !         |         |          |         |
  !   0.5   |    1    |          |    3    |
  !         |         |          |         |
  !   0.0   * ------- *          * ------- *
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5  3.0
  !
  !               Node Ids at corners
  !              Element Ids in centers
  ! 
   !!!!! 
  ! 
  ! The owners for 1 PET are all Pet 0.
  ! The owners for 4 PETs are as follows:
  !
  !                   Mesh Owners
  !
  !   3.0   * ------- *          * ------- *
  !         |         |          |         |
  !   2.5   |    2    |          |    2    |
  !         |         |          |         |
  !   2.0   * ------- *          * ------- *
  !         
  !   1.5   
  !         
  !   1.0   * ------- *          * --------*
  !         |         |          |         |
  !   0.5   |    0    |          |    1    |
  !         |         |          |         |
  !   0.0   * ------- *          * --------*
  !            
  !        0.0  0.5  1.0  1.5   2.0  2.5  3.0
  !
  !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine test_mesh_create_ee_1type(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer :: numElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:,:)
  real(ESMF_KIND_R8), pointer :: elemCornerCoords(:,:,:)
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

      ! Fill in elem data
      numElems=4

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,3,7,9/) 

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/0.5,0.5/)   ! 1
      elemCoords(:,2)=(/2.5,0.5/)   ! 3
      elemCoords(:,3)=(/0.5,2.5/)   ! 7
      elemCoords(:,4)=(/2.5,2.5/) ! 9
     
     !! elem corner Coords
     allocate(elemCornerCoords(2,4,numElems))
     elemCornerCoords(:,1,1)=(/0.0,0.0/)  ! 1
     elemCornerCoords(:,2,1)=(/1.0,0.0/)  ! 1
     elemCornerCoords(:,3,1)=(/1.0,1.0/)  ! 1
     elemCornerCoords(:,4,1)=(/0.0,1.0/)  ! 1

     elemCornerCoords(:,1,2)=(/2.0,0.0/)  ! 3
     elemCornerCoords(:,2,2)=(/3.0,0.0/)  ! 3
     elemCornerCoords(:,3,2)=(/3.0,1.0/)  ! 3
     elemCornerCoords(:,4,2)=(/2.0,1.0/)  ! 3

     elemCornerCoords(:,1,3)=(/0.0,2.0/)  ! 7
     elemCornerCoords(:,2,3)=(/1.0,2.0/)  ! 7
     elemCornerCoords(:,3,3)=(/1.0,3.0/)  ! 7
     elemCornerCoords(:,4,3)=(/0.0,3.0/)  ! 7

     elemCornerCoords(:,1,4)=(/2.0,2.0/)  ! 9
     elemCornerCoords(:,2,4)=(/3.0,2.0/)  ! 9
     elemCornerCoords(:,3,4)=(/3.0,3.0/)  ! 9
     elemCornerCoords(:,4,4)=(/2.0,3.0/)  ! 9
 
   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

      ! Fill in elem data
      numElems=1

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/) 

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/0.5,0.5/) ! 1
     
     !! elem corner Coords
     allocate(elemCornerCoords(2,4,numElems))
     elemCornerCoords(:,1,1)=(/0.0,0.0/)  ! 1
     elemCornerCoords(:,2,1)=(/1.0,0.0/)  ! 1
     elemCornerCoords(:,3,1)=(/1.0,1.0/)  ! 1
     elemCornerCoords(:,4,1)=(/0.0,1.0/)  ! 1

     else if (localPet .eq. 1) then

      ! Fill in elem data
      numElems=1

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/3/) 

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/2.5,0.5/) ! 3

     !! elem corner Coords
     allocate(elemCornerCoords(2,4,numElems))
     elemCornerCoords(:,1,1)=(/2.0,0.0/)  ! 3
     elemCornerCoords(:,2,1)=(/3.0,0.0/)  ! 3
     elemCornerCoords(:,3,1)=(/3.0,1.0/)  ! 3
     elemCornerCoords(:,4,1)=(/2.0,1.0/)  ! 3

     else if (localPet .eq. 2) then

      ! Fill in elem data
      numElems=2

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/7,9/) 

      !! elem coords
      allocate(elemCoords(2,numElems))
      elemCoords(:,1)=(/0.5,2.5/) ! 7
      elemCoords(:,2)=(/2.5,2.5/) ! 9

     !! elem corner Coords
     allocate(elemCornerCoords(2,4,numElems))
     elemCornerCoords(:,1,1)=(/0.0,2.0/)  ! 7
     elemCornerCoords(:,2,1)=(/1.0,2.0/)  ! 7
     elemCornerCoords(:,3,1)=(/1.0,3.0/)  ! 7
     elemCornerCoords(:,4,1)=(/0.0,3.0/)  ! 7

     elemCornerCoords(:,1,2)=(/2.0,2.0/)  ! 9
     elemCornerCoords(:,2,2)=(/3.0,2.0/)  ! 9
     elemCornerCoords(:,3,2)=(/3.0,3.0/)  ! 9
     elemCornerCoords(:,4,2)=(/2.0,3.0/)  ! 9

     else if (localPet .eq. 3) then

      ! Fill in elem data
      numElems=0

      !! elem ids
      allocate(elemIds(numElems))

      !! elem coords
      allocate(elemCoords(2,numElems))

     !! elem corner Coords
     allocate(elemCornerCoords(2,4,numElems))

     endif
   endif

   ! Create Mesh structure in 1 step
   mesh=ESMF_MeshCreate(parametricDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        elementIds=elemIds,&
        elementType=ESMF_MESHELEMTYPE_QUAD,&
        elementCoords=elemCoords,&
        elementCornerCoords=elemCornerCoords, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemCoords)
   deallocate(elemCornerCoords)

   ! Output Mesh for debugging
   call ESMF_MeshWrite(mesh,"meshee1t",rc=localrc)
   if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Return success
   rc=ESMF_SUCCESS

end subroutine test_mesh_create_ee_1type


end program ESMF_MeshUTest

