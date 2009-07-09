! $Id: ESMF_MeshUTest.F90,v 1.11 2009/07/09 22:33:32 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
    '$Id: ESMF_MeshUTest.F90,v 1.11 2009/07/09 22:33:32 oehmke Exp $'
!------------------------------------------------------------------------------

  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name

  !LOCAL VARIABLES:
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: vm
  logical :: correct
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer :: numNodes
  integer :: numElems
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)

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
               2.0,2.0/)

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
  elemTypes=ESMF_MESHELEMENT_QUAD

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
  write(name, *) "Test creating a small 2x2 2D QUAD Mesh in 1 step on 4 procs"
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
       elemTypes=ESMF_MESHELEMENT_QUAD

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
       elemTypes=ESMF_MESHELEMENT_QUAD

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
       elemTypes=ESMF_MESHELEMENT_QUAD

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
       elemTypes=ESMF_MESHELEMENT_QUAD

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

  !! Write mesh for debugging
  call ESMF_MeshWrite(mesh,"tmesh",rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! endif for skip for != 4 procs
  endif 

  call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), name, failMsg, result, ESMF_SRCLINE)


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

