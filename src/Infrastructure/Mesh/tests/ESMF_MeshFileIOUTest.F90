! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_MeshFileIOUTest

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
  logical :: correct


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

  ! Make sure MOAB is off
  call ESMF_MeshSetMOAB(.false.)


 ! This surrounds all the tests to make turning off everything but one test easier
#if 1

  !------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test Mesh create from medium size ESMFMesh file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS
       
  call test_mesh_create_from_med_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical 3x3 ESMFMesh file with a given elem distribution."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_3x3_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical 3x3 ESMFMesh file with a given node distribution."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_node_redist_from_file(correct, rc)


#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------



  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from Cartesian 3x3 ESMFMesh file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_cart_3x3_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from ESMFMesh file with 1D elementConn."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_sph_1DeC_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test coordSys conversion for ESMFMesh file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS
  
  call check_coordSys_convert_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical 3D ESMFMesh file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_3D_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical polybreak ESMFMesh file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_pb_EM_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical 3x3 UGrid file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_3x3_UG_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical polybreak UGrid file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_pb_UG_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical 3x3 SCRIP file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_3x3_SC_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test create Mesh from spherical 3D UGRID file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call check_mesh_from_sph_3D_UG_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test creating a dual mesh from file."
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! initialize check variables
  correct=.true.
  rc=ESMF_SUCCESS

  call test_create_dual_from_file(correct, rc)

#ifdef ESMF_PIO
  call ESMF_Test(((rc.eq.ESMF_SUCCESS)), name, failMsg, result, ESMF_SRCLINE)
#else
  write(failMsg, *) "Did not return ESMC_RC_LIB_NOT_PRESENT"
  call ESMF_Test((rc==ESMC_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE) 
#endif
  !-----------------------------------------------------------------------------
#endif

  !------------------------------------------------------------------------
  call ESMF_TestEnd(ESMF_SRCLINE) ! calls ESMF_Finalize() internally
  !------------------------------------------------------------------------

contains

! Read in a medium sized ESMFMesh file that we got from a user
! and make sure that mesh create from file doesn't return errors.
! Since this is a larger file it's harder to verify that every
! part is correct, so this is more of a smoke test in the unit tests
! to make sure a reasonably sized file will work.
! More precise verifications are done with smaller files in the following tests.
! Larger mesh files are checked in the esmf_extended_tests
subroutine  test_mesh_create_from_med_EM_file(correct, rc)
  logical :: correct
  integer :: rc
  type(ESMF_Mesh) :: mesh
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid, elemDistgrid
  integer, allocatable :: elemIds(:)
  integer :: numElems, numPerPet, numThisPet
  integer :: minId,maxId
  integer :: i, pos

  ! Init correct
  correct=.true.

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) return
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) return

  ! Number of elems in file
  numElems=936

  ! Approx. number per PET
  numPerPet=numElems/petCount

  ! Figure out range for this PET
  minId=localPet*numPerPet+1
  maxId=(localPet+1)*numPerPet
  
  ! If this Pet is last, put the rest there
  if (localPet .eq. petCount-1) maxId=numElems
  
  !write(*,*) localPet,"# ",minId,maxId

  ! Allocate elemIds
  allocate(elemIds(maxId-minId+1))
  
  ! Fill
  pos=1
  do i=minId,maxId
     elemIds(pos)=i
     pos=pos+1
  enddo

  ! Create element Distgrid
  elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
  if (rc /= ESMF_SUCCESS) return
  
  
  ! Create Mesh from file
  mesh=ESMF_MeshCreate("data/ne4np4-esmf.nc", &
       fileformat=ESMF_FILEFORMAT_ESMFMESH, &
       elementDistgrid=elemDistgrid, &
       rc=rc)
  if (rc /= ESMF_SUCCESS) return


  !! Write mesh for debugging
  ! call ESMF_MeshWrite(mesh,"test_mesh",rc=rc)
  ! if (rc /= ESMF_SUCCESS) return

  ! Get rid of Mesh
  call ESMF_MeshDestroy(mesh, rc=rc)
  if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemIds
  deallocate(elemIds)

   ! Return success
   rc=ESMF_SUCCESS

end subroutine test_mesh_create_from_med_EM_file

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
  ! 

  ! This is a more through test that reads in a spherical 3x3 mesh file 
  ! (that creates the mesh drawn above) and verifies that every part
  ! is correct. 
subroutine  check_mesh_from_sph_3x3_EM_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0,1,0,1,0, &
                1,0,1,0,1,0,&
                1,0/)


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


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 1, 1, 1, 1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.1_ESMF_KIND_R8, 1.2_ESMF_KIND_R8, 1.3_ESMF_KIND_R8, 1.4_ESMF_KIND_R8, &
           1.5_ESMF_KIND_R8, 1.6_ESMF_KIND_R8, 1.7_ESMF_KIND_R8, 1.8_ESMF_KIND_R8, 0.59_ESMF_KIND_R8, 0.5_ESMF_KIND_R8/)


   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,5,6/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0/)


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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.1_ESMF_KIND_R8/)

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,4,6,7,8/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/0,1,0,0,1,0/)

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.2_ESMF_KIND_R8,1.3_ESMF_KIND_R8/)

     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/5,6,7,   &
               9,10,11, &
               13,14,15/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1, &
                1,0,1, &
                1,0,1/)

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
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15


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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.4_ESMF_KIND_R8,1.5_ESMF_KIND_R8,1.7_ESMF_KIND_R8,1.8_ESMF_KIND_R8/)

     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/7,8,11,12,15,16/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0,1,0/)

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
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.6_ESMF_KIND_R8,0.59_ESMF_KIND_R8,0.5_ESMF_KIND_R8/)
     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_3x3_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
        addUserArea=.true., &
        elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return



   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElemConns .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst


   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (.not. nodeMaskIsPresentTst) correct=.false.
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (.not. elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(nodeMaskTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))
   allocate(elemMaskTst(numElemsTst))
   allocate(elemAreaTst(numElemsTst))
   allocate(elemCoordsTst(2*numElemsTst))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        nodeMask=nodeMaskTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        elementMask=elemMaskTst, & 
        elementArea=elemAreaTst, & 
        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo


   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check node Mask
   do i=1,numNodesTst
      if (nodeMask(i) .ne. nodeMaskTst(i)) correct=.false.
   enddo


   ! Debug output
   ! write(*,*) "nodeMask   =",nodeMask
   ! write(*,*) "nodeMaskTst=",nodeMaskTst

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Check elem mask
   do i=1,numElems
      if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
   enddo

   ! Check elem area
   do i=1,numElems
      if (elemArea(i) .ne. elemAreaTst(i)) correct=.false.
   enddo

   ! Check elem Coords
   k=1
   do i=1,numElemsTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   deallocate(nodeMask)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)
   deallocate(elemArea)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(nodeMaskTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)
   deallocate(elemMaskTst)
   deallocate(elemAreaTst)
   deallocate(elemCoordsTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return


   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_3x3_EM_file

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
  ! 

  ! This is a more through test that reads in a Cartesian 3x3 mesh file 
  ! (that creates the mesh drawn above) and verifies that every part
  ! is correct. 
subroutine  check_mesh_from_cart_3x3_EM_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 1, 1, 1, 1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.1_ESMF_KIND_R8, 1.2_ESMF_KIND_R8, 1.3_ESMF_KIND_R8, 1.4_ESMF_KIND_R8, &
           1.5_ESMF_KIND_R8, 1.6_ESMF_KIND_R8, 1.7_ESMF_KIND_R8, 1.8_ESMF_KIND_R8, 0.59_ESMF_KIND_R8, 0.5_ESMF_KIND_R8/)


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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.1_ESMF_KIND_R8/)

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.2_ESMF_KIND_R8,1.3_ESMF_KIND_R8/)

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
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15


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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.4_ESMF_KIND_R8,1.5_ESMF_KIND_R8,1.7_ESMF_KIND_R8,1.8_ESMF_KIND_R8/)

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
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.6_ESMF_KIND_R8,0.59_ESMF_KIND_R8,0.5_ESMF_KIND_R8/)
     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_cart_3x3_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
        addUserArea=.true., &
        elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return



   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_CART) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElemConns .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst


   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (      nodeMaskIsPresentTst) correct=.false.  ! There isn't a nodeMask in the file
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (.not. elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))
   allocate(elemMaskTst(numElemsTst))
   allocate(elemAreaTst(numElemsTst))
   allocate(elemCoordsTst(2*numElemsTst))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        elementMask=elemMaskTst, & 
        elementArea=elemAreaTst, & 
        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo


   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Check elem mask
   do i=1,numElems
      if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
   enddo

   ! Check elem area
   do i=1,numElems
      if (elemArea(i) .ne. elemAreaTst(i)) correct=.false.
   enddo

   ! Check elem Coords
   k=1
   do i=1,numElemsTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Debugging
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)
   deallocate(elemArea)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)
   deallocate(elemMaskTst)
   deallocate(elemAreaTst)
   deallocate(elemCoordsTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return


   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_cart_3x3_EM_file



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Creates the following mesh on
  ! 1 or 4 PETs. Returns an error
  ! if run on other than 1 or 4 PETs
  !
  !                     Mesh Ids
  !
  !   3.0   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  12    / |
  !   2.5   |    9    |    10    |     /    |
  !         |         |          |  /    11 |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    6    |    7     |    8     |
  !         |         |          |          |
  !   1.0   5 ------- 6 -------- 7 -------- 8
  !         | 1    /  |  3     / |          |
  !   0.5   |    /    |     /    |    5     |
  !         | /    2  | /     4  |          |
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
  !         | 0    /  | 1     /  |          |
  !         |    /    |    /     |    1     |
  !         | /    0  | /      1 |          |
  !   0.0   0 ------- 0 -------- 1 -------- 1
  !
  !        0.0       1.0        2.0        3.0
  !
  !               Node Owners at corners
  !              Element Owners in centers
  !
  ! 

  ! This is a test that reads in a mesh from an ESMFMesh format file that
  ! has a 1D elementConn array, and verifies that everything is correct.
  ! The mesh is drawn above. 
subroutine  check_mesh_sph_1DeC_EM_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0,1,0,1,0, &
                1,0,1,0,1,0,&
                1,0/)


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
      numTriElems=6
      numQuadElems=6
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4,5,6,7,8,9,10,11,12/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_TRI,  & ! 1
                  ESMF_MESHELEMTYPE_TRI,  & ! 2
                  ESMF_MESHELEMTYPE_TRI,  & ! 3
                  ESMF_MESHELEMTYPE_TRI,  & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 8
                  ESMF_MESHELEMTYPE_QUAD, & ! 9
                  ESMF_MESHELEMTYPE_QUAD, & ! 10
                  ESMF_MESHELEMTYPE_TRI,  & ! 11
                  ESMF_MESHELEMTYPE_TRI/)   ! 12

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.25,0.75, & ! 1
                   0.75,0.25, & ! 2
                   1.25,0.75, & ! 3
                   1.75,0.25, & ! 4
                   2.5,0.5,   & ! 5
                   0.5,1.5,   & ! 6
                   1.5,1.5,   & ! 7
                   2.5,1.5,   & ! 8
                   0.5,2.5,   & ! 9
                   1.5,2.5,   & ! 10
                   2.75,2.25, & ! 11
                   2.25,2.75/)  ! 12

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,6,5,     & ! 1
                 1,2,6,     & ! 2
                 2,7,6,     & ! 3
                 2,3,7,     & ! 4
                 3,4,8,7,   & ! 5
                 5,6,10,9,  & ! 6
                 6,7,11,10, & ! 7
                 7,8,12,11, & ! 8
                 9,10,14,13, & ! 9
                 10,11,15,14, & ! 10
                 11,12,16, & ! 11
                 11,16,15/) ! 12


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/0.51_ESMF_KIND_R8, 0.52_ESMF_KIND_R8, 0.53_ESMF_KIND_R8, 0.54_ESMF_KIND_R8, &
           1.5_ESMF_KIND_R8, 1.6_ESMF_KIND_R8, 1.7_ESMF_KIND_R8, 1.8_ESMF_KIND_R8, &
           1.9_ESMF_KIND_R8, 1.1_ESMF_KIND_R8, 0.511_ESMF_KIND_R8,  0.512_ESMF_KIND_R8/)


   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,5,6/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0/)


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
      numTriElems=2
      numQuadElems=0
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! 1
                  ESMF_MESHELEMTYPE_TRI/)  ! 2

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.25,0.75, &  ! 1
                   0.75,0.25/) ! 2

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,4,3, &  ! 1
                 1,2,4 /)  ! 2

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/0.51_ESMF_KIND_R8, 0.52_ESMF_KIND_R8/)

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,4,6,7,8/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/0,1,0,0,1,0/)

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
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/3,4,5/)

      !! elem types
      allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, &  ! 3
                   ESMF_MESHELEMTYPE_TRI, &  ! 4
                   ESMF_MESHELEMTYPE_QUAD/)  ! 5

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.25,0.75, & ! 3
                   1.75,0.25, & ! 4
                   2.50,0.50/)  ! 5

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,5,4,   &   ! 3
                 1,2,5,   &   ! 4
                 2,3,6,5/)    ! 5

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/0.53_ESMF_KIND_R8,0.54_ESMF_KIND_R8,1.5_ESMF_KIND_R8/)

     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/5,6,7,   &
               9,10,11, &
               13,14,15/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1, &
                1,0,1, &
                1,0,1/)

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
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15

      ! Fill in elem data
      numTriElems=0
      numQuadElems=4
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

       !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,7,9,10/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 9
                  ESMF_MESHELEMTYPE_QUAD/)  ! 10


      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,1.5, & ! 6
                   1.5,1.5, & ! 7
                   0.5,2.5, & ! 9
                   1.5,2.5/)  ! 10

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,  & ! 6
                 2,3,6,5,  & ! 7
                 4,5,8,7,  & ! 9
                 5,6,9,8/)   ! 10

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.6_ESMF_KIND_R8,1.7_ESMF_KIND_R8,1.9_ESMF_KIND_R8,1.1_ESMF_KIND_R8/)

     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/7,8,11,12,15,16/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0,1,0/)

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
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
                   3/)  ! 16

      ! Fill in elem data
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/8,11,12/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 8
                  ESMF_MESHELEMTYPE_TRI,  & ! 11
                  ESMF_MESHELEMTYPE_TRI/)   ! 12

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 8
                   2.75,2.25,& ! 11
                   2.25,2.75/)  ! 12

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3, & ! 8
                 3,4,6, & ! 11
                 3,6,5/) ! 12

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.8_ESMF_KIND_R8,0.511_ESMF_KIND_R8,0.512_ESMF_KIND_R8/)
     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_1D_elemConn_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
        addUserArea=.true., &
        elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_1Dec",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) return

   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElemConns .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst


   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (.not. nodeMaskIsPresentTst) correct=.false.
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (.not. elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(nodeMaskTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))
   allocate(elemMaskTst(numElemsTst))
   allocate(elemAreaTst(numElemsTst))
   allocate(elemCoordsTst(2*numElemsTst))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        nodeMask=nodeMaskTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        elementMask=elemMaskTst, & 
        elementArea=elemAreaTst, & 
        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo


   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check node Mask
   do i=1,numNodesTst
      if (nodeMask(i) .ne. nodeMaskTst(i)) correct=.false.
   enddo


   ! Debug output
   ! write(*,*) "nodeMask   =",nodeMask
   ! write(*,*) "nodeMaskTst=",nodeMaskTst

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Check elem mask
   do i=1,numElems
      if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
   enddo

   ! Check elem area
   do i=1,numElems
      if (elemArea(i) .ne. elemAreaTst(i)) correct=.false.
   enddo

   ! Check elem Coords
   k=1
   do i=1,numElemsTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   deallocate(nodeMask)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)
   deallocate(elemArea)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(nodeMaskTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)
   deallocate(elemMaskTst)
   deallocate(elemAreaTst)
   deallocate(elemCoordsTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_sph_1DeC_EM_file


! This test checks the coordinate system conversion functionality
! It reads in a small 3x3 spherical grid specified in degrees
! in the file and converts it to radians. The file used
! it the same 3x3 spherical one used for other tests in this file.  
subroutine  check_coordSys_convert_EM_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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


      ! Fill in elem data
      numTriElems=2
      numQuadElems=8
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems


      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4,5,6,7,8,9,10/)

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

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4


     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 0.0,1.0, &  ! 5
                 1.0,1.0 /)  ! 6



      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1/)

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,0.5/)  ! 1

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/1.0,0.0, &  ! 2
                   2.0,0.0, &  ! 3
                  3.0,0.0, &  ! 4
                  1.0,1.0, &  ! 6
                  2.0,1.0, &  ! 7
                  3.0,1.0 /)  ! 8

      ! Fill in elem data
      numTriElems=0
      numQuadElems=2
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems


      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/2,3/)

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.5,0.5, & ! 2
                   2.5,0.5/)  ! 3


     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

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


      ! Fill in elem data
      numTriElems=0
      numQuadElems=4
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

       !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/4,5,7,8/)

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,1.5, & ! 4
                   1.5,1.5, & ! 5
                   0.5,2.5, & ! 7
                   1.5,2.5/)  ! 8


     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6

     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/2.0,1.0, &  ! 7
                  3.0,1.0, &  ! 8
                  2.0,2.0, &  ! 11
                  3.0,2.0, &  ! 12
                  2.0,3.0, &  ! 15
                  3.0,3.0 /)  ! 16


      ! Fill in elem data
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,9,10/)

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 6
                   2.75,2.25,& ! 9
                   2.25,2.75/)  ! 10

     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_3x3_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
        addUserArea=.true., &
        coordSys=ESMF_COORDSYS_SPH_RAD, &
        elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_RAD) correct=.false.

   ! Allocate space for tst arrays
   allocate(nodeCoordsTst(2*numNodes))
   allocate(elemCoordsTst(2*numElems))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeCoords=nodeCoordsTst, &
        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check node coords
   k=1
   do i=1,numNodes ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k)*ESMF_COORDSYS_DEG2RAD .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

  ! check elem coords
   k=1
   do i=1,numElems ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (elemCoords(k)*ESMF_COORDSYS_DEG2RAD .ne. elemCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Debug output
   !write(*,*) "elemCoords   =",elemCoords
   !write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeCoords)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemCoords)

   ! Deallocate tst Arrays
   deallocate(nodeCoordsTst)
   deallocate(elemCoordsTst)


   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return


   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_coordSys_convert_EM_file


! This is a more through test that reads in a spherical 3D 2x2x1 mesh file 
! and verifies that every part is correct. 
subroutine  check_mesh_from_sph_3D_EM_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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

! XMRKX !

  ! Create Dest Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=18

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10,11,12,13,14,15,16,17,18/)

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,1.0, & ! node id 1
                  1.0,0.0,1.0, & ! node id 2
                  2.0,0.0,1.0, & ! node id 3
                  0.0,1.0,1.0, & ! node id 4
                  1.0,1.0,1.0, & ! node id 5
                  2.0,1.0,1.0, & ! node id 6
                  0.0,2.0,1.0, & ! node id 7
                  1.0,2.0,1.0, & ! node id 8
                  2.0,2.0,1.0, & ! node id 9
                  0.0,0.0,2.0,              & ! node id 10
                  1.0,0.0,2.0,              & ! node id 11
                  2.0,0.0,2.0,              & ! node id 12
                  0.0,1.0,2.0,              & ! node id 13
                  1.0,1.0,2.0,              & ! node id 14
                  2.0,1.0,2.0,              & ! node id 15
                  0.0,2.0,2.0,              & ! node id 16
                  1.0,2.0,2.0,              & ! node id 17
                  2.0,2.0,2.0 /)              ! node id 18

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0


     ! Set the number of each type of element, plus the total number.
     numElems=4

     ! Allocate and fill the element id array.
     allocate(elemIds(numElems))
     elemIds=(/1,2,3,4/)

     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numElems))
     elemTypes=ESMF_MESHELEMTYPE_HEX


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(8*numElems))
     elemConn=(/1,2,5,4,10,11,14,13, &  ! elem id 1
                2,3,6,5,11,12,15,14, &  ! elem id 2
                4,5,8,7,13,14,17,16,   &  ! elem id 3
                5,6,9,8,14,15,18,17/)  ! elem id 4

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5,10,11,13,14/)

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/0.0,0.0,1.0, & ! node id 1
                    1.0,0.0,1.0, & ! node id 2
                    0.0,1.0,1.0, & ! node id 4
                    1.0,1.0,1.0, & ! node id 5
                    0.0,0.0,2.0,              & ! node id 10
                    1.0,0.0,2.0, & ! node id 11
                    0.0,1.0,2.0, & ! node id 13
                    1.0,1.0,2.0 /) ! node id 14


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0, & ! node id 5
                    0, & ! node id 10
                    0, & ! node id 11
                    0, & ! node id 13
                    0/)  ! node id 14

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6,11,12,14,15/)

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,1.0, & ! node id 2
                    2.0,0.0,1.0, & ! node id 3
                    1.0,1.0,1.0, & ! node id 5
                    2.0,1.0,1.0, & ! node id 6
                    1.0,0.0,2.0,              & ! node id 11
                    2.0,0.0,2.0,              & ! node id 12
                    1.0,1.0,2.0,              & ! node id 14
                    2.0,1.0,2.0/)               ! node id 15

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1, & ! node id 6
                    0, & ! node id 11
                    1, & ! node id 12
                    0, & ! node id 14
                    1/)  ! node id 15

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,13,14,16,17/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.0,1.0,1.0, & ! node id 4
                     1.0,1.0,1.0, & ! node id 5
                     0.0,2.0,1.0, & ! node id 7
                     1.0,2.0,1.0, & ! node id 8
                     0.0,1.0,2.0,              & ! node id 13
                     1.0,1.0,2.0,              & ! node id 14
                     0.0,2.0,2.0,              & ! node id 16
                     1.0,2.0,2.0/)               ! node id 17


        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     0, & ! node id 13
                     0, & ! node id 14
                     2, & ! node id 16
                     2/)  ! node id 17
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1


     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9,14,15,17,18/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,1.0,1.0, &  ! node id 5
                     2.0,1.0,1.0, &  ! node id 6
                     1.0,2.0,1.0, &  ! node id 8
                     2.0,2.0,1.0, &  ! node id 9
                     1.0,1.0,2.0,              &  ! node id 14
                     2.0,1.0,2.0,              &  ! node id 15
                     1.0,2.0,2.0,              &  ! node id 17
                     2.0,2.0,2.0/)                ! node id 18

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3, & ! node id 9
                     0, & ! node id 14
                     1, & ! node id 15
                     2, & ! node id 17
                     3/)  ! node id 18

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

       endif
    endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_2x2x1_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
         elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_3D_EM",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) return

   ! Init correct to true before looking for problems
   correct=.true.

 ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 3) correct=.false.
   if (sdim .ne. 3) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.

   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElems*8 .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst

   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (nodeMaskIsPresentTst) correct=.false.
   if (elemMaskIsPresentTst) correct=.false.
   if (elemAreaIsPresentTst) correct=.false.
   if (elemCoordsIsPresentTst) correct=.false.

   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(3*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))

   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo

   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,3 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo


   ! Debug output
   ! write(*,*) "nodeMask   =",nodeMask
   ! write(*,*) "nodeMaskTst=",nodeMaskTst

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_3D_EM_file



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Creates the following mesh on
  ! 1 or 4 PETs. Returns an error
  ! if run on other than 1 or 4 PETs
  !
  !                     Mesh Ids
  !
  !   3.0   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  11b   / |
  !   2.5   |    9    |    10    |     /    |
  !         |         |          |  /   11a |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    6    |    7     |    8     |
  !         |         |          |          |
  !   1.0   5 ------- 6 -------- 7 -------- 8
  !         | 1    /  |  3     / |          |
  !   0.5   |    /    |     /    |    5     |
  !         | /    2  | /     4  |          |
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
  !         | 0    /  | 1     /  |          |
  !         |    /    |    /     |    1     |
  !         | /    0  | /      1 |          |
  !   0.0   0 ------- 0 -------- 1 -------- 1
  !
  !        0.0       1.0        2.0        3.0
  !
  !               Node Owners at corners
  !              Element Owners in centers
  !
  ! 

  ! This is a test that reads in a mesh from an ESMFMesh format file that
  ! has a 1D elementConn array, and a polybreak in the elementConn variable. 
  ! It verifies everything that it can verify is correct.
  ! (Some ESMF_MeshGet() arguments don't work for polybreak yet.) 
  ! The mesh is drawn above. 
subroutine  check_mesh_from_sph_pb_EM_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns
  integer :: numTriElems, numQuadElems, numPolyBreakElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0,1,0,1,0, &
                1,0,1,0,1,0,&
                1,0/)


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
      numTriElems=4
      numQuadElems=6
      numPolyBreakElems=1
      numElems=numTriElems+numQuadElems+numPolyBreakElems
      numElemConns=3*numTriElems+ &
                   4*numQuadElems+ &
                   7*numPolyBreakElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2,3,4,5,6,7,8,9,10,11/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_TRI,  & ! 1
                  ESMF_MESHELEMTYPE_TRI,  & ! 2
                  ESMF_MESHELEMTYPE_TRI,  & ! 3
                  ESMF_MESHELEMTYPE_TRI,  & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 8
                  ESMF_MESHELEMTYPE_QUAD, & ! 9
                  ESMF_MESHELEMTYPE_QUAD, & ! 10
                  7/) ! 11

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.25,0.75, & ! 1
                   0.75,0.25, & ! 2
                   1.25,0.75, & ! 3
                   1.75,0.25, & ! 4
                   2.5,0.5,   & ! 5
                   0.5,1.5,   & ! 6
                   1.5,1.5,   & ! 7
                   2.5,1.5,   & ! 8
                   0.5,2.5,   & ! 9
                   1.5,2.5,   & ! 10
                   2.5,2.5/)    ! 11

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,6,5,     & ! 1
                 1,2,6,     & ! 2
                 2,7,6,     & ! 3
                 2,3,7,     & ! 4
                 3,4,8,7,   & ! 5
                 5,6,10,9,  & ! 6
                 6,7,11,10, & ! 7
                 7,8,12,11, & ! 8
                 9,10,14,13, & ! 9
                 10,11,15,14, & ! 10
                 11,12,16, ESMF_MESH_POLYBREAK, 11,16,15/) ! 11


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/0.51_ESMF_KIND_R8, 0.52_ESMF_KIND_R8, 0.53_ESMF_KIND_R8, 0.54_ESMF_KIND_R8, &
           1.5_ESMF_KIND_R8, 1.6_ESMF_KIND_R8, 1.7_ESMF_KIND_R8, 1.8_ESMF_KIND_R8, &
           1.9_ESMF_KIND_R8, 1.1_ESMF_KIND_R8, 1.11_ESMF_KIND_R8/)


   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,5,6/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0/)


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
      numTriElems=2
      numQuadElems=0
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/1,2/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! 1
                  ESMF_MESHELEMTYPE_TRI/)  ! 2

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.25,0.75, &  ! 1
                   0.75,0.25/) ! 2

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,4,3, &  ! 1
                 1,2,4 /)  ! 2

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/0.51_ESMF_KIND_R8, 0.52_ESMF_KIND_R8/)

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,4,6,7,8/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/0,1,0,0,1,0/)

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
      numTriElems=2
      numQuadElems=1
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/3,4,5/)

      !! elem types
      allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, &  ! 3
                   ESMF_MESHELEMTYPE_TRI, &  ! 4
                   ESMF_MESHELEMTYPE_QUAD/)  ! 5

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.25,0.75, & ! 3
                   1.75,0.25, & ! 4
                   2.50,0.50/)  ! 5

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,5,4,   &   ! 3
                 1,2,5,   &   ! 4
                 2,3,6,5/)    ! 5

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/0.53_ESMF_KIND_R8,0.54_ESMF_KIND_R8,1.5_ESMF_KIND_R8/)

     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/5,6,7,   &
               9,10,11, &
               13,14,15/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1, &
                1,0,1, &
                1,0,1/)

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
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15

      ! Fill in elem data
      numTriElems=0
      numQuadElems=4
      numElems=numTriElems+numQuadElems
      numElemConns=3*numTriElems+4*numQuadElems

       !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,7,9,10/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD, & ! 9
                  ESMF_MESHELEMTYPE_QUAD/)  ! 10


      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.5,1.5, & ! 6
                   1.5,1.5, & ! 7
                   0.5,2.5, & ! 9
                   1.5,2.5/)  ! 10

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,5,4,  & ! 6
                 2,3,6,5,  & ! 7
                 4,5,8,7,  & ! 9
                 5,6,9,8/)   ! 10

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,1,1/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.6_ESMF_KIND_R8,1.7_ESMF_KIND_R8,1.9_ESMF_KIND_R8,1.1_ESMF_KIND_R8/)

     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/7,8,11,12,15,16/)

     !! node Mask
     allocate(nodeMask(numNodes))
     nodeMask=(/1,0,1,0,1,0/)

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
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
                   3/)  ! 16

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numPolyBreakElems=1
      numElems=numTriElems+numQuadElems+numPolyBreakElems
      numElemConns=3*numTriElems+ &
                   4*numQuadElems+ &
                   7*numPolyBreakElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/8,11/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 8
                  7/) ! 11

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 8
                   2.5,2.5/) ! 11

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3, & ! 8
                 11,12,16, ESMF_MESH_POLYBREAK, 11,16,15/) ! 11

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0/)

      !! elem area
      !! (need kinds because not R4 doesn't rep. close enough)
      allocate(elemArea(numElems))
      elemArea=(/1.8_ESMF_KIND_R8,1.11_ESMF_KIND_R8/)
     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_polybreak_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
        addUserArea=.true., &
        elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_pb_esmf",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) return

   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
    !   elementConnCount=numElemConnsTst, & ! DOESN'T WORK YET WITH NGONS
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
  ! if (numElemConns .ne. numElemConnsTst) correct=.false. ! DOESN'T WORK YET WITH NGONS

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst


   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (.not. nodeMaskIsPresentTst) correct=.false.
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (.not. elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(nodeMaskTst(numNodesTst))
  ! allocate(elemIdsTst(numElemsTst))    ! DOESN'T WORK YET WITH NGONS
  ! allocate(elemTypesTst(numElemsTst))  ! DOESN'T WORK YET WITH NGONS
  ! allocate(elemConnTst(numElemConnsTst)) ! DOESN'T WORK YET WITH NGONS
  ! allocate(elemMaskTst(numElemsTst))    ! DOESN'T WORK YET WITH NGONS
  ! allocate(elemAreaTst(numElemsTst))   ! DOESN'T WORK YET WITH NGONS
  ! allocate(elemCoordsTst(2*numElemsTst)) ! DOESN'T WORK YET WITH NGONS

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        nodeMask=nodeMaskTst, &
   !     elementIds=elemIdsTst, &  ! DOESN'T WORK YET WITH NGONS
   !     elementTypes=elemTypesTst, & ! DOESN'T WORK YET WITH NGONS
   !     elementConn=elemConnTst, & ! DOESN'T WORK YET WITH NGONS
   !     elementMask=elemMaskTst, &  ! DOESN'T WORK YET WITH NGONS
   !     elementArea=elemAreaTst, & ! DOESN'T WORK YET WITH NGONS
   !     elementCoords=elemCoordsTst, & ! DOESN'T WORK YET WITH NGONS
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo


   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check node Mask
   do i=1,numNodesTst
      if (nodeMask(i) .ne. nodeMaskTst(i)) correct=.false.
   enddo


   ! Debug output
   ! write(*,*) "nodeMask   =",nodeMask
   ! write(*,*) "nodeMaskTst=",nodeMaskTst

   ! Check elem ids
   ! DOESN'T WORK YET WITH NGONS
   !do i=1,numElemsTst
   !   if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   !enddo

   ! Check elem Types
   ! DOESN'T WORK YET WITH NGONS
   !do i=1,numElemsTst
   !   if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   !enddo

   ! Check elem Connections
   ! DOESN'T WORK YET WITH NGONS
   !do i=1,numElemConnsTst
   !   if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   !enddo

   ! Check elem mask
   ! DOESN'T WORK YET WITH NGONS
   !do i=1,numElems
   !   if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
   !enddo

   ! Check elem area
   ! DOESN'T WORK YET WITH NGONS
   !do i=1,numElems
   !   if (elemArea(i) .ne. elemAreaTst(i)) correct=.false.
   !enddo

   ! Check elem Coords
   ! DOESN'T WORK YET WITH NGONS
   !k=1
   !do i=1,numElemsTst ! Loop over nodes
   !   do j=1,2 ! Loop over coord spatial dim
   !      if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
   !      k=k+1
   !   enddo
   !enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   deallocate(nodeMask)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)
   deallocate(elemArea)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(nodeMaskTst)
   !deallocate(elemIdsTst)  ! DOESN'T WORK YET WITH NGONS
   !deallocate(elemTypesTst) ! DOESN'T WORK YET WITH NGONS
   !deallocate(elemConnTst) ! DOESN'T WORK YET WITH NGONS
   !deallocate(elemMaskTst) ! DOESN'T WORK YET WITH NGONS
   !deallocate(elemAreaTst) ! DOESN'T WORK YET WITH NGONS
   !deallocate(elemCoordsTst) ! DOESN'T WORK YET WITH NGONS

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_pb_EM_file



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
  ! 

  ! This is a more through test that reads in a spherical 3x3 UGRID mesh file 
  ! (that creates the mesh drawn above) and verifies that every part
  ! is correct. 
subroutine  check_mesh_from_sph_3x3_UG_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 0, 1, 0, 1/)

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/)

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/)

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
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15


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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,0,1/)

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
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0,1/)

     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_3x3_ugrid.nc", &
        fileformat=ESMF_FILEFORMAT_UGRID, &
        elementDistgrid=elemDistgrid, &
        maskFlag=ESMF_MESHLOC_ELEMENT, &
        varName="elemMask", &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_3x3_ugrid",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElemConns .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst


   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (      nodeMaskIsPresentTst) correct=.false.
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (      elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))
   allocate(elemMaskTst(numElemsTst))
   allocate(elemCoordsTst(2*numElemsTst))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        elementMask=elemMaskTst, & 
        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo


   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Check elem mask
   do i=1,numElems
      if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
   enddo

   ! Debug output
   !write(*,*) "elemMask   =",elemMask
   !write(*,*) "elemMaskTst=",elemMaskTst

   ! Check elem Coords
   k=1
   do i=1,numElemsTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)
   deallocate(elemMaskTst)
   deallocate(elemCoordsTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return


   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_3x3_UG_file


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Creates the following mesh on
  ! 1 or 4 PETs. Returns an error
  ! if run on other than 1 or 4 PETs
  !
  !                     Mesh Ids
  !
  !   3.0   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  9b    / |
  !   2.5   |    7    |    8     |     /    |
  !         |         |          |  /   9a  |
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
  ! 

  ! This is a more through test that reads in a spherical polybreak UGRID mesh file 
  ! (that creates the mesh drawn above) and verifies that every part we can check
  ! is correct.
subroutine  check_mesh_from_sph_pb_UG_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns
  integer :: numTriElems, numQuadElems, numPolybreakElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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
      numQuadElems=8
      numPolybreakElems=1
      numElems=numTriElems+numQuadElems+numPolybreakElems
      numElemConns=3*numTriElems+4*numQuadElems+7*numPolybreakElems

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
                  7/)  ! 9


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
                 11,12,16, ESMF_MESH_POLYBREAK, 11,16,15/) ! 9


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 0, 1, 0/)

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/)

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/)

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
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15


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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,0,1/)

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
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
                   3/)  ! 16

      ! Fill in elem data
      numTriElems=0
      numQuadElems=1
      numPolybreakElems=1
      numElems=numTriElems+numQuadElems+numPolybreakElems
      numElemConns=3*numTriElems+4*numQuadElems+7*numPolybreakElems

      !! elem ids
      allocate(elemIds(numElems))
      elemIds=(/6,9/)

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  7/) ! 9

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.5,1.5, & ! 6
                   2.5,2.5/)  ! 9

      !! elem conn
      allocate(elemConn(numElemConns))
      elemConn=(/1,2,4,3, & ! 6
                 3,4,6,ESMF_MESH_POLYBREAK,3,6,5/) ! 9

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0/)

     endif
   endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_polybreak_ugrid.nc", &
        fileformat=ESMF_FILEFORMAT_UGRID, &
        elementDistgrid=elemDistgrid, &
        maskFlag=ESMF_MESHLOC_ELEMENT, &
        varName="elemMask", &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_pb_ugrid",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Init correct to true before looking for problems
   correct=.true.

   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
   !     elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   !if (numElemConns .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst


   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (      nodeMaskIsPresentTst) correct=.false.
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (      elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
!   allocate(elemIdsTst(numElemsTst))
!   allocate(elemTypesTst(numElemsTst))
!   allocate(elemConnTst(numElemConnsTst))
!   allocate(elemMaskTst(numElemsTst))
!   allocate(elemCoordsTst(2*numElemsTst))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
!        elementIds=elemIdsTst, &
!        elementTypes=elemTypesTst, &
!        elementConn=elemConnTst, &
!        elementMask=elemMaskTst, & 
!        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo


   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check elem ids
!   do i=1,numElemsTst
!      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
!   enddo

   ! Check elem Types
!   do i=1,numElemsTst
!      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
!   enddo

   ! Check elem Connections
!   do i=1,numElemConnsTst
!      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
!   enddo

   ! Check elem mask
!   do i=1,numElems
!      if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
!   enddo

   ! Debug output
   !write(*,*) "elemMask   =",elemMask
   !write(*,*) "elemMaskTst=",elemMaskTst

   ! Check elem Coords
!   k=1
!   do i=1,numElemsTst ! Loop over nodes
!      do j=1,2 ! Loop over coord spatial dim
!         if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
!         k=k+1
!      enddo
!   enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
!   deallocate(elemIdsTst)
!   deallocate(elemTypesTst)
!   deallocate(elemConnTst)
!   deallocate(elemMaskTst)
!   deallocate(elemCoordsTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return


   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_pb_UG_file


! This is a more through test that reads in a spherical 3D 2x2x1 mesh file 
! and verifies that every part is correct. 
subroutine  check_mesh_from_sph_3D_UG_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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

! XMRKX !

  ! Create Dest Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=18

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10,11,12,13,14,15,16,17,18/)

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,1.0, & ! node id 1
                  1.0,0.0,1.0, & ! node id 2
                  2.0,0.0,1.0, & ! node id 3
                  0.0,1.0,1.0, & ! node id 4
                  1.0,1.0,1.0, & ! node id 5
                  2.0,1.0,1.0, & ! node id 6
                  0.0,2.0,1.0, & ! node id 7
                  1.0,2.0,1.0, & ! node id 8
                  2.0,2.0,1.0, & ! node id 9
                  0.0,0.0,2.0,              & ! node id 10
                  1.0,0.0,2.0,              & ! node id 11
                  2.0,0.0,2.0,              & ! node id 12
                  0.0,1.0,2.0,              & ! node id 13
                  1.0,1.0,2.0,              & ! node id 14
                  2.0,1.0,2.0,              & ! node id 15
                  0.0,2.0,2.0,              & ! node id 16
                  1.0,2.0,2.0,              & ! node id 17
                  2.0,2.0,2.0 /)              ! node id 18

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0


     ! Set the number of each type of element, plus the total number.
     numElems=4

     ! Allocate and fill the element id array.
     allocate(elemIds(numElems))
     elemIds=(/1,2,3,4/)

     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numElems))
     elemTypes=ESMF_MESHELEMTYPE_HEX


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(8*numElems))
     elemConn=(/1,2,5,4,10,11,14,13, &  ! elem id 1
                2,3,6,5,11,12,15,14, &  ! elem id 2
                4,5,8,7,13,14,17,16,   &  ! elem id 3
                5,6,9,8,14,15,18,17/)  ! elem id 4

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5,10,11,13,14/)

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/0.0,0.0,1.0, & ! node id 1
                    1.0,0.0,1.0, & ! node id 2
                    0.0,1.0,1.0, & ! node id 4
                    1.0,1.0,1.0, & ! node id 5
                    0.0,0.0,2.0,              & ! node id 10
                    1.0,0.0,2.0, & ! node id 11
                    0.0,1.0,2.0, & ! node id 13
                    1.0,1.0,2.0 /) ! node id 14


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0, & ! node id 5
                    0, & ! node id 10
                    0, & ! node id 11
                    0, & ! node id 13
                    0/)  ! node id 14

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6,11,12,14,15/)

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,1.0, & ! node id 2
                    2.0,0.0,1.0, & ! node id 3
                    1.0,1.0,1.0, & ! node id 5
                    2.0,1.0,1.0, & ! node id 6
                    1.0,0.0,2.0,              & ! node id 11
                    2.0,0.0,2.0,              & ! node id 12
                    1.0,1.0,2.0,              & ! node id 14
                    2.0,1.0,2.0/)               ! node id 15

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1, & ! node id 6
                    0, & ! node id 11
                    1, & ! node id 12
                    0, & ! node id 14
                    1/)  ! node id 15

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,13,14,16,17/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.0,1.0,1.0, & ! node id 4
                     1.0,1.0,1.0, & ! node id 5
                     0.0,2.0,1.0, & ! node id 7
                     1.0,2.0,1.0, & ! node id 8
                     0.0,1.0,2.0,              & ! node id 13
                     1.0,1.0,2.0,              & ! node id 14
                     0.0,2.0,2.0,              & ! node id 16
                     1.0,2.0,2.0/)               ! node id 17


        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     0, & ! node id 13
                     0, & ! node id 14
                     2, & ! node id 16
                     2/)  ! node id 17
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1


     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9,14,15,17,18/)

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,1.0,1.0, &  ! node id 5
                     2.0,1.0,1.0, &  ! node id 6
                     1.0,2.0,1.0, &  ! node id 8
                     2.0,2.0,1.0, &  ! node id 9
                     1.0,1.0,2.0,              &  ! node id 14
                     2.0,1.0,2.0,              &  ! node id 15
                     1.0,2.0,2.0,              &  ! node id 17
                     2.0,2.0,2.0/)                ! node id 18

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3, & ! node id 9
                     0, & ! node id 14
                     1, & ! node id 15
                     2, & ! node id 17
                     3/)  ! node id 18

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/)

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

       endif
    endif


   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_2x2x1_ugrid.nc", &
        fileformat=ESMF_FILEFORMAT_UGRID, &
         elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_3D_UG",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) return

   ! Init correct to true before looking for problems
   correct=.true.

 ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 3) correct=.false.
   if (sdim .ne. 3) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.

   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElems*8 .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) "numNodes=",numNodes,numNodesTst
   !write(*,*) "numElems=",numElems,numElemsTst
   !write(*,*) "numElemConns=",numElemConns,numElemConnsTst

   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (nodeMaskIsPresentTst) correct=.false.
   if (elemMaskIsPresentTst) correct=.false.
   if (elemAreaIsPresentTst) correct=.false.
   if (elemCoordsIsPresentTst) correct=.false.

   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(3*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))

   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

!   do i=1,numElemsTst
!      write(*,*) localPet,"# ",elemIdsTst(i),"ea=",elemAreaTst(i),"ec=",elemCoordsTst(2*i-1),elemCoordsTst(2*i),"em=",elemMaskTst(i)
!   enddo

   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,3 
         ! Don't check 3rd dim, because it's normalized to Earth radius
         if ((j<3) .and. (nodeCoords(k) .ne. nodeCoordsTst(k))) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo


   ! Debug output
   ! write(*,*) "nodeMask   =",nodeMask
   ! write(*,*) "nodeMaskTst=",nodeMaskTst

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Debug output
   ! write(*,*) "elemCoords   =",elemCoords
   ! write(*,*) "elemCoordsTst=",elemCoordsTst

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_3D_UG_file


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
  ! 

  ! This is a more through test that reads in a spherical 3x3 SCRIP mesh file 
  ! (that creates the mesh drawn above) and verifies that every part is correct. 
  
  ! NOTE: The order of the nodes in the created mesh is heavily dependant on the SCRIP format to 
  !       ESMF Mesh format conversion program in IO/interface. If that changes it's possible that this
  !       test will start reporting a failure and it will need to be changed to correct the order, etc. 
  !       of the nodes. 

subroutine  check_mesh_from_sph_3x3_SC_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: elemDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys

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
                  1.0,1.0, &  ! 3
                  0.0,1.0, &  ! 4
                  2.0,0.0, &  ! 5
                  2.0,1.0, &  ! 6
                  3.0,0.0, &  ! 7
                  3.0,1.0, &  ! 8
                  1.0,2.0, &  ! 9
                  0.0,2.0, &  ! 10
                  2.0,2.0, &  ! 11
                  3.0,2.0, &  ! 12
                  1.0,3.0, &  ! 13
                  0.0,3.0, &  ! 14
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
      elemConn=(/1,2,3,4,   & ! 1
                 2,5,6,3,   & ! 2
                 5,7,8,6,   & ! 3
                 4,3,9,10,  & ! 4
                 3,6,11,9, & ! 5
                 6,8,12,11, & ! 6
                 10,9,13,14, & ! 7
                 9,11,15,13, & ! 8
                 11,12,16, & ! 9
                 11,16,15/) ! 10


      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1, 0, 1, 1, 1, 1, 0, 1, 0, 1/)

   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

     ! Fill in node data
     numNodes=4

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4/)


     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/0.0,0.0, & ! 1
                  1.0,0.0, &  ! 2
                  1.0,1.0, &  ! 3
                  0.0,1.0 /)  ! 4

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
      elemConn=(/1,2,3,4/) ! 1

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/)

     else if (localPet .eq. 1) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/2,3,5,6,7,8/)


     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/1.0,0.0, &  ! 2
                  1.0,1.0, &  ! 3
                  2.0,0.0, &  ! 5
                  2.0,1.0, &  ! 6
                  3.0,0.0, &  ! 7
                  3.0,1.0 /)  ! 8



      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 2
                   0, & ! 3
                   1, & ! 5
                   1, & ! 6
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
      elemConn=(/1,3,4,2,   & ! 2
                 3,5,6,4/)    ! 3

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/)

     else if (localPet .eq. 2) then

     ! Fill in node data
     numNodes=9

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/3,4,6,   &
               9,10,11, &
               13,14,15/)

     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/1.0,1.0, &  ! 3
                  0.0,1.0, &  ! 4
                  2.0,1.0, &  ! 6
                  1.0,2.0, &  ! 9
                  0.0,2.0, &  ! 10
                  2.0,2.0, &  ! 11
                  1.0,3.0, &  ! 13
                  0.0,3.0, &  ! 14
                  2.0,3.0/)   ! 15


      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! 3
                   0, & ! 4
                   1, & ! 6
                   2, & ! 9
                   2, & ! 10
                   2, & ! 11
                   2, & ! 13
                   2, & ! 14
                   2/)  ! 15


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
      elemConn=(/2,1,4,5,  & ! 4
                 1,3,6,4,  & ! 5
                 5,4,7,8,  & ! 7
                 4,6,9,7/)   ! 8

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,0,1/)

     else if (localPet .eq. 3) then

     ! Fill in node data
     numNodes=6

     !! node ids
     allocate(nodeIds(numNodes))
     nodeIds=(/6,8,11,12,15,16/)


     !! node Coords
     allocate(nodeCoords(numNodes*2))
     nodeCoords=(/2.0,1.0, &  ! 6
                  3.0,1.0, &  ! 8
                  2.0,2.0, &  ! 11
                  3.0,2.0, &  ! 12
                  2.0,3.0, &  ! 15
                  3.0,3.0 /)  ! 16


      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=(/1, & ! 7
                   1, & ! 8
                   2, & ! 11
                   3, & ! 12
                   2, & ! 15
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
                 3,4,6,   & ! 9
                 3,6,5/)    ! 10

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0,1/)

     endif
   endif

   ! Create distgrid to ensure that elements are created on the
   ! same PET as described above
   elemdistgrid=ESMF_DistGridCreate(elemIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Read mesh from file that's the same as the one described by info set up above
   mesh=ESMF_MeshCreate("data/test_sph_3x3_scrip.nc", &
        fileformat=ESMF_FILEFORMAT_SCRIP, &
        elementDistgrid=elemDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_3x3_scrip",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


   ! Init correct to true before looking for problems
   correct=.true.


   ! Get dim and coord info
   call ESMF_MeshGet(mesh, &
        parametricDim=pdim, &
        spatialDim=sdim, &
        coordSys=coordSys, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (pdim .ne. 2) correct=.false.
   if (sdim .ne. 2) correct=.false.
   if (coordSys .ne. ESMF_COORDSYS_SPH_DEG) correct=.false.


   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        elementCount=numElemsTst, &
        elementConnCount=numElemConnsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check Counts
   if (numNodes .ne. numNodesTst) correct=.false.
   if (numElems .ne. numElemsTst) correct=.false.
   if (numElemConns .ne. numElemConnsTst) correct=.false.

   ! Debugging
   !write(*,*) localPet,"numNodes=",numNodes,numNodesTst
   !write(*,*) localPet,"numElems=",numElems,numElemsTst
   !write(*,*) localPet,"numElemConns=",numElemConns,numElemConnsTst

   ! Get is present information
   call ESMF_MeshGet(mesh, &
        nodeMaskIsPresent=nodeMaskIsPresentTst, &
        elementMaskIsPresent=elemMaskIsPresentTst, &
        elementAreaIsPresent=elemAreaIsPresentTst, &
        elementCoordsIsPresent=elemCoordsIsPresentTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug
   ! write(*,*) "nodeMaskIsPresent=",nodeMaskIsPresentTst
   ! write(*,*) "elemMaskIsPresent=",elemMaskIsPresentTst
   ! write(*,*) "elemAreaIsPresent=",elemAreaIsPresentTst
   ! write(*,*) "elemCoordsIsPresent=",elemCoordsIsPresentTst

   ! Check is present info
   if (      nodeMaskIsPresentTst) correct=.false.
   if (.not. elemMaskIsPresentTst) correct=.false.
   if (      elemAreaIsPresentTst) correct=.false.
   if (.not. elemCoordsIsPresentTst) correct=.false.


   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeCoordsTst(2*numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))
   allocate(elemIdsTst(numElemsTst))
   allocate(elemTypesTst(numElemsTst))
   allocate(elemConnTst(numElemConnsTst))
   allocate(elemMaskTst(numElemsTst))
   allocate(elemCoordsTst(2*numElemsTst))


   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeCoords=nodeCoordsTst, &
        nodeOwners=nodeOwnersTst, &
        elementIds=elemIdsTst, &
        elementTypes=elemTypesTst, &
        elementConn=elemConnTst, &
        elementMask=elemMaskTst, & 
        elementCoords=elemCoordsTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Check node ids
   do i=1,numNodesTst
      if (nodeIds(i) .ne. nodeIdsTst(i)) correct=.false.
   enddo

   ! Check node Coords
   k=1
   do i=1,numNodesTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (nodeCoords(k) .ne. nodeCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! Check node Owners
   do i=1,numNodesTst
      if (nodeOwners(i) .ne. nodeOwnersTst(i)) correct=.false.
   enddo

   ! Check elem ids
   do i=1,numElemsTst
      if (elemIds(i) .ne. elemIdsTst(i)) correct=.false.
   enddo

   ! Check elem Types
   do i=1,numElemsTst
      if (elemTypes(i) .ne. elemTypesTst(i)) correct=.false.
   enddo

   ! Check elem Connections
   do i=1,numElemConnsTst
      if (elemConn(i) .ne. elemConnTst(i)) correct=.false.
   enddo

   ! Check elem mask
   do i=1,numElems
      if (elemMask(i) .ne. elemMaskTst(i)) correct=.false.
   enddo

   ! Check elem Coords
   k=1
   do i=1,numElemsTst ! Loop over nodes
      do j=1,2 ! Loop over coord spatial dim
         if (elemCoords(k) .ne. elemCoordsTst(k)) correct=.false.
         k=k+1
      enddo
   enddo

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)
   deallocate(elemMask)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeCoordsTst)
   deallocate(nodeOwnersTst)
   deallocate(elemIdsTst)
   deallocate(elemTypesTst)
   deallocate(elemConnTst)
   deallocate(elemMaskTst)
   deallocate(elemCoordsTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(elemDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return


   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_from_sph_3x3_SC_file



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
  ! 

  ! This test checks the create dual option to mesh create from file.
  ! It uses a 3x3 ESMF Mesh file (shown above) and creates a dual
  ! from it.  
subroutine  test_create_dual_from_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc

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

  ! Read mesh from file 
  mesh=ESMF_MeshCreate("data/test_sph_3x3_esmf.nc", &
       fileformat=ESMF_FILEFORMAT_ESMFMESH, &
       convertToDual=.true., &
       rc=rc)
  if (rc /= ESMF_SUCCESS) return
  
   ! DEBUG OUTPUT
   !call ESMF_MeshWrite(mesh,"mesh_dual",rc=rc)
   !if (rc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine test_create_dual_from_file

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
  ! 

  ! This test reads in a mesh and then uses the mesh create from file's 
  ! built in redist capability to redist to a node distribution 
subroutine  check_mesh_node_redist_from_file(correct, rc)
  type(ESMF_Mesh) :: mesh
  logical :: correct
  integer :: rc
  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
  integer :: numNodes, numOwnedNodes, numOwnedNodesTst
  integer :: numElems,numOwnedElemsTst
  integer :: numElemConns, numTriElems, numQuadElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, allocatable :: elemMask(:)
  real(ESMF_KIND_R8),allocatable :: elemArea(:)
  integer :: petCount, localPet
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: nodeDistgrid
  type(ESMF_Field) :: nodeField, elemField
  integer :: i,j,k
  integer :: numNodesTst, numElemsTst,numElemConnsTst
  integer,allocatable :: elemIdsTst(:)
  integer,allocatable :: elemTypesTst(:)
  integer,allocatable :: elemConnTst(:)
  real(ESMF_KIND_R8),allocatable :: elemAreaTst(:)
  integer,allocatable :: nodeIdsTst(:)
  real(ESMF_KIND_R8),allocatable :: nodeCoordsTst(:)
  integer,allocatable :: nodeOwnersTst(:)
  integer,allocatable :: nodeMaskTst(:)
  integer,allocatable :: elemMaskTst(:)
  real(ESMF_KIND_R8), allocatable :: elemCoordsTst(:)
  logical :: nodeMaskIsPresentTst
  logical :: elemMaskIsPresentTst
  logical :: elemAreaIsPresentTst
  logical :: elemCoordsIsPresentTst             
  integer :: pdim, sdim
  type(ESMF_CoordSys_Flag) :: coordSys
  logical :: found

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


   else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
     if (localPet .eq. 0) then

        ! Fill in node data
        numNodes=4

        !! node ids
        allocate(nodeIds(numNodes))
        nodeIds=(/11,12,15,16/)
        
     else if (localPet .eq. 1) then

        ! Fill in node data
        numNodes=4
        
        !! node ids
        allocate(nodeIds(numNodes))
        nodeIds=(/9,10, &
                  13,14/)

     else if (localPet .eq. 2) then

        ! Fill in node data
        numNodes=4
        
        !! node ids
        allocate(nodeIds(numNodes))
        nodeIds=(/3,4,7,8/)


     else if (localPet .eq. 3) then

        ! Fill in node data
        numNodes=4
        
        !! node ids
        allocate(nodeIds(numNodes))
        nodeIds=(/1,2,5,6/)
     endif
   endif


   ! Create distgrid to test node redist
   nodedistgrid=ESMF_DistGridCreate(nodeIds, rc=rc)
   if (rc /= ESMF_SUCCESS) return
  

   ! Read mesh from file and redist to node distribution in distgrid
   mesh=ESMF_MeshCreate("data/test_sph_3x3_esmf.nc", &
        fileformat=ESMF_FILEFORMAT_ESMFMESH, &
        nodalDistgrid=nodeDistgrid, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Init correct to true before looking for problems
   correct=.true.

   ! Get counts 
   call ESMF_MeshGet(mesh, &
        nodeCount=numNodesTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! Debug Output
   !write(*,*) "numNodes=",numNodes,numNodesTst

   ! Allocate space for tst arrays
   allocate(nodeIdsTst(numNodesTst))
   allocate(nodeOwnersTst(numNodesTst))

 ! XMRKX
   ! Get Information
   call ESMF_MeshGet(mesh, &
        nodeIds=nodeIdsTst, &
        nodeOwners=nodeOwnersTst, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return


   ! Loop over node ids that we specified should
   ! be on this PET and make sure they are here
   do i=1,numNodes

      ! See if we can find in list of node ids from mesh
      found=.false.
      do j=1,numNodesTst
         if (nodeIdsTst(j) == nodeIds(i)) then
            found=.true.
            exit
         endif
      enddo

      ! If not found set correct to false
      if (.not. found) correct=.false.
   enddo


   ! deallocate node data
   deallocate(nodeIds)

   ! Deallocate tst Arrays
   deallocate(nodeIdsTst)
   deallocate(nodeOwnersTst)

   ! Get rid of Mesh
   call ESMF_MeshDestroy(mesh, rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Get rid of elemDistgrid
  call ESMF_DistGridDestroy(nodeDistgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) return

   ! Return success
   rc=ESMF_SUCCESS

end subroutine check_mesh_node_redist_from_file



end program ESMF_MeshFileIOUTest
